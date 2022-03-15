utils::globalVariables("progress_bar")
#' Convert OpenAlex collection of works from list format to data frame
#'
#' It converts bibliographic collection of works gathered from OpenAlex database \href{https://openalex.org/}{https://openalex.org/} into data frame.
#' The function converts a list of works obtained using \code{oaApiRequest} into a data frame/tibble.
#'
#' @param data is a list. data is the output of the function \code{oaApiRequest}.
#'
#' @return a data.frame.
#'
#' For more extensive information about OpenAlex API, please visit: \href{https://docs.openalex.org/api}{https://docs.openalex.org/api}
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' # Query to search all works citing the article:
#' #  Aria, M., & Cuccurullo, C. (2017). bibliometrix:
#' #   An R-tool for comprehensive science mapping analysis.
#' #   Journal of informetrics, 11(4), 959-975.
#'
#' #  published in 2021.
#' #  The paper is associated to the OpenAlex id W2755950973.
#'
#' #  Results have to be sorted by relevance score in a descending order.
#'
#' query <- oaQueryBuild(
#' identifier=NULL,
#' entity = "works",
#' filter = "cites:W2755950973",
#' date_from = "2021-01-01",
#' date_to = "2021-12-31",
#' search=NULL,
#' sort="relevance_score:desc",
#' endpoint = "https://api.openalex.org/")
#'
#' res <- oaApiRequest(
#'    query_url = query,
#'    total.count = FALSE,
#'    verbose = FALSE
#'    )
#'
#' df <- oaWorks2df(res)
#'
#' df
#'
#' }
#'
# @export
oaWorks2df <- function(data){

  # replace NULL with NA
  data <- simple_rapply(data, function(x) if(is.null(x)) NA else x)

  if (!is.null(data$id)){data <- list(data)}

  if (is.null(data[[1]]$id)) {
    message("the list does not contain a valid OpenAlex collection")
    return()
  }

  n <- length(data)

  list_df<- vector(mode = "list", length = n)

  pb <- progress::progress_bar$new(
    format = "  converting [:bar] :percent eta: :eta",
    total = n, clear = FALSE, width = 60)


  for (i in 1:n){
    pb$tick()
    #print(i)
    paper <- data[[i]]


    id <- paper$id
    title <- paper$display_name
    pubdate <- paper$publication_date
    relscore <- paper$relevance_score
    # host venue
    so_id <- paper$host_venue$id
    so <- paper$host_venue$display_name
    publisher <- paper$host_venue$publisher
    issn <- list(unlist(paper$host_venue$issn))
    url <- paper$host_venue$url
    oa <- paper$host_venue$is_oa
    if (!is.na(paper$biblio[1])){
      first_page <- paper$biblio$first_page[1]
      last_page <- paper$biblio$last_page[1]
      volume <- paper$biblio$volume[1]
      issue <- paper$biblio$issue[1]
    } else{
      first_page <- NA
      last_page <- NA
      volume <- NA
      issue <- NA
    }

    # authorships and affilitation
    author <- list(do.call(rbind,lapply(paper$authorships, function(l){

      if (length(l[["institutions"]])>0){
        institution_id <- l[["institutions"]][[1]]$id
        institution_name <- l[["institutions"]][[1]]$display_name
        institution_ror <- l[["institutions"]][[1]]$ror
        institution_country <- l[["institutions"]][[1]]$country_code
        institution_type <- l[["institutions"]][[1]]$type
      } else {
        institution_id <- NA
        institution_name <- NA
        institution_ror <- NA
        institution_country <- NA
        institution_type <- NA
      }
      L <- data.frame(
        au_id=l[["author"]]$id,
        au_name=l[["author"]]$display_name,
        au_orcid=l[["author"]]$orcid,
        au_position=l$author_position,
        au_affiliation_raw=l$raw_affiliation_string,
          institution_id = institution_id,
          institution_name = institution_name,
          institution_ror = institution_ror,
          institution_country = institution_country,
          institution_type = institution_type
      )
    })))

    # concepts
    concept <- list(do.call(rbind,lapply(paper$concepts, function(l){
      L <- data.frame(
        concept_id=l$id,
        concept_name=l$display_name,
        concept_score=l$score,
        concept_lecel=l$level,
        concept_url=l$wikidata
      )
    })))

    TC <- paper$cited_by_count

    # Total Citations per Year
    if(length(paper$counts_by_year)>0){
      TCperYear <- unlist(paper$counts_by_year)
      lab <- names(TCperYear)
      TCperYear <- list(data.frame(year=TCperYear[lab=="year"],
                                   TC=TCperYear[lab=="cited_by_count"]))
    }else{
      TCperYear=NA
    }

    PY <- paper$publication_year
    cited_by_url <- paper$cited_by_api_url
    if (length(paper$ids)>0){
      ids <- unlist(paper$ids)
      ids <- list(data.frame(item=names(ids), value=ids))
    }else{
      ids <- NA
    }
    DI <- paper$doi
    DT <- paper$type
    CR <- unlist(paper$referenced_works)
    related_works <- unlist(paper$related_works)

    # Abstract
    if (!is.na(paper$abstract_inverted_index[1])){
      ab <- data.frame(term=names(unlist(paper$abstract_inverted_index)),pos=unlist(paper$abstract_inverted_index))

      if (nrow(ab)>0){
        ab$term2=gsub("[0-9]*$","",ab$term)
        ### rimuove numeri al termine delle parole dell'abstract, e ne ordina i temrini
        ab <- ab %>%
          group_by(.data$term2) %>%
          mutate(number = seq.int(length(.data$term2)),
                 number = nchar(as.character(.data$number)),
                 len = length(.data$term2))
        ab2 <- ab %>%
          filter(.data$len==1)

        ab <- ab %>%
          filter(.data$len>1) %>%
          mutate(term = substr(.data$term,1,nchar(.data$term)-.data$number)) %>%
          bind_rows(ab2) %>%
          arrange(as.numeric(.data$pos))
        ab <- paste(ab$term, collapse=" ")
      } else {ab <- ""}
    } else {ab <- ""}

    list_df[[i]] <- tibble(id=id, TI=title, author=author, AB=ab, pubdata=pubdate,
                           relscore=relscore, SO=so, SO_ID=so_id, PU=publisher, IS=issn, URL=url,
                           first_page=first_page, last_page=last_page, volume=volume, issue=issue,
                           OA=oa, TC=TC, TCperYear=TCperYear, PY=PY, cited_by_url=cited_by_url,
                           ids=list(ids), DI=DI, DT=DT, CR=list(CR), related_works=list(related_works),
                           concept=concept)
  }
  df <- do.call(rbind,list_df)
}


# replace NULL with NA
simple_rapply <- function(x, fn)
{
  if(is.list(x))
  {
    lapply(x, simple_rapply, fn)
  } else
  {
    fn(x)
  }
}
