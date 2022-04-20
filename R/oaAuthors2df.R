utils::globalVariables("progress_bar")
#' Convert OpenAlex collection of authors' records from list format to data frame
#'
#' It converts bibliographic collection of authors' records gathered from OpenAlex database \href{https://openalex.org/}{https://openalex.org/} into data frame.
#' The function converts a list of authors' records obtained using \code{oaApiRequest} into a data frame/tibble.
#'
#' @param data is a list. data is the output of the function \code{oaApiRequest}.
#' @param verbose is a logical. If TRUE, information about the querying process will be plotted on screen. Default is \code{verbose=TRUE}.
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
#' # Query to search information about all authors affiliated to the University of Naples Federico II
#' # which have authored at least 100 publications:
#'
#' # University of Naples Federico II is associated to the OpenAlex id I71267560.
#'
#'
#' query_author <- oaQueryBuild(
#'  identifier = NULL,
#'  entity = "authors",
#'  filter = "last_known_institution.id:I71267560,works_count:>99")
#'
#' res <- oaApiRequest(
#'    query_url = query_author,
#'    total.count = FALSE,
#'    verbose = FALSE
#'    )
#'
#' df <- oa2df(res, entity = "authors")
#'
#' df
#'
#' }
#'
# @export
oaAuthors2df <- function(data, verbose = TRUE){

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
    if (isTRUE(verbose)) pb$tick()

    item <- data[[i]]

    id <- item$id
    name <- item$display_name
    if (length(item$display_name_alternatives)>0){
      name_alternatives <- list(unlist(item$display_name_alternatives))
    }else{
      name_alternatives <- NA
    }
    #
    if (length(item$ids)>0){
      ids <- unlist(item$ids)
      ids <- list(data.frame(item=names(ids), value=ids))
    }else{
      ids <- NA
    }

    rel_score <- item$relevance_score
    orcid <- item$orcid
    works_count <- item$works_count
    TC <- item$cited_by_count
    if (!is.na(item$last_known_institution[[1]])){
      affiliation_id <- item$last_known_institution$id
      affiliation_ror <- item$last_known_institution$ror
      affiliation_name <- item$last_known_institution$display_name
      affiliation_country <- item$last_known_institution$country_code
      affiliation_type <- item$last_known_institution$type
    }else{
      affiliation_id <- NA
      affiliation_ror <- NA
      affiliation_name <- NA
      affiliation_country <- NA
      affiliation_type <- NA
    }


    # Total Citations per Year
    TCperYear <- unlist(item$counts_by_year)
    lab <- names(TCperYear)
    TCperYear <- list(data.frame(year=TCperYear[lab=="year"], works_count=TCperYear[lab=="works_count"],
                                 TC=TCperYear[lab=="cited_by_count"]))
    # concepts
    concept <- list(do.call(rbind,lapply(item$x_concepts, function(l){
      L <- data.frame(
        concept_id=l$id,
        concept_name=l$display_name,
        concept_score=l$score,
        concept_lecel=l$level,
        concept_url=l$wikidata
      )
    })))
    works_api_url <- item$works_api_url

    list_df[[i]] <- tibble(id=id, name=name, name_alternatives=name_alternatives, rel_score=rel_score, ids=ids,
                           orcid=orcid, works_count=works_count, TC=TC, TCperYear=TCperYear, affiliation_name=affiliation_name,
                           affiliation_id=affiliation_id, affiliation_ror=affiliation_ror,
                           affiliation_country=affiliation_country, affiliation_type=affiliation_type,
                           concept=concept, works_api_url=works_api_url)
  }
  df <- do.call(rbind,list_df)
}

