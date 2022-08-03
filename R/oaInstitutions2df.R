utils::globalVariables("progress_bar")
#' Convert OpenAlex collection of institutions' records from list format to data frame
#'
#' It converts bibliographic collection of institutions' records gathered from OpenAlex database \href{https://openalex.org/}{https://openalex.org/} into data frame.
#' The function converts a list of institutions' records obtained using \code{oaApiRequest} into a data frame/tibble.
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
#' \dontrun{
#'
#' # Query to search information about all Italian educational institutions
#'
#'
#' query_inst <- oaQueryBuild(
#'   entity = "institutions",
#'   filter = "country_code:it,type:education"
#' )
#'
#' res <- oaApiRequest(
#'   query_url = query_inst,
#'   total.count = FALSE,
#'   verbose = FALSE
#' )
#'
#' df <- oa2df(res, entity = "institutions")
#'
#' df
#' }
#'
#' # @export
oaInstitutions2df <- function(data, verbose = TRUE) {

  # replace NULL with NA
  data <- simple_rapply(data, `%||%`, y = NA)

  if (!is.null(data$id)) {
    data <- list(data)
  }

  if (is.null(data[[1]]$id)) {
    message("the list does not contain a valid OpenAlex collection")
    return()
  }

  n <- length(data)
  list_df <- vector(mode = "list", length = n)

  pb <- progress::progress_bar$new(
    format = "  converting [:bar] :percent eta: :eta",
    total = n, clear = FALSE, width = 60
  )

  for (i in 1:n) {
    if (isTRUE(verbose)) pb$tick()
    # print(i)
    item <- data[[i]]

    sub_unlist <- `names<-`(
      tibble::as_tibble(lapply(
        item[c("display_name_alternatives", "display_name_acronyms")],
        subs_na,
        type = "flat"
      )),
      c("name_alternatives", "name_acronyms")
    )

    sub_df <- `names<-`(
      tibble::as_tibble(lapply(
        item[c("international", "geo", "associated_institutions")],
        subs_na,
        type = "row_df"
      )),
      c("name_international", "geo", "associated_inst")
    )
    sub_identical <- tibble::as_tibble(
      item[c("id", "ror", "works_api_url", "type", "works_count")]
    )
    ids <- subs_na(item$ids, type = "col_df") # TODO is tibble ok? (no rownames)
      
    # TODO changing the name is not very robust here
    # TODO check rel_score
    rel_score <- item$relevance_score
    sub_modified <- `names<-`(
      tibble::as_tibble(item[c(
        "display_name", "country_code", "homepage_url",
        "image_url", "image_thumbnail_url", "cited_by_count"
      )]),
      c("name", "country", "homepage", "image", "thumbnail", "TC")
    )

    # Total Citations per Year
    # TODO
    # Do we need to change these names?
    # there was a typo here earlier: lecel should be level
    c_tcs <- do.call(rbind.data.frame, item$counts_by_year)
    names(c_tcs)[names(c_tcs) == "cited_by_count"] <- "TC"
    TCperYear <- list(c_tcs)

    if (length(item$x_concept) == 0){
      concept <- NA
    } else {
      c_concepts <- do.call(rbind.data.frame, item$x_concepts)
      c_concepts <- c_concepts[, c("id", "display_name", "score", "level", "wikidata")]
      names(c_concepts) <- c("concept_id", "concept_name", "concept_score", "concept_level", "concept_url")
      concept <- list(c_concepts)
    }

    # list_df_old <- tibble::tibble(
    #   id = id, name = name, name_alternatives = name_alternatives, name_acronyms = name_acronyms,
    #   name_international = name_international, ror = ror, ids = ids, country = country, geo = geo,
    #   type = type, homepage = homepage, image = image, thumbnail = thumbnail, associated_inst = associated_inst,
    #   rel_score = rel_score, works_count = works_count, TC = TC, TCperYear = TCperYear,
    #   concept = concept, works_api_url = works_api_url
    # )
    item_organized <- tibble::tibble(
      sub_unlist, sub_df, sub_identical, sub_modified, ids = ids,
      rel_score = rel_score, TCperYear = TCperYear, concept = concept
    )

    col_order <- c(
      "id", "name", "name_alternatives", "name_acronyms", "name_international",
      "ror", "ids", "country", "geo", "type", "homepage", "image", "thumbnail",
      "associated_inst", # "rel_score", 
      "works_count", "TC", "TCperYear", "concept",
      "works_api_url"
    )
    list_df[[i]] <- item_organized[, col_order]
  }
  df <- do.call(rbind, list_df)
}
