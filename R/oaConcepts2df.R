utils::globalVariables("progress_bar")
#' Convert OpenAlex collection of concepts' records from list format to data frame
#'
#' It converts bibliographic collection of concepts' records gathered from OpenAlex database \href{https://openalex.org/}{https://openalex.org/} into data frame.
#' The function converts a list of concepts' records obtained using \code{oa_request} into a data frame/tibble.
#'
#' @param data is a list. data is the output of the function \code{oa_request}.
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
#' query_inst <- oa_query(
#'   entity = "institutions",
#'   filter = "country_code:it,type:education"
#' )
#'
#' res <- oa_request(
#'   query_url = query_inst,
#'   count_only = FALSE,
#'   verbose = FALSE
#' )
#'
#' df <- oa2df(res, entity = "concepts")
#'
#' df
#' }
#'
#' # @export
oaConcepts2df <- function(data, verbose = TRUE) {

  # replace NULL with NA
  data <- simple_rapply(data, function(x) if (is.null(x)) NA else x)

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

    item <- data[[i]]

    id <- item$id
    name <- item$display_name
    if (length(item$international) > 0) {
      name_international <- list(as.data.frame(item$international$display_name))
    } else {
      name_international <- NA
    }
    wikidata <- item$wikidata
    rel_score <- item$relevance_score
    level <- item$level
    description <- item$description
    if (length(item$international) > 0) {
      description_international <- list(as.data.frame(item$international$description))
    } else {
      description_international <- NA
    }
    works_count <- item$works_count
    TC <- item$cited_by_count
    if (length(item$ids) > 0) {
      ids <- unlist(item$ids)
      ids <- list(data.frame(item = names(ids), value = ids))
    } else {
      ids <- NA
    }
    image <- item$image_url
    thumbnail <- item$image_thumbnail_url
    if (length(item$ancestors) > 0) {
      ancestors <- unlist(item$ancestors)
      lab <- names(ancestors)
      ancestors <- list(data.frame(
        id = ancestors[lab == "id"], name = ancestors[lab == "display_name"],
        level = ancestors[lab == "level"], wikidata = ancestors[lab == "wikidata"]
      ))
    } else {
      ancestors <- NA
    }
    if (length(item$related_concepts) > 0) {
      rel_concepts <- unlist(item$related_concepts)
      lab <- names(rel_concepts)
      rel_concepts <- list(data.frame(
        id = rel_concepts[lab == "id"], name = rel_concepts[lab == "display_name"],
        level = rel_concepts[lab == "level"], wikidata = rel_concepts[lab == "wikidata"],
        score = rel_concepts[lab == "score"]
      ))
    } else {
      ancestors <- NA
    }

    ## Total Citation per Year
    TCperYear <- unlist(item$counts_by_year)
    lab <- names(TCperYear)
    TCperYear <- list(data.frame(
      year = TCperYear[lab == "year"], works_count = TCperYear[lab == "works_count"],
      TC = TCperYear[lab == "cited_by_count"]
    ))

    works_api_url <- item$works_api_url

    list_df[[i]] <- tibble::tibble(
      id = id, name = name, name_international = name_international, description = description,
      description_international = description_international, wikidata = wikidata, level = level,
      ids = ids, image = image, thumbnail = thumbnail,
      ancestors = ancestors, rel_concepts = rel_concepts,
      rel_score = rel_score, works_count = works_count, TC = TC, TCperYear = TCperYear,
      works_api_url = works_api_url
    )
  }
  df <- do.call(rbind, list_df)
}
