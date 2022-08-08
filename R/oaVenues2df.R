utils::globalVariables("progress_bar")
#' Convert OpenAlex collection of venues' records from list format to data frame
#'
#' It converts bibliographic collection of venues' records gathered from OpenAlex database \href{https://openalex.org/}{https://openalex.org/} into data frame.
#' The function converts a list of venues' records obtained using \code{oaApiRequest} into a data frame/tibble.
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
#' # Query to search information about the Journal of Informetrics (OA id:V205292342)
#'
#'
#' query_inst <- oaQueryBuild(
#'   identifier = "V205292342",
#'   entity = "venues",
#' )
#'
#' res <- oaApiRequest(
#'   query_url = query_inst,
#'   total.count = FALSE,
#'   verbose = FALSE
#' )
#'
#' df <- oa2df(res, entity = "venues")
#'
#' df
#' }
#'
#' # @export
oaVenues2df <- function(data, verbose = TRUE) {

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
    publisher <- item$publisher
    issn_l <- list(unlist(item$issn_l))
    issn <- list(unlist(item$issn))
    rel_score <- item$relevance_score
    works_count <- item$works_count
    TC <- item$cited_by_count
    is_oa <- item$is_oa
    is_in_doaj <- item$is_in_doaj
    homepage <- item$homepage_url
    if (length(item$ids) > 0) {
      ids <- unlist(item$ids)
      ids <- list(data.frame(item = names(ids), value = ids))
    } else {
      ids <- NA
    }

    # Total Citations per Year
    TCperYear <- unlist(item$counts_by_year)
    lab <- names(TCperYear)
    TCperYear <- list(data.frame(
      year = TCperYear[lab == "year"], works_count = TCperYear[lab == "works_count"],
      TC = TCperYear[lab == "cited_by_count"]
    ))

    # concepts
    concept <- list(do.call(rbind, lapply(item$x_concepts, function(l) {
      L <- data.frame(
        concept_id = l$id,
        concept_name = l$display_name,
        concept_score = l$score,
        concept_lecel = l$level,
        concept_url = l$wikidata
      )
    })))
    works_api_url <- item$works_api_url

    list_df[[i]] <- tibble::tibble(
      id = id, name = name, publisher = publisher, issn = issn, issn_l = issn_l, is_oa = is_oa, is_in_doaj = is_in_doaj,
      ids = ids, homepage = homepage, rel_score = rel_score, works_count = works_count, TC = TC, TCperYear = TCperYear,
      concept = concept, works_api_url = works_api_url
    )
  }
  df <- do.call(rbind, list_df)
}
