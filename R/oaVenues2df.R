utils::globalVariables("progress_bar")
#' Convert OpenAlex collection of venues' records from list format to data frame
#'
#' It converts bibliographic collection of venues' records gathered from OpenAlex database \href{https://openalex.org/}{https://openalex.org/} into data frame.
#' The function converts a list of venues' records obtained using \code{oa_request} into a data frame/tibble.
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
#' # Query to search information about the Journal of Informetrics (OA id:V205292342)
#'
#'
#' query_inst <- oa_query(
#'   identifier = "V205292342",
#'   entity = "venues"
#' )
#'
#' res <- oa_request(
#'   query_url = query_inst,
#'   count_only = FALSE,
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

  if (length(data) == 0 || is.null(data[[1]]$id)) {
    message("One list does not contain a valid OpenAlex collection")
    return()
  }

  n <- length(data)
  pb <- oa_progress(n)
  list_df <- vector(mode = "list", length = n)

  for (i in 1:n) {
    if (verbose) pb$tick()

    item <- data[[i]]

    sub_identical <- item[
      c("id", "display_name", "publisher", "works_count", "cited_by_count",
        "is_oa", "is_in_doaj", "homepage_url", "works_api_url")]

    sub_flat <- lapply(
      item[c("issn_l", "issn")],
      subs_na, type = "flat"
    )

    sub_id <- list(
      ids = subs_na(item$ids, type = "col_df"),
      relevance_score = item$relevance_score %||% NA
    )

    sub_rbind_dfs <- lapply(
      item[c("counts_by_year", "x_concepts")],
      subs_na, type = "rbind_df"
    )

    list_df[[i]] <- tibble::as_tibble(
      c(sub_identical, sub_flat, sub_id, sub_rbind_dfs))
  }

  col_order <- c(
    "id", "display_name", "publisher", "issn", "issn_l", "is_oa", "is_in_doaj",
    "ids", "homepage_url", "relevance_score", "works_count", "cited_by_count",
    "counts_by_year", "x_concepts", "works_api_url"
  )

  do.call(rbind, list_df)[, col_order]
}
