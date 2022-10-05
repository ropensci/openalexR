utils::globalVariables("progress_bar")
#' Convert OpenAlex collection of authors' records from list format to data frame
#'
#' It converts bibliographic collection of authors' records gathered from OpenAlex database \href{https://openalex.org/}{https://openalex.org/} into data frame.
#' The function converts a list of authors' records obtained using \code{oa_request} into a data frame/tibble.
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
#' # Query to search information about all authors affiliated to the University of Naples Federico II
#' # which have authored at least 100 publications:
#'
#' # University of Naples Federico II is associated to the OpenAlex id I71267560.
#'
#'
#' query_author <- oa_query(
#'   identifier = NULL,
#'   entity = "authors",
#'   last_known_institution.id = "I71267560",
#'   works_count = ">99"
#' )
#'
#' res <- oa_request(
#'   query_url = query_author,
#'   count_only = FALSE,
#'   verbose = FALSE
#' )
#'
#' df <- oa2df(res, entity = "authors")
#'
#' df
#' }
#'
#' # @export
oaAuthors2df <- function(data, verbose = TRUE) {
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

  inst_cols <- c("id", "display_name", "ror", "country_code", "type")
  empty_inst <- empty_list(inst_cols)

  for (i in 1:n) {
    if (verbose) pb$tick()

    item <- data[[i]]

    sub_identical <- item[
      c("id", "works_count", "display_name", "orcid",
        "works_api_url", "cited_by_count")]

    sub_id <- list(
      ids = subs_na(item$ids, type = "col_df"),
      relevance_score = item$relevance_score %||% NA
    )

    sub_flat <- lapply(
      item[c("display_name_alternatives")],
      subs_na, type = "flat"
    )
    sub_rbind_dfs <- lapply(
      item[c("counts_by_year", "x_concepts")],
      subs_na, type = "rbind_df"
    )

    sub_affiliation <- item$last_known_institution
    if (is.na(sub_affiliation[[1]])) {
      sub_affiliation <- empty_inst
    }
    sub_affiliation <- prepend(sub_affiliation, "affiliation")

    list_df[[i]] <- tibble::as_tibble(
      c(sub_identical, sub_id, sub_flat, sub_rbind_dfs, sub_affiliation))
  }

  col_order <- c(
    "id", "display_name", "display_name_alternatives", "relevance_score",
    "ids", "orcid", "works_count", "cited_by_count", "counts_by_year",
    "affiliation_display_name", "affiliation_id", "affiliation_ror",
    "affiliation_country_code", "affiliation_type", "x_concepts",
    "works_api_url"
  )

  do.call(rbind, list_df)[, col_order]
}
