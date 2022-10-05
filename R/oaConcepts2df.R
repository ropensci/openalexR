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
#'   entity = "concepts",
#'   display_name.search = "electrodynamics"
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
  data <- simple_rapply(data, `%||%`, y = NA)

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
      c("id", "display_name", "wikidata", "level", "description",
        "image_url", "image_thumbnail_url",  "works_count", "cited_by_count",
        "works_api_url")]

    sub_id <- list(
      ids = subs_na(item$ids, type = "col_df"),
      relevance_score = item$relevance_score %||% NA
    )

    sub_rbind_dfs <- lapply(
      item[c("counts_by_year", "ancestors", "related_concepts")],
      subs_na, type = "rbind_df"
    )

    sub_row <- lapply(
      item$international[c("display_name", "description")],
      subs_na, type = "row_df"
    )
    names(sub_row) <- paste(names(sub_row), "international", sep = "_")

    list_df[[i]] <- tibble::as_tibble(
      c(sub_identical, sub_id, sub_rbind_dfs, sub_row)
    )
  }

  col_order <- c(
    "id", "display_name", "display_name_international", "description",
    "description_international", "wikidata", "level", "ids",
    "image_url", "image_thumbnail_url", "ancestors",
    "related_concepts", "relevance_score", "works_count",
    "cited_by_count", "counts_by_year",
    "works_api_url"
  )

  do.call(rbind, list_df)[, col_order]
}
