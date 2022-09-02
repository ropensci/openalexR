utils::globalVariables("progress_bar")
#' Convert OpenAlex collection of institutions' records from list format to data frame
#'
#' It converts bibliographic collection of institutions' records gathered from OpenAlex database \href{https://openalex.org/}{https://openalex.org/} into data frame.
#' The function converts a list of institutions' records obtained using \code{oa_request} into a data frame/tibble.
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
#' query_inst <- oa_query(
#'   entity = "institutions",
#'   country_code = "it",
#'   type = "education"
#' )
#'
#' res <- oa_request(
#'   query_url = query_inst,
#'   count_only = FALSE,
#'   verbose = FALSE
#' )
#'
#' oa2df(res, entity = "institutions")
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
    if (verbose) pb$tick()

    item <- data[[i]]
    sub_identical <- item[
      c("id", "ror", "works_api_url", "type", "works_count",
        "display_name", "country_code", "homepage_url",
        "image_url", "image_thumbnail_url", "cited_by_count",
        "updated_date", "created_date")]

    sub_id <- list(
      ids = subs_na(item$ids, type = "col_df"),
      relevance_score = item$relevance_score %||% NA
    )

    sub_flat <- lapply(
      item[c("display_name_alternatives", "display_name_acronyms")],
      subs_na, type = "flat"
    )

    sub_row <- lapply(
      item[c("international", "geo", "associated_institutions")],
      subs_na, type = "row_df"
    )

    sub_rbind_dfs <- lapply(
      item[c("counts_by_year", "x_concepts")],
      subs_na, type = "rbind_df"
    )

    list_df[[i]] <- tibble::as_tibble(
      c(sub_flat, sub_row, sub_identical, sub_id, sub_rbind_dfs))
  }


  col_order <- c(
    "id", "display_name", "display_name_alternatives", "display_name_acronyms",
    "international", "ror", "ids", "country_code", "geo", "type",
    "homepage_url", "image_url", "image_thumbnail_url",
    "associated_institutions", "relevance_score", "works_count",
    "cited_by_count", "counts_by_year",
    "works_api_url", "x_concepts", "updated_date", "created_date"
  )

  do.call(rbind, list_df)[, col_order]
}
