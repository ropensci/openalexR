# Adapted from Lionel's suggestion in
# https://github.com/r-lib/coro/issues/41

# Stateful environment for singletons
the <- new.env()

#' A generator for making request to OpenAlex API
#' Returns one record at a time.
#'
#' TODO check group_by
#' @param ... arguments passed to the generator including
#' `query_url`, `mailto`, `api_key`, and `verbose`.
#' See `oa_request` for details on these arguments.
#'
#' @examples
#' if (require("coro")) {
#'   query_url <- "https://api.openalex.org/works?filter=cites%3AW2755950973"
#'   oar <- oa_generate(query_url, verbose = TRUE)
#'   p1 <- oar() # record 1
#'   p2 <- oar() # record 2
#'   p3 <- oar() # record 3
#'   head(p1)
#'   head(p3)
#' }
#'
#' @export
#' @return Generator function.
oa_generate <- function(...) {
  if (!requireNamespace("coro", quietly = TRUE)) {
    stop(
      "Package \"coro\" must be installed to use this generator.",
      call. = FALSE
    )
  }

  if (is.null(the$oa_generate)) {
    the$oa_generate <- coro::generator(
      function(query_url,
               mailto = oa_email(),
               api_key = oa_apikey(),
               verbose = FALSE) {
        ua <- httr::user_agent("https://github.com/ropensci/openalexR/")
        query_ls <- list("per-page" = 200)
        if (!is.null(mailto)) {
          if (isValidEmail(mailto)) {
            query_ls[["mailto"]] <- mailto
          } else {
            message(mailto, " is not a valid email address")
          }
        }
        paging <- "cursor"
        query_ls[paging] <- "*"
        res <- api_request(query_url, ua, query_ls, api_key = api_key)
        if (is.null(res$meta)) {
          return(res)
        }

        if (grepl("group_by", query_url)) {
          result_name <- "group_by"
        } else {
          result_name <- "results"
        }
        n_items <- res$meta$count

        if (n_items <= 0) {
          warning("No records found!")
          return()
        }

        for (i in seq.int(n_items)) { # cursor pagination
          if (verbose) {
            message("Getting record ", i, " of ", n_items, " records...")
          }
          coro::yield(res[[result_name]][[(i - 1) %% 200 + 1]])
          if (i %% 200 == 0) {
            next_page <- get_next_page(paging, 0, res)
            query_ls[[paging]] <- next_page
            res <- api_request(query_url, ua, query = query_ls)
          }
        }
      }
    )
  }

  the$oa_generate(...)
}
