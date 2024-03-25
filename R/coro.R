# Adapted from Lionel's suggestion in
# https://github.com/r-lib/coro/issues/41

# Stateful environment for singletons
the <- new.env()

#' Iterating through records
#'
#' A generator for making request to OpenAlex API
#' Returns one record at a time.
#'
#' @param ... arguments passed to the generator including
#' `query_url`, `mailto`, `api_key`, and `verbose`.
#' See `oa_request` for details on these arguments.
#'
#' @examples
#' if (require("coro")) {
#'   # Example 1: basic usage getting one record at a time
#'   query_url <- "https://api.openalex.org/works?filter=cites%3AW1160808132"
#'   oar <- oa_generate(query_url, verbose = TRUE)
#'   p1 <- oar() # record 1
#'   p2 <- oar() # record 2
#'   p3 <- oar() # record 3
#'   head(p1)
#'   head(p3)
#'
#'   # Example 2: using `coro::loop()` to iterate through the generator
#'   query_url <- "https://api.openalex.org/works?filter=cited_by%3AW1847168837"
#'   oar <- oa_generate(query_url)
#'   coro::loop(for (x in oar) {
#'     print(x$id)
#'   })
#'
#'   # Example 3: save records in blocks of 100
#'   query_url <- "https://api.openalex.org/works?filter=cites%3AW1160808132"
#'   oar <- oa_generate(query_url)
#'   n <- 100
#'   recs <- vector("list", n)
#'   i <- 0
#'
#'   coro::loop(for (x in oar) {
#'     j <- i %% n + 1
#'     recs[[j]] <- x
#'     if (j == n) {
#'       # saveRDS(recs, sprintf("rec-%s.rds", i %/% n))
#'       recs <- vector("list", n) # reset recs
#'     }
#'     i <- i + 1
#'   })
#'   head(x)
#'   j
#'   # 398 works total, so j = 98 makes sense.
#'
#'   # You can also manually call the generator until exhausted
#'   # using `while (!coro::is_exhausted(record_i))`.
#'   # More details at https://coro.r-lib.org/articles/generator.html.
#'
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
        next_page <- "*"
        query_ls[[paging]] <- next_page
        res <- api_request(query_url, ua, query_ls, api_key = api_key)
        if (is.null(res$meta)) {
          return(res)
        }
        n_items <- res$meta$count

        if (n_items <= 0) {
          warning("No records found!")
          return()
        }

        i <- 0
        j <- 0

        if (grepl("group_by", query_url)) {
          result_name <- "group_by"
          mssg <- function(i) sprintf("Downloading group %s", i)
          groups_count <- res$meta$groups_count

          while (!is.null(next_page) && (j <= groups_count)) {
            i <- i + 1
            j <- (i - 1) %% 200 + 1
            if (verbose) message(mssg(i))
            coro::yield(res[[result_name]][[j]])
            if (j == 200) {
              next_page <- get_next_page(paging, 0, res)
              query_ls[[paging]] <- next_page
              res <- api_request(query_url, ua, query = query_ls)
              groups_count <- res$meta$groups_count
            }
          }
        } else {
          result_name <- "results"
          mssg <- function(i) sprintf("Getting record %s of %s records...", i, n_items)

          for (i in seq.int(n_items)){
            j <- (i - 1) %% 200 + 1
            if (verbose) message(mssg(i))
            coro::yield(res[[result_name]][[j]])
            if (j == 200) {
              next_page <- get_next_page(paging, 0, res)
              query_ls[[paging]] <- next_page
              res <- api_request(query_url, ua, query = query_ls)
            }
          }
        }
      }
    )
  }

  the$oa_generate(...)
}
