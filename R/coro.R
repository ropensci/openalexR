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
#'   query_url <- "https://api.openalex.org/works?filter=cites%3AW1160808132"
#'   oar <- oa_generate(query_url, verbose = TRUE)
#'   p1 <- oar() # record 1
#'   p2 <- oar() # record 2
#'   p3 <- oar() # record 3
#'   head(p1)
#'   head(p3)
#'
#'   # You can also call the generate function until exhausted.
#'   # More details at https://coro.r-lib.org/articles/generator.html.
#'
#'   oar <- oa_generate(query_url)
#'   record_i <- NULL
#'   j <- 0
#'   while (!(is.symbol(record_i) && record_i == ".__exhausted__.")) {
#'     j <- j + 1
#'     recs_100 <- vector("list", 100)
#'     for (i in seq.int(100)) {
#'       record_i <- oar()
#'       recs_100[[i]] <- record_i
#'     }
#'     # saveRDS(recs_100, sprintf("rec-%s.rds", j))
#'   }
#'   record_i
#'   j
#'   # 398 works total, so j = 4 makes sense.
#'   # the last two elements of the latest recs_100 is .__exhausted__.
#'   str(tail(recs_100), max.level = 1)
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
