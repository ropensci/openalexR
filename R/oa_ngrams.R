#' Convert a "Work" entity's ngram data from list to data frame
#'
#' @keywords internal
ngram2df <- function(ngram) {
  if (is.null(ngram$meta$doi)) ngram$meta$doi <- NA_character_
  ngram_df <- as.data.frame.list(rev(ngram$meta), col.names = c("id", "doi", "count"))
  ngram_df$ngrams <- if (length(ngram$ngrams) == 0) {
    list(NULL)
  } else {
    list(ngram$ngrams)
  }
  ngram_df
}

#' Get N-grams of works
#'
#' Some work entities in OpenAlex include N-grams (word sequences and their frequencies) of their full text.
#' The N-grams are obtained from Internet Archive, which uses the spaCy parser to index scholarly works.
#' See <https://docs.openalex.org/api-entities/works/get-n-grams> for coverage and more technical details.
#'
#' @note A faster implementation is available for `curl` >= v5.0.0, and `oa_ngrams` will issue a one-time message
#' about this. This can be suppressed with `options("oa_ngrams.message.curlv5" = FALSE)`.
#'
#' @param works_identifier Character. OpenAlex ID(s) of "works" entities as item identifier(s).
#' These IDs start with "W". See more at <https://docs.openalex.org/api-entities/works#id>.
#' @param ... Unused.
#' @inheritParams oa_query
#'
#' @return A dataframe of paper metadatada and a list-column of ngrams.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ngrams_data <- oa_ngrams(c("W1963991285", "W1964141474"))
#'
#' # 10 most common ngrams in the first work
#' first_paper_ngrams <- ngrams_data$ngrams[[1]]
#' first_paper_ngrams[
#'   order(first_paper_ngrams$ngram_count, decreasing = TRUE),
#' ][
#'   1:10,
#' ]
#'
#' # Missing N-grams are `NULL` in the `ngrams` list-column
#' oa_ngrams("https://openalex.org/W2284876136")
#' }
oa_ngrams <- function(works_identifier, ...,
                      endpoint = "https://api.openalex.org",
                      verbose = FALSE) {

  ngrams_failed_template <- data.frame(id = NA, doi = NA, count = NA, ngrams = I(list(NULL)))

  out <- tryCatch(
    {
      # Check if input is ID of Works entity
      normalized_id <- shorten_oaid(works_identifier)
      if (!all(grepl("^W", normalized_id))) {
        stop(
          "Invalid OpenAlex Work entity ID(s) at `works_identifier`: ",
          paste(which(!grepl("^W", normalized_id)), sep = ", ")
        )
      }

      # Setup for querying
      query_urls <- paste(endpoint, "works", normalized_id, "ngrams", sep = "/")
      n <- length(query_urls)

      #ngrams_failed_template <- data.frame(id = NA, doi = NA, count = NA, ngrams = I(list(NULL)))
      if (verbose) {
        pb <- oa_progress(n)
      }

      # Fetch
      if (utils::packageVersion("curl") >= "5") {
        # Parallel fetch
        ngrams_files <- asNamespace("curl")$multi_download(query_urls, file.path(tempdir(), normalized_id), progress = verbose)
        ngrams_success <- ifelse(ngrams_files$success, ngrams_files$destfile, NA)
        # Convert
        ngrams_dfs <- lapply(ngrams_success, function(x) {
          if (verbose) pb$tick()
          if (is.na(x)) {
            ngrams_failed_template
          } else {
            ngram2df(jsonlite::fromJSON(x))
          }
        })
        file.remove(ngrams_files$destfile)
      } else {
        # One-time message
        if (getOption("oa_ngrams.message.curlv5", TRUE)) {
          message("Use `{curl}` >= v5.0.0 for a faster implementation of `oa_ngrams`")
          options("oa_ngrams.message.curlv5" = FALSE)
        }
        # Serial fetch
        if (verbose) {
          pb_dl <- oa_progress(n, "OpenAlex downloading")
        }

        ngrams_list <- vector("list", n)
        for (i in seq_len(n)) {
          if (verbose) pb_dl$tick()
          ngrams_list[[i]] <- tryCatch(
            jsonlite::fromJSON(query_urls[i]),
            error = function(...) ngrams_failed_template
          )
        }
        # Convert
        ngrams_dfs <- vector("list", n)
        for (i in seq_len(n)) {
          if (verbose) pb$tick()
          ngrams_dfs[[i]] <- ngram2df(ngrams_list[[i]])
        }
      }

      tibble::as_tibble(do.call(rbind.data.frame, ngrams_dfs))
    },
    error=function(cond) {
      message("ngrams not available for this work")
     # message(cond)
      # Choose a return value in case of error
      return(ngrams_failed_template)
    }
  )
  return(out)
}
