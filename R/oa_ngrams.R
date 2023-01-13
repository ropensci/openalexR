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
#' See <https://docs.openalex.org/api/get-n-grams> for coverage and more technical details.
#'
#' @param works_identifier Character. OpenAlex ID(s) of "works" entities as item identifier(s).
#' These IDs normally start with "W". See more at <https://docs.openalex.org/about-the-data/work#id>.
#' @param ... Unused.
#' @inheritParams oa_request
#'
#' @return A dataframe of paper metadatada and a list-column of ngrams.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ngrams_data <- oa_ngrams(c("W1963991285", "W1964141474"))
#'
#' library(dplyr)
#' first_paper_ngrams <- ngrams_data$ngrams[[1]]
#' top_10_ngrams <- first_paper_ngrams %>%
#'   slice_max(ngram_count, n = 10, with_ties = FALSE)
#'
#' # Missing ngrams are `NULL`
#' oa_ngrams("https://openalex.org/W2284876136")
#'
#' }
oa_ngrams <- function(works_identifier, ..., verbose = FALSE) {
  query_urls <- paste0("https://api.openalex.org/works/", gsub("^https://openalex.org/", "", works_identifier), "/ngrams")

  n <- length(query_urls)
  pb <- oa_progress(n, "OpenAlex downloading")

  final_res <- vector("list", n)
  for (i in seq_along(query_urls)) {
    if (verbose) pb$tick()
    res <- jsonlite::fromJSON(query_urls[i])
    final_res[[i]] <- ngram2df(res)
  }
  final_res

  tibble::as_tibble(do.call(rbind.data.frame, final_res))
}

#' @rdname oa_ngrams
oa_ngrams2 <- function(works_identifier, ..., verbose = FALSE) {

  if (utils::packageVersion("curl") < 5) {
    stop("`oa_ngrams2()` requires `{curl}` >= 5.0.0")
  }

  normalized_id <- gsub("^https://openalex.org/", "", works_identifier)
  if (!all(grepl("^W", normalized_id))) {
    stop("Invalid Work entity ID(s) at `works_identifier`: ", paste(which(!grepl("^W", normalized_id)), sep = ", "))
  }

  query_urls <- paste0("https://api.openalex.org/works/", normalized_id, "/ngrams")
  ngrams_files <- curl::multi_download(query_urls, file.path(tempdir(), works_identifier), progress = verbose)
  ngrams_success <- ifelse(ngrams_files$success, ngrams_files[ngrams_files$success, "destfile", drop = TRUE], NA)

  ngrams_failed_template <- data.frame(id = NA, doi = NA, count = NA, ngrams = I(list(NULL)))
  ngrams_list <- lapply(ngrams_success, function(x) {
    if (is.na(x)) {
      ngrams_failed_template
    } else {
      res <- jsonlite::fromJSON(x)
      ngram2df(res)
    }
  })

  file.remove(ngrams_files$destfile)

  tibble::as_tibble(do.call(rbind.data.frame, ngrams_list))
}
