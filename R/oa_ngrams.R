#' Convert a "Work" entity's ngram data from list to data frame
#'
#' @keywords internal
ngram2df <- function(ngram) {
  ngram_df <- as.data.frame(rev(ngram$meta))
  names(ngram_df)[1] <- "id"
  ngrams_list <- if (length(ngram$ngrams) == 0) {
    NULL
  } else {
    do.call(rbind.data.frame, ngram$ngrams)
  }
  ngram_df$ngrams <- list(ngrams_list)
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
#' }
oa_ngrams <- function(works_identifier, mailto = oa_email(), verbose = FALSE) {
  query_urls <- paste0("https://api.openalex.org/works/", gsub("^https://openalex.org/", "", works_identifier), "/ngrams")

  ua <- httr::user_agent(cfg()$user_agent)

  query_ls <- list()
  if (!is.null(mailto)) {
    if (isValidEmail(mailto)) {
      query_ls[["mailto"]] <- mailto
    } else {
      message(mailto, " is not a valid email address")
    }
  }

  n <- length(query_urls)
  pb <- oa_progress(n, "OpenAlex downloading")

  final_res <- vector("list", n)
  for (i in seq_along(query_urls)) {
    if (verbose) pb$tick()
    res <- api_request(query_urls[i], ua, query = query_ls)
    final_res[[i]] <- ngram2df(res)
  }
  final_res

  tibble::as_tibble(do.call(rbind.data.frame, final_res))
}
