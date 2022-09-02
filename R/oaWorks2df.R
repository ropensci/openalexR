utils::globalVariables("progress_bar")
#' Convert OpenAlex collection of works from list format to data frame
#'
#' It converts bibliographic collection of works gathered from OpenAlex database \href{https://openalex.org/}{https://openalex.org/} into data frame.
#' The function converts a list of works obtained using \code{oa_request} into a data frame/tibble.
#'
#' @param data is a list. data is the output of the function \code{oa_request}.
#' @param abstract Logical. If TRUE, the function returns also the abstract of each item. Default is \code{abstract=TRUE}.
#' @param verbose is a logical. If TRUE, information about the querying process will be plotted on screen. Default is \code{verbose=TRUE}.
#'
#' @return a data.frame.
#'
#' For more extensive information about OpenAlex API, please visit: \href{https://docs.openalex.org/api}{https://docs.openalex.org/api}
#'
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#'
#' # Query to search all works citing the article:
#' #  Aria, M., & Cuccurullo, C. (2017). bibliometrix:
#' #   An R-tool for comprehensive science mapping analysis.
#' #   Journal of informetrics, 11(4), 959-975.
#'
#' #  published in 2021.
#' #  The paper is associated to the OpenAlex id W2755950973.
#'
#' #  Results have to be sorted by relevance score in a descending order.
#'
#' query <- oa_query(
#'   identifier = NULL,
#'   entity = "works",
#'   cites = "W2755950973",
#'   from_publication_date = "2021-01-01",
#'   to_publication_date = "2021-12-31",
#'   search = NULL,
#'   endpoint = "https://api.openalex.org/"
#' )
#'
#' res <- oa_request(
#'   query_url = query,
#'   count_only = FALSE,
#'   verbose = FALSE
#' )
#'
#' df <- oa2df(res, entity = "works")
#'
#' df
#' }
#'
#' # @export

oaWorks2df <- function(data, abstract = TRUE, verbose = TRUE) {

  if (!is.null(data$id)) {
    data <- list(data)
  }

  if (is.null(data[[1]]$id)) {
    message("the list does not contain a valid OpenAlex collection")
    return()
  }

  n <- length(data)
  pb <- oa_progress(n)
  list_df <- vector(mode = "list", length = n)
  # biblio_cols <- c("volume", "issue", "first_page", "last_page")
  inst_cols <- c("id", "display_name", "ror", "country_code", "type")
  venue_cols <- c(
    so_id = "id", so = "display_name", publisher = "publisher",
    url = "url", is_oa = "is_oa"
  )

  for (i in 1:n) {
    if (verbose) pb$tick()

    paper <- data[[i]]
    paper <- simple_rapply(paper, `%||%`, y = NA)

    sub_identical <- paper[
      c(
        "id", "display_name", "publication_date", "doi", "type",
        "cited_by_count", "publication_year", "cited_by_api_url"
      )
    ]
    sub_biblio <- paper$biblio

    sub_flat <- lapply(
      paper[c("referenced_works", "related_works")],
      subs_na,
      type = "flat"
    )

    sub_id <- list(
      ids = subs_na(paper$ids, type = "col_df"),
      relevance_score = paper$relevance_score %||% NA
    )

    sub_rbind_dfs <- lapply(
      paper[c("counts_by_year", "concepts")],
      subs_na,
      type = "rbind_df"
    )

    sub_venue <- setNames(paper$host_venue[venue_cols], names(venue_cols))
    sub_venue$issn <- subs_na(paper$host_venue$issn, "flat")

    empty_inst <- empty_list(inst_cols)

    # authorships and affilitation
    author <- list(do.call(rbind.data.frame, lapply(paper$authorships, function(l) {
      l_inst <- l$institutions
      inst_idx <- lengths(l_inst) > 0
      if (length(inst_idx) > 0) {
        first_inst <- l_inst[inst_idx][[1]]
      } else {
        first_inst <- empty_inst
      }
      first_inst <- append_prefix(first_inst, "institution")
      aff_raw <- list(au_affiliation_raw = l$raw_affiliation_string[1])
      l_author <- append_prefix(l$author, "au")
      c(l_author, l["author_position"], aff_raw, first_inst)
    })))

    # Abstract
    if (!is.na(paper$abstract_inverted_index[1]) & isTRUE(abstract)) {
      ab <- abstract_build(paper$abstract_inverted_index)
    } else {
      ab <- ""
    }

    list_df[[i]] <- tibble::as_tibble(
      c(
        sub_identical, sub_flat, sub_id, sub_rbind_dfs, sub_venue, sub_biblio,
        list(author = author), list(ab = ab)
      )
    )
  }

  col_order <- c(
    "id", "display_name", "author", "ab", "publication_date", "relevance_score",
    "so", "so_id", "publisher", "issn", "url", "first_page", "last_page",
    "volume", "issue", "is_oa", "cited_by_count", "counts_by_year",
    "publication_year", "cited_by_api_url", "ids", "doi", "type",
    "referenced_works", "related_works", "concepts"
  )

  do.call(rbind.data.frame, list_df)[, col_order]
}

abstract_build <- function(ab) {
  w <- rep(names(ab), lengths(ab))
  ind <- unlist(ab)
  if (is.null(ind)){
    ab <- ""
  } else {
    ab <- paste(w[order(ind)], collapse = " ", sep = "")
  }
}
