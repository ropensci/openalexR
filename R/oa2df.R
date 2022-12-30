#' Convert OpenAlex collection from list to data frame
#'
#' It converts bibliographic collections gathered from OpenAlex database \href{https://openalex.org/}{https://openalex.org/} into data frame.
#' The function converts a collection of records about works, authors, institutions, venues or concepts obtained using \code{oa_request} into a data frame/tibble.
#'
#' @param data List. Output of \code{oa_request}.
#' @param entity Character. Scholarly entity of the search.
#' The argument can be one of c("works", "authors", "venues", "institutions", "concepts").
#' @param abstract Logical. If TRUE, the function returns also the abstract of each item.
#' Ignored if entity is different from "works". Defaults to TRUE.
#' @inheritParams oa_query
#' @inheritParams oa_request
#' @return A tibble/dataframe result of the original OpenAlex result list.
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
#'   entity = "works",
#'   cites = "W2755950973",
#'   from_publication_date = "2021-01-01",
#'   to_publication_date = "2021-04-30"
#' )
#'
#' res <- oa_request(
#'   query_url = query,
#'   count_only = FALSE,
#'   verbose = FALSE
#' )
#'
#' oa2df(res, entity = "works")
#' }
#'
#' @export
oa2df <- function(data, entity, abstract = TRUE, count_only = FALSE, group_by = NULL, verbose = TRUE) {
  if (!is.null(group_by)) {
    return(do.call(rbind.data.frame, data))
  }

  if (count_only && length(data) == 4) {
    return(unlist(data))
  }

  switch(entity,
    works = works2df(data, abstract, verbose),
    authors = authors2df(data, verbose),
    institutions = institutions2df(data, verbose),
    venues = venues2df(data, verbose),
    concepts = concepts2df(data, verbose),
    snowball = snowball2df(data)
  )
}


#' Convert OpenAlex collection of works from list format to data frame
#'
#' It converts bibliographic collection of works gathered from OpenAlex database \href{https://openalex.org/}{https://openalex.org/} into data frame.
#' The function converts a list of works obtained using \code{oa_request} into a data frame/tibble.
#'
#' @param abstract Logical. If TRUE, the function returns also the abstract of each item.
#' Defaults to TRUE.
#' @inheritParams oa2df
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
#'
works2df <- function(data, abstract = TRUE, verbose = TRUE) {
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
  # biblio_cols <- c("volume", "issue", "first_page", "last_page")
  inst_cols <- c("id", "display_name", "ror", "country_code", "type")
  venue_cols <- c(
    so_id = "id", so = "display_name", publisher = "publisher",
    url = "url", is_oa = "is_oa"
  )
  empty_inst <- empty_list(inst_cols)

  for (i in 1:n) {
    if (verbose) pb$tick()

    paper <- data[[i]]
    paper <- simple_rapply(paper, `%||%`, y = NA)

    sub_identical <- paper[
      c(
        "id", "display_name", "publication_date", "doi", "type",
        "cited_by_count", "publication_year", "cited_by_api_url",
        "is_paratext", "is_retracted"
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

    # Type conversions for concepts df
    sub_rbind_dfs$concepts <- lapply(sub_rbind_dfs$concepts, function(x) {
      if (is.atomic(x) && is.na(x)) {
        NULL
      } else if (!is.null(x$score)) {
        transform(x, score = as.double(score))
      } else {
        x
      }
    })

    sub_venue <- setNames(paper$host_venue[venue_cols], names(venue_cols))
    sub_venue$issn <- subs_na(paper$host_venue$issn, "flat")

    # authorships and affilitation
    author <- list(do.call(rbind.data.frame, lapply(paper$authorships, function(l) {
      l_inst <- l$institutions
      inst_idx <- lengths(l_inst) > 0
      if (length(inst_idx) > 0 && any(inst_idx)) {
        first_inst <- l_inst[inst_idx][[1]]
      } else {
        first_inst <- empty_inst
      }
      first_inst <- prepend(first_inst, "institution")
      aff_raw <- list(au_affiliation_raw = l$raw_affiliation_string[1])
      l_author <- l_author <- if (length(l$author) > 0) {
        prepend(l$author, "au")
      } else {
        empty_list(c("au_id", "au_display_name", "au_orcid"))
      }

      c(l_author, l["author_position"], aff_raw, first_inst)
    })))

    # Abstract
    if (abstract && !is.na(paper$abstract_inverted_index[1])) {
      ab <- abstract_build(paper$abstract_inverted_index)
    } else {
      ab <- NA
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
    "referenced_works", "related_works", "is_paratext", "is_retracted", "concepts"
  )

  do.call(rbind.data.frame, list_df)[, col_order]
}

abstract_build <- function(ab) {
  w <- rep(names(ab), lengths(ab))
  ind <- unlist(ab)
  if (is.null(ind)) {
    return("")
  }

  paste(w[order(ind)], collapse = " ", sep = "")
}


#' Convert OpenAlex collection of authors' records from list format to data frame
#'
#' It converts bibliographic collection of authors' records gathered from OpenAlex database \href{https://openalex.org/}{https://openalex.org/} into data frame.
#' The function converts a list of authors' records obtained using \code{oa_request} into a data frame/tibble.
#'
#' @inheritParams oa2df
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
authors2df <- function(data, verbose = TRUE) {
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
      c(
        "id", "works_count", "display_name", "orcid",
        "works_api_url", "cited_by_count"
      )
    ]

    sub_id <- list(
      ids = subs_na(item$ids, type = "col_df"),
      relevance_score = item$relevance_score %||% NA
    )

    sub_flat <- lapply(
      item[c("display_name_alternatives")],
      subs_na,
      type = "flat"
    )
    sub_rbind_dfs <- lapply(
      item[c("counts_by_year", "x_concepts")],
      subs_na,
      type = "rbind_df"
    )

    sub_affiliation <- item$last_known_institution
    if (is.na(sub_affiliation[[1]])) {
      sub_affiliation <- empty_inst
    }
    sub_affiliation <- prepend(sub_affiliation, "affiliation")

    list_df[[i]] <- tibble::as_tibble(
      c(sub_identical, sub_id, sub_flat, sub_rbind_dfs, sub_affiliation)
    )
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


#' Convert OpenAlex collection of institutions' records from list format to data frame
#'
#' It converts bibliographic collection of institutions' records gathered from OpenAlex database \href{https://openalex.org/}{https://openalex.org/} into data frame.
#' The function converts a list of institutions' records obtained using \code{oa_request} into a data frame/tibble.
#'
#' @inheritParams oa2df
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
institutions2df <- function(data, verbose = TRUE) {

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
      c(
        "id", "ror", "works_api_url", "type", "works_count",
        "display_name", "country_code", "homepage_url",
        "image_url", "image_thumbnail_url", "cited_by_count",
        "updated_date", "created_date"
      )
    ]

    sub_id <- list(
      ids = subs_na(item$ids, type = "col_df"),
      relevance_score = item$relevance_score %||% NA
    )

    sub_flat <- lapply(
      item[c("display_name_alternatives", "display_name_acronyms")],
      subs_na,
      type = "flat"
    )

    sub_row <- lapply(
      item[c("international", "geo")],
      subs_na,
      type = "row_df"
    )

    sub_rbind_dfs <- lapply(
      item[c("counts_by_year", "x_concepts", "associated_institutions")],
      subs_na,
      type = "rbind_df"
    )

    list_df[[i]] <- tibble::as_tibble(
      c(sub_flat, sub_row, sub_identical, sub_id, sub_rbind_dfs)
    )
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


#' Convert OpenAlex collection of venues' records from list format to data frame
#'
#' It converts bibliographic collection of venues' records gathered from OpenAlex database \href{https://openalex.org/}{https://openalex.org/} into data frame.
#' The function converts a list of venues' records obtained using \code{oa_request} into a data frame/tibble.
#'
#' @inheritParams oa2df
#'
#' @return a data.frame.
#'
#' For more extensive information about OpenAlex API, please visit: \href{https://docs.openalex.org/api}{https://docs.openalex.org/api}
#'
#'
#' @examples
#' \dontrun{
#'
#' # Query to search information about the Journal of Informetrics (OA id:V205292342)
#'
#'
#' query_inst <- oa_query(
#'   identifier = "V205292342",
#'   entity = "venues"
#' )
#'
#' res <- oa_request(
#'   query_url = query_inst,
#'   count_only = FALSE,
#'   verbose = FALSE
#' )
#'
#' df <- oa2df(res, entity = "venues")
#'
#' df
#' }
#'
#' # @export
venues2df <- function(data, verbose = TRUE) {

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

  for (i in 1:n) {
    if (verbose) pb$tick()

    item <- data[[i]]

    sub_identical <- item[
      c(
        "id", "display_name", "publisher", "works_count", "cited_by_count",
        "is_oa", "is_in_doaj", "homepage_url", "works_api_url"
      )
    ]

    sub_flat <- lapply(
      item[c("issn_l", "issn")],
      subs_na,
      type = "flat"
    )

    sub_id <- list(
      ids = subs_na(item$ids, type = "col_df"),
      relevance_score = item$relevance_score %||% NA
    )

    sub_rbind_dfs <- lapply(
      item[c("counts_by_year", "x_concepts")],
      subs_na,
      type = "rbind_df"
    )

    list_df[[i]] <- tibble::as_tibble(
      c(sub_identical, sub_flat, sub_id, sub_rbind_dfs)
    )
  }

  col_order <- c(
    "id", "display_name", "publisher", "issn", "issn_l", "is_oa", "is_in_doaj",
    "ids", "homepage_url", "relevance_score", "works_count", "cited_by_count",
    "counts_by_year", "x_concepts", "works_api_url"
  )

  do.call(rbind, list_df)[, col_order]
}


#' Convert OpenAlex collection of concepts' records from list format to data frame
#'
#' It converts bibliographic collection of concepts' records gathered from OpenAlex database \href{https://openalex.org/}{https://openalex.org/} into data frame.
#' The function converts a list of concepts' records obtained using \code{oa_request} into a data frame/tibble.
#'
#' @inheritParams oa2df
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
concepts2df <- function(data, verbose = TRUE) {

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
      c(
        "id", "display_name", "wikidata", "level", "description",
        "image_url", "image_thumbnail_url", "works_count", "cited_by_count",
        "works_api_url"
      )
    ]

    sub_id <- list(
      ids = subs_na(item$ids, type = "col_df"),
      relevance_score = item$relevance_score %||% NA
    )

    sub_rbind_dfs <- lapply(
      item[c("counts_by_year", "ancestors", "related_concepts")],
      subs_na,
      type = "rbind_df"
    )

    sub_row <- lapply(
      item$international[c("display_name", "description")],
      subs_na,
      type = "row_df"
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


#' Flatten snowball result
#'
#' |  id|title |...|cited_by_count| referenced_works   |cited_by |...|
#' | 100|foo   |...|             1| 98, 99             |101      |...|
#' | 200|bar   |...|             2| 198, 199           |201, 202 |...|
#' | 300|wug   |...|             2| 296, 297, 298, 299 |301, 302 |...|
#'
#' @param data List result from `oa_snowball`.
#' @param verbose Logical. If TRUE, print information on wrangling process.
#'
#' @return Tibble/data.frame of works with additional columns:
#' append `citing`, `backward_count`, `cited_by`, `forward_count`, `connection`,
#' and `connection_count.` For each work/row, these counts are WITHIN one
#' data search, and so `forward_count` <= `cited_by_count`.
#'
#' Consider the universe of all works linked to a set of starting works, (`oa_input = TRUE`)
#' for each work/row i:
#' - citing: works in the universe that i cites
#' - backward_count: number of works in the universe that i cites
#' - cited_by: works that i is cited by
#' - forward_count: number of works in the universe that i is cited by
#' - connection: works in the universe linked to i
#' - connection_count: number of works in the universe linked to i (degree of i)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' flat_snow <- snowball2df(oa_snowball(
#'   identifier = "W1516819724",
#'   verbose = TRUE
#' ))
#'
#' flat_snow[, c("id", "connection", "connection_count")]
#' }
snowball2df <- function(data, verbose = FALSE) {
  nodes <- data$nodes
  ids <- nodes$id[nodes$oa_input]
  edges_df <- data$edges

  citing <- do.call(rbind.data.frame, by(
    edges_df, list(edges_df$from),
    function(x) {
      list(
        id = unique(x$from),
        citing = paste(x$to, collapse = ";"),
        backward_count = nrow(x)
      )
    }
  ))

  cited_by <- do.call(rbind.data.frame, by(
    edges_df, list(edges_df$to),
    function(x) {
      list(
        id = unique(x$to),
        cited_by = paste(x$from, collapse = ";"),
        forward_count = nrow(x)
      )
    }
  ))

  if (verbose) message("Appending new columns...")

  nodes_augmented <- merge(
    merge(nodes, citing, all.x = TRUE),
    cited_by,
    all.x = TRUE
  )

  nodes_augmented$connection <- apply(
    nodes_augmented[, c("citing", "cited_by")], 1,
    function(x) paste(x[!is.na(x)], collapse = ";")
  )

  nodes_augmented[is.na(nodes_augmented$backward_count), "backward_count"] <- 0
  nodes_augmented[is.na(nodes_augmented$forward_count), "forward_count"] <- 0
  nodes_augmented$connection_count <-
    nodes_augmented$backward_count + nodes_augmented$forward_count

  nodes_augmented
}
