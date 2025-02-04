#' Convert OpenAlex collection from list to data frame
#'
#' It converts bibliographic collections gathered from OpenAlex database \href{https://openalex.org/}{https://openalex.org/} into data frame.
#' The function converts a collection of records about works, authors, institutions, venues or keywords obtained using \code{oa_request} into a data frame/tibble.
#'
#' @param data List. Output of \code{oa_request}.
#' @param entity Character. Scholarly entity of the search.
#' The argument can be one of
#' c("works", "authors", "institutions", "keywords", "funders", "sources", "publishers", "topics").
#' @param abstract Logical. If TRUE, the function returns also the abstract of each item.
#' Ignored if entity is different from "works". Defaults to TRUE.
#' @param verbose Logical.
#' If TRUE, print information about the dataframe conversion process.
#' Defaults to TRUE.
#'
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
oa2df <- function(data, entity, options = NULL, count_only = FALSE, group_by = NULL, abstract = TRUE, verbose = TRUE) {

  if (rlang::is_interactive()) {
    rlang::warn(
      "Note: `oa_fetch` and `oa2df` now return new names for some columns in openalexR v2.0.0.
      See NEWS.md for the list of changes.
      Call `get_coverage()` to view the all updated columns and their original names in OpenAlex.",
      .frequency = "regularly",
      .frequency_id = "oa2df_column_change"
    )
  }

  if (length(data) == 0) {
    return(NULL)
  }

  if (!is.null(group_by)) {
    return(do.call(rbind.data.frame, data))
  }

  if (count_only && length(data) < 8) { # assuming less than 8 fields in output$meta
    return(unlist(data))
  }

  if (entity != "snowball") {
    ch <- ifelse(is.null(options$select), "id", options$select[[1]])
    if (!is.null(data[[ch]])) {
      data <- list(data)
    }
  }

  switch(entity,
    works = works2df(data, abstract, verbose),
    authors = authors2df(data, verbose),
    institutions = institutions2df(data, verbose),
    concepts = concepts2df(data, verbose),
    keywords = keywords2df(data, verbose),
    funders = funders2df(data, verbose),
    sources = sources2df(data, verbose),
    publishers = publishers2df(data, verbose),
    topics = topics2df(data, verbose),
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
#' @param pb Progress bar object. If verbose, computed from `oa_progress`.
#' NULL otherwise.
#' @inheritParams oa2df
#'
#' @return a data.frame.
#'
#' For more extensive information about OpenAlex API, please visit: <https://docs.openalex.org>
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
#'   endpoint = "https://api.openalex.org"
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
#' @export
#'
works2df <- function(data, abstract = TRUE, verbose = TRUE,
                     pb = if (verbose) oa_progress(length(data)) else NULL) {
  col_order <- c(
    "id", "title", "display_name", "authorships", "abstract", "doi",
    "publication_date", "publication_year", "relevance_score", "fwci",
    "cited_by_count", "counts_by_year", "cited_by_api_url", "ids", "type",
    "is_oa", "is_oa_anywhere", "oa_status", "oa_url",
    "any_repository_has_fulltext", "source_display_name", "source_id", "issn_l",
    "host_organization", "host_organization_name",
    "landing_page_url", "pdf_url", "license", "version", "referenced_works",
    "referenced_works_count", "related_works", "concepts", "topics", "keywords",
    "is_paratext", "is_retracted", "language", "grants", "apc",
    "first_page", "last_page", "volume", "issue"
  )
  works_process <- tibble::tribble(
    ~type, ~field,
    "identical", "id",
    "identical", "display_name",
    "identical", "title",
    "identical", "publication_date",
    "identical", "doi",
    "identical", "type",
    "identical", "cited_by_count",
    "identical", "publication_year",
    "identical", "cited_by_api_url",
    "identical", "is_paratext",
    "identical", "is_retracted",
    "identical", "relevance_score",
    "identical", "language",
    "identical", "fwci",
    "identical", "referenced_works_count",
    "flat", "grants",
    "flat", "referenced_works",
    "flat", "related_works",
    "rbind_df", "keywords",
    "rbind_df", "counts_by_year",
    "rbind_df", "concepts",
    "flat", "apc_list",
    "flat", "apc_paid",
    "flat", "ids"
  )

  so_cols <- c(
    source_id = "id",
    source_display_name = "display_name",
    issn_l = "issn_l",
    host_organization = "host_organization",
    host_organization_name = "host_organization_name"
  )

  n <- length(data)
  list_df <- vector(mode = "list", length = n)

  for (i in seq.int(n)) {
    if (verbose) pb$tick()

    paper <- data[[i]]

    fields <- works_process[works_process$field %in% names(paper), ]
    sim_fields <- mapply(
      function(x, y) subs_na(paper[[x]], type = y),
      fields$field,
      fields$type,
      SIMPLIFY = FALSE
    )
    if (!is.null(sim_fields$publication_date)) {
      sim_fields$publication_date <- as.Date(sim_fields$publication_date)
    }
    authorships <- process_paper_authors(paper$authorships)
    ab <- abstract_build(paper$abstract_inverted_index, abstract)
    paper_biblio <- replace_w_na(paper$biblio)
    open_access <- replace_w_na(paper$open_access)
    if (length(open_access) > 0) {
      names(open_access)[[1]] <- "is_oa_anywhere"
    }

    so_info <- paper$primary_location
    venue <- so_info[names(so_info) != "source"]
    source <- so_info$source
    if (!is.null(source)){
      source <- setNames(source[so_cols], names(so_cols))
    }

    # Process APC
    apc <- NULL
    if (any(lengths(paper[c("apc_list", "apc_paid")]) > 0)) {
      apc_fields <- list(
        value = NA_real_, currency = NA_character_,
        value_usd = NA_real_, provenance = NA_character_
      )
      apc_list <- paper$apc_list[lengths(paper$apc_list) != 0]
      apc_paid <- paper$apc_paid[lengths(paper$apc_paid) != 0]
      apc <- list(rbind.data.frame(
        c(type = "list", utils::modifyList(apc_fields, as.list(apc_list))),
        c(type = "paid", utils::modifyList(apc_fields, as.list(apc_paid)))
      ))
    }
    topics <- process_topics(paper, "score")
    out_ls <- c(sim_fields, venue, source, open_access, paper_biblio,
                list(authorships = authorships, abstract = ab, apc = apc), topics)
    out_ls[sapply(out_ls, is.null)] <- NULL
    list_df[[i]] <- out_ls
  }

  out_df <- rbind_oa_ls(list_df)
  out_df[, intersect(col_order, names(out_df))]
}


#' Convert OpenAlex collection of authors' records from list format to data frame
#'
#' It converts bibliographic collection of authors' records gathered from OpenAlex database \href{https://openalex.org/}{https://openalex.org/} into data frame.
#' The function converts a list of authors' records obtained using \code{oa_request} into a data frame/tibble.
#'
#' @inheritParams works2df
#'
#' @return a data.frame.
#'
#' For more extensive information about OpenAlex API, please visit: <https://docs.openalex.org>
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
#' res <- oa_fetch(
#'   entity = "authors",
#'   last_known_institutions.id = "I71267560",
#'   works_count = ">700",
#'   output = "list"
#' )
#'
#' df <- oa2df(res, entity = "authors")
#'
#' df
#' }
#'
#' @export
authors2df <- function(data, verbose = TRUE,
                       pb = if (verbose) oa_progress(length(data)) else NULL) {
  n <- length(data)
  list_df <- vector(mode = "list", length = n)

  author_process <- tibble::tribble(
    ~type, ~field,
    "identical", "id",
    "identical", "works_count",
    "identical", "display_name",
    "identical", "orcid",
    "identical", "works_api_url",
    "identical", "cited_by_count",
    "identical", "relevance_score",
    "flat", "display_name_alternatives",
    "rbind_df", "counts_by_year",
    "flat", "ids"
  )

  for (i in seq.int(n)) {
    if (verbose) pb$tick()

    item <- data[[i]]

    fields <- author_process[author_process$field %in% names(item), ]
    sim_fields <- mapply(
      function(x, y) subs_na(item[[x]], type = y),
      fields$field,
      fields$type,
      SIMPLIFY = FALSE
    )

    # current affiliations
    if (!is.null(item$last_known_institutions)) {
      l_inst <- item$last_known_institutions
      affs <- list(last_known_institutions = process_affil(l_inst))
    } else {
      affs <- NULL
    }

    topics <- process_topics(item, "count")
    list_df[[i]] <- c(sim_fields, affs, item$summary_stats, topics)
  }

  col_order <- c(
    "id", "display_name", "display_name_alternatives", "relevance_score",
    "ids", "orcid", "works_count", "cited_by_count", "counts_by_year",
    "2yr_mean_citedness", "h_index", "i10_index",
    "last_known_institutions", "topics", "works_api_url"
  )

  out_df <- rbind_oa_ls(list_df)
  out_df[, intersect(col_order, names(out_df))]
}


#' Convert OpenAlex collection of institutions' records from list format to data frame
#'
#' It converts bibliographic collection of institutions' records gathered from OpenAlex database \href{https://openalex.org/}{https://openalex.org/} into data frame.
#' The function converts a list of institutions' records obtained using \code{oa_request} into a data frame/tibble.
#'
#' @inheritParams works2df
#'
#' @return a data.frame.
#'
#' For more extensive information about OpenAlex API, please visit: <https://docs.openalex.org>
#'
#'
#' @examples
#' \dontrun{
#'
#' # Query to search information about all Italian educational institutions
#'
#' res <- oa_fetch(
#'   entity = "institutions",
#'   country_code = "it",
#'   type = "education",
#'   output = "list"
#' )
#'
#' oa2df(res, entity = "institutions")
#' }
#'
#' @export
institutions2df <- function(data, verbose = TRUE,
                            pb = if (verbose) oa_progress(length(data)) else NULL) {
  n <- length(data)
  list_df <- vector(mode = "list", length = n)

  institution_process <- tibble::tribble(
    ~type, ~field,
    "identical", "id",
    "identical", "ror",
    "identical", "works_api_url",
    "identical", "type",
    "identical", "works_count",
    "identical", "display_name",
    "identical", "country_code",
    "identical", "homepage_url",
    "identical", "image_url",
    "identical", "image_thumbnail_url",
    "identical", "cited_by_count",
    "identical", "updated_date",
    "identical", "created_date",
    "identical", "relevance_score",
    "flat", "summary_stats",
    "flat", "display_name_alternatives",
    "flat", "display_name_acronyms",
    "row_df", "geo",
    "rbind_df", "counts_by_year",
    "rbind_df", "associated_institutions",
    "flat", "ids"
  )

  for (i in seq.int(n)) {
    if (verbose) pb$tick()

    item <- data[[i]]
    fields <- institution_process[institution_process$field %in% names(item), ]
    sim_fields <- mapply(
      function(x, y) subs_na(item[[x]], type = y),
      fields$field,
      fields$type,
      SIMPLIFY = FALSE
    )
    interna <- NULL
    if (!is.null(item$international)) {
      interna <- list(
        international_display_name = subs_na(
          item$international$display_name,
          type = "flat"
        )
      )
    }
    topics <- process_topics(item, "count")
    list_df[[i]] <- c(sim_fields, interna, topics)
  }


  col_order <- c(
    "id", "display_name", "display_name_alternatives", "display_name_acronyms",
    "international_display_name", "ror", "ids", "country_code", "geo", "type",
    "homepage_url", "image_url", "image_thumbnail_url",
    "associated_institutions", "relevance_score", "works_count",
    "cited_by_count", "counts_by_year", "summary_stats",
    "works_api_url", "topics", "updated_date", "created_date"
  )

  out_df <- rbind_oa_ls(list_df)
  out_df[, intersect(col_order, names(out_df))]
}


#' Convert OpenAlex collection of concepts' records from list format to data frame
#'
#' It converts bibliographic collection of concepts' records gathered from OpenAlex database \href{https://openalex.org/}{https://openalex.org/} into data frame.
#' The function converts a list of concepts' records obtained using \code{oa_request} into a data frame/tibble.
#'
#' @inheritParams works2df
#'
#' @return a data.frame.
#'
#' For more extensive information about OpenAlex API, please visit: <https://docs.openalex.org>
#'
#'
#' @examples
#' \dontrun{
#'
#' # Query to search information about all Italian educational institutions
#'
#'
#' res <- oa_query(
#'   entity = "concepts",
#'   display_name.search = "electrodynamics",
#'   output = "list"
#' )
#'
#' df <- oa2df(res, entity = "concepts")
#'
#' df
#' }
#'
#' @export
concepts2df <- function(data, verbose = TRUE,
                        pb = if (verbose) oa_progress(length(data)) else NULL) {
  concept_process <- tibble::tribble(
    ~type, ~field,
    "identical", "id",
    "identical", "display_name",
    "identical", "wikidata",
    "identical", "level",
    "identical", "description",
    "identical", "image_url",
    "identical", "image_thumbnail_url",
    "identical", "works_count",
    "identical", "cited_by_count",
    "identical", "works_api_url",
    "identical", "relevance_score",
    "rbind_df", "counts_by_year",
    "rbind_df", "ancestors",
    "rbind_df", "related_concepts",
    "flat", "ids"
  )

  n <- length(data)
  list_df <- vector(mode = "list", length = n)

  for (i in seq.int(n)) {
    if (verbose) pb$tick()

    item <- data[[i]]
    fields <- concept_process[concept_process$field %in% names(item), ]
    sim_fields <- mapply(
      function(x, y) subs_na(item[[x]], type = y),
      fields$field,
      fields$type,
      SIMPLIFY = FALSE
    )

    intern_fields <- NULL
    if (!is.null(item$international)) {
      intern_fields <- lapply(
        item$international[c("display_name", "description")],
        subs_na,
        type = "flat"
      )
      names(intern_fields) <- paste(names(intern_fields), "international", sep = "_")
    }

    list_df[[i]] <- c(sim_fields, intern_fields)
  }

  col_order <- c(
    "id", "display_name", "display_name_international", "description",
    "description_international", "wikidata", "level", "ids",
    "image_url", "image_thumbnail_url", "ancestors",
    "related_concepts", "relevance_score", "works_count",
    "cited_by_count", "counts_by_year",
    "works_api_url"
  )

  out_df <- rbind_oa_ls(list_df)
  out_df[, intersect(col_order, names(out_df))]
}


#' Convert keywords from list to data frame
#'
#' The function converts a list of keywords obtained using \code{oa_request} or
#' \code{oa_fetch(output = "list")} into a data frame/tibble. More on keyword at
#' <https://help.openalex.org/hc/en-us/articles/24736201130391-Keywords>.
#'
#' @inheritParams works2df
#'
#' @return a data.frame.
#'
#'
#' @examples
#' \dontrun{
#'
#' x <- oa_fetch(
#'   entity = "keywords",
#'   options = list(sample = 5),
#'   output = "list"
#' )
#'
#' df <- oa2df(x, entity = "keywords")
#'
#' df
#' }
#'
#' @export
keywords2df <- function(data, verbose = TRUE) {
  tibble::as_tibble(subs_na(data, "rbind_df")[[1]])
}


#' Convert OpenAlex collection of funders' records from list format to data frame
#'
#' It converts bibliographic collection of funders' records gathered from OpenAlex database \href{https://openalex.org/}{https://openalex.org/} into data frame.
#' The function converts a list of funders' records obtained using \code{oa_request} into a data frame/tibble.
#'
#' @inheritParams works2df
#'
#' @return a data.frame.
#'
#' For more extensive information about OpenAlex API, please visit: <https://docs.openalex.org>
#'
#'
#' @examples
#' \dontrun{
#'
#' # Get funders located in Canada with more than 100,000 citations
#'
#' res <- oa_request(
#'   "https://api.openalex.org/funders?filter=country_code:ca,cited_by_count:>100000"
#' )
#'
#' df <- oa2df(res, entity = "funders")
#'
#' df
#' }
#'
#' @export
funders2df <- function(data, verbose = TRUE,
                       pb = if (verbose) oa_progress(length(data)) else NULL) {
  funder_process <- tibble::tribble(
    ~type, ~field,
    "identical", "id",
    "identical", "display_name",
    "flat", "alternate_titles",
    "identical", "country_code",
    "identical", "description",
    "identical", "homepage_url",
    "identical", "image_url",
    "identical", "image_thumbnail_url",
    "identical", "grants_count",
    "identical", "works_count",
    "identical", "cited_by_count",
    "flat", "summary_stats",
    "flat", "ids",
    "rbind_df", "counts_by_year",
    "rbind_df", "roles",
    "identical", "updated_date",
    "identical", "created_date"
  )

  n <- length(data)
  list_df <- vector(mode = "list", length = n)

  for (i in seq.int(n)) {
    if (verbose) pb$tick()

    item <- data[[i]]
    fields <- funder_process[funder_process$field %in% names(item), ]
    sim_fields <- mapply(
      function(x, y) subs_na(item[[x]], type = y),
      fields$field,
      fields$type,
      SIMPLIFY = FALSE
    )
    list_df[[i]] <- sim_fields
  }

  out_df <- rbind_oa_ls(list_df)
  out_df
}



#' Convert OpenAlex collection of sources' records from list format to data frame
#'
#' It converts bibliographic collection of sources' records gathered from OpenAlex database \href{https://openalex.org/}{https://openalex.org/} into data frame.
#' The function converts a list of sources' records obtained using \code{oa_request} into a data frame/tibble.
#'
#' @inheritParams works2df
#'
#' @return a data.frame.
#'
#' For more extensive information about OpenAlex API, please visit: <https://docs.openalex.org>
#'
#'
#' @examples
#' \dontrun{
#'
#' # Get sources from Nature
#'
#' res <- oa_request(
#'   "https://api.openalex.org/sources?search=nature"
#' )
#'
#' df <- oa2df(res, entity = "sources")
#'
#' df
#' }
#'
#' @export
sources2df <- function(data, verbose = TRUE,
                       pb = if (verbose) oa_progress(length(data)) else NULL) {
  source_process <- tibble::tribble(
    ~type, ~field,
    "identical", "id",
    "identical", "issn_l",
    "flat", "issn",
    "identical", "display_name",
    "identical", "host_organization",
    "identical", "host_organization_name",
    "flat", "host_organization_lineage",
    "identical", "relevance_score",
    "identical", "works_count",
    "identical", "cited_by_count",
    "flat", "summary_stats",
    "identical", "is_oa",
    "identical", "is_in_doaj",
    "identical", "is_indexed_in_scopus",
    "flat", "ids",
    "identical", "homepage_url",
    "rbind_df", "apc_prices",
    "identical", "apc_usd",
    "identical", "country_code",
    "flat", "societies",
    "flat", "alternate_titles",
    "identical", "abbreviated_title",
    "identical", "type",
    "rbind_df", "counts_by_year",
    "identical", "works_api_url",
    "identical", "updated_date",
    "identical", "created_date"
  )

  n <- length(data)
  list_df <- vector(mode = "list", length = n)

  for (i in seq.int(n)) {
    if (verbose) pb$tick()

    item <- data[[i]]
    fields <- source_process[source_process$field %in% names(item), ]
    sim_fields <- mapply(
      function(x, y) subs_na(item[[x]], type = y),
      fields$field,
      fields$type,
      SIMPLIFY = FALSE
    )
    topics <- process_topics(item, "count")
    list_df[[i]] <- c(sim_fields, topics)
  }

  out_df <- rbind_oa_ls(list_df)
  out_df
}



#' Convert OpenAlex collection of publishers' records from list format to data frame
#'
#' It converts bibliographic collection of publishers' records gathered from OpenAlex database \href{https://openalex.org/}{https://openalex.org/} into data frame.
#' The function converts a list of publishers' records obtained using \code{oa_request} into a data frame/tibble.
#'
#' @inheritParams works2df
#'
#' @return a data.frame.
#'
#' For more extensive information about OpenAlex API, please visit: <https://docs.openalex.org>
#'
#'
#' @examples
#' \dontrun{
#'
#' # Get publishers located in Canada with more than 100,000 citations
#'
#' res <- oa_request(
#'   "https://api.openalex.org/publishers?filter=country_codes:ca"
#' )
#'
#' df <- oa2df(res, entity = "publishers")
#'
#' df
#' }
#'
#' @export
publishers2df <- function(data, verbose = TRUE,
                          pb = if (verbose) oa_progress(length(data)) else NULL) {
  publisher_process <- tibble::tribble(
    ~type, ~field,
    "identical", "id",
    "identical", "display_name",
    "flat", "alternate_titles",
    "identical", "hierarchy_level",
    "flat", "parent_publisher",
    "flat", "lineage",
    "identical", "country_codes",
    "identical", "homepage_url",
    "identical", "image_url",
    "identical", "image_thumbnail_url",
    "identical", "works_count",
    "identical", "cited_by_count",
    "flat", "summary_stats",
    "flat", "ids",
    "rbind_df", "counts_by_year",
    "rbind_df", "roles",
    "identical", "sources_api_url",
    "identical", "updated_date",
    "identical", "created_date"
  )

  n <- length(data)
  list_df <- vector(mode = "list", length = n)

  for (i in seq.int(n)) {
    if (verbose) pb$tick()

    item <- data[[i]]
    fields <- publisher_process[publisher_process$field %in% names(item), ]
    sim_fields <- mapply(
      function(x, y) subs_na(item[[x]], type = y),
      fields$field,
      fields$type,
      SIMPLIFY = FALSE
    )
    list_df[[i]] <- sim_fields
  }

  out_df <- rbind_oa_ls(list_df)
  out_df
}


#' Convert OpenAlex collection of topics' records from list format to data frame
#'
#' It converts collection of topics' records gathered from the OpenAlex database.
#' The function converts a list of topics' records obtained using \code{oa_request} into a data frame/tibble.
#'
#' @inheritParams works2df
#'
#' @return a data.frame.
#'
#' For more extensive information about OpenAlex API, please visit: <https://docs.openalex.org>
#'
#'
#' @examples
#' \dontrun{
#'
#' # Query to search information about all Italian educational institutions
#'
#'
#' res <- oa_query(
#'   entity = "topics",
#'   display_name.search = "electrodynamics",
#'   output = "list"
#' )
#'
#' df <- oa2df(res, entity = "topics")
#'
#' df
#' }
#'
#' @export
topics2df <- function(data, verbose = TRUE,
                        pb = if (verbose) oa_progress(length(data)) else NULL) {
  topic_process <- tibble::tribble(
    ~type, ~field,
    "identical", "id",
    "identical", "display_name",
    "identical", "description",
    "flat", "ids",
    "identical", "relevance_score",
    "identical", "works_count",
    "identical", "cited_by_count",
    "identical", "updated_date",
    "identical", "created_date",
    "rbind_df", "siblings",
    "flat", "keywords"
  )

  n <- length(data)
  list_df <- vector(mode = "list", length = n)

  for (i in seq.int(n)) {
    if (verbose) pb$tick()

    item <- data[[i]]
    fields <- topic_process[topic_process$field %in% names(item), ]
    sim_fields <- mapply(
      function(x, y) subs_na(item[[x]], type = y),
      fields$field,
      fields$type,
      SIMPLIFY = FALSE
    )
    domains <- unlist(item[c("subfield", "field", "domain")], recursive = FALSE)
    domains <- as.data.frame(do.call(cbind, domains))
    names(domains) <- gsub("\\.", "_", names(domains))
    list_df[[i]] <- c(sim_fields, domains)
  }

  col_order <- c(
    "id", "display_name", "description", "keywords", "ids",
    "subfield_id", "subfield_display_name", "field_id", "field_display_name",
    "domain_id", "domain_display_name", "siblings", "relevance_score",
    "works_count", "cited_by_count", "updated_date", "created_date"
  )

  out_df <- rbind_oa_ls(list_df)
  out_df[, intersect(col_order, names(out_df))]
}



#' Flatten snowball result
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

  tibble::as_tibble(nodes_augmented)
}
