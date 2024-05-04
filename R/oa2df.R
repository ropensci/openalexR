#' Convert OpenAlex collection from list to data frame
#'
#' It converts bibliographic collections gathered from OpenAlex database \href{https://openalex.org/}{https://openalex.org/} into data frame.
#' The function converts a collection of records about works, authors, institutions, venues or concepts obtained using \code{oa_request} into a data frame/tibble.
#'
#' @param data List. Output of \code{oa_request}.
#' @param entity Character. Scholarly entity of the search.
#' The argument can be one of
#' c("works", "authors", "institutions", "concepts", "funders", "sources", "publishers").
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
    funders = funders2df(data, verbose),
    sources = sources2df(data, verbose),
    publishers = publishers2df(data, verbose),
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
    "id", "title", "display_name", "author", "ab", "publication_date", "relevance_score",
    "so", "so_id", "host_organization", "issn_l", "url", "pdf_url",
    "license", "version", "first_page", "last_page", "volume", "issue", "is_oa",
    "is_oa_anywhere", "oa_status", "oa_url", "any_repository_has_fulltext",
    "language", "grants", "cited_by_count", "counts_by_year",
    "publication_year", "cited_by_api_url", "ids", "doi", "type",
    "referenced_works", "related_works", "is_paratext", "is_retracted",
    "concepts", "topics"
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
    "flat", "grants",
    "flat", "referenced_works",
    "flat", "related_works",
    "rbind_df", "counts_by_year",
    "rbind_df", "concepts",
    "flat", "ids"
  )

  venue_cols <- c(
    url = "landing_page_url",
    pdf_url = "pdf_url",
    is_oa = "is_oa",
    license = "license",
    version = "version"
  )
  so_cols <- c(
    so_id = "id",
    so = "display_name",
    issn_l = "issn_l",
    host_organization = "host_organization_name"
  )
  inst_cols <- c("id", "display_name", "ror", "country_code", "type", "lineage")
  empty_inst <- empty_list(inst_cols)

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

    author <- venue <- ab <- NULL

    if (!is.null(paper$primary_location)) {
      so_info <- paper$primary_location["source"]
      so_info <- if (length(so_info[[1]]) == 0) NA else so_info[[1]]
      venue_info <- replace_w_na(paper$primary_location[venue_cols])
      venue <- setNames(
        c(venue_info, so_info[so_cols]),
        c(names(venue_cols), names(so_cols))
      )
    }

    # authorships and affilitation
    if (!is.null(paper$authorships)) {
      author <- subs_na(
        lapply(paper$authorships, function(l) {
          l_inst <- l$institutions
          inst_idx <- lengths(l_inst) > 0
          if (length(inst_idx) > 0 && any(inst_idx)) {
            first_inst <- l_inst[inst_idx][[1]]
            first_inst$lineage <- paste(first_inst$lineage, collapse = ", ")
          } else {
            first_inst <- empty_inst
          }
          first_inst <- prepend(first_inst, "institution")
          aff_raw <- list(
            au_affiliation_raw =
              if (length(l$raw_affiliation_strings)) {
                l$raw_affiliation_strings[[1]]
              } else {
                NA_character_
              }
          )
          l_author <- if (length(l$author) > 0) {
            prepend(replace_w_na(l$author), "au")
          } else {
            empty_list(c("au_id", "au_display_name", "au_orcid"))
          }
          c(l_author, l[c("author_position", "is_corresponding")], aff_raw, first_inst)
        }), "rbind_df"
      )
    }

    # Abstract
    if (!is.null(paper$abstract_inverted_index) && abstract) {
      ab <- abstract_build(paper$abstract_inverted_index)
    }
    paper_biblio <- replace_w_na(paper$biblio)
    open_access <- replace_w_na(paper$open_access)
    if (length(open_access) > 0) {
      names(open_access)[[1]] <- "is_oa_anywhere"
    }

    # Topics
    process_paper_topics <- function(paper) {
      topics <- paper$topics
      if (is.null(topics)) {
        return(NULL)
      }
      topics_ls <- lapply(seq_along(topics), function(i) {
        topic <- topics[[i]]
        relev <- c(
          # Hoist fields for the topic entity
          list(topic = topic[c("id", "display_name")]),
          # Keep info about other entities as-is
          topic[vapply(topic, is.list, logical(1))]
        )
        relev_df <- subs_na(relev, "rbind_df")[[1]]
        relev_df <- tibble::rownames_to_column(relev_df, "name")
        cbind(i = i, score = topic$score, relev_df)
      })
      topics_df <- do.call(rbind.data.frame, topics_ls)
      list(tibble::as_tibble(topics_df))
    }
    topics <- process_paper_topics(paper)

    out_ls <- c(
      sim_fields, venue, open_access, paper_biblio,
      list(author = author, ab = ab, topics = topics)
    )
    out_ls[sapply(out_ls, is.null)] <- NULL
    list_df[[i]] <- out_ls
  }

  out_df <- rbind_oa_ls(list_df)
  out_df[, intersect(col_order, names(out_df))]
}

abstract_build <- function(ab) {
  if (is.null(ab)) {
    return(NA)
  }
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
#' query_author <- oa_query(
#'   identifier = NULL,
#'   entity = "authors",
#'   last_known_institutions.id = "I71267560",
#'   works_count = ">500"
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
#' @export
authors2df <- function(data, verbose = TRUE,
                       pb = if (verbose) oa_progress(length(data)) else NULL) {
  n <- length(data)
  list_df <- vector(mode = "list", length = n)

  inst_cols <- c("id", "display_name", "ror", "country_code", "type", "lineage")
  empty_inst <- empty_list(inst_cols)
  empty_inst$affiliations_other <- list(NULL)

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
    "rbind_df", "x_concepts",
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
    sub_affiliation <- item$last_known_institutions
    if (!is.null(sub_affiliation) && length(sub_affiliation)) {
      sub_affiliation <- sub_affiliation[[1]]
      if (is.na(sub_affiliation[[1]])) {
        sub_affiliation <- empty_inst
      }
      sub_affiliation$lineage <- paste(sub_affiliation$lineage, collapse = ", ")
      sub_affiliation <- prepend(sub_affiliation, "affiliation")
    }
    sub_affiliation <- replace_w_na(sub_affiliation)

    if (!is.null(item$affiliations)) {
      affiliations_other <- sapply(item$affiliations, function(x) x$institution$id)
      if (!is.null(sub_affiliation$affiliation_id)) {
        affiliations_other <- affiliations_other[affiliations_other != sub_affiliation$affiliation_id]
      }
      sub_affiliation$affiliations_other <- list(affiliations_other)
    }

    list_df[[i]] <- c(sim_fields, sub_affiliation)
  }

  col_order <- c(
    "id", "display_name", "display_name_alternatives", "relevance_score",
    "ids", "orcid", "works_count", "cited_by_count", "counts_by_year",
    "affiliation_display_name", "affiliation_id", "affiliation_ror",
    "affiliation_country_code", "affiliation_type", "affiliation_lineage",
    "affiliations_other",
    "x_concepts", "works_api_url"
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
    "flat", "display_name_alternatives",
    "flat", "display_name_acronyms",
    "row_df", "geo",
    "rbind_df", "counts_by_year",
    "rbind_df", "x_concepts",
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
        display_name_international = subs_na(
          item$international$display_name,
          type = "flat"
        )
      )
    }
    list_df[[i]] <- c(sim_fields, interna)
  }


  col_order <- c(
    "id", "display_name", "display_name_alternatives", "display_name_acronyms",
    "display_name_international", "ror", "ids", "country_code", "geo", "type",
    "homepage_url", "image_url", "image_thumbnail_url",
    "associated_institutions", "relevance_score", "works_count",
    "cited_by_count", "counts_by_year",
    "works_api_url", "x_concepts", "updated_date", "created_date"
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
    "flat", "ids",
    "identical", "homepage_url",
    "rbind_df", "apc_prices",
    "identical", "apc_usd",
    "identical", "country_code",
    "flat", "societies",
    "flat", "alternate_titles",
    "identical", "abbreviated_title",
    "identical", "type",
    "rbind_df", "x_concepts",
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
    list_df[[i]] <- sim_fields
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
