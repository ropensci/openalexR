#' Construct a set of options for an OpenAlex query
#'
#' `oa_options()` builds the `options` argument of [oa_fetch()] and [oa_query()].
#' It gathers the "power-user" parameters of an OpenAlex query --- field
#' selection, sorting, random sampling, and paging --- into a single, validated
#' object. Using `oa_options()` (instead of a bare `list()`) gives you argument
#' autocompletion, sensible defaults, and early errors on typos or invalid
#' values.
#'
#' Passing a plain `list()` to `options` is still supported for backward
#' compatibility, but unknown keys in a plain list are forwarded to the API
#' verbatim and therefore silently ignored if misspelled. `oa_options()` warns
#' you about unknown keys instead.
#'
#' @param select Character vector. Top-level fields to show in output.
#' Defaults to NULL, which returns all fields.
#' <https://docs.openalex.org/how-to-use-the-api/get-single-entities/select-fields>
#' @param sort Character. Attribute to sort by.
#' For example: "display_name" for sources or "cited_by_count:desc" for works.
#' See more at <https://docs.openalex.org/how-to-use-the-api/get-lists-of-entities/sort-entity-lists>.
#' @param sample Integer. Number of (random) records to return.
#' Should be no larger than 10,000.
#' Defaults to NULL, which returns all records satisfying the query.
#' Read more at <https://docs.openalex.org/how-to-use-the-api/get-lists-of-entities/sample-entity-lists>.
#' @param seed Integer.
#' A seed value in order to retrieve the same set of random records in
#' the same order when used multiple times with `sample`.
#' IMPORTANT NOTE: Depending on your query, random results with a seed value may
#' change over time due to new records coming into OpenAlex.
#' This argument is likely only useful when queries happen close together
#' (within a day).
#' @param per_page Numeric. Number of items to download per page.
#' The per-page argument can assume any number between 1 and 200.
#' Defaults to 200.
#' @param paging Character.
#' Either "cursor" for cursor paging or "page" for basic paging.
#' When used with `sample` and/or `pages`, paging is automatically set to
#' basic paging (`paging = "page"`) to avoid duplicates and get the right page.
#' See <https://docs.openalex.org/how-to-use-the-api/get-lists-of-entities/paging>.
#' @param pages Integer vector.
#' The range of pages to return. If NULL, return all pages.
#' @param ... Additional query parameters forwarded to the OpenAlex API
#' verbatim. Unknown parameters trigger a warning, since they are most often
#' typos; supply them here only if you intend to use a parameter not yet wrapped
#' by openalexR.
#'
#' @return A named list of query options with class `oa_options`.
#' @export
#'
#' @examples
#' oa_options(select = c("id", "doi"), sort = "cited_by_count:desc")
#' oa_options(sample = 50, seed = 1)
#' oa_options(per_page = 50)
oa_options <- function(select = NULL,
                       sort = NULL,
                       sample = NULL,
                       seed = NULL,
                       per_page = 200,
                       paging = c("cursor", "page"),
                       pages = NULL,
                       ...) {
  paging <- match.arg(paging)

  if (!is.null(sample)) {
    sample <- as.integer(sample)
    if (is.na(sample) || sample <= 0) {
      cli::cli_abort("{.arg sample} must be a positive integer.")
    }
    if (sample > 10000) {
      cli::cli_abort(
        "{.arg sample} must be no larger than 10,000, not {.val {sample}}."
      )
    }
  }

  if (!is.null(seed)) {
    seed <- as.integer(seed)
    if (is.null(sample)) {
      cli::cli_warn(
        "{.arg seed} is only used together with {.arg sample}; ignoring it."
      )
      seed <- NULL
    }
  }

  if (!is.null(per_page)) {
    per_page <- as.integer(per_page)
    if (is.na(per_page) || per_page < 1 || per_page > 200) {
      cli::cli_abort(
        "{.arg per_page} must be between 1 and 200, not {.val {per_page}}."
      )
    }
  }

  extras <- list(...)
  if (length(extras) > 0) {
    nms <- names(extras)
    if (is.null(nms) || any(nms == "")) {
      cli::cli_abort("All arguments passed via {.arg ...} must be named.")
    }
    cli::cli_warn(c(
      "!" = "{length(nms)} unknown option{?s} passed to {.fn oa_options}: \\
             {.val {nms}}.",
      "i" = "Unknown options are forwarded to the OpenAlex API verbatim; \\
             check for typos if this is unintended."
    ))
  }

  out <- c(
    list(
      select = select,
      sort = sort,
      sample = sample,
      seed = seed,
      per_page = per_page,
      paging = paging,
      pages = pages
    ),
    extras
  )

  # drop NULL entries so the object only carries values that are actually set
  out <- out[!vapply(out, is.null, logical(1))]

  structure(out, class = c("oa_options", "list"))
}

# Internal: coerce the user-supplied `options` (NULL, plain list, or
# `oa_options` object) into a normalized list that always carries the paging
# defaults needed for routing. `oa_options` objects are pre-validated; plain
# lists stay permissive for backward compatibility.
as_oa_options <- function(options) {
  if (is.null(options)) {
    return(oa_options())
  }

  if (inherits(options, "oa_options")) {
    return(options)
  }

  if (!is.list(options)) {
    cli::cli_abort(
      "{.arg options} must be a list or an {.fn oa_options} object."
    )
  }

  # plain list: fill in paging defaults if absent, keep everything else as-is
  if (is.null(options[["per_page"]])) options[["per_page"]] <- 200L
  if (is.null(options[["paging"]])) options[["paging"]] <- "cursor"
  options
}

# Internal: names of fields that control paging/requesting rather than the
# query URL. These are routed to `oa_request()` and must NOT leak into the URL.
oa_paging_fields <- function() {
  c("per_page", "paging", "pages")
}
