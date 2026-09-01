# Columns each show_*() helper needs from the `oa_fetch()` output.
show_required_cols <- function(entity) {
  switch(
    entity,
    works = c("id", "authorships", "concepts"),
    authors = c("id", "orcid", "topics")
  )
}

# Zero-row prototype returned when there is nothing to show.
show_prototype <- function(entity) {
  cols <- switch(
    entity,
    works = list(
      id = character(0),
      display_name = character(0),
      first_author = character(0),
      last_author = character(0),
      top_concepts = character(0)
    ),
    authors = list(
      id = character(0),
      display_name = character(0),
      orcid = character(0),
      works_count = integer(0),
      cited_by_count = integer(0),
      top_concepts = character(0)
    )
  )
  tibble::as_tibble(cols)
}

# Shared input check for show_works()/show_authors(). Returns NULL when the
# caller should carry on as usual, or a zero-row tibble to return instead.
check_show_input <- function(x, entity, arg = "x", call = rlang::caller_env()) {
  if (is.null(x)) {
    cli::cli_warn(
      c(
        "!" = "{.arg {arg}} is {.code NULL}; returning a zero-row tibble.",
        "i" = "{.fn oa_fetch} returns {.code NULL} when the query matched no
               records or when the API request failed.",
        "i" = "Re-run the query with {.code verbose = TRUE} to see what happened."
      ),
      class = "openalexR_empty_show",
      call = call
    )
    return(show_prototype(entity))
  }

  if (!is.data.frame(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a data frame returned by {.fn oa_fetch}, not
       {.cls {class(x)[[1]]}}.",
      call = call
    )
  }

  if (nrow(x) == 0) {
    return(show_prototype(entity))
  }

  missing_cols <- setdiff(show_required_cols(entity), names(x))
  if (length(missing_cols)) {
    cli::cli_abort(
      c(
        "x" = "{.arg {arg}} is missing {cli::qty(missing_cols)}required
               column{?s}: {.val {missing_cols}}.",
        "i" = "Did you call {.code oa_fetch(entity = \"{entity}\")}?",
        "i" = "Columns dropped by {.code oa_options(select = )} cannot be shown."
      ),
      call = call
    )
  }

  NULL
}

#' Simplify the OpenAlex authors result
#'
#' This function is mostly for the package's internal use,
#' but we export it so you can try it out.
#' However, we expect that you'll likely write your own function to
#' simplify the result however you want.
#'
#' @param x Dataframe/tibble. Result of the OpenAlex query for authors
#' already converted to data frame/tibble.
#' @param simp_func R function to simplify the result. Default to `head`.
#' If you want the entire table, set `simp_fun = identity`
#'
#'
#' @return Simplified tibble to display.
#' The first column, `id` is the short-form OpenAlex ID of the authors.
#' If `x` is `NULL` (which is what [oa_fetch()] returns when a query matched no
#' records or the API request failed) or has zero rows, a zero-row tibble with
#' these columns is returned, with a warning in the `NULL` case.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' show_authors(oa_fetch(
#'   identifier = c("A5023888391", "A5014077037"),
#'   verbose = TRUE
#' ))
#' }
show_authors <- function(x, simp_func = utils::head) {
  empty <- check_show_input(x, entity = "authors")
  if (!is.null(empty)) {
    return(simp_func(empty))
  }

  x$id <- vapply(x$id, shorten_oaid, character(1), USE.NAMES = FALSE)

  if (any(!is.na(x$orcid))) {
    x$orcid <- vapply(x$orcid, shorten_orcid, character(1), USE.NAMES = FALSE)
  }

  x$top_concepts <- vapply(
    x$topics,
    function(y) {
      if (is.logical(y)) {
        return(NA_character_)
      }
      top_subfields <- y[y$type == "subfield", ]
      paste(utils::head(top_subfields, 3)$display_name, collapse = ", ")
    },
    character(1)
  )

  simp_func(x[, c(
    "id",
    "display_name",
    "orcid",
    "works_count",
    "cited_by_count",
    "top_concepts"
  )])
}


#' Simplify the OpenAlex works result
#'
#' This function is mostly for the package's internal use,
#' but we export it so you can try it out.
#' However, we expect that you'll likely write your own function to
#' simplify the result however you want.
#'
#' @param x Dataframe/tibble. Result of the OpenAlex query for authors
#' already converted to data frame/tibble.
#' @param simp_func R function to simplify the result. Default to `head`.
#' If you want the entire table, set `simp_fun = identity`.
#'
#' @return Simplified tibble to display.
#' The first column, `id` is the short-form OpenAlex ID of the works.
#' If `x` is `NULL` (which is what [oa_fetch()] returns when a query matched no
#' records or the API request failed) or has zero rows, a zero-row tibble with
#' these columns is returned, with a warning in the `NULL` case.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' show_works(oa_fetch(
#'   identifier = c("W2741809807", "W2755950973"),
#'   verbose = TRUE
#' ))
#' }
show_works <- function(x, simp_func = utils::head) {
  empty <- check_show_input(x, entity = "works")
  if (!is.null(empty)) {
    return(simp_func(empty))
  }

  x$id <- vapply(x$id, shorten_oaid, character(1), USE.NAMES = FALSE)
  x$first_author <- vapply(
    x$authorships,
    get_auth_position,
    character(1),
    position = "first"
  )
  x$last_author <- vapply(
    x$authorships,
    get_auth_position,
    character(1),
    position = "last"
  )

  x$top_concepts <- vapply(
    x$concepts,
    function(y) {
      if (is.logical(y)) {
        return(NA_character_)
      }
      op_level <- min(2, max(y$level))
      paste(
        utils::head(y[y$level == op_level, "display_name"], 3),
        collapse = ", "
      )
    },
    character(1)
  )

  simp_cols <- intersect(
    c(
      "id",
      "display_name",
      "first_author",
      "last_author",
      "so",
      "url",
      "is_oa",
      "top_concepts",
      "role"
    ),
    names(x)
  )

  simp_func(x[, simp_cols])
}

get_auth_position <- function(y, position = "first") {
  if (length(y) == 1 && is.na(y)) {
    return(NA_character_)
  }
  last <- y[y$author_position == position, "display_name", drop = TRUE]
  if (length(last) == 0) {
    return(NA_character_)
  }
  last
}
