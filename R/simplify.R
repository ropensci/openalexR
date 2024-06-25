#' Simplify the OpenAlex authors result
#'
#' This function is mostly for the package's internal use,
#' but we export it so you can try it out.
#' However, we expect that you'll likely write your own function to
#' simplify the result however you want.
#'
#' @param x Dataframe/tibble. Result of the OpenAlex query for authors
#' already converted to dataframe/tibble.
#' @param simp_func R function to simplify the result. Default to `head`.
#' If you want the entire table, set `simp_fun = identity`
#'
#'
#' @return Simplified tibble to display.
#' The first column, `id` is the short-form OpenAlex ID of the authors.
#'
#' @export
#'
#' @examples
#'
#' show_authors(oa_fetch(
#'   identifier = c("A5023888391", "A5014077037"),
#'   verbose = TRUE
#' ))
#'
show_authors <- function(x, simp_func = utils::head) {
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
      top_subfields <- y[y$name == "subfield", ]
      paste(utils::head(top_subfields, 3)$display_name,
        collapse = ", "
      )
    },
    character(1)
  )

  simp_func(x[, c(
    "id", "display_name", "orcid", "works_count",
    "cited_by_count", "affiliation_display_name", "top_concepts"
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
#' already converted to dataframe/tibble.
#' @param simp_func R function to simplify the result. Default to `head`.
#' If you want the entire table, set `simp_fun = identity`.
#'
#' @return Simplified tibble to display.
#' The first column, `id` is the short-form OpenAlex ID of the works
#'
#' @export
#'
#' @examples
#'
#' show_works(oa_fetch(
#'   identifier = c("W2741809807", "W2755950973"),
#'   verbose = TRUE
#' ))
#'
show_works <- function(x, simp_func = utils::head) {
  x$id <- vapply(x$id, shorten_oaid, character(1), USE.NAMES = FALSE)
  x$first_author <- vapply(
    x$author, get_auth_position, character(1),
    position = "first"
  )
  x$last_author <- vapply(
    x$author, get_auth_position, character(1),
    position = "last"
  )

  x$top_concepts <- vapply(
    x$concepts,
    function(y) {
      if (is.logical(y)) {
        return(NA_character_)
      }
      op_level <- min(2, max(y$level))
      paste(utils::head(y[y$level == op_level, "display_name"], 3),
        collapse = ", "
      )
    },
    character(1)
  )

  simp_cols <- intersect(
    c(
      "id", "display_name", "first_author", "last_author",
      "so", "url", "is_oa", "top_concepts", "role"
    ),
    names(x)
  )

  simp_func(x[, simp_cols])
}

get_auth_position <- function(y, position = "first") {
  if (length(y) == 1 && is.na(y)) {
    return(NA_character_)
  }
  last <- y[y$author_position == position, "au_display_name"]
  if (length(last) == 0) {
    return(NA_character_)
  }
  last
}
