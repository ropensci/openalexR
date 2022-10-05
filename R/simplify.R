
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
#'   identifier = c("A923435168", "A2208157607"),
#'   verbose = TRUE
#' ))
#'
show_authors <- function(x, simp_func = utils::head){
  x$id <- sapply(strsplit(x$id, split= "/"), function(y) utils::tail(y, 1))

  if (any(!is.na(x$orcid))){
    x$orcid <- sapply(strsplit(x$orcid, split= "/"), function(y) utils::tail(y, 1))
  }

  x$top_concepts <- sapply(
    x$x_concepts,
    function(y) {
      if (is.logical(y)) return(NA)
      op_level <- min(1, max(y$level))
      paste(utils::head(y[y$level == op_level, "display_name"], 3), collapse = ", ")
    }
  )

  simp_func(x[, c("id", "display_name", "orcid", "works_count",
                  "cited_by_count", "affiliation_display_name", "top_concepts")])
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
show_works <- function(x, simp_func = utils::head){
  x$id <- sapply(strsplit(x$id, split= "/"), function(y) utils::tail(y, 1))
  x$first_author <- sapply(
    x$author,
    function(y) y[y$author_position == "first", "au_display_name"]
  )
  x$last_author <- sapply(
    x$author,
    function(y) y[y$author_position == "last", "au_display_name"]
  )

  x$top_concepts <- sapply(
    x$concepts,
    function(y) {
      if (is.logical(y)) return(NA)
      op_level <- min(2, max(y$level))
      paste(utils::head(y[y$level == op_level, "display_name"], 3), collapse = ", ")
    }
  )

  simp_cols <- intersect(
    c("id", "display_name", "first_author", "last_author", "so", "url", "is_oa", "top_concepts", "role"),
    names(x)
  )

  simp_func(x[, simp_cols])
}
