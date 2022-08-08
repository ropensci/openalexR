# apply a function to all elements of a list

simple_rapply <- function(x, fn, ...) {
  if (is.list(x)) {
    lapply(x, simple_rapply, fn, ...)
  } else {
    fn(x, ...)
  }
}

`%||%` <- function(x, y) if (is.null(x)) y else x

subs_na <- function(x, type = c("row_df", "col_df", "flat")) {
  type <- match.arg(type)

  if (length(x) == 0) {
    return(NA)
  }

  switch(type,
    row_df = list(as.data.frame(x)),
    col_df = list(tibble::enframe(unlist(x))),
    flat = list(unlist(x))
  )
}
