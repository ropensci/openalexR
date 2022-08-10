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

isValidEmail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case = TRUE)
}

append_flt <- function(x, pre = "from_publication_date", collapse = "|") {
  if (is.null(x)) {
    return(NULL)
  }

  if (length(x) > 1) x <- paste(x, collapse = collapse)
  paste0(pre, ":", x)
}

id_type <- function(identifier) {
  switch(toupper(substr(identifier, 1, 1)),
    W = "works",
    A = "authors",
    V = "venues",
    I = "institutions",
    C = "concepts"
  )
}
