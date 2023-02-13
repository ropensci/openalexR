# apply a function to all elements of a list

simple_rapply <- function(x, fn, ...) {
  if (is.list(x)) {
    lapply(x, simple_rapply, fn, ...)
  } else {
    fn(x, ...)
  }
}

`%||%` <- function(x, y) if (is.null(x)) y else x

subs_na <- function(x, type = c("row_df", "col_df", "flat", "rbind_df"), prefix = NULL) {
  type <- match.arg(type)

  if (length(x) == 0) {
    return(NA)
  }

  out <- switch(type,
    row_df = as.data.frame(x),
    col_df = tibble::enframe(unlist(x)),
    flat = unlist(x),
    rbind_df = do.call(rbind.data.frame, x)
  )

  if (!is.null(prefix)){
    out <- prepend(out, prefix)
  }

  list(out)
}

prepend <- function(x, prefix = ""){
  names(x) <- paste(prefix, names(x), sep = "_")
  x
}

empty_list <- function(vars) {
  setNames(as.list(rep(NA, length(vars))), vars)
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
    C = "concepts",
    NA
  )
}

oa_email <- function() {
  return(getOption("openalexR.mailto", default = NULL))
}

oa_progress <- function(n, text = "converting") {
  progress::progress_bar$new(
    format = paste(" ", text, "[:bar] :percent eta: :eta"),
    total = n, clear = FALSE, width = 60
  )
}

asl <- function(z) {
  if (length(z) > 1) return(z)

  z_low <- tolower(z)
  if (z_low == "true" || z_low == "false") {
    return(z_low)
  } else {
    return(z)
  }
}

shorten_oaid <- function(id) {
  gsub("^https://openalex.org/", "", id)
}

shorten_orcid <- function(id) {
  gsub("^https://orcid.org/", "", id)
}
