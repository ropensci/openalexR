# apply a function to all elements of a list

simple_rapply <- function(x, fn, ...) {
  if (is.list(x)) {
    lapply(x, simple_rapply, fn, ...)
  } else {
    fn(x, ...)
  }
}

`%||%` <- function(x, y) if (is.null(x)) y else x

subs_na <- function(x, df = FALSE){
  if (length(x) > 0) {
    if (df){
      return(list(as.data.frame(x)))
    }
    return(list(unlist(x)))
  } else {
    return(NA)
  }
}
