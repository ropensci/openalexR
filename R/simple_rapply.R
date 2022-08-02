# apply a function to all elements of a list

simple_rapply <- function(x, fn) {
  if (is.list(x)) {
    lapply(x, simple_rapply, fn)
  } else {
    fn(x)
  }
}
