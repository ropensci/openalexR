% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simplify.R
\name{show_works}
\alias{show_works}
\title{Simplify the OpenAlex works result}
\usage{
show_works(x, simp_func = utils::head)
}
\arguments{
\item{x}{Dataframe/tibble. Result of the OpenAlex query for authors
already converted to dataframe/tibble.}

\item{simp_func}{R function to simplify the result. Default to `head`.
If you want the entire table, set `simp_fun = identity`.}
}
\value{
Simplified tibble to display.
The first column, `id` is the short-form OpenAlex ID of the works
}
\description{
This function is mostly for the package's internal use,
but we export it so you can try it out.
However, we expect that you'll likely write your own function to
simplify the result however you want.
}
\examples{

show_works(oa_fetch(
  identifier = c("W2741809807", "W2755950973"),
  verbose = TRUE
))

}
