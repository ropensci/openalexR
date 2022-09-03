#' A function to perform a snowball search
#' and convert the result to a tibble/data frame.
#' @param identifier Character. It indicates a vector of item identifiers.
#' @param output a tibble/data.frame.
#' @param mailto is a character. To get into the polite pool, the arguments mailto have to give OpenAlex an email where they can contact you.
#' @param endpoint is character. It indicates the url of the OpenAlex Endpoint API server. The default value is endpoint = "https://api.openalex.org/".
#' @param verbose is a logical. If TRUE, information about the querying process will be plotted on screen. Default is \code{verbose=FALSE}.
#'
#' @return A data.frame or a tibble. Result of the snowball search.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' snowball_docs <- oa_snowball(
#'   identifier = c("W2741809807", "W2755950973"),
#'   endpoint = "https://api.openalex.org/",
#'   verbose = TRUE
#' )
#' }
oa_snowball <- function(
                     identifier = NULL, ## identifier of a work, author, venue, etc.
                     output = c("tibble", "dataframe"),
                     mailto = NULL,
                     endpoint = "https://api.openalex.org/",
                     verbose = FALSE) {
  output <- match.arg(output)

  if (output == "dataframe") output <- "tibble"

# fetching all documents citing the target papers
  if (isTRUE(verbose)) message("Collecting all documents citing the target papers")
citing <- oa_fetch(
  entity = "works",
  cites = identifier,
  output = output,
  endpoint = endpoint,
  mailto = mailto,
  verbose = verbose
)

# collecting the reference lists of the target papers
paper <- oa_fetch(
  entity = "works",
  identifier = identifier,
  output = output,
  endpoint = endpoint,
  mailto = mailto,
  verbose = verbose
)

# fetching all documents cited by the target papers
CR <- unique(unlist(paper$referenced_works))
list_CR <- split(CR, ceiling(seq_along(CR)/50))
cited <- list()
if (isTRUE(verbose)) message("Collecting all documents cited by the target papers")
for (i in 1:length(list_CR)){
  cited[[i]] <- oa_fetch(
    entity = "works",
    identifier = list_CR[[i]],
    output = output,
    endpoint = endpoint,
    mailto = mailto,
    verbose = verbose
  )
}
cited<- do.call(rbind,cited)

# merging all documents in a single data frame
citing$role <- "citing"
cited$role <- "cited"
paper$role <- "target"
df <- rbind(citing,cited, paper)

return(df)
}



