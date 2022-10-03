#' A function to perform a snowball search
#' and convert the result to a tibble/data frame.
#' @param identifier Character. It indicates a vector of openalex_id identifiers.
#' @param output a tibble/data.frame.
#' @param mailto is a character. To get into the polite pool, the arguments mailto have to give OpenAlex an email where they can contact you.
#' @param endpoint is character. It indicates the url of the OpenAlex Endpoint API server. The default value is endpoint = "https://api.openalex.org/".
#' @param verbose is a logical. If TRUE, information about the querying process will be plotted on screen. Default is \code{verbose=FALSE}.
#'
#' @return A list containing 2 objects: relationship links among documents (from -> to); data frame with publication records.
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
oa_snowball <- function(identifier = NULL,
                        output = c("tibble", "dataframe"),
                        mailto = oa_email(),
                        endpoint = "https://api.openalex.org/",
                        verbose = FALSE) {
  output <- match.arg(output)

  if (output == "dataframe") output <- "tibble"

  # fetching all documents citing the target papers
  if (verbose) message("Collecting all documents citing the target papers")

  citing <- oa_fetch(
    entity = "works",
    cites = identifier,
    output = output,
    endpoint = endpoint,
    mailto = mailto,
    verbose = verbose
  )

  # collecting records about the target papers
  paper <- oa_fetch(
    entity = "works",
    identifier = identifier,
    output = output,
    endpoint = endpoint,
    mailto = mailto,
    verbose = verbose
  )

  # collecting all documents cited by the target papers
  if (verbose) message("Collecting all documents cited by the target papers")

  cited <- oa_fetch(
    entity = "works",
    cited_by = identifier,
    output = output,
    endpoint = endpoint,
    mailto = mailto,
    verbose = verbose
  )

  # merging all documents in a single data frame
  if (is.null(citing)) citing <- paper[0, TRUE]

  citing$role <- "citing"
  cited$role <- "cited"
  paper$role <- "target"

  # relationships
  citing_rel <- tibble::tibble(
    from = rep(gsub("https://openalex.org/","",citing$id), lengths(citing$referenced_works)),
    to = gsub("https://openalex.org/","",unlist(citing$referenced_works)))
  citing_rel <- citing_rel[citing_rel$to %in% identifier,]
  cited_rel <- tibble::tibble(
    from = rep(gsub("https://openalex.org/","",paper$id), lengths(paper$referenced_works)),
    to = gsub("https://openalex.org/","",unlist(paper$referenced_works)))

  data <- rbind(citing, cited, paper)
  data <- data[!duplicated(data$id),]
  results <- list(relationships = rbind(citing_rel,cited_rel), data = data)
}
