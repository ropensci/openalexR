#' A function to perform a snowball search
#' and convert the result to a tibble/data frame.
#' @param identifier Character. It indicates a vector of openalex_id identifiers.
#' @param id_type Type of OpenAlex IDs to return. Defaults to "short",
#' which remove the prefix https://openalex.org/ in the works' IDs,
#' for example, W2755950973.
#' If "original", the OpenAlex IDs are kept as are,
#' for example, https://openalex.org/W2755950973
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
#'   verbose = TRUE
#' )
#' }
oa_snowball <- function(identifier = NULL,
                        id_type = c("short", "original"),
                        mailto = oa_email(),
                        endpoint = "https://api.openalex.org/",
                        verbose = FALSE) {

  id_type <- match.arg(id_type)
  identifier <- shorten_oaid(identifier)

  # collecting records about the target papers
  paper <- oa_fetch(
    entity = "works",
    identifier = identifier,
    output = "tibble",
    endpoint = endpoint,
    mailto = mailto,
    verbose = verbose
  )

  # fetching all documents citing the target papers
  if (verbose) message("Collecting all documents citing the target papers...")
  citing <- oa_fetch(
    entity = "works",
    cites = identifier,
    output = "tibble",
    endpoint = endpoint,
    mailto = mailto,
    verbose = verbose
  )

  # collecting all documents cited by the target papers
  if (verbose) message("Collecting all documents cited by the target papers...")
  cited <- oa_fetch(
    entity = "works",
    cited_by = identifier,
    output = "tibble",
    endpoint = endpoint,
    mailto = mailto,
    verbose = verbose
  )

  # merging all documents in a single data frame
  if (is.null(citing)){
    citing <- paper[0, TRUE]
    citing_rel <- NULL
  } else {
    citing_rel <- tibble::tibble(
      from = rep(citing$id, lengths(citing$referenced_works)),
      to = unlist(citing$referenced_works)
    )
    citing_rel <- citing_rel[shorten_oaid(citing_rel$to) %in% identifier, ]
  }

  if (is.null(cited)){
    cited <- paper[0, TRUE]
    cited_rel <- NULL
  } else {
    cited_rel <- tibble::tibble(
      from = rep(paper$id, lengths(paper$referenced_works)),
      to = unlist(paper$referenced_works)
    )
  }

  citing$role <- "citing"
  cited$role <- "cited"
  paper$role <- "target"
  nodes <- rbind(citing, cited, paper)
  both_ids <- intersect(citing$id, cited$id)
  nodes[nodes$id %in% both_ids, "role"] <- "both"
  nodes <- nodes[!duplicated(nodes$id), ]

  # relationships/edges
  edges <- rbind(citing_rel, cited_rel)

  if (id_type == "short"){
    edges$to <- shorten_oaid(edges$to)
    edges$from <- shorten_oaid(edges$from)
    nodes$id <- shorten_oaid(nodes$id)
  }

  list(nodes = nodes, edges = edges)
}

#' Flatten snowball result
#'
#' |  id|title |...|cited_by_count| referenced_works   |cited_by |...|
#' | 100|foo   |...|             1| 98, 99             |101      |...|
#' | 200|bar   |...|             2| 198, 199           |201, 202 |...|
#' | 300|wug   |...|             2| 296, 297, 298, 299 |301, 302 |...|
#'
#' @param snowball List result from `oa_snowball`.
#'
#' @return Tibble/data.frame of works with additional `cited_by` column.
#' @export
#'
#' @examples
#' \dontrun{
#' flat_snow <- to_disk(oa_snowball(
#'   identifier = "W1516819724",
#'   verbose = TRUE
#' ))
#'
#' flat_snow[, c("id", "cited_by")]
#' }
to_disk <- function(snowball){
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop(
      "Package \"dplyr\" must be installed to use this function.",
      call. = FALSE
    )
  }
  `%>%` <- dplyr::`%>%`
  .data <- dplyr::.data

  nodes <- snowball$nodes
  ids <- nodes[nodes$role == "target", "id", drop = TRUE]
  collapse_citations <- snowball$edges %>%
    dplyr::filter(.data$to %in% ids) %>%
    dplyr::group_by(id = .data$to) %>%
    dplyr::summarise(
      cited_by = paste(.data$from, collapse = ", "),
      .groups = "drop"
    )

  snowball$nodes %>%
    dplyr::left_join(collapse_citations, by = "id")
}
