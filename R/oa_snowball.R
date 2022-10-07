#' A function to perform a snowball search
#' and convert the result to a tibble/data frame.
#' @param identifier Character vector of openalex_id identifiers.
#' @param id_type Type of OpenAlex IDs to return. Defaults to "short",
#' which remove the prefix https://openalex.org/ in the works' IDs,
#' for example, W2755950973.
#' If "original", the OpenAlex IDs are kept as are,
#' for example, https://openalex.org/W2755950973
#' @inheritParams oa_fetch
#' @param ... Additional arguments to pass to `oa_fetch`, e.g., additional filters
#' these arguments are only used in the search of articles cite and are cited by
#' the given articles in `identifiers`.
#'
#' @return A list containing 2 elements:
#' - nodes: dataframe with publication records.
#' The last column `oa_input` indicates whether the work was
#' one of the input `identifier`(s).
#' - edges: publication link dataframe of 2 columns `from, to`
#' such that a row `A, B` means A -> B means A cites B.
#' In bibliometrics, the "citation action" comes from A to B.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' snowball_docs <- oa_snowball(
#'   identifier = c("W2741809807", "W2755950973"),
#'   from_publication_date = "2022-01-01",
#'   verbose = TRUE
#' )
#' }
oa_snowball <- function(identifier = NULL,
                        id_type = c("short", "original"),
                        mailto = oa_email(),
                        endpoint = "https://api.openalex.org/",
                        verbose = FALSE,
                        ...) {
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
    verbose = verbose,
    ...
  )

  # collecting all documents cited by the target papers
  if (verbose) message("Collecting all documents cited by the target papers...")
  cited <- oa_fetch(
    entity = "works",
    cited_by = identifier,
    output = "tibble",
    endpoint = endpoint,
    mailto = mailto,
    verbose = verbose,
    ...
  )

  # merging all documents in a single data frame
  if (is.null(citing)) {
    citing <- paper[0, TRUE]
    citing_rel <- NULL
  } else {
    citing_rel <- tibble::tibble(
      from = rep(citing$id, lengths(citing$referenced_works)),
      to = unlist(citing$referenced_works)
    )
    citing_rel <- citing_rel[shorten_oaid(citing_rel$to) %in% identifier, ]
  }

  if (is.null(cited)) {
    cited <- paper[0, TRUE]
    cited_rel <- NULL
  } else {
    cited_rel <- tibble::tibble(
      from = rep(paper$id, lengths(paper$referenced_works)),
      to = unlist(paper$referenced_works)
    )
  }

  citing$oa_input <- FALSE
  cited$oa_input <- FALSE
  paper$oa_input <- TRUE
  nodes <- rbind(paper, citing, cited)
  nodes <- nodes[!duplicated(nodes$id), ]

  # relationships/edges
  edges <- rbind(citing_rel, cited_rel)
  # remove duplicates when two input identifiers cite each other
  edges <- edges[!duplicated(edges), ]

  if (id_type == "short") {
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
#' @return Tibble/data.frame of works with additional columns:
#' append `citing`, `backward_count`, `cited_by`, `forward_count`, `connection`,
#' and `connection_count.` For each work/row, these counts are WITHIN one
#' snowball search, and so `forward_count` <= `cited_by_count`.
#'
#' Consider the universe of all works linked to a set of starting works, (`oa_input = TRUE`)
#' for each work/row i:
#' - citing: works in the universe that i cites
#' - backward_count: number of works in the universe that i cites
#' - cited_by: works that i is cited by
#' - forward_count: number of works in the universe that i is cited by
#' - connection: works in the universe linked to i
#' - connection_count: number of works in the universe linked to i (degree of i)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' flat_snow <- to_disk(oa_snowball(
#'   identifier = "W1516819724",
#'   verbose = TRUE
#' ))
#'
#' flat_snow[, c("id", "connection", "connection_count")]
#' }
to_disk <- function(snowball) {
  nodes <- snowball$nodes
  ids <- nodes$id[nodes$oa_input]
  edges_df <- snowball$edges

  citing <- do.call(rbind.data.frame, by(
    edges_df, list(edges_df$from),
    function(x) {
      list(
        id = unique(x$from),
        citing = paste(x$to, collapse = ";"),
        backward_count = nrow(x)
      )
    }
  ))

  cited_by <- do.call(rbind.data.frame, by(
    edges_df, list(edges_df$to),
    function(x) {
      list(
        id = unique(x$to),
        cited_by = paste(x$from, collapse = ";"),
        forward_count = nrow(x)
      )
    }
  ))

  nodes_augmented <- merge(
    merge(nodes, citing, all.x = TRUE),
    cited_by, all.x = TRUE
  )

  nodes_augmented$connection <- apply(
    nodes_augmented[, c("citing", "cited_by")], 1,
    function(x) paste(x[!is.na(x)], collapse = ";")
  )

  nodes_augmented[is.na(nodes_augmented$backward_count), "backward_count"] <- 0
  nodes_augmented[is.na(nodes_augmented$forward_count), "forward_count"] <- 0
  nodes_augmented$connection_count <-
    nodes_augmented$backward_count + nodes_augmented$forward_count

  nodes_augmented
}
