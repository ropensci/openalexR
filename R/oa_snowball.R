#' A function to perform a snowball search
#' and convert the result to a tibble/data frame.
#' @param identifier Character vector of openalex identifiers.
#' @param ... Additional arguments to pass to `oa_fetch` when querying the
#' input works, such as `doi`.
#' @param id_type Type of OpenAlex IDs to return. Defaults to "short",
#' which remove the prefix https://openalex.org/ in the works' IDs,
#' for example, W2755950973.
#' If "original", the OpenAlex IDs are kept as are,
#' for example, https://openalex.org/W2755950973
#' @param citing_params parameters used in the search of works citing the input works.
#' @param cited_by_params parameters used in the search of works cited by the input works.
#' @inheritParams oa_fetch
#'
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
#'   citing_params = list(from_publication_date = "2022-01-01"),
#'   cited_by_params = list(),
#'   verbose = TRUE
#' )
#' }
oa_snowball <- function(identifier = NULL,
                        ...,
                        id_type = c("short", "original"),
                        mailto = oa_email(),
                        endpoint = "https://api.openalex.org",
                        verbose = FALSE,
                        citing_params = list(),
                        cited_by_params = list()) {
  id_type <- match.arg(id_type)
  base_args <- list(
    entity = "works",
    output = "tibble",
    endpoint = endpoint,
    mailto = mailto,
    verbose = verbose
  )

  # collecting records about the target papers
  paper <- fetch_snow(
    c(base_args, list(identifier = identifier)),
    list(...)
  )

  # fetching documents citing the target papers
  identifier <- shorten_oaid(paper$id)
  if (verbose) message("Collecting all documents citing the target papers...")
  citing <- suppressWarnings(
    fetch_snow(
      c(base_args, list(cites = identifier)),
      citing_params
    )
  )

  # fetching documents cited by the target papers
  if (verbose) message("Collecting all documents cited by the target papers...")
  cited <- suppressWarnings(
    fetch_snow(
      c(base_args, list(cited_by = identifier)),
      cited_by_params
    )
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
  nodes <- rbind_oa_ls(list(paper, citing, cited))
  nodes <- nodes[!duplicated(nodes$id), ]

  # relationships/edges
  edges <- rbind(citing_rel, cited_rel)
  # remove duplicates when two input identifiers cite each other
  edges <- edges[!duplicated(edges), ]
  # remove edges to/from NA nodes
  edges <- edges[stats::complete.cases(edges), ]
  # remove edges to missing nodes (ex: deleted works)
  edges <- edges[edges$from %in% nodes$id & edges$to %in% nodes$id, ]

  if (id_type == "short") {
    edges$to <- shorten_oaid(edges$to)
    edges$from <- shorten_oaid(edges$from)
    nodes$id <- shorten_oaid(nodes$id)
  }

  list(nodes = nodes, edges = edges)
}


fetch_snow <- function(args, filt){
  if (!is.null(filt$options$select)){
    # id and referenced_works is needed to find citing papers
    filt$options$select <- union(filt$options$select, c("id", "referenced_works"))
  }

  # collecting records about the target papers
  do.call(oa_fetch, c(args, filt))
}
