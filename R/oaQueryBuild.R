#' Generate an OpenAlex query from a set of parameters
#'
#' It generates a valid query, written following the OpenAlex API Language, from a set of parameters.
#'
#' @param identifier Character. It indicates an item identifier.
#' @param entity Character. It indicates the scholarly entity of the search.
#' The argument can be one of c("works", "authors", "venues", "institutions", "concepts").
#' If not provided, `entity` is guessed from `identifier`.
#' @param \dots Filter arguments. Filters narrow the list down to just entities that meet a particular condition--specifically, a particular value for a particular attribute.
#' Filters are formatted as attribute = value. The complete list of filter attributes for each entity can be found
#' For example, `cited_by_count = ">100"`,
#' `title.search = c("bibliometric analysis", "science mapping")`,
#' or `to_publication_date = "2021-12-31"`.
#' at \href{https://docs.openalex.org/api/get-lists-of-entities#filter}{https://docs.openalex.org/api/get-lists-of-entities#filter}
#' @param sort Character. Property to sort by.
#' For example: "display_name" for venues or "cited_by_count:desc" for works.
#' See more at <https://docs.openalex.org/api/get-lists-of-entities/sort-entity-lists>.
#' @param group_by Character. Property to group by.
#' For example: "oa_status" for works.
#' https://docs.openalex.org/api/get-groups-of-entities
#' @param search Character. Search is just another kind of filter, one that all five endpoints support. But unlike the other filters, search doesn't require an exact match.
#' To filter using search, append .search to the end of the property you're filtering for.
#' @param endpoint is character. It indicates the url of the OpenAlex Endpoint API server. The default value is endpoint = "https://api.openalex.org/".
#' @param verbose is a logical. If TRUE, information about the querying process will be plotted on screen. Default is \code{verbose=FALSE}.
#'
#' @return a character containing the query in OpenAlex format.
#'
#' For more extensive information about OpenAlex API, please visit: \href{https://docs.openalex.org/api}{https://docs.openalex.org/api}
#'
#'
#' @examples
#' \dontrun{
#'
#' query_auth <- oa_query(identifier = "A923435168", verbose = TRUE)
#'
#' ### EXAMPLE 1: Full record about an entity.
#'
#' # Query to obtain allinformation about a particular work/author/institution/etc.:
#'
#' #  The following paper is associated to the OpenAlex-id W2755950973.
#'
#' #  Aria, M., & Cuccurullo, C. (2017). bibliometrix:
#' #   An R-tool for comprehensive science mapping analysis.
#' #   Journal of informetrics, 11(4), 959-975.
#'
#' query_work <- oa_query(
#'   identifier = "W2755950973",
#'   endpoint = "https://api.openalex.org/",
#'   verbose = TRUE
#' )
#'
#'
#' #  The author Massimo Aria is associated to the OpenAlex-id A923435168:
#'
#' query_auth <- oa_query(identifier = "A923435168", verbose = TRUE)
#'
#'
#' ### EXAMPLE 2: all works citing a particular work.
#'
#' # Query to search all works citing the article:
#' #  Aria, M., & Cuccurullo, C. (2017). bibliometrix:
#' #   An R-tool for comprehensive science mapping analysis.
#' #   Journal of informetrics, 11(4), 959-975.
#'
#' #  published in 2021.
#' #  The paper is associated to the OpenAlex id W2755950973.
#'
#' #  Results have to be sorted by relevance score in a descending order.
#'
#' query1 <- oa_query(
#'   entity = "works",
#'   cites = "W2755950973",
#'   from_publication_date = "2021-01-01",
#'   to_publication_date = "2021-12-31",
#'   verbose = TRUE
#' )
#'
#' ### EXAMPLE 3: All works matching a string in their title
#'
#' # Query to search all works containing the exact string
#' # "bibliometric analysis" OR "science mapping" in the title, published in the first half of 2021.
#'
#' # Results have to be sorted by relevance score in a descending order.
#'
#' query2 <- oa_query(
#'   entity = "works",
#'   title.search = c("bibliometric analysis", "science mapping"),
#'   from_publication_date = "2021-01-01",
#'   to_publication_date = "2021-06-30",
#'   sort = "cited_by_count:desc",
#'   verbose = TRUE
#' )
#' }
#'
#' @export
#'

oa_query <- function(...,
                     identifier = NULL, ## identifier of a work, author, venue, etc.
                     entity = id_type(identifier[[1]]),
                     search = NULL,
                     sort = NULL,
                     group_by = NULL,
                     endpoint = "https://api.openalex.org/",
                     verbose = FALSE) {

  filter <- list(...)
  multiple_id <- length(identifier) > 1

  # if multiple identifiers are provided, use openalex_id as a filter property
  # TODO openalex_id -> openalex
  if (multiple_id) filter <- c(filter, list(openalex_id = identifier))

  if (length(filter) > 0 || multiple_id) {
    flt_ready <- mapply(append_flt, filter, names(filter))
    flt_ready[sapply(flt_ready, is.null)] <- NULL
    flt_ready <- paste0(flt_ready, collapse = ",")
  } else {
    flt_ready <- list()
  }

  if (is.null(identifier) || multiple_id) {
    if (is.null(filter) & (is.null(search))) {
      message("Identifier is missing, please specify filter or search argument.")
      return()
    }

    path <- entity
    query <- list(
      filter = flt_ready,
      search = search,
      sort = sort,
      group_by = group_by
    )
  } else {
    path <- paste(entity, identifier, sep = "/")
    query <- NULL
  }

  query_url <- httr::modify_url(
    endpoint,
    path = path,
    query = query
  )

  if (verbose) print(query_url)

  query_url
}
