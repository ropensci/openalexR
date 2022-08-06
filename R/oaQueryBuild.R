#' Generate an OpenAlex query from a set of parameters
#'
#' It generates a valid query, written following the OpenAlex API Language, from a set of parameters.
#'
#' @param identifier is a character. It indicates an item identifier.
#' @param entity is a character. It indicates the scholarly entity of the search. The argument can be one of
#' c("works", "authors", "venues", "institutions", "concepts"). The default value is entity = works".
#' @param filter is a character. Filters narrow the list down to just entities that meet a particular condition--specifically, a particular value for a particular attribute.
#' Filters are formatted thusly: attribute:value. The complete list of filter attributes for each entity can be found
#' at \href{https://docs.openalex.org/api/get-lists-of-entities#filter}{https://docs.openalex.org/api/get-lists-of-entities#filter}
#' @param date_from is a character. It indicates the starting date of the time-span. The format is YYYY-MM-DD. The default values is \code{date_from=NULL}.
#' @param date_to is a character. It indicates the ending date of the time-span. The format is YYYY-MM-DD. The default values is \code{date_from=NULL}.
#' @param search is a character. Search is just another kind of filter, one that all five endpoints support. But unlike the other filters, search doesn't require an exact match.
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
#'
#' query_work <- oaQueryBuild(
#'   identifier = "W2755950973",
#'   endpoint = "https://api.openalex.org/"
#' )
#'
#' res_work <- oaApiRequest(
#'   query_url = query_work,
#'   format = "list",
#'   total.count = FALSE,
#'   verbose = FALSE
#' )
#'
#' #  The author Massimo Aria is associated to the OpenAlex-id A923435168:
#'
#'
#' query_author <- oaQueryBuild(
#'   identifier = "A923435168",
#'   endpoint = "https://api.openalex.org/"
#' )
#'
#' res_author <- oaApiRequest(
#'   query_url = query_author,
#'   format = "list",
#'   total.count = FALSE,
#'   verbose = FALSE
#' )
#'
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
#' query1 <- oaQueryBuild(
#'   identifier = NULL,
#'   entity = "works",
#'   filter = "cites:W2755950973",
#'   date_from = "2021-01-01",
#'   date_to = "2021-12-31",
#'   search = NULL,
#'   endpoint = "https://api.openalex.org/"
#' )
#'
#' res1 <- oaApiRequest(
#'   query_url = query1,
#'   total.count = FALSE,
#'   verbose = FALSE
#' )
#'
#' ### EXAMPLE 3: All works matching a string in their title
#'
#' # Query to search all works containing the exact string
#' # "bibliometric analysis" OR "science mapping" in the title, published in 2020 or 2021.
#'
#' # Results have to be sorted by relevance score in a descending order.
#'
#'
#' query2 <- oaQueryBuild(
#'   identifier = NULL,
#'   entity = "works",
#'   filter = 'title.search:"bibliometric analysis"|"science mapping"',
#'   date_from = "2020-01-01",
#'   date_to = "2021-12-31",
#'   search = NULL,
#'   endpoint = "https://api.openalex.org/"
#' )
#'
#' res2 <- oaApiRequest(
#'   query_url = query2,
#'   format = "list",
#'   total.count = FALSE,
#'   verbose = FALSE
#' )
#'
#' ### EXAMPLE 4: How to check how many works match a query
#' # Query to search all works containing the exact string
#' # "bibliometric analysis" OR "science mapping" in the title, published in 2020 or 2021.
#'
#' # Quey only to know how many works could be retrieved (total.count=TRUE)
#'
#' query3 <- oaQueryBuild(
#'   identifier = NULL,
#'   entity = "works",
#'   filter = 'title.search:"bibliometric analysis"|"science mapping"',
#'   date_from = "2020-01-01",
#'   date_to = "2021-12-31",
#'   search = NULL,
#'   endpoint = "https://api.openalex.org/"
#' )
#'
#' res3 <- oaApiRequest(
#'   query_url = query3,
#'   format = "list",
#'   total.count = TRUE,
#'   verbose = FALSE
#' )
#'
#' res3$count # number of items retrieved by our query
#' }
#'
#' @export
#'

oaQueryBuild <- function(identifier = NULL, ## identifier of a work, author, venue, etc.
                         entity = c("works", "authors", "venues", "institutions", "concepts"),
                         filter = NULL,
                         date_from = NULL,
                         date_to = NULL,
                         search = NULL,
                         endpoint = "https://api.openalex.org/",
                         verbose = FALSE) {
  entity <- match.arg(entity)

  id <- c("NoMissing", "Missing")
  id <- id[is.null(identifier) + 1]

  switch(id,
    Missing = {
      if (is.null(filter) & (is.null(search))) {
        message("Identifier is missing, please specify filter or search argument.")
        return()
      }
      if (!is.null(date_from)) date_from <- paste(",from_publication_date:", date_from, sep = "")
      if (!is.null(date_to)) date_to <- paste(",to_publication_date:", date_to, sep = "")
      filter <- paste(filter, date_from, date_to, sep = "")
      path <- entity

      query <- list(
        filter = filter,
        search = search
      )
    },
    NoMissing = {
      path <- paste(entity, identifier, sep = "/")
      query <- NULL
    }
  )

  query_url <- httr::modify_url(
    endpoint,
    path = path,
    query = query
  )

  # if (id == "Missing") {
    # query_url <- paste(query_url,"&per-page=200",sep="")
  # }

  if (isTRUE(verbose)) print(query_url)

  return(query_url)
}
