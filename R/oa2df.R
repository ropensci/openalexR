#' Convert OpenAlex collection from list to data frame
#'
#' It converts bibliographic collections gathered from OpenAlex database \href{https://openalex.org/}{https://openalex.org/} into data frame.
#' The function converts a collection of records about works, authors, institutions, venues or concepts obtained using \code{oa_request} into a data frame/tibble.
#'
#' @param data is a list. data is the output of the function \code{oa_request}.
#' @param entity is a character. It indicates the scholarly entity of the search. The argument can be equal to
#' entity = c("works", "authors", "venues", "institutions", "concepts"). The default value is entity = works".
#' @param abstract Logical. If TRUE, the function returns also the abstract of each item. Default is \code{abstract=TRUE}.
#' The argument is ignored if entity is different from "works".
#' @param count_only Logical. If TRUE, the function returns only the number of item matching the query. Default is \code{count_only=FALSE}.
#' @param group_by Character. Property to group by.
#' For example: "oa_status" for works.
#' https://docs.openalex.org/api/get-groups-of-entities
#' @param verbose is a logical. If TRUE, information about the querying process will be plotted on screen. Default is \code{verbose=TRUE}.
#' @return a data.frame.
#'
#' For more extensive information about OpenAlex API, please visit: \href{https://docs.openalex.org/api}{https://docs.openalex.org/api}
#'
#'
#' @examples
#' \dontrun{
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
#' query <- oa_query(
#'   entity = "works",
#'   cites = "W2755950973",
#'   from_publication_date = "2021-01-01",
#'   to_publication_date = "2021-04-30"
#' )
#'
#' res <- oa_request(
#'   query_url = query,
#'   count_only = FALSE,
#'   verbose = FALSE
#' )
#'
#' oa2df(res, entity = "works")
#'
#' }
#'
#' @export
oa2df <- function(data, entity, abstract = TRUE, count_only = FALSE, group_by = NULL, verbose = TRUE) {
  if (!is.null(group_by)){
    return(do.call(rbind.data.frame, data))
  }

  if (count_only && length(data) == 4) return(unlist(data))

  switch(entity,
    works = oaWorks2df(data, abstract, verbose),
    authors = oaAuthors2df(data, verbose),
    institutions = oaInstitutions2df(data, verbose),
    venues = oaVenues2df(data, verbose),
    concepts = oaConcepts2df(data, verbose)
  )
}
