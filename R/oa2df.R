#' Convert OpenAlex collection from list to data frame
#'
#' It converts bibliographic collections gathered from OpenAlex database \href{https://openalex.org/}{https://openalex.org/} into data frame.
#' The function converts a collection of records about works, authors, institutions, venues or concepts obtained using \code{oa_request} into a data frame/tibble.
#'
#' @param data List. Output of \code{oa_request}.
#' @param entity Character. Scholarly entity of the search.
#' The argument can be one of c("works", "authors", "venues", "institutions", "concepts").
#' @param abstract Logical. If TRUE, the function returns also the abstract of each item.
#' Ignored if entity is different from "works". Defaults to TRUE.
#' @inheritParams oa_query
#' @inheritParams oa_request
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
