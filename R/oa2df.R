#' Convert OpenAlex collection from list to data frame
#'
#' It converts bibliographic collection gathered from OpenAlex database \href{https://openalex.org/}{https://openalex.org/} into data frame.
#' The function converts a collection of records about works, authors, institutions, venues or concepts obtained using \code{oaApiRequest} into a data frame/tibble.
#'
#' @param data is a list. data is the output of the function \code{oaApiRequest}.
#' @param entity is a character. It indicates the scholarly entity of the search. The argument can be equal to
#' entity = c("works", "authors", "venues", "institutions", "concepts"). The default value is entity = works".
#' @return a data.frame.
#'
#' For more extensive information about OpenAlex API, please visit: \href{https://docs.openalex.org/api}{https://docs.openalex.org/api}
#'
#'
#' @examples
#'
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
#' query <- oaQueryBuild(
#' identifier=NULL,
#' entity = "works",
#' filter = "cites:W2755950973",
#' date_from = "2021-01-01",
#' date_to = "2021-12-31",
#' search=NULL,
#' sort="relevance_score:desc",
#' endpoint = "https://api.openalex.org/")
#'
#' res <- oaApiRequest(
#'    query_url = query,
#'    total.count = FALSE,
#'    verbose = FALSE
#'    )
#'
#' df <- oa2df(res, entity="works")
#'
#' df
#'
#' }
#'
#' @export
oa2df <- function(data, entity="works"){
  entity_list = c("works", "authors", "venues", "institutions", "concepts")
  if (!(entity %in% entity_list)){
    message('Please choose one of the following entity type: "works", "authors", "venues", "institutions", "concepts"')
    return()
  }
  switch(entity,
         works={
           df <- oaWorks2df(data)
         },
         authors={
           df <- oaAuthors2df(data)
         },
         institutions={
           df <- oaInstitutions2df(data)
         },
         venues={
           df <- oaVenues2df(data)
         },
         concepts={
           df <- oaConcepts2df(data)
         }
  )

}
