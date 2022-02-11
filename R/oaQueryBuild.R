#' Generate an OpenAlex query from a set of parameters
#'
#' It generates a valid query, written following the OpenAlex API Language, from a set of parameters.
#'
#' @param identifier is a character. It indicates an item identifier.
#' @param entity is a character. It indicates the scholarly entity of the search. The argument can be equal to
#' entity = c("works", "authors", "venues", "institutions", "concepts"). The default value is entity = works".
#' @param filter is a character. Filters narrow the list down to just entities that meet a particular condition--specifically, a particular value for a particular attribute.
#' Filters are formatted thusly: attribute:value. The complete list of filter attributes for each entity can be found
#' at \href{https://docs.openalex.org/api/get-lists-of-entities#filter}{https://docs.openalex.org/api/get-lists-of-entities#filter}
#' @param date_from is a character. It indicates the starting date of the time-span. The format is YYYY-MM-DD. The default values is \code{date_from=NULL}.
#' @param date_to is a character. It indicates the ending date of the time-span. The format is YYYY-MM-DD. The default values is \code{date_from=NULL}.
#' @param search is a character. Search is just another kind of filter, one that all five endpoints support. But unlike the other filters, search doesn't require an exact match.
#' To filter using search, append .search to the end of the property you're filtering for.
#' @param sort is character. Use the sort parameter to specify the property you want your list sorted by.
#' You can sort by these properties, where they exist:display_name, cited_by_count, works_count, publication_date, relevance_score.
#' @param endpoint is character. It indicates the url of the OpenAlex Endpoint API server. The default value is endpoint = "https://api.openalex.org/"
#'
#' @return a character containing the query in OpenAlex format.
#'
#' For more extensive information about OpenAlex API, please visit: \href{https://docs.openalex.org/api}{https://docs.openalex.org/api}
#'
#'
#' @examples
#'
#' \dontrun{
#' ### FIRST EXAMPLE
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
#'
#' ### SECOND EXAMPLE
#' # Query to search all works containing the exact string
#' # "bibliometric analysis" OR "science mapping" in the title, published in 2020 or 2021.
#'
#' # Results have to be sorted by relevance score in a descending order.
#'
#' query <- oaQueryBuild(
#'    identifier=NULL,
#'    entity = "works",
#'    filter = 'title.search:"bibliometric analysis"|"science mapping"',
#'    date_from = "2020-01-01",
#'    date_to = "2021-12-31",
#'    search=NULL,
#'    sort="relevance_score:desc",
#'    endpoint = "https://api.openalex.org/")
#' }
#'
#'
#'
#' @export
#'

oaQueryBuild <- function(
  identifier = NULL, ## identifier of a work, author, venue, etc.
  entity = "works",
  filter=NULL,
  date_from=NULL,
  date_to=NULL,
  search=NULL,
  sort=NULL,
  endpoint = "https://api.openalex.org/") {

  entity_list = c("works", "authors", "venues", "institutions", "concepts")
  format_list = c("table", "object")

  # if (!(format[1] %in% format_list) | length(format)>1){
  #   cat("\nYou have selected an incorrect format.\nDefault value (format='table' will be used)")
  # }


  if (!(entity[1] %in% entity_list)|length(entity)>1){
    cat("\nPlease choose a single entity value from the following list:\n",
        entity_list)
    return("error")
  }

  id <- c("NoMissing","Missing")
  id <- id[(is.null(identifier))+1]

  if (!is.null(date_from)) date_from = paste(",from_publication_date:",date_from,sep="")
  if (!is.null(date_to)) date_to = paste(",to_publication_date:",date_to,sep="")
  filter <- paste(filter,date_from,date_to,sep="")

  switch(id,
         Missing={
           if (is.null(filter) & (is.null(search))) {
             message("Identifier is missing, please specify filter or search argument.")
             return()
           }
          path = entity
          query <- list(
             filter = filter,
             search = search,
             sort = sort
           )
          },
        NoMissing={
           path <- sprintf("%s/%s", entity, identifier)
           query = NULL
         }
         )

  query_url <- httr::modify_url(
    endpoint,
    path = path,
    query = query
  )

  if (id=="Missing") {
    query_url <- paste(query_url,"&per-page=50",sep="")
  }else{
    query_url <- paste(query_url,"&per-page=1",sep="")
  }


  attr(query_url,"identifier") <- id

  return(query_url)

}
