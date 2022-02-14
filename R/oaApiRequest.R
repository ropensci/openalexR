#library(progress)
utils::globalVariables("progress_bar")
#' Get bibliographic records from OpenAlex databases
#'
#' It gets bibliographic records from OpenAlex database \href{https://openalex.org/}{https://openalex.org/}.
#' The function \code{oaApiRequest} queries OpenAlex database using a query formulated through the function \code{oaQueryBuild}.
#'
#' @param query_url is a character. It contains a search query formulated using the OpenAlex API language. A query can be automatically generated using the function \code{oaQueryBuild}.
#' @param total.count is a logical. If TRUE, the function returns only the number of item matching the query. Default is \code{total.count=FALSE}.
#' @param verbose is a logical. If TRUE, information about the querying process will be plotted on screen. Default is \code{verbose=FALSE}.
#'
#' @return a data.frame or a list.
#'
#' For more extensive information about OpenAlex API, please visit: \href{https://docs.openalex.org/api}{https://docs.openalex.org/api}
#'
#'
#' @examples
#'
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
#' identifier = "W2755950973",
#' entity = "works",
#' endpoint = "https://api.openalex.org/")
#'
#' res <- oaApiRequest(
#'    query_url = query_work,
#'    total.count = FALSE,
#'    verbose = FALSE
#'    )
#'
#' #  The author Massimo Aria is associated to the OpenAlex-id A923435168.
#'
#'
#' query_author <- oaQueryBuild(
#' identifier = "A923435168",
#' entity = "authors",
#' endpoint = "https://api.openalex.org/")
#'
#' res <- oaApiRequest(
#'    query_url = query_author,
#'    total.count = FALSE,
#'    verbose = FALSE
#'    )
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
#' query2 <- oaQueryBuild(
#' identifier=NULL,
#' entity = "works",
#' filter = "cites:W2755950973",
#' date_from = "2021-01-01",
#' date_to = "2021-12-31",
#' search=NULL,
#' sort="relevance_score:desc",
#' endpoint = "https://api.openalex.org/")
#'
#' res2 <- oaApiRequest(
#'    query_url = query2,
#'    total.count = FALSE,
#'    verbose = FALSE
#'    )
#'
#' ### EXAMPLE 3: All works matching a string in their title
#'
#' # Query to search all works containing the exact string
#' # "bibliometric analysis" OR "science mapping" in the title, published in 2020 or 2021.
#'
#' # Results have to be sorted by relevance score in a descending order.
#'
#'
#' query3 <- oaQueryBuild(
#'    identifier=NULL,
#'    entity = "works",
#'    filter = 'title.search:"bibliometric analysis"|"science mapping"',
#'    date_from = "2020-01-01",
#'    date_to = "2021-12-31",
#'    search=NULL,
#'    sort="relevance_score:desc",
#'    endpoint = "https://api.openalex.org/")
#'
#' res3 <- oaApiRequest(
#'    query_url = query3,
#'    total.count = FALSE,
#'    verbose = FALSE
#'    )
#'
#' ### EXAMPLE 4: How to check how many works match a query
#' # Query to search all works containing the exact string
#' # "bibliometric analysis" OR "science mapping" in the title, published in 2020 or 2021.
#'
#' # Quey only to know how many works could be retrieved (total.count=TRUE)
#'
#' query4 <- oaQueryBuild(
#'    identifier=NULL,
#'    entity = "works",
#'    filter = 'title.search:"bibliometric analysis"|"science mapping"',
#'    date_from = "2020-01-01",
#'    date_to = "2021-12-31",
#'    search=NULL,
#'    sort="relevance_score:desc",
#'    endpoint = "https://api.openalex.org/")
#'
#' res4 <- oaApiRequest(
#'    query_url = query4,
#'    total.count = TRUE,
#'    verbose = FALSE
#'    )
#'
#' res4$count #number of items retrieved by our query
#'}
#' @export
#'
oaApiRequest <- function(query_url,
                         #format = "data.frame",
                         total.count = FALSE,
                         verbose=FALSE){

  ua <- httr::user_agent(cfg()$user_agent)

  if (verbose == TRUE) message("Requesting url: ", query_url)


  res <- oa_request(query_url, ua)
  if (!is.null(res$meta)){
    ## return only item counting
    if (isTRUE(total.count)){
      return(res$meta)
    }
  } else {
    return(res)
  }

  ## count number of pages
  n_items <- res$meta$count
  n_pages <- ceiling(res$meta$count / res$meta$per_page)
  pages <- 1:n_pages
  ##

  if (n_items <= 0) return (list())

  if (n_items > 1e4)
    stop("A maximum of 10000 results can be paged, this query exceeds this limit.")

  if (verbose)
    message("About to get a total of ", length(pages), " pages of results",
            " with a total of ", n_items, " records.")

  pb <- progress::progress_bar$new(
    format = "  open alex resolving [:bar] :percent eta: :eta",
    total = length(pages), clear = FALSE, width = 60)

  data <- vector("list", length = length(pages))
  for (i in pages){
    pb$tick()
    Sys.sleep(1 / 100)
    query_url2 <- paste(query_url,"&page=",page=i, sep ="")
    res <- oa_request(query_url2, ua)
    if (!is.null(res$results)) data[[i]] <- res$results
  }

  data <- unlist(data, recursive = FALSE)


  return(data)


}


oa_request <- function(query_url,ua){

  res <- httr::GET(query_url, ua)

  if (httr::status_code(res) == 200) {

    if (httr::http_type(res) != "application/json") {
      stop("API did not return json", call. = FALSE)
    }

    data <- jsonlite::fromJSON(
      httr::content(res, as = "text", encoding = "utf-8"),
      simplifyVector = FALSE
    )

    return(data)
  }

  if (httr::status_code(res) == 429){
    message("HTTP status 429 Too Many Requests")
    return(list())
  }

  if (httr::status_code(res) != 429 & httr::status_code(res) != 200){
    message("HTTP status ", httr::status_code(res))
    return(list())
  }

}



cfg <- function(){

  res <- list(
    user_agent = "http://github.com/hadley/httr"
  )

  if (Sys.getenv("OPENALEX_USERAGENT") != "") {
    res$user_agent <- Sys.getenv("OPENALEX_USERAGENT")
  }
  return (res)
}


oaDataFrame <- function(res){
  name <- NULL
  data <-
    tibble::enframe(unlist(res)) %>%
    dplyr::mutate(name = gsub(".", "_", name, fixed = TRUE))
  return(data)
}
