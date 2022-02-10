#library(progress)
utils::globalVariables("progress_bar")
#' Get bibliographic records from OpenAlex databases
#' It gets bibliographic records from OpenAlex database \href{https://openalex.org/}{https://openalex.org/}.
#' The function \code{oaApiRequest} queries OpenAlex database using a query formulated through the function \code{oaQueryBuild}.
#'
#' @param query_url is a character. It contains a search query formulated using the OpenAlex API language. A query can be automatically generated using the function \code{oaQueryBuild}.
#' @param format is a character. It indicates the data format of the returned object. The argument can be \code{format=c("data.frame","list")}. Default is \code{format="data.frame"}.
#' @param overview
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
#' # Query to search all works citing the article:
#'   Aria, M., & Cuccurullo, C. (2017). bibliometrix:
#'   An R-tool for comprehensive science mapping analysis.
#'   Journal of informetrics, 11(4), 959-975.
#'
#'   published in 2021.
#'   The paper is associated to the OpenAlex id W2755950973.
#'
#'   Results have to be sorted by relevance score in a descending order.
#'
#' query <- oaQueryBuild(
#' identifier=NULL,
#' entity = "works",
#' filter="cites:W2755950973,publication_year:2021",
#' search=NULL,
#' sort="relevance_score:desc",
#' endpoint = "https://api.openalex.org/")
#'
#' res <- oaApiRequest(query,
#'                format = "data.frame",
#'                verbose=T)
#' }
#'
#'
#' @export
#'
oaApiRequest <- function(query_url,
                         format = "data.frame",
                         overview = FALSE,
                         verbose=FALSE){

  format_list = c("data.frame", "list")

  if (!(format[1] %in% format_list) | length(format)>1){
    cat("\nYou have selected an incorrect format.\nDefault value (format='data.frame' will be used)")
  }

  ua <- httr::user_agent(cfg()$user_agent)

  if (verbose == TRUE) message("Requesting url: ", query_url)


  res <- oa_request(query_url, ua)
  if (isTRUE(overview)) return(res$meta)
  if (attr(query_url,"identifier")=="NoMissing"){ ## return of single item by identifier
    if (format=="data.frame") {
      data <- oaDataFrame(res)
      return(data)
      } else {return(res)}
  }

  ## count number of pages
  n_pages <- ceiling(res$meta$count / res$meta$per_page)
  n_items <- res$meta$count
  pages <- 1:n_pages
  ##

  if (n_items <= 0) return (list())

  if (n_items > 1e4)
    stop("A maximum of 10000 results can be paged, this query exceeds that.")

  if (verbose)
    message("About to crawl a total of ", length(pages), " pages of results",
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

  if (format=="data.frame") {
    data <- oaDataFrame(data)
  }else{
    data <- unlist(data, recursive = FALSE)
  }

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
