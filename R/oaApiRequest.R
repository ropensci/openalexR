# library(progress)
utils::globalVariables("progress_bar")


#' A composition function to perform query building, requesting,
#' and convert the result to a tibble/data frame.
#' @inheritParams oa_query
#' @inheritParams oa_request
#'
#' @return A data.frame or a list. Result of the query.
#' @export
#'
#' @examples
#' \dontrun{
#' paper_meta <- oa_fetch(
#'   identifier = "W2755950973",
#'   entity = "works",
#'   endpoint = "https://api.openalex.org/",
#'   count_only = TRUE,
#'   verbose = TRUE
#' )
#' }
oa_fetch <- function(...,
                     identifier = NULL, ## identifier of a work, author, venue, etc.
                     entity = c("works", "authors", "venues", "institutions", "concepts"),
                     search = NULL,
                     sort = NULL,
                     group_by = NULL,
                     endpoint = "https://api.openalex.org/",
                     per_page = 200,
                     count_only = FALSE,
                     mailto = NULL,
                     verbose = FALSE) {
  entity <- match.arg(entity)
  print(entity)
  oa2df(
    oa_request(
      oa_query(
        ...,
        identifier = identifier,
        entity = entity,
        search = search,
        sort = sort,
        group_by = group_by,
        endpoint = endpoint,
        verbose = verbose
      ),
      per_page = per_page,
      count_only = count_only,
      mailto = mailto,
      verbose = verbose
    ),
    entity = entity
  )
}


#' Get bibliographic records from OpenAlex databases
#'
#' It gets bibliographic records from OpenAlex database \href{https://openalex.org/}{https://openalex.org/}.
#' The function \code{oa_request} queries OpenAlex database using a query formulated through the function \code{oa_query}.
#'
#' @param query_url is a character. It contains a search query formulated using the OpenAlex API language. A query can be automatically generated using the function \code{oa_query}.
#' @param per_page is a numeric. It indicates how many items to download per page. The per-page argument can assume any number between 1 and 200. Default is \code{per_page=200}.
#' @param count_only is a logical. If TRUE, the function returns only the number of item matching the query. Default is \code{count_only=FALSE}.
#' @param mailto is a character. To get into the polite pool, the arguments mailto have to give OpenAlex an email where they can contact you.
#' @param verbose is a logical. If TRUE, information about the querying process will be plotted on screen. Default is \code{verbose=FALSE}.
#'
#' @return a data.frame or a list.
#'
#' For more extensive information about OpenAlex API, please visit: \href{https://docs.openalex.org/api}{https://docs.openalex.org/api}
#'
#'
#' @examples
#' \dontrun{
#'
#' ### EXAMPLE 1: Full record about an entity.
#'
#' # Query to obtain all information about a particular work/author/institution/etc.:
#'
#' #  The following paper is associated to the OpenAlex-id W2755950973.
#'
#' #  Aria, M., & Cuccurullo, C. (2017). bibliometrix:
#' #   An R-tool for comprehensive science mapping analysis.
#' #   Journal of informetrics, 11(4), 959-975.
#'
#'
#' query_work <- oa_query(
#'   identifier = "W2755950973",
#'   entity = "works",
#'   endpoint = "https://api.openalex.org/"
#' )
#'
#' res <- oa_request(
#'   query_url = query_work,
#'   count_only = FALSE,
#'   verbose = FALSE
#' )
#'
#' #  The author Massimo Aria is associated to the OpenAlex-id A923435168.
#'
#'
#' query_author <- oa_query(
#'   identifier = "A923435168",
#'   entity = "authors",
#'   endpoint = "https://api.openalex.org/"
#' )
#'
#' res <- oa_request(
#'   query_url = query_author,
#'   count_only = FALSE,
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
#' query2 <- oa_query(
#'   identifier = NULL,
#'   entity = "works",
#'   filter = "cites:W2755950973",
#'   from_publication_date = "2021-01-01",
#'   to_publication_date = "2021-12-31",
#'   search = NULL,
#'   endpoint = "https://api.openalex.org/"
#' )
#'
#' res2 <- oa_request(
#'   query_url = query2,
#'   count_only = FALSE,
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
#' query3 <- oa_query(
#'   identifier = NULL,
#'   entity = "works",
#'   filter = 'title.search:"bibliometric analysis"|"science mapping"',
#'   from_publication_date = "2020-01-01",
#'   to_publication_date = "2021-12-31",
#'   search = NULL,
#'   endpoint = "https://api.openalex.org/"
#' )
#'
#' res3 <- oa_request(
#'   query_url = query3,
#'   count_only = FALSE,
#'   verbose = FALSE
#' )
#'
#' ### EXAMPLE 4: How to check how many works match a query
#' # Query to search all works containing the exact string
#' # "bibliometric analysis" OR "science mapping" in the title, published in 2020 or 2021.
#'
#' # Query only to know how many works could be retrieved (count_only=TRUE)
#'
#' query4 <- oa_query(
#'   identifier = NULL,
#'   entity = "works",
#'   filter = 'title.search:"bibliometric analysis"|"science mapping"',
#'   from_publication_date = "2020-01-01",
#'   to_publication_date = "2021-12-31",
#'   search = NULL,
#'   endpoint = "https://api.openalex.org/"
#' )
#'
#' res4 <- oa_request(
#'   query_url = query4,
#'   count_only = TRUE,
#'   verbose = FALSE
#' )
#'
#' res4$count # number of items retrieved by our query
#' }
#' @export
#'
oa_request <- function(query_url,
                       per_page = 200,
                       count_only = FALSE,
                       mailto = NULL,
                       verbose = FALSE) {
  ua <- httr::user_agent(cfg()$user_agent)

  # building query...
  # first, download info about n. of items returned by the query
  query_ls <- list("per-page" = 1) # TODO did we mean for this to be 1???

  if (!is.null(mailto)) {
    if (isValidEmail(mailto)) {
      query_ls[["mailto"]] <- mailto
    } else {
      message(mailto, " is not a valid email address")
    }
  }

  if (verbose == TRUE) message("Requesting url: ", query_url)
  res <- api_request(query_url, ua, query = query_ls)

  if (!is.null(res$meta)) {
    ## return only item counting
    if (count_only) {
      return(res$meta)
    }
  } else {
    return(res)
  }

  ## number of pages
  n_items <- res$meta$count
  n_pages <- ceiling(res$meta$count / per_page)
  pages <- seq.int(n_pages)
  ##

  if (n_items <= 0) {
    return(list())
  }

  if (verbose) {
    message(
      "About to get a total of ", n_pages, " pages of results",
      " with a total of ", n_items, " records."
    )
  }

  pb <- progress::progress_bar$new(
    format = "  OpenAlex downloading [:bar] :percent eta: :eta",
    total = length(pages), clear = FALSE, width = 60
  )

  # Setting items per page
  query_ls[["per-page"]] <- per_page

  # Activation of cursor pagination
  cursor <- "*" # cursor request
  data <- vector("list", length = n_pages)
  for (i in pages) {
    if (verbose) pb$tick()
    Sys.sleep(1 / 100)
    query_ls[["cursor"]] <- cursor
    res <- api_request(query_url, ua, query = query_ls)
    cursor <- res$meta$next_cursor # next cursor
    if (!is.null(res$results)) data[[i]] <- res$results
  }

  unlist(data, recursive = FALSE)
}


api_request <- function(query_url, ua, query = query) {
  res <- httr::GET(query_url, ua, query = query)

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

  if (httr::status_code(res) == 429) {
    message("HTTP status 429 Too Many Requests")
    return(list())
  }

  if (httr::status_code(res) != 429 & httr::status_code(res) != 200) {
    message("HTTP status ", httr::status_code(res))
    return(list())
  }
}


cfg <- function(.ua = base::getOption("HTTPUserAgent")) {
  ## >> maybe something like this
  if (is.null(.ua) || length(.ua) == 0L) {
    .ua <-
      paste0(
        "curl/",
        curl::curl_version()[[1]],
        " RCurl/",
        utils::packageVersion("RCurl"),
        " httr/",
        utils::packageVersion("httr")
      )
  }

  res <- list(user_agent = .ua)

  if (Sys.getenv("OPENALEX_USERAGENT") != "") {
    res$user_agent <- Sys.getenv("OPENALEX_USERAGENT")
  }
  res
}
