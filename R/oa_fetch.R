#' Available entities in the OpenAlex database
#'
#' @return Character vector of 5 entity options.
#' @export
#' @examples
#' oa_entities()
oa_entities <- function() {
  c(
    "works", "authors", "institutions", "concepts",
    "funders", "sources", "publishers"
  )
}

#' Fetching records
#'
#' A composition function to perform query building, requesting,
#' and convert the result to a tibble/data frame.
#' @inheritParams oa_query
#' @inheritParams oa_request
#' @param abstract Logical. If TRUE, the function returns also the abstract of each item.
#' Default to \code{abstract = TRUE}.
#' The argument is ignored if entity is different from "works".
#' @param output Character. Type of output, either a list or a tibble/data.frame.
#'
#' @return A data.frame or a list. Result of the query.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' paper_meta <- oa_fetch(
#'   identifier = "W2755950973",
#'   entity = "works",
#'   count_only = TRUE,
#'   abstract = TRUE,
#'   verbose = TRUE
#' )
#'
#' oa_fetch(
#'   entity = "works",
#'   doi = c(
#'     "10.1371/journal.pone.0266781",
#'     "10.1371/journal.pone.0267149"
#'   ),
#'   verbose = TRUE,
#'   count_only = TRUE
#' )
#'
#' oa_fetch(
#'   entity = "works",
#'   doi = c(
#'     "10.1371/journal.pone.0266781",
#'     "10.1371/journal.pone.0267149"
#'   ),
#'   options = list(select = c("doi", "id", "cited_by_count", "type")),
#'   verbose = TRUE
#' )
#'
#' oa_fetch(
#'   identifier = c("A5069892096", "A5023888391"),
#'   verbose = TRUE
#' )
#' }
oa_fetch <- function(entity = if (is.null(identifier)) NULL else id_type(shorten_oaid(identifier[[1]])),
                     identifier = NULL,
                     ...,
                     options = NULL,
                     search = NULL,
                     group_by = NULL,
                     output = c("tibble", "dataframe", "list"),
                     abstract = TRUE,
                     endpoint = "https://api.openalex.org",
                     per_page = 200,
                     paging = NULL,
                     pages = NULL,
                     count_only = FALSE,
                     mailto = oa_email(),
                     api_key = oa_apikey(),
                     verbose = FALSE) {
  output <- match.arg(output)
  entity <- match.arg(entity, oa_entities())

  if (output == "dataframe") output <- "tibble"
  filter <- list(...)

  # if multiple identifiers are provided, use openalex or doi as a filter attribute
  multiple_id <- length(identifier) > 1
  if (multiple_id) filter <- c(filter, list(openalex = identifier))

  # overcome OA limitation of combining 50 values (OR) for a filter at a time
  # https://docs.openalex.org/how-to-use-the-api/get-lists-of-entities/filter-entity-lists#addition-or
  # here, we assume there is only ONE "large" filter
  large_filter <- which(lengths(filter) > 50)
  if (length(large_filter) == 0) {
    list_id <- list(`1` = NULL)
  } else {
    list_id <- split(
      filter[[large_filter]],
      ceiling(seq_along(filter[[large_filter]]) / 50)
    )
  }

  if (!is.null(options$sample) && (options$sample > per_page)) {
    paging <- "page"
  } else if (!is.null(options$page)) {
    paging <- "page"
  } else if (is.null(paging)) {
    paging <- "cursor"
  }

  final_res <- list()
  for (i in seq_along(list_id)) {
    filter_i <- filter
    if (length(large_filter) > 0) {
      filter_i[[large_filter]] <- list_id[[i]]
    }

    Sys.sleep(1 / 10)
    final_res[[i]] <- oa_request(
      oa_query(
        filter = filter_i,
        multiple_id = multiple_id,
        identifier = identifier,
        entity = entity,
        search = search,
        options = options,
        group_by = group_by,
        endpoint = endpoint,
        verbose = verbose
      ),
      per_page = per_page,
      paging = paging,
      pages = pages,
      count_only = count_only,
      mailto = mailto,
      api_key = api_key,
      verbose = verbose
    )
  }

  if (length(final_res[[1]]) == 0) { # || is.null(final_res[[1]][[1]]$id)
    return(NULL)
  }
  final_res <- unlist(final_res, recursive = FALSE)

  if (output == "list") {
    return(final_res)
  }

  # Flatten out the initial chunking of 50 at a time
  final_res <- list(final_res)
  do.call(rbind, lapply(
    final_res, oa2df,
    entity = entity, options = options, abstract = abstract,
    count_only = count_only, group_by = group_by,
    verbose = verbose
  ))
}

#' Get bibliographic records from OpenAlex database
#'
#' `oa_request` makes a request and downloads bibliographic records from
#' OpenAlex database \href{https://openalex.org/}{https://openalex.org/}.
#' The function \code{oa_request} queries OpenAlex database using a query
#' formulated through the function \code{oa_query}.
#'
#' @param query_url Character string.
#' A search query formulated using the OpenAlex API language and
#' can be generated with \code{oa_query}.
#' @param per_page Numeric. Number of items to download per page.
#' The per-page argument can assume any number between 1 and 200.
#' Defaults to 200.
#' @param paging Character.
#' Either "cursor" for cursor paging or "page" for basic paging.
#' When used with `options$sample` and or `pages`,
#' paging is also automatically set to basic paging: `paging = "page"`
#' to avoid duplicates and get the right page.
#' See https://docs.openalex.org/how-to-use-the-api/get-lists-of-entities/paging.
#' @param pages Integer vector.
#' The range of pages to return. If NULL, return all pages.
#' @param count_only Logical.
#' If TRUE, the function returns only the number of item matching the query.
#' Defaults to FALSE.
#' @param mailto Character string.
#' Gives OpenAlex an email to enter the polite pool.
#' @param api_key Character string.
#' Your OpenAlex Premium API key, if available.
#' @param verbose Logical.
#' If TRUE, print information about the querying process. Defaults to TRUE.
#'
#' @return a data.frame or a list of bibliographic records.
#'
#' For more extensive information about OpenAlex API, please visit:
#' <https://docs.openalex.org>
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
#' res <- oa_request(query_url = "https://api.openalex.org/works/W2755950973")
#'
#' #  The author Massimo Aria is associated to the OpenAlex-id A5069892096.
#'
#' query_author <- oa_query(
#'   identifier = "A5069892096",
#'   entity = "authors"
#' )
#' query_author
#' res <- oa_request(
#'   query_url = query_author,
#'   count_only = FALSE,
#'   verbose = FALSE
#' )
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
#'   cites = "W2755950973",
#'   from_publication_date = "2021-12-01",
#'   to_publication_date = "2021-12-31",
#'   search = NULL,
#'   endpoint = "https://api.openalex.org"
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
#'   entity = "works",
#'   title.search = c("bibliometric analysis", "science mapping"),
#'   from_publication_date = "2021-12-01",
#'   to_publication_date = "2021-12-31"
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
#'   entity = "works",
#'   title.search = c("bibliometric analysis", "science mapping"),
#'   from_publication_date = "2020-01-01",
#'   to_publication_date = "2021-12-31"
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
                       paging = "cursor",
                       pages = NULL,
                       count_only = FALSE,
                       mailto = oa_email(),
                       api_key = oa_apikey(),
                       verbose = FALSE) {
  # https://httr.r-lib.org/articles/api-packages.html#set-a-user-agent
  ua <- httr::user_agent("https://github.com/ropensci/openalexR/")

  # building query...
  is_group_by <- grepl("group_by", query_url)
  if (is_group_by) {
    result_name <- "group_by"
    query_ls <- list()
  } else {
    result_name <- "results"
    query_ls <- list("per-page" = 1)
  }

  if (!is.null(mailto)) {
    if (isValidEmail(mailto)) {
      query_ls[["mailto"]] <- mailto
    } else {
      message(mailto, " is not a valid email address")
    }
  }

  # first, download info about n. of items returned by the query
  res <- api_request(query_url, ua, query = query_ls, api_key = api_key)

  if (!is.null(res$meta)) {
    ## return only item counting
    if (count_only) {
      return(res$meta)
    }
  } else {
    return(res)
  }

  # Setting items per page
  query_ls[["per-page"]] <- per_page

  if (is_group_by) {
    data <- vector("list")
    res <- NULL
    i <- 1
    next_page <- get_next_page("cursor", i, res)
    if (verbose) cat("\nDownloading groups...\n|")
    while (!is.null(next_page)) {
      if (verbose) cat("=")
      Sys.sleep(1 / 100)
      query_ls[[paging]] <- next_page
      res <- api_request(query_url, ua, query = query_ls)
      data <- c(data, res[[result_name]])
      i <- i + 1
      next_page <- get_next_page("cursor", i, res)
    }
    cat("\n")
    return(data)
  }

  n_items <- res$meta$count
  n_pages <- ceiling(n_items / per_page)

  ## number of pages
  if (is.null(pages)) {
    pages <- seq.int(n_pages)
  } else {
    pages <- pages[pages <= n_pages]
    n_pages <- length(pages)
    n_items <- min(n_items - per_page * (utils::tail(pages, 1) - n_pages), per_page * n_pages)
    message("Using basic paging...")
    paging <- "page"
  }

  if (n_items <= 0 || n_pages <= 0) {
    warning("No records found!")
    return(list())
  }

  pg_plural <- if (n_pages > 1) " pages" else " page"

  if (verbose) {
    message(
      "Getting ", n_pages, pg_plural, " of results",
      " with a total of ", n_items, " records..."
    )
    pb <- oa_progress(n = n_pages, text = "OpenAlex downloading")
  }

  # Activation of cursor pagination
  data <- vector("list", length = n_pages)
  res <- NULL
  for (i in pages) {
    if (verbose) pb$tick()
    Sys.sleep(1 / 100)
    next_page <- get_next_page(paging, i, res)
    query_ls[[paging]] <- next_page
    res <- api_request(query_url, ua, query = query_ls)
    if (!is.null(res[[result_name]])) data[[i]] <- res[[result_name]]
  }

  data <- unlist(data, recursive = FALSE)

  if (grepl("filter", query_url) && grepl("works", query_url)) {
    truncated <- unlist(truncated_authors(data))
    if (length(truncated)) {
      truncated <- shorten_oaid(truncated)
      warning(
        "\nThe following work(s) have truncated lists of authors: ",
        paste(truncated, collapse = ", "),
        ".\nQuery each work separately by its identifier to get full list of authors.\n",
        "For example:\n  ",
        paste0(
          "lapply(c(\"",
          paste(utils::head(truncated, 2), collapse = "\", \""),
          "\"), \\(x) oa_fetch(identifier = x))"
        ),
        "\nDetails at https://docs.openalex.org/api-entities/authors/limitations."
      )
    }
  }

  data
}

truncated_authors <- function(list_result) {
  lapply(
    list_result,
    function(x){
      trunc <- x$is_authors_truncated
      if (!is.null(trunc) && trunc) x$id else NULL
    }
  )
}

get_next_page <- function(paging, i, res = NULL) {
  if (paging == "page") { # basic paging
    return(i)
  }
  # cursor paging
  if (i == 1) {
    return("*")
  }
  res$meta$next_cursor
}

#' Generate an OpenAlex query from a set of parameters
#'
#' It generates a valid query, written following the OpenAlex API Language, from a set of parameters.
#'
#' @param filter Character string.
#' Filters narrow the list down to just entities that meet a particular
#' condition--specifically, a particular value for a particular attribute.
#' Filters are formatted as attribute = value.
#' The complete list of filter attributes for each entity can be found at
#' <https://docs.openalex.org/how-to-use-the-api/get-lists-of-entities/filter-entity-lists>.
#' For example, `cited_by_count = ">100"`,
#' `title.search = c("bibliometric analysis", "science mapping")`,
#' or `to_publication_date = "2021-12-31"`.
#' @param multiple_id Logical. Whether there are multiple identifiers.
#' @param identifier Character. OpenAlex ID(s) as item identifier(s).
#' See more at <https://docs.openalex.org/how-to-use-the-api/get-single-entities#the-openalex-id>.
#' @param entity Character. Scholarly entity of the search.
#' The argument can be one of
#' c("works", "authors", "institutions", "concepts", "funders", "sources", "publishers").
#' If not provided, `entity` is guessed from `identifier`.
#' @param options List. Additional parameters to add in the query. For example:
#'
#' - `select` Character vector. Top-level fields to show in output.
#' Defaults to NULL, which returns all fields.
#' https://docs.openalex.org/how-to-use-the-api/get-single-entities/select-fields
#'
#' - `sort` Character. Attribute to sort by.
#' For example: "display_name" for sources or "cited_by_count:desc" for works.
#' See more at <https://docs.openalex.org/how-to-use-the-api/get-lists-of-entities/sort-entity-lists>.
#'
#' - `sample` Integer. Number of (random) records to return.
#' Should be no larger than 10,000.
#' Defaults to NULL, which returns all records satisfying the query.
#' Read more at <https://docs.openalex.org/how-to-use-the-api/get-lists-of-entities/sample-entity-lists>.
#'
#' - `seed` Integer.
#' A seed value in order to retrieve the same set of random records in
#' the same order when used multiple times with `sample`.
#' IMPORTANT NOTE: Depending on your query, random results with a seed value may change over time due to new records coming into OpenAlex.
#' This argument is likely only useful when queries happen close together (within a day).
#'
#' @param search Character. Search is just another kind of filter, one that all five endpoints support.
#' But unlike the other filters, search does NOT require an exact match.
#' This is particularly useful in author queries because many authors have middle names, which may not exist or do so in a variety of forms.
#' The `display_name` filter requires an exact match and will NOT find all these authors.
#' For example, author "Phillip H. Kuo" and "Phillip Hsin Kuo" can only be found either using search = "Phillip Kuo" or display_name = c("Phillip H. Kuo", "Phillip Hsin Kuo").
#' To filter using search, append .search to the end of the attribute you're filtering for.
#' @param group_by Character. Attribute to group by.
#' For example: "oa_status" for works.
#' See more at <https://docs.openalex.org/how-to-use-the-api/get-groups-of-entities>.
#' @param endpoint Character. URL of the OpenAlex Endpoint API server.
#' Defaults to endpoint = "https://api.openalex.org".
#' @param verbose Logical. If TRUE, print information on querying process.
#' Default to \code{verbose = FALSE}.
#' To shorten the printed query URL, set the environment variable openalexR.print
#' to the number of characters to print: \code{Sys.setenv(openalexR.print = 70)}.
#' @param \dots Additional filter arguments.
#'
#' @return a character containing the query in OpenAlex format.
#'
#' For more extensive information about OpenAlex API, please visit:
#' <https://docs.openalex.org>.
#'
#'
#' @examples
#' \dontrun{
#'
#' query_auth <- oa_query(identifier = "A5069892096", verbose = TRUE)
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
#'   verbose = TRUE
#' )
#'
#' #  The author Massimo Aria is associated to the OpenAlex-id A5069892096:
#'
#' query_auth <- oa_query(identifier = "A5069892096", verbose = TRUE)
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
#'   options = list(sort = "cited_by_count:desc"),
#'   verbose = TRUE
#' )
#' }
#'
#' @export
#'

oa_query <- function(filter = NULL,
                     multiple_id = FALSE,
                     identifier = NULL,
                     entity = if (is.null(identifier)) NULL else id_type(identifier[[1]]),
                     options = NULL,
                     search = NULL,
                     group_by = NULL,
                     endpoint = "https://api.openalex.org",
                     verbose = FALSE,
                     ...) {
  entity <- match.arg(entity, oa_entities())
  filter <- c(filter, list(...))

  empty_filters <- which(lengths(filter) == 0)
  if (length(empty_filters) > 0) {
    filter <- filter[-empty_filters]
    stop(
      "Filters must have a value: ",
      paste(names(empty_filters), collapse = ", "),
      call. = FALSE
    )
  }

  if (length(filter) > 0 || multiple_id) {
    null_locations <- vapply(filter, is.null, logical(1))
    filter[null_locations] <- NULL # remove NULL elements
    filter <- lapply(filter, asl)
    flt_ready <- mapply(append_flt, filter, names(filter))
    flt_ready <- paste0(flt_ready, collapse = ",")
  } else {
    flt_ready <- list()
  }

  if (!is.null(options$select)) {
    options$select <- paste(options$select, collapse = ",")
  }

  if (is.null(identifier) || multiple_id) {
    if (length(filter) == 0 &&
      is.null(search) &&
      is.null(group_by) &&
      is.null(options$sample)) {
      message("Identifier is missing, please specify filter or search argument.")
      return()
    }

    path <- entity
    query <- c(
      list(
        filter = flt_ready,
        search = search,
        group_by = group_by
      ),
      options
    )
  } else {
    path <- paste(entity, identifier, sep = "/")
    query <- options
  }

  query_url <- httr::modify_url(
    endpoint,
    path = path,
    query = query
  )

  if (is.null(oa_print())) {
    url_display <- query_url
  } else {
    query_url <- utils::URLdecode(query_url)
    query_url_more <- if (oa_print() < nchar(query_url)) "..."
    url_display <- paste0(substr(query_url, 1, oa_print()), query_url_more)
  }

  if (verbose) message("Requesting url: ", url_display)

  query_url
}

#' oa_fetch but for a random query
#'
#' @inheritParams oa_fetch
#'
#' @return A data.frame or a list. One row or one element.
#' Result of the random query.
#' If you would like to select more than one random entity, say, 10,
#' use `options = list(sample = 10)` argument in `oa_fetch`.
#'
#' @export
#'
#' @examples
#' oa_random()
oa_random <- function(entity = oa_entities(),
                      output = c("tibble", "dataframe", "list"),
                      endpoint = "https://api.openalex.org") {
  output <- match.arg(output)
  entity <- match.arg(entity, oa_entities())
  if (output == "dataframe") output <- "tibble"

  query_url <- paste(endpoint, entity, "random", sep = "/")
  res <- oa_request(query_url)

  final_res <- switch(output,
    list = res,
    tibble = oa2df(res,
      entity = entity
    )
  )

  final_res
}

api_request <- function(query_url, ua, query, api_key = oa_apikey()) {
  res <- httr::GET(query_url, ua, query = query, httr::add_headers(api_key = api_key))

  if (httr::status_code(res) == 400) {
    stop("HTTP status 400 Request Line is too large")
  }

  if (httr::status_code(res) == 429) {
    message("HTTP status 429 Too Many Requests")
    return(list())
  }

  m <- httr::content(res, "text", encoding = "UTF-8")

  if (httr::status_code(res) == 503) {
    mssg <- regmatches(m, regexpr("(?<=<title>).*?(?=<\\/title>)", m, perl = TRUE))
    message(mssg, ". Please try setting `per_page = 25` in your function call!")
    return(list())
  }

  parsed <- jsonlite::fromJSON(m, simplifyVector = FALSE)

  if (httr::status_code(res) == 200) {
    if (httr::http_type(res) != "application/json") {
      stop("API did not return json", call. = FALSE)
    }
    return(parsed)
  }

  if (httr::http_error(res)) {
    stop(
      sprintf(
        "OpenAlex API request failed [%s]\n%s\n<%s>",
        httr::status_code(res),
        parsed$error,
        parsed$message
      ),
      call. = FALSE
    )
  }

  if (httr::status_code(res) != 429 & httr::status_code(res) != 200) {
    message("HTTP status ", httr::status_code(res))
    return(list())
  }
}
