
#' A generator for making request to OpenAlex API
#' TODO check group_by
#' @inheritParams oa_request
#'
#' @examples
#' \dontrun{
#' query_url <- "https://api.openalex.org/works?filter=cites%3AW2755950973"
#' oar <- oa_generate_records(
#'   query_url, per_page = 10,
#'   verbose = TRUE
#' )
#' p1 <- oar() # item 1
#' p2 <- oar() # item 2
#' p3 <- oar() # item 3
#' head(p1)
#' head(p3)
#' }
#'
#' @export
oa_generate_records <- if (!requireNamespace("coro", quietly = TRUE)) {
  message(
    "Package \"coro\" must be installed to use this generator."
  )
  NULL
} else coro::generator(
  function(query_url,
           per_page = 200,
           mailto = oa_email(),
           api_key = oa_apikey(),
           verbose = FALSE) {

    ua <- httr::user_agent("https://github.com/ropensci/openalexR/")
    query_ls <- list("per-page" = 200)
    if (!is.null(mailto)) {
      if (isValidEmail(mailto)) {
        query_ls[["mailto"]] <- mailto
      } else {
        message(mailto, " is not a valid email address")
      }
    }
    paging <- "cursor"
    query_ls[paging] <- "*"
    res <- api_request(query_url, ua, query_ls, api_key = api_key)
    is_group_by <- grepl("group_by", query_url)
    if (is_group_by) {
      return(res$group_by)
    }

    if (is.null(res$meta)) {
      return(res)
    }
    n_items <- res$meta$count

    if (n_items <= 0) {
      warning("No records found!")
      return()
    }

    # pattern <- "\"next_cursor\":\"(.*?)\""
    for (i in seq.int(n_items)) { # cursor pagination
      if (verbose) {
        message("Getting record ", i, " of ", n_items, " records...")
      }
      coro::yield(res$results[[(i-1) %% 200 + 1]])
      if (i %% 200 == 0){
        next_page <- get_next_page(paging, 0, res)
        query_ls[[paging]] <- next_page
        res <- api_request(query_url, ua, query = query_ls)
        print(query_ls)
      }

      # if (i == 1){
      #   next_cursor <- "*"
      # } else {
      #   next_cursor <- sub(pattern, "\\1", regmatches(res, regexpr(pattern, res)))
      # }
      # query_ls[["cursor"]] <- next_cursor

      # res <- api_request_raw(query_url, ua, query_ls, api_key = api_key)

    }
  }
)


api_request_raw <- function(query_url, ua, query, api_key = oa_apikey()) {
  res <- httr::GET(query_url, ua, query = query, httr::add_headers(api_key = api_key))

  if (httr::status_code(res) == 200) {
    if (httr::http_type(res) != "application/json") {
      stop("API did not return json", call. = FALSE)
    }

    data <- httr::content(res, as = "text", encoding = "utf-8")

    return(data)
  }

  if (httr::status_code(res) == 429) {
    message("HTTP status 429 Too Many Requests")
    return("")
  }

  parsed <- httr::content(res, "text", encoding = "UTF-8")

  if (httr::http_error(res)) {
    stop(
      sprintf(
        "OpenAlex API request failed [%s]\n<%s>",
        httr::status_code(res),
        parsed
      ),
      call. = FALSE
    )
  }

  if (httr::status_code(res) != 429 & httr::status_code(res) != 200) {
    message("HTTP status ", httr::status_code(res))
    return("")
  }
}
