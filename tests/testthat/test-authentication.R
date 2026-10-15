test_that("api_request sends bearer authentication only when a key is present", {
  headers <- list()
  local_mocked_bindings(
    GET = function(url, ua, query, auth, ...) {
      headers[[length(headers) + 1L]] <<- auth$headers
      structure(list(), class = "response")
    },
    status_code = function(res) 200L,
    http_type = function(res) "application/json",
    content = function(res, ...) "{}",
    .package = "httr"
  )

  for (key in list("test-key", NULL, "")) {
    api_request("https://api.openalex.org/works", httr::user_agent("test"),
                list(), api_key = key)
  }
  expect_equal(unname(headers[[1]]["Authorization"]), "Bearer test-key")
  expect_false("api_key" %in% names(headers[[1]]))
  expect_false("Authorization" %in% names(headers[[2]]))
  expect_false("Authorization" %in% names(headers[[3]]))
})

test_that("oa_request retains explicit keys throughout pagination", {
  for (raw in c(FALSE, TRUE)) {
    keys <- character()
    local_mocked_bindings(
      api_request = function(query_url, ua, query, api_key = NULL,
                             parse = TRUE, ...) {
        keys <<- c(keys, api_key)
        body <- list(meta = list(count = 2L), results = list(list(id = "W1")))
        if (parse) body else jsonlite::toJSON(body, auto_unbox = TRUE)
      }
    )
    oa_request("https://api.openalex.org/works", per_page = 1,
               api_key = "explicit-key", parse = !raw)
    expect_identical(keys, rep("explicit-key", 3L))
  }
})

test_that("oa_request retains explicit keys when paging groups", {
  keys <- character()
  local_mocked_bindings(
    api_request = function(query_url, ua, query, api_key = NULL,
                           parse = TRUE, ...) {
      keys <<- c(keys, api_key)
      body <- list(meta = list(groups_count = 2L),
                   group_by = list(list(key = "x", key_display_name = "X")))
      if (parse) body else jsonlite::toJSON(body, auto_unbox = TRUE)
    }
  )
  oa_request("https://api.openalex.org/works?group_by=type", per_page = 1,
             api_key = "explicit-key")
  expect_identical(keys, rep("explicit-key", 2L))
})
