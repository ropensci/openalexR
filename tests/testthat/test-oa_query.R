test_that("OA query works", {
  expect_equal(
    oa_query(
      entity = "works",
      doi = c(
        "10.1371/journal.pone.0266781",
        "10.1371/journal.pone.0267149"
      )
    ),
    paste0(
      "https://api.openalex.org/works?filter=doi%3A10.1371%2F",
      "journal.pone.0266781%7C10.1371%2Fjournal.pone.0267149"
    )
  )

})

test_that("OA query works with multiple identifiers", {
  expect_equal(
    oa_query(
      identifier = c("W2162348455", "W2746723710")
    ),
    "https://api.openalex.org/works?filter=openalex%3AW2162348455%7CW2746723710"
  )
})

test_that("oa_query returns NULL when no identifier or filter is supplied", {
  expect_null(oa_query())
  expect_message(oa_query())
})

test_that("oa_query accepts oa_options() and keeps paging out of the URL", {
  url <- oa_query(
    entity = "works",
    doi = "10.1371/journal.pone.0266781",
    options = oa_options(
      per_page = 50,
      paging = "page",
      pages = 1:3,
      sort = "cited_by_count:desc"
    )
  )
  expect_match(url, "sort=cited_by_count")
  expect_false(grepl("per_page|per-page|paging|pages", url))
})
