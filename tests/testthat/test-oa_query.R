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

test_that("oa_query returns NULL when no identifier or filter is supplied", {
  expect_null(oa_query())
  expect_message(oa_query())
})
