test_that("OA query works", {
  expect_equal(
    oa_query(
      entity = "works",
      doi = c(
        "10.1371/journal.pone.0266781",
        "10.1371/journal.pone.0267149"
      )
    ),
    "https://api.openalex.org/works?filter=doi%3A10.1371%2Fjournal.pone.0266781%7C10.1371%2Fjournal.pone.0267149"
  )
})
