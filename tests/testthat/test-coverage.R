test_that("field coverage information is up to date", {
  skip_on_cran()
  cols <- lapply(get_coverage(), function(x) {
    covered_by <- x$covered_by[!is.na(x$covered_by)]
    # Additional fields from the parent Search entity
    c(covered_by, "relevance_score")
  })

  # Works
  w <- oa_fetch("works", options = list(sample = 1))
  expect_true(all(colnames(w) %in% cols$works))

  # Authors
  a <- oa_fetch("authors", options = list(sample = 1))
  expect_true(all(colnames(a) %in% cols$authors))
})
