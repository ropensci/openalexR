test_that("generator works", {
  skip_if_not_installed("coro")
  skip_on_cran()

  query_url <- paste0(
    "https://api.openalex.org/authors?",
    "filter=openalex%3AA5023888391%7CA5069892096"
  )
  oar <- oa_generate(query_url)
  p1 <- oar() # record 1
  p2 <- oar() # record 2

  expect_type(p1, "list")
  expect_type(p2, "list")
  expect_equal(
    sort(c(p1$id, p2$id)),
    c(
      "https://openalex.org/A5023888391",
      "https://openalex.org/A5069892096"
    )
  )
})
