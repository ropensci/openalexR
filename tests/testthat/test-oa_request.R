test_that("Invalid filter errors out", {
  query_url <- "https://api.openalex.org/authors?filter=openalex%3AA923435168%7CA2208157607"

  expect_error(oa_request(query_url))
})


test_that("oa_fetch works", {
  work_ids <- c("W2741809807")
  multi_works <- oa_fetch(
    identifier = work_ids,
    verbose = TRUE
  )
  expect_equal(
    multi_works$id,
    paste0("https://openalex.org/", work_ids)
  )

})
