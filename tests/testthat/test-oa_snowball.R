test_that("oa_snowball", {
  work_ids <- c("W2741809807")
  multi_works <- oa_snowball(
    identifier = work_ids,
    verbose = TRUE
  )
  expect_equal(
    multi_works$id[multi_works$role=="target"],
    paste0("https://openalex.org/", work_ids)
  )

})
