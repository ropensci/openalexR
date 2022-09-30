test_that("oa_snowball works", {
  work_ids <- c("W2741809807")
  multi_works <- oa_snowball(
    identifier = work_ids,
    verbose = TRUE
  )
  expect_equal(
    multi_works$id[multi_works$role=="target"],
    paste0("https://openalex.org/", work_ids)
  )

  snowball_docs <- oa_snowball(
    identifier = c("W4295757800", "W4296128995", "W4297497355"),
    endpoint = "https://api.openalex.org/",
    verbose = TRUE
  )

  expect_true(is.data.frame(snowball_docs))

})
