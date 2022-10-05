test_that("oa_snowball works", {
  skip_on_cran()

  work_ids <- c("W3045921891", "W3046863325")
  multi_works <- oa_snowball(
    identifier = work_ids,
    verbose = TRUE
  )$nodes

  expect_equal(
    sort(multi_works$id[multi_works$role=="target"]),
    sort(work_ids)
  )

  snowball_docs <- oa_snowball(
    identifier = c("W4295757800", "W4296128995", "W4297497355")
  )

  expect_true(is.list(snowball_docs))

})
