test_that("oa_snowball works", {
  skip_on_cran()

  work_ids <- c("W3045921891", "W3046863325")
  multi_works <- oa_snowball(
    identifier = work_ids,
    verbose = TRUE
  )
  multi_nodes <- multi_works$nodes
  flat_snow <- to_disk(multi_works)

  expect_equal(
    sort(multi_nodes$id[multi_nodes$role=="target"]),
    sort(work_ids)
  )

  expect_equal(nrow(flat_snow), nrow(multi_nodes))
  expect_equal(ncol(flat_snow), ncol(multi_nodes) + 1)
  expect_true("cited_by" %in% names(flat_snow))
  expect_s3_class(flat_snow, "data.frame")
})

test_that("oa_snowball works for recent articles with no citations yet", {
  snowball_docs <- oa_snowball(
    identifier = c("W4295757800", "W4296128995", "W4297497355")
  )
  expect_true(is.list(snowball_docs))
})
