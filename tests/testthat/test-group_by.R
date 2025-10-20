test_that("search and group_by works", {
  skip_on_cran()
  # https://github.com/ropensci/openalexR/issues/327
  g1 <- oa_fetch(
    title_and_abstract.search = "nature",
    group_by = "topics.subfield.id"
  )
  expect_s3_class(g1, "data.frame")
  expect_equal(colnames(g1), c("key", "key_display_name", "count"))
})

test_that("group_by works", {
  skip_on_cran()
  # https://github.com/ropensci/openalexR/issues/327

  # grouping by the boolean "is_oa" should return 2 rows
  g2 <- oa_fetch(entity = "works", search = "biodiversity", group_by = "is_oa")
  expect_s3_class(g2, "data.frame")
  expect_equal(nrow(g2), 2)
  expect_equal(colnames(g2), c("key", "key_display_name", "count"))
})
