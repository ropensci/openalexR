test_that("show_authors works", {
  skip_on_cran()

  author_ids <- c("A5023888391", "A5014077037")
  author_simplified <- show_authors(oa_fetch(identifier = author_ids))

  expect_s3_class(author_simplified, "data.frame")
  expect_equal(nrow(author_simplified), length(author_ids))
  expect_type(author_simplified$top_concepts, "character")
})

test_that("show_works works", {
  skip_on_cran()

  work_ids <- c("W2741809807", "W2755950973")
  work_simplified <- show_works(oa_fetch(identifier = work_ids))

  expect_s3_class(work_simplified, "data.frame")
  expect_equal(nrow(work_simplified), length(work_ids))
  expect_type(work_simplified$top_concepts, "character")
})
