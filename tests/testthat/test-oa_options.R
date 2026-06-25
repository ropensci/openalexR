test_that("oa_options() builds a classed list with paging defaults", {
  o <- oa_options()
  expect_s3_class(o, "oa_options")
  expect_equal(o$per_page, 200L)
  expect_equal(o$paging, "cursor")
  expect_null(o$select)
})

test_that("oa_options() keeps only the query fields that are set", {
  o <- oa_options(select = c("id", "doi"), sort = "cited_by_count:desc")
  expect_equal(o$select, c("id", "doi"))
  expect_equal(o$sort, "cited_by_count:desc")
  expect_false("sample" %in% names(o))
  expect_false("seed" %in% names(o))
})

test_that("oa_options() validates sample", {
  expect_error(oa_options(sample = 20000), "10,000")
  expect_error(oa_options(sample = -1), "positive")
  expect_equal(oa_options(sample = 50)$sample, 50L)
})

test_that("oa_options() warns on seed without sample and drops it", {
  expect_warning(o <- oa_options(seed = 1), "sample")
  expect_false("seed" %in% names(o))
  expect_equal(oa_options(sample = 10, seed = 1)$seed, 1L)
})

test_that("oa_options() validates paging and per_page", {
  expect_error(oa_options(paging = "bogus"))
  expect_warning(oa_options(per_page = 500), "200")
})

test_that("oa_options() warns on unknown options but still forwards them", {
  expect_warning(o <- oa_options(slect = "id"), "Unknown")
  expect_equal(o$slect, "id")
})

test_that("as_oa_options() normalizes NULL, lists, and oa_options objects", {
  expect_s3_class(as_oa_options(NULL), "oa_options")

  from_list <- as_oa_options(list(select = "id"))
  expect_equal(from_list$select, "id")
  expect_equal(from_list$per_page, 200L)
  expect_equal(from_list$paging, "cursor")

  obj <- oa_options(sample = 5, seed = 1)
  expect_identical(as_oa_options(obj), obj)

  expect_error(as_oa_options(1), "list")
})
