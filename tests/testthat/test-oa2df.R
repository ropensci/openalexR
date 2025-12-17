test_that("oa2df works", {
  skip_on_cran()

  naples <- oa_fetch(identifier = "I71267560")
  expect_s3_class(naples, "data.frame")
  expect_s3_class(naples, "tbl")
  expect_true(grepl("Naples", naples$display_name))
  expect_equal(naples$country_code, "IT")

  nejm <- oa_fetch(identifier = "S137773608")
  expect_true(grepl("Nature", nejm$display_name))
  expect_s3_class(nejm, "data.frame")
  expect_s3_class(nejm, "tbl")

  medicine <- oa_fetch(identifier = "medicine", entity = "keywords")
  expect_equal(medicine$display_name, "Medicine")
  expect_s3_class(medicine, "data.frame")
  expect_s3_class(medicine, "tbl")
})
