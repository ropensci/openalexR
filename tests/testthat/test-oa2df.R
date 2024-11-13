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

  www <- oa_fetch(identifier = "C136764020")
  expect_equal(www$display_name, "World Wide Web")
  expect_s3_class(www, "data.frame")
  expect_s3_class(www, "tbl")
})
