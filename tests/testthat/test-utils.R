test_that("append_flt works", {
  expect_equal(append_flt("2022-08-08"), "from_publication_date:2022-08-08")
  expect_equal(append_flt("2022-08-08", "to_publication_date"), "to_publication_date:2022-08-08")
  expect_equal(append_flt(NULL, "to_publication_date"), NULL)
  expect_equal(
    append_flt(c("10.1371/journal.pone.0266781", "10.1371/journal.pone.0267149"), "doi"),
    "doi:10.1371/journal.pone.0266781|10.1371/journal.pone.0267149"
  )
  expect_equal(
    append_flt(c("bibliometric analysis", "science mapping"), "title.search"),
    "title.search:bibliometric analysis|science mapping"
  )
})

test_that("asl works", {
  expect_equal(asl("true"), "true")
  expect_equal(asl(TRUE), "true")
  expect_equal(asl("True"), "true")
  expect_equal(asl("TRue"), "true")

  expect_equal(asl("false"), "false")
  expect_equal(asl(FALSE), "false")
  expect_equal(asl("FalSE"), "false")

  expect_equal(asl(89), 89)
  expect_equal(asl("TRUEFA"), "TRUEFA")
})

test_that("shorten_oaid works", {
  expect_equal(shorten_oaid(c(
    "https://openalex.org/W3045921891",
    "https://openalex.org/W3046863325",
    "W3045921891"
  )), c("W3045921891", "W3046863325", "W3045921891"))
})
