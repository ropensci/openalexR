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
