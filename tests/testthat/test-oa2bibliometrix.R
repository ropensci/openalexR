test_that("oa2bibliometrix works", {
  skip_on_cran()

  aria_citations <- oa_fetch(
    entity = "works",
    cites = "W2755950973",
    from_publication_date = "2021-01-01",
    to_publication_date = "2021-01-31"
  )

  aria_bibli <- oa2bibliometrix(aria_citations)
  expect_s3_class(aria_bibli, "data.frame")
  expect_equal(nrow(aria_citations), nrow(aria_bibli))
})
