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
    sort(multi_nodes$id[multi_nodes$oa_input]),
    sort(work_ids)
  )

  expect_equal(nrow(flat_snow), nrow(multi_nodes))
  expect_equal(ncol(flat_snow), ncol(multi_nodes) + 6)
  expect_true(all(
    c(
      "cited_by", "citing", "connection",
      "forward_count", "backward_count", "connection_count"
    ) %in%
      names(flat_snow)
  ))

  expect_s3_class(flat_snow, "data.frame")
})

test_that("oa_snowball works for recent articles with no citations yet", {
  snowball_docs <- oa_snowball(
    identifier = c("W4295757800", "W4296128995", "W4297497355")
  )
  expect_true(is.list(snowball_docs))
})

test_that("oa_snowball works for DOIs", {
  work_dois <- c(
    "10.1145/3383583.3398584",
    "https://doi.org/10.1038/s41586-022-05258-z"
  )

  snowball_doi <- oa_snowball(
    doi = work_dois,
    verbose = TRUE
  )

  doi_nodes <- snowball_doi$nodes
  dois_in <- doi_nodes$doi[doi_nodes$oa_input]

  expect_true(is.list(snowball_doi))
  expect_true(any(grepl(work_dois[[1]], dois_in)))
  expect_true(any(grepl(work_dois[[2]], dois_in)))
})
