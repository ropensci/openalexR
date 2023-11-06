test_that("oa_snowball works", {
  skip_on_cran()

  work_ids <- c("W3045921891", "W3046863325")
  Sys.sleep(1/10)
  multi_works <- oa_snowball(identifier = work_ids) # no cited papers
  multi_nodes <- multi_works$nodes
  flat_snow <- oa2df(multi_works, "snowball")

  expect_equal(
    sort(multi_nodes$id[multi_nodes$oa_input]),
    sort(work_ids)
  )

  expect_equal(nrow(flat_snow), nrow(multi_nodes))
  expect_equal(ncol(flat_snow), ncol(multi_nodes) + 6)

  c(
    "cited_by", "citing", "connection",
    "forward_count", "backward_count", "connection_count"
  ) |>
    `%in%`(names(flat_snow)) |>
    all() |>
    expect_true()

  expect_s3_class(flat_snow, "data.frame")
})

test_that("oa_snowball works for recent articles with no citations yet", {
  skip_on_cran()

  Sys.sleep(1/10)
  snowball_docs <- oa_snowball(
    identifier = c("W4295757800", "W4296128995", "W4297497355")
  )
  expect_true(is.list(snowball_docs))
})

test_that("oa_snowball works for DOIs", {
  skip_on_cran()

  work_dois <- c(
    "10.1145/3383583.3398584",
    "https://doi.org/10.1038/s41586-022-05258-z"
  )
  Sys.sleep(1/10)
  snowball_doi <- oa_snowball(doi = work_dois)
  doi_nodes <- snowball_doi$nodes
  dois_in <- doi_nodes$doi[doi_nodes$oa_input]

  expect_true(is.list(snowball_doi))
  expect_true(any(grepl(work_dois[[1]], dois_in)))
  expect_true(any(grepl(work_dois[[2]], dois_in)))
})

test_that("oa_snowball works for author orcids", {
  skip_on_cran()

  orcids <- c("0000-0003-3737-6565", "0000-0002-8517-9411")
  Sys.sleep(1/10)
  # find publications by these two authors this year
  snowball_orcid <- oa_snowball(
    author.orcid = orcids,
    from_publication_date = "2022-01-01",
    to_publication_date = "2022-12-31",
    citing_params = list(from_publication_date = "2022-10-01"),
    cited_by_params = list(from_publication_date = "2021-10-01")
  )

  nodes <- snowball_orcid$nodes
  orcids_in <- lapply(nodes$author[nodes$oa_input], function(x) x$au_orcid)
  either_orcid <- paste(orcids, collapse = "|")

  expect_true(is.list(snowball_orcid))

  orcids_in |>
    vapply(function(x) any(grepl(either_orcid, x)), logical(1)) |>
    all() |>
    expect_true()
})
