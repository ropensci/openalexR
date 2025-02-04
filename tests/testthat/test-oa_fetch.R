test_that("Invalid filter errors out", {
  skip_on_cran()

  # open_alex is not a valid filter
  query_url <- paste0(
    "https://api.openalex.org/authors?",
    "filter=open_alex%3AA5069892096%7CA5023888391"
  )

  expect_error(oa_request(query_url))
})

test_that("oa_request returns list", {
  skip_on_cran()

  query_url <- paste0(
    "https://api.openalex.org/authors?",
    "filter=openalex%3AA5069892096%7CA5023888391"
  )

  expect_type(oa_request(query_url), "list")
})

test_that("oa_request gives messages for unexpected input", {
  skip_on_cran()

  query_url <- paste0(
    "https://api.openalex.org/authors?",
    "filter=openalex%3AA5048491430%7CA5023888391"
  )
  expect_message(oa_request(query_url, verbose = TRUE))
  expect_message(oa_request(query_url, mailto = 123))
})

test_that("oa_fetch works for multiple works", {
  skip_on_cran()

  work_ids <- c("W2741809807", "W3046863325")
  multi_works <- oa_fetch(
    identifier = work_ids,
    output = "dataframe",
    mailto = "example@email.com"
  )

  expect_equal(
    sort(multi_works$id),
    paste0("https://openalex.org/", sort(work_ids))
  )

  expect_true("affiliation_raw" %in% names(multi_works$authorships[[1]]))

  Sys.sleep(1 / 10)
  # warn about truncated authors
  expect_warning(oa_fetch(identifier = c("W4381194940", "W4386241859")))

  Sys.sleep(1 / 10)
  filtered_works <- oa_fetch(
    entity = "w",
    publication_date = "2020-08-01",
    cited_by_count = ">1000"
  )
  expect_s3_class(filtered_works, "data.frame")

  expect_warning(oa_fetch(doi = "123"))
})

test_that("oa_fetch options$select works", {
  skip_on_cran()

  x <- oa_fetch(
    entity = "works",
    doi = c(
      "10.1371/journal.pone.0266781",
      "10.1371/journal.pone.0267149"
    ),
    options = list(select = c("doi", "id", "cited_by_count", "type"))
  )
  expect_equal(
    dim(x),
    c(2, 4)
  )
})

test_that("Error when input entity can't be matched", {
  skip_on_cran()

  expect_error(
    oa_fetch(
      entity = "wa",
      publication_date = "2020-08-01",
      cited_by_count = ">1000"
    )
  )

  Sys.sleep(1 / 10)
  expect_error(
    oa_fetch(
      entity = "insta",
      display_name.search = "University of Florida"
    )
  )
})

test_that("oa_fetch instutitions binds associated_institutions correctly", {
  skip_on_cran()
  Sys.sleep(1 / 10)
  inst <- oa_fetch(identifier = "I1292875679")
  expect_true(ncol(inst$associated_institutions[[1]]) >= 6)
})

test_that("oa_fetch sample works", {
  skip_on_cran()
  Sys.sleep(1 / 10)
  random2021 <- oa_fetch(
    "works",
    publication_year = 2021,
    options = list(sample = 20)
  )
  Sys.sleep(1 / 10)
  random10 <- oa_fetch(
    "works",
    options = list(sample = 10, seed = 1)
  )

  expect_equal(nrow(random2021), 20)
  expect_equal(nrow(random10), 10)
})


test_that("search works with sampling", {
  skip_on_cran()

  w <- oa_fetch("works", search = "open science", options = list(sample = 5))
  expect_equal(nrow(w), 5)
})

test_that("oa_fetch authors can deal with NA institutions", {
  skip_on_cran()

  Sys.sleep(1 / 10)
  expect_s3_class(
    oa_fetch(
      entity = "authors",
      orcid = "0000-0001-7482-0480"
    ),
    "data.frame"
  )
  Sys.sleep(1 / 10)
  expect_type(
    oa_fetch(
      entity = "authors",
      orcid = "0000-0001-7482-0480",
      output = "list"
    ),
    "list"
  )
})

test_that("oa_fetch can combine (OR) more than 50 DOIs in a filter", {
  skip_on_cran()

  valid_dois <- c(
    "https://doi.org/10.1016/j.jbusres.2021.04.070",
    "https://doi.org/10.1016/j.jbusres.2020.06.057",
    "https://doi.org/10.1016/j.jbusres.2019.10.039",
    "https://doi.org/10.3145/epi.2020.ene.03",
    "https://doi.org/10.1111/ijcs.12605",
    "https://doi.org/10.7759/cureus.7357",
    "https://doi.org/10.1080/00207543.2020.1717011",
    "https://doi.org/10.1016/j.psep.2019.11.014",
    "https://doi.org/10.1016/j.jbusres.2019.11.025",
    "https://doi.org/10.3390/land9010028",
    "https://doi.org/10.1016/j.scitotenv.2020.136776",
    "https://doi.org/10.21037/atm-20-4235",
    "https://doi.org/10.1007/s11625-020-00833-7",
    "https://doi.org/10.1016/j.chb.2019.106177",
    "https://doi.org/10.1016/j.jclepro.2020.120988",
    "https://doi.org/10.1016/j.tmaid.2020.101566",
    "https://doi.org/10.1080/00207543.2019.1671625",
    "https://doi.org/10.2196/18228",
    "https://doi.org/10.1007/s10462-018-9652-0",
    "https://doi.org/10.1016/j.jclepro.2020.122679",
    "https://doi.org/10.1016/j.jhazmat.2020.123110",
    "https://doi.org/10.3390/molecules25153406",
    "https://doi.org/10.1186/s12879-020-05293-z",
    "https://doi.org/10.1186/s12992-020-00651-7",
    "https://doi.org/10.1007/s11205-020-02281-3",
    "https://doi.org/10.3390/su12219132",
    "https://doi.org/10.1016/j.jclepro.2019.119908",
    "https://doi.org/10.1108/tqm-10-2019-0243",
    "https://doi.org/10.3390/geosciences10100379",
    "https://doi.org/10.1016/j.jbusres.2021.07.015",
    "https://doi.org/10.5530/jscires.8.3.32",
    "https://doi.org/10.1016/j.ijpe.2020.107868",
    "https://doi.org/10.26355/eurrev_202003_20712",
    "https://doi.org/10.1016/j.wneu.2020.01.171",
    "https://doi.org/10.1016/j.jclepro.2020.121503",
    "https://doi.org/10.3389/fpubh.2020.00477",
    "https://doi.org/10.1016/j.jclepro.2020.124132",
    "https://doi.org/10.1016/j.apenergy.2020.114753",
    "https://doi.org/10.1016/j.landusepol.2020.104787",
    "https://doi.org/10.1007/s11192-020-03590-7",
    "https://doi.org/10.1016/j.chemosphere.2019.124627",
    "https://doi.org/10.1016/j.ijhm.2019.102387",
    "https://doi.org/10.1016/j.techfore.2020.119963",
    "https://doi.org/10.21037/atm.2020.04.26",
    "https://doi.org/10.1016/j.jbef.2021.100577",
    "https://doi.org/10.1111/hir.12295",
    "https://doi.org/10.21873/invivo.11951",
    "https://doi.org/10.1007/s11356-021-13094-3",
    "https://doi.org/10.1016/j.coesh.2019.10.008",
    "https://doi.org/10.1016/j.net.2020.08.005",
    "https://doi.org/10.1016/j.iot.2020.100318"
  )
  Sys.sleep(1 / 10)
  many_doi_results <- oa_fetch(entity = "works", doi = valid_dois)

  expect_s3_class(
    many_doi_results,
    "data.frame"
  )

  expect_true(nrow(many_doi_results) >= length(valid_dois) - 5)
})

test_that("oa_fetch can combine (OR) more than 50 ORCIDs in a filter", {
  skip_on_cran()

  valid_orcids <- c(
    "https://orcid.org/0000-0002-8525-3159",
    "https://orcid.org/0000-0001-7641-0637",
    "https://orcid.org/0000-0002-6465-982X",
    "https://orcid.org/0000-0002-8270-3134",
    "https://orcid.org/0000-0001-9336-6543",
    "https://orcid.org/0000-0002-3012-7446",
    "https://orcid.org/0000-0001-6428-8611",
    "https://orcid.org/0000-0002-4951-4526",
    "https://orcid.org/0000-0002-6354-3913",
    "https://orcid.org/0000-0001-7523-7967",
    "https://orcid.org/0000-0002-3792-0818",
    "https://orcid.org/0000-0002-9412-2556",
    "https://orcid.org/0000-0002-4147-892X",
    "https://orcid.org/0000-0002-7060-8404",
    "https://orcid.org/0000-0001-9080-6267",
    "https://orcid.org/0000-0001-5129-940X",
    "https://orcid.org/0000-0002-7369-2058",
    "https://orcid.org/0000-0002-9460-5144",
    "https://orcid.org/0000-0002-8517-9411",
    "https://orcid.org/0000-0003-1345-9649",
    "https://orcid.org/0000-0003-3737-6565",
    "https://orcid.org/0000-0001-5882-1168",
    "https://orcid.org/0000-0001-9558-6099",
    "https://orcid.org/0000-0003-3421-5627",
    "https://orcid.org/0000-0003-2916-6402",
    "https://orcid.org/0000-0002-0811-6580",
    "https://orcid.org/0000-0001-6535-5492",
    "https://orcid.org/0000-0001-8934-7569",
    "https://orcid.org/0000-0002-1355-9175",
    "https://orcid.org/0000-0001-8693-5947",
    "https://orcid.org/0000-0003-4126-9244",
    "https://orcid.org/0000-0002-5015-1099",
    "https://orcid.org/0000-0001-8713-9213",
    "https://orcid.org/0000-0001-5035-5983",
    "https://orcid.org/0000-0002-2817-5377",
    "https://orcid.org/0000-0002-5935-7544",
    "https://orcid.org/0000-0001-9059-7442",
    "https://orcid.org/0000-0003-2796-9148",
    "https://orcid.org/0000-0002-5639-3128",
    "https://orcid.org/0000-0001-7327-0106",
    "https://orcid.org/0000-0002-7319-418X",
    "https://orcid.org/0000-0003-1759-1700",
    "https://orcid.org/0000-0003-4867-5149",
    "https://orcid.org/0000-0002-2622-0672",
    "https://orcid.org/0000-0003-1013-5809",
    "https://orcid.org/0000-0001-5200-1476",
    "https://orcid.org/0000-0001-9443-8123",
    "https://orcid.org/0000-0002-4180-2216",
    "https://orcid.org/0000-0003-1761-3180",
    "https://orcid.org/0000-0003-4886-7482",
    "https://orcid.org/0000-0001-6618-8542"
  )

  many_orcid_results <- oa_fetch(entity = "authors", orcid = valid_orcids)

  expect_s3_class(many_orcid_results, "data.frame")
  # https://orcid.org/0000-0002-4147-892X no longer corresponds to two openalex id
  expect_true(nrow(many_orcid_results) >= length(valid_orcids) - 5)
})

test_that("oa_random works", {
  skip_on_cran()

  random_works <- oa_random("works")
  expect_type(random_works, "list")
  expect_s3_class(random_works, "data.frame")
  expect_equal(nrow(random_works), 1)
})

test_that("oa_fetch other entities works", {
  skip_on_cran()

  random_authors <- oa_fetch(entity = "authors", options = list(sample = 20))
  random_sources <- oa_fetch(entity = "sources", options = list(sample = 20))
  random_concepts <- oa_fetch(entity = "concepts", options = list(sample = 20))
  random_institutions <- oa_fetch(entity = "institutions", options = list(sample = 20))
  random_topics <- oa_fetch(entity = "topics", options = list(sample = 20))

  expect_equal(nrow(random_authors), 20)
  expect_equal(nrow(random_sources), 20)
  expect_equal(nrow(random_concepts), 20)
  expect_equal(nrow(random_institutions), 20)
  expect_equal(nrow(random_topics), 20)
})

test_that("paging works with sample", {
  skip_on_cran()

  w <- oa_fetch(
    "works",
    from_publication_date = Sys.Date() - 2,
    to_publication_date = Sys.Date(),
    options = list(sample = 50, seed = 1),
    per_page = 20
  )
  expect_equal(nrow(w), 50)
  expect_equal(sum(duplicated(w)), 0) # no duplicates
})

test_that("oa_fetch works for funders", {
  skip_on_cran()

  s <- oa_fetch("funders", country_code = "ca", cited_by_count = ">100000")
  expect_s3_class(s, "data.frame")
  expect_equal(ncol(s), 17)
  expect_true(nrow(s) > 1)
})

test_that("oa_fetch works for sources", {
  skip_on_cran()

  s <- oa_fetch(entity = "sources", search = "nature")
  expect_s3_class(s, "data.frame")
  expect_equal(ncol(s), 28)
  expect_true(nrow(s) > 200)
})

test_that("oa_fetch works for publishers", {
  skip_on_cran()

  s <- oa_fetch(entity = "publishers", country_codes = "ca")
  expect_s3_class(s, "data.frame")
  expect_equal(ncol(s), 19)
  expect_true(nrow(s) > 100)
})

test_that("oa_fetch works with 1 identifier", {
  skip_on_cran()

  w <- oa_fetch(identifier = "W3127908559") # Work
  a <- oa_fetch(identifier = "A5023888391") # Author
  i <- oa_fetch(identifier = "I4200000001") # Institution
  f <- oa_fetch(identifier = "F4320332161") # Funder
  p <- oa_fetch(identifier = "P4310311775") # Publisher
  s <- oa_fetch(identifier = "S1983995261") # Source
  co <- oa_fetch(identifier = "C2522767166") # Concept

  expect_s3_class(w, "data.frame")
  expect_s3_class(a, "data.frame")
  expect_s3_class(i, "data.frame")
  expect_s3_class(f, "data.frame")
  expect_s3_class(p, "data.frame")
  expect_s3_class(s, "data.frame")
  expect_s3_class(co, "data.frame")

  expect_equal(dim(w), c(1, 43))
  expect_equal(dim(a), c(1, 14))
  expect_equal(dim(i), c(1, 22))
  expect_equal(dim(f), c(1, 17))
  expect_equal(dim(p), c(1, 19))
  expect_equal(dim(s), c(1, 27))
  expect_equal(dim(co), c(1, 16))

})

test_that("oa_fetch for identifiers works with options", {
  skip_on_cran()

  i <- oa_fetch(
    identifier = "I201448701",
    options = list(select = c("ids", "country_code"))
  )

  a <- oa_fetch(
    identifier = "A5023888391",
    options = list(select = c("display_name", "orcid", "cited_by_count"))
  )

  expect_equal(dim(i), c(1, 2))
  expect_equal(dim(a), c(1, 3))
})

test_that("different paging methods yield the same result", {
  skip_on_cran()

  w0 <- oa_fetch(
    entity = "works",
    title.search = c("bibliometric analysis", "science mapping"),
    cited_by_count = ">50",
    options = list(select = "id"),
    from_publication_date = "2021-01-01",
    to_publication_date = "2021-12-31",
    verbose = TRUE
  )

  w24 <- oa_fetch(
    entity = "works",
    title.search = c("bibliometric analysis", "science mapping"),
    cited_by_count = ">50",
    options = list(select = "id"),
    from_publication_date = "2021-01-01",
    to_publication_date = "2021-12-31",
    verbose = TRUE,
    pages = c(2, 4:5),
    per_page = 10
  )
  
  expect_equal(
    w0[c(11:20, 31:min(50, nrow(w0))), ],
    w24
  )

})

test_that("pages works", {
  skip_on_cran()

  # The last 10 pages when per_page = 20
  # should be the same as the 10 pages when fetching page 2
  w1 <- oa_fetch(
    search = "transformative change",
    options = list(select = c("id", "display_name", "publication_date")),
    pages = 1,
    per_page = 20,
    verbose = TRUE
  )
  w2 <- oa_fetch(
    search = "transformative change",
    options = list(select = c("id", "display_name", "publication_date")),
    pages = 2,
    per_page = 10,
    verbose = TRUE
  )
  expect_equal(w1[11:20,], w2)
})

test_that("output=raw works", {
  skip_on_cran()

  output_raw <- oa_fetch(
    entity = "works",
    search = "language",
    per_page = 2,
    options = list(sample = 5, seed = 1),
    output = "raw"
  )

  # length and type checks
  expect_type(output_raw, "character")
  expect_length(output_raw, ceiling(5 / 2)) # length determined by pages
  raw_parsed <- lapply(output_raw, function(x) {
    jsonlite::fromJSON(x, simplifyVector = FALSE)$results
  })
  expect_equal(lengths(raw_parsed), c(2, 2, 1)) # num results in each page
  raw_parsed_flattened <- unlist(raw_parsed, recursive = FALSE)

  # equivalence check to tibble format
  expect_identical(
    works2df(raw_parsed_flattened),
    oa_fetch(
      entity = "works",
      search = "language",
      per_page = 2,
      options = list(sample = 5, seed = 1),
      output = "tibble"
    )
  )

})
