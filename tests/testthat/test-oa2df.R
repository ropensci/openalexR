test_that("oa2df converts every entity type offline", {
  samples <- readRDS(test_path("fixtures", "entity_list_samples.rds"))

  for (en in names(samples)) {
    df <- suppressWarnings(oa2df(samples[[en]], entity = en, verbose = FALSE))
    expect_s3_class(df, "tbl_df")
    expect_equal(nrow(df), length(samples[[en]]), info = en)
    expect_true("id" %in% names(df), info = en)
    expect_true(all(grepl("openalex.org", df$id)), info = en)
  }
})

test_that("oa2df builds derived columns for works", {
  samples <- readRDS(test_path("fixtures", "entity_list_samples.rds"))
  works <- suppressWarnings(oa2df(samples$works, entity = "works", verbose = FALSE))

  # abstract reconstructed from the inverted index
  expect_type(works$abstract, "character")
  # nested authorship table with hoisted source fields
  expect_type(works$authorships, "list")
  expect_s3_class(works$authorships[[1]], "data.frame")
  expect_true("display_name" %in% names(works$authorships[[1]]))
  expect_true(all(c("topics", "source_display_name") %in% names(works)))

  # abstract = FALSE drops the abstract column
  no_ab <- suppressWarnings(
    oa2df(samples$works, entity = "works", abstract = FALSE, verbose = FALSE)
  )
  expect_false("abstract" %in% names(no_ab))
})

test_that("oa2df hoists nested fields for authors, institutions and topics", {
  samples <- readRDS(test_path("fixtures", "entity_list_samples.rds"))

  authors <- suppressWarnings(oa2df(samples$authors, entity = "authors", verbose = FALSE))
  # summary_stats are spread into their own columns
  expect_true(all(c("h_index", "i10_index") %in% names(authors)))
  expect_type(authors$last_known_institutions, "list")

  institutions <- suppressWarnings(
    oa2df(samples$institutions, entity = "institutions", verbose = FALSE)
  )
  expect_true("geo" %in% names(institutions))
  expect_type(institutions$topics, "list")

  topics <- suppressWarnings(oa2df(samples$topics, entity = "topics", verbose = FALSE))
  # subfield/field/domain are flattened to *_id / *_display_name columns
  expect_true(all(
    c("subfield_id", "field_display_name", "domain_id") %in% names(topics)
  ))
})

test_that("oa2df returns NULL for empty input", {
  expect_null(suppressWarnings(oa2df(list(), entity = "works", verbose = FALSE)))
})

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
