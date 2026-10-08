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

test_that("oa2df preserves nested keyword fields in one row per record", {
  topic <- list(
    id = "https://openalex.org/T14475",
    display_name = "History of Science and Medicine",
    score = 0.1061,
    subfield = list(id = "https://openalex.org/subfields/1207")
  )
  keyword <- list(
    id = "https://openalex.org/keywords/medicine",
    display_name = "medicine",
    description = "Medical research",
    display_name_alternatives = list(),
    ids = list(openalex = "https://openalex.org/keywords/medicine",
               wikidata = "https://www.wikidata.org/wiki/Q11190"),
    primary_topic = NULL,
    topics = list(topic, topic),
    works_count = 22021L
  )
  other <- keyword
  other$id <- "https://openalex.org/keywords/biology"
  other$display_name <- "biology"
  other$display_name_alternatives <- list("Biology", "Biological science")
  other$primary_topic <- topic
  other$topics <- list()
  other$description <- NULL

  df <- oa2df(list(keyword, other), entity = "keywords", verbose = FALSE)
  expect_equal(nrow(df), 2L)
  expect_equal(df$display_name, c("medicine", "biology"))
  expect_equal(df$works_count, c(22021L, 22021L))
  expect_equal(df$description, c("Medical research", NA_character_))
  expect_identical(df$topics, list(keyword$topics, other$topics))
  expect_identical(df$ids[[1]], keyword$ids)
  expect_identical(df$primary_topic, list(NA, topic))
  expect_identical(df$display_name_alternatives,
                   list(list(), other$display_name_alternatives))

  single <- oa2df(keyword, entity = "keywords", verbose = FALSE)
  expect_equal(nrow(single), 1L)
  expect_identical(single$topics[[1]], keyword$topics)
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
  expect_equal(tolower(medicine$display_name), "medicine")
  expect_s3_class(medicine, "data.frame")
  expect_s3_class(medicine, "tbl")
})
