test_that("show_authors works", {
  skip_on_cran()

  author_ids <- c("A5023888391", "A5014077037")
  author_simplified <- show_authors(oa_fetch(identifier = author_ids))

  expect_s3_class(author_simplified, "data.frame")
  expect_equal(nrow(author_simplified), length(author_ids))
  expect_type(author_simplified$top_concepts, "character")
})

test_that("show_works works", {
  skip_on_cran()

  work_ids <- c("W2741809807", "W2755950973")
  work_simplified <- show_works(oa_fetch(identifier = work_ids))

  expect_s3_class(work_simplified, "data.frame")
  expect_equal(nrow(work_simplified), length(work_ids))
  expect_type(work_simplified$top_concepts, "character")
})

test_that("show_works() and show_authors() handle NULL input", {
  expect_warning(
    work_empty <- show_works(NULL),
    class = "openalexR_empty_show"
  )
  expect_s3_class(work_empty, "tbl_df")
  expect_equal(nrow(work_empty), 0L)
  expect_named(
    work_empty,
    c("id", "display_name", "first_author", "last_author", "top_concepts")
  )

  expect_warning(
    author_empty <- show_authors(NULL),
    class = "openalexR_empty_show"
  )
  expect_equal(nrow(author_empty), 0L)
  expect_named(
    author_empty,
    c(
      "id",
      "display_name",
      "orcid",
      "works_count",
      "cited_by_count",
      "top_concepts"
    )
  )
  expect_type(author_empty$works_count, "integer")
})

test_that("show_works() applies simp_func on the empty path", {
  expect_warning(
    out <- show_works(NULL, simp_func = identity),
    class = "openalexR_empty_show"
  )
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0L)
})

test_that("show_works() accepts a zero-row data frame without warning", {
  zero <- tibble::tibble(
    id = character(0),
    display_name = character(0),
    authorships = list(),
    concepts = list()
  )
  expect_no_warning(out <- show_works(zero))
  expect_equal(nrow(out), 0L)
})

test_that("show_works() rejects input that is not a data frame", {
  expect_error(show_works(1:3), "must be a data frame")
  expect_error(show_works("W2741809807"), "must be a data frame")
})

test_that("show_*() report missing columns", {
  expect_error(
    show_works(data.frame(display_name = "x")),
    "missing required columns"
  )
  expect_error(show_authors(data.frame(id = "A1")), "missing required columns")
})

test_that("a failed request flows to show_works() without a cryptic error", {
  # Regression for ropensci/openalexR#368: a rate-limited request made
  # oa_fetch() return NULL, and show_works(NULL) failed with
  # "incorrect number of dimensions".
  local_mocked_bindings(
    api_request = function(..., parse = TRUE) if (parse) list() else "{}"
  )

  expect_warning(
    res <- oa_fetch(identifier = "W2741809807"),
    class = "openalexR_empty_response"
  )
  expect_null(res)

  expect_warning(shown <- show_works(res), class = "openalexR_empty_show")
  expect_s3_class(shown, "tbl_df")
  expect_equal(nrow(shown), 0L)
})
