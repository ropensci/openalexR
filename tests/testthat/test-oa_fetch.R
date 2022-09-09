test_that("Invalid filter errors out", {
  query_url <- "https://api.openalex.org/authors?filter=openalex%3AA923435168%7CA2208157607"

  expect_error(oa_request(query_url))
})


test_that("oa_fetch works", {
  work_ids <- c("W2741809807")
  multi_works <- oa_fetch(
    identifier = work_ids,
    verbose = TRUE
  )
  expect_equal(
    sort(multi_works$id),
    paste0("https://openalex.org/", sort(work_ids))
  )

  expect_s3_class(
    oa_fetch(
      entity = "w",
      publication_date = "2020-08-01",
      cited_by_count = ">1000"
    ),
    "data.frame"
  )
})

test_that("Error when input entity can't be matched", {
  expect_error(
    oa_fetch(
      entity = "wa",
      publication_date = "2020-08-01",
      cited_by_count = ">1000"
    )
  )

  expect_error(
    oa_fetch(
      entity = "insta",
      display_name.search = "University of Florida"
    )
  )
})

test_that("oa_fetch authors can deal with NA institutions", {
  # Old error:
  # Error in rbind(deparse.level, ...) :
  #   numbers of columns of arguments do not match

  expect_s3_class(
    oa_fetch(
      entity = "authors",
      orcid = "0000-0001-7482-0480"),
    "data.frame"
  )

  expect_type(
    oa_fetch(
      entity = "authors",
      orcid = "0000-0001-7482-0480",
      output = "list"),
    "list"
  )
})

