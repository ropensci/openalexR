test_that("field coverage information is up to date", {
  skip_on_cran()

  # main part of s (before .), summary_stats.i10_index -> summary_stats
  main_s <- function(s) {
    sort(unique(sapply(strsplit(s, "\\."), \(x) x[[1]]))) |>
      setdiff(c("relevance_score"))
    # Additional fields from the parent Search entity
    # `fulltext_origin` only available for works with `has_fulltext:true`
  }

  cols <- lapply(get_coverage(), function(x) {
    covered_by <- main_s(x$oa2df[!is.na(x$oa2df)])
  })

  oris <- lapply(get_coverage(), function(x) {
    main_s(x$original[!is.na(x$original)])
  })

  # Works
  out_list <- oa_fetch(identifier = "W2741809807", output = "list")
  out <- oa2df(out_list, "works")
  expect_equal(main_s(names(out_list)), oris$works)
  expect_equal(main_s(colnames(out)), cols$works)

  # Authors
  out_list <- oa_fetch(identifier = "A5023888391", output = "list")
  out <- oa2df(out_list, "authors")
  expect_equal(main_s(names(out_list)), oris$authors)
  expect_equal(main_s(colnames(out)), cols$authors)

  # Topics
  out_list <- oa_fetch(identifier = "T11636", output = "list")
  out <- oa2df(out_list, "topics")
  expect_equal(main_s(names(out_list)), oris$topics)
  expect_equal(main_s(colnames(out)), cols$topics)

  # Keywords
  out_list <- oa_fetch(
    identifier = "medicine",
    entity = "keywords",
    output = "list"
  )
  out <- oa2df(out_list, "keywords")
  expect_equal(main_s(names(out_list)), oris$keywords)
  expect_equal(main_s(colnames(out)), cols$keywords)

  # Institutions
  out_list <- oa_fetch(identifier = "I27837315", output = "list")
  out <- oa2df(out_list, "institutions")
  expect_equal(main_s(names(out_list)), oris$institutions)
  expect_equal(main_s(colnames(out)), cols$institutions)

  # Sources
  out_list <- oa_fetch(identifier = "S137773608", output = "list")
  out <- oa2df(out_list, "sources")
  expect_equal(main_s(names(out_list)), oris$sources)
  expect_equal(main_s(colnames(out)), cols$sources)

  # Publishers
  # out_list <- oa_fetch(identifier = "P4310311775", output = "list")
  out_list <- oa_fetch(identifier = "P4310320990", output = "list")
  out <- oa2df(out_list, "publishers")
  expect_equal(main_s(names(out_list)), oris$publishers)
  expect_equal(main_s(colnames(out)), cols$publishers)

  # Funders
  out_list <- oa_fetch(identifier = "F4320332161", output = "list")
  out <- oa2df(out_list, "funders")
  expect_equal(main_s(names(out_list)), oris$funders)
  expect_equal(main_s(colnames(out)), cols$funders)

  # # Concepts
  # out_list <- oa_fetch(identifier = "C137773608", output = "list")
  # out <- oa2df(out_list, "concepts")
  # expect_equal(main_s(names(out_list)), main_s(oris$concepts))
  # expect_equal(main_s(colnames(out)), main_s(cols$concepts))
})
