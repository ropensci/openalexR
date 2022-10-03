test_that("Invalid filter errors out", {
  # open_alex is not a valid filter
  query_url <- "https://api.openalex.org/authors?filter=open_alex%3AA923435168%7CA2208157607"

  expect_error(oa_request(query_url))
})

test_that("oa_request returns list", {
  query_url <- "https://api.openalex.org/authors?filter=openalex%3AA923435168%7CA2208157607"

  expect_type(oa_request(query_url), "list")
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

test_that("oa_fetch can combine (OR) more than 50 DOIs in a filter", {
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
    "https://doi.org/10.1016/j.iot.2020.100318")

  many_doi_results <- oa_fetch(entity = "works", doi = valid_dois)

  expect_s3_class(
    many_doi_results,
    "data.frame"
  )

  expect_equal(nrow(many_doi_results), length(valid_dois))
})


test_that("oa_fetch can combine (OR) more than 50 ORCIDs in a filter", {
  valid_orcids <- c(
    "https://orcid.org/0000-0002-8525-3159",
    "https://orcid.org/0000-0001-7641-0637",
    "https://orcid.org/0000-0002-6465-982X",
    "https://orcid.org/0000-0002-8270-3134",
    "https://orcid.org/0000-0001-8446-2349",
    "https://orcid.org/0000-0002-3128-0135",
    "https://orcid.org/0000-0001-6428-8611",
    "https://orcid.org/0000-0002-4951-4526",
    "https://orcid.org/0000-0002-6354-3913",
    "https://orcid.org/0000-0001-7523-7967",
    "https://orcid.org/0000-0002-3792-0818",
    "https://orcid.org/0000-0002-9412-2556",
    "https://orcid.org/0000-0002-4147-892X",
    "https://orcid.org/0000-0002-7060-8404",
    "https://orcid.org/0000-0001-9080-6267",
    "https://orcid.org/0000-0002-8057-6864",
    "https://orcid.org/0000-0002-7369-2058",
    "https://orcid.org/0000-0002-9460-5144",
    "https://orcid.org/0000-0002-8517-9411",
    "https://orcid.org/0000-0003-1345-9649",
    "https://orcid.org/0000-0001-6048-7277",
    "https://orcid.org/0000-0001-5882-1168",
    "https://orcid.org/0000-0001-9558-6099",
    "https://orcid.org/0000-0003-3421-5627",
    "https://orcid.org/0000-0002-8850-6764",
    "https://orcid.org/0000-0002-0811-6580",
    "https://orcid.org/0000-0001-6535-5492",
    "https://orcid.org/0000-0001-8934-7569",
    "https://orcid.org/0000-0002-1355-9175",
    "https://orcid.org/0000-0001-8693-5947",
    "https://orcid.org/0000-0003-4126-9244",
    "https://orcid.org/0000-0001-9102-9359",
    "https://orcid.org/0000-0002-5119-8358",
    "https://orcid.org/0000-0001-5035-5983",
    "https://orcid.org/0000-0002-2817-5377",
    "https://orcid.org/0000-0002-5935-7544",
    "https://orcid.org/0000-0001-9059-7442",
    "https://orcid.org/0000-0003-2796-9148",
    "https://orcid.org/0000-0002-5639-3128",
    "https://orcid.org/0000-0001-6591-5015",
    "https://orcid.org/0000-0002-7319-418X",
    "https://orcid.org/0000-0003-1759-1700",
    "https://orcid.org/0000-0003-4867-5149",
    "https://orcid.org/0000-0002-2622-0672",
    "https://orcid.org/0000-0003-1013-5809",
    "https://orcid.org/0000-0001-5200-1476",
    "https://orcid.org/0000-0001-9443-8123",
    "https://orcid.org/0000-0002-4180-2216",
    "https://orcid.org/0000-0003-1761-3180",
    "https://orcid.org/0000-0002-3721-1122",
    "https://orcid.org/0000-0001-6618-8542"
  )

  many_orcid_results <- oa_fetch(entity = "authors", orcid = valid_orcids)

  expect_s3_class(
    many_orcid_results,
    "data.frame"
  )
  # https://orcid.org/0000-0002-4147-892X corresponds to two openalex id
  expect_true(nrow(many_orcid_results) >= length(valid_orcids))
})

