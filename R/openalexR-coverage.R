#' Get coverage of OpenAlex fields in openalexR
#'
#' @param entity The OA entity to inspect field coverage for. Returns
#'   information for all fields if `NULL` (default)
#'
#' @return Data frame of field coverage information
#' @export
#'
#' @examples
#' head(get_coverage(entity = "works"))
get_coverage <- function(entity = NULL) {
  if (!is.null(entity)) {
    oa2df_coverage[[entity]]
  } else {
    oa2df_coverage
  }
}

#' @keywords internal
oa2df_coverage <- list()

oa2df_coverage$works <- tibble::tribble(
                        ~original,                   ~covered_by,                                                ~comments,
        "abstract_inverted_index",                          "ab",                      "reconstructed from inverted index",
                       "apc_list",                            NA,                                                       NA,
                       "apc_paid",                            NA,                                                       NA,
                    "authorships",                      "author",                                                       NA,
               "best_oa_location",                            NA,                                                       NA,
                         "biblio",                      "volume",                                                       NA,
                         "biblio",                       "issue",                                                       NA,
                         "biblio",                  "first_page",                                                       NA,
                         "biblio",                   "last_page",                                                       NA,
               "cited_by_api_url",            "cited_by_api_url",                                                       NA,
                 "cited_by_count",              "cited_by_count",                                                       NA,
       "cited_by_percentile_year",                            NA,                                                       NA,
                       "concepts",                    "concepts",                                                       NA,
       "corresponding_author_ids",                            NA,         "reconstructable from `author$is_corresponding`",
  "corresponding_institution_ids",                            NA,         "reconstructable from `author$is_corresponding`",
       "countries_distinct_count",                            NA, "reconstructable from `author$institution_country_code`",
                 "counts_by_year",              "counts_by_year",                                                       NA,
                   "created_date",                            NA,                                                       NA,
                   "display_name",                "display_name",                                                       NA,
                            "doi",                         "doi",                                                       NA,
                "fulltext_origin",                            NA,                                                       NA,
                         "grants",                      "grants",                                                       NA,
                   "has_fulltext", "any_repository_has_fulltext",                                                       NA,
                             "id",                          "id",                                                       NA,
                            "ids",                         "ids",                                                       NA,
                     "indexed_in",                            NA,                                                       NA,
    "institutions_distinct_count",                            NA,           "reconstructable from `author$institution_id`",
                    "is_paratext",                 "is_paratext",                                                       NA,
                   "is_retracted",                "is_retracted",                                                       NA,
                       "keywords",                            NA,                                                       NA,
                       "language",                    "language",                                                       NA,
                      "locations",                            NA,                                                       NA,
                "locations_count",                            NA,                                                       NA,
                           "mesh",                            NA,                                                       NA,
                     "ngrams_url",                            NA,                                      "see `oa_ngrams()`",
                    "open_access",              "is_oa_anywhere",                                            "see PR #135",
                    "open_access",                   "oa_status",                                                       NA,
                    "open_access",                      "oa_url",                                                       NA,
               "primary_location",                       "is_oa",                                                       NA,
               "primary_location",                          "so",                                                       NA,
               "primary_location",                       "so_id",                                                       NA,
               "primary_location",           "host_organization",                                                       NA,
               "primary_location",                      "issn_l",                                                       NA,
               "primary_location",                         "url",                                                       NA,
               "primary_location",                     "pdf_url",                                                       NA,
               "primary_location",                     "license",                                                       NA,
               "primary_location",                     "version",                                                       NA,
                  "primary_topic",                            NA,                                                       NA,
               "publication_date",            "publication_date",                                                       NA,
               "publication_year",            "publication_year",                                                       NA,
               "referenced_works",            "referenced_works",                                                       NA,
         "referenced_works_count",                            NA,                "reconstructable from `referenced_works`",
                  "related_works",               "related_works",                                                       NA,
  "sustainable_development_goals",                            NA,                                                       NA,
                          "title",                "display_name",                                                       NA,
                         "topics",                            NA,                                                       NA,
                           "type",                        "type",                                                       NA,
                  "type_crossref",                            NA,                                                       NA,
                   "updated_date",                            NA,                                                       NA
  )









