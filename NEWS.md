# openalexR (development version)
* Breaking change: two arguments in `oa_snowball` are renamed:
citing_filter is now citing_params,
and cited_by_filter is now cited_by_params.
* improve `oa_snowball` performance
* allowed the use of `options$sample` with `search`

# openalexR 1.2.2
* solved issue with CRAN test
* 
# openalexR 1.2.1
* many improvements in bibliometrix support
* solved issue with CRAN test

# openalexR 1.2.0
* many improvements in oa_snowball
* added new openalex entities
* solved CRAN issue about  packageVersion() 

# openalexR 1.1.0
* Basic paging is applied when using options$sample
* Bug fixes for rbind in oa2df

# openalexR 1.0.2.9
* Breaking change: Reorder of the first two arguments in `oa_fetch`: `entity` now comes before `idenfitifier`.
This should not affect your workflow too much unless you have been getting article information from OpenAlex IDs.
* new arguments to `oa_fetch`: `sample` and `seed` allows the user to download a random subset of the entities instead of the entire set.
* `oa_ngrams` gets you N-grams of works
* `abstract` now defaults to TRUE to avoid issues.
* New argument to oa_fetch: api_key
* Arguments sample, seed, sort, and select are now grouped into `options`.
* Bug fixes
* Improved documentation: group functions in Reference, details on search, etc.

# openalexR 1.0.1

* Improve `snowball` #9.
* Batch queries when a filter has more than 50 values #18.
* Bug fixes.
* Added a `NEWS.md` file to track changes to the package.

# openalexR 1.0.0
* Breaking change: now uses a more canonical way to filter #7.
* In `oa_fetch`, `abstract` now defaults to FALSE to save compute/query time/space. You will need to set `abstract = TRUE` to retrieve abstracts for the articles.
* Added website, hex.
* Added tests.
* Added R CMD CHECK as a GH Action
* Bug fixes.
* Added vignettes.

# openalexR 0.0.2
* Some bug fixes

# openalexR 0.0.1
* First CRAN release
