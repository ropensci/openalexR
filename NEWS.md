# openalexR (development version)
* Breaking change: Reorder of the first two arguments in `oa_fetch`: `entity` now comes before `idenfitifier`.
This should not affect your workflow too much unless you have been getting article information from OpenAlex IDs.
* `oa_ngrams` gets you N-grams of works
* Bug fixes

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
