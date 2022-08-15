
<!-- README.md is generated from README.Rmd. Please edit that file -->

# openalexR <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->
<!-- badges: end -->

**openalexR** helps you interface with the
[OpenAlex](https://openalex.org) API to retrieve bibliographic
infomation about publications, authors, venues, institutions and
concepts with 4 main functions:

-   `oa_query()`: generates a valid query, written following the
    OpenAlex API syntax, from a set of arguments provided by the user.

-   `oa_request()`: downloads a collection of entities matching the
    query created by `oa_query()` or manually written by the user, and
    returns a JSON object in a list format.

-   `oa2df()`: converts the JSON object in classical bibliographic
    tibble/data frame.

-   `oa_fetch()`: composes three functions above so the user can execute
    everything in one step, *i.e.*, `oa_query |> oa_request |> oa2df`

## Installation

You can install the developer version of the openalexR from
[GitHub](https://github.com) with:

``` r
install.packages("remotes")
remotes::install_github("massimoaria/openalexR")
```

You can install the released version of openalexR from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("openalexR")
```

## Package loading

``` r
library(openalexR)
library(dplyr)
```

## Works (think papers, publications)

This paper:

    Aria, M., & Cuccurullo, C. (2017). bibliometrix: 
    An R-tool for comprehensive science mapping analysis. 
    Journal of informetrics, 11(4), 959-975.

is associated to the
[OpenAlex-id](https://docs.openalex.org/about-the-data#the-openalex-id)
**W2755950973**. If you know your paper’s OpenAlex ID, all you need to
do is passing `identifier = <openalex id>` as an argument in
`oa_fetch()`:

``` r
paper_id <- oa_fetch(
  identifier = "W2755950973",
  entity = "works",
  verbose = TRUE
)
#> [1] "https://api.openalex.org/works/W2755950973"
#> Requesting url: https://api.openalex.org/works/W2755950973
#> Warning: replacing previous import 'lifecycle::last_warnings' by
#> 'rlang::last_warnings' when loading 'hms'
dplyr::glimpse(paper_id)
#> Rows: 1
#> Columns: 25
#> $ id            <chr> "https://openalex.org/W2755950973"
#> $ TI            <chr> "bibliometrix: An R-tool for comprehensive science mappi…
#> $ author        <list> [<data.frame[2 x 10]>]
#> $ AB            <chr> "Abstract The use of bibliometrics is gradually extendin…
#> $ pubdata       <chr> "2017-11-01"
#> $ SO            <chr> "Journal of Informetrics"
#> $ SO_ID         <chr> "https://openalex.org/V205292342"
#> $ PU            <chr> "Elsevier"
#> $ IS            <list> <"1875-5879", "1751-1577">
#> $ URL           <chr> "https://doi.org/10.1016/j.joi.2017.08.007"
#> $ first_page    <chr> "959"
#> $ last_page     <chr> "975"
#> $ volume        <chr> "11"
#> $ issue         <chr> "4"
#> $ OA            <lgl> FALSE
#> $ TC            <int> 1456
#> $ TCperYear     <list> [<data.frame[5 x 2]>]
#> $ PY            <int> 2017
#> $ cited_by_url  <chr> "https://api.openalex.org/works?filter=cites:W2755950973"
#> $ ids           <list> [[<data.frame[3 x 2]>]]
#> $ DI            <chr> "https://doi.org/10.1016/j.joi.2017.08.007"
#> $ DT            <chr> "journal-article"
#> $ CR            <list> <"https://openalex.org/W189804332", "https://openalex.o…
#> $ related_works <list> <"https://openalex.org/W150292108", "https://openalex.or…
#> $ concept       <list> [<data.frame[8 x 5]>]
```

`oa_fetch()` is a composition of functions:
`oa_query |> oa_request |> oa2df`. As results, `oa_query()` returns the
query string including the OpenAlex endpoint API server address
(default). `oa_request()` downloads the bibliographic records matching
the query. Finally, `oa2df()` converts the final result list to a
tibble. The final result is a complicated tibble, but we can use
`show_works()` to display a simplified version:

``` r
paper_id %>% 
  show_works() %>%
  knitr::kable()
```

| short_id    | TI                                                                 | first_author | last_author        | SO                      | URL                                         | OA    | top_concepts                                                                     |
|:------------|:-------------------------------------------------------------------|:-------------|:-------------------|:------------------------|:--------------------------------------------|:------|:---------------------------------------------------------------------------------|
| W2755950973 | bibliometrix: An R-tool for comprehensive science mapping analysis | Massimo Aria | Corrado Cuccurullo | Journal of Informetrics | <https://doi.org/10.1016/j.joi.2017.08.007> | FALSE | Workflow, Bibliometrics, Computer science, Data science, Software, Software tool |

### External id formats

OpenAlex endpoint accepts OpenAlex IDs and other [external
IDs](https://docs.openalex.org/api/get-single-entities#id-formats)
(*e.g.*, DOI, ISSN) in several formats, including Digital Object
Identifier (DOI) and Persistent Identifiers (PIDs).

``` r
oa_fetch(
  # identifier = "https://doi.org/10.1016/j.joi.2017.08.007", # would also work (PIDs)
  identifier = "doi:10.1016/j.joi.2017.08.007",
  entity = "works"
) %>% 
  show_works() %>%
  knitr::kable()
```

| short_id    | TI                                                                 | first_author | last_author        | SO                      | URL                                         | OA    | top_concepts                                                                     |
|:------------|:-------------------------------------------------------------------|:-------------|:-------------------|:------------------------|:--------------------------------------------|:------|:---------------------------------------------------------------------------------|
| W2755950973 | bibliometrix: An R-tool for comprehensive science mapping analysis | Massimo Aria | Corrado Cuccurullo | Journal of Informetrics | <https://doi.org/10.1016/j.joi.2017.08.007> | FALSE | Workflow, Bibliometrics, Computer science, Data science, Software, Software tool |

### More than one publications/authors

[https://api.openalex.org/authors/https://orcid.org/](https://api.openalex.org/authors/https://orcid.org/0000-0003-1613-5981)

If you know the OpenAlex IDs of these entities, you can also feed them
into the `identifier` argument.

``` r
oa_fetch(
  identifier = c("W2741809807", "W2755950973"),
  # identifier = c("https://doi.org/10.1016/j.joi.2017.08.007", "https://doi.org/10.1016/j.joi.2017.08.007"), # TODO
  entity = "works",
  verbose = TRUE
) %>% 
  show_works() %>%
  knitr::kable()
#> [1] "https://api.openalex.org/works?filter=openalex_id%3AW2741809807%7CW2755950973"
#> Requesting url: https://api.openalex.org/works?filter=openalex_id%3AW2741809807%7CW2755950973
#> About to get a total of 1 pages of results with a total of 2 records.
```

| short_id    | TI                                                                                           | first_author       | last_author        | SO                      | URL                                         | OA    | top_concepts                                                                            |
|:------------|:---------------------------------------------------------------------------------------------|:-------------------|:-------------------|:------------------------|:--------------------------------------------|:------|:----------------------------------------------------------------------------------------|
| W2741809807 | The state of OA: a large-scale analysis of the prevalence and impact of Open Access articles | Heather A. Piwowar | Stefanie Haustein  | PeerJ                   | <https://doi.org/10.7717/peerj.4375>        | TRUE  | Citation, License, Scholarly communication, Web of science, Bibliometrics, Open science |
| W2755950973 | bibliometrix: An R-tool for comprehensive science mapping analysis                           | Massimo Aria       | Corrado Cuccurullo | Journal of Informetrics | <https://doi.org/10.1016/j.joi.2017.08.007> | FALSE | Workflow, Bibliometrics, Computer science, Data science, Software, Software tool        |

However, if you only know their external identifies, say, DOIs, you
would need to use `doi` as a filter (either the canonical form with
<https://doi.org/> or without should work):

``` r
oa_fetch(
  # identifier = c("W2741809807", "W2755950973"),
  doi = c("10.1016/j.joi.2017.08.007", "https://doi.org/10.1093/bioinformatics/btab727"),
  entity = "works",
  verbose = TRUE
) %>% 
  show_works() %>%
  knitr::kable()
#> [1] "https://api.openalex.org/works?filter=doi%3A10.1016%2Fj.joi.2017.08.007%7Chttps%3A%2F%2Fdoi.org%2F10.1093%2Fbioinformatics%2Fbtab727"
#> Requesting url: https://api.openalex.org/works?filter=doi%3A10.1016%2Fj.joi.2017.08.007%7Chttps%3A%2F%2Fdoi.org%2F10.1093%2Fbioinformatics%2Fbtab727
#> About to get a total of 1 pages of results with a total of 2 records.
```

| short_id    | TI                                                                                     | first_author     | last_author        | SO                      | URL                                              | OA    | top_concepts                                                                                                |
|:------------|:---------------------------------------------------------------------------------------|:-----------------|:-------------------|:------------------------|:-------------------------------------------------|:------|:------------------------------------------------------------------------------------------------------------|
| W3206431085 | PMLB v1.0: an open-source dataset collection for benchmarking machine learning methods | Joseph D. Romano | Jason H. Moore     | Bioinformatics          | <https://doi.org/10.1093/bioinformatics/btab727> | TRUE  | Benchmarking, Python (programming language), Computer science, Benchmark (surveying), Open source, Workflow |
| W2755950973 | bibliometrix: An R-tool for comprehensive science mapping analysis                     | Massimo Aria     | Corrado Cuccurullo | Journal of Informetrics | <https://doi.org/10.1016/j.joi.2017.08.007>      | FALSE | Workflow, Bibliometrics, Computer science, Data science, Software, Software tool                            |

### Filters

In most cases, we are interested in downloading a collection of items
that meet one or more inclusion/exclusion criteria (filters). Supported
attributes for each endpoints are listed
[here](https://docs.openalex.org/api/get-lists-of-entities/filter-entity-lists).

**Example**: We want to download all works that have been cited more
than 50 times, published between 2020 and 2021, and include the strings
“bibliometric analysis” or “science mapping” in the title. Maybe we also
want the results to be sorted by total citations in a descending order.

Setting the argument `count_only = TRUE`, the function `oa_request()`
returns the number of items matching the query without downloading the
collection.

``` r
oa_fetch(
  identifier = NULL,
  entity = "works",
  title.search = c("bibliometric analysis", "science mapping"),
  cited_by_count = ">50", 
  from_publication_date = "2020-01-01",
  to_publication_date = "2021-12-31",
  search = NULL,
  sort = "cited_by_count:desc",
  endpoint = "https://api.openalex.org/",
  count_only = TRUE,
  verbose = TRUE
)
#> [1] "https://api.openalex.org/works?filter=title.search%3Abibliometric%20analysis%7Cscience%20mapping%2Ccited_by_count%3A%3E50%2Cfrom_publication_date%3A2020-01-01%2Cto_publication_date%3A2021-12-31&sort=cited_by_count%3Adesc"
#> Requesting url: https://api.openalex.org/works?filter=title.search%3Abibliometric%20analysis%7Cscience%20mapping%2Ccited_by_count%3A%3E50%2Cfrom_publication_date%3A2020-01-01%2Cto_publication_date%3A2021-12-31&sort=cited_by_count%3Adesc
#>               count db_response_time_ms                page            per_page 
#>                  23                  19                   1                   1
```

We can now download the records and transform it into a tibble/data
frame by setting `count_only = FALSE` (also the default value):

``` r
oa_fetch(
  entity = "works",
  title.search = c("bibliometric analysis", "science mapping"),
  cited_by_count = ">50", 
  from_publication_date = "2020-01-01",
  to_publication_date = "2021-12-31",
  sort = "cited_by_count:desc",
  count_only = FALSE
) %>%
  show_works() %>%
  knitr::kable()
```

| short_id    | TI                                                                                                                            | first_author        | last_author        | SO                                           | URL                                             | OA    | top_concepts                                                                                                                                                                      |
|:------------|:------------------------------------------------------------------------------------------------------------------------------|:--------------------|:-------------------|:---------------------------------------------|:------------------------------------------------|:------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| W3160856016 | How to conduct a bibliometric analysis: An overview and guidelines                                                            | Naveen Donthu       | Weng Marc Lim      | Journal of Business Research                 | <https://doi.org/10.1016/j.jbusres.2021.04.070> | TRUE  | Bibliometrics, Field (mathematics), Data science, Resource (disambiguation), Computer science, Management science                                                                 |
| W3038273726 | Investigating the emerging COVID-19 research trends in the field of business and management: A bibliometric analysis approach | Surabhi Verma       | Anders Gustafsson  | Journal of Business Research                 | <https://doi.org/10.1016/j.jbusres.2020.06.057> | TRUE  | Coronavirus disease 2019 (COVID-19), Field (mathematics), Bibliometrics, 2019-20 coronavirus outbreak, Severe acute respiratory syndrome coronavirus 2 (SARS-CoV-2), Data science |
| W2990450011 | Forty-five years of Journal of Business Research: A bibliometric analysis                                                     | Naveen Donthu       | Debidutta Pattnaik | Journal of Business Research                 | <https://doi.org/10.1016/j.jbusres.2019.10.039> | FALSE | Bibliometrics, Library science, Data science                                                                                                                                      |
| W3001491100 | Software tools for conducting bibliometric analysis in science: An up-to-date review                                          | Jose A. Moral-Munoz | Manuel Cobo        | Profesional De La Informacion                | <https://doi.org/10.3145/epi.2020.ene.03>       | TRUE  | Bibliometrics, Software, Computer science, Data science, Library science, Software engineering                                                                                    |
| W3011866596 | A Bibliometric Analysis of COVID-19 Research Activity: A Call for Increased Output                                            | Mohamad A. Chahrour | Hussein H. Khachfe | Cureus                                       | <https://doi.org/10.7759/cureus.7357>           | TRUE  | Medicine, Pandemic, Observational study, Gross domestic product, Coronavirus disease 2019 (COVID-19), Population                                                                  |
| W3000910650 | Disruption risks in supply chain management: a literature review based on bibliometric analysis                               | Song Xu             | Wenting Yang       | International Journal of Production Research | <https://doi.org/10.1080/00207543.2020.1717011> | TRUE  | Supply chain, Supply chain management, Supply chain risk management, Risk analysis (engineering), Business, Computer science                                                      |

Read on to see how we can shorten these two function calls.

## Authors

Similarly to work, we can use identifier to pass in authors’ OpenAlex
ID.

**Example**: We want more information on authors with IDs **A923435168**
and **A2208157607**.

``` r
oa_fetch(
  identifier = c("A923435168", "A2208157607"),
  verbose = TRUE
) %>%
  show_authors() %>%
  knitr::kable()
#> [1] "https://api.openalex.org/authors?filter=openalex_id%3AA923435168%7CA2208157607"
#> Requesting url: https://api.openalex.org/authors?filter=openalex_id%3AA923435168%7CA2208157607
#> About to get a total of 1 pages of results with a total of 2 records.
```

| short_id    | name         | orcid               | works_count |   TC | affiliation_name                 | top_concepts                                                                            |
|:------------|:-------------|:--------------------|------------:|-----:|:---------------------------------|:----------------------------------------------------------------------------------------|
| A923435168  | Massimo Aria | 0000-0002-8517-9411 |         102 | 2887 | University of Naples Federico II | Biology, Medicine, Computer science, Mathematics, Psychology, Statistics                |
| A2208157607 | Jason Priem  | 0000-0001-6187-6610 |          49 | 2184 | Our Research                     | Computer science, World Wide Web, Political science, Law, Library science, Data science |

**Example**: We want download all authors’ records of scholars who work
at the University of Naples Federico II (OpenAlex ID: I71267560) and who
have published more than 499 works.

Let’s first check how many records match the query, then set
`count_only = FALSE` to download the entire collection. We can do this
by first defining a list of arguments, then adding `count_only` (default
`FALSE`) to this list:

``` r
my_arguments <- list(
  entity = "authors",
  last_known_institution.id = "I71267560",
  works_count = ">499"
  )

do.call(oa_fetch, c(my_arguments, list(count_only = TRUE)))
#>               count db_response_time_ms                page            per_page 
#>                  22                  22                   1                   1
do.call(oa_fetch, my_arguments) %>% 
  show_authors() %>%
  knitr::kable()
```

| short_id    | name                     | orcid               | works_count |    TC | affiliation_name                 | top_concepts                                                                                      |
|:------------|:-------------------------|:--------------------|------------:|------:|:---------------------------------|:--------------------------------------------------------------------------------------------------|
| A2600338221 | Alberto Orso Maria Iorio | 0000-0002-3798-1135 |        1144 | 49621 | University of Naples Federico II | Physics, Nuclear physics, Particle physics, Quantum mechanics, Large Hadron Collider, Mathematics |
| A2011452631 | Leonardo Merola          | NA                  |        1115 | 35840 | University of Naples Federico II | Physics, Nuclear physics, Particle physics, Quantum mechanics, Large Hadron Collider, Biology     |
| A3113327292 | Vincenzo Canale          | NA                  |         990 | 30781 | University of Naples Federico II | Physics, Quantum mechanics, Particle physics, Nuclear physics, Large Hadron Collider, Geology     |
| A2062713547 | G. De Nardo              | NA                  |         959 | 20140 | University of Naples Federico II | Physics, Nuclear physics, Particle physics, Quantum mechanics, Hadron, Atomic physics             |
| A223517670  | Ettore Novellino         | 0000-0002-2181-2142 |         933 | 23082 | University of Naples Federico II | Chemistry, Biology, Biochemistry, Genetics, Medicine, Organic chemistry                           |
| A2159261619 | Annamaria Colao          | 0000-0001-6986-266X |         927 | 40708 | University of Naples Federico II | Medicine, Biology, Internal medicine, Endocrinology, Pathology, Chemistry                         |

You can also filter by other
[filters](https://docs.openalex.org/api/get-lists-of-entities/filter-entity-lists#authors-filters)
such as `display_name`, `has_orcid`, and `orcid`:

``` r
oa_fetch(
  entity = "authors",
  display_name = "Massimo Aria",
  has_orcid = "true"
) %>%
  show_authors() %>%
  knitr::kable()
```

| short_id    | name         | orcid               | works_count |   TC | affiliation_name                 | top_concepts                                                                   |
|:------------|:-------------|:--------------------|------------:|-----:|:---------------------------------|:-------------------------------------------------------------------------------|
| A923435168  | Massimo Aria | 0000-0002-8517-9411 |         102 | 2887 | University of Naples Federico II | Biology, Medicine, Computer science, Mathematics, Psychology, Statistics       |
| A2902074455 | Massimo Aria | 0000-0002-8517-9411 |          29 |   57 | University of Naples Federico II | Mathematics, Computer science, Biology, Economics, Political science, Medicine |

``` r
oa_fetch(
  entity = "authors",
  orcid = "0000-0002-8517-9411"
) %>%
  show_authors() %>%
  knitr::kable()
```

| short_id    | name         | orcid               | works_count |   TC | affiliation_name                 | top_concepts                                                                   |
|:------------|:-------------|:--------------------|------------:|-----:|:---------------------------------|:-------------------------------------------------------------------------------|
| A923435168  | Massimo Aria | 0000-0002-8517-9411 |         102 | 2887 | University of Naples Federico II | Biology, Medicine, Computer science, Mathematics, Psychology, Statistics       |
| A2902074455 | Massimo Aria | 0000-0002-8517-9411 |          29 |   57 | University of Naples Federico II | Mathematics, Computer science, Biology, Economics, Political science, Medicine |

## Institutions

**Example**: We want download all records regarding Italian institutions
(country_code:it) that are classified as educational (type:education).
Again, we check how many records match the query then download the
collection:

``` r
italian_insts <- list(
  entity = "institutions",
  country_code = "it",
  type = "education",
  verbose = TRUE
)

do.call(oa_fetch, c(italian_insts, list(count_only = TRUE)))
#> [1] "https://api.openalex.org/institutions?filter=country_code%3Ait%2Ctype%3Aeducation"
#> Requesting url: https://api.openalex.org/institutions?filter=country_code%3Ait%2Ctype%3Aeducation
#>               count db_response_time_ms                page            per_page 
#>                 231                   1                   1                   1
dplyr::glimpse(do.call(oa_fetch, italian_insts))
#> [1] "https://api.openalex.org/institutions?filter=country_code%3Ait%2Ctype%3Aeducation"
#> Requesting url: https://api.openalex.org/institutions?filter=country_code%3Ait%2Ctype%3Aeducation
#> About to get a total of 2 pages of results with a total of 231 records.
#> Rows: 231
#> Columns: 19
#> $ id                 <chr> "https://openalex.org/I861853513", "https://openale…
#> $ name               <chr> "Sapienza University of Rome", "University of Bolog…
#> $ name_alternatives  <list> "Università degli Studi di Roma \"La Sapienza\"", …
#> $ name_acronyms      <list> NA, "UNIBO", "UNIPD", "UNIMI", NA, NA, "UNITO", "U…
#> $ name_international <list> [<data.frame[1 x 85]>], [<data.frame[1 x 103]>], […
#> $ ror                <chr> "https://ror.org/02be6w209", "https://ror.org/01111…
#> $ ids                <list> [<tbl_df[6 x 2]>], [<tbl_df[6 x 2]>], [<tbl_df[6 x…
#> $ country            <chr> "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT…
#> $ geo                <list> [<data.frame[1 x 7]>], [<data.frame[1 x 7]>], [<da…
#> $ type               <chr> "education", "education", "education", "education",…
#> $ homepage           <chr> "http://www.uniroma1.it/", "http://www.unibo.it/en/…
#> $ image              <chr> "https://upload.wikimedia.org/wikipedia/en/4/45/Sap…
#> $ thumbnail          <chr> "https://upload.wikimedia.org/wikipedia/en/thumb/4/…
#> $ associated_inst    <list> [<data.frame[1 x 24]>], [<data.frame[1 x 12]>], [<…
#> $ works_count        <int> 163734, 130504, 129044, 127634, 91431, 86804, 85397…
#> $ TC                 <int> 10604126, 9662346, 9535357, 9249788, 5999184, 56226…
#> $ TCperYear          <list> [<data.frame[11 x 3]>], [<data.frame[11 x 3]>], [<…
#> $ concept            <list> [<data.frame[14 x 5]>], [<data.frame[15 x 5]>], [<…
#> $ works_api_url      <chr> "https://api.openalex.org/works?filter=institutions…
```

## Venues (think journals)

**Example**: We want download all records regarding journals that have
published more than 100,000 works:

``` r
big_journals <- list(
  entity = "venues",
  works_count = ">100000",
  verbose = TRUE
)

do.call(oa_fetch, c(big_journals, list(count_only = TRUE)))
#> [1] "https://api.openalex.org/venues?filter=works_count%3A%3E100000"
#> Requesting url: https://api.openalex.org/venues?filter=works_count%3A%3E100000
#>               count db_response_time_ms                page            per_page 
#>                  52                   2                   1                   1
dplyr::glimpse(do.call(oa_fetch, big_journals))
#> [1] "https://api.openalex.org/venues?filter=works_count%3A%3E100000"
#> Requesting url: https://api.openalex.org/venues?filter=works_count%3A%3E100000
#> About to get a total of 1 pages of results with a total of 52 records.
#> Rows: 52
#> Columns: 14
#> $ id            <chr> "https://openalex.org/V2751751161", "https://openalex.or…
#> $ name          <chr> "Social Science Research Network", "Research Papers in E…
#> $ publisher     <chr> NA, NA, "Wiley", NA, "Elsevier", "BMJ", "Springer Nature…
#> $ issn          <list> NA, NA, <"1431-5890", "0931-7597", "1522-2667">, <"1611…
#> $ issn_l        <list> NA, NA, "0931-7597", "0302-9743", "0140-6736", "0959-81…
#> $ is_oa         <lgl> NA, NA, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,…
#> $ is_in_doaj    <lgl> NA, NA, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,…
#> $ ids           <list> [<data.frame[2 x 2]>], [<data.frame[2 x 2]>], [<data.fr…
#> $ homepage      <chr> "http://www.ssrn.com/", "http://www.repec.org/", NA, "ht…
#> $ works_count   <int> 761305, 753036, 740934, 514023, 467744, 448839, 434063, …
#> $ TC            <int> 2664226, 2360376, 233707, 5413674, 7062712, 3423228, 191…
#> $ TCperYear     <list> [<data.frame[11 x 3]>], [<data.frame[11 x 3]>], [<data.…
#> $ concept       <list> [<data.frame[20 x 5]>], [<data.frame[21 x 5]>], [<data.…
#> $ works_api_url <chr> "https://api.openalex.org/works?filter=host_venue.id:V27…
```

## Concepts (think theme, keywords)

**Example**: We want to download the records of all the concepts that
concern at least one million works:

``` r
popular_concepts <- list(
  entity = "concepts",
  works_count = ">1000000",
  verbose = TRUE
)

do.call(oa_fetch, c(popular_concepts, list(count_only = TRUE)))
#> [1] "https://api.openalex.org/concepts?filter=works_count%3A%3E1000000"
#> Requesting url: https://api.openalex.org/concepts?filter=works_count%3A%3E1000000
#>               count db_response_time_ms                page            per_page 
#>                 146                   3                   1                   1
dplyr::glimpse(do.call(oa_fetch, popular_concepts))
#> [1] "https://api.openalex.org/concepts?filter=works_count%3A%3E1000000"
#> Requesting url: https://api.openalex.org/concepts?filter=works_count%3A%3E1000000
#> About to get a total of 1 pages of results with a total of 146 records.
#> Rows: 146
#> Columns: 16
#> $ id                        <chr> "https://openalex.org/C41008148", "https://o…
#> $ name                      <chr> "Computer science", "Medicine", "Chemistry",…
#> $ name_international        <list> [<data.frame[1 x 185]>], [<data.frame[1 x 1…
#> $ description               <chr> "theoretical study of the formal foundation …
#> $ description_international <list> [<data.frame[1 x 40]>], [<data.frame[1 x 44…
#> $ wikidata                  <chr> "https://www.wikidata.org/wiki/Q21198", "htt…
#> $ level                     <int> 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0,…
#> $ ids                       <list> [<data.frame[5 x 2]>], [<data.frame[5 x 2]>…
#> $ image                     <chr> "https://upload.wikimedia.org/wikipedia/comm…
#> $ thumbnail                 <chr> "https://upload.wikimedia.org/wikipedia/comm…
#> $ ancestors                 <list> NA, NA, NA, NA, NA, NA, NA, [<data.frame[2 …
#> $ rel_concepts              <list> [<data.frame[93 x 5]>], [<data.frame[51 x 5…
#> $ works_count               <int> 40513549, 36892224, 20875029, 17567558, 1705…
#> $ TC                        <int> 214363426, 366806828, 324514640, 158829615, …
#> $ TCperYear                 <list> [<data.frame[11 x 3]>], [<data.frame[11 x 3…
#> $ works_api_url             <chr> "https://api.openalex.org/works?filter=conce…
```

## Other examples

*Get all works citing a particular work*

We can download all publications citing another publication by using the
filter attribute **cites**.

For example, if we want to download all publications citing the article
Aria and Cuccurullo (2017), we have just to set the argument filter as
`cites = "W2755950973"` where “W2755950973” is the OA id for the article
by Aria and Cuccurullo.

``` r
aria_count <- oa_fetch(
  entity = "works",
  cites = "W2755950973",
  count_only = TRUE,
  verbose = TRUE
) 
#> [1] "https://api.openalex.org/works?filter=cites%3AW2755950973"
#> Requesting url: https://api.openalex.org/works?filter=cites%3AW2755950973
aria_count
#>               count db_response_time_ms                page            per_page 
#>                1476                  37                   1                   1
```

This query will return a collection of 1476 publications. Let’s to
download it and then to convert it into a data frame:

``` r
oa_fetch(
  entity = "works",
  cites = "W2755950973",
  count_only = TRUE,
  verbose = TRUE
) %>% 
  dplyr::glimpse()
#> [1] "https://api.openalex.org/works?filter=cites%3AW2755950973"
#> Requesting url: https://api.openalex.org/works?filter=cites%3AW2755950973
#>  Named int [1:4] 1476 25 1 1
#>  - attr(*, "names")= chr [1:4] "count" "db_response_time_ms" "page" "per_page"
```

## Convert an OpenAlex data frame to a bibliometrix object

The bibliometrix R-package (<https://www.bibliometrix.org>) provides a
set of tools for quantitative research in bibliometrics and
scientometrics. Today it represents one of the most used science mapping
software in the world. In a recent survey on bibliometric analysis
tools, Moral-Muñoz et al. (2020) wrote: “At this moment, maybe
Bibliometrix and its Shiny platform contain the more extensive set of
techniques implemented, and together with the easiness of its interface,
could be a great software for practitioners”.

The function **oa2bibliometrix** converts a bibliographic data frame of
works into a bibliometrix object. This object can be used as input
collection of a science mapping workflow.

``` r
bib_ls <- list(
  identifier = NULL,
  entity = "works",
  cites = "W2755950973",
  from_publication_date = "2022-01-01",
  to_publication_date = "2022-03-31"
)

do.call(oa_fetch, c(bib_ls, list(count_only = TRUE)))
#>               count db_response_time_ms                page            per_page 
#>                 215                   8                   1                   1
do.call(oa_fetch, bib_ls) %>% 
  oa2bibliometrix() %>% 
  dplyr::glimpse()
#> Rows: 215
#> Columns: 37
#> $ AU            <chr> "CHENG WANG;TAO LV;RONGJIANG CAI;JIANFENG XU;LIYA WANG",…
#> $ RP            <chr> "SCHOOL OF ECONOMICS AND MANAGEMENT, CHINA UNIVERSITY OF…
#> $ C1            <chr> "SCHOOL OF ECONOMICS AND MANAGEMENT, CHINA UNIVERSITY OF…
#> $ AU_UN         <chr> "CHINA UNIVERSITY OF MINING AND TECHNOLOGY;CHINA UNIVERS…
#> $ AU_CO         <chr> "CHINA;CHINA;CHINA;CHINA;NA", "ITALY;ITALY;ITALY", "SOUT…
#> $ ID            <chr> "SCOPUS;BIBLIOMETRICS;SUSTAINABILITY;HIERARCHY;AGENCY (P…
#> $ id_url        <chr> "https://openalex.org/W4220662981", "https://openalex.or…
#> $ TI            <chr> "BIBLIOMETRIC ANALYSIS OF MULTI-LEVEL PERSPECTIVE ON SUS…
#> $ author        <list> [<data.frame[5 x 10]>], [<data.frame[3 x 10]>], [<data.…
#> $ AB            <chr> "THE MULTI-LEVEL PERSPECTIVE (MLP) IS A PROMINENT FRAMEW…
#> $ pubdata       <chr> "2022-03-31", "2022-03-31", "2022-03-31", "2022-03-29", …
#> $ SO            <chr> "SUSTAINABILITY", "EUROPEAN JOURNAL OF INNOVATION MANAGE…
#> $ SO_ID         <chr> "https://openalex.org/V10134376", "https://openalex.org/…
#> $ PU            <chr> "MDPI AG", "Emerald (MCB UP)", "MDPI AG", "Emerald (MCB …
#> $ IS            <list> "2071-1050", <"1460-1060", "1758-7115">, <"1661-7827", …
#> $ URL           <chr> "https://doi.org/10.3390/su14074145", "https://doi.org/1…
#> $ first_page    <chr> "4145", NA, "4165", NA, NA, "1073", "1082", NA, NA, NA, …
#> $ last_page     <chr> "4145", NA, "4165", NA, NA, "1073", "1082", NA, NA, NA, …
#> $ volume        <chr> "14", NA, "19", NA, "9", "14", "14", NA, NA, NA, "14", "…
#> $ issue         <chr> "7", NA, "7", NA, NA, "7", "7", NA, NA, NA, "7", "1", "4…
#> $ OA            <lgl> TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE…
#> $ TC            <int> 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ TCperYear     <list> [<data.frame[1 x 2]>], NA, NA, [<data.frame[1 x 2]>], N…
#> $ PY            <int> 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 20…
#> $ cited_by_url  <chr> "https://api.openalex.org/works?filter=cites:W4220662981…
#> $ ids           <list> [[<data.frame[2 x 2]>]], [[<data.frame[2 x 2]>]], [[<da…
#> $ DI            <chr> "https://doi.org/10.3390/su14074145", "https://doi.org/1…
#> $ DT            <chr> "JOURNAL-ARTICLE", "JOURNAL-ARTICLE", "JOURNAL-ARTICLE",…
#> $ CR            <chr> "https://openalex.org/W946529684;https://openalex.org/W1…
#> $ related_works <list> <"https://openalex.org/W1222457", "https://openalex.org…
#> $ concept       <list> [<data.frame[12 x 5]>], [<data.frame[6 x 5]>], [<data.f…
#> $ id_oa         <chr> "W4220662981", "W4220731660", "W4220971062", "W422071230…
#> $ DB            <chr> "openalex", "openalex", "openalex", "openalex", "openale…
#> $ JI            <chr> "V10134376", "V90149737", "V15239247", "V20489460", "V25…
#> $ J9            <chr> "V10134376", "V90149737", "V15239247", "V20489460", "V25…
#> $ SR_FULL       <chr> "CHENG WANG, 2022, V10134376", "GIANLUCA ELIA, 2022, V90…
#> $ SR            <chr> "CHENG WANG, 2022, V10134376", "GIANLUCA ELIA, 2022, V90…
```

# About OpenAlex

![Image from
<https://docs.openalex.org/>](https://i.imgur.com/FXTji65.png)

[OpenAlex](https://openalex.org) is a fully open catalog of the global
research system. It’s named after the ancient [Library of
Alexandria](https://en.wikipedia.org/wiki/Library_of_Alexandria). The
OpenAlex dataset describes scholarly entities and how those entities are
connected to each other. There are five types of entities:

-   **Works** are papers, books, datasets, etc; they cite other works

-   **Authors** are people who create works

-   **Venues** are journals and repositories that host works

-   **Institutions** are universities and other orgs that are affiliated
    with works (via authors)

-   **Concepts** *tag* Works with a topic

## Acknowledgements

Package hex made with [Midjourney](https://www.midjourney.com/home/)
which inherits a [CC BY-NC 4.0
license](https://creativecommons.org/licenses/by-nc/4.0/legalcode).
