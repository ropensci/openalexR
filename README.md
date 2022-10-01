<!-- README.md is generated from README.Rmd. Please edit that file -->

# openalexR <img src="man/figures/logo.png" align="right" height="139"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/massimoaria/openalexR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/massimoaria/openalexR/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

**openalexR** helps you interface with the [OpenAlex](https://openalex.org) API to retrieve bibliographic infomation about publications, authors, venues, institutions and concepts with 4 main functions:

-   `oa_query()`: generates a valid query, written following the OpenAlex API syntax, from a set of arguments provided by the user.

-   `oa_request()`: downloads a collection of entities matching the query created by `oa_query()` or manually written by the user, and returns a JSON object in a list format.

-   `oa2df()`: converts the JSON object in classical bibliographic tibble/data frame.

-   `oa_fetch()`: composes three functions above so the user can execute everything in one step, *i.e.*, `oa_query |> oa_request |> oa2df`

-   `oa_random()`: to get random entity, e.g., `oa_random("works")` gives a different work each time you run it

## Installation

You can install the developer version of the openalexR from [GitHub](https://github.com) with:

``` r
install.packages("remotes")
remotes::install_github("massimoaria/openalexR")
```

You can install the released version of openalexR from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("openalexR")
```

Before we go any further, we highly recommend you set `openalexR.mailto` option so that your requests go to [the polite pool](https://docs.openalex.org/api#the-polite-pool) for faster response times:

``` r
options(openalexR.mailto = "example@email.com")
```

Bonus point if you put this in your `.Rprofile` with `usethis::edit_r_profile()`.

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

is associated to the [OpenAlex-id](https://docs.openalex.org/about-the-data#the-openalex-id) **W2755950973**. If you know your paper's OpenAlex ID, all you need to do is passing `identifier = <openalex id>` as an argument in `oa_fetch()`:

``` r
paper_id <- oa_fetch(
  identifier = "W2755950973",
  entity = "works",
  verbose = TRUE
)
#> [1] "https://api.openalex.org/works/W2755950973"
#> Requesting url: https://api.openalex.org/works/W2755950973
dplyr::glimpse(paper_id)
#> Rows: 1
#> Columns: 26
#> $ id               <chr> "https://openalex.org/W2755950973"
#> $ display_name     <chr> "bibliometrix : An R-tool for comprehensive science m…
#> $ author           <list> [<data.frame[2 x 10]>]
#> $ ab               <chr> "Abstract The use of bibliometrics is gradually exten…
#> $ publication_date <chr> "2017-11-01"
#> $ relevance_score  <lgl> NA
#> $ so               <chr> "Journal of Informetrics"
#> $ so_id            <chr> "https://openalex.org/V205292342"
#> $ publisher        <chr> "Elsevier"
#> $ issn             <list> <"1875-5879", "1751-1577">
#> $ url              <chr> "https://doi.org/10.1016/j.joi.2017.08.007"
#> $ first_page       <chr> "959"
#> $ last_page        <chr> "975"
#> $ volume           <chr> "11"
#> $ issue            <chr> "4"
#> $ is_oa            <lgl> FALSE
#> $ cited_by_count   <int> 1516
#> $ counts_by_year   <list> [<data.frame[6 x 2]>]
#> $ publication_year <int> 2017
#> $ cited_by_api_url <chr> "https://api.openalex.org/works?filter=cites:W275595…
#> $ ids              <list> [<tbl_df[3 x 2]>]
#> $ doi              <chr> "https://doi.org/10.1016/j.joi.2017.08.007"
#> $ type             <chr> "journal-article"
#> $ referenced_works <list> <"https://openalex.org/W767067438", "https://openalex…
#> $ related_works    <list> <"https://openalex.org/W2086064646", "https://openale…
#> $ concepts         <list> [<data.frame[3 x 5]>]
```

`oa_fetch()` is a composition of functions: `oa_query |> oa_request |> oa2df`. As results, `oa_query()` returns the query string including the OpenAlex endpoint API server address (default). `oa_request()` downloads the bibliographic records matching the query. Finally, `oa2df()` converts the final result list to a tibble. The final result is a complicated tibble, but we can use `show_works()` to display a simplified version:

``` r
paper_id %>% 
  show_works() %>%
  knitr::kable()
```

| short_id    | display_name                                                        | first_author | last_author        | so                      | url                                         | is_oa | top_concepts                                          |
|:--------|:--------|:--------|:--------|:--------|:--------|:--------|:--------|
| W2755950973 | bibliometrix : An R-tool for comprehensive science mapping analysis | Massimo Aria | Corrado Cuccurullo | Journal of Informetrics | <https://doi.org/10.1016/j.joi.2017.08.007> | FALSE | Computer science, Data science, Information retrieval |

### External id formats

OpenAlex endpoint accepts OpenAlex IDs and other [external IDs](https://docs.openalex.org/api/get-single-entities#id-formats) (*e.g.*, DOI, ISSN) in several formats, including Digital Object Identifier (DOI) and Persistent Identifiers (PIDs).

``` r
oa_fetch(
  # identifier = "https://doi.org/10.1016/j.joi.2017.08.007", # would also work (PIDs)
  identifier = "doi:10.1016/j.joi.2017.08.007",
  entity = "works"
) %>% 
  show_works() %>%
  knitr::kable()
```

| short_id    | display_name                                                        | first_author | last_author        | so                      | url                                         | is_oa | top_concepts                                          |
|:--------|:--------|:--------|:--------|:--------|:--------|:--------|:--------|
| W2755950973 | bibliometrix : An R-tool for comprehensive science mapping analysis | Massimo Aria | Corrado Cuccurullo | Journal of Informetrics | <https://doi.org/10.1016/j.joi.2017.08.007> | FALSE | Computer science, Data science, Information retrieval |

### More than one publications/authors

[https://api.openalex.org/authors/https://orcid.org/](https://api.openalex.org/authors/https://orcid.org/0000-0003-1613-5981)

If you know the OpenAlex IDs of these entities, you can also feed them into the `identifier` argument.

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

| short_id    | display_name                                                                                 | first_author       | last_author        | so                      | url                                         | is_oa | top_concepts                                                                            |
|:--------|:--------|:--------|:--------|:--------|:--------|:--------|:--------|
| W2755950973 | bibliometrix : An R-tool for comprehensive science mapping analysis                          | Massimo Aria       | Corrado Cuccurullo | Journal of Informetrics | <https://doi.org/10.1016/j.joi.2017.08.007> | FALSE | Computer science, Data science, Information retrieval                                   |
| W2741809807 | The state of OA: a large-scale analysis of the prevalence and impact of Open Access articles | Heather A. Piwowar | Stefanie Haustein  | PeerJ                   | <https://doi.org/10.7717/peerj.4375>        | TRUE  | Citation, License, Scholarly communication, Web of science, Bibliometrics, Open science |

However, if you only know their external identifies, say, DOIs, you would need to use `doi` as a filter (either the canonical form with <https://www.doi.org/> or without should work):

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

| short_id    | display_name                                                                           | first_author     | last_author        | so                      | url                                              | is_oa | top_concepts                                                                                                |
|:--------|:--------|:--------|:--------|:--------|:--------|:--------|:--------|
| W2755950973 | bibliometrix : An R-tool for comprehensive science mapping analysis                    | Massimo Aria     | Corrado Cuccurullo | Journal of Informetrics | <https://doi.org/10.1016/j.joi.2017.08.007>      | FALSE | Computer science, Data science, Information retrieval                                                       |
| W3206431085 | PMLB v1.0: an open-source dataset collection for benchmarking machine learning methods | Joseph D. Romano | Jason H. Moore     | Bioinformatics          | <https://doi.org/10.1093/bioinformatics/btab727> | TRUE  | Benchmarking, Python (programming language), Computer science, Benchmark (surveying), Open source, Workflow |

### Filters

In most cases, we are interested in downloading a collection of items that meet one or more inclusion/exclusion criteria (filters). Supported attributes for each endpoints are listed [here](https://docs.openalex.org/api/get-lists-of-entities/filter-entity-lists).

**Example**: We want to download all works published by a set of authors. We can do this by filtering on the authorships.author.id/author.id or authorships.author.orcid/author.orcid attribute (see more on attributes [here](https://docs.openalex.org/api/get-lists-of-entities/filter-entity-lists#works-attribute-filters)):

``` r
oa_fetch(
  entity = "works",
  author.id = c("A923435168", "A2208157607"),
  verbose = TRUE
) %>% 
  show_works() %>% 
  knitr::kable()
#> [1] "https://api.openalex.org/works?filter=author.id%3AA923435168%7CA2208157607"
#> Requesting url: https://api.openalex.org/works?filter=author.id%3AA923435168%7CA2208157607
#> About to get a total of 1 pages of results with a total of 151 records.
```

| short_id    | display_name                                                                                 | first_author       | last_author          | so                                                                    | url                                                                                | is_oa | top_concepts                                                                             |
|:--------|:--------|:--------|:--------|:--------|:--------|:--------|:--------|
| W2755950973 | bibliometrix : An R-tool for comprehensive science mapping analysis                          | Massimo Aria       | Corrado Cuccurullo   | Journal of Informetrics                                               | <https://doi.org/10.1016/j.joi.2017.08.007>                                        | FALSE | Computer science, Data science, Information retrieval                                    |
| W2741809807 | The state of OA: a large-scale analysis of the prevalence and impact of Open Access articles | Heather A. Piwowar | Stefanie Haustein    | PeerJ                                                                 | <https://doi.org/10.7717/peerj.4375>                                               | TRUE  | Citation, License, Scholarly communication, Web of science, Bibliometrics, Open science  |
| W2122130843 | Scientometrics 2.0: New metrics of scholarly impact on the social Web                        | Jason Priem        | Bradely H. Hemminger | First Monday                                                          | <https://doi.org/10.5210/fm.v15i7.2874>                                            | FALSE | Bookmarking, Microblogging, Altmetrics, Social media, Computer science, World Wide Web   |
| W2041540760 | How and why scholars cite on Twitter                                                         | Jason Priem        | Kaitlin L. Costello  | Proceedings Of The Association For Information Science And Technology | <https://doi.org/10.1002/meet.14504701201>                                         | TRUE  | Microblogging, Conversation, Social media, Citation, Scholarly communication, Altmetrics |
| W1553564559 | Altmetrics in the wild: Using social media to explore scholarly impact                       | Jason Priem        | Bradley M. Hemminger | arXiv: Digital Libraries                                              | <https://arxiv.org/abs/1203.4745>                                                  | FALSE | Altmetrics, Social media, Citation, Computer science, Data science, Diversity (politics) |
| W3130540911 | altmetrics: a manifesto                                                                      | Jason Priem        | Cameron Neylon       | NA                                                                    | <https://digitalcommons.unl.edu/cgi/viewcontent.cgi?article=1187&context=scholcom> | FALSE | Altmetrics, Manifesto                                                                    |

``` r

orcids <- c("0000-0003-3737-6565", "0000-0002-8517-9411")
canonical_orcids <- paste0("https://orcid.org/", orcids)
oa_fetch(
  entity = "works",
  author.orcid = canonical_orcids,
  verbose = TRUE
) %>% 
  show_works() %>% 
  knitr::kable()
#> [1] "https://api.openalex.org/works?filter=author.orcid%3Ahttps%3A%2F%2Forcid.org%2F0000-0003-3737-6565%7Chttps%3A%2F%2Forcid.org%2F0000-0002-8517-9411"
#> Requesting url: https://api.openalex.org/works?filter=author.orcid%3Ahttps%3A%2F%2Forcid.org%2F0000-0003-3737-6565%7Chttps%3A%2F%2Forcid.org%2F0000-0002-8517-9411
#> About to get a total of 2 pages of results with a total of 211 records.
```

| short_id    | display_name                                                                                                                              | first_author          | last_author         | so                              | url                                             | is_oa | top_concepts                                                                                                  |
|:--------|:--------|:--------|:--------|:--------|:--------|:--------|:--------|
| W2755950973 | bibliometrix : An R-tool for comprehensive science mapping analysis                                                                       | Massimo Aria          | Corrado Cuccurullo  | Journal of Informetrics         | <https://doi.org/10.1016/j.joi.2017.08.007>     | FALSE | Computer science, Data science, Information retrieval                                                         |
| W1979874437 | Analysis of powered two-wheeler crashes in Italy by classification trees and rules discovery                                              | Alfonso Montella      | Filomena Mauriello  | Accident Analysis & Prevention  | <https://doi.org/10.1016/j.aap.2011.04.025>     | FALSE | Crash, Computer science, Data mining, Identification (biology), Decision tree, Probabilistic logic            |
| W2955219525 | Scaling tree-based automated machine learning to biomedical big data with a feature set selector                                          | Trang T. Le           | Jason H. Moore      | Bioinformatics                  | <https://doi.org/10.1093/bioinformatics/btz470> | TRUE  | Pipeline (software), Computer science, Scalability, Feature (linguistics), Set (abstract data type), Big data |
| W2952824318 | A Nonlinear Simulation Framework Supports Adjusting for Age When Analyzing BrainAGE                                                       | Trang T. Le           | Tulsa Investigators | Frontiers in Aging Neuroscience | <https://doi.org/10.3389/fnagi.2018.00317>      | TRUE  | Correlation, Mood, Set (abstract data type), Contrast (vision), Anxiety, Psychology                           |
| W2408216567 | Foundations and trends in performance management. A twenty-five years bibliometric analysis in business and public administration domains | Corrado Cuccurullo    | Fabrizia Sarto      | Scientometrics                  | <https://doi.org/10.1007/s11192-016-1948-8>     | FALSE | Administration (probate law), Bibliometrics, Regional science, Trend analysis, Political science, Sociology   |
| W2281330131 | Coopetition and sustainable competitive advantage. The case of tourist destinations                                                       | Valentina Della Corte | Massimo Aria        | Tourism Management              | <https://doi.org/10.1016/j.tourman.2015.12.009> | FALSE | Coopetition, Competitive advantage, Business, Destinations, Tourist destinations, Tourism                     |

**Example**: We want to download all works that have been cited more than 50 times, published between 2020 and 2021, and include the strings "bibliometric analysis" or "science mapping" in the title. Maybe we also want the results to be sorted by total citations in a descending order.

Setting the argument `count_only = TRUE`, the function `oa_request()` returns the number of items matching the query without downloading the collection.

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
#>                  27                  20                   1                 200
```

We can now download the records and transform it into a tibble/data frame by setting `count_only = FALSE` (also the default value):

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

| short_id    | display_name                                                                                                                   | first_author        | last_author        | so                                        | url                                             | is_oa | top_concepts                                                                                                      |
|:--------|:--------|:--------|:--------|:--------|:--------|:--------|:--------|
| W3160856016 | How to conduct a bibliometric analysis: An overview and guidelines                                                             | Naveen Donthu       | Weng Marc Lim      | Journal of Business Research              | <https://doi.org/10.1016/j.jbusres.2021.04.070> | TRUE  | Bibliometrics, Field (mathematics), Data science, Computer science, Resource (disambiguation), Management science |
| W3038273726 | Investigating the emerging COVID-19 research trends in the field of business and management: A bibliometric analysis approach. | Surabhi Verma       | Anders Gustafsson  | Journal of Business Research              | <https://pubmed.ncbi.nlm.nih.gov/32834211/>     | NA    | Scopus, Coronavirus disease 2019 (COVID-19), Pandemic, Bibliometrics, Web of science, Field (mathematics)         |
| W2990450011 | Forty-five years of Journal of Business Research: A bibliometric analysis                                                      | Naveen Donthu       | Debidutta Pattnaik | Journal of Business Research              | <https://doi.org/10.1016/j.jbusres.2019.10.039> | FALSE | Bibliometrics, Regional science                                                                                   |
| W3001491100 | Software tools for conducting bibliometric analysis in science: An up-to-date review                                           | Jose A. Moral-Munoz | Manuel Cobo        | Profesional De La Informacion             | <https://doi.org/10.3145/epi.2020.ene.03>       | TRUE  | Bibliometrics, Software, Computer science, Data science, Library science, Software engineering                    |
| W3011866596 | A Bibliometric Analysis of COVID-19 Research Activity: A Call for Increased Output                                             | Mohamad A. Chahrour | Hussein H. Khachfe | Cureus                                    | <https://doi.org/10.7759/cureus.7357>           | TRUE  | Pandemic, Medicine, Observational study, Gross domestic product, Coronavirus disease 2019 (COVID-19), Population  |
| W3044902155 | Financial literacy: A systematic review and bibliometric analysis                                                              | Kirti Goyal         | Satish Kumar       | International Journal of Consumer Studies | <https://doi.org/10.1111/ijcs.12605>            | FALSE | Financial literacy, Content analysis, Citation, Citation analysis, Bibliometrics, Literacy                        |

Read on to see how we can shorten these two function calls.

## Authors

Similarly to work, we can use identifier to pass in authors' OpenAlex ID.

**Example**: We want more information on authors with IDs **A923435168** and **A2208157607**.

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

| short_id    | display_name | orcid               | works_count | cited_by_count | affiliation_display_name         | top_concepts                                                                            |
|:---------|:---------|:---------|---------:|---------:|:---------|:-----------|
| A923435168  | Massimo Aria | 0000-0002-8517-9411 |         102 |           2952 | University of Naples Federico II | Biology, Medicine, Computer science, Mathematics, Psychology, Statistics                |
| A2208157607 | Jason Priem  | 0000-0001-6187-6610 |          49 |           2142 | HortResearch                     | Computer science, World Wide Web, Political science, Law, Library science, Data science |

**Example**: We want download all authors' records of scholars who work at the University of Naples Federico II (OpenAlex ID: I71267560) and who have published more than 499 works.

Let's first check how many records match the query, then set `count_only = FALSE` to download the entire collection. We can do this by first defining a list of arguments, then adding `count_only` (default `FALSE`) to this list:

``` r
my_arguments <- list(
  entity = "authors",
  last_known_institution.id = "I71267560",
  works_count = ">499"
  )

do.call(oa_fetch, c(my_arguments, list(count_only = TRUE)))
#>               count db_response_time_ms                page            per_page 
#>                  22                   7                   1                 200
do.call(oa_fetch, my_arguments) %>% 
  show_authors() %>%
  knitr::kable()
```

| short_id    | display_name             | orcid               | works_count | cited_by_count | affiliation_display_name         | top_concepts                                                                                      |
|:---------|:---------|:---------|---------:|---------:|:---------|:-----------|
| A2600338221 | Alberto Orso Maria Iorio | 0000-0002-3798-1135 |        1144 |          47933 | University of Naples Federico II | Physics, Nuclear physics, Particle physics, Quantum mechanics, Large Hadron Collider, Mathematics |
| A2011452631 | Leonardo Merola          | NA                  |        1115 |          35281 | University of Naples Federico II | Physics, Particle physics, Nuclear physics, Quantum mechanics, Large Hadron Collider, Biology     |
| A3113327292 | Vincenzo Canale          | NA                  |         990 |          29062 | University of Naples Federico II | Physics, Quantum mechanics, Particle physics, Nuclear physics, Large Hadron Collider, Geology     |
| A2062713547 | G. De Nardo              | NA                  |         959 |          20140 | University of Naples Federico II | Physics, Nuclear physics, Particle physics, Quantum mechanics, Hadron, Atomic physics             |
| A223517670  | Ettore Novellino         | 0000-0002-2181-2142 |         933 |          23003 | University of Naples Federico II | Chemistry, Biology, Biochemistry, Genetics, Medicine, Organic chemistry                           |
| A2159261619 | Annamaria Colao          | 0000-0001-6986-266X |         927 |          40286 | University of Naples Federico II | Medicine, Biology, Internal medicine, Endocrinology, Pathology, Chemistry                         |

You can also filter by other [filters](https://docs.openalex.org/api/get-lists-of-entities/filter-entity-lists#authors-filters) such as `display_name`, `has_orcid`, and `orcid`:

``` r
oa_fetch(
  entity = "authors",
  display_name = "Massimo Aria",
  has_orcid = "true"
) %>%
  show_authors() %>%
  knitr::kable()
```

| short_id    | display_name | orcid               | works_count | cited_by_count | affiliation_display_name         | top_concepts                                                                   |
|:---------|:---------|:---------|---------:|---------:|:---------|:-----------|
| A923435168  | Massimo Aria | 0000-0002-8517-9411 |         102 |           2952 | University of Naples Federico II | Biology, Medicine, Computer science, Mathematics, Psychology, Statistics       |
| A2902074455 | Massimo Aria | 0000-0002-8517-9411 |          29 |             62 | University of Naples Federico II | Mathematics, Computer science, Economics, Biology, Political science, Business |

``` r

oa_fetch(
  entity = "authors",
  orcid = "0000-0002-8517-9411"
) %>%
  show_authors() %>%
  knitr::kable()
```

| short_id    | display_name | orcid               | works_count | cited_by_count | affiliation_display_name         | top_concepts                                                                   |
|:---------|:---------|:---------|---------:|---------:|:---------|:-----------|
| A923435168  | Massimo Aria | 0000-0002-8517-9411 |         102 |           2952 | University of Naples Federico II | Biology, Medicine, Computer science, Mathematics, Psychology, Statistics       |
| A2902074455 | Massimo Aria | 0000-0002-8517-9411 |          29 |             62 | University of Naples Federico II | Mathematics, Computer science, Economics, Biology, Political science, Business |

## Institutions

**Example**: We want download all records regarding Italian institutions (country_code:it) that are classified as educational (type:education). Again, we check how many records match the query then download the collection:

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
#>                 231                  23                   1                 200
dplyr::glimpse(do.call(oa_fetch, italian_insts))
#> [1] "https://api.openalex.org/institutions?filter=country_code%3Ait%2Ctype%3Aeducation"
#> Requesting url: https://api.openalex.org/institutions?filter=country_code%3Ait%2Ctype%3Aeducation
#> About to get a total of 2 pages of results with a total of 231 records.
#> Rows: 231
#> Columns: 22
#> $ id                        <chr> "https://openalex.org/I861853513", "https://…
#> $ display_name              <chr> "Sapienza University of Rome", "University o…
#> $ display_name_alternatives <list> "Università degli Studi di Roma \"La Sapien…
#> $ display_name_acronyms     <list> NA, "UNIBO", "UNIPD", "UNIMI", NA, NA, "UNI…
#> $ international             <list> [<data.frame[1 x 85]>], [<data.frame[1 x 10…
#> $ ror                       <chr> "https://ror.org/02be6w209", "https://ror.or…
#> $ ids                       <list> [<tbl_df[6 x 2]>], [<tbl_df[6 x 2]>], [<tbl…
#> $ country_code              <chr> "IT", "IT", "IT", "IT", "IT", "IT", "IT", "I…
#> $ geo                       <list> [<data.frame[1 x 7]>], [<data.frame[1 x 7]>…
#> $ type                      <chr> "education", "education", "education", "educ…
#> $ homepage_url              <chr> "http://www.uniroma1.it/", "http://www.unibo…
#> $ image_url                 <chr> "https://upload.wikimedia.org/wikipedia/en/4…
#> $ image_thumbnail_url       <chr> "https://upload.wikimedia.org/wikipedia/en/t…
#> $ associated_institutions   <list> [<data.frame[1 x 24]>], [<data.frame[1 x 12…
#> $ relevance_score           <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ works_count               <int> 163971, 130673, 129218, 127781, 91598, 86908…
#> $ cited_by_count            <int> 10582871, 9649329, 9516585, 9171398, 5981760…
#> $ counts_by_year            <list> [<data.frame[11 x 3]>], [<data.frame[11 x 3…
#> $ works_api_url             <chr> "https://api.openalex.org/works?filter=insti…
#> $ x_concepts                <list> [<data.frame[14 x 5]>], [<data.frame[15 x 5…
#> $ updated_date              <chr> "2022-09-05T22:37:07.398707", "2022-09-02T15…
#> $ created_date              <chr> "2016-06-24", "2016-06-24", "2016-06-24", "2…
```

## Venues (think journals)

**Example**: We want download all records regarding journals that have published more than 100,000 works:

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
#>                  51                   6                   1                 200
dplyr::glimpse(do.call(oa_fetch, big_journals))
#> [1] "https://api.openalex.org/venues?filter=works_count%3A%3E100000"
#> Requesting url: https://api.openalex.org/venues?filter=works_count%3A%3E100000
#> About to get a total of 1 pages of results with a total of 51 records.
#> Rows: 51
#> Columns: 15
#> $ id              <chr> "https://openalex.org/V2751751161", "https://openalex.…
#> $ display_name    <chr> "Social Science Research Network", "Research Papers in…
#> $ publisher       <chr> NA, NA, "Wiley", NA, "Elsevier", "BMJ", "Springer Natu…
#> $ issn            <list> NA, NA, <"1431-5890", "0931-7597", "1522-2667">, <"16…
#> $ issn_l          <list> NA, NA, "0931-7597", "0302-9743", "0140-6736", "0959-…
#> $ is_oa           <lgl> NA, NA, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALS…
#> $ is_in_doaj      <lgl> NA, NA, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALS…
#> $ ids             <list> [<tbl_df[2 x 2]>], [<tbl_df[2 x 2]>], [<tbl_df[6 x 2]…
#> $ homepage_url    <chr> "http://www.ssrn.com/", "http://www.repec.org/", NA, "…
#> $ relevance_score <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ works_count     <int> 758135, 752740, 738207, 514757, 469642, 449173, 432408…
#> $ cited_by_count  <int> 2605930, 2318815, 223273, 5382214, 6715418, 3301643, 1…
#> $ counts_by_year  <list> [<data.frame[11 x 3]>], [<data.frame[11 x 3]>], [<dat…
#> $ x_concepts      <list> [<data.frame[20 x 5]>], [<data.frame[20 x 5]>], [<dat…
#> $ works_api_url   <chr> "https://api.openalex.org/works?filter=host_venue.id:V…
```

## Concepts (think theme, keywords)

**Example**: We want to download the records of all the concepts that concern at least one million works:

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
#>                 147                  40                   1                 200
dplyr::glimpse(do.call(oa_fetch, popular_concepts))
#> [1] "https://api.openalex.org/concepts?filter=works_count%3A%3E1000000"
#> Requesting url: https://api.openalex.org/concepts?filter=works_count%3A%3E1000000
#> About to get a total of 1 pages of results with a total of 147 records.
#> Rows: 147
#> Columns: 17
#> $ id                         <chr> "https://openalex.org/C41008148", "https://…
#> $ display_name               <chr> "Computer science", "Medicine", "Chemistry"…
#> $ display_name_international <list> [<data.frame[1 x 185]>], [<data.frame[1 x …
#> $ description                <chr> "theoretical study of the formal foundation…
#> $ description_international  <list> [<data.frame[1 x 40]>], [<data.frame[1 x 4…
#> $ wikidata                   <chr> "https://www.wikidata.org/wiki/Q21198", "ht…
#> $ level                      <int> 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0…
#> $ ids                        <list> [<tbl_df[5 x 2]>], [<tbl_df[5 x 2]>], [<tb…
#> $ image_url                  <chr> "https://upload.wikimedia.org/wikipedia/com…
#> $ image_thumbnail_url        <chr> "https://upload.wikimedia.org/wikipedia/com…
#> $ ancestors                  <list> NA, NA, NA, NA, NA, NA, NA, [<data.frame[2…
#> $ related_concepts           <list> [<data.frame[93 x 5]>], [<data.frame[51 x …
#> $ relevance_score            <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ works_count                <int> 40806340, 37239610, 20883170, 17657582, 171…
#> $ cited_by_count             <int> 212764188, 354063411, 316656878, 154932922,…
#> $ counts_by_year             <list> [<data.frame[11 x 3]>], [<data.frame[11 x …
#> $ works_api_url              <chr> "https://api.openalex.org/works?filter=conc…
```

## Other examples

*Get all works citing a particular work*

We can download all publications citing another publication by using the filter attribute **cites**.

For example, if we want to download all publications citing the article Aria and Cuccurullo (2017), we have just to set the argument filter as `cites = "W2755950973"` where "W2755950973" is the OA id for the article by Aria and Cuccurullo.

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
#>                1551                  52                   1                 200
```

This query will return a collection of 1551 publications. Among these articles, let's download the ones published in the following year:

``` r
oa_fetch(
  entity = "works",
  cites = "W2755950973",
  publication_year = 2018,
  count_only = FALSE,
  verbose = TRUE
) %>% 
  dplyr::glimpse()
#> [1] "https://api.openalex.org/works?filter=cites%3AW2755950973%2Cpublication_year%3A2018"
#> Requesting url: https://api.openalex.org/works?filter=cites%3AW2755950973%2Cpublication_year%3A2018
#> About to get a total of 1 pages of results with a total of 16 records.
#> Rows: 16
#> Columns: 26
#> $ id               <chr> "https://openalex.org/W2902888572", "https://openalex…
#> $ display_name     <chr> "A global bibliometric analysis of Plesiomonas-relate…
#> $ author           <list> [<data.frame[2 x 10]>], [<data.frame[7 x 10]>], [<da…
#> $ ab               <chr> "Plesiomonas shigelloides is an emerging pathogen wit…
#> $ publication_date <chr> "2018-11-29", "2018-06-25", "2018-04-01", "2018-09-27…
#> $ relevance_score  <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ so               <chr> "PLOS ONE", "PLOS ONE", "Journal of Experimental Zool…
#> $ so_id            <chr> "https://openalex.org/V202381698", "https://openalex.…
#> $ publisher        <chr> "Public Library of Science", "Public Library of Scien…
#> $ issn             <list> "1932-6203", "1932-6203", <"1932-5231", "1932-5223">…
#> $ url              <chr> "https://doi.org/10.1371/journal.pone.0207655", "http…
#> $ first_page       <chr> "e0207655", NA, "162", "10589", "3", "38", NA, "e0096…
#> $ last_page        <chr> "e0207655", NA, "176", "10604", "15", "38", NA, "e009…
#> $ volume           <chr> "13", "13", "329", "101", NA, "4", NA, "4", "22", "4"…
#> $ issue            <chr> "11", "6", NA, "12", NA, "3", NA, "11", "3", "1", NA,…
#> $ is_oa            <lgl> TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALS…
#> $ cited_by_count   <int> 64, 47, 45, 42, 29, 28, 14, 14, 7, 7, 7, 5, 4, 1, 0, 0
#> $ counts_by_year   <list> [<data.frame[4 x 2]>], [<data.frame[5 x 2]>], [<data.…
#> $ publication_year <int> 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018…
#> $ cited_by_api_url <chr> "https://api.openalex.org/works?filter=cites:W2902888…
#> $ ids              <list> [<tbl_df[5 x 2]>], [<tbl_df[5 x 2]>], [<tbl_df[4 x 2]…
#> $ doi              <chr> "https://doi.org/10.1371/journal.pone.0207655", "htt…
#> $ type             <chr> "journal-article", "journal-article", "journal-articl…
#> $ referenced_works <list> <"https://openalex.org/W1623369780", "https://openale…
#> $ related_works    <list> <"https://openalex.org/W261917380", "https://openale…
#> $ concepts         <list> [<data.frame[2 x 5]>], [<data.frame[11 x 5]>], [<dat…
```

## Convert an OpenAlex data frame to a bibliometrix object

The bibliometrix R-package (<https://www.bibliometrix.org>) provides a set of tools for quantitative research in bibliometrics and scientometrics. Today it represents one of the most used science mapping software in the world. In a recent survey on bibliometric analysis tools, Moral-Muñoz et al. (2020) wrote: "At this moment, maybe Bibliometrix and its Shiny platform contain the more extensive set of techniques implemented, and together with the easiness of its interface, could be a great software for practitioners".

The function **oa2bibliometrix** converts a bibliographic data frame of works into a bibliometrix object. This object can be used as input collection of a science mapping workflow.

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
#>                 226                  56                   1                 200
do.call(oa_fetch, bib_ls) %>% 
  oa2bibliometrix() %>% 
  dplyr::glimpse()
#> Rows: 226
#> Columns: 43
#> $ AU               <chr> "", "", "", "", "", "", "", "", "", "", "", "", "", "…
#> $ RP               <chr> "DEPARTMENT OF FORESTRY AND NATURAL RESOURCES, PURDUE…
#> $ C1               <chr> "DEPARTMENT OF FORESTRY AND NATURAL RESOURCES, PURDUE…
#> $ AU_UN            <chr> "", "", "", "", "", "", "", "", "", "", "", "", "", "…
#> $ AU_CO            <chr> "USA;USA;USA", "ITALY;ITALY;ITALY;MALAYSIA", "INDIA;I…
#> $ ID               <chr> "ORGANISM;DAPHNIA;NANOTOXICOLOGY;DAPHNIA MAGNA;ECOTOX…
#> $ id_url           <chr> "https://openalex.org/W3212020496", "https://openalex…
#> $ display_name     <chr> "Emerging trends in nanoparticle toxicity and the sig…
#> $ author           <list> [<data.frame[3 x 10]>], [<data.frame[4 x 10]>], [<da…
#> $ ab               <chr> "Nanoparticle production is on the rise due to its ma…
#> $ publication_date <chr> "2022-03-01", "2022-01-10", "2022-01-10", "2022-02-22…
#> $ relevance_score  <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ so               <chr> "Chemosphere", "British Food Journal", "Journal of ho…
#> $ so_id            <chr> "https://openalex.org/V203465130", "https://openalex.…
#> $ publisher        <chr> "Elsevier", "Emerald (MCB UP)", "Emerald (MCB UP)", "…
#> $ issn             <list> <"0045-6535", "1879-1298">, <"0007-070X", "1758-4108…
#> $ url              <chr> "https://doi.org/10.1016/j.chemosphere.2021.132941", …
#> $ first_page       <chr> "132941", "2239", NA, "2508", "1129", "118420", "125"…
#> $ last_page        <chr> "132941", "2261", NA, "2508", "1155", "118420", "134"…
#> $ volume           <chr> "291", "124", NA, "19", "39", "292", "157", NA, "29",…
#> $ issue            <chr> NA, "7", NA, "5", "6", NA, NA, NA, "2", NA, NA, NA, N…
#> $ is_oa            <lgl> FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, …
#> $ cited_by_count   <int> 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3,…
#> $ counts_by_year   <list> [<data.frame[1 x 2]>], [<data.frame[1 x 2]>], [<data…
#> $ publication_year <int> 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022,…
#> $ cited_by_api_url <chr> "https://api.openalex.org/works?filter=cites:W3212020…
#> $ ids              <list> [<tbl_df[4 x 2]>], [<tbl_df[2 x 2]>], [<tbl_df[2 x 2…
#> $ doi              <chr> "https://doi.org/10.1016/j.chemosphere.2021.132941", …
#> $ type             <chr> "journal-article", "journal-article", "journal-articl…
#> $ referenced_works <list> <"https://openalex.org/W321855510", "https://openale…
#> $ related_works    <list> <"https://openalex.org/W2040538662", "https://openal…
#> $ concepts         <list> [<data.frame[13 x 5]>], [<data.frame[8 x 5]>], [<dat…
#> $ id_oa            <chr> "W3212020496", "W4205146162", "W4206031355", "W421477…
#> $ CR               <chr> "https://openalex.org/W321855510;https://openalex.org…
#> $ TI               <chr> "EMERGING TRENDS IN NANOPARTICLE TOXICITY AND THE SIG…
#> $ AB               <chr> "NANOPARTICLE PRODUCTION IS ON THE RISE DUE TO ITS MA…
#> $ SO               <chr> "CHEMOSPHERE", "BRITISH FOOD JOURNAL", "JOURNAL OF HO…
#> $ DT               <chr> "JOURNAL-ARTICLE", "JOURNAL-ARTICLE", "JOURNAL-ARTICL…
#> $ DB               <chr> "openalex", "openalex", "openalex", "openalex", "open…
#> $ JI               <chr> "V203465130", "V99313352", "V4210226067", "V15239247"…
#> $ J9               <chr> "V203465130", "V99313352", "V4210226067", "V15239247"…
#> $ SR_FULL          <chr> "NA, , V203465130", "NA, , V99313352", "NA, , V421022…
#> $ SR               <chr> "NA, , V203465130", "NA, , V99313352", "NA, , V421022…
```

# About OpenAlex

![oar-img](man/figures/oar.jpeg)

::: {style="text-align: right"}
Schema credits: [\@dhimmel](https://github.com/dhimmel)
:::

[OpenAlex](https://openalex.org) is a fully open catalog of the global research system. It's named after the ancient [Library of Alexandria](https://en.wikipedia.org/wiki/Library_of_Alexandria). The OpenAlex dataset describes scholarly entities and how those entities are connected to each other. There are five types of entities:

-   **Works** are papers, books, datasets, etc; they cite other works

-   **Authors** are people who create works

-   **Venues** are journals and repositories that host works

-   **Institutions** are universities and other orgs that are affiliated with works (via authors)

-   **Concepts** *tag* Works with a topic

## Acknowledgements

Package hex was made with [Midjourney](https://www.midjourney.com/home/) and thus inherits a [CC BY-NC 4.0 license](https://creativecommons.org/licenses/by-nc/4.0/legalcode).
