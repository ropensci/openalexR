
<!-- README.md is generated from README.Rmd. Please edit that file -->

# openalexR

<!-- badges: start -->
<!-- badges: end -->

The goal of openalexR is to gather bibliographic metadata about
publications, authors, venues, institutions and concepts from OpenAlex
using API.

OpenAlex is a fully open catalog of the global research system. It’s
named after the ancient [Library of
Alexandria](https://en.wikipedia.org/wiki/Library_of_Alexandria). The
OpenAlex dataset describes scholarly entities and how those entities are
connected to each other. There are five types of entities:

-   **Works** are papers, books, datasets, etc; they cite other works

-   **Authors** are people who create works

-   **Venues** are journals and repositories that host works

-   **Institutions** are universities and other orgs that are affiliated
    with works (via authors)

-   **Concepts** *tag* Works with a topic

(source: [OpenAlex website](https://openalex.org))

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
```

# openalexR overview

The basic idea of openalexR is to provide three main functions helping
the user to:

-   Create a query by passing one or more arguments to a function
    (function **oaQueryBuild**)

-   Gather a collection of entities in JSON format (function
    **oaApiRequest**)

-   Transform the JSON in a data frame (similar to an excel sheet) can
    be used as input in a bibliometric or science mapping analysis
    (e.g. using the bibliometrix package) (function **oa2df**)

OpenAlex defined a custom query language based on entity type. You can
choose to write a valid query using that language or, in alternative,
using the function **oaQueryBuild**.

**oaQueryBuild** generates a valid query, written following the OpenAlex
API language, from a set of arguments provided by the user.

The function **oaApiRequest** downloads a collection of entities
matching the query created by **oaQueryBuild** or manually written by
the user. The function will return a JSON object in a list format.

Finally, the function **oa2df** converts the JSON object in classical
bibliographic data frame.

## Get full records through entity IDs.

### Query to obtain all information about a single publications

The following paper:

    Aria, M., & Cuccurullo, C. (2017). bibliometrix: 
    An R-tool for comprehensive science mapping analysis. 
    Journal of informetrics, 11(4), 959-975.

is associated to the OpenAlex-id **W2755950973**.

In this example, we need to pass a single argument to the function, that
is, the identifier of the entity to download: identifier =
“W2755950973”.

``` r
query_work <- oaQueryBuild(
  identifier = "W2755950973",
  entity = "works"
)

cat(query_work)
#> https://api.openalex.org/works/W2755950973
```

As results, **oaQueryBuild** returns the query string including the
OpenAlex endpoint API server address. You should change it by using the
argument “endpoint = *address*”

The function **oaApiRequest** downloads the bibliographic records
matching the query.

``` r
res <- oaApiRequest(
  query_url = query_work
)

df <- oa2df(res, entity = "works")

dplyr::glimpse(df)
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

### Query to obtain all information about a single publications using external id formats

OpenAlex endpoint accepts an OpenAlex ID, but many external IDs (*e.g.*,
DOI, ISSN) are accepted as well, in several formats.

#### DOI (Digital Object Identifier)

We can get a publication record through its DOI using the format
**doi:***doi identifier*. Example:

``` r
query_work <- oaQueryBuild(
  identifier = "doi:10.1016/j.joi.2017.08.007",
  entity = "works"
)

cat(query_work)
#> https://api.openalex.org/works/doi:10.1016/j.joi.2017.08.007
```

``` r
res <- oaApiRequest(
  query_url = query_work
)
df <- oa2df(res, entity = "works")

dplyr::glimpse(df)
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

#### Persistent Identifiers (PIDs)

Many persistent identifiers (PIDs) are canonically expressed as a URL
that will take you to the thing being identified. Where these URL
formats exist, OpenAlex treats them as the canonical ID, and also
accepts them as valid IDs. Example:

``` r
query_work <- oaQueryBuild(
  identifier = "doi:https://doi.org/10.1016/j.joi.2017.08.007",
  entity = "works"
)

cat(query_work)
#> https://api.openalex.org/works/doi:https://doi.org/10.1016/j.joi.2017.08.007

res <- oaApiRequest(
  query_url = query_work
)

df <- oa2df(res, entity = "works")

dplyr::glimpse(df)
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

### Query to obtain all information about a two o more publications

To download the records of two o more identifiers through a single
query, we can recursively apply **oaApiRequest** to each id using the
function **lapply**.

``` r
ids <- c("W2755950973", "W3005144120")

res <- lapply(ids, function(x) {
  oaApiRequest(
    query_url = oaQueryBuild(
      identifier = x,
      entity = "works"
    )
  )
})

df <- oa2df(res, entity = "works")

dplyr::glimpse(df)
#> Rows: 2
#> Columns: 25
#> $ id            <chr> "https://openalex.org/W2755950973", "https://openalex.or…
#> $ TI            <chr> "bibliometrix: An R-tool for comprehensive science mappi…
#> $ author        <list> [<data.frame[2 x 10]>], [<data.frame[3 x 10]>]
#> $ AB            <chr> "Abstract The use of bibliometrics is gradually extendi…
#> $ pubdata       <chr> "2017-11-01", "2020-06-01"
#> $ SO            <chr> "Journal of Informetrics", "Social Indicators Research"
#> $ SO_ID         <chr> "https://openalex.org/V205292342", "https://openalex.org…
#> $ PU            <chr> "Elsevier", "Springer Nature"
#> $ IS            <list> <"1875-5879", "1751-1577">, <"1573-0921", "0303-8300">
#> $ URL           <chr> "https://doi.org/10.1016/j.joi.2017.08.007", "https://do…
#> $ first_page    <chr> "959", "803"
#> $ last_page     <chr> "975", "831"
#> $ volume        <chr> "11", "149"
#> $ issue         <chr> "4", "3"
#> $ OA            <lgl> FALSE, FALSE
#> $ TC            <int> 1456, 48
#> $ TCperYear     <list> [<data.frame[5 x 2]>], [<data.frame[3 x 2]>]
#> $ PY            <int> 2017, 2020
#> $ cited_by_url  <chr> "https://api.openalex.org/works?filter=cites:W2755950973…
#> $ ids           <list> [[<data.frame[3 x 2]>]], [[<data.frame[3 x 2]>]]
#> $ DI            <chr> "https://doi.org/10.1016/j.joi.2017.08.007", "https://do…
#> $ DT            <chr> "journal-article", "journal-article"
#> $ CR            <list> <"https://openalex.org/W189804332", "https://openalex.or…
#> $ related_works <list> <"https://openalex.org/W150292108", "https://openalex.o…
#> $ concept       <list> [<data.frame[8 x 5]>], [<data.frame[6 x 5]>]
```

### Query to obtain all information about a single author

The author Massimo Aria is associated to the OpenAlex-id A923435168.

``` r
query_author <- oaQueryBuild(
  identifier = "A923435168",
  entity = "authors"
)

cat(query_author)
#> https://api.openalex.org/authors/A923435168
```

``` r
res_author <- oaApiRequest(
  query_url = query_author,
  total.count = FALSE,
  verbose = FALSE
)
```

``` r
df <- oa2df(res_author, entity = "authors")

dplyr::glimpse(df)
#> Rows: 1
#> Columns: 15
#> $ id                  <chr> "https://openalex.org/A923435168"
#> $ name                <chr> "Massimo Aria"
#> $ name_alternatives   <lgl> NA
#> $ ids                 <list> [<data.frame[4 x 2]>]
#> $ orcid               <chr> "https://orcid.org/0000-0002-8517-9411"
#> $ works_count         <int> 102
#> $ TC                  <int> 2952
#> $ TCperYear           <list> [<data.frame[11 x 3]>]
#> $ affiliation_name    <chr> "University of Naples Federico II"
#> $ affiliation_id      <chr> "https://openalex.org/I71267560"
#> $ affiliation_ror     <chr> "https://ror.org/05290cv24"
#> $ affiliation_country <chr> "IT"
#> $ affiliation_type    <chr> "education"
#> $ concept             <list> [<data.frame[25 x 5]>]
#> $ works_api_url       <chr> "https://api.openalex.org/works?filter=author.id:A…
```

## Get all entities matching a set of inclusion/exclusion criteria (filters)

In most cases, we are interested in downloading a collection of items
that meet one or more inclusion/exclusion criteria (filters).

In this case, the query definition will not be based on a single
identifier but the choice of the entity type (usually “works”) and one
or more filters about this entity.

Filters narrow the list down to just entities that meet a particular
condition–specifically, a particular value for a particular attribute.
Supported attributes for each endpoints are listed on [OpenAlex API
Documentation
Website](https://docs.openalex.org/api/get-lists-of-entities#filter).

Filters are formatted thusly: **attribute:***value*. You set them using
the *?filter* query parameter. Filters are case-insensitive.

Each endpoint supports its own list of filters. Here they are, by
endpoint:

### /works filters

You can filter using these attributes of the Works object.

-   display_name.search (alias: title.search)

-   publication_year

-   publication_date

-   from_publication_date

-   to_publication_date

-   host_venue.issn

-   authorships.author.id (alias: author.id)

-   type

etc.

You can find more documentation about each attribute on the [OA
Documentation Work page](https://docs.openalex.org/about-the-data/work).

### /authors filters

You can filter using these attributes of the Authors object.

-   display_name.search

-   works_count

-   cited_by_count

-   last_known_institution.id

etc.

You can find more documentation about each attribute on the [OA
Documentation Author
page](https://docs.openalex.org/about-the-data/author).

### /venues filters

You can filter using these attributes of the Venue object.

-   display_name.search

-   issn

-   works_count

-   cited_by_count

etc.

You can find more documentation about each attribute on the [OA
Documentation Venue
page](https://docs.openalex.org/about-the-data/venue).

### /institutions filters

You can filter using these attributes of the Institution object.

-   display_name.search

-   country_code

-   type

-   works_count

-   cited_by_count

-   x_concepts.id

You can find more documentation about each attribute on the [OA
Documentation Institution
page](https://docs.openalex.org/about-the-data/institution).

### /concepts filters

You can filter using these attributes of the Concept object. You can
find more documentation about each attribute on the Concept page.

-   display_name.search

-   level

-   works_count

-   cited_by_count

-   ancestors.id

You can find more documentation about each attribute on the [OA
Documentation Concept
page](https://docs.openalex.org/about-the-data/concept).

Below we show some examples of filters in use.

### Filters based on string matching

**We want to download all works, cited more than 50 times, published
between 2020 and 2021, which include the strings “bibliometric analysis”
or “science mapping” in the title.**

To do that, we have to set filters about three attributes: title content
(*“title.search”*), starting date for publication
(*“from_publication_date”*), and ending date for publication
(*“to_publication_date”*).

Starting and ending dates can be passed to the function **oaQueryBuild**
using the arguments *date_from* and *date_to*. The format is YYYY-MM-DD.

The other attributes can be passed to the function through the argument
*filter*.

When an attribute has more than one item, these can be separated by the
boolean operator OR represented by the symbol **\|** .

On the contrary, different attributes have to be separated by commas.

e.g. **filter = ‘title.search:“bibliometric analysis”\|“science
mapping”, cited_by_count:\>50’**

where:

-   ‘title.search:“bibliometric analysis”\|“science mapping’

means all works containing the string “bibliometric analysis” OR
“science mapping” in the publication title.

and:

-   cited_by_count:\>50

means all works cited more than 10 times.

The whole filter **‘title.search:“bibliometric analysis”\|“science
mapping”,cited_by_count:\>50’**

can be read as:

    *"all works containing the string "bibliometric analysis" OR "science mapping" 
    in the publication title AND cited more than 50 times"*.

``` r
query <- oaQueryBuild(
  identifier = NULL,
  entity = "works",
  filter = 'title.search:"bibliometric analysis"|"science mapping",cited_by_count:>50',
  date_from = "2020-01-01",
  date_to = "2021-12-31",
  search = NULL,
  # sort = "cited_by_count:desc",
  endpoint = "https://api.openalex.org/"
)
```

The **sort** argument indicates how results have to be sorted.

In this example results are sorted by total citations in a descending
order.

Setting the argument total.count=TRUE, the function **oaApiRequest**
returns the number of items matching the query without downloading the
collection.

``` r
res <- oaApiRequest(
  query_url = query,
  total.count = TRUE,
  verbose = FALSE
)

res$count
#> [1] 21
```

Then, we can download the collection:

``` r
res <- oaApiRequest(
  query_url = query,
  total.count = FALSE,
  verbose = FALSE
)

## OpenAlex downloading [======================] 100% eta:  0s
```

and transform it into a data frame:

``` r
df <- oa2df(res, entity = "works")
dplyr::glimpse(df)
#> Rows: 21
#> Columns: 26
#> $ id            <chr> "https://openalex.org/W3160856016", "https://openalex.or…
#> $ TI            <chr> "How to conduct a bibliometric analysis: An overview and…
#> $ author        <list> [<data.frame[5 x 10]>], [<data.frame[2 x 10]>], [<data.…
#> $ AB            <chr> "Bibliometric analysis is a popular and rigorous method …
#> $ pubdata       <chr> "2021-09-01", "2020-09-01", "2020-03-01", "2020-01-19", …
#> $ relscore      <dbl> 212.59715, 166.90810, 164.94908, 133.21031, 132.68163, 1…
#> $ SO            <chr> "Journal of Business Research", "Journal of Business Res…
#> $ SO_ID         <chr> "https://openalex.org/V93284759", "https://openalex.org/…
#> $ PU            <chr> "Elsevier", "Elsevier", "Elsevier", "Ediciones Profesion…
#> $ IS            <list> <"1873-7978", "0148-2963">, <"1873-7978", "0148-2963">,…
#> $ URL           <chr> "https://doi.org/10.1016/j.jbusres.2021.04.070", "https:…
#> $ first_page    <chr> "285", "253", "1", NA, "80", NA, "136776", "3508", "816"…
#> $ last_page     <chr> "296", "261", "14", NA, "105", NA, "136776", "3526", "81…
#> $ volume        <chr> "133", "118", "109", "29", "45", NA, "714", "58", "8", "…
#> $ issue         <chr> NA, NA, NA, "1", "1", NA, NA, "11", "13", "1", NA, "7", …
#> $ OA            <lgl> TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE,…
#> $ TC            <int> 256, 232, 163, 115, 98, 114, 84, 100, 73, 87, 89, 61, 55…
#> $ TCperYear     <list> [<data.frame[2 x 2]>], [<data.frame[3 x 2]>], [<data.fr…
#> $ PY            <int> 2021, 2020, 2020, 2020, 2021, 2020, 2020, 2020, 2020, 20…
#> $ cited_by_url  <chr> "https://api.openalex.org/works?filter=cites:W3160856016…
#> $ ids           <list> [[<data.frame[3 x 2]>]], [[<data.frame[5 x 2]>]], [[<da…
#> $ DI            <chr> "https://doi.org/10.1016/j.jbusres.2021.04.070", "https:…
#> $ DT            <chr> "journal-article", "journal-article", "journal-article",…
#> $ CR            <list> <"https://openalex.org/W1021000864", "https://openalex.…
#> $ related_works <list> <"https://openalex.org/W9427990", "https://openalex.org…
#> $ concept       <list> [<data.frame[7 x 5]>], [<data.frame[7 x 5]>], [<data.fr…
```

### Get all works citing a particular work.

We can download all publications citing another publication by using the
filter attribute **cites:***id*.

For example, if we want to download all publications citing the article
Aria and Cuccurullo (2017), we have just to set the argument filter as:

**filter = “cites:W2755950973”**

where *“W2755950973”* is the OA id for the article by Aria and
Cuccurullo.

``` r
query1 <- oaQueryBuild(
  identifier = NULL,
  entity = "works",
  filter = "cites:W2755950973",
  date_from = NULL,
  date_to = NULL,
  # sort = NULL
)

res1 <- oaApiRequest(
  query_url = query1,
  total.count = TRUE,
  verbose = FALSE
)
```

This query will return a collection of 1422 publications. Let’s to
download it and then to convert it into a data frame:

``` r
res <- oaApiRequest(
  query_url = query1,
  total.count = FALSE,
  verbose = FALSE
)

# OpenAlex downloading [=====================] 100% eta:  0s

df <- oa2df(res, entity = "works")
dplyr::glimpse(df)
#> Rows: 1,422
#> Columns: 25
#> $ id            <chr> "https://openalex.org/W4288801103", "https://openalex.or…
#> $ TI            <chr> "Anaerobic digestion of sewage sludge for biogas &amp; b…
#> $ author        <list> [<data.frame[7 x 10]>], [<data.frame[2 x 10]>], [<data.…
#> $ AB            <chr> "• A critical review on anaerobic digestion of sewage sl…
#> $ pubdata       <chr> "2022-12-01", "2022-11-01", "2022-11-01", "2022-11-01", …
#> $ SO            <chr> "Fuel", "Energy Reports", "Annals of tourism research em…
#> $ SO_ID         <chr> "https://openalex.org/V164770093", "https://openalex.org…
#> $ PU            <chr> "Elsevier", "Elsevier", "Elsevier", "Elsevier", "Elsevie…
#> $ IS            <list> <"0016-2361", "1873-7153">, "2352-4847", "2666-9579", <…
#> $ URL           <chr> "https://doi.org/10.1016/j.fuel.2022.125416", "https://d…
#> $ first_page    <chr> "125416", "2699", "100054", "417", "399", "567", "100399…
#> $ last_page     <chr> "125416", "2711", "100054", "436", "416", "584", "100399…
#> $ volume        <chr> "329", "8", "3", "150", "150", "150", "31", "149", "215"…
#> $ issue         <chr> NA, NA, "2", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ OA            <lgl> FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FAL…
#> $ TC            <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,…
#> $ TCperYear     <list> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, [<d…
#> $ PY            <int> 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 20…
#> $ cited_by_url  <chr> "https://api.openalex.org/works?filter=cites:W4288801103…
#> $ ids           <list> [[<data.frame[2 x 2]>]], [[<data.frame[2 x 2]>]], [[<da…
#> $ DI            <chr> "https://doi.org/10.1016/j.fuel.2022.125416", "https://d…
#> $ DT            <chr> "journal-article", "journal-article", "journal-article",…
#> $ CR            <list> <"https://openalex.org/W1963772285", "https://openalex.…
#> $ related_works <list> <"https://openalex.org/W1249074", "https://openalex.org…
#> $ concept       <list> [<data.frame[11 x 5]>], [<data.frame[6 x 5]>], [<data.f…
```

### Get all authors matching a set of filters

We want download all authors’ records of scholars who work at the
University of Naples Federico II (OpenAlex ID: I71267560) and who have
published more than 499 works:

``` r
query_author <- oaQueryBuild(
  identifier = NULL,
  entity = "authors",
  filter = "last_known_institution.id:I71267560,works_count:>499"
)
```

Check how many records match the query:

``` r
res <- oaApiRequest(
  query_url = query_author,
  total.count = TRUE,
  verbose = FALSE
)
res$count
#> [1] 25

## 34
```

Then, we download and convert the collection:

``` r
res <- oaApiRequest(
  query_url = query_author,
  total.count = FALSE,
  verbose = FALSE
)
df <- oa2df(res, entity = "authors")

dplyr::glimpse(df)
#> Rows: 25
#> Columns: 15
#> $ id                  <chr> "https://openalex.org/A2600338221", "https://opena…
#> $ name                <chr> "Alberto Orso Maria Iorio", "Leonardo Merola", "G.…
#> $ name_alternatives   <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ ids                 <list> [<data.frame[3 x 2]>], [<data.frame[2 x 2]>], [<d…
#> $ orcid               <chr> "https://orcid.org/0000-0002-3798-1135", NA, NA, N…
#> $ works_count         <int> 1165, 1119, 1024, 995, 933, 930, 789, 781, 775, 77…
#> $ TC                  <int> 66456, 46373, 27850, 40410, 23013, 41100, 47049, 1…
#> $ TCperYear           <list> [<data.frame[11 x 3]>], [<data.frame[11 x 3]>], […
#> $ affiliation_name    <chr> "University of Naples Federico II", "University of…
#> $ affiliation_id      <chr> "https://openalex.org/I71267560", "https://openale…
#> $ affiliation_ror     <chr> "https://ror.org/05290cv24", "https://ror.org/0529…
#> $ affiliation_country <chr> "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "I…
#> $ affiliation_type    <chr> "education", "education", "education", "education"…
#> $ concept             <list> [<data.frame[35 x 5]>], [<data.frame[49 x 5]>], […
#> $ works_api_url       <chr> "https://api.openalex.org/works?filter=author.id:A…
```

### Get all institutions matching a set of filters

We want download all records regarding Italian institutions
(country_code:it) that are classified as educational (type:education):

``` r
query_inst <- oaQueryBuild(
  entity = "institutions",
  filter = "country_code:it,type:education"
)
```

We check how many records match the query:

``` r
res <- oaApiRequest(
  query_url = query_inst,
  total.count = TRUE,
  verbose = TRUE
)
#> Requesting url: https://api.openalex.org/institutions?filter=country_code%3Ait%2Ctype%3Aeducation
res$count
#> [1] 231
```

Then we download and convert the collection:

``` r
res <- oaApiRequest(
  query_url = query_inst,
  total.count = FALSE,
  verbose = TRUE
)
#> Requesting url: https://api.openalex.org/institutions?filter=country_code%3Ait%2Ctype%3Aeducation
#> About to get a total of 2 pages of results with a total of 231 records.
df <- oa2df(res, entity = "institutions")

dplyr::glimpse(df)
#> Rows: 231
#> Columns: 19
#> $ id                 <chr> "https://openalex.org/I861853513", "https://openale…
#> $ name               <chr> "Sapienza University of Rome", "University of Padua…
#> $ name_alternatives  <list> "Università degli Studi di Roma \"La Sapienza\"", …
#> $ name_acronyms      <list> NA, "UNIPD", "UNIBO", "UNIMI", NA, NA, "UNITO", "U…
#> $ name_international <list> [<data.frame[1 x 85]>], [<data.frame[1 x 79]>], [<…
#> $ ror                <chr> "https://ror.org/02be6w209", "https://ror.org/00240…
#> $ ids                <list> [<data.frame[6 x 2]>], [<data.frame[6 x 2]>], [<da…
#> $ country            <chr> "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT…
#> $ geo                <list> [<data.frame[1 x 7]>], [<data.frame[1 x 7]>], [<da…
#> $ type               <chr> "education", "education", "education", "education",…
#> $ homepage           <chr> "http://www.uniroma1.it/", "https://www.unipd.it/en…
#> $ image              <chr> "https://upload.wikimedia.org/wikipedia/en/4/45/Sap…
#> $ thumbnail          <chr> "https://upload.wikimedia.org/wikipedia/en/thumb/4/…
#> $ associated_inst    <list> [<data.frame[1 x 24]>], [<data.frame[1 x 12]>], [<…
#> $ works_count        <int> 166018, 130596, 130146, 128812, 91864, 86535, 86316…
#> $ TC                 <int> 11801520, 10453747, 9922093, 10054600, 6365775, 571…
#> $ TCperYear          <list> [<data.frame[11 x 3]>], [<data.frame[11 x 3]>], [<…
#> $ concept            <list> [<data.frame[14 x 5]>], [<data.frame[15 x 5]>], [<…
#> $ works_api_url      <chr> "https://api.openalex.org/works?filter=institutions…
```

### Get all venues matching a set of filters

We want download all records regarding journals that have published more
than 100,000 works:

``` r
query_venue <- oaQueryBuild(
  entity = "venues",
  filter = "works_count:>100000"
)
```

We check how many records match the query:

``` r
res <- oaApiRequest(
  query_url = query_venue,
  total.count = TRUE,
  verbose = TRUE
)
#> Requesting url: https://api.openalex.org/venues?filter=works_count%3A%3E100000
res$count
#> [1] 52
```

Then we download and convert the collection:

``` r
res <- oaApiRequest(
  query_url = query_venue,
  total.count = FALSE,
  verbose = TRUE
)
#> Requesting url: https://api.openalex.org/venues?filter=works_count%3A%3E100000
#> About to get a total of 1 pages of results with a total of 52 records.

df <- oa2df(res, entity = "venues")

dplyr::glimpse(df)
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
#> $ works_count   <int> 795328, 754208, 744378, 512481, 464506, 453060, 434256, …
#> $ TC            <int> 2827453, 2479946, 251559, 5575514, 7324653, 3619810, 195…
#> $ TCperYear     <list> [<data.frame[11 x 3]>], [<data.frame[11 x 3]>], [<data.…
#> $ concept       <list> [<data.frame[20 x 5]>], [<data.frame[21 x 5]>], [<data.…
#> $ works_api_url <chr> "https://api.openalex.org/works?filter=host_venue.id:V27…
```

### Get all concepts matching a set of filters

We want to download the records of all the concepts that concern at
least one million works:

``` r
query_concept <- oaQueryBuild(
  entity = "concepts",
  filter = "works_count:>1000000"
)
```

We check how many records match the query:

``` r
res <- oaApiRequest(
  query_url = query_concept,
  total.count = TRUE,
  verbose = TRUE
)
#> Requesting url: https://api.openalex.org/concepts?filter=works_count%3A%3E1000000
res$count
#> [1] 146

## 112
```

Then we download and convert the collection:

``` r
res <- oaApiRequest(
  query_url = query_concept,
  total.count = FALSE,
  verbose = TRUE
)
#> Requesting url: https://api.openalex.org/concepts?filter=works_count%3A%3E1000000
#> About to get a total of 1 pages of results with a total of 146 records.

df <- oa2df(res, entity = "concepts")

dplyr::glimpse(df)
#> Rows: 146
#> Columns: 16
#> $ id                        <chr> "https://openalex.org/C41008148", "https://o…
#> $ name                      <chr> "Computer science", "Medicine", "Chemistry",…
#> $ name_international        <list> [<data.frame[1 x 185]>], [<data.frame[1 x 1…
#> $ description               <chr> "theoretical study of the formal foundation …
#> $ description_international <list> [<data.frame[1 x 40]>], [<data.frame[1 x 44…
#> $ wikidata                  <chr> "https://www.wikidata.org/wiki/Q21198", "htt…
#> $ level                     <int> 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0,…
#> $ ids                       <list> [<data.frame[5 x 2]>], [<data.frame[5 x 2]>…
#> $ image                     <chr> "https://upload.wikimedia.org/wikipedia/comm…
#> $ thumbnail                 <chr> "https://upload.wikimedia.org/wikipedia/comm…
#> $ ancestors                 <list> NA, NA, NA, NA, NA, NA, NA, [<data.frame[2 …
#> $ rel_concepts              <list> [<data.frame[93 x 5]>], [<data.frame[51 x 5…
#> $ works_count               <int> 40337048, 36498873, 20835084, 17392391, 1704…
#> $ TC                        <int> 218024837, 375275103, 330416120, 161663667, …
#> $ TCperYear                 <list> [<data.frame[11 x 3]>], [<data.frame[11 x 3…
#> $ works_api_url             <chr> "https://api.openalex.org/works?filter=conce…
```

## Convert an OpenAlex data frame to a bibliometrix object

The bibliometrix R-package (<https://www.bibliometrix.org>) provides a
set of tools for quantitative research in bibliometrics and
scientometrics. It is written in the R language, which is an open-source
environment and ecosystem.

Today it represents one of the most used science mapping software in the
world. In a recent survey on bibliometric analysis tools, Moral-Muñoz et
al. (2020) wrote: “At this moment, maybe Bibliometrix and its Shiny
platform contain the more extensive set of techniques implemented, and
together with the easiness of its interface, could be a great software
for practitioners”.

The function **oa2bibliometrix** converts a bibliographic data frame of
works into a bibliometrix object. This object can be used as input
collection of a science mapping workflow.

``` r
query1 <- oaQueryBuild(
  identifier = NULL,
  entity = "works",
  filter = "cites:W2755950973",
  date_from = "2022-01-01",
  date_to = "2022-03-31",
  # sort = NULL
)

res1 <- oaApiRequest(
  query_url = query1,
  total.count = TRUE,
  verbose = FALSE
)
```

This query will return a collection of 209 publications. Let’s download
it:

``` r
res1 <- oaApiRequest(
  query_url = query1,
  total.count = FALSE,
  verbose = FALSE
)
```

Convert it into a data frame and then into a bibliometrix object:

``` r
df <- oa2df(res1, entity = "works")
M <- oa2bibliometrix(df)
dplyr::glimpse(M)
#> Rows: 209
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
#> $ TC            <int> 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,…
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
