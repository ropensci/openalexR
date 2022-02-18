openalexR
====================================

# An R-package to gather bibliographic data from OpenAlex. 

&nbsp;

    The goal of openalexR is to gather bibliographic metadata about publications, 
    authors, venues, institutions and concepts from OpenAlex using API.
    
OpenAlex is a fully open catalog of the global research system. It's named after the ancient Library of Alexandria.
The OpenAlex dataset describes scholarly entities and how those entities are connected to each other. There are five types of entities:

* **Works** are papers, books, datasets, etc; they cite other works

* **Authors** are people who create works

* **Venues** are journals and repositories that host works

* **Institutions** are universities and other orgs that are affiliated with works (via authors)

* **Concepts** *tag* Works with a topic

(source:  [OpenAlex website](https://CRAN.R-project.org))

&nbsp; 

## Installation

You can install the developer version of the openalexR from [GitHub](https://github.com) with:

    install.packages("devtools")
    devtools::install_github("massimoaria/openalexR")



You can install the released version of openalexR from [CRAN](https://CRAN.R-project.org) with:


    install.packages("openalexR")

&nbsp; 

## Load the package

```{r}
   library(openalexR)
```

   

&nbsp; 

&nbsp; 


# How to downalod items from OpenAlex endpoijnt

OpenAlex defined a custom query language based on entity type. You can choose to write a valid query using that language or, in alternative, using the function **oaQueryBuild**.

**oaQueryBuild** generates a valid query, written following the OpenAlex API language, from a set of arguments provided by the user.

&nbsp; 

## Get full records through entity IDs.

&nbsp; 

### Query to obtain all information about a single publications

The following paper:

    Aria, M., & Cuccurullo, C. (2017). bibliometrix: 
    An R-tool for comprehensive science mapping analysis. 
    Journal of informetrics, 11(4), 959-975.

is associated to the OpenAlex-id **W2755950973**.

In this example, we need to pass a single argument to the function, that is, the identifier of the entity to download: identifier = "W2755950973".

```{r}
query_work <- oaQueryBuild(
  identifier = "W2755950973",
  entity = "works"
  )

cat(query_work)

## https://api.openalex.org/works/W2755950973
```


As results, **oaQueryBuild** returns the query string including the OpenAlex endpoint API server address. You should change it by using the argument "endpoint = *address*"

The function **oaApiRequest** downloads the bibliographic records matching the query.


```{r}
 res <- oaApiRequest(
    query_url = query_work
    )
```

```{r}
df <- oa2df(res, entity = "works")

dplyr::glimpse(df)

# Rows: 1
# Columns: 22
# $ id            <chr> "https://openalex.org/W2755950973"
# $ TI            <chr> "bibliometrix: An R-tool for comprehensive science mapping analysis"
# $ author        <list> [<data.frame[2 x 10]>]
# $ AB            <chr> ""
# $ pubdata       <chr> "2017-11-01"
# $ relscore      <dbl> 15.8851
# $ SO            <chr> "Journal of Informetrics"
# $ SO_ID         <chr> "https://openalex.org/V205292342"
# $ PU            <chr> "Elsevier"
# $ IS            <list> <"1875-5879", "1751-1577">
# $ URL           <chr> "https://doi.org/10.1016/j.joi.2017.08.007"
# $ OA            <lgl> FALSE
# $ TC            <int> 1120
# $ TCperYear     <lgl> NA
# $ PY            <int> 2017
# $ cited_by_url  <chr> "https://api.openalex.org/works?filter=cites:W2755950973"
# $ ids           <list> [[<data.frame[4 x 2]>]]
# $ DI            <chr> "https://doi.org/10.1016/j.joi.2017.08.007"
# $ DT            <chr> "journal-article"
# $ CR            <list> <"https://openalex.org/W192816456", "https://openalex.org/W2132948308", "https://openalex.org/W349374990", "https://openalex.org/W767067438", "https://openalex.org/W2002117998", "https://openalex.org/W2034800299", "https://o…
# $ related_works <list> <"https://openalex.org/W2408216567", "https://openalex.org/W3125505924", "https://openalex.org/W2150220236", "https://openalex.org/W3125707221", "https://openalex.org/W2135455887", "https://openalex.org/W2128438887", "https:…
# $ concept       <list> [<data.frame[4 x 5]>]
```

&nbsp; 

### Query to obtain all information about a single publications using external id formats

OpenAlex endpoint accepts an OpenAlex ID, but many external IDs (eg DOI, ISSN) are accepted as well, in several formats.


**DOI (Digital Object Identifier)**

We can get a publication record through its DOI using the format **doi:***doi identifier*. Example:
```{r}
query_work <- oaQueryBuild(
  identifier = "doi:10.1016/j.joi.2017.08.007",
  entity = "works"
  )

cat(query_work)

## https://api.openalex.org/works/doi:https://doi.org/10.1016/j.joi.2017.08.007
```

```{r}
 res <- oaApiRequest(
    query_url = query_work
    )
```

```{r}
df <- oa2df(res, entity = "works")

dplyr::glimpse(df)

# Rows: 1
# Columns: 22
# $ id            <chr> "https://openalex.org/W2755950973"
# $ TI            <chr> "bibliometrix: An R-tool for comprehensive science mapping analysis"
# $ author        <list> [<data.frame[2 x 10]>]
# $ AB            <chr> ""
# $ pubdata       <chr> "2017-11-01"
# $ relscore      <dbl> 15.26293
# $ SO            <chr> "Journal of Informetrics"
# $ SO_ID         <chr> "https://openalex.org/V205292342"
# $ PU            <chr> "Elsevier"
# $ IS            <list> <"1875-5879", "1751-1577">
# $ URL           <chr> "https://doi.org/10.1016/j.joi.2017.08.007"
# $ OA            <lgl> FALSE
# $ TC            <int> 1120
# $ TCperYear     <lgl> NA
# $ PY            <int> 2017
# $ cited_by_url  <chr> "https://api.openalex.org/works?filter=cites:W2755950973"
# $ ids           <list> [[<data.frame[4 x 2]>]]
# $ DI            <chr> "https://doi.org/10.1016/j.joi.2017.08.007"
# $ DT            <chr> "journal-article"
# $ CR            <list> <"https://openalex.org/W192816456", "https://openalex.org/W2132948308", "…
# $ related_works <list> <"https://openalex.org/W2408216567", "https://openalex.org/W3125505924", …
# $ concept       <list> [<data.frame[4 x 5]>]
```



**Persistent Identifiers (PIDs)**

Many persistent identifiers (PIDs) are canonically expressed as a URL that will take you to the thing being identified. Where these URL formats exist, OpenAlex treats them as the canonical ID, and also accepts them as valid IDs. Example:
```{r}
query_work <- oaQueryBuild(
  identifier = "doi:https://doi.org/10.1016/j.joi.2017.08.007",
  entity = "works"
  )

cat(query_work)

## https://api.openalex.org/works/doi:https://doi.org/10.1016/j.joi.2017.08.007
```

```{r}
 res <- oaApiRequest(
    query_url = query_work
    )
```

```{r}
df <- oa2df(res, entity = "works")

dplyr::glimpse(df)

# Rows: 1
# Columns: 22
# $ id            <chr> "https://openalex.org/W2755950973"
# $ TI            <chr> "bibliometrix: An R-tool for comprehensive science mapping analysis"
# $ author        <list> [<data.frame[2 x 10]>]
# $ AB            <chr> ""
# $ pubdata       <chr> "2017-11-01"
# $ relscore      <dbl> 15.26293
# $ SO            <chr> "Journal of Informetrics"
# $ SO_ID         <chr> "https://openalex.org/V205292342"
# $ PU            <chr> "Elsevier"
# $ IS            <list> <"1875-5879", "1751-1577">
# $ URL           <chr> "https://doi.org/10.1016/j.joi.2017.08.007"
# $ OA            <lgl> FALSE
# $ TC            <int> 1120
# $ TCperYear     <lgl> NA
# $ PY            <int> 2017
# $ cited_by_url  <chr> "https://api.openalex.org/works?filter=cites:W2755950973"
# $ ids           <list> [[<data.frame[4 x 2]>]]
# $ DI            <chr> "https://doi.org/10.1016/j.joi.2017.08.007"
# $ DT            <chr> "journal-article"
# $ CR            <list> <"https://openalex.org/W192816456", "https://openalex.org/W2132948308", "…
# $ related_works <list> <"https://openalex.org/W2408216567", "https://openalex.org/W3125505924", …
# $ concept       <list> [<data.frame[4 x 5]>]
```

### Query to obtain all information about a two o more publications

To download the records of two o more identifiers through a single query, we can recursively apply **oaApiRequest** to each id using the function **lapply**.

```{r}
ids <- c("W2755950973","W3005144120")

res <- lapply(ids, function(x){
  oaApiRequest(
    query_url = oaQueryBuild(
      identifier = x,
      entity = "works"
    )
  )
})
```

```{r}
df <- oa2df(res, entity = "works")

dplyr::glimpse(df)

# Rows: 2
# Columns: 22
# $ id            <chr> "https://openalex.org/W2755950973", "https://openalex.org/W3005144120"
# $ TI            <chr> "bibliometrix: An R-tool for comprehensive science mapping analysis", "Mapping the Evolution of Social Research and Data Scien…
# $ author        <list> [<data.frame[2 x 10]>], [<data.frame[3 x 10]>]
# $ AB            <chr> "", "Social Indicators Research (SIR) year by year has consolidated its preeminent position in the debate concerning the study…
# $ pubdata       <chr> "2017-11-01", "2020-06-01"
# $ relscore      <dbl> 15.88510, 15.72882
# $ SO            <chr> "Journal of Informetrics", "Social Indicators Research"
# $ SO_ID         <chr> "https://openalex.org/V205292342", "https://openalex.org/V112952035"
# $ PU            <chr> "Elsevier", "Springer Nature"
# $ IS            <list> <"1875-5879", "1751-1577">, <"1573-0921", "0303-8300">
# $ URL           <chr> "https://doi.org/10.1016/j.joi.2017.08.007", "https://doi.org/10.1007/s11205-020-02281-3"
# $ OA            <lgl> FALSE, FALSE
# $ TC            <int> 1120, 32
# $ TCperYear     <lgl> NA, NA
# $ PY            <int> 2017, 2020
# $ cited_by_url  <chr> "https://api.openalex.org/works?filter=cites:W2755950973", "https://api.openalex.org/works?filter=cites:W3005144120"
# $ ids           <list> [[<data.frame[4 x 2]>]], [[<data.frame[4 x 2]>]]
# $ DI            <chr> "https://doi.org/10.1016/j.joi.2017.08.007", "https://doi.org/10.1007/s11205-020-02281-3"
# $ DT            <chr> "journal-article", "journal-article"
# $ CR            <list> <"https://openalex.org/W192816456", "https://openalex.org/W2132948308", "https://openalex.org/W349374990", "https://openalex.…
# $ related_works <list> <"https://openalex.org/W2408216567", "https://openalex.org/W3125505924", "https://openalex.org/W2150220236", "https://openalex…
# $ concept       <list> [<data.frame[4 x 5]>], [<data.frame[8 x 5]>]
```

### Query to obtain all information about a single author

The author Massimo Aria is associated to the OpenAlex-id A923435168.

```{r}

query_author <- oaQueryBuild(
  identifier = "A923435168",
  entity = "authors")

cat(query_author)

## https://api.openalex.org/authors/A923435168
```

```{r}
 res_author <- oaApiRequest(
    query_url = query_author,
    total.count = FALSE,
    verbose = FALSE
 )
```

```{r}
df <- oa2df(res_author, entity = "authors")

dplyr::glimpse(df)

# Rows: 1
# Columns: 16
# $ id                  <chr> "https://openalex.org/A923435168"
# $ name                <chr> "Massimo Aria"
# $ name_alternatives   <lgl> NA
# $ rel_score           <dbl> 16.74535
# $ ids                 <list> [<data.frame[4 x 2]>]
# $ orcid               <chr> "https://orcid.org/0000-0002-8517-9411"
# $ works_count         <int> 102
# $ TC                  <int> 2525
# $ TCperYear           <list> [<data.frame[11 x 3]>]
# $ affiliation_name    <chr> "University of Naples Federico II"
# $ affiliation_id      <chr> "https://openalex.org/I71267560"
# $ affiliation_ror     <chr> "https://ror.org/05290cv24"
# $ affiliation_country <chr> "IT"
# $ affiliation_type    <chr> "education"
# $ concept             <list> [<data.frame[17 x 5]>]
# $ works_api_url       <chr> "https://api.openalex.org/works?filter=author.id:A923435168"
```

&nbsp;

&nbsp;

## Get all entities matching a set of inclusion/exclusion criteria (filters) 

In most cases, we are interested in downloading a collection of items that meet one or more inclusion/exclusion criteria (filters).

&nbsp;

In this case, the query definition will not be based on a single identifier but the choice of the entity type (usually "works") and one or more filters about this entity.

Filters narrow the list down to just entities that meet a particular condition--specifically, a particular value for a particular attribute. Supported attributes for each endpoints are listed on [OpenAlex API Documentation Website](https://docs.openalex.org/api/get-lists-of-entities#filter).

Filters are formatted thusly: **attribute:***value*. You set them using the *?filter* query parameter. Filters are case-insensitive.

&nbsp;

Each endpoint supports its own list of filters. Here they are, by endpoint:

&nbsp;

**/works filters**

You can filter using these attributes of the Works object. 

* display_name.search (alias: title.search)

* publication_year

* publication_date

* from_publication_date

* to_publication_date

* host_venue.issn

* authorships.author.id (alias: author.id)

* type

etc.

You can find more documentation about each attribute on the [OA Documentation Work page](https://docs.openalex.org/about-the-data/work).

&nbsp;

**/authors filters**

You can filter using these attributes of the Authors object. 

* display_name.search

* works_count

* cited_by_count

* last_known_institution.id

etc.

You can find more documentation about each attribute on the [OA Documentation Author page](https://docs.openalex.org/about-the-data/author).

&nbsp;

**/venues filters**

You can filter using these attributes of the Venue object. 

* display_name.search

* issn

* works_count

* cited_by_count

etc. 

You can find more documentation about each attribute on the [OA Documentation Venue page](https://docs.openalex.org/about-the-data/venue).

&nbsp;

**/institutions filters**

You can filter using these attributes of the Institution object. 

* display_name.search

* country_code

* type

* works_count
 
* cited_by_count

* x_concepts.id

You can find more documentation about each attribute on the  [OA Documentation Institution page](https://docs.openalex.org/about-the-data/institution).

&nbsp;

**/concepts filters**

You can filter using these attributes of the Concept object. You can find more documentation about each attribute on the Concept page.

* display_name.search

* level

* works_count

* cited_by_count

* ancestors.id

You can find more documentation about each attribute on the [OA Documentation Concept page](https://docs.openalex.org/about-the-data/concept).

&nbsp;

Below we show some examples of filters in use.

&nbsp;

### Filters based on string matching

**We want to download all works, cited more than 50 times, published between 2020 and 2021, which include the strings "bibliometric analysis" or "science mapping" in the title.**

To do that, we have to set filters about three attributes: title content (*"title.search"*), starting date for publication (*"from_publication_date"*), and ending date for publication (*"to_publication_date"*).

Starting and ending dates can be passed to the function **oaQueryBuild** using the arguments *date_from* and *date_to*. The format is YYYY-MM-DD.

The other attributes can be passed to the function through the argument *filter*.  

When an attribute has more than one item, these can be separated by the boolean operator OR represented by the symbol **|** .

On the contrary, different attributes have to be separated by commas.

e.g. **filter = 'title.search:"bibliometric analysis"|"science mapping", cited_by_count:>50'**

where:

* 'title.search:"bibliometric analysis"|"science mapping'

means all works containing the string "bibliometric analysis" OR "science mapping" in the publication title.

and:

* cited_by_count:>50

means all works cited more than 10 times.

The whole filter **'title.search:"bibliometric analysis"|"science mapping",cited_by_count:>50'** 

can be read as:

    *"all works containing the string "bibliometric analysis" OR "science mapping" 
    in the publication title AND cited more than 50 times"*.



```{r}
query <- oaQueryBuild(
    identifier=NULL,
    entity = "works",
    filter = 'title.search:"bibliometric analysis"|"science mapping",cited_by_count:>50',
    date_from = "2020-01-01",
    date_to = "2021-12-31",
    search=NULL,
    sort="relevance_score:desc",
    endpoint = "https://api.openalex.org/")
```


The **sort** argument indicates how results have to be sorted. 

In this example results are sorted by relevance score in a descending order.

Setting the argument total.count=TRUE, the function **oaApiRequest** returns the number of items matching the query without downloading the collection. 

```{r}
 res <- oaApiRequest(
    query_url = query,
    total.count = TRUE,
    verbose = FALSE
    )

res$count

## 235
```

Then, we can download the collection:

```{r}
 res <- oaApiRequest(
    query_url = query,
    total.count = FALSE,
    verbose = FALSE
    )
    
## OpenAlex downloading [======================] 100% eta:  0s
```

and transform it into a data frame:

```{r}
df <- oa2df(res, entity = "works")

## converting [===============================] 100% eta:  0s
```

```{r}
dplyr::glimpse(df)

# Rows: 235
# Columns: 22
# $ id            <chr> "https://openalex.org/W2755950973", "https://openalex.org/W1965746216", "https://openalex.org/W2108680868", "ht…
# $ TI            <chr> "bibliometrix: An R-tool for comprehensive science mapping analysis", "Green supply chain management: A review …
# $ author        <list> [<data.frame[2 x 10]>], [<data.frame[3 x 10]>], [<data.frame[4 x 10]>], [<data.frame[3 x 10]>], [<data.frame[2…
# $ AB            <chr> "", "", "", "Social responsibilities of businesses and their managers have been discussed since the 1950s. Yet …
# $ pubdata       <chr> "2017-11-01", "2015-04-01", "2011-07-01", "2005-09-01", "2007-07-11", "2015-11-01", "2012-08-01", "2015-12-01",…
# $ relscore      <dbl> 454.6580, 398.4448, 373.5673, 314.2572, 303.1636, 295.9913, 279.6742, 278.5277, 251.8241, 249.3967, 243.1842, 2…
# $ SO            <chr> "Journal of Informetrics", "International Journal of Production Economics", "Journal of the Association for Inf…
# $ SO_ID         <chr> "https://openalex.org/V205292342", "https://openalex.org/V184816971", "https://openalex.org/V80113298", "https:…
# $ PU            <chr> "Elsevier", "Elsevier", "Wiley", "SAGE", "Springer Nature", "Wiley", "Wiley", "Springer Nature", "Journal of Da…
# $ IS            <list> <"1875-5879", "1751-1577">, <"0925-5273", "1873-7579">, <"1532-2882", "1532-2890">, <"0007-6503", "1552-4205">…
# $ URL           <chr> "https://doi.org/10.1016/j.joi.2017.08.007", "https://doi.org/10.1016/j.ijpe.2015.01.003", "https://doi.org/10.…
# $ OA            <lgl> FALSE, FALSE, FALSE, NA, FALSE, TRUE, FALSE, TRUE, TRUE, NA, FALSE, FALSE, FALSE, NA, FALSE, FALSE, TRUE, FALSE…
# $ TC            <int> 1120, 720, 829, 656, 272, 652, 377, 401, 316, 280, 209, 136, 76, 209, 300, 313, 227, 164, 179, 172, 115, 125, 1…
# $ TCperYear     <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
# $ PY            <int> 2017, 2015, 2011, 2005, 2007, 2015, 2012, 2015, 2017, 2009, 2016, 2005, 2014, 2004, 2011, 2007, 2017, 2008, 201…
# $ cited_by_url  <chr> "https://api.openalex.org/works?filter=cites:W2755950973", "https://api.openalex.org/works?filter=cites:W196574…
# $ ids           <list> [[<data.frame[4 x 2]>]], [[<data.frame[4 x 2]>]], [[<data.frame[4 x 2]>]], [[<data.frame[4 x 2]>]], [[<data.fr…
# $ DI            <chr> "https://doi.org/10.1016/j.joi.2017.08.007", "https://doi.org/10.1016/j.ijpe.2015.01.003", "https://doi.org/10.…
# $ DT            <chr> "journal-article", "journal-article", "journal-article", "journal-article", "journal-article", "journal-article…
# $ CR            <list> <"https://openalex.org/W192816456", "https://openalex.org/W2132948308", "https://openalex.org/W349374990", "ht…
# $ related_works <list> <"https://openalex.org/W2408216567", "https://openalex.org/W3125505924", "https://openalex.org/W2150220236", "…
# $ concept       <list> [<data.frame[4 x 5]>], [<data.frame[8 x 5]>], [<data.frame[4 x 5]>], [<data.frame[8 x 5]>], [<data.frame[9 x 5…
```

&nbsp;

### Get all workss citing a particular work.

We can download all publications citing another publication by using the filter attribute **cites:***id*.

For example, if we want to download all publications citing the article Aria and Cuccurullo (2017), we have just to set the argument filter as:

**filter = "cites:W2755950973"**

where *"W2755950973"* is the OA id for the article by Aria and Cuccurullo.

```{r}
query1 <- oaQueryBuild(
 identifier=NULL,
 entity = "works",
 filter = "cites:W2755950973",
 date_from = NULL,
 date_to = NULL,
 sort=NULL)

res1 <- oaApiRequest(
    query_url = query1,
    total.count = TRUE,
    verbose = FALSE
    )
```

This query will return a collection of:
```{r}
cat(res1$count, "publications")

## 1094 publications
```
