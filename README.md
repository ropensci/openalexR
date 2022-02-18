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

(source:  [OpenAlex website](https://openalex.org))

&nbsp; 

## Installation

You can install the developer version of the openalexR from [GitHub](https://github.com) with:

    install.packages("devtools")
    devtools::install_github("massimoaria/openalexR")



You can install the released version of openalexR from [CRAN](https://CRAN.R-project.org) with:


    install.packages("openalexR")

&nbsp; 

## Package loading

```{r}
   library(openalexR)
```

   

&nbsp; 

&nbsp; 


# openalexR overview

The basic idea of openalexR is to provide three main functions helping the user to:

* Create a query by passing one or more arguments to a function (function **oaQueryBuild**)

* Gather a collection of entities in JSON format (function **oaApiRequest**)

* Transform the JSON in a data frame (similar to an excel sheet) can be used as input in a bibliometric or science mapping analysis (e.g. using the bibliometrix package) (function **oa2df**)

&nbsp;

OpenAlex defined a custom query language based on entity type. You can choose to write a valid query using that language or, in alternative, using the function **oaQueryBuild**.

**oaQueryBuild** generates a valid query, written following the OpenAlex API language, from a set of arguments provided by the user.

The function **oaApiRequest** downloads a collection of entities matching the query created by **oaQueryBuild** or manually written by the user. The function will return a JSON object in a list format.

Finally, the function **oa2df** converts  the JSON object in classical bibliographic data frame.


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

### Get all works citing a particular work.

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

Let's to download it:

```{r}
res1 <- oaApiRequest(
    query_url = query1,
    total.count = FALSE,
    verbose = FALSE
    )

# OpenAlex downloading [=====================] 100% eta:  0s
```

And then to convert it into a data frame:

```{r}
df <- oa2df(res, entity = "works")

# converting [===============================] 100% eta:  0s
```
```{r}
dplyr::glimpse(df)

# Rows: 1,094
# Columns: 22
# $ id            <chr> "https://openalex.org/W4206700957", "https://openalex.org/W3215846996", "https://openalex.org/W3167593369", "ht…
# $ TI            <chr> "Developing metrics for emerging technologies: identification and assessment", "20 years after To Err Is Human:…
# $ author        <list> [<data.frame[5 x 10]>], [<data.frame[4 x 10]>], [<data.frame[5 x 10]>], [<data.frame[7 x 10]>], [<data.frame[3…
# $ AB            <chr> "• current research topics in the field of emerging technologies • direction of future research in the field of…
# $ pubdata       <chr> "2022-03-01", "2022-03-01", "2022-03-01", "2022-02-15", "2022-02-01", "2022-01-26", "2022-01-25", "2022-01-24",…
# $ relscore      <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
# $ SO            <chr> "Technological Forecasting and Social Change", "Safety Science", "International Transactions in Operational Res…
# $ SO_ID         <chr> "https://openalex.org/V39307421", "https://openalex.org/V123149298", "https://openalex.org/V267729", "https://o…
# $ PU            <chr> "Elsevier", "Elsevier", "Wiley", "Elsevier", "Elsevier", NA, NA, "Springer Nature", "Taylor & Francis", "Spring…
# $ IS            <list> <"0040-1625", "1873-5509">, <"1879-1042", "0925-7535">, <"1475-3995", "0969-6016">, <"1873-3336", "0304-3894">…
# $ URL           <chr> "https://doi.org/10.1016/j.techfore.2021.121456", "https://www.sciencedirect.com/science/article/abs/pii/S09257…
# $ OA            <lgl> NA, NA, TRUE, FALSE, FALSE, NA, TRUE, NA, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, NA…
# $ TC            <int> NA, 0, 3, 0, 0, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
# $ TCperYear     <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
# $ PY            <int> 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 202…
# $ cited_by_url  <chr> "https://api.openalex.org/works?filter=cites:W4206700957", "https://api.openalex.org/works?filter=cites:W321584…
# $ ids           <list> [[<data.frame[4 x 2]>]], [[<data.frame[4 x 2]>]], [[<data.frame[4 x 2]>]], [[<data.frame[4 x 2]>]], [[<data.fr…
# $ DI            <chr> "https://doi.org/10.1016/j.techfore.2021.121456", "https://doi.org/10.1016/j.ssci.2021.105593", "https://doi.or…
# $ DT            <chr> "journal-article", "journal-article", "journal-article", "journal-article", "journal-article", "journal-article…
# $ CR            <list> <"https://openalex.org/W2105822516", "https://openalex.org/W2787955605", "https://openalex.org/W2135455887", "…
# $ related_works <list> "https://openalex.org/W2791453970", <NULL>, <"https://openalex.org/W2510274029", "https://openalex.org/W226895…
# $ concept       <list> [<data.frame[5 x 5]>], <NULL>, [<data.frame[6 x 5]>], [<data.frame[12 x 5]>], [<data.frame[4 x 5]>], [<data.fr…
```

&nbsp;

### Get all authors matching a set of filters

We want download all authors' records of scholars who work at the University of Naples Federico II (OpenAlex ID: I71267560) and who have published more than 499 works:

```{r}
query_author <- oaQueryBuild(
  identifier = NULL,
  entity = "authors",
  filter = "last_known_institution.id:I71267560,works_count:>499")

```

Check how many records match the query:

```{r}
res <- oaApiRequest(query_url = query_author,
                    total.count = TRUE,
                    verbose=FALSE)
res$count

## 34
```

Then, we download and convert the collection:

```{r}
res <- oaApiRequest(query_url = query_author,
                    total.count = FALSE,
                    verbose=FALSE)
df <- oa2df(res, entity = "authors")

dplyr::glimpse(df)

# Rows: 34
# Columns: 16
# $ id                  <chr> "https://openalex.org/A2061787601", "https://openalex.org/A2163747539", "https://openalex.org/A3088886425…
# $ name                <chr> "Luca Lista", "Crisostomo Sciacca", "Alberto Aloisio", "Alberto Orso Maria Iorio", "Leonardo Merola", "Sa…
# $ name_alternatives   <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
# $ rel_score           <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
# $ ids                 <list> [<data.frame[3 x 2]>], [<data.frame[3 x 2]>], [<data.frame[3 x 2]>], [<data.frame[3 x 2]>], [<data.frame…
# $ orcid               <chr> "https://orcid.org/0000-0001-6471-5492", "https://orcid.org/0000-0002-8412-4072", "https://orcid.org/0000…
# $ works_count         <int> 2538, 2230, 1506, 1165, 1119, 1055, 1025, 995, 965, 933, 921, 792, 780, 774, 769, 758, 743, 743, 740, 717…
# $ TC                  <int> 97989, 60833, 77894, 67649, 46274, 34562, 27109, 39599, 47609, 21869, 39123, 60080, 16636, 47579, 42042, …
# $ TCperYear           <list> [<data.frame[11 x 3]>], [<data.frame[11 x 3]>], [<data.frame[11 x 3]>], [<data.frame[11 x 3]>], [<data.f…
# $ affiliation_name    <chr> "University of Naples Federico II", "University of Naples Federico II", "University of Naples Federico II…
# $ affiliation_id      <chr> "https://openalex.org/I71267560", "https://openalex.org/I71267560", "https://openalex.org/I71267560", "ht…
# $ affiliation_ror     <chr> "https://ror.org/05290cv24", "https://ror.org/05290cv24", "https://ror.org/05290cv24", "https://ror.org/0…
# $ affiliation_country <chr> "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT…
# $ affiliation_type    <chr> "education", "education", "education", "education", "education", "education", "education", "education", "…
# $ concept             <list> [<data.frame[28 x 5]>], [<data.frame[23 x 5]>], [<data.frame[46 x 5]>], [<data.frame[35 x 5]>], [<data.f…
# $ works_api_url       <chr> "https://api.openalex.org/works?filter=author.id:A2061787601", "https://api.openalex.org/works?filter=aut…
```

&nbsp;

### Get all institutions matching a set of filters

We want download all records regarding Italian institutions (country_code:it) that are classified as educational (type:education):

```{r}
query_inst <- oaQueryBuild(
  entity = "institutions",
  filter = "country_code:it,type:education")

```

We check how many records match the query:

```{r}
res <- oaApiRequest(query_url = query_inst,
                    total.count = TRUE,
                    verbose=TRUE)
res$count

## 231
```

Then we download and convert the collection:

```{r}
res <- oaApiRequest(query_url = query_inst,
                    total.count = FALSE,
                    verbose=TRUE)
df <- oa2df(res, entity = "institutions")

dplyr::glimpse(df)

# Rows: 231
# Columns: 20
# $ id                 <chr> "https://openalex.org/I861853513", "https://openalex.org/I189158943", "https://openalex.org/I9360294", "ht…
# $ name               <chr> "Sapienza University of Rome", "University of Milan", "University of Bologna", "University of Padua", "Uni…
# $ name_alternatives  <list> "Università degli Studi di Roma \"La Sapienza\"", "Statale", NA, "Università di Padova", "UniFI", NA, NA,…
# $ name_acronyms      <list> NA, "UNIMI", "UNIBO", "UNIPD", NA, "UNITO", "UniPi", NA, NA, "UniGe", "UNIPV", NA, NA, NA, NA, NA, NA, "U…
# $ name_international <list> [<data.frame[1 x 84]>], [<data.frame[1 x 59]>], [<data.frame[1 x 99]>], [<data.frame[1 x 78]>], [<data.fr…
# $ ror                <chr> "https://ror.org/02be6w209", "https://ror.org/00wjc7c48", "https://ror.org/01111rn36", "https://ror.org/00…
# $ ids                <list> [<data.frame[6 x 2]>], [<data.frame[6 x 2]>], [<data.frame[6 x 2]>], [<data.frame[6 x 2]>], [<data.frame[…
# $ country            <chr> "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT"…
# $ geo                <list> [<data.frame[1 x 7]>], [<data.frame[1 x 7]>], [<data.frame[1 x 7]>], [<data.frame[1 x 7]>], [<data.frame[…
# $ type               <chr> "education", "education", "education", "education", "education", "education", "education", "education", "e…
# $ homepage           <chr> "http://www.uniroma1.it/", "http://www.unimi.it/ENG/", "http://www.unibo.it/en/homepage", "https://www.uni…
# $ image              <chr> "https://upload.wikimedia.org/wikipedia/en/4/45/Sapienza_University_of_Rome.png", "https://upload.wikimedi…
# $ thumbnail          <chr> "https://upload.wikimedia.org/wikipedia/en/thumb/4/45/Sapienza_University_of_Rome.png/83px-Sapienza_Univer…
# $ associated_inst    <list> [<data.frame[1 x 24]>], [<data.frame[1 x 12]>], [<data.frame[1 x 12]>], [<data.frame[1 x 12]>], [<data.fr…
# $ rel_score          <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
# $ works_count        <int> 170399, 155881, 127161, 124356, 87214, 86939, 78857, 74975, 62800, 59244, 58427, 55810, 48666, 48050, 4484…
# $ TC                 <int> 12295727, 12148721, 10608296, 10126781, 6064970, 7184436, 6238824, 5442312, 2437364, 4395213, 4426608, 426…
# $ TCperYear          <list> [<data.frame[11 x 3]>], [<data.frame[11 x 3]>], [<data.frame[11 x 3]>], [<data.frame[11 x 3]>], [<data.fr…
# $ concept            <list> [<data.frame[13 x 5]>], [<data.frame[10 x 5]>], [<data.frame[12 x 5]>], [<data.frame[14 x 5]>], [<data.fr…
# $ works_api_url      <chr> "https://api.openalex.org/works?filter=institutions.id:I861853513", "https://api.openalex.org/works?filter…
```

&nbsp;

### Get all venues matching a set of filters

We want download all records regarding journals that have published more than 100,000 works:

```{r}
query_venue <- oaQueryBuild(
  entity = "venues",
  filter = "works_count:>100000")

```

We check how many records match the query:

```{r}
res <- oaApiRequest(query_url = query_venue,
                    total.count = TRUE,
                    verbose=TRUE)
res$count

## 41
```

Then we download and convert the collection:

```{r}
res <- oaApiRequest(query_url = query_venue,
                    total.count = FALSE,
                    verbose=TRUE)

df <- oa2df(res, entity="venues")

dplyr::glimpse(df)

# Rows: 41
# Columns: 15
# $ id            <chr> "https://openalex.org/V2751751161", "https://openalex.org/V3121261024", "https://openalex.org/V41354064", "http…
# $ name          <chr> "Social Science Research Network", "Research Papers in Economics", "ChemInform", "Lecture Notes in Computer Sci…
# $ publisher     <chr> NA, NA, "Wiley", NA, "BMJ", "Public Library of Science", "Springer Nature", "Elsevier", NA, "American Associati…
# $ issn          <list> NA, NA, <"1431-5890", "0931-7597", "1522-2667">, <"1611-3349", "0302-9743">, "0959-8138", "1932-6203", <"1476-…
# $ issn_l        <list> NA, NA, "0931-7597", "0302-9743", "0959-8138", "1932-6203", "0028-0836", "0140-6736", "0003-0503", "0036-8075"…
# $ is_oa         <lgl> NA, NA, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, NA, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, NA, TRUE, FALSE, F…
# $ is_in_doaj    <lgl> NA, NA, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, NA, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, NA, TRUE, FALSE, …
# $ ids           <list> [<data.frame[2 x 2]>], [<data.frame[2 x 2]>], [<data.frame[6 x 2]>], [<data.frame[5 x 2]>], [<data.frame[4 x 2…
# $ homepage      <chr> "http://www.ssrn.com/", "http://www.repec.org/", NA, "http://www.springer.com/lncs", "http://www.bmj.com/thebmj…
# $ rel_score     <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
# $ works_count   <int> 876560, 791035, 727829, 492176, 293439, 281416, 268358, 238844, 238230, 221709, 205392, 195400, 194314, 157804,…
# $ TC            <int> 3232676, 2929586, 251764, 5719698, 3508157, 6554607, 19013848, 6953795, 25496, 16332877, 6949261, 14653679, 111…
# $ TCperYear     <list> [<data.frame[11 x 3]>], [<data.frame[11 x 3]>], [<data.frame[10 x 3]>], [<data.frame[11 x 3]>], [<data.frame[1…
# $ concept       <list> [<data.frame[10 x 5]>], [<data.frame[11 x 5]>], [<data.frame[11 x 5]>], [<data.frame[11 x 5]>], [<data.frame[9…
# $ works_api_url <chr> "https://api.openalex.org/works?filter=host_venue.id:V2751751161", "https://api.openalex.org/works?filter=host_…
```

&nbsp;

### Get all concepts matching a set of filters

We want to download the records of all the concepts that concern at least one million works:

```{r}
query_concept <- oaQueryBuild(
  entity = "concepts",
  filter = "works_count:>1000000")
```

We check how many records match the query:

```{r}
res <- oaApiRequest(query_url = query_concept,
                    total.count = TRUE,
                    verbose=TRUE)
res$count

## 112
```

Then we download and convert the collection:

```{r}
res <- oaApiRequest(query_url = query_concept,
                    total.count = FALSE,
                    verbose=TRUE)

df <- oa2df(res, entity="concepts")

dplyr::glimpse(df)

# Rows: 112
# Columns: 17
# $ id                        <chr> "https://openalex.org/C71924100", "https://openalex.org/C41008148", "https://openalex.org/C18559268…
# $ name                      <chr> "Medicine", "Computer science", "Chemistry", "Political science", "Materials science", "Biology", "…
# $ name_international        <list> [<data.frame[1 x 193]>], [<data.frame[1 x 184]>], [<data.frame[1 x 243]>], [<data.frame[1 x 127]>]…
# $ description               <chr> "field of study for diagnosing, treating and preventing disease", "theoretical study of the formal …
# $ description_international <list> [<data.frame[1 x 44]>], [<data.frame[1 x 38]>], [<data.frame[1 x 66]>], [<data.frame[1 x 34]>], [<…
# $ wikidata                  <chr> "https://www.wikidata.org/wiki/Q11190", "https://www.wikidata.org/wiki/Q21198", "https://www.wikida…
# $ level                     <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, …
# $ ids                       <list> [<data.frame[5 x 2]>], [<data.frame[5 x 2]>], [<data.frame[5 x 2]>], [<data.frame[5 x 2]>], [<data…
# $ image                     <chr> "https://upload.wikimedia.org/wikipedia/commons/d/d2/Asklepios.3.jpg", "https://upload.wikimedia.or…
# $ thumbnail                 <chr> "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d2/Asklepios.3.jpg/66px-Asklepios.3.jpg", "…
# $ ancestors                 <list> NA, NA, NA, NA, NA, NA, NA, NA, NA, [<data.frame[1 x 4]>], [<data.frame[2 x 4]>], NA, NA, NA, NA, …
# $ rel_concepts              <list> [<data.frame[51 x 5]>], [<data.frame[93 x 5]>], [<data.frame[75 x 5]>], [<data.frame[30 x 5]>], [<…
# $ rel_score                 <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
# $ works_count               <int> 38212152, 27325984, 20878571, 18779901, 18307811, 14944984, 14915488, 13658404, 12763237, 12194186,…
# $ TC                        <int> 424256175, 230387386, 360713464, 49635741, 215259063, 338109876, 157672135, 13719028, 61920457, 199…
# $ TCperYear                 <list> [<data.frame[11 x 3]>], [<data.frame[11 x 3]>], [<data.frame[11 x 3]>], [<data.frame[11 x 3]>], [<…
# $ works_api_url             <chr> "https://api.openalex.org/works?filter=concepts.id:C71924100", "https://api.openalex.org/works?filt…
```


