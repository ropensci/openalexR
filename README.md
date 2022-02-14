openalexR
====================================

## An R package to get bibliographic data from OpenAlex. 

*https://github.com/massimoaria/openalexR*

*Latest version: `r packageVersion("openalexR")`, `r Sys.Date()`*

&nbsp; 

**by Massimo Aria**

Full Professor in Social Statistics 

PhD in Computational Statistics

Laboratory and Research Group STAD Statistics, Technology, Data Analysis

Department of Economics and Statistics 

University of Naples Federico II

email aria@unina.it

http://www.massimoaria.com

&nbsp; 

# An R-package to gather bibliographic data from OpenAlex. 

## Introduction 

&nbsp;

    The goal of openalexR is to gather bibliographic metadata about publications, authors, venues, institutions and concepts from OpenAlex using API.
    
OpenAlex is a fully open catalog of the global research system. It's named after the ancient Library of Alexandria.
The OpenAlex dataset describes scholarly entities and how those entities are connected to each other. There are five types of entities:

* **Works** are papers, books, datasets, etc; they cite other works

* **Authors** are people who create works

* **Venues** are journals and repositories that host works

* **Institutions** are universities and other orgs that are affiliated with works (via authors)

* **Concepts** *tag* Works with a topic

(source:  [OpenAlex website](https://CRAN.R-project.org))





## Installation

You can install the developer version of the openalexR from [GitHub](https://github.com) with:

    install.packages("devtools")
    devtools::install_github("massimoaria/openalexR")



You can install the released version of openalexR from [CRAN](https://CRAN.R-project.org) with:


    install.packages("openalexR")



&nbsp; 

&nbsp; 


## Load the package

```{r}
   library(openalexR)
```

   

&nbsp; 

&nbsp; 


# Some Brief Examples

## Get full records through entity IDs.

### Query to obtain all information about a single publications

The following paper:

    Aria, M., & Cuccurullo, C. (2017). bibliometrix: An R-tool for comprehensive science mapping analysis. Journal of informetrics, 11(4), 959-975.

is associated to the OpenAlex-id W2755950973.

The function **oaQueryBuild** supports the query creation by providing a set of arguments.

In this example, we need to pass a single argument to the function, that is, the identifier of the entity to download: identifier = "W2755950973".

```{r}
query_work <- oaQueryBuild(
  identifier = "W2755950973",
  entity = "works"
  )

cat(query_work)
```

## https://api.openalex.org/works/W2755950973

As results, **oaQueryBuild** returns the query string including the OpenAlex endpoint API server address. You should change it by using the argument "endpoint = *address*"

The function **oaApiRequest** downloads the bibliographic records matching the query.


```{r}
 res <- oaApiRequest(
    query_url = query_work
    )
```

```{r}
cat("\nID ", res[["id"]])
cat("\nDOI ",res[["doi"]])
cat("\nPublication Title ", res[["title"]])
cat("\nFirst Author's name ",res$authorships[[1]][["author"]][["display_name"]])
cat("\nFirst Author's affiliation ",res$authorships[[1]][["institutions"]][[1]][["display_name"]])
cat("\nSecond Author's name ", res$authorships[[2]][["author"]][["display_name"]])

## ID  https://openalex.org/W2755950973
## DOI  https://doi.org/10.1016/j.joi.2017.08.007
## Publication Title  bibliometrix: An R-tool for comprehensive science mapping analysis
## First Author's name  Massimo Aria
## First Author's affiliation  University of Naples Federico II
## Second Author's name  Corrado Cuccurullo
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
cat("\nID ", res[["id"]])
cat("\nDOI ",res[["doi"]])
cat("\nPublication Title ", res[["title"]])

## ID  https://openalex.org/W2755950973
## DOI  https://doi.org/10.1016/j.joi.2017.08.007
## Publication Title  bibliometrix: An R-tool for comprehensive science mapping analysis
```




### Query to obtain all information about a two o more publications

To download the records of two o more identifiers through a single query, we can recursively apply oaApiRequest to each id using the function lapply.

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
cat("\n Author's Name ", res_author[["display_name"]])
cat("\n Author's ORCID ",res_author[["orcid"]])
cat("\n Author's Total Citations ", res_author[["cited_by_count"]])
cat("\n Author's Publication Count ",res_author[["works_count"]])

## Author's Name  Massimo Aria
## Author's ORCID  https://orcid.org/0000-0002-8517-9411
## Author's Total Citations  2525
## Author's Publication Count  102
```


&nbsp;

&nbsp;

## Get all works matching a set of inclusion/exclusion criteria (filters) 

In most cases, we are interested in downloading a collection of items that meet one or more inclusion/exclusion criteria (filters).

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

    *"all works containing the string "bibliometric analysis" OR "science mapping" in the publication title AND cited more than 50 times"*.



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

## 19
```

Then, we can download the collection:

```{r}
 res <- oaApiRequest(
    query_url = query,
    total.count = FALSE,
    verbose = FALSE
    )
```

and transform it into a data frame:

```{r}
df <- oaWorks2df(res)

## from list to data frame converting [=======] 100% eta:  0s
```

```{r}
dplyr::glimpse(df)

# Rows: 19
# Columns: 21
# $ id            <chr> "https://openalex.org/W3011866596", "https://openalex.org/W2…
# $ TI            <chr> "A Bibliometric Analysis of COVID-19 Research Activity: A Ca…
# $ author        <list> [<data.frame[7 x 10]>], [<data.frame[3 x 10]>], [<data.fram…
# $ AB            <chr> "BACKGROUND: The novel coronavirus disease 2019 (COVID-19) h…
# $ pubdata       <chr> "2020-03-21", "2020-03-01", "2020-09-01", "2020-03-01", "202…
# $ relscore      <dbl> 139.28250, 138.93117, 134.48438, 131.31546, 120.48048, 119.8…
# $ SO            <chr> "Cureus", "Journal of Business Research", "Journal of Busine…
# $ SO_ID         <chr> "https://openalex.org/V2738950867", "https://openalex.org/V9…
# $ PU            <chr> "Cureus, Inc.", "Elsevier", "Elsevier", "Eur Rev Med Pharmac…
# $ IS            <list> "2168-8184", <"1873-7978", "0148-2963">, <"1873-7978", "014…
# $ URL           <chr> "https://doi.org/10.7759/cureus.7357", "https://doi.org/10.1…
# $ OA            <lgl> TRUE, FALSE, NA, NA, TRUE, NA, FALSE, TRUE, FALSE, TRUE, TRU…
# $ TC            <int> 136, 119, 169, 86, 94, 100, 66, 82, 66, 65, 56, 67, 63, 58, …
# $ PY            <int> 2020, 2020, 2020, 2020, 2021, 2020, 2020, 2020, 2021, 2020, …
# $ cited_by_url  <chr> "https://api.openalex.org/works?filter=cites:W3011866596", "…
# $ ids           <list> <"https://doi.org/10.7759/cureus.7357", "3011866596", "http…
# $ DI            <chr> "https://doi.org/10.7759/cureus.7357", "https://doi.org/10.1…
# $ DT            <chr> "journal-article", "journal-article", "journal-article", "jo…
# $ CR            <list> <"https://openalex.org/W3008827533", "https://openalex.org/…
# $ related_works <list> <"https://openalex.org/W3001118548", "https://openalex.org/…
# $ concept       <list> [<data.frame[9 x 5]>], [<data.frame[8 x 5]>], [<data.frame[…
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
