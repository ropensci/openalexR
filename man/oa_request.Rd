% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oa_fetch.R
\name{oa_request}
\alias{oa_request}
\title{Get bibliographic records from OpenAlex database}
\usage{
oa_request(
  query_url,
  per_page = 200,
  paging = "cursor",
  pages = NULL,
  count_only = FALSE,
  mailto = oa_email(),
  api_key = oa_apikey(),
  parse = TRUE,
  verbose = FALSE
)
}
\arguments{
\item{query_url}{Character string.
A search query formulated using the OpenAlex API language and
can be generated with \code{oa_query}.}

\item{per_page}{Numeric. Number of items to download per page.
The per-page argument can assume any number between 1 and 200.
Defaults to 200.}

\item{paging}{Character.
Either "cursor" for cursor paging or "page" for basic paging.
When used with `options$sample` and or `pages`,
paging is also automatically set to basic paging: `paging = "page"`
to avoid duplicates and get the right page.
See https://docs.openalex.org/how-to-use-the-api/get-lists-of-entities/paging.}

\item{pages}{Integer vector.
The range of pages to return. If NULL, return all pages.}

\item{count_only}{Logical.
If TRUE, the function returns only the number of item matching the query.
Defaults to FALSE.}

\item{mailto}{Character string.
Gives OpenAlex an email to enter the polite pool.}

\item{api_key}{Character string.
Your OpenAlex Premium API key, if available.}

\item{parse}{Logical.
If FALSE, returns the raw JSON response as string.}

\item{verbose}{Logical.
If TRUE, print information about the querying process. Defaults to TRUE.}
}
\value{
a data.frame or a list of bibliographic records.

For more extensive information about OpenAlex API, please visit:
<https://docs.openalex.org>
}
\description{
`oa_request` makes a request and downloads bibliographic records from
OpenAlex database \href{https://openalex.org/}{https://openalex.org/}.
The function \code{oa_request} queries OpenAlex database using a query
formulated through the function \code{oa_query}.
}
\examples{
\dontrun{

### EXAMPLE 1: Full record about an entity.

# Query to obtain all information about a particular work/author/institution/etc.:

#  The following paper is associated to the OpenAlex-id W2755950973.

#  Aria, M., & Cuccurullo, C. (2017). bibliometrix:
#   An R-tool for comprehensive science mapping analysis.
#   Journal of informetrics, 11(4), 959-975.

res <- oa_request(query_url = "https://api.openalex.org/works/W2755950973")

#  The author Massimo Aria is associated to the OpenAlex-id A5069892096.

query_author <- oa_query(
  identifier = "A5069892096",
  entity = "authors"
)
query_author
res <- oa_request(
  query_url = query_author,
  count_only = FALSE,
  verbose = FALSE
)

### EXAMPLE 2: all works citing a particular work.

# Query to search all works citing the article:
#  Aria, M., & Cuccurullo, C. (2017). bibliometrix:
#   An R-tool for comprehensive science mapping analysis.
#   Journal of informetrics, 11(4), 959-975.

#  published in 2021.
#  The paper is associated to the OpenAlex id W2755950973.

#  Results have to be sorted by relevance score in a descending order.

query2 <- oa_query(
  identifier = NULL,
  entity = "works",
  cites = "W2755950973",
  from_publication_date = "2021-12-01",
  to_publication_date = "2021-12-31",
  search = NULL,
  endpoint = "https://api.openalex.org"
)

res2 <- oa_request(
  query_url = query2,
  count_only = FALSE,
  verbose = FALSE
)

### EXAMPLE 3: All works matching a string in their title

# Query to search all works containing the exact string
# "bibliometric analysis" OR "science mapping" in the title, published in 2020 or 2021.

# Results have to be sorted by relevance score in a descending order.


query3 <- oa_query(
  entity = "works",
  title.search = c("bibliometric analysis", "science mapping"),
  from_publication_date = "2021-12-01",
  to_publication_date = "2021-12-31"
)

res3 <- oa_request(
  query_url = query3,
  count_only = FALSE,
  verbose = FALSE
)

### EXAMPLE 4: How to check how many works match a query
# Query to search all works containing the exact string
# "bibliometric analysis" OR "science mapping" in the title, published in 2020 or 2021.

# Query only to know how many works could be retrieved (count_only=TRUE)

query4 <- oa_query(
  entity = "works",
  title.search = c("bibliometric analysis", "science mapping"),
  from_publication_date = "2020-01-01",
  to_publication_date = "2021-12-31"
)

res4 <- oa_request(
  query_url = query4,
  count_only = TRUE,
  verbose = FALSE
)

res4$count # number of items retrieved by our query
}
}
