% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oa2df.R
\name{oa2df}
\alias{oa2df}
\title{Convert OpenAlex collection from list to data frame}
\usage{
oa2df(
  data,
  entity,
  options = NULL,
  count_only = FALSE,
  group_by = NULL,
  abstract = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{data}{List. Output of \code{oa_request}.}

\item{entity}{Character. Scholarly entity of the search.
The argument can be one of
c("works", "authors", "institutions", "keywords", "funders", "sources", "publishers", "topics").}

\item{options}{List. Additional parameters to add in the query. For example:

- `select` Character vector. Top-level fields to show in output.
Defaults to NULL, which returns all fields.
https://docs.openalex.org/how-to-use-the-api/get-single-entities/select-fields

- `sort` Character. Attribute to sort by.
For example: "display_name" for sources or "cited_by_count:desc" for works.
See more at <https://docs.openalex.org/how-to-use-the-api/get-lists-of-entities/sort-entity-lists>.

- `sample` Integer. Number of (random) records to return.
Should be no larger than 10,000.
Defaults to NULL, which returns all records satisfying the query.
Read more at <https://docs.openalex.org/how-to-use-the-api/get-lists-of-entities/sample-entity-lists>.

- `seed` Integer.
A seed value in order to retrieve the same set of random records in
the same order when used multiple times with `sample`.
IMPORTANT NOTE: Depending on your query, random results with a seed value may change over time due to new records coming into OpenAlex.
This argument is likely only useful when queries happen close together (within a day).}

\item{count_only}{Logical.
If TRUE, the function returns only the number of item matching the query.
Defaults to FALSE.}

\item{group_by}{Character. Attribute to group by.
For example: "oa_status" for works.
See more at <https://docs.openalex.org/how-to-use-the-api/get-groups-of-entities>.}

\item{abstract}{Logical. If TRUE, the function returns also the abstract of each item.
Ignored if entity is different from "works". Defaults to TRUE.}

\item{verbose}{Logical.
If TRUE, print information about the dataframe conversion process.
Defaults to TRUE.}
}
\value{
A tibble/dataframe result of the original OpenAlex result list.
}
\description{
It converts bibliographic collections gathered from OpenAlex database \href{https://openalex.org/}{https://openalex.org/} into data frame.
The function converts a collection of records about works, authors, institutions, venues or keywords obtained using \code{oa_request} into a data frame/tibble.
}
\examples{
\dontrun{

# Query to search all works citing the article:
#  Aria, M., & Cuccurullo, C. (2017). bibliometrix:
#   An R-tool for comprehensive science mapping analysis.
#   Journal of informetrics, 11(4), 959-975.

#  published in 2021.
#  The paper is associated to the OpenAlex id W2755950973.

#  Results have to be sorted by relevance score in a descending order.

query <- oa_query(
  entity = "works",
  cites = "W2755950973",
  from_publication_date = "2021-01-01",
  to_publication_date = "2021-04-30"
)

res <- oa_request(
  query_url = query,
  count_only = FALSE,
  verbose = FALSE
)

oa2df(res, entity = "works")
}

}
