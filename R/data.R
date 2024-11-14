#' Concepts and abbreviations.
#'
#' 0-level concepts and corresponding abbreviations.
#' Reference: https://www.ncbi.nlm.nih.gov/nlmcatalog/journals/
#'
#' @format A data frame with 19 observations and 3 variables:
#' \code{id}, \code{display_name}, and \code{abbreviation}.
#'
"concept_abbrev"

#' Index of Countries and their alpha-2 and aplha-3 codes.
#'
#' Data frame contains the list of countries and their alpha-2 and
#' aplha-3 codes.\cr
#'
#' @format A data frame with 250 rows and 3 variables:
#'  \describe{
#'     \item{Country}{country names}
#'     \item{Alpha2}{countries' alpha-2 codes}
#'     \item{Alpha3}{countries' alpha-3 codes}
#'     }
#'
"countrycode"

#' Coverage of OpenAlex entity fields after converting to data frame.
#'
#' List with 8 elements associated with 8 OpenAlex entities.
#'
#' @format Each element is a dataframe with 3 columns
#'  \describe{
#'     \item{original}{original field name from OpenAlex}
#'     \item{oa2df}{new column name in output dataframe from oa2df}
#'     \item{comment}{additional notes}
#'     }
#'
"oa2df_coverage"
