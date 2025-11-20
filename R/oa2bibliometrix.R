#' Convert OpenAlex collection from data frame to bibliometrix object
#'
#' It converts bibliographic collections gathered from OpenAlex database \href{https://openalex.org/}{https://openalex.org/} into a
#' bibliometrix data frame (\href{https://bibliometrix.org/}{https://bibliometrix.org/})
#' Column names follow https://images.webofknowledge.com/images/help/WOS/hs_wos_fieldtags.html.
#'
#' @param df is bibliographic collection of works downloaded from OpenAlex.
#' @return a data.frame with class "bibliometrix".
#' @details Use \code{bibliometrix::convert2df()} (bibliometrix R package) instead.
#'
#' @examples
#' \dontrun{
#'
#' # Query to search all works citing the article:
#' #  Aria, M., & Cuccurullo, C. (2017). bibliometrix:
#' #   An R-tool for comprehensive science mapping analysis.
#' #   Journal of informetrics, 11(4), 959-975.
#'
#' #  published in 2021.
#' #  The paper is associated to the OpenAlex id W2755950973.
#'
#' #  Results have to be sorted by relevance score in a descending order.
#'
#' query <- oa_query(
#'   entity = "works",
#'   cites = "W2755950973",
#'   from_publication_date = "2021-10-01",
#'   to_publication_date = "2021-12-31"
#' )
#'
#' res <- oa_request(
#'   query_url = query,
#'   count_only = FALSE,
#'   verbose = FALSE
#' )
#'
#' df <- oa2df(res, entity = "works")
#'
#' M <- oa2bibliometrix(df)
#' }
#'
#' @export
#'
oa2bibliometrix <- function(df) {
  .Deprecated(
    msg = "oa2bibliometrix() is deprecated. Please use bibliometrix::convert2df() instead."
  )

  df$id_oa <- shorten_oaid(df$id)
  names(df)[names(df) == "id"] <- "id_url"

  if (substr(df$id_oa[1], 1, 1) != "W") {
    warning(
      "oa2bibliometrix accepts only OpenAlex data frames containing 'works' (entity = 'works')"
    )
    return()
  }

  countrycode <- getExportedValue("openalexR", "countrycode")
  countrycode$Country <- as.character(countrycode$Country)

  # Authors
  AU_info <- lapply(df$authorships[7], function(l) {
    if (length(l) == 0 || (length(l) == 1 && is.na(l))) {
      return(empty_list(
        c("AU", "RP", "C1", "AU_UN", "AU_CO")
      ))
    } else {
      AU <- au_collapse(l$display_name)
      C1 <- au_collapse(l$affiliation_raw)
      RP <- au_collapse(l$affiliation_raw[1])
      AU_UN <- au_collapse(lapply(l$affiliations, function(x) x$display_name))
      AU_CO <- au_collapse(countrycode[
        unlist(lapply(l$affiliations, function(x) x$country_code)),
        1
      ])
      list(AU = AU, RP = RP, C1 = C1, AU_UN = AU_UN, AU_CO = AU_CO)
    }
  })
  AU_info <- do.call(rbind.data.frame, AU_info)

  # References
  df$CR <- unlist(lapply(df$referenced_works, function(l) {
    paste(shorten_oaid(l), collapse = ";")
  }))

  # Keywords
  ID <- unlist(lapply(df$concepts, function(l) {
    if (is.logical(l)) {
      return(NA)
    }

    au_collapse(l$display_name[l$score > 0])
  }))

  df <- cbind(AU_info, ID, df)

  df$TI <- toupper(df$display_name)
  df$AB <- toupper(df$abstract)
  df$SO <- toupper(df$source_display_name)
  df$DT <- toupper(df$type)
  df$DB <- "OPENALEX"
  df$JI <- shorten_oaid(df$source_id)
  df$J9 <- df$JI
  df$PY <- df$publication_year
  df$TC <- df$cited_by_count
  df$DI <- gsub("https://doi.org/", "", df$doi)
  df <- df[
    !names(df) %in%
      c(
        "display_name",
        "ab",
        "so",
        "type",
        "publication_year",
        "cited_by_count"
      )
  ]

  ### SR field creation
  suppressWarnings(df <- SR(df))
  d <- duplicated(df$SR)
  if (sum(d) > 0) {
    cat("\nRemoved ", sum(d), "duplicated documents\n")
  }
  df <- df[!d, ]

  row.names(df) <- df$SR
  class(df) <- c("bibliometrixDB", "data.frame")
  return(df)
}

### SR field
SR <- function(df) {
  listAU <- strsplit(as.character(df$AU), ";")
  FirstAuthor <- unlist(lapply(listAU, function(l) {
    l <- trimws(l[1])
    if (!(length(l) > 0)) {
      l <- "NA"
    }
    return(l)
  }))

  SR <- gsub("\\s+", " ", paste(FirstAuthor, df$PY, df$SO, sep = ", "))
  df$SR_FULL <- SR

  ## assign an unique name to each document
  i <- 0
  dup <- duplicated(SR)
  while (any(dup)) {
    i <- i + 1
    SR[dup] <- paste(SR[dup], letters[i], sep = "-")
    dup <- duplicated(SR)
  }
  df$SR <- SR

  return(df)
}

au_collapse <- function(x) {
  toupper(paste(x, collapse = ";"))
}
