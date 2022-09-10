#' Convert OpenAlex collection from data frame to bibliometrix object
#'
#' It converts bibliographic collections gathered from OpenAlex database \href{https://openalex.org/}{https://openalex.org/} into a
#' bibliometrix data frame (\href{https://bibliometrix.org/}{https://bibliometrix.org/})
#'
#' @param df is bibliographic collection of works donwloaded from OpenALex.
#' @return a data.frame with class "bibliometrix".
#'
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
#'   identifier = NULL,
#'   entity = "works",
#'   cites = "W2755950973",
#'   from_publication_date = "2021-01-01",
#'   to_publication_date = "2021-12-31",
#'   search = NULL,
#'   endpoint = "https://api.openalex.org/"
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
  df$id_oa <- gsub("https://openalex.org/", "", df$id)
  names(df)[names(df) == "id"] <- "id_url"

  if (substr(df$id_oa[1], 1, 1) != "W") {
    message("oa2bibliometrix accepts only OpenAlex data frames containing 'works' (entity = 'works')")
    return()
  }

  utils::data("countrycode", envir = environment())
  countrycode$Country <- as.character(countrycode$Country)

  # Authors
  AU_info <- lapply(df$author, function(l) {
    AU <- au_collapse(l$au_display_name)
    C1 <- au_collapse(l$au_affiliation_raw)
    RP <- au_collapse(l$au_affiliation_raw[1])
    AU_UN <- au_collapse(l$institution_name)
    l$institution_country[is.na(l$institution_country)] <- "Not available"
    AU_CO <- au_collapse(countrycode[l$institution_country, 1])
    list(AU = AU, RP = RP, C1 = C1, AU_UN = AU_UN, AU_CO = AU_CO)
  })

  AU_info <- do.call(rbind.data.frame, AU_info)

  # References
  df$CR <- unlist(lapply(df$referenced_works, function(l) {
    paste(l, collapse = ";")
  }))

  # Keywords
  ID <- unlist(lapply(df$concepts, function(l) {
    if (is.logical(l)) return(NA)
    au_collapse(l$display_name)
  }))

  df <- cbind(AU_info, ID, df)

  df$TI <- toupper(df$display_name)
  df$AB <- toupper(df$ab)
  df$SO <- toupper(df$so)
  df$DT <- toupper(df$type)
  df$DB <- "openalex"
  df$JI <- gsub("https://openalex.org/", "", df$so_id)
  df$J9 <- df$JI
  df$PY <- df$publication_year
  df$TC <- df$cited_by_count
  df <-  df[!names(df) %in% c("display_name","ab","so","type","publication_year","cited_by_count")]


  ### SR field creation
  suppressWarnings(df <- SR(df))
  d <- duplicated(df$SR)
  if (sum(d) > 0) cat("\nRemoved ", sum(d), "duplicated documents\n")
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
    if (!(length(l) > 0)) l <- "NA"
    return(l)
  }))

  SR <- paste(FirstAuthor, df$PY, df$SO, sep = ", ")

  df$SR_FULL <- gsub("\\s+", " ", SR)

  ## assign an unique name to each document
  SR <- gsub("\\s+", " ", SR)
  st <- i <- 0
  while (st == 0) {
    ind <- which(duplicated(SR))
    if (length(ind) > 0) {
      i <- i + 1
      SR[ind] <- paste0(SR[ind], "-", letters[i], sep = "")
    } else {
      st <- 1
    }
  }
  df$SR <- SR

  return(df)
}

au_collapse <- function(x){
  toupper(paste(x, collapse = ";"))
}
