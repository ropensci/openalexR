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
#'
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
#' query <- oaQueryBuild(
#' identifier=NULL,
#' entity = "works",
#' filter = "cites:W2755950973",
#' date_from = "2021-01-01",
#' date_to = "2021-12-31",
#' search = NULL,
#' sort="relevance_score:desc",
#' endpoint = "https://api.openalex.org/")
#'
#' res <- oaApiRequest(
#'    query_url = query,
#'    total.count = FALSE,
#'    verbose = FALSE
#'    )
#'
#' df <- oa2df(res, entity="works")
#'
#' M <-  oa2bibliometrix(df)
#'
#' }
#'
#' @export
#'
oa2bibliometrix <- function(df){


    df <- df %>%
      mutate(id_oa = gsub("https://openalex.org/","",.data$id)) %>%
      rename(id_url = .data$id)

    if (substr(df$id_oa[1],1,1)!="W"){
      message("oa2bibliometrix accepts only OpenAlex data frames containing 'works' (entity = 'works')")
      return()
    }

  data("countrycode",envir=environment())
  countrycode$Country <- as.character(countrycode$Country)

  # Authors
  AU_info <- lapply(df$author, function(l){
    AU <- paste(l$au_name, collapse=";")
    C1 <- paste(l$au_affiliation_raw, collapse=";")
    RP <- l$au_affiliation_raw[1]
    AU_UN <- paste(l$institution_name, collapse=";")
    AU_CO <- paste(countrycode[l$institution_country,1], collapse = ";")
    data.frame(AU=AU,RP=RP,C1=C1, AU_UN=AU_UN,AU_CO=AU_CO)
  })

  AU_info <- do.call(rbind, AU_info)

  # References
  df$CR <- unlist(lapply(df$CR, function(l){
    paste(l, collapse=";")
  }))

  # Keywords
  ID <- unlist(lapply(df$concept, function(l){
    paste(l$concept_name, collapse=";")
  }))

  df <- cbind(AU_info, ID, df)

  label <- c("TI","AU","C1","RP","ID","AB","AU_UN","AU_CO", "SO", "DT")

  for (i in label) df[[i]] <- toupper(df[[i]])

  df$DB <- "openalex"

  df$JI <- gsub("https://openalex.org/","",df$SO_ID)

  df$J9 <- df$JI


  ### SR field creation
  suppressWarnings(df <- SR(df))
  d <- duplicated(df$SR)
  if (sum(d)>0) cat("\nRemoved ",sum(d),"duplicated documents\n")
  df <- df[!d,]

  row.names(df) <- df$SR

  class(df) <- c("bibliometrixDB", "data.frame")
  return(df)
}

### SR field
SR <- function(df){
  listAU <- strsplit(as.character(df$AU),";")
  FirstAuthor <- unlist(lapply(listAU, function(l){
    l <- trimws(l[1])
    if (!(length(l)>0)) l <- "NA"
    return(l)
  }))


  SR <- paste(FirstAuthor, df$PY, df$J9, sep=", ")

  df$SR_FULL<- gsub("\\s+", " ", SR)

  ## assign an unique name to each document
  SR<- gsub("\\s+", " ", SR)
  st<-i<-0
  while(st==0){
    ind <- which(duplicated(SR))
    if (length(ind)>0){
      i <- i+1
      SR[ind]=paste0(SR[ind],"-",letters[i],sep="")}else{st <- 1}}
  df$SR<- SR

  return(df)
}
