utils::globalVariables("progress_bar")
#' Convert OpenAlex collection of institutions' records from list format to data frame
#'
#' It converts bibliographic collection of institutions' records gathered from OpenAlex database \href{https://openalex.org/}{https://openalex.org/} into data frame.
#' The function converts a list of institutions' records obtained using \code{oaApiRequest} into a data frame/tibble.
#'
#' @param data is a list. data is the output of the function \code{oaApiRequest}.
#' @param verbose is a logical. If TRUE, information about the querying process will be plotted on screen. Default is \code{verbose=TRUE}.
#'
#' @return a data.frame.
#'
#' For more extensive information about OpenAlex API, please visit: \href{https://docs.openalex.org/api}{https://docs.openalex.org/api}
#'
#'
#' @examples
#' \dontrun{
#'
#' # Query to search information about all Italian educational institutions
#'
#'
#' query_inst <- oaQueryBuild(
#'   entity = "institutions",
#'   filter = "country_code:it,type:education"
#' )
#'
#' res <- oaApiRequest(
#'   query_url = query_inst,
#'   total.count = FALSE,
#'   verbose = FALSE
#' )
#'
#' df <- oa2df(res, entity = "institutions")
#'
#' df
#' }
#'
#' # @export
oaInstitutions2df <- function(data, verbose = TRUE) {

  # replace NULL with NA
  data <- simple_rapply(data, function(x) if (is.null(x)) NA else x)

  if (!is.null(data$id)) {
    data <- list(data)
  }

  if (is.null(data[[1]]$id)) {
    message("the list does not contain a valid OpenAlex collection")
    return()
  }

  n <- length(data)

  list_df <- vector(mode = "list", length = n)

  pb <- progress::progress_bar$new(
    format = "  converting [:bar] :percent eta: :eta",
    total = n, clear = FALSE, width = 60
  )

  for (i in 1:n) {
    if (isTRUE(verbose)) pb$tick()

    item <- data[[i]]

    id <- item$id
    name <- item$display_name
    if (length(item$display_name_alternatives) > 0) {
      name_alternatives <- list(unlist(item$display_name_alternatives))
    } else {
      name_alternatives <- NA
    }
    if (length(item$display_name_acronyms) > 0) {
      name_acronyms <- list(unlist(item$display_name_acronyms))
    } else {
      name_acronyms <- NA
    }
    if (length(item$international) > 0) {
      name_international <- list(as.data.frame(item$international))
    } else {
      name_international <- NA
    }
    #
    ror <- item$ror
    rel_score <- item$relevance_score
    country <- item$country_code
    type <- item$type
    homepage <- item$homepage_url
    image <- item$image_url
    thumbnail <- item$image_thumbnail_url
    works_count <- item$works_count
    TC <- item$cited_by_count
    if (length(item$ids) > 0) {
      ids <- unlist(item$ids)
      ids <- list(data.frame(item = names(ids), value = ids))
    } else {
      ids <- NA
    }
    if (length(item$geo) > 0) {
      geo <- list(as.data.frame(item$geo))
    } else {
      geo <- NA
    }
    if (length(item$associated_institutions) > 0) {
      associated_inst <- list(as.data.frame(item$associated_institutions))
    } else {
      associated_inst <- NA
    }

    # Total Citations per Year
    TCperYear <- unlist(item$counts_by_year)
    lab <- names(TCperYear)
    TCperYear <- list(data.frame(
      year = TCperYear[lab == "year"], works_count = TCperYear[lab == "works_count"],
      TC = TCperYear[lab == "cited_by_count"]
    ))
    # concepts
    concept <- list(do.call(rbind, lapply(item$x_concepts, function(l) {
      L <- data.frame(
        concept_id = l$id,
        concept_name = l$display_name,
        concept_score = l$score,
        concept_lecel = l$level,
        concept_url = l$wikidata
      )
    })))
    works_api_url <- item$works_api_url

    list_df[[i]] <- tibble::tibble(
      id = id, name = name, name_alternatives = name_alternatives, name_acronyms = name_acronyms,
      name_international = name_international, ror = ror, ids = ids, country = country, geo = geo,
      type = type, homepage = homepage, image = image, thumbnail = thumbnail, associated_inst = associated_inst,
      rel_score = rel_score, works_count = works_count, TC = TC, TCperYear = TCperYear,
      concept = concept, works_api_url = works_api_url
    )
  }
  df <- do.call(rbind, list_df)
}
