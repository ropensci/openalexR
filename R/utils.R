
`%||%` <- function(x, y) if (is.null(x)) y else x

replace_w_na <- function(x, y = NA){
  lapply(x, `%||%`, y = y)
}

subs_na <- function(x, type, prefix = NULL) {
  if (length(x) == 0) {
    return(NA)
  }

  if (type == "identical") {
    return(x)
  }

  out <- switch(type,
    row_df = as.data.frame(replace_w_na(x)),
    flat = unlist(x),
    rbind_df = do.call(rbind.data.frame, lapply(x, replace_w_na))
  )

  if (!is.null(prefix)) {
    out <- prepend(out, prefix)
  }

  list(out)
}

prepend <- function(x, prefix = "") {
  names(x) <- paste(prefix, names(x), sep = "_")
  x
}

empty_list <- function(vars) {
  setNames(as.list(rep(NA, length(vars))), vars)
}

isValidEmail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case = TRUE)
}

append_flt <- function(x, pre = "from_publication_date", collapse = "|") {
  if (is.null(x)) {
    return(NULL)
  }

  if (length(x) > 1) x <- paste(x, collapse = collapse)
  paste0(pre, ":", x)
}

id_type <- function(identifier) {
  switch(toupper(substr(identifier, 1, 1)),
    W = "works",
    A = "authors",
    I = "institutions",
    C = "concepts",
    S = "sources",
    P = "publishers",
    `F` = "funders",
    `T` = "topics",
    NA
  )
}

oa_print <- function() {
  p <- as.integer(Sys.getenv("openalexR.print"))
  if (is.na(p)){
    return(NULL)
  }
  p
}

oa_progress <- function(n, text = "converting") {
  progress::progress_bar$new(
    format = paste(" ", text, "[:bar] :percent eta: :eta"),
    total = n, clear = FALSE, width = 60
  )
}

asl <- function(z) {
  if (length(z) > 1) {
    return(z)
  }

  z_low <- tolower(z)
  if (z_low == "true" || z_low == "false") {
    return(z_low)
  } else {
    return(z)
  }
}

shorten_oaid <- function(id) {
  gsub("https://openalex.org/", "", id, fixed = TRUE)
}

shorten_orcid <- function(id) {
  gsub("https://orcid.org/", "", id, fixed = TRUE)
}

rbind_oa_ls <- function(list_df) {
  all_names <- unique(unlist(lapply(list_df, names)))
  do.call(
    rbind.data.frame,
    lapply(
      list_df,
      function(x) {
        tibble::as_tibble(c(x, sapply(
          setdiff(all_names, names(x)),
          function(y) NA
        )))
      }
    )
  )
}

#' Get email from options
#' @return Character string. Email of the requester.
#' @keywords internal
#' @export
oa_email <- function() {
  email <- Sys.getenv("openalexR.mailto")
  if (email == "") {
    email <- getOption("openalexR.mailto", default = NULL)
  }
  email
}

#' Get apikey from options
#' @return Character string. API key of the requester.
#' @keywords internal
#' @export
oa_apikey <- function() {
  apikey <- Sys.getenv("openalexR.apikey")
  if (apikey == "") {
    apikey <- getOption("openalexR.apikey", default = NULL)
  }
  apikey
}


#' Process topics
#'
#' @param entity List. One single work or author to process.
#' @param extra Character. Either "score" (work) or "count" (author).
#'
#' @return List. A list of one tibble with the processed topics.
#' @keywords internal
#'
process_topics <- function(entity, extra) {
  topics <- entity$topics
  if (is.null(topics)) {
    return(NULL)
  }
  topics_ls <- lapply(seq_along(topics), function(i) {
    topic <- topics[[i]]
    relev <- c(
      # Hoist fields for the topic entity
      list(topic = topic[c("id", "display_name")]),
      # Keep info about other entities as-is
      topic[vapply(topic, is.list, logical(1))]
    )
    relev_df <- subs_na(relev, "rbind_df")[[1]]
    relev_df <- tibble::rownames_to_column(relev_df, "name")
    cbind(i = i, topic[extra], relev_df)
  })
  topics_df <- do.call(rbind.data.frame, topics_ls)
  list(topics = list(tibble::as_tibble(topics_df)))
}
