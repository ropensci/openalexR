`%||%` <- function(x, y) if (is.null(x)) y else x

replace_w_na <- function(x, y = NA) {
  lapply(x, `%||%`, y = y)
}

subs_na <- function(x, type, prefix = NULL) {
  if (length(x) == 0) {
    return(NA)
  }

  if (type == "identical") {
    return(x)
  }

  out <- switch(
    type,
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

empty_df <- function(
  column_names = c(
    "id",
    "display_name",
    "ror",
    "country_code",
    "type",
    "lineage"
  )
) {
  setNames(
    data.frame(
      lapply(column_names, function(x) character(0)),
      stringsAsFactors = FALSE
    ),
    column_names
  )
}

isValidEmail <- function(x) {
  grepl(
    "\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>",
    as.character(x),
    ignore.case = TRUE
  )
}

append_flt <- function(x, pre = "from_publication_date", collapse = "|") {
  if (is.null(x)) {
    return(NULL)
  }

  if (length(x) > 1) {
    x <- paste(x, collapse = collapse)
  }
  paste0(pre, ":", x)
}

id_type <- function(identifier) {
  switch(
    toupper(substr(identifier, 1, 1)),
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
  if (is.na(p)) {
    return(NULL)
  }
  p
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
        tibble::as_tibble(c(
          x,
          sapply(
            setdiff(all_names, names(x)),
            function(y) NA
          )
        ))
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


#' Build abstract from inverted index
#'
#' @param ab List. Inverted index of abstract.
#' @param build Logical. If TRUE, build the abstract.
#'
#' @return Character string. The abstract of the paper.
#' @keywords internal
abstract_build <- function(ab, build = TRUE) {
  if (is.null(ab) || !build) {
    return(NULL)
  }
  w <- rep(names(ab), lengths(ab))
  ind <- unlist(ab)
  if (is.null(ind)) {
    return("")
  }

  paste(w[order(ind)], collapse = " ", sep = "")
}

#' Process paper authorships
#'
#' @param authorships List. Authorships element of paper.
#'
#' @return List. A list of one dataframe with the processed authors:
#' id, display_name, orcid, author_position, is_corresponding, affiliations, affiliation_raw
#' @keywords internal
process_paper_authors <- function(authorships) {
  if (is.null(authorships)) {
    return(NULL)
  }
  authors_ls <- lapply(authorships, function(l) {
    l_author <- if (length(l$author)) {
      replace_w_na(l$author)
    } else {
      empty_list(names(l$author))
    }

    affiliation_raw <- if (length(l$raw_affiliation_strings)) {
      l$raw_affiliation_strings[[1]]
    } else {
      NA_character_
    }

    affs <- list(
      affiliations = process_affil(l$institutions),
      affiliation_raw = affiliation_raw
    )

    c(l_author, l[c("author_position", "is_corresponding")], affs)
  })

  list(rbind_oa_ls(authors_ls))
}


#' Process affiliations
#'
#' @param l_institution List. Nested elements include
#' id, display_name, ror, country_code, type, lineage
#'
#' @return Dataframe of with the following columns:
#' id, display_name, ror, country_code, type, lineage
#' @keywords internal
process_affil <- function(l_institution) {
  if (!length(l_institution)) {
    return(list(empty_df()))
  }
  l_inst <- lapply(l_institution, function(x) {
    x$lineage <- paste(x$lineage, collapse = ", ")
    x
  })
  subs_na(l_inst, "rbind_df")
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
    relev_df$type <- rownames(relev_df)
    cbind(i = i, topic[extra], relev_df)
  })
  topics_df <- do.call(rbind.data.frame, topics_ls)
  list(topics = list(tibble::as_tibble(topics_df)))
}
