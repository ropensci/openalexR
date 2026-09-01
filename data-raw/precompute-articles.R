# Re-render the pkgdown articles in vignettes/articles/ from their .Rmd.orig
# sources.
#
# The generated .Rmd files are committed. This keeps the docs build reproducible
# and, critically, means the r-universe docs job (which builds
# https://docs.ropensci.org/openalexR and cannot hold per-package secrets, so it
# has no OpenAlex API key) never touches the OpenAlex API. A rate-limited
# (HTTP 429) build previously made oa_fetch() return NULL and broke the whole
# site. See ropensci/openalexR#368.
#
# Usage, from the package root:
#   Rscript data-raw/precompute-articles.R                # all articles
#   Rscript data-raw/precompute-articles.R institution    # just one
#
# Requires openalexR installed, plus: knitr dplyr tidyr purrr ggplot2 ragg
#                                     coro rentrez rrapply

stopifnot("run me from the package root" = file.exists("DESCRIPTION"))

art_dir <- "vignettes/articles"

## ---- credentials -----------------------------------------------------------
## Bail out loudly rather than baking rate-limited garbage into the articles.
key <- openalexR::oa_apikey()

if (is.null(key) || !nzchar(key)) {
  stop(
    "`openalexR.apikey` is not set.\n",
    "OpenAlex has required an API key since February 2026; without one the ",
    "requests will fail, oa_fetch() will return NULL, and the rendered ",
    "articles will be wrong.\n",
    "Set Sys.setenv(openalexR.apikey = ...) or run the ",
    "`precompute-articles` GitHub Action instead.",
    call. = FALSE
  )
}

## ---- deterministic, colour-free, non-interactive rendering -----------------
Sys.setenv(NO_COLOR = "1")
options(
  cli.num_colors = 1L, # no raw ANSI escapes in the baked .Rmd
  cli.dynamic = FALSE, # no in-place spinner redraws
  cli.unicode = TRUE, # keep the nice "i" / tick glyphs
  cli.progress_show_after = Inf, # never emit a progress bar
  width = 80L, # pkgdown's default `code.width`
  useFancyQuotes = FALSE
)

## ---- which articles --------------------------------------------------------
origs <- sort(list.files(
  art_dir,
  pattern = "\\.Rmd\\.orig$",
  full.names = TRUE
))
sel <- commandArgs(trailingOnly = TRUE)
if (length(sel)) {
  origs <- origs[sub("\\.Rmd\\.orig$", "", basename(origs)) %in% sel]
}
stopifnot("no .Rmd.orig files matched" = length(origs) > 0)

## ---- knit ------------------------------------------------------------------
knit_one <- function(orig) {
  name <- sub("\\.Rmd\\.orig$", "", basename(orig))
  out <- file.path(art_dir, paste0(name, ".Rmd"))
  message("\n=== ", basename(orig), " ===")

  # Drop this article's stale figures so renamed/removed chunks do not leave
  # orphans behind in git.
  unlink(Sys.glob(file.path(art_dir, "figure", paste0(name, "-*"))))

  # knitr writes `fig.path` relative to the working directory and puts that same
  # literal string in the image link, so we must knit from inside `art_dir` for
  # pkgdown to resolve the figures.
  wd <- setwd(art_dir)
  on.exit(setwd(wd), add = TRUE)

  # knit() snapshots and restores opts_chunk, so this must be inside the loop.
  # `error = FALSE` matters: knit()'s default is TRUE, which would happily bake
  # a rate-limit error message into the article instead of failing the run.
  knitr::opts_chunk$set(
    error = FALSE,
    fig.path = file.path("figure", paste0(name, "-")),
    dev = "ragg_png", # matches pkgdown's ragg::agg_png
    dpi = 200,
    fig.width = 7,
    fig.asp = 1 / 1.618,
    out.width = "100%"
  )
  knitr::knit(basename(orig), output = basename(out), quiet = FALSE)

  setwd(wd)
  on.exit()
  out
}

## ---- sanity-check the rendered output --------------------------------------
verify <- function(out) {
  txt <- readLines(out, warn = FALSE)
  bad <- grep(
    paste(
      "HTTP status 429",
      "Too Many Requests",
      "HTTP status 5",
      "^#>\\s*Error",
      "^##\\s*Error",
      "OpenAlex API request failed",
      sep = "|"
    ),
    txt,
    value = TRUE
  )
  if (length(bad)) {
    stop(
      sprintf(
        "%s looks rate-limited or errored; refusing to commit it:\n  %s",
        out,
        paste(utils::head(bad, 5L), collapse = "\n  ")
      ),
      call. = FALSE
    )
  }
  if (any(grepl("^\\s*```\\{", txt))) {
    stop(
      out,
      " still contains executable chunks -- knit() did not run?",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

for (o in origs) {
  verify(knit_one(o))
}

message("\nDone. Review with:  git diff --stat -- ", art_dir)
