onAttach <- function(libname, pkgname) {
  if (!grepl("suppress", Sys.getenv("openalexR.message"), ignore.case = TRUE)) {
    cli::cli_inform(
      c(
        "!" = "openalexR v2.0.0 introduces breaking changes.",
        "i" = "See NEWS.md for details.",
        "i" = "To suppress this message, add {.envvar openalexR.message = suppressed} to your {.file .Renviron} file."
      ),
      class = "packageStartupMessage"
    )
  }
}
