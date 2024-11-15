.onAttach <- function(libname, pkgname) {
  if (!grepl("suppress", Sys.getenv("openalexR.message"), ignore.case = TRUE)){
    packageStartupMessage(
      "openalexR v2.0.0 introduces breaking changes.\n",
      "See NEWS.md for details."
    )
  }
}
