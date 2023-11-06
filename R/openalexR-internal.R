.onAttach <- function(libname, pkgname) {
  if (!grepl("suppress", Sys.getenv("openalexR.message"), ignore.case = TRUE)){
    packageStartupMessage(
      "Thank you for using openalexR!\n",
      "To acknowledge our work, please cite the package by calling ",
      "`citation(\"openalexR\")`.\n",
      "To suppress this message, add `openalexR.message = suppressed` ",
      "to your .Renviron file."
    )
  }
}
