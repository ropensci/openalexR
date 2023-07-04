.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Thank you for using openalexR!\n",
    "To acknowledge our work, please cite the package by calling\n",
    "`citation(\"openalexR\")`."
  )
}
