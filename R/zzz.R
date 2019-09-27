.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(
    "Welcome to sen2r. To use the package from a GUI, launch",
    " > sen2r()",
    "Documentation: http://sen2r.ranghetti.info",
    if (Sys.info()["sysname"] %in% c("Windows", "Darwin")) {
      paste(
        "\nIMPORTANT: sen2r depends on some external tools;",
        "before using it, it is strongly recommended to run function",
        " > check_sen2r_deps()",
        "to check them and install the missing ones.",
        sep = "\n"
      )
    },
    sep = "\n"
  ))
  
  if (interactive()) {
    permission <- ask_permission()
  } else {
    permission <- FALSE
  }
  
}
