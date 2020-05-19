.onAttach <- function(libname, pkgname) {

  # If sen2r settings dir does not exist, ask for creating it
  invisible(give_write_permission())

  # Welcome message
  packageStartupMessage(paste(
    "Welcome to sen2r. To use the package from a GUI, launch",
    " > sen2r()",
    "Documentation: http://sen2r.ranghetti.info",
    # if (all(
    #   Sys.info()["sysname"] %in% c("Windows", "Darwin"),
    #   length(load_binpaths()) == 0
    # )) {
    #   paste(
    #     "\nIMPORTANT: sen2r depends on some external tools;",
    #     "before using it, it is strongly recommended to run function",
    #     " > check_sen2r_deps()",
    #     "to check them and install the missing ones.",
    #     sep = "\n"
    #   )
    # },
    sep = "\n"
  ))
  
}
