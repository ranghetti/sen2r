.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(
    "Welcome to SALTO. To use the package from a GUI, launch",
    "    > sto()",
    "Documentation: https://ranghetti.github.io/salto\n",
    "IMPORTANT: SALTO depends on some external tools;",
    "before using it, it is strongly recommended to run function",
    "    > salto::check_dependencies()",
    "to check them and install the missign ones.",
    sep = "\n"))
}
