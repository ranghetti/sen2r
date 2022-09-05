if (full_tests) {

# Retrieve Travis credentials
tests_apihub <- try(sen2r::read_scihub_login(), silent = TRUE)

if (!inherits(tests_apihub, "try-error")) {
  
  # Use a temporary apihub file
  write_scihub_login(
    tests_apihub[1], tests_apihub[2],
    tests_apihub_path <- tempfile(),
    check = FALSE
  )
  
  # Return message
  message("Tests are run with SciHub user \"",tests_apihub[1],"\".")
  
} else {
  message("Problems with SciHub credentials; check secrets.")
}

}