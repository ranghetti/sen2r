# Retrieve Travis credentials
tests_apihub <- if (Sys.getenv("GITHUB_ACTIONS") == "true") {c(
  paste0("sen2r_travis_",Sys.getenv("R_VERSION_STRING")),
  Sys.getenv("CI_PASSWORD")
)} else {
  saved_creds <- try(sen2r::read_scihub_login(), silent = TRUE)
}

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
