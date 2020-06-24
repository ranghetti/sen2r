# Retrieve Travis credentials
tests_apihub <- if (FALSE) {c( #Sys.getenv("TRAVIS") == "true") {c( #FIXME when creds will be active
  paste0("sen2r_travis_",Sys.getenv("TRAVIS_R_VERSION_STRING")),
  Sys.getenv("TRAVIS_PASSWORD")
)} else {
  saved_creds <- try(sen2r::read_scihub_login(), silent = TRUE)
  if (inherits(saved_creds, "try-error")) {
    c("user", "user")
  } else {
    saved_creds[1,]
  }
}

# Use a temporary apihub file
write_scihub_login(tests_apihub[1], tests_apihub[2], tests_apihub_path <- tempfile())

# Return message
message("Tests are run with SciHub user \"",tests_apihub[1],"\".")
