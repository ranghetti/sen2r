#' @title Print a message
#' @description A common interface for printing messages of several types.
#' @details Several functions print messages in different formats
#'  (message, error, warning, cat, R output) and with different syntaxes
#'  (concatenating parameters or accepting a single argument, appending
#'   a new line, etc.).
#'   This accessory function provides a common interface for different types:
#'   several arguments are accepted and concatenated with the `sep` argument;
#'   the format is defined with the `format` argument; a date is optionally
#'   placed before the message.
#' @param ... `R` objects which are concatenated.
#' @param type Type of the output .Accepted values: 
#'  - 'message' for a diagnostic message;
#'  - 'string' for a character output;
#'  - 'cat' for the output of `cat()` function;
#'  - 'error' and 'warning' for an error or warning message. 
#'  
#'  Intentionally, no default value is defined.
#' @param sep (optional) character used to separate input values
#'  (default is nothing).
#' @param date Logical value: set `TRUE` to place the date before the message
#'  and after the prefix (this is useful for logs or time consuming operations); 
#'  default is FALSE.
#' @param date_format Format of the date (see `strftime()`)
#'  for the definition of the format). The default format is
#'  `'\%Y-\%m-\%d \%H:\%M:\%S'`.
#' @param width Positive integer: target column for wrapping lines in the output
#'  (set to `Inf` for no wrapping).
#' @param indent Non-negative integer: indentation of the first line in a paragraph
#'  It can be also a logical: in this case, if TRUE (default) the value
#'  is optimised in order to align first line with the followings.
#' @param exdent Non-negative integer: indentation of subsequent lines in paragraphs.
#'  It can be also a logical: in this case, if TRUE (default) the value
#'  is optimised in order to align lines with the first line.
#' @param prefix Character: prefix for each line except the first.
#' @param initial Character: prefix for the first line.
#' @return Message (in the defined format).
#'
#' @author Luigi Ranghetti, phD (2020) \email{luigi@@ranghetti.info}
#' @note License: GPL 3.0

print_message <- function(
  ..., 
  type, 
  sep = "", 
  date = FALSE, 
  date_format = "",
  width = 0.9 * getOption("width"),
  indent = TRUE, exdent = TRUE,
  prefix = "", initial = prefix
) {

  if (date == TRUE) {
    initial <- paste0(initial, "[",strftime(Sys.time(), format=date_format),"] ")
  }
  if (all(is.logical(exdent), exdent)) {
    exdent <- max(0, min(
      nchar(initial) - nchar(prefix),
      .1 * width
    ))
  }
  if (all(is.logical(indent), indent)) {
    indent <- max(0, min(
      nchar(prefix) - nchar(initial),
      .1 * width
    ))
  }
  message_string <- strwrap(
    unlist(strsplit(paste(c(...), collapse=sep),"\n")),
    width = width, 
    indent = indent, exdent = exdent, 
    prefix = prefix, initial = initial
  )
  switch(
    type,
    message = message(paste(message_string, collapse = "\n")),
    string = message_string,
    cat = cat(message_string, sep="\n"),
    error = stop(paste0("\n",paste0(message_string, collapse = "\n")), call.=FALSE),
    warning = warning(paste(message_string, collapse = "\n"), call.=FALSE),
    waiting = {cat(message_string, sep="\n"); readline(prompt = "... ")},
    stop(paste0("Type '",type,"' not yet supported."))
  )
}
