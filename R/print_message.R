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
#' @param type Type of the output (accepted values: 'message'
#'  for a diagnostic message, 'string' for a character output, 'cat' for the
#'  output of [cat()] function, 'error' and 'warning' for an error or
#'  warning message. Intentionally, no default value is defined.
#' @param sep (optional) character used to separate input values
#'  (default is nothing).
#' @param date Logical value: set TRUE to place the date before the message
#'  (useful for logs or time consuming operations); default is FALSE.
#' @param date_format Format of the date (see [strftime()])
#'  for the definition of the format). The default format is
#'  "\%Y-\%m-\%d \%H:\%M:\%S".
#' @return Message (in the defined format).
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0

print_message <- function(..., type, sep="", date=FALSE, date_format="") {
  message_string <- paste(if (date) {
    paste0("[",strftime(Sys.time(), format=date_format),"]")
  }, paste(c(...), collapse=sep))
  switch(
    type,
    message = message(message_string),
    string = message_string,
    cat = cat(message_string,"\n",sep=""),
    error = stop(message_string, call.=FALSE),
    warning = warning(message_string, call.=FALSE),
    waiting = invisible(readline(message_string)),
    stop(paste0("Type '",type,"' not yet supported."))
  )
}
