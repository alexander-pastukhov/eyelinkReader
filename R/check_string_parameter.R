#' Checks for validity of a string parameter, stops if not valid
#'
#' @param string_parameter String scalar
#'
#' @return character, value of the string_parameter
#' @export
#'
#' @examples
#' start_marker <- "TRIALID"
#' check_string_parameter(start_marker)
check_string_parameter <- function(string_parameter){
  param_name <- as.character(match.call()[[2]])
  if (!is.character(string_parameter)) stop(sprintf("%s must be a scalar string value", param_name))
  if (length(string_parameter) != 1) stop(sprintf("%s must be a scalar string value", param_name))
  if (nchar(string_parameter) == 0) warning(sprintf("Empty string for %s parameter", param_name))

  string_parameter
}
