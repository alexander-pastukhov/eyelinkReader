#' Checks for validity of a logical flag, stops if not valid
#'
#' @param logical_flag Logical scalar
#'
#' @return logical, value of the logical_flag
#' @export
#'
#' @examples
#' import_samples <- TRUE
#' check_logical_flag(import_samples)
check_logical_flag <- function(logical_flag){
  param_name <- as.character(match.call()[[2]])
  if (!is.logical(logical_flag)) stop(sprintf("%s must be a scalar logical value", param_name))
  if (length(logical_flag) != 1) stop(sprintf("%s must be a scalar logical value", param_name))

  logical_flag
}
