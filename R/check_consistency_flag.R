#' Checks consistency flag, stops if invalid, returns code if valid.
#'
#' @param consistency consistency check control for the time stamps of the start
#' and end events, etc. Could be \code{'no consistency check'} (0),
#' \code{'check consistency and report'} (1), \code{'check consistency and fix'} (2).
#'
#' @return integer index
#' @export
#'
#' @examples
#' check_consistency_flag('no consistency check')
check_consistency_flag <- function(consistency) {
  # converting consistency to integer constant that C-code understands
  requested_consistency <-  factor(consistency,
                                   levels= c('no consistency check', 'check consistency and report', 'check consistency and fix'))
  if (is.na(requested_consistency)) stop(sprintf('Bad consistency check value "%s".', consistency))

  # zero-based index
   as.numeric(requested_consistency) - 1
}
