#' Reads edf-file preamble
#'
#' @description Read the preamble of the EDF file and parses it into an reading-friendly format
#' @param file name of the EDF file
#' @param fail_loudly logical, whether lack of compiled library means
#' error (\code{TRUE}, default) or just warning (\code{FALSE}).
#'
#' @return a character vector but with added class \code{eyelinkPreamble} to simplify printing.
#' @export
#' @importFrom stringr str_split str_remove_all
#'
#' @examples
#' if (eyelinkReader::is_compiled()) {
#'     read_preamble(system.file("extdata", "example.edf", package = "eyelinkReader"))
#' }
read_preamble <- function(file, fail_loudly = TRUE){
  # failing with NULL, if no error was forced
  if (!check_that_compiled(fail_loudly)) return(NULL)

  # getting the preamble as a single string and splitting it by new-line
  preamble <- eyelinkReader::read_preamble_str(file) %>%
    stringr::str_split('\\n', simplify = FALSE)

  # removing leading '** ', cause why would we need them?
  preamble <- stringr::str_remove_all(preamble[[1]], '[\\*]{2} ')

  # dropping any empty strings
  preamble <- preamble[sapply(preamble, stringr::str_length)>0]

  # assigning class name
  class(preamble) <- 'eyelinkPreamble'
  preamble
}


#' @export
print.eyelinkPreamble <- function(x, ...){
  preamble_output <- lapply(x, function(y){cat(y); cat("\n")})
}
