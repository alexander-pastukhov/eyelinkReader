#' Reads edf-file preamble
#'
#' @description Read the preamble of the EDF file and parses it into an reading-friendly format
#' @param file name of the EDF file
#'
#' @return an object of class "edfR_preamble"
#' @export
#'
#' @examples
#'  read_preamble(system.file("extdata", "example.edf", package = "edfR"))
read_preamble <- function(file){
  # getting the preamble as a single string and splitting it by new-line
  preamble <- read_preamble_str(file) %>%
    stringr::str_split('\\n', simplify = FALSE)

  # removing leading '** ', cause why would we need them?
  preamble <- preamble[[1]] %>%
    stringr::str_remove_all('[\\*]{2} ')

  # dropping any empty strings
  preamble <- preamble[sapply(preamble, stringr::str_length)>0]

  # assigning class name
  class(preamble) <- 'edfR_preamble'
  preamble
}


#' @export
print.edfR_preamble <- function(x, ...){
  preamble_output <- lapply(x, function(y){cat(y); cat("\n")})
}
