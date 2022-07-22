#' Checks whether EDF API library was present and
#' interface was successfully be compiled
#'
#' @param fail_loudly logical, whether lack of compiled library means
#' error (\code{TRUE}), just warning (\code{FALSE}), or silent (\code{NA},
#' for test use only).
#'
#' @export
#' @keywords internal
#'
#' @examples
#' check_that_compiled(fail_loudly = FALSE)
check_that_compiled <- function(fail_loudly = TRUE){
  # try to compile
  if (!eyelinkReader::compiled_library_status() && !is.na(fail_loudly)) {
    if (fail_loudly) {
      stop("Failed to compile interface to EDF API, function will return NULL. Please read the manual for further details.")
    } else {
      warning("Failed to compile interface to EDF API, function will return NULL. Please read the manual for further details.")
    }
  }

  eyelinkReader::compiled_library_status()
}
