#' Checks whether EDF API library was present and
#' interface was successfully be compiled
#'
#' @param fail_loudly logical, whether lack of compiled library means
#' error (\code{TRUE}), just warning (\code{FALSE}), or silent (\code{NA},
#' for test use only).
#'
#' @export
#'
#' @examples
#' check_that_compiled(fail_loudly = FALSE)
check_that_compiled <- function(fail_loudly = TRUE){
  if (!eyelinkReader::is_compiled() && !is.na(fail_loudly)) {
    if (fail_loudly) {
      stop("No compiled interface to EDF API. Please see diagnostic messages when package is loading.")
    } else {
      warning("No compiled interface to EDF API, function will return NULL. Please see diagnostic messages when package is loading.")
    }
  }

  eyelinkReader::is_compiled()
}


#' Determine whether library is compiled and can be used
#'
#' @return logical
#' @importFrom methods getPackageName
#' @importFrom rlang env
#' @export
#' @examples
#' is_compiled()
is_compiled <- function(){
  exists('read_edf_file',
         where=paste0('package:', getPackageName(env())),
         mode='function')
}
