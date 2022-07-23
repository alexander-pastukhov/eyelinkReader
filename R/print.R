#' Print info about \code{\link{eyelinkRecording}}
#'
#' @param x \code{\link{eyelinkRecording}} object
#' @param ... Addition parameters (unused)
#' @export
#' @examples
#' \donttest{
#'   if (eyelinkReader::compiled_library_status()) {
#'     recording <- read_edf(system.file("extdata", "example.edf", package = "eyelinkReader"))
#'     print(recording)
#'   }
#' }
print.eyelinkRecording <- function(x, ...){
  if (nrow(x$headers)==1){
    trialsN <- 'one trial'
  }
  else{
    trialsN <- sprintf('%d trials', nrow(x$headers))
  }

  if ('events' %in% names(x)){
    if ('samples' %in% names(x))
    {
      cat(sprintf('%d events and %d samples in %s.\n', nrow(x$events), nrow(x$samples), trialsN))
    }
    else{
      cat(sprintf('%d events in %s.\n', nrow(x$events), trialsN))
    }
  }
  else{
    if ('samples' %in% names(x))
    {
      cat(sprintf('%d samples in %s.\n', nrow(x$samples), trialsN))
    }
    else{
      cat(sprintf('%s. Neither events nor samples were imported.', trialsN))
    }
  }

  cat('Preamble:\n')
  print(x$preamble)
}
