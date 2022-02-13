#' Extract blinks
#'
#' @description Extracts blinks from the \code{events} table. Normally,
#' you don't need to call this function yourself,
#' as it is called during the \code{\link{read_edf}} with default settings
#' (\emph{e.g.}, \code{import_blinks = TRUE}).
#'
#' @param object Either an \code{\link{eyelinkRecording}} object or data.frame with events,
#' i.e., \code{events} slot of the \code{\link{eyelinkRecording}} object.
#'
#' @return Object of the same time as input, i.e., either a \code{\link{eyelinkRecording}} object
#' with an additional \code{blinks} slot or a data.frame with blinks' information. See
#' \code{\link{eyelinkRecording}} for details.
#'
#' @seealso read_edf, eyelinkRecording
#' @export
#' @examples
#' if (eyelinkReader::is_compiled()) {
#'     # blinks are extracted during the initial read_edf call
#'     recording <- read_edf(system.file("extdata", "example.edf", package = "eyelinkReader"),
#'                           import_blinks = TRUE)
#'
#'     # blinks are extracted during the initial read_edf call by default
#'     recording <- read_edf(system.file("extdata", "example.edf", package = "eyelinkReader"))
#'
#'     # blinks are extracted later
#'     recording <- read_edf(system.file("extdata", "example.edf", package = "eyelinkReader"),
#'                           import_blinks = FALSE)
#'
#'     # by passing the recording
#'     recording <- extract_blinks(recording)
#'
#'     # by passing events table
#'     recording$blinks <- extract_blinks(recording$events)
#' }
extract_blinks <- function(object) { UseMethod("extract_blinks") }


#' @rdname extract_blinks
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr %>% filter mutate select
extract_blinks.data.frame <- function(object){
  object %>%
    dplyr::filter(.data$type == 'ENDBLINK') %>%
    dplyr::mutate(duration = .data$entime - .data$sttime) %>%
    dplyr::select(c("trial", "sttime", "entime", "sttime_rel", "entime_rel", "duration", "eye"))
}

#' @rdname extract_blinks
#' @export
extract_blinks.eyelinkRecording <- function(object){
  object$blinks <- extract_blinks(object$events)
  object
}
