#' Extract saccades from recorded events
#'
#' @description Extract saccades from the \code{events} table of the \code{\link{eyelinkRecording}} object.
#' Normally, you don't need to call this function yourself,
#' as it is called during the \code{\link{read_edf}} with default
#' settings (\emph{e.g.}, \code{import_saccades = TRUE}).
#'
#' @param object Either an \code{\link{eyelinkRecording}} object or data.frame with events,
#' i.e., \code{events} slot of the \code{\link{eyelinkRecording}} object.
#'
#' @return Object of the same time as input, i.e., either a \code{\link{eyelinkRecording}} object
#' with an additional \code{saccades} slot or a data.frame with saccades' information. See
#' \code{\link{eyelinkRecording}} for details.
#' @seealso read_edf, eyelinkRecording
#'
#' @export
#'
#' @examples
#' data(gaze)
#'
#' # by passing the recording
#' gaze <- extract_saccades(gaze)
#'
#' # by passing events table
#' saccades <- extract_saccades(gaze$events)
extract_saccades <- function(object) { UseMethod("extract_saccades") }

#' @rdname extract_saccades
#' @export
#' @importFrom dplyr %>% filter mutate select
#' @importFrom rlang .data
extract_saccades.data.frame <- function(object){
  object %>%
    dplyr::filter(.data$type == 'ENDSACC') %>%
    dplyr::mutate(duration = .data$entime - .data$sttime) %>%
    dplyr::select(-c("time", "type", "read", "status", "flags", "input", "buttons", "parsedby", "message"))
}


#' @rdname extract_saccades
#' @export
extract_saccades.eyelinkRecording <- function(object){
  object$saccades <- extract_saccades(object$events)
  object
}

