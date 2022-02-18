#' Extract fixations
#'
#' @description Extracts fixations from the \code{events} table of the \code{\link{eyelinkRecording}} object.
#' Normally, you don't need to call this function yourself,
#' as it is called during the \code{\link{read_edf}} with default settings
#' (\emph{e.g.}, \code{import_fixations = TRUE}).
#'
#' @param object Either an \code{\link{eyelinkRecording}} object or data.frame with events,
#' i.e., \code{events} slot of the \code{\link{eyelinkRecording}} object.
#'
#' @return Object of the same time as input, i.e., either a \code{\link{eyelinkRecording}} object
#' with an additional \code{fixations} slot or a data.frame with fixations' information. See
#' \code{\link{eyelinkRecording}} for details.
#'
#' @seealso read_edf, eyelinkRecording
#' @export
#'
#' @examples
#' data(gaze)
#'
#' # by passing the recording
#' gaze <- extract_fixations(gaze)
#'
#' # by passing events table
#' fixations <- extract_fixations(gaze$events)
extract_fixations <- function(object) { UseMethod("extract_fixations") }


#' @rdname extract_fixations
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr %>% filter mutate select
extract_fixations.data.frame <- function(object){
  object %>%
    dplyr::filter(.data$type == 'ENDFIX') %>%
    dplyr::mutate(duration = .data$entime - .data$sttime) %>%
    dplyr::select(-c("time", "type", "read", "status", "flags", "input", "buttons", "parsedby", "message"))
}

#' @rdname extract_fixations
#' @export
extract_fixations.eyelinkRecording <- function(object){
  object$fixations <- extract_fixations(object$events)
  object
}

