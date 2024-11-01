#' Extract triggers, a custom message type
#'
#' @description Extracts trigger events, messages that adhere to a
#' \code{<message_prefix> <label>} format. Their purpose is to identify the time
#' instance of specific interest.
#' Please note that due to a non-standard nature of this function \strong{is not} called
#' during the \code{\link{read_edf}} call and you need to call it separately.
#'
#' @param object Either an \code{\link{eyelinkRecording}} object or data.frame with events,
#' i.e., \code{events} slot of the \code{\link{eyelinkRecording}} object.
#' @param message_prefix Beginning of the message string that identifies trigger messages.
#' Defaults to \code{"TRIGGER"}.
#'
#' @return Object of the same time as input, i.e., either a \code{\link{eyelinkRecording}} object
#' with an additional \code{triggers} slot or a data.frame with triggers' information. See
#' \code{\link{eyelinkRecording}} for details.
#'
#' @seealso read_edf, eyelinkRecording
#' @export
#'
#' @examples
#' data(gaze)
#'
#' # by passing the recording
#' gaze <- extract_triggers(gaze)
#'
#' # by passing events table
#' triggers <- extract_triggers(gaze$events)
#'
#' # with an explicit message prefix
#' triggers <- extract_triggers(gaze$events, "TRIGGER")
extract_triggers <- function(object, message_prefix = "TRIGGER") { UseMethod("extract_triggers") }


#' @rdname extract_triggers
#' @export
#' @importFrom dplyr %>% filter mutate select
#' @importFrom rlang .data
extract_triggers.data.frame <- function(object, message_prefix = "TRIGGER"){
  # Extracts key events: my own custom set of messages, not part of the EDF API!
  # Looks for events coded as 'KEY_EVENT <message>'
  # Returns trial, key event id (<message>), and timing information

  object %>%
    dplyr::filter(grepl(paste0('^', message_prefix), message)) %>%
    dplyr::mutate(label = trimws(gsub(message_prefix, '', .data$message))) %>%
    dplyr::select(c("trial", "sttime", "sttime_rel", "label"))
}

#' @rdname extract_triggers
#' @export
extract_triggers.eyelinkRecording <- function(object, message_prefix = "TRIGGER"){
  object$triggers <- extract_triggers(object$events, message_prefix)
  object
}

