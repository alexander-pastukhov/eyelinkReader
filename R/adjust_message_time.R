#' Adjusts message time based on embedded text offset
#'
#' Uses text in the message to adjust its time. E.g.,
#' for a message \code{"-50 TARGET_ONSET"} that was sent at \code{105600}
#' the actual onset occurred 50 milliseconds earlier (\code{-50}). The function
#' adjusts the event timing and removes the timing offset information from
#' the message. I.e., the example message becomes \code{"TARGET_ONSET"} and
#' its time become \code{105550}.
#'
#' @param object An \code{\link{eyelinkRecording}} object.
#' @param prefix String with a regular expression that defines the offset.
#' Defaults to \code{"^[-+]?[:digit:]+[:space:]+"} (a string starts with a positive
#' or negative integer offset followed by a white space and the rest of the message).
#'
#' @return An \code{\link{eyelinkRecording}} object.
#' @export
#' @importFrom stringr str_detect str_extract str_replace
#' @importFrom dplyr arrange
#'
#' @examples
#' data(gaze)
#' gaze <- adjust_message_time(gaze)
adjust_message_time <- function(object, prefix) { UseMethod("adjust_message_time") }

#' @rdname adjust_message_time
#' @export
adjust_message_time.eyelinkRecording <- function(object, prefix = "^[-+]?[:digit:]+[:space:]+"){
  # check that events are in the recording at all
  if (!("events" %in% names(gaze))) {
    warning("No events in an eyelinkRecording object, nothing to do.")
    return(object)
  }

  # find messages that need adjusting
  need_adjusting <- which(stringr::str_detect(object$events$message, prefix))

  if (length(need_adjusting) > 0) {
    # get temporal offset as a number
    offset <- as.numeric(str_extract(object$events$message[need_adjusting], prefix))

    # remove offset from the message
    object$events$message[need_adjusting] <- str_replace(object$events$message[need_adjusting], prefix, "")

    # adjust time
    object$events$sttime[need_adjusting] <- object$events$sttime[need_adjusting] + offset
    object$events$sttime_rel[need_adjusting] <- object$events$sttime_rel[need_adjusting] + offset

    # sort events based on adjusted time
    object$events <- dplyr::arrange(object$events, .data$sttime)
  }

  object
}

