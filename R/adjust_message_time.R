#' Adjusts message time based on embedded text offset
#'
#' Uses text in the message to adjust its time. E.g.,
#' for a message \code{"-50 TARGET_ONSET"} that was sent at \code{105600}
#' the actual onset occurred 50 milliseconds earlier (\code{-50}). The function
#' adjusts the event timing and removes the timing offset information from
#' the message. I.e., the example message becomes \code{"TARGET_ONSET"} and
#' its time become \code{105550}.
#'
#' @param object An \code{\link{eyelinkRecording}} object or data.frame with events,
#' i.e., \code{events} slot of the \code{\link{eyelinkRecording}} object.
#' @param prefix String with a regular expression that defines the offset.
#' Defaults to \code{"^[-+]?[:digit:]+[:space:]+"} (a string starts with a positive
#' or negative integer offset followed by a white space and the rest of the message).
#'
#' @return Object of the same time as input, i.e., either a \code{\link{eyelinkRecording}} object
#' with \emph{modified} \code{events} slot or a data.frame with offset-adjusted events.
#' @export
#'
#' @examples
#' data(gaze)
#'
#' # by passing events table
#' adjusted_events <- adjust_message_time(gaze$events)
#'
#' # by passing the recording
#' gaze <- adjust_message_time(gaze)

adjust_message_time <- function(object, prefix) { UseMethod("adjust_message_time") }

#' @rdname adjust_message_time
#' @importFrom stringr str_detect str_extract str_replace
#' @importFrom dplyr arrange
#' @export
adjust_message_time.data.frame <- function(object, prefix = "^[-+]?[:digit:]+[:space:]+"){
  # find messages that need adjusting
  need_adjusting <- which(stringr::str_detect(object$message, prefix))

  if (length(need_adjusting) > 0) {
    # get temporal offset as a number
    offset <- as.numeric(str_extract(object$message[need_adjusting], prefix))

    # remove offset from the message
    object$message[need_adjusting] <- stringr::str_replace(object$message[need_adjusting], prefix, "")

    # adjust time
    object$sttime[need_adjusting] <- object$sttime[need_adjusting] + offset
    object$sttime_rel[need_adjusting] <- object$sttime_rel[need_adjusting] + offset

    # sort events based on adjusted time
    object <- dplyr::arrange(object, .data$sttime)
  }

  object
}

#' @rdname adjust_message_time
#' @export
adjust_message_time.eyelinkRecording <-  function(object, prefix = "^[-+]?[:digit:]+[:space:]+"){
  # check that events are in the recording at all
  if (!("events" %in% names(object))) {
    warning("No events in an eyelinkRecording object, nothing to do.")
    return(object)
  }

  # modify in place
  object$events <- adjust_message_time(object$events, prefix)
  object
}
