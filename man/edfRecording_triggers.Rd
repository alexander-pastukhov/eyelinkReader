\name{edfRecording$triggers}
\alias{edfRecording$triggers}
\title{Triggers extracted from events table}
\description{Trigger events, messages that adhere to a \code{TRIGGER <label>} format. This is a \bold{non-standard message} that the author uses to mark temporal events like onsets or offsets, similar to those used in M/EEG.
  \itemize{
    \item{\code{trial}} {trial index}
    \item{\code{sttime}} {time of the trigger}
    \item{\code{sttime_rel}} {time of the trigger, relative to the trial start time}
    \item{\code{label}} {<label> part of the message, can contain white spaces}
  }
}
