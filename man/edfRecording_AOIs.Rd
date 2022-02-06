\name{edfRecording$AOIs}
\alias{edfRecording$AOIs}
\title{Areas of interest (AOIs) extracted from events table}
\description{Areas of interest (AOIs) extracted from the \code{\link[=edfRecording$events]{events}} table. Assumes that a
\bold{rectangular} AOI was defined using \code{!V IAREA RECTANGLE <index> <left> <top> <right> <bottom> <label>} command, where
\code{<label>} is a string label and all other variables are integer.
  \itemize{
    \item{\code{trial}} {trial index}
    \item{\code{sttime}} {time when AOI was defined}
    \item{\code{sttime_rel}} {time when AOI was defined, relative to the trial start time}
    \item{\code{index}} {integer index of the AOI}
    \item{\code{label}} {string with AOI label. Please note that if the original label contained spaces, label will be the part up to the first space and the error messages will be generated}
    \item{\code{left, top, right, bottom}} {four corners of the AOI rectangle. Y-axis follow bold{Eyelink} convention!}
  }
}
