\name{edfRecording$blinks}
\alias{edfRecording$blinks}
\title{Blinks extracted from events table}
\description{Blinks extracted from the \code{\link[=edfRecording$events]{events}} table. Column descriptions were copied directly  from the \emph{EDF access C API manual}. Please refer to that manual for further details. Non-standard additional fields are marked in bold.
  \itemize{
    \item{\bold{trial}} {trial index}
    \item{\code{sttime}} {start time of the event}
    \item{\code{entime}} {end time of the event}
    \item{\bold{sttime_rel}} {start time of the event, relative to the trial start time}
    \item{\bold{entime_rel}} {end time of the event, relative to the trial start time}
    \item{\bold{duration}} {saccade duration}
    \item{\code{eye}}{\code{'LEFT'} (1) or \code{'RIGHT'} (2)}
  }
}
