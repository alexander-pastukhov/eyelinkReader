\name{edfRecording$recordings}
\alias{edfRecording$recordings}
\title{Information about the start and end of the recording}
\description{Recordgins table which is a collection of all \code{RECORDING} structures imported from the edf file. Column descriptions were copied directly  from the \emph{EDF access C API manual}. Please refer to that manual for further details. Non-standard additional fields are marked in bold.
  \itemize{
    \item{\bold{trial}} {trial index}
    \item{\code{time}} {start time or end time}
    \item{\code{sample rate}} {sample rate in Hz: 250, 500, 1000 or 2000.}
    \item{\code{eflags}} {extra information about events}
    \item{\code{sflags}} {extra information about samples}
    \item{\code{state}} {\code{'START'} (2) or \code{'END'} (1)}
    \item{\code{record_type}} {\code{'SAMPLES'} (1), \code{'EVENTS'} (2), or \code{'SAMPLES and EVENTS'} (3)}
    \item{\code{pupil_type}} {\code{'AREA'} (0) or \code{'DIAMETER'} (1)}
    \item{\code{recording_mode}} {\code{'PUPIL'} (0) or \code{'CR'} (1)}
    \item{\code{filter_type}} {1, 2, or 3}
    \item{\code{pos_type}} {should be \code{'GAZE'} (0), \code{'HREF'} (1) or \code{'RAW'}, but currently this column is kept as numeric, since observed values do not match the declared constants.}
    \item{\code{eye}} {\code{'LEFT'} (1), \code{'RIGHT'} (2) or \code{'LEFT and RIGHT'} (3)}
  }
}
