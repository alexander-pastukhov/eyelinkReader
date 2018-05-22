\name{edfRecording$headers}
\alias{edfRecording$headers}
\title{Trial headers table of the eye data recording class}
\description{Trial headers table which is a collection of all \code{TRIAL} structures imported from the edf file. Column descriptions were copied directly  from the \emph{EDF access C API manual}. Please refer to that manual for further details. All fields of the \code{RECORDINGS} structure are prefixed with \code{rec_}. Non-standard additional fields are marked in bold.
  \itemize{
    \item{\bold{trial}} {trial index}
    \item{\code{duration}} {duration of the trial}
    \item{\code{starttime}} {start time of the trial}
    \item{\code{endtime}} {end time of the trial}

    \item{\code{rec_time}} {start time or end time}
    \item{\code{rec_sample rate}} {sample rate in Hz: 250, 500, 1000 or 2000.}
    \item{\code{rec_eflags}} {extra information about events}
    \item{\code{rec_sflags}} {extra information about samples}
    \item{\code{rec_state}} {\code{'START'} (2) or \code{'END'} (1)}
    \item{\code{rec_record_type}} {\code{'SAMPLES'} (1), \code{'EVENTS'} (2), or \code{'SAMPLES and EVENTS'} (3)}
    \item{\code{rec_pupil_type}} {\code{'AREA'} (0) or \code{'DIAMETER'} (1)}
    \item{\code{rec_recording_mode}} {\code{'PUPIL'} (0) or \code{'CR'} (1)}
    \item{\code{rec_filter_type}} {1, 2, or 3}
    \item{\code{rec_pos_type}} {should be \code{'GAZE'} (0), \code{'HREF'} (1) or \code{'RAW'}, but currently this column is kept as numeric, since observed values do not match the declared constants.}
    \item{\code{rec_eye}} {\code{'LEFT'} (1), \code{'RIGHT'} (2) or \code{'LEFT and RIGHT'} (3)}
  }
}
