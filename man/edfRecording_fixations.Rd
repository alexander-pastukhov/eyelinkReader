\name{edfRecording$fixations}
\alias{edfRecording$fixations}
\title{Fixations extracted from events table}
\description{Fixations extracted from the \code{\link[=edfRecording$events]{events}} table. Column descriptions were copied directly  from the \emph{EDF access C API manual}. Please refer to that manual for further details. Non-standard additional fields are marked in bold.
  \itemize{
    \item{\bold{trial}} {trial index}
    \item{\code{sttime}} {start time of the event}
    \item{\code{entime}} {end time of the event}
    \item{\bold{sttime_rel}} {start time of the event, relative to the trial start time}
    \item{\bold{entime_rel}} {end time of the event, relative to the trial start time}
    \item{\bold{duration}} {fixation duration}
    \item{\code{hstx, hsty}} {headref starting points}
    \item{\code{gstx, gsty}} {gaze starting points}
    \item{\code{sta}} {pupil size at start}
    \item{\code{henx, heny}} {headref ending points}
    \item{\code{genx, geny}} {gaze ending points}
    \item{\code{ena}} {pupil size at end}
    \item{\code{havx, havy}} {headref averages}
    \item{\code{gavx, gavy}} {gaze averages}
    \item{\code{ava}} {average pupil size}
    \item{\code{avel}} {accumulated average velocity}
    \item{\code{pvel}} {accumulated peak velocity}
    \item{\code{svel}} {start velocity}
    \item{\code{evel}} {end velocity}
    \item{\code{supd_x, supd_y}} {start units-per-degree}
    \item{\code{eupd_x, eupd_y}} {end units-per-degree}
    \item{\code{eye}} {\code{'LEFT'} (1) or \code{'RIGHT'} (2)}
  }
}
