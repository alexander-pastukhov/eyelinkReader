\name{edfRecording$events}
\alias{edfRecording$events}
\title{Events table of the eye data recording class}
\description{Events table which is a collection of all \code{FEVENT} imported from the EDF file. Column descriptions were copied directly  from the \emph{EDF access C API manual}. Please refer to that manual for further details. Additional non-standard fields are marked in bold.
\itemize{
  \item{\strong{trial}} {trial index, starts at 1}
  \item{\code{time}} {effective time of event}
  \item{\code{type}} {event type}
  \item{\code{read}} {flags which items were included}
  \item{\code{sttime}} {start time of the event}
  \item{\code{entime}} {end time of the event}
  \item{\strong{sttime_rel}} {start time of the event, relative to the start time of the trial}
  \item{\strong{entime_rel}} {end time of the event, relative to the start time of the trial}
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
  \item{\code{status}} {error, warning flags}
  \item{\code{flags}} {flags to indicate contents}
  \item{\code{input}} {extra (input word)}
  \item{\code{buttons}} {button state & changes}
  \item{\code{parsedby}} {7 bits of flags: PARSEDBY codes}
  \item{\code{message}} {any message string}
}
