\name{edfRecording$samples}
\alias{edfRecording$samples}
\title{Samples table of the eye data recording class}
\description{Samples table which is a collection of all \code{FSAMPLE} imported from the edf file. Please note that \code{\link{read_edf}} parameters determines which fields are imported. Column descriptions were copied directly  from the \emph{EDF access C API manual}. Please refer to that manual for further details. Suffixes \code{L} and \code{R} denote left and right eye. Non-standard additional fields are marked in bold.
\itemize{
  \item{\bold{trial}} {trial index}
  \item{\code{time}} {time stamp of sample}
  \item{\bold{time_rel}} {time stamp of sample, relative to the start of the trial}
  \item{\code{pxL, pxR, pyL, pyR}} {pupil coordinates}
  \item{\code{hxL, hxR, hyL, hyR}} {headref coordinates}
  \item{\code{paL, paR}} {pupil size or area}
  \item{\code{gxL, gxR, gyL, gyR}} {screen gaze coordinates}
  \item{\code{rx, ry}} {screen pixels per degree.}
  \item{\code{gxvelL, gxvelR, gyvelL, gyvelR}} {gaze velocity}
  \item{\code{hxvelL, hxvelR, hyvelL, hyvelR}} {headref velocity}
  \item{\code{rxvelL, rxvelR, ryvelL, ryvelR}} {raw velocity}
  \item{\code{fgxvelL, fgxvelR, fgyvelL, fgyvelR}} {fast gaze velocity}
  \item{\code{fhxvelL, fhxvelR, fhyvelL, fhyvelR}} {fast headref velocity}
  \item{\code{frxvelL, frxvelR, fryvelL, fryvelR}} {fast raw velocity}
  \item{\code{hdata_1 - hdata_8}} {head-tracker data (not pre-scaled). Each column correspond to a single element of the \code{INT16 FSAMPLE::hdata[8]} }
  \item{\code{flags}} {flags to indicate contents}
  \item{\code{input}} {extra (input word)}
  \item{\code{buttons}} {button state & changes}
  \item{\code{htype}} {head-tracker data type (0=none)}
  \item{\code{errors}} {process error flags}
}
