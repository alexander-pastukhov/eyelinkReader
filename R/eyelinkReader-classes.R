#' Class \code{eyelinkRecording}.
#'
#' S3 class containing information imported from an edf-file.
#'
#' @name eyelinkRecording-class
#' @aliases eyelinkRecording
#' @docType class
#'
#' @details
#' See \code{methods(class = "eyelinkRecording")} for an overview of available methods.
#'
#' @slot preamble A preamble of the recording, see also \code{\link{read_preamble}} and \code{\link{eyelinkPreamble}}.
#' @slot events Events table which is a collection of all \code{FEVENT} imported from the EDF file. See description below.
#' @slot samples Samples table  which is a collection of all \code{FSAMPLE} imported from the EDF file. See description below.
#'
#' @section Events:
#' Events table which is a collection of all \code{FEVENT} imported from the EDF file.
#' Column descriptions were copied directly  from the \emph{EDF access C API manual}.
#' Please refer to that manual for further details. Additional non-standard fields are marked in bold.
#'
#' * \code{trial} Trial index, starts at 1.
#' * \code{time} Time of event.
#' * \code{type} Event type.
#' * \code{read} Flags which items were included.
#' * \code{sttime} Start time of the event.
#' * \code{entime} End time of the event.
#' * \strong{\code{sttime_rel}} Start time of the event, relative to the start time of the trial.
#' * \strong{\code{entime_rel}} End time of the event, relative to the start time of the trial.
#' * \code{hstx}, \code{hsty} Head reference starting points.
#' * \code{gstx}, \code{gsty} Gaze starting points.
#' * \code{sta} Pupil size at start.
#' * \code{henx}, \code{heny} Headref ending points.
#' * \code{genx}, \code{geny} Gaze ending points.
#' * \code{ena} Pupil size at end.
#' * \code{havx}, \code{havy} Headref averages.
#' * \code{gavx}, \code{gavy} Gaze averages.
#' * \code{ava} Average pupil size.
#' * \code{avel} Accumulated average velocity.
#' * \code{pvel} Accumulated peak velocity.
#' * \code{svel} Start velocity.
#' * \code{evel} End velocity.
#' * \code{supd_x}, \code{supd_y} Start units-per-degree.
#' * \code{eupd_x}, \code{eupd_y} End units-per-degree.
#' * \code{eye} Either \code{'LEFT'} (1) or \code{'RIGHT'} (2).
#' * \code{status} Error, warning flags.
#' * \code{flags} Flags to indicate contents.
#' * \code{input} Extra (input word).
#' * \code{buttons} Button state and changes.
#' * \code{parsedby} 7 bits of flags, PARSEDBY codes.
#' * \code{message} Any message string.
#'
#' @section Samples
#' @seealso
#'   \code{\link{fit_cumhist}}
NULL
