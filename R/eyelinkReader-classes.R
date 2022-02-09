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
#' @slot headers Headers of the individual trials, see description below.
#' @slot recordings Individual recording start/end information, see description below.
#' @slot display_coords Recorded screen coordinates.
#' @slot saccades Saccades extracted from \code{events}, see description below.
#' @slot fixations Fixations extracted from \code{events}, see description below.
#' @slot blinks Blinks extracted from \code{events}, see description below.
#' @slot variables Recorded variables extracted from \code{events}, see description below.
#'
#' @section Events:
#' Events table which is a collection of all \code{FEVENT} imported from the EDF file.
#' Column descriptions were copied directly  from the \emph{EDF access C API manual}.
#' Please refer to that manual for further details. Additional non-standard fields are marked in bold.
#'
#' * \strong{\code{trial}} Trial index, starts at 1.
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
#' @section Samples:
#' Samples table which is a collection of all \code{FSAMPLE} imported from the EDF file.
#' Please note that \code{\link{read_edf}} parameters determines which fields are imported.
#' Column descriptions were copied directly  from the \emph{EDF access C API manual}.
#' Please refer to that manual for further details. Suffixes \code{L} and \code{R} denote left and right eye.
#' Non-standard additional fields are marked in bold.
#' * \strong{\code{trial}} Trial index, starts at 1.
#' * \code{time} Time of sample.
#' * \strong{\code{time_rel}} Time relative to the start of the trial.
#' * \code{pxL}, \code{pxR}, \code{pyL}, \code{pyR} Pupil coordinates.
#' * \code{hxL}, \code{hxR}, \code{hyL}, \code{hyR} Headref coordinates.
#' * \code{paL}, \code{paR} Pupil size or area.
#' * \code{gxL}, \code{gxR}, \code{gyL}, \code{gyR} Screen gaze coordinates.
#' * \code{rx}, \code{ry} Screen pixels per degree.
#' * \code{gxvelL}, \code{gxvelR}, \code{gyvelL}, \code{gyvelR} Gaze velocity.
#' * \code{hxvelL}, \code{hxvelR}, \code{hyvelL}, \code{hyvelR} Headref velocity.
#' * \code{rxvelL}, \code{rxvelR}, \code{ryvelL}, \code{ryvelR} Raw velocity.
#' * \code{fgxvelL}, \code{fgxvelR}, \code{fgyvelL}, \code{fgyvelR} Fast gaze velocity.
#' * \code{fhxvelL}, \code{fhxvelR}, \code{fhyvelL}, \code{fhyvelR} Fast headref velocity.
#' * \code{frxvelL}, \code{frxvelR}, \code{fryvelL}, \code{fryvelR} Fast raw velocity.
#' * \code{hdata_1} -\code{hdata_8} Head-tracker data (not pre-scaled). Each column correspond to a single element of the \code{INT16 FSAMPLE::hdata[8]}.
#' * \code{flags} Flags to indicate contents.
#' * \code{input} Extra (input word).
#' * \code{buttons} Button state & changes.
#' * \code{htype} Head-tracker data type (0=none).
#' * \code{errors} Process error flags.
#'
#' @section Headers:
#' Trial headers table which is a collection of all \code{TRIAL} structures imported from the EDF file.
#' Column descriptions were copied directly  from the \emph{EDF access C API manual}.
#' Please refer to that manual for further details.
#' All fields of the \code{RECORDINGS} structure are prefixed with \code{rec_}.
#' Non-standard additional fields are marked in bold.
#' * \strong{\code{trial}} Trial index.
#' * \code{duration} Duration of the trial.
#' * \code{starttime} Start time of the trial.
#' * \code{endtime} End time of the trial.
#' * \code{rec_time} Start time or end time.
#' * \code{rec_sample rate} Sample rate in Hz: 250, 500, 1000 or 2000.
#' * \code{rec_eflags} Extra information about events.
#' * \code{rec_sflags} Extra information about samples.
#' * \code{rec_state} \code{'START'} (2) or \code{'END'} (1).
#' * \code{rec_record_type} \code{'SAMPLES'} (1), \code{'EVENTS'} (2), or \code{'SAMPLES and EVENTS'} (3).
#' * \code{rec_pupil_type} \code{'AREA'} (0) or \code{'DIAMETER'} (1).
#' * \code{rec_recording_mode} \code{'PUPIL'} (0) or \code{'CR'} (1).
#' * \code{rec_filter_type} 1, 2, or 3.
#' * \code{rec_pos_type} Should be \code{'GAZE'} (0), \code{'HREF'} (1) or \code{'RAW'}, but currently this column is kept as numeric, since observed values do not match the declared constants.
#' * \code{rec_eye} \code{'LEFT'} (1), \code{'RIGHT'} (2) or \code{'LEFT and RIGHT'} (3).
#'
#' @section Recordings:
#' Recordings table which is a collection of all \code{RECORDING} structures imported from the EDF file.
#' Column descriptions were copied directly  from the \emph{EDF access C API manual}.
#' Please refer to that manual for further details. Non-standard additional fields are marked in bold.
#' * \strong{\code{trial}} Trial index.
#' * \code{time} Start time or end time.
#' * \code{sample rate} Sample rate in Hz: 250, 500, 1000 or 2000.
#' * \code{eflags} Extra information about events.
#' * \code{sflags} Extra information about samples.
#' * \code{state} \code{'START'} (2) or \code{'END'} (1).
#' * \code{record_type} \code{'SAMPLES'} (1), \code{'EVENTS'} (2), or \code{'SAMPLES and EVENTS'} (3).
#' * \code{pupil_type} \code{'AREA'} (0) or \code{'DIAMETER'} (1).
#' * \code{recording_mode} \code{'PUPIL'} (0) or \code{'CR'} (1).
#' * \code{filter_type} 1, 2, or 3.
#' * \code{pos_type} Should be \code{'GAZE'} (0), \code{'HREF'} (1) or \code{'RAW'}, but currently this column is kept as numeric, since observed values do not match the declared constants.
#' * \code{eye} \code{'LEFT'} (1), \code{'RIGHT'} (2) or \code{'LEFT and RIGHT'} (3).
#'
#' @section Saccades and Fixations:
#' Saccades and fixations extracted from the \code{events}, tables have the same structure.
#' Column descriptions were copied directly  from the \emph{EDF access C API manual}.
#' Please refer to that manual for further details. Non-standard additional fields are marked in bold.
#' * \strong{\code{trial}} Trial index.
#' * \code{time} Time of event.
#' * \code{sttime} Start time.
#' * \code{entime} End time.
#' * \strong{\code{sttime_rel}} Start time, relative to the start time of the trial.
#' * \strong{\code{entime_rel}} End time, relative to the start time of the trial.
#' * \strong{\code{duration}} Duration.
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
#'
#' @section Blinks:
#' Blinks extracted from the \code{events} table. Column descriptions were copied directly from the \emph{EDF access C API manual}.
#' Please refer to that manual for further details. Non-standard additional fields are marked in bold.
#' * \strong{\code{trial}} Trial index.
#' * \code{time} Time of event.
#' * \code{sttime} Start time.
#' * \code{entime} End time.
#' * \strong{\code{sttime_rel}} Start time, relative to the start time of the trial.
#' * \strong{\code{entime_rel}} End time, relative to the start time of the trial.
#' * \strong{\code{duration}} Duration.
#' * \code{eye} Either \code{'LEFT'} (1) or \code{'RIGHT'} (2).
#'
#' @section Variables:
#' User recorded variables extracted from message events with a \code{'TRIAL_VAR'} prefix.
#' Original format can be either \code{'TRIAL_VAR <name> <value>'} or \code{'TRIAL_VAR <name>=<value>'}.
#' The \code{<name>} cannot contain spaces or \code{'='} sign.
#' White spaces are trimmed for both \code{<name>} and \code{<value>}.
#' * \code{trial} Trial index.
#' * \code{time} Time of event.
#' * \code{sttime} Start time.
#' * \code{sttime_rel} Start time, relative to the start time of the trial.
#' * \code{variable} Variable name, the \code{<name>} part of the event message.
#' * \code{value} Variable value, the \code{<value>} part of the event message.
#'
#' @seealso
#'   \code{\link{read_edf}}
NULL
