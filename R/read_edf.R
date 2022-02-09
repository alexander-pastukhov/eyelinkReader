#' Read EDF file with gaze data recorded by SR Research Eyelink Eyetracker
#'
#' Reads EDF file with gaze data recorded by SR Research Eyelink Eyetracker
#' and returns an \code{\link{edfRecording}} object that contains events, samples,
#' and recordings, as well as specific events such as saccades, fixations, blinks, etc.
#'
#' @param file full name of the EDF file
#' @param consistency consistency check control for the time stamps of the start
#' and end events, etc. Could be \code{'no consistency check'},
#' \code{'check consistency and report'} (default), \code{'check consistency and fix'}.
#' @param import_events logical, whether to import events, defaults to
#' code{TRUE}
#' @param import_recordings logical, whether to import information about start/end of the recording, defaults to
#' code{TRUE}
#' @param import_samples logical, whether to import samples, defaults to \code{FALSE}.
#' Please note that specifying\code{sample_attributes} automatically sets it to \code{TRUE}.
#' @param sample_attributes a character vector that lists sample attributes to be imported.
#' By default, all attributes are imported (default). For the complete list of sample attributes
#' please refer to \code{\link{edfRecording}} or EDF API documentation.
#' @param start_marker event string that marks the beginning of the trial. Defaults to \code{"TRIALID"}.
#' @param end_marker event string that marks the end of the trial. Defaults to \code{"TRIAL OK"}.
#' Please note that an \strong{empty} string \code{''} means that a trial lasts from one \code{start_marker} till the next one.
#' @param import_saccades logical, whether to extract saccade events into a separate table for convinience. Defaults to \code{TRUE}.
#' @param import_blinks logical, wheather to extract blink events into a separate table for convinience. Defaults to \code{TRUE}.
#' @param import_fixations logical, whether to extract fixation events into a separate table for convinience. Defaults to \code{TRUE}.
#' @param import_variables logical, whether to extract stored variables into a separate table for convinience. Defaults to \code{TRUE}.
#' @param verbose logical, whether the number of trials and the progress are shown in the console. Defaults to \code{TRUE}.
#' @param fail_loudly logical, whether lack of compiled library means
#' error (\code{TRUE}, default) or just warning (\code{FALSE}).
#'
#' @return an \code{\link{eyelinkRecording}} object that contains events, samples,
#' and recordings, as well as specific events such as saccades, fixations, blinks, etc.
#' @export
#' @importFrom fs file_exists
#' @examples
#' if (eyelinkReader::is_compiled()) {
#'     # Import only events and recordings information
#'     recording <- read_edf(system.file("extdata", "example.edf", package = "eyelinkReader"))
#'
#'     # Import events and samples (only time and  screen gaze coordinates)
#'     recording <- read_edf(system.file("extdata", "example.edf", package = "eyelinkReader"),
#'                       sample_attributes = c('time', 'gx', 'gy'))
#'
#'     # Import events and samples (all attributes)
#'     recording <- read_edf(system.file("extdata", "example.edf", package = "eyelinkReader"),
#'                           import_samples= TRUE)
#' }
read_edf <- function(file,
                     consistency = 'check consistency and report',
                     import_events = TRUE,
                     import_recordings = TRUE,
                     import_samples = FALSE,
                     sample_attributes = NULL,
                     start_marker = 'TRIALID',
                     end_marker = 'TRIAL OK',
                     import_saccades = TRUE,
                     import_blinks = TRUE,
                     import_fixations = TRUE,
                     import_variables = TRUE,
                     verbose = TRUE,
                     fail_loudly = TRUE){
  # failing with NULL, if no error was forced
  if (!check_that_compiled(fail_loudly)) return(NULL)


  # sanity checks before we pass parameters to C-code
  if (!fs::file_exists(file)) stop("File not found.")
  check_logical_flag(import_events)
  check_logical_flag(import_recordings)
  check_logical_flag(import_saccades)
  check_logical_flag(import_blinks)
  check_logical_flag(import_fixations)
  check_logical_flag(import_variables)
  check_logical_flag(verbose)
  check_string_parameter(start_marker)
  check_string_parameter(end_marker)

  # converting consistency to integer constant that C-code understands
  requested_consistency <- check_consistency_flag(consistency)

  # figuring out which sample attributes to import, if any
  sample_attr_flag <- logical_index_for_sample_attributes(import_samples, sample_attributes)
  import_samples <- sum(sample_attr_flag) > 0

  # importing data
  edf_recording <- read_edf_file(file,
                                 requested_consistency,
                                 import_events,
                                 import_recordings,
                                 import_samples,
                                 sample_attr_flag,
                                 start_marker,
                                 end_marker,
                                 verbose)

  # adding preamble
  edf_recording$preamble <- read_preamble(file)

  # checking display info, if present
  if (!is.null(edf_recording$display_coords)){
    edf_recording$display_coords<- as.numeric(unlist(strsplit(trimws(gsub("DISPLAY_COORDS", "", edf_recording$display_coords)), " ")))
  }

  # converting header to data.frame
  edf_recording$headers <- data.frame(edf_recording$headers)
  edf_recording$headers <- convert_header_codes(edf_recording$headers);

  # replacing -32768 with NA and converting lists to data.frames
  if (import_samples){
    edf_recording$samples <- data.frame(convert_NAs(data.frame(edf_recording$samples)))
  }
  if (import_events){
    edf_recording$events <- data.frame(convert_NAs(data.frame(edf_recording$events)))
  }
  if (import_recordings){
    edf_recording$recordings <- data.frame(convert_NAs(data.frame(edf_recording$recordings)))
    edf_recording$recordings <- convert_recording_codes(edf_recording$recordings)
  }

  if (import_events){
    edf_recording$events$eye <- factor(edf_recording$events$eye, levels= c(0, 1), labels= c('LEFT', 'RIGHT'))
    edf_recording$events$type <- factor(edf_recording$events$type,
                                        levels= c(1, 2, 10, 3, 4, 5, 6, 7, 8, 9, 15, 16, 17, 18, 24, 25, 28, 0x3F),
                                        labels = c('STARTPARSE', 'ENDPARSE', 'BREAKPARSE',
                                                   'STARTBLINK', 'ENDBLINK', 'STARTSACC', 'ENDSACC', 'STARTFIX', 'ENDFIX', 'FIXUPDATE',
                                                   'STARTSAMPLES', 'ENDSAMPLES', 'STARTEVENTS', 'ENDEVENTS',
                                                   'MESSAGEEVENT', 'BUTTONEVENT', 'INPUTEVENT', 'LOST_DATA_EVENT'))
  }


  # extracting specific event types, if requested
  if (import_events){
    if (import_saccades){
      edf_recording$saccades <- extract_saccades(edf_recording$events)
    }
    if (import_blinks){
      edf_recording$blinks <- extract_blinks(edf_recording$events)
    }
    if (import_fixations){
      edf_recording$fixations <- extract_fixations(edf_recording$events)
    }
    if (import_variables){
      edf_recording$variables <- extract_variables(edf_recording$events)
    }
  }

  class(edf_recording) <- 'eyelinkRecording'
  return (edf_recording);
}


