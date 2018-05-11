#' Read EDF file
#'
#' Reads EDF file and returns an \code{\link{edfRecording}} object that contains events, samples,
#' and recordings, as well as specific events such as saccades, fixations, blinks, etc.
#'
#' @param file full name of the EDF file
#' @param consistency consistency check control (for the time stamps of the start
#' and end events, etc). Could be \code{'no consistency check'},
#' \code{'check consistency and report'} (default), \code{'check consistency and fix'}.
#' @param import_events
#' @param import_recordings
#' @param import_samples
#' @param sample_attributes
#' @param start_marker_string
#' @param end_marker_string
#' @param screen
#' @param convert_codes
#' @param import_saccades
#' @param import_blinks
#' @param import_fixations
#' @param import_variables
#'
#' @return
#' @export
#'
#' @examples
read_edf <- function(file,
                     consistency= 'check consistency and report',
                     import_events= TRUE,
                     import_recordings= TRUE,
                     import_samples= FALSE,
                     sample_attributes= NULL,
                     start_marker_string= 'TRIALID',
                     end_marker_string= 'TRIAL OK',
                     screen= list(),
                     convert_codes= TRUE,
                     import_saccades= TRUE,
                     import_blinks= TRUE,
                     import_fixations= TRUE,
                     import_variables= TRUE){

  # converting consistency to integer constant that C-code understands
  requested_consistency <-  factor(consistency, levels= c('no consistency check', 'check consistency and report', 'check consistency and fix'))
  if (is.na(requested_consistency)){
    warning(sprintf('Bad consistency check value "%s", defaulting to "check consistency and report".', consistency))
    requested_consistency <- 1
  }
  else{
    requested_consistency <- as.numeric(requested_consistency) -1
  }

  # storing screen info, if available
  if (length(screen)>0){
    screen$pixels_per_degree <- pixels_per_degree(screen$resolution, screen$size.cm, screen$eye.screen.distance.cm)
    pixels_per_degree<- screen$pixels_per_degree
  }
  else{
    pixels_per_degree <- c(-1, -1)
  }

  # figuring out which sample attributes to import, if any
  sample_attr_labels <- c('time', 'px', 'py', 'hx', 'hy', 'pa', 'gx', 'gy', 'rx', 'ry', 'gxvel', 'gyvel', 'hxvel', 'hyvel', 'rxvel', 'ryvel', 'fgxvel', 'fgyvel', 'fhxvel', 'fhyvel', 'frxvel', 'fryvel', 'hdata', 'flags', 'input', 'buttons', 'htype', 'errors')
  if ((import_samples) || (!is.null(sample_attributes))){

    if (is.null(sample_attributes) ){
      sample_attr_flag <- rep(TRUE, times = length(sample_attr_labels))
    }
    else{
      sample_attr_flag <- rep(FALSE, times = length(sample_attr_labels))
      sample_attr_flag[match(sample_attributes, sample_attr_labels)] <- TRUE
      if (sum(sample_attr_flag)==0){
        warning('import_samples flag is TRUE, but no valid sample attributes were provided. No samples will be imported.');
        import_samples= FALSE;
      }
      else{
        import_samples= TRUE;
      }
    }
  }
  else{
    sample_attr_flag <- rep(FALSE, times = length(sample_attr_labels))
  }

  # importing data
  edf_recording<- read_edf_file(file, requested_consistency, import_events, import_recordings, import_samples, sample_attr_flag, start_marker_string, end_marker_string, pixels_per_degree)

  # adding preamble !!!!!!! REMEMBER TO REPLACE WITH read_preamble, ONCE IT IS READY!
  edf_recording$preamble <- read_preamble_str(file)

  # checking display info, if present
  if (!is.null(edf_recording$display_coords)){
    edf_recording$display_coords<- as.numeric(unlist(strsplit(trimws(gsub("DISPLAY_COORDS", "", edf_recording$display_coords)), " ")))
  }

  # replacing -32768 with NA
  if (import_samples){
    edf_recording$samples <- data.frame(convert_NAs(data.frame(edf_recording$samples)))
  }
  if (import_events){
    edf_recording$events <- data.frame(convert_NAs(data.frame(edf_recording$events)))
  }
  if (import_recordings){
    edf_recording$recordings <- data.frame(convert_NAs(data.frame(edf_recording$recordings)))
  }

  # converting header to data.frame
  edf_recording$headers <- data.frame(edf_recording$headers)
  if (convert_codes){
    edf_recording$headers <- convert_header_codes(edf_recording$headers);

    if (import_recordings){
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

  class(edf_recording) <- 'edfRecording'
  return (edf_recording);
}
