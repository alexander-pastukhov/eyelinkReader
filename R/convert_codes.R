#' Converts integer constants in trial headers to factor with explicit labels
#'
#' @description Converts integer constants in trial headers to factor with explicit labels.
#' Please refer to EDF API manual for further details.
#' @param trial_headers data.frame that contains trial headers.
#'
#' @return a modified trial_headers table
#' @keywords internal
convert_header_codes <- function(trial_headers){
  trial_headers$rec_state <- factor(trial_headers$rec_state, levels = c(0, 1), labels= c('END', 'START'))
  trial_headers$rec_record_type <- factor(trial_headers$rec_record_type, levels = c(1, 2, 3), labels= c('SAMPLES', 'EVENTS', 'SAMPLES and EVENTS'))
  trial_headers$rec_pupil_type <- factor(trial_headers$rec_pupil_type, levels = c(0, 1), labels= c('AREA', 'DIAMETER'))
  trial_headers$rec_recording_mode <- factor(trial_headers$rec_recording_mode, levels = c(0, 1), labels= c('PUPIL', 'CR'))
  # trial_headers$rec_pos_type <- factor(trial_headers$rec_pos_type, levels = c(0, 1, 2), labels= c('GAZE', 'HREF', 'RAW'))
  trial_headers$rec_eye <- factor(trial_headers$rec_eye, levels = c(1, 2, 3), labels= c('LEFT', 'RIGHT', 'LEFT and RIGHT'))

  return (trial_headers);
}

#' Converts integer constants in recordings to factor with explicit labels
#'
#' @description Converts integer constants in trial recordings information to factor with explicit labels.
#' Please refer to EDF API manual for further details.
#' @param trial_recordings data.frame that contains trial recordings.
#'
#' @return a modified trial_recordings table
#' @keywords internal
convert_recording_codes <- function(trial_recordings){
  trial_recordingsstate <- factor(trial_recordings$state, levels = c(0, 1), labels= c('END', 'START'))
  trial_recordings$record_type <- factor(trial_recordings$record_type, levels = c(1, 2, 3), labels= c('SAMPLES', 'EVENTS', 'SAMPLES and EVENTS'))
  trial_recordings$pupil_type <- factor(trial_recordings$pupil_type, levels = c(0, 1), labels= c('AREA', 'DIAMETER'))
  trial_recordings$recording_mode <- factor(trial_recordings$recording_mode, levels = c(0, 1), labels= c('PUPIL', 'CR'))
  # trial_recordings$pos_type <- factor(trial_recordings$pos_type, levels = c(0, 1, 2), labels= c('GAZE', 'HREF', 'RAW'))
  trial_recordings$eye <- factor(trial_recordings$eye, levels = c(1, 2, 3), labels= c('LEFT', 'RIGHT', 'LEFT and RIGHT'))

  return (trial_recordings);
}
