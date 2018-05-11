#' Title
#'
#' @param events
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate select
extract_saccades <- function(events){
  saccades <- events %>%
    dplyr::filter(type=='ENDSACC') %>%
    dplyr::mutate(duration= entime-sttime) %>%
    dplyr::select(-time, -type, -read, -status, -flags, -input, -buttons, -parsedby, -message)
  return(saccades)
}

extract_blinks <- function(events){
  blinks <- events %>%
    dplyr::filter(type=='ENDBLINK') %>%
    dplyr::mutate(duration= entime-sttime) %>%
    dplyr::select(trial, sttime, entime, sttime_rel, entime_rel, duration, eye)
  return(blinks)
}

extract_fixations <- function(events){
  fixations <- events %>%
    dplyr::filter(type=='ENDFIX') %>%
    dplyr::mutate(duration= entime-sttime) %>%
    dplyr::select(-time, -type, -read, -status, -flags, -input, -buttons, -parsedby, -message)
  return(fixations)
}

extract_variables <- function(events){
  variables <- events %>%
    dplyr::filter(grepl('TRIAL_VAR', message)) %>%
    tidyr::separate(message, c('header', 'assignment'), sep='TRIAL_VAR', remove=FALSE) %>%
    dplyr::mutate(assignment= gsub('=', ' ', assignment)) %>%
    dplyr::mutate(assignment2= sub(' ', "=", trimws(assignment))) %>%
    tidyr::separate(assignment2, c('variable', 'value'), sep='=', remove=FALSE) %>%
    dplyr::mutate(
      variable= trimws(variable),
      value= trimws(value)) %>%
    dplyr::select(trial, sttime, sttime_rel, variable, value)

  return(variables)
}
