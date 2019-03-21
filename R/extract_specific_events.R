#' Extract saccades
#'
#' @description Extracts saccades from the events table. Normally, you don't need to call this function yourself,
#' as it is envoked during the \code{\link{read_edf}} with default settings (\emph{e.g.}, \code{import_saccades = TRUE}).
#' @param events An \code{\link[=edfRecording$events]{events}} table of the \code{\link{edfRecording}} object.
#'
#' @return A data.frame with information on \code{\link[=edfRecording$saccades]{saccades}}
#' @seealso read_edf, edfRecording
#' @export
#'
#' @examples
#' # saccades are extracted during the initial read_edf call
#' recording <- read_edf(system.file("extdata", "example.edf", package = "edfR"),
#'                       import_saccades = TRUE)
#'
#' # saccades are extracted during the initial read_edf call by default
#' recording <- read_edf(system.file("extdata", "example.edf", package = "edfR"))
#'
#' # saccades are extracted later
#' recording <- read_edf(system.file("extdata", "example.edf", package = "edfR"),
#'                       import_saccades = FALSE)
#' recording$saccades <- extract_saccades(recording$events)
#' @importFrom dplyr %>%
#' @importFrom dplyr filter mutate select
extract_saccades <- function(events){
  saccades <- events %>%
    dplyr::filter(type=='ENDSACC') %>%
    dplyr::mutate(duration= entime-sttime) %>%
    dplyr::select(-time, -type, -read, -status, -flags, -input, -buttons, -parsedby, -message)
  return(saccades)
}

#' Extract blinks
#'
#' @description Extracts blinks from the events table. Normally, you don't need to call this function yourself,
#' as it is envoked during the \code{\link{read_edf}} with default settings (\emph{e.g.}, \code{import_blinks = TRUE}).
#' @param events An \code{\link[=edfRecording$events]{events}} table of the \code{\link{edfRecording}} object.
#'
#' @return A data.frame with information on \code{\link[=edfRecording$blinks]{blinks}}
#' @seealso read_edf, edfRecording
#' @export
#'
#' @examples
#' # blinks are extracted during the initial read_edf call
#' recording <- read_edf(system.file("extdata", "example.edf", package = "edfR"),
#'                       import_blinks = TRUE)
#'
#' # blinks are extracted during the initial read_edf call by default
#' recording <- read_edf(system.file("extdata", "example.edf", package = "edfR"))
#'
#' # blinks are extracted later
#' recording <- read_edf(system.file("extdata", "example.edf", package = "edfR"),
#'                       import_blinks = FALSE)
#' recording$blinks <- extract_blinks(recording$events)
#' @importFrom dplyr %>%
#' @importFrom dplyr filter mutate select
extract_blinks <- function(events){
  blinks <- events %>%
    dplyr::filter(type=='ENDBLINK') %>%
    dplyr::mutate(duration= entime-sttime) %>%
    dplyr::select(trial, sttime, entime, sttime_rel, entime_rel, duration, eye)
  return(blinks)
}

#' Extract fixations
#'
#' @description Extracts fixations from the events table. Normally, you don't need to call this function yourself,
#' as it is envoked during the \code{\link{read_edf}} with default settings (\emph{e.g.}, \code{import_fixations = TRUE}).
#' @param events An \code{\link[=edfRecording$events]{events}} table of the \code{\link{edfRecording}} object.
#'
#' @return A data.frame with information on \code{\link[=edfRecording$fixations]{fixations}}
#' @seealso read_edf, edfRecording
#' @export
#'
#' @examples
#' # fixations are extracted during the initial read_edf call
#' recording <- read_edf(system.file("extdata", "example.edf", package = "edfR"),
#'                       import_fixations = TRUE)
#'
#' # fixations are extracted during the initial read_edf call by default
#' recording <- read_edf(system.file("extdata", "example.edf", package = "edfR"))
#'
#' # fixations are extracted later
#' recording <- read_edf(system.file("extdata", "example.edf", package = "edfR"),
#'                       import_fixations = FALSE)
#' recording$fixations <- extract_fixations(recording$events)
#' @importFrom dplyr %>%
#' @importFrom dplyr filter mutate select
extract_fixations <- function(events){
  fixations <- events %>%
    dplyr::filter(type=='ENDFIX') %>%
    dplyr::mutate(duration= entime-sttime) %>%
    dplyr::select(-time, -type, -read, -status, -flags, -input, -buttons, -parsedby, -message)
  return(fixations)
}

#' Extract variables
#'
#' @description Extracts variables from the events table. Normally, you don't need to call this function yourself,
#' as it is envoked during the \code{\link{read_edf}} with default settings (\emph{e.g.}, \code{import_variables = TRUE}).
#' @param events An \code{\link[=edfRecording$events]{events}} table of the \code{\link{edfRecording}} object.
#' @param wideFormat Logical, if \code{TRUE} spreads variables into columns turning table into a wide format
#' with a single row per trial. Timing information is dropped. Defaults to \code{TRUE}.
#'
#' @return A data.frame with information on \code{\link[=edfRecording$variables]{variables}}
#' @seealso read_edf, edfRecording
#' @export
#'
#' @examples
#' # variables are extracted during the initial read_edf call
#' recording <- read_edf(system.file("extdata", "example.edf", package = "edfR"),
#'                       import_variables = TRUE)
#'
#' # variables are extracted during the initial read_edf call by default
#' recording <- read_edf(system.file("extdata", "example.edf", package = "edfR"))
#'
#' # variables are extracted later
#' recording <- read_edf(system.file("extdata", "example.edf", package = "edfR"),
#'                       import_variables = FALSE)
#' recording$variables <- extract_variables(recording$events)
#' @importFrom dplyr %>%
#' @importFrom dplyr filter mutate select
extract_variables <- function(events, wideFormat= TRUE){
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

  if (wideFormat){
    variables <- variables %>%
      select(trial, variable, value) %>%
      tidyr::spread(key= variable, value= value)
  }

  variables
}



#' Extract triggers, a custom message type
#'
#' @description Extracts trigger events, messages that adhere to a "TRIGGER label" format.
#' Their purpose is to identify the time instance of specific interest.
#' Please note that due to a non-standard nature of this function \strong{is not} envoked
#' during the \code{\link{read_edf}} call and you need to call it separately.
#' @param events An \code{\link[=edfRecording$events]{events}} table of the \code{\link{edfRecording}} object.
#' @param wideFormat Whether to convert the table to the wide format, so that each trigger
#' is a separate column.  Works well only if same trigger sequence is used in every trial. Possible values
#' \code{"absolute"} (values represent aboslute time), \code{"relative"} (relative to the trial start),
#' or \code{NULL} (narrow format).Defaults to \code{NULL}.
#'
#' @return A data.frame with information on \code{\link[=edfRecording$triggers]{triggers}}
#' @seealso read_edf, edfRecording
#' @export
#'
#' @examples
#' recording <- read_edf(system.file("extdata", "example.edf", package = "edfR"))
#' recording$triggers <- extract_triggers(recording$events)
#' @importFrom dplyr %>%
#' @importFrom dplyr filter mutate select
extract_triggers <- function(events, wideFormat=NULL){
  # Extracts key events: my own custom set of messages, not part of the EDF API!
  # Looks for events coded as 'KEY_EVENT <message>'
  # Returns trial, key event id (<message>), and timing information

  triggers <- events %>%
    dplyr::filter(grepl('^TRIGGER', message)) %>%
    dplyr::mutate(label= trimws(stringr::str_replace(message, 'TRIGGER', ''))) %>%
    dplyr::select(trial, sttime, sttime_rel, label)

  if (wideFormat == "absolute"){
    triggers <- triggers %>%
      select(trial, sttime, label) %>%
      tidyr::spread(key= label, value= sttime)
  }
  else if (wideFormat == "relative"){
    triggers <- triggers %>%
      select(trial, sttime_rel, label) %>%
      tidyr::spread(key= label, value= sttime_rel)
  }

  triggers
}


#' Extracts rectangular areas of interest
#'
#' @description Extracts rectangular areas of interest (AOI), as defined by "!V IAREA RECTANGLE" command.
#' Specifically, we expect it to be in format \code{!V IAREA RECTANGLE <index> <left> <top> <right> <bottom> <label>}, where
#' \code{<label>} is a string label and all other variables are integer.
#'
#' @param events An \code{\link[=edfRecording$events]{events}} table of the \code{\link{edfRecording}} object.
#'
#' @return A data.frame with the list of \code{\link[=edfRecording$AOIs]{AOIs}}
#' @export
#'
#' @examples
#' gaze <- read_edf(system.file("extdata", "example.edf", package = "edfR"))
#' AOIs <- extract_AOIs(gaze$events)
extract_AOIs <- function(events){
  events %>%
  dplyr::filter(stringr::str_detect(message, '^!V IAREA RECTANGLE')) %>%
  tidyr::separate(col= message,
        into= c("Exclamation", "IAREA", "RECTANGLE", "index", "left", "top", "right", "bottom", "label"),
        sep= ' ', remove = FALSE) %>%
  dplyr::select(trial, sttime, sttime_rel, index, label, left, top, right, bottom) %>%
  dplyr::mutate(left= as.numeric(left),
        right= as.numeric(right),
        top= as.numeric(top),
        bottom= as.numeric(bottom),
        index= as.integer(index))
}
