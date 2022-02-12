#' Extract saccades
#'
#' @description Extracts saccades from the events table. Normally, you don't need to call this function yourself,
#' as it is called during the \code{\link{read_edf}} with default settings (\emph{e.g.}, \code{import_saccades = TRUE}).
#'
#' @param events An \code{\link[=eyelinkRecording]{events}} table of the \code{\link{eyelinkRecording}} object.
#'
#' @return A data.frame with information on \code{\link[=eyelinkRecording]{saccades}}
#' @seealso read_edf, eyelinkRecording
#'
#' @export
#' @importFrom dplyr %>% filter mutate select
#' @importFrom rlang .data
#'
#' @examples
#' if (eyelinkReader::is_compiled()) {
#'     # saccades are extracted during the initial read_edf call
#'     recording <- read_edf(system.file("extdata", "example.edf", package = "eyelinkReader"),
#'                           import_saccades = TRUE)
#'
#'     # saccades are extracted during the initial read_edf call by default
#'     recording <- read_edf(system.file("extdata", "example.edf", package = "eyelinkReader"))
#'
#'     # saccades are extracted later
#'     recording <- read_edf(system.file("extdata", "example.edf", package = "eyelinkReader"),
#'                           import_saccades = FALSE)
#'     recording$saccades <- extract_saccades(recording$events)
#' }
extract_saccades <- function(events){
  events %>%
    dplyr::filter(.data$type == 'ENDSACC') %>%
    dplyr::mutate(duration = .data$entime - .data$sttime) %>%
    dplyr::select(-c("time", "type", "read", "status", "flags", "input", "buttons", "parsedby", "message"))
}

#' Extract blinks
#'
#' @description Extracts blinks from the events table. Normally, you don't need to call this function yourself,
#' as it is called during the \code{\link{read_edf}} with default settings (\emph{e.g.}, \code{import_blinks = TRUE}).
#' @param events An \code{\link[=eyelinkRecording]{events}} table of the \code{\link{eyelinkRecording}} object.
#'
#' @return A data.frame with information on \code{\link[=eyelinkRecording]{blinks}}
#' @seealso read_edf, eyelinkRecording
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr %>% filter mutate select
#'
#' @examples
#' if (eyelinkReader::is_compiled()) {
#'     # blinks are extracted during the initial read_edf call
#'     recording <- read_edf(system.file("extdata", "example.edf", package = "eyelinkReader"),
#'                           import_blinks = TRUE)
#'
#'     # blinks are extracted during the initial read_edf call by default
#'     recording <- read_edf(system.file("extdata", "example.edf", package = "eyelinkReader"))
#'
#'     # blinks are extracted later
#'     recording <- read_edf(system.file("extdata", "example.edf", package = "eyelinkReader"),
#'                           import_blinks = FALSE)
#'     recording$blinks <- extract_blinks(recording$events)
#' }
extract_blinks <- function(events){
  events %>%
    dplyr::filter(.data$type == 'ENDBLINK') %>%
    dplyr::mutate(duration = .data$entime - .data$sttime) %>%
    dplyr::select(c("trial", "sttime", "entime", "sttime_rel", "entime_rel", "duration", "eye"))
}

#' Extract fixations
#'
#' @description Extracts fixations from the events table. Normally, you don't need to call this function yourself,
#' as it is called during the \code{\link{read_edf}} with default settings (\emph{e.g.}, \code{import_fixations = TRUE}).
#' @param events An \code{\link[=eyelinkRecording]{events}} table of the \code{\link{eyelinkRecording}} object.
#'
#' @return A data.frame with information on \code{\link[=eyelinkRecording]{fixations}}
#' @seealso read_edf, eyelinkRecording
#' @importFrom rlang .data
#' @importFrom dplyr %>% filter mutate select
#' @export
#'
#' @examples
#' if (eyelinkReader::is_compiled()) {
#'     # fixations are extracted during the initial read_edf call
#'     recording <- read_edf(system.file("extdata", "example.edf", package = "eyelinkReader"),
#'                           import_fixations = TRUE)
#'
#'     # fixations are extracted during the initial read_edf call by default
#'     recording <- read_edf(system.file("extdata", "example.edf", package = "eyelinkReader"))
#'
#'     # fixations are extracted later
#'     recording <- read_edf(system.file("extdata", "example.edf", package = "eyelinkReader"),
#'                           import_fixations = FALSE)
#'     recording$fixations <- extract_fixations(recording$events)
#' }
extract_fixations <- function(events){
  events %>%
    dplyr::filter(.data$type == 'ENDFIX') %>%
    dplyr::mutate(duration = .data$entime - .data$sttime) %>%
    dplyr::select(-c("time", "type", "read", "status", "flags", "input", "buttons", "parsedby", "message"))
}

#' Extract variables
#'
#' @description Extracts variables from the events table. Normally, you don't need to call this function yourself,
#' as it is called during the \code{\link{read_edf}} with default settings (\emph{e.g.}, \code{import_variables = TRUE}).
#' @param events An \code{\link[=eyelinkRecording]{events}} table of the \code{\link{eyelinkRecording}} object.
#'
#' @return A data.frame with information on \code{\link[=eyelinkRecording]{variables}}
#' @seealso read_edf, eyelinkRecording
#' @export
#' @importFrom dplyr %>% filter mutate select
#'
#' @examples
#' if (eyelinkReader::is_compiled()) {
#'     # variables are extracted during the initial read_edf call
#'     recording <- read_edf(system.file("extdata", "example.edf", package = "eyelinkReader"),
#'                           import_variables = TRUE)
#'
#'     # variables are extracted during the initial read_edf call by default
#'     recording <- read_edf(system.file("extdata", "example.edf", package = "eyelinkReader"))
#'
#'     # variables are extracted later
#'     recording <- read_edf(system.file("extdata", "example.edf", package = "eyelinkReader"),
#'                           import_variables = FALSE)
#'    recording$variables <- extract_variables(recording$events)
#' }
extract_variables <- function(events){
  events %>%
    dplyr::filter(grepl('TRIAL_VAR', message)) %>%
    tidyr::separate(.data$message, c('header', 'assignment'), sep='TRIAL_VAR', remove=FALSE) %>%
    dplyr::mutate(assignment = gsub('=', ' ', .data$assignment)) %>%
    dplyr::mutate(assignment2 = sub(' ', "=", trimws(.data$assignment))) %>%
    tidyr::separate(.data$assignment2, c('variable', 'value'), sep='=', remove=FALSE) %>%
    dplyr::mutate(variable = trimws(.data$variable),
                  value = trimws(.data$value)) %>%
    dplyr::select(c("trial", "sttime", "sttime_rel", "variable", "value"))
}


#' Extract triggers, a custom message type
#'
#' @description Extracts trigger events, messages that adhere to a "TRIGGER label" format.
#' Their purpose is to identify the time instance of specific interest.
#' Please note that due to a non-standard nature of this function \strong{is not} called
#' during the \code{\link{read_edf}} call and you need to call it separately.
#' @param events An \code{\link[=eyelinkRecording]{events}} table of the \code{\link{eyelinkRecording}} object.
#'
#' @return A data.frame with information on \code{\link[=eyelinkRecording]{triggers}}
#' @seealso read_edf, eyelinkRecording
#' @export
#' @importFrom dplyr %>% filter mutate select
#' @importFrom rlang .data
#'
#' @examples
#' if (eyelinkReader::is_compiled()) {
#'     recording <- read_edf(system.file("extdata", "example.edf", package = "eyelinkReader"))
#'     recording$triggers <- extract_triggers(recording$events)
#' }
extract_triggers <- function(events){
  # Extracts key events: my own custom set of messages, not part of the EDF API!
  # Looks for events coded as 'KEY_EVENT <message>'
  # Returns trial, key event id (<message>), and timing information

  events %>%
    dplyr::filter(grepl('^TRIGGER', message)) %>%
    dplyr::mutate(label = trimws(gsub('TRIGGER', '', .data$message))) %>%
    dplyr::select(c("trial", "sttime", "sttime_rel", "label"))
}


#' Extracts rectangular areas of interest
#'
#' @description Extracts rectangular areas of interest (AOI), as defined by "!V IAREA RECTANGLE" command.
#' Specifically, we expect it to be in format \code{!V IAREA RECTANGLE <index> <left> <top> <right> <bottom> <label>},
#' where \code{<label>} is a string label and all other variables are integer.
#'
#' @param events An \code{events} table of the \code{\link{eyelinkfRecording}} object.
#'
#' @return A data.frame with the list of \code{\link{eyelinkfRecording}}
#' @export
#' @importFrom dplyr %>% filter select mutate
#' @importFrom tidyr separate
#' @importFrom stringr str_detect
#'
#' @examples
#' gaze <- data('example')
#' gaze$AOIs <- extract_AOIs(gaze$events)
extract_AOIs <- function(events){
  events %>%
    dplyr::filter(stringr::str_detect(.data$message, '^!V IAREA RECTANGLE')) %>%
    tidyr::separate(col = .data$message,
             into = c("Exclamation", "IAREA", "RECTANGLE", "index", "left", "top", "right", "bottom", "label"),
             sep = ' ',
             remove = FALSE) %>%
    dplyr::select(c("trial", "sttime", "sttime_rel", "index", "label", "left", "top", "right", "bottom")) %>%
    dplyr::mutate(left= as.numeric(.data$left),
                  right= as.numeric(.data$right),
                  top= as.numeric(.data$top),
                  bottom= as.numeric(.data$bottom),
                  index= as.integer(.data$index))
}
