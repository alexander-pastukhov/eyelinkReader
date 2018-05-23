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
