#' Extract variables
#'
#' @description Extracts variables from the \code{events} table of the \code{\link{eyelinkRecording}} object.
#' Normally, you don't need to call this function yourself,
#' as it is called during the \code{\link{read_edf}} with default settings
#' (\emph{e.g.}, \code{import_variables = TRUE}).
#'
#' @param object Either an \code{\link{eyelinkRecording}} object or data.frame with events,
#' i.e., \code{events} slot of the \code{\link{eyelinkRecording}} object.
#'
#' @return Object of the same time as input, i.e., either a \code{\link{eyelinkRecording}} object
#' with an additional \code{variables} slot or a data.frame with variables' information. See
#' \code{\link{eyelinkRecording}} for details.
#' @seealso read_edf, eyelinkRecording
#' @export
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
#'
#'     # by passing the recording
#'     recording <- extract_variables(recording)
#'
#'     # by passing events table
#'     variables <- extract_variables(recording$events)
#' }
extract_variables <- function(object) { UseMethod("extract_variables") }


#' @rdname extract_variables
#' @export
#' @importFrom dplyr %>% filter mutate select
#' @importFrom tidyr separate
extract_variables.data.frame <- function(object){
  object %>%
    dplyr::filter(grepl('TRIAL_VAR', message)) %>%
    tidyr::separate(.data$message, c('header', 'assignment'), sep='TRIAL_VAR', remove=FALSE) %>%
    dplyr::mutate(assignment = gsub('=', ' ', .data$assignment)) %>%
    dplyr::mutate(assignment2 = sub(' ', "=", trimws(.data$assignment))) %>%
    tidyr::separate(.data$assignment2, c('variable', 'value'), sep='=', remove=FALSE) %>%
    dplyr::mutate(variable = trimws(.data$variable),
                  value = trimws(.data$value)) %>%
    dplyr::select(c("trial", "sttime", "sttime_rel", "variable", "value"))
}

#' @rdname extract_variables
#' @export
extract_variables.eyelinkRecording <- function(object){
  object$variables <- extract_variables(object$events)
  object
}

