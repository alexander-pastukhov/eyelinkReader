#' Extracts rectangular areas of interest (AOI)
#'
#' @description Extracts rectangular areas of interest (AOI),
#' as defined by "!V IAREA RECTANGLE" command.
#' Specifically, we expect it to be in format
#' \code{!V IAREA RECTANGLE <index> <left> <top> <right> <bottom> <label>},
#' where \code{<label>} is a string label and all other variables are integer.
#' Please note that due to a non-standard nature of this function \strong{is not} called
#' during the \code{\link{read_edf}} call and you need to call it separately.
#'
#' @param object Either an \code{\link{eyelinkRecording}} object or data.frame with events,
#' i.e., \code{events} slot of the \code{\link{eyelinkRecording}} object.
#'
#' @return Object of the same time as input, i.e., either a \code{\link{eyelinkRecording}} object
#' with an additional \code{AOIs} slot or a data.frame with AOIs' information. See
#' \code{\link{eyelinkRecording}} for details.
#' @export
#'
#' @examples
#' if (eyelinkReader::is_compiled()) {
#'     recording <- read_edf(system.file("extdata", "example.edf", package = "eyelinkReader"))
#'
#'     # by passing the recording
#'     recording <- extract_AOIs(recording)
#'
#'     # by passing events table
#'     AOIs <- extract_AOIs(recording$events)
#' }
extract_AOIs <- function(object) { UseMethod("extract_AOIs") }

#' @rdname extract_AOIs
#' @export
#' @importFrom dplyr %>% filter select mutate
#' @importFrom tidyr separate
#' @importFrom stringr str_detect
#' @importFrom rlang .data
extract_AOIs.data.frame <- function(object){
  object %>%
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

#' @rdname extract_AOIs
#' @export
extract_AOIs.eyelinkRecording <- function(object){
  object$AOIs <- extract_AOIs(object$events)
  object
}

