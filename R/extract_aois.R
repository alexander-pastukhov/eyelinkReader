#' Extracts rectangular areas of interest (AOI)
#'
#' @description Extracts rectangular areas of interest (AOI),
#' as defined by \code{"!V IAREA RECTANGLE"} command.
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
#' data(gaze)
#'
#' # by passing the recording
#' gaze <- extract_AOIs(gaze)
#'
#' # by passing events table
#' AOIs <- extract_AOIs(gaze$events)
extract_AOIs <- function(object) { UseMethod("extract_AOIs") }

#' @rdname extract_AOIs
#' @export
#' @importFrom dplyr %>% filter select mutate
#' @importFrom tidyr separate
#' @importFrom stringr str_detect
#' @importFrom rlang .data
extract_AOIs.data.frame <- function(object){
  object %>%
    dplyr::filter(.data$type == "MESSAGEEVENT", stringr::str_starts(.data$message, "!V IAREA RECTANGLE")) |>
    dplyr::select("trial", "sttime", "sttime_rel", "message") |>
    dplyr::mutate(message = stringr::str_remove_all(.data$message, "!V IAREA RECTANGLE "),
                  chunks = stringr::str_split(.data$message, " "),
                  index = purrr::map_int(.data$chunks, ~as.integer(.[1])),
                  label = purrr::map_chr(.data$chunks, ~paste0(.[6:length(.)], collapse = " ")),
                  left = purrr::map_int(.data$chunks, ~as.integer(.[2])),
                  top = purrr::map_int(.data$chunks, ~as.integer(.[3])),
                  right = purrr::map_int(.data$chunks, ~as.integer(.[4])),
                  bottom = purrr::map_int(.data$chunks, ~as.integer(.[5]))) |>
    dplyr::select(-"chunks", -"message")
}

#' @rdname extract_AOIs
#' @export
extract_AOIs.eyelinkRecording <- function(object){
  object$AOIs <- extract_AOIs(object$events)
  object
}

