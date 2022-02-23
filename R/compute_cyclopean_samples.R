#' Computes cyclopean samples by averaging over binocular data
#'
#' Computes cyclopean samples by averaging over binocular recorded properties
#' such as \code{pxL}/\code{pxR}, \code{pyL}/\code{pyR}, \code{hxL}/\code{hxR},
#' etc. Uses function specified via \code{fun}
#' parameter to compute the average with \code{na.rm = TRUE} option.
#' In case of a monocular recording or when the information from one eye missing,
#' uses information from one eye only, ignoring the other column.
#' In both binocular and monocular recording cases, simplifies column names
#' so that \code{pxL} and/or \code{pxR} are replaced
#' with a single column \code{px}, \code{pyL}/\code{pyR} with \code{py}, etc.
#'
#' @param object Either an \code{\link{eyelinkRecording}} object or data.frame with samples,
#' i.e., \code{samples} slot of the \code{\link{eyelinkRecording}} object.
#' @param fun Function used to average across eyes, defaults to \code{\link{mean}}.
#'
#' @return Object of the same time as input, i.e., either a \code{\link{eyelinkRecording}} object
#' with \emph{modified} \code{samples} slot or a data.frame with cyclopean samples.

#' @export
#'
#' @examples
#' data(gaze)
#'
#' # by passing samples table
#' cyclopean_samples <- compute_cyclopean_samples(gaze$samples)
#'
#' # storing cyclopean samples as a separate table in recording
#' gaze$cyclopean_samples <- compute_cyclopean_samples(gaze$samples)
#'
#' # by passing the recording, cyclopean samples replace original ones
#' gaze <- compute_cyclopean_samples(gaze)
compute_cyclopean_samples <- function(object, fun = mean) { UseMethod("compute_cyclopean_samples") }


#' @rdname compute_cyclopean_samples
#' @export
#' @importFrom dplyr %>% select all_of mutate_if
#' @importFrom tidyr separate
#' @importFrom stringr str_detect str_remove
#' @importFrom rlang .data
compute_cyclopean_samples.data.frame <- function(object, fun = mean) {
  # figuring out columns that we need to compute the mean over
  i_eye_specific_column <- stringr::str_detect(names(object), "[L|R]$")
  eye_specific_columns <- names(object)[i_eye_specific_column]
  joint_eye_columns <- unique(stringr::str_remove(eye_specific_columns, "[L|R]$"))
  resulting_columns <- unique(stringr::str_remove(names(object), "[L|R]$"))

  # averaging over eye-specific columns
  for(current_column in joint_eye_columns){
    eye_components <- eye_specific_columns[stringr::str_detect(eye_specific_columns, sprintf("^%s[L|R]$", current_column))]
    if (identical(fun, mean)) {
      # rowMeans are MUCH faster than applying mean to margin row
      object[[current_column]] <- rowMeans(object[, eye_components], na.rm = TRUE)
    } else {
      # universal solution for everything else
      object[[current_column]] <- apply(object[, eye_components], MARGIN = 1, FUN = fun, na.rm = TRUE)
    }
  }

  object %>%
    # drop original eye-specific columns
    select(all_of(resulting_columns)) %>%

    # convert NaN to NA because mean(c(NA, NA), na.rm = TRUE)) returns NaN, not NA
    mutate_if(is.numeric, ~ifelse(is.nan(.x), NA, .x))
}


#' @rdname compute_cyclopean_samples
#' @export
compute_cyclopean_samples.eyelinkRecording <- function(object, fun = mean) {
  # check that samples are in the recording at all
  if (!("samples" %in% names(object))) {
    stop("No samples in an eyelinkRecording object.")
  }

  # modify in place
  object$samples <- compute_cyclopean_samples(object$samples, fun)
  object
}
