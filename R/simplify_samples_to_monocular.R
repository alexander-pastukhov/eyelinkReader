#' Simplifies samples by averaging over binocular recorded properties
#'
#' Simplifies samples by averaging over binocular recorded properties
#' such as \code{px}, \code{py}, \code{hx}, \code{hy}, \code{pa},
#' \code{gx}, \code{gy}, etc. Uses function specified via \code{fun}
#' parameter to compute the average with \code{na.rm = TRUE} option.
#' In case of a monocular recording or information from one eye missing,
#' this corresponds to using information from one eye only.
#'
#' @param object Either an \code{\link{eyelinkRecording}} object or data.frame with samples,
#' i.e., \code{samples} slot of the \code{\link{eyelinkRecording}} object.
#' @param fun Function used to average across eyes, defaults to \code{\link{mean}}.
#'
#' @return Object of the same time as input, i.e., either a \code{\link{eyelinkRecording}} object
#' with \emph{modified} \code{samples} slot or a data.frame with simplified monocular samples.

#' @export
#'
#' @examples
#' data(gaze)
#'
#' # by passing samples table
#' monocular_samples <- simplify_samples_to_monocular(gaze$samples)
#'
#' # by passing the recording
#' gaze <- simplify_samples_to_monocular(gaze)
simplify_samples_to_monocular <- function(object, fun = mean) { UseMethod("simplify_samples_to_monocular") }


#' @rdname simplify_samples_to_monocular
#' @export
#' @importFrom dplyr %>% select all_of mutate_if
#' @importFrom tidyr separate
#' @importFrom stringr str_detect str_remove
#' @importFrom rlang .data
simplify_samples_to_monocular.data.frame <- function(object, fun = mean) {
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


#' @rdname simplify_samples_to_monocular
#' @export
simplify_samples_to_monocular.eyelinkRecording <- function(object, fun = mean) {
  # check that samples are in the recording at all
  if (!("samples" %in% names(object))) {
    stop("No samples in an eyelinkRecording object.")
  }

  # modify in place
  object$samples <- simplify_samples_to_monocular(object$samples, fun)
  object
}
