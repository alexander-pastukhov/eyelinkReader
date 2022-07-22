#' Plot fixations and saccades for a set of trials
#'
#' This is only a basic plotting utility intended primarily
#' for a quick visual check. Please refer to companion vignette on plotting
#' for details about geoms and implementing your own custom plotting routine.
#'
#' @param x \code{\link{eyelinkRecording}} object
#' @param trial Trials to be plotted, could be a scalar index,
#' a vector of indexes, or \code{NULL} (all trials). Defaults to \code{1}.
#' @param show_fixations logical, whether to draw fixation as circles. Defaults to \code{TRUE}.
#' @param fixation_size_property Which fixation property is used as circle aesthetics. Defaults to \code{"duration"}.
#' @param size_legend An optional legend title, defaults to \code{"Fixation duration [ms]"}
#' if \code{fixation_size_property} is \code{"duration"} and to \code{NA} otherwise. In the latter case, the legend
#' title is unmodified (i.e., determined by ggplot).
#' @param show_saccades logical, whether to draw saccades as line segments. Defaults to \code{TRUE}.
#' @param saccade_color_property Which saccade property is used as color aesthetics. Defaults to \code{"sttime_rel"}
#' (onset time relative to the trial start).
#' @param color_legend An optional legend title, defaults to \code{"Saccade onset [ms]"}
#' if \code{saccade_color_property} is \code{"sttime_rel"} and to \code{NA} otherwise. In the latter case, the legend
#' title is unmodified (i.e., determined by ggplot).
#' @param ... Addition parameters (unused)
#'
#' @return ggplot object
#' @export
#' @importFrom ggplot2 ggplot coord_equal scale_x_continuous scale_y_reverse geom_point labs geom_segment aes_string
#' @importFrom methods hasArg
#'
#' @examples
#' data(gaze)
#'
#' # fixations and saccades for the first trial
#' plot(gaze)
#'
#' # fixations for the all trials
#' plot(gaze, trial = NULL, show_saccades = FALSE)
#'
#' # saccades for the first two trials
#' plot(gaze, trial = 1:2, show_fixations = FALSE)
#'
#' # color codes duration of a saccade
#' plot(gaze, saccade_color_property = "duration")

plot.eyelinkRecording <- function(x,
                                  trial = 1,
                                  show_fixations = TRUE,
                                  fixation_size_property = "duration",
                                  size_legend = ifelse(fixation_size_property == "duration", "Fixation duration [ms]", NA),
                                  show_saccades = TRUE,
                                  saccade_color_property = "sttime_rel",
                                  color_legend = ifelse(saccade_color_property == "sttime_rel", "Saccade onset [ms]", NA),
                                  ...){
  # empty plot with equally scaled x- and y-axes
  the_plot <-
    ggplot() +
    coord_equal(expand=FALSE)

  # setting limits based on display coordinated (if available)
  if ("display_coords" %in% names(x)) {
    the_plot <-
      the_plot +
      scale_x_continuous(name = "x", limits = x$display_coords[c(1, 3)]) +
      scale_y_reverse(name = "y", limits = x$display_coords[c(4, 2)])
  }

  # adding fixations
  if (show_fixations & "fixations" %in% names(x)) {
    if (is.null(trial)) {
      fixations <- x$fixations
    } else {
      fixations <- x$fixations[x$fixations$trial %in% trial, ]
    }
    the_plot <- the_plot + geom_point(data=fixations, aes_string(x = "gavx", y = "gavy", size = fixation_size_property), alpha=0.3)

    if (!is.null(fixation_size_property) & !is.na(size_legend)) {
      the_plot <- the_plot + labs(size = size_legend)
    }
  }

  # adding saccades
  if (show_saccades & "saccades" %in% names(x)) {
    if (is.null(trial)) {
      saccades <- x$saccades
    } else {
      saccades <- x$saccades[x$saccades$trial %in% trial, ]
    }
    the_plot <-
      the_plot +
      geom_segment(data=saccades, aes_string(x = "gstx", y = "gsty", xend = "genx", yend = "geny", color = saccade_color_property))

    if (!is.null(saccade_color_property) & !is.na(color_legend)) {
      the_plot <- the_plot + labs(color = color_legend)
    }
  }

  the_plot
}
