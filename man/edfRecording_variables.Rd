\name{edfRecording$variables}
\alias{edfRecording$variables}
\title{User recorded variables extracted from events table}
\description{User recorded variables extracted from message events with a \code{'TRIAL_VAR'} prefix. Original format can be either \code{'TRIAL_VAR <name> <value>'} or \code{'TRIAL_VAR <name>=<value>'}. The \code{<name>} cannot contain spaces or \code{'='} sign. White spaces are trimmed for both \code{<name>} and \code{<value>}.
\itemize{
  \item{\code{trial}} {trial index}
  \item{\code{sttime}} {time when the variable was recorded}
  \item{\code{sttime_rel}} {time when the variable was recorded, relative to the trial start time}
  \item{\code{variable}} {variable name, the \code{<name>} part of the event message}
  \item{\code{value}} {variable value, the \code{<value>} part of the event message}
}
}
\seealso{\code{\link[=edfRecording$events]{events}} table.}
