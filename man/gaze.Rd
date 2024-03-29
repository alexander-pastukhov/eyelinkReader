% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{gaze}
\alias{gaze}
\title{Imported example.edf, events and samples}
\format{
An object of class \code{eyelinkRecording} of length 12.
}
\usage{
gaze
}
\description{
An \code{\link{eyelinkRecording}} for \emph{example.edf} via
\code{read_edf(system.file("extdata", "example.edf", package = "eyelinkReader"), import_samples = TRUE)}).
Contains all extracted events including triggers, areas of interested, and display coordinates. The original
recording consist of ten trials with a participant fixating on a target that jumps to a new location after
one second and stays on the screen for another second. Includes all relevant events.
}
\details{
See \code{\link{eyelinkRecording}} for details.
}
\seealso{
\code{\link{eyelinkRecording}}, \code{\link{read_edf}}
}
\keyword{datasets}
