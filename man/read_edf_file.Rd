% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{read_edf_file}
\alias{read_edf_file}
\title{Internal function that reads EDF file}
\usage{
read_edf_file(
  filename,
  consistency,
  import_events,
  import_recordings,
  import_samples,
  sample_attr_flag,
  start_marker_string,
  end_marker_string,
  verbose
)
}
\arguments{
\item{filename}{full name of the EDF file}

\item{consistency}{consistency check control (for the time stamps of the start
and end events, etc). 0, no consistency check. 1, check consistency and report.
2, check consistency and fix.}

\item{import_events}{load/skip loading events.}

\item{import_recordings}{load/skip loading recordings.}

\item{import_samples}{load/skip loading of samples.}

\item{sample_attr_flag}{boolean vector that indicates which sample fields are to be stored}

\item{start_marker_string}{event that marks trial start. Defaults to "TRIALID", if empty.}

\item{end_marker_string}{event that marks trial end}

\item{verbose}{whether to show progressbar and report number of trials}
}
\value{
contents of the EDF file. Please see read_edf for details.
}
\description{
Reads EDF file into a list that contains events, samples, and recordings.
DO NOT call this function directly. Instead, use read_edf function that implements
parameter checks and additional postprocessing.
}
\keyword{internal}
