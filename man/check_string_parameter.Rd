% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_string_parameter.R
\name{check_string_parameter}
\alias{check_string_parameter}
\title{Checks for validity of a string parameter, stops if not valid}
\usage{
check_string_parameter(string_parameter)
}
\arguments{
\item{string_parameter}{String scalar}
}
\value{
character, value of the string_parameter
}
\description{
Checks for validity of a string parameter, stops if not valid
}
\examples{
start_marker <- "TRIALID"
check_string_parameter(start_marker)
}
\keyword{internal}
