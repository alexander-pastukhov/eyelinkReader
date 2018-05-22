\name{edfRecording}
\alias{edfRecording}
\title{S3 class "edfRecording", containing information imported from an edf-file.}
\description{
  S3 class "edfRecording" is a list containing the at least some of following components imported from the EDF file.

  \itemize{
    \item{\code{preamble}} {preamble of the recording, see also \code{\link{read_preamble}}.}
    \item{\code{events}} {see \code{\link[=edfRecording$events]{events}} table.}
    \item{\code{samples}} {see \code{\link[=edfRecording$samples]{samples}} table.}
    \item{\code{headers}} {headers of the individual trials, see \code{\link[=edfRecording$headers]{headers}} table.}
    \item{\code{recordings}} {individual recording start/end information, see \code{\link[=edfRecording$recordings]{recordings}} table.}
    \item{\code{display_coords}} {recorded screen coordinates}
    \item{\code{saccades}} {saccades extracted from \code{events}, see \code{\link[=edfRecording$saccades]{saccades}} table.}
    \item{\code{fixations}} {fixations extracted from \code{events}, see \code{\link[=edfRecording$fixations]{fixations}} table.}
    \item{\code{blinks}} {blinks extracted from \code{events}, see \code{\link[=edfRecording$blinks]{blinks}} table.}
    \item{\code{variables}} {recorded variables extracted from \code{events}, see \code{\link[=edfRecording$variables]{variables}} table.}
  }
}

\seealso{
  \code{\link{read_edf}}, \code{\link{read_preamble}}
}
