#' Checks validity of sample attribute vector and returns logical indexes.
#'
#' @param import_samples logical, whether importing was requested.
#' @param sample_attributes character vector with names of attributes.
#'
#' @return logical vector
#' @export
#'
#' @examples
#' logical_index_for_sample_attributes('time', 'gx', 'gy')
logical_index_for_sample_attributes <- function(import_samples, sample_attributes){
  sample_attr_labels <- c('time', 'px', 'py', 'hx', 'hy', 'pa',
                          'gx', 'gy', 'rx', 'ry',
                          'gxvel', 'gyvel', 'hxvel', 'hyvel', 'rxvel', 'ryvel',
                          'fgxvel', 'fgyvel', 'fhxvel', 'fhyvel', 'frxvel', 'fryvel',
                          'hdata', 'flags', 'input', 'buttons', 'htype', 'errors')

  # definitely no import, neither a logical flag nor attribute names
  if ((!import_samples) && is.null(sample_attributes)) {
    return(rep(FALSE, times = length(sample_attr_labels)))
  }

  # logical import flag but no specific attribute listed -> all of them
  if (is.null(sample_attributes)){
    return(rep(TRUE, times = length(sample_attr_labels)))
  }

  # some attributes, check that all names are valid
  if (!all(sample_attributes %in% sample_attr_labels)) stop("Invalid sample attribute names.")
  if (length(sample_attributes %in% sample_attr_labels) == 0) stop("Empty sample attribute names vector.")

  # logical indexing
  sample_attr_flag <- rep(FALSE, times = length(sample_attr_labels))
  sample_attr_flag[match(sample_attributes, sample_attr_labels)] <- TRUE
  return(sample_attr_flag)
}
