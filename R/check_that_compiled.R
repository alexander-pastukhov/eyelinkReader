#' Attempts to compile EDF interface
#'
#' @return NULL
#' @export
#' @keywords internal
#'
#' @examples
#' attempt_to_compile
attempt_to_compile <- function(){
  pkgname <- 'eyelinkReader'
  required_headers <- c('edf.h', 'edf_data.h', 'edftypes.h')

  locate_files <- function(files, folders) {
    for(a_folder in folders) if (all(file.exists(paste0(a_folder, "/", files)))) return(a_folder)
    return(NULL)
  }

  # copying the C++ source file file to a temporary folder for compilation
  temp_dir <- tempdir()
  file.copy(system.file("cpp", "edf_interface.cpp", package = pkgname), temp_dir)
  filename <- paste0(temp_dir, "/edf_interface.cpp")

  # make a copy of original compilation flags
  the_CXXFLAGS <- Sys.getenv("PKG_CXXFLAGS")
  the_PKG_LIBS <- Sys.getenv("PKG_LIBS")

  # figuring out which OS are we dealing with
  if (Sys.info()["sysname"] == "Windows") {
    include_path <- locate_files(required_headers,
                                 c(Sys.getenv("EDFAPI_INC"),
                                   "c:/Program Files (x86)/SR Research/EyeLink/Includes/eyelink"))

    if (.Machine$sizeof.pointer == 8) {
      # 64-bit
      library_file <- "edfapi64"
      library_path <- locate_files(paste0(library_file, ".dll"),
                                   c(Sys.getenv("EDFAPI_LIB64"),
                                     paste0(Sys.getenv("EDFAPI_LIB"), "/x64"),
                                     "c:/Program Files (x86)/SR Research/EyeLink/libs/x64"))
    } else {
      # 32-bit
      library_file <- "edfapi"
      library_path <- locate_files(paste0(library_file, ".dll"),
                                   c(Sys.getenv("EDFAPI_LIB"),
                                     "c:/Program Files (x86)/SR Research/EyeLink/libs"))
    }

    if (all(!is.null(c(include_path, library_path)))) {
      Sys.setenv("PKG_CXXFLAGS"=sprintf('-I"%s"', include_path))
      Sys.setenv("PKG_LIBS"=sprintf('-L"%s" -l%s', library_path, library_file))
      message("Compiling EDF API library interface, this will take a moment...")
      compilation_outcome <- try(Rcpp::sourceCpp(filename, env = parent.env(environment())))
      if (is(compilation_outcome, "try-error")) {
        #message("Could not locate EDF API, please read installation instructions.")
      }
    } else {
      #message("Could not locate EDF API, please read installation instructions.")
    }
  } else if (Sys.info()["sysname"] == "Linux") {
    include_path <- locate_files(required_headers,
                                 c(Sys.getenv("EDFAPI_INC"),
                                   "/usr/include/EyeLink"))
    if (!is.null(include_path)) {
      Sys.setenv("PKG_CXXFLAGS"=sprintf('-I"%s"', include_path))
      Sys.setenv("PKG_LIBS"='-ledfapi')
      message("Compiling EDF API library interface, this will take a moment...")
      compilation_outcome <- try(Rcpp::sourceCpp(filename,
                                                 env = parent.env(environment()),
                                                 echo = FALSE,
                                                 verbose = FALSE))
    }

    if (is.null(include_path) || is(compilation_outcome, "try-error")) {
      #message("Could not locate EDF API, please read installation instructions.")
    }
  } else if (Sys.info()["sysname"] == "Darwin") {
    include_path <- locate_files(required_headers,
                                 c(Sys.getenv("EDFAPI_INC"),
                                   '/Library/Frameworks/edfapi.framework/Headers/'))
    library_path <-'/Library/Frameworks/edfapi.framework/'
    if (!is.null(include_path)) {
      Sys.setenv("PKG_CXXFLAGS"=sprintf('-I"%s"', include_path))
      Sys.setenv("PKG_LIBS"=sprintf('-framework edfapi -F%s -rpath %s', library_path, library_path))
      message("Compiling EDF API library interface, this will take a moment...")
      compilation_outcome <- try(Rcpp::sourceCpp(filename,
                                                 env = parent.env(environment()),
                                                 echo = FALSE,
                                                 verbose = FALSE))
    }

    if (is.null(include_path) || is(compilation_outcome, "try-error")) {
      #message("Could not locate EDF API, please read installation instructions.")
    }

  } else {
    #message("Unfortunately, there is no EDF API implementation for your plaform.")
  }

  # restore original compilation flags
  Sys.setenv("PKG_CXXFLAGS" = the_CXXFLAGS)
  Sys.setenv("PKG_LIBS" = the_PKG_LIBS)
}


#' Checks whether EDF API library was present and
#' interface was successfully be compiled
#'
#' @param fail_loudly logical, whether lack of compiled library means
#' error (\code{TRUE}), just warning (\code{FALSE}), or silent (\code{NA},
#' for test use only).
#'
#' @export
#' @keywords internal
#'
#' @examples
#' check_that_compiled(fail_loudly = FALSE)
check_that_compiled <- function(fail_loudly = TRUE){
  # try to compile
  if (!eyelinkReader::compiled_library_status()) attempt_to_compile()

  if (!eyelinkReader::compiled_library_status() && !is.na(fail_loudly)) {
    if (fail_loudly) {
      stop("Failed to compile interface to EDF API, function will return NULL. Please read the manual for further details.")
    } else {
      warning("Failed to compile interface to EDF API, function will return NULL. Please read the manual for further details.")
    }
  }

  eyelinkReader::compiled_library_status()
}
