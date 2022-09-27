#' Compiles the library linking it to EDF API
#'
#' @param libname character
#' @param pkgname character
#' @return No return value, called to compile EDF API interface.
#' @export
#' @importFrom methods is
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # attempt_to_compile()
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

    if (all(c(!is.null(include_path), !is.null(library_path)))) {
      Sys.setenv("PKG_CXXFLAGS"=sprintf('-I"%s"', include_path))
      Sys.setenv("PKG_LIBS"=sprintf('-L"%s" -l%s', library_path, library_file))
      compilation_outcome <- try(Rcpp::sourceCpp(filename, env = parent.env(environment())))
    }
  } else if (Sys.info()["sysname"] == "Linux") {
    include_path <- locate_files(required_headers,
                                 c(Sys.getenv("EDFAPI_INC"),
                                 "/usr/include/EyeLink"))
    if (!is.null(include_path)) {
      Sys.setenv("PKG_CXXFLAGS"=sprintf('-I"%s"', include_path))
      Sys.setenv("PKG_LIBS"='-ledfapi')
      compilation_outcome <- try(Rcpp::sourceCpp(filename,
                                                 env = parent.env(environment()),
                                                 echo = FALSE,
                                                 verbose = FALSE))
    }
  } else if (Sys.info()["sysname"] == "Darwin") {
    include_path <- locate_files(required_headers,
                                 c(Sys.getenv("EDFAPI_INC"),
                                   '/Library/Frameworks/edfapi.framework/Headers/'))
    library_path <-'/Library/Frameworks/edfapi.framework/'
    if (!is.null(include_path)) {
      Sys.setenv("PKG_CXXFLAGS"=sprintf('-I"%s"', include_path))
      Sys.setenv("PKG_LIBS"=sprintf('-framework edfapi -F%s -rpath %s', library_path, library_path))
      compilation_outcome <- try(Rcpp::sourceCpp(filename,
                                                 env = parent.env(environment()),
                                                 echo = FALSE,
                                                 verbose = FALSE))
    }
  }

  # restore original compilation flags
  Sys.setenv("PKG_CXXFLAGS" = the_CXXFLAGS)
  Sys.setenv("PKG_LIBS" = the_PKG_LIBS)
}


#' Check if the library was compiled
#'
#' @param libname character
#' @param pkgname character
#' @return No return value, called to check and warn if compiled EDF API interface is missing.
#' @export
#' @keywords internal
.onAttach <- function(libname, pkgname) {
  if (!eyelinkReader::compiled_library_status()){
    if (Sys.info()["sysname"] %in% c("Windows", "Linux", "Darwin")){
      packageStartupMessage("Failed to compile EDF API interface, please read installation instructions.")
    } else{
      packageStartupMessage("Unfortunately, there is no EDF API implementation for your plaform.")
    }
  }
}
