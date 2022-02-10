#' Compiles the library linking it to EDF API
#'
#' @param libname character
#' @param pkgname character
#' @export
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  required_headers <- c('edf.h', 'edf_data.h', 'edftypes.h')

  locate_files <- function(files, folders) {
    for(a_folder in folders) if (all(file.exists(paste0(a_folder, "/", files)))) return(a_folder)

    NULL
  }

  # copying the C++ source file file to a temporary folder for compilation
  temp_dir <- tempdir()
  file.copy(system.file("cpp", "edf_interface.cpp", package = pkgname), temp_dir)
  filename <- paste0(temp_dir, "/edf_interface.cpp")


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
      packageStartupMessage("Compiling EDF API library interface, this will take a moment...")
      Sys.setenv("PKG_CXXFLAGS"=sprintf('-I"%s"', include_path))
      Sys.setenv("PKG_LIBS"=sprintf('-L"%s" -l%s', library_path, library_file))
      Rcpp::sourceCpp(filename, env = parent.env(environment()))
    } else {
      packageStartupMessage("Could not locate EDF API, please read installation instructions.")
    }
  } else if (Sys.info()["sysname"] == "Linux") {
    include_path <- locate_files(required_headers,
                                 c(Sys.getenv("EDFAPI_INC"),
                                 "/usr/include/EyeLink"))
    if (!is.null(include_path)) {
      Sys.setenv("PKG_CXXFLAGS"=sprintf('-I"%s"', include_path))
      Sys.setenv("PKG_LIBS"='-ledfapi')
      packageStartupMessage("Compiling EDF API library interface, this will take a moment...")
      compilation_outcome <- try(Rcpp::sourceCpp(filename,
                                                 env = parent.env(environment()),
                                                 echo = FALSE,
                                                 verbose = FALSE))
    }

    if (is.null(include_path) || class(x) == "try-error") {
      packageStartupMessage("Could not locate EDF API, please read installation instructions.")
    }
  } else if (Sys.info()["sysname"] == "Darwin") {
    packageStartupMessage("Mac OSX compilation is not yet implemented but will be added soon.")
  } else {
    packageStartupMessage("Unfortunately, there is no EDF API implementation for your plaform.")
  }
}
