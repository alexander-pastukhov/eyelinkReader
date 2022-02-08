.onLoad <- function(libname, pkgname) {
  message("Compiling EDF API library interface, this will take moment...")
  temp_dir <- tempdir()
  file.copy(system.file("cpp", "edf_interface.cpp", package = pkgname), temp_dir)
  filename <- paste0(temp_dir, "/edf_interface.cpp")

  # setting compilation flahs
  Sys.setenv("PKG_CXXFLAGS"='-I"c:/Program Files (x86)/SR Research/EyeLink/Includes/eyelink"')
  Sys.setenv("PKG_LIBS"='-L"c:/Program Files (x86)/SR Research/EyeLink/libs/x64" -ledfapi64')

  Rcpp::sourceCpp(filename, env = parent.env(environment()))
}
