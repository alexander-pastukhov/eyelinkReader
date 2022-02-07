.onLoad <- function(libname, pkgname) {
  temp_dir <- tempdir()
  file.copy(system.file("cpp", "edf_interface.cpp", package = pkgname), temp_dir)
  filename <- paste0(temp_dir, "/edf_interface.cpp")
  # compiled_code <- Rcpp::sourceCpp(filename, env = parent.env(environment()))
}
