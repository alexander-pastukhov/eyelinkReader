.onLoad <- function(libname, pkgname) {
  temp_dir <- tempdir()
  file.copy(system.file("cpp", "convert_NAs.cpp", package = "eyelinkReader"), temp_dir)
  filename <- paste0(temp_dir, "/convert_NAs.cpp")
  compiled_code <- Rcpp::sourceCpp(filename, env = parent.env(environment()))
}
