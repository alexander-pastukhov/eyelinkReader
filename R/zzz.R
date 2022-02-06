.onLoad <- function(libname, pkgname) {
  print(environment()) # For demonstration purposes only;
  print(parent.env(environment())) # Don't really do this.
  variable <- 42
  assign("variable", variable, envir = parent.env(environment()))

  # pkg_ns_env <- parent.env(environment())
  # temp_dir <- tempdir()
  # file.copy(system.file("cpp", "convert_NAs.cpp", package = "eyelinkReader"), temp_dir)
  # filename <- paste0(temp_dir, "/convert_NAs.cpp")
  # # print(filename)
  # compiled_code <- Rcpp::sourceCpp(filename, env = pkg_ns_env)
  # value <- 42
  # assign("convert_missing_to_NAs", value, envir = parent.env(environment()))
  # assign("convert_missing_to_NAs", pkg_ns_env$convert_missing_to_NAs, envir = parent.env(environment()))
  # eyelinkReader::convert_missing_to_NAs <- pkg_ns_env$convert_missing_to_NAs
  # if (exists("convert_missing_to_NAs")) {
  #   assign("convert_missing_to_NAs", convert_missing_to_NAs, pkg_ns_env)
  #   rm("convert_missing_to_NAs")
  # }
  # compiled_code <- Rcpp::sourceCpp(system.file("cpp", "convert_NAs.cpp", package = "eyelinkReader"),
                                   # env = pkg_ns_env)
  # lapply(compiled_code$functions, function(name) assign(name, the_module[[name]], pkg_ns_env))
}
