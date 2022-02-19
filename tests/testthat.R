Sys.setenv("R_TESTS" = "")

library(testthat)
library(eyelinkReader)

test_check("eyelinkReader")
