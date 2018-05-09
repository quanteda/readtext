Sys.setenv("R_TESTS" = "")

library(testthat)
library(quanteda)

Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = TRUE)

test_check("readtext")
