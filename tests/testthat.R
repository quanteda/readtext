Sys.setenv("R_TESTS" = "")

library(testthat)
library(readtext)
library(quanteda)

# save, then reset, readtext options
opts <- readtext_options()
readtext_options(reset = TRUE)

# set the soon-to-be-official-R-policy condition
Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = TRUE)

test_check("readtext")

# restore the options
readtext_options(opts)
