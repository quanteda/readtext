Sys.setenv("R_TESTS" = "")

library(testthat)
library(quanteda)

### the following two functions are in the GitHub quanteda but not the 
### CRAN version, so they are defined here for the test purposes as well
### will be removed once quanteda is refreshed on CRAN
### -- KB 2016-11-19
texts.readtext <- function(x, groups = NULL, ...) {
    if (!is.null(groups))
        stop("groups argument not supported for texts() on a readtext object")
    as.character(x)
    # result <- x[["text"]]
    # names(result) <- row.names(x)
    # result
}
docvars.readtext <- function(x, field = NULL) {
    if (!is.null(field))
        warning("field argument not used for docvars on a readtext object", noBreaks. = TRUE)
    as.data.frame(x[, -which(names(x) %in% c("doc_id", "text")), drop = FALSE])
}

docnames.readtext <- function(x) {
    x[["doc_id"]]
}

ndoc.readtext <- function(x) {
    nrow(x)
}


test_check("readtext")
