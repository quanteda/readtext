#' extract texts from a readtext object
#' 
#' Get the texts in a text or corpus object, with grouping options. Works for
#' plain character vectors too, if \code{groups} is a factor.
#' @param x a \link{readtext} object created by \code{\link{readtext}}
#' @param groups character vector containing the names of document variables in 
#'   a corpus, or a factor equal in length to the number of documents, used for 
#'   aggregating the texts through concatenation.  If \code{x} is of type
#'   character, then \code{groups} must be a factor.
#' @param ... unused
#' @return For \code{texts}, a character vector of the texts in the corpus.
#'   
#'   For \code{texts <-}, the corpus with the updated texts.
#' @export
texts <- function(x, groups = NULL, ...) {
    if (length(addedArgs <- list(...)))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    UseMethod("texts")
}

# @method texts readtext
#' @rdname texts 
#' @export
texts.readtext <- function(x, groups = NULL, ...) {
    if (!is.null(groups))
        stop("groups argument not supported for texts() on a readtext object")
    result <- x[["texts"]]
    names(result) <- row.names(x)
    result
}


#' extract document-level metadata from a readtext object
#' 
#' Get the document-level metadata, also known as "docvars", from a \link{readtext} object.
#' @param x a \link{readtext} object created by \code{\link{readtext}}
#' @param field string containing the document-level variable name
#' @return \code{docvars} returns a data.frame of the document-level variables
#' @export
docvars <- function(x, field = NULL) {
    UseMethod("docvars")
}

# @method docvars readtext
#' @rdname docvars
#' @export
docvars.readtext <- function(x, field = NULL) {
    if (!is.null(field))
        warning("field argument not used for docvars on a readtext object", noBreaks. = TRUE)
    as.data.frame(x[, -which(names(x)=="texts"), drop = FALSE])
}

