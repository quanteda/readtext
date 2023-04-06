#' Get corpus texts \[deprecated\]
#'
#' Get the texts from a [readtext] object.
#' 
#' This function is **deprecated**.
#' 
#' Use [as.character.readtext()] to turn a readtext object into a simple named
#' character vector of documents.
#' @param x a [readtext] object
#' @param ... not used
#' @param spacer when concatenating texts by using `groups`, this will be the
#'   spacing added between texts.  (Default is two spaces.)
#' @return a character vector of the texts in the corpus
#' @export
#' @keywords corpus internal
texts <- function(x, ...) {
    UseMethod("texts")
}

#' @rdname texts
#' @export
texts.readtext <- function(x, ...) {
    .Deprecated("Use as.character() instead")
    as.character(x)
}

#' Extract document variables from a readtext object
#' 
#' Returns document variables from a readtext object.
#' @param x a [readtext] object whose document variables will be extracted
#' @returns a data.frame of document variables
#' @export
docvars <- function(x) {
    UseMethod("docvars")
}

#' @export
docvars.readtext <- function(x) {
    as.data.frame(x[, -c(match(c("doc_id", "text"), colnames(x)))])
}

#' Extract document names from a readtext object
#' 
#' Returns document names from a readtext object.
#' @param x a readtext object whose document names will be extracted
#' @returns a character vector of document names
#' @export
docnames <- function(x) {
    UseMethod("docnames")
}

#' @export
docnames.readtext <- function(x) {
    x[["doc_id"]]
}
