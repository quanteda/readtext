#' print method for a readtext object
#' 
#' Print a readtext object in a nicely formatted way.
#' @method print readtext
#' @param x the readtext object to be printed
#' @param n a single integer, the number of rows of a readtext object to print.
#' @param text_width number of characters to display of the text field
#' @param ... not used here
#' @importFrom utils head
#' @importFrom tibble trunc_mat
#' @importFrom stringi stri_sub
#' @export
print.readtext <- function(x, n = 6L, text_width = 10L, ...) {
    cat("readtext object consisting of ", nrow(x), 
        " document", ifelse(nrow(x) == 1, "", "s"), " and ", 
        ncol(x)-2, " docvar", ifelse((ncol(x)-2) == 1, "", "s"), 
        ".\n", sep="")
    x$text <- paste0("\"", stringi::stri_sub(x$text, length = text_width), "\"...")
    # x <- cbind(data.frame(doc_id = rownames(x), stringsAsFactors = FALSE), x)
    class(x) <- "data.frame"
    print(tibble::trunc_mat(x, n = n))
}

#' return only the texts from a readtext object
#' 
#' An accessor function to return the texts from a \link{readtext} object as a
#' character vector, with names matching the document names.
#' @method as.character readtext
#' @param x the readtext object whose texts will be extracted
#' @param ... further arguments passed to or from other methods
#' @export
as.character.readtext <- function(x, ...) {
    result <- x[["text"]]
    names(result) <- x[["doc_id"]]
    result
}

