
#' @method print readtext
#' @keywords internal
print.readtext <- function(x, ...) {
    cat("readtext object consisting of ", nrow(x), 
        " document", ifelse(nrow(x) == 1, "", "s"), " and ", 
        ncol(x)-1, " docvar", ifelse((ncol(x)-1) == 1, "", "s"), 
        ".\n", sep="")
    # print(head(as.data.frame(x))
}


#' return only the texts from a readtext object
#' 
#' An accessor function to return the texts from a \link{readtext} object as a
#' character vector, with names matching the document names.
#' @method as.character readtext
#' @param x the readtext object whose texts will be extracted
#' @param use.names logical; if \code{TRUE}, attach document names to the vector
#'   of texts
#' @param ... further arguments passed to or from other methods
#' @export
as.character.readtext <- function(x, use.names = TRUE, ...) {
    result <- x[["text"]]
    if (use.names) names(result) <- row.names(x)
    result
}


#' return only the docvars from a readtext object
#' 
#' An accessor function to return the non-text variables from a \link{readtext} object.
#' @method as.data.frame readtext
#' @param x the readtext object whose non-text variables will be extracted
#' @param ... further arguments passed to or from other methods
#' @keywords internal
# as.data.frame.readtext <- function(x, ...) {
#     if (length(x) == 1 & names(x) == "text") {
#         return(NULL) 
#     } else {
#         return(x[, -which(names(x) == "text")])
#     }
# }
