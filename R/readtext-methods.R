#' print method for a readtext object
#' 
#' Print a readtext object in a nicely formatted way.
#' @method print readtext
#' @param x the readtext object to be printed
#' @param n a single integer, the number of rows of a readtext object to print.
#' @param text_width number of characters to display of the text field
#' @param ... not used here
#' @importFrom utils head
#' @importFrom pillar tbl_format_setup tbl_format_header tbl_format_body tbl_format_footer
#' @importFrom stringi stri_sub
#' @keywords internal
#' @export
print.readtext <- function(x, n = 6L, text_width = 10L, ...) {
    cat("readtext object consisting of ", nrow(x),
        " document", ifelse(nrow(x) == 1, "", "s"), " and ",
        ncol(x) - 2, " docvar", ifelse((ncol(x) - 2) == 1, "", "s"),
        ".\n", sep = "")
    x$text <- paste0("\"", stringi::stri_sub(x$text, length = text_width), "\"...")
    # x <- cbind(data.frame(doc_id = rownames(x), stringsAsFactors = FALSE), x)
    # class(x) <- "data.frame"
    print(trunc_mat_tibble(x, n = n))
}

trunc_mat_tibble <- function(x, n = NULL, width = NULL, n_extra = NULL) {
    if (!inherits(x, "tbl")) {
        class(x) <- c("tbl", class(x))
    }
    
    setup <- pillar::tbl_format_setup(x, width = width, n = n, max_extra_cols = n_extra)
    
    header <- pillar::tbl_format_header(x, setup)
    body <- pillar::tbl_format_body(x, setup)
    footer <- pillar::tbl_format_footer(x, setup)
    
    text <- c(header, body, footer)
    structure(list(text = text, summary = list(NULL)), class = "trunc_mat")
}

#' @export
format.trunc_mat_tibble <- function(x, width = NULL, ...) {
    unclass(x)[[1]]
}

#' @export
print.trunc_mat_tibble <- function(x, ...) {
    writeLines(format(x, ...))
    invisible(x)
}

#' return only the texts from a readtext object
#' 
#' An accessor function to return the texts from a [readtext] object as a
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
