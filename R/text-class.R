

#' @export
#' @method print readtext
#' @keywords internal
print.readtext <- function(x, ...) {
    cat("readtext object consisting of ", nrow(x), 
        " document", ifelse(nrow(x) == 1, "", "s"), " and ", 
        ncol(x)-1, " docvar", ifelse((ncol(x)-1) == 1, "", "s"), 
        ".\n", sep="")
    # print(head(as.data.frame(x))
}
