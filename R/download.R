#' [Experimental] Download remote file and save as a temporary file
#' 
#' @param url location of a remote file
#' @param cache \code{TRUE}, save remote file in system's temporary folder and
#'   load it from the next time
#' @inheritParams readtext
#' @return path to a temporary file
#' @keywords internal experimental
#' @export
download <- function(url, cache = TRUE, verbosity = getOption("readtext_verbosity")) {
    cache_remote(url, FALSE, cache, NULL, verbosity)
}

