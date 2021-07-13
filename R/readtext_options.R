#' Get or set package options for readtext
#'
#' Get or set global options affecting functions across \pkg{readtext}.
#' @param ... options to be set, as key-value pair, same as
#'   [options()]. This may be a list of valid key-value pairs, useful
#'   for setting a group of options at once (see examples).
#' @param reset logical; if `TRUE`, reset all \pkg{readtext} options to
#'   their default values
#' @param initialize logical; if `TRUE`, reset only the \pkg{readtext}
#'   options that are not already defined.  Used for setting initial values when
#'   some have been defined previously, such as in `.Rprofile`.
#' @details Currently available options are: \describe{
#' \item{`verbosity`}{Default
#'   verbosity for messages produced when reading files.  See
#'   [readtext()].}
#' }
#' @return When called using a `key = value` pair (where `key` can be
#' a label or quoted character name)), the option is set and `TRUE` is
#' returned invisibly.
#'
#' When called with no arguments, a named list of the package options is
#' returned.
#'
#' When called with `reset = TRUE` as an argument, all arguments are
#' options are reset to their default values, and `TRUE` is returned
#' invisibly.
#' @export
#' @examples
#' \dontrun{
#' # save the current options
#' (opt <- readtext_options())
#'
#' # set higher verbosity
#' readtext_options(verbosity = 3)
#' 
#' # read something in here
#' if (!interactive()) pkgload::load_all()
#' DATA_DIR <- system.file("extdata/", package = "readtext")
#' readtext(paste0(DATA_DIR, "/txt/UDHR/*"))
#' 
#' # reset to saved options
#' readtext_options(opt)
#' }
readtext_options <- function(..., reset = FALSE, initialize = FALSE) {
    
    args <- list(...)
    # if the ... is a list already, use that
    if (length(args) == 1 && is.list(args[[1]])) 
        args <- args[[1]]
    
    # initialize automatically it not yet done so
    if (is.null(options("readtext_initialized")) || !"package:readtext" %in% search())
        readtext_initialize()
        
    if (initialize) {
        readtext_initialize()
        return(invisible(TRUE))
    } else if (reset) {
        readtext_reset()
        return(invisible(TRUE))
    } else if (!length(args)) {
        # return all option values with names
        opts_names <- names(get_options_default())
        opts <- options()[paste0("readtext_", opts_names)]
        names(opts) <- stri_sub(names(opts), 10, -1) # remove prefix
        return(opts)
    } else if (is.null(names(args))) {
        # return a option value
        return(getOption(paste0("readtext_", args[[1]])))
    } else {
        # set value
        for (key in names(args)) {
            set_option_value(key, args[[key]])
        }
        return(invisible(args))
    }
}

readtext_initialize <- function() {
    opts <- get_options_default()
    for (key in names(opts)) {
        if (is.null(getOption(paste0("readtext_", key))))
            set_option_value(key, opts[[key]])
    }
    options("readtext_initialized" = TRUE)
}

readtext_reset <- function() {
    opts <- get_options_default()
    for (key in names(opts)) {
        set_option_value(key, opts[[key]])
    }
    options('readtext_initialized' = TRUE)
}

set_option_value <- function(key, value) {
    opts <- get_options_default()
    
    # check for key validity
    if (!key %in% names(opts))
        stop(key, " is not a valid readtext option")
    
    # assign the key-value
    opts <- list(value)
    names(opts) <- paste0("readtext_", key)
    
    options(opts)
}

# returns default options
get_options_default <- function(){
    opts <- list(verbosity = 1)
    return(opts)
}
