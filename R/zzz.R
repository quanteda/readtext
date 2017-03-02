.onAttach <- function(...) {
    # set user options
    options(readtext_verbosity = 1)
    # startup message
    # packageStartupMessage("readability version ", as.character(utils::packageVersion("readability")))
}
