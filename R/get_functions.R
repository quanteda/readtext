## txt format
get_txt <- function(f, ...) {
    txt <- paste(readLines(con <- file(f, ...), warn = FALSE), collapse="\n")
    close(con)
    data.frame(texts = txt, stringsAsFactors = FALSE)
}


## csv format
get_csv <- function(path, text_field, ...) {
    docs <- utils::read.csv(path, stringsAsFactors = FALSE, ...)
    if (is.character(text_field)) {
        text_fieldi <- which(names(docs) == text_field)
        if (length(text_fieldi) == 0)
            stop(paste("There is no field called", text_field, "in file", path))
        text_field <- text_fieldi
    } else if (is.numeric(text_field) & (text_field > ncol(docs))) {
        stop(paste0("There is no ", text_field, "th field in file ", path))
    }
    
    data.frame(texts = docs[, text_field], docs[, -text_field, drop = FALSE],
               stringsAsFactors = FALSE)
}



#  Dispatch to get_json_object or get_json_tweets depending on whether 
#  it looks like a twitter json file
get_json <- function(path, text_field, encoding, ...) {
    # encoding param is not used
    stopifnot(file.exists(path))
    tryCatch({
        return(get_json_tweets(path, ...))
    },
    error = function(e) {
        tryCatch({
            warning("Doesn't look like Tweets json file, trying general JSON")
            return(get_json_object(path, text_field, ...))
        },
        error = function(e) {
            if (e == paste("There is no field called", text_field, "in file", path)) {
                stop(e)
            }
            warning("File doesn't contain a single valid JSON object, trying line-delimited json")
            return(get_json_lines(path, text_field, ...))
        })
    })
    
}

## Twitter json
get_json_tweets <- function(path, source="twitter", ...) {
    if (!requireNamespace("streamR", quietly = TRUE))
        stop("You must have streamR installed to read Twitter json files.")
    
    # read raw json data
    txt <- readLines(path, warn = FALSE, ...)
    
    results <- streamR::parseTweets(txt, verbose=FALSE, ...)
    data.frame(texts = results[, 1], as.data.frame(results[, -1, drop = FALSE]),
               stringsAsFactors = FALSE)
}

## general json
#' @importFrom data.table setDT
get_json_object <- function(path, text_field, ...) {
    if (!requireNamespace("jsonlite", quietly = TRUE))
        stop("You must have jsonlite installed to read json files.")
    if (is.numeric(text_field)) {
        stop('Cannot use numeric text_field with json file')
    }
    
    docs <- jsonlite::fromJSON(path, flatten=TRUE, ...)
    docs <- data.table::setDT(docs)
    if (!(text_field %in% colnames(docs))) {
        stop(paste("There is no field called", text_field, "in file", path))
    }
    
    data.frame(texts = docs[[text_field]], docs[, -text_field, with = FALSE],
               stringsAsFactors = FALSE)
}

#' @importFrom data.table rbindlist
get_json_lines <- function(path, text_field, ...) {
    if (!requireNamespace("jsonlite", quietly = TRUE))
        stop("You must have jsonlite installed to read json files.")
    if (is.numeric(text_field)) {
        stop('Cannot use numeric text_field with json file')
    }
    
    lines <- readLines(path, warn = FALSE)
    
    docs <- data.table::rbindlist(
        lapply(lines, function(x)jsonlite::fromJSON(x, flatten=TRUE, ...)),
        use.names=TRUE, fill=TRUE
    )
    
    if (!(text_field %in% colnames(docs))) {
        stop(paste("There is no field called", text_field, "in file", path))
    }
    
    data.frame(texts = docs[[text_field]], docs[, -text_field, with = FALSE],
               stringsAsFactors = FALSE)
}


## flat xml format
get_xml <- function(path, text_field, encoding,...) {
    # TODO: encoding param is ignored
    if (!requireNamespace("XML", quietly = TRUE))
        stop("You must have XML installed to read XML files.")
    
    docs <- XML::xmlToDataFrame(path, stringsAsFactors = FALSE, ...)
    if (is.numeric(text_field) & (text_field > ncol(docs))) {
        stop(paste0("There is no ", text_field, "th field in file ", path))
    }
    if (is.character(text_field)) {
        text_fieldi <- which(names(docs)==text_field)
        if (length(text_fieldi)==0)
            stop(paste("There is no node called", text_field, "in file", path))
        text_field <- text_fieldi
    }
    else {
        warning(paste("You should specify text_field by name rather than by index, unless",
                      "you're certain that your XML file's fields are always in the same order."))
    }
    
    # Because XML::xmlToDataFrame doesn't impute column types, we have to do it
    # ourselves, to match get_csv's behaviour
    data.frame(texts = docs[, text_field], 
               imputeDocvarsTypes(docs[, -text_field, drop = FALSE]),
               stringsAsFactors = FALSE)
}


