## txt format
get_txt <- function(f, ...) {
    txt <- paste(readLines(con <- file(f, ...), warn = FALSE), collapse="\n")
    close(con)
    data.frame(texts = txt, stringsAsFactors = FALSE)
}


## csv format
get_csv <- function(path, textfield, ...) {


    args <- list(...)

    # Replace native.enc with UTF-8 if that's what it is
    if (args$encoding == 'native.enc') {  
        #  http://r.789695.n4.nabble.com/Find-out-what-quot-native-enc-quot-corresponds-to-td4639208.html
        args$encoding = strsplit(Sys.getlocale("LC_CTYPE"), '\\.')[[1]][2]
    }
    if (!(args$encoding %in% c('Latin-1', 'UTF-8'))) { 
        # If the encoding is not one fread supports, open the file using R's native function
        #  Use the encoding arg to open the file, pass all other args to fread
        txt <- paste(readLines(con <- file(path), encoding=args$encoding, warn = FALSE), collapse="\n")
        close(con)
        args$encoding <- NULL
        args <- c(list(input=txt, data.table=F, stringsAsFactors=F), args)
    }
    else {
        args <- c(list(input=path, data.table=F, stringsAsFactors=F), args)
    }
    docs <- do.call(data.table::fread, args)

    if (is.character(textfield)) {
        textfieldi <- which(names(docs) == textfield)
        if (length(textfieldi) == 0)
            stop(paste("There is no field called", textfield, "in file", path))
        textfield <- textfieldi
    } else if (is.numeric(textfield) & (textfield > ncol(docs))) {
        stop(paste0("There is no ", textfield, "th field in file ", path))
    }
    data.frame(texts = docs[, textfield], docs[, -textfield, drop = FALSE],
               stringsAsFactors = FALSE)
}



#  Dispatch to get_json_object or get_json_tweets depending on whether 
#  it looks like a twitter json file
get_json <- function(path, textfield, encoding, ...) {
    # encoding param is not used
    stopifnot(file.exists(path))
    tryCatch({
        return(get_json_tweets(path, ...))
    },
    error = function(e) {
        tryCatch({
            warning("Doesn't look like Tweets json file, trying general JSON")
            return(get_json_object(path, textfield, ...))
        },
        error = function(e) {
            if (e == paste("There is no field called", textfield, "in file", path)) {
                stop(e)
            }
            warning("File doesn't contain a single valid JSON object, trying line-delimited json")
            return(get_json_lines(path, textfield, ...))
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
get_json_object <- function(path, textfield, ...) {
    if (!requireNamespace("jsonlite", quietly = TRUE))
        stop("You must have jsonlite installed to read json files.")
    if (is.numeric(textfield)) {
        stop('Cannot use numeric textfield with json file')
    }
    
    docs <- jsonlite::fromJSON(path, flatten=TRUE, ...)
    docs <- data.table::setDT(docs)
    if (!(textfield %in% colnames(docs))) {
        stop(paste("There is no field called", textfield, "in file", path))
    }
    
    data.frame(texts = docs[[textfield]], docs[, -textfield, with = FALSE],
               stringsAsFactors = FALSE)
}

#' @importFrom data.table rbindlist
get_json_lines <- function(path, textfield, ...) {
    if (!requireNamespace("jsonlite", quietly = TRUE))
        stop("You must have jsonlite installed to read json files.")
    if (is.numeric(textfield)) {
        stop('Cannot use numeric textfield with json file')
    }
    
    lines <- readLines(path, warn = FALSE)
    
    docs <- data.table::rbindlist(
        lapply(lines, function(x)jsonlite::fromJSON(x, flatten=TRUE, ...)),
        use.names=TRUE, fill=TRUE
    )
    
    if (!(textfield %in% colnames(docs))) {
        stop(paste("There is no field called", textfield, "in file", path))
    }
    
    data.frame(texts = docs[[textfield]], docs[, -textfield, with = FALSE],
               stringsAsFactors = FALSE)
}


## flat xml format
get_xml <- function(path, textfield, encoding,...) {
    # TODO: encoding param is ignored
    if (!requireNamespace("XML", quietly = TRUE))
        stop("You must have XML installed to read XML files.")
    
    docs <- XML::xmlToDataFrame(path, stringsAsFactors = FALSE, ...)
    if (is.numeric(textfield) & (textfield > ncol(docs))) {
        stop(paste0("There is no ", textfield, "th field in file ", path))
    }
    if (is.character(textfield)) {
        textfieldi <- which(names(docs)==textfield)
        if (length(textfieldi)==0)
            stop(paste("There is no node called", textfield, "in file", path))
        textfield <- textfieldi
    }
    else {
        warning(paste("You should specify textfield by name rather than by index, unless",
                      "you're certain that your XML file's fields are always in the same order."))
    }
    
    # Because XML::xmlToDataFrame doesn't impute column types, we have to do it
    # ourselves, to match get_csv's behaviour
    data.frame(texts = docs[, textfield], 
               imputeDocvarsTypes(docs[, -textfield, drop = FALSE]),
               stringsAsFactors = FALSE)
}


