###
### low-level file handling functions
###

## txt format
get_txt <- function(f, ...) {
    txt <- paste(readLines(con <- file(f, ...), warn = FALSE), collapse = "\n")
    close(con)
    data.frame(text = txt, stringsAsFactors = FALSE)
}


## csv format
get_csv <- function(path, text_field, ...) {


    args <- list(...)

    # Replace native.enc with UTF-8 if that's what it is
    if (args$encoding == 'native.enc') {  
        #  http://r.789695.n4.nabble.com/Find-out-what-quot-native-enc-quot-corresponds-to-td4639208.html
        args$encoding = strsplit(Sys.getlocale("LC_CTYPE"), '\\.')[[1]][2]
    }
    if (!(args$encoding %in% c('Latin-1', 'UTF-8'))) { 
        # If the encoding is not one fread supports, open the file using R's native function
        #  Use the encoding arg to open the file, pass all other args to fread
        txt <- paste(readLines(con <- file(path), encoding=args$encoding, warn = FALSE), collapse="\n")
        close(con)
        args$encoding <- NULL
        args <- c(list(input=txt, data.table=F, stringsAsFactors=F), args)
    }
    else {
        args <- c(list(input=path, data.table=F, stringsAsFactors=F), args)
    }
    docs <- do.call(data.table::fread, args)

    text_field <- get_numeric_textfield(text_field, docs, path)

    data.frame(text = docs[, text_field], docs[, -text_field, drop = FALSE],
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
            if (getOption("readtext_verbosity") >= 1) warning("Doesn't look like Tweets json file, trying general JSON")
            return(get_json_object(path, text_field, ...))
        },
        error = function(e) {
            if (e == paste("There is no field called", text_field, "in file", path)) {
                stop(e)
            }
            if (getOption("readtext_verbosity") >= 1) warning("File doesn't contain a single valid JSON object, trying line-delimited json")
            return(get_json_lines(path, text_field, ...))
        })
    })
    
}

## Twitter json
get_json_tweets <- function(path, source="twitter", ...) {
    # if (!requireNamespace("streamR", quietly = TRUE))
    #     stop("You must have streamR installed to read Twitter json files.")
    
    # read raw json data
    txt <- readLines(path, warn = FALSE, ...)
    
    results <- streamR::parseTweets(txt, verbose=FALSE, ...)
    data.frame(text = results[, 1], as.data.frame(results[, -1, drop = FALSE]),
               stringsAsFactors = FALSE)
}

## general json
#' @importFrom data.table setDT
get_json_object <- function(path, text_field, ...) {
    # if (!requireNamespace("jsonlite", quietly = TRUE))
    #     stop("You must have jsonlite installed to read json files.")
    if (is.numeric(text_field)) {
        stop('Cannot use numeric text_field with json file')
    }
    
    docs <- jsonlite::fromJSON(path, flatten=TRUE, ...)
    docs <- data.table::setDT(docs)
    if (!(text_field %in% colnames(docs))) {
        stop(paste("There is no field called", text_field, "in file", path))
    }
    
    data.frame(text = docs[[text_field]], docs[, -text_field, with = FALSE],
               stringsAsFactors = FALSE)
}

#' @importFrom data.table rbindlist
get_json_lines <- function(path, text_field, ...) {
    # if (!requireNamespace("jsonlite", quietly = TRUE))
    #     stop("You must have jsonlite installed to read json files.")
    if (is.numeric(text_field)) {
        stop('Cannot use numeric text_field with json file')
    }
    
    lines <- readLines(path, warn = FALSE)
    
    docs <- data.table::rbindlist(
        lapply(lines, function(x)jsonlite::fromJSON(x, flatten=TRUE, ...)),
        use.names = TRUE, fill = TRUE
    )
    
    if (!(text_field %in% colnames(docs))) {
        stop(paste("There is no field called", text_field, "in file", path))
    }
    
    data.frame(text = docs[[text_field]], docs[, -text_field, with = FALSE],
               stringsAsFactors = FALSE)
}


## flat xml format
get_xml <- function(path, text_field, encoding, collapse = "", ...) {
    # TODO: encoding param is ignored
    # if (!requireNamespace("XML", quietly = TRUE))
    #     stop("You must have XML installed to read XML files.")
    
    if (is_probably_xpath(text_field))  {
        xml <- XML::xmlTreeParse(path, useInternalNodes = TRUE)
        txt <- XML::xpathApply(xml, text_field, XML::xmlValue, ...)
        txt <- paste0(txt, collapse = collapse)
        return(data.frame(text = txt, stringsAsFactors = FALSE))
    }
    else {
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
            if (getOption("readtext_verbosity") >= 1) {
                warning(paste("You should specify text_field by name rather than by index, unless",
                          "you're certain that your XML file's fields are always in the same order."))
            }
        }
        
        # Because XML::xmlToDataFrame doesn't impute column types, we have to do it
        # ourselves, to match get_csv's behaviour
        return(data.frame(text = docs[, text_field], 
                   imputeDocvarsTypes(docs[, -text_field, drop = FALSE]),
                   stringsAsFactors = FALSE))
    }
}


get_html <- function(f, ...) {
    args <- list(...)

    # http://stackoverflow.com/a/3195926
    html <- XML::htmlTreeParse(f, useInternal = TRUE)
    txt <- XML::xpathApply(html, "//body//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)]", 
                           XML::xmlValue)
    txt <- txt[!grepl('^\\s*$', txt)] # Remove text which is just whitespace
    txt <- paste0(txt, collapse='\n')

    data.frame(text = txt, stringsAsFactors = FALSE)
}


get_pdf <- function(f, ...) {
    args <- list(...)

    txt <- pdftools::pdf_text(as.character(f))

    # tryCatch({
    #     txt <- system2("pdftotext", c(shQuote(f), "-enc UTF-8", "-nopgbrk", "-"), 
    #                stdout = TRUE)
    # },
    # error = function(e) {
    #     if (grepl('error in running command', e)) {
    #         stop(e, 'Please check whether pdftotext is installed. You can download it as part of Xpdf from http://www.foolabs.com/xpdf/home.html')
    #     } else {
    #     stop(e)
    #     }
    # })

    txt <- paste0(txt, collapse='\n')
    Encoding(txt) <- "UTF-8"
    data.frame(text = txt, stringsAsFactors = FALSE)
}

get_docx <- function(f, ...) {
    args <- list(...)

    path <- extractArchive(f, ignoreMissing=FALSE)
    path <- sub('/\\*$', '', path)
    path <- file.path(path, 'word', 'document.xml')

    xml <- XML::xmlTreeParse(path, useInternalNodes = TRUE)
    txt <- XML::xpathApply(xml, "//w:p", XML::xmlValue)
    txt <- txt[!grepl('^\\s*$', txt)] # Remove text which is just whitespace
    txt <- paste0(txt, collapse = "\n")

    data.frame(text = txt, stringsAsFactors = FALSE)
}

get_doc <- function(f, ...) {
    args <- list(...)
    
    txt <- antiword::antiword(as.character(normalizePath(f)))
    
    # tryCatch({
    #     txt <- system2("antiword", shQuote(normalizePath(f)), stdout = TRUE)
    # },
    # error = function(e) {
    #     if (grepl('error in running command', e)) {
    #         stop(e, 'Please check whether antiword is installed. You can download it from http://www.winfield.demon.nl/')
    #     } else {
    #     stop(e)
    #     }
    # })
    txt <- paste0(txt, collapse = "\n")
    txt <- trimws(txt)
    data.frame(text = txt, stringsAsFactors = FALSE)
}


get_excel <- function(f, text_field, ...) {
    sheet_names <- readxl::excel_sheets(f)
    sheets <- lapply(sheet_names, function(x, ...) {readxl::read_excel(f, sheet=x, ...)})

    if (length(unique(sapply(sheets, ncol))) != 1) {
        warning('Not all worksheets in file "', f, '" have the same number of columns.')
    }

    docs <- data.table::rbindlist(sheets, fill=TRUE)
    text_field <- get_numeric_textfield(text_field, docs, path=f)

    data.frame(text = docs[,text_field, with=F], docs[, -text_field, with=FALSE],
               stringsAsFactors = FALSE)
}


get_ods <- function(f, text_field, ...) {
    sheet_names <- readODS::ods_sheets(f)
    sheets <- lapply(sheet_names, function(x, ...) {readODS::read_ods(f, sheet=x, ...)})

    if (length(unique(sapply(sheets, ncol))) != 1) {
        warning('Not all worksheets in file "', f, '" have the same number of columns.')
    }

    docs <- data.table::rbindlist(sheets, fill=TRUE)
    text_field <- get_numeric_textfield(text_field, docs, path=f)

    data.frame(text = docs[,text_field, with=F], docs[, -text_field, with=FALSE],
               stringsAsFactors = FALSE)
}
