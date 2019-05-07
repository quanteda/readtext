## txt format
get_txt <- function(path, source, ...) {
    con <- file(path, ...)
    txt <- paste(readLines(con, warn = FALSE), collapse = "\n")
    close(con)
    data.frame(text = txt, stringsAsFactors = FALSE)
}

## csv format
get_csv <- function(path, text_field, encoding, source, ...) {

    # Replace native.enc with UTF-8 if that's what it is
    # http://r.789695.n4.nabble.com/Find-out-what-quot-native-enc-quot-corresponds-to-td4639208.html
    if (encoding == "native.enc")
        encoding <- strsplit(Sys.getlocale("LC_CTYPE"), "\\.")[[1]][2]
    if (!encoding %in% c("Latin-1", "UTF-8")) {
        # If the encoding is not one fread supports, open the file using R's native function
        #  Use the encoding arg to open the file, pass all other args to fread
        con <- file(path)
        txt <- paste(readLines(con, encoding = encoding, warn = FALSE), collapse = "\n")
        close(con)
        result <- data.table::fread(input = txt, data.table = FALSE, stringsAsFactors = FALSE, ...)
    } else {
        result <- data.table::fread(input = path, data.table = FALSE, stringsAsFactors = FALSE, encoding = encoding, ...)
    }
    sort_fields(result, path, text_field)
}



    
#  Dispatch to get_json_object or get_json_tweets depending on whether 
#  it looks like a twitter json file
get_json <- function(path, text_field, encoding, source, verbosity = 1, ...) {

    if (!source %in% c("auto", "twitter"))
        stop("'twitter' is the only source type available for json")
    
    if (source == "twitter") {
        return(get_json_tweets(path, verbosity, ...))
    } else {
        if (is.numeric(text_field))
            stop("Cannot use numeric text_field with json file")
        result <- get_json_object(path, verbosity, ...)
        if (!is.null(result))
            return(sort_fields(result, path, text_field))

        result <- get_json_lines(path, verbosity, ...)
        if (!is.null(result))
            return(sort_fields(result, path, text_field))
        stop("This JSON file format is not supported.", call. = FALSE)
    }
}

## Twitter json
get_json_tweets <- function(path, verbosity = 1, ...) {
    # if (!requireNamespace("streamR", quietly = TRUE))
    #     stop("You must have streamR installed to read Twitter json files.")
    # read raw json data
    txt <- readLines(path, warn = FALSE, ...)
    tryCatch({
        streamR::parseTweets(txt, verbose = FALSE, ...)
    },
        error = function(e) {
        if (verbosity >= 1)
            stop("Doesn't look like Tweets JSON file, trying general JSON.")
        return(NULL)
    })
}

## general json
#' @importFrom data.table setDT
get_json_object <- function(path, verbosity = 1, ...) {
    # if (!requireNamespace("jsonlite", quietly = TRUE))
    #     stop("You must have jsonlite installed to read json files.")
    #as.data.frame(jsonlite::fromJSON(path, flatten = TRUE, ...), stringsAsFactors = FALSE)
    tryCatch({
        data.table::setDT(jsonlite::read_json(path, simplifyVector = TRUE))
    },
    error = function(e) {
        if (verbosity >= 1)
            message("File doesn't contain a single valid JSON object.")
        return(NULL)
    })
}

#' @importFrom data.table rbindlist
get_json_lines <- function(path, verbosity = 1, ...) {
    # if (!requireNamespace("jsonlite", quietly = TRUE))
    #     stop("You must have jsonlite installed to read json files.")

    tryCatch({
        lines <- readLines(path, warn = FALSE)
        jsonlite::fromJSON(lines[1], flatten = TRUE, ...)
        data.table::rbindlist(
            lapply(lines, function(x) jsonlite::fromJSON(stri_trim(x), flatten = TRUE, ...)),
            use.names = TRUE, fill = TRUE
        )
    },
    error = function(e) {
        if (verbosity >= 1)
            stop("This JSON file format is not supported.", call. = FALSE)
    })
}


## flat xml format
get_xml <- function(path, text_field, encoding, source, collapse = "", verbosity = 1, 
                    ...) {
    # TODO: encoding param is ignored
    # if (!requireNamespace("XML", quietly = TRUE))
    #     stop("You must have XML installed to read XML files.")

    if (is_probably_xpath(text_field)) {
        xml <- xml2::read_xml(path)
        txt <- xml2::xml_text(xml2::xml_find_all(xml, text_field), ...)
        txt <- paste0(txt, collapse = collapse)
        return(data.frame(text = txt, stringsAsFactors = FALSE))
    } else {
        xml <- xml2::read_xml(path)
        result <- xml2_to_dataframe(xml)
        if (is.numeric(text_field)) {
            if (text_field > ncol(result)) 
                stop(paste0("There is no ", text_field, "th field in file ", path))
            if (verbosity >= 1) {
                warning(paste("You should specify text_field by name rather than by index, unless",
                              "you're certain that your XML file's fields are always in the same order."))
            }
        }

        # Because XML::xmlToDataFrame doesn't impute column types, we have to do it
        # ourselves, to match get_csv's behaviour
        sort_fields(result, path, text_field, impute_types = TRUE)
    }
}

get_html <- function(path, encoding, source, verbosity = 1, ...) {

    if (!source %in% c("auto", "nexis"))
        stop("'nexis' is the only source type available for HTML.")
    
    if (source == "nexis") {
        return(get_nexis_html(path, verbosity = verbosity, ...))
        tryCatch({
            #return(get_nexis_html(path, ...))
        },
        error = function(e) {
            if (verbosity >= 1) stop("Doesn't look like Nexis HTML file")
        })
    } else {
        # http://stackoverflow.com/a/3195926
        # html <- XML::htmlTreeParse(path, useInternal = TRUE)
        # txt <- XML::xpathApply(html, "//body//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)]",
        #                        XML::xmlValue)
        html <- xml2::read_html(path)
        txt <- xml2::xml_text(xml2::xml_find_all(html, "//body//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)]"))
        txt <- txt[stri_trim(txt) != ""]
        txt <- paste0(txt, collapse = "\n")

        data.frame(text = txt, stringsAsFactors = FALSE)
    }
}


get_pdf <- function(path, source, ...) {

    txt <- pdftools::pdf_text(as.character(path))
    txt <- paste0(txt, collapse = "\n")
    Encoding(txt) <- "UTF-8"
    data.frame(text = txt, stringsAsFactors = FALSE)
}

get_odt <- function(path, source, ...) {
	path <- extract_archive(path, ignore_missing = FALSE)
	path <- sub("/\\*$", "", path)
	path <- file.path(path, "content.xml")
	
	xml <- xml2::read_xml(path)
	txt <- xml2::xml_text(xml2::xml_find_all(xml, "//text:p"))
	
	txt <- txt[!grepl("^\\s*$", txt)] # Remove text which is just whitespace
	txt <- paste0(txt, collapse = "\n")
	
	data.frame(text = txt, stringsAsFactors = FALSE)
}

get_docx <- function(path, source, ...) {
    path <- extract_archive(path, ignore_missing = FALSE)
    path <- sub("/\\*$", "", path)
    path <- file.path(path, "word", "document.xml")

    xml <- xml2::read_xml(path)
    txt <- xml2::xml_text(xml2::xml_find_all(xml, "//w:p"))
    
    # xml <- XML::xmlTreeParse(path, useInternalNodes = TRUE)
    # txt <- XML::xpathApply(xml, "//w:p", XML::xmlValue)
    txt <- txt[!grepl("^\\s*$", txt)] # Remove text which is just whitespace
    txt <- paste0(txt, collapse = "\n")

    data.frame(text = txt, stringsAsFactors = FALSE)
}

get_doc <- function(path, source, ...) {

    path <- normalizePath(path)
    txt <- antiword::antiword(as.character(path))

    # tryCatch({
    #     txt <- system2("antiword", shQuote(normalizePath(f)), stdout = TRUE)
    # },
    # error = function(e) {
    #     if (grepl("error in running command", e)) {
    #         stop(e, "Please check whether antiword is installed. You can download it from http://www.winfield.demon.nl/")
    #     } else {
    #     stop(e)
    #     }
    # })
    txt <- paste0(txt, collapse = "\n")
    txt <- trimws(txt)
    data.frame(text = txt, stringsAsFactors = FALSE)
}

get_rtf <- function(path, source, ...) {
    path <- normalizePath(path)
    txt <- striprtf::read_rtf(as.character(path))
    txt <- paste0(txt, collapse = "\n")
    txt <- trimws(txt)
    data.frame(text = txt, stringsAsFactors = FALSE)
}

get_excel <- function(path, text_field, source, ...) {

    sheet_names <- readxl::excel_sheets(path)
    sheets <- lapply(sheet_names, function(x, ...) readxl::read_excel(path, sheet = x, ...))

    if (length(unique(sapply(sheets, ncol))) != 1) {
        warning("Not all worksheets in file \"", path, "\" have the same number of columns.")
    }

    result <- data.table::rbindlist(sheets, fill = TRUE)
    sort_fields(result, path, text_field, impute_types = TRUE)
}


get_ods <- function(path, text_field, source, ...) {
    sheet_names <- readODS::ods_sheets(path)
    sheets <- lapply(sheet_names, function(x, ...) readODS::read_ods(path, sheet = x, ...))

    if (length(unique(sapply(sheets, ncol))) != 1)
        warning("Not all worksheets in file \"", path, "\" have the same number of columns.")

    result <- data.table::rbindlist(sheets, fill = TRUE)
    sort_fields(result, path, text_field, impute_types = TRUE)
}


xml2_to_dataframe <- function(xml) {
    xml_list <- xml2::as_list(xml)
    depth_check <- function(this, thisdepth = 0) {
        if (!is.list(this)) {
            return(thisdepth)
        } else if (thisdepth > 3) {
            return(thisdepth)
        } else {
            return(max(unlist(lapply(this, depth_check, thisdepth = thisdepth + 1))))    
        }
    }
    if (depth_check(xml_list[[1]]) != 3) {
        stop("The xml format does not fit for the extraction without xPath\n  Use xPath method instead")
    }
    ret <- data.table::rbindlist(xml_list[[1]], fill = TRUE)
    data.table::setDF(ret)
    return(ret)
}
