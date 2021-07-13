# Returns supported file extensions
extensions <- function() {
    c("csv", "txt", "json", "zip", "gz", "tar", "xml", "tab", "rtf",
      "tsv", "html", "pdf", "odt", "docx", "doc", "xls", "xlsx", "ods")
}

## from the tools package
file_ext <- function (x) {
    pos <- regexpr("\\.([[:alnum:]]+)$", x)
    ifelse(pos > -1L, substring(x, pos + 1L), "")
}

#' @importFrom tools file_path_sans_ext
get_docvars_filenames <- function(path, dvsep = "_", docvarnames = NULL,
                                  include_path = FALSE, verbosity) {

    if (include_path) {
        name <- path
    } else {
        name <- tools::file_path_sans_ext(basename(path))
    }
    parts <- strsplit(name, dvsep)
    if (any(length(parts[[1]]) != lengths(parts)))
        stop("Filename elements are not equal in length.")

    docvar <-  data.frame(matrix(unlist(parts), nrow = length(parts), byrow = TRUE),
                         stringsAsFactors = FALSE)
    # assign default names in any case
    names(docvar) <- paste("docvar", seq(ncol(docvar)), sep = "")
    if (!is.null(docvarnames)) {
        names(docvar)[seq_along(docvarnames)] <- docvarnames
        if (verbosity >= 1 && length(docvarnames) != ncol(docvar)) {
            warning("Fewer docnames supplied than existing docvars - last ",
                    ncol(docvar) - length(docvarnames), " docvar",
                    if (ncol(docvar) - length(docvarnames) == 1) "" else "s",
                    " given generic names.")

        }
    }
    return(docvar)
}

#' Get path to temporary file or directory
#' 
#' @param prefix a string appended to random file or directory names.
#' @param temp_dir a path to temporary directory. If `NULL`, value from
#'   `tempdir()` will be used.
#' @param directory logical; if `TRUE`, temporary directory will be
#'   created.
#' @param seed  a seed value for `digest::digest`. If code{NULL}, a random
#'   value will be used.
#' @keywords internal
get_temp <- function(prefix = "readtext-", temp_dir = NULL, directory = FALSE, seed = NULL) {
    if (is.null(temp_dir))
        temp_dir <- tempdir()
    if (is.null(seed))
        seed <- stats::rnorm(1)
    path <- NULL
    while (is.null(path)) {
        f <- paste0(prefix, digest::digest(seed))
        if (.Platform$OS.type == 'windows') {
            path <- paste0(temp_dir, '\\', f)
        } else {
            path <- paste0(temp_dir, '/', f)
        }
        if (is.null(seed) && file.exists(path)) {
            path <- NULL
            seed <- stats::rnorm(1)
        }
    }
    if (!file.exists(path) && directory)
        dir.create(path, showWarnings = FALSE)
    return(path)
}



#  The implementation of list_files and list_file might seem very
#  complex, but it was arrived at after a lot of toil. The main design decision
#  made here is that the user should be able to pass many different types of
#  string to list_files and get a consistent result: a list of local
#  filenames. (One additional wrinkle is that there are two functions,
#  list_files and list_file. This is to ensure that
#  list_file is only ever called with a length 1 argument, even though it
#  can return multiple filenames. For the purposes of this explanation, this
#  distinction is elided).
#  There are four possible types of values for x
#     - a simple filename
#     - a remote URL
#     - a glob pattern
#     - a vector of some combination of the above
#  list_files has a recursive design, because  some of these arguments
#  can resolve to arguments which need further processing: e.g. a remote URL
#  could resolve to a zip file which needs to be extracted. The termination
#  condition for the recursion is when the argument passed is a local filepath
#  which refers to a single file and needs no further processing, e.g. something
#  like '/path/to/text.tsv'. However, it is not possible to determine if a given
#  argument is a path to a single file or a glob pattern which matches multiple
#  files, without actually trying the match. This matters because if it's the
#  result of a globbing expression, then it could potentially need further
#  processing, but if it's not, it the recursion needs to end. We can't know
#  beforehand because the rules for globbing are implementation-dependent
#  (systems might treat '/path/to/file\*.tsv' as either a filename or a path
#  depending on  whether they support escaping of glob wildcards. We could have
#  tested the return value from Sys.glob to see whether the system treats a
#  given string as a glob pattern or a simple filename. Unfortunately,
#  Sys.glob() will return character(0) for either a glob pattern which matches
#  no files, or a non-glob filename for a file that doesn't exist, so that
#  doesn't work either. We also can't test whether a pattern is a regular file
#  by looking at the extension, because '/path/to/*.zip' is a glob expression
#  with a 'zip' extension.
list_files <- function(x, ignore_missing = FALSE, last_round = FALSE, list_file = TRUE, verbosity = 1) {
    if (!(ignore_missing || (length(x) > 0)))
        stop("File '", x, "' does not exist.")

    file <- unlist(lapply(x, function (x) list_file(x, ignore_missing, last_round, list_file, verbosity)))

    if (is.null(file))
        return(character(0))
    sort(file)
}


#' @import stringi
list_file <- function(file, ignore_missing, last_round, cache, verbosity = 1) {
    #  Remove "file" scheme
    file <- stri_replace_first_regex(file, "^file://", "")
    scheme <- stri_match_first_regex(file, "^([A-Za-z][A-Za-z0-9+.-]+)://")[, 2]
    
    # If not a URL (or a file:// URL) , treat it as a local file
    if (!is.na(scheme)) {
        return(list_files(download_remote(file, ignore_missing, cache, verbosity),
                          ignore_missing, FALSE, verbosity))
    }
    
    # Now, special local files
    ext <- tools::file_ext(file)
    if (ext %in% c("zip", "gz", "tar", "bz")) {
        if (verbosity >= 3)
            message(", unpacking .", ext, " archive", appendLF = FALSE)
        return(list_files(extract_archive(file, ignore_missing = ignore_missing), 
                          ignore_missing, FALSE, verbosity))
    }
    
    #  At this point, it may be a simple local file or a glob pattern, but as
    #  above, we have no way of telling a priori whether this is the case
    if (last_round) {
        #  We get to this point if the path wasn't to some file that needed
        #  special treatment (zip, remote, etc.) and it was treated as a glob
        #  pattern, which means that it is definitely not a glob pattern this
        #  time
        if (dir.exists(file))
            return(list_files(file.path(file, "*"), ignore_missing, FALSE, verbosity))
        
        if (!(ignore_missing || file.exists(file)))
            stop("File '", file, "' does not exist.")
        
        if (verbosity >= 3)
            message("... reading (", tools::file_ext(file), ") file: ", basename(file))
        return(file)
    } else {
        #  If it wasn't a glob pattern last time, then it may be this time
        if (verbosity >= 3) message(", using glob pattern")
        file <- Sys.glob(file)
        return(
            list_files(file, ignore_missing, TRUE, verbosity)
        )
    }
}


extract_archive <- function(file, ignore_missing) {
    if (!(ignore_missing || file.exists(file)))
        stop("File '", file, "' does not exist.")

    path <- get_temp(directory = TRUE)
    ext <- tolower(tools::file_ext(file))
    if (ext %in% c("zip", "docx", "odt")) {
        utils::unzip(file, exdir = path)
    } else if (ext %in% c("gz", "tar", "bz")) {
        utils::untar(file, exdir = path)
    } else {
        stop("Archive extension '", tools::file_ext(file), "' unrecognised.")
    }
    # Create a glob that matches all the files in the archive
    file.path(path, "*")
}


download_remote <- function (url, ignore_missing, cache, verbosity = 1) {
    # First, check that this is not a URL with an unsupported scheme
    scheme <- stri_match(url, regex = "^([a-z][a-z+.-]*):")[, 2]
    if (!scheme %in% c("http", "https", "ftp"))
        stop(paste("Unsupported URL scheme", scheme))
    
    # If this is a supported-scheme remote URL
    name <- stri_replace_last_regex(basename(url), "\\?.*$", "") # remove parameters in URL
    ext <- tools::file_ext(name)
    if (!ext %in% extensions())
        stop("Remote URL does not end in known extension. Please download the file manually.")
    path <- cache_remote(url, ignore_missing, cache, name, verbosity)
    return(path)
}

#' Internal function to cache remote file
#' @param url location of a remote file
#' @param ignore_missing if `TRUE`, warns for download status
#' @param cache `TRUE`, save file in system's temporary folder and load it
#'   from the next time
#' @param basename name of temporary file to preserve file extensions. If
#'   `NULL`, random string will be used.
#' @inheritParams readtext
#' @import  httr
#' @keywords internal
cache_remote <- function(url, ignore_missing, cache, basename = NULL, verbosity = 1) {
    
    if (!is.null(basename)) {
        path <- file.path(get_temp(directory = TRUE, seed = url))
        if (.Platform$OS.type == 'windows') {
            path <- paste(path, basename, sep = "\\")
        } else {
            path <- paste(path, basename, sep = "/")
        }
    } else {
        path <- file.path(get_temp(directory = FALSE, seed = url))
    }
    
    download = TRUE
    if (cache && file.exists(path))
        download = FALSE
    if (download) {
        if (verbosity >= 3) {
            message("Downloading from ", url)
            r <- GET(url, write_disk(path, overwrite = TRUE), progress())
        } else {
            r <- GET(url, write_disk(path, overwrite = TRUE))
        }
        if (ignore_missing) {
            warn_for_status(r)
        } else {
            stop_for_status(r)
        }
    } else {
        if (verbosity >= 3)
            message("Use cache for ", url)
    }
    return(path)
}

#' Return basenames that are unique
#' @param x character vector; file paths
#' @param path_only logical; if `TRUE`, only return the unique part of the path
#' @keywords internal
#' @examples
#' files <- c("../data/glob/subdir1/test.txt", "../data/glob/subdir2/test.txt")
#' readtext:::basename_unique(files)
#' # [1] "subdir1/test.txt" "subdir2/test.txt"
#' readtext:::basename_unique(files, path_only = TRUE)
#' # [1] "subdir1" "subdir2"
#' readtext:::basename_unique(c("../data/test1.txt", "../data/test2.txt"))
#' # [1] "test1.txt" "test2.txt"
basename_unique <- function(x, path_only = FALSE) {
    temp <- as.data.frame(strsplit(x, "/"), fix.empty.names = FALSE)
    if (path_only)
        temp <- temp[length(temp) * -1, , drop = FALSE]
    is_uniform <- apply(temp, 1, function(x) length(unique(x)) == 1)
    if (all(is_uniform)) {
        index <- integer()
    } else {
        index <- seq(min(which(!is_uniform)), max(which(!is_uniform)))
    }
    temp <- temp[index, , drop = FALSE]
    unlist(lapply(temp, paste0, collapse = "/"), use.names = FALSE)
}


#' Detect and set variable types automatically
#' 
#' Detect and set variable types in a similar way as `read.csv()` does.
#' Should be used when imported data.frame is all characters.
#' @param x data.frame; columns are all characters vectors
#' @keywords internal
impute_types <- function(x) {
    if (nrow(x) == 0) return(x)
    # Impute types of columns, just like read.table
    x <- lapply(x, function(x) type.convert(as.character(x), as.is = TRUE))
    # And convert columns which have been made into factors into strings
    is_factor <- sapply(x, is.factor)
    x[is_factor] <- lapply(x[is_factor], as.character)
    x <- data.frame(x, stringsAsFactors = FALSE)
    return(x)
}

is_probably_xpath <- function(x) {
    invalid_xml_element_chars <- c("/", "@")
    any(
        sapply(invalid_xml_element_chars, function(i) grepl(i, x, perl = TRUE))
    )
}

#' Move text to the first column and set types to document variables
#' 
#' @param x data.frame; contains texts and document variables
#' @param path character; file path from which `x` is created; only use in error message
#' @param text_field numeric or character; indicate position of a text column in x
#' @param impute_types logical; if `TRUE`, set types of variables automatically
#' @keywords internal
sort_fields <- function(x, path, text_field, impute_types = TRUE) {
    x <- as.data.frame(x)
    index <- seq(ncol(x))
    flag <- FALSE
    text_field <- text_field[1]
    if (is.numeric(text_field)) {
        flag <- index == text_field
        if (sum(flag) == 0)
            stop(sprintf("There is no %dth field in file %s.", text_field, path))
    } else if (is.character(text_field)) {
        flag <- names(x) == text_field
        if (sum(flag) == 0) {
            stop(sprintf("There is no field called %s in file %s.", text_field, path))
        } else if (sum(flag) > 1) {
            stop(sprintf("There is more than one field called %s in file %s.", text_field, path))
        }
    }
    x <- x[, c(index[flag], index[!flag]), drop = FALSE]
    names(x)[1] <- "text"
    if (impute_types) {
        return(impute_types(x))
    } else {
        return(x)
    }
}

#' Set the docid for multi-document objects
#' 
#' @param x data.frame; contains texts and document variables
#' @param path character; file path from which `x` is created; only use in error message
#' @param docid_field numeric or character; indicate position of a text column in x
#' @param impute_types logical; if `TRUE`, set types of variables automatically
#' @keywords internal
add_docid <- function(x, path, docid_field) {
    if(is.null(docid_field) && ("doc_id" %in% names(x))) {
        message("A field called \"doc_id\" exists in the file.",
                " If you intend to use it as a document identifier,",
                " use \"docid_field\" option.")
    }
    if (is.null(docid_field)) {
        return(x)
    }

    x <- as.data.frame(x)
    index <- seq(ncol(x))
    flag <- FALSE
    docid_field <- docid_field[1]
    if (is.numeric(docid_field)) {
        flag <- index == docid_field
        if (sum(flag) == 0) {
            stop(sprintf("There is no %dth field in file %s.", docid_field, path))
            return(x)
        }
    } else if (is.character(docid_field)) {
        flag <- names(x) == docid_field
        if (sum(flag) == 0) {
            stop(sprintf("There is no field called %s in file %s.", docid_field, path))
            return(x)
        } else if (sum(flag) > 1) {
            stop(sprintf("There is more than one field called %s in file %s.", docid_field, path))
        }
    }
    row.names(x) <- x[, which(flag)]
    x <- x[, -which(flag), drop = FALSE]
    return(x)
}
