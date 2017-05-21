
## from the tools package
file_ext <- function (x) {
    pos <- regexpr("\\.([[:alnum:]]+)$", x)
    ifelse(pos > -1L, substring(x, pos + 1L), "")
}

#' @importFrom tools file_path_sans_ext
getdocvarsFromFilenames <- function(fnames, dvsep="_", docvarnames=NULL, include_path=FALSE) {
    snames <- fnames
    if (include_path==FALSE) {
        snames <- tools::file_path_sans_ext(basename(snames))
    }
    parts <- strsplit(snames, dvsep)
    
    if (!all(sapply(parts,function(x) identical(length(x), length(parts[[1]])))))
        stop("Filename elements are not equal in length.")
    
    dvars <-  data.frame(matrix(unlist(parts), nrow=length(parts), byrow=TRUE), 
                         stringsAsFactors=FALSE)
    # assign default names in any case
    names(dvars) <- paste("docvar", 1:ncol(dvars), sep="")  
    if (!is.null(docvarnames)) {
        names(dvars)[1:length(docvarnames)] <- docvarnames
        if (length(docvarnames) != ncol(dvars)) {
            if (getOption("readtext_verbosity") >= 1) { 
                warning("Fewer docnames supplied than existing docvars - last ",
                        ncol(dvars) - length(docvarnames), " docvar",
                        ifelse((ncol(dvars) - length(docvarnames))==1, "", "s"),
                        " given generic names.")
            }
        }
    }
    dvars
}

# @rdname mktemp
# make temporary files and directories in a more reasonable way than tempfile()
# or tempdir(): here, the filename is different each time you call mktemp()
mktemp <- function(prefix = "tmp.", base_path = NULL, directory = FALSE) {
    #  Create a randomly-named temporary file or directory, sort of like
    #  https://www.mktemp.org/manual.html
    if (is.null(base_path))
        base_path <- tempdir()

    alphanumeric <- c(0:9, LETTERS, letters)

    filename <- paste0(sample(alphanumeric, 10, replace=T), collapse='')
    filename <- paste0(prefix, filename)
    filename <- file.path(base_path, filename)
    while (file.exists(filename) || dir.exists(filename)) {
        filename <- paste0(sample(alphanumeric, 10, replace=T), collapse='')
        filename <- paste0(prefix, filename)
        filename <- file.path(base_path, filename)
    }

    if (directory) {
        dir.create(filename)
    }
    else {
        file.create(filename)
    }

    return(filename)
}


downloadRemote <- function (i, ignoreMissing) {
    # First, check that this is not a URL with an unsupported scheme
    scheme <- stringi::stri_match(i, regex='^([a-z][a-z+.-]*):')[, 2]
    if (!(scheme %in% c('http', 'https', 'ftp'))) {
        stop(paste('Unsupported URL scheme', scheme))
    }
    
    # If this is a supported-scheme remote URL
    extension <- tools::file_ext(i)
    if (!(extension %in% SUPPORTED_FILETYPES)) {
        stop('Remote URL does not end in known extension. Please download the file manually.')
    }
    localfile <- file.path(mktemp(directory=T), basename(i))
    r <- httr::GET(i, httr::write_disk(localfile))
    if (ignoreMissing) {
        httr::warn_for_status(r)
    }
    else {
        httr::stop_for_status(r)
    }
    localfile
}

listMatchingFiles <- function(x, ignoreMissing = FALSE, lastRound = FALSE) {
    #  The implementation of listMatchingFiles and listMatchingFile might seem
    #  very complex, but it was arrived at after a lot of toil. The main design
    #  decision made here is that the user should be able to pass many
    #  different types of string to listMatchingFiles and get a consistent result:
    #  a list of local filenames. (One additional wrinkle is that there are two
    #  functions, listMatchingFiles and listMatchingFile. This is to ensure that
    #  listMatchingFile is only ever called with a length 1 argument, even though
    #  it can return multiple filenames. For the purposes of this explanation, 
    #  this distinction is elided).
    #  There are four possible types of values for x
    #     - a simple filename
    #     - a remote URL
    #     - a glob pattern
    #     - a vector of some combination of the above
    #  listMatchingFiles has a recursive design, because  some of these 
    #  arguments can resolve to arguments which need further processing: e.g.
    #  a remote URL could resolve to a zip file which needs to be extracted.
    #  The termination condition for the recursion is when the argument passed
    #  is a local filepath which refers to a single file and needs no further
    #  processing, e.g. something like '/path/to/text.tsv'. However, it is not
    #  possible to determine if a given argument is a path to a single file 
    #  or a glob pattern which matches multiple files, without actually trying
    #  the match. This matters because if it's the result of a globbing expression,
    #  then it could potentially need further processing, but if it's not, it the recursion
    #  needs to end. We can't know beforehand because the rules for globbing are 
    #  implementation-dependent (systems might treat '/path/to/file\*.tsv' as
    #  either a filename or a path depending on  whether they support escaping
    #  of glob wildcards. We could have tested the return value from Sys.glob
    #  to see whether the system treats a given string as a glob pattern or a 
    #  simple filename. Unfortunately, Sys.glob() will return character(0)
    #  for either a glob pattern which matches no files, or a non-glob filename
    #  for a file that doesn't exist, so that doesn't work either.
    #  We also can't test whether a pattern is a regular file by looking at the
    #  extension, because '/path/to/*.zip' is a glob expression with a 'zip'
    #  extension.
    
    if (!(ignoreMissing || (length(x) > 0))) {
        stop("File '", x, "' does not exist.")
    }
    
    matchingFiles <- unlist(
        lapply(x, function (x) listMatchingFile(
            x,
            ignoreMissing=ignoreMissing,
            lastRound=lastRound)
        )
    )
    
    if (is.null(matchingFiles)) return(character(0))
    
    matchingFiles
}

extractArchive <- function(i, ignoreMissing) {
    if (!(ignoreMissing || file.exists(i)))
        stop("File '", i, "' does not exist.")
    
    td <- mktemp(directory=T)
    if (tools::file_ext(i) == 'zip' ||
        tools::file_ext(i) == 'docx'
        )
        utils::unzip(i, exdir = td)
    else if ( tools::file_ext(i) == 'gz' ||
              tools::file_ext(i) == 'tar' ||
              tools::file_ext(i) == 'bz' )
        utils::untar(i, exdir = td)
    else 
        stop("Archive extension '", tools::file_ext(i), "' unrecognised.")
    
    # Create a glob that matches all the files in the archive
    file.path(td, '*')
}

#' @importFrom stringi stri_match
#' @importFrom stringi stri_replace
listMatchingFile <- function(x, ignoreMissing, lastRound) {
    filenames <- c()
    #  Remove 'file' scheme
    i <- stringi::stri_replace_first_regex(x, "^file://", "")
    
    scheme <- stringi::stri_match_first_regex(i, "^([A-Za-z][A-Za-z0-9+.-]+)://")[, 2]
    
    # If not a URL (or a file:// URL) , treat it as a local file
    if (!is.na(scheme)) {
        if (getOption("readtext_verbosity") >=3 ) message(', reading remote file', appendLF = FALSE)
        #  If there is a non-'file' scheme, treat it as remote
        localfile <- downloadRemote(i, ignoreMissing=ignoreMissing)
        return(listMatchingFiles(localfile, ignoreMissing=ignoreMissing))
    }
    
    # Now, special local files
    if (tools::file_ext(i) == 'zip' ||
        tools::file_ext(i) == 'gz' ||
        tools::file_ext(i) == 'tar' ||
        tools::file_ext(i) == 'bz' 
    ) {
        if (getOption("readtext_verbosity") >= 3) 
            message(", unpacking .", tools::file_ext(i), " archive", appendLF = FALSE)
        archiveFiles <- extractArchive(i, ignoreMissing=ignoreMissing)
        return(listMatchingFiles(archiveFiles, ignoreMissing=ignoreMissing))
    }
    
    #  At this point, it may be a simple local file or a glob pattern, but as
    #  above, we have no way of telling a priori whether this is the case
    if (lastRound) {
        #  We get to this point if the path wasn't to some file that needed
        #  special treatment (zip, remote, etc.) and it was treated as a glob
        #  pattern, which means that it is definitely not a glob pattern this
        #  time
        if (dir.exists(i)) {
                return(listMatchingFiles(ignoreMissing=ignoreMissing, file.path(i, '*')))
        }
        if (!(ignoreMissing || file.exists(i))) {
                stop("File '", i, "' does not exist.")
        }
        if (getOption("readtext_verbosity") >= 3) 
            message("... reading (", tools::file_ext(i), ") file: ", basename(i))
        return(i)
    }
    else {
        #  If it wasn't a glob pattern last time, then it may be this time
        if (getOption("readtext_verbosity") >= 3) message(", using glob pattern")
        i <- Sys.glob(i)
        return(
            listMatchingFiles(i, ignoreMissing=ignoreMissing, lastRound=T)
        )
    }
    
}

## KB
##
## return basenames that are unique
##   pathonly if TRUE, just return the unique part of the path
##
## Examples:
##
## files <- c("../data/glob/subdir1/test.txt", "../data/glob/subdir2/test.txt")
## basename_unique(files)
## # [1] "subdir1/test.txt" "subdir2/test.txt"
## basename_unique(files, pathonly = TRUE)
## # [1] "subdir1" "subdir2"
## basename_unique(c("../data/test1.txt", "../data/test2.txt"))
## # [1] "test1.txt" "test2.txt"
basename_unique <- function(x, pathonly = FALSE) {
    # make a data.frame with the parts
    df <- data.frame(do.call(rbind, strsplit(x, "/")))
    
    # get the level of the pathname that makes it unique
    elements <- apply(df, 2, function(x) ifelse(length(unique(x)) == 1, TRUE, FALSE))

    # trap the case where pathonly is TRUE and all path elements are equal
    if (all(elements[-length(elements)]) & pathonly==TRUE) return("")
    
    first_different_element <- min(which(!elements))
    last_element <- ifelse(pathonly, ncol(df) - 1, ncol(df))
    bnames <- apply(df[, first_different_element : last_element, drop = FALSE], 1, 
                    paste, collapse = "/")
    return(bnames)
}


## impute the variable types, just like read.csv
## needed when the files read in tend to make everything character 
imputeDocvarsTypes <- function(docv) {
    if (nrow(docv) == 0) return(docv)
    # Impute types of columns, just like read.table
    docv[] <- lapply(docv, function(x) type.convert(as.character(x), as.is=T))
    # And convert columns which have been made into factors into strings
    factor_cols <- vapply(docv, is.factor, FUN.VALUE=c(T))
    docv[factor_cols] <- lapply(docv[factor_cols], as.character)
    data.frame(docv)
}

is_probably_xpath <- function(x) {
    invalid_xml_element_chars <- c('/', '@')
    any(
        sapply(invalid_xml_element_chars, function(i) {grepl(i, x, perl=T)})
    )
}


get_numeric_textfield <- function(text_field, docs, path) {
    if (is.character(text_field)) {
        text_fieldi <- which(names(docs) == text_field)
        if (length(text_fieldi) == 0)
            stop(paste("There is no field called", text_field, "in file", path))
        text_field <- text_fieldi
    } else if (is.numeric(text_field) & (text_field > ncol(docs))) {
        stop(paste0("There is no ", text_field, "th field in file ", path))
    }
    text_field
}
