## some globals
SUPPORTED_FILETYPE_MAPPING <-        c('csv', 'txt', 'json', 'zip', 'gz', 'tar', 'xml', 'tab', 'tsv')
names(SUPPORTED_FILETYPE_MAPPING) <- c('csv', 'txt', 'json', 'zip', 'gz', 'tar', 'xml', 'tab', 'tsv')


#' read a text file(s)
#' 
#' Read texts and (if any) associated document-level meta-data from one or more source files. 
#' The text source files 
#' come from the textual component of the files, and the document-level
#' metadata ("docvars") come from either the file contents or filenames.
#' @param file the complete filename(s) to be read. This is designed to 
#'   automagically handle a number of common scenarios, so the value can be a
#    single filename, a vector of file names a remote URL, or a file "mask" using a 
#'   "glob"-type'  wildcard value.  Currently available filetypes are: 
#'   \describe{
#'   \item{\code{txt}}{plain text files:
#'   So-called structured text files, which describe both texts and metadata:
#'   For all structured text filetypes, the column, field, or node 
#'   which contains the the text must be specified with the \code{textfield}
#'   parameter, and all other fields are treated as docvars.}
#'   \item{\code{json}}{data in some form of JavaScript 
#'   Object Notation, consisting of the texts and optionally additional docvars.
#'   The supported formats are:
#'   \itemize{
#'   \item a single JSON object per file
#'   \item line-delimited JSON, with one object per line
#'   \item line-delimited JSON, of the format produced from a Twitter stream.
#'   This type of file has special handling which simplifies the Twitter format
#'   into docvars.  The correct format for each JSON file is automatically detected.}}
#'   \item{\code{csv,tab,tsv}}{comma- or tab-separated values}
#'   \item{\code{xml}}{Basic flat XML documents are supported -- those of the 
#'   kind supported by the function xmlToDataFrame function of the \strong{XML} 
#'   package.}
#'   \code{file} can also not be a path to a single local file, such as
#'    \item{a wildcard value}{any valid 
#'   pathname with a wildcard ("glob") expression that can be expanded by the 
#'   operating system.  This may consist of multiple file types.} 
#'   \item{a URL to a remote}{which is downloaded then loaded} 
#'   \item{\code{zip,tar,tar.gz,tar.bz}}{archive file, which is unzipped. The 
#'   contained files must be either at the top level or in a single directory.
#'   Archives, remote URLs and glob patterns can resolve to any of the other 
#'   filetypes, so you could have, for example, a remote URL to a zip file which
#'   contained Twitter JSON files.}
#'   }
#' @param textfield a variable (column) name or column number indicating where 
#'   to find the texts that form the documents for the corpus.  This must be 
#'   specified for file types \code{.csv} and \code{.json}. For XML files
#'   an XPath expression can be specified. 
#' @param docvarsfrom  used to specify that docvars should be taken from the 
#'   filenames, when the \code{readtxt} inputs are filenames and the elements 
#'   of the filenames are document variables, separated by a delimiter 
#'   (\code{dvsep}).  This allows easy assignment of docvars from filenames such
#'   as \code{1789-Washington.txt}, \code{1793-Washington}, etc. by \code{dvsep}
#'   or from meta-data embedded in the text file header (\code{headers}).
#' @param dvsep separator used in filenames to delimit docvar elements if 
#'   \code{docvarsfrom="filenames"} is used
#' @param docvarnames character vector of variable names for \code{docvars}, if 
#'   \code{docvarsfrom} is specified.  If this argument is not used, default 
#'   docvar names will be used (\code{docvar1}, \code{docvar2}, ...).
#' @param encoding vector: either the encoding of all files, or one encoding
#'   for each files
#' @param ignoreMissingFiles if \code{FALSE}, then if the file
#'   argument doesn't resolve to an existing file, then an error will be thrown.
#'   Note that this can happen in a number of ways, including passing a path 
#'   to a file that does not exist, to an empty archive file, or to a glob 
#'   pattern that matches no files.
#' @param ... additional arguments passed through to low-level file reading 
#'   function, such as \code{\link{file}}, \code{\link{read.csv}}, etc.  Useful 
#'   for specifying an input encoding option, which is specified in the same was
#'   as it would be give to \code{\link{iconv}}.  See the Encoding section of 
#'   \link{file} for details.  Also useful for passing arguments through to
#'   \code{\link{read.csv}}, for instance `quote = ""`, if quotes are causing
#'   problems within comma-delimited fields.
#' @details If \code{cache = TRUE}, the constructor does not store a copy of 
#'   the texts, but rather reads
#'   in the texts and associated data, and saves them to a temporary disk file 
#'   whose location is specified in the \link{readtext} object.  This 
#'   prevents a complete copy of the object from cluttering the global 
#'   environment and consuming additional space.  This does mean however that 
#'   the state of the file containing the source data will not be cross-platform
#'   and may not be persistent across sessions.  So the recommended usage is to 
#'   load the data into a corpus in the same session in which \code{readtxt} is
#'   called.
#' @return a data.frame consisting of a first column \code{texts} that contains
#' the texts, with any additional columns consisting of docvars.  This object can
#' be input directly into the \pkg{quanteda} package's \code{\link[quanteda]{corpus}} 
#' to construct a corpus.
#' @author Adam Obeng, Kenneth Benoit, and Paul Nulty
#' @export
#' @importFrom utils unzip type.convert
#' @importFrom httr GET write_disk
readtxt <- function(file, ignoreMissingFiles = FALSE, textfield = NULL, 
                    docvarsfrom = c("metadata", "filenames"), dvsep="_", 
                    docvarnames = NULL, encoding = NULL, ...) {
    
    # some error checks
    if (!is.character(file))
        stop("file must be a character (specifying file location(s))")
    
    docvarsfrom <- match.arg(docvarsfrom)
    # # just use the first, if both are specified?
    # if (is.missing(docvarsfrom))
    #     
    # if (!all(docvarsfrom %in% c( c("metadata", "filenames"))))
    #     stop("illegal docvarsfrom value")
     
    if (is.null(textfield)) textfield <- 1
    files <- listMatchingFiles(file, ignoreMissing = ignoreMissingFiles)
    
    if (is.null(encoding)) {
        encoding <- getOption("encoding")
    }
    if (length(encoding) > 1) {
        if (length(encoding) != length(files)) {
            stop('encoding parameter must be length 1, or as long as the number of files')
        }
        sources <- mapply(function(x, e) getSource(f = x, textfield = textfield, encoding = e, ...),
                         files, encoding,
                         SIMPLIFY = FALSE)
    } else {
        sources <- lapply(files, function(x) getSource(x, textfield, encoding = encoding, ...))
    }
    
    # combine all of the data.frames returned
    result <- data.frame(data.table::rbindlist(sources, use.names = TRUE, fill = TRUE),
                         stringsAsFactors = FALSE)

    # this is in case some smart-alec (like AO) globs different directories 
    # for identical filenames
    uniqueparts <- basename_unique(files, pathonly = TRUE)
    row.names(result) <- if (!identical(uniqueparts, "")) {
         paste(uniqueparts, as.character(sapply(sources, row.names)), sep = "/")
    } else {
         as.character(sapply(sources, row.names))
    }

    if ("filenames" %in% docvarsfrom) {
        filenameDocvars <- getdocvarsFromFilenames(files, dvsep = dvsep, 
                                                   docvarnames = docvarnames)
        result <- cbind(result, imputeDocvarsTypes(filenameDocvars))
    }
    
    class(result) <- c("readtext", class(result))
    result
}

## read each file as appropriate, calling the get_* functions for recognized
## file types
getSource <- function(f, textfield, ...) {
    # extension <- file_ext(f)

    # fileType <- tryCatch({
    #      SUPPORTED_FILETYPE_MAPPING[[extension]]
    # }, error = function(e) {
    #     if (e == 'subscript out of bounds') {
    #         stop(paste('Unsupported extension', extension, 'of file', f))
    #     }
    #     else {
    #         stop(e)
    #     }
    # })

    ## SIMPLER -KB
    fileType <- file_ext(f)
    if (!(fileType %in% SUPPORTED_FILETYPE_MAPPING))
        stop(paste('Unsupported extension', fileType, 'of file', f))

    newSource <- switch(fileType, 
               txt = get_txt(f, ...),
               csv = get_csv(f, textfield, sep=',', ...),
               tsv = get_csv(f, textfield, sep='\t', ...),
               tab = get_csv(f, textfield, sep='\t', ...),
               json = get_json(f, textfield, ...),
               xml = get_xml(f, textfield, ...)
        )

    # assign filename (variants) unique text names
    if ((len <- nrow(newSource)) > 1) {
        row.names(newSource) <- paste(basename(f), seq_len(len), sep = ".")
    } else {
        row.names(newSource) <- basename(f)
    }

    return(newSource)
}




