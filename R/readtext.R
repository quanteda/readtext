#' read a text file(s)
#' 
#' Read texts and (if any) associated document-level meta-data from one or more source files. 
#' The text source files 
#' come from the textual component of the files, and the document-level
#' metadata ("docvars") come from either the file contents or filenames.
#' @param file the complete filename(s) to be read. This is designed to 
#'   automagically handle a number of common scenarios, so the value can be a
#    single filename, a vector of file names a remote URL, or a file "mask" using a 
#'   "glob"-type wildcard value.  Currently available filetypes are: 
#'   
#'   **Single file formats:**
#'   
#'   \describe{
#'   \item{`txt`}{plain text files:
#'   So-called structured text files, which describe both texts and metadata:
#'   For all structured text filetypes, the column, field, or node 
#'   which contains the the text must be specified with the `text_field`
#'   parameter, and all other fields are treated as docvars.}
#'   \item{`json`}{data in some form of JavaScript 
#'   Object Notation, consisting of the texts and optionally additional docvars.
#'   The supported formats are:
#'   \itemize{
#'   \item a single JSON object per file
#'   \item line-delimited JSON, with one object per line
#'   \item line-delimited JSON, of the format produced from a Twitter stream.
#'   This type of file has special handling which simplifies the Twitter format
#'   into docvars.  The correct format for each JSON file is automatically detected.}}
#'   \item{`csv,tab,tsv`}{comma- or tab-separated values}
#'   \item{`html`}{HTML documents, including specialized formats from known
#'   sources, such as Nexis-formatted HTML.  See the `source` parameter
#'   below.}
#'   \item{`xml`}{XML documents are supported -- those of the 
#'   kind that can be read by [xml2::read_xml()] and navigated through 
#'   [xml2::xml_find_all()]. For xml files, an additional
#'   argument `collapse` may be passed through `...` that names the character(s) to use in 
#'   appending different text elements together.}
#'   \item{`pdf`}{pdf formatted files, converted through \pkg{pdftools}.}  
#'   \item{`odt`}{Open Document Text formatted files.}
#'   \item{`doc, docx`}{Microsoft Word formatted files.}
#'   \item{`rtf`}{Rich Text Files.}
#'      
#'   **Reading multiple files and file types:** 
#'   
#'   In addition, `file` can also not be a path 
#'   to a single local file, but also combinations of any of the above types, such as:
#'    \item{a wildcard value}{any valid 
#'   pathname with a wildcard ("glob") expression that can be expanded by the 
#'   operating system.  This may consist of multiple file types.} 
#'   \item{a URL to a remote}{which is downloaded then loaded} 
#'   \item{`zip,tar,tar.gz,tar.bz`}{archive file, which is unzipped. The 
#'   contained files must be either at the top level or in a single directory.
#'   Archives, remote URLs and glob patterns can resolve to any of the other 
#'   filetypes, so you could have, for example, a remote URL to a zip file which
#'   contained Twitter JSON files.}
#'   }
#' @param text_field,docid_field a variable (column) name or column number
#'   indicating where to find the texts that form the documents for the corpus
#'   and their identifiers.  This must be specified for file types `.csv`,
#'   `.json`, and `.xls`/`.xlsx` files.  For XML files, an XPath
#'   expression can be specified.
#' @param docvarsfrom  used to specify that docvars should be taken from the 
#'   filenames, when the `readtext` inputs are filenames and the elements 
#'   of the filenames are document variables, separated by a delimiter 
#'   (`dvsep`).  This allows easy assignment of docvars from filenames such
#'   as `1789-Washington.txt`, `1793-Washington`, etc. by `dvsep`
#'   or from meta-data embedded in the text file header (`headers`).
#'   If `docvarsfrom` is set to `"filepaths"`, consider the full path to the
#'   file, not just the filename.
#' @param dvsep separator (a regular expression character string) used in 
#'  filenames to delimit docvar elements if  `docvarsfrom="filenames"` 
#'  or `docvarsfrom="filepaths"` is used
#' @param docvarnames character vector of variable names for `docvars`, if 
#'   `docvarsfrom` is specified.  If this argument is not used, default 
#'   docvar names will be used (`docvar1`, `docvar2`, ...).
#' @param encoding vector: either the encoding of all files, or one encoding
#'   for each files
#' @param ignore_missing_files if `FALSE`, then if the file
#'   argument doesn't resolve to an existing file, then an error will be thrown.
#'   Note that this can happen in a number of ways, including passing a path 
#'   to a file that does not exist, to an empty archive file, or to a glob 
#'   pattern that matches no files.
#' @param source used to specify specific formats of some input file types, such
#'   as JSON or HTML. Currently supported types are `"twitter"` for JSON and
#'   `"nexis"` for HTML.
#' @param cache if `TRUE`, save remote file to a temporary folder. Only used
#'   when `file` is a URL.
#' @param verbosity \itemize{
#'   \item 0: output errors only
#'   \item 1: output errors and warnings (default)
#'   \item 2: output a brief summary message
#'   \item 3: output detailed file-related messages
#' }
#' @param ... additional arguments passed through to low-level file reading 
#'   function, such as [file()], \link[data.table]{fread}, etc.  Useful 
#'   for specifying an input encoding option, which is specified in the same was
#'   as it would be give to [iconv()].  See the Encoding section of 
#'   [file] for details.  
#' @return a data.frame consisting of a columns `doc_id` and `text` 
#'   that contain a document identifier and the texts respectively, with any 
#'   additional columns consisting of document-level variables either found 
#'   in the file containing the texts, or created through the 
#'   `readtext` call.
#' @export
#' @importFrom utils unzip type.convert
#' @importFrom httr GET write_disk
#' @examples 
#' \dontrun{
#' ## get the data directory
#' if (!interactive()) pkgload::load_all()
#' DATA_DIR <- system.file("extdata/", package = "readtext")
#' 
#' ## read in some text data
#' # all UDHR files
#' (rt1 <- readtext(paste0(DATA_DIR, "/txt/UDHR/*")))
#' 
#' # manifestos with docvars from filenames
#' (rt2 <- readtext(paste0(DATA_DIR, "/txt/EU_manifestos/*.txt"),
#'                  docvarsfrom = "filenames", 
#'                  docvarnames = c("unit", "context", "year", "language", "party"),
#'                  encoding = "LATIN1"))
#'                  
#' # recurse through subdirectories
#' (rt3 <- readtext(paste0(DATA_DIR, "/txt/movie_reviews/*"), 
#'                  docvarsfrom = "filepaths", docvarnames = "sentiment"))
#' 
#' ## read in csv data
#' (rt4 <- readtext(paste0(DATA_DIR, "/csv/inaugCorpus.csv")))
#' 
#' ## read in tab-separated data
#' (rt5 <- readtext(paste0(DATA_DIR, "/tsv/dailsample.tsv"), text_field = "speech"))
#' 
#' ## read in JSON data
#' (rt6 <- readtext(paste0(DATA_DIR, "/json/inaugural_sample.json"), text_field = "texts"))
#' 
#' ## read in pdf data
#' # UNHDR
#' (rt7 <- readtext(paste0(DATA_DIR, "/pdf/UDHR/*.pdf"), 
#'                  docvarsfrom = "filenames", 
#'                  docvarnames = c("document", "language")))
#' Encoding(rt7$text)
#'
#' ## read in Word data (.doc)
#' (rt8 <- readtext(paste0(DATA_DIR, "/word/*.doc")))
#' Encoding(rt8$text)
#'
#' ## read in Word data (.docx)
#' (rt9 <- readtext(paste0(DATA_DIR, "/word/*.docx")))
#' Encoding(rt9$text)
#'
#' ## use elements of path and filename as docvars
#' (rt10 <- readtext(paste0(DATA_DIR, "/pdf/UDHR/*.pdf"), 
#'                   docvarsfrom = "filepaths", dvsep = "[/_.]"))
#' }
readtext <- function(file, ignore_missing_files = FALSE, text_field = NULL,
                    docid_field = NULL,
                    docvarsfrom = c("metadata", "filenames", "filepaths"), dvsep = "_",
                    docvarnames = NULL, encoding = NULL, source = NULL, cache = TRUE,
                    verbosity = readtext_options("verbosity"),
                    ...) {

    args <- list(...)
    if ("textfield" %in% names(args)) {
        warning("textfield is deprecated; use text_field instead.")
        text_field <- args[["textfield"]]
    }
    

    # # in case the function was called without attaching the package,
    # # in which case the option is never set
    # if (is.null(verbosity))
    #     verbosity <- 1
    if (!verbosity %in% 0:3)
        stop("verbosity must be one of 0, 1, 2, 3.")
    if (!all(is.character(file)))
        stop("file must be a character (specifying file location(s)).")
    if (!is.null(source) && !is.character(source))
        stop("source must be a character.")
        
    docvarsfrom <- match.arg(docvarsfrom)
    # # just use the first, if both are specified?
    # if (is.missing(docvarsfrom))
    #  
    # if (!all(docvarsfrom %in% c( c("metadata", "filenames"))))
    #     stop("illegal docvarsfrom value")
    if (is.null(text_field))
        text_field <- 1
    if (length(encoding) < 2 && is.null(encoding))
        encoding <- getOption("encoding")
    if (is.null(source))
        source <- "auto"
    if (verbosity >= 2)
        message("Reading texts from ", file)
    
    # TODO: files need to be imported as they are discovered. Currently
    # list_files() uses a lot of storage space for temporary files when there
    # are a lot of archives.
    files <- list_files(file, ignore_missing_files, FALSE, cache, verbosity)
    if (length(encoding) == 1) {
        encoding <- rep(encoding, length(files))
    } else {
        if (length(encoding) != length(files))
            stop("Encoding parameter must be length 1, or as long as the number of files")
    }
    
    sources <- mapply(function(x, e) {
        get_source(x, text_field = text_field, docid_field = docid_field, 
                   encoding = e, source = source, verbosity = verbosity, ...)
    }, files, encoding, SIMPLIFY = FALSE)

    # combine all of the data.frames returned
    result <- data.frame(doc_id = "",
                         data.table::rbindlist(sources, use.names = TRUE, fill = TRUE),
                         stringsAsFactors = FALSE)

    # this is in case some smart-alec (like AO) globs different directories
    # for identical filenames
    ids <- lapply(sources, row.names)
    id <- unlist(ids, use.names = FALSE)
    if (any(duplicated(id))) {
        prefix <- rep(basename_unique(files, path_only = TRUE), lengths(ids))
        #if (lengths(prefix) > 1)
        id <- paste(prefix, id, sep = "/")
    }

    if (docvarsfrom %in% c("filepaths", "filenames")) {
        docvar <- get_docvars_filenames(files, dvsep, docvarnames, docvarsfrom == "filepaths", verbosity)
        result <- cbind(result, impute_types(docvar))
    }

    # change rownames to doc_id
    result$doc_id <- id
    rownames(result) <- NULL

    if (verbosity >= 2)
        message(" ... read ", nrow(result), " document",  if (nrow(result) == 1) "" else "s.")

    class(result) <- c("readtext", "data.frame")
    result
}

## Read each file as appropriate, calling the get_* functions for recognized
## file types
get_source <- function(path, text_field, docid_field, replace_specialchar = FALSE, verbosity = 1, ...,
                       # deprecated arguments
                       textfield) {

    ext <- tolower(file_ext(path))
    if (ext %in% extensions()) {
        if (dir.exists(path)) {
            call <- deparse(sys.call(1))
            call <- sub(path, paste0(sub("/$", "", path), "/*"), call, fixed = TRUE)
            stop("File '", path, "' does not exist, but a directory of this name does exist. ",
                 "To read all files in a directory, you must pass a glob expression like ", call, ".")
        }
    } else {
        if (verbosity >= 1)
            warning("Unsupported extension ", sQuote(ext), " of file ", path , " treating as plain text.")
        ext <- "txt"
    }

    if (verbosity >= 3)
        message(" ... reading (", ext, ") file: ", path)

    result <- switch(ext,
               txt = get_txt(path, ...),
               csv = get_csv(path, text_field, docid_field, sep = ",", ...),
               tsv = get_csv(path, text_field, docid_field, sep = "\t", ...),
               tab = get_csv(path, text_field, docid_field, sep = "\t", ...),
               json = get_json(path, text_field, docid_field, verbosity = verbosity, ...),
               xml = get_xml(path, text_field, verbosity = verbosity, ...), 
               html = get_html(path, verbosity = verbosity, ...),
               pdf = get_pdf(path, ...),
               odt = get_odt(path, ...),
               docx = get_docx(path, ...),
               doc = get_doc(path, ...),
               rtf = get_rtf(path, ...),
               xls = get_excel(path, text_field, docid_field, ...),
               xlsx = get_excel(path, text_field, docid_field, ...),
               ods = get_ods(path, text_field, docid_field, ...)
        )

    # assign filename (variants) unique text names
    len <- nrow(result)
    # TODO: stop using row.names as it errors when duplicated
    if (len > 1) {
        if (is.null(docid_field))
            row.names(result) <- paste(basename(path), seq_len(len), sep = ".")
    } else {
        row.names(result) <- basename(path)
    }

    if (replace_specialchar)
        result$text <- replace_charclass(result$text)

    return(result)
}

replace_charclass <- function (text) {
    mapping <- c(
        "\\p{Dash_Punctuation}" = "-",
        "\\p{Space_Separator}" = " ",
        "\\p{Initial_Punctuation}" = "'",
        "\\p{Final_Punctuation}" = "'",
        "\\p{Private_Use}" = "",
        "\\p{Unassigned}" = ""
    )

    for (i in seq_along(mapping))
        text <- stri_replace_all(text, names(mapping[i]), regex = mapping[i])
    return(text)
}
