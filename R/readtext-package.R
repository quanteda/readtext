#' Import and handling for plain and formatted text files
#' 
#' A set of functions for  importing and handling text files and formatted text 
#' files with additional meta-data, such including .csv, .tab, .json, .xml, .xls,
#' .xlsx, and others.
#' 
#' \pkg{readtext} makes it easy to import text files in various formats, 
#' including using operating system filemasks to load in groups of files based 
#' on glob pattern matches, including files in multiple directories or 
#' sub-directories. \pkg{readtext} can also read multiple files into R from 
#' compressed archive files such as .gz, .zip, .tar.gz, etc.  Finally 
#' \pkg{readtext} reads in the document-level meta-data associated with texts, 
#' if those texts are in a format (e.g. .csv, .json) that includes additional, 
#' non-textual data.
#' @section Package options: \describe{ \item{`readtext_verbosity`}{Default
#'   verbosity for messages produced when reading files.  See
#'   [readtext()].} }
#' @author Ken Benoit, Adam Obeng, and Paul Nulty
#' @name readtext-package
"_PACKAGE"
