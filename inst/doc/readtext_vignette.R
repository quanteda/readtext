## ----echo = FALSE--------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, 
                      comment = "##")

## ----eval=TRUE, message = FALSE------------------------------------------
# Load readtext package
library(readtext)

## ------------------------------------------------------------------------
# Get the data directory from readtext
DATA_DIR <- system.file("extdata/", package = "readtext")

## ------------------------------------------------------------------------
# Read in all files from a folder
readtext(paste0(DATA_DIR, "/txt/UDHR/*"))

## ------------------------------------------------------------------------
# Manifestos with docvars from filenames
readtext(paste0(DATA_DIR, "/txt/EU_manifestos/*.txt"),
         docvarsfrom = "filenames", 
         docvarnames = c("unit", "context", "year", "language", "party"),
         dvsep = "_", 
         encoding = "ISO-8859-1")

## ------------------------------------------------------------------------
# Recurse through subdirectories
readtext(paste0(DATA_DIR, "/txt/movie_reviews/*"))

## ------------------------------------------------------------------------
# Read in comma-separated values
readtext(paste0(DATA_DIR, "/csv/inaugCorpus.csv"), text_field = "texts")

## ------------------------------------------------------------------------
# Read in tab-separated values
readtext(paste0(DATA_DIR, "/tsv/dailsample.tsv"), text_field = "speech")

## ------------------------------------------------------------------------
## Read in JSON data
readtext(paste0(DATA_DIR, "/json/inaugural_sample.json"), text_field = "texts")

## ------------------------------------------------------------------------
## Read in Universal Declaration of Human Rights pdf files
(rt_pdf <- readtext(paste0(DATA_DIR, "/pdf/UDHR/*.pdf"), 
                    docvarsfrom = "filenames", 
                    docvarnames = c("document", "language"),
                    sep = "_"))

## ------------------------------------------------------------------------
## Read in Word data (.docx)
readtext(paste0(DATA_DIR, "/word/*.docx"))

## ------------------------------------------------------------------------
# Note: Example required: which URL should we use?

## ------------------------------------------------------------------------
# Note: Archive file required. The only zip archive included in readtext has 
# different encodings and is difficult to import (see section 4.2).

## ---- message = FALSE----------------------------------------------------
require(quanteda)

## ------------------------------------------------------------------------
# read in comma-separated values with readtext
rt_csv <- readtext(paste0(DATA_DIR, "/csv/inaugCorpus.csv"), text_field = "texts")

# create quanteda corpus
corpus_csv <- corpus(rt_csv)
summary(corpus_csv, 5)

## ---- message = FALSE----------------------------------------------------
# Load stringi package
require(stringi)

## ------------------------------------------------------------------------
# Make some text with page numbers
sample_text_a <- "The quick brown fox named Seamus jumps over the lazy dog also named Seamus, 
page 1 
with the newspaper from a boy named quick Seamus, in his mouth.
page 2
The quicker brown fox jumped over 2 lazy dogs."

sample_text_a

# Remove "page" and respective digit
sample_text_a2 <- unlist(stri_split_fixed(sample_text_a, '\n'), use.names = FALSE)
sample_text_a2 <- stri_replace_all_regex(sample_text_a2, "page \\d*", "")
sample_text_a2 <- stri_trim_both(sample_text_a2)
sample_text_a2 <- sample_text_a2[sample_text_a2 != '']
stri_paste(sample_text_a2, collapse = '\n')

## ------------------------------------------------------------------------
sample_text_b <- "The quick brown fox named Seamus 
- 1 - 
jumps over the lazy dog also named Seamus, with 
- 2 - 
the newspaper from a boy named quick Seamus, in his mouth. 
- 33 - 
The quicker brown fox jumped over 2 lazy dogs."

sample_text_b

sample_text_b2 <- unlist(stri_split_fixed(sample_text_b, '\n'), use.names = FALSE)
sample_text_b2 <- stri_replace_all_regex(sample_text_b2, "[-] \\d* [-]", "")
sample_text_b2 <- stri_trim_both(sample_text_b2)
sample_text_b2 <- sample_text_b2[sample_text_b2 != '']
stri_paste(sample_text_b2, collapse = '\n')

## ------------------------------------------------------------------------
# create a temporary directory to extract the .zip file
FILEDIR <- tempdir()
# unzip file
unzip(system.file("extdata", "data_files_encodedtexts.zip", package = "readtext"), exdir = FILEDIR)

## ------------------------------------------------------------------------
# get encoding from filename
filenames <- list.files(FILEDIR, "^(Indian|UDHR_).*\\.txt$")

head(filenames)

# Strip the extension
filenames <- gsub(".txt$", "", filenames)
parts <- strsplit(filenames, "_")
fileencodings <- sapply(parts, "[", 3)

head(fileencodings)

# Check whether certain file encodings are not supported
notAvailableIndex <- which(!(fileencodings %in% iconvlist()))
fileencodings[notAvailableIndex]

## ------------------------------------------------------------------------
txts <- readtext(paste0(DATA_DIR, "/data_files_encodedtexts.zip"), 
                 encoding = fileencodings,
                 docvarsfrom = "filenames", 
                 docvarnames = c("document", "language", "input_encoding"))
print(txts, n = 50)

## ------------------------------------------------------------------------
corpus_txts <- corpus(txts)
summary(corpus_txts, 5)

