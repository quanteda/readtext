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
rt_txt_1 <- readtext(paste0(DATA_DIR, "txt/UDHR/*"))
str(rt_txt_1)

## ------------------------------------------------------------------------
# Manifestos with docvars from filenames
rt_txt_2 <- readtext(paste0(DATA_DIR, "txt/EU_manifestos/*.txt"),
                docvarsfrom = "filenames", 
                docvarnames = c("unit", "context", "year", "language", "party"),
                dvsep = "_")

str(rt_txt_2)

## ------------------------------------------------------------------------
# Recurse through subdirectories
rt_txt_3 <- readtext(paste0(DATA_DIR, "txt/movie_reviews/*"))
str(rt_txt_3)

## ------------------------------------------------------------------------
# Read in comma-separated values
rt_csv <- readtext(paste0(DATA_DIR, "csv/inaugCorpus.csv"), textfield = "texts")
str(rt_csv)

## ------------------------------------------------------------------------
# Read in tab-separated values
rt_tsv <- readtext(paste0(DATA_DIR, "tsv/dailsample.tsv"), textfield = "speech")
str(rt_tsv)

## ------------------------------------------------------------------------
## Read in JSON data
rt_json <- readtext(paste0(DATA_DIR, "json/inaugural_sample.json"), textfield = "texts")
str(rt_json)

## ------------------------------------------------------------------------
## Read in Universal Declaration of Human Rights pdf files
rt_pdf <- readtext(paste0(DATA_DIR, "pdf/UDHR/*.pdf"), 
                docvarsfrom = "filenames", 
                docvarnames = c("document", "language"),
                sep = "_")
str(rt_pdf)

## ------------------------------------------------------------------------
Encoding(rt_pdf$text)

## ---- message = FALSE, eval = FALSE--------------------------------------
#  install.packages("antiword")
#  require(antiword)

## ------------------------------------------------------------------------
## Read in Word data (.docx)
rt_docx <- readtext(paste0(DATA_DIR, "word/*.docx"))

str(rt_docx)
Encoding(rt_docx$text)

## ------------------------------------------------------------------------
# Note: Example required: which URL should we use?

## ------------------------------------------------------------------------
# Note: Archive file required. The only zip archive included in readtext has 
# different encodings and is difficult to import (see section 4.2).

## ---- message = FALSE----------------------------------------------------
require(quanteda)

## ------------------------------------------------------------------------
# Read in comma-separated values with readtext
rt_csv <- readtext(paste0(DATA_DIR, "csv/inaugCorpus.csv"), textfield = "texts")

# Create quanteda corpus
corpus_csv <- quanteda::corpus(rt_csv)

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
# Create a temporary directory to extract the .zip file
FILEDIR <- tempdir()

# Unzip file
unzip(system.file("extdata", "data_files_encodedtexts.zip", package = "readtext"), exdir = FILEDIR)

## ------------------------------------------------------------------------
# Get encoding from filename
filenames <- list.files(FILEDIR, "\\.txt$")

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
# Read txt files
txts <- readtext(paste0(FILEDIR,  "/", "*.txt"), encoding = fileencodings)

substring(texts(txts)[1], 1, 40)  # English, looking good

substring(texts(txts)[4], 1, 40)  # Arabic, looking good 

substring(texts(txts)[40], 1, 40) # Cyrillic, looking good

substring(texts(txts)[7], 1, 40)  # Chinese, looking good

substring(texts(txts)[26], 1, 40) # Hindi, looking good

## ------------------------------------------------------------------------
txts <- readtext(paste0(FILEDIR, "/", "*.txt"), 
                 encoding = fileencodings,
                 docvarsfrom = "filenames", 
                 docvarnames = c("document", "language", "input_encoding"))

## ------------------------------------------------------------------------
corpus_txts <- corpus(txts)

summary(corpus_txts, 5)

