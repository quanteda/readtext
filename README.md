---
output:
  md_document:
    variant: markdown_github
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# readtext: Import and handling for plain and formatted text files

[![CRAN Version](http://www.r-pkg.org/badges/version/readtext)](https://CRAN.R-project.org/package=readtext)
![Downloads](http://cranlogs.r-pkg.org/badges/readtext)
[![Travis-CI Build Status](https://travis-ci.org/kbenoit/readtext.svg?branch=master)](https://travis-ci.org/kbenoit/readtext)
[![Build status](https://ci.appveyor.com/api/projects/status/x6dtvh2m7mj3b026/branch/master?svg=true)](https://ci.appveyor.com/project/kbenoit/readtext)
[![codecov.io](https://codecov.io/github/kbenoit/readtext/coverage.svg?branch=master)][1]


[1]: https://codecov.io/gh/kbenoit/readtext/branch/master

An R package for reading text files in all their various formats, by Ken Benoit, Paul Nulty, and Adam Obeng.  (In alphabetical order, not by order of contribution.)

## Introduction

**readtext** is a one-function package that does exactly what it says on the tin: It reads files containing text, along with any associated document-level metadata, which we call "docvars", for document variables.  Plain text files do not have docvars, but other forms such as .csv, .tab, .xml, and .json files usually do.  

**readtext** accepts filemasks, so that you can specify a pattern to load multiple texts, and these texts can even be of multiple types.  **readtext** is smart enough to process them correctly, returning a data.frame with a primary field "text" containing a character vector of the texts, and additional columns of the data.frame as found in the document variables from the source files.

As encoding can also be a challenging issue for those reading in texts, we include functions for diagnosing encodings on a file-by-file basis, and allow you to specify vectorized input encodings to read in file types with individually set (and different) encodings.  (All ecnoding functions are handled by the **stringi** package.)

## How to Install

1.  From GitHub

    
    ```r
    # devtools packaged required to install quanteda from Github 
    devtools::install_github("kbenoit/quanteda") 
    ```

2.  From CRAN

    (coming soon)

## Inter-operability with **quanteda**

**readtext** was originally developed in early versions of the [**quanteda**](https://github.com/kbenoit/quanteda) package for the quantitative analysis of textual data.  It was spawned from the `textfile()` function from that package, and now lives exclusively in **readtext**.  Because **quanteda**'s corpus constructor recognizes the data.frame format returned by `readtext()`, it can construct a corpus directly from a `readtext` object, preserving all docvars and other meta-data.


```r
require(readtext)
## Loading required package: readtext
require(quanteda)
## Loading required package: quanteda
## quanteda version 0.9.8.9022
## 
## Attaching package: 'quanteda'
## The following object is masked from 'package:base':
## 
##     sample

FILEDIR <- tempdir()
unzip(system.file("extdata", "encodedTextFiles.zip", package = "readtext"), exdir = FILEDIR)

# get encoding from filename
filenames <- list.files(FILEDIR, "\\.txt$")
# strip the extension
filenames <- gsub(".txt$", "", filenames)
parts <- strsplit(filenames, "_")
fileencodings <- sapply(parts, "[", 3)
fileencodings
##  [1] "UTF-16LE"     "UTF-8-BOM"    "ISO-8859-6"   "MACARABIC"   
##  [5] "UTF-8"        "WINDOWS-1256" "GB2312"       "GBK"         
##  [9] "UTF-8"        "UTF-16BE"     "UTF-16LE"     "UTF-8"       
## [13] "WINDOWS-1252" "ISO-8859-1"   "MACROMAN"     "UTF-8"       
## [17] "WINDOWS-1252" "ISO-8859-1"   "MACROMAN"     "UTF-8"       
## [21] "WINDOWS-1252" "CP1253"       "ISO-8859-7"   "MACGREEK"    
## [25] "UTF-8"        "UTF-8"        "ISO-8859-1"   "UTF-8"       
## [29] "WINDOWS-1252" "CP932"        "ISO-2022-JP"  "UTF-8"       
## [33] "WINDOWS-936"  "ISO-2022-KR"  "UTF-8"        "ISO-8859-5"  
## [37] "KOI8-R"       "MACCYRILLIC"  "UTF-8"        "WINDOWS-1251"
## [41] "UTF-8"

# find out which conversions are unavailable (through iconv())
cat("Encoding conversions not available for this platform:")
## Encoding conversions not available for this platform:
notAvailableIndex <- which(!(fileencodings %in% iconvlist()))
fileencodings[notAvailableIndex]
## [1] "UTF-8-BOM"

# read in some text files
# try readtext
txts <- readtext(paste0(FILEDIR, "/", "*.txt"))
## Error in listMatchingFile(x, ignoreMissing = ignoreMissing, lastRound = lastRound): object 'verbosity' not found
# substring(texts(txts)[1], 1, 80)  # gibberish
# substring(texts(txts)[4], 1, 80)  # hex
# substring(texts(txts)[40], 1, 80) # hex

# read them in again
txts <- readtext(paste0(FILEDIR,  "/", "*.txt"), encoding = fileencodings)
## Error in listMatchingFile(x, ignoreMissing = ignoreMissing, lastRound = lastRound): object 'verbosity' not found
substring(texts(txts)[1], 1, 80)  # English
## Error in texts(txts): object 'txts' not found
substring(texts(txts)[4], 1, 80)  # Arabic, looking good 
## Error in texts(txts): object 'txts' not found
substring(texts(txts)[40], 1, 80) # Cyrillic, looking good
## Error in texts(txts): object 'txts' not found
substring(texts(txts)[7], 1, 80)  # Chinese, looking good
## Error in texts(txts): object 'txts' not found
substring(texts(txts)[26], 1, 80) # Hindi, looking good
## Error in texts(txts): object 'txts' not found

txts <- readtext(paste0(FILEDIR, "/", "*.txt"), 
                 encoding = fileencodings,
                 docvarsfrom = "filenames", 
                 docvarnames = c("document", "language", "inputEncoding"))
## Error in listMatchingFile(x, ignoreMissing = ignoreMissing, lastRound = lastRound): object 'verbosity' not found
encodingCorpus <- corpus(txts, textField = "texts", 
                         source = "Created by encoding-tests.R") 
## Error in corpus(txts, textField = "texts", source = "Created by encoding-tests.R"): object 'txts' not found
summary(encodingCorpus)
## Error in summary(encodingCorpus): object 'encodingCorpus' not found
```

Piping works too:

```r
require(magrittr)
## Loading required package: magrittr
readtext(paste0(FILEDIR,  "/", "*.txt"), encoding = fileencodings) %>%
    corpus(textField = "texts") %>% 
        summary
## Error in listMatchingFile(x, ignoreMissing = ignoreMissing, lastRound = lastRound): object 'verbosity' not found
```





