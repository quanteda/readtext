
# readtext: Import and handling for plain and formatted text files

<!-- badges: start -->

[![CRAN
Version](https://www.r-pkg.org/badges/version/readtext)](https://CRAN.R-project.org/package=readtext)
[![](https://img.shields.io/badge/devel%20version-0.91-royalblue.svg)](https://github.com/quanteda/readtext)
[![Downloads](https://cranlogs.r-pkg.org/badges/readtext)](https://CRAN.R-project.org/package=readtext)
[![Total
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/readtext?color=orange)](https://CRAN.R-project.org/package=readtext)
[![R-CMD-check](https://github.com/quanteda/readtext/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/quanteda/readtext/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/quanteda/readtext/branch/master/graph/badge.svg)](https://app.codecov.io/gh/quanteda/readtext?branch=master)
<!-- badges: end -->

An R package for reading text files in all their various formats, by Ken
Benoit, Adam Obeng, Paul Nulty, Aki Matsuo, Kohei Watanabe, and Stefan
Müller.

## Introduction

**readtext** is a one-function package that does exactly what it says on
the tin: It reads files containing text, along with any associated
document-level metadata, which we call “docvars”, for document
variables. Plain text files do not have docvars, but other forms such as
.csv, .tab, .xml, and .json files usually do.

**readtext** accepts filemasks, so that you can specify a pattern to
load multiple texts, and these texts can even be of multiple types.
**readtext** is smart enough to process them correctly, returning a
data.frame with a primary field “text” containing a character vector of
the texts, and additional columns of the data.frame as found in the
document variables from the source files.

As encoding can also be a challenging issue for those reading in texts,
we include functions for diagnosing encodings on a file-by-file basis,
and allow you to specify vectorized input encodings to read in file
types with individually set (and different) encodings. (All encoding
functions are handled by the **stringi** package.)

## How to Install

1.  From CRAN

    ``` r
    install.packages("readtext")
    ```

2.  From GitHub, if you want the latest development version.

    ``` r
    # devtools packaged required to install readtext from Github 
    remotes::install_github("quanteda/readtext") 
    ```

Linux note: There are a couple of dependencies that may not be available
on linux systems. On Debian/Ubuntu try installing these packages by
running these commands at the command line:

``` bash
sudo apt-get install libpoppler-cpp-dev   # for antiword
```

## Demonstration: Reading one or more text files

**readtext** supports plain text files (.txt), data in some form of
JavaScript Object Notation (.json), comma-or tab-separated values (.csv,
.tab, .tsv), XML documents (.xml), as well as PDF, Microsoft Word
formatted files and other document formats (.pdf, .doc, .docx, .odt,
.rtf). **readtext** also handles multiple files and file types using for
instance a “glob” expression, files from a URL or an archive file (.zip,
.tar, .tar.gz, .tar.bz).

The file formats are determined automatically by the filename
extensions. If a file has no extension or is unknown, **readtext** will
assume that it is plain text. The following command, for instance, will
load in all of the files from the subdirectory `txt/UDHR/`:

``` r
library("readtext")
# get the data directory from readtext
DATA_DIR <- system.file("extdata/", package = "readtext")

# read in all files from a folder
readtext(paste0(DATA_DIR, "/txt/UDHR/*"))
## readtext object consisting of 13 documents and 0 docvars.
## # A data frame: 13 × 2
##   doc_id            text                         
##   <chr>             <chr>                        
## 1 UDHR_chinese.txt  "\"世界人权宣言\n联合国\"..."
## 2 UDHR_czech.txt    "\"VŠEOBECNÁ \"..."          
## 3 UDHR_danish.txt   "\"Den 10. de\"..."          
## 4 UDHR_english.txt  "\"Universal \"..."          
## 5 UDHR_french.txt   "\"Déclaratio\"..."          
## 6 UDHR_georgian.txt "\"FLFVBFYBC \"..."          
## # ℹ 7 more rows
```

For files that contain multiple documents, such as comma-separated-value
documents, you will need to specify the column name containing the
texts, using the `text_field` argument:

``` r
# read in comma-separated values and specify text field
readtext(paste0(DATA_DIR, "/csv/inaugCorpus.csv"), text_field = "texts")
## readtext object consisting of 5 documents and 3 docvars.
## # A data frame: 5 × 5
##   doc_id            text                 Year President  FirstName
##   <chr>             <chr>               <int> <chr>      <chr>    
## 1 inaugCorpus.csv.1 "\"Fellow-Cit\"..."  1789 Washington George   
## 2 inaugCorpus.csv.2 "\"Fellow cit\"..."  1793 Washington George   
## 3 inaugCorpus.csv.3 "\"When it wa\"..."  1797 Adams      John     
## 4 inaugCorpus.csv.4 "\"Friends an\"..."  1801 Jefferson  Thomas   
## 5 inaugCorpus.csv.5 "\"Proceeding\"..."  1805 Jefferson  Thomas
```

For a more complete demonstration, see the package
[vignette](https://readtext.quanteda.io/articles/readtext_vignette.html).

## Inter-operability with other packages

### With **quanteda**

**readtext** was originally developed in early versions of the
[**quanteda**](https://github.com/quanteda/quanteda) package for the
quantitative analysis of textual data. Because **quanteda**’s corpus
constructor recognizes the data.frame format returned by `readtext()`,
it can construct a corpus directly from a readtext object, preserving
all docvars and other meta-data.

``` r
library("quanteda")
## Package version: 3.3.1
## Unicode version: 14.0
## ICU version: 71.1
## Parallel computing: 10 of 10 threads used.
## See https://quanteda.io for tutorials and examples.
## 
## Attaching package: 'quanteda'
## The following object is masked from 'package:readtext':
## 
##     texts
# read in comma-separated values with readtext
rt_csv <- readtext(paste0(DATA_DIR, "/csv/inaugCorpus.csv"), text_field = "texts")
# create quanteda corpus
corpus_csv <- corpus(rt_csv)
summary(corpus_csv, 5)
## Corpus consisting of 5 documents, showing 5 documents:
## 
##               Text Types Tokens Sentences Year  President FirstName
##  inaugCorpus.csv.1   625   1539        23 1789 Washington    George
##  inaugCorpus.csv.2    96    147         4 1793 Washington    George
##  inaugCorpus.csv.3   826   2577        37 1797      Adams      John
##  inaugCorpus.csv.4   717   1923        41 1801  Jefferson    Thomas
##  inaugCorpus.csv.5   804   2380        45 1805  Jefferson    Thomas
```

### Text Interchange Format compatibility

**readtext** returns a data.frame that is formatted as per the corpus
structure of the [Text Interchange
Format](https://github.com/ropenscilabs/tif), it can easily be used by
other packages that can accept a corpus in data.frame format.

If you only want a named `character` object, **readtext** also defines
an `as.character()` method that inputs its data.frame and returns just
the named character vector of texts, conforming to the TIF definition of
the character version of a corpus.
