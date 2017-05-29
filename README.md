<!-- README.md is generated from README.Rmd. Please edit that file -->
readtext: Import and handling for plain and formatted text files
================================================================

[![CRAN Version](http://www.r-pkg.org/badges/version/readtext)](https://CRAN.R-project.org/package=readtext) ![Downloads](http://cranlogs.r-pkg.org/badges/readtext) [![Travis-CI Build Status](https://travis-ci.org/kbenoit/readtext.svg?branch=master)](https://travis-ci.org/kbenoit/readtext) [![Build status](https://ci.appveyor.com/api/projects/status/x6dtvh2m7mj3b026/branch/master?svg=true)](https://ci.appveyor.com/project/kbenoit/readtext) [![codecov.io](https://codecov.io/github/kbenoit/readtext/coverage.svg?branch=master)](https://codecov.io/gh/kbenoit/readtext/branch/master)

An R package for reading text files in all their various formats, by Ken Benoit, Adam Obeng, Paul Nulty, and Stefan Müller.

Introduction
------------

**readtext** is a one-function package that does exactly what it says on the tin: It reads files containing text, along with any associated document-level metadata, which we call "docvars", for document variables. Plain text files do not have docvars, but other forms such as .csv, .tab, .xml, and .json files usually do.

**readtext** accepts filemasks, so that you can specify a pattern to load multiple texts, and these texts can even be of multiple types. **readtext** is smart enough to process them correctly, returning a data.frame with a primary field "text" containing a character vector of the texts, and additional columns of the data.frame as found in the document variables from the source files.

As encoding can also be a challenging issue for those reading in texts, we include functions for diagnosing encodings on a file-by-file basis, and allow you to specify vectorized input encodings to read in file types with individually set (and different) encodings. (All encoding functions are handled by the **stringi** package.)

How to Install
--------------

1.  From CRAN

    ``` r
    install.packages("readtext")
    ```

2.  From GitHub, if you want the latest development version.

    ``` r
    # devtools packaged required to install readtext from Github 
    devtools::install_github("kbenoit/readtext") 
    ```

Demonstration: Reading one or more text files
---------------------------------------------

**readtext** supports plain text files (.txt), data in some form of JavaScript Object Notation (.json), comma-or tab-separated values (.csv, .tab, .tsv), XML documents (.xml), as well as PDF and Microsoft Word formatted files (.pdf, .doc, .docx). **readtext** also handles multiple files and file types using for instance a "glob" expression, files from a URL or an archive file (.zip, .tar, .tar.gz, .tar.bz).

The file formats are determined automatically by the filename extensions. If a file has no extension or is unknown, **readtext** will assume that it is plain text. The following command, for instance, will load in all of the files from the subdirectory `txt/UDHR/`:

``` r
require(readtext)
## Loading required package: readtext
# get the data directory from readtext
DATA_DIR <- system.file("extdata/", package = "readtext")

# read in all files from a folder
readtext(paste0(DATA_DIR, "/txt/UDHR/*"))
## readtext object consisting of 13 documents and 0 docvars.
## # data.frame [13 x 2]
##              doc_id                          text
##               <chr>                         <chr>
## 1  UDHR_chinese.txt "\"世界人权宣言\n联合国\"..."
## 2    UDHR_czech.txt           "\"VŠEOBECNÁ \"..."
## 3   UDHR_danish.txt           "\"Den 10. de\"..."
## 4  UDHR_english.txt           "\"Universal \"..."
## 5   UDHR_french.txt           "\"Déclaratio\"..."
## 6 UDHR_georgian.txt           "\"FLFVBFYBC \"..."
## # ... with 7 more rows
```

For files that contain multiple documents, such as comma-separated-value documents, you will need to specify the column name containing the texts, using the `text_field` argument:

``` r
# read in comma-separated values and specify text field
readtext(paste0(DATA_DIR, "/csv/inaugCorpus.csv"), text_field = "texts")
## readtext object consisting of 5 documents and 3 docvars.
## # data.frame [5 x 5]
##              doc_id                text  Year  President FirstName
##               <chr>               <chr> <int>      <chr>     <chr>
## 1 inaugCorpus.csv.1 "\"Fellow-Cit\"..."  1789 Washington    George
## 2 inaugCorpus.csv.2 "\"Fellow cit\"..."  1793 Washington    George
## 3 inaugCorpus.csv.3 "\"When it wa\"..."  1797      Adams      John
## 4 inaugCorpus.csv.4 "\"Friends an\"..."  1801  Jefferson    Thomas
## 5 inaugCorpus.csv.5 "\"Proceeding\"..."  1805  Jefferson    Thomas
```

For a more complete demonstration, see the package [vignette](http://cdn.rawgit.com/kbenoit/readtext/master/inst/doc/readtext_vignette.html).

Inter-operability with **quanteda**
-----------------------------------

**readtext** was originally developed in early versions of the [**quanteda**](http:/github.com/kbenoit/quanteda) package for the quantitative analysis of textual data. Because **quanteda**'s corpus constructor recognizes the data.frame format returned by `readtext()`, it can construct a corpus directly from a readtext object, preserving all docvars and other meta-data.

``` r
require(quanteda)
# read in comma-separated values with readtext
rt_csv <- readtext(paste0(DATA_DIR, "/csv/inaugCorpus.csv"), text_field = "texts")
# create quanteda corpus
corpus_csv <- corpus(rt_csv)
summary(corpus_csv, 5)
```
