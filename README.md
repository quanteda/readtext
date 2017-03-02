<!-- README.md is generated from README.Rmd. Please edit that file -->
readtext: Import and handling for plain and formatted text files
================================================================

[![CRAN Version](http://www.r-pkg.org/badges/version/readtext)](https://CRAN.R-project.org/package=readtext) ![Downloads](http://cranlogs.r-pkg.org/badges/readtext) [![Travis-CI Build Status](https://travis-ci.org/kbenoit/readtext.svg?branch=master)](https://travis-ci.org/kbenoit/readtext) [![Build status](https://ci.appveyor.com/api/projects/status/x6dtvh2m7mj3b026/branch/master?svg=true)](https://ci.appveyor.com/project/kbenoit/readtext) [![codecov.io](https://codecov.io/github/kbenoit/readtext/coverage.svg?branch=master)](https://codecov.io/gh/kbenoit/readtext/branch/master)

An R package for reading text files in all their various formats, by Ken Benoit, Paul Nulty, and Adam Obeng. (In alphabetical order, not by order of contribution.)

Introduction
------------

**readtext** is a one-function package that does exactly what it says on the tin: It reads files containing text, along with any associated document-level metadata, which we call "docvars", for document variables. Plain text files do not have docvars, but other forms such as .csv, .tab, .xml, and .json files usually do.

**readtext** accepts filemasks, so that you can specify a pattern to load multiple texts, and these texts can even be of multiple types. **readtext** is smart enough to process them correctly, returning a data.frame with a primary field "text" containing a character vector of the texts, and additional columns of the data.frame as found in the document variables from the source files.

As encoding can also be a challenging issue for those reading in texts, we include functions for diagnosing encodings on a file-by-file basis, and allow you to specify vectorized input encodings to read in file types with individually set (and different) encodings. (All ecnoding functions are handled by the **stringi** package.)

How to Install
--------------

1.  From GitHub

    ``` r
    # devtools packaged required to install quanteda from Github 
    devtools::install_github("kbenoit/quanteda") 
    ```

2.  From CRAN

    (coming soon)

Inter-operability with **quanteda**
-----------------------------------

**readtext** was originally developed in early versions of the [**quanteda**](https://github.com/kbenoit/quanteda) package for the quantitative analysis of textual data. It was spawned from the `textfile()` function from that package, and now lives exclusively in **readtext**. Because **quanteda**'s corpus constructor recognizes the data.frame format returned by `readtext()`, it can construct a corpus directly from a `readtext` object, preserving all docvars and other meta-data.

``` r
require(readtext)
## Loading required package: readtext
require(quanteda)
## Loading required package: quanteda
## quanteda version 0.9.9.29
## Using 7 of 8 cores for parallel computing
## 
## Attaching package: 'quanteda'
## The following object is masked from 'package:utils':
## 
##     View
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
# substring(texts(txts)[1], 1, 80)  # gibberish
# substring(texts(txts)[4], 1, 80)  # hex
# substring(texts(txts)[40], 1, 80) # hex

# read them in again
txts <- readtext(paste0(FILEDIR,  "/", "*.txt"), encoding = fileencodings)
substring(texts(txts)[1], 1, 80)  # English
##                                                  IndianTreaty_English_UTF-16LE.txt 
## "WHEREAS, the Sisseton and Wahpeton Bands of Dakota or Sioux Indians, on the 20th"
substring(texts(txts)[4], 1, 80)  # Arabic, looking good 
##                                                           UDHR_Arabic_MACARABIC.txt 
## "الديباجة\nلما كان الاعتراف بالكرامة المتأصلة في جميع أعضاء الأسرة البشرية وبحقوقه"
substring(texts(txts)[40], 1, 80) # Cyrillic, looking good
##                                                       UDHR_Russian_WINDOWS-1251.txt 
## "Всеобщая декларация прав человека\nПринята и провозглашена резолюцией 217 А (III)"
substring(texts(txts)[7], 1, 80)  # Chinese, looking good
##                                                                                                                           UDHR_Chinese_GB2312.txt 
## "世界人权宣言\n联合国大会一九四八年十二月十日第217A(III)号决议通过并颁布\n\n1948年12月10日，联合国大会通过并颁布《世界人权宣言》。这一具有历史意"
substring(texts(txts)[26], 1, 80) # Hindi, looking good
##                                                         UDHR_Hindi_UTF-8.txt 
## "मानव अधिकारों की सार्वभौम घोषणा\n\n१० दिसम्बर १९४८ को यूनाइटेड नेशन्स की जनरल असेम"

txts <- readtext(paste0(FILEDIR, "/", "*.txt"), 
                 encoding = fileencodings,
                 docvarsfrom = "filenames", 
                 docvarnames = c("document", "language", "inputEncoding"))
encodingCorpus <- corpus(txts, textField = "texts", 
                         source = "Created by encoding-tests.R") 
## Warning in corpus.character(x[, text_fieldi], docvars = x[, -text_fieldi, :
## Arguments textFieldsource not used.
summary(encodingCorpus)
## Corpus consisting of 41 documents.
## 
##                                Text Types Tokens Sentences     document
##   IndianTreaty_English_UTF-16LE.txt   690   2938       155 IndianTreaty
##  IndianTreaty_English_UTF-8-BOM.txt   646   3104       154 IndianTreaty
##          UDHR_Arabic_ISO-8859-6.txt   753   1555        86         UDHR
##           UDHR_Arabic_MACARABIC.txt   753   1555        86         UDHR
##               UDHR_Arabic_UTF-8.txt   753   1555        86         UDHR
##        UDHR_Arabic_WINDOWS-1256.txt   753   1555        86         UDHR
##             UDHR_Chinese_GB2312.txt   596   1840        62         UDHR
##                UDHR_Chinese_GBK.txt   596   1840        62         UDHR
##              UDHR_Chinese_UTF-8.txt   596   1840        62         UDHR
##           UDHR_English_UTF-16BE.txt   559   1916        61         UDHR
##           UDHR_English_UTF-16LE.txt   559   1916        61         UDHR
##              UDHR_English_UTF-8.txt   576   1943        63         UDHR
##       UDHR_English_WINDOWS-1252.txt   576   1943        63         UDHR
##          UDHR_French_ISO-8859-1.txt   670   2132        58         UDHR
##            UDHR_French_MACROMAN.txt   670   2132        58         UDHR
##               UDHR_French_UTF-8.txt   670   2132        58         UDHR
##        UDHR_French_WINDOWS-1252.txt   670   2132        58         UDHR
##          UDHR_German_ISO-8859-1.txt   661   1831        61         UDHR
##            UDHR_German_MACROMAN.txt   661   1831        61         UDHR
##               UDHR_German_UTF-8.txt   661   1831        61         UDHR
##        UDHR_German_WINDOWS-1252.txt   661   1831        61         UDHR
##               UDHR_Greek_CP1253.txt   735   2193       106         UDHR
##           UDHR_Greek_ISO-8859-7.txt   735   2193       106         UDHR
##             UDHR_Greek_MACGREEK.txt   735   2193       106         UDHR
##                UDHR_Greek_UTF-8.txt   735   2193       106         UDHR
##                UDHR_Hindi_UTF-8.txt   661   2388       104         UDHR
##       UDHR_Icelandic_ISO-8859-1.txt   725   1887       106         UDHR
##            UDHR_Icelandic_UTF-8.txt   725   1887       106         UDHR
##     UDHR_Icelandic_WINDOWS-1252.txt   725   1887       106         UDHR
##             UDHR_Japanese_CP932.txt   578   2482        65         UDHR
##       UDHR_Japanese_ISO-2022-JP.txt   578   2482        65         UDHR
##             UDHR_Japanese_UTF-8.txt   578   2482        65         UDHR
##       UDHR_Japanese_WINDOWS-936.txt   578   2482        65         UDHR
##         UDHR_Korean_ISO-2022-KR.txt   661   1322        65         UDHR
##               UDHR_Korean_UTF-8.txt   661   1322        65         UDHR
##         UDHR_Russian_ISO-8859-5.txt   762   1829        62         UDHR
##             UDHR_Russian_KOI8-R.txt   762   1829        62         UDHR
##        UDHR_Russian_MACCYRILLIC.txt   762   1829        62         UDHR
##              UDHR_Russian_UTF-8.txt   762   1829        62         UDHR
##       UDHR_Russian_WINDOWS-1251.txt   762   1829        62         UDHR
##                 UDHR_Thai_UTF-8.txt   541   2404        34         UDHR
##   language inputEncoding
##    English      UTF-16LE
##    English     UTF-8-BOM
##     Arabic    ISO-8859-6
##     Arabic     MACARABIC
##     Arabic         UTF-8
##     Arabic  WINDOWS-1256
##    Chinese        GB2312
##    Chinese           GBK
##    Chinese         UTF-8
##    English      UTF-16BE
##    English      UTF-16LE
##    English         UTF-8
##    English  WINDOWS-1252
##     French    ISO-8859-1
##     French      MACROMAN
##     French         UTF-8
##     French  WINDOWS-1252
##     German    ISO-8859-1
##     German      MACROMAN
##     German         UTF-8
##     German  WINDOWS-1252
##      Greek        CP1253
##      Greek    ISO-8859-7
##      Greek      MACGREEK
##      Greek         UTF-8
##      Hindi         UTF-8
##  Icelandic    ISO-8859-1
##  Icelandic         UTF-8
##  Icelandic  WINDOWS-1252
##   Japanese         CP932
##   Japanese   ISO-2022-JP
##   Japanese         UTF-8
##   Japanese   WINDOWS-936
##     Korean   ISO-2022-KR
##     Korean         UTF-8
##    Russian    ISO-8859-5
##    Russian        KOI8-R
##    Russian   MACCYRILLIC
##    Russian         UTF-8
##    Russian  WINDOWS-1251
##       Thai         UTF-8
## 
## Source:  /Users/kbenoit/GitHub/readtext/* on x86_64 by kbenoit
## Created: Thu Mar  2 08:46:50 2017
## Notes:
```

Piping works too:

``` r
require(magrittr)
## Loading required package: magrittr
readtext(paste0(FILEDIR,  "/", "*.txt"), encoding = fileencodings) %>%
    corpus(textField = "texts") %>% 
        summary
## Warning in corpus.character(x[, text_fieldi], docvars = x[, -text_fieldi, :
## Argument textField not used.
## Corpus consisting of 41 documents.
## 
##                                Text Types Tokens Sentences
##   IndianTreaty_English_UTF-16LE.txt   690   2938       155
##  IndianTreaty_English_UTF-8-BOM.txt   646   3104       154
##          UDHR_Arabic_ISO-8859-6.txt   753   1555        86
##           UDHR_Arabic_MACARABIC.txt   753   1555        86
##               UDHR_Arabic_UTF-8.txt   753   1555        86
##        UDHR_Arabic_WINDOWS-1256.txt   753   1555        86
##             UDHR_Chinese_GB2312.txt   596   1840        62
##                UDHR_Chinese_GBK.txt   596   1840        62
##              UDHR_Chinese_UTF-8.txt   596   1840        62
##           UDHR_English_UTF-16BE.txt   559   1916        61
##           UDHR_English_UTF-16LE.txt   559   1916        61
##              UDHR_English_UTF-8.txt   576   1943        63
##       UDHR_English_WINDOWS-1252.txt   576   1943        63
##          UDHR_French_ISO-8859-1.txt   670   2132        58
##            UDHR_French_MACROMAN.txt   670   2132        58
##               UDHR_French_UTF-8.txt   670   2132        58
##        UDHR_French_WINDOWS-1252.txt   670   2132        58
##          UDHR_German_ISO-8859-1.txt   661   1831        61
##            UDHR_German_MACROMAN.txt   661   1831        61
##               UDHR_German_UTF-8.txt   661   1831        61
##        UDHR_German_WINDOWS-1252.txt   661   1831        61
##               UDHR_Greek_CP1253.txt   735   2193       106
##           UDHR_Greek_ISO-8859-7.txt   735   2193       106
##             UDHR_Greek_MACGREEK.txt   735   2193       106
##                UDHR_Greek_UTF-8.txt   735   2193       106
##                UDHR_Hindi_UTF-8.txt   661   2388       104
##       UDHR_Icelandic_ISO-8859-1.txt   725   1887       106
##            UDHR_Icelandic_UTF-8.txt   725   1887       106
##     UDHR_Icelandic_WINDOWS-1252.txt   725   1887       106
##             UDHR_Japanese_CP932.txt   578   2482        65
##       UDHR_Japanese_ISO-2022-JP.txt   578   2482        65
##             UDHR_Japanese_UTF-8.txt   578   2482        65
##       UDHR_Japanese_WINDOWS-936.txt   578   2482        65
##         UDHR_Korean_ISO-2022-KR.txt   661   1322        65
##               UDHR_Korean_UTF-8.txt   661   1322        65
##         UDHR_Russian_ISO-8859-5.txt   762   1829        62
##             UDHR_Russian_KOI8-R.txt   762   1829        62
##        UDHR_Russian_MACCYRILLIC.txt   762   1829        62
##              UDHR_Russian_UTF-8.txt   762   1829        62
##       UDHR_Russian_WINDOWS-1251.txt   762   1829        62
##                 UDHR_Thai_UTF-8.txt   541   2404        34
## 
## Source:  /Users/kbenoit/GitHub/readtext/* on x86_64 by kbenoit
## Created: Thu Mar  2 08:46:51 2017
## Notes:
```
