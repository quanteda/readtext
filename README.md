<!-- README.md is generated from README.Rmd. Please edit that file -->
readtext: Import and handling for plain and formatted text files
================================================================

[![CRAN Version](http://www.r-pkg.org/badges/version/readtext)](https://CRAN.R-project.org/package=readtext) ![Downloads](http://cranlogs.r-pkg.org/badges/readtext) [![Travis-CI Build Status](https://travis-ci.org/kbenoit/readtext.svg?branch=master)](https://travis-ci.org/kbenoit/readtext) [![Build status](https://ci.appveyor.com/api/projects/status/x6dtvh2m7mj3b026/branch/master?svg=true)](https://ci.appveyor.com/project/kbenoit/readtext) [![codecov.io](https://codecov.io/github/kbenoit/readtext/coverage.svg?branch=master)](https://codecov.io/gh/kbenoit/readtext/branch/master)

An R package for reading text files in all their various formats, by Ken Benoit, Adam Obeng, and Paul Nulty.

Introduction
------------

**readtext** is a one-function package that does exactly what it says on the tin: It reads files containing text, along with any associated document-level metadata, which we call "docvars", for document variables. Plain text files do not have docvars, but other forms such as .csv, .tab, .xml, and .json files usually do.

**readtext** accepts filemasks, so that you can specify a pattern to load multiple texts, and these texts can even be of multiple types. **readtext** is smart enough to process them correctly, returning a data.frame with a primary field "text" containing a character vector of the texts, and additional columns of the data.frame as found in the document variables from the source files.

As encoding can also be a challenging issue for those reading in texts, we include functions for diagnosing encodings on a file-by-file basis, and allow you to specify vectorized input encodings to read in file types with individually set (and different) encodings. (All ecnoding functions are handled by the **stringi** package.)

How to Install
--------------

1.  From GitHub

    ``` r
    # devtools packaged required to install readtext from Github 
    devtools::install_github("kbenoit/readtext") 
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
## quanteda version 0.9.9.57
## Using 3 of 4 cores for parallel computing
## 
## Attaching package: 'quanteda'
## The following object is masked from 'package:utils':
## 
##     View

# set verbosity level to 2
options(readtext_verbosity = 2)

FILEDIR <- tempdir()
unzip(system.file("extdata", "data_files_encodedtexts.zip", package = "readtext"), exdir = FILEDIR)

# get encoding from filename
filenames <- list.files(FILEDIR, "\\.txt$")
# strip the extension
filenames <- gsub(".txt$", "", filenames)
parts <- strsplit(filenames, "_")
fileencodings <- sapply(parts, "[", 3)
fileencodings
##  [1] "UTF-16LE"     "UTF-8-BOM"    "ISO-8859-6"   "UTF-8"       
##  [5] "WINDOWS-1256" "GB2312"       "GBK"          "UTF-8"       
##  [9] "UTF-16BE"     "UTF-16LE"     "UTF-8"        "WINDOWS-1252"
## [13] "ISO-8859-1"   "UTF-8"        "WINDOWS-1252" "ISO-8859-1"  
## [17] "UTF-8"        "WINDOWS-1252" "CP1253"       "ISO-8859-7"  
## [21] "UTF-8"        "UTF-8"        "ISO-8859-1"   "UTF-8"       
## [25] "WINDOWS-1252" "CP932"        "ISO-2022-JP"  "UTF-8"       
## [29] "WINDOWS-936"  "ISO-2022-KR"  "UTF-8"        "ISO-8859-5"  
## [33] "KOI8-R"       "UTF-8"        "WINDOWS-1251" "UTF-8"

# find out which conversions are unavailable (through iconv())
cat("Encoding conversions not available for this platform:")
## Encoding conversions not available for this platform:
notAvailableIndex <- which(!(fileencodings %in% iconvlist()))
fileencodings[notAvailableIndex]
## [1] "UTF-8-BOM"

# read in some text files
# try readtext
txts <- readtext(paste0(FILEDIR, "/", "*.txt"))
## Reading texts from /var/folders/46/zfn6gwj15d3_n6dhyy1cvwc00000gp/T//
## RtmpmFk3IF/*.txt
## 
##  ... read 36 documents.
# substring(texts(txts)[1], 1, 80)  # gibberish
# substring(texts(txts)[4], 1, 80)  # hex
# substring(texts(txts)[40], 1, 80) # hex

# read them in again
txts <- readtext(paste0(FILEDIR,  "/", "*.txt"), encoding = fileencodings)
## Reading texts from /var/folders/46/zfn6gwj15d3_n6dhyy1cvwc00000gp/T//RtmpmFk3IF/*.txt
##  ... read 36 documents.
substring(texts(txts)[1], 1, 80)  # English
##                                                                                  1 
## "WHEREAS, the Sisseton and Wahpeton Bands of Dakota or Sioux Indians, on the 20th"
substring(texts(txts)[4], 1, 80)  # Arabic, looking good 
##                                                                                   4 
## "الديباجة\nلما كان الاعتراف بالكرامة المتأصلة في جميع أعضاء الأسرة البشرية وبحقوقه"
substring(texts(txts)[40], 1, 80) # Cyrillic, looking good
## <NA> 
##   NA
substring(texts(txts)[7], 1, 80)  # Chinese, looking good
##                                                                                                                                                 7 
## "世界人权宣言\n联合国大会一九四八年十二月十日第217A(III)号决议通过并颁布\n\n1948年12月10日，联合国大会通过并颁布《世界人权宣言》。这一具有历史意"
substring(texts(txts)[26], 1, 80) # Hindi, looking good
##                                                                                                                                                    26 
## "『世界人権宣言』\n \n\n（1948.12.10 第３回国連総会採択）\n\n \n\n〈前文〉\n　\n人類社会のすべての構成員の固有の尊厳と平等で譲ることのできない権利と"

txts <- readtext(paste0(FILEDIR, "/", "*.txt"), 
                 encoding = fileencodings,
                 docvarsfrom = "filenames", 
                 docvarnames = c("document", "language", "inputEncoding"))
## Reading texts from /var/folders/46/zfn6gwj15d3_n6dhyy1cvwc00000gp/T//RtmpmFk3IF/*.txt
##  ... read 36 documents.
encodingCorpus <- corpus(txts, textField = "texts", 
                         source = "Created by encoding-tests.R") 
## Warning in corpus.character(x[, text_fieldi], docvars = x[, -text_fieldi, :
## Arguments textFieldsource not used.
summary(encodingCorpus)
## Corpus consisting of 36 documents.
## 
##    Text Types Tokens Sentences                             doc_id
##   text1   690   2938       155  IndianTreaty_English_UTF-16LE.txt
##   text2   646   3104       154 IndianTreaty_English_UTF-8-BOM.txt
##   text3   753   1555        86         UDHR_Arabic_ISO-8859-6.txt
##   text4   753   1555        86              UDHR_Arabic_UTF-8.txt
##   text5   753   1555        86       UDHR_Arabic_WINDOWS-1256.txt
##   text6   596   1840        62            UDHR_Chinese_GB2312.txt
##   text7   596   1840        62               UDHR_Chinese_GBK.txt
##   text8   596   1840        62             UDHR_Chinese_UTF-8.txt
##   text9   559   1916        61          UDHR_English_UTF-16BE.txt
##  text10   559   1916        61          UDHR_English_UTF-16LE.txt
##  text11   576   1943        63             UDHR_English_UTF-8.txt
##  text12   576   1943        63      UDHR_English_WINDOWS-1252.txt
##  text13   670   2132        58         UDHR_French_ISO-8859-1.txt
##  text14   670   2132        58              UDHR_French_UTF-8.txt
##  text15   670   2132        58       UDHR_French_WINDOWS-1252.txt
##  text16   661   1831        61         UDHR_German_ISO-8859-1.txt
##  text17   661   1831        61              UDHR_German_UTF-8.txt
##  text18   661   1831        61       UDHR_German_WINDOWS-1252.txt
##  text19   735   2193       106              UDHR_Greek_CP1253.txt
##  text20   735   2193       106          UDHR_Greek_ISO-8859-7.txt
##  text21   735   2193       106               UDHR_Greek_UTF-8.txt
##  text22   661   2388       104               UDHR_Hindi_UTF-8.txt
##  text23   725   1887       106      UDHR_Icelandic_ISO-8859-1.txt
##  text24   725   1887       106           UDHR_Icelandic_UTF-8.txt
##  text25   725   1887       106    UDHR_Icelandic_WINDOWS-1252.txt
##  text26   578   2482        65            UDHR_Japanese_CP932.txt
##  text27   578   2482        65      UDHR_Japanese_ISO-2022-JP.txt
##  text28   578   2482        65            UDHR_Japanese_UTF-8.txt
##  text29   578   2482        65      UDHR_Japanese_WINDOWS-936.txt
##  text30   661   1322        65        UDHR_Korean_ISO-2022-KR.txt
##  text31   661   1322        65              UDHR_Korean_UTF-8.txt
##  text32   762   1829        62        UDHR_Russian_ISO-8859-5.txt
##  text33   762   1829        62            UDHR_Russian_KOI8-R.txt
##  text34   762   1829        62             UDHR_Russian_UTF-8.txt
##  text35   762   1829        62      UDHR_Russian_WINDOWS-1251.txt
##  text36   541   2404        34                UDHR_Thai_UTF-8.txt
##      document  language inputEncoding
##  IndianTreaty   English      UTF-16LE
##  IndianTreaty   English     UTF-8-BOM
##          UDHR    Arabic    ISO-8859-6
##          UDHR    Arabic         UTF-8
##          UDHR    Arabic  WINDOWS-1256
##          UDHR   Chinese        GB2312
##          UDHR   Chinese           GBK
##          UDHR   Chinese         UTF-8
##          UDHR   English      UTF-16BE
##          UDHR   English      UTF-16LE
##          UDHR   English         UTF-8
##          UDHR   English  WINDOWS-1252
##          UDHR    French    ISO-8859-1
##          UDHR    French         UTF-8
##          UDHR    French  WINDOWS-1252
##          UDHR    German    ISO-8859-1
##          UDHR    German         UTF-8
##          UDHR    German  WINDOWS-1252
##          UDHR     Greek        CP1253
##          UDHR     Greek    ISO-8859-7
##          UDHR     Greek         UTF-8
##          UDHR     Hindi         UTF-8
##          UDHR Icelandic    ISO-8859-1
##          UDHR Icelandic         UTF-8
##          UDHR Icelandic  WINDOWS-1252
##          UDHR  Japanese         CP932
##          UDHR  Japanese   ISO-2022-JP
##          UDHR  Japanese         UTF-8
##          UDHR  Japanese   WINDOWS-936
##          UDHR    Korean   ISO-2022-KR
##          UDHR    Korean         UTF-8
##          UDHR   Russian    ISO-8859-5
##          UDHR   Russian        KOI8-R
##          UDHR   Russian         UTF-8
##          UDHR   Russian  WINDOWS-1251
##          UDHR      Thai         UTF-8
## 
## Source:  /Users/kbenoit/Dropbox (Personal)/GitHub/readtext/* on x86_64 by kbenoit
## Created: Wed May 17 11:48:23 2017
## Notes:
```

Piping works too:

``` r
require(magrittr)
## Loading required package: magrittr
readtext(paste0(FILEDIR,  "/", "*.txt"), encoding = fileencodings) %>%
    corpus(textField = "texts") %>% 
        summary
## Reading texts from /var/folders/46/zfn6gwj15d3_n6dhyy1cvwc00000gp/T//
## RtmpmFk3IF/*.txt
## 
##  ... read 36 documents.
## Warning in corpus.character(x[, text_fieldi], docvars = x[, -text_fieldi, :
## Argument textField not used.
## Corpus consisting of 36 documents.
## 
##    Text Types Tokens Sentences                             doc_id
##   text1   690   2938       155  IndianTreaty_English_UTF-16LE.txt
##   text2   646   3104       154 IndianTreaty_English_UTF-8-BOM.txt
##   text3   753   1555        86         UDHR_Arabic_ISO-8859-6.txt
##   text4   753   1555        86              UDHR_Arabic_UTF-8.txt
##   text5   753   1555        86       UDHR_Arabic_WINDOWS-1256.txt
##   text6   596   1840        62            UDHR_Chinese_GB2312.txt
##   text7   596   1840        62               UDHR_Chinese_GBK.txt
##   text8   596   1840        62             UDHR_Chinese_UTF-8.txt
##   text9   559   1916        61          UDHR_English_UTF-16BE.txt
##  text10   559   1916        61          UDHR_English_UTF-16LE.txt
##  text11   576   1943        63             UDHR_English_UTF-8.txt
##  text12   576   1943        63      UDHR_English_WINDOWS-1252.txt
##  text13   670   2132        58         UDHR_French_ISO-8859-1.txt
##  text14   670   2132        58              UDHR_French_UTF-8.txt
##  text15   670   2132        58       UDHR_French_WINDOWS-1252.txt
##  text16   661   1831        61         UDHR_German_ISO-8859-1.txt
##  text17   661   1831        61              UDHR_German_UTF-8.txt
##  text18   661   1831        61       UDHR_German_WINDOWS-1252.txt
##  text19   735   2193       106              UDHR_Greek_CP1253.txt
##  text20   735   2193       106          UDHR_Greek_ISO-8859-7.txt
##  text21   735   2193       106               UDHR_Greek_UTF-8.txt
##  text22   661   2388       104               UDHR_Hindi_UTF-8.txt
##  text23   725   1887       106      UDHR_Icelandic_ISO-8859-1.txt
##  text24   725   1887       106           UDHR_Icelandic_UTF-8.txt
##  text25   725   1887       106    UDHR_Icelandic_WINDOWS-1252.txt
##  text26   578   2482        65            UDHR_Japanese_CP932.txt
##  text27   578   2482        65      UDHR_Japanese_ISO-2022-JP.txt
##  text28   578   2482        65            UDHR_Japanese_UTF-8.txt
##  text29   578   2482        65      UDHR_Japanese_WINDOWS-936.txt
##  text30   661   1322        65        UDHR_Korean_ISO-2022-KR.txt
##  text31   661   1322        65              UDHR_Korean_UTF-8.txt
##  text32   762   1829        62        UDHR_Russian_ISO-8859-5.txt
##  text33   762   1829        62            UDHR_Russian_KOI8-R.txt
##  text34   762   1829        62             UDHR_Russian_UTF-8.txt
##  text35   762   1829        62      UDHR_Russian_WINDOWS-1251.txt
##  text36   541   2404        34                UDHR_Thai_UTF-8.txt
## 
## Source:  /Users/kbenoit/Dropbox (Personal)/GitHub/readtext/* on x86_64 by kbenoit
## Created: Wed May 17 11:48:24 2017
## Notes:
```
