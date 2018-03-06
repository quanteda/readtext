context("test ability to load encoded files correctly")

test_that("test readtext encoding parameter: ASCII encoded file, read as UTF-8: (should work)", {
    FILEDIR <- "../data/encoding"

    skip_on_cran()
    skip_on_travis()
    utf8_bytes <- data.table::fread(file.path(FILEDIR, "UTF-8__bytes.tsv"))[[1]]
    expect_that(
        as.numeric(charToRaw(
            texts(readtext(file.path(FILEDIR, "UTF-8__characters.txt"), encoding = "utf-8"),
            ))),
        equals(utf8_bytes)
    )
})

test_that("test readtext encoding parameter: UTF-8 encoded file, read as UTF-16 (should not work)", {
    skip_on_cran()
    skip_on_travis()
    expect_warning(
        misread_texts <- texts(readtext(file.path("../data/encoding", 
                                                  "UTF-8__characters.txt"), 
                                        encoding = "utf-16"))
    )
    utf8_bytes <- data.table::fread(file.path("../data/encoding", 
                                              "UTF-8__bytes.tsv"))[[1]]
    expect_false(
        all(as.numeric(charToRaw(misread_texts)) == utf8_bytes)
    )
})

test_that("test encoding handling (skipped on travis and CRAN", {
    skip_on_cran()
    skip_on_travis()
    skip_on_os("windows")

    # Currently, these encodings don't work for reasons that seem unrelated
    # to quanteda, and are either a problem in base R or on travis-ci
    broken_encodings <- c(
        "437", "850", "852", "855", "857", "860", "861", "862", "863", "865", 
        "869", "BIG5-HKSCS", "CHINESE", "CP1251", "CP1255", "CP1256", "CP1361",
        "CP154", "CP737", "CP858", "CP864", "CP856", "CP932", "CP950", "EUC-JISX0213", 
        "EUC-JP", "EUC-KR", "GB18030", "HEBREW", "HZ","ISO-2022-JP-1", "ISO-2022-JP-2", 
        "ISO-2022-JP-3", "ISO-8859-11", "ISO-IR-166", "KOI8-R",
        "UNICODE-1-1-UTF-7",
        "MACCENTRALEUROPE", "MACCYRILLIC", "MACGREEK", "MACICELAND", "MACTURKISH",
        "MS_KANJI", "SHIFT_JISX0213"
    )
    
    
    FILEDIR <- "../data/encoding"
    
    filenames <- list.files(FILEDIR, "*__characters.txt$")
    parts <- strsplit(gsub(".txt$", "", filenames), "__")
    fileencodings <- sapply(parts, "[", 1)
    
    fileencodings <- fileencodings[!(fileencodings %in% broken_encodings)]
    filenames <- file.path(FILEDIR, paste0(fileencodings,  "__characters.txt"))
    
    for (i in 1:length(fileencodings)) {
        filename <- filenames[[i]]
        encoding <- fileencodings[[i]]
        
        test_that(paste("test readtext encoding parameter, encoding", encoding), {
            characters <- as.numeric(charToRaw(
                texts(readtext(filename, encoding=fileencodings[[i]]))
            ))
            bytes <- data.table::fread(gsub("__characters.txt", "__bytes.tsv", filename))[[1]]
            expect_equal(characters, bytes)
        })
    }
    test_that("Test loading all these files at once with different encodings", {
        encodedreadtxts <- readtext(filenames, encoding = fileencodings)
    })
})
