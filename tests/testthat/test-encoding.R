context("test encoding function")

test_that("test encoding function on simple text files", {
  skip_on_appveyor()
  skip_on_os("windows")
  skip_on_cran()
  cat("This is UTF-8 aęiíoõuü 日本語", file = (tempf <- tempfile(fileext = ".txt")))
    rt <- readtext(tempf)
    enc <- encoding(rt, verbose = FALSE)
    expect_equal(
        enc,
        list(probably = "UTF-8", all = "UTF-8")
    )

    expect_equal(
        enc,
        list(probably = "UTF-8", all = "UTF-8")
    )

    expect_message(
        encoding(rt, verbose = TRUE),
        "Probable encoding: UTF-8"
    )

    rt <- readtext(tempf, encoding = "UTF-8")
})

test_that("test encoding function on simple text files, Russian 8-bit encoding", {
    cat(iconv("Это предложение на русском языке.", from = "UTF-8", to = "KOI8-R"),
              file = (tempf <- tempfile(fileext = ".txt")))
    rt <- readtext(tempf)
    enc <- encoding(rt, verbose = FALSE)
    expect_equal(
        enc,
        list(probably = "KOI8-R", all = "KOI8-R")
    )

    # converting
    rt <- readtext(tempf, encoding = "KOI8-R")
    enc <- encoding(rt, verbose = FALSE)
    expect_equal(
        enc,
        list(probably = "UTF-8", all = "UTF-8")
    )
})
