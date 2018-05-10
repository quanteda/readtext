context("test readtext_options")

test_that("readtext_options initialize works correctly", {
    readtext_options(verbosity = 4)
    expect_equal(readtext_options("verbosity"), 4)
    readtext_options(initialize = TRUE)
    expect_equal(readtext_options("verbosity"), 4)
    readtext_options(reset = TRUE)
    expect_equal(readtext_options("verbosity"), 1)
})

test_that("readtext_options returns an error for non-existing options", {
    expect_error(
        readtext_options(notanoption = TRUE),
        "notanoption is not a valid readtext option"
    )
    expect_equal(
        readtext_options("notanoption"),
        NULL
    )
})

test_that("readtext_options works correctly to set options", {
    readtext_options(verbosity = 2)
    expect_equal(
        readtext_options("verbosity"),
        getOption("readtext_verbosity")
    )
})

test_that("readtext functions work if package is not attached (#864)", {
    skip("skipping test of option setting when readtext is not attached")
    DATA_DIR <- system.file("extdata/", package = "readtext")
    detach("package:readtext", unload = TRUE)
    expect_is(
        readtext::readtext(paste0(DATA_DIR, "csv/inaugCorpus.csv"), verbosity = 0),
        "data.frame"
    )
    library("readtext")
})

test_that("readtext_options reset works correctly", {
    readtext_options(reset = TRUE)
    opts <- readtext:::get_options_default()
    expect_equal(
        readtext_options(),
        opts
    )
})
