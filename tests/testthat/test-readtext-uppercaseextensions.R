context("test filename extension case")

test_that("test case for txt files with glob", {
    tmp <- readtext( '../data/TXTcaps/*.TXT')
})

test_that("test case for csv files with glob", {
    expect_equal(
        length(texts(readtext(
            '../data/CSVcaps/*.CSV', textfield='text'
        ))),
        4
    )
    expect_equal(
        nrow(docvars(readtext(
            '../data/csv/*.CSV', textfield='text'
        ))),
        4
    )
})
