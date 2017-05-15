# devtools::load_all('~/code/readtext/')
# options(error=traceback)

context("test filename extension case")

test_that("test case for txt files with glob", {
    expect_equal(
        length(texts(readtext( '../data/TXTcaps/*.TXT'))),
        2
    )

    # Expect no warning
    expect_warning( readtext( '../data/TXTcaps/*.TXT'), NA)
})

test_that("test case for csv files with glob", {
    expect_equal(
        length(texts(readtext(
            '../data/CSVcaps/*.CSV', text_field='text'
        ))),
        4
    )
    # Expect no warning
    expect_warning( readtext( '../data/CSVcaps/*.CSV'), NA)
})
