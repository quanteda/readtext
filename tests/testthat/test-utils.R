test_that("Test readtext:::get_temp function for test dirs", {
    filename <- readtext:::get_temp()
    filename2 <- readtext:::get_temp()
    expect_false(filename == filename2)
    
    # test directory parameter
    dirname <- readtext:::get_temp(directory = TRUE)
    expect_true(dir.exists(dirname))

    # test prefix parameter
    filename <- readtext:::get_temp(prefix = "testprefix")
    expect_equal(
        substr(basename(filename), 1, 10),
        "testprefix"
    )

    # test that a new filename will be given if the original already exists
    org_filename <- readtext:::get_temp()
    new_filename <- readtext:::get_temp()
    expect_false(org_filename == new_filename)
    
    # file names are the same when seed is given
    org_filename2 <- readtext:::get_temp(seed = 'xyz')
    new_filename2 <- readtext:::get_temp(seed = 'xyz')
    expect_true(org_filename2 == new_filename2)
})

test_that("Test is_probably_xpath", {
    expect_false(is_probably_xpath("A"))
    expect_false(is_probably_xpath("a:what"))
    expect_true(is_probably_xpath("/A/B/C"))
    expect_true(is_probably_xpath("A/B/C"))
})

test_that("Test readtext:::get_docvars_filenames for parsing filenames", {
    filenames <- c("~/tmp/documents/USA_blue_horse.txt",
                   "~/tmp/documents/France_green_dog.txt",
                   "~/tmp/documents/China_red_dragon.txt",
                   "~/tmp/spaced words/Ireland_black_bear.txt")
    df <- readtext:::get_docvars_filenames(filenames,
                                           docvarnames = c("country", "color",
                                                           "animal"),
                                           verbosity = 2)
    expect_equal(df$animal,
                 c("horse", "dog", "dragon", "bear"))
    expect_equal(names(df), c("country", "color", "animal"))
    expect_s3_class(df, "data.frame")
})


test_that("file_ext returns expected extensions", {
    filenames <- c("~/tmp/documents/USA_blue_horse.txt",
                   "~/tmp/documents/France_green_dog.csv",
                   "~/tmp/documents/China_red_dragon.json",
                   "~/tmp/spaced words/Ireland_black_bear.tar.gz")
    expect_equal(readtext:::file_ext(filenames),
                 c("txt", "csv", "json", "gz"))
})


test_that("Test download_remote", {
    expect_error(
        download_remote("http://www.google.com/404.txt", ignore_missing = FALSE)
    )

})
