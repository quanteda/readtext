test_that("Test readtext:::mktemp function for test dirs",{
    filename <- readtext:::mktemp()
    expect_true(file.exists(filename))
    filename2 <- readtext:::mktemp()
    expect_true(file.exists(filename2))
    expect_false(filename == filename2)
    
    # test directory parameter
    dirname <- readtext:::mktemp(directory=T)
    expect_true(dir.exists(dirname))
    
    # test prefix parameter
    filename <- readtext:::mktemp(prefix='testprefix')
    expect_equal(
        substr(basename(filename), 1, 10),
        'testprefix'
    )
    
    # test that a new filename will be given if the original already exists
    set.seed(0)
    original_filename <- readtext:::mktemp()
    set.seed(0)
    new_filename <- readtext:::mktemp()
    expect_false(original_filename == new_filename)
    expect_true(file.exists(original_filename))
    expect_true(file.exists(new_filename))
    
    
})

test_that("Test is_probably_xpath",{
    expect_false(is_probably_xpath('A'))
    expect_false(is_probably_xpath('a:what'))
    expect_true(is_probably_xpath('/A/B/C'))
    expect_true(is_probably_xpath('A/B/C'))
})



test_that("Test readtext:::getdocvarsFromFilenames for parsing filenames", {
    
    filenames <- c("~/tmp/documents/USA_blue_horse.txt",
                   "~/tmp/documents/France_green_dog.txt",
                   "~/tmp/documents/China_red_dragon.txt",
                   "~/tmp/spaced words/Ireland_black_bear.txt")
    df <- readtext:::getdocvarsFromFilenames(filenames, 
                                             docvarnames = c("country", "color", "animal"))
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


test_that("Test downloadRemote",{

    expect_error(
        downloadRemote('http://www.google.com/404.txt', ignoreMissing=F)
    )

})
