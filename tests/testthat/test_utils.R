test_that("Test mktemp function",{
    filename <- mktemp()
    expect_true(file.exists(filename))
    filename2 <- mktemp()
    expect_true(file.exists(filename2))
    expect_false(filename == filename2)
    
    # test directory parameter
    dirname <- mktemp(directory=T)
    expect_true(dir.exists(dirname))
    
    # test prefix parameter
    filename <- mktemp(prefix='testprefix')
    expect_equal(
        substr(basename(filename), 1, 10),
        'testprefix'
    )
    
    # test that a new filename will be given if the original already exists
    set.seed(0)
    original_filename <- mktemp()
    set.seed(0)
    new_filename <- mktemp()
    expect_false(original_filename == new_filename)
    expect_true(file.exists(original_filename))
    expect_true(file.exists(new_filename))
    
    
})


test_that("Test catm",{
    expect_output(
        catm('a', 'b'),
        'a b'
    )
})

test_that("Test downloadRemote",{

    expect_error(
        downloadRemote('http://www.google.com/404.txt', ignoreMissing=F)
    )

})
