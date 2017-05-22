# TODO: re-do docs
# TODO: Check and remove extranous codes # TODO: recurse file listing for e.g. remote ZIP file
# TODO: readtext with csv doesn't seem to require text_field


context('test readtext.R')

test_that("test readtext with single filename", {
    fox <- c(fox.txt = "The quick brown fox jumps over the lazy dog.")
    expect_equal(
        texts(readtext('../data/fox/fox.txt')),
        fox
    )
})


test_that("test readtext with vector of filenames", {
    expect_equal(
        length(texts(readtext(
            c(
                '../data/fruits/apple.txt',
                '../data/fruits/orange.txt'
            )
        ))),
        2
    )
})

test_that("test readtext with glob-style mask", {
    expect_equal(
        length(texts(readtext(
            '../data/glob/*.txt'
        ))),
        5
    )
    
    expect_equal(
        length(texts(readtext(
            '../data/glob/?.txt'
        ))),
        4
    )
    
    
    # Glob in non-last part of filename
    expect_equal(
        length(texts(readtext(
            '../data/glob/*/test.txt'
        ))),
        2
    )
    
    # It would be nice to be able to test that escaped glob doesn't special 
    # characters in filename, but R check won't allow a file of this name to
    # exist in the package... This should still pass if run the test manually
    # (having created the file, supposing your platform supports it)
    #  expect_equal(
    #      length(texts(readtext(
    #              '../data/glob/special/\\*.txt'
    #      ))),
    #      1
    #  )
    
    
})

test_that("test structured readtext with glob-style mask", {
    expect_equal(
        length(texts(readtext(
            '../data/csv/*.csv', text_field='text'
        ))),
        4
    )
    expect_equal(
        nrow(docvars(readtext(
            '../data/csv/*.csv', text_field='text'
        ))),
        4
    )
})


test_that("test remote text file", {
    expect_equal(
        texts(readtext('https://raw.githubusercontent.com/kbenoit/readtext/master/tests/data/fox/fox.txt')),
        c(fox.txt='The quick brown fox jumps over the lazy dog.')
    )
    # ignore_missing_files with an existing file should make no difference
    expect_equal(
        texts(readtext('https://raw.githubusercontent.com/kbenoit/readtext/master/tests/data/fox/fox.txt', 
                       ignore_missing_files = TRUE)),
        c(fox.txt='The quick brown fox jumps over the lazy dog.')
    )
})


test_that("test remote csv file", {
    expect_equal(
        texts(readtext("https://raw.githubusercontent.com/kbenoit/readtext/master/tests/data/csv/test.csv", text_field='text')),
        c(test.csv.1 = 'Lorem ipsum.', test.csv.2 = 'Dolor sit')
    )
})


context('test that require recursive invocation of listFileNames (i.e. because a special filename resolves to another special filename)')

test_that("test zip file", {
    skip_on_appveyor()
    skip_on_cran()
    skip_on_os("windows")
    DATA_DIR <- system.file("extdata/", package = "readtext")
        expect_equal(
        length(texts(readtext(paste0(DATA_DIR, "data_files_encodedtexts.zip")))),
        36
    )
})




test_that("test non-implemented functions", {
    # 100% code coverage of non-existent functions! 
})

test_that("test warning for unrecognized filetype", {
    expect_that(
        readtext('../data/empty/empty.nonesuch'),
        gives_warning('Unsupported extension " nonesuch " of file')
    )

    # But test that it still loads
    expect_that(
        readtext('../data/unknown/unknown'),
        gives_warning('Unsupported extension "  " of file *')
    )
    expect_equal(
        readtext('../data/unknown/unknown')$text,
        c("The quick brown fox jumps over the lazy dog.")
    )

})


# TODO: Refactor this to loop over filetypes
test_that("test csv files", {
    # Test corpus object
    testcorpus <- readtext('../data/csv/test.csv', text_field='text')
    expect_that(
        docvars(testcorpus),
        equals(data.frame(list(colour = c('green', 'red'), number = c(42, 99)), 
                          stringsAsFactors = FALSE))
    )
    expect_equal(
        texts(testcorpus),
        c(test.csv.1 = 'Lorem ipsum.', test.csv.2 = 'Dolor sit')
    )
    
    expect_that(
        docvars(readtext('../data/csv/*', text_field='nonesuch')),
        throws_error("There is no field called")
    )
    
    expect_that(
        docvars(readtext('../data/csv/*', text_field = 9000)),
        throws_error("There is no 9000th field")
    )
    
})

test_that("test tab files", {
    testreadtext <- readtext('../data/tab/test.tab', text_field = 'text')
    expect_that(
        docvars(testreadtext),
        equals(data.frame(list(colour=c('green', 'red'), number=c(42, 99)), 
                          stringsAsFactors = FALSE))
    )
    expect_that(
        texts(testreadtext),
        equals(c(test.tab.1='Lorem ipsum.', test.tab.2='Dolor sit'))
    )
    
    expect_that(
        readtext('../data/tab/test.tab', text_field='nonexistant'),
        throws_error('There is no field called nonexistant')
    )
    
})

test_that("test tsv files", {
    testreadtext <- readtext('../data/tsv/test.tsv', text_field='text')
    expect_that(
        docvars(testreadtext),
        equals(data.frame(list(colour=c('green', 'red'), number=c(42, 99)), 
                          stringsAsFactors = FALSE))
    )
    expect_that(
        texts(testreadtext),
        equals(c(test.tsv.1='Lorem ipsum.', test.tsv.2='Dolor sit'))
    )
    
    expect_that(
        readtext('../data/tsv/test.tsv', text_field='nonexistant'),
        throws_error('There is no field called nonexistant')
    )
    
})


test_that("test xml files", {
    # Test corpus object
    testcorpus <- readtext('../data/xml/test.xml', text_field='text')
    expect_that(
        data.frame(testcorpus[,-c(1,2)]),
        equals(data.frame(list(colour=c('green', 'red'), number=c(42, 99)), 
                          stringsAsFactors = FALSE))
    )
    expect_that(
        unname(texts(testcorpus)),
        equals(c('Lorem ipsum.','Dolor sit'))
    )
    expect_that(
        docnames(testcorpus),
        equals(c("test.xml.1", "test.xml.2"))
    )

    expect_that(
        readtext('../data/xml/test.xml', text_field=1),
        gives_warning('You should specify text_field by name.*')
    )
    expect_that(
        unname(texts(readtext('../data/xml/test.xml', text_field=1))),
        equals(c('Lorem ipsum.', 'Dolor sit'))
    )
    expect_that(
        docnames(testcorpus),
        equals(c("test.xml.1", "test.xml.2"))
    )

    
    expect_that(
        docvars(readtext('../data/xml/*', text_field='nonesuch')),
        throws_error("There is no node called")
    )
    expect_that(
        docvars(readtext('../data/xml/*', text_field=9000)),
        throws_error("There is no 9000th field")
    )
})


test_that("test xml files with XPath", {

    expected <- c('The quick brown fox')
    names(expected) <- 'tei.xml'

    actual <- readtext('../data/xml/tei.xml',
                      text_field='/tei:TEI/tei:text/tei:body//tei:p',
                      namespaces=c(tei = "http://www.tei-c.org/ns/1.0"))
    expect_equal(texts(actual), expected)


    actual <- readtext('../data/xml/tei.xml', collapse='P',
                      text_field='/tei:TEI/tei:text/tei:body//tei:p',
                      namespaces=c(tei = "http://www.tei-c.org/ns/1.0"))
    expect_equal(unname(texts(actual)), 'The Pquick Pbrown Pfox')

    actual <- readtext('../data/xml/tei.xml', collapse='P',
                      text_field='/tei:TEI//*/text()',
                      namespaces=c(tei = "http://www.tei-c.org/ns/1.0"))
    expect_equal(unname(texts(actual)), 'Lorem Ipsum 1PSome PlacePAnywhere, USPNopePThe Pquick Pbrown PfoxPNope')

})


test_that("test readtext() with docvarsfrom=filenames", {
    
    expect_that(
        docvars(readtext('../data/docvars/one/*', docvarsfrom='filenames')),
        equals(data.frame(list(docvar1=c(1L, 2L), docvar2=c('apple', 'orange')), 
                          stringsAsFactors = FALSE))
    )
    
    expect_that(
        docvars(readtext('../data/docvars/dash/*', docvarsfrom='filenames', dvsep='-')),
        equals(data.frame(list(docvar1=c(1,2), docvar2=c('apple', 'orange')), 
                          stringsAsFactors = FALSE))
    )
    
    
    expect_that(
        docvars(readtext('../data/docvars/two/*txt', docvarsfrom='filenames')),
        equals(data.frame(list(docvar1=c(1,2), docvar2=c('apple', 'orange')), docvar3=c('red', 'orange'), 
                          stringsAsFactors = FALSE))
    )
    
    expect_that(
        docvars(readtext('../data/docvars/two/*json', text_field = 'nonesuch', 
                        docvarsfrom = 'filenames')),
        throws_error("There is no field called")
    )
    
    expect_that(
        docvars(readtext('../data/docvars/unequal/*', docvarsfrom='filenames')),
        throws_error("Filename elements are not equal in length.")
    )
    
    expect_that(
        docvars(readtext('../data/docvars/two/*txt', docvarsfrom='filenames',
                         docvarnames=c('id', 'fruit', 'colour'))),
        equals(data.frame(list(id=c(1,2), fruit=c('apple', 'orange')), 
                          colour=c('red', 'orange'), stringsAsFactors=F))
    )

    expect_that(
        docvars(readtext('../data/docvars/two/*txt', docvarsfrom='filenames',
                         docvarnames=c('id', 'fruit')
        )),
        gives_warning('Fewer docnames supplied than existing docvars - last 1 docvar given generic names.')
    )
    expect_that(
        docvars(readtext('../data/docvars/two/*txt', docvarsfrom='filenames',
                         docvarnames=c('id', 'fruit')
        )),
        equals(data.frame(list(id=c(1,2), fruit=c('apple', 'orange')), 
                          docvar3=c('red', 'orange'), stringsAsFactors = FALSE))
    )
    
    
    expect_that(
        docvars(readtext('../data/docvars/two/*txt', docvarsfrom='filenames',
                         docvarnames=c('id')
        )),
        gives_warning('Fewer docnames supplied than existing docvars - last 2 docvars given generic names.')
    )
    
    #TODO: What happens if you supply more docnames?
    
    expect_error(
        docvars(readtext('../data/docvars/two/*txt', docvarsfrom='nonesuch'))
    )
    
    #  Docvars from both metadata and filename
    expect_equal(
        docvars(readtext('../data/docvars/csv/*', docvarsfrom=c('filenames'), docvarnames=c('id', 'fruit'), text_field='text')),
        data.frame(list(shape=c('round', NA), texture=c(NA, 'rough'), id=c(1, 2), fruit=c('apple', 'orange')), 
                   stringsAsFactors = FALSE)
    )
    
    # #  Docvars from both metadata and filename
    # expect_equal(
    #     docvars(readtext('../data/docvars/json/*', docvarsfrom=c('filenames', 'metadata'), docvarnames=c('id', 'fruit'), text_field='text')),
    #     data.frame(list(id=c(1, 2), fruit=c('apple', 'orange'), shape=c('round', NA), texture=c(NA, 'rough')), stringsAsFactors=FALSE)
    # )
    
})


test_that("test texts.readtext error with groups!=NULL", {
    expect_that(
        texts(readtext('../data/fox/fox.txt'), groups='anything'),
        throws_error()
    )
})

test_that("test docvars.readtext warning with field!=NULL", {
    expect_that(
        docvars(readtext('../data/fox/fox.txt'), field='anything'),
        gives_warning()
    )
})


test_that("test that readtext encoding argument must be either length 1 or same length as the number of files", {
    expect_that(
        readtext(
            c('../data/fox/fox.txt', '../data/fox/fox.txt', '../data/fox/fox.txt', '../data/fox/fox.txt'),
            encoding=c('utf-8', 'utf-8')
        ),
        throws_error('encoding parameter must be length 1, or as long as the number of files')
    )
})

context('Loading a corpus from a zip archive')
test_that("A single-level zip file containing txt files can be loaded",{
    qc <- readtext('../data/zip/inauguralTopLevel.zip')
    expect_equal(nrow(qc), 57)
})




context('Loading an empty gzipped tar archive')
test_that("An empty tar.gz file raises an error",{
    expect_that(
        readtext('../data/empty/test.tar.gz'),
        throws_error("File '../data/empty/test.tar.gz' does not exist")
    )
})


test_that("test reading structured text files with different columns", {
    testcorpus <- readtext(
        "../data/fruits/*.csv",
        text_field='text'
    )
    
    expect_that(
        docvars(testcorpus),
        equals(data.frame(list(
            color=c('green', 'orange', NA, NA), 
            shape=c(NA, NA, 'round', 'long')
        ),
        stringsAsFactors=F))
    )
    expected_texts <- c('apple', 'orange', 'apple', 'banana')
    names(expected_texts) <- c('1.csv.1', '1.csv.2', '2.csv.1', '2.csv.2')
    expect_that(
        texts(testcorpus),
        equals(expected_texts)
    )
})




context("Tests of new readtext internals. If these fail, it doesn't necessarily affect the exposed API")

context("Tests for listMatchingFiles")

test_that("Test function to list files", {
    expect_that(
        readtext:::listMatchingFiles('nonesuch://example.org/test.txt'),
        throws_error('Unsupported URL scheme')
    )           
    
    testExistingFile <- mktemp()
    expect_equal(readtext:::listMatchingFiles(testExistingFile), testExistingFile)
    expect_equal(readtext:::listMatchingFiles(paste0('file://', testExistingFile)), testExistingFile)
    
    
    # Test vector of filenames
    testExistingFile2 <- mktemp()
    expect_equal(
        readtext:::listMatchingFiles(c(testExistingFile, testExistingFile2)),
        c(testExistingFile, testExistingFile2)
    )
    
    # TODO: Test vector of filename and URL
    expect_equal(
        readtext:::listMatchingFiles(c(testExistingFile, testExistingFile2)),
        c(testExistingFile, testExistingFile2)
    )
    
    file.remove(testExistingFile)
    expect_that(
        readtext:::listMatchingFiles(testExistingFile),
        throws_error("File '' does not exist")
    )
    expect_equal(
        readtext:::listMatchingFiles(testExistingFile, ignoreMissing=T),
        character(0)
    )
    
    
    #Test globbing
    tempdir <- mktemp(directory=T)
    
    file.create(file.path(tempdir, '1.tsv'))
    file.create(file.path(tempdir, '2.tsv'))
    file.create(file.path(tempdir, '10.tsv'))
    
    expect_equal(
        length(listMatchingFiles(paste0(tempdir, '/', '*.tsv' ))),
        3
    )
    
    expect_equal(
        length(listMatchingFiles(paste0(tempdir, '/', '?.tsv' ))),
        2
    )
    
    expect_that(
        length(listMatchingFiles(paste0(tempdir, '/', '?.txt' ))),
        throws_error("File '' does not exist")
    )
    
    
    # Test globbing subdir
    
    tempsubdir1 <- mktemp(base_path=tempdir, directory=T)
    tempsubdir2 <- mktemp(base_path=tempdir, directory=T)
    
    file.create(file.path(tempsubdir1, '1.tsv'))
    file.create(file.path(tempsubdir1, '2.tsv'))
    file.create(file.path(tempsubdir2, '1.tsv'))
    
    expect_equal(
        length(readtext:::listMatchingFiles(paste0(tempdir, '/', '*/', '?.tsv' ))),
        3
    )
    
    
    expect_that(
        readtext:::listMatchingFiles('http://example.org/test.nonesuch'),
        throws_error('Remote URL does not end in known extension.')
    )
    
})
    
test_that("Test function to list files with remote sources", {
    skip_on_cran()
    expect_error(
      readtext:::listMatchingFiles('http://www.google.com/404.txt'),
      ".*404.*"
    )
    
    expect_equal(
      dim(readtext('http://www.google.com/404.txt', ignore_missing_files = TRUE)),
      c(1,2)
    )
})


test_that("text vectors have names of the files they come from by default (bug 221)", {

        expect_equal(
            names(texts(readtext(
                '../data/fox/fox.txt'
            ))),
            'fox.txt'
        )

        actual_names <- names(texts(readtext(
            '../data/csv/*.csv', text_field='text'
        )))
        expect_equal(
            setdiff(
                c('test.csv.1', 'test.csv.2', 'test2.csv.1', 'test2.csv.2'),
                actual_names
            ),
            character(0)
        )

        actual_names <- names(texts(readtext(
            '../data/glob/*.txt'
        )))
        expect_equal(
            setdiff(
                c('1.txt', '2.txt', '3.txt', '4.txt', '10.txt'),
                actual_names
            ),
            character(0)
        )

        actual_names <- names(texts(readtext(
            '../data/tar/test.tar'
        )))
        expect_equal(
            setdiff(
                c('test.txt', 'test2.txt', 'test3.txt', 'test4.txt'),
                actual_names
            ),
            character(0)
        )

}) 

test_that("test globbed tar file",{
    skip_on_cran()
    skip_on_travis()
    expect_equal(
        unname(texts(readtext("../data/tar/*"))),
        c("Lorem ipsum", "brown fox", "Dolor sit", "The quick")
    )
})

test_that("test html file",{
    expected <- c("The quick brown fox \njumps over the lazy dog")
    names(expected) <- 'html5.html'

    expect_equal(
        texts(readtext('../data/html/html5.html')),
        expected
    )

})


test_that("test malformed html file",{
    skip_on_os("windows")
    expected <- c("The quick brown fox \n    \njumps over the lazy dog")
    names(expected) <- 'malformed_html5.html'
    expect_equal(
        texts(readtext('../data/html/malformed_html5.html')),
        expected
    )
})


test_that("test for pdf file", {
    skip_on_os("windows")
    skip_on_travis()
    expected <- c(test.pdf = "The quick brown fox jumps over the lazy dog\n                                     1\n")
    expect_equal(
        texts(readtext('../data/pdf/test.pdf')),
        expected
    )
})

test_that("test for docx file", {
    expected <- c("The quick brown fox jumps over the lazy dog")
    names(expected) <- 'test.docx'
    
    expect_equal(
        texts(readtext('../data/docx/test.docx')),
        expected
    )

})



test_that("test for doc file", {
    skip_on_os("windows")  
    expected <- paste(rep(c("The quick brown fox jumps over the lazy dog."), 10), collapse =' ')
    names(expected) <- 'test.doc'

    txts <- texts(readtext('../data/doc/test.doc'))
    namestmp <- names(txts)
    txts <- stringi::stri_replace_all_regex(txts, "\\n", " ")
    names(txts) <- namestmp

    expect_equal(
        txts,
        expected
    )
})

test_that("test json files", {
    skip_on_cran()
    skip_on_travis()
    expect_equal(
        unname(texts(readtext('../data/json/*json', text_field='text'))),
        c("Lorem ipsum", "Dolor sit", "The quick", "brown fox", "Now is the winter")
    )
    
    #  test.json and test2.json are newline-delimited json
    #  test3.json is a single json object
    expected_docvars <- data.frame(list(
        colour=c('green', 'red', 'orange', 'blue', NA), 
        number=c(42, 99, 0, NA, 3)),
        stringsAsFactors = FALSE)
    expected_docvars <- expected_docvars[order(expected_docvars$number),]
    row.names(expected_docvars) <- NULL
    actual_docvars <- docvars(readtext('../data/json/*json', text_field='text'))
    actual_docvars <- actual_docvars[order(actual_docvars$number),]
    row.names(actual_docvars) <- NULL
    row.names(actual_docvars)
    row.names(expected_docvars)
    expect_equal(
        actual_docvars,
        expected_docvars
    )
    
    expect_that(
        texts(readtext('../data/json/*json', text_field=1)),
        throws_error('Cannot use numeric text_field with json file')
    )
    
    expect_that(
        texts(readtext('../data/json/test3.json', text_field='nonesuch')),
        throws_error('There is no field called nonesuch in file')
    )
    
    
    # Twitter json files
    tweetSource <- readtext('../data/tweets/stream.json')
    
    expect_equal(
        texts(tweetSource),
        c(stream.json.1="I jumped over the lazy @dog", stream.json.2="Yawn")
    )
    
    expect_equal(
        docvars(tweetSource)$statuses_count,
        c(16204, 200)
    )
    
    expect_equal(
        docvars(tweetSource)$screen_name,
        c('foxxy', 'dog')
    )
    
    
})

if (.Platform$OS.type == "unix") {
    test_that("test readtext with folder", {
        expect_equal(
            length(readtext('../data/fruits')$text),
            7
        )
    })
}    



context('Loading a corpus from a tar archive')
test_that("A single-level tar file containing txt files can be loaded",{
    skip_on_cran()
    skip_on_travis()
    expect_equal(
        unname(texts(readtext("../data/tar/test.tar"))),
        c("Lorem ipsum", "brown fox", "Dolor sit", "The quick")
    )
})

context('Loading a corpus from a gzipped tar archive')
test_that("A single-level tar.gz file containing txt files can be loaded",{
    skip_on_cran()
    skip_on_travis()
    expect_equal(
        unname(texts(readtext('../data/targz/test.tar.gz'))),
        c("Lorem ipsum", "brown fox", "Dolor sit", "The quick")
    )
})

context('Loading a corpus from a bzipped tar archive')
test_that("A single-level tar.bz file containing txt files can be loaded",{
    skip_on_cran()
    skip_on_travis()
    skip_on_os("windows")
    expect_equal(
        unname(texts(readtext("../data/tarbz/test.tar.bz"))),
        c("Lorem ipsum", "brown fox", "Dolor sit", "The quick")
    )
})

context('Tests for verbosity argument')
test_that("test warning for unrecognized filetype", {
       expect_that(
           readtext('../data/empty/empty.nonesuch'),
           gives_warning('Unsupported extension " nonesuch " of file')
       )
       expect_that(
           readtext('../data/empty/empty.nonesuch', verbosity=3),
           gives_warning('Unsupported extension " nonesuch " of file')
       )
       expect_that(
           readtext('../data/empty/empty.nonesuch', verbosity=2),
           gives_warning('Unsupported extension " nonesuch " of file')
       )
       expect_that(
           readtext('../data/empty/empty.nonesuch', verbosity=1),
           gives_warning('Unsupported extension " nonesuch " of file')
       )
       expect_that(
           readtext('../data/empty/empty.nonesuch', verbosity=0),
           not(gives_warning())
       )
})

test_that("messages from listMatchingFile",{
    expect_silent(
        readtext('../data/zip/inauguralTopLevel.zip', verbosity=0)
    )
    expect_silent(
        readtext('../data/zip/inauguralTopLevel.zip', verbosity=1)
    )
    expect_message(
        readtext('../data/zip/inauguralTopLevel.zip', verbosity=2),
        "Reading texts from \\.\\./data/zip/inauguralTopLevel\\.zip"
    )
    expect_message(
        readtext('../data/zip/inauguralTopLevel.zip', verbosity=3),
        "reading \\(txt\\) file: 1789-Washington\\.txt"
    )
})

test_that("readtext called with textfield works with deprecation warning", {
    expect_equal(
        length(texts(readtext(
            '../data/csv/*.csv', textfield='text'
        ))),
        4
    )
    expect_equal(
        nrow(docvars(readtext(
            '../data/csv/*.csv', textfield='text'
        ))),
        4
    )
    expect_equal(
        length(texts(readtext(
            '../data/csv/*.csv', textfield='text'
        ))),
        4
    )
    expect_warning(
        readtext('../data/csv/*.csv', textfield='text'),
        "textfield is deprecated; use text_field instead"
    )
})


test_that("tests for Excel files", {

    expect_equal(unname(texts(
        readtext('../data/xls/test.xlsx', text_field='text'))),
        c('The quick', 'brown fox', 'jumps over', 'the lazy dog.')
    )
    expect_that(
        docvars(readtext('../data/xls/test.xlsx', text_field='text')),
        equals(data.frame(list(
                        colour=c('orange', 'blue', 'pink', 'pink'),
                        number=c(0, NA, NA, NA),
                        taste=c(NA, NA, 'sweet', 'umami')
                        ), stringsAsFactors=FALSE))
    )


    expect_equal(
        texts(readtext('../data/xls/test.xls', text_field='text')),
        c('test.xls.1'='The quick', 'test.xls.2'='brown fox', 
          'test.xls.3'='jumps over', 'test.xls.4'='the lazy dog.')
    )
    expect_that(
        docvars(readtext('../data/xls/test.xls', text_field='text')),
        equals(data.frame(list(
                        colour=c('orange', 'blue', 'pink', 'pink'),
                        number=c(0, NA, NA, NA),
                        taste=c(NA, NA, 'sweet', 'umami')
                        ), stringsAsFactors=FALSE))
    )


})


test_that("tests for ODS files", {

    expect_equal(unname(texts(
        readtext('../data/ods/test.ods', text_field='text'))),
        c('The quick', 'brown fox', 'jumps over', 'the lazy dog.')
    )
    expect_equal(
        docvars(readtext('../data/ods/test.ods', text_field='text')),
        data.frame(list(
                        colour=c('orange', 'blue', 'pink', 'pink'),
                        number=c(0, NA, NA, NA),
                        taste=c(NA, NA, 'sweet', 'umami')
                        ), stringsAsFactors=FALSE)
    )

})
