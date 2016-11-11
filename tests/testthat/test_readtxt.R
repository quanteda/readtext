# TODO: re-do docs
# TODO: Check and remove extranous codes
# TODO: recurse file listing for e.g. remote ZIP file
# TODO: readtxt with csv doesn't seem to require textfield


context('test readtxt.R')

test_that("test print.readtext", {
    
    expect_that(
        print(readtxt( '../data/fox/fox.txt')),
        prints_text('readtext object consisting of 1 document and 0 docvars.')
    )
    
    testreadtext <- readtxt(
        c(
            '../data/fruits/apple.txt',
            '../data/fruits/orange.txt'
        )
    )
    expect_that(
        print(testreadtext),
        prints_text('readtext object consisting of 2 documents and 0 docvars.')
    )
    
    expect_that(
        print(readtxt('../data/csv/test.csv', textfield='text')),
        prints_text('readtext object consisting of 2 documents and 2 docvars.')
    )
    
    
})


test_that("test readtxt with single filename", {
    fox <- c(fox.txt = "The quick brown fox jumps over the lazy dog.")
    expect_equal(
        texts(readtxt('../data/fox/fox.txt')),
        fox
    )
})

test_that("test classes, slots, and extractor functions", {
 
 testreadtxt <- readtxt('../data/fox/fox.txt')
 docvars(testreadtxt)
 
 
 expect_is(testreadtxt, 'readtext')
 
})

test_that("test readtxt with vector of filenames", {
    expect_equal(
        length(texts(readtxt(
            c(
                '../data/fruits/apple.txt',
                '../data/fruits/orange.txt'
            )
        ))),
        2
    )
})

test_that("test readtxt with glob-style mask", {
    expect_equal(
        length(texts(readtxt(
            '../data/glob/*.txt'
        ))),
        5
    )
    
    expect_equal(
        length(texts(readtxt(
            '../data/glob/?.txt'
        ))),
        4
    )
    
    
    # Glob in non-last part of filename
    expect_equal(
        length(texts(readtxt(
            '../data/glob/*/test.txt'
        ))),
        2
    )
    
    # It would be nice to be able to test that escaped glob doesn't special 
    # characters in filename, but R check won't allow a file of this name to
    # exist in the package... This should still pass if run the test manually
    # (having created the file, supposing your platform supports it)
    #  expect_equal(
    #      length(texts(readtxt(
    #              '../data/glob/special/\\*.txt'
    #      ))),
    #      1
    #  )
    
    
})

test_that("test structured readtxt with glob-style mask", {
    expect_equal(
        length(texts(readtxt(
            '../data/csv/*.csv', textfield='text'
        ))),
        4
    )
    expect_equal(
        nrow(docvars(readtxt(
            '../data/csv/*.csv', textfield='text'
        ))),
        4
    )
})


test_that("test remote text file", {
    expect_equal(
        texts(readtxt('https://raw.githubusercontent.com/kbenoit/quanteda/master/tests/data/fox/fox.txt')),
        c(fox.txt='The quick brown fox jumps over the lazy dog.')
    )
    # ignoreMissing with an existing file should make no difference
    expect_equal(
        texts(readtxt('https://raw.githubusercontent.com/kbenoit/quanteda/master/tests/data/fox/fox.txt', ignoreMissing=T)),
        c(fox.txt='The quick brown fox jumps over the lazy dog.')
    )
})


test_that("test remote csv file", {
    expect_equal(
        texts(readtxt("https://raw.githubusercontent.com/kbenoit/quanteda/master/tests/data/csv/test.csv", textfield='text')),
        c(test.csv.1 = 'Lorem ipsum.', test.csv.2 = 'Dolor sit')
    )
})



context('test that require recursive invocation of listFileNames (i.e. because a special filename resolves to another special filename)')
test_that("test remote zip file", {
    expect_equal(
        length(texts(
            readtxt('https://github.com/kbenoit/quanteda/raw/master/inst/extdata/encodedTextFiles.zip')
        )),
        41
    )
})




test_that("test non-implemented functions", {
    
    expect_that(
        readtxt('../data/empty/empty.doc'),
        throws_error('Unsupported extension doc')
    )
    
    expect_that(
        readtxt('../data/empty/empty.docx'),
        throws_error('Unsupported extension docx')
    )
    
    expect_that(
        readtxt('../data/empty/empty.pdf'),
        throws_error('Unsupported extension pdf')
    )
    
    
})

test_that("test warning for unrecognized filetype", {
    expect_that(
        readtxt('../data/empty/empty.nonesuch'),
        throws_error('Unsupported extension nonesuch')
    )
})


# TODO: Refactor this to loop over filetypes
test_that("test csv files", {
    # Test corpus object
    testcorpus <- readtxt('../data/csv/test.csv', textfield='text')
    expect_that(
        docvars(testcorpus),
        equals(data.frame(list(colour = c('green', 'red'), number = c(42, 99)), 
                          stringsAsFactors = FALSE, 
                          row.names = c("test.csv.1", "test.csv.2")))
    )
    expect_equal(
        texts(testcorpus),
        c(test.csv.1 = 'Lorem ipsum.', test.csv.2 = 'Dolor sit')
    )
    
    expect_that(
        docvars(readtxt('../data/csv/*', textfield='nonesuch')),
        throws_error("There is no field called")
    )
    
    expect_that(
        docvars(readtxt('../data/csv/*', textfield = 9000)),
        throws_error("There is no 9000th field")
    )
    
})

test_that("test tab files", {
    testreadtext <- readtxt('../data/tab/test.tab', textfield = 'text')
    expect_that(
        docvars(testreadtext),
        equals(data.frame(list(colour=c('green', 'red'), number=c(42, 99)), 
                          stringsAsFactors = FALSE,
                          row.names = c("test.tab.1", "test.tab.2")))
    )
    expect_that(
        texts(testreadtext),
        equals(c(test.tab.1='Lorem ipsum.', test.tab.2='Dolor sit'))
    )
    
    expect_that(
        readtxt('../data/tab/test.tab', textfield='nonexistant'),
        throws_error('There is no field called nonexistant')
    )
    
})

test_that("test tsv files", {
    testreadtext <- readtxt('../data/tsv/test.tsv', textfield='text')
    expect_that(
        docvars(testreadtext),
        equals(data.frame(list(colour=c('green', 'red'), number=c(42, 99)), 
                          stringsAsFactors=F,
                          row.names = c("test.tsv.1", "test.tsv.2")))
    )
    expect_that(
        texts(testreadtext),
        equals(c(test.tsv.1='Lorem ipsum.', test.tsv.2='Dolor sit'))
    )
    
    expect_that(
        readtxt('../data/tsv/test.tsv', textfield='nonexistant'),
        throws_error('There is no field called nonexistant')
    )
    
})


test_that("test xml files", {
    # Test corpus object
    testcorpus <- readtxt('../data/xml/test.xml', textfield='text')
    expect_that(
        docvars(testcorpus),
        equals(data.frame(list(colour=c('green', 'red'), number=c(42, 99)), 
                          stringsAsFactors = FALSE,
                          row.names = c("test.xml.1", "test.xml.2")))
    )
    expect_that(
        texts(testcorpus),
        equals(c(test.xml.1='Lorem ipsum.', test.xml.2='Dolor sit'))
    )
    
    expect_that(
        readtxt('../data/xml/test.xml', textfield=1),
        gives_warning('You should specify textfield by name.*')
    )
    expect_that(
        texts(readtxt('../data/xml/test.xml', textfield=1)),
        equals(c(test.xml.1='Lorem ipsum.', test.xml.2='Dolor sit'))
    )
    
    expect_that(
        docvars(readtxt('../data/xml/*', textfield='nonesuch')),
        throws_error("There is no node called")
    )
    expect_that(
        docvars(readtxt('../data/xml/*', textfield=9000)),
        throws_error("There is no 9000th field")
    )
})



test_that("test readtxt() with docvarsfrom=filenames", {
    
    expect_that(
        docvars(readtxt('../data/docvars/one/*', docvarsfrom='filenames')),
        equals(data.frame(list(docvar1=c(1L, 2L), docvar2=c('apple', 'orange')), 
                          stringsAsFactors = FALSE,
                          row.names = c("1_apple.txt", "2_orange.txt")))
    )
    
    expect_that(
        docvars(readtxt('../data/docvars/dash/*', docvarsfrom='filenames', dvsep='-')),
        equals(data.frame(list(docvar1=c(1,2), docvar2=c('apple', 'orange')), 
                          stringsAsFactors = FALSE,
                          row.names = c("1-apple.txt", "2-orange.txt")))
    )
    
    
    expect_that(
        docvars(readtxt('../data/docvars/two/*txt', docvarsfrom='filenames')),
        equals(data.frame(list(docvar1=c(1,2), docvar2=c('apple', 'orange')), docvar3=c('red', 'orange'), 
                          stringsAsFactors = FALSE,
                          row.names = c("1_apple_red.txt", "2_orange_orange.txt")))
    )
    
    expect_that(
        docvars(readtxt('../data/docvars/two/*json', textfield='nonesuch', 
                        docvarsfrom='filenames')),
        throws_error("There is no field called")
    )
    
    expect_that(
        docvars(readtxt('../data/docvars/unequal/*', docvarsfrom='filenames')),
        throws_error("Filename elements are not equal in length.")
    )
    
    expect_that(
        docvars(readtxt('../data/docvars/two/*txt', docvarsfrom='filenames',
                         docvarnames=c('id', 'fruit', 'colour'))),
        equals(data.frame(list(id=c(1,2), fruit=c('apple', 'orange')), 
                          colour=c('red', 'orange'), stringsAsFactors=F,
                          row.names = c("1_apple_red.txt", "2_orange_orange.txt")))
    )
    
    
    expect_that(
        docvars(readtxt('../data/docvars/two/*txt', docvarsfrom='filenames',
                         docvarnames=c('id', 'fruit')
        )),
        gives_warning('Fewer docnames supplied than existing docvars - last 1 docvar given generic names.')
    )
    expect_that(
        docvars(readtxt('../data/docvars/two/*txt', docvarsfrom='filenames',
                         docvarnames=c('id', 'fruit')
        )),
        equals(data.frame(list(id=c(1,2), fruit=c('apple', 'orange')), 
                          docvar3=c('red', 'orange'), stringsAsFactors = FALSE,
                          row.names = c("1_apple_red.txt", "2_orange_orange.txt")))
    )
    
    
    expect_that(
        docvars(readtxt('../data/docvars/two/*txt', docvarsfrom='filenames',
                         docvarnames=c('id')
        )),
        gives_warning('Fewer docnames supplied than existing docvars - last 2 docvars given generic names.')
    )
    
    #TODO: What happens if you supply more docnames?
    
    expect_error(
        docvars(readtxt('../data/docvars/two/*txt', docvarsfrom='nonesuch'))
    )
    
    #  Docvars from both metadata and filename
    expect_equal(
        docvars(readtxt('../data/docvars/csv/*', docvarsfrom=c('filenames'), docvarnames=c('id', 'fruit'), textfield='text')),
        data.frame(list(shape=c('round', NA), texture=c(NA, 'rough'), id=c(1, 2), fruit=c('apple', 'orange')), 
                   stringsAsFactors = FALSE,
                   row.names = c("1_apple.csv", "2_orange.csv"))
    )
    
    # #  Docvars from both metadata and filename
    # expect_equal(
    #     docvars(readtxt('../data/docvars/json/*', docvarsfrom=c('filenames', 'metadata'), docvarnames=c('id', 'fruit'), textfield='text')),
    #     data.frame(list(id=c(1, 2), fruit=c('apple', 'orange'), shape=c('round', NA), texture=c(NA, 'rough')), stringsAsFactors=FALSE)
    # )
    
})

test_that("test texts.readtext error with groups!=NULL", {
    expect_that(
        texts(readtxt('../data/fox/fox.txt'), groups='anything'),
        throws_error()
    )
})

test_that("test docvars.readtext warning with field!=NULL", {
    expect_that(
        docvars(readtxt('../data/fox/fox.txt'), field='anything'),
        gives_warning()
    )
})




 test_that("test readtxt encoding parameter: UTF-8 encoded file, read as UTF-16 (should not work)", {
      expect_warning(
        misread_texts <- texts(readtxt(file.path('../data/encoding', 'UTF-8__characters.txt'), encoding='utf-16'))
      )
      utf8_bytes <- data.table::fread(file.path('../data/encoding', 'UTF-8__bytes.tsv'))[[1]]
      expect_false(
             all(as.numeric(charToRaw(misread_texts)) == utf8_bytes)
      )
 })


test_that("test that readtxt encoding argument must be either length 1 or same length as the number of files", {
    expect_that(
        readtxt(
            c('../data/fox/fox.txt', '../data/fox/fox.txt', '../data/fox/fox.txt', '../data/fox/fox.txt'),
            encoding=c('utf-8', 'utf-8')
        ),
        throws_error('encoding parameter must be length 1, or as long as the number of files')
    )
})

context('Loading a corpus from a zip archive')
test_that("A single-level zip file containing txt files can be loaded",{
    qc <- readtxt('../data/zip/inauguralTopLevel.zip')
    expect_equal(nrow(qc), 57)
})




context('Loading an empty gzipped tar archive')
test_that("An empty tar.gz file raises an error",{
    expect_that(
        readtxt('../data/empty/test.tar.gz'),
        throws_error("File ../data/empty/test.tar.gz does not exist")
    )
})


test_that("test reading structured text files with different columns", {
    testcorpus <- readtxt(
        "../data/fruits/*.csv",
        textfield='text'
    )
    
    expect_that(
        docvars(testcorpus),
        equals(data.frame(list(
            color=c('green', 'orange', NA, NA), 
            shape=c(NA, NA, 'round', 'long')
        ),
        stringsAsFactors=F,
        row.names = c("1.csv.1", "1.csv.2", "2.csv.1", "2.csv.2")
        ))
    )
    expected_texts <- c('apple', 'orange', 'apple', 'banana')
    names(expected_texts) <- c('1.csv.1', '1.csv.2', '2.csv.1', '2.csv.2')
    expect_that(
        texts(testcorpus),
        equals(expected_texts)
    )
})




context("Tests of new readtxt internals. If these fail, it doesn't necessarily affect the exposed API")

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
        throws_error('File does not exist')
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
        throws_error('File does not exist')
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
      listMatchingFiles('http://www.google.com/404.txt'),
      ".*404.*"
    )
    
    expect_equal(
      length(readtext:::listMatchingFiles('http://www.google.com/404.txt', ignoreMissing=T)),
      0
    )
})


test_that("text vectors have names of the files they come from by default (bug 221)", {

        expect_equal(
            names(texts(readtxt(
                '../data/fox/fox.txt'
            ))),
            'fox.txt'
        )

        actual_names <- names(texts(readtxt(
            '../data/csv/*.csv', textfield='text'
        )))
        expect_equal(
            setdiff(
                c('test.csv.1', 'test.csv.2', 'test2.csv.1', 'test2.csv.2'),
                actual_names
            ),
            character(0)
        )

        actual_names <- names(texts(readtxt(
            '../data/glob/*.txt'
        )))
        expect_equal(
            setdiff(
                c('1.txt', '2.txt', '3.txt', '4.txt', '10.txt'),
                actual_names
            ),
            character(0)
        )

        actual_names <- names(texts(readtxt(
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
    expect_equal(
        sort(unname(texts(readtxt('../data/tar/*')))),
        c('brown fox', 'Dolor sit', 'Lorem ipsum', 'The quick')
    )
})

test_that("test json files", {
    expect_equal(
        sort(unname(texts(readtxt('../data/json/*json', textfield='text')))),
        c('brown fox', 'Dolor sit', 'Lorem ipsum', 'Now is the winter', 'The quick')
    )
    
    #  test.json and test2.json are newline-delimited json
    #  test3.json is a single json object
    expected_docvars <- data.frame(list(
        colour=c('green', 'red', 'orange', 'blue', NA), 
        number=c(42, 99, 0, NA, 3)),
        stringsAsFactors=F)
    expected_docvars <- expected_docvars[order(expected_docvars$number),]
    row.names(expected_docvars) <- NULL
    actual_docvars <- docvars(readtxt('../data/json/*json', textfield='text'))
    actual_docvars <- actual_docvars[order(actual_docvars$number),]
    row.names(actual_docvars) <- NULL
    row.names(actual_docvars)
    row.names(expected_docvars)
    expect_equal(
        actual_docvars,
        expected_docvars
    )
    
    expect_that(
        texts(readtxt('../data/json/*json', textfield=1)),
        throws_error('Cannot use numeric textfield with json file')
    )
    
    expect_that(
        texts(readtxt('../data/json/test3.json', textfield='nonesuch')),
        throws_error('There is no field called nonesuch in file')
    )
    
    
    # Twitter json files
    tweetSource <- readtxt('../data/tweets/stream.json')
    
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

test_that("test encoding handling (skipped on travis and CRAN", {
    skip_on_cran()
    skip_on_travis()
    
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
    
    
    FILEDIR <- '../data/encoding'
    
    filenames <- list.files(FILEDIR, "*__characters.txt$")
    parts <- strsplit(gsub(".txt$", "", filenames), "__")
    fileencodings <- sapply(parts, "[", 1)
    
    fileencodings <- fileencodings[!(fileencodings %in% broken_encodings)]
    filenames <- file.path(FILEDIR, paste0(fileencodings,  "__characters.txt"))
    
    for (i in 1:length(fileencodings)) {
        filename <- filenames[[i]]
        encoding <- fileencodings[[i]]
        
        test_that(paste("test readtxt encoding parameter, encoding", encoding), {
            characters <- as.numeric(charToRaw(
                texts(readtxt(filename, encoding=fileencodings[[i]]))
            ))
            bytes <- data.table::fread(gsub('__characters.txt', '__bytes.tsv', filename))[[1]]
            expect_equal(characters, bytes)
        })
    }
    test_that("Test loading all these files at once with different encodings", {
        encodedreadtxtsCorpus <- corpus(readtxt(filenames, encoding=fileencodings))
    })
})

test_that("test readtxt encoding parameter: ASCII encoded file, read as UTF-8: (should work)", {
    skip_on_cran()
    skip_on_travis()
    utf8_bytes <- data.table::fread(file.path(FILEDIR, 'UTF-8__bytes.tsv'))[[1]]
    expect_that(
        as.numeric(charToRaw(
            texts(readtxt(file.path(FILEDIR, 'UTF-8__characters.txt'), encoding='utf-8'),
            ))),
        equals(utf8_bytes)
    )
})

 context('Loading a corpus from a tar archive')
 test_that("A single-level tar file containing txt files can be loaded",{
     expect_equal(
         unname(sort(texts(readtxt('../data/tar/test.tar')))),
         c('brown fox', 'Dolor sit', 'Lorem ipsum', 'The quick')
     )
 })

 context('Loading a corpus from a gzipped tar archive')
 test_that("A single-level tar.gz file containing txt files can be loaded",{
     expect_equal(
         sort(unname(texts(readtxt('../data/targz/test.tar.gz')))),
         c('brown fox', 'Dolor sit', 'Lorem ipsum', 'The quick')
     )
 })

 context('Loading a corpus from a bzipped tar archive')
 test_that("A single-level tar.bz file containing txt files can be loaded",{
     skip_on_os("windows")
     expect_equal(
         sort(unname(texts(readtxt('../data/tarbz/test.tar.bz')))),
         c('brown fox', 'Dolor sit', 'Lorem ipsum', 'The quick')
     )
 })
