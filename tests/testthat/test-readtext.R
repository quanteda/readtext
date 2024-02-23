# TODO: re-do docs
# TODO: Check and remove extranous codes # TODO: recurse file listing for e.g. remote ZIP file
# TODO: readtext with csv doesn"t seem to require text_field

dvars <- function(x) as.data.frame(x)[, -c(1:2)]

test_that("test readtext with single filename", {
    fox <- c(fox.txt = "The quick brown fox jumps over the lazy dog.")
    expect_equal(
        as.character(readtext("../data/fox/fox.txt")),
        fox
    )
})

test_that("test for deprecated warning for text()", {
    fox <- c(fox.txt = "The quick brown fox jumps over the lazy dog.")
    expect_warning(
        texts(readtext("../data/fox/fox.txt")),
        "'texts.readtext' is deprecated.
Use 'Use as.character() instead' instead.",
        fixed = TRUE
    )
})

test_that("test readtext with vector of filenames", {
    expect_equal(
        length(as.character(readtext(
            c(
                "../data/fruits/apple.txt",
                "../data/fruits/orange.txt"
            )
        ))),
        2
    )
})

test_that("test readtext with glob-style mask", {
    expect_equal(
        length(as.character(readtext(
            "../data/glob/*.txt"
        ))),
        5
    )

    expect_equal(
        length(as.character(readtext(
            "../data/glob/?.txt"
        ))),
        4
    )

    # Glob in non-last part of filename
    expect_equal(
        length(as.character(readtext(
            "../data/glob/*/test.txt"
        ))),
        2
    )

    # It would be nice to be able to test that escaped glob doesn"t special
    # characters in filename, but R check won"t allow a file of this name to
    # exist in the package... This should still pass if run the test manually
    # (having created the file, supposing your platform supports it)
    #  expect_equal(
    #      length(as.character(readtext(
    #              "../data/glob/special/\\*.txt"
    #      ))),
    #      1
    #  )
})

test_that("test structured readtext with glob-style mask", {
    expect_equal(
        nrow(readtext("../data/csv/test*.csv", text_field = "text")),
        6
    )
    expect_equal(
        nrow(readtext("../data/csv/test*.csv", text_field = "text")),
        6
    )
})


test_that("test remote text file", {
    expect_equal(
        as.character(readtext("https://raw.githubusercontent.com/kbenoit/readtext/master/tests/data/fox/fox.txt")),
        c(fox.txt = "The quick brown fox jumps over the lazy dog.")
    )
    # ignore_missing_files with an existing file should make no difference
    expect_equal(
        as.character(readtext("https://raw.githubusercontent.com/kbenoit/readtext/master/tests/data/fox/fox.txt",
                       ignore_missing_files = TRUE)),
        c(fox.txt = "The quick brown fox jumps over the lazy dog.")
    )
    
    # ingores parameters in URL
    expect_equal(
        as.character(readtext("https://raw.githubusercontent.com/kbenoit/readtext/master/tests/data/fox/fox.txt?x=1&y=2",
                       ignore_missing_files = TRUE)),
        c(fox.txt = "The quick brown fox jumps over the lazy dog.")
    )

    
})


test_that("test remote csv file", {
    expect_equal(
        as.character(readtext("https://raw.githubusercontent.com/kbenoit/readtext/master/tests/data/csv/test.csv", text_field = "text")),
        c(test.csv.1 = "Lorem ipsum.", test.csv.2 = "Dolor sit")
    )
})


context("test that require recursive invocation of listFileNames (i.e. because a special filename resolves to another special filename)")

test_that("test zip file", {
    skip_on_cran()
    skip_on_os("windows")
    DATA_DIR <- system.file("extdata/", package = "readtext")
        expect_equal(
        length(as.character(readtext(paste0(DATA_DIR, "/data_files_encodedtexts.zip")))),
        36
    )
})

test_that("test warning for unrecognized filetype", {
    expect_warning(
        readtext("../data/empty/empty.nonesuch"),
        paste0("Unsupported extension ", sQuote("nonesuch"), " of file *")
    )

    # But test that it still loads
    expect_warning(
        readtext("../data/unknown/unknown"),
        paste0("Unsupported extension ", sQuote(""), " of file *")
    )
    expect_equal(
        readtext("../data/unknown/unknown", verbosity = 0)$text,
        "The quick brown fox jumps over the lazy dog."
    )
})


# TODO: Refactor this to loop over filetypes
test_that("test csv files", {
    # Test corpus object
    testcorpus <- readtext("../data/csv/test.csv", text_field = "text")
    expect_identical(
        as.data.frame(testcorpus)[, -c(1:2)],
        data.frame(list(colour = c("green", "red"), number = c(42L, 99L)))
    )
    expect_equal(
        as.character(testcorpus),
        c(test.csv.1 = "Lorem ipsum.", test.csv.2 = "Dolor sit")
    )

    expect_error(
        readtext("../data/csv/*", text_field = "nonesuch"),
        "There is no field called"
    )

    expect_error(
        readtext("../data/csv/*", text_field = 9000),
        "There is no 9000th field"
    )
})

test_that("test tab files", {
    testcorpus <- readtext("../data/tab/test.tab", text_field = "text")
    expect_equal(
        as.data.frame(testcorpus)[, -c(1:2)],
        data.frame(list(colour = c("green", "red"), number = c(42, 99)), 
                          stringsAsFactors = FALSE)
    )
    expect_equal(
        testcorpus$text,
        unname(c(test.tab.1 = "Lorem ipsum.", test.tab.2 = "Dolor sit"))
    )
    
    expect_error(
        readtext("../data/tab/test.tab", text_field = "nonexistent"),
                 "There is no field called nonexistent"
    )
    
})

test_that("test tsv files", {
    testcorpus <- readtext("../data/tsv/test.tsv", text_field = "text")
    expect_that(
        as.data.frame(testcorpus)[, -c(1:2)],
        equals(data.frame(list(colour = c("green", "red"), number = c(42, 99)), 
                          stringsAsFactors = FALSE))
    )
    expect_that(
        as.character(testcorpus),
        equals(c(test.tsv.1 = "Lorem ipsum.", test.tsv.2 = "Dolor sit"))
    )
    
    expect_error(
        readtext("../data/tsv/test.tsv", text_field = "nonexistant"),
                 "There is no field called nonexistant"
    )
    
})


test_that("test xml files", {
    # Test corpus object
    testcorpus <- readtext("../data/xml/test.xml", text_field = "text")
    expect_equal(
        data.frame(testcorpus[, -c(1, 2)]),
        data.frame(list(colour = c("green", "red"), number = c(42, 99)), 
                          stringsAsFactors = FALSE)
    )
    expect_equal(
        unname(as.character(testcorpus)),
        c("Lorem ipsum.", "Dolor sit")
    )
    expect_identical(
        testcorpus$doc_id,
        c("test.xml.1", "test.xml.2")
    )

    expect_warning(
        readtext("../data/xml/test.xml", text_field = 1),
        "You should specify text_field by name.*"
    )
    expect_identical(
        unname(as.character(suppressWarnings(readtext("../data/xml/test.xml", text_field = 1)))),
        c("Lorem ipsum.", "Dolor sit")
    )
    expect_identical(
        testcorpus$doc_id,
        c("test.xml.1", "test.xml.2")
    )
    
    expect_error(
        readtext("../data/xml/test.xml", text_field = "nonesuch"),
        "There is no field called"
    )
    
    expect_error(
        readtext("../data/xml/test.xml", text_field = 9000),
        "There is no 9000th field"
    )
})


test_that("test xml files with XPath", {

    expected <- c("The quick brown fox")
    names(expected) <- "tei.xml"

    actual <- readtext("../data/xml/tei.xml",
                      text_field = "/d1:TEI/d1:text/d1:body//d1:p")#,
                      # namespaces = c(tei = "https://www.tei-c.org/ns/1.0"))
    expect_equal(as.character(actual), expected)


    actual <- readtext("../data/xml/tei.xml", collapse = "P",
                       text_field = "/d1:TEI/d1:text/d1:body//d1:p")
                       # namespaces = c(tei = "https://www.tei-c.org/ns/1.0"))
    expect_equal(unname(as.character(actual)), "The Pquick Pbrown Pfox")

    actual <- readtext("../data/xml/tei.xml", collapse = "P",
                      text_field = "/d1:TEI//*/text()")#,
                      # namespaces = c(tei = "https://www.tei-c.org/ns/1.0"))
    expect_equal(unname(as.character(actual)), "Lorem Ipsum 1PSome PlacePAnywhere, USPNopePThe Pquick Pbrown PfoxPNope")

})


test_that("test readtext() with docvarsfrom = filenames", {
    
    expect_that(
        dvars(readtext("../data/docvars/one/*", docvarsfrom = "filenames")),
        equals(data.frame(list(docvar1 = c(1L, 2L), docvar2 = c("apple", "orange")), 
                          stringsAsFactors = FALSE))
    )
    
    expect_that(
        dvars(readtext("../data/docvars/dash/*", docvarsfrom = "filenames", dvsep = "-")),
        equals(data.frame(list(docvar1 = c(1,2), docvar2 = c("apple", "orange")), 
                          stringsAsFactors = FALSE))
    )
    
    
    expect_that(
        dvars(readtext("../data/docvars/two/*txt", docvarsfrom = "filenames")),
        equals(data.frame(list(docvar1 = c(1,2), docvar2 = c("apple", "orange")), docvar3 = c("red", "orange"), 
                          stringsAsFactors = FALSE))
    )
    
    expect_error(
        readtext("../data/docvars/two/*json", text_field = "nonesuch", docvarsfrom = "filenames"),
                "There is no field called"
    )
    
    expect_error(
        readtext("../data/docvars/unequal/*", docvarsfrom = "filenames"),
        "Filename elements are not equal in length."
    )
    
    expect_that(
        dvars(readtext("../data/docvars/two/*txt", docvarsfrom = "filenames",
                         docvarnames = c("id", "fruit", "colour"))),
        equals(data.frame(list(id = c(1,2), fruit = c("apple", "orange")), 
                          colour = c("red", "orange"), stringsAsFactors = FALSE))
    )

    expect_warning(
        dvars(readtext("../data/docvars/two/*txt", docvarsfrom = "filenames",
                         docvarnames = c("id", "fruit")
        )),
        "Fewer docnames supplied than existing docvars - last 1 docvar given generic names."
    )
    expect_that(
        dvars(suppressWarnings(readtext("../data/docvars/two/*txt", docvarsfrom = "filenames",
                         docvarnames = c("id", "fruit")))),
        equals(data.frame(list(id = c(1,2), fruit = c("apple", "orange")), 
                          docvar3 = c("red", "orange"), stringsAsFactors = FALSE))
    )
    
    expect_warning(
        dvars(readtext("../data/docvars/two/*txt", docvarsfrom = "filenames",
                         docvarnames = c("id")
        )),
        "Fewer docnames supplied than existing docvars - last 2 docvars given generic names."
    )
    
    #TODO: What happens if you supply more docnames?
    
    expect_error(
        dvars(readtext("../data/docvars/two/*txt", docvarsfrom = "nonesuch"))
    )
    
    #  Docvars from both metadata and filename
    expect_equal(
        dvars(readtext("../data/docvars/csv/*", docvarsfrom = c("filenames"), docvarnames = c("id", "fruit"), text_field = "text")),
        data.frame(list(shape = c("round", NA), texture = c(NA, "rough"), id = c(1, 2), fruit = c("apple", "orange")), 
                   stringsAsFactors = FALSE)
    )
    
    # #  Docvars from both metadata and filename
    # expect_equal(
    #     dvars(readtext("../data/docvars/json/*", docvarsfrom = c("filenames", "metadata"), docvarnames = c("id", "fruit"), text_field = "text")),
    #     data.frame(list(id = c(1, 2), fruit = c("apple", "orange"), shape = c("round", NA), texture = c(NA, "rough")), stringsAsFactors = FALSE)
    # )
    
})

test_that("test docvars.readtext warning with field != NULL", {
    expect_identical(
        dvars(readtext("../data/fox/fox.txt")),
        data.frame(x = 1)[, 0, drop=FALSE]
    )
})

test_that("test that readtext encoding argument must be either length 1 or same length as the number of files", {
    expect_error(
        readtext(
            c("../data/fox/fox.txt", "../data/fox/fox.txt", "../data/fox/fox.txt", "../data/fox/fox.txt"),
            encoding = c("utf-8", "utf-8")
        ),
        "Encoding parameter must be length 1, or as long as the number of files"
    )
})

context("Loading a corpus from a zip archive")
test_that("A single-level zip file containing txt files can be loaded",{
    qc <- readtext("../data/zip/inauguralTopLevel.zip")
    expect_equal(nrow(qc), 57)
})




context("Loading an empty gzipped tar archive")
test_that("An empty tar.gz file raises an error",{
    expect_error(
        readtext("../data/empty/test.tar.gz"),
        "File '../data/empty/test.tar.gz' does not exist"
    )
})


test_that("test reading structured text files with different columns", {
    testcorpus <- readtext(
        "../data/fruits/*.csv",
        text_field = "text"
    )
    
    expect_equal(
        dvars(testcorpus),
        data.frame(list(
            color = c("green", "orange", NA, NA), 
            shape = c(NA, NA, "round", "long")
        ),
        stringsAsFactors = FALSE)
    )
    expected_texts <- c("apple", "orange", "apple", "banana")
    names(expected_texts) <- c("1.csv.1", "1.csv.2", "2.csv.1", "2.csv.2")
    expect_equal(
        as.character(testcorpus),
        expected_texts
    )
})




context("Tests of new readtext internals. If these fail, it doesn't necessarily affect the exposed API")

context("Tests for list_files")

test_that("Test function to list files", {
    
    expect_error(
        readtext:::list_files("nonesuch://example.org/test.txt"),
        "Unsupported URL scheme"
    )
    
    testExistingFile <- readtext:::get_temp()
    file.create(testExistingFile)
    expect_equal(readtext:::list_files(testExistingFile), 
                 testExistingFile)
    expect_equal(readtext:::list_files(paste0('file://', testExistingFile)), 
                 testExistingFile)
    
    
    # Test vector of filenames
    testExistingFile2 <- readtext:::get_temp()
    file.create(testExistingFile2)
    expect_equal(
        readtext:::list_files(c(testExistingFile, testExistingFile2)),
        sort(c(testExistingFile, testExistingFile2))
    )
    
    # TODO: Test vector of filename and URL
    expect_equal(
        readtext:::list_files(c(testExistingFile, testExistingFile2)),
        sort(c(testExistingFile, testExistingFile2))
    )
    
    file.remove(testExistingFile)
    expect_error(
        readtext:::list_files(testExistingFile),
        "File '' does not exist"
    )
    expect_equal(
        readtext:::list_files(testExistingFile, ignore_missing = TRUE),
        character(0)
    )
    
    
    #Test globbing
    tempdir <- readtext:::get_temp(directory = TRUE)
    
    file.create(file.path(tempdir, "1.tsv"))
    file.create(file.path(tempdir, "2.tsv"))
    file.create(file.path(tempdir, "10.tsv"))
    
    expect_equal(
        length(readtext:::list_files(paste0(tempdir, "/", "*.tsv" ))),
        3
    )
    
    expect_equal(
        length(readtext:::list_files(paste0(tempdir, "/", "?.tsv" ))),
        2
    )
    
    expect_error(
        length(readtext:::list_files(paste0(tempdir, "/", "?.txt" ))),
        "File '' does not exist"
    )
    
    
    # Test globbing subdir
    
    tempsubdir1 <- readtext:::get_temp(temp_dir = tempdir, directory = TRUE)
    tempsubdir2 <- readtext:::get_temp(temp_dir = tempdir, directory = TRUE)
    
    file.create(file.path(tempsubdir1, "1.tsv"))
    file.create(file.path(tempsubdir1, "2.tsv"))
    file.create(file.path(tempsubdir2, "1.tsv"))
    
    expect_equal(
        length(readtext:::list_files(paste0(tempdir, "/", "*/", "?.tsv" ))),
        3
    )
    
    
    expect_error(
        readtext:::list_files("http://example.org/test.nonesuch"),
        "Remote URL does not end in known extension."
    )
    
})
    
test_that("Test function to list files with remote sources", {
    skip_on_cran()
    expect_error(
      readtext:::list_files("https://www.google.com/404.txt"),
      ".*404.*"
    )
    
    expect_equal(
      dim(readtext("https://www.google.com/404.txt", ignore_missing_files = TRUE)),
      c(1,2)
    )
})


test_that("text vectors have names of the files they come from by default (bug 221)", {

        expect_equal(
            names(as.character(readtext("../data/fox/fox.txt"))),
            "fox.txt"
        )

        actual_names <- names(as.character(readtext(
            "../data/csv/test*.csv", text_field = "text"
        )))
        expect_true(
            setequal(
                c("test.csv.1", "test.csv.2", "test2.csv.1", "test2.csv.2", "test3.csv.1", "test3.csv.2"),
                actual_names
            )
        )

        actual_names <- names(as.character(readtext(
            "../data/glob/*.txt"
        )))
        expect_true(
            setequal(
                c("1.txt", "2.txt", "3.txt", "4.txt", "10.txt"),
                actual_names
            )
        )

        actual_names <- names(as.character(readtext(
            "../data/tar/test.tar"
        )))
        expect_true(
            setequal(
                c("test.txt", "test2.txt", "test3.txt", "test4.txt"),
                actual_names
            )
        )

}) 

test_that("test globbed tar file",{
    skip_on_cran()
    skip_on_os("linux")
    expect_equal(
        unname(as.character(readtext("../data/tar/*"))),
        c("Lorem ipsum", "brown fox", "Dolor sit", "The quick")
    )
})

test_that("test html file",{
    expected <- c("The quick brown fox \njumps over the lazy dog")
    names(expected) <- "html5.html"

    expect_equal(
        as.character(readtext("../data/html/html5.html")),
        expected
    )

})

test_that("test malformed html file",{
    skip_on_os("windows")
    expected <- c("The quick brown fox \n    \njumps over the lazy dog")
    names(expected) <- "malformed_html5.html"
    expect_equal(
        as.character(readtext("../data/html/malformed_html5.html")),
        expected
    )
})

test_that("test for pdf file", {
    skip_on_os("windows")
    expect_output(
        cat(as.character(readtext("../data/pdf/test.pdf"))),
        "The quick brown fox jumps over the lazy dog\n" 
    )
})

test_that("test for odt file", {
	expected <- c("The quick brown fox jumps over the lazy dog")
	names(expected) <- "test.odt"
	expect_equal(
		as.character(readtext("../data/odt/test.odt")),
		expected
	)
	
})

test_that("test for docx file", {
    expected <- c("The quick brown fox jumps over the lazy dog")
    names(expected) <- "test.docx"
    
    expect_equal(
        as.character(readtext("../data/docx/test.docx")),
        expected
    )
})

test_that("test for doc file", {
    skip_on_os("windows")  
    expected <- paste(rep(c("The quick brown fox jumps over the lazy dog."), 10), collapse = " ")
    names(expected) <- "test.doc"

    txts <- as.character(readtext("../data/doc/test.doc"))
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
    skip_on_os("linux")
    expect_equal(
        unname(as.character(readtext("../data/json/test*json", text_field = "text"))),
        c("Lorem ipsum", "Dolor sit", "The quick", "brown fox", "Now is the winter")
    )
    
    #  test.json and test2.json are newline-delimited json
    #  test3.json is a single json object
    expected_docvars <- data.frame(list(
        colour = c("green", "red", "orange", "blue", NA), 
        number = c(42, 99, 0, NA, 3)),
        stringsAsFactors = FALSE)
    expected_docvars <- expected_docvars[order(expected_docvars$number),]
    row.names(expected_docvars) <- NULL
    actual_docvars <- dvars(readtext("../data/json/test*json", text_field = "text"))
    actual_docvars <- actual_docvars[order(actual_docvars$number),]
    row.names(actual_docvars) <- NULL
    row.names(actual_docvars)
    row.names(expected_docvars)
    expect_equal(
        actual_docvars,
        expected_docvars
    )
    
    expect_error(
        readtext("../data/json/test*json", text_field = 1),
        "Cannot use numeric text_field with json file"
    )
    
    expect_error(
        readtext("../data/json/test3.json", text_field = "nonesuch"),
        "There is no field called nonesuch in file"
    )
    
    
    # Twitter json files
    tweetSource <- readtext("../data/tweets/stream.json", source = "twitter")
    
    expect_equal(
        as.character(tweetSource),
        c(stream.json.1 = "I jumped over the lazy @dog", stream.json.2 = "Yawn")
    )
    
    expect_equal(
        dvars(tweetSource)$statuses_count,
        c(16204, 200)
    )
    
    expect_equal(
        dvars(tweetSource)$screen_name,
        c("foxxy", "dog")
    )
    
    
})

if (.Platform$OS.type == "unix") {
    test_that("test readtext with folder", {
        expect_equal(
            length(readtext("../data/fruits")$text),
            7
        )
    })
}    



context("Loading a corpus from a tar archive")
test_that("A single-level tar file containing txt files can be loaded",{
    skip_on_cran()
    skip_on_os("linux")
    expect_equal(
        unname(as.character(readtext("../data/tar/test.tar"))),
        c("Lorem ipsum", "brown fox", "Dolor sit", "The quick")
    )
})

context("Loading a corpus from a gzipped tar archive")
test_that("A single-level tar.gz file containing txt files can be loaded",{
    skip_on_cran()
    skip_on_os("linux")
    expect_equal(
        unname(as.character(readtext("../data/targz/test.tar.gz"))),
        c("Lorem ipsum", "brown fox", "Dolor sit", "The quick")
    )
})

context("Loading a corpus from a bzipped tar archive")
test_that("A single-level tar.bz file containing txt files can be loaded",{
    skip_on_cran()
    skip_on_os("linux")
    skip_on_os("windows")
    expect_equal(
        unname(as.character(readtext("../data/tarbz/test.tar.bz"))),
        c("Lorem ipsum", "brown fox", "Dolor sit", "The quick")
    )
})

context("Tests for verbosity argument")
test_that("test warning for unrecognized filetype", {
       expect_warning(
           readtext("../data/empty/empty.nonesuch"),
           paste0("Unsupported extension ", sQuote("nonesuch"), " of file")
       )
       expect_warning(
           readtext("../data/empty/empty.nonesuch", verbosity = 3),
           paste0("Unsupported extension ", sQuote("nonesuch"), " of file")
       )
       expect_warning(
           readtext("../data/empty/empty.nonesuch", verbosity = 2),
           paste0("Unsupported extension ", sQuote("nonesuch"), " of file")
       )
       expect_warning(
           readtext("../data/empty/empty.nonesuch", verbosity = 1),
           paste0("Unsupported extension ", sQuote("nonesuch"), " of file")
       )
       expect_silent(
           readtext("../data/empty/empty.nonesuch", verbosity = 0)
       )
})

test_that("messages from list_file",{
    expect_silent(
        readtext("../data/zip/inauguralTopLevel.zip", verbosity = 0)
    )
    expect_silent(
        readtext("../data/zip/inauguralTopLevel.zip", verbosity = 1)
    )
    expect_message(
        readtext("../data/zip/inauguralTopLevel.zip", verbosity = 2),
        "Reading texts from \\.\\./data/zip/inauguralTopLevel\\.zip"
    )
    expect_message(
        readtext("../data/zip/inauguralTopLevel.zip", verbosity = 3),
        "reading \\(txt\\) file: .*1789-Washington\\.txt"
    )
})

test_that("readtext called with textfield works with deprecation warning", {
    expect_equal(
        nrow(readtext("../data/csv/test*.csv", text_field = "text")),
        6
    )
    expect_equal(
        nrow(dvars(readtext("../data/csv/test*.csv", text_field = "text"))),
        6
    )
    expect_equal(
        length(as.character(readtext("../data/csv/test*.csv", text_field = "text"))),
        6
    )
    expect_warning(
        readtext("../data/csv/test*.csv", textfield = "text"),
        "textfield is deprecated; use text_field instead"
    )
})


test_that("tests for Excel files", {

    expect_equal(unname(as.character(
        readtext("../data/xls/test.xlsx", text_field = "text"))),
        c("The quick", "brown fox", "jumps over", "the lazy dog.")
    )
    expect_that(
        dvars(readtext("../data/xls/test.xlsx", text_field = "text")),
        equals(data.frame(list(
                        colour = c("orange", "blue", "pink", "pink"),
                        number = c(0, NA, NA, NA),
                        taste = c(NA, NA, "sweet", "umami")
                        ), stringsAsFactors = FALSE))
    )


    expect_equal(
        as.character(readtext("../data/xls/test.xls", text_field = "text")),
        c("test.xls.1" = "The quick", "test.xls.2" = "brown fox", 
          "test.xls.3" = "jumps over", "test.xls.4" = "the lazy dog.")
    )
    expect_that(
        dvars(readtext("../data/xls/test.xls", text_field = "text")),
        equals(data.frame(list(
                        colour = c("orange", "blue", "pink", "pink"),
                        number = c(0, NA, NA, NA),
                        taste = c(NA, NA, "sweet", "umami")
                        ), stringsAsFactors = FALSE))
    )


})

test_that("tests for ODS files", {
    expect_equal(unname(as.character(
        readtext("../data/ods/test.ods", text_field = "text"))),
        c("The quick", "brown fox", "jumps over", "the lazy dog.")
    )
    expect_identical(
        readtext("../data/ods/test.ods", text_field = "text"),
        structure(list(doc_id = c("test.ods.1", "test.ods.2", "test.ods.3", 
                                  "test.ods.4"), text = c("The quick", "brown fox", "jumps over", 
                                                          "the lazy dog."), colour = c("orange", "blue", "pink", "pink"
                                                          ), number = c(0L, NA, NA, NA), taste = c(NA, NA, "sweet", "umami"
                                                          )), row.names = c(NA, -4L), class = c("readtext", "data.frame"
                                                          ))
    )

})


test_that("rases error when source is not valid", {
    
    expect_error(
        readtext('../data/nexis/sun_2000-11-01_0001.html', source = 1),
        'source must be a character'
    )
    expect_error(
        readtext('../data/nexis/sun_2000-11-01_0001.html', source = 'something'),
        "'nexis' is the only source type available for HTML."
    )
    expect_silent(
        readtext('../data/nexis/sun_2000-11-01_0001.html', source = 'nexis')
    )
    
    expect_error(
        readtext('../data/tweets/stream.json', source = 1),
        'source must be a character'
    )
    
    expect_error(
        readtext('../data/tweets/stream.json', source = 'something'),
        "'twitter' is the only source type available for json"
    )
    
    expect_silent(
        readtext('../data/tweets/stream.json', source = 'twitter')
    )
    
})

test_that("readtext works with one-column csv files (#138)", {
    expect_equivalent(
        readtext("../data/csv/data_onecol.csv"),
        data.frame(doc_id = paste("data_onecol.csv", 1:2, sep = "."),
                   text = c("foo foo foo foo", "bar bar bar bar"),
                   stringsAsFactors = FALSE)
    )
    expect_equivalent(
        readtext("../data/csv/data_twocol.csv"),
        data.frame(doc_id = paste("data_twocol.csv", 1:2, sep = "."),
                   text = c("foo foo foo foo", "bar bar bar bar"),
                   y = 1:2,
                   stringsAsFactors = FALSE)
    )
    expect_equivalent(
        readtext("../data/csv/data_twocol.csv", text_field = "x", docid_field = "y"),
        data.frame(doc_id = c("1", "2"),
                   text = c("foo foo foo foo", "bar bar bar bar"),
                   stringsAsFactors = FALSE)
    )
})

test_that("tests for ODS files", {
    expect_identical(
        unname(as.character(readtext("../data/rtf/*.rtf"))),
        c("The quick brown fox jumps over the lazy dog",
          "This is an example of “rich text” format.")
    )
})

test_that("tests for files with doc_id", {
    expect_identical(
        as.character(readtext("../data/csv/withdocid.csv", docid_field = "doc_id", text_field = "text")),
        c(doc1 = "The quick", doc2 = "brown fox", doc3 = "jumped over")
    )
    expect_identical(
        as.character(readtext("../data/csv/withdocid.csv", text_field = "text"))[1],
        c(withdocid.csv.1 = "The quick")
    )
    expect_identical(
        as.character(readtext("../data/ods/withdocid.ods", docid_field = "doc_id", text_field = "text")),
        c(doc1 = "The quick", doc2 = "brown fox", doc3 = "jumped over")
    )
    expect_identical(
        as.character(readtext("../data/xls/withdocid.xls", docid_field = "doc_id", text_field = "text")),
        c(doc1 = "The quick", doc2 = "brown fox", doc3 = "jumped over")
    )
    expect_identical(
        as.character(readtext("../data/json/withdocid.json", docid_field = "doc_id", text_field = "text")),
        c(doc1 = "Lorem ipsum", doc2 = "Dolor sit")
    )
    expect_message(
        readtext("../data/csv/withdocid.csv", text_field = "text"),
        'A field called "doc_id" exists in the file. If you intend to use it as a document identifier, use "docid_field" option.')
    expect_error(
        readtext("../data/xls/withdocid.xls", docid_field = "nonesuch"),
        "There is no field called nonesuch"
    )
    expect_error(
        readtext("../data/xls/withdocid.xls", docid_field = 9000),
        "There is no 9000th field"
    )  
})
