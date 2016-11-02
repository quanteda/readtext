context('test encodings.R')


test_that("test if encoding() detects encodings correctly", {
	  #     skip_on_cran()
	  #     skip_on_travis()
	FILEDIR <- '../data/encoding'

	filenames <- list.files(FILEDIR, "*__characters.txt$")
	parts <- strsplit(gsub(".txt$", "", filenames), "__")
	fileencodings <- sapply(parts, "[", 1)

	filenames <- file.path(FILEDIR, paste0(fileencodings,  "__characters.txt"))

	for (i in 1:length(fileencodings)) {
		filename <- filenames[[i]]
		detected_encoding <- encoding2(filename)$probably
		actual_encoding <- fileencodings[[i]]
		test_that(paste("test textfile encoding parameter, encoding", actual_encoding), {
			expect_equal(detected_encoding, actual_encoding)
		})
	}

})
