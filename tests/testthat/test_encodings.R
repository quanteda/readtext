context('test encodings.R')


FILEDIR <- '../data/encoding'

filenames <- list.files(FILEDIR, "*__characters.txt$")
parts <- strsplit(gsub(".txt$", "", filenames), "__")
fileencodings <- sapply(parts, "[", 1)

fileencodings <- fileencodings[!(fileencodings %in% broken_encodings)]
filenames <- file.path(FILEDIR, paste0(fileencodings,  "__characters.txt"))

for (i in 1:length(fileencodings)) {
	filename <- filenames[[i]]
	detected_encoding <- encoding2(filename)$probably
	actual_encoding <- fileencodings[[i]]
	expect_equal(detected_encoding, actual_encoding)
}
