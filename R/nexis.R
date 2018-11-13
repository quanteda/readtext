#' extract texts and meta data from Nexis HTML files
#'
#' This extract headings, body texts and meta data (date, byline, length,
#' section, edition) from items in HTML files downloaded by the scraper.
#' @param path either path to a HTML file or a directory that contains HTML
#'   files
#' @param paragraph_separator a character to separate paragraphs in body texts
#' @inheritParams readtext
#' @param ... only to trap extra arguments
#' @import stringi
#' @keywords internal
#' @examples
#' \dontrun{
#' irt <- readtext:::get_nexis_html('tests/data/nexis/irish-times_1995-06-12_0001.html')
#' afp <- readtext:::get_nexis_html('tests/data/nexis/afp_2013-03-12_0501.html')
#' gur <- readtext:::get_nexis_html('tests/data/nexis/guardian_1986-01-01_0001.html')
#' sun <- readtext:::get_nexis_html('tests/data/nexis/sun_2000-11-01_0001.html')
#' spg <- readtext:::get_nexis_html('tests/data/nexis/spiegel_2012-02-01_0001.html', 
#'                                   language_date = 'german')
#' 
#' all <- readtext('tests/data/nexis', source = 'nexis')
#' all <- readtext('tests/data/nexis', source = 'nexis')
#' }
get_nexis_html <- function(path, paragraph_separator = "\n\n",
                           verbosity, ...) {

    #language_date <- match.arg(language_date)

    line <- readLines(path, warn = FALSE, encoding = "UTF-8")
    html <- paste0(fix_html(line), collapse = "\n")

    #Load as DOM object
    # dom <- htmlParse(html, encoding = "UTF-8")
    dom <- xml2::read_html(html)
    data <- data.frame()
    for (doc in xml2::xml_find_all(dom, "//doc")) {
        data <- rbind(data, extract_attrs(doc, paragraph_separator, verbosity))
    }
    colnames(data) <- c("pub", "edition", "date", "byline", "length", "section", "head", "body")
    data$file <- basename(path)

    return(data)
}

#' @import stringi
extract_attrs <- function(node, paragraph_separator, verbosity) {

    attrs <- list(pub = "", edition = "", date = "", byline = "",
                  length = "", section = "", head = "", body = "")

    # if (language_date == "german") {
    #     regex <- paste0(c("([0-9]{1,2})",
    #                       "[. ]+(Januar|Februar|März|Maerz|April|Mai|Juni|Juli|August|September|Oktober|November|Dezember)",
    #                       "[ ]+([0-9]{4})",
    #                       "([ ]+(Montag|Dienstag|Mittwoch|Donnerstag|Freitag|Samstag|Sonntag))?",
    #                       "([, ]+(.+))?"), collapse = "")
    # } else {
    #     regex <- paste0(c("(January|February|March|April|May|June|July|August|September|October|November|December)",
    #                       "[, ]+([0-9]{1,2})",
    #                       "[, ]+([0-9]{4})",
    #                       "([,; ]+(Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday))?",
    #                       "([, ]+(.+))?"), collapse = "")
    # }

    n_max <- 0;
    i <- 1
    #print(node)
    for (div in xml2::xml_find_all(node, ".//div")) {

        #str <- xmlValue(div, ".//text()")
        str <- xml2::xml_text(div)
        str <- clean_text(str)
        n <- stri_length(str);
        if (is.na(n)) next

        #cat("----------------\n")
        #cat(i, stri_trim(s), "\n")

        if (i == 2) {
            attrs$pub <- stri_trim(str)
        } else if (i == 3) {
            attrs$date <- stri_trim(str)
            # if (raw_date) {
            #     attrs$date <- stri_trim(str)
            # } else {
            #     m <- stri_match_first_regex(str, regex)
            #     if (all(!is.na(m[1,2:4]))) {
            #         date <- paste0(m[1,2:4], collapse = " ")
            #         if (language_date == "german") {
            #             datetime <- stri_datetime_parse(date, "d MMMM y", locale = "de_DE")
            #         } else {
            #             datetime <- stri_datetime_parse(date, "MMMM d y", locale = "en_EN")
            #         }
            #         attrs$date <- stri_datetime_format(datetime, "yyyy-MM-dd")
            #     }
            #     if (!is.na(m[1,8])) {
            #         attrs$edition <- stri_trim(m[1,8])
            #     }
            # }
        } else if (i == 4) {
            attrs$head <- stri_trim(str)
        } else if (i >= 5) {
            if (stri_detect_regex(str, "^BYLINE: ")) {
                attrs$byline <- stri_trim(stri_replace_first_regex(str, "^BYLINE: ", ""))
            } else if (stri_detect_regex(str, "^SECTION: ")) {
                attrs$section <- stri_trim(stri_replace_first_regex(str, "^SECTION: ", ""));
            } else if (stri_detect_regex(str, "^LENGTH: ")) {
                attrs$length <- stri_trim(stri_replace_all_regex(str, "[^0-9]", ""))
            } else if (!is.null(attrs$length) && n > n_max &&
                       !stri_detect_regex(str, "^(BYLINE|URL|LOAD-DATE|LANGUAGE|GRAPHIC|PUBLICATION-TYPE|JOURNAL-CODE): ")){
                ps <- xml2::xml_find_all(div, ".//p")
                p <- sapply(ps, xml2::xml_text)
                attrs$body <- stri_trim(paste0(p, collapse = paste0(" ", paragraph_separator, " ")))
                n_max <- n
            }
        }
        i <- i + 1
    }
    if (verbosity <= 1) {
        if (attrs$pub[1] == "" || is.na(attrs$pub[1]))
            warning("Failed to extract publication name")
        if (attrs$date[1] == "" || is.na(attrs$date[1]))
            warning("Failed to extract date")
        if (attrs$head[1] == "" || is.na(attrs$head[1]))
            warning("Failed to extract heading")
        if (attrs$body[1] == "" || is.na(attrs$body[1]))
            warning("Failed to extract body text")
    }
    return(as.data.frame(attrs, stringsAsFactors = FALSE))
}

#' @import stringi
clean_text <- function(str) {
    str <- stri_replace_all_regex(str, "[[:^print:]]", " ");
    str <- stri_replace_all_regex(str, "[\\r\\n\\t]", " ")
    str <- stri_replace_all_regex(str, "\\s\\s+", " ")
    str <- stri_trim(str);
    return(str)
}

#' @import stringi
fix_html <- function(line){
    line <- stri_replace_all_fixed(line, "<!-- Hide XML section from browser", "");
    line <- stri_replace_all_fixed(line, "<DOCFULL> -->", "<DOCFULL>");
    line <- stri_replace_all_fixed(line, "</DOC> -->", "</DOC>");
    line <- stri_replace_all_fixed(line, "<BR>", "<BR> ");
    return(line)
}
