readtext v0.81
==============
* Moves some quanteda functions to this package: `docvars()`, `docnames()`, `texts()`
* Updates print method to use **pillar** instead of tibble
* Modernizes some of the **testthat** syntax.

readtext v0.81
==============
* Fixed a problem in the examples breaking CRAN checks on Solaris.
* Changed documentation to markdown.

readtext v0.80
==============
* Updated for compatibility with newer versions of the **readODS** and **pdftools** packages.
* ".DOCX" (with uppercase filename extensions) are now handled correctly.

readtext v0.76
==============
* Fixed a bug in the assignment function for document IDs that caused failure when reading a file with only a text_field and a specified docid_field.

readtext v0.75
==============
* Added docid_field argument for columnar data (#155).

readtext v0.74
==============

* Added support for RTF format (.rtf).


readtext v0.7.2
==============

* Added support for Open Document format (.odt).


readtext v0.7.2
==============

* Fixed #138, which caused single-column .csv-type files not to load correctly.


readtext v0.7.1
==============

*  Added `readtext_options()`, fixes #123.


readtext v0.7.0
==============

*  Move to **xml2** instead of the older **XML** package.  
*  Change options settings so that package can be used without loading it first.


readtext v0.5.0
==============

First CRAN release.
