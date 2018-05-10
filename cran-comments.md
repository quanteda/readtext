## Purpose: Bug fix in v0.70

As soon as v0.70 was on CRAN, Jeroen Oooms reported that we had broken some of the tests in the **cld2** package.  We have fixed this comprehensively now and are resubmitting for this reason.

While not noted in the notes with v0.70, that version also fixed a problem when the environment variable `_R_CHECK_LENGTH_1_CONDITION_` was set to `TRUE`. This version also fixes that.

Both conditions are now part of the unit tests.

## Test environments

* local OS X install, R 3.5.0
* ubuntu 16.04 LTS, R 3.4.4
* win-builder (devel and release)

## R CMD check results

No ERRORS, NOTES, WARNINGS.

## Reverse dependencies

No errors when running devtools::revdep_check().
