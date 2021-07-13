## Purpose: Fix a problem in examples on Solaris

We were notified recently that there were problems with readtext that needed fixing, in the "donttest" category.  These are now not run in the examples.

## Test environments

* local OS X install, R 4.1.0
* win-builder (devel and release)

## R CMD check results

No ERRORS, NOTES, WARNINGS.

## Reverse dependencies

No errors when running `devtools::revdep_check()`.
