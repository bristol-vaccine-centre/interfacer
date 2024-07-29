## R CMD check results

Tested via github workflows on:
* macOS-latest / release
* windows-latest / release
* ubuntu-latest / release
* ubuntu-latest / oldrel-1
* ubuntu-latest / oldrel-2
* ubuntu-latest / devel
please see https://github.com/bristol-vaccine-centre/interfacer/actions/workflows/R-CMD-check.yaml

0 errors | 0 warnings | 1 note

* This is a resubmission to fix issues identified in CRAN submission v0.2.2:
1) additional spaces in DESCRIPTION file. FIXED
2) functions missing return values. FIXED
3) example containing unexported method. REMOVED
4) `if (FALSE)` stanza in examples for function that can only be used interactively. FIXED


* This is a new release.
* There are no references describing the methods in this package.

