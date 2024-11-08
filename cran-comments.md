## R CMD check results

Tested via github workflows on:
* macOS-latest / release
* windows-latest / release
* ubuntu-latest / release
* ubuntu-latest / oldrel-1
* ubuntu-latest / oldrel-2
* ubuntu-latest / devel
please see https://github.com/bristol-vaccine-centre/interfacer/actions/workflows/R-CMD-check.yaml

0 errors | 0 warnings | 0 note

This release is fully backward compatible with no breaking changes to API. There 
are additional functions included for checking consistency of non-dataframe
parameters, recycling vector parameters, and inferring values of interdependent
parameters. These capabilities are described in a new vignette. The package now
has a logo.

* There are no reverse dependencies.
* There are no references describing the methods in this package.

