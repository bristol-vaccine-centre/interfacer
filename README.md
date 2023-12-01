
# interfacer

<!-- badges: start -->
[![R-CMD-check](https://github.com/bristol-vaccine-centre/interfacer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bristol-vaccine-centre/interfacer/actions/workflows/R-CMD-check.yaml)
[![DOI](https://zenodo.org/badge/667791472.svg)](https://zenodo.org/badge/latestdoi/667791472)
[![interfacer status
badge](https://bristol-vaccine-centre.r-universe.dev/badges/interfacer)](https://bristol-vaccine-centre.r-universe.dev)
<!-- badges: end -->

`Interfacer` is primarily aimed at R package developers. It provides a framework
for specifying the structure of dataframes as parameters for user functions and
checking that user supplied dataframes conform to expectations. Missing columns
or incorrectly typed columns can be identified and useful error messages
returned. Specifying structure is part of the function definition and can be
automatically included in Roxygen documentation.

## Installation

This package is hosted in the [Bristol Vaccine Centre
r-universe](https://https://bristol-vaccine-centre.r-universe.dev/).
Installation from there is as follows:

``` r
options(repos = c(
  "bristol-vaccine-centre" = 'https://https://bristol-vaccine-centre.r-universe.dev/',
  CRAN = 'https://cloud.r-project.org'))

# Download and install interfacer in R
install.packages("interfacer")
```

You can install the development version of interfacer from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bristol-vaccine-centre/interfacer")
```

More likely though you will be including this in another package via a
DESCRIPTION file:

```yaml
...
Imports: 
    tidyverse,
    interfacer
Remotes: github::bristol-vaccine-centre/interfacer
Suggests: 
    knitr,
    rmarkdown
...
```

## Example

`interfacer` is used then within a function definition in a new package to 
constrain the input of a function to be a particular shape.

```r
#' An example function
#'
#' @param mydata `r interfacer::idocument(example_fn, mydata)`
#' @param another an example   
#' @param ... not used
#'
#' @return ... something not yet defined ...
#' @export
example_fn = function(
  
  # this parameter will be a dataframe with id and test columns
  mydata = interfacer::iface(
    id = integer + group_unique ~ "an integer ID",
    test = logical ~ "the test result"
  ),
  
  another = "value",
  ...
  
) {
  # this line enforces the `iface` rules for the dataframe, coercing columns
  # if possible and throwing helpful errors if not.
  mydata = interfacer::ivalidate(mydata, ...)
  # rest of function body ...
}
```

