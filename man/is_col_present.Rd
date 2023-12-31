% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-dataframe.R
\name{is_col_present}
\alias{is_col_present}
\title{Check for existence of a set of columns in a dataframe}
\usage{
is_col_present(df, ...)
}
\arguments{
\item{df}{a dataframe to test}

\item{...}{the column names (unquoted)}
}
\value{
TRUE if the columns are all there, false otherwise
}
\description{
Check for existence of a set of columns in a dataframe
}
\examples{
is_col_present(iris, Species, Petal.Width)
}
