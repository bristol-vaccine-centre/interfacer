% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/devtools.R
\name{iclip}
\alias{iclip}
\title{Create an \code{iface} specification from an example dataframe}
\usage{
iclip(df, df_name = deparse(substitute(df)))
}
\arguments{
\item{df}{a prototype dataframe}

\item{df_name}{an optional name for the parameter (defaults to \verb{i_<df name>})}
}
\value{
nothing, populates clipboard
}
\description{
When developing with \code{interfacer} it is useful to be able to base a function
input off a prototype that you are for example using as testing. This
function generates an \code{interfacer::iface} specification for the supplied data
frame and copies it  to the clipboard so that it can be pasted into the
package code you are working on.
}
\details{
If the dataframe contains one or more list columns with nested dataframes
the nested dataframes are also defined using a second \code{iface} specification.
}
\examples{
if (interactive()) iclip(iris)
}
\concept{document}
