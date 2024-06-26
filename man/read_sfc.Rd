% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_sfc.R
\name{read_sfc}
\alias{read_sfc}
\title{Read SFC file}
\usage{
read_sfc(file, zone = "America/Lima")
}
\arguments{
\item{file}{Path file}

\item{zone}{Time zone}
}
\value{
A data frame
}
\description{
Import SURFACE output AERMET
}
\examples{
file_sfc <- system.file("extdata", "AERMET2.SFC", package="Raermod")
read_sfc(file_sfc)

}
