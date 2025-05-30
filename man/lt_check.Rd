% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lifetables.R
\name{lt_check}
\alias{lt_check}
\title{\code{lt_check}}
\usage{
lt_check(data_out)
}
\arguments{
\item{data_out}{a data.frame or tibble. The data.frame output of the \code{lt_flexible} function.}
}
\value{
A  figure where the users \code{e(0)} values would be indicated in color and HMD values would be black:
}
\description{
Creates a plot of \code{e(0)} values from Human Mortality Database (HMD) and compare it with those obtained by user.
}
\examples{
library(readr)
library(dplyr)
# single age data
fpath <- system.file("extdata", 
"single_hmd_spain.csv.gz", 
package = "ODAPbackend")
data_in <- read_csv(fpath) |>
  dplyr::select(-1)

data_out <- lt_flexible(
  data_in,
  OAnew      = 100,
  age_out    = "single",
  extrapFrom = 80,
  extrapFit  = NULL,  # Default NULL, computed later
  extrapLaw  = NULL,
  radix      = 1e+05,
  SRB        = 1.05,
  a0rule     = "Andreev-Kingkade",
  axmethod   = "UN (Greville)"
)

lt_check(data_out$data_out)

}
