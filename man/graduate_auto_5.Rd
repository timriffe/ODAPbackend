% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graduation.R
\name{graduate_auto_5}
\alias{graduate_auto_5}
\title{\code{graduate_auto_5}}
\usage{
graduate_auto_5(dat_5, variable)
}
\arguments{
\item{dat_5}{tibble or data.frame. A tibble with two columns - \code{Population} - or any other chosen numeric variable that comes in 5-year age groups with counts to be graduated and corresponding numeric \code{Age} column (lower bound of age group).}

\item{variable}{character. A scalar with the \code{variable} name which is to be graduated. Supported variable names are \code{Pop}, \code{Population}, \code{Exposures}, \code{Exp}, or \code{Death}.}
}
\value{
A tibble with 2 columns - your chosen \code{variable} with graduated and smoothed counts and \code{Age}

data_out. A tibble with two numeric columns - smoothed counts for the chosen \code{variable} and \code{Age} - chosen age grouping
}
\description{
Implements the graduation with method protocol procedure for data with 5-year age groups.
}
\examples{
library(readr)
library(dplyr)
fpath <- system.file("extdata", 
"five_hmd_spain.csv.gz", 
package = "ODAPbackend")
data_in <- read_csv(fpath) |>
  dplyr::select(-1) |>
  filter(.id == 1)

graduate_auto_5(data_in, "Exposures")$Exposures

}
