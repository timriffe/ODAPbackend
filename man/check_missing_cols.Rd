% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkers.R
\name{check_missing_cols}
\alias{check_missing_cols}
\title{\code{check_missing_cols}}
\usage{
check_missing_cols(data)
}
\arguments{
\item{data}{tibble. A tibble generated by the output of the read_data function.}
}
\value{
A data.frame with 3 columns: message - \code{check} - the name of the test, \code{message} - The error message with corresponding information generated if the test is failed (if the test is passed evaluates to NA), \code{pass} - binary result of the test either "Fail" or "Pass".
}
\description{
Check if any of the crucial columns are missing from the data: (\code{Deaths}, \code{Exposures}, \code{Age}).
}
\examples{
 library(tibble)
Exposures <- c(100958,466275,624134,559559,446736,370653,301862,249409,
              247473,223014,172260,149338,127242,105715,79614,53660,
              31021,16805,8000,4000,2000,1000)

Deaths <- c(8674,1592,618,411,755,1098,1100,1357,
           1335,3257,2200,4023,2167,4578,2956,4212,
           2887,2351,1500,900,500,300)

data <- tibble(Deaths = Deaths,
                       Exposures = Exposures,
                       Age = c(0, 1, seq(5, 100, by = 5)),
                       AgeInt = c(diff(Age), NA))

check_missing_cols(
    data = data)

}
