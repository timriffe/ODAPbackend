% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lifetables.R
\name{lt_summary}
\alias{lt_summary}
\title{lt_summary}
\usage{
lt_summary(data_out)
}
\arguments{
\item{data_out}{a data.frame. The data.frame output of the lt_flexible function.}
}
\value{
A list with 2 data.frames containing the information on the following usefull statistics
e0 - life expectancy at birth
e65 - life expectancy at age 65
\code{S[1]} - The standard deviation in age at death from birth
\code{S[11]} - The standard deviation in age at death from age 10
IQR1 - interquartile range survivorship age from a lifetable
IQR2 - interquartile range survivorship age from a lifetable
IQR3 - interquartile range survivorship age from a lifetable
mod_age - modal age at death
q15_45 - probability that the person ages 54 will die at age 60
}
\description{
Creates a table of useful summary statistics from the lifetable.
}
\examples{
\dontrun{
Exposures <- c(100958,466275,624134,559559,446736,370653,301862,249409,
               247473,223014,172260,149338,127242,105715,79614,53660,
               31021,16805,8000,4000,2000,1000)

Deaths <- c(8674,1592,618,411,755,1098,1100,1357,
            1335,3257,2200,4023,2167,4578,2956,4212,
            2887,2351,1500,900,500,300)

Age = c(0, 1, seq(5, 100, by = 5))
data_out <- 
  lt_flexible(data_in,
              OAnew     = 100,
              age_out   = "single",  
              extrapFrom = 80,
              extrapFit = NULL, 
              radix     = 1e+05,
              extrapLaw = NULL,
              SRB       = 1.05,
              a0rule    = "ak",
              axmethod  = "un")
              
lt_summary(data_out)
}

}
