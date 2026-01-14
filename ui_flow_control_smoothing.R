# This script in progress, pending some directions

devtools::load_all()
library(dplyr)
data_in <- readr::read_csv(system.file("extdata",
                                       "abridged_hmd_spain.csv.gz",
                                       package="ODAPbackend"))

# standard checks run before doing the thing.
initial_data_checks <- check_data(data_in)
initial_data_checks


head(data_in) # has .id column

# Here explicitly we return the same age groups
# used in the input data.



output <- smooth_flexible(data_in,
                          variable = "Deaths", # user specifies this
                          age_out = "single", # always use this: not a user choice
                          fine_method = "sprague", # user specifies from list (see below),
                          rough_method = "none", # always use this: not a user choice
                          constrain_infants = TRUE, # in case abridged age 0 should be maintained as-is
                          Sex = "t", # If this is given as columnin the data, then it's detected. 
                          # Otherwise user should specify. 
                          by_args = NULL, # can be ignored if .id present; don't include Sex here.
                          # in ui this is handled earlier in group definitions
                          i18n = NULL # ui arg
)
















