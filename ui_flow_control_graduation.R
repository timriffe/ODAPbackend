devtools::load_all()
library(dplyr)
data_in <- readr::read_csv(system.file("extdata",
                                       "abridged_hmd_spain.csv.gz",
                                       package="ODAPbackend"))

# standard checks run before doing the thing.
initial_data_checks <- check_data(data_in)
initial_data_checks


head(data_in) # has .id column

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


output$data_out # valid output

# figure separate for each .id
print(output$figures[[1]]$figure)
args(smooth_flexible)

# user-specified arguments:
# data_in, incoming data
# variable: which count variable do we graduate?
# method: graduation method. This arg is called fine_method in smooth_flexible(), 
#         but here we can call it 'method', and pass that to fine_method. 
#         Options include: "auto", "none", "sprague", "beers(ord)", 
#                          "beers(mod)", "grabill", "pclm", "mono", "uniform"

# constrain_infants = TRUE; only relevant if incoming data in abridged
# Sex = "t": only ask if method = "auto" AND there is no Sex column in data_in; 
# user options "Male", "Female", "Total", function feeder values: "m","f","t", as elsewhere

# fixed arguments:
# age_out = "single" (by definition)
# rough_method = "none"
# by_args = NULL (.id created earlier)
# i18n handled by frontend.

