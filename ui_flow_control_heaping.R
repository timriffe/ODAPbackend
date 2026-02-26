# TODO update this to show operations on grouped data w .id

devtools::load_all()
library(dplyr)

# single-age input data
data_in <- readr::read_csv(system.file("extdata",
                                       "dat_heap_smooth.csv.gz",
                                       package="ODAPbackend"))
data_in <- data_in[,-4]

# standard checks run before doing the thing.
initial_data_checks <- check_data(data_in)
initial_data_checks

# have user specify which column is to be checked for heaping.
# i.e. just one column at a time; the same heaping measures could
# be used for any count data.
heaping_exposure <- check_heaping_general(data_in, "Exposures")
heaping_exposure
# what should be displayed? Just a single pane, a heat table of the results (fill in color column)? Include our badness indicators. Same
# table as in the lifetable sequence. Possibly we want translation ability for this table?

# this could can be run on single _or_abridged / 5-year data. For five-year data, only the second two measures are run.


# abridged input data
data_in <- readr::read_csv(system.file("extdata",
                                       "abridged_hmd_spain.csv.gz",
                                       package="ODAPbackend"))

# standard checks run before doing the thing.

initial_data_checks <- check_data(data_in)
initial_data_checks

# have user specify which column is to be checked for heaping.
# i.e. just one column at a time; the same heaping measures could
# be used for any count data.
heaping_exposure <- check_heaping_general(data_in, "Exposures")


# TBH I've never seen these two measures disagree so much!
# roughness is getting it wrong this time, so we better pair
# with a visualization. Oh I see it's because check_heaping_general()
# does not yet use .id. It needs an update to do this.



