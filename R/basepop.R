

# TODO:
# make wrapper following the same input-output format as smooth_flexible(),
# modified to the needs of basepop.

# We either have 5-year-in - 5-year out, single-year-in - single-year-out,
# or abridged-in - abridged out.

# for the case of 5-year-in 5-year out, one can either graduate to single, 
# run basepop_single(), then group to 5-year, or one can graduate to single (pclm),
# abridge ages, then run basepop_five(). Let's do this second one; example seen
# in DemoTools documentation of basepop_five()

# age regrouping should be handled elsewhere.
# TR recommends first graduating and then running basepop, 
# not the other way around

# Two main DemoTools functions to use are basepop_five() and basepop_single().

remotes::install_github("timriffe/DemoTools")
















