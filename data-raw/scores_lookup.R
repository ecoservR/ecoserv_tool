###########################################################################
### Script to create the builtin lookup table for service models        ###
### The object is built internally and called when needed               ###
### Version: December 2020                                              ###
###########################################################################

## This needs to be run frequently on the latest version of the lookup table.
## (Still to do:
##### revise possible HabCodes
##### tidy up information
##### revise carbon scores
##### add carbon sequestration scores



## Import spreadsheet

hab_lookup <- read.csv("C://ecoservR_package_support/ecoserv_hab_lookup.csv", sep = ";")

## Any tidying up? all done in the spreadsheet



usethis::use_data(hab_lookup)
