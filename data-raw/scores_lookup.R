###########################################################################
### Script to create the builtin lookup table for service models        ###
### The object is built internally and called when needed               ###
### Version: December 2020                                              ###
###########################################################################

## This needs to be run frequently on the latest version of the lookup table.
## Still to do:
##### revise possible HabCodes
##### tidy up information
##### revise carbon scores (estuarine codes revised by Lucy Dowdall December 2020)
##### add carbon sequestration scores



## Import spreadsheet

#hab_lookup <- read.csv("C://ecoservR_package_support/ecoserv_hab_lookup.csv", sep = ";")

hab_lookup <- ecoservR::hab_lookup  # by default the table to fix should be in the package


# History of changes ------------------------------------------------------
# from March 2022 changing things from the package lookup table rather than from the excel sheet
# will need a full review someday

## 16 March 2022: fix cost for saltmarsh for mire network

#hab_lookup[hab_lookup$HabBroad == "Saltmarsh",]$CostMire <- 1
#hab_lookup[hab_lookup$HabBroad %in% c("Sand dune", "Intertidal"), ]$CostMire <- 10




usethis::use_data(hab_lookup, overwrite = TRUE)
