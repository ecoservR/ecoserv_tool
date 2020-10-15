###########################################################################
### Script to create the builtin corine lookup table (vector version)   ###
### The object is built internally and called when needed               ###
### Version: CORINE CLC 2018 version 20                                 ###
###########################################################################

## Import CLC legend

clc <- read.csv("C://ecoservR_package_support/CLC_legend.csv", sep = ";")

# Keep only code and description

clc <- dplyr::select(clc, CLC_CODE, LABEL3)

# Rename columns

names(clc) <- c("numcode", "desc")


corine_lookup <- as.factor(setNames(clc$desc, clc$numcode))  # turn into named vector for later

usethis::use_data(corine_lookup)
