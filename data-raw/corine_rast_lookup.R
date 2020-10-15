###########################################################################
### Script to create the builtin corine lookup table (rasterversion)    ###
### The object is built internally and called when needed               ###
### Version: CORINE CLC 2018 version 20                                 ###
###########################################################################

## Import CLC legend

clc <- read.csv("C://ecoservR_package_support/CLC2018_CLC2018_V2018_20.txt", sep = ",", header=FALSE)

# Keep only code and description

clc <- dplyr::select(clc, V1,V6)

# Rename columns

names(clc) <- c("numcode", "desc")

corine_lookup_raster <- clc

usethis::use_data(corine_lookup_raster)
