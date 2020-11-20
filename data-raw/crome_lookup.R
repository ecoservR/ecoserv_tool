###########################################################################
### Script to create the builtin CROME lookup table (vector version)    ###
### The object is built internally and called when needed               ###
### Version: Crop Map of England 2019                                   ###
###########################################################################

## Import CROME legend

crome <- read.csv("C://ecoservR_package_support/CROME_legend.csv", sep = ";")

# Keep only code and description

crome <- crome %>% dplyr::select(LUCODE, label = Land.Use.Description)

# Rename columns

names(crome) <- c("numcode", "desc")


crome_lookup <- as.factor(setNames(crome$desc, crome$numcode))  # turn into named vector for later

usethis::use_data(crome_lookup)
