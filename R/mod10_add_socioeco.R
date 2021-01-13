#######################################
### Worflow functions               ###
### for EcoservR                    ###
### Sandra Angers-Blondin           ###
### 05-01-2021                      ###
#######################################


## The socioeconomic datasets are way too large to push to GitHub whole, and it would be wasteful to load the whole national dataset into memory anyway.

## Instead the socioeconomic module can be adapted to recognize which grid ref are needed, and only loads the relevant files.


## Once the package is installed with the lazy-load data in place in the inst/extdata folder, the actual path (user-specific) can be found with
# system.file("extdata/IMD", package = "ecoservR")
# we can therefore use list.files to get list of available tiles and only load those required (these are the only ones that will be actually loaded in memory) by using regex on tile names

test <- list.files(system.file("extdata/IMD", package = "ecoservR"))


