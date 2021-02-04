###############################################################################
### Script to create the socioeconomic datasets                             ###
### Population data: adapted from Ecoserv-GIS v 3.3, based on 2011 census   ###
### IMD data: Index of Multiple Deprivation 2019                            ###
###############################################################################

### Notes

## TO DO: run with the updated Risk group (considering children as well as over 65). Code has been updated but not re-run as the tiles take so long to upload to Git.

## This script is meant to regenerate tiles for socio economic data whenever a new IMD or census dataset becomes available.
## Currently the population data is extracted straight from EcoservGIS: when 2021 census data becomes available this will need updating.

## IMPORTANT: When saving the tiles in inst folder, make sure that no individual dataset exceeds 100MB otherwise upload to GitHub will fail.


require(dplyr)

## Read in data

census <- readRDS("C://Basemapper/builtin/Census2011_lite/census2011.RDS")

imd <- readRDS("C://Basemapper/builtin/IMD_2019/IMD.RDS")


## Keep only strict minimum to decrease file size

imd <- dplyr::select(imd, health = HDDScore)

census <- census %>%
   dplyr::mutate(Risk_group = (under10 + o65plus)/all_people) %>%  # recalculate risk group to include children and elderly
   dplyr::select(housePop = House_Pop,
                 riskgroup = Risk_group)


## Tidy up geometry

imd <- ecoservR::checkgeometry(imd, "POLYGON")

census <- ecoservR::checkgeometry(census, "POLYGON")



# Split into grid tiles ---------------------------------------------------

## The datasets are way too large to push to GitHub, and it would be wasteful to load the whole national dataset into memory anyway.

## Instead the socioeconomic module can be adapted to recognize which grid ref are needed, and only loads the relevant files.


## We create the OS grid reference from the more detailed grid built into the tool

mygrid <- ecoservR::grid

mygrid <- mygrid %>% mutate(gridref = substr(TILE_NAME, 1, 2)) %>%  ## find 100 km x 100 km grid ref from letters
   rmapshaper::ms_dissolve(field = "gridref") %>%
   sf::st_as_sf() %>%
   sf::st_make_valid()



## Intersect both datasets with this grid; the grid ref will appear as a new attribute, which we can use to subset and save the data in chunks

imd <- imd %>% sf::st_intersection(mygrid)
census <- census %>% sf::st_intersection(mygrid)


## Clean the datasets again

imd <- ecoservR::checkgeometry(imd, "POLYGON")
census <- ecoservR::checkgeometry(census, "POLYGON")


## Split them in named lists based on grid ref

imd <- split(imd, imd$gridref)
census <- split(census, census$gridref)


mapply(function(x, n) {
   saveRDS(dplyr::select(x, -gridref), file = paste0("inst/extdata/IMD/IMD_", n, ".RDS"))},
   x = imd,
   n = names(imd))


mapply(function(x, n) {
   saveRDS(dplyr::select(x, -gridref), file = paste0("inst/extdata/census/census_", n, ".RDS"))},
   x = census,
   n = names(census))


## TQ census file is too big for GitHub; split into two

tq <- census$TQ

saveRDS(dplyr::select(tq[1:21000, ], -gridref), file = paste0("inst/extdata/census/census_", "TQa", ".RDS"))
saveRDS(dplyr::select(tq[21001:nrow(tq), ], -gridref), file = paste0("inst/extdata/census/census_", "TQb", ".RDS"))



