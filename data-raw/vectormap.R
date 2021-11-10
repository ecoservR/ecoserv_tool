###############################################################################
### Script to create the vectormap datasets                                 ###
### Airports, railways and roads from VectorMap District                    ###
###############################################################################

### Notes

## VectorMap District is a big dataset but airports, railways and major roads can be extracted as they are all we need. They get used in air purification demand and noise regulation demand


require(dplyr)
require(sf)

## Read in data

airports <- st_read("C://ecoservR_package_support/vectormap_for_package/airports_osvectormap.gpkg")
railways <- st_read("C://ecoservR_package_support/vectormap_for_package/railways_osvectormap.gpkg")
roads <- st_read("C://ecoservR_package_support/vectormap_for_package/roads_osvectormap.gpkg")

## Keep only strict minimum to decrease file size, tidy up

airports <- st_as_sf(st_geometry(airports)) %>% st_make_valid()

railways <- st_as_sf(st_geometry(railways)) %>% st_make_valid()

roads <- roads %>% mutate(
   classification = case_when(
      grepl("Motorway", classification) ~ "Motorway",
      grepl("Dual", classification) ~ "A Road", ## according to EcoservGIS the dual carriageways are A roads, and other A roads are minor (??) roads so this needs to come first
      grepl("A Road", classification) ~ "Minor Road",
      grepl("Primary Road", classification) ~ "Minor Road"
   )
) %>% dplyr::select(classification) %>% st_make_valid()

## save in external data

saveRDS(airports, file = "inst/extdata/vectormap/airports.RDS")
saveRDS(railways, file = "inst/extdata/vectormap/railways.RDS")
saveRDS(roads, file = "inst/extdata/vectormap/roads.RDS")



