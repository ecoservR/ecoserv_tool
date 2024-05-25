#######################################
### Wrapper functions for mod01     ###
### for EcoservR                    ###
### Sandra Angers-Blondin           ###
### 29 March 2021                   ###
#######################################

#' Grid Study Area
#'
#' Not intended to be called directly by user. Takes a study area and intersects it with the OS 10km x 10km grid to assign a grid reference to each square and enable serial processing of MasterMap.

#' @param x A sf polygon representing the study area
#' @return A named list of tile boundaries (clipped to study area shape).
#'
#' @export
grid_study <- function(x){

   # Check that study area is same projection as EcoservR's grid object
   x <- checkcrs(x, 27700)

   # Intersect study area with grid
   SAgrid <- suppressWarnings(sf::st_intersection(sf::st_set_crs(grid, 27700), sf::st_geometry(x)))
   SAgrid$TILE_NAME <- droplevels(SAgrid$TILE_NAME)  # drop tile names not occuring in study area
   SAgrid <- split(SAgrid, SAgrid$TILE_NAME)    # convert to list
   SAgrid <- lapply(SAgrid, function(y) y %>%   # tidy up geometries (if study area has funky shape a tile might have disconnected parts of polygons)
                       sf::st_union() %>%
                       sf::st_cast(to="MULTIPOLYGON") %>%
                       sf::st_make_valid() %>% sf::st_as_sf()
   )

}


#' Get File Extent
#'
#' Not intended to be called directly by user. Queries a list of files on disk and extracts only their spatial extent (without reading the file in memory.) Used to associate file names to OS 10km grid tiles for faster Mastermap loading in prepare_basemap().

#' @param fileList a list of file paths. For shapefiles, should be the dsn path (folder); for other spatial formats, should be the full path including extension.
#' @param layer the layer name. For shapefiles, this correspond to the file name and should be a list of same length as fileList. For other spatial formats, layer names can be obtained in a GIS system or by using st_layers() and should be a character string.
#' @return A list of rectangular polygons corresponding to the extent of each file in fileList.
#'
#' @export
getFileExtent <- function(fileList, layer){

   message("Detecting spatial extent of MasterMap data...")

   if(inherits(layer, "list")){ # workflow for shapefiles

      ex <- mapply(function(x, n){

         info <- rgdal::ogrInfo(x, layer = n)  # extract info on extent and CRS

         # create and return the object
         raster::extent(info$extent %>%
                           matrix(., byrow=FALSE, ncol =2)) %>%
            methods::as(., "SpatialPolygons") %>%
            sf::st_as_sf() %>% sf::st_set_crs(info$wkt2)
      },
      x = fileList,
      n = layer,
      SIMPLIFY = FALSE)

      # add the file name to the extent object and bind as dataframe
      ex <- mapply(function(x, dsn, l) {dplyr::mutate(x, file = dsn,
                                                      layername = l)},
                   x = ex,
                   dsn = fileList,
                   l = layer,
                   SIMPLIFY = FALSE) %>% do.call(rbind, .)




   } else if (inherits(layer, "character")){ # workflow for other files

      ex <- lapply(fileList, function(x){

         info <- rgdal::ogrInfo(x, layer = layer)  # extract info on extent and CRS

         # create and return the object
         raster::extent(info$extent %>%
                           matrix(., byrow=FALSE, ncol =2)) %>%
            methods::as(., "SpatialPolygons") %>%
            sf::st_as_sf() %>% sf::st_set_crs(info$wkt2)
      })

      # add the file name to the extent object and bind as dataframe
      ex <- mapply(function(x, n) {dplyr::mutate(x, file = n,
                                                 layername = layer)},
                   x = ex,
                   n = fileList,
                   SIMPLIFY = FALSE) %>% do.call(rbind, .)

   }


   return(ex)

}


