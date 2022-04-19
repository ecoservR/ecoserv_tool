####################################
### New extract function         ###
### For EcoservR                 ###
### 7 April 2022                 ###
####################################

## TESTED:
### numeric raster (dtm): OK
### categorical raster, continuous coverage (corine): OK
### categ. raster with NA (phi): OK: NA values MUST be recoded to a known code (8888) so that they are a class in themselves
### do small polygons get values?: yes, no need for centroid extraction


#' Extract Raster into Polygons
#'
#' This function extracts the information contained in raster tiles into the basemap. It operates a one-to-one matching using the tiles names. If unnamed, the workflow is slower and could leave out a small number of polygons.
#'
#' @param sf One sf object, i.e. a basemap tile
#' @param rast A list of raster tiles, ideally named to match mastermap tiles (as obtained through makeTiles())
#' @param fun The function to be applied in zonal stats. Either "mean" for numeric values, or "majority" for the most common occurrence.
#' @param tile The OS tile grid reference to match raster to mastermap tile; can be set dynamically with mapply
#' @param newcol The name of the new attribute with extracted values
#' @return An updated mm object with a new column containing the extracted values
#' @export
extractRaster <- function(sf, rast, fun, tile = NULL, newcol){
   # sf is one sf object (e.g. a basemap tile)
   # rast is a list of raster tiles
   # fun is the function to be applied for zonal stats: either "mean" (will omit NA values) or "majority"
   # tile is the OStile ref to match raster to mm tile; can be set dynamically in mapply. If null, will just do the old method of intersecting tiles and extracting everything that has a match.
   # newcol is the name of the new column with the extracted values


   ### Data checks ----

   # Is the raster input a list?
   if (inherits(rast, "RasterLayer")){
      rast <- list(rast)
   }
   # Is the mm a list of sf tiles?
   if (!inherits(sf, "sf")) stop("Basemap tiles must be sf objects")

   # Are raster list elements really rasters?
   stopifnot(inherits(rast, "list") & class(rast[[1]]) %in% c("raster", "RasterLayer"))

   ### Function definition ----

   if (fun == "mean"){
      funct <- function(x) mean(x, na.rm = TRUE)

   } else if (fun == "majority"){
      funct <- function(x) raster::modal(x, na.rm = TRUE)

      # when NA present, modal returns NA rather than the most frequent values.
      # When omitting, values are often leaked to adjacent polygons (if they have just 1 cell overlapping).
      # Solution: recode NAs and change back later (done in first step of loop)

   } else {stop("Invalid function specified for extractRaster.")}


   ## Geometry preparation

   # checking that geometry is polygons only
   if (sf::st_geometry_type(sf, by_geometry = FALSE) != "POLYGON"){
      sf <- checkgeometry(sf, "POLYGON")
   }


   # Create empty column in the mm object that we will fill with values
   sf[newcol] <- NA

   ##### MAIN EXTRACTION WORKFLOW -------------------------------------------------------

   ## This is the fastest way but will only work if mm and rast are named lists
   ## with a matching OS 10x10km grid reference code.

   if (!is.null(tile) & !is.null(names(rast))){

      message("Extracting values for tile ", tile)

      # Extract the raster(s) matching the mm tile from the list

      if(length(names(rast)[names(rast)==tile]) > 1){

         # if more than one raster in that tile (e.g. small dtm tiles) we combine and merge
         r <- do.call(raster::merge, unname(rast[names(rast) == tile])) # for some reason the merge doesn't work with a named list, so we unname after selecting on names!!
      } else {
         r <- rast[[tile]]  # otherwise one raster per tile as normal
      }

      # It could be that there is no raster if it is a patchy dataset, in which case the new column full of NAs has been created and there is nothing else to do. If there is a raster, we proceed:
      if (!is.null(r)){


         if (fun == "majority"){ ## if we want the mode, we swap NAs for a value that can be interpreted
            r[is.na(r)] <- 8888

            # important to keep NAs, because if they are ignored a tiny overlap with a habitat type will assign this value to the polygon when it should really remain NA
         }

         ### Proceed to the extraction
         # Exactextract returns a vector of values as long as the polygons in sf

         extracted <- exactextractr::exact_extract(r, sf, fun)

         sf[[newcol]] <- extracted


      } # end of raster extraction

   }   # end of new workflow


   ######################################################################################


   else {  # Old method (for compatibility with NCS for a while) - delete workflow at some point

      message("Error in ecoservR::extract_raster: expecting named basemap tiles")

      # # Create sf extent polygon to compare to raster
      # xsf <- as(raster::extent(sf), "SpatialPolygons") %>%
      #    sf::st_as_sf() %>%
      #    sf::st_set_crs(sf::st_crs(sf))
      #
      # ### The core processing steps -----
      # ## An index of cells and polygons is created and used
      # ## to query the raster during the extract step, which speeds things up considerably.
      # ## Transforming the raster to a brick also speeds things up a lot.
      #
      # for (i in 1:length(rast)){
      #
      #    r <- rast[[i]]      # load raster in memory
      #
      #    # create the raster extent polygon for checking intersection
      #    ex <- sf::st_as_sf(as(raster::extent(r), "SpatialPolygons")) %>%
      #       sf::st_set_crs(sf::st_crs(sf))   # caveat: rasters must always be OSGB grid ref
      #
      #    if(sf::st_crs(ex) != sf::st_crs(xsf)){ex <- sf::st_transform(ex, sf::st_crs(xsf))}
      #
      #    # Only proceed with this iteration if the raster intersects the sf object
      #    # if (gIntersects(as(extent(r), "SpatialPolygons"),
      #    #                 xsf) == FALSE)
      #    if(sf::st_intersects(ex, xsf, sparse = FALSE) == FALSE) {
      #       next
      #    }
      #
      #
      #    if (fun == "majority"){ ## if we want the mode, we swap NAs for a value that can be interpreted
      #       r[is.na(r)] <- 8888
      #    }
      #
      #    ## The raster and sf object intersect, but if raster tiles are smaller than the sf object we only need to update those polygons that overlap. We create a selection of those polygons and an index to refer to them:
      #
      #    belongs <- which(sf::st_intersects(sf, ex, sparse = FALSE) == TRUE & is.na(sf[[newcol]] == TRUE))   # index of sf poly in raster extent that DON'T yet have a value (otherwise may get overwritten on subsequent run)
      #
      #
      #    if (length(belongs) > 0){
      #
      #       # Create index of cells in each polygon that belongs to the raster tile
      #
      #       index <- tabularaster::cellnumbers(r, sf[belongs, ])      # create the index
      #       names(index) <- c("poly_index", "cell_index")             # rename index columns
      #
      #       # there is a small amount of overlap - some polygons come up in two or more rasters and values will not be reliable for those (to fix later)
      #
      #       ### Extraction
      #
      #       index <- index %>% dplyr::mutate(
      #          vals = raster::extract(raster::brick(r), cell_index)  # raster to brick makes this super fast
      #       ) %>%
      #          dplyr::group_by(poly_index) %>%       # for each polygon, perform the desired function
      #          dplyr::summarise(vals = funct(vals))  # apply custom functions here
      #
      #
      #       # Add the new values to the sf object
      #       ## NOTE: relatively trustworthy for majority, but would give erroneous mean
      #
      #       sf[belongs,][index$poly_index,][[newcol]] <- ifelse( # ifelse is vectorized; will only replace the value if it wasn't already there
      #          (is.na(sf[belongs,][index$poly_index, ][[newcol]]) | sf[belongs,][index$poly_index, ][[newcol]] == 8888),
      #          index$vals,
      #          sf[belongs,][index$poly_index, ][[newcol]])
      #
      #
      #       ### Extraction from points
      #
      #       if (nrow(index) < length(belongs)){ # if some polygons didn't get a value after extraction
      #
      #
      #          # The previous steps will leave out small polygons that don't catch a cell centroid.
      #          # We use extraction from points for those guys.
      #
      #          missed <- suppressWarnings({
      #             sf[belongs,][-index$poly_index,][,1] %>%   # the missed polygons (just keeping 1st col)
      #                sf::st_centroid() %>%    # converted to points
      #                sf::st_cast(to = "MULTIPOINT") %>%
      #                sf::st_cast(to = "POINT", warn = FALSE)  # make sure otherwise crash the cellnumbers function
      #          })
      #          if (nrow(missed) > 0){ ## check that object is not empty, otherwise crashes
      #
      #             # The order of the OBJECT is different every time
      #             # but the cell index comes in the same order
      #
      #             index_miss <- tabularaster::cellnumbers(r, missed) # create the index for the points
      #             names(index_miss) <- c("point_indexWRONG", "cell_index")
      #
      #
      #             # Only ever one value per point so no need to apply function, just extract value
      #             index_miss <- index_miss %>% dplyr::mutate(
      #                vals = raster::extract(raster::brick(r), cell_index))
      #
      #
      #             ## Add back the point info: the index has to be glued back in the order it came, NOT considering the point index which is somehow wrong
      #
      #             sf[belongs,][-index$poly_index,][[newcol]] <- index_miss$vals
      #
      #          }
      #
      #       }  # end of conditional extraction from points
      #
      #    } # end of "belongs" condition
      #
      #    message(paste("Extracting data...", sep = " "))
      #
      # } # end of loop

   }  # end of old method



   return(sf)

   suppressWarnings(on.exit(rm(index, r, fun)))
}
