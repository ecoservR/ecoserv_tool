#######################################
### Tile and extract functions      ###
### for EcoservR                    ###
### Sandra Angers-Blondin           ###
### 08 Oct 2020                     ###
#######################################


#' Prepare Vector Tiles
#'
#' This function uses the extent of the mastermap tiles to create vector tiles of the new layer we want to integrate. The tiles are a bit larger than the mastermap tile, ensuring full coverage. If the mastermap tiles are named, so will the vector tiles be, facilitating extraction.

#' @param mm The mastermap object, as a list of sf objects
#' @param vect The vector dataset - sf format - to tile
#' @param studyArea The study area boundaries to clip the dataset
#' @param value The attribute or attributes of interest in the dataset
#' @return A list of vector tiles, named after the mastermap tiles
#' @export
prepTiles <- function(mm, vect, studyArea = studyAreaBuffer, value){
   # mm is the basemap list object
   # studyArea is the contour of the study area (sf object)
   # value is the attribute we want to rasterize (usually a land cover type)

   # Subset to required value(s) and check that is a factor if it should be
   if (!is.null(value)){

      # keep only needed columns
      vect <- vect[, value]

      for (i in 1:length(value)){    # if many columns provided, check each

         if(class(vect[[value[i]]]) %in% c("character")){   # character gets coerced to factor

            vect[[value[i]]] <- as.factor(vect[[value[i]]])
         }

         if(!class(vect[[value[i]]]) %in% c("double", "numeric", "integer")){ # can only drop levels from factors

            vect[[value[i]]] <- droplevels(vect[[value[i]]])
         }
      }

   } else {
      vect <- sf::st_geometry(vect) %>% sf::st_as_sf() # if no attribute supplied, we only need the geometry
   }


   # Create polygons representing boxes around each mm tile
   mmex <- lapply(mm, function(x)
      sf::st_as_sf(as(raster::extent(x), "SpatialPolygons")) %>% sf::st_set_crs(27700))  # list of mm extents

   names(mmex) <- names(mm)  # assign name to tiles

   for (i in 1:length(mmex)){
      mmex[[i]][["OStile"]] <- names(mmex[i])
   }

   mmex <- do.call(rbind, mmex) # bind in one object


   # Cut up the vector file and split the vector tiles into a list
   vect <- suppressWarnings({
      sf::st_intersection(vect, mmex) %>%
         checkgeometry(., "POLYGON")
   })

   vect <- split(vect, vect$OStile)

   if (length(vect) == 0) {return(NULL)}


   ## If a tile is empty, remove it

   vect <- vect[sapply(vect, function(x) nrow(x) > 0)]

   # # Cast to single-part polygon (behaves better when rasterizing)
   ### Should be taken care of now that checkgeometry is used above
   #
   # vect <- lapply(vect, function(x) sf::st_cast(x, to = "MULTIPOLYGON") %>%
   #                   sf::st_cast(to = "POLYGON", warn = FALSE))

   return(vect)

   # This function only generate VECTOR tiles. The resulting list of vector tiles can be fed into the makeTile function to create the temporary raster tiles.

}



#' Make Raster Tiles
#'
#' This function rasterizes a set of vector tiles, usually obtained by prepTiles.

#' @param vectlist List of vector tiles, ideally named
#' @param value The attribute of interest in the dataset
#' @param scratch The path to the temporary scratch folder
#' @param name a descriptor passed to temporary files
#' @return A list of raster tiles, named after the mastermap tiles
#' @export
makeTiles <- function(vectlist, value = NULL, scratch = parent.frame()$scratch_path, name){

   # vectlist is a named list of vector tiles (created by prepTile, OStile as name)
   # value is the value of interest to be rasterized (default null- presence of a feature, but more likely a land cover type)
   # scratch is the path to the temporary file folder
   # name is a descriptor that gets passed to the temporary file

   if (!dir.exists(scratch)){  # create the folder for saving tiles temporarily
      dir.create(scratch)
   }

   ## Creating raster attribute table if the value of interest has factor levels
   # Factors created and checked in prepTiles so assuming same for all vector tiles

   if (!is.null(value) && !is.numeric(vectlist[[1]][[value]])){  # numeric variables don't need levels

      rat <- list(ID = 1:length(levels(vectlist[[1]][[value]])),       # number sequence matching levels
                  rcode = levels(vectlist[[1]][[value]])) %>%         # extract levels
         as.data.frame()
   }


   ## Create raster templates for rasterizing


   rasts <- lapply(vectlist, function(x)
      raster::raster(ex = raster::extent(x),         # same extent as vector tile
                     res = 2))
   names(rasts) <- names(vectlist)

   ## Rasterize one at a time, write to disk and keep connection in list

   stack <- vector(mode = "list", length = length(vectlist))  # initialize empty list

   for (i in 1:length(vectlist)){
      stack[[i]] <- raster::writeRaster(
         fasterize::fasterize(vectlist[[i]], rasts[[i]], field = value),
         filename = file.path(scratch, paste(name,"temp",i, sep = "_")),
         overwrite = TRUE
      )
      if (!is.null(value) && !is.numeric(vectlist[[1]][[value]])){
         levels(stack[[i]]) <- rat  # add the attributes to the raster (if applicable)
      }
   }

   names(stack) <- names(vectlist)

   return(stack)
   suppressWarnings(on.exit(rm(i, rat, rasts)))
}


#' Extract Raster into Polygons
#'
#' This function extracts the information contained in raster tiles into the basemap. Ideally it operates a one-to-one matching using the tiles names. If unnamed, the workflow is slower and could leave out a small number of polygons. Extraction from centroids is used for small polygons that get missed during the standard extraction.
#'
#' @param sf One sf object, i.e. a basemap tile
#' @param rast A list of raster tiles, ideally named to match mastermap tiles
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



   if (length(which(lapply(rast, function(x) raster::canProcessInMemory(x)) == FALSE)) > 0)
      message("Warning: Raster may too big to be processed in memory.")  # if one tile is too big for memory, SHOW MESSAGE BUT DON'T STOP....


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
         }

         # Create index of cells in each polygon that belongs to the raster tile.
         # Second column is a pixel in the raster, and first column is the index object it corresponds to.

         index <- tabularaster::cellnumbers(r, sf)                     # create the index
         names(index) <- c("poly_index", "cell_index")   # rename index columns

         ### Extraction

         index <- index %>% dplyr::mutate(
            vals = raster::extract(raster::brick(r), cell_index)  # raster to brick makes this super fast
         ) %>%
            dplyr::group_by(poly_index) %>%       # for each polygon, perform the desired function
            dplyr::summarise(vals = funct(vals))  # apply custom functions here


         # Add the new values to the sf object

         sf[index$poly_index,][[newcol]] <- index$vals


         ### EXTRACTION FROM POINTS: small polygons may not get assigned a value. In those cases we take their centroid and query the raster for the underlying value.

         ### Extraction from points

         if (nrow(index) < nrow(sf)){ # if some polygons didn't get a value after extraction,
            # We get their centroid:

            missed <- suppressWarnings({
               sf[-index$poly_index,][,1] %>%   # the missed polygons (just keeping 1st col)
                  sf::st_centroid() %>%    # converted to points
                  sf::st_cast(to = "MULTIPOINT") %>%
                  sf::st_cast(to = "POINT", warn = FALSE)  # make sure otherwise crash the cellnumbers function
            })

            # If we indeed have centroids:

            if (nrow(missed) > 0){ ## check that object is not empty, otherwise crashes

               # The order of the OBJECT is different every time
               # but the cell index comes in the same order

               index_miss <- tabularaster::cellnumbers(r, missed) # create the index for the points
               names(index_miss) <- c("point_indexWRONG", "cell_index")


               # Only ever one value per point so no need to apply function, just extract value
               index_miss <- index_miss %>%
                  dplyr::mutate(
                     vals = raster::extract(raster::brick(r), cell_index))


               ## Add back the point info: the index has to be glued back in the order it came, NOT considering the point index which is somehow wrong

               sf[-index$poly_index,][[newcol]] <- index_miss$vals

            }

         }  # end of conditional extraction from points

      } # end of raster extraction

   }   # end of new workflow


   ######################################################################################


   else {  # Old method (for compatibility with NCS for a while)

      # Create sf extent polygon to compare to raster
      xsf <- as(raster::extent(sf), "SpatialPolygons") %>%
         sf::st_as_sf() %>%
         sf::st_set_crs(sf::st_crs(sf))

      ### The core processing steps -----
      ## An index of cells and polygons is created and used
      ## to query the raster during the extract step, which speeds things up considerably.
      ## Transforming the raster to a brick also speeds things up a lot.

      for (i in 1:length(rast)){

         r <- rast[[i]]      # load raster in memory

         # create the raster extent polygon for checking intersection
         ex <- sf::st_as_sf(as(raster::extent(r), "SpatialPolygons")) %>%
            sf::st_set_crs(sf::st_crs(sf))   # caveat: rasters must always be OSGB grid ref

         if(sf::st_crs(ex) != sf::st_crs(xsf)){ex <- sf::st_transform(ex, sf::st_crs(xsf))}

         # Only proceed with this iteration if the raster intersects the sf object
         # if (gIntersects(as(extent(r), "SpatialPolygons"),
         #                 xsf) == FALSE)
         if(sf::st_intersects(ex, xsf, sparse = FALSE) == FALSE) {
            next
         }


         if (fun == "majority"){ ## if we want the mode, we swap NAs for a value that can be interpreted
            r[is.na(r)] <- 8888
         }

         ## The raster and sf object intersect, but if raster tiles are smaller than the sf object we only need to update those polygons that overlap. We create a selection of those polygons and an index to refer to them:

         belongs <- which(sf::st_intersects(sf, ex, sparse = FALSE) == TRUE & is.na(sf[[newcol]] == TRUE))   # index of sf poly in raster extent that DON'T yet have a value (otherwise may get overwritten on subsequent run)


         if (length(belongs) > 0){

            # Create index of cells in each polygon that belongs to the raster tile

            index <- tabularaster::cellnumbers(r, sf[belongs, ])      # create the index
            names(index) <- c("poly_index", "cell_index")             # rename index columns

            # there is a small amount of overlap - some polygons come up in two or more rasters and values will not be reliable for those (to fix later)

            ### Extraction

            index <- index %>% dplyr::mutate(
               vals = raster::extract(raster::brick(r), cell_index)  # raster to brick makes this super fast
            ) %>%
               dplyr::group_by(poly_index) %>%       # for each polygon, perform the desired function
               dplyr::summarise(vals = funct(vals))  # apply custom functions here


            # Add the new values to the sf object
            ## NOTE: relatively trustworthy for majority, but would give erroneous mean

            sf[belongs,][index$poly_index,][[newcol]] <- ifelse( # ifelse is vectorized; will only replace the value if it wasn't already there
               (is.na(sf[belongs,][index$poly_index, ][[newcol]]) | sf[belongs,][index$poly_index, ][[newcol]] == 8888),
               index$vals,
               sf[belongs,][index$poly_index, ][[newcol]])


            ### Extraction from points

            if (nrow(index) < length(belongs)){ # if some polygons didn't get a value after extraction


               # The previous steps will leave out small polygons that don't catch a cell centroid.
               # We use extraction from points for those guys.

               missed <- suppressWarnings({
                  sf[belongs,][-index$poly_index,][,1] %>%   # the missed polygons (just keeping 1st col)
                     sf::st_centroid() %>%    # converted to points
                     sf::st_cast(to = "MULTIPOINT") %>%
                     sf::st_cast(to = "POINT", warn = FALSE)  # make sure otherwise crash the cellnumbers function
               })
               if (nrow(missed) > 0){ ## check that object is not empty, otherwise crashes

                  # The order of the OBJECT is different every time
                  # but the cell index comes in the same order

                  index_miss <- tabularaster::cellnumbers(r, missed) # create the index for the points
                  names(index_miss) <- c("point_indexWRONG", "cell_index")


                  # Only ever one value per point so no need to apply function, just extract value
                  index_miss <- index_miss %>% dplyr::mutate(
                     vals = raster::extract(raster::brick(r), cell_index))


                  ## Add back the point info: the index has to be glued back in the order it came, NOT considering the point index which is somehow wrong

                  sf[belongs,][-index$poly_index,][[newcol]] <- index_miss$vals

               }

            }  # end of conditional extraction from points

         } # end of "belongs" condition

         message(paste("Extracting data...", sep = " "))

      } # end of loop

   }  # end of old method



   return(sf)

   suppressWarnings(on.exit(rm(index, missed, r, fun, pts, belongs)))
}


#' Add Attributes
#'
#' Replace the information extracted in a raster by its text description
#'
#' @param mm The mastermap object, a list of sf tiles
#' @param colname The name of the column that needs updating
#' @param key The raster attribute table or lookup table allowing to match the numeric code to the appropriate level.
#' @return An updated mm object
#' @export
addAttributes <- function(mm, colname, key){

   # mm is the mastermap object (one sf object)
   # colname is the name of the column where we want the values changed
   # key is the raster attribute table (extracted separately when needed) in dataframe form

   ## Checks

   if (!colname %in% colnames(mm)) stop("Invalid column name specified.")

   ## Input prep

   if (is.logical(mm[[colname]])){  # if a column happens to be full of NA, type switches to logical and ruins the function, so making sure this doesn't happen
      mm[[colname]] <- as.integer(mm[[colname]])
   }


   names(key) <- c("ID", "rcode")
   key <- rbind(key, c("8888", NA))  # adding the NA code so can get matched properly
   key <- as.factor(setNames(key$rcode, key$ID))  # switching key to a named vector format

   # weird punctuation is a tidyverse thing

   mm[[colname]] <- suppressWarnings(
      dplyr::recode(mm[[colname]], !!!key)   # this will replace codes by their text description. Anything not in there will become NA (so 8888 becomes NA automatically)
   )

   return(mm)
}

