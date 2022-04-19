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

