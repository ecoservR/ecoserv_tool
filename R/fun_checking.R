#######################################
### Data checking functions         ###
### for EcoservR                    ###
### Sandra Angers-Blondin           ###
### 23 Sept 2020                    ###
#######################################

#' Check Attribute Validity
#'
#' This function checks that the attribute names supplied by user are indeed present in a dataset without importing it, so that a large object is only read when attributes are valid.
#' This function is not meant to be called directly by user but called before importing data.

#' @param folder Folder where spatial file or files are stored
#' @param layer Layer name if several layers present in folder
#' @param user_names A named vector of attributes, names being the target EcoservR name and items being the actual name in dataset
#' @param name The name of the dataset that is being checked, to customise error message if needed
#' @return A list of spatial files
#' @export
checkNames <- function(folder, layer = NULL, user_names, name = NULL){

   # Using st_read with a query of 0 rows only returns the headers and metadata.

   type <- guessFiletype(folder)  # identify file extension

   if (is.null(layer)){
      layer <- sf::st_layers(list.files(folder, pattern = type, full.names = TRUE, recursive = TRUE)[[1]])[[1]] # if layer not specified, probably just one and we check the first
   }

   # if layer is not NULL we have a regex to use to get the layer name
   layers <- sf::st_layers(list.files(folder, pattern = type, full.names = TRUE, recursive = TRUE)[[1]])[[1]] ## all layer names of 1st file
   layer <- layers[grepl(layer, layers)]  # get just the name that matches

   # import 0 rows from the data but returns headers
   actualnames <- sf::st_read(list.files(folder, pattern = type, full.names = TRUE, recursive = TRUE)[[1]],
                          layer = layer,
                          query = paste("select * from \"", layer, "\" limit 0", sep=""),
                          quiet = TRUE)

   if (any(!user_names %in% names(actualnames))){

      stop("Layer ", name, " does not contain specified attribute(s): ",
           paste(user_names[!user_names %in% names(actualnames)], collapse = " "))
   }

}



#' Check Coordinate Reference System
#'
#' This function checks the coordinate reference system of a spatial object, and transforms to the British National Grid or other desired target if needed.

#' @param x Simple features spatial object, or list of sf objects
#' @param target EPSG code of desired end projection, default British National Grid, or another sf object whose projection you want to match
#' @return sf object with desired projection
#' @export
checkcrs <- function(x, target = 27700){

   if (!is.numeric(target) && !inherits(target, "sf")) stop("Target CRS must be EPSG code or sf object with desired CRS")

   if (inherits(target, "sf")){
      target <- sf::st_crs(target)
   } else {
      target <- try(sf::st_crs(paste("EPSG:", target, sep = "")), silent = TRUE) # attempt to set the CRS
      # if number not a valid CRS, stop function
      if(inherits(target, "try-error")) stop("Invalid CRS specified.")
   }

   # if we're dealing with a list, only perform on the elements that need it
   if (inherits(x, "list")){

      x <- lapply(x,
                  function(y){if (sf::st_crs(y) == target){
                     y  # if tile already has the right CRS, do nothing
                  } else {  # otherwise transform CRS
                     sf::st_transform(y, target)
                  }
                  })

   } else if(inherits(x, "sf")){
      if (sf::st_crs(x) != target){
         x <- sf::st_transform(x, target)
         message("Reprojected data to ", target$input)
      }
   }
   return(x)
}

#' Check Geometry
#'
#' This function checks the geometry type of a spatial object, and casts to the desired geometry (POLYGON or POINT). Also makes the geometries valid. Used mostly before operations requiring single-part polygon geometry. If intersections return linestrings, triangles or points, those are dropped.

#' @param x Simple features spatial object, or list of sf objects
#' @param target EPSG code of desired end projection, default British National Grid, or another sf object whose projection you want to match
#' @return sf object with desired geometry type.
#' @export

checkgeometry <- function(x, target = "POLYGON"){

   if (!target %in% c("POLYGON", "POINT")) stop("Target geometry must be POLYGON or POINT")

   # if we're dealing with a list, only perform on the elements that need it
   if (inherits(x, "list")){

      x <- lapply(x,
                  function(y){
                     if (sf::st_geometry_type(y, by_geometry = FALSE) == target){
                        return(sf::st_make_valid(y))  # if tile already has the right geometry, do nothing but validate
                     } else {  # otherwise fix geometry and cast to target (multi to single part)
                        y <- sf::st_make_valid(y) %>%
                           # remove geometries that are valid but incorrect type
                           # (e.g line and points from polygon clipping)
      # Note: this WILL lead to small holes in some places
                           dplyr::filter(sf::st_is(., if(target == "POLYGON"){
                              c("POLYGON","MULTIPOLYGON")} else {c("POINT","MULTIPOINT")})) %>%
                           sf::st_cast(to = ifelse(target == "POLYGON", "MULTIPOLYGON", "MULTIPOINT"), warn = FALSE) %>%
                           sf::st_cast(to = target, warn = FALSE)
                        return(y)
                     }
                  })

   } else if(inherits(x, "sf")){
      if (sf::st_geometry_type(x, by_geometry = FALSE) != target){
         x <- sf::st_make_valid(x) %>%
            # remove geometries that are valid but incorrect type (e.g line and points from polygon clipping)
            dplyr::filter(sf::st_is(., if(target == "POLYGON"){
               c("POLYGON","MULTIPOLYGON")} else {c("POINT","MULTIPOINT")})) %>%
            sf::st_cast(to = ifelse(target == "POLYGON", "MULTIPOLYGON", "MULTIPOINT"), warn =FALSE) %>%
            sf::st_cast(to = target, warn = FALSE)} else {
               x <- sf::st_make_valid(x)
            }
   }
   return(x)
}



#' Remove Duplicated Polygons
#'
#' This function loops through a list of tiles and removes polygons that are duplicated across tiles. If only one tile is used, the distinct function.

#' @param tiles List of simple features spatial objects, usually OS MasterMap tiles
#' @param ID Attribute containing a polygon's unique identifier
#' @return The same list with duplicates removed
#' @export
#'
removeDuplicPoly <- function(tiles, ID){
   # tiles is the list of tiles (in sf format)
   # ID is the identification field (column name)

   if (!inherits(tiles, "list")) stop("Basemap must be provided in list form")

   if (length(tiles) > 1){ # method for basemap tiles

      # create empty list to start (pre-allocate memory)
      IDlist <- vector(mode = "character",
                       length=sum(unlist(lapply(tiles, function(x) nrow(x)))))
      IDlist <- rep(NA_character_, length(IDlist))

      for (i in 1:(length(tiles)-1)) { # for each list element apart from the last

         tiles[[i]] <- tiles[[i]][!duplicated(tiles[[i]][[ID]]), ] # remove duplicates from within the tile itself

         # get list of unique IDs
         addtolist <- as.character(tiles[[i]][[ID]])
         # (with base subsetting, we can call directly ID and it will be evaluated to whatever column name the user specified)

         if(length(addtolist) == 0){next} # move to next tile if nothing in this one

         if (i > 1){  # change in place rather than append to a list for better memory management
            lowindex <- min(which(is.na(IDlist)))
            maxindex <- lowindex + length(addtolist) - 1
            IDlist[lowindex:maxindex] <- addtolist

         } else {  # the first iteration is simpler
            IDlist[1:length(addtolist)] <- addtolist
         }

         tiles[[i + 1]] <- tiles[[i + 1]][!(as.character(tiles[[i + 1]][[ID]]) %in% IDlist), ] # the next list element gets filtered out of any value that already appears in the list of polygon IDs


      }

      rm(IDlist, addtolist)

   } else if (length(tiles) == 1){   # method for only one map tile
      tiles[[1]] <- tiles[[1]][!duplicated(tiles[[1]][[ID]]), ]  # remove any duplicated IDs
   }

   return(tiles)

} # end of function



