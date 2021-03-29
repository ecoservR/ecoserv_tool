#######################################
### Import functions                ###
### for EcoservR                    ###
### Sandra Angers-Blondin           ###
### 23 Sept 2020                    ###
#######################################


#' Automatically Detect File Type
#'
#' Not intended to be called by user. Used in pre-processing to detect whether spatial files (vector data) are in an acceptable format. Accepted file extensions are "gpkg", "shp", "json", "gz", "gml", "tab".

#' @param path Folder where files are stored
#' @return A character string to be fed into loadSpatial.
#'
#' @export
guessFiletype <- function(path){

   if (!dir.exists(path)) stop(paste0("Folder '", path,"' not found. Please check directory path."))

   accepted <- c(".gpkg$", ".shp$", ".json$", ".gz$", ".gml$", ".tab$")

   detected <- list.files(path, pattern = paste0(accepted, collapse="|"),
                          recursive = TRUE,
                          full.names = TRUE,
                          ignore.case = TRUE)

   if (length(detected) == 0) stop("No spatial files found.")

   ## Narrow down to the format(s) present in folder
   format <- accepted[unlist(lapply(seq_along(accepted),
                                    function(x)
                                       any(grepl(accepted[x], detected, ignore.case = TRUE))
   ))]

   if (length(format) > 1) message(paste0("Warning: Multiple spatial formats found in '", path, "': ", paste0(format, collapse = ", "), ". Import is likely to fail."))

   return(format)

}


# New loadSpatial function ------------------------------------------------

#' Load Spatial Data
#'
#' This function looks into a folder for a specified file type and loads all corresponding files into a list.
#' It has the option of adding a query layer to only read in memory the features that intersect an area of interest.

#' @param folder Folder where spatial file or files are stored
#' @param layer Layer name if several layers present in folder
#' @param filetype File format. Accepted file extensions are "gpkg", "shp", "json", "gz", "gml", "tab".
#' @param querylayer A sf vector layer to use as a spatial query to only load intersecting features. Speeds up data import; default NULL for no spatial query.
#' @return A list of spatial files
#' @export

loadSpatial <- function(folder, layer = NULL, filetype, querylayer = NULL){
   # folder is the folder where the files are stored
   # layer is the layer name, without extension (e.g. topographicLine, topographicArea...)
   # filetype is the format, currently supporting gpkg, shp
   # querylayer is typically the study area, to only import relevant features

   ## List all acceptable spatial files in folder
   fileList <- list.files(folder, pattern = paste0(filetype, collapse="|"),
                          recursive = TRUE, full.names = TRUE,
                          ignore.case = TRUE)



   ## Handling for shapefiles is different than for the rest as layers need to be specified
   ## (if specifying folder only, just the first shp will be read)

   if(grepl("shp", filetype)){

      # If layer specified, subset to just this (otherwise lists all layers to be imported)
      if (!is.null(layer)){
         fileList <- fileList[grepl(layer, fileList, ignore.case = TRUE)]
      }

      # get the full path for each layer
      dsn_paths <- lapply(fileList, function(x) file.path(dirname(x)))

      # and simplify shapefile names in original list to get layer name (remove directories and extensions)
      fileList <- lapply(fileList, function(x)
         sub(pattern = filetype,
             replacement = "\\1", basename(x), ignore.case = TRUE))  # remove extension from file name

      if (is.null(querylayer)){

         # for each file in the list, read the selected layer into a new list and return it
         imported <- mapply(function(x, y) sf::st_read(dsn = x, layer = y),
                            x=dsn_paths,
                            y=fileList,
                            SIMPLIFY = FALSE)   # very important otherwise removes class sf


      } else {

         # for each file in the list, read the selected layer into a new list,
         # using the study area as a spatial query, clipping and tidying up as we go

         imported <- mapply(function(x, y){

            suppressWarnings({
               item <- poly_in_boundary(x, querylayer, layer = y)

               if (nrow(item) > 0){
                  item <- ecoservR::checkcrs(item, querylayer) %>%
                     ecoservR::checkgeometry(., "POLYGON") %>%
                     sf::st_intersection(sf::st_geometry(querylayer)) %>%
                     ecoservR::checkgeometry(., "POLYGON")
               }
            })
         },
         x=dsn_paths,
         y=fileList,
         SIMPLIFY = FALSE)   # very important otherwise removes class sf

      }

      return(imported)
      rm(fileList, dsn_paths)

   } else {  # END OF WORKFLOW FOR SHAPEFILES

      ## For geopackages and other single files, we only want to specify the layer name if we know the dataset is likely to come in a bundle of layers; otherwise we read the first (and probably only) layer

      if (is.null(layer)) { # if no layer specified, will load the first layer available

         # warning message
         if (length(unique(sf::st_layers(fileList[[1]])$name)) > 1) message(paste0(
            "Multiple layers found in '",
            fileList[[1]],
            "': \n importing first layer '",
            sf::st_layers(fileList[[1]])$name[[1]],
            "'."))

         # import features with or without a spatial query

         if (is.null(querylayer)){

            imported <- lapply(fileList, function(x) sf::st_read(dsn = x))

         } else {
            imported <- lapply(fileList, function(x) {
               suppressWarnings({
                  item <- poly_in_boundary(x, querylayer)

                  if (nrow(item) > 0){
                     item <- ecoservR::checkcrs(item, querylayer) %>%
                        ecoservR::checkgeometry(., "POLYGON") %>%
                        sf::st_intersection(sf::st_geometry(querylayer)) %>%
                        ecoservR::checkgeometry(., "POLYGON")
                  }
                  return(item)
               })
            })
         }

      } else {  # workflow for a named layer

         # for each file in the list, read the selected layer into a new list and return it


         # with or without a spatial query

         if (is.null(querylayer)){

            imported <- lapply(fileList, function(x) sf::st_read(dsn = x,
                                                                 layer = layer))

         } else {

            imported <- lapply(fileList, function(x) {
               suppressWarnings({
                  item <- poly_in_boundary(x, querylayer, layer = layer)

                  if (nrow(item) > 0){
                     item <- ecoservR::checkcrs(item, querylayer) %>%
                        ecoservR::checkgeometry(., "POLYGON") %>%
                        sf::st_intersection(sf::st_geometry(querylayer)) %>%
                        ecoservR::checkgeometry(., "POLYGON")
                  }

                  return(item)
               })
            })
         }


      }

      # remove empty list items
      imported <- imported[sapply(imported, function(x) nrow(x) > 0)]

      return(imported)

   }
}


#' Import Designated Areas
#'
#' Not intended to be called by user. Used in the accessible nature model to bring in external datasets in an automated way.

#' @param path Folder where files are stored
#' @param studyArea The study area outline to clip the data to
#' @param dataset A character string giving the name of the dataset; used for error messages
#' @return A sf dataframe ready to be used by the model. If no geometries occur in the study area, returns NULL.
#' @export
importDesignatedAreas <- function(path, studyArea, dataset){

   ### Checks

   if (!dir.exists(path)) stop(paste(dataset, " folder not found. Please check directory path.", sep = ""))


   # load files into a list
   message("Importing ", dataset)
   df <- loadSpatial(path, filetype = guessFiletype(path))   # requires custom functions to be loaded

   df <- lapply(df, function(x)
      sf::st_geometry(x) %>%  # drop everything but geometry
         sf::st_as_sf() %>%
         sf::st_transform(27700))  # make sure they're all in OSGB

   df <- df[sapply(df, function(x) nrow(x) > 0)]
   # merge all layers with observations and then crop to study area

   df <- do.call(rbind, df)
   df <- sf::st_intersection(df, sf::st_geometry(studyArea))


   if (nrow(df) == 0){ # if no features after intersection, ignore
      message(paste("No occurrence of", dataset, "in study area. Ignoring this step."))
      return(NULL)
   } else {
      return(df)
   }
}


#' Filtered Import
#'
#' Helper function to read only features contained in a study area rather than load every feature in the dataset. Called by loadSpatial() for most baseline dataset imports to reduce memory load and speed up computation.

#' @param x Folder where files are stored
#' @param y The study area outline to query the data
#' @param layer Name of the layer, if required
#' @return A sf dataframe ready to be used by the model. If no features are intersecting the defined boundary, returns a sf dataframe of length 0.
#' @export
poly_in_boundary <- function(x, y, layer = NULL){

   if (is.null(layer)){  # for simple datasets with no layer

      # identify CRS of the data to be read
      mycrs <- rgdal::ogrInfo(x)$wkt

      if (sf::st_crs(y)$wkt != mycrs){
         y <- sf::st_transform(y, mycrs)  # reproject study area to data's projection
      }

      query <- y %>%
         sf::st_union() %>%
         sf::st_cast(to="MULTIPOLYGON") %>%
         sf::st_geometry() %>% # convert to sfc
         sf::st_as_text() # convert to well known text

      mydata <- sf::st_read(x, wkt_filter = query, quiet = TRUE)

   } else {  # when we have a layer name

      # identify CRS of the data to be read
      mycrs <- rgdal::ogrInfo(x, layer = layer)$wkt

      if (sf::st_crs(y)$wkt != mycrs){
         y <- sf::st_transform(y, mycrs)  # reproject study area to data's projection
      }

      query <- y %>%
         sf::st_union() %>%
         sf::st_cast(to="MULTIPOLYGON") %>%
         sf::st_geometry() %>% # convert to sfc
         sf::st_as_text() # convert to well known text

      mydata <- sf::st_read(x, layer = layer, wkt_filter = query, quiet = TRUE)

   }

   return(mydata)
}


