#######################################
### Import functions                ###
### for EcoservR                    ###
### Sandra Angers-Blondin           ###
### 23 Sept 2020                    ###
#######################################


#' Load Spatial Data
#'
#' This function looks into a folder for a specified file type and loads all corresponding files into a list.

#' @param folder Folder where spatial file or files are stored
#' @param layer Layer name if several layers present in folder
#' @param filetype File format, either "gpkg" for geopackage or "shp" for shapefile
#' @return A list of spatial files
#' @export

loadSpatial <- function(folder, layer = NULL, filetype){
   # folder is the folder where the files are stored
   # layer is the layer name, without extension (e.g. topographicLine, topographicArea...)
   # filetype is the format, currently supporting gpkg, shp

   ## IMPORT FOR SHAPEFILES
   if (filetype == "shp"){

      # Create list of files matching the file format in the folder specified by user
      fileList <- list.files(folder, pattern='.shp$',
                             all.files=TRUE,
                             recursive = TRUE,  # recursive means looking into subfolders
                             full.names=FALSE)  # full name FALSE means that we get the file name only

      # If layer specified, subset to just this (otherwise lists all layers to be imported)
      if (!is.null(layer)){
         fileList <- fileList[grepl(layer, fileList)]
      }

      # get the full path for each layer
      dsn_paths <- lapply(fileList, function(x) file.path(folder, dirname(x)))

      # and simplify shapefile names in original list to get layer name (remove directories and extensions)
      fileList <- lapply(fileList, function(x)
         sub(pattern = "(.*)\\..*$",
             replacement = "\\1", basename(x)))



      # for each file in the list, read the selected layer into a new list and return it
      imported <- mapply(function(x, y) sf::st_read(dsn = x, layer = y),
                         dsn_paths,
                         fileList,
                         SIMPLIFY = FALSE)   # very important otherwise removes class sf

      return(imported)
      rm(fileList, dsn_paths)

      ## IMPORT FOR GEOPACKAGES
   } else if (filetype == "gpkg"){  # for a geopackage

      # Create list of geopackage files in the specified folder
      fileList <- list.files(folder, pattern='.gpkg$',
                             all.files=TRUE, recursive = TRUE,
                             full.names=FALSE)  # full name FALSE means that we get the file name only

      if (is.null(layer)) { # if no layer specified, will load the first layer available
         imported <- lapply(fileList, function(x) sf::st_read(dsn = file.path(folder, x)))
      } else {

         # for each file in the list, read the selected layer into a new list and return it
         imported <- lapply(fileList, function(x) sf::st_read(dsn = file.path(folder, x), layer = layer))
      }
      return(imported)

   } else {
      print("File format not currently supported")
   }

} # end of function




#' Automatically Detect File Type
#'
#' Not intended to be called by user. Used in pre-processing to detect whether spatial files are of a geopackage or shapefile format.

#' @param path Folder where files are stored
#' @return A character string, "gpkg" or "shp", to be fed into loadSpatial
#' @export
guessFiletype <- function(path){

if (!dir.exists(path)) stop("Folder not found. Please check directory path.")

if (length(list.files(path, pattern = ".shp", recursive = TRUE)) > 0 &
    length(list.files(path, pattern = ".gpkg", recursive = TRUE)) ==  0){

   return("shp")} else

      if (length(list.files(path, pattern = ".shp", recursive = TRUE)) == 0
          & length(list.files(path, pattern = ".gpkg", recursive = TRUE)) > 0){

         return("gpkg")

      } else

         if (length(list.files(path, pattern = ".shp", recursive = TRUE)) == 0 &
             length(list.files(path, pattern = ".gpkg", recursive = TRUE)) == 0){

            stop("No spatial files found. (Must be shapefile or geopackage)")

         } else

            if (length(list.files(path, pattern = ".shp", recursive = TRUE)) > 0 &
                length(list.files(path, pattern = ".gpkg", recursive = TRUE)) > 0){
               stop("Files must be either shapefiles or geopackage, not both.")
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




