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


#' Find Latest Basemap
#'
#' This function finds the latest updated file in a directory. Use this to reload your basemap and resumng your project after interrupting a session.

#' @param dir Folder where basemap files are saved
#' @param pattern The text pattern to look for, default MM
#' @return The last updated RDS basemap object
#' @export
findLast <- function(dir = NULL, pattern = "MM"){
   if (is.null(dir) || !dir.exists(dir)) {stop("Please supply a valid directory.")}

   filelist <- list.files(dir, pattern = pattern, full.names = TRUE)
   times <- file.info(filelist)$mtime

   message("Reading last basemap saved: ", filelist[which.max(times)])
   readRDS(filelist[which.max(times)])

}

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


#' Import Project Log
#'
#' This function finds the project log saved in the working directory

#' @param wd The working directory - if left out, the function will detect the directory automatically. Alternatively a different project folder can be set, although not recommended.
#' @return The RDS project log
#' @export
getProjectLog <- function(wd = NULL){

   if (is.null(wd)){
   wd <- getwd() # get the user's working directory
   }

   files <- list.files(wd, pattern = "projectlog.RDS",
                       full.names = TRUE)

   # If there are many project logs, grab latest
   if (length(files) > 1){
      times <- file.info(files)$mtime
   files <- files[which.max(times)]
   }

   if (length(files) == 0) stop("Could not find project log in your project folder.")

   projlog <- readRDS(files)
   projlog$log_path <- wd # insert the location of the file log as a list element

   return(projlog)
}

#' Update Project Log
#'
#' This function finds the project log saved in the working directory and overwrites it with the new log

#' @param projlog The updated project log to be saved
#' @return
#' @export
updateProjectLog <- function(projlog){

   if (is.null(projlog$log_path)){
      projlog$log_path <- getwd() # get the user's working directory if no path saved in log
   }

   files <- list.files(projlog$log_path, pattern = "projectlog.RDS",
                       full.names = TRUE)

   # If there are many project logs, grab latest
   if (length(files) > 1){
      times <- file.info(files)$mtime
      files <- files[which.max(times)]
   }

   if (length(files) == 0) stop("Could not find project log in your project folder.")

   saveRDS(projlog, file.path(projlog$log_path, basename(files)))
}
