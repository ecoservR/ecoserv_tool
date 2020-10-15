#######################################
### Cleaning functions              ###
### for EcoservR                    ###
### Sandra Angers-Blondin           ###
### 08 Oct 2020                     ###
#######################################


#' Clean Up and Free Memory
#'
#' This function deletes the contents of the scratch folder and uses garbage collection to return memory to the OS.

#' @param folder Folder where spatial file or files are stored
#' @param layer Layer name if several layers present in folder
#' @param filetype File format, either "gpkg" for geopackage or "shp" for shapefile
#' @return A list of spatial files
#' @export
#'
cleanUp <- function(scratch = scratch_path){
# scratch is the path to the temporary folder

invisible(  # this just prevents message being printed to the console
   file.remove(dir(    # delete all files
      file.path(scratch),
      full.names = TRUE,
      recursive = TRUE
   ))
)

invisible(  # remove folders too
   unlink(list.dirs(file.path(scratch), recursive = FALSE),  # FALSE here essential otherwise would delete scratch folder itself
          recursive = TRUE)  # but true here to delete empty folders within scratch
)

invisible(gc())  # probably not doing much but forces R to return memory to system if available
}
