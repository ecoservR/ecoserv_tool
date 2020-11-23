#######################################
### Project workflow functions      ###
### for EcoservR                    ###
### Sandra Angers-Blondin           ###
### 23 Nov. 2020                    ###
#######################################

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

#' Find Latest Basemap
#'
#' This function finds the latest updated file in a directory. Use this to reload your basemap and resuming your project after interrupting a session.

#' @param dir Folder where basemap files are saved
#' @param pattern The text pattern to look for, default MM
#' @return The last updated RDS basemap object
#' @export
findLast <- function(dir = NULL, pattern = "MM"){
   if (is.null(dir) || !dir.exists(dir)) {stop("Please supply a valid directory.")}

   filelist <- list.files(dir, pattern = pattern, full.names = TRUE)
   times <- file.info(filelist)$mtime

   message("Loading last basemap saved: ", filelist[which.max(times)])
   readRDS(filelist[which.max(times)])

}



#' Resume Existing Project
#'
#' This function checks a folder for the latest project log, and once it finds it loads in the environment the latest mm object and the buffered study area.

#' @param folder Project folder (by default the working directory)
#' @param trustlog Should the project log decide which MM object to load? If TRUE, looks for the file path recorded in the last_success slot even if a more recent version exists. If FALSE, prefers the MM file with the latest timestamp.
#' @return The projectLog object, the mm object, and the studyAreaBuffer object. Also prints an indication of what datasets are already incorporated and which remain.
#' @export
resumeProject <- function(folder = NULL, trustlog = FALSE){

   if (is.null(folder)){
      folder <- getwd() # get the user's working directory
   }

   ## Look into project folder for project log

   projectLog <- getProjectLog(wd = folder)

   ## Identify the latest saved mm object

   filelist <- list.files(projectLog$output_temp, pattern = "MM", full.names = TRUE, recursive = TRUE)
   times <- file.info(filelist)$mtime

   latest <- filelist[which.max(times)]  # subset to latest

   if (length(filelist) == 0) stop("No MM object to load.")

   ## Compare to the last success if there is a record of that

   if(!is.null(projectLog$last_success)){

   success <- list.files(pattern = projectLog$last_success,
                                   projectLog$output_temp,
                                   recursive = TRUE,
                                   full.names = TRUE)

    mytest <- identical(success, latest)


    if (mytest){

       mm <- readRDS(success)  # they're identical so load mm

    } else {
       # if last success and latest file clash:

       if (isFALSE(trustlog)){  # give priority to the latest file

   warning(paste0("Last recorded MM object in project log is not the most recent file. Loading most recent file ", latest, " . To override this, specify trustlog = TRUE in function call."))

      mm <- readRDS(latest)

       } else { # give priority to the log

   warning(paste0("Last recorded MM object in project log is not the most recent file. Loading MM from ", success, " as indicated in project log. To override this, specify trustlog = FALSE in function call."))

      mm <- readRDS(success)

       }

    } # end of file clash resolution

   } else {  # if nothing recorded in the log get latest
      mm <- readRDS(latest)
    }


 ## Load the study buffer

   studyAreaBuffer <- sf::st_read(projectLog$SAbuffer)


## Examine MM attributes and give a summary of what's done and not.

   # all possible attributes
   allattr <- c("OS MasterMap" = "Theme",
                "OS Greenspace" = "GI",
                "OS Open Greenspace" = "GI",
                "CORINE land cover" = "corine",
                "National Forest Inventory" = "nfi",
                "Priority Habitat Inventory" = "phi",
                "Crop Map of England" = "crome",
                "digital terrain model" = "elev")

   # link up attributes and dataset

   ds_names <- projectLog$df$prettynames  # dataset names

   mmattr <- names(mm[[1]]) # attributes actually in map

   mmattr <- allattr[allattr %in% mmattr]  # keep only attributes contained


   included <- ds_names[!is.na(projectLog$df$path) & ds_names %in% names(mmattr)]

   toadd <- ds_names[!is.na(projectLog$df$path) & !ds_names %in% names(mmattr) & ds_names != "your study area"]

   message(paste0("Your current basemap includes data from: \n",
                  paste0(included, collapse = " \n"),
                  ". \n\n Now you can add: \n",
                  paste0(toadd, collapse = "\n"),
                  "\n"))

   return({
      ## returns the objects in the global environment
      invisible({
         mm <<- mm
         studyAreaBuffer <<- studyAreaBuffer
         projectLog <<- projectLog
      })
   })


}




