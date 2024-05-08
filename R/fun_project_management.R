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
                       ignore.case = TRUE,
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
#' @return Updated project log is saved in the project folder
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
   } else if (length(files) == 0) {
      message("Warning: you are creating a project log manually.")
      files <- paste0(projlog$title, "_projectLog.RDS")
      }

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

   message(paste0("WARNING: Last recorded MM object in project log is not the most recent file. Loading most recent file ", latest, " . To override this, specify trustlog = TRUE in function call."))

      mm <- readRDS(latest)

       } else { # give priority to the log

   message(paste0("WARNING: Last recorded MM object in project log is not the most recent file. Loading MM from ", success, " as indicated in project log. To override this, specify trustlog = FALSE in function call."))

      mm <- readRDS(success)

       }

    } # end of file clash resolution

   } else {  # if nothing recorded in the log get latest
      mm <- readRDS(latest)
    }


 ## Load the study buffer

   SA <- list.files(projectLog$output_temp, pattern = "studyAreaBuffer.RDS", full.names = TRUE, recursive = TRUE)
   if(length(SA) > 1){
    message(paste0("WARNING: More than 1 buffered study area found. Importing ",
                   SA[[1]]))
    studyAreaBuffer <- readRDS(SA[[1]])
   } else if (length(SA) == 1){
      studyAreaBuffer <- readRDS(SA[[1]])
   } else {
      stop(paste0("No buffered study area found in ",
      file.path(folder, projectLog$output_temp),
      " . Did you move or rename the file?"))
   }



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

   if (!is.null(projectLog$ignored)){
      ## if some datasets did not cover the study area (module ran but no data added)

   toadd <- setdiff(toadd, projectLog$ignored)  # exclude them from list of data to add
   }

   if (!is.null(projectLog$performance$add_hedges)){
      toadd <- c(toadd, projectLog$df[projectLog$df$dataset == "hedgerows", ]$prettynames)
   }


   if (length(toadd) > 0){
   message(paste0("Your current basemap (",
   ifelse(isFALSE(trustlog), basename(latest), basename(success)), ") includes data from: \n",
                  paste0(included, collapse = " \n"),
                  ". \n\n Now you can add: \n",
                  paste0(toadd, collapse = "\n"),
                  "\n"))
   } else {
      message(paste0("Your current basemap includes data from: \n",
                     paste0(included, collapse = " \n"),
                     ". \n\n No more data to add.",
                     "\n"))
}
   return({
      ## returns the objects in the global environment
      invisible({
         mm <<- mm
         studyAreaBuffer <<- studyAreaBuffer
         projectLog <<- projectLog
      })
   })


}


#' Retrieve Final Basemap
#'
#' This function loads the final basemap saved for a project and prepares it in order to run ecosystem service models.

#' @param folder Folder in which the final version of the basemap is saved (by default the "final" subfolder in the project's folder)
#' @param filename A character string to match to file names in the folder and identify the file to load. Default to "final.RDS"
#' @param projectLog The project log file which will be used to find the files on disk

#' @return the classified basemap, as a list of sf tiles
#' @export
get_final_map <- function(folder = NULL, filename = "final.RDS", projectLog = parent.frame()$projectLog){

   #projectLog <- getProjectLog()  # load project log from working directory

   # if user specified a different folder to look into (e.g. for intervention map) we will be looking for a file in there, otherwise we set the folder to the "final" subfolder

   if (is.null(folder)){
      folder <- file.path(getwd(), "final") # get the user's working directory
   }

   ## Identify the final mm object

   filelist <- list.files(folder,
                          pattern = filename,
                          full.names = TRUE, recursive = TRUE)
   times <- file.info(filelist)$mtime

   latest <- filelist[which.max(times)]  # subset to latest

   if (length(filelist) == 0) stop("Cannot find your final basemap. Check that a file name containing \"", filename, "\" exists in ", folder , " or set folder and filename arguments as appropriate.")

   # Read in the final file
   mm <- readRDS(latest)

   message("Basemap imported. Importing study area...")


   ## Load the study area

   studypath <- unname(projectLog$df[projectLog$df$dataset == "studyArea", ][["path"]])

   ### Import the study area outline (specifying OSGB as crs)

   studyArea <- try(loadSpatial(studypath,
                                layer = NULL,
                                filetype = guessFiletype(studypath)) %>%
                       do.call(rbind, .))

   if (inherits(studyArea, "try-error")) stop("Could not import study area from ", studypath) else message("Study area imported.")

   studyArea <- suppressWarnings({
      checkcrs(studyArea, 27700) %>%   # check that CRS is Brit National Grid, and transform if not
         sf::st_set_crs(., 27700) %>%  # set the crs manually to get rid of GDAL errors with init string format
         sf::st_make_valid() %>% sf::st_geometry() %>% sf::st_as_sf()
   })

   ## Check mm polygons and bind in one object

   message("Validating basemap geometry...")
   mm <- checkgeometry(mm, "POLYGON")
   mm <- do.call(rbind, mm)

   ## Examine MM attributes and make sure what we need is there

   # absolutely required attribute
   if (!c("HabCode_B") %in% names(mm)) stop("Basemap must contain attribute HabCode_B")

   # potentially required attributes

   accessnat <- c("Group", "GI", "GIpublic")


   if (isFALSE(all(accessnat %in% names(mm)))){
      message("Warning: you will not be able to run the Access to Nature capacity model without the attribute(s) ",
              paste0(accessnat[!accessnat %in% names(mm)], collapse = " & ")
      )
   }

   demandattr <- c("housePop", "health", "riskgroup")

   if (isFALSE(all(demandattr %in% names(mm)))){
      message("Warning: you will not be able to run demand models without the attribute(s) ",
              paste0(demandattr[!demandattr %in% names(mm)], collapse = " & ")
      )
   }

   airdemand <- c("Make", "Theme")

   if (isFALSE(all(airdemand %in% names(mm)))){
      message("Warning: you will not be able to run air purification demand model without the attribute(s) ",
              paste0(airdemand[!airdemand %in% names(mm)], collapse = " & ")
      )
   }

   ### Keep only required attributes and drop rest from mm for faster processing

   mm <- mm[,intersect(c("HabCode_B", accessnat, demandattr, airdemand, "elev"), names(mm))]


   ### BUG FIX: fix typo in "Undetermined Greenspace" so that legacy maps (before correction) can still run properly
   if ("GI" %in% names(mm) && "Undertermined Greenspace" %in% unique(mm$GI)){

      mm$GI[!is.na(mm$GI) & mm$GI == "Undertermined Greenspace"] <- "Undetermined GI"
   }


   message("Ready to map ecosystem services.")

   return({
      ## returns the objects in the global environment
      invisible({
         mm <<- mm
         studyArea <<- studyArea
         #projectLog <<- projectLog
      })
   })


}


#' Performance Summary
#'
#' Outputs an HTML file containing information about the basemap and processing time for each step,

#' @param x The (final) basemap
#' @param projectLog The RDS project log file generated by the wizard app and containing all file paths to data inputs and model parameters
#' @return a HTML document with model run times and information about map size
#' @export
performance_summary <- function(x = parent.frame()$mm,
                                projectLog = parent.frame()$projectLog){

   ## Calculate number of polygons

   if (inherits(x, "list")){
      npoly <- sum(unlist(lapply(x, function(y) nrow(y))))

   } else {
      npoly <- nrow(x)
   }

   ## Calculate size of study area

   studypath <- projectLog$df[projectLog$df$dataset == "studyArea", ][["path"]]

   ### Import the study area outline (specifying OSGB as crs)

   SA <- invisible({
      try(loadSpatial(studypath,
                         layer = NULL,
                         filetype = guessFiletype(studypath)) %>%
                       do.call(rbind, .))
   })

   if (inherits(SA, "try-error")) stop("Could not import study area from ", studypath)

   SA <- suppressWarnings({
      checkcrs(SA, 27700) %>%   # check that CRS is Brit National Grid, and transform if not
         sf::st_set_crs(., 27700) %>%  # set the crs manually to get rid of GDAL errors with init string format
         sf::st_make_valid() %>% sf::st_geometry() %>% sf::st_as_sf()
   })

   # study area, in hectares
   SA_area <- sum(unlist(as.numeric(sf::st_area(SA)))) / 10000

   rm(SA)


   # Create table of performance

   df <- projectLog$performance[sapply(projectLog$performance,
                                       function(x) !is.null(x) == TRUE)]

   df <- unlist(df) %>% as.data.frame(row.names = names(.))


   write(
      paste0("<html><body>
         <h1>Performance summary for ", gsub("_", " ", projectLog$title),"</h1>",
         "<h3>Date: ", Sys.Date(), "</h3>",
         "<h3>Operating system: ", Sys.info()[[1]], "</h3>",
         "<br>",
         "<p>Study area size: ", round(SA_area, digits = 2), " ha </p>",
         "<p>Map contains: ", npoly , " polygons </p>",
         kableExtra::kbl(df, format = "html",
                         digits = 2,
                         col.names = c("Time taken (min)")),
         "</body></html>"),
      file = file.path(projectLog$projpath, paste0(projectLog$title, "_performance.html"))
   )


   message("Saved summary in ", file.path(projectLog$projpath, paste0(projectLog$title, "_performance.html")))
}


