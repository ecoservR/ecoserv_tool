#######################################
### Net gain analysis functions     ###
### for EcoservR                    ###
### Sandra Angers-Blondin           ###
### 03-06-2021                      ###
#######################################

#' Load Rasters
#'
#' This function imports ecosystem score rasters (capacity or demand as specified by user). Generally this would be for one model run (baseline or intervention scenario). Note that both the type and pattern arguments work with regular expressions so ideally rasters will not have been renamed.

#' @param path The path to the folder containing the desired rasters.
#' @param type The type of rasters to import (either "capacity" or "demand") - the name of the rasters must contain this string.
#' @param pattern An additional optional string to restrict which maps are loaded (for instance if many model runs have been put in the same folder).
#' @return A RasterStack object, with names corresponding to the service
#' @export

loadRasters <- function(path, type, pattern = NULL) {
   # where path is the file path to the folder where rasters are stored
   # and type is either "capacity" or "demand"
   # pattern can further subset the list (e.g. if many maps in same folder)

   # List the files in the folder
   rasts <- list.files(path, pattern = ".tif", full.names = TRUE)

   # Select only those of interest
   rasts <- rasts[!grepl("rescaled", rasts)]  # use raw scores for comparison
   rasts <- rasts[grepl(type, rasts)]         # loading either capacity or demand

   # if another pattern to search for, use it here
   if (!is.null(pattern)){
      rasts <- rasts[grepl(pattern, rasts)]  # subsetting using pattern
   }

   # and stack them
   mystack <- raster::stack(rasts)

   # and rename them!

   rasts[grepl("air_purification", rasts)] <- "Air purification"
   rasts[grepl("flood_mitigation", rasts)] <- "Flood risk mitigation"
   rasts[grepl("climate_regulation", rasts)] <- "Local climate regulation"
   rasts[grepl("noise_regulation", rasts)] <- "Noise regulation"
   rasts[grepl("pollination", rasts)] <- "Pollination"
   rasts[grepl("nature_accessOnly", rasts)] <- "Accessible nature experience"
   rasts[grepl("carbon", rasts)] <- "Carbon storage"
   rasts[grepl("food", rasts)] <- "Food production"

   names(mystack) <- rasts

   return(mystack)
}


#' Plotify Rasters
#'
#' Turns rasters into a plottable dataframe for use with ggplot2

#' @param r A raster or RasterStack object
#' @return A RasterStack object, with names corresponding to the service
#' @export

plotifyRasters <- function(r){
   # where r is one raster or a raster stack

   plotified <- vector("list", dim(r)[[3]]) # empty list

   for (i in 1:length(plotified)){
      plotified[[i]] <- rasterVis::gplot(r[[i]])
   }

   names(plotified) <- names(r)
   return(plotified)
}

#' Rescale Rasters
#'
#' Rasters from different model runs (e.g. pre and post intervention scenario) may have different score ranges. It is important to rescale them using a common maximum so we can measure the change.

#' @param x A raster or RasterStack object (usually baseline scores).
#' @param y A raster or RasterStack object (usually scenario scores).
#' @return A list of two Raster or RasterStack objects, rescaled 0-100 using a common maximum (per service).
#' @export

rescale_rasters <- function(x, y){

   if (dim(x)[[3]] != dim(y)[[3]]){stop("x and y must have equal number of layers")}

   maxis <- vector(mode = "list", length = dim(x)[[3]])

   # collect max values in x

   for (i in 1:dim(x)[[3]]){
      maxis[[i]] <- max(raster::values(x[[i]]), na.rm = TRUE)
   }

   #  compare to max values in y and update vector only if value is greater

   for (i in 1:dim(x)[[3]]){
      maxy <- max(raster::values(y[[i]]), na.rm = TRUE)
      maxis[[i]] <- ifelse(
         maxy > maxis[[i]], maxy, maxis[[i]])
   }


   # Now rescale x and y

   for (i in 1:dim(x)[[3]]){

      x[[i]] <- x[[i]]/maxis[[i]]*100
      y[[i]] <- y[[i]]/maxis[[i]]*100

   }

   return(list(x, y))

}

#' Change Statistics
#'
#' Measures the change in average (demand or capacity) score between two sets of service maps. The pre and post maps should already be cropped/masked to the desired extent, and rescaled to the same maximum using the rescale_rasters function.

#' @param pre A raster or RasterStack object (usually baseline scores).
#' @param post A raster or RasterStack object (usually scenario scores).
#' @return A data frame containing the before/after average scores, and the point and percent change, for each service.
#' @export

changeStats <- function(pre, post){
   # where pre and post are raster bricks (layers in same order, and already cropped/masked to desired extent)

   if (!identical(names(pre), names(post))){stop("Raster stack must have the same layers")}

   ## Initialise empty data frame
   df <- tibble::tibble(
      service = names(pre),
      score_pre = as.numeric(NA),
      score_post = as.numeric(NA))

   for (i in 1:length(names(pre))){

      df[i, "score_pre"] <- mean(raster::getValues(pre[[i]]), na.rm = TRUE)
      df[i, "score_post"] <- mean(raster::getValues(post[[i]]), na.rm = TRUE)
   }

   # Calculate % change

   df <- dplyr::mutate(df,
                       change_pts = score_post - score_pre,
                       change_pct = (score_post - score_pre)/score_pre*100)

   return(df)

}



