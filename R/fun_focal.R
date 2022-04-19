#######################################
### Focal statistics - heatmaps     ###
### for EcoservR                    ###
### Sandra Angers-Blondin           ###
### 19-04-2022                      ###
#######################################

#' Focal Statistics Scores
#'
#' This function is a wrapper around terra::focal and performs focal statistics in a circle of a specified radius and for three use cases: sum, mean, or fraction cover.
#'
#' @param r The raster object on which to perform focal statistics
#' @param radius The radius (in m) of the window used for focal statistics
#' @param type The function to perform: either "sum" (sum all values in focal window), "mean" (average all values in focal window), or "cover" (fraction cover of a land class of interest). For "cover", the raster must have values of 1 for the feature of interest, and of 0 for the rest. NAs are not considered in sums and means.
#' @return A raster of the same resolution and extent as r, with the focal statistics calculated
#' @export
focalScore <- function(r, radius, type){


   #convert the original Raster package format to the Terra package format
   r_spat_rast <- terra::rast(r)


   # Create circular focal window based on resolution of r and specified radius

   fw <- terra::focalMat(r, radius, "circle")


   ## Calculate the focal statistics

   if (type %in% c("sum", "mean")){

   # replace weights by 1

   fw[fw > 0] <- 1

   ## Mean or sum specified by fun = "type"
   focal_r <- terra::focal(x = r_spat_rast, w = fw, fun = type, na.rm = TRUE, expand = TRUE)


   } else if (type == "cover"){

   ### Proportions
   # To get a fraction cover in a given radius, values must all be 0 or 1 (presence and absence), and we must use the weighted focal window matrix (not set to 0s and 1s like above). Summing weights by our cover values of 0 or 1 gives the fraction cover

      focal_r <- terra::focal(x = r_spat_rast, w = fw, fun = "sum", na.rm=TRUE, expand = TRUE)

   }


   # Convert back to raster format

   focalRaster <- raster::raster(focal_r)


   return(focalRaster)
}
