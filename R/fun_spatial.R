#######################################
### Custom spatial functions        ###
### for EcoservR                    ###
### Sandra Angers-Blondin           ###
### 06 October 2020                 ###
#######################################


#' Faster Intersection by Indexing
#'
#' This function performs a spatial intersection, but first performs a check to identify only the features that need clipping. Features completely outside the boundary are discarded, and those completely inside are kept as is.

#' @param x a simple features object
#' @param cliplayer The layer used for clipping, also a simple features object
#' @return A sf object clipped to the desired boundary
#' @export

faster_intersect <- function(x, cliplayer){

   x <- checkcrs(x, cliplayer)  # check that crs is same and otherwise change it

   test <- sf::st_intersects(x, sf::st_geometry(cliplayer)) # test if polygons overlap clipping layer


   # if result is empty (o: FALSE), out of study area, we delete

   x <- x[lengths(test) > 0,]  # exclude polygons that don't even intersect

   if (nrow(x) > 0) { # only if there are polygons left:

      # now we check which polygons are fully within as we don't need to clip them at all
      test2 <- sf::st_covered_by(x, sf::st_geometry(cliplayer))

      # and we intersect the rest

      cut <- x[lengths(test2) == 0,]  # extract objects to cut
      x <- x[lengths(test2) != 0,]  # and remove them from main object

      # perform the intersection
      cut <- suppressWarnings(
         sf::st_intersection(cut, sf::st_geometry(cliplayer))
         ) %>%
         checkgeometry(., "POLYGON")

      x <- rbind(x, cut)


   }

   x <- sf::st_make_valid(x)  # make valid
   return(x)
}



#' Functional Threshold
#'
#' Create a mask to remove small patches not considered to provide a service. Used in some ecosystem service capacity models. Not meant to be used directly.

#' @param x a capacity score raster
#' @param local the wide search radius, in meters (defaults to 0 in case it doesn't apply to model)
#' @param res the resolution of the raster (m)
#' @param proportion the proportion of cells in the search radius which we allow to have a low score (10 units) and still consider 0
#' @param threshold the size threshold (m2) for a patch to be considered functional
#' @return the score raster, masked
#' @export

functionalMask <- function(x, local = 0, res, proportion = 0.10, threshold){

### Create the score value up to which we consider a cell to have no capacity
   # This is because in some models, the search radius is so big that it's almost
   # impossible to have a cell of score 0; but a very low score doesn't mean
   # the service is really provided

   # Therefore we allow "proportion" of cells to have a score of 10 within the area of the circle with "local" radius (number of cells in that circle depends on "res")

   minscore <- (pi*local^2)/(res^2)*proportion*10   # if a given model doesn't do focal stats, then having "local" set to 0 will create a minscore of 0, which is what we want


   ### Create a binary score raster (NA: no capacity / 1: some capacity)
   score_bin <- x  # create a copy of the score raster
   score_bin[score_bin <= minscore] <- NA # everything that has a score representing no (or virtually no) capacity gets assigned NA (faster for next steps)
   score_bin[score_bin > minscore] <- 1 # the rest has some capacity, so given value of 1

   ### Create the patch raster that will form the mask

   patch <- landscapemetrics::get_patches(score_bin,  # Identifying raster "clumps" (groups of connected pixels that are not NA)
                        class = 1,  # the value we assigned to patches
                        directions = 8,  # looking for neighbouring cells in all 8 directions
                        return_raster = TRUE)[[1]][[1]]  #[[1]][[1]] extracts the raster from the (nested) list (bug fix for landscapemetrics 1.5.3)


   # Calculate area of each patch, creating an index of which ID need to be removed

   remove <- landscapemetrics::lsm_p_area(score_bin) %>%  # calculate area of every patch (in hectares)
      dplyr::mutate(value = as.numeric(value*10000)) %>%   # transform to meters squared
      dplyr::filter(value < threshold) %>%  # retain patches under the size threshold
      dplyr::select(id)  # keep only their id

   rm(score_bin)  # remove no longer needed objects to free up memory

   # create the mask by removing offending patches

   if (nrow(remove) > 0){
   # turn the ID number flags of small patches into NA (no patch)
   patch <- raster::subs(patch,
                         data.frame(id = remove$id,
                                    newval = NA),
                         which = 2, subsWithNA = FALSE)
   }

   # mask areas of 0 value
   x <- raster::mask(x, patch, maskvalue = NA)  # every NA

   ## Avoiding raster-specific use of brackets for subsetting as doesn't always work within package (use named function isntead)
   #x[is.na(x) == TRUE] <- 0  # the masked areas get value of NA, but we want 0

   x <- raster::reclassify(x, cbind(NA, NA, 0), right=FALSE)

   return(x)

   on.exit(rm(patch, remove))

}


# Function to set a max search distance in a raster before a distance analysis ----------------

#' Calculate Euclidean Distance
#'
#' Calculate distance to a source (e.g. a habitat type, roads, an airport) when the landscape can be converted into binary data: source = 1, pixels to compute distance for = 8888 (and rest NA, masked by a buffer layer).

#' @param r a raster with values of 1 (target pixels) and 8888 (pixels to calculate distances)
#' @return a distance raster where the value is the distance (in m) to a source. Sources have a value of 0.
#' @export
#'
calculateDistances <- function(r){
   # where r is the raster, processed to have source coded as 1 and target cells as 8888

   require(spatstat.geom)
   require(maptools)

   # convert to spatstat point object with marks (habitat or no)

   points <- spatstat.geom::as.ppp(raster::rasterToPoints(r, spatial = TRUE))   # a ppp objects with marks 8888 (non) or 1 (habitat)
   plist <- spatstat.geom::split.ppp(points, f = as.factor(points$marks)) # split them
   pointsHab <- plist[[1]]  # the habitat points
   pointsNon <- plist[[2]]  # the 8888 (non habitat) points

   rm(points, plist) # remove unnecessary objects

   # Find the nearest neighbour of type Y (habitat patch) for each point X (non habitat cell)
   distances <- spatstat.geom::nncross(pointsNon, pointsHab, k = 1, what = "dist")


   ### Update the demand raster with the distances ----

   # In the raster, replace all the non-habitat (8888) values by the distance values, and tidy up other values
   r[r == 8888] <- distances   # add distances to each non habitat cell
   r[r == 1] <- 0              # habitats have distance 0 from themselves

   return(r)
}



# Rename geometry column --------------------------------------------------

#' Rename geometry column
#'
#' The geometry column of sf object can have inconsistent names (e.g. geom, geometry, etc). This function renames the geometry attribute to a given name.

#' @param g The sf object whose geometry you want to change
#' @param name The name you want the geometry column renamed to
#' @return the sf object g with the geometry column name changed
#' @export
#'
rename_geometry <- function(g, name){
   current = attr(g, "sf_column")
   names(g)[names(g)==current] = name
   sf::st_geometry(g)=name
   g
}



# Find center point of a raster -------------------------------------------

#' Find center of a raster
#'
#' Calculates the middle point of a raster from its extent. Assumes the projection is British National Grid (EPSG 27700). Not to be called directly: used in the add_DTM() workflow to assign a grid reference to tiles.

#' @param r The raster object
#' @return A sf point
#' @export
#'
raster_centroid <- function(r){

   bbox <- sp::bbox(r)

   centro <- sf::st_point(
      c(mean(c(bbox[[1]], bbox[[3]])),
        mean(c(bbox[[2]], bbox[[4]])))) %>%
      sf::st_geometry() %>% sf::st_sf(crs = 27700)

}
