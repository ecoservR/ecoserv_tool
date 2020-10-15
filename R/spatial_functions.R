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

      x[lengths(test2) == 0,] <- suppressWarnings(
         sf::st_intersection(x[lengths(test2) == 0,], sf::st_geometry(cliplayer))
      )

   }

   x <- sf::st_make_valid(x)  # make valid
   return(x)
}
