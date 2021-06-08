#######################################
### Net gain plotting functions     ###
### for EcoservR                    ###
### Sandra Angers-Blondin           ###
### 03-06-2021                      ###
#######################################

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


#' Produce Change Maps
#'
#' Creates a panelled figure of before/after and change in ecosystem service scores, overlaid onto a simple black and white baseline map for context.

#' @param x A raster or RasterStack object (usually baseline scores).
#' @param y A raster or RasterStack object (usually scenario scores).
#' @param site The boundary of the site, as an sf object
#' @param sitename The name of the site to appear in the figure caption
#' @param buffersize A buffer size (in meters) to apply around the site to give more context to the maps (default 50 m)
#' @param nrow Number of rows along which to plot the panels (default 1, so three maps side by side)
#' @return A plot object with scores before and after intervention and the % change per pixel
#' @export

panelMaps <- function(x, y, site, sitename, buffersize = 50, nrow = 2){
   # x and y are the raster bricks we want to compare
   # site is the boundary (sf) of the site we want
   # sitename is a string to be used in the figure caption

   ### Crop the rasters to the region around site
   pre <- crop(x, st_buffer(site, buffersize))
   post <- crop(y, st_buffer(site, buffersize))

   ## choose zoom level based on site size

   sitearea <- as.numeric(st_area(site))/1000000

   if (sitearea < 10){
      zoomlevel <- 15
   } else if (dplyr::between(sitearea, 10,20)){
      zoomlevel <- 14
   } else {
      zoomlevel <- 12}


   ## get a basemap (need to convert data to WGS84). Black and white to not muddle the colour scale on scores
   bgmap <- get_map(location = unname(st_bbox(site %>% st_buffer(buffersize) %>%  st_transform(4326))),
                    maptype = "toner", color = "bw",
                    source = "stamen", zoom = zoomlevel)

   ## The map itself is in projection 3857, but the bounding box is WGS84. So we need to fix the bounding box to get everything in same CRS.
   #https://stackoverflow.com/questions/47749078/how-to-put-a-geom-sf-produced-map-on-top-of-a-ggmap-produced-raster

   # Define a function to fix the bbox to be in EPSG:3857
   ggmap_bbox <- function(map) {
      if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
      # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector,
      # and set the names to what sf::st_bbox expects:
      map_bbox <- setNames(unlist(attr(map, "bb")),
                           c("ymin", "xmin", "ymax", "xmax"))

      # Coonvert the bbox to an sf polygon, transform it to 3857,
      # and convert back to a bbox (convoluted, but it works)
      bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))

      # Overwrite the bbox of the ggmap object with the transformed coordinates
      attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
      attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
      attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
      attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
      map
   }

   # Use the function:
   bgmap <- ggmap_bbox(bgmap)


   ## Create change raster

   change <- (post-pre)/pre*100

   if (max(values(change), na.rm=TRUE) > 5000){ #problem with local clim regulation: crazy high 15000% score?!
      change <- reclassify(change,
                           matrix(c(5000, Inf, 5000)))
   }



   ## Reproject data

   r <- raster()
   extent(r) <- extent(st_buffer(site, buffersize) %>% st_transform(3857))
   res(r) <- res(x)
   crs(r) <- CRS("+init=epsg:3857")

   pre <- projectRaster(pre, r,
                        #crs = CRS("+init=epsg:3857"),
                        method = "bilinear")

   post <- projectRaster(post, crs = CRS("+init=epsg:3857"), method = "bilinear")

   change <- projectRaster(change, crs = CRS("+init=epsg:3857"), method = "bilinear")


   ## Get the max value for before/after palette consistency
   maxpre <- maxValue(pre)
   maxpost <- maxValue(post)
   maxval <- ifelse(maxpre > maxpost, maxpre, maxpost)
   minpre <- minValue(pre)
   minpost <- minValue(post)
   minval <- ifelse(minpre < minpost, minpre, minpost)
   minval <- ifelse(minval < 0, 0, minval)

   rm(maxpre,maxpost,minpre,minpost)

   ## Rescale the pre and post to consistent 100%
   pre <- pre/maxval*100
   post <- post/maxval*100

   # calculate the new minimum so that color palette is streched to range
   newmin <- ifelse(minValue(pre) < minValue(post), minValue(pre), minValue(post))

   preplots <- plotifyRasters(pre)     # get rasters in shape for ggplot
   postplots <- plotifyRasters(post)
   rasplots <- plotifyRasters(change)

   prelist <- vector("list", length = length(preplots))
   postlist <- vector("list", length = length(postplots))
   changelist <- vector("list", length = length(rasplots))  # initialize empty list


   ### Do the plotting

   for (i in 1:length(rasplots)){  # Produce a plot for each service


      ## Before and after maps

      prelist[[i]] <- ggmap(bgmap) +
         geom_raster(data = preplots[[i]][[1]], aes(x = x, y = y, fill = value), alpha = 0.6) +
         geom_sf(data = site %>% st_transform(3857), fill = NA, colour = "navy", size = 1, inherit.aes = FALSE) +
         #labs(title = gsub("\\.", " ", names(pre)[[i]])) +  # add title (replacing dots by space)
         labs(title = "Before") +
         ggspatial::annotation_scale(location = 'br', line_width = 0.5, width_hint = 0.25) +
         coord_sf(crs = st_crs(3857)) +
         scale_fill_viridis(limits = c(newmin[[i]],100),
                            na.value = "white", name = "Capacity") +
         theme_bw() +
         maptheme +theme(legend.position = "none")


      postlist[[i]] <- ggmap(bgmap) +
         geom_raster(data = postplots[[i]][[1]], aes(x = x, y = y, fill = value), alpha = 0.6) +
         geom_sf(data = site %>% st_transform(3857), fill = NA, colour = "navy", size = 1, inherit.aes = FALSE) +
         #labs(title = gsub("\\.", " ", names(post)[[i]])) +  # add title (replacing dots by space)
         labs(title = "After") +
         ggspatial::annotation_scale(location = 'br', line_width = 0.5, width_hint = 0.25) +
         coord_sf(crs = st_crs(3857)) +
         scale_fill_viridis(limits = c(newmin[[i]],100),
                            na.value = "white", name = "Capacity") +
         theme_bw() +
         maptheme


      ## Change map

      changelist[[i]] <- ggmap(bgmap) +
         geom_raster(data = rasplots[[i]][[1]], aes(x = x, y = y, fill = value), alpha = 0.7) +
         geom_sf(data = site %>% st_transform(3857), fill = NA, colour = "navy", size = 1, inherit.aes = FALSE) +
         #labs(title = gsub("\\.", " ", names(pre)[[i]])) +  # add title (replacing dots by space)
         labs(title = "Change") +
         ggspatial::annotation_scale(location = 'br', line_width = 0.5, width_hint = 0.25) +
         coord_sf(crs = st_crs(3857)) +
         scale_fill_gradient2(name = "Change (%)",
                              low = "#513236",
                              mid = "#f8f4f9",
                              high = "#3e8b86",
                              midpoint = 0,
                              na.value = "white") +
         theme_bw() +
         maptheme


   } # end loop


   ## Plot all three together, for each service separately
   ## and save to disk

   finalplots <- vector(mode = "list", length = length(rasplots))

   for (i in 1:length(finalplots)){

      require(patchwork)

      finalplots[[i]] <- wrap_plots(prelist[[i]], postlist[[i]], changelist[[i]],
                                    nrow = nrow) +  # number of rows to be defined as argument depending on the aspect ratio of the site

         plot_annotation(
            title = gsub("\\.", " ", names(pre)[[i]]),
            subtitle = paste0(sitename, " interventions"),
            theme = theme(plot.title = element_text(size = 16),
                          plot.subtitle = element_text(size = 14)
            ))


   }


   return(finalplots)


}
