### Custom functions


# Check layer geometry -------------------------------------------------------

# check if layer(s) has polygons and return name(s)

check_layers <- function(folder){

   files <- list.files(folder,
                       pattern = paste0(c(".gpkg$", ".shp$", ".gml$", ".gz$", ".asc$", ".tif$"), collapse = "|"), # update to accept all file formats we might need
                       recursive = TRUE,
                       full.names = TRUE)

   st_layers_V <- Vectorize(st_layers, vectorize.args = "dsn", SIMPLIFY = FALSE)

   info <- st_layers_V(files)
   info <- lapply(info, function(x) x$name[x$geomtype %in% c("Polygon", "Multi Polygon")])

   name <- unique(unlist(info))

   # length(name) >= 1  # is there at least one suitable layer

}



# Check attribute names-------------------------------------------------------


checkAttrNames <- function(folder, type = "shp", layerstring = NULL, user_names, name = NULL){

   # Using st_read with a query of 0 rows only returns the headers and metadata.

   files <- list.files(folder, pattern = paste0(type, "$"), full.names = TRUE, recursive = TRUE)

   # if layer not specified, probably just one (type) and we check the attributes of the first
   if (is.null(layerstring)){
      layername <- sf::st_layers(list.files(folder, pattern = paste0(type, "$"), full.names = TRUE, recursive = TRUE)[[1]])[[1]]
   } else {
      # if we specified a layer we need to identify it

      layername <- lapply(as.list(files), function(x) sf::st_layers(x)[[1]]) # layer name for each file
      #layername <- layername[grepl(layerstring, layername)] %>% unlist()

      files <- files[grepl(layerstring, layername, ignore.case = TRUE)] # subset to files with required layer

   }

   if (is.null(layername)) stop("Could not find layer with specified name in this folder.")

   firstfilelayers <- sf::st_layers(files[[1]])[[1]]

   if (!is.null(layerstring)){
      mylayer <- firstfilelayers[grepl(layerstring, firstfilelayers, ignore.case = TRUE)]
   } else {
      mylayer <- firstfilelayers[[1]]
   }
   # import 0 rows from the data but returns headers (using first file that contains this layer)
   actualnames <- sf::st_read(files[[1]],
                              #layer = mylayer,
                              query = paste("select * from \"", mylayer, "\" limit 0", sep=""),
                              quiet = TRUE)

   return(list(names(actualnames)))

}


## Revised filetype funtion to also detect rasters

guessFiletypeShiny <- function(path){


   if (!is.na(path) && path != "NA" && !is.null(path)){ # if the path is not NA, we check (otherwise return NA to be filtered out)

      if (!dir.exists(path)){

         detected <- "error folder"

      } else {

         ## Get list of all (unique) file extensions and subset to accepted formats only

         formats <- c(".shp$", ".gpkg$", ".json$", ".tif$", ".asc$", ".gz$", ".gml$")

         detected <- list.files(path, pattern = paste(formats, collapse = "|"), recursive = TRUE) %>% # all files with ending
            tools::file_ext() %>% unique()  # get unique extensions

         if (length(detected) == 0) {
            detected <- "error no"} else
               if (length(detected) > 1){
                  detected <- detected
               }
      }
   } else {
      detected <- NA_character_
   }
   return(detected)

}

guessFiletypeV <- Vectorize(guessFiletypeShiny , "path", USE.NAMES = FALSE)


## MODULE TO CAPTURE A PATH --------------------

definePathsUI <- function(id, buttonLabel, winTitle) {
   ns <- NS(id)

   div(style="display:block;",

       # mastermap path button

       fluidRow(
          column(4, shinyFiles::shinyDirButton(ns("dirChooseButton"), label = buttonLabel,
                                               title = winTitle,
                                               buttonType = "default",
                                               class = NULL, icon = NULL, style = "margin: 20px 0px")
          ),

          column(8, shiny::verbatimTextOutput(ns("pathName"),
                                              placeholder = TRUE)  # placeholder to display box even before path is filled in
          )
       )
   )
}


definePaths <- function(input, output, session, defaultpath = NULL, set = FALSE) {
   # defaultpath is the base path to use for faster navigation (ideally project folder)
   # set: should the default path be set as the output path from the start? If FALSE (default), the path is only
   # actually set after the user navigates to a folder. If TRUE, the path appears in the text box and is assigned
   # unless the user changes it by navigating to a different folder

   ### Define reactive values for file paths ----
   path <- shiny::reactiveValues(path = NA)# initialise empty reactive value

   volumes <- shinyFiles::getVolumes()      # recognise drives available locally

   # if supplying a default path:

   if (!is.null(defaultpath)){

      # due to a bug in shinyfiles we need to create a new "volume" that is the full project path we want to direct the user to

      volumes <- c(volumes(), "Detected Project" = defaultpath)
      names(volumes) <- c(names(volumes)[1:(length(volumes)-1)], defaultpath)

      if (set == TRUE){
         path$path <- defaultpath   # show path already only if we're pretty sure it's the one needed
      }


      # if we have a default path we make shinyFiles go straight to it

      shinyFiles::shinyDirChoose(input, id = "dirChooseButton", root = volumes,
                                 defaultRoot =  names(volumes)[[length(volumes)]]) # the default "root" is the one we added at the end of volumes()

      ## this was in theory the way to specify the root and the project folder but there is a bug in shiny file
      #
      #              defaultRoot = names(volumes())[[agrep(substr(defaultpath, 1, 3), names(volumes()))]], # set default disk to the one contained in default path
      #              defaultPath = gsub(volumes()[[agrep(substr(defaultpath, 1, 3), names(volumes()))]],
      #                                 "", dirname(defaultpath))
      # )



      # When clicking the browse button user can choose a different folder: save the path as a reactive
      shiny::observeEvent(input$dirChooseButton, {
         temppath <- try(shinyFiles::parseDirPath(volumes, input$dirChooseButton) %>% as.character())
         req(temppath) # only assign to the reactive once there is a path
         path$path <- temppath  # updates the reactive value when directory is selected
      })


   } else {

      # Find directory for a set of files - if no project folder specified present all computer
      shinyFiles::shinyDirChoose(input, id = "dirChooseButton", root = volumes)  # associated with DirButton - gets a path



      # When clicking the browse button user can choose a different folder: save the path as a reactive
      shiny::observeEvent(input$dirChooseButton, {
         temppath <- try(shinyFiles::parseDirPath(volumes, input$dirChooseButton) %>% as.character())
         req(temppath) # only assign to the reactive once there is a path
         path$path <- temppath  # updates the reactive value when directory is selected
      })

   }

   ## Show the selected path in the text box
   output$pathName <- shiny::renderText({
      req(!is.na(path$path))  # need actual path before displaying it on screen (prevents NA from showing up)
      path$path})

   return(shiny::reactive(path$path))  # return the path
}


## MODULE TO POP UP A MODAL FOR ATTRIBUTES ------

# Modal module server
attrPopupModule <- function(input, output, session,
                            dataset = NULL,  # dataset name
                            attrname = "attribute", # attribute name
                            searchlist) {  # the actual attribute names extracted from dataset


   searchlist <- unlist(searchlist)  # the rv$realnames comes as a list

   myModal <- function(failed = FALSE) {
      ns <- session$ns
      modalDialog(
         selectInput(ns("selectattr"),
                     paste0("How is ", dataset, " attribute ", attrname," named in your dataset?"),
                     choices = searchlist,
                     selected = searchlist[agrep(attrname, searchlist)]  # pre-select closest match

         ),
         if (failed)
            div(tags$b("Invalid name, try again", style = "color: red;")),
         actionButton(ns("ok"), "OK"),
         easyClose = FALSE,
         footer = NULL)
   }

   newname <- reactiveValues(good = NULL)


   showModal(myModal())


   # Verify input and prompt further or close on button click
   observeEvent(input$ok, {

      # Check that data object exists and is data frame - don't allow empty string
      if (!is.null(input$selectattr) && any(grepl(input$selectattr, searchlist, ignore.case = TRUE))
      ){

         # all good
         newname$good <- input$selectattr
         removeModal()

      } else {
         showModal(myModal(failed = TRUE))
      }
   })

   return(reactive(newname$good))
}
