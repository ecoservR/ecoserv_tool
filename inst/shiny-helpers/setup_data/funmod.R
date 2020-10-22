### Custom functions

checkAttrNames <- function(folder, layerstring = NULL, user_names, name = NULL){

   # Using st_read with a query of 0 rows only returns the headers and metadata.

   type <- guessFiletype(folder)  # identify file extension

   files <- list.files(folder, pattern = type, full.names = TRUE, recursive = TRUE)

   # if layer not specified, probably just one (type) and we check the attributes of the first
   if (is.null(layerstring)){
      layername <- sf::st_layers(list.files(folder, pattern = type, full.names = TRUE, recursive = TRUE)[[1]])[[1]]
   } else {
      # if we specified a layer we need to identify it

      layername <- lapply(as.list(files), function(x) sf::st_layers(x)[[1]]) # layer name for each file
      #layername <- layername[grepl(layerstring, layername)] %>% unlist()

      files <- files[grepl(layerstring, layername)] # subset to files with required layer
   }

if (is.null(layername)) stop("Could not find layer with specified name in this folder.")

   firstfilelayers <- sf::st_layers(files[[1]])[[1]]

   if (!is.null(layerstring)){
   mylayer <- firstfilelayers[grepl(layerstring, firstfilelayers)]
   } else {
   mylayer <- firstfilelayers[[1]]
}
   # import 0 rows from the data but returns headers (using first file that contains this layer)
   actualnames <- sf::st_read(files[[1]],
                              layer = mylayer, query = paste("select * from \"", mylayer, "\" limit 0", sep=""),
                              quiet = TRUE)

   return(list(names(actualnames)))

}


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




## MODULE TO CAPTURE A PATH --------------------

definePathsUI <- function(id, buttonLabel, winTitle) {
   ns <- NS(id)

   div(style="display:block;",

       # mastermap path button

       fluidRow(
          column(4, shinyFiles::shinyDirButton(ns("dirChooseButton"), label = buttonLabel,
                                               title = winTitle,
                                               buttonType = "default", class = NULL, icon = NULL, style = "margin: 20px 0px")
          ),
          column(8, shiny::verbatimTextOutput(ns("pathName"),
                                              placeholder = TRUE)  # placeholder to display box even before path is filled in
          )
       )
   )
}


definePaths <- function(input, output, session) {
   ### Define reactive values for file paths ----
   path <- shiny::reactiveValues(path = NA)
   volumes <- shinyFiles::getVolumes()  # recognise drives available locally


   # Find directory for a set of files
   shinyFiles::shinyDirChoose(input, id = "dirChooseButton", root = volumes)  # associated with DirButton - gets a path

   shiny::observeEvent(input$dirChooseButton, {
      temppath <- try(shinyFiles::parseDirPath(volumes, input$dirChooseButton) %>% as.character())
      req(temppath) # only assign to the reactive once there is a path
      path$path <- temppath  # updates the reactive value when directory is selected
   })

   output$pathName <- shiny::renderText({
      req(!is.na(path$path))  # need actual path before displaying it on screen (prevents NA from showing up)
      path$path})

   return(shiny::reactive(path$path))
}


## MODULE TO POP UP A MODAL FOR PATHS ------

# Modal module UI  #  NO NEED FOR A UI
# modalModuleUI <- function(id) {
#    ns <- NS(id)
# }

# Modal module server
modalModule <- function(input, output, session,
                        layername = "layer or attribute", # dataset name
                        holder="type something",
                        searchlist) {  # a list of (lists of) layer names


   myModal <- function(failed = FALSE) {
      ns <- session$ns
      modalDialog(
         textInput(ns("entertext"), paste0("How is ", layername," named in your dataset?"),
                   placeholder = holder
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

      # Check that data object exists and is data frame.
      if (all(unlist(lapply(searchlist, function(x) any(grepl(input$entertext,x)))))
      ){

         # all good
         newname$good <- input$entertext
         removeModal()

      } else {
         showModal(myModal(failed = TRUE))
      }
   })

   return(reactive(newname$good))
}
