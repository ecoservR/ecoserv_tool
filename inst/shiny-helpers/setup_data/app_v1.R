require(magrittr)

### Custom functions

checkAttrNames <- function(folder, layerstring = NULL, user_names, name = NULL){

   # Using st_read with a query of 0 rows only returns the headers and metadata.

   type <- guessFiletype(folder)  # identify file extension

   firstfile <- list.files(folder, pattern = type, full.names = TRUE, recursive = TRUE)[[1]]

   if (is.null(layerstring)){ # if layer not specified, probably just one and we check the first
      layername <- sf::st_layers(list.files(folder, pattern = type, full.names = TRUE, recursive = TRUE)[[1]])[[1]]
   } else {
      layername <- sf::st_layers(firstfile)[[1]]
      layername <- layername[grepl(layerstring, layername)]
   }

   # import 0 rows from the data but returns headers
   actualnames <- sf::st_read(firstfile,
                          layer = layername, query = paste("select * from \"", layername, "\" limit 0", sep=""),
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


# ui ----------------------------------------------------------------------


ui <- fluidPage(

   shinyjs::useShinyjs(),
   shinyalert::useShinyalert(),

   tags$head(
      shiny::includeCSS(file.path('www', 'theme.css'))),

   shiny::titlePanel(
      title=div(id = "header")  # branding
   ),

   div(id = "setpaths", class = "main-content",

   #shiny::mainPanel(width = 12,

   shiny::titlePanel("Select your data inputs"),

   p("For each data input you want to use, please navigate to the", strong("folder"), "containing the relevant files. These folders should be self-contained, i.e. not include other datasets. Sub-folders are allowed (e.g. mastermap tiles). Please refer to user guide for more information."),

   shiny::fluidRow(
shiny::column(6,
   definePathsUI(id = "button1",
                 buttonLabel = "MasterMap*",
                 winTitle = "Please select MasterMap folder"),

   definePathsUI(id = "button2",
                 buttonLabel = "Study area*",
                 winTitle = "Please select study area folder"),

   definePathsUI(id = "button3",
                 buttonLabel = "OS Greenspace",
                 winTitle = "Please select OS Greenspace folder"),

   definePathsUI(id = "button4",
                 buttonLabel = "OS Open Greenspace*",
                 winTitle = "Please select OS Open Greenspace folder"),

   definePathsUI(id = "button5",
                 buttonLabel = "CORINE",
                 winTitle = "Please select CORINE folder")
),
shiny::column(6,
   definePathsUI(id = "button6",
                 buttonLabel = "National Forest Inventory",
                 winTitle = "Please select NFI folder"),

   definePathsUI(id = "button7",
                 buttonLabel = "Priority Habitat Inventory",
                 winTitle = "Please select Priority Habitat folder"),

   definePathsUI(id = "button8",
                 buttonLabel = "Crop Map of England",
                 winTitle = "Please select CROME folder"),

   definePathsUI(id = "button9",
                 buttonLabel = "Terrain (DTM) data",
                 winTitle = "Please select DTM folder"),

   definePathsUI(id = "button10",
                 buttonLabel = "Hedgerow data",
                 winTitle = "Please select hedgerow folder")
)
),

shiny::h2(""),

   shinyjs::disabled(actionButton("checkdata", "Submit and check")),
   p("* required data inputs"),

shinyjs::hidden( # doesn't need an actual output to show spinner
   div(id = "busymsg",
       shinycssloaders::withSpinner(textOutput("busy"), size = 1.2, proxy.height = 100,
                                    type = 5, color = "#0dc9b6")
       )
   ),

br(),

# textOutput("isvalid")
# tableOutput("reactval"),
# tableOutput("faultyvec"),
# textOutput("testindex")

 #  )# end of main panel
   ), # end of div for paths

shinyjs::hidden(div(id = "setproject", class = "main-content",

       shiny::titlePanel("Set your project folder"),

       p("Please select your project folder where outputs will be saved.
         This should correspond to the folder where you are currently working in your R project."),

       definePathsUI(id = "projectpath",
                     buttonLabel = "Project folder",
                     winTitle = "Please select your project folder"),

       textInput("projtitle", "Give your project a title",
                 placeholder = "e.g. Dane catchment"),

       br(),

       shiny::h2("Set parameters for analysis and classification"),
       shiny::p("We recommend using the default values unless you have data specific to your study area.
                Please refer to the user guide for more information."),

      fluidRow(shiny::column(width = 6,
         numericInput("SAbuffer", "Study area buffer (m)", value = 500, min = 0, max = 3000),
         numericInput("housemin", "Minimum house size", value = 30, min = 0, max = 1000),
         numericInput("housemax", "Maximum house size", value = 800, min = 0, max = 100000),
         numericInput("gardensize", "Garden size threshold (m2)", value = 800, min = 0, max = 10000),
         numericInput("gardenshape", "Garden shape ratio", value = 10, min = 0, max = 20),
         numericInput("upland", "Elevation threshold for upland habitats (m)", value = 250, min = 200, max = 500)
                             ),
               shiny::column(width = 6,
         numericInput("montane", "Elevation threshold for montane habitats (m)", value = 600, min = 300, max = 900),
         numericInput("arable_min", "Minimum size for arable land parcel (m2)", value = 5000, min = 1000, max = 10000),
         numericInput("improved_max", "Maximum size for improved grassland (m2)", value = 250000, min = 10000, max = 500000),
         numericInput("slope_semi", "Max slope for semi-improved land (degrees)", value = 11, min = 5, max = 20),
         numericInput("slope_unimp", "Slope threshold for unimproved land (degrees)", value = 18, min = 5, max = 20),
         numericInput("slope_dry", "Slope threshold for dry slopes (degrees)", value = 18, min = 5, max = 20)

                             #) # end of main panel
      ),

      br(),

      shinyjs::disabled(actionButton("setproj", "Set up my project"))
      #,

      #shinycssloaders::withSpinner(textOutput("success"))



    )# end of main panel
)), # end of div for project setup
br(),

div(id = "footer",
    br(),
   fluidRow(
      img(src = "LJMU_logo.JPG", height="55px", width = "auto"),
      img(src = "natcapsolutions.png", height="40px", width = "auto")
   )
)

)# end of ui


# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {


# Empty object that will collect paths --------------
   paths <- reactiveValues(mm = NA_character_,
                           studyArea = NA_character_,
                           green = NA_character_,
                           opgreen = NA_character_,
                           nfi = NA_character_,
                           phi = NA_character_,
                           crome = NA_character_,
                           dtm = NA_character_,
                           hedge = NA_character_,
                           proj = NA_character_)


# Capture all the file paths, and also display the chosen paths in the text boxes ---------

## Required paths hard coded for testing - but reactive works ## DO FIX LATER
   paths$mm <- callModule(definePaths, "button1")
   #paths$mm <- reactive("C://Basemapper/data/mastermap") # for testing
   paths$studyArea <- callModule(definePaths, "button2")
   #paths$studyArea <- reactive("C://Basemapper/data/studyArea") # for testing
   paths$green <- callModule(definePaths, "button3")
   paths$opgreen <- callModule(definePaths, "button4")
   #paths$opgreen <- reactive("C://Basemapper/data/opengreenspace") # for testing
   paths$corine <- callModule(definePaths, "button5")
   paths$nfi <- callModule(definePaths, "button6")
   paths$phi <- callModule(definePaths, "button7")
   paths$crome <- callModule(definePaths, "button8")
   paths$dtm <- callModule(definePaths, "button9")
   paths$hedge <- callModule(definePaths, "button10")


## We create an empty df that will store the working copy of the datalog object
   rv <- reactiveValues(df = NULL)



# datalog object created from paths ------------
datalog <- reactive({
   dplyr::tibble(
      dataset = c("mm",
                  "studyArea",
                  "OS_Greenspace",
                  "OS_OpenGreenspace",
                  "corine",
                  "nfi",
                  "phi",
                  "crome",
                  "terrain",
                  "hedgerows"),
      prettynames = c(  # full dataset names for better pop up messages
         "OS MasterMap",
         "your study area",
         "OS Greenspace",
         "OS Open Greenspace",
         "CORINE land cover",
         "National Forest Inventory",
         "Priority Habitat Inventory",
         "Crop Map of England",
         "digital terrain model",
         "hedgerow linear data"
      ),
      status = c("C","C","O","C", "O","O","O","O", "O", "O"),  # compulsory or optional
      path = c(
         paths$mm(),
         paths$studyArea(),
         paths$green(),
         paths$opgreen(),
         paths$corine(),
         paths$nfi(),
         paths$phi(),
         paths$crome(),
         paths$dtm(),
         paths$hedge()
      ),
      layer = c("TopographicArea",
                NA_character_,
                NA_character_,
                "GreenspaceSite",
                NA_character_,
                NA_character_,
                NA_character_,
                NA_character_,
                NA_character_,
                NA_character_),
      cols = c(
         mm = list(c("TOID" = "TOID", # mmcols
               "PhysicalLevel" = "PhysicalLevel",
               "DescriptiveGroup" = "DescriptiveGroup",
               "DescriptiveTerm" = "DescriptiveTerm",
               "Theme" = "Theme",
               "Make" = "Make")),

         SA = list(c(NA_character_)), # studyarea

         green = list(c("TOID" = "toid",
           "priFunc" = "priFunc")),  # green cols

         opgreen = list(c("id" = "id", # opgreen cols
           "op_function" = "function.")),

         corine = list(c("code" = "Code_18")),  # corine cols

         nfi = list(c("IFT_IOA" = "IFT_IOA",
           "CATEGORY" = "CATEGORY")), # nfi cols

         phi = list(c("Main_Habit" = "Main_Habit")), # phi cols

         crome = list(c("cromeid" = "cromeid",  # crome cols
           "lucode" = "lucode")),

         dtm = list(c(NA_character_)), # dtm

         hedge = list(c(NA_character_))  # hedges
      ),
      realnames = list(NA)
   )
})


# Only allow submission of paths when compulsory datasets have inputs ------

observeEvent(req(paths$mm(),
                 paths$studyArea(),
                 paths$opgreen()
                 ), {
   shinyjs::enable("checkdata")
})



# # Display the updated table (for testing)
#
# output$reactval <- renderTable({
#       req(rv$df)
#       dplyr::select(rv$df, -cols, -realnames) %>% dplyr::filter(!is.na(path))
#       })
#
#

# Trigger the layer check ----------------------------------------------------

# copy paths to the reactive values object

observeEvent(input$checkdata, {

# # Display the table (for testing)
#    output$logtable <- renderTable({
#       dplyr::select(datalog(), -cols, -realnames)})

rv$df <- dplyr::filter(datalog(), !is.na(path))

})  # end of check button observer


## Create list of layers for mm and opengreenspace
layers <- reactive({

   req(rv$df[rv$df$dataset == "mm", ][["path"]])

   mm <- lapply(list.files(rv$df[rv$df$dataset == "mm", ][["path"]],
                           pattern = paste0(guessFiletype(rv$df[rv$df$dataset == "mm", ][["path"]]),"$"),
                           recursive=TRUE,full.names = TRUE),
                function(x) sf::st_layers(x)[[1]])

   opgr <- lapply(list.files(rv$df[rv$df$dataset == "OS_OpenGreenspace", ][["path"]],
                             pattern = paste0(guessFiletype(rv$df[rv$df$dataset == "OS_OpenGreenspace", ][["path"]]),"$"),
                             recursive=TRUE,full.names = TRUE),
                  function(x) sf::st_layers(x)[[1]])

   layerlist <- list(mm, opgr)
   names(layerlist) <- c("mm", "OS_OpenGreenspace") # naming so can be used in subsetting
   return(layerlist)

})

## Check if the layers we need are in that list of layers
validlayers <- reactive({
   req(layers())
   test <- mapply(function(x, n)
      any(grepl(rv$df[rv$df$dataset == n, "layer"], x)),
      x = layers(),
      n = names(layers())
   )

   return(test) # return the result of the logical test
})

# output$isvalid <- renderText({
#    req(validlayers, input$checkdata)
#    validlayers()
# })

## If we can't find layers, ask user:
# create empty reactive value to store input
newlayers <- reactiveValues(mm = NULL,
                            opgr = NULL)

# if mm invalid, trigger pop up and save input
observe({
   if (!validlayers()[[1]]) {
      newlayers$mm <- callModule(modalModule, "modal_mm",
                                 layername = "the layer for OS MasterMap",
                                 holder = "e.g. TopographicArea",
                                 searchlist = layers()$mm)
   }
   })

# if there is an input, update working df - WORKS!!
observe({
   req(newlayers$mm, newlayers$mm())

   rv$df[rv$df$dataset == "mm", "layer"] <- newlayers$mm()
   })


# once mm verified AND if greenspace invalid, fix it too
observe({

   if (validlayers()[[1]] && !validlayers()[[2]]) {

      newlayers$opgr <- callModule(modalModule, "modal_opgr",
                                 layername = "the layer for OS Open Greenspace",
                                 holder = "e.g. GreenspaceSite",
                                 searchlist = layers()$OS_OpenGreenspace)
   }
   })

# if there is an input, update working df - WORKS!!
observe({
   req(newlayers$opgr, newlayers$opgr()) # object exists AND its value is non null

   rv$df[rv$df$dataset == "OS_OpenGreenspace", "layer"] <- newlayers$opgr()
})


# When all layers are ok, check attributes --------------------------------

## ALL OK! the faulty() checks updates every time a new correct user input is added, so cycles through until everything has a match

# We need to make sure the columns required in the models are named properly

# output$names <- renderText({  # This chunk means the loop updating the rv$df with actual names works
#
#   req(rv$df)
#    unlist(rv$df$realnames)
#
# }) # end of observer

# This observer registers the real attributes in the user's data
observe({
   if(all(unlist(validlayers()))){
   #req(rv$df)

for (i in 1:nrow(rv$df)){
   if (!is.na(rv$df[i, ][["path"]])){

   rv$df[i, ][["realnames"]] <- checkAttrNames(
      rv$df[i, ][["path"]],
      layerstring = if (!is.na(rv$df[i, ][["layer"]])){
         # the layer name is a regular expression, not the actual name always, so we do a search
         # for the first layer containing it
         rv$df[i, ][["layer"]]
         } else NULL,
      rv$df[i, ][["cols"]][[1]],
      rv$df[i, ][["dataset"]]
   )
   }
}
      }

})


# Let's see which mm attributes are faulty (not named as expected) - this finds them properly

faulty <- reactive({
   req(rv$df$realnames)

   faultylist <- vector(mode = "list", length = length(rv$df$dataset))  # initialise empty list
   names(faultylist) <- rv$df$dataset # name the list

   for (i in 1:length(faultylist)){  # loop through each dataset and check which attributes are wrong

   if(rv$df[i, ][["dataset"]] %in% c("studyArea", "dtm", "hedgerows")){
      # no attributes needed for SA, dtm or hedge
      faultylist[[i]] <- c(as.integer(0))[-1]  # create an empty integer

   } else { # for all other datasets we need to know which attributes are not named properly
   faultylist[[i]] <- which(!rv$df[i, ][["cols"]][[1]] %in% rv$df[i, ][["realnames"]][[1]])
         }
   } # end of loop

   return(faultylist)
})

#output$faultyvec <- renderTable({lapply(faulty(), function(x) length(x))})

newattr <- reactiveValues(name = NULL)  # empty reactive to store attribute input

# Reactively check whether there are any problems with names
observe({

   if (all(unlist(validlayers())) & # do not display attributes popups until layers sorted
       any(lengths(faulty()) > 0)){

   # Because we're in an observer, we can keep evaluating the first condition, and it will update as cases are resolved

   ds <- names(which(lengths(faulty()) > 0)[1]) # first dataset that has a problem
   index <- faulty()[[ds]] # extract index of wrong attributes for this dataset

   newattr$name <- callModule(modalModule, paste0("modal_mmcols_", ds, index[1]), # unique id for modal
                  layername = paste0(rv$df[rv$df$dataset == ds, ][["prettynames"]], # pretty dataset name
                                     " attribute ",
                                     names(rv$df[rv$df$dataset == ds, ][["cols"]][[1]])[index[1]]),
                  holder = "attribute name exactly as it appears",
                  searchlist = rv$df[rv$df$dataset == ds, ][["realnames"]])

   newattr$ds <- ds
   newattr$index <- index[1]


   } else {newattr$name <- reactive(NULL)} # NULL so that no assignment is carried in next observer if nothing needs changing


})

# Assign the name specified by user into the dataframe
observe({
   req(newattr$name, newattr$name())
   rv$df[rv$df$dataset == newattr$ds, ][["cols"]][[1]][newattr$index] <- newattr$name()
})


## testing the changed value - works
# output$testindex <- renderText(rv$df[rv$df$dataset == "mm", ][["cols"]][[1]])




# Display spinning wheel while everything is being checked ----------------

observeEvent(input$checkdata, {
   shinyjs::showElement("busymsg")
})

# All data inputs ok, choose project folder and title --------------------------

# Create a trigger that signals that all verification are done
oktrigger <- reactive({
   req(validlayers, faulty)
   all(validlayers()) & all(lengths(faulty()) == 0) # all layers and attributes ok
})

# when trigger is true (all checks done), enable next page
observe({
   if (oktrigger()){
      shinyjs::hideElement("setpaths")
      shinyjs::showElement("setproject")


   paths$proj <- callModule(definePaths, "projectpath")
   }
})

params = reactiveValues()  # empty reactive values to store parameters

# When all inputs are there, enable button

observeEvent(req(paths$proj, paths$proj(), input$projtitle), {

   if (file.access(paths$proj()) == 0){
      # if write permission ok, enable button
   shinyjs::enable("setproj")} else {
   # otherwise bring a popup
      shinyalert::shinyalert(title = "Please select another folder",
                             text = "You do not appear to have write permission to this folder. Please select a different one.",
                             type = "warning")
   }

 })


observe({
   # Save inputs to params object

   params$SAbuffer = input$SAbuffer  # study area buffer
   params$gardensize = input$gardensize    # max size of a private garden, in m2
   params$gardenshape = input$gardenshape    # shape index threshold for a garden
   params$housemax = input$housemax    # max size for a house
   params$housemin = input$housemin       # min size for a house
   params$arable_min = input$arable_min   # min area (in m2) to consider a B4/J11 arable (smaller will become B4)
   params$improved_max = input$improved_max # area (m2) above which polygons considered too big to be B4 (will become J11)
   params$montane = input$montane       # elevation for montane habitats
   params$upland = input$upland        # elevation limit separating lowlands and uplands; used to make assumptions about semi improved vs improved grasslands
   params$slope_semi = input$slope_semi     # slope threshold for semi-improved grasslands
   params$slope_unimp = input$slope_unimp    # slope threshold for unimproved grasslands
   params$slope_dry = input$slope_dry      # slope threshold for dry slopes (turning uncertain wet stuff into heather) *check ArcGIS for default

})

# save the log file to project folder

observeEvent(input$setproj, {

   final_log <- list(
      title = gsub(" ", "_", input$projtitle),
      output_temp = paths$proj(),
      df = rbind(
         rv$df,
         dplyr::filter(datalog(), is.na(path))), # add back the datasets we're not using
      parameters = reactiveValuesToList(params)
   )

   saveRDS(final_log, file = file.path(paths$proj(),
                                       paste0(gsub(" ", "_", input$projtitle),
                                              "_projectlog.RDS")))

})

observe({
   req(input$setproj)
   if (file.exists(file.path(paths$proj(),
                             paste0(gsub(" ", "_", input$projtitle),
                                    "_projectlog.RDS"))
   )){
shinyalert::shinyalert(title = "Success!",
                       text = "You can now exit the wizard and return to your R session.",
                       type = "success")
   }

})

#  output$success <- renderText({
#    req(input$setproj)
#       if (file.exists(file.path(paths$proj(),
#                                 paste0(gsub(" ", "_", input$projtitle),
#                                     "_projectlog.RDS"))
#       )){
#          "Success! You can now exit the wizard."
#       }
#
# })


}  # end of server




shinyApp(ui = ui, server = server, options = list(width = 1600))

