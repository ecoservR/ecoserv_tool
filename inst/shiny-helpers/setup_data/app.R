############################################################
### Wizard app to select data sources                    ###
### Saves a project log to the project folder            ###
### Sandra Angers-Blondin                                ###
### November 2020, updates Apr 2022                      ###
############################################################

library(dplyr)
library(shiny)
library(sf)

## No need to source the functions if they are placed within a "R" folder in app directory
#source("funmod.R") # source functions and modules
#source(system.file("shiny-helpers/setup_data/funmod.R", package = "ecoservR"))


# Global stuff ------------------------------------------------------------

# retrieve the user's working directory
#user_proj <-  shiny::getShinyOption("projwd")

user_proj <- projwd   # set when the app is launched via the launchWizard function (wd passed as argument)

# UI ----------------------------------------------------------------------
ui <- fluidPage(

   shinyjs::useShinyjs(),

   tags$head(
      shiny::includeCSS(file.path('www', 'theme.css'))
   ),

   shiny::titlePanel(
      title=div(id = "header")  # branding
   ),

   ## Field to set up project path

   div(id = "projectpath", class = "main-content",
       shiny::titlePanel("Select your project folder"),
       p("This is the folder where project parameters and model outputs should be stored. It should be automatically populated if you are working within an R project as recommended."),
       shiny::fluidRow(
          definePathsUI(id = "buttonproject",
                        buttonLabel = "Project folder",
                        winTitle = "Please select your project folder"),
       )
   ),

   ## Field to set up all data inputs

   div(id = "setpaths", class = "main-content",

       shiny::titlePanel("Select your data inputs"),

       p("For each data input you want to use, please navigate to the", strong("folder"), "(NOT individual files) containing the relevant data. These folders should be self-contained, i.e. not include other spatial datasets. Sub-folders are allowed (e.g. mastermap tiles). Please refer to user guide for more information."),

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
                                      buttonLabel = "OS Open Greenspace",
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


       shinyjs::hidden(
          div(id = "step1_confirmation",

              h5(icon("check-circle", class = NULL, lib = "font-awesome"), "All seems good! We'll use these datasets. Press next to continue."),
              actionButton("nextpage", "Next")
          )
       ),

       shinyjs::hidden(
          div(id = "datapreview",
              shinycssloaders::withSpinner(
                 tableOutput("df")
              ))
       ),


       br()

   ), # end of div for paths

   shinyjs::hidden(div(id = "setproject", class = "main-content",


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



# Server ------------------------------------------------------------------
server <- function(input, output, session) {

   ### Initialise reactive values -----

   # an empty dataframe that we'll fill upon hitting the check button
   rv <- reactiveValues(df = NULL,
                        realnames = vector(mode = "list", length = 10))   # list of attribute names from datasets



   # Collect file paths ------
   # they are all reactive expressions returned from module
   # Before a value is selected, value is NA

   paths <- reactiveValues(
      mm = callModule(definePaths, "button1", defaultpath = user_proj),
      studyArea = callModule(definePaths, "button2", defaultpath = user_proj),
      green = callModule(definePaths, "button3", defaultpath = user_proj),
      opgreen = callModule(definePaths, "button4", defaultpath = user_proj),
      corine = callModule(definePaths, "button5", defaultpath = user_proj),
      nfi = callModule(definePaths, "button6", defaultpath = user_proj),
      phi = callModule(definePaths, "button7", defaultpath = user_proj),
      crome = callModule(definePaths, "button8", defaultpath = user_proj),
      dtm = callModule(definePaths, "button9", defaultpath = user_proj),
      hedge = callModule(definePaths, "button10", defaultpath = user_proj),
      proj = callModule(definePaths, "buttonproject", defaultpath = user_proj, set = TRUE)
   )

   # # For testing and development
   # paths <- reactiveValues(
   #   mm = reactive(file.path(projwd, "mastermap")),
   #   studyArea = reactive(file.path(projwd,"studyarea")),
   #   green = reactive(file.path(projwd,"greenspace")),
   #   opgreen = reactive(file.path(projwd,"opengreenspace")),
   #   corine = reactive(file.path(projwd,"corine")),
   #   nfi = reactive(file.path(projwd,"nfi")),
   #   phi = reactive(file.path(projwd,"phi")),
   #   crome = reactive(file.path(projwd,"crome")),
   #   dtm = reactive(file.path(projwd,"dtm")),
   #   hedge = reactive(NA),
   #   proj = callModule(definePaths, "buttonproject", defaultpath = user_proj, set = TRUE)
   # )


   ### Data checking ------

   ## Only enable Check button when compulsory datasets are not NA

   observeEvent(req(paths$mm(),
                    paths$studyArea()
   ), {
      shinyjs::enable("checkdata")
   })


   ## When checked is clicked, create the dataframe with all data
   # This will record all the paths (user inputs) and set the initial values for the layer and attribute checks

   observeEvent(input$checkdata, {

      rv$df <- dplyr::tibble(

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

         type = try(guessFiletypeV(path)),  # automatically detect extension

         # when specified, layer name must contain this string
         layer = NA_character_,


         cols = c(
            mm = list(c("TOID" = "TOID", # mmcols
                        "PhysicalLevel" = "PhysicalLevel",
                        "Group" = "DescriptiveGroup",
                        "Term" = "DescriptiveTerm",
                        "Theme" = "Theme",
                        "Make" = "Make")),

            SA = list(c(NA_character_)), # studyarea

            green = list(c("TOID" = "toid",
                           "priFunc" = "primaryFunction")),  # green cols

            opgreen = list(c("id" = "id", # opgreen cols
                             "op_function" = "function.")),

            corine = list(c("code" = "Code_18")),  # corine cols  IF VECTOR FORMAT

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


   ### File input check ----

   validtype <- reactive({
      req(rv$df)

      if (any(grepl("error", rv$df$type))){
         FALSE } else {TRUE}
   })


   ## If file extension cannot be detected, show a pop up informing the user of errors

   observeEvent({
      c(validtype(),
        input$checkdata
      )},
      {

         if (isFALSE(validtype())){

            errortable <- rv$df[grepl("error", rv$df$type), c("prettynames", "type")] %>%
               dplyr::mutate(type = dplyr::case_when(
                  type == "error folder" ~ "Directory not found. Check file path.",
                  type == "error no" ~ "No spatial files found in folder or subfolders.",
                  type == "error multiple" ~ "Multiple spatial file extensions detected."
               ))
            names(errortable) <- c("Dataset", "Error")

            shinyalert::shinyalert(title = "Oops!",
                                   text = kableExtra::kbl(errortable, format= "html"),  # rendering table as HTML so displays in modal
                                   type = "error",
                                   html = TRUE)
         }
      })



   ### Layer name check -----
   ## This new layer check is less thorough but less annoying. It only makes sure that there is at least one polygon layer in the specified folder and records the name.
   ## If there are many names, they get strung together with | as separator to allow regex to work in the checkAttrNames function

   # For mastermap

   observe({
      req(isTRUE(validtype()))

      mmlayers <-check_layers(rv$df[rv$df$dataset == "mm", ][["path"]])

      if (length(mmlayers) < 1){

         shinyalert::shinyalert(title = "Geometry error",
                                text = "Could not find a polygon layer for MasterMap Topography. Please check your data inputs and run the wizard again.",
                                type = "error")

      }


      rv$df[rv$df$dataset == "mm", ][["layer"]] <- paste0(mmlayers, collapse = "|")

   })

   ## For Open greenspace

   observe({
      req(isTRUE(validtype()))

      opgrlayers <- check_layers(rv$df[rv$df$dataset == "OS_OpenGreenspace", ][["path"]])


      if (length(opgrlayers) < 1){

         shinyalert::shinyalert(title = "Geometry error",
                                text = "Could not find a polygon layer for OS Open Greenspace. Please check your data inputs and run the wizard again.",
                                type = "error")

      }

      rv$df[rv$df$dataset == "OS_OpenGreenspace", ][["layer"]] <- paste0(opgrlayers, collapse = "|")


   })



   ## Check if we have polygon data among the files

   validlayers <- reactive({

      # test <- mapply(function(x, n)
      #    any(grepl(rv$df[rv$df$dataset == n, "layer"], x, ignore.case = TRUE)),
      #    x = layers(),
      #    n = names(layers())
      # )

      test <- !is.na(rv$df[rv$df$dataset == "mm", ][["layer"]]) & !is.na( rv$df[rv$df$dataset == "OS_OpenGreenspace", ][["layer"]])

      return(unlist(test)) # return the result of the logical test
   })



   ### Attribute name check -----

   ## When layers are sorted, check all required attributes

   # This observer waits for other validity checks to pass, and then stores all the attributes
   # fetched from the data into a reactive list (that is NOT part of the rv$df, otherwise would cause re-evaluation of all the checks)

   observe({
      req(isTRUE(validtype()) && all(validlayers()))

      # only evaluate when other checks have passed and the df is created

      #message("Data inputs and layers are valid, checking attributes")

      for (i in 1:nrow(rv$df)){

         isolate({
            # for each data input that has been specified (avoid empty paths and raster files which don't have attributes)
            if (!is.na(rv$df[i, ][["path"]]) && !is.null(rv$df[i, ][["path"]]) &&
                !rv$df[i, ][["type"]] %in% c("asc", "tif")){

               rv$realnames[i] <- checkAttrNames(  # compare required attributes to headers in data
                  folder = rv$df[i, ][["path"]],
                  type = rv$df[i, ][["type"]],
                  layerstring = if (!is.na(rv$df[i, ][["layer"]]) && !is.null(rv$df[i, ][["layer"]])){
                     # the layer name is a regular expression, not the actual name always, so we do a search
                     # for the first layer containing it
                     rv$df[i, ][["layer"]]
                  } else NULL,
                  user_names = rv$df[i, ][["cols"]][[1]],
                  name = rv$df[i, ][["dataset"]]
               )

            }

         }) # end of isolate

      }

   })


   ## Compare the actual names to the expected names, and bring a pop-up if clarification is required

   faulty <- reactive({
      req(rv$realnames)

      faultylist <- vector(mode = "list", length = length(rv$df$dataset))  # initialise empty list
      names(faultylist) <- rv$df$dataset # name the list

      for (i in 1:length(faultylist)){  # loop through each dataset and check which attributes are wrong

         if(is.na(rv$df[i, ][["path"]]) | is.null(rv$df[i, ][["path"]]) |
            rv$df[i, ][["dataset"]] %in% c("studyArea", "dtm", "hedgerows") |
            rv$df[i, ][["type"]] %in% c("tif", "asc")){
            # no attributes needed for unused datasets, SA, dtm or hedge, or raster data

            faultylist[[i]] <- c(as.integer(0))[-1]  # create an empty integer

         } else { # for all other datasets we need to know which attributes are not named properly
            ## we allow difference in case

            #faultylist[[i]] <- which(!rv$df[i, ][["cols"]][[1]] %in% rv$realnames[[i]])

            faultylist[[i]] <- which(!unlist(grepl(
               paste0(rv$realnames[[i]], collapse = "|"),
               rv$df[i, ][["cols"]][[1]],
               ignore.case = TRUE)))

         }
      } # end of loop

      return(faultylist)
   })

   observe({
      req(isTRUE(validtype()) && all(validlayers()), rv$realnames, faulty())

   })

   newattr <- reactiveValues(name = NULL)  # empty reactive to store attribute input

   ## Reactively check whether there are any problems with names

   observe({

      req(isTRUE(validtype()) && all(validlayers()), rv$realnames, faulty())

      if (any(lengths(faulty()) > 0)){

         # Because we're in an observer, we can keep evaluating the first condition,
         # and it will update as cases are resolved

         ds <- names(which(lengths(faulty()) > 0)[1]) # first dataset that has a problem
         index <- faulty()[[ds]] # extract index of wrong attributes for this dataset

         newattr$name <- callModule(attrPopupModule, paste0("modal_mmcols_", ds, index[1]), # unique id for modal
                                    dataset = rv$df[rv$df$dataset == ds, ][["prettynames"]],
                                    attrname = names(rv$df[rv$df$dataset == ds, ][["cols"]][[1]])[index[1]],
                                    searchlist = rv$realnames[which(rv$df$dataset == ds)])


         newattr$ds <- ds
         newattr$index <- index[1]


      } else {newattr$name <- reactive(NULL)} # NULL so that no assignment is carried in next observer if nothing needs changing

   })

   ## Assign the name specified by user into the dataframe
   observe({
      req(newattr$name, newattr$name())

      rv$df[rv$df$dataset == newattr$ds, ][["cols"]][[1]][newattr$index] <- newattr$name()
   })



   ## Dataframe that appears to confirm paths and layers ---------

   observeEvent(input$checkdata, {
      shinyjs::showElement("datapreview")
   })

   output$df <- renderTable({
      req(input$checkdata)
      dplyr::filter(rv$df[,c(1:4)], !is.na(path)) %>%
         dplyr::select(-dataset) %>%
         dplyr::rename(dataset = prettynames)
   })

   # Move to project parameters page -----------------------------------------

   ## When all tests are successful, show confirmation and action button

   observe({

      req(isTRUE(validtype()) && all(validlayers()), rv$realnames, faulty())

      if (all(lengths(faulty()) == 0)){

         shinyjs::showElement("step1_confirmation")
      }

   })


   ## When next is clicked, show second page

   observeEvent(input$nextpage, {
      shinyjs::hideElement("setpaths")
      shinyjs::hideElement("projectpath")
      shinyjs::showElement("setproject")

   })


   params = reactiveValues()  # empty reactive values to store parameters


   ## When all inputs are there, enable button to save log

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

      ## remove projpath from data paths whenever possible to increase portability
      ## working on a copy, because working on the reactive rv$df causes checkAttrNames to reevaluate

      df <- rv$df
      df$path <- gsub(paste0(user_proj, "/"), "", df$path)


      final_log <- list(
         title = gsub(" ", "_", input$projtitle),
         projpath = paths$proj(),
         output_temp = file.path(paths$proj(), "intermediary"),
         df = df, # add back the datasets we're not using
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




}  # end of server

shinyApp(ui = ui, server = server, options = list(width = 1600))
