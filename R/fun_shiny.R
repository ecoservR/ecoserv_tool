#######################################
### App launch functions            ###
### for EcoservR                    ###
### Sandra Angers-Blondin           ###
### 5 October 2020                  ###
#######################################

#' Launch Project Setup Wizard
#' This function launches an interactive app to prompt the user to select data inputs and parameters.

#' @return Nothing - but saves an RDS file with the project parameters
#' @export
launchWizard <- function(projwd = NULL){

   if (is.null(projwd)){
      projwd <- getwd()
   }

   .GlobalEnv$projwd <- projwd
   on.exit(rm(projwd, envir=.GlobalEnv))

    appDir <- system.file("shiny-helpers", "setup_data", package = "ecoservR")

    if (appDir == "") { stop("Could not find wizard app.", call. = FALSE) }
    shiny::runApp(appDir, display.mode = "normal")

    }




