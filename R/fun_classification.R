#######################################
### Classification functions        ###
### for EcoservR                    ###
### Sandra Angers-Blondin           ###
### 11-01-2021                      ###
#######################################


#' Permute Terms
#'
#' This function returns all possible permutations of given terms.

#' @param ... Terms to shuffle, in quotation marks
#' @param sep The separator that will appear between elements in the resulting string (default is comma without spaces for OS MasterMap terms)
#' @return A vector containing all possible permutations of the terms, each permutation as a character string.
#' @export

permute <- function(..., sep = ","){
   x <- c(...)
   n <- length(x)
   r <- length(x)
   combin <- gtools::permutations(n, r, v = x)  # create all possible combinations of the elements

   apply(combin, 1, paste, collapse=sep) # string the combinations
}



# ROUND 1 Classification from Mastermap -----------------------------------

#' Classify Habitats Using OS MasterMap Terms
#'
#' Assigns the most likely habitat type based on information contained by OS MasterMap (using Descriptive Group and Descriptive Term).

#' @param x A basemap sf object, which must contain the attributes Group (Descriptive Group from OS MasterMap), Term (Descriptive Term from OS MasterMap) and Make (from OS MasterMap).
#' @param params A list of parameters used as thresholds to classify certain habitats. See EcoservR documentation.
#' @return The basemap sf object with a new attribute HabCode_B
#' @export

classif_mastermap <- function(x, params){

   x <- x %>% dplyr::mutate(

      ### first round of classif from MasterMap only ----

      HabCode_B = dplyr::case_when(

         # active mines should be classified as such - we do it first so they don't get swept under the rug by another regular expression
         grepl("Spoil Heap", Term) & !grepl("Inactive", Term) ~ "I22",  # should we classify if vegetated?
         grepl("Mineral Workings", Term) & !grepl("Inactive", Term) ~ "I2u",

         # inactive mines get classified as the vegetation that colonised them
         (grepl("Mineral Workings", Term) | grepl("Spoil Heap", Term)) &
            grepl("Coniferous Trees", Term) &
            grepl("Nonconiferous Trees", Term) ~ "A13",

         (grepl("Mineral Workings", Term) | grepl("Spoil Heap", Term)) &
            grepl("Coniferous Trees", Term) ~ "A12",

         (grepl("Mineral Workings", Term) | grepl("Spoil Heap", Term)) &
            grepl("Nonconiferous Trees", Term) ~ "A11",

         (grepl("Mineral Workings", Term) | grepl("Spoil Heap", Term)) &
            grepl("Heath", Term) ~ "D/I1",

         (grepl("Mineral Workings", Term) | grepl("Spoil Heap", Term)) &
            grepl("Rough Grassland", Term) ~ "Bu1",

         (grepl("Mineral Workings", Term) | grepl("Spoil Heap", Term)) &
            grepl("Scrub", Term) ~ "A2",


         Term == "Landfill" ~ "I24",


         # classifying roads, roadsides, and surfaces -----
         grepl("Road Or Track", Group) & Make == "Natural" ~ "J512",  # unsurfaced road
         grepl("Road Or Track", Group) & Make == "Manmade" ~ "J511",  # surfaced road
         grepl("General Surface", Group) & Make %in% c("Manmade", "Unknown") ~ "J37",  # sealed surface
         grepl("Roadside", Group) & Make == "Natural" ~ "J12v",  # improve
         grepl("Roadside", Group) & Make %in% c("Manmade", "Unknown") ~ "J52",  # pavement
         grepl("Rail", Group) & Make != "Natural" ~ "J53",      # railway
         grepl("Rail", Group) & Make == "Natural" ~ "C31",  # improve; rail verges
         grepl("Path", Group) & Make == "Manmade" ~ "J54",      # sealed path



         # classify gardens -----
         grepl("General Surface", Group) & Make == "Multiple" &
            shp_area > params$gardensize & shp_index > params$gardenshape & GI != "Private Garden" ~ "J55",

         grepl("General Surface", Group) & Make == "Multiple" & shp_area <= params$gardensize ~ "J56",   # catch-all rule if GI not available

         grepl("General Surface", Group) & Make == "Multiple" & shp_index <= params$gardenshape ~ "J56",


         # Buildings and manmade structures -----
         # takes precedence over some previous classification
         # anything with Structure in the DescGroup gets converted to Structure code -----


         grepl("Building", Group) & shp_area > params$housemax ~ "J361",  # bigger than house is a business
         grepl("Building", Group) & shp_area < params$housemin ~ "J362",  # smaller than house is a shed
         grepl("Building", Group) & dplyr::between(shp_area, params$housemin, params$housemax) ~ "J360",  # domestic
         ## I did not include the proximity to a garden - not all houses have gardens?!
         ## PROBLEM: a lot of farm buildings get classified as domestic...

         Group == "Glasshouse" ~ "J364",

         grepl("Structure", Group) & Make == "Manmade" ~ "J363",  # more general; whatever not caught above might be assigned as a structure with this


         grepl("Building", Group) ~ "J36u",  # improve
         # most general


         # Woodlands and trees -----
         # order shouldn't matter here as they are all equivalent (no overlap of multiple conditions)

         grepl("Hedgerow", Term, ignore.case = TRUE) ~ "J21",  # for hedges from mod08

         Term == "Orchard" ~ "A112o",


         ## Broadleaved forests
         Term == "Nonconiferous Trees" ~ "A11",

         Term %in% c(
            permute("Nonconiferous Trees", "Boulders (Scattered)"),
            permute("Nonconiferous Trees", "Rock (Scattered)"),
            permute("Nonconiferous Trees", "Boulders (Scattered)", "Rock (Scattered)")) ~ "A11",  # forest with boulders


         Term %in% c(
            permute("Nonconiferous Trees", "Scrub"),
            permute("Nonconiferous Trees", "Scrub", "Coppice Or Osiers"),
            permute("Coppice Or Osiers", "Nonconiferous Trees"),
            permute("Nonconiferous Trees", "Boulders (Scattered)", "Coppice Or Osiers"),
            permute("Nonconiferous Trees", "Rock (Scattered)", "Coppice Or Osiers"),
            permute("Boulders (Scattered)","Nonconiferous Trees","Scrub"),
            permute("Rock (Scattered)","Nonconiferous Trees","Scrub"),
            permute("Boulders (Scattered)","Rock (Scattered)", "Nonconiferous Trees","Scrub")) ~ "A11/A2", # forest with scrub


         ## Coniferous forests

         Term == "Coniferous Trees" ~ "A12",

         Term %in% c(
            permute("Coniferous Trees", "Boulders (Scattered)"),
            permute("Coniferous Trees", "Shingle"),
            permute("Coniferous Trees", "Boulders (Scattered)", "Rock (Scattered)"),
            permute("Coniferous Trees", "Rock (Scattered)")) ~ "A12", # forest with boulders


         Term %in% c(
            permute("Coniferous Trees", "Scrub"),
            permute("Coniferous Trees", "Scrub", "Boulders (Scattered)"),
            permute("Coniferous Trees", "Scrub", "Rock (Scattered)")
            )~ "A12/A2",  # forest with scrub


         ## Mixed forests

         Term %in% permute("Coniferous Trees", "Nonconiferous Trees") ~ "A13",

         Term %in% c(
            permute("Coniferous Trees", "Nonconiferous Trees", "Shingle"),
            permute("Coniferous Trees", "Nonconiferous Trees", "Boulders (Scattered)"),
            permute("Coniferous Trees", "Nonconiferous Trees", "Boulders (Scattered)", "Rock (Scattered)"),
            permute("Coniferous Trees", "Nonconiferous Trees", "Rock (Scattered)")) ~ "A13",  # forest with boulders


         Term %in% c(
            permute("Coniferous Trees", "Nonconiferous Trees", "Coppice Or Osiers"),
            permute("Scrub", "Nonconiferous Trees", "Coniferous Trees"),
            permute("Scrub", "Nonconiferous Trees", "Coniferous Trees", "Rock (Scattered)"),
            permute("Scrub", "Nonconiferous Trees", "Coniferous Trees", "Boulders (Scattered)")) ~ "A13/A2",  # forest with scrub

         ## Scrub
         Term == "Scrub" ~ "A2",
         Term == "Coppice Or Osiers" ~ "A2",
         Term %in% c(
            permute("Coniferous Trees (Scattered)", "Scrub"),
            permute("Coniferous Trees (Scattered)", "Scrub", "Boulders (Scattered)"),
            permute("Nonconiferous Trees (Scattered)", "Scrub", "Boulders (Scattered)"),
            permute("Coniferous Trees (Scattered)", "Scrub", "Rock (Scattered)")
         ) ~ "A2",


         Term %in% c(
            permute("Scrub", "Coppice Or Osiers"),
            permute("Scrub", "Boulders (Scattered)"),
            permute("Scrub", "Rock (Scattered)")) ~ "A2",

         ## Scattered trees
         Term == "Nonconiferous Trees (Scattered)" ~ "A31",
         Term %in% c(
            permute("Nonconiferous Trees (Scattered)","Boulders (Scattered)"),
            permute("Nonconiferous Trees (Scattered)","Rock"),
            permute("Nonconiferous Trees (Scattered)","Boulders"),
            permute("Nonconiferous Trees (Scattered)","Rock", "Boulders"),
            permute("Nonconiferous Trees (Scattered)","Rock (Scattered)")) ~ "A31",
         Term %in% permute("Scrub", "Nonconiferous Trees (Scattered)") ~ "A31/A2",


         Term == "Coniferous Trees (Scattered)" ~ "A32",
         Term %in% c(
            permute("Coniferous Trees (Scattered)","Boulders (Scattered)"),
            permute("Coniferous Trees (Scattered)","Rock (Scattered)")) ~ "A32",
         Term %in% permute("Scrub", "Coniferous Trees (Scattered)") ~ "A32/A2",


         Term %in% permute("Coniferous Trees (Scattered)", "Nonconiferous Trees (Scattered)") ~ "A33",
         Term %in% c(
            permute("Nonconiferous Trees (Scattered)","Coniferous Trees (Scattered)", "Boulders (Scattered)"),
            permute("Nonconiferous Trees (Scattered)","Coniferous Trees (Scattered)", "Boulders (Scattered)")) ~ "A33",
         Term %in% permute("Coniferous Trees (Scattered)", "Nonconiferous Trees (Scattered)", "Scrub") ~ "A33/A2",



         ## Grasslands -----

         # Unknown, probable semi-improved
         Term == "Rough Grassland" ~ "Bu",

         # Rough grassland with rocky stuff must be unimproved
         # **perhaps not quite right
         Term %in% c(
            permute("Rough Grassland","Boulders"),
            permute("Rough Grassland","Boulders (Scattered)"),
            permute("Rough Grassland","Rock"),
            permute("Rough Grassland","Rock (Scattered)"),
            permute("Rough Grassland","Boulders","Rock"),
            permute("Rough Grassland","Rock","Boulders (Scattered)"),
            permute("Rough Grassland","Rock (Scattered)","Boulders"),
            permute("Rough Grassland","Rock (Scattered)","Boulders (Scattered)")) ~ "Bu1",

         # Grassland with scattered trees or scrub
         Term %in% c(
            permute("Rough Grassland","Scrub"),
            permute("Rough Grassland","Scrub","Nonconiferous Trees (Scattered)"),
            permute("Rough Grassland","Nonconiferous Trees (Scattered)")) ~ "Bu_A2/A31",


         # Grassland with more trees
         Term %in% c(
            permute("Rough Grassland","Nonconiferous Trees"),
            permute("Rough Grassland","Scrub","Nonconiferous Trees"),
            permute("Rough Grassland","Nonconiferous Trees (Scattered)")) ~ "Bu_A11",

         Term %in% permute("Rough Grassland","Coniferous Trees") ~ "Bu_A12",


         ## Rocky stuff -----

         Term == "Scree" ~ "I12",

         Term %in% c(
            "Boulders",
            permute("Boulders","Rock"),
            permute("Boulders (Scattered)","Rock"),
            permute("Boulders","Rock (Scattered)"),
            permute("Boulders","Rock"),
            "Boulders (Scattered)")  ~ "I14b",

         Term %in% c("Rock",
                                "Rock (Scattered)") ~ "I1",

         Term == "Cliff" ~ "I11/H8",    # can't know if it's maritime or inland without other data

         Term %in% c("Shingle") ~ "H3",

         # Classifying coastal
         Group == "Tidal Water" &
            (Term != "Foreshore" | is.na(Term)) ~ "G26",  # no lookup yet

         Term %in% c(permute("Sand","Mud"),
                     permute("Mud","Sand"),
                     permute("Foreshore","Mud"),
                     permute("Foreshore","Sand"),
                     "Mud",
                     "Sand") ~ "H11",


         # how do we know whether they are intertidal or above tide line?
         Term %in% c(permute("Sand","Shingle"),
                     permute("Foreshore","Shingle"),
                     permute("Foreshore","Sand","Shingle"),
                     "Shingle") ~ "H12",

         Term %in% c(permute("Sand", "Rough Grassland")) ~ "H65",
         # probably sand dunes, which will get picked up by PHI

         Term %in% c(
            permute("Foreshore","Rock"),
            permute("Boulders", "Sand"),
            permute("Rock", "Shingle"),
            permute("Rock", "Sand", "Shingle"),
            permute("Boulders","Foreshore"),
            permute("Boulders", "Shingle"),
            permute("Boulders", "Rock", "Shingle")) ~ "H13",

         grepl("Saltmarsh", Term) ~ "H2u",

         Term %in% c(
            permute("Mud","Shingle"),
            permute("Mud","Shingle", "Sand")) ~ "H1u",
         grepl("Foreshore", Term) & Make != "Manmade" ~ "H1u",




         ## Marsh habitats -----

         Term %in% c("Marsh") ~ "B5/E3/F/H2",  # replaced Marsh Reeds or Saltmarsh by Marsh

         Term %in% c(
            permute("Marsh","Nonconiferous Trees","Scrub"),
            permute("Marsh","Coniferous Trees","Scrub"),
            permute("Marsh","Nonconiferous Trees (Scattered)","Scrub"),
            permute("Marsh","Coniferous Trees (Scattered)","Scrub"),
            permute("Marsh","Nonconiferous Trees (Scattered)"),
            permute("Marsh","Coniferous Trees (Scattered)"),
            permute("Marsh","Nonconiferous Trees"),
            permute("Marsh","Coniferous Trees"),
            permute("Marsh","Scrub"),
            permute("Marsh","Nonconiferous Trees","Coniferous Trees"),
            permute("Marsh","Nonconiferous Trees","Coniferous Trees", "Scrub")) ~ "B5_Au",


         Term %in% c(
            permute("Marsh","Rough Grassland"),
            permute("Marsh","Rough Grassland", "Boulders"),
            permute("Marsh","Rough Grassland", "Boulders (Scattered)"),
            permute("Marsh","Rough Grassland", "Rock"),
            permute("Marsh","Rough Grassland", "Rock (Scattered)"),
            permute("Marsh", "Rock"),
            permute("Marsh", "Boulders"),
            permute("Marsh", "Rock (Scattered)"),
            permute("Marsh", "Boulders (Scattered)")) ~ "B5",

         ## Heathlands ----

         # Heath with rocks
         Term %in% c(
            permute("Heath","Boulders (Scattered)"),
            permute("Heath","Rock (Scattered)"),
            permute("Heath","Rock"),
            permute("Heath","Boulders"),
            permute("Heath","Boulders", "Rock"),
            permute("Heath","Boulders (Scattered)", "Rock"),
            permute("Heath","Boulders", "Rock (Scattered)"),
            permute("Heath","Boulders (Scattered)", "Rock (Scattered)")) ~ "D/I1",

         # Marshy heath
         Term %in% permute("Heath","Marsh","Rough Grassland") ~ "D5/D6",

         Term %in% c(
            "Heath",
            permute("Heath","Marsh")) ~ "D_B5/E3/F/H2",   # improve


         Term %in% permute("Marsh","Rough Grassland") ~ "B5/E3/F/H2",   # improve

         Term %in% c(
            permute("Heath","Rough Grassland","Boulders (Scattered)","Rock (Scattered)"),
            permute("Heath","Rough Grassland","Boulders (Scattered)"),
            permute("Heath","Rough Grassland","Rock (Scattered)"),
            permute("Heath","Rough Grassland","Rock","Boulders"),
            permute("Heath","Rough Grassland","Rock"),
            permute("Heath","Rough Grassland","Boulders"),
            permute("Heath","Rough Grassland","Boulders", "Rock (Scattered)"),
            permute("Heath","Rough Grassland","Boulders (Scattered)", "Rock"),
            permute("Heath","Rough Grassland")) ~ "D5",

         # catch-all for rest of heathery stuff

         grepl("Heath", Term) ~ "Du",  # NEW

         Term %in% c("Reeds",
                                permute("Reeds","Static Water")) ~ "F1",    # should it be swamp or something else?

         ## After all possible grasslands classified with other rules; uncertain agricultural land
         grepl("General Surface", Group) & Make == "Natural" ~ "B4/J11",


         ## Water habitats -----
         Group == "Sea" ~ "G3",

         grepl("Static Water", Term) | grepl("Reservoir", Term)  ~ "G1u",  # standing water

         grepl("Watercourse", Term) | grepl("Waterfall", Term) |
            grepl("Canal", Term)  ~ "G2u",  # running water

         grepl("Drain", Term) ~ "G",

         Group == "Inland Water" & Make == "Natural" ~ "G",
         Group == "Inland Water" & Make != "Natural" ~ "J363",   #structure: probably lock

         # unclassified things the size of a house and roughly square must be buildings
         Group == "Unclassified" & Make != "Natural" & shp_area < params$housemax & dplyr::between(shp_index, 1, 2) ~ "J363"

      )

   ) %>%  # end of mutate for mastermap classification

      ## add a filler condition for whatever has not been updated (unknown trees etc):

      dplyr::mutate(HabCode_B = ifelse(
         is.na(HabCode_B), # if there is no classification, add some fillers for habitats with trees and general grass
         dplyr::case_when(
            grepl("Heath", Term) &
               (grepl("Trees", Term) | grepl("Scrub", Term)) ~ "D5/Au",

            grepl("Rough Grassland", Term) &
               (grepl("Trees", Term) | grepl("Scrub", Term)) ~ "Bu_Au",

            grepl("Heath", Term) &
               grepl("Rough Grassland", Term) &
               (grepl("Trees", Term) | grepl("Scrub", Term)) ~ "D5/Bu/Au",

            Group == "Unclassified" & Theme == "Land" ~ "B"
         ),
         HabCode_B
      )
      )

   return(x)
}  # end of MasterMap classification function





# Classification with PHI -----------------------------------------

#' Classify Habitats Using OS Priority Habitats Inventory
#'
#' Assigns the most likely habitat type based on information contained in priority habitat inventory and first classification step by OS data. Not meant to be called directly; rather is used conditionally in classify_habitats().

#' @param x A basemap sf object, which must contain the attributes HabCode_B and phi
#' @return The basemap sf object with updated attribute HabCode_B
#' @export

classif_phi <- function(x){

   ## added grepl "J11" in many searches because putting crome/corine before PHI meant that several polygons were classified as arable and not picked up under PHI

   ## Do we need to add a OR is.na(HabCode_B) to each statement? Test on subset

   x <- x %>% dplyr::mutate(HabCode_B = dplyr::case_when(

      # from MOST SPECIFIC to most general

      HabCode_B == "J12v" ~ "J12v",  # avoid getting road verges mixed in with other J12 (amenity) polygons which could be overridden; verges shouldn't change

      ## Orchards

      phi == "Traditional orchard" & grepl("A", HabCode_B) ~ "A112o_T",  # traditional orchard
      phi != "Traditional orchard" & HabCode_B == "A112o" ~ "A112o",     # commercial orchard


      ## Semi-natural broadleaved
      phi == "Deciduous woodland" & (grepl("A", HabCode_B)|is.na(HabCode_B)) &
         HabCode_B != "B5_Au" & !grepl("I2", HabCode_B) ~ "A111", # all things with trees except boggy or quarry types


      ## Montane habitats
      phi == "Mountain heaths and willow scrub" &
         grepl(paste0(c("A", "B", "D", "J11"), collapse = "|"), HabCode_B) ~ "A2m",


      ## Moorland (no trees)
      phi == "Grass moorland" &
         grepl(paste0(c("B", "D", "J11"), collapse = "|"), HabCode_B) &
         !grepl("A", HabCode_B) ~ "B11m",

      ## Acid grassland
      phi == "Lowland dry acid grassland" &
         grepl(paste0(c("B", "D", "J11", "J12"), collapse = "|"), HabCode_B) &
         !grepl("A", HabCode_B) ~ "B11",

      phi == "Lowland meadows" & grepl("B", HabCode_B) & !grepl("A", HabCode_B) ~ "B21",
      phi == "Lowland meadows" & grepl(paste0(c("J11","J12"), collapse = "|"), HabCode_B) & !grepl("A", HabCode_B) ~ "B21", # in case mistakenly assigned as arable by corine/crome


      grepl("Calcareous grassland", phi, ignore.case = TRUE) & grepl("B", HabCode_B) & !grepl("A", HabCode_B) ~ "B31",
      grepl("Calcareous grassland", phi, ignore.case = TRUE) & grepl(paste0(c("J11","J12"), collapse = "|"), HabCode_B) & !grepl("A", HabCode_B) ~ "B31", # in case mistakenly assigned as arable by corine/crome
      grepl("Calcareous grassland", phi, ignore.case = TRUE) & grepl("D", HabCode_B) & !grepl("A", HabCode_B) ~ "B31",

      ## Floodplain or grazing marsh
      grepl("Floodplain", phi, ignore.case = TRUE) & grepl("B", HabCode_B) ~ "B4f",
      grepl("Floodplain", phi, ignore.case = TRUE) & grepl(paste0(c("J11","J12"), collapse = "|"), HabCode_B) ~ "B4f", # in case mistakenly assigned as arable by corine/crome
      grepl("Floodplain", phi, ignore.case = TRUE) & grepl("A", HabCode_B) ~ "B4f",


      phi == "Purple moor grass and rush pastures"  & grepl("B", HabCode_B) & !grepl("A", HabCode_B) ~ "B5",
      phi == "Purple moor grass and rush pastures"  & grepl("D", HabCode_B) & !grepl("A", HabCode_B) ~ "B5",

      # all heathlands excluding montane scrub
      grepl("heath", phi) & !grepl("Mountain heaths", phi) & HabCode_B == "A2b" ~ "D/I1",

      grepl("heath", phi) & !grepl("Mountain heaths", phi) & grepl(paste0(c("B","J11","J12"), collapse = "|"), HabCode_B)  & !grepl("A", HabCode_B) ~ "Du",  # not sure if should include fragmented heath
      grepl("heath", phi) & !grepl("Mountain heaths", phi) & HabCode_B %in% c("D/E", "D_B5/E3/F/H2") ~ "Du",

      phi == "Upland flushes, fens and swamps" & grepl(paste0(c("B","J11","J12"), collapse = "|"), HabCode_B) & !grepl("A", HabCode_B) ~ "E2/E3/F1",
      phi == "Upland flushes, fens and swamps" & grepl("D", HabCode_B) & !grepl("A", HabCode_B) ~ "E2/E3/F1",

      phi == "Lowland fens" & grepl(paste0(c("B","J11","J12"), collapse = "|"), HabCode_B) & !grepl("A", HabCode_B) ~ "E3/F1",
      phi == "Lowland fens" & grepl("D", HabCode_B) & !grepl("A", HabCode_B) ~ "E3/F1",

      phi == "Blanket bog" & HabCode_B %in% c("Bu", "Bu1/Bu2", "B4/J11", "B5", "B5/E3/F/H3", "B5/E3/F/H3_Bu1/Bu2") ~ "E161",
      phi == "Blanket bog" & grepl(paste0(c("B", "D"), collapse = "|"), HabCode_B) & !grepl("A", HabCode_B) ~ "E161",  # these should be E161

      grepl("raised bog", phi, ignore.case = TRUE) & grepl(paste0(c("B","J11","J12"), collapse = "|"), HabCode_B) ~ "E162",
      grepl("raised bog", phi, ignore.case = TRUE) & grepl(paste0(c("B", "D"), collapse = "|"), HabCode_B) & !grepl("A", HabCode_B) ~ "E162",

      phi == "Reedbeds" & grepl("B", HabCode_B) ~ "F1",
      phi == "Reedbeds" & grepl("D", HabCode_B) ~ "F1",

      phi == "Saline lagoons" & grepl("G", HabCode_B) ~ "G16",

      phi == "Coastal sand dunes" & grepl("Grassland", Term, ignore.case = TRUE) ~ "H65",
      phi == "Coastal sand dunes" & grepl("Heath", Term, ignore.case = TRUE) ~ "H66",
      phi == "Coastal sand dunes" & grepl("Scrub", Term, ignore.case = TRUE) ~ "H67",
      phi == "Coastal sand dunes" & grepl("H", HabCode_B) ~ "H6u",
      phi == "Coastal sand dunes" & grepl(paste0(c("B","J11","J12"), collapse = "|"), HabCode_B) ~ "H6u",
      phi == "Coastal sand dunes" & grepl("D", HabCode_B) ~ "H6u",

      phi == "Coastal vegetated shingle" & grepl("H", HabCode_B) ~ "H3/H5",
      phi == "Coastal vegetated shingle" & grepl(paste0(c("B","J11","J12"), collapse = "|"), HabCode_B) ~ "H3/H5",
      phi == "Coastal vegetated shingle" & grepl("D", HabCode_B) ~ "H3/H5",
      phi == "Coastal vegetated shingle" & grepl("A", HabCode_B) ~ "H3/H5",

      phi == "Mudflats" & grepl("B", HabCode_B) ~ "H11",
      phi == "Mudflats" & grepl("H", HabCode_B) ~ "H11",

      phi == "Coastal saltmarsh" & grepl("H", HabCode_B) ~ "H2u",
      phi == "Coastal saltmarsh" & grepl(paste0(c("B","J11","J12"), collapse = "|"), HabCode_B) ~ "H2u",
      phi == "Coastal saltmarsh" & grepl("D", HabCode_B) ~ "H2u",

      phi == "Maritime cliff and slope" & HabCode_B %in% c("H1u", "I11/H8") ~ "H8",
      phi == "Maritime cliff and slope" & grepl(paste0(c("B","J11","J12"), collapse = "|"), HabCode_B) ~ "H8",
      phi == "Maritime cliff and slope" & grepl("D", HabCode_B) ~ "H8",

      phi == "Limestone pavement" & grepl("I", HabCode_B) ~ "I13",
      phi == "Limestone pavement" & grepl(paste0(c("B","J11","J12"), collapse = "|"), HabCode_B) ~ "I13",
      phi == "Limestone pavement" & grepl("D", HabCode_B) ~ "I13",


      ## Calaminarian grassland is grassland community on heavy metal soils
      phi == "Calaminarian grassland" & grepl(paste0(c("B","J11","J12"), collapse = "|"), HabCode_B) ~ "Ic",
      phi == "Calaminarian grassland" & grepl("I", HabCode_B) ~ "Ic",

      # more general codes (unknown habitat types after rest has been classified)

      phi == "Good quality semi-improved grassland"  & grepl("B", HabCode_B) & !grepl("A", HabCode_B) ~ "Bu2",
      phi == "Good quality semi-improved grassland"  & grepl(paste0(c("J11","J12"), collapse = "|"), HabCode_B) & !grepl("A", HabCode_B) ~ "Bu2",
      phi == "Good quality semi-improved grassland"  & grepl("D", HabCode_B) & !grepl("A", HabCode_B) ~ "Bu1",


      # any grassland that comes after the GQ semi improved is natural (unimproved)
      grepl("grassland", phi, ignore.case = TRUE) & grepl("B", HabCode_B) & !grepl("A", HabCode_B) ~ "Bu1",
      grepl("grassland", phi, ignore.case = TRUE) & grepl("D", HabCode_B) & !grepl("A", HabCode_B) ~ "Bu1",

      TRUE ~ HabCode_B  # a catch-all statement so that non updated values remain (and don't become NA)

      # These may no longer have equivalent
      # phi == BROWN IN TABLE...WHAT BAP HABITAT? & grepl("B", habCode) ~ "J13",
      # phi == BROWN IN TABLE...WHAT BAP HABITAT?& grepl("D", habCode) ~ "J13",
   )
   )

   return(x)
}



# Classification with greenspace ------------------------------------------

#' Create GI column from MasterMap
#'
#' If the GI attribute is missing from basemap (e.g. because there was no OS Greenspace / Open Greenspace coverage for a site), this function creates the attribute based on information from OS Mastermap.

#' @param x A basemap sf object
#' @param params A list of parameters used as thresholds to classify certain habitats. See EcoservR documentation.
#' @return The basemap sf object with new attribute GI
#' @export

create_GI <- function(x, params){
   ## If there is no GI attribute (e.g. there was no OS coverage for the study area), we assign simple GI categories based on MasterMap before classifying. Not very useful for the classification but necessary for access rules.

   if (!"GI" %in% names(x)){

      x <- x %>% dplyr::mutate(GI = dplyr::case_when(

         Make == "Manmade" ~ "Not Greenspace",

         grepl("Foreshore", Term) ~ "Beach Or Foreshore",
         grepl("Agricultural Land", Term) ~ "Undetermined Greenspace",

         Group == "Natural Environment" ~ "Natural",

         Make == "Multiple" & Term == "Multi Surface" & Group == "General Surface" & shp_area < params$gardensize & shp_index < params$gardenshape ~ "Private Garden", # most frequent combination for Private Garden

         Group == "General Surface" & Make == "Natural" ~ "Undetermined Greenspace"

      ))

   }
   return(x)
}



#' Classify Habitats Using OS Greenspace / Open Greenspace
#'
#' Revises habitat classification by considering green infrastructure information. Not meant to be called directly; rather is used conditionally in classify_habitats().

#' @param x A basemap sf object, which must contain the attributes HabCode_B and GI
#' @return The basemap sf object with updated attribute HabCode_B
#' @export

classif_green <- function(x){

   # we'll use these a few times
   amenity_classes <- c("Amenity",
                        "Religious Grounds", "Cemetery",
                        "Public Park Or Garden",
                        "Play Space",
                        "Bowling Green",
                        "Golf Course",
                        "Tennis Court",
                        "Playing Field",
                        "Other Sports Facility",
                        "Institutional Grounds",
                        "School Grounds")

   x <- x %>% dplyr::mutate(HabCode_B = dplyr::case_when(

      GI == "Allotments Or Community Growing Spaces" & grepl("B", HabCode_B) ~ "J11t",

      # TO DO: Coastal does not seem to exist in MM Greenspace, so look for amenity with MM group foreshore?

      GI == "Coastal" & HabCode_B == "B4/J11" ~ "H1u",  # check factor name on full dataset
      GI == "Beach Or Foreshore" & (HabCode_B == "B4/J11" | !grepl("H", HabCode_B)) ~ "H1u",


      GI == "Camping Or Caravan Park" & HabCode_B == "B4/J11" ~ "J34",

      ## We make sure to assign a more natural code than J12 if we have some info on land cover (even if GI is amenity)

      grepl("Agricultural Land", Term) & GI %in% amenity_classes ~ "B4", #NEW
      grepl("Rough Grassland", Term) & GI %in% amenity_classes ~ HabCode_B, #NEW
      grepl(paste0(c("Trees", "Scrub"), collapse = "|"), Term) & GI %in% amenity_classes ~ HabCode_B,


      # all amenity fields, if we haven't found anything better suited
      GI %in% amenity_classes &  (grepl(paste0(c("A3", "B"), collapse = "|"), HabCode_B) | is.na(HabCode_B)) ~ "J12",



      grepl("General Surface", Group) & GI == "Private Garden" ~ "J56",

      # natural or semi natural not needed because better informed by other datasets

      Group == "Unclassified" & GI == "Not Greenspace" ~ "Unclassified, not greenspace",

      Group == "Unclassified" & GI == "Land Use Changing" ~ "Unclassified, area in development",

      TRUE ~ HabCode_B  # a catch-all statement so that non updated values remain

   )
   )
   return(x)
}




# Classification with NFI -------------------------------------------------

#' Classify Habitats Using National Forest Inventory
#'
#' Revises habitat classification of woodland. Currently only adds information for felled woodland. Not meant to be called directly; rather is used conditionally in classify_habitats().

#' @param x A basemap sf object, which must contain the attributes HabCode_B and nfi.
#' @return The basemap sf object with updated attribute HabCode_B
#' @export

classif_nfi <- function(x){

   x <- dplyr::mutate(x,
                      HabCode_B = dplyr::case_when(

                         nfi == "Felled" & grepl("A", HabCode_B) ~ "A4",
                         TRUE ~ HabCode_B

                      ))

   return(x)

}

# Classification with Corine and Crome ------------------------------------

#' Classify Habitats Using CORINE and/or CROME
#'
#' Revises habitat classification of agricultural land to differentiate pastures from arable land. Uses either CORINE Land Cover, Crop Map of England, or (ideally) both. Not meant to be called directly; rather is used conditionally in classify_habitats().

#' @param x A basemap sf object, which must contain the attributes HabCode_B and corine or crome.
#' @return The basemap sf object with updated attribute HabCode_B
#' @export

classif_agri <- function(x){

   # We first try to classify based on both corine and crome if present

   if (!is.null(x$corine) & !is.null(x$crome)){

      x <- dplyr::mutate(x,
                         HabCode_B = dplyr::case_when(


                            ## NEW: forest tracks (rides?) in a plantation tend to get assigned B4/J11 - we use corine and the shape ratio to identify them
                            corine %in% c("Coniferous forest", 'Transitional woodland-shrub', 'Mixed forest') & shp_index > 5 & HabCode_B == "B4/J11" ~ "J512",

                            ## NEW: let's preserve rough grasslands
                            HabCode_B == "Bu" ~ "Bu",
                            HabCode_B == "Bu1" ~ "Bu1",

                            # don't do anything if crome is not an obviously agricultural type
                            crome %in% c("Heathland and Bracken", "Heather", "Perennial Crops and Isolated Trees", "Water", "Trees and Scrubs, short Woody plants, hedgerows", "Unknown or Mixed Vegetation") ~ HabCode_B,

                            HabCode_B == "J12v" ~ "J12v",  # avoid getting road verges mixed in with other J12 (amenity) polygons which could be overridden; verges shouldn't change

                            # pastures, crome and corine in agreement
                            grepl(paste0(c("B", "J11"#, "J12"
                            ), collapse = "|"), HabCode_B) &
                               !grepl("A", HabCode_B) & !grepl("B5", HabCode_B) & # don't allow marshy or stuff with trees (Bu_A etc) to creep in
                               corine == "Pastures" &
                               crome %in% c("Grass", "Non-vegetated or sparsely-vegetated Land") ~ "B4/Bu",

                            # crops, crome and corine in agreement
                            grepl(paste0(c("B", "J11"#, "J12"
                            ), collapse = "|"), HabCode_B) &
                               !grepl("A", HabCode_B) & !grepl("B5", HabCode_B) & # don't allow stuff with trees (Bu_A etc) to creep in
                               grepl(paste0(c("arable land", "crops", "cultivation"), collapse = "|"), corine) &
                               !(crome %in% c("Grass", "Non-vegetated or sparsely-vegetated Land")) ~ "J11",

                            # probably rough grazing (corine is natural)
                            (grepl(paste0(c("B4", "J11"#, "J12"
                            ), collapse = "|"), HabCode_B) | HabCode_B == "B") &
                               corine %in% c("Natural grasslands", "Moors and heathland", "Peat bogs") ~ "Bu2",

                            # pastures in disagreement (crome wins, but only if land already identified as probable agriculture to avoid converting natural grasslands)
                            (grepl(paste0(c("B4", "J11"#, "J12"
                            ), collapse = "|"), HabCode_B) | HabCode_B == "B") &
                               crome == "Grass" ~ "B4",

                            # arable in disagreement (crome wins, but only if land already identified as probable agriculture to avoid converting natural grasslands)
                            (grepl(paste0(c("B4", "J11"#, "J12"
                            ), collapse = "|"), HabCode_B) | HabCode_B == "B") &
                               !(crome %in% c("Grass", "Non-vegetated or sparsely-vegetated Land")) ~ "J11",


                            # if a polygon was unclassified and crome is vegetated or corine is pastures
                            Group == "Unclassified" & Make == "Natural" &
                               (grepl("arable land", "crops", "cultivation") | !crome %in% c("Non-vegetated or sparsely-vegetated Land")) ~ "B4/J11",

                            TRUE ~ HabCode_B
                         ))
   } else
      if (!is.null(x$corine) & is.null(x$crome)){

         # If only CORINE is used, we split between pastures and arable but not as accurate as corine & crome agreeing

         x <- dplyr::mutate(x,
                            HabCode_B = dplyr::case_when(

                               ## do not overwrite anything with a specific greenspace function
                               !GI %in% c("Not Greenspace", "Undetermined Greenspace") & !is.na(GI) ~ HabCode_B,

                               ## NEW: forest tracks in a plantation tend to get assigned B4/J11 - we use corine and the shape ratio to identify them
                               corine %in%  c("Coniferous forest", 'Transitional woodland-shrub', 'Mixed forest')  & shp_index > 5 & HabCode_B == "B4/J11" ~ "J512",

                               # pastures
                               HabCode_B %in% c("B4/J11", "B") & corine == "Pastures" ~ "B4/Bu",

                               # crops
                               HabCode_B %in% c("B4/J11", "B") & grepl("arable_land", corine) ~ "J11",

                               # unclassified things
                               Group == "Unclassified" & grepl("arable_land", corine) ~ "J11",
                               Group == "Unclassified" & grepl("Pastures", corine) ~ "B4/Bu",

                               TRUE ~ HabCode_B
                            ))
      } else if (is.null(x$corine) & !is.null(x$crome)){
         # If only CROME is used, we try to refine the unknown grasslands


         x <- dplyr::mutate(x,
                            HabCode_B = dplyr::case_when(

                               # NEW  don't do anything if crome is not an obviously agricultural type
                               crome %in% c("Heathland and Bracken", "Heather", "Perennial Crops and Isolated Trees", "Water", "Trees and Scrubs, short Woody plants, hedgerows", "Unknown or Mixed Vegetation") ~ HabCode_B,

                               # pastures
                               HabCode_B %in% c("B4/J11", "B") & crome %in% c("Grass", "Non-vegetated or sparsely-vegetated Land") ~ "B4/Bu",

                               # crops
                               HabCode_B %in% c("B4/J11", "B") & !crome %in% c("Grass", "Non-vegetated or sparsely-vegetated Land") ~ "J11",


                               TRUE ~ HabCode_B
                            ))

      }


   # Now if CROME is available we give it the last word in identifying pastures (regardless what CORINE says)

   if (!is.null(x$crome)){
      x <- dplyr::mutate(x,
                         HabCode_B = dplyr::case_when(

                            # NEW don't do anything if crome is not an obviously agricultural type
                            crome %in% c("Heathland and Bracken", "Heather", "Perennial Crops and Isolated Trees", "Water", "Trees and Scrubs, short Woody plants, hedgerows", "Unknown or Mixed Vegetation") ~ HabCode_B,

                            # pastures
                            HabCode_B == "B4/J11" &
                               crome %in% c("Grass", "Non-vegetated or sparsely-vegetated Land") ~ "B4/Bu",

                            # crops
                            HabCode_B == "B4/J11" &
                               !(crome %in% c("Grass", "Non-vegetated or sparsely-vegetated Land")) ~ "J11",

                            TRUE ~ HabCode_B
                         ))
   }

   return(x)
}


# Classification of agri land based on size ------------------------------------

#' Classify Habitats Using Size Criteria
#'
#' Revises habitat classification of agricultural land using the parameters set with the project. Not meant to be called directly; rather is used conditionally in classify_habitats().

#' @param x A basemap sf object
#' @param params A list of parameters used as thresholds to classify certain habitats. See EcoservR documentation.
#' @return The basemap sf object with updated attribute HabCode_B
#' @export

classif_area <- function(x, params){
   x <- x %>% dplyr::mutate(
      HabCode_B = dplyr::case_when(

         # pastures
         HabCode_B %in% c("B4/J11", "B") & shp_area < params$arable_min ~ "B4/Bu",

         # arable land
         HabCode_B %in% c("B4/J11", "B") & shp_area > params$improved_max ~ "J11",

         TRUE ~ HabCode_B
      ))
}



# Classification based on elevation ---------------------------------------

#' Classify Habitats Using Elevation
#'
#' Revises habitat classification of agricultural land and upland habitats using the parameters set with the project. Not meant to be called directly; rather is used conditionally in classify_habitats().

#' @param x A basemap sf object with the "elev" and "slope" attributes
#' @param params A list of parameters used as thresholds to classify certain habitats. See EcoservR documentation.
#' @return The basemap sf object with updated attribute HabCode_B
#' @export

classif_elev <- function(x, params){

   x <- x %>% dplyr::mutate(
      HabCode_B = dplyr::case_when(

         ## Grasslands on steep slopes are more likely to be unimproved
         HabCode_B %in% c("B4/J11", "B", "B4/Bu", "Bu1/Bu2", "Bu", "Bu2", "B4", "J11") &
            slope > params$slope_unimp ~ "Bu1",

         ## Grasslands on moderate slopes are unknown, probably semi-improved
         HabCode_B %in% c("B4/J11", "B", "B4/Bu", "Bu1/Bu2", "Bu", "Bu2", "B4", "B4f", "J11") &
            dplyr::between(slope, params$slope_semi, params$slope_unimp) ~ "Bu",

         ## Heather on steep slopes must be dry
         HabCode_B %in% c("D5_B5/E3/F/H2", "E2/E3/F1") &
            slope > params$slope_dry ~ "Du",

         TRUE ~ HabCode_B)) %>%

      dplyr::mutate(HabCode_B = dplyr::case_when(

         ## Things at high elevations are montane habitats

         grepl("A", HabCode_B) & elev > params$montane ~ "A2m",  # mountane scrub
         grepl("D", HabCode_B) & elev > params$montane ~ "D4",   # mountane heath / shrub

         ## Above a certain elevation (boundary between upland and lowland), we assume there are no crops and most grasslands would be semi-improved (rough grazing)
         # NEW: REVISED TO REDUCE THAT ASSIGNMENT

         HabCode_B %in% c(#"J11",
            "B4/J11",
            "Bu",
            "B4/Bu",
            "B"
            #,"B4"
         ) & elev > params$upland ~ "Bu2",

         TRUE ~ HabCode_B
      )
      )
}




# Classify GI access ------------------------------------------------------

#' Classify Accessibility of Greenspaces
#'
#' Adds a GIpublic attribute indicating whether a greenspace is public, private or has restricted access (only accessible at certain times or to certain groups of people e.g. through membership). Not meant to be called directly; is called in classify_habitats().

#' @param x A basemap sf object with the GI attribute
#' @return The basemap sf object with new attribute GIpublic
#' @export

classif_access <- function(x){

x <- dplyr::mutate(x,
          GIpublic = dplyr::case_when(
             GI %in% c("Amenity", "Undetermined Greenspace") &
                Term == "Agricultural Land" ~ "Private",  # sometimes amenity category assigned to fields, which are probably not public

             GI == "Private Garden" ~ "Private",

             GI == "Land Use Changing" ~ "Private",   # probable construction site/area in development

             GI %in% c("Amenity", "Public Park Or Garden", "Cemetery",
                       "Play Space", "Religious Grounds") ~ "Public",

             GI %in% c("Camping Or Caravan Park", "Bowling Green",
                       "Golf Course", "Tennis Court",
                       "Other Sports Facility", "School Grounds",
                       "Playing Field", "Institutional Grounds",
                       "Allotments Or Community Growing Spaces") ~ "Restricted"
          ))

return(x)
}


