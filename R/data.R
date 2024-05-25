#' Lookup Table for CORINE Vector Data
#'
#' A lookup vector to match the CORINE land use type description (values) to the numeric code (names) used in the CORINE Land Cover 2018 vector dataset.
#'
#' @format A named vector with 47 land use types
#' @source \url{https://land.copernicus.eu/pan-european/corine-land-cover/clc2018}
"corine_lookup"


#' Lookup Table for CORINE Raster Data
#'
#' A lookup table to match the CORINE land use type description to the numeric code used in the CORINE Land Cover 2018 raster dataset.
#'
#' @format A data frame with 45 rows and 2 variables:
#' \describe{
#'   \item{numcode}{numeric value found in CLC raster data}
#'   \item{desc}{land use text description to match to numcode}
#' }
#' @source \url{https://land.copernicus.eu/pan-european/corine-land-cover/clc2018}
"corine_lookup_raster"


#' EcoservR Lookup Table
#'
#' A lookup table to add information to the environmental baseline produced by EcoservR, based on the "HabCode_B" field. This is the main table used to assign scores to habitats within the ecosystem service models.
#'
#' @format A data frame with XX rows and YY variables.
#' \describe{
#'   \item{Ph1code}{EcoservR habitat code to match to the HabCode_B field in the baseline}
#'   \item{HabNmPLUS}{Expanded description of habitat type}
#'   \item{HabBroad}{Broad habitat type}
#'   \item{HabClass}{General habitat class}
#'   \item{TotCarb}{Average carbon storage capacity for a given habitat (tons per hectare)}
#'   \item{AirPurScore}{Air purification capacity for a given habitat (0-100)}
#'   \item{Noise}{Noise regulation capacity for a given habitat (0-100)}
#'   \item{Rough}{Manning's surface roughness coefficient for a given habitat}
#'   \item{Naturalness}{Perceived naturalness for a given habitat (0-10)}
#' }
"hab_lookup"


