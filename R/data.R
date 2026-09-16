#' Sample N2O flux observations from the Svanhovd experiment
#'
#' A real subset of chamber-based N2O flux measurements from two chambers
#' (11 and 13) over the 2022 growing season (May-August), bundled for use in
#' package examples.
#'
#' @format A data frame with 282 rows and 21 variables:
#' \describe{
#'   \item{Plot}{Experimental plot identifier.}
#'   \item{Chamber}{Chamber identifier (factor).}
#'   \item{Fertiliz}{Fertilization level, `"High"` or `"Low"`.}
#'   \item{Timestamp}{Observation time (`POSIXct`).}
#'   \item{Date}{Observation date.}
#'   \item{N2O_flux}{N2O flux (mg N2O m-2 h-1).}
#'   \item{CO2_flux}{CO2 flux (mg CO2 m-2 h-1).}
#'   \item{Soil_Cond}{Soil conductivity.}
#'   \item{Soil_Moist}{Soil moisture (fraction).}
#'   \item{Soil_T_2, Soil_T_5, Soil_T_75}{Soil temperature at 2/5/75 cm depth (deg C).}
#'   \item{Precip}{Precipitation (mm).}
#'   \item{VPD}{Vapor pressure deficit.}
#'   \item{DOY}{Day of year.}
#'   \item{D_harvest, D_fertiliz}{Days since last harvest/fertilization.}
#'   \item{fert_dose, fert_ef}{Fertilization dose and its decayed effect.}
#'   \item{hrs_freeze}{Consecutive hours of sub-zero soil temperature.}
#'   \item{y_bin}{Existing MAD-based hot-moment label (0/1).}
#' }
#' @source Svanhovd N2O flux experiment (NIBIO); subset of
#'   `df_N2O_for_training.RData`.
"n2o_sample"
