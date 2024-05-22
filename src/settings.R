#' Settings
#'
#' This file contains global settings for the project.
#'
#' @author Sebastiano Quintavalle
#' @date 2024-05-15
#' @version 1.0
#'


# File paths - define the paths to the data files
FILE_PATHS <- list(
    Shootings        = "data/shootings.csv",        # https://data.cityofnewyork.us/Public-Safety/NYPD-Shooting-Incident-Data-Year-To-Date-/5ucz-vwe8/about_data
    Arrests          = "data/arrests.csv",          # https://data.cityofnewyork.us/Public-Safety/NYPD-Arrest-Data-Year-to-Date-/uip8-fykc/about_data
    NycMonthCrimes   = "data/nyc_month_crimes.csv"  # Preprocessing output file
)

# Testing years
YEAR_TEST <- c(2022, 2023)

# Style
PALETTE = "Set2"  # https://r-graph-gallery.com/38-rcolorbrewers-palettes.html
