#' Settings
#'
#' This file contains global settings for the project.
#'
#' @author Sebastiano Quintavalle
#' @date 2024-05-15
#' @version 1.0
#'


# Requirements
REQUIREMENTS <- c("car", "leaps", "RColorBrewer", "glmnet", "effects")

# File paths - define the paths to the data files
FILE_PATHS <- list(
    Shootings        = "data/shootings.csv",        # https://data.cityofnewyork.us/Public-Safety/NYPD-Shooting-Incident-Data-Year-To-Date-/5ucz-vwe8/about_data
    Arrests          = "data/arrests.csv",          # https://data.cityofnewyork.us/Public-Safety/NYPD-Arrest-Data-Year-to-Date-/uip8-fykc/about_data
    NycMonthCrimes   = "data/nyc_month_crimes.csv"  # Preprocessing output file
)

# Globals
SEED    <- 123            # Pseudo random number generator seed
LANG    <- "en_US.UTF-8"  # Language

# Testing years
YEAR_TEST <- c(2022, 2023)

# Palettes
PALETTE <- list(

    boroughs = c(
        "#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a"
    ),
    
    vibrant = c(
        "#FF5733", "#0bb52a", "#1D3CCA", "#80059e", "#d519c9",
        "#FF8C33", "#4eff9b", "#2d0562", "#ff9999", "#08750b",
        "#1c012b", "#31e4ff", "#FF338C", "#A8FF33", "#FF5733",
        "#6a0000", "#748dff", "#ff3366", "#211d1d", "#713131"
    )
)
