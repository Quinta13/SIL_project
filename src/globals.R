#' Settings
#'
#' This file contains global settings for the project.
#'
#' @author Sebastiano Quintavalle
#' @date 2024-05-15
#' @version 1.0
#'

# --- PACKAGES ---

REQUIREMENTS <- c(
    "ggplot2",  "reshape2",    "scales", 
    "car",      "leaps",       "effects", 
    "grid",     "gridExtra",   "RColorBrewer", 
    "tidyr",    "dplyr",       "kableExtra",
    "glmnet",   "glmnetUtils", "chron",
    "corrplot", "tsoutliers",  "forecast",
    "lubridate"
)


# --- FILE PATHS ---

PATHS <- list(
    Shootings      = "data/shootings.csv",        # https://data.cityofnewyork.us/Public-Safety/NYPD-Shooting-Incident-Data-Year-To-Date-/5ucz-vwe8/about_data
    Arrests        = "data/arrests.csv",          # https://data.cityofnewyork.us/Public-Safety/NYPD-Arrest-Data-Year-to-Date-/uip8-fykc/about_data
    NycMonthCrimes = "data/nyc_month_crimes.csv"  # 0_data_preprocessing.Rmd
)


# --- GLOBAL VARIABLES ---

SEED    <- 123            # Pseudo random number generator seed
LANG    <- "en_US.UTF-8"  # Language


# --- DATA ---

YEAR_TEST <- c(2022, 2023)  # Year to use as test set


# --- COLORS ---

PALETTE <- list(

    boroughs = list(
        Bronx        = "#e33f1add",
        Brooklyn     = "#1f78b4dd",
        Manhattan    = "#33a02cdd",
        Queens       = "#6a3d9add",
        StatenIsland = "#ff7f00dd"
    ),

    pca_histo = c(
        "#0076b6ee", "#0096c7ee", "#00b4d8ee", 
        "#48cae4ee", "#90e0efee", "#ade8f4ee"
    ),

    pca_biplot = c("#0e426c", "#ff3c00d6"),

    crimes = c(
        "#ff5722ee", "#ff9800ee", "#ffc107ee", "#ffeb3bee", 
        "#cddc39ee", "#8bc34aee", "#4caf50ee", "#009688ee", 
        "#00bcd4ee", "#03a9f4ee", "#2196f3ee", "#3f51b5ee", 
        "#637ab7ee", "#9c27b0ee", "#e81e63ee", "#f44336ee",
        "#540404ee"
    ),

    outliers = list(
        ts  = "#000000",
        old = "#af1818ee",
        new = "#53e953ee"
    )

)

