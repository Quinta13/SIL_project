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
    "car",          "chron",        "corrplot", 
    "dplyr",        "effects",      "factoextra", 
    "forecast",     "ggplot2",      "glmnet", 
    "glmnetUtils",  "grid",         "gridExtra", 
    "kableExtra",   "leaps",        "lubridate", 
    "Matrix",       "MASS",         "mgcv",
    "performance",  "RColorBrewer", "reshape2",  
    "scales",       "tidyr",        "tsoutliers"
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

LEVELS_ORDER <- c(
    "Manhattan",
    "Queens",
    "Bronx",
    "Brooklyn",
    "StatenIsland"
)

LEVEL <- 0.95  # Confidence level for the prediction intervals

ALPHA <- 1.    # Elastic net mixing parameter default

YEAR_TEST <- c(2022, 2023)  # Year to use as test set

CRIME_NAMES <- c(
    "Assault",
    "Burglary",
    "DrugParaphernalia",
    "LicensingFirearm",
    "Forgery",
    "Larceny",
    "Mischief",
    "MarijuanaPossession",
    "Murder",
    "Robbery",
    "SubstancePossession",
    "Theft",
    "Trespass",
    "WeaponPossession"
)

# Aggregation

CRIME_FAMILIES <- list(
    "IllegalPossesion" = c("MarijuanaPossession", "SubstancePossession", "WeaponPossession"),
    "AgainstProperty"  = c("Robbery", "Theft", "Trespass"),
    "Offense"          = c("Assault", "Mischief"),
    "Drop"             = c("Larceny")
)

# --- COLORS ---

ALPHA_CH <- 0.25  # Alpha channel for the colors

PALETTE <- list(

    boroughs = list(
        Bronx        = "#e33f1add",
        Brooklyn     = "#1f78b4dd",
        Manhattan    = "#33a02cdd",
        Queens       = "#6a3d9add",
        StatenIsland = "#ff7f00dd"
    ),

    pca = list(
        line  = "#03016b",
        histo = c(
            "#0076b6", "#0096c7", "#00b4d8", 
            "#48cae4", "#90e0ef", "#ade8f4"
        ),
        gradient = c(
            "#00AFBB", "#E7B800", "#FC4E07"
        ),
        biplot = list(
            ind = "#696969",
            var = "#2E9FDF"
        )
    ),

    crimes = c(
        "#ff5722ee", "#ff9800ee", "#ffc107ee", "#ffeb3bee", 
        "#cddc39ee", "#8bc34aee", "#4caf50ee", "#009688ee", 
        "#00bcd4ee", "#03a9f4ee", "#2196f3ee", "#3f51b5ee", 
        "#637ab7ee", "#9c27b0ee", "#e81e63ee", "#540404ee"
    ),

    outliers = list(
        ts  = "#000000",
        new = "#af1818ee",
        old = "#53e953ee"
    )

)
