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
ALPHA_GLMNET <- c(0, 1 - 0.1, 1)  # Elastic net mixing parameter grid

YEAR_TEST <- c(2022, 2023)  # Year to use as test set

CRIME_NAMES <- c(
    "Assault",
    "Burglary",
    "DrugParaphernalia",
    "LicensingFirearm",
    "Larceny",
    "Mischief",
    "MarijuanaPossession",
    "Murder",
    "Robbery",
    "SubstancePossession",
    "Theft",
    "WeaponPossession"
)

# Aggregation
CRIME_FAMILIES <- list(
    "Offense"     = c("Assault", "Mischief", "Robbery"),
    "DrugRelated" = c("MarijuanaPossession", "SubstancePossession", "DrugParaphernalia"),
    "Drop"        = c("Larceny", "WeaponPossession")
)

# --- COLORS ---

ALPHA_CH <- 0.25  # Alpha channel for the colors

PALETTE <- list(

    boroughs = list(
        Manhattan    = "#33a02cdd",
        Queens       = "#6a3d9add",
        Bronx        = "#e33f1add",
        Brooklyn     = "#1f78b4dd",
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
        "#FF0000", "#FF7600", "#FFEB00",
        "#9DFF00", "#158f00", "#00FFC4", 
        "#00C4FF", "#00458f", "#9D00FF", 
        "#FF00EB", "#FF0076", "#520404",
        "#766242"
    ),

    outliers = list(
        ts  = "#000000",
        new = "#af1818ee",
        old = "#53e953ee"
    )

)
