#' Environment Setup
#'
#' Initialization Script
#'
#' This script contains the initialization code for project.
#' It is responsible for setting up the necessary environment and loading
#' any required libraries or data.
#'
#' @author Sebastiano Quintavalle
#' @date 2024-05-15
#' @version 1.0
#' 

# 1. Clear preexisting environment variables
rm(list = ls())

# Specify local source files
SRC_FILES = c(

    "globals.R",
    "utils.R",
    "analysis.R",
    "plot.R",
    "time.R"

)

# Load source files
for (file in SRC_FILES) {
    source(file.path("src", file))
}

# Setup environment
setup_environment()
