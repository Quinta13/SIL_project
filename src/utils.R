# --- ENVIRONMENT SETUP ---

#' Load Required Packages
#'
#' This function checks if the required packages are installed and loads them.
#'
#' @param requirements A character vector specifying the names of the required packages.
#'
#' @return None
#' @export
#'
#' @examples
#' load_requirements(c("dplyr", "ggplot2"))
#'
load_requirements <- function(requirements) {

    for (req in requirements){

        # If the package is not installed, install it
        if (!require(req, character.only = TRUE)){
            print(paste("Package", req, "not found. Installing..."))
            install.packages(req)
        }

        # Load the package
        library(req, character.only = TRUE)

    }
}


#' Setup Environment function
#'
#' This function sets up the environment for the project by loading required packages,
#' closing all open devices, setting the seed for reproducibility, and setting the local language.
#'
#' @keywords setup environment
#' @return None
#' @export
#'
#' @examples
#' setup_environment()
#'
setup_environment <- function() {

    # Load packages
    load_requirements(REQUIREMENTS)

    # Close all open devices
    if (!is.null(dev.list())) dev.off()

    # Set seed for reproducibility
    set.seed(SEED)

    # Set locale language
    Sys.setlocale("LC_TIME", LANG)
    
}

# --- DATA PREPARATION ---

#' Prepare crimes data frame
#'
#' This function prepares the crimes data frame by converting the "Borough" column to a factor
#' and setting the order of the levels. It also adds a discrete variant of the "Month" column
#' by converting it to a factor and reordering the levels by month.
#'
#' @param crimes_df The input data frame containing crime data.
#'
#' @return The modified crimes data frame.
#'
#' @examples
#' crimes <- prepare_crimes(crimes_data)
#' head(crimes)
#'
#' @export
prepare_crimes <- function(crimes_df, col_merge = TRUE) {

    # Cast Borough to factor and set their order
    crimes_df$Borough <- factor(crimes_df$Borough, levels = LEVELS_ORDER)

    # Add a discrete variant of the Month 
    crimes_df$MonthName <- as.factor(month.abb[crimes_df$Month])
    crimes_df$MonthName <-    factor(crimes_df$MonthName, levels = month.abb) # reorder levels by month

    # Merge the two CrimeTypes related to Substance possession
    if(col_merge) {
        # Union of Marijuana and Other Drugs
        crimes_df$SubstancePossession <- 
            crimes_df$MarijuanaSellPossession + 
            crimes_df$SubstancePossession
        crimes_df$MarijuanaSellPossession <- NULL
    }

    # Use arrests in previous month as predictors
    first.year  <- crimes_df$Year[1]
    first.month <- crimes_df$Month[1]

    # Loop through each column and create a new shifted column
    for (col in c(CRIME_NAMES, "TotArrests")) {
        crimes_df[[col]] <- c(
            NA, crimes_df[[col]][-length(crimes_df[[col]])]
        )
    }

    # Remove the first date
    crimes_df <- crimes_df[
        !(crimes_df$Year == first.year & crimes_df$Month == first.month), 
    ]

    return(crimes_df)

}


#' Split a dataframe into train and test sets based on a given year
#'
#' This function takes a dataframe and a vector of year as input and splits the dataframe into two sets:
#' a train set and a test set. The test set contains all the rows with the specified year,
#' while the train set contains all the remaining rows.
#'
#' @param df_ The input dataframe to be split
#' @param year_test The year to be used for creating the test set
#'
#' @return A list containing the train and test sets
#' @export
train_test_split_by_year <- function(df_, year_test) {

    # Check if year_test is in df_$Year
    if (!any(year_test %in% df_$Year)) {
        stop(paste("Years", year_test, "don't match with the dataframe ones. "))
    }

    # Filter by year
    df_test <- df_[ (df_$Year %in% year_test), ]
    df_     <- df_[!(df_$Year %in% year_test), ]

    # Reset rows
    rownames(df_)     <- NULL
    rownames(df_test) <- NULL

    return (
        list(
            train = df_,
            test  = df_test
        )
    )

}

split_statenisland <- function(crimes, crimes_test) {

    # Split
    si_crimes      <- crimes     [crimes     $Borough == "StatenIsland", ]
    si_crimes_test <- crimes_test[crimes_test$Borough == "StatenIsland", ]
    crimes         <- crimes     [crimes     $Borough != "StatenIsland", ]
    crimes_test    <- crimes_test[crimes_test$Borough != "StatenIsland", ]

    # Reset indexes
    rownames(si_crimes)      <- NULL
    rownames(si_crimes_test) <- NULL
    rownames(crimes)         <- NULL
    rownames(crimes_test)    <- NULL

    # Adjust Borough levels
    si_crimes     $Borough <- NULL
    si_crimes_test$Borough <- NULL
    crimes        $Borough <- droplevels(crimes     $Borough)
    crimes_test   $Borough <- droplevels(crimes_test$Borough)

    return(
        list(
            si_crimes      = si_crimes,
            si_crimes_test = si_crimes_test,
            crimes         = crimes,
            crimes_test    = crimes_test
        )
    )
}

# --- MISC ---

remove_alpha_channel <- function(colors) {
    return(
        unname(sapply(
            colors, 
            function(color) substr(color, 1, 7)
        ))
    )
}

# Function to extract legend
extract_legend <- function(plot) {

    g <- ggplotGrob(plot)
    legend <- g$grobs[[which(sapply(g$grobs, function(x) x$name) == "guide-box")]]
    return(legend)

}

replace_outliers <- function(
    df_, 
    y,
    outliers, 
    level
) {

    if(nrow(outliers) == 0) return(df_)

for (i in 1:nrow(outliers)) {

    time <- outliers[i, "Time"]
    year  <- as.integer(format(time, "%Y"))
    month <- as.integer(format(time, "%m"))
    new_value <- outliers[i, "Value.New"]

    df_[
        df_$Borough == level &
        df_$Year    == year &
        df_$Month   == month
    ,][[y]] <- c(new_value)
}

return(df_)

}

get_epoch <- function(year, month) {
    (year - min(crimes$Year)) * 12 + month
}

knit_table <- function(table) {

    knitr::kable(table, format = "html") %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

}
