# --- ENVIRONMENT SETUP ---

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

prepare_crimes <- function(crimes_df, use_families = TRUE) {

    # 1. Cast Borough as factor
    crimes_df$Borough <- factor(crimes_df$Borough, levels = LEVELS_ORDER)

    # 2. Create Month factor column
    crimes_df$MonthName <- as.factor(month.abb[crimes_df$Month])
    crimes_df$MonthName <-    factor(crimes_df$MonthName, levels = month.abb) # reorder levels by month

    # 3. Merge column by family
    if(use_families) {
        
        for(fam_name in names(CRIME_FAMILIES)) {

            cols <- CRIME_FAMILIES[[fam_name]]

            # Sum arrests in family column
            if(length(cols) == 1) { crimes_df[[fam_name]] <- crimes_df[[cols[1]]]       } 
            else                  { crimes_df[[fam_name]] <- rowSums(crimes_df[, cols]) }

            # Delete original columns
            for (col in cols) crimes_df[[col]] <- NULL

            # Mofify the crime names in the light of family aggregations
            CRIME_NAMES <- c(CRIME_NAMES, fam_name)
            CRIME_NAMES <- CRIME_NAMES[!CRIME_NAMES %in% cols]

        }

        # Drop columns indicated in the Drop family
        crimes_df$Drop <- NULL
        CRIME_NAMES <- CRIME_NAMES[!CRIME_NAMES %in% "Drop"]
    
    }

    # 4. Shift arrests columns by one 
    first.year  <- crimes_df$Year [1]
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

    # 5. Add epoch column
    crimes_df$T <- get_epoch(crimes_df$Year, crimes_df$Month)

    # 6. Add sin and cos columns
    crimes_df$CosT <- round(cos(2 * pi * crimes_df$T / 12), 5)
    crimes_df$SinT <- round(sin(2 * pi * crimes_df$T / 12), 5)

    # Rows and columns postprocessing
    rownames(crimes_df) <- NULL
    crimes_df           <- crimes_df[, c("Borough", "Year", "T", "CosT", "SinT", "Month", "MonthName", "Shootings" , CRIME_NAMES, "TotArrests")]

    return(list(
        df=crimes_df,
        crime_names=CRIME_NAMES
    ))

}


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


prepare_outliers <- function(crimes_df) {

    for(y in c("Shootings", CRIME_NAMES)){

        for(borough in levels(crimes_df$Borough)) {

            outliers_out <- outliers_diagnostic(
                df_ = crimes_df[crimes_df$Borough == borough,],
                y = y,
                title = borough,
                ylab = "Shootings incidents",
                colors = PALETTE$outliers
            )    

            crimes_df <- replace_outliers(
                df_=crimes_df,
                y = y,
                outliers = outliers_out$outliers,
                level=borough
            )

        }
    }

    crimes_df$TotArrests <- rowSums(crimes_df[, CRIME_NAMES])

    return(crimes_df)

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

    first.year <- year[1]
    first.month <- month[1]

    (year - first.year) * 12 + (month +1 - first.year) %% 12 - 1

}

knit_table <- function(table) {

    knitr::kable(table, format = "html") %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

}

zero_to_point <-  function(df_) {
    df_ <- as.matrix(df_)
    df_[df_ == 0] <- "."
    df_ <- as.data.frame(df_, stringsAsFactors = FALSE)
    return(df_)
}
