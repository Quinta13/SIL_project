#' Time utils
#'
#' This file contains utility functions for working with time series data.
#'
#' @author Sebastiano Quintavalle
#' @date 2024-05-15
#' @version 1.0
#'


# --- CONVERSION ---


df_to_month_ts <- function(df_, y) {
    
    # Define the time series object
    
    return (
        ts(
            df_[[y]], 
            start = c(df_$Year[1], df_$Month[1]), 
            frequency = 12
        )
    )
    
}

reshape_postprocess <- function(df_, id.vars) {

    # Post process
    df_ <- data.frame(df_)
    colnames(df_) <- gsub("X", "", colnames(df_)) 

    # Ts conversion
    df_ts <- melt(
        df_, 
        id.vars = id.vars, 
        variable.name = "YearMonth",
        value.name = "Count"
    )

    df_ts$YearMonth <- as.Date(
        paste0(df_ts$YearMonth, "01"), 
        format = "%Y%b%d"
    )

    return(df_ts)

}

ts_crimes_reshape <- function(df_) {

    # Pivot to long format
    df_long <- df_ %>%
        pivot_longer(
            cols = -c(MonthName, Year), 
            names_to = "CrimeType", 
            values_to = "Count"
        )
    
    # Combine Year and MonthName into a single column
    df_long <- df_long %>%
        mutate(
            YearMonth = paste(formatC(Year, width = 4, flag = "0"), MonthName, sep = ""))

    # Sort by Year and MonthName
    df_long <- df_long %>%
        arrange(Year, MonthName)

    # Pivot back to wide format
    df_wide <- df_long %>%
        select(-MonthName, -Year) %>%
        pivot_wider(names_from = YearMonth, values_from = Count)  
    
    return(reshape_postprocess(df_=df_wide, id.vars="CrimeType"))
    
}

ts_borough_reshape <- function(df_, crime_type) {

    # Pivot to long format
    df_long <- df_ %>%
        mutate(MonthName = factor(MonthName, levels = month.abb)) %>%
        mutate(YearMonth = paste(formatC(Year, width = 4, flag = "0"), MonthName, sep = "")) %>%
        arrange(Borough, Year, MonthName)

    # Pivot back to wide format
    df_wide <- df_long %>%
        select(-MonthName, -Year) %>%
        pivot_wider(names_from = YearMonth, values_from = !!crime_type)

    return(reshape_postprocess(df_=df_wide, id.vars="Borough"))

}

fractional_year_to_date <- function(fractional_year) {
        # Separate the year and the fractional part
        year <- floor(fractional_year)
        fraction <- fractional_year - year
        
        # Convert the fractional part to days (approximate)
        days_in_year <- 365
        day_of_year <- round(fraction * days_in_year)
        
        # Construct the date
        date <- ymd(paste0(year, "-01-01")) + days(day_of_year)
        
        return(date)
    }


# --- TIME SERIES ANALYSIS ---


plot_feature_ccf <- function(
    df_,
    features,
    y,
    lag.max = 36,
    cex.main = 0.8
) {

    # Plot the CCF for each feature
    for (x in features) {

        # Plot the CCF
        ccf(
            x = df_[[x]],
            y = df_[[y]],
            lag.max = lag.max,
            ylab = "CCF",
            main = ""
        )

        # Add title with font size option
        title(
            main = paste("CCF -", x, "vs", y),
            cex.main = cex.main
        )

    }
}

plot_ccf_grid <- function(
    df,
    y,
    level
) {

    # Extract levels
    levels     = unique(df[[level]])
    n_elements = length(levels)

    par(mfrow=c(n_elements, n_elements))

    for(i in 1:n_elements) {

        df1   <- df[df[[level]] == levels[i], ]

        for(j in 1:4) {

            df2   <- df[df[[level]] == levels[j], ]

            # In the case it's computed on the same data, it's the ACF
            if (i == j) {
                title_ = paste(levels[i], y, "- ACF")
                y_lab_ = "ACF"
            # Otherwise CCF
            } else {
                title_ = paste(levels[i], "&", levels[j], y, "- PACF")
                y_lab_ = "CCF"
            }

            ccf(
                df1[[y]], df2[[y]], 
                xlab = "Lag", 
                ylab = y_lab_,
                main = title_
            )
        }
    }

    par(mfrow=c(1, 1))

}


plot_ts_stl_decomposition <- function(
    df_, y, 
    title = ""
) {
    
    # Define the time series object
    month_ts <- df_to_month_ts(df_, y=y)

    # Plot the stl decomposition
    plot(stl(month_ts, s.window = "periodic"), main = title)
    grid()
    
}


outliers_diagnostic <- function(df_, colors, y, title="", ylab="") {

  # Plot the original time series
  p <- plot_ts(
    ts_crimes_reshape(
        df_=df_[, c("Year", "MonthName", y)]
    ),
    group = "CrimeType",
    y = y,
    col = colors$ts,
    ylab = "Shootings",
    title = title,
    lwd = 0.7,
    ticks  =5
  )

  ts <- df_to_month_ts(df_, y=y)
  
  # Extract outliers index and times 
  outliers <- tsoutliers(ts)
  idx      <- outliers$index
  times    <- time(ts)[idx]
  
  # Extract old and new observations
  old  <- ts[idx]
  new_ <- as.integer(outliers$replacements)
  

  df_outliers <- data.frame(
    Time = as.Date(rep(unlist((
        lapply(times, fractional_year_to_date)
    )), 2)),
    Value = c(old, new_),
    Label = c(rep("Old", length(old)), rep("New", length(new_)))
  )

    
    p <- p + geom_point(
        data = df_outliers,
        aes(x = Time, y = Value, color = Label),
        size = 2
    ) +
        scale_color_manual(values=c(colors$old, colors$new))

    df_outliers <- reshape(
        df_outliers,
        idvar = "Time", timevar = "Label", 
        direction = "wide"
    )

    return(list(
        plot = p,
        outliers = df_outliers
    ))

}
