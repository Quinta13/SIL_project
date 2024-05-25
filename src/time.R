#' Time utils
#'
#' This file contains utility functions for working with time series data.
#'
#' @author Sebastiano Quintavalle
#' @date 2024-05-15
#' @version 1.0
#'


# --- TYPE CONVERSION ---


#' Convert a data frame column to a monthly time series object
#'
#' This function takes a data frame and a column name as input and converts the column into a monthly time series object.
#' The resulting time series object has a frequency of 12, representing monthly data.
#'
#' @param df_ The data frame containing the column to be converted
#' @param y The name of the column to be converted
#' @return A monthly time series object
#' 
#' @examples
#' df <- data.frame(Year = c(2010, 2010, 2011, 2011),
#'                  Month = c(1, 2, 1, 2),
#'                  Value = c(10, 20, 30, 40))
#' ts_obj <- df_to_month_ts(df, "Value")
#' print(ts_obj)
#' 
#' @export df_to_month_ts
#' 
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


# --- PLOTTING UTILS ---


#' Plot Multiple Time Series
#'
#' This function plots multiple time series on the same plot.
#'
#' @param ts_list A list of time series objects.
#' @param title The title of the plot.
#' @param ylab The label for the y-axis.
#' @param lwd The line width for the time series plots. Default is 1.
#' @param lty The line type for the time series plots. Default is "solid".
#' @param legend_pos The position of the legend on the plot. Default is "topleft".
#'
#' @return None
#'
#' @examples
#' # Create some time series objects
#' ts1 <- ts(rnorm(100), start = 2000)
#' ts2 <- ts(rnorm(100), start = 2000)
#' ts3 <- ts(rnorm(100), start = 2000)
#'
#' # Plot the time series
#' plot_multiple_ts(list(ts1, ts2, ts3), "Multiple Time Series", "Value")
plot_multiple_ts <- function(
    ts_list,
    title, 
    ylab, 
    lwd        = 1, 
    lty        = "solid", 
    legend_pos = "topleft"
) {

    # Plot the first time series
    plot(
        ts_list[[names(ts_list)[1]]],
        ylim = range(unlist(ts_list)),
        type="l", col=palette()[1],
        main=title, 
        xlab="Year", ylab=ylab, 
        lwd=lwd, lty=lty
    )
    grid()

    # Add the remaining time series to the plot
    for(i in 2:length(ts_list)) {
        i_name = names(ts_list)[i]
        lines(
            ts_list[[i_name]], type="l", 
            lwd=lwd, lty=lty, 
            col=get_palette_color(i)
        )
    }

    # Add legend
    legend(
        legend_pos, 
        legend=names(ts_list), 
        cex = 0.6,
        lty=lty, 
        lwd=lwd,
        col=palette()[1:length(ts_list)]
    )

}


#' Plot Monthly Time Series
#'
#' This function plots a monthly time series from a data frame.
#'
#' @param df_ The data frame containing the time series data.
#' @param y The column name of the time series variable in the data frame.
#' @param title The title of the plot (optional).
#' @param y_lab The label for the y-axis (optional, default is "Count").
#' @param x_ticks The number of x-axis ticks to display (optional, default is 1).
#' 
#' @return None
#' 
#' @examples
#' 
#' df <- data.frame(
#'  Year = rep(2019, 12),
#'  Month = 1:12,
#'  Shootings = c(10, 12, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)
#' )
#' 
#' plot_month_ts(df, "Shootings", "Monthly Shootings")
#'
#' @export plot_month_ts
#'
plot_month_ts <- function(
    df_, y, 
    title   = "", 
    y_lab   = "Count",
    x_ticks = 1
) {
    
    # Define the time series object
    month_ts <- df_to_month_ts(df_, y=y)

    # Plot the time series, avoiding the default x-axis labels
    plot(
        month_ts, 
        xlab = "Year", ylab = paste(y, "Count"),
        main = title,  xaxt = "n"
    )

    # Add custom x-axis labels
    axis(
        side = 1, 
        at = seq(min(df_$Year)-1, max(df_$Year) + 1, x_ticks)
    )

    # Add a grid
    grid()
    
}


# --- TIME SERIES ANALYSIS ---


#' Plot Cross-Correlation Function (CCF) for Features
#'
#' This function plots the Cross-Correlation Function (CCF) between each feature
#' in a given data frame and a target variable.
#'
#' @param df_ The data frame containing the features and the target variable.
#' @param features A character vector specifying the names of the features to plot.
#' @param y The name of the target variable.
#' @param lag.max The maximum lag to consider in the CCF calculation.
#' @param cex.main The font size for the main title of each plot.
#'
#' @return None (plots are displayed on the screen).
#'
#' @examples
#' # Assuming df_ is a data frame with columns "feature1", "feature2", and "target"
#' plot_feature_ccf(df_, c("feature1", "feature2"), "target", lag.max = 36, cex.main = 0.8)
#'
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


#' Plot Cross-Correlation Function (CCF) Grid
#'
#' This function plots the cross-correlation function (CCF) grid for a given data frame.
#'
#' @param df The data frame containing the data.
#' @param y The variable of interest.
#' @param level The grouping variable.
#'
#' @return None
#'
#' @examples
#' df <- data.frame(
#'   x = rnorm(100),
#'   y = rnorm(100),
#'   group = rep(c("A", "B"), 50)
#' )
#' plot_ccf_grid(df, "y", "group")
#'
#' @export
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


#' Plot the STL decomposition of a time series
#'
#' This function takes a data frame and a column name representing a time series,
#' and plots the seasonal, trend, and residual components of the time series using
#' the STL (Seasonal and Trend decomposition using Loess) method.
#'
#' @param df_ The data frame containing the time series data.
#' @param y The column name representing the time series.
#' @param title The title for the plot (optional).
#'
#' @return None
#' 
#' @examples
#' df <- data.frame(
#'  Year = rep(2019, 12),
#'  Month = 1:12,
#'  Shootings = c(10, 12, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)
#' )
#' 
#' plot_ts_stl_decomposition(df, "Shootings", "STL Decomposition of Shootings")
#' 
#' @export plot_ts_stl_decomposition
#'
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