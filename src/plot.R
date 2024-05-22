#' Plotting Functions
#'
#' This file contains generic plotting functions for data visualization.
#'
#' @author Sebastiano Quintavalle
#' @date 2024-05-15
#' @version 1.0
#'


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

#' Plot Cross-Correlation Function
#'
#' This function plots the cross-correlation function between two data frames.
#'
#' @param df1 The first data frame.
#' @param df2 The second data frame.
#' @param y The column name to be used for cross-correlation.
#' @param title The title of the plot (default is an empty string).
#' @param y_lab The label for the y-axis (default is "CCF").
#'
#' @return None
#'
#' @examples
#' df1 <- data.frame(x = 1:10, y = 11:20)
#' df2 <- data.frame(x = 1:10, y = 21:30)
#' plot_ccf(df1, df2, "y", "Cross-Correlation Plot", "Correlation")
#'
#' @export plot_ccf
plot_ccf <- function(
    df1, df2, y, 
    title = "",
    y_lab = "CCF"
) {
    
    # Plot the cross-correlation function
    ccf(
        df1[[y]], df2[[y]], 
        xlab = "Lag", 
        ylab = "CCF",
        main = title
    )
    
}



#' Create a scatterplot for a given dataframe
#'
#' This function creates a scatterplot using the specified x and y variables from a dataframe.
#' It also provides options to add jitter to the x-axis, customize the title, and position the legend.
#'
#' @param df_ The input dataframe.
#' @param x The name of the variable to be plotted on the x-axis.
#' @param y The name of the variable to be plotted on the y-axis.
#' @param title The title of the scatterplot (default is an empty string).
#' @param jitter_ A logical value indicating whether to add jitter to the x-axis (default is FALSE).
#' @param legend_pos The position of the legend in the plot (default is "topright").
#'
#' @return None
#'
#' @examples
#' # Example usage:
#' borough_scatterplot(df, "x_variable", "y_variable", title = "Scatterplot", jitter_ = TRUE, legend_pos = "bottomright")
#'
#' @export borough_scatterplot
borough_scatterplot <- function(
    df_, x, y, 
    title      = "", 
    jitter_    = FALSE, 
    legend_pos = "topright"
) {
    
    # Define palette
    palette(brewer.pal(n = length(unique(df_$Borough)), name = PALETTE))

    # Add jitter to the x-axis if needed
    x_ = df_[[x]] 
    if (jitter_) { x_ <- jitter(df_[[x]]) }
    
    # Create the scatterplot
    plot(
        x = x_,
        y = df_[[y]],
        col  = df_$Borough,
        xlab = x,   ylab = y,
        pch  = 16,  main = title,
    )

    # Add a legend
    legend(
        legend_pos,
        legend = levels(df_$Borough),
        col    = 1:length(levels(df_$Borough)),
        pch    = 16
    )
    
}

#' Subset Regression Information
#'
#' This function takes a subset regression model as input and plots the summary information.
#' It extracts the summary information using the `summary` function and plots four different
#' metrics: Rsq, AdjRSq, AIC, and BIC. For each metric, it plots the values against the number
#' of variables and highlights the best value with a red point. The function uses the 
#' `which.max` and `which.min` functions to find the index of the best value for each metric.
#'
#' @param subset_reg A subset regression model object.
#'
#' @return None
#'
#' @examples
#' subset_regression_info(subset_reg)
#'
subset_regression_info <- function(subset_reg) {
    # Function implementation
}
subset_regression_info <- function(subset_reg) {

    par(mfrow=c(2, 2))

    # Extract the summary information
    summary_ <- summary(subset_reg)

    print(summary_)

    # Define the titles, names, and operations
    titles <- c("Rsq",     "AdjRSq",  "AIC",     "BIC"    )
    names  <- c("rsq",     "adjr2",   "cp",      "bic"    )
    op     <- c(which.max, which.max, which.min, which.min)

    for (i in 1:4) {

        # Plot the information
        plot(
            summary_[[names[i]]], 
            xlab = "Number of Variables", 
            ylab = titles[i], 
            type = "l"
        )

        if (titles[i] != "Rsq") { 
            
            # Find the best value and extract its index
            best <- op[[i]](summary_[[names[i]]])

            # Evidence best numer of variables in the plot
            points(
                best, summary_[[names[i]]][best], 
                col = "red", cex = 2, pch = 20
            )   
        
        }

    }

    par(mfrow=c(1, 1))

}
