#' Plotting Functions
#'
#' This file contains generic plotting functions for data visualization.
#'
#' @author Sebastiano Quintavalle
#' @date 2024-05-15
#' @version 1.0
#'

# --- PLOTTING UTILS ---

pred_error_bar <- function(df_, stats, col, y, lty='solid') {

    # Transform prediction into time series
    pred_ts <- df_to_month_ts(
        data.frame(
            Shootings = stats$Pred,
            Year      = df_  $Year,
            Month     = df_  $Month
        ),
        y=y
    )

    # Add prediction line
    lines(
        pred_ts,
        col = col, lwd = 3, lty = lty
    )

    # Draw confidence intervals using polygon
    polygon(
        c(time(pred_ts), rev(time(pred_ts))),
        c(
                stats$Pred + stats$Std, 
            rev(stats$Pred - stats$Std)
        ),
        col = adjustcolor(col, alpha.f = 0.25),
        border = NA
    )

}


#' levels_scatterplot Function
#'
#' This function creates a scatterplot with different levels of a variable.
#'
#' @param df_ The data frame containing the variables.
#' @param x The name of the variable to be plotted on the x-axis.
#' @param y The name of the variable to be plotted on the y-axis.
#' @param levels The name of the variable representing the levels.
#' @param title The title of the scatterplot (default is an empty string).
#' @param jitter_ A logical value indicating whether to add jitter to the x-axis (default is FALSE).
#' @param legend_pos The position of the legend in the plot (default is "topright").
#'
#' @return None
#'
#' @examples
#' df <- data.frame(x = rnorm(100), y = rnorm(100), levels = sample(letters[1:3], 100, replace = TRUE))
#' levels_scatterplot(df, "x", "y", "levels", "Scatterplot with Levels", TRUE, "bottomleft")
#'
#' @export
levels_scatterplot <- function(
    df_, x, y, levels,
    title      = "", 
    jitter_    = FALSE, 
    legend_pos = "topright"
) {
    
    # Define palette
    palette(brewer.pal(n = length(unique(df_[[levels]])), name = PALETTE))

    # Add jitter to the x-axis if needed
    x_ = df_[[x]] 
    if (jitter_) { x_ <- jitter(df_[[x]]) }
    
    # Create the scatterplot
    plot(
        x = x_,
        y = df_[[y]],
        col  = df_[[levels]],
        xlab = x,   ylab = y,
        pch  = 16,  main = title,
    )

    # Add a legend
    legend(
        legend_pos,
        legend = levels(df_$Borough),
        col    = 1:length(levels(df_[[levels]])),
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
                col = "#ff3c00", cex = 2, pch = 20
            )   
        
        }

    }

    par(mfrow=c(1, 1))

}
