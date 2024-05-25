#' Plotting Functions
#'
#' This file contains generic plotting functions for data visualization.
#'
#' @author Sebastiano Quintavalle
#' @date 2024-05-15
#' @version 1.0
#'

# --- PLOTTING UTILS ---

draw_pred_ci <- function(df_, stats, col, y, lty='solid') {

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



#' Plot Coefficients
#'
#' This function plots the coefficients of a model, including the sum of the coefficients.
#'
#' @param coefs A numeric vector of coefficients.
#' @param xlab The label for the x-axis. Default is an empty string.
#' @param title The title of the plot. Default is an empty string.
#' @param labels A character vector of labels for the x-axis. Default is NULL.
#'
#' @return None
#'
#' @examples
#' coefs <- c(0.5, 0.3, -0.2)
#' plot_coefficients(coefs, xlab="Features", title="Model Coefficients", labels=c("A", "B", "C"))
#'
plot_coefficients <- function(coefs, xlab="", title="", labels=NULL) {

    coefs2 <- c(coefs, -sum(coefs))

    plot(
        coefs2, 
        xlab=xlab,
        ylab="Coefficient", 
        xaxt="n", 
        main=title,
        col="steelblue", 
        pch=19, 
        type="o"
    )
    
    grid()

    axis(
        side=1, 
        at=1:length(coefs2), 
        labels=labels
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


#' Plot model lines
#'
#' This function plots the model lines for different levels of a categorical variable.
#'
#' @param df_ The data frame containing the data.
#' @param model The model object.
#' @param x The name of the predictor variable on the x-axis.
#' @param y The name of the response variable on the y-axis.
#' @param levels The name of the categorical variable used to create different lines.
#' @param title The title of the plot.
#' @param ylab The label for the y-axis.
#'
#' @return None
#'
#' @examples
#' df <- data.frame(x = 1:10, y = 1:10, levels = rep(c("A", "B"), each = 5))
#' model <- lm(y ~ x, data = df)
#' plot_model_lines(df, model, "x", "y", "levels", "Model Lines", "Response")
#' 
plot_model_lines <- function(
    df_, model, x, y, levels, title, ylab
) {

    # Plot points
    levels_scatterplot(
        df_=df_,
        x=x, y=y, levels=levels
    )

    # Create dummy DF for predictions
    dummy_df <- data.frame(tmp = seq(min(crimes[[x]]), max(crimes[[x]]), by = 1))
    dummy_df[[x]] <- dummy_df$tmp
    dummy_df[[y]] <- NULL

    levels_values <- levels(crimes[[levels]])

    for (i in 1:length(levels_values)) {

        # Extract the level value
        value <- levels_values[i]

        # Compute predictions for that Borough
        dummy_df[[levels]] <- rep(value, nrow(dummy_df))
        preds <- predict(model, newdata = dummy_df)

        lines(
            x = dummy_df[[x]],
            y = preds,
            col = get_palette_color(i),
            lwd = 2
        )
    }

}
