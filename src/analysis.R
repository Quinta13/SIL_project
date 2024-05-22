#' Analysis Functions
#'
#' This file contains functions for performing diagnostic analysis on linear regression models and comparing predictions of multiple models.
#'
#' @author Sebastiano Quintavalle
#' @date 2024-05-15
#' @version 1.0
#'

#' Perform diagnostic analysis for a linear regression model
#'
#' This function performs various diagnostic analyses for a linear regression model.
#'
#' @param lm The linear regression model object.
#' @param residuals Logical value indicating whether to plot the residuals.
#' @param normality Logical value indicating whether to plot the normality diagnostic plot.
#' @param influencial_points Logical value indicating whether to plot the influencial points diagnostic plot.
#' @param effects Logical value indicating whether to plot the effects diagnostic plot.
#' @param effects_sub A vector of indices specifying which effects to plot. Default is an empty vector.
#'
#' @return None
#'
#' @examples
#' # Fit a linear regression model
#' model <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
#'
#' # Perform diagnostic analysis
#' lm_diagnostic(model)
#' 
#' @export lm_diagnostic
#'
lm_diagnostic <- function(
    lm, 
    residuals          = TRUE,
    normality          = TRUE,
    influencial_points = TRUE,
    effects            = TRUE,
    effects_sub = c()
) {

    # Print the summary
    print(summary(lm))

    # Residuals
    if(residuals) {
        residualPlots(lm)
    }

    # Normality
    if(normality) {
        qqPlot(
            residuals(lm), 
            ylab = "Residuals quantiles", 
            xlab = "Normal quantiles"
        )
    }

    # Influencial points
    if(influencial_points) {
        influenceIndexPlot(lm, vars = "Cook")
    }

    # Effects
    if(effects) {
        if (length(effects_sub) > 0) { plot(allEffects(lm)[effects_sub]) }
        else                         { plot(allEffects(lm))              }
    }

}


#' Compare predictions of multiple models
#'
#' This function takes a list of models, a test dataset, and a target variable, and compares the predictions of the models.
#' It computes various statistics for each model, including R-squared, adjusted R-squared, mean squared error (MSE), and variance of the predictions.
#' It also plots the actual values of the target variable against the predicted values for each model.
#'
#' @param models A list of models to compare
#' @param df_test The test dataset
#' @param y The name of the target variable
#' @param title The title of the plot (optional)
#'
#' @return A data frame containing the computed statistics for each model
#'
#' @examples
#' models <- list(lm(Sepal.Length ~ Sepal.Width, data = iris), lm(Sepal.Length ~ Petal.Length, data = iris))
#' df_test <- iris[101:150, ]
#' y <- "Sepal.Length"
#' models_prediction_comparison(models, df_test, y, title = "Comparison of Models")
#' 
models_prediction_comparison <- function(models, df_test, y, title="") {

    # 1. TABLE

    # Compute models statistics
    results <- lapply(models, function(model) {

        # R2 and Adj R2
        r_squared     <- summary(model)$r.squared
        adj_r_squared <- summary(model)$adj.r.squared
        
        # MSE and Variance of the predictions
        predictions <- predict(model, newdata=df_test)
        residuals   <- df_test[[y]] - predictions
        mse         <- mean((residuals)^2)
        var         <- var(residuals)

        list(
            R2       = r_squared, 
            AdjR2    = adj_r_squared, 
            MSE      = mse, 
            Variance = var
        )

    })
    
    # Create a data frame from the results
    results_df <- data.frame(
        R2    = sapply(results, function(x) x$R2      ),
        AdjR2 = sapply(results, function(x) x$AdjR2   ),
        MSE   = sapply(results, function(x) x$MSE     ),
        Var   = sapply(results, function(x) x$Variance)
    )
    
    # 2. PLOT

    # Plot month vs shootings for the test set
    plot(
        df_to_month_ts(df_test, y=y), 
        xlab="Month", ylab="Shootings Count", main=title
    )

    # Plot model performance
    for(i in 1:length(models)) {

        model_name <- names(models)[i]

        lines(
            df_to_month_ts(
                data.frame(
                    Shootings = predict(models[[model_name]], newdata=df_test),
                    Year      = df_test$Year,
                    Month     = df_test$Month
                ),
                y="Shootings"
            ),
            col = palette()[i], lwd = 3, lty = 'dashed'
        )
    }

    # Add a legend
    legend(
        "topright", 
        legend = names(models), 
        col = palette()[1:length(models)], 
        lwd = 2
    )

    return(results_df)

}
