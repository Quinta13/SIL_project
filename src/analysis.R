#' Analysis Functions
#'
#' This file contains functions for performing diagnostic analysis on linear regression models and comparing predictions of multiple models.
#'
#' @author Sebastiano Quintavalle
#' @date 2024-05-15
#' @version 1.0
#'
#' 

# --- DATA MANIPULATION ---

#' Discretize the columns in a data frame
#'
#' This function takes a data frame as input and discretizes specific columns
#' in the data frame. It converts the "Borough" column to a factor, the "Month"
#' column to a factor with month names, and the "Year" column to a factor.
#'
#' @param df_ The input data frame
#' @return The discretized data frame
#'
#' @examples
#' df <- data.frame(Borough = c("Manhattan", "Brooklyn", "Queens"),
#'                  Month = c(1, 2, 3),
#'                  Year = c(2020, 2021, 2022))
#' discretize(df)
#'
#' @export
discretize <- function(df_) {

    df_$Borough   <- as.factor(df_$Borough)
    df_$MonthName <- as.factor(month.name[df_$Month])
    df_$MonthName <-    factor(df_$MonthName, levels = month.name) # reorder levels by month
    df_$YearN     <- as.factor(df_$Year)

    return(df_)

}


#' Split the dataset into training and testing sets based on the specified years.
#'
#' This function takes a dataframe and a vector of years to be used for testing. It splits the dataframe into two subsets: one for training and one for testing.
#' The rows with years present in the \code{year_test} vector are extracted into the testing subset, while the remaining rows are kept in the training subset.
#' Additionally, unused levels of the \code{YearN} variable are dropped from both subsets.
#'
#' @param df_ The input dataframe.
#' @param year_test A vector of years to be used for testing.
#'
#' @return A list containing two dataframes: \code{train} for training and \code{test} for testing.
#'
#' @examples
#' df <- data.frame(Year = c(2010, 2011, 2012, 2013, 2014),
#'                  YearN = c("Y1", "Y2", "Y3", "Y4", "Y5"),
#'                  Value = c(10, 20, 30, 40, 50))
#' split_data <- train_test_split(df, c(2013, 2014))
#' train_data <- split_data$train
#' test_data <- split_data$test
#' 
#' @export
train_test_split <- function(df_, year_test) {

    df_test <- df_[ (df_$Year %in% year_test), ]
    df_     <- df_[!(df_$Year %in% year_test), ]

    # Drop unused levels of YearN
    df_    $YearN <- droplevels(df_    $YearN)
    df_test$YearN <- droplevels(df_test$YearN)

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

#' Perform PCA analysis on a data frame
#'
#' This function performs Principal Component Analysis (PCA) on a given data frame.
#' It aggregates the data based on a specified level, performs PCA, and plots the results.
#' Additionally, it allows highlighting specific arrows in the biplot.
#'
#' @param df_ The input data frame.
#' @param features A character vector specifying the column names of the features to be used in the analysis.
#' @param levels A character vector specifying the column name of the level to aggregate the data on.
#' @param title A character string specifying the title of the biplot.
#' @param highligth A character vector specifying the row names of the arrows to be highlighted in the biplot.
#'
#' @return None
#'
#' @examples
#' # Example usage
#' pca_analysis(df, c("feature1", "feature2"), "level", "PCA Analysis", c("arrow1", "arrow2"))
#'
#' @export
pca_analysis <- function(df_, features, levels, title, highligth = c()) {

    # Aggregate data based on the input level
    df_agg <- aggregate(
        df_[, features],
        by = list(df_[[levels]]),
        FUN = mean
    )

    colnames(df_agg) <- c(levels, features)
    rownames(df_agg) <- df_agg[[levels]]
    df_agg[[levels]] <- NULL

    # Perform PCA
    pca_out <- prcomp(df_agg, scale = TRUE)

    # Print mean and variance of the features
    print(
        data.frame(
            row.names = names(pca_out$center),
            Center    = pca_out$center,
            Scale     = pca_out$scale
        )
    )

    # Plot
    biplot(pca_out, scale = 0, main = title)

    # Highlight specific arrows
    if (length(highligth) > 0) {
        highlighted_indices <- which(rownames(pca_out$rotation) %in% highligth)
        arrows(
            0, 0, pca_out$rotation[highlighted_indices, 1], pca_out$rotation[highlighted_indices, 2],
            col = "red", length = 0.1, lwd = 2
        )
    }

}

# --- MODEL DIAGNOSTIC ---

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

# --- PREDICTIONS ---

#' Compute prediction statistics
#'
#' This function takes a trained model, a data frame, and a target variable as input,
#' and computes various prediction statistics including predictions, mean squared error (MSE),
#' variance, and standard deviation.
#'
#' @param model The trained model object.
#' @param df_ The data frame containing the predictor variables.
#' @param y The name of the target variable.
#'
#' @return A list containing the following prediction statistics:
#'   - Pred: The predicted values.
#'   - MSE: The mean squared error.
#'   - Var: The variance of the predictions.
#'   - Std: The standard deviation of the predictions.
#'
#' @examples
#' # Example usage
#' model <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
#' df <- iris[, c("Sepal.Width", "Petal.Length")]
#' y <- "Sepal.Length"
#' stats <- get_prediction_stats(model, df, y)
#' print(stats)
#'
get_prediction_stats <- function(model, df_, y) {
    
    # Compute predictions
    preds <- predict(model, newdata=df_)
    resid <- df_[[y]] - preds
    mse   <- mean((resid)^2)
    var   <- var(preds)
    std   <-  sd(preds)
    
    return(list(
        Pred = preds,
        MSE  = mse,
        Var  = var,
        Std  = std
    ))
    
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


    # Compute predictions
    models_stats <- lapply(models, function(model) {

        # R2 and Adj R2
        r_squared     <- summary(model)$r.squared
        adj_r_squared <- summary(model)$adj.r.squared
        
        # Prediction statistics
        pred_stats <- get_prediction_stats(model, df_test, y)

        list(
            R2    = r_squared,
            AdjR2 = adj_r_squared,
            Pred  = pred_stats$Pred,
            MSE   = pred_stats$MSE,
            Var   = pred_stats$Var,
            Std   = pred_stats$Std
        )

    })
    
    # 2. PLOT

    # Plot month vs shootings for the test set
    for(i in 1:2) {  # First only bars, second with CI
        plot(
            df_to_month_ts(df_test, y=y), 
            xlab="Month", ylab="Shootings Count", main=title
        )
    }

    # Plot model performance
    for(i in 1:length(models)) {

        model_name <- names(models)[i]

        draw_pred_ci(
            df_=df_test,
            stats=models_stats[[model_name]],
            y=y,
            col=get_palette_color(i)
        )

    }

    # Add a grid
    grid()

    # Add a legend
    legend(
        "topright", 
        legend = names(models), 
        col = palette()[1:length(models)], 
        lwd = 2
    )

    return(data.frame(
        R2    = sapply(models_stats, function(x) x$R2   ),
        AdjR2 = sapply(models_stats, function(x) x$AdjR2),
        MSE   = sapply(models_stats, function(x) x$MSE  ),
        Var   = sapply(models_stats, function(x) x$Var  ),
        Std   = sapply(models_stats, function(x) x$Std  )
    ))

}


#' Function to plot multiple time series and their prediction errors
#'
#' This function takes a model, a data frame, a response variable, and a grouping variable
#' and plots multiple time series based on the grouping variable. For each group, it also
#' evaluates the prediction statistics using the specified model and plots the prediction
#' error bars.
#'
#' @param model The model object used for prediction
#' @param df_ The data frame containing the time series data
#' @param y The response variable to be predicted
#' @param levels The grouping variable used to split the data frame
#' @param title The title of the plot (optional)
#' @param ylab The y-axis label of the plot (optional)
#'
#' @return None
#'
#' @examples
#' models_levels_predictions(
#'   model = lm(y ~ x, data = df),
#'   df_ = df,
#'   y = "y",
#'   levels = "group",
#'   title = "Time Series Plot",
#'   ylab = "Response Variable"
#' )
model_levels_predictions <- function(
    model, 
    df_, 
    y, 
    levels, 
    title="", 
    ylab=""
) {

    # Plot the true time series
    plot_multiple_ts(
        ts_list = lapply(
            split(df_, df_[[levels]]),
            df_to_month_ts,
            y = y
        ),
        title = title,
        ylab = ylab
    )

    level_values <- levels(df_[[levels]])

    for(i in 1:length(level_values)) {

        # Extract the subset for the specific level
        value <- level_values[i]
        df_value <- df_[df_[[levels]] == value, ]

        # Evaluate predictions
        stats <- get_prediction_stats(
            model=model, df_=df_value, y=y
        )

        # Plot the predictions errors bars
        draw_pred_ci(
            df_=df_value, 
            y=y, 
            col=get_palette_color(i),
            stats=stats,
            lty='dashed'
        )

    }

}
