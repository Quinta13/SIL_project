#' Plotting Functions
#'
#' This file contains generic utility functions for data analysis, such as
#' model diagnostics, PCA analysis, predictor selection and model predictions.
#'
#' @author Sebastiano Quintavalle
#' @date 2024-05-15
#' @version 1.0
#'
#' 


# --- LINEAR MODEL DIAGNOSTIC ---


vif_diagnostic <- function(model) {

    # Calculate VIF values
    vif_values <- vif(model)

    # Return GVIF diagnostic in the case the model is a GLM
    if(! "numeric" %in% class(vif_values)) {
        return(gvif_diagnostic(model))
    }

    # Create an empty data frame to store the results
    vif_df <- data.frame(
        VIF = numeric(),
        Indicator = character(),
        stringsAsFactors = FALSE
    )

    # Iterate over each predictor and store its VIF value and indicator in the data frame
    for (i in 1:length(vif_values)) {

        predictor <- names(vif_values)[i]
        vif_value <- vif_values[i]
        
        # Determine the indicator based on VIF value
        # https://stats.stackexchange.com/questions/559924/which-threshold-should-i-use-for-gvif1-2⋅df-variance-inflation-factor
        indicator <- ifelse(
            vif_value > (10.0), "***", ifelse(
            vif_value > ( 5.0), "**" , ifelse(
            vif_value > ( 2.5), "*"  , 
                                "."
        )))

        # Add the predictor name, VIF value, and indicator to the data frame
        vif_df <- rbind(vif_df, data.frame(
            VIF              = vif_value,
            Indicator        = indicator,
            stringsAsFactors = FALSE
        ))
    
    }

    rownames(vif_df) <- names(vif_values)

    # Print the data frame
    print(vif_df)
    print("")
    print("Legend: . < 2.5, * < 5, ** < 10, *** > 10")

    # Return the data frame
    return(vif_df)

}


lm_diagnostic <- function(
    model, 
    residuals    = FALSE,
    qq           = FALSE,
    cook_dist    = FALSE,
    effects      = FALSE,
    collinearity = FALSE,
    effects_sub  = c(),
    name         = ""
) {

    # Print the summary
    print(summary(model))

    # Residuals
    if(residuals) {
        invisible(residualPlots(model, main = paste("Residuals -", name)))
    }

    # Normality
    if(qq) {
        qqPlot(
            residuals(model), 
            main = paste("QQ Plot -", name),
            ylab = "Residuals quantiles", 
            xlab = "Normal quantiles"
        )
    }

    # Influential points
    if(cook_dist) {
        influenceIndexPlot(
            model, 
            main = paste("Influence Index Plot -", name),
            vars = "Cook"
        )
    }

    # Effects
    if(effects) {
        if (length(effects_sub) > 0) { plot(allEffects(model, main=paste("Effect plots -", name))[effects_sub]) }
        else                         { plot(allEffects(model, main=paste("Effect plots -", name)))              }
    }

    # VIF
    if(collinearity) {
        print(paste("VIF diagnostic -", name))
        vif_diagnostic(model)
        print("")
    }

    par(mfrow=c(1, 1))

}

# --- GLM DIAGNOSTIC ---

glm_plot_predictor_residuals <- function(
    model, df_, 
    residual_type = "pearson", ncol = 0,
    title = ""
) {
    
    # Extract residuals based on the specified type
    residuals <- residuals(model, type = residual_type)
    df_$residuals <- residuals
    df_$Fitted    <- fitted(model)

    # Get the terms used in the model
    terms_used <- attr(terms(model), "term.labels")
    terms_used <- c(terms_used, "Fitted")

    # Remove from terms used all the ones starting with "I("
    terms_used <- terms_used[!grepl("^I\\(", terms_used)]

    # Remove poly and only keep year
    terms_used <- sub("poly\\(([^,]+),.*", "\\1", terms_used)

    # Remove all interaction terms - aka they have : in the Pred name
    terms_used <- terms_used[!grepl(":", terms_used)]

    if(ncol == 0) ncol <- ceiling(sqrt(length(terms_used)))

    # Create a list to store the plots
    plot_list <- list()

    # Generate a plot for each predictor
    for (pred in terms_used) {

        if(class(df_[[pred]]) == "factor") {

            p <- ggplot(df_, aes_string(x = pred, y = "residuals")) +
                geom_boxplot(color = "#003865") +
                labs(
                    x = element_blank(),
                    y = element_blank(),
                    title = pred,
                ) +
                theme_minimal()

        } else {
            
            p <- ggplot(df_, aes_string(x = pred, y = "residuals")) +
                geom_point(color = "#003865") +
                geom_smooth(method = "loess", se = FALSE, color = "#ac0000f1") +
                labs(
                    x = element_blank(),
                    y = element_blank(),
                    title = pred,
                ) +
                theme_minimal()
        }
        
    
        plot_list[[pred]] <- p
    }

    # Arrange the plots in a grid
    grid.arrange(
        grobs = plot_list, 
        ncol = ncol,
        top = textGrob(paste(title, "-", residual_type, "residuals - Predictors"), 
        gp = gpar(fontsize = 16, fontface = "bold"))
    )

}


glm_diagnostic <- function(
    model, df_, 
    residuals = FALSE, qq = FALSE, 
    collinearity = FALSE, name = ""
) {

    # Summary
    print(summary(model))

    # VIF
    if(collinearity) {
        print(paste("GVIF", name))
        gvif_diagnostic(model)
        print("")
    }

    # Residuals
    if (residuals) {
        glm_plot_predictor_residuals(model, df_, title=name)
    }


}


gvif_diagnostic <- function(model) {

    # Calculate VIF values
    vif_values <- vif(model)

    # Create an empty data frame to store the results
    vif_df <- data.frame(
        aGVIF = numeric(),
        Indicator = character(),
        stringsAsFactors = FALSE
    )

    # Iterate over each predictor and store its VIF value and indicator in the data frame
    for (i in 1:nrow(vif_values)) {

        predictor <- rownames(vif_values)[i]
        vif_value <- vif_values[i, 3]
        
        # Determine the indicator based on VIF value
        indicator <- ifelse(
            vif_value > sqrt(10.0), "***", ifelse(
            vif_value > sqrt( 5.0), "**" , ifelse(
            vif_value > sqrt( 2.5), "*"  , 
                                    "."
        )))

        # Add the predictor name, VIF value, and indicator to the data frame
        vif_df <- rbind(vif_df, data.frame(
            aGVIF = vif_value,
            Indicator = indicator,
            stringsAsFactors = FALSE)
        )
    }

    rownames(vif_df) <- rownames(vif_values)

    # Print the data frame and legend
    print(vif_df)
    print("")
    print("Legend: . < 2.5, * < 5, ** < 10, *** > 10")

    # Return the data frame
    return(vif_df)
}


# --- MODEL UTILS  ---


plot_model_coefficients <- function(
    coefs, 
    xlab   = "",
    title  = "",
    labels = NULL
) {

    coefs2 <- c(coefs, -sum(coefs))

    df <- data.frame(
        Index = 1:length(coefs2),
        Coefficient = coefs2,
        Label = labels
    )

    # Plot using ggplot2
    p <- ggplot(df, aes(x=Index, y=Coefficient)) +
            geom_point(color="steelblue", size=3) +
            geom_line(color="steelblue") +
            labs(x=xlab, y="Coefficient", title=title) +
            scale_x_continuous(breaks=1:length(coefs2), labels=labels) +
            theme_minimal() +
            theme(axis.text.x=element_text(angle=45, hjust=1)) +
            theme(
                panel.grid.major = element_line(color = "grey80"), 
                panel.grid.minor = element_line(color = "grey90")
            )

    return(p)

}


pca_analysis <- function(
    df_, features, levels,
    explained       = TRUE,
    individual      = TRUE,
    vars            = TRUE,
    biplot          = TRUE,
    biplot_title    = "Biplot - PCA",
    individuals     = "Individuals - PCA",
    variables       = "Variables - PCA"
) {

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

    if(explained) {

        # Plot explained variance
        explained_variance  <- pca_out$sdev^2
        proportion_variance <- explained_variance / sum(explained_variance)
        cumulative_variance <- cumsum(proportion_variance)

        # Create a data frame for plotting
        variance_df <- data.frame(

            PC = 1:length(proportion_variance),
            Proportion = proportion_variance,
            Cumulative = cumulative_variance
        )

        pal <- PALETTE$pca$histo[1:(length(variance_df$PC))] 

        grid.arrange(

            # Proportion of explained variance
            ggplot(variance_df, aes(x = PC, y = Proportion)) +
                geom_bar(stat = "identity", fill = pal) +
                labs(
                    x = "Principal Component", 
                    y = "Proportion of Explained Variance"
                ) +
                ggtitle("Proportion of Explained Variance") +
                theme_minimal(),

            # Cumulative explained variance
            ggplot(variance_df, aes(x = PC, y = Cumulative)) +
                geom_line(color = PALETTE$pca$line) +
                geom_point(aes(x = PC, y = Cumulative), color = pal, size = 3)+
                labs(
                    x = "Principal Component", 
                    y = "Cumulative Explained Variance"
                ) +
                ggtitle("Cumulative Explained Variance") +
                theme_minimal(), 
            ncol = 2
        )
    }

    if(individual) {
        print(fviz_pca_ind(
            pca_out, col.ind = "cos2", repel = TRUE,
            title = individuals,
            gradient.cols = PALETTE$pca$gradient
        ))
    }

    if(vars) {
        print(fviz_pca_var(
            pca_out, col.var = "contrib", repel = TRUE,
            title = variables,
            gradient.cols = PALETTE$pca$gradient
        ))
    }

    if(biplot) {
            
        # Biplot
        fviz_pca_biplot(
            pca_out, repel = TRUE, 
            col.var = PALETTE$pca$biplot$var, 
            col.ind = PALETTE$pca$biplot$ind
        )

    }

}

# --- PREDICTOR SELECTION ---


plot_correlation_matrix <- function(df, title="") {

    # Compute the correlation matrix for numeric columns
    numeric_cols <- sapply(df, is.numeric)
    cor_matrix   <- cor(df[, numeric_cols], use = "complete.obs")

    # Visualize the correlation matrix using corrplot
    corrplot(
        cor_matrix, 
        method = "color",
        addCoef.col = "black",
        main = title,
        cex.main = 1.5,
        mar = c(0, 0, 5, 0),
        tl.cex = 0.5,
        number.cex = 0.5
    )

}


shrinkage_grid_search <- function(df_, y, alphas, family="gaussian")  {

    coefs <- list()

    coefs$lm <- coef(lm(
        as.formula(paste(y, "~ .")), data = df_
    ))

    for(alpha in alphas) {
        for(type in c("min", "1se")) {
            name <- paste0("(", type, ", alpha=", alpha, ")")
            coefs[[name]] <- coef(
                shrinkage_selection(
                    df_ = df_,
                    y = "Shootings",
                    type = type,
                    alpha = alpha,
                    plot = FALSE,
                    family = family
                )
            )
        }
    }

    # Convert each sparse matrix to a data frame
    coefs_df_list <- lapply(names(coefs), function(name) {
        df_ <- as.data.frame(as.matrix(coefs[[name]]))
        df_
    })

    # Combine all data frames by row names
    combined_df <- do.call(cbind, coefs_df_list)
    colnames(combined_df) <- names(coefs)

    combined_df <- round(combined_df, 5)

    # Add NonZero counts
    combined_df <- rbind(combined_df, SelectedPredictors = apply(combined_df, 2, function(x) sum(x != 0)))


    return(combined_df)

}


shrinkage_selection <- function(
    df_,
    y,
    alpha  = ALPHA,
    type   = "min",
    title  = "",
    family = "gaussian",
    plot = TRUE
) {

    
    # Create model matrix with all predictors
    mm <- model.matrix(as.formula(paste(y, "~ .")), data = df_)

    df_x <- mm[, -1]  # remove intercept
    df_y <- df_[[y]]

    lasso.mod <- cv.glmnet(df_x, df_y, alpha = alpha, family=family)

    # Extract best lambda
    best.lambda <- lasso.mod[[paste0("lambda.", type)]]  # either lambda.min or lambda.1se

    # Fit the model for all lambdas
    lasso.fit   <- glmnet(df_x, df_y, alpha=alpha, family=family)

    if(plot) {

        par(mfrow=c(1, 2))

        # Increase the top margin for the title
        par(mar = c(8, 4, 4, 2) + 1)

        # Plot cross validation best
        plot(lasso.mod, main = "Shrinkage cross validation")
        abline(v = log(best.lambda), lwd = 2., col = "steelblue", lty = "dashed")

        # Plot associated level of lambda
        plot(lasso.fit, xvar = "lambda", main="Shrinkage path")
        abline(v = log(best.lambda), col= "steelblue", lty = "dashed")

        # Reset the margin to default
        par(mar = c(5, 4, 4, 2))

        par(mfrow=c(1, 1))
    
    }

    # Extract the final model with fixed best lambda
    final_model <- glmnet(
        df_x, df_y, 
        family=family, alpha=alpha, 
        lambda=best.lambda
    )

    # Compute log likelihood
    y_pred <- predict(final_model, newx = df_x)
    residuals <- df_y - y_pred
    rss <- sum(residuals^2)
    n <- length(df_y)
    sigma2 <- rss / n

    if(family == "gaussian") {
        log_likelihood <- -n/2 * (log(2 * pi * sigma2) + 1)
    } else if(family == "poisson") {
        
        # pred_linear <- log(y_pred)
        # term1 <- sum(df_y * pred_linear)
        # term2 <- sum(log(factorial(df_y)))
        # term3 <- sum(pred_linear)

        # log_likelihood <- term1 - term2 - term3

        log_likelihood <- -n/2 * (log(2 * pi * sigma2) + 1)
        
    } else {
        log_likelihood <- NA
    }

    final_model$loglik <- log_likelihood

    return(final_model)

}


subset_regression_info <- function(subset_reg, title="") {

    # Extract the summary information
    summary_ <- summary(subset_reg)

    print(summary_)

    # Create a data frame for the summary information
    summary_df <- data.frame(
        Number_of_Variables = 1:length(summary_$rsq),
        Rsq = summary_$rsq,
        AdjRSq = summary_$adjr2,
        AIC = summary_$cp,
        BIC = summary_$bic
    )

    # Define the titles, names, and operations
    titles <- c("Rsq", "AdjRSq", "AIC", "BIC")
    names <- c("Rsq", "AdjRSq", "AIC", "BIC")
    op <- c(which.max, which.max, which.min, which.min)

    best_values = list()

    # Create the grid arrangement
    grid.arrange(

        grobs = lapply(1:4, function(i) {

            # Plot the information
            p <- ggplot(
                summary_df, 
                aes(x = Number_of_Variables, y = .data[[names[i]]])
            ) +
                geom_line(color="#011c33d6") +
                xlab("Number of Variables") +
                ylab(titles[i]) +
                theme_minimal() +
                scale_x_continuous(breaks = 1:length(summary_df$Number_of_Variables))

            if (titles[i] != "Rsq") {

                # Find the best value and extract its index
                best <- op[[i]](summary_df[[names[i]]])
                best_values[[i]] <- best

                # Highlight the best value with a red point
                p <- p + geom_point(data = summary_df[best, ], aes(x = Number_of_Variables, y = .data[[names[i]]]), color = "#ff3c00", size=2)
            
            }

            # Return the plot
            p
        }),

        ncol = 2,

        top = grid::textGrob(paste(title, "- Subset Regression - Best number of predictors")), gp = grid::gpar(fontsize = 16, fontface = "bold"))

}

# --- PREDICTIONS ---


generate_predictions_with_ci <- function(
    model, 
    newdata,
    y, 
    response = "response",
    level = LEVEL,
    glmnet_predictors = c()
) {

    if("lm" %in% class(model)) {

        if("glm" %in% class(model)) {

            # https://stackoverflow.com/questions/14423325/confidence-intervals-for-predictions-from-logistic-regression
        
            predictions = predict(model, newdata, type = "response", se.fit = TRUE)

            critical_value <- qt(level, df = model$df.residual)  # For a 95% CI

            newdata[[y]]  <- predictions$fit
            newdata$Lower <- predictions$fit - critical_value * predictions$se.fit
            newdata$Upper <- predictions$fit + critical_value * predictions$se.fit


        } 
        else {

            predictions <- data.frame(
                predict(model, newdata = newdata, type=response, interval = "confidence", level = level)
            )

            # Combine predictions with new data
            newdata[[y]]  <- predictions$fit
            newdata$Lower <- predictions$lwr
            newdata$Upper <- predictions$upr

        }

    } else if("glmnet" %in% class(model)) {

        # https://stats.stackexchange.com/questions/478115/how-do-i-calculate-confidence-intervals-on-an-elastic-net-regression-in-r

        df_dummy <- model.matrix(~., data = newdata[, glmnet_predictors])[, -1]
        predictions <- predict(
            model, 
            type = response,
            newx = df_dummy,
            se.fit = TRUE
        )
    
        # Compute confidence intervals
        newdata[[y]] <- predictions
        newdata$Lower <- predictions
        newdata$Upper <- predictions
        
    
    } else {
        stop(paste("Model class not supported:", class(model)))
    }

    # Generate predictions with confidence intervals
    return(newdata)
}


get_prediction_stats <- function(
    model, 
    df_, 
    y,
    level = LEVEL,
    glmnet_predictors = c()
) {
    
    # Compute predictions

    preds <- generate_predictions_with_ci(
        model, 
        newdata = df_, 
        y = y, 
        response = "response",
        level = level,
        glmnet_predictors = glmnet_predictors
    )

    resid  <- df_[[y]] - preds[[y]]
    mse    <- mean((resid)^2)
    avg_se <- mean(preds[[y]] - preds$Lower)
    
    return(list(
        MSE   = mse,
        AvgSE = avg_se
    ))
    
}


get_models_prediction_stats <- function(
    models,
    df_, 
    y,
    glmnet_predictors = c()
) {

    
    return (
        
        lapply(models, function(model) {

            # Prediction statistics
            pred_stats <- get_prediction_stats(model, df_, y, glmnet_predictors=glmnet_predictors)

            # R2 and Adj R2
            if("lm" %in% class(model)) {

                if("glm" %in% class(model)) {
                    r_squared     <- NA
                    adj_r_squared <- NA
                    aic <- AIC(model)
                    bic <- BIC(model)
                } else {
                    r_squared     <- summary(model)$r.squared
                    adj_r_squared <- summary(model)$adj.r.squared
                    aic = AIC(model)
                    bic = BIC(model)
                }
            
            } else if("glmnet" %in% class(model)) {

                r_squared <- NA
                adj_r_squared <- NA

                n   <- nobs(model)
                k   <- sum(coef(model) != 0)

                aic <- - 2 * model$loglik +      2 * k 
                bic <- - 2 * model$loglik + log(n) * k

            } else {
                stop(paste("Model class not supported for model", class(model)))
            }

            
            # Prediction statistics
            pred_stats <- get_prediction_stats(model, df_, y, glmnet_predictors=glmnet_predictors)

            list(
                R2    = r_squared,
                AdjR2 = adj_r_squared,
                AIC   = aic,
                BIC   = bic,
                MSE   = pred_stats$MSE,
                AvgSE = pred_stats$AvgSE
            )

        })
    )


}


generate_prediction_table <- function(models, predictor_name, predictor_values) {
  # Initialize an empty data frame to store the results
  prediction_table <- data.frame(matrix(ncol = length(predictor_values) + 1, nrow = length(models)))
  
  # Set column names
  colnames(prediction_table) <- c("Model", paste(predictor_values))
  
  # Set row names
  prediction_table$Model <- names(models)
  
  # Loop over each model
  for (i in 1:length(models)) {
    model_name <- names(models)[i]
    model <- models[[model_name]]
    
    # Create a data frame for the predictor values
    dummy_df <- data.frame(matrix(ncol = 1, nrow = length(predictor_values)))
    colnames(dummy_df) <- predictor_name
    dummy_df[[predictor_name]] <- predictor_values
    
    # Predict using the model
    predictions <- predict(model, newdata = dummy_df, type = "response")
    
    # Store predictions in the result table
    prediction_table[i, -1] <- predictions
  }
  
  return(prediction_table)
}


df_group_stats <- function(data, group_col, value_col) {
    stats_table <- aggregate(
        data[[value_col]] ~ data[[group_col]], 
        data = data, 
        FUN = function(x) c(Mean = mean(x), Var = var(x), Std = sd(x))
    )

    stats_table <- data.frame(
        tmp    = stats_table[[1]],
        "Mean" = stats_table[[2]][, 1],
        "Var"  = stats_table[[2]][, 2],
        "Std"  = stats_table[[2]][, 3]
    )

    rownames(stats_table) <- stats_table$tmp
    stats_table$tmp <- NULL

    # Reorder stats_table columns
    stats_table <- stats_table[, c("Mean", "Var", "Std")]

    return(stats_table)
}


stats_to_table <- function(stats) {

    return(data.frame(
        R2    = sapply(stats, function(x) x$R2   ),
        AdjR2 = sapply(stats, function(x) x$AdjR2),
        AIC   = sapply(stats, function(x) x$AIC  ),
        BIC   = sapply(stats, function(x) x$BIC  ),
        MSE   = sapply(stats, function(x) x$MSE  ),
        AvgSE = sapply(stats, function(x) x$AvgSE)
    ))
}
