#' Analysis Functions

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
    df_, features, levels, title
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

    # Print mean and variance of the features
    print(
        data.frame(
            row.names = names(pca_out$center),
            Center    = pca_out$center,
            Scale     = pca_out$scale
        )
    )

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

    pal <- PALETTE$pca_histo[1:(length(variance_df$PC))] 

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
            geom_line(color = "#03016b") +
            geom_point(aes(x = PC, y = Cumulative), color = pal, size = 3)+
            labs(
                x = "Principal Component", 
                y = "Cumulative Explained Variance"
            ) +
            ggtitle("Cumulative Explained Variance") +
            theme_minimal(), 
        ncol = 2
    )

    # BIPLOT
    biplot(pca_out, scale = 0, main = title, col = PALETTE$pca_biplot)


}

lm_diagnostic <- function(
    lm, 
    residuals          = TRUE,
    normality          = TRUE,
    influencial_points = TRUE,
    effects            = TRUE,
    vif                = TRUE,
    effects_sub = c()
) {

    # Print the summary
    print(summary(lm))

    # Residuals
    if(residuals) {
        print("Residuals")
        residualPlots(lm)
        print("")
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

    # VIF
    if(vif) {
        print("VIF diagnostic")
        print(vif_diagnostic(lm))
        print("")
    }

    par(mfrow=c(1, 1))

}

vif_diagnostic <- function(model) {

    # Calculate VIF values
    vif_values <- vif(model)

    # Create an empty data frame to store the results
    vif_df <- data.frame(
        VIF = numeric(),
        Indicator = character(),
        stringsAsFactors = FALSE
    )

    # Iterate over each predictor and store its VIF value and indicator in the data frame
    for (i in seq_along(vif_values)) {
        predictor <- names(vif_values)[i]
        vif_value <- vif_values[i]
        
        # Determine the indicator based on VIF value
        indicator <- ifelse(vif_value > 10, "**", ifelse(vif_value > 5, "*", "."))
        
        # Add the predictor name, VIF value, and indicator to the data frame
        vif_df <- rbind(vif_df, data.frame(
            VIF = vif_value,
            Indicator = indicator,
            stringsAsFactors = FALSE)
        )
    }

    # Return the data frame
    return(vif_df)
}


get_prediction_stats <- function(
    model, 
    df_, 
    y,
    glmnet_predictors = c()
) {
    
    # Compute predictions

    if("lm" %in% class(model)) {
        preds <- predict(model, newdata=df_, type="response")
    } else if("glmnet" %in% class(model)) {
        df_dummy <- model.matrix(~., data = df_[, glmnet_predictors])[, -1]
        preds    <- predict(model, newx = df_dummy, type="response")
    } else {
        stop("Unkown model")
    }

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

stats_to_table <- function(stats) {

    return(data.frame(
        R2    = sapply(stats, function(x) x$R2   ),
        AdjR2 = sapply(stats, function(x) x$AdjR2),
        AIC   = sapply(stats, function(x) x$AIC  ),
        BIC   = sapply(stats, function(x) x$BIC  ),
        MSE   = sapply(stats, function(x) x$MSE  ),
        Var   = sapply(stats, function(x) x$Var  ),
        Std   = sapply(stats, function(x) x$Std  )
    ))

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

    stats_table[[group_col]] <- stats_table$tmp
    stats_table$tmp <- NULL

    # Reorder stats_table columns
    stats_table <- stats_table[, c(group_col, "Mean", "Var", "Std")]

    return(stats_table)
}


get_models_prediction_stats <- function(
    models,
    df_, 
    y,
    glmnet_predictors = c()
) {
    
    return (
        
        lapply(models, function(model) {

            # R2 and Adj R2
            if("lm" %in% class(model)) {
                r_squared     <- summary(model)$r.squared
                adj_r_squared <- summary(model)$adj.r.squared
                aic = AIC(model)
                bic = BIC(model)
            } else {
                r_squared     <- model$dev.ratio
                adj_r_squared <- 0
                aic = 0
                bic = 0
            }

            
            # Prediction statistics
            pred_stats <- get_prediction_stats(model, df_, y, glmnet_predictors=glmnet_predictors)

            list(
                R2    = r_squared,
                AdjR2 = adj_r_squared,
                AIC   = aic,
                BIC   = bic,
                Pred  = pred_stats$Pred,
                MSE   = pred_stats$MSE,
                Var   = pred_stats$Var,
                Std   = pred_stats$Std
            )

        })
    )


}

shrinkage_selection <- function(
    df_,
    y,
    alpha = 1,
    type  = "min",
    title = ""
) {

    par(mfrow=c(1, 2))
    mm <- model.matrix(as.formula(paste(y, "~ .")), data = df_)

    df_x <- mm[, -1]  # remove intercept
    df_y <- df_[[y]]

    lasso.mod <- cv.glmnet(df_x, df_y, alpha = 1)

    best.lambda  <- if(type == "min") {
        lasso.mod$lambda.min
    } else if(type == "1se") {
        lasso.mod$lambda.1se
    } else {
        stop("Invalid type argument. Please choose either 'min' or '1se'.")
    }
    lasso.coef   <- coef(lasso.mod, best.lambda)

    plot(lasso.mod, main = "Lasso cross validation")
    abline(v = log(best.lambda), lwd = 2., col = "steelblue", lty = "dashed")

    lasso.fit <- glmnet(df_x, df_y, alpha=alpha)

    plot(lasso.fit, xvar = "lambda", main=title)
    abline(v = log(best.lambda), col= "steelblue", lty = "dashed")

    final_model <- glmnet(df_x, df_y, alpha=alpha, lambda=best.lambda)

    par(mfrow=c(1, 1))

    return(final_model)


}

subset_regression_info <- function(subset_reg) {

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

        top = grid::textGrob("Subset Regression - Best number of predictors", gp = grid::gpar(fontsize = 16, fontface = "bold"))
    )


}

