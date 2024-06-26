#' Plotting Functions
#'
#' This file contains generic plotting functions for data visualization.
#'
#' @author Sebastiano Quintavalle
#' @date 2024-05-15
#' @version 1.0
#'

# --- BOXPLOT ---

plot_boxplot <- function(df, x, y, title, colors=c()) {

    p = ggplot(df, aes(x = get(x), y = get(y))) +
        geom_boxplot(aes(fill = .data[[x]]), alpha = 0.7) +
        labs(title = title, x = x, y = y) +
        theme(legend.position = "none")

    if(length(colors)>0) {
        p <- p + scale_fill_manual(values = colors)
    }

    return(p)

}

# --- SCATTERPLOTS ---

basic_scatterplot <- function(
    data, x, y,
    size    = 1,
    shape   = 16, 
    color   = "steelblue",
    jitter_ = FALSE,
    title   = "",
    xlab    = "",
    ylab    = ""
) {

    # Extract axis values
    x_ <- data[[x]]
    y_ <- data[[y]]

    # Add jitter to both x-axis if needed
    if (jitter_) {
        x_ <- jitter(data[[x]]) 
        y_ <- jitter(data[[y]])
    }

    # Create the scatterplot using ggplot
    p <- ggplot(data=data, aes(x=x_, y=y_)) +
        geom_point(shape=16, color=color, size=size) +
        labs(
            x=ifelse(xlab == "", element_blank(), xlab), 
            y=ifelse(ylab == "", element_blank(), ylab),
            title=title
        )

    return(p)

}


levels_scatterplot <- function(
    df_, x, y, levels,
    title      = "", 
    title_size = 16,
    legend_pos = "topright",
    xlab       = "",
    ylab       = "",
    jitter_    = FALSE, 
    ticks      = 10,
    shape      = 16,
    size       = 1,
    colors     = c(),
    legend     = TRUE,
    legend.position  = "right",
    legend.direction = "vertical",
    legend.size      = 12
) {
    
    # Extract axis values
    x_ = df_[[x]] 
    y_ = df_[[y]]

    # Add jitter to both x-axis and y-axis if needed
    if (jitter_) {
        x_ <- jitter(x_) 
        y_ <- jitter(y_)
    }
    
    # Create the scatterplot using ggplot
    p <- ggplot(
        data = df_, 
        aes(x = x_, y = y_, color = .data[[levels]])
    ) +
        geom_point(shape = shape, size = size) +
        labs(
            title = title,
            x = xlab,
            y = ylab
        ) +
        scale_color_discrete(labels = levels(df_[[levels]])) +
        theme(
            axis.text.x = element_text(),
            axis.text.y = element_text(),
            legend.position = legend.position,
            legend.direction = legend.direction,
            plot.title = element_text(size = title_size),
            legend.text = element_text(size = legend.size),
            legend.title = element_text(size = legend.size + 2),
        ) +
        guides(colour = guide_legend(override.aes = list(size=legend.size / 6))) + 
        scale_x_continuous(breaks = pretty_breaks(n = ticks))

    if(length(colors) > 0) {
        p <- p + scale_color_manual(values = colors)
    }

    if(!legend) {
        p <- p + theme(legend.position = "none")
    }

    return(p)
}

# --- TIME SERIES ---

plot_ts <- function(
    df_ts, group, y,
    xlab   = "Year",
    ylab   = "",
    title  = "",
    title.size = 15,
    col    = "#4683b4a3",
    lwd    = 1,
    ticks  = 10,
    yrange = c()
) {

    p = ggplot(
        data=df_ts[df_ts[[group]] == y, ], # Use only the specified feature
        aes(x=YearMonth, y=Count)
    ) +
        geom_line(color=col, linewidth=lwd) +
        labs(
            x=xlab,
            y=ifelse(ylab == "", y, ylab),
            title=title
        ) +
        theme(
            axis.text.x=element_text(),
            plot.title = element_text(size = title.size)
        ) +
        scale_x_date(breaks=pretty_breaks(n=ticks))
    
    if (length(yrange) > 0) {
        p <- p + scale_y_continuous(limits=yrange)
    }
    
    return(p)

}

plot_multiple_ts <- function(
    df_ts, group,
    xlab   = NULL,
    ylab   = NULL,
    title  = "",
    title_size = 16,
    lwd    = 1,
    ticks  = 5,
    colors = c(),
    legend = TRUE,
    legend.position  = "right",
    legend.direction = "vertical",
    legend.size      = 12,
    yrange = c()
) {
    p <- ggplot(
        data = df_ts, 
        aes(
            x = YearMonth, 
            y = Count, 
            group = !!sym(group),
            colour = !!sym(group)
        )
    ) +
    geom_line(linewidth = lwd) +
    labs(
        x = xlab,
        y = ylab,
        title = title
    ) +
    theme(
        axis.text.x = element_text(),
        axis.text.y = element_text(),
        legend.position = legend.position,
        legend.direction = legend.direction,
        plot.title = element_text(size = title_size),
        legend.text = element_text(size = legend.size),
        legend.title = element_text(size = legend.size + 2),
        legend.key.height = unit(legend.size/6, "lines")
    ) +
    scale_x_date(breaks = pretty_breaks(n = ticks))

    if (length(colors) > 0) {
        p <- p + scale_color_manual(values = colors)
    }

    if (!legend) {
        p <- p + theme(legend.position = "none")
    }

    if (length(yrange) > 0) {
        p <- p + scale_y_continuous(limits = yrange)
    }

    if (is.null(xlab)) {
        p <- p + theme(axis.title.x = element_blank())
    }

    if (is.null(ylab)) {
        p <- p + theme(axis.title.y = element_blank())
    }

    return(p)
}

# --- PREDICTIONS ---

plot_model_prediction <- function(
    df_, model, x, y, 
    title="",
    xlab = "",
    ylab = "",
    jitter_=FALSE,
    col="steelblue",
    col_points="steelblue",
    points_size=1,
    lwd=1.,
    glmnet_predictors=c()
) {

    # Plot points
    p <- basic_scatterplot(
        data=df_, x=x, y=y, jitter_=jitter_,
        xlab = xlab, ylab = ylab,
        color=col_points, size=points_size
    )

    # Create dummy DF for predictions
    dummy_df <- data.frame(
        tmp=seq(min(df_[[x]]), max(df_[[x]]), 
        by=1)
    )

    dummy_df[[x]] <- as.numeric(dummy_df$tmp)
    dummy_df$tmp  <- NULL
    
    dummy_df[[y]] <- predict(
        model, 
        newdata=dummy_df, 
        type="response"
    )
        
    # Add lines to the plot
    p <- p + geom_line(
        data=dummy_df, 
        aes(x=.data[[x]], y=.data[[y]]), 
        linetype="dashed", 
        color=col,
        linewidth=lwd
    )

    return(p)

}



plot_multiple_model_prediction <- function(
    df_, models, x, y, 
    title="",
    xlab = "",
    ylab ="",
    jitter_=FALSE,
    colors=c(),
    points_size=1,
    lwd = 1,
    glmnet_predictors=c(),
    level = 0.
) {

    # Plot points
    p <- basic_scatterplot(
        data=df_, x=x, y=y, jitter_=jitter_,
        color='black', size=points_size,
        title=title, xlab=xlab, ylab=ylab
    )

    if(length(colors) == 0) {
        colors <- rainbow(length(models))
    }

    # Create a color mapping for the models
    model_colors <- setNames(colors, names(models))

    # Loop over models to add prediction lines
    for(i in 1:length(models)) {

        model_name <- names(models)[i]
        model <- models[[model_name]]

        # Create dummy DF for predictions
        dummy_df <- data.frame(tmp=seq(min(df_[[x]]), max(df_[[x]]), by=1))

        dummy_df[[x]] <- as.numeric(dummy_df$tmp)
        dummy_df$tmp <- NULL

        pred_df <- generate_predictions_with_ci(
            model=model, newdata=dummy_df, y=y, level=level
        )
        
        p <- p + geom_line(
            data=pred_df, 
            aes_string(x=x, y=y, color=shQuote(model_name)), 
            linetype="dashed", 
            linewidth=1
        )

        
        if (level > 0) {
            p <- p + geom_ribbon(
            data = pred_df,
            aes_string(
                x = x, 
                ymin = "Lower", 
                ymax = "Upper",
                fill = shQuote(model_name),
                color = NULL
            ),
            alpha = ALPHA_CH,
            inherit.aes = FALSE
            )
        }
        }

    # Use scale_color_manual to specify the colors
    p <- p + scale_color_manual(values=model_colors, name="Model")
    p <- p + scale_fill_manual(values=model_colors, name="Model")
    # Show only the geom_line legend
    p <- p + guides(
        color=guide_legend(title="Model"), 
        fill=guide_legend(title="Model")
    )

    return(p)

}

plot_model_prediction_over_time <- function(
    df_, model, x, y, 
    title="",
    ylab ="",
    col  ="steelblue",
    col_observations="steelblue",
    lwd=1,
    lwd_observations=1,
    ticks=10,
    level = 0.,
    yrange=c(),
    glmnet_predictors=c()
) {

    # Plot ts
    
    p <- plot_ts(
        df_ts=ts_crimes_reshape(
            df_[, c("Year", "MonthName", y)]
        ),
        group="CrimeType",
        y=y,
        ylab=ylab,
        title=title,
        col=col_observations,
        lwd=lwd_observations,
        yrange=yrange,
        ticks=ticks
    )

    # Create dummy DF for predictions
    preds <- generate_predictions_with_ci(
        model=model, 
        newdata=df_, 
        y=y, 
        level=level,
        glmnet_predictors=glmnet_predictors
    )

    # Add the prediction line
    p <- p + geom_line(
        aes_string(y=preds[[y]]),
        lwd=lwd,
        color=col,
        linetype="dashed"
    )

    # Optionally add the confidence interval ribbon
    if (level > 0) {
        p <- p + geom_ribbon(
            aes_string(
                ymin=preds$Lower, 
                ymax=preds$Upper, 
            ),
            fill=col,
            alpha=ALPHA_CH
        )
    }

    return(p)

}


plot_model_prediction_per_level <- function(
    df_, model, x, y, levels, 
    title="",
    xlab="", ylab ="",
    points_size = 1,
    level = 0,
    colors = c(),
    glmnet_predictors=c(),
    legend = TRUE
) {

    levels_values <- levels(df_[[levels]])

    if(length(colors) == 0) {
        colors <- rainbow(length(levels_values))
    }

    # Plot points
    p <- levels_scatterplot(
        df_=df_, x=x, y=y, levels=levels, size=points_size,
        xlab=xlab, ylab=ylab, title=title, colors=colors
    )


    # Create dummy DF for predictions
    dummy_df <- data.frame(
        tmp=seq(min(df_[[x]]), max(df_[[x]]), 
        by=1)
    )
    dummy_df[[x]] <- as.numeric(dummy_df$tmp)
    dummy_df[[y]] <- NULL
    dummy_df$tmp  <- NULL

    
    for (i in 1:length(levels_values)) {

        # Extract the level value
        value <- levels_values[i]

        # Compute predictions for that Borough
        dummy_df[[levels]] <- rep(value, nrow(dummy_df))
        dummy_df <- generate_predictions_with_ci(
            model=model, 
            newdata=dummy_df, 
            y=y, 
            level=level,
        )

        # Add lines to the plot
        p <- p + geom_line(
            data=dummy_df, 
            aes(x=.data[[x]], y=.data[[y]], color=.data[[levels]]), 
            linetype="dashed", 
            size=1.,
            
        )

        # Optionally add the confidence interval ribbon
        if (level > 0) {
            p <- p + geom_ribbon(
                data=dummy_df,
                aes(
                    x=.data[[x]], 
                    ymin=.data[["Lower"]], 
                    ymax=.data[["Upper"]],
                    fill=.data[[levels]]
                ),
                alpha=ALPHA_CH,
                color=NA,
                inherit.aes=FALSE
            )
        }

    }



    # Use scale_color_manual to specify the colors
    p <- p + scale_color_manual(values=colors, name=levels)
    p <- p + scale_fill_manual(values=colors, name=levels)
# 
    # # Show only the geom_line legend
    p <- p + guides(
        fill="none"
    )

    if(!legend) {
        p <- p + theme(legend.position = "none")
    }

    return(p)

}


plot_multiple_model_predictions_over_time <- function(
    df_, models, y,
    title="",
    ylab ="",
    level   =0.,
    lwd  =1,
    lwd_observations=1,
    alpha = 0.25,
    ticks=10,
    colors=c(),
    glmnet_predictors=c(),
    grid_rows=0
) {

    if(grid_rows == 0) {
        # Create the base plot
        p <- plot_ts(
            df_ts     =ts_crimes_reshape(df_),
            group     ="CrimeType",
            y         =y,
            ylab      =ylab,
            title     =title,
            col       ="black",
            lwd       =lwd_observations,
            ticks   =ticks
        )

        # Loop through each model and add the lines and ribbons
        for (i in 1:length(models)) {

            model_name <- names(models)[i]
            model      <- models[[model_name]]

            preds <- generate_predictions_with_ci(
                model=model, 
                newdata=df_, 
                y=y, 
                level=level
            )

            # Add the prediction line
            p <- p + geom_line(
                aes_string(y=preds[[y]], col=shQuote(model_name)),
                lwd=lwd,
                linetype="dashed"
            )

            # Optionally add the confidence interval ribbon
            if (level > 0) {
                p <- p + geom_ribbon(
                    aes_string(
                        ymin=preds$Lower, 
                        ymax=preds$Upper, 
                        fill=shQuote(model_name),
                        color=NULL
                    ),
                    alpha=ALPHA_CH
                )
            }

        }

        if(length(colors)>0) {
            p <- p + scale_color_manual(values=colors)
        }
        
        # Remove the fill from the legend
        p <- p + guides(fill = "none")
        
    
    } else {

        if(length(colors) == 0) {
            colors <- rainbow(length(models))
        }

        pp <- list()

        for(i in 1:length(models)) {

            model_name <- names(models)[i]
            model <- models[[model_name]]

            df_ts <- ts_crimes_reshape(df_)

            p <- plot_ts(
                df_ts=df_ts,
                group="CrimeType",
                y=y,
                ylab=ylab,
                title=model_name,
                col="black",
                lwd=lwd_observations,
                ticks=ticks
            )

            preds <- generate_predictions_with_ci(
                model=model, 
                newdata=df_, 
                y=y, 
                level=level
            )

            df_ts <- df_ts[df_ts$CrimeType == y,]
            df_ts$Count <- preds[[y]]

            p <- p + geom_line(
                data=df_ts, 
                aes(x=YearMonth, y=Count), 
                linetype="dashed",
                linewidth=lwd,
                color=colors[i]
            )
            
            if(level > 0) {
                p <- p + geom_ribbon(
                    aes_string(
                        ymin=preds$Lower, 
                        ymax=preds$Upper
                        #fill=colors[i]
                    ),
                    alpha=ALPHA_CH,
                    fill = colors[i]
                )
            }

            pp[[model_name]] <- p
        
        }

        # Arrange the plots in a grid
        p <- grid.arrange(
            grobs = pp,
            nrow = grid_rows,
            top = textGrob(
                title, 
                gp = gpar(fontsize = 15, fontface = "bold"))
        )

    }

    return(p)

}


plot_model_predictions_per_level_over_time <- function(
    model, df_, y, levels, 
    title="", 
    ylab ="",
    level = 0.,
    lwd  =1,
    lwd_observations=1,
    colors=c(),
    ticks=10,
    glmnet_predictors=c(),
    grid_rows=0
) {

    if(grid_rows == 0) {

        # Plot the true time series
        p <- plot_multiple_ts(

            df_ts <- ts_borough_reshape(
                df_=df_[, c("Year", "MonthName", "Borough", y)],
                crime_type="Shootings"
            ),
            group='Borough',
            xlab="Year",
            ylab=ylab,
            title=title,
            lwd=lwd_observations,
            ticks=ticks
        
        )

        level_values <- levels(df_[[levels]])
        
        for (level_name in level_values) {

            df_level   <- df_[df_[[levels]] == level_name, ]

            df_ts_level <- ts_borough_reshape(
                df_=df_level[, c("Year", "MonthName", "Borough", y)],
                crime_type="Shootings"
            )

            preds <- generate_predictions_with_ci(
                model=model, 
                newdata=df_level, 
                y=y,
                glmnet_predictors=glmnet_predictors
            )

            df_ts_level$Count <- preds[[y]]
            df_ts_level$Upper <- preds$Upper
            df_ts_level$Lower <- preds$Lower

            p <- p + geom_line(
                data=df_ts_level, 
                aes(x=YearMonth, y=Count, group=Borough, colour=Borough),
                linetype="dashed", linewidth=lwd
            )
            
            if(level > 0) {

                # add confidence interval geom_ribbon
                p <- p + geom_ribbon(
                    data=df_ts_level,
                    aes(
                        x=YearMonth, 
                        ymin=Lower, 
                        ymax=Upper,
                        fill=Borough
                    ),
                    alpha=ALPHA_CH,
                    colour=NA,
                    show.legend=FALSE
                )

            }

        }
        if (length(colors) > 0) {
            p <- p + scale_color_manual(values=colors)
            p <- p + scale_fill_manual(values=colors)
        }

    } else {

        levels_names <- levels(df_[[levels]])

        if(length(colors) == 0) {
            colors <- rainbow(length(levels_names))
        }

        # Precompute yrange with also predictions
        ranges=list()

        for(level_name in levels_names) {
            df_level=df_[df_[[levels]] == level_name, ]

            preds <- generate_predictions_with_ci(
                model=model, 
                newdata=df_level, 
                y=y,
                glmnet_predictors = glmnet_predictors
            )

            fitted <- preds[[y]]

            ranges[[level_name]] <- c(fitted, preds$Lower, preds$Upper)
        }

        # Create an empty list to store the plots
        pp <- list()

        yrange <- range(df_[[y]], ranges)

        # Loop through each level
        for(i in 1:length(levels_names)) {

            level_name <- levels_names[i]
            
            # Create the plot for the current level
            pp[[i]] <- plot_model_prediction_over_time(
                df_=df_[df_[[levels]] == level_name, ],
                model=model,
                x=x,
                y=y,
                col=colors[i],
                title=level_name,
                col_observations=colors[i],
                lwd=lwd,
                lwd_observations=lwd_observations,
                glmnet_predictors=glmnet_predictors,
                yrange=yrange,
                ticks=ticks,
                level=level
            )
        
        }

        # Arrange the plots in a grid
        p <- grid.arrange(
            grobs = pp,
            top = textGrob(
                title, 
                gp = gpar(fontsize = 15, fontface = "bold"))
        )
    
    }

    return(p)

}
