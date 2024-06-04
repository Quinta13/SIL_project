#' Plotting Functions
#'
#' This file contains generic plotting functions for data visualization.
#'
#' @author Sebastiano Quintavalle
#' @date 2024-05-15
#' @version 1.0
#'

# --- SCATTERPLOTS ---

#' Create a basic scatterplot
#'
#' This function creates a basic scatterplot using ggplot2 library.
#'
#' @param data The data frame containing the variables to be plotted.
#' @param x The name of the variable to be plotted on the x-axis.
#' @param y The name of the variable to be plotted on the y-axis.
#' @param size The size of the points in the scatterplot (default is 1).
#' @param shape The shape of the points in the scatterplot (default is 16).
#' @param color The color of the points in the scatterplot (default is "steelblue").
#' @param jitter_ A logical value indicating whether to add jitter to the x-axis (default is FALSE).
#' @param xlab The label for the x-axis (default is an empty string).
#' @param ylab The label for the y-axis (default is an empty string).
#'
#' @return A ggplot object representing the scatterplot.
#'
#' @examples
#' data <- data.frame(x = rnorm(100), y = rnorm(100))
#' basic_scatterplot(data, "x", "y")
#'
#'
#' @export
#' 
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
            x=ifelse(xlab == element_blank(), x, xlab), 
            y=ifelse(ylab == element_blank(), y, ylab),
            title=title
        )

    return(p)

}


#' Create a scatterplot with different levels
#'
#' This function creates a scatterplot with different levels based on a specified variable.
#'
#' @param df_ The data frame containing the variables.
#' @param x The name of the variable to be plotted on the x-axis.
#' @param y The name of the variable to be plotted on the y-axis.
#' @param levels The name of the variable used to determine the levels.
#' @param title The title of the scatterplot (default is an empty string).
#' @param legend_pos The position of the legend in the plot (default is "topright").
#' @param xlab The label for the x-axis (default is an empty string).
#' @param ylab The label for the y-axis (default is an empty string).
#' @param jitter_ A logical value indicating whether to add jitter to the x-axis (default is FALSE).
#' @param ticks The number of ticks on the x-axis (default is 10).
#' @param shape The shape of the points in the scatterplot (default is 16).
#' @param size The size of the points in the scatterplot (default is 1).
#' @param colors A vector of colors to be used for the levels (default is an empty vector).
#'
#' @return A ggplot object representing the scatterplot.
#'
#' @examples
#' df <- data.frame(
#'   x = rnorm(100),
#'   y = rnorm(100),
#'   levels = sample(c("A", "B", "C"), 100, replace = TRUE)
#' )
#' levels_scatterplot(df, "x", "y", "levels", title = "Scatterplot with Levels")
#'
#' @export 
levels_scatterplot <- function(
    df_, x, y, levels,
    title      = "", 
    title_size = 16,
    legend_pos = "topright",
    xlab       = NULL,
    ylab       = NULL,
    jitter_    = FALSE, 
    ticks      = 10,
    shape      = 16,
    size       = 1,
    colors     = c(),
    legend     = TRUE,
    legend.position  = "right",
    legend.direction = "vertical"
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
            x = xlab,
            y = ylab,
            title = title
        ) +
        scale_color_discrete(labels = levels(df_[[levels]])) +
        theme(
            axis.text.x = element_text(),
            axis.text.y = element_text(),
            legend.position = legend.position,
            legend.direction = legend.direction,
            plot.title = element_text(size = title_size)
        ) +
        scale_x_continuous(breaks = pretty_breaks(n = ticks))

    if(length(colors) > 0) {
        p <- p + scale_color_manual(values = colors)
    }

    if(!legend) {
        p <- p + theme(legend.position = "none")
    }

    if(is.null(xlab)) {
        p <- p + theme(axis.title.x = element_blank())
    }

    if(is.null(ylab)) {
        p <- p + theme(axis.title.y = element_blank())
    }

    return(p)
}

# --- TIME SERIES ---

#' Plot Time Series Data
#'
#' This function plots time series data based on the specified level and year.
#'
#' @param df_ts The data frame containing the time series data.
#' @param group The group of the data to filter on.
#' @param y The year to filter on.
#' @param xlab The label for the x-axis. Default is "Year".
#' @param ylab The label for the y-axis. If empty, the y parameter will be used as the label.
#' @param title The title of the plot.
#' @param col The color of the line. Default is "#4683b4a3".
#' @param lwd The line width. Default is 1.
#' @param ticks The number of ticks on the x-axis. Default is 10.
#' @param yrange The range of values for the y-axis. If empty, the range will be automatically determined.
#'
#' @return A ggplot object representing the plot.
#'
#' @examples
#' df <- data.frame(
#'   YearMonth = seq(as.Date("2022-01-01"), as.Date("2022-12-31"), by = "month"),
#'   Count = rnorm(12)
#' )
#' plot_ts(df, "Year", 2022)
#'
#' @export
#' 
plot_ts <- function(
    df_ts, group, y,
    xlab   = "Year",
    ylab   = "",
    title  = "",
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
        theme(axis.text.x=element_text()) +
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
        plot.title = element_text(size = title_size)
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
    ylab ="",
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
    ylab ="",
    jitter_=FALSE,
    colors=c(),
    points_size=1,
    lwd_observations = 1,
    lwd = 1,
    glmnet_predictors=c()
) {

    # Plot points
    p <- basic_scatterplot(
        data=df_, x=x, y=y, jitter_=jitter_,
        color='black', size=points_size
    )

    if(length(colors) == 0) {
    colors <- rainbow(length(models))
    }

# Loop over models to add prediction lines
for(i in 1:length(models)) {
  
  model_name <- names(models)[i]
  model <- models[[model_name]]
  
  # Create dummy DF for predictions
  dummy_df <- data.frame(tmp=seq(min(df_[[x]]), max(df_[[x]]), by=1))
  
  dummy_df[[x]] <- as.numeric(dummy_df$tmp)
  dummy_df$tmp <- NULL
  
  dummy_df[[y]] <- predict(model, newdata=dummy_df, type="response")
  dummy_df$label <- model_name  # Add label column for legend
  
  p <- p + geom_line(
    data=dummy_df, 
    aes_string(x=x, y=y, color="label"), 
    linetype="dashed", 
    linewidth=1
  )
}

# Use scale_color_manual to specify the colors
p <- p + scale_color_manual(values=colors, name="Model") +
  theme(legend.title = element_text(size=10), legend.text = element_text(size=8))


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
    ci=FALSE,
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
    stats <- get_prediction_stats(
        model=model, df_=df_, y=y,
        glmnet_predictors=glmnet_predictors
    )

    predictions <- stats$Pred
    error       <- stats$Std

    # Add the prediction line
    p <- p + geom_line(
        aes_string(y=predictions),
        lwd=lwd,
        color=col,
        linetype="dashed"
    )

    # Optionally add the confidence interval ribbon
    if (ci) {
        p <- p + geom_ribbon(
            aes_string(
                ymin=predictions - error, 
                ymax=predictions + error, 
            ),
            fill=col,
            alpha=0.22
        )
    }

    return(p)

}


plot_model_prediction_per_level <- function(
    df_, model, x, y, levels, 
    title="",
    ylab ="",
    glmnet_predictors=c()
) {

    # Plot points
    p <- levels_scatterplot(
        df_=df_, x=x, y=y, levels=levels, title=title
    )

    # Create dummy DF for predictions
    dummy_df <- data.frame(
        tmp=seq(min(df_[[x]]), max(df_[[x]]), 
        by=1)
    )
    dummy_df[[x]] <- as.numeric(dummy_df$tmp)
    dummy_df[[y]] <- NULL
    dummy_df$tmp  <- NULL

    levels_values <- levels(df_[[levels]])

    for (i in 1:length(levels_values)) {

        # Extract the level value
        value <- levels_values[i]

        # Compute predictions for that Borough
        dummy_df[[levels]] <- rep(value, nrow(dummy_df))
        dummy_df[[y]]      <- predict(model, newdata=dummy_df, type="response")
            
        # Add lines to the plot
        p <- p + geom_line(
            data=dummy_df, 
            aes(x=.data[[x]], y=.data[[y]], color=.data[[levels]]), 
            linetype="dashed", 
            size=1.
        )

    }

    return(p)

}


plot_multiple_model_predictions_over_time <- function(
    df_, models_stats, y,
    title="",
    ylab ="",
    ci   =FALSE,
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
        for (model_name in names(models_stats)) {
            
            model_stats <- models_stats[[model_name]]
            predictions <- model_stats$Pred
            error       <- model_stats$Std

            # Add the prediction line
            p <- p + geom_line(
                aes_string(y=predictions, col=shQuote(model_name)),
                lwd=lwd,
                linetype="dashed"
            )

            # Optionally add the confidence interval ribbon
            if (ci) {
                p <- p + geom_ribbon(
                    aes_string(
                        ymin=predictions - error, 
                        ymax=predictions + error, 
                        fill=shQuote(model_name)
                    ),
                    alpha=alpha
                )
            }

        }

        if(length(colors)>0) {
            p <- p + scale_color_manual(values=colors)
        }
        
        p <- p + guides(
            color=guide_legend(title="Model name"), 
            fill="none"
        )
    
    } else {

        if(length(colors) == 0) {
            colors <- rainbow(length(models_stats))
        }

        pp <- list()

        for(i in 1:length(models_stats)) {

            model_name <- names(models_stats)[i]

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

            stats = models_stats[[model_name]]
            predictions <- stats$Pred
            error <- stats$Std

            df_ts <- df_ts[df_ts$CrimeType == y,]
            df_ts$Count <- predictions

            p <- p + geom_line(
                data=df_ts, 
                aes(x=YearMonth, y=Count), 
                linetype="dashed",
                linewidth=lwd,
                color=colors[i]
            )
            
            if(ci) {
                p <- p + geom_ribbon(
                    aes_string(
                        ymin=predictions - error, 
                        ymax=predictions + error
                        #fill=colors[i]
                    ),
                    alpha=alpha,
                    fill = colors[i]
                )
            }

            pp[[model_name]] <- p
        
        }

        # Arrange the plots in a grid
        grid.arrange(
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
    ci   =FALSE,
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

            stats <- get_prediction_stats(
                model=model, df_=df_level, y=y,
                glmnet_predictors=glmnet_predictors
            )

            df_ts_level$Count <- stats$Pred

            p <- p + geom_line(
                data=df_ts_level, 
                aes(x=YearMonth, y=Count, group=Borough, colour=Borough),
                linetype="dashed", linewidth=lwd
            )
            
            if(ci) {

                # add confidence interval geom_ribbon
                p <- p + geom_ribbon(
                    data=df_ts_level,
                    aes(x=YearMonth, ymin=Count - stats$Std, ymax=Count + stats$Std, fill=Borough),
                    alpha=0.20,
                    colour=NA,
                    show.legend=FALSE
                )

            }

        }
        if (length(colors) > 0) {
            p <- p + scale_color_manual(values=colors)
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
            stats <-  get_prediction_stats(
                model=model, df_=df_level, y=y,
                glmnet_predictors=glmnet_predictors
            )
            preds  <- stats$Pred
            errors <- stats$Std
            ranges[[level_name]] <- range(preds) + c(-errors, +errors)
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
                title=paste(title, level_name),
                col_observations=colors[i],
                lwd=lwd,
                lwd_observations=lwd_observations,
                glmnet_predictors=glmnet_predictors,
                yrange=yrange,
                ticks=ticks,
                ci=ci
            )
        
        }

        # Arrange the plots in a grid
        p = do.call(grid.arrange, c(pp, nrow=grid_rows))
    
    }

    return(p)

}
