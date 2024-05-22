#' Time utils
#'
#' This file contains utility functions for working with time series data.
#'
#' @author Sebastiano Quintavalle
#' @date 2024-05-15
#' @version 1.0
#'

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
