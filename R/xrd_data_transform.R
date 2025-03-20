# =========================================================================
# xrd_data_transform.R - XRD Data Transformation
# 
# This script provides simple functions for transforming XRD data, with
# auto offsets as the default and easy options for custom offsets.
# =========================================================================

#' Transform XRD data with auto or custom offsets
#'
#' @param data A data frame containing XRD patterns
#' @param custom_offsets Named vector of custom offsets (e.g., c("J_DI_250_AD_" = 200))
#'        If provided, these exact offsets will be used.
#'        If NULL (default), auto offsets will be applied.
#' @param auto_step Step size for auto offset calculation (default: 300)
#' @param auto_adjust_percent Percentage to adjust auto step (can be negative)
#'        For example, -50 will use half the auto_step, +100 will double it.
#' @param theta_col Name of the column containing 2Theta values (default: "2Theta")
#' @param remove_trailing Whether to remove trailing underscore from column names (default: TRUE)
#' @param order_method How to order samples: "as_is", "alphabetical", "reverse" (default: "reverse")
#' @param long_format Whether to return data in long format for ggplot (default: FALSE)
#' @return Transformed data frame
#'
transform_xrd_data <- function(data, 
                              custom_offsets = NULL,
                              auto_step = 300,
                              auto_adjust_percent = 0,
                              theta_col = "2Theta",
                              remove_trailing = TRUE,
                              order_method = "reverse",
                              long_format = FALSE) {
  
  # Get data columns (excluding theta column)
  data_columns <- names(data)[names(data) != theta_col]
  
  # Generate new column names (with trailing underscore removed if specified)
  if (remove_trailing) {
    new_column_names <- sapply(data_columns, function(col) {
      if (endsWith(col, "_")) {
        return(substr(col, 1, nchar(col) - 1))
      } else {
        return(col)
      }
    })
  } else {
    new_column_names <- data_columns
  }
  
  # Determine if we're using custom or auto offsets
  using_custom_offsets <- !is.null(custom_offsets)
  
  if (using_custom_offsets) {
    # Check if all specified offsets exist in the data
    missing_cols <- names(custom_offsets)[!names(custom_offsets) %in% data_columns]
    if (length(missing_cols) > 0) {
      warning(paste("Some columns in custom_offsets don't exist in data:", 
                    paste(missing_cols, collapse=", ")))
    }
    
    # Only keep offsets for columns that exist in the data
    custom_offsets <- custom_offsets[names(custom_offsets) %in% data_columns]
    
    # For any columns without custom offsets, use auto offset
    missing_data_cols <- data_columns[!data_columns %in% names(custom_offsets)]
    if (length(missing_data_cols) > 0) {
      # Calculate the adjusted auto step
      adjusted_step <- auto_step * (1 + auto_adjust_percent/100)
      
      # Create auto offsets for missing columns
      start_offset <- ifelse(length(custom_offsets) > 0, 
                            max(custom_offsets) + adjusted_step, 
                            0)
      auto_offsets <- seq(from = start_offset, 
                         by = adjusted_step, 
                         length.out = length(missing_data_cols))
      names(auto_offsets) <- missing_data_cols
      
      # Combine custom and auto offsets
      offsets <- c(custom_offsets, auto_offsets)
      offsets <- offsets[data_columns]  # Ensure correct order
    } else {
      # All columns have custom offsets
      offsets <- custom_offsets[data_columns]  # Ensure correct order
    }
  } else {
    # Using auto offsets for all columns
    # Calculate the adjusted auto step
    adjusted_step <- auto_step * (1 + auto_adjust_percent/100)
    
    # Create sequential offsets
    offsets <- seq(0, by = adjusted_step, length.out = length(data_columns))
    names(offsets) <- data_columns
  }
  
  # Create a new data frame for the transformed data
  result <- data.frame(data[[theta_col]])
  colnames(result) <- c("two_theta")
  
  # Add each transformed column
  for (i in seq_along(data_columns)) {
    col_name <- data_columns[i]
    new_name <- new_column_names[i]
    result[[new_name]] <- data[[col_name]] + offsets[col_name]
  }
  
  # Determine column order based on order_method
  ordered_columns <- switch(order_method,
    "as_is" = new_column_names,
    "alphabetical" = sort(new_column_names),
    "reverse" = sort(new_column_names, decreasing = TRUE),
    # Default to as_is if invalid method
    new_column_names
  )
  
  # Convert to long format if requested
  if (long_format) {
    require(tidyr)
    require(dplyr)
    
    long_data <- result %>%
      pivot_longer(-two_theta, names_to = "samples", values_to = "intensities") %>%
      mutate(samples = factor(samples, levels = ordered_columns))
    
    return(long_data)
  } else {
    # Return wide format but with columns in the desired order
    result_ordered <- result[, c("two_theta", ordered_columns)]
    return(result_ordered)
  }
}

# ===========================================================================
# USAGE INSTRUCTIONS
# ===========================================================================
#
# CASE 1: DEFAULT AUTO OFFSETS
# ----------------------------
# The simplest way to use this function is with all default parameters:
#
# # For wide format (original data structure)
# transformed_data <- transform_xrd_data(
#   data = alldat2
# )
#
# # For long format (ready for ggplot)
# transformed_data <- transform_xrd_data(
#   data = alldat2,
#   long_format = TRUE
# )
#
# This will:
# - Apply auto offsets with a step size of 300
# - Remove trailing underscores from column names
# - Order columns in reverse alphabetical order
# - Return data in wide or long format as requested
#
# CASE 2: ADJUSTING AUTO OFFSET SIZE
# ----------------------------------
# To make auto offsets smaller or larger:
#
# # Make auto offsets 50% smaller (better for comparison)
# transformed_data <- transform_xrd_data(
#   data = alldat2,
#   auto_adjust_percent = -50,
#   long_format = TRUE     # Ready for ggplot
# )
#
# # Make auto offsets twice as large
# transformed_data <- transform_xrd_data(
#   data = alldat2,
#   auto_adjust_percent = 100,
#   long_format = TRUE     # Ready for ggplot
# )
#
# CASE 3: USING CUSTOM OFFSETS
# ----------------------------
# To specify exact offset values for some or all columns:
#
# # Define offsets for specific columns
# my_offsets <- c(
#   "J_DI_250_AD_" = 200,  # Column name must exactly match data
#   "J_DI_250_HT_" = 700,
#   "J_DI_250_GL_" = 1000
# )
#
# # Get data ready for ggplot with custom offsets
# transformed_data <- transform_xrd_data(
#   data = alldat2,
#   custom_offsets = my_offsets,
#   long_format = TRUE     # Ready for ggplot
# )
#
# NOTE: Any columns not specified in custom_offsets will still 
# get auto offsets. Auto offsets will start from (highest custom offset + auto_step).
#
# CASE 4: USING WITH GGPLOT
# -------------------------
# Simply add long_format = TRUE to any of the above examples:
#
# transformed_data <- transform_xrd_data(
#   data = alldat2,
#   auto_adjust_percent = -80,  # Smaller spacing for better comparison
#   long_format = TRUE          # Ready for ggplot
# )
#
# # Then use with ggplot
# ggplot(transformed_data, aes(x = two_theta, y = intensities, color = samples)) +
#   geom_line(size = 1)