# =========================================================================
# xrd_plotting.R - XRD Pattern Plotting Functions
# 
# This script provides functions for plotting XRD patterns with moving averages,
# minor axis breaks, and automatic color selection.
# 
# Usage from RMarkdown: 
#   source("R/xrd_plotting.R")
#   plot_xrd_patterns(your_data_frame)
# =========================================================================

#' Plot XRD patterns with optional moving average smoothing
#'
#' @param data A data frame containing the XRD patterns
#' @param theta_col Name of the column containing 2Theta values (default: "2Theta")
#' @param output_dir Directory to save plots (default: "output/xrd_plots")
#' @param ma_window Moving average window size (default: 7)
#' @param create_zip Whether to create a zip archive of plots (default: TRUE)
#' @param plot_height Height of saved plots in pixels (default: 600)
#' @param plot_width Width of saved plots in pixels (default: 800)
#' @param plot_res Resolution of saved plots (default: 120)
#' @param pattern_prefix Optional prefix to remove from pattern names for display (default: NULL)
#' @return Invisibly returns a list of file paths created
#'
plot_xrd_patterns <- function(data, 
                             theta_col = "2Theta",
                             output_dir = "output/xrd_plots",
                             ma_window = 7,
                             create_zip = TRUE,
                             plot_height = 600,
                             plot_width = 800,
                             plot_res = 120,
                             pattern_prefix = NULL) {
  
  # Check if required packages are installed
  required_pkgs <- c("RColorBrewer", "zoo", "Hmisc")
  missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
  
  if (length(missing_pkgs) > 0) {
    stop(paste("Missing required packages:", paste(missing_pkgs, collapse = ", "), 
              "\nPlease install with: install.packages(c('", 
              paste(missing_pkgs, collapse = "', '"), "'))", sep = ""))
  }
  
  # Load required packages
  library(RColorBrewer)
  library(zoo)
  library(Hmisc)
  
  # Ensure output directories exist
  raw_dir <- file.path(output_dir, "raw")
  smoothed_dir <- file.path(output_dir, "smoothed")
  combined_dir <- file.path(output_dir, "combined")
  zip_dir <- "output/zip"
  
  # Create output directories if they don't exist
  for (dir in c(output_dir, raw_dir, smoothed_dir, combined_dir, zip_dir)) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }
  }
  
  # Get the column names (excluding 2Theta/theta_col which is the x-axis)
  data_columns <- names(data)[names(data) != theta_col]
  
  # Create a color palette (avoiding yellow)
  spectral_colors <- brewer.pal(11, "Spectral")
  plot_colors <- spectral_colors[c(1:3, 8:11)]  # Remove yellows
  
  # If we need more colors than available, extend the palette
  if(length(data_columns) > length(plot_colors)) {
    extended_colors <- rep(plot_colors, ceiling(length(data_columns) / length(plot_colors)))
    plot_colors <- extended_colors[1:length(data_columns)]
  }
  
  # Track created files
  created_files <- list()
  
  # Loop through each data column and create individual plots
  for(i in seq_along(data_columns)) {
    # Extract the column name for this iteration
    col_name <- data_columns[i]
    
    # Clean the name for display (optionally remove prefix)
    display_name <- col_name
    if (!is.null(pattern_prefix)) {
      display_name <- gsub(pattern_prefix, "", display_name)
    }
    
    # Clean the name for file saving (remove special characters)
    clean_name <- gsub("[^a-zA-Z0-9]", "_", col_name)
    
    # Get the data and transform with sqrt
    x_data <- data[[theta_col]]
    y_data <- sqrt(as.numeric(data[[col_name]]))
    
    # Calculate moving average (with handling for NA values at edges)
    y_smooth <- rollmean(y_data, k = ma_window, fill = NA)
    
    # Replace NA values at the edges with original data
    na_indices <- which(is.na(y_smooth))
    y_smooth[na_indices] <- y_data[na_indices]
    
    # Create plot with raw data and moving average overlay (save to raw dir)
    file_path1 <- file.path(raw_dir, paste0(clean_name, ".png"))
    create_xrd_plot(x_data, y_data, y_smooth, display_name, plot_colors[i], 
                    file_path1, plot_width, plot_height, plot_res, overlay = TRUE)
    created_files <- c(created_files, file_path1)
    
    # Create plot with only the moving average (save to smoothed dir)
    file_path2 <- file.path(smoothed_dir, paste0(clean_name, "_smoothed.png"))
    create_xrd_plot(x_data, y_data, y_smooth, paste0(display_name, " (Smoothed)"), 
                    plot_colors[i], file_path2, plot_width, plot_height, plot_res, 
                    overlay = FALSE)
    created_files <- c(created_files, file_path2)
  }
  
  # Create combined plots
  numeric_data <- as.data.frame(lapply(data[data_columns], as.numeric))
  y_max <- max(sqrt(unlist(numeric_data)), na.rm = TRUE)
  
  # Track all y_smooth data for the combined plots
  all_y_smooth <- list()
  all_y_data <- list()
  
  for(i in seq_along(data_columns)) {
    col_name <- data_columns[i]
    y_data <- sqrt(as.numeric(data[[col_name]]))
    y_smooth <- rollmean(y_data, k = ma_window, fill = NA)
    na_indices <- which(is.na(y_smooth))
    y_smooth[na_indices] <- y_data[na_indices]
    
    all_y_data[[i]] <- y_data
    all_y_smooth[[i]] <- y_smooth
  }
  
  # Create combined plot with both raw and smoothed data
  file_path3 <- file.path(combined_dir, "Combined_XRD_Patterns.png")
  create_combined_plot(data[[theta_col]], all_y_data, all_y_smooth, 
                      data_columns, plot_colors, y_max, file_path3,
                      1200, 800, plot_res, pattern_prefix, overlay = TRUE)
  created_files <- c(created_files, file_path3)
  
  # Create combined plot with only smoothed data
  file_path4 <- file.path(combined_dir, "Combined_XRD_Patterns_Smoothed.png")
  create_combined_plot(data[[theta_col]], all_y_data, all_y_smooth, 
                      data_columns, plot_colors, y_max, file_path4,
                      1200, 800, plot_res, pattern_prefix, overlay = FALSE)
  created_files <- c(created_files, file_path4)
  
  # Compress and zip the output directory if requested
  if (create_zip) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    zip_filename <- file.path(zip_dir, paste0("XRD_plots_", timestamp, ".zip"))
    
    # Get all files from subdirectories
    files_to_zip <- c(
      list.files(raw_dir, full.names = TRUE),
      list.files(smoothed_dir, full.names = TRUE),
      list.files(combined_dir, full.names = TRUE)
    )
    
    # Create zip archive
    zip(zipfile = zip_filename, files = files_to_zip)
    cat("XRD plots have been compressed and saved as", zip_filename, "\n")
    created_files <- c(created_files, zip_filename)
  }
  
  cat("Created", length(created_files), "files.\n")
  cat("- Raw plots:", length(list.files(raw_dir)), "files in", raw_dir, "\n")
  cat("- Smoothed plots:", length(list.files(smoothed_dir)), "files in", smoothed_dir, "\n")
  cat("- Combined plots:", length(list.files(combined_dir)), "files in", combined_dir, "\n")
  if (create_zip) cat("- Zip archive saved to:", zip_filename, "\n")
  
  invisible(created_files)
}

# Helper function to create a single XRD plot
create_xrd_plot <- function(x_data, y_data, y_smooth, title, color, 
                           file_path, width, height, resolution,
                           overlay = TRUE) {
  
  # Set up the plot
  png(file_path, width = width, height = height, res = resolution)
  par(mar = c(5, 5, 4, 2) + 0.1)  # Increase margins for axis labels
  
  if (overlay) {
    # Plot raw data first
    plot(x_data, y_data,
         type = "l",
         col = adjustcolor(color, alpha.f = 0.5),  # Semi-transparent
         lwd = 1.5,
         xlab = expression(paste("2", theta^o)),
         ylab = expression(sqrt("Intensity")),
         main = paste("XRD Pattern:", title))
    
    # Add moving average line
    lines(x_data, y_smooth, 
          col = color, 
          lwd = 2.5)
    
    # Add a legend
    legend("topright", 
           legend = c("Raw Data", "Moving Average"),
           col = c(adjustcolor(color, alpha.f = 0.5), color),
           lwd = c(1.5, 2.5),
           cex = 0.8,
           bty = "n")
  } else {
    # Plot only smoothed data
    plot(x_data, y_smooth,
         type = "l",
         col = color,
         lwd = 2.5,
         xlab = expression(paste("2", theta^o)),
         ylab = expression(sqrt("Intensity")),
         main = paste("XRD Pattern:", title))
  }
  
  # Add minor breaks to both axes
  minor.tick(nx = 4, ny = 4, tick.ratio = 0.5)
  
  # Close the plotting device
  dev.off()
}

# Helper function to create a combined XRD plot
create_combined_plot <- function(x_data, all_y_data, all_y_smooth, 
                               data_columns, plot_colors, y_max, 
                               file_path, width, height, resolution,
                               pattern_prefix = NULL, overlay = TRUE) {
  
  # Clean display names
  display_names <- data_columns
  if (!is.null(pattern_prefix)) {
    display_names <- gsub(pattern_prefix, "", display_names)
  }
  
  # Set up the plot
  png(file_path, width = width, height = height, res = resolution)
  par(mar = c(5, 5, 4, 2) + 0.1)  # Increase margins for axis labels
  
  # Create empty plot
  plot(x_data, rep(NA, length(x_data)),
       type = "n",  # Empty plot
       ylim = c(0, y_max * 1.05),  # Add 5% margin at top
       xlab = expression(paste("2", theta^o)),
       ylab = expression(sqrt("Intensity")),
       main = ifelse(overlay, "Combined XRD Patterns", "Combined XRD Patterns (Smoothed)"))
  
  # Add minor breaks to both axes
  minor.tick(nx = 4, ny = 4, tick.ratio = 0.5)
  
  # Add each spectrum
  for(i in seq_along(data_columns)) {
    # If overlay is TRUE, add both raw and smoothed data
    if (overlay) {
      # Add raw data (semi-transparent)
      lines(x_data, all_y_data[[i]],
            col = adjustcolor(plot_colors[i], alpha.f = 0.3),
            lwd = 1)
    }
    
    # Add smoothed data
    lines(x_data, all_y_smooth[[i]],
          col = plot_colors[i],
          lwd = 2.5)
  }
  
  # Add a legend with pattern names
  legend("topright", 
         legend = display_names,
         col = plot_colors,
         lwd = 2.5,
         cex = 0.7,  # Smaller text for readability
         bty = "n")  # No box around legend
  
  # Close the plotting device
  dev.off()
}

# Helper function to get good contrast colors
get_contrast_colors <- function(n) {
  if (n <= 8) {
    return(brewer.pal(8, "Dark2")[1:n])
  } else {
    return(colorRampPalette(brewer.pal(8, "Dark2"))(n))
  }
}