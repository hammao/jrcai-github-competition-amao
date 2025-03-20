# =========================================================================
# xrd_import.R - XRD Data Import Functions
# 
# This script provides functions for importing and processing XRD data files
# in XRDML format using the rxylib package.
# =========================================================================

#' Import XRD data from XRDML files in a directory
#'
#' @param dir_path Directory path containing XRDML files (default: "data/XRD/")
#' @param pattern Regex pattern to match XRDML files (default: ".*\\.(XRDML|xrdml)$")
#' @param recursive Whether to search subdirectories (default: TRUE)
#' @param name_pattern Regex pattern to extract sample names (default: ".*?(J_[A-Za-z0-9_]+)_\\..*$")
#' @param verbose Whether to show progress messages (default: TRUE)
#' @param base_dir Base directory for relative paths (default: ".")
#' @return A tibble containing merged XRD patterns
#'
import_xrd_data <- function(dir_path = "data/XRD/", 
                           pattern = ".*\\.(XRDML|xrdml)$",
                           recursive = TRUE,
                           name_pattern = ".*?(J_[A-Za-z0-9_]+)_\\..*$",
                           verbose = TRUE,
                           base_dir = ".") {
  
  # Check if required packages are installed
  required_pkgs <- c("rxylib", "dplyr")
  missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
  
  if (length(missing_pkgs) > 0) {
    stop(paste("Missing required packages:", paste(missing_pkgs, collapse = ", "), 
              "\nPlease install with: install.packages(c('", 
              paste(missing_pkgs, collapse = "', '"), "'))", sep = ""))
  }
  
  # Load required packages
  library(rxylib)
  library(dplyr)
  
  # Create full path with base_dir
  full_dir_path <- file.path(base_dir, dir_path)
  
  # Check if directory exists
  if (!dir.exists(full_dir_path)) {
    stop(paste("Directory not found:", full_dir_path))
  }
  
  if (verbose) cat("Searching for XRD files in:", full_dir_path, "\n")
  
  # List files matching the pattern
  file_list <- list.files(
    path = full_dir_path,
    pattern = pattern,
    recursive = recursive, 
    ignore.case = TRUE, 
    include.dirs = TRUE, 
    full.names = TRUE
  )
  
  if (length(file_list) == 0) {
    # Try a more permissive pattern to help with debugging
    all_files <- list.files(path = full_dir_path, recursive = recursive, full.names = FALSE)
    if (length(all_files) > 0) {
      message("No files matching pattern, but found these files:")
      message(paste(" -", all_files, collapse = "\n"))
    }
    stop(paste("No files found matching pattern:", pattern, "in directory:", full_dir_path))
  }
  
  if (verbose) cat(paste("Found", length(file_list), "XRD files.\n"))
  
  # Extract sample names from filenames using the provided pattern
  sample_names <- gsub(name_pattern, "\\1", basename(file_list))
  
  if (verbose) {
    cat("Sample names extracted:\n")
    cat(paste(" -", sample_names, "\n"))
  }
  
  # Read the data
  if (verbose) cat("Reading XRD data...\n")
  
  xrd_list <- lapply(file_list, function(file) {
    if (verbose) cat(paste(" - Reading:", basename(file), "\n"))
    read_xyData(file, verbose = FALSE, metaData = FALSE)
  })
  
  # Extract data blocks
  data_blocks <- lapply(xrd_list, function(x) x$dataset[[1]]$data_block)
  
  # Rename columns with sample names
  data_blocks <- lapply(seq_along(data_blocks), function(i) {
    if (ncol(data_blocks[[i]]) >= 2) {
      colnames(data_blocks[[i]])[2] <- sample_names[i]
      return(data_blocks[[i]])
    } else {
      warning(paste("Data for", sample_names[i], "has fewer than 2 columns, skipping."))
      return(NULL)
    }
  })
  
  # Remove NULL entries (if any)
  data_blocks <- data_blocks[!sapply(data_blocks, is.null)]
  
  if (length(data_blocks) == 0) {
    stop("No valid data blocks found after processing.")
  }
  
  # Merge all data frames
  if (verbose) cat("Merging data...\n")
  
  merged_data <- Reduce(function(...) merge(..., by = "2Theta", all = TRUE), data_blocks)
  
  # Convert to tibble if tibble is available
  if (requireNamespace("tibble", quietly = TRUE)) {
    library(tibble)
    merged_data <- as_tibble(merged_data)
  }
  
  if (verbose) {
    cat("Import complete!\n")
    cat(paste("Dimensions:", nrow(merged_data), "rows,", ncol(merged_data), "columns\n"))
    cat(paste("2Theta range:", min(merged_data$`2Theta`), "to", max(merged_data$`2Theta`), "\n"))
  }
  
  return(merged_data)
}

#' Read a single XRD file
#'
#' @param file_path Path to the XRD file
#' @param sample_name Optional sample name to use as column name (default: derived from filename)
#' @param name_pattern Regex pattern to extract sample name from filename (default: ".*?(J_[A-Za-z0-9_]+)_\\..*$")
#' @param base_dir Base directory for relative paths (default: ".")
#' @return A data frame with 2Theta and intensity values
#'
read_single_xrd <- function(file_path, 
                           sample_name = NULL,
                           name_pattern = ".*?(J_[A-Za-z0-9_]+)_\\..*$",
                           base_dir = ".") {
  
  if (!requireNamespace("rxylib", quietly = TRUE)) {
    stop("Package 'rxylib' is required but not installed. Please install it with: install.packages('rxylib')")
  }
  
  library(rxylib)
  
  # Create full path with base_dir
  full_file_path <- file.path(base_dir, file_path)
  
  # Check if file exists
  if (!file.exists(full_file_path)) {
    stop(paste("File not found:", full_file_path))
  }
  
  # Determine sample name
  if (is.null(sample_name)) {
    sample_name <- gsub(name_pattern, "\\1", basename(full_file_path))
  }
  
  # Read the data
  xrd_data <- read_xyData(full_file_path, verbose = FALSE, metaData = FALSE)
  
  # Extract data block
  data_block <- xrd_data$dataset[[1]]$data_block
  
  # Rename columns
  if (ncol(data_block) >= 2) {
    colnames(data_block)[2] <- sample_name
  } else {
    warning("Data has fewer than 2 columns.")
  }
  
  return(data_block)
}

#' Extract and parse metadata from XRD files
#'
#' @param file_path Path to the XRD file
#' @param base_dir Base directory for relative paths (default: ".")
#' @return A list containing metadata elements
#'
extract_xrd_metadata <- function(file_path, base_dir = ".") {
  
  if (!requireNamespace("rxylib", quietly = TRUE)) {
    stop("Package 'rxylib' is required but not installed. Please install it with: install.packages('rxylib')")
  }
  
  # Create full path with base_dir
  full_file_path <- file.path(base_dir, file_path)
  
  if (!requireNamespace("xml2", quietly = TRUE)) {
    warning("Package 'xml2' is recommended for better metadata extraction but not installed.")
    # Fall back to basic metadata from rxylib
    library(rxylib)
    xrd_data <- read_xyData(full_file_path, verbose = FALSE, metaData = TRUE)
    return(xrd_data$metadata)
  }
  
  # Use xml2 for better metadata extraction from XRDML
  library(xml2)
  
  # Check if file exists
  if (!file.exists(full_file_path)) {
    stop(paste("File not found:", full_file_path))
  }
  
  # Check if it's an XML/XRDML file
  if (!grepl("\\.(xml|xrdml)$", full_file_path, ignore.case = TRUE)) {
    warning("File does not have XML/XRDML extension. Metadata extraction may fail.")
  }
  
  tryCatch({
    # Parse the XML
    xml_doc <- read_xml(full_file_path)
    
    # Extract common metadata elements
    metadata <- list(
      sample_id = xml_find_first(xml_doc, "//sample/id") %>% xml_text(),
      sample_name = xml_find_first(xml_doc, "//sample/name") %>% xml_text(),
      measurement_type = xml_find_first(xml_doc, "//xrdMeasurement/@measurementType") %>% xml_text(),
      status = xml_find_first(xml_doc, "//xrdMeasurement/@status") %>% xml_text(),
      wavelength = xml_find_first(xml_doc, "//usedWavelength/kAlpha1") %>% xml_text(),
      scan_axis = xml_find_first(xml_doc, "//scan/@scanAxis") %>% xml_text(),
      start_position = xml_find_first(xml_doc, "//positions[@axis='2Theta']/startPosition") %>% xml_text() %>% as.numeric(),
      end_position = xml_find_first(xml_doc, "//positions[@axis='2Theta']/endPosition") %>% xml_text() %>% as.numeric(),
      comments = xml_find_all(xml_doc, "//comment/entry") %>% xml_text()
    )
    
    return(metadata)
  }, error = function(e) {
    warning(paste("Error extracting metadata:", e$message))
    # Fall back to basic metadata
    library(rxylib)
    xrd_data <- read_xyData(full_file_path, verbose = FALSE, metaData = TRUE)
    return(xrd_data$metadata)
  })
}