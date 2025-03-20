# Setup project directory structure
# This script creates the directory structure relative to its own location
# And moves XRD files to the correct location

# Get the directory where this script is located
script_dir <- tryCatch({
  dirname(rstudioapi::getSourceEditorContext()$path)
}, error = function(e) {
  # If not in RStudio or can't get path
  "."  # Current directory
})

# Create relative directory paths
dirs <- c(
  file.path(script_dir, "R"),                        # Scripts directory
  file.path(script_dir, "data"),                     # Data directory
  file.path(script_dir, "data/XRD"),                 # XRD data files
  file.path(script_dir, "output"),                   # Main output directory
  file.path(script_dir, "output/figures"),           # Figures directory
  file.path(script_dir, "output/CSV"),               # Extracted Data directory
  file.path(script_dir, "output/xrd_plots"),         # XRD plots main directory
  file.path(script_dir, "output/xrd_plots/raw"),     # Raw XRD plots
  file.path(script_dir, "output/xrd_plots/smoothed"),# Smoothed XRD plots
  file.path(script_dir, "output/xrd_plots/combined"),# Combined XRD plots
  file.path(script_dir, "output/zip")                # Zip archives
)

# Create directories if they don't exist
for (dir in dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat("Created directory:", dir, "\n")
  } else {
    cat("Directory already exists:", dir, "\n")
  }
}

# Move R scripts to R/ directory if they exist in the script's directory
scripts <- c("xrd_import.R", "xrd_data_transform.R", "xrd_plotting.R", "colors.R")

for (script in scripts) {
  source_file <- file.path(script_dir, script)
  target_file <- file.path(script_dir, "R", script)
  
  if (file.exists(source_file)) {
    file.copy(source_file, target_file, overwrite = FALSE)
    cat("Copied", script, "to R/ directory\n")
  } else {
    cat("Script not found in current directory:", source_file, "\n")
  }
}

# Check for XRD directory in the script_dir
xrd_original_dir <- file.path(script_dir, "XRD")
xrd_target_dir <- file.path(script_dir, "data", "XRD")

if (dir.exists(xrd_original_dir)) {
  cat("\nFound XRD directory in main folder!\n")
  
  # Find XRD files (with various potential extensions)
  xrd_files <- list.files(
    path = xrd_original_dir, 
    pattern = "\\.xrdml$|\\.XRDML$|\\.xy$|\\.XY$|\\.txt$|\\.TXT$", 
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  # If no files found with specific patterns, try getting all files
  if (length(xrd_files) == 0) {
    cat("No XRD files found with standard extensions, checking for any files...\n")
    xrd_files <- list.files(
      path = xrd_original_dir,
      recursive = TRUE,
      full.names = TRUE
    )
  }
  
  # Copy XRD files to the data/XRD directory
  if (length(xrd_files) > 0) {
    cat("Found", length(xrd_files), "files in XRD directory. Moving to data/XRD/...\n")
    
    # Ensure target directory exists
    if (!dir.exists(xrd_target_dir)) {
      dir.create(xrd_target_dir, recursive = TRUE)
    }
    
    # Copy each file, preserving directory structure if recursive
    for (file in xrd_files) {
      # Get relative path from original XRD dir
      rel_path <- substring(file, nchar(xrd_original_dir) + 2)
      
      # Create target path
      target_path <- file.path(xrd_target_dir, rel_path)
      
      # Create directories if needed
      target_dir <- dirname(target_path)
      if (!dir.exists(target_dir)) {
        dir.create(target_dir, recursive = TRUE)
      }
      
      # Copy the file
      file.copy(file, target_path, overwrite = FALSE)
      cat(" - Copied:", rel_path, "\n")
    }
    
    cat("All files moved to data/XRD/ successfully!\n")
  } else {
    cat("No files found in XRD directory.\n")
  }
} else {
  cat("\nNo XRD directory found in main folder.\n")
  cat("Please manually add your XRD files to the data/XRD/ directory.\n")
}

# Create a simple README file
readme_content <- "# XRD Analysis Project\n\nThis directory contains a self-contained XRD analysis project.\n\n## Structure\n\n- `R/`: R scripts for XRD data analysis\n- `data/XRD/`: XRD data files\n- `output/`: Output files\n- `J_0.Rmd`: Main analysis document\n"
writeLines(readme_content, file.path(script_dir, "README.md"))

cat("\nProject setup complete! You can now edit your Rmd file to use the new structure.\n")
cat("Note: All paths in scripts should be relative to this project directory.\n")