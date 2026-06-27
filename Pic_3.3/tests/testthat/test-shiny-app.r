# Test file for PiC Shiny Application Updates
# This should be placed in tests/testthat/test-shiny-app.R

library(testthat)

# Helper function to get path to shiny app files
get_shiny_path <- function(filename) {
  # Try to find the shiny app directory
  possible_paths <- c(
    # When testing installed package
    system.file("shinyapp", filename, package = "PiC"),
    # When testing in development
    file.path("inst", "shinyapp", filename),
    # When running from package root
    file.path("..", "..", "inst", "shinyapp", filename)
  )
  
  for (path in possible_paths) {
    if (file.exists(path) && nchar(path) > 0) {
      return(path)
    }
  }
  
  skip(paste("Could not find shiny app file:", filename))
}

# Test 1: Cross-platform volume detection
test_that("Cross-platform volume detection works", {
  skip_if_not(requireNamespace("fs", quietly = TRUE), "fs package not available")
  
  # Source the global.R file
  global_path <- get_shiny_path("global.R")
  source(global_path, local = TRUE)
  
  volumes <- get_system_volumes()
  
  expect_true(is.list(volumes) || is.character(volumes))
  expect_true(length(volumes) > 0)
  expect_true("Home" %in% names(volumes))
  
  # Platform-specific checks
  if (.Platform$OS.type == "windows") {
    expect_true("Documents" %in% names(volumes))
  } else {
    expect_true("Root" %in% names(volumes))
  }
})

# Test 2: Path normalization
test_that("Path normalization is cross-platform", {
  test_paths <- c(
    "C:\\Users\\Test\\Documents",
    "/home/user/documents",
    "~/Desktop/analysis"
  )
  
  for (path in test_paths) {
    # Skip invalid paths for current OS
    if (.Platform$OS.type == "windows" && grepl("^/", path)) next
    if (.Platform$OS.type != "windows" && grepl("^[A-Z]:", path)) next
    
    normalized <- normalizePath(path, winslash = "/", mustWork = FALSE)
    expect_true(is.character(normalized))
    expect_false(grepl("\\\\", normalized))
  }
})

# Test 3: File validation
test_that("File validation helper works", {
  global_path <- get_shiny_path("global.R")
  source(global_path, local = TRUE)
  
  # Test with tempdir (should exist)
  expect_true(validate_path(tempdir()))
  
  # Test with invalid path
  expect_false(validate_path("/nonexistent/path/that/does/not/exist"))
  
  # Test with empty string
  expect_false(validate_path(""))
})

# Test 4: File size formatting
test_that("File size formatting works", {
  global_path <- get_shiny_path("global.R")
  source(global_path, local = TRUE)
  
  expect_equal(format_file_size(512), "512 B")
  expect_match(format_file_size(2048), "KB")
  expect_match(format_file_size(2048 * 1024), "MB")
  expect_match(format_file_size(2048 * 1024 * 1024), "GB")
})

# Test 5: Function list structure
test_that("Function list has correct structure", {
  global_path <- get_shiny_path("global.R")
  source(global_path, local = TRUE)
  
  expect_true(exists("function_list"))
  expect_true(is.list(function_list))
  
  # Check required functions
  required_functions <- c("Forest_seg", "SegOne", "Floseg", "Voxels")
  for (func in required_functions) {
    expect_true(func %in% names(function_list),
                info = paste(func, "not found in function_list"))
    expect_true("description" %in% names(function_list[[func]]),
                info = paste(func, "missing description"))
    expect_true("params" %in% names(function_list[[func]]),
                info = paste(func, "missing params"))
  }
})

# Test 6: Parameter validation
test_that("Parameters have required fields", {
  global_path <- get_shiny_path("global.R")
  source(global_path, local = TRUE)
  
  for (func_name in names(function_list)) {
    params <- function_list[[func_name]]$params
    
    for (param_name in names(params)) {
      param <- params[[param_name]]
      
      # Check required fields
      expect_true("type" %in% names(param), 
                  info = paste(func_name, param_name, "missing type"))
      expect_true("label" %in% names(param),
                  info = paste(func_name, param_name, "missing label"))
      
      # Check type-specific fields
      if (param$type == "numeric") {
        expect_true("default" %in% names(param),
                    info = paste(func_name, param_name, "missing default"))
      }
    }
  }
})

# Test 7: Directory creation and permissions
test_that("Directory operations work correctly", {
  # Create a test directory in temp
  test_dir <- file.path(tempdir(), "pic_test", "subdir")
  
  # Clean up if exists from previous run
  if (dir.exists(test_dir)) unlink(test_dir, recursive = TRUE)
  
  expect_false(dir.exists(test_dir))
  
  # Test recursive creation
  dir.create(test_dir, recursive = TRUE)
  expect_true(dir.exists(test_dir))
  
  # Test write permissions
  test_file <- file.path(test_dir, ".write_test")
  can_write <- tryCatch({
    writeLines("test", test_file)
    file.remove(test_file)
    TRUE
  }, error = function(e) FALSE)
  
  expect_true(can_write)
  
  # Cleanup
  unlink(file.path(tempdir(), "pic_test"), recursive = TRUE)
})

# Test 8: File listing and sorting
test_that("File listing and sorting works", {
  # Create test files with different timestamps
  test_dir <- file.path(tempdir(), "pic_file_test")
  dir.create(test_dir, showWarnings = FALSE)
  
  # Create test files
  file1 <- file.path(test_dir, "old_file.txt")
  file2 <- file.path(test_dir, "new_file.txt")
  file3 <- file.path(test_dir, "middle_file.csv")
  
  writeLines("test1", file1)
  Sys.sleep(0.1)
  writeLines("test2", file3)
  Sys.sleep(0.1)
  writeLines("test3", file2)
  
  # List and sort files
  files <- list.files(test_dir, pattern = "\\.(txt|csv)$", full.names = FALSE)
  expect_equal(length(files), 3)
  
  file_info <- file.info(file.path(test_dir, files))
  files_sorted <- files[order(file_info$mtime, decreasing = TRUE)]
  
  # Most recent should be first
  expect_equal(files_sorted[1], "new_file.txt")
  
  # Cleanup
  unlink(test_dir, recursive = TRUE)
})

# Test 9: Server file exists and has key functions
test_that("Server file structure is correct", {
  server_path <- get_shiny_path("server.R")
  expect_true(file.exists(server_path))
  
  # Read server file
  server_content <- readLines(server_path, warn = FALSE)
  server_text <- paste(server_content, collapse = "\n")
  
  # Check for key functions and patterns
  expect_true(grepl("app_server", server_text),
              info = "app_server function not found")
  expect_true(grepl("update_file_list", server_text),
              info = "update_file_list function not found")
  expect_true(grepl('param_values\\[\\["output_path"\\]\\]', server_text),
              info = "output_path injection not found")
  expect_true(grepl("observeEvent\\(input\\$run_function", server_text),
              info = "run_function observer not found")
})

# Test 10: UI file exists and has required elements
test_that("UI file structure is correct", {
  ui_path <- get_shiny_path("ui.R")
  expect_true(file.exists(ui_path))
  
  # Read UI file
  ui_content <- readLines(ui_path, warn = FALSE)
  ui_text <- paste(ui_content, collapse = "\n")
  
  # Check for key UI elements
  expect_true(grepl("app_ui", ui_text),
              info = "app_ui not found")
  expect_true(grepl("function_select", ui_text),
              info = "function_select input not found")
  expect_true(grepl("output_path", ui_text),
              info = "output_path input not found")
  expect_true(grepl("output_file", ui_text),
              info = "output_file selector not found")
  expect_true(grepl("point_cloud", ui_text),
              info = "point_cloud output not found")
})