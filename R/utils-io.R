#' Internal utility: Coerce input to x,y,z data.frame
#' 
#' @description
#' Converts various input formats (file path, data.frame, matrix, vector) 
#' into a standardized 3-column data.frame with columns x, y, z.
#' 
#' @param a Input data - can be:
#'   \itemize{
#'     \item Character string: path to .xyz/.txt file
#'     \item Data.frame or matrix: with at least 3 columns
#'     \item Numeric vector: flat sequence of x,y,z values
#'   }
#' 
#' @return Data.frame with columns x, y, z (numeric)
#' 
#' @keywords internal
#' @noRd
.coerce_to_xyz <- function(a) {
  if (is.null(a)) {
    stop("Input 'a' is NULL; expected data.frame/matrix/vector or path to .xyz/.txt file")
  }
  
  # Case 1: File path
  if (is.character(a) && length(a) == 1 && file.exists(a)) {
    return(.read_xyz_spaces(a))
  }
  
  # Case 2: Data.frame or matrix (>= 3 columns)
  if (is.data.frame(a) || is.matrix(a)) {
    if (ncol(a) < 3) {
      stop("Input must have at least 3 columns (x,y,z).")
    }
    df <- as.data.frame(a[, 1:3, drop = FALSE])
    colnames(df) <- c("x", "y", "z")
    df[] <- lapply(df, function(col) suppressWarnings(as.numeric(col)))
    
    if (any(!is.finite(df$x) | !is.finite(df$y) | !is.finite(df$z))) {
      stop("Non-numeric values found in x/y/z; check decimal separators.")
    }
    if (nrow(df) == 0) {
      stop("Input 'a' has 0 rows.")
    }
    return(df)
  }
  
  # Case 3: Flat vector (x,y,z sequences)
  if (is.atomic(a) && is.vector(a)) {
    n <- length(a)
    if (n %% 3 != 0) {
      stop("Vector length must be multiple of 3 (x,y,z sequences).")
    }
    df <- data.frame(
      x = as.numeric(a[seq(1, n, 3)]),
      y = as.numeric(a[seq(2, n, 3)]),
      z = as.numeric(a[seq(3, n, 3)])
    )
    if (any(!is.finite(df$x) | !is.finite(df$y) | !is.finite(df$z))) {
      stop("Non-numeric values in input vector.")
    }
    return(df)
  }
  
  stop("Unsupported input type for 'a': ", paste(class(a), collapse = ", "))
}


#' Internal utility: Read XYZ file with space/tab separators
#' 
#' @description
#' Reads point cloud files with flexible separator handling.
#' Attempts space-delimited first, falls back to auto-detection.
#' 
#' @param path Character string with file path
#' 
#' @return Data.frame with columns x, y, z (numeric)
#' 
#' @keywords internal
#' @noRd
.read_xyz_spaces <- function(path) {
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }
  
  # First attempt: 3 numeric columns separated by spaces
  dt <- tryCatch(
    data.table::fread(
      path,
      sep = " ",             # spaces (handles multiple consecutive)
      header = FALSE,
      data.table = FALSE,
      showProgress = FALSE,
      strip.white = TRUE,
      colClasses = c("numeric", "numeric", "numeric"),
      select = 1:3,          # keep only first 3 columns
      fill = TRUE,           # don't crash on short rows
      quote = ""
    ),
    error = function(e) NULL
  )
  
  # Fallback: auto-separator if file has mixed delimiters
  if (is.null(dt)) {
    dt <- tryCatch(
      data.table::fread(
        path,
        sep = "auto",
        header = FALSE,
        data.table = FALSE,
        showProgress = FALSE,
        strip.white = TRUE,
        select = 1:3,
        fill = TRUE,
        quote = ""
      ),
      error = function(e) NULL
    )
  }
  
  if (is.null(dt)) {
    stop("Unable to read file: ", path)
  }
  
  df <- as.data.frame(dt[, 1:3, drop = FALSE])
  colnames(df) <- c("x", "y", "z")
  
  # Robust numeric coercion (in case fallback read strings)
  df[] <- lapply(df, function(col) suppressWarnings(as.numeric(col)))
  
  if (nrow(df) == 0) {
    stop("File is empty: ", path)
  }
  
  if (any(!is.finite(df$x) | !is.finite(df$y) | !is.finite(df$z))) {
    bad <- which(!is.finite(df$x) | !is.finite(df$y) | !is.finite(df$z))
    stop(
      "Non-numeric/NA values in x,y,z (example rows: ", 
      paste(head(bad, 5), collapse = ", "), "). ",
      "Check separators, decimal points, or corrupted rows."
    )
  }
  
  df
}