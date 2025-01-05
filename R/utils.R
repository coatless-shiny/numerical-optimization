# Ensure that the initial points are valid ----
validate_initial_points <- function(points_str, dims) {
  tryCatch({
    ## Split the string and convert to numeric
    vals <- as.numeric(strsplit(points_str, ",")[[1]])
    
    ## Check if the number of points match the dimensions
    if (length(vals) != dims) {
      return(list(valid = FALSE, message = "Number of points must match dimensions"))
    }
    
    ## Check if any of the values are NA
    if (any(is.na(vals))) {
      return(list(valid = FALSE, message = "Invalid number format"))
    }
    
    ## Return the valid points
    return(list(valid = TRUE, values = vals))
  }, error = function(e) {
    ## Return invalid if an error occurs
    return(list(valid = FALSE, message = "Invalid format"))
  })
}

# Custom function to format numeric output ----
format_numeric_output <- function(value, digits = 6) {
  sprintf(paste0("%.", digits, "g"), value)
}

