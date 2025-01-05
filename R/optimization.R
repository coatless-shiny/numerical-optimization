# Shim for the optimization functions in R ----
advanced_optim <- function(par, fn, method, runs = 1, progress = NULL, control = list()) {
  # Store results in a list
  results <- list()
  
  # Method-specific control parameter adjustments
  adjusted_control <- control
  
  # Handle method-specific parameters
  if (method == "L-BFGS-B") {
    # Convert reltol to factr if provided
    if (!is.null(control$reltol)) {
      adjusted_control$factr <- control$reltol * 1e7
      adjusted_control$reltol <- NULL
    }
    # Set default factr if neither provided
    if (is.null(adjusted_control$factr)) {
      adjusted_control$factr <- 1e7
    }
    # Set default pgtol if not provided
    if (is.null(adjusted_control$pgtol)) {
      adjusted_control$pgtol <- 1e-8
    }
  } else if (method == "CG") {
    # Handle CG type parameter
    if (!is.null(control$type)) {
      if (control$type == "Fletcher-Reeves") {
        adjusted_control$type <- 1
      } else if (control$type == "Polak-Ribiere") {
        adjusted_control$type <- 2
      } else if (control$type == "BFGS") {
        adjusted_control$type <- 3
      } else {
        stop("Invalid type for CG method. Must be one of: Fletcher-Reeves, Polak-Ribiere, or BFGS")
      }
    } else {
      adjusted_control$type <- 1  # Default to Fletcher-Reeves
    }
  }
  
  for(i in seq_len(runs)) {
    # Initialize vectors to store iteration history
    path <- matrix(par, nrow = 1)
    values <- numeric()
    iteration <- 1
    
    # Create wrapped function that tracks all evaluations
    wrapped_fn <- function(x) {
      # Handle NA/NaN inputs
      if (any(is.na(x))) {
        return(Inf)
      }
      
      # Store current point
      path <<- rbind(path, x)
      
      # Calculate function value
      current_value <- tryCatch({
        fn(x)
      }, error = function(e) {
        return(Inf)
      })
      
      # Handle NA/NaN outputs
      if (is.na(current_value) || is.nan(current_value)) {
        current_value <- Inf
      }
      
      # Store current value
      values <<- c(values, current_value)
      
      # Update progress if provided
      if (is.function(progress)) {
        progress(iteration, current_value)
      }
      
      iteration <<- iteration + 1
      return(current_value)
    }
    
    # Run optimization with error handling
    result <- tryCatch({
      optim(par = par, 
            fn = wrapped_fn, 
            method = method, 
            control = adjusted_control)
    }, error = function(e) {
      # Return a structured error result
      list(
        par = rep(NA, length(par)),
        value = Inf,
        counts = c(NA, NA),
        convergence = 99,
        message = paste("Optimization error:", e$message)
      )
    })
    
    # Store complete history in result
    result$path <- path
    result$values <- values
    result$run <- i
    results[[i]] <- result
  }
  
  return(results)
}


# Obtain a starting point for optimization ----
get_initial_point <- function(dims, init_method, func_info, init_distance = NULL, custom_init = NULL) {
  domain <- func_info$domain
  
  switch(init_method,
         "random" = {
           # Generate random points within the domain bounds
           # Use runif to generate uniform random numbers between domain[1] and domain[2]
           runif(dims, min = domain[1], max = domain[2])
         },
         "fixed" = {
           # Default to the minimum point
           opt_point <- rep(func_info$minimum_at[1], dims)
           
           # Default to 2 units away from the minimum
           if (is.null(init_distance)) init_distance <- 2
           
           # Ensure the initial point stays within domain bounds
           new_point <- opt_point + init_distance
           pmax(domain[1], pmin(domain[2], new_point))
         },
         "custom" = {
           # Use custom initial point if provided
           
           # Ensure custom_init is provided
           if (is.null(custom_init)) stop("Custom initial point not provided")
           
           # Convert custom_init to numeric vector
           vals <- as.numeric(strsplit(custom_init, ",")[[1]])
           
           # Ensure the correct number of values are provided
           if (length(vals) != dims) {
             vals <- rep(vals[1], dims)
           }
           
           # Ensure custom points are within domain bounds
           pmax(domain[1], pmin(domain[2], vals))
         },
         stop("Unknown initialization method")
  )
}