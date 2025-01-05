# Landscape Analysis Graph w/ Contour, Surface, & Gradient Field ----
create_landscape_plot <- function(func_info, analysis_type, grid_size, show_contours = TRUE) {
  domain <- func_info$domain
  x <- seq(domain[1], domain[2], length.out = grid_size)
  y <- x
  z <- outer(x, y, function(x, y) {
    sapply(1:length(x), function(i) {
      func_info$fn(c(x[i], y[i]))
    })
  })
  
  # Create base plot based on analysis type
  p <- switch(analysis_type,
              "contour" = {
                # Create the contour plot first
                p <- plot_ly() |>
                  add_contour(x = x, y = y, z = z)
                
                # Add minimum point for contour plot if it exists
                if (length(func_info$minimum_at) == 2) {
                  p <- p |> add_trace(
                    x = list(func_info$minimum_at[1]),
                    y = list(func_info$minimum_at[2]),
                    type = "scatter",
                    mode = "markers",
                    marker = list(
                      size = 12,
                      color = "red",
                      symbol = "star"
                    ),
                    name = "Global Minimum",
                    showlegend = TRUE
                  )
                }
                p
              },
              "surface" = {
                p <- plot_ly(x = x, y = y, z = z, type = "surface")
                # Add minimum point for surface plot
                if (length(func_info$minimum_at) == 2) {
                  p <- p |> add_trace(
                    x = c(func_info$minimum_at[1]),
                    y = c(func_info$minimum_at[2]),
                    z = c(func_info$minimum),
                    type = "scatter3d",
                    mode = "markers",
                    marker = list(
                      size = 8,
                      color = "red",
                      symbol = "star"
                    ),
                    name = "Global Minimum",
                    showlegend = TRUE
                  )
                }
                p
              },
              "gradient" = {
                p <- create_gradient_plot(x, y, z, func_info$fn, show_contours)
                # Add minimum point for gradient plot
                if (length(func_info$minimum_at) == 2) {
                  p <- p |>  add_trace(
                    x = c(func_info$minimum_at[1]),
                    y = c(func_info$minimum_at[2]),
                    type = "scatter",
                    mode = "markers",
                    marker = list(
                      size = 12,
                      color = "red",
                      symbol = "star"
                    ),
                    name = "Global Minimum",
                    showlegend = TRUE
                  )
                }
                p
              },
              stop("Unknown analysis type")
  )
  
  p <- p |>
    layout(
      plot_bgcolor  = "transparent",
      paper_bgcolor = "transparent"
    )
  return(p)
}

# Gradient Field Plotting Function ----
create_gradient_plot <- function(x, y, z, fn, show_contours = TRUE) {
  # Compute gradient field
  dx <- outer(x, y, function(x, y) {
    sapply(1:length(x), function(i) {
      eps <- 1e-6
      (fn(c(x[i] + eps, y[i])) - fn(c(x[i] - eps, y[i]))) / (2 * eps)
    })
  })
  
  dy <- outer(x, y, function(x, y) {
    sapply(1:length(x), function(i) {
      eps <- 1e-6
      (fn(c(x[i], y[i] + eps)) - fn(c(x[i], y[i] - eps))) / (2 * eps)
    })
  })
  
  # Subsample points for arrows to avoid overcrowding
  sample_points <- seq(1, length(x), length.out = 12)
  x_sample <- x[sample_points]
  y_sample <- y[sample_points]
  dx_sample <- dx[sample_points, sample_points]
  dy_sample <- dy[sample_points, sample_points]
  
  # Normalize gradients for better visualization
  magnitudes <- sqrt(dx_sample^2 + dy_sample^2)
  max_magnitude <- max(magnitudes, na.rm = TRUE)
  scale_factor <- 1.2 * (diff(range(x))/length(x)) / max_magnitude
  
  # Create base plot
  p <- plot_ly()
  
  # Add contours if enabled
  if (show_contours) {
    p <- p |>  add_contour(
      x = x,
      y = y,
      z = z,
      showscale = TRUE,
      contours = list(
        coloring = 'heatmap',
        showlabels = TRUE
      )
    )
  }
  
  # Add arrows for gradient field
  for (i in seq_along(x_sample)) {
    for (j in seq_along(y_sample)) {
      if (!is.na(dx_sample[i,j]) && !is.na(dy_sample[i,j])) {
        # Calculate arrow endpoints
        x0 <- x_sample[i]
        y0 <- y_sample[j]
        dx_val <- dx_sample[i,j] * scale_factor
        dy_val <- dy_sample[i,j] * scale_factor
        
        # Add arrow
        p <- p |> add_trace(
          x = c(x0, x0 + dx_val),
          y = c(y0, y0 + dy_val),
          type = "scatter",
          mode = "lines",
          line = list(
            color = "red",
            width = 2
          ),
          showlegend = FALSE
        )
        
        # Add arrowhead
        angle <- pi/6  # 30 degrees
        length <- sqrt(dx_val^2 + dy_val^2) * 0.3
        theta <- atan2(dy_val, dx_val)
        
        x1 <- x0 + dx_val
        y1 <- y0 + dy_val
        
        x_head1 <- x1 - length * cos(theta + angle)
        y_head1 <- y1 - length * sin(theta + angle)
        x_head2 <- x1 - length * cos(theta - angle)
        y_head2 <- y1 - length * sin(theta - angle)
        
        p <- p |> add_trace(
          x = c(x1, x_head1, x1, x_head2),
          y = c(y1, y_head1, y1, y_head2),
          type = "scatter",
          mode = "lines",
          line = list(
            color = "red",
            width = 2
          ),
          showlegend = FALSE
        )
      }
    }
  }
  
  # Add layout
  p |> layout(
    title = "Gradient Field",
    xaxis = list(title = "x1"),
    yaxis = list(title = "x2"),
    showlegend = FALSE,
    plot_bgcolor  = "transparent",
    paper_bgcolor = "transparent"
  )
}

# Convergence Plotting Function ----
create_convergence_plot <- function(results) {
  plot_data <- data.frame(
    Iteration = seq_along(results$values),
    Value = results$values
  )
  
  plot_ly(plot_data, x = ~Iteration, y = ~Value, 
          type = "scatter", mode = "lines+markers",
          hovertemplate = paste(
            "Iteration: %{x}<br>",
            "Value: %{y:.6g}<br>",
            "<extra></extra>"  # This removes the trace name from tooltip
          )) |>
    layout(
      title = "Convergence Plot",
      xaxis = list(title = "Iteration"),
      yaxis = list(title = "Objective Value", type = "log"),
      showlegend = FALSE,
      plot_bgcolor = "transparent",
      paper_bgcolor = "transparent"
    )
}

# Optimization Path Routing Function ----
create_optimization_path_plot <- function(results, func_info, plot_type = "contour") {
  
  # Check if results contain only 2 parameters
  if (length(results$par) != 2) {
      # Return an empty plot with a message if not 2D
      p <- plot_ly() |>
           # TODO: Figure out how to silence warning
           layout(
             annotations = list(
               x = 0.5,
               y = 0.5,
               text = "Optimization path visualization is only available for 2-dimensional problems",
               showarrow = FALSE,
               font = list(size = 14)
             ),
             xaxis = list(visible = FALSE),
             yaxis = list(visible = FALSE),
             plot_bgcolor = "transparent",
             paper_bgcolor = "transparent"
           )
      return(p)
  }
  
  if (plot_type == "contour") {
    create_optimization_contour_plot(results, func_info)
  } else if (plot_type == "surface") {
    create_optimization_surface_plot(results, func_info)
  } else {
    stop("Unknown plot type")
  }
  
}

# Processing optimization path and domain ----
process_optimization_data <- function(path, func_info, base_resolution = 100) {
  
  # Extract domain from function info
  domain <- func_info$domain
  
  # Determine plot bounds based on optimization path and default domain
  x_min <- min(min(path[,1]), domain[1])
  x_max <- max(max(path[,1]), domain[2])
  y_min <- min(min(path[,2]), domain[1])
  y_max <- max(max(path[,2]), domain[2])
  
  # Calculate domain span and adjust grid resolution
  x_span <- x_max - x_min
  y_span <- y_max - y_min
  default_span <- domain[2] - domain[1]
  
  # Increase grid resolution if domain is larger than default
  resolution_scale <- max(1, max(x_span, y_span) / default_span)
  grid_resolution <- ceiling(base_resolution * resolution_scale)
  
  # Generate grid points
  x_range <- seq(x_min, x_max, length.out = grid_resolution)
  y_range <- seq(y_min, y_max, length.out = grid_resolution)
  
  # Calculate z values for the grid
  z <- outer(x_range, y_range, function(x, y) {
    sapply(1:length(x), function(i) {
      func_info$fn(c(x[i], y[i]))
    })
  })
  
  # Calculate z-coordinates for the path
  path_z <- sapply(1:nrow(path), function(i) {
    func_info$fn(path[i,])
  })
  
  # Create hover text
  hover_text <- sprintf(
    "Iteration: %d<br>f(%.6g, %.6g) = %.6g",
    1:nrow(path),
    path[,1], path[,2],
    path_z
  )
  
  list(
    x_range = x_range,
    y_range = y_range,
    z = z,
    path_z = path_z,
    hover_text = hover_text,
    domain = domain
  )
}

# Build a 2D plot with contour lines and optimization path ----
create_optimization_contour_plot <- function(results, func_info) {
  path <- results$path
  
  # Process data using helper function
  data <- process_optimization_data(path, func_info, base_resolution = 100)
  
  # Create base plot with contour
  plot <- plot_ly() %>%
    add_contour(
      x = data$x_range,
      y = data$y_range,
      z = data$z,
      colorscale = "Viridis",
      showscale = TRUE,
      name = "Function Value",
      contours = list(showlabels = TRUE)
    )
  
  # Add global minimum point
  if (length(func_info$minimum_at) == 2) {
    plot <- plot %>%
      add_trace(
        x = func_info$minimum_at[1],
        y = func_info$minimum_at[2],
        type = "scatter",
        mode = "markers",
        marker = list(
          color = "green",
          size = 12,
          symbol = "star",
          line = list(color = "white", width = 1)
        ),
        name = "Global Minimum",
        text = sprintf("Global Minimum<br>f(%.6g, %.6g) = %.6g", 
                       func_info$minimum_at[1], func_info$minimum_at[2], func_info$minimum),
        hoverinfo = "text"
      )
  }
  
  # Add optimization path
  if (nrow(path) > 1) {
    plot <- plot %>%
      add_trace(
        x = path[,1],
        y = path[,2],
        type = "scatter",
        mode = "lines+markers",
        line = list(
          color = "red",
          width = 2,
          shape = "spline"
        ),
        marker = list(
          size = 8,
          color = "red",
          symbol = "circle"
        ),
        text = data$hover_text,
        hoverinfo = "text",
        showlegend = FALSE
      )
  }
  
  # Add start point
  start_z <- data$path_z[1]
  plot <- plot %>%
    add_trace(
      x = path[1,1],
      y = path[1,2],
      type = "scatter",
      mode = "markers",
      marker = list(
        size = 12,
        color = "blue",
        symbol = "circle",
        line = list(color = "white", width = 1)
      ),
      name = "Start",
      text = sprintf("Start<br>f(%.6g, %.6g) = %.6g", 
                     path[1,1], path[1,2], start_z),
      hoverinfo = "text"
    )
  
  # Add end point
  end_z <- data$path_z[nrow(path)]
  plot <- plot %>%
    add_trace(
      x = path[nrow(path),1],
      y = path[nrow(path),2],
      type = "scatter",
      mode = "markers",
      marker = list(
        size = 12,
        color = "purple",
        symbol = "diamond",
        line = list(color = "white", width = 1)
      ),
      name = "End",
      text = sprintf("End<br>f(%.6g, %.6g) = %.6g", 
                     path[nrow(path),1], path[nrow(path),2], end_z),
      hoverinfo = "text"
    )
  
  # Update layout
  plot %>% layout(
    showlegend = TRUE,
    xaxis = list(
      title = "x1", 
      zeroline = FALSE,
      range = data$domain  # Use domain for initial view
    ),
    yaxis = list(
      title = "x2", 
      zeroline = FALSE
    ),
    plot_bgcolor = "transparent",
    paper_bgcolor = "transparent",
    legend = list(
      orientation = "h",
      xanchor = "center",
      x = 0.5,
      y = -0.15
    )
  )
}

# Build a 3D Surface graph plot with contour lines and optimization path ----
create_optimization_surface_plot <- function(results, func_info) {
  path <- results$path
  
  # Process data using helper function
  data <- process_optimization_data(path, func_info, base_resolution = 500)
  
  # Create base plot with surface
  plot <- plot_ly() %>%
    add_surface(
      x = data$x_range,
      y = data$y_range,
      z = data$z,
      colorscale = "Viridis",
      showscale = TRUE,
      name = "f(x1, x2)",
      contours = list(
        z = list(show = TRUE, usecolormap = TRUE, project = list(z = TRUE))
      )
    )
  
  # Add global minimum point
  # TODO: Allow for multiple minima in the future
  if (length(func_info$minimum_at) == 2) {
    plot <- plot %>%
      add_trace(
        x = func_info$minimum_at[1],
        y = func_info$minimum_at[2],
        z = func_info$minimum,
        type = "scatter3d",
        mode = "markers",
        marker = list(
          color = "green",
          size = 8,
          symbol = "star",
          line = list(color = "white", width = 1)
        ),
        name = "Global Minimum"
      )
  }
  
  # Add optimization path with surface following
  if (nrow(path) > 1) {
    # Create interpolated path that follows the surface
    interpolated_path <- list(x = numeric(), y = numeric(), z = numeric())
    
    for (i in seq_len(nrow(path)-1)) {
      alpha <- seq(0, 1, length.out = 10)
      for (t in alpha) {
        x_interp <- path[i,1] * (1-t) + path[i+1,1] * t
        y_interp <- path[i,2] * (1-t) + path[i+1,2] * t
        z_interp <- func_info$fn(c(x_interp, y_interp))
        
        interpolated_path$x <- c(interpolated_path$x, x_interp)
        interpolated_path$y <- c(interpolated_path$y, y_interp)
        interpolated_path$z <- c(interpolated_path$z, z_interp)
      }
    }
    
    # Add interpolated path
    plot <- plot %>%
      add_trace(
        x = interpolated_path$x,
        y = interpolated_path$y,
        z = interpolated_path$z,
        type = "scatter3d",
        mode = "lines",
        line = list(
          color = "red",
          width = 3
        ),
        showlegend = TRUE,
        name = "Optimization Path"
      )
    
    # Add original points as markers
    plot <- plot %>%
      add_trace(
        x = path[,1],
        y = path[,2],
        z = data$path_z,
        type = "scatter3d",
        mode = "markers",
        marker = list(
          size = 4,
          color = "red",
          symbol = "circle"
        ),
        text = data$hover_text,
        hoverinfo = "text",
        showlegend = FALSE
      )
  }
  
  # Add start point
  plot <- plot %>%
    add_trace(
      x = path[1,1],
      y = path[1,2],
      z = data$path_z[1],
      type = "scatter3d",
      mode = "markers",
      marker = list(
        size = 8,
        color = "blue",
        symbol = "circle",
        line = list(color = "white", width = 1)
      ),
      name = "Start",
      text = sprintf("Start<br>f(%.6g, %.6g) = %.6g", 
                     path[1,1], path[1,2], data$path_z[1]),
      hoverinfo = "text"
    )
  
  # Add end point
  plot <- plot %>%
    add_trace(
      x = path[nrow(path),1],
      y = path[nrow(path),2],
      z = data$path_z[nrow(path)],
      type = "scatter3d",
      mode = "markers",
      marker = list(
        size = 8,
        color = "purple",
        symbol = "diamond",
        line = list(color = "white", width = 1)
      ),
      name = "End",
      text = sprintf("End<br>f(%.6g, %.6g) = %.6g", 
                     path[nrow(path),1], path[nrow(path),2], data$path_z[nrow(path)]),
      hoverinfo = "text"
    )
  
  # Position camera on default domain and center
  # Ensure transparency for plot and paper background for dark mode
  plot %>% layout(
    scene = list(
      xaxis = list(title = "x1", range = data$domain),
      yaxis = list(title = "x2", range = data$domain),
      zaxis = list(title = "f(x1, x2)"),
      camera = list(
        eye = list(x = 1.5, y = 1.5, z = 1.5),
        center = list(
          x = (data$domain[1] + data$domain[2])/2,
          y = (data$domain[1] + data$domain[2])/2,
          z = 0
        )
      ),
      aspectratio = list(x = 1, y = 1, z = 1)
    ),
    plot_bgcolor = "transparent",
    paper_bgcolor = "transparent"
  )
}