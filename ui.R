# UI ----
ui <- page_navbar(
  title = "Numerical Optimization Algorithms",
  theme = get_app_theme(),
  bg = "#8C1515",
  
  
  ## Navigation panels ----
  nav_panel(
    title = "Single Optimization",
    singleOptimizationUI("single")
  ),
  # nav_panel(
  #   title = "Algorithm Comparison",
  #   algorithmComparisonUI("compare")
  # ),
  nav_panel(
    title = "Landscape Analysis",
    landscapeAnalysisUI("landscape")
  ),
  
  ## Add a spacer ----
  nav_spacer(),
  
  ## Enable dark mode ----
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "light")
  )
)
