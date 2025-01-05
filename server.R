server <- function(input, output, session) {
  # Initialize modules
  singleOptimizationServer("single")
  #algorithmComparisonServer("compare")
  landscapeAnalysisServer("landscape")
}