# Support optim for now in public build ----
OPTIMIZATION_METHODS <- c(
  "BFGS" = "BFGS",
  "Nelder-Mead" = "Nelder-Mead",
  "CG" = "CG",
  "L-BFGS-B" = "L-BFGS-B",
  "SANN" = "SANN"
)

# List of test functions ----
TEST_FUNCTIONS <- list(
  "Unimodal Functions" = c(
    "Simple Bowl (Sphere)" = "sphere",
    "Weighted Bowl (Sum Squares)" = "sum_squares",
    "Flat Plate (Booth)" = "booth",
    "Rotated Plate (Matyas)" = "matyas",
    "Banana Valley (Rosenbrock)" = "rosenbrock"
  ),
  
  "Multimodal Functions" = c(
    "Three Valleys (Three-Hump Camel)" = "three_hump_camel",
    "Six Valleys (Six-Hump Camel)" = "six_hump_camel",
    "Regular Grid (Rastrigin)" = "rastrigin",
    "Exponential Grid (Ackley)" = "ackley",
    "Wave Pattern (Griewank)" = "griewank",
    "Water Ripples (Drop-Wave)" = "drop_wave",
    "Cross Pattern (Cross-in-Tray)" = "cross_in_tray",
    "Hidden Minimum (Easom)" = "easom"
  ),
  
  "Special Functions" = c(
    "Oscillating Bowl (Bohachevsky)" = "bohachevsky1",
    "Sharp Peaks (Beale)" = "beale",
    "Valley-Peak Mix (Goldstein-Price)" = "goldstein_price"
  )
)

# Visualization settings ----
GRID_RESOLUTION_RANGE <- c(20, 200)
DEFAULT_GRID_SIZE <- 50

# Optimization settings ----
DEFAULT_DIMENSIONS <- 2
MAX_DIMENSIONS <- 10
DEFAULT_MAX_ITERATIONS <- 1000
DEFAULT_TOLERANCE <- 1e-8