# List of test functions ----
test_functions <- list(
  sphere = list(
    fn = function(x) {
      if (is.matrix(x)) x <- as.vector(x)
      sum(x^2)
    },
    latex = "f(\\mathbf{x}) = \\sum_{i=1}^n x_i^2",
    domain = c(-5.12, 5.12),
    minimum = 0,
    minimum_at = c(0, 0),
    description = "The Sphere function is a simple, convex, and continuous function."
  ),
  
  bohachevsky1 = list(
    fn = function(x) {
      if (length(x) != 2) stop("Bohachevsky function requires exactly 2 dimensions")
      x1 <- x[1]; x2 <- x[2]
      x1^2 + 2*x2^2 - 0.3*cos(3*pi*x1) - 0.4*cos(4*pi*x2) + 0.7
    },
    latex = "f(x_1, x_2) = x_1^2 + 2x_2^2 - 0.3\\cos(3\\pi x_1) - 0.4\\cos(4\\pi x_2) + 0.7",
    domain = c(-100, 100),
    minimum = 0,
    minimum_at = c(0, 0),
    description = "Bohachevsky function #1 combines quadratic terms with cosine modulation."
  ),
  
  sum_squares = list(
    fn = function(x) {
      if (is.matrix(x)) x <- as.vector(x)
      sum(seq_along(x) * x^2)
    },
    latex = "f(\\mathbf{x}) = \\sum_{i=1}^n ix_i^2",
    domain = c(-10, 10),
    minimum = 0,
    minimum_at = c(0, 0),
    description = "The Sum Squares function is a continuous, convex, and unimodal function."
  ),
  
  booth = list(
    fn = function(x) {
      if (length(x) != 2) stop("Booth function requires exactly 2 dimensions")
      (x[1] + 2*x[2] - 7)^2 + (2*x[1] + x[2] - 5)^2
    },
    latex = "f(x_1, x_2) = (x_1 + 2x_2 - 7)^2 + (2x_1 + x_2 - 5)^2",
    domain = c(-10, 10),
    minimum = 0,
    minimum_at = c(1, 3),
    description = "The Booth function is a plate-shaped function."
  ),
  
  eggholder = list(
    fn = function(x) {
      if (length(x) != 2) stop("Eggholder function requires exactly 2 dimensions")
      -1 * (x[2] + 47) * sin(sqrt(abs(x[2] + x[1]/2 + 47))) - x[1] * sin(sqrt(abs(x[1] - (x[2] + 47))))
    },
    latex = "f(x_1, x_2) = -\\left(x_2 + 47\\right)\\sin\\left(\\sqrt{|x_2 + x_1/2 + 47|}\\right) \\\\- x_1\\sin\\left(\\sqrt{|x_1 - x_2 - 47|}\\right)",
    domain = c(-512, 512),
    minimum = -959.6407,
    minimum_at = c(512, 404.2319),
    description = "The Eggholder function is a multimodal function with many local minima."
  ),
  
  holdertable = list(
    fn = function(x) {
      if (length(x) != 2) stop("Holder Table function requires exactly 2 dimensions")
      -abs(sin(x[0]) * cos(x[1]) * exp(abs(1 - sqrt(x[0]^2 + x[1]^2)/pi)))
    },
    latex = "f(x_1, x_2) = -|\\sin(x_1)\\cos(x_2)\\exp\\left(|1 - \\sqrt{x_1^2 + x_2^2}/\\pi|\\right)|",
    domain = c(-10, 10),
    minimum = -19.2085,
    minimum_at = c(8.05502, 9.66459),
    description = "The Holder Table function has many local minima."
  ),
  
  schaffer2 = list(
    fn = function(x) {
      if (length(x) != 2) stop("Schaffer Function N2 requires exactly 2 dimensions")
      0.5 + (sin(x[1]^2 - x[2]^2)^2 - 0.5) / (1 + 0.001 * (x[1]^2 + x[2]^2))^2
    },
    latex = "f(x_1, x_2) = 0.5 + \\frac{\\sin^2(x_1^2 - x_2^2) - 0.5}{\\left[1 + 0.001(x_1^2 + x_2^2)\\right]^2}",
    domain = c(-100, 100),
    minimum = 0,
    minimum_at = c(0, 0),
    description = "Schaffer function #2 is a multimodal function with many local minima."
  ),
  schaffer4 = list(
    fn = function(x) {
      if (length(x) != 2) stop("Schaffer Function N4 requires exactly 2 dimensions")
      0.5 + (cos(sin(abs(x[1]^2 - x[2]^2)))^2 - 0.5) / (1 + 0.001 * (x[1]^2 + x[2]^2))^2
    },
    latex = "f(x_1, x_2) = 0.5 + \\frac{\\cos^2\\left(\\sin\\left(|x_1^2 - x_2^2|\\right)\\right) - 0.5}{\\left[1 + 0.001(x_1^2 + x_2^2)\\right]^2}",
    domain = c(-100, 100),
    minimum = 0.292579,
    minimum_at = c(0, 1.253115),
    description = "Schaffer function #4 is a multimodal function with many local minima."
  ),
  
  matyas = list(
    fn = function(x) {
      if (length(x) != 2) stop("Matyas function requires exactly 2 dimensions")
      0.26 * (x[1]^2 + x[2]^2) - 0.48 * x[1] * x[2]
    },
    latex = "f(x_1, x_2) = 0.26(x_1^2 + x_2^2) - 0.48x_1x_2",
    domain = c(-10, 10),
    minimum = 0,
    minimum_at = c(0, 0),
    description = "The Matyas function is plate-shaped with a global minimum at the origin."
  ),
  
  rosenbrock = list(
    fn = function(x) {
      if (is.matrix(x)) x <- as.vector(x)
      sum(100 * (x[-1] - x[-length(x)]^2)^2 + (1 - x[-length(x)])^2)
    },
    latex = "f(\\mathbf{x}) = \\sum_{i=1}^{n-1} [100(x_{i+1} - x_i^2)^2 + (1 - x_i)^2]",
    domain = c(-5, 5),
    minimum = 0,
    minimum_at = c(1, 1),
    description = "The Rosenbrock function is a classic optimization problem, also known as the banana function."
  ),
  
  three_hump_camel = list(
    fn = function(x) {
      if (length(x) != 2) stop("Three-hump camel function requires exactly 2 dimensions")
      2*x[1]^2 - 1.05*x[1]^4 + (x[1]^6)/6 + x[1]*x[2] + x[2]^2
    },
    latex = "f(x_1, x_2) = 2x_1^2 - 1.05x_1^4 + \\frac{x_1^6}{6} + x_1x_2 + x_2^2",
    domain = c(-5, 5),
    minimum = 0,
    minimum_at = c(0, 0),
    description = "The Three-Hump Camel function has three local minima."
  ),
  
  six_hump_camel = list(
    fn = function(x) {
      if (length(x) != 2) stop("Six-hump camel function requires exactly 2 dimensions")
      (4 - 2.1*x[1]^2 + x[1]^4/3)*x[1]^2 + x[1]*x[2] + (-4 + 4*x[2]^2)*x[2]^2
    },
    latex = "f(x_1, x_2) = (4 - 2.1x_1^2 + \\frac{x_1^4}{3})x_1^2 + x_1x_2 + (-4 + 4x_2^2)x_2^2",
    domain = c(-3, 3),
    minimum = -1.0316,
    minimum_at = c(0.0898, -0.7126),
    description = "The Six-Hump Camel function has six local minima, two of which are global."
  ),
  
  rastrigin = list(
    fn = function(x) {
      if (is.matrix(x)) x <- as.vector(x)
      10 * length(x) + sum(x^2 - 10 * cos(2 * pi * x))
    },
    latex = "f(\\mathbf{x}) = 10n + \\sum_{i=1}^n [x_i^2 - 10\\cos(2\\pi x_i)]",
    domain = c(-5.12, 5.12),
    minimum = 0,
    minimum_at = c(0, 0),
    description = "The Rastrigin function is highly multimodal with many local minima."
  ),
  
  ackley = list(
    fn = function(x) {
      if (is.matrix(x)) x <- as.vector(x)
      -20 * exp(-0.2 * sqrt(mean(x^2))) - exp(mean(cos(2 * pi * x))) + 20 + exp(1)
    },
    latex = "f(\\mathbf{x}) = -20\\exp\\left(-0.2\\sqrt{\\frac{1}{n}\\sum_{i=1}^n x_i^2}\\right) - \\exp\\left(\\frac{1}{n}\\sum_{i=1}^n \\cos(2\\pi x_i)\\right) + 20 + e",
    domain = c(-32.768, 32.768),
    minimum = 0,
    minimum_at = c(0, 0),
    description = "The Ackley function has many local minima but a single global minimum."
  ),
  
  griewank = list(
    fn = function(x) {
      if (is.matrix(x)) x <- as.vector(x)
      sum(x^2)/4000 - prod(cos(x/sqrt(seq_along(x)))) + 1
    },
    latex = "f(\\mathbf{x}) = \\frac{1}{4000}\\sum_{i=1}^n x_i^2 - \\prod_{i=1}^n \\cos\\left(\\frac{x_i}{\\sqrt{i}}\\right) + 1",
    domain = c(-600, 600),
    minimum = 0,
    minimum_at = c(0, 0),
    description = "The Griewank function has many widespread local minima."
  ),
  
  drop_wave = list(
    fn = function(x) {
      if (length(x) != 2) stop("Drop-Wave function requires exactly 2 dimensions")
      r <- sqrt(x[1]^2 + x[2]^2)
      numerator <- -(1 + cos(12 * r))
      denominator <- 0.5 * (x[1]^2 + x[2]^2) + 2
      numerator/denominator
    },
    latex = "f(x_1, x_2) = -\\frac{1 + \\cos(12\\sqrt{x_1^2 + x_2^2})}{0.5(x_1^2 + x_2^2) + 2}",
    domain = c(-5.12, 5.12),
    minimum = -1,
    minimum_at = c(0, 0),
    description = "The Drop-Wave function is a multimodal, continuous, and non-convex function that resembles ripples of water drops."
  ),
  
  cross_in_tray = list(
    fn = function(x) {
      if (length(x) != 2) stop("Cross-in-tray function requires exactly 2 dimensions")
      -0.0001 * (abs(sin(x[1])*sin(x[2])*exp(abs(100 - sqrt(x[1]^2 + x[2]^2)/pi))) + 1)^0.1
    },
    latex = "f(x_1, x_2) = -0.0001\\left(|\\sin(x_1)\\sin(x_2)\\exp(|100 - \\sqrt{x_1^2 + x_2^2}/\\pi|)| + 1\\right)^{0.1}",
    domain = c(-10, 10),
    minimum = -2.06261,
    minimum_at = c(1.3491, 1.3491),
    description = "The Cross-in-Tray function has multiple global minima with steep ridges."
  ),
  
  easom = list(
    fn = function(x) {
      if (length(x) != 2) stop("Easom function requires exactly 2 dimensions")
      -cos(x[1])*cos(x[2])*exp(-(x[1] - pi)^2 - (x[2] - pi)^2)
    },
    latex = "f(x_1, x_2) = -\\cos(x_1)\\cos(x_2)\\exp(-(x_1 - \\pi)^2 - (x_2 - \\pi)^2)",
    domain = c(-100, 100),
    minimum = -1,
    minimum_at = c(pi, pi),
    description = "The Easom function has several steep ridges and drops."
  ),
  
  beale = list(
    fn = function(x) {
      if (length(x) != 2) stop("Beale function requires exactly 2 dimensions")
      (1.5 - x[1] + x[1]*x[2])^2 + 
        (2.25 - x[1] + x[1]*x[2]^2)^2 + 
        (2.625 - x[1] + x[1]*x[2]^3)^2
    },
    latex = "f(x_1, x_2) = (1.5 - x_1 + x_1x_2)^2 + (2.25 - x_1 + x_1x_2^2)^2 + (2.625 - x_1 + x_1x_2^3)^2",
    domain = c(-4.5, 4.5),
    minimum = 0,
    minimum_at = c(3, 0.5),
    description = "The Beale function is multimodal with sharp peaks."
  ),
  
  goldstein_price = list(
    fn = function(x) {
      if (length(x) != 2) stop("Goldstein-Price function requires exactly 2 dimensions")
      (1 + (x[1] + x[2] + 1)^2 * 
          (19 - 14*x[1] + 3*x[1]^2 - 14*x[2] + 6*x[1]*x[2] + 3*x[2]^2)) *
        (30 + (2*x[1] - 3*x[2])^2 * 
           (18 - 32*x[1] + 12*x[1]^2 + 48*x[2] - 36*x[1]*x[2] + 27*x[2]^2))
    },
    latex = "\\begin{align*}f(x_1, x_2) = &[1 + (x_1 + x_2 + 1)^2(19 - 14x_1 + 3x_1^2 - 14x_2 + 6x_1x_2 + 3x_2^2)] \\\\&\\times [30 + (2x_1 - 3x_2)^2(18 - 32x_1 + 12x_1^2 + 48x_2 - 36x_1x_2 + 27x_2^2)]\\end{align*}",
    domain = c(-2, 2),
    minimum = 3,
    minimum_at = c(0, -1),
    description = "The Goldstein-Price function is a challenging 2D optimization problem."
  )
)

#' Get information about a specific test function
#' @param func_name Name of the function
#' @return List containing function information
#' @export
get_function_info <- function(func_name) {
  if (!func_name %in% names(test_functions)) {
    stop("Unknown function name")
  }
  test_functions[[func_name]]
}

#' Get information about a specific test function
#' @param func_name Name of the function
#' @return List containing function information
#' @export
get_function_info <- function(func_name) {
  if (!func_name %in% names(test_functions)) {
    stop("Unknown function name")
  }
  test_functions[[func_name]]
}