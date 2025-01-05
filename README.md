# Numerical Optimization Algorithms

The Numerical Optimization Algorithms is a web-based application that allows for 
the exploration of numerical optimization algorithms on various test functions.
The application is built with
R Shiny and deployed using shinylive. Check out the live application at
<https://shiny.thecoatlessprofessor.com/numerical-optimization/>

## Deployment

This application is deployed using shinylive, allowing it to run directly in
the browser without requiring an R server. Shinylive converts the R code to 
WebAssembly, making it possible to run R applications entirely client-side.

## Local Development Environment

1. Clone the repository:

```bash
git clone https://github.com/coatless-shiny/numerical-optimization.git
```

2. Open the `numerical-optimization.Rproj`

3. Install required R packages:

```r
install.packages(c("shiny", "plotly", "bslib", "bsicons", "shinylive"))
```

4. Run the application:

```r
shiny::runApp()
```

5. Check if the application can be converted to `{shinylive}`:

```r
shinylive::export(".", "_site")
```

## Acknowledgments

- Built using the R Shiny framework
- Uses the bslib package for Bootstrap 5 theming
- Deployed using shinylive for browser-based execution
