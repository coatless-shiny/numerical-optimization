library(shiny)
library(bslib)
library(plotly)

# Source all R files in the R directory and its subdirectories
app_source <- c(
  list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE), 
  list.files("config", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
)

for (file in app_source) {
  source(file)
}