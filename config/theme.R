# Setup the Stanford theme with bslib ----
stanford_theme <- bs_theme(
  version = 5,
  primary = "#8C1515",   # Stanford Cardinal Red
  secondary = "#4D4F53", # Stanford Cool Grey
  success = "#175E54",   # Stanford Dark Green
  info = "#006CB8",      # Stanford Blue
  warning = "#B26F16",   # Stanford Brown
  danger = "#820000",    # Stanford Dark Red
  base_font = font_collection(
    "system-ui", "-apple-system", "BlinkMacSystemFont", "'Segoe UI'",
    "Roboto", "'Helvetica Neue'", "Arial", "sans-serif"
  ),
  heading_font = font_collection(
    "Source Serif Pro", "Georgia", "'Times New Roman'", "Times", "serif"
  ),
  code_font = font_collection(
    "Source Code Pro", "'Courier New'", "Courier", "monospace"
  )
)

# Function to return the theme ----
get_app_theme <- function() {
  stanford_theme
}