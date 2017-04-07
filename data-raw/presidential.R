library(readr)
presidential <- read_csv("data-raw/presidential.csv")
devtools::use_data(presidential, overwrite = TRUE)
