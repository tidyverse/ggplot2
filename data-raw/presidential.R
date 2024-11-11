library(readr)
presidential <- read_csv("data-raw/presidential.csv")[]
usethis::use_data(presidential, overwrite = TRUE)
