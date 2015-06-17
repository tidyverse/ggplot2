library(readr)

mpg <- read_csv("data-raw/mpg.csv")
use_data(mpg, overwrite = TRUE)
