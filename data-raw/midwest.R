library(readr)

midwest <- read_csv("data-raw/midwest.csv")
use_data(midwest, overwrite = TRUE)
