library(readr)

seals <- read_csv("data-raw/seals.csv")
use_data(seals, overwrite = TRUE)
