library(readr)

msleep <- read_csv("data-raw/msleep.csv")
use_data(msleep, overwrite = TRUE)
