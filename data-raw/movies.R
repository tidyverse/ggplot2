library(readr)

movies <- read_csv("data-raw/movies.csv")
use_data(movies, overwrite = TRUE)
