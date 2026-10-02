library(readr)
diamonds <- read_csv("data-raw/diamonds.csv", col_types =
  list(
    cut = col_factor(c("Fair", "Good", "Very Good", "Premium", "Ideal"), TRUE),
    color = col_factor(c("D", "E", "F", "G", "H", "I", "J"), TRUE),
    clarity = col_factor(c("I1", "SI2", "SI1", "VS2", "VS1", "VVS2", "VVS1", "IF"), TRUE)
  )
)

devtools::use_data(diamonds, overwrite = TRUE)
