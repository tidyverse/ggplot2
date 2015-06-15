# Download from http://research.stlouisfed.org

library(readr)
library(dplyr)
library(purrr)

series <- c("PCE", "POP", "PSAVERT", "UEMPMED", "UNEMPLOY")
url <- paste0("http://research.stlouisfed.org/fred2/series/", series, "/downloaddata/", series, ".csv")

fields <- lapply(url, read_csv)

economics <- fields %>%
  map2(tolower(series), function(x, series) setNames(x, c("date", series))) %>%
  reduce(inner_join, by = "date") %>%
  mutate(date = structure(date, class = "Date")) # Not sure why this is lost

write.csv(economics, "data-raw/economics.csv", row.names = FALSE, quote = FALSE)
devtools::use_data(economics, overwrite = TRUE)
