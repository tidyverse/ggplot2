# Download from http://research.stlouisfed.org

library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(dplyr)

series <- c("PCE", "POP", "PSAVERT", "UEMPMED", "UNEMPLOY")
url <- paste0("http://research.stlouisfed.org/fred2/series/", series, "/downloaddata/", series, ".csv")

fields <- map(url, read_csv,
  col_types = cols(
    DATE = col_date(format = ""),
    VALUE = col_double()
  )
)

economics <- fields %>%
  map2(tolower(series), function(x, series) setNames(x, c("date", series))) %>%
  reduce(inner_join, by = "date") %>%
  filter(date <= as.Date("2015-04-01"))

write.csv(economics, "data-raw/economics.csv", row.names = FALSE, quote = FALSE)
usethis::use_data(economics, overwrite = TRUE)

rescale01 <- function(x) (x - min(x)) / diff(range(x))
economics_long <- economics %>%
  gather(variable, value, -date) %>%
  group_by(variable) %>%
  mutate(value01 = rescale01(value)) %>%
  ungroup()

usethis::use_data(economics_long, overwrite = TRUE)
