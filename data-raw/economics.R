# Download from http://research.stlouisfed.org

library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(dplyr)

# create directory for storing raw CSVs
RAW_CSV_DIR <- "data-raw/economics_raw"
dir.create(RAW_CSV_DIR, recursive = TRUE, showWarnings = FALSE)

# paths to CSV files
series <- c("PCE", "POP", "PSAVERT", "UEMPMED", "UNEMPLOY")
csv <- file.path(RAW_CSV_DIR, paste0(series, ".csv"))

# These CSVs are available from http://research.stlouisfed.org/fred2, but the
# data might not be the same due to some revisions. So, we store the CSVs under
# data-raw/ (See the discussion on #2962). To update, use the following code:
#
#   url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", series)
#   walk2(url, csv, function(x, dest) download.file(x, destfile = dest))

# read the CSV files
fields <- map(
  csv, read_csv,
  skip = 1, col_names = c("DATE", "VALUE"), col_types = "Dd"
)

economics <- fields %>%
  map2(tolower(series), function(x, series) setNames(x, c("date", series))) %>%
  reduce(inner_join, by = "date")

write.csv(economics, "data-raw/economics.csv", row.names = FALSE, quote = FALSE)
usethis::use_data(economics, overwrite = TRUE)

rescale01 <- function(x) (x - min(x)) / diff(range(x))
economics_long <- economics %>%
  gather(variable, value, -date) %>%
  group_by(variable) %>%
  mutate(value01 = rescale01(value)) %>%
  ungroup()

usethis::use_data(economics_long, overwrite = TRUE)
