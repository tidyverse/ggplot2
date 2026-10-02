library(rvest)
library(tidyr)
library(readr)
library(dplyr)

# Find list of all pages -------------------------------------------------------
root <- read_html("http://recenter.tamu.edu/Data/hs/")
links <- root %>%
  html_nodes(".threecol a")
pages <- links %>%
  html_attr("href") %>%
  url_absolute(xml_url(root)) %>%
  setNames(html_text(links))

# Extract table from each page -------------------------------------------------
to_char <- function(df) {
  df[] <- lapply(df, as.character)
  df
}

tamu_table <- . %>%
  html() %>%
  html_node(".dataTable") %>%
  html_table()

tables <- lapply(pages, tamu_table)
data <- lapply(tables, . %>% .[-1, ] %>% to_char) %>%
  Map(function(df, city) {
    df$city <- city
    df
  }, ., names(.)) %>%
  bind_rows() %>%
  as_data_frame()

data[data == "-"] <- NA

months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
  "Oct", "Nov", "Dec")

txhousing <- data %>%
  mutate(
    Sales         = parse_numeric(Sales),
    DollarVolume  = parse_numeric(DollarVolume),
    AveragePrice  = parse_numeric(AveragePrice),
    MedianPrice   = parse_numeric(MedianPrice),
    TotalListings = parse_numeric(TotalListings),
    MonthsInventory = parse_numeric(MonthsInventory)
  ) %>%
  extract(Date, c("Year", "Month"), "(\\d*)-?([a-zA-Z]*)", convert = TRUE) %>%
  mutate(
    Year = zoo::na.locf(ifelse(Year == "", NA, Year)),
    Month = match(Month, months)) %>%
  select(city, year = Year, month = Month, sales = Sales,
    volume = DollarVolume, average = AveragePrice, median = MedianPrice,
    listings = TotalListings, inventory = MonthsInventory) %>%
  mutate(date = year + (month - 1) / 12) %>%
  # Don't need totals & Palestine is v. low quality
  filter(!(city %in% c("Texas Totals", "Palestine"))) %>%
  # Reduce file size
  filter(year >= 2000) %>%
  select(-average)

write.csv(txhousing, "data-raw/tx-housing.csv", row.names = FALSE, quote = FALSE)
devtools::use_data(txhousing, overwrite = TRUE)
