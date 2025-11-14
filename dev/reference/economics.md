# US economic time series

This dataset was produced from US economic time series data available
from <https://fred.stlouisfed.org/>. `economics` is in "wide" format,
`economics_long` is in "long" format.

## Usage

``` r
economics

economics_long
```

## Format

A data frame with 574 rows and 6 variables:

- date:

  Month of data collection

- pce:

  personal consumption expenditures, in billions of dollars,
  <https://fred.stlouisfed.org/series/PCE>

- pop:

  total population, in thousands,
  <https://fred.stlouisfed.org/series/POP>

- psavert:

  personal savings rate, <https://fred.stlouisfed.org/series/PSAVERT/>

- uempmed:

  median duration of unemployment, in weeks,
  <https://fred.stlouisfed.org/series/UEMPMED>

- unemploy:

  number of unemployed in thousands,
  <https://fred.stlouisfed.org/series/UNEMPLOY>

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with
2870 rows and 4 columns.
