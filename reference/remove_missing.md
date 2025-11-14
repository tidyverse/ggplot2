# Convenience function to remove missing values from a data.frame

Remove all non-complete rows, with a warning if `na.rm = FALSE`. ggplot
is somewhat more accommodating of missing values than R generally. For
those stats which require complete data, missing values will be
automatically removed with a warning. If `na.rm = TRUE` is supplied to
the statistic, the warning will be suppressed.

## Usage

``` r
remove_missing(df, na.rm = FALSE, vars = names(df), name = "", finite = FALSE)
```

## Arguments

- df:

  data.frame

- na.rm:

  If true, will suppress warning message.

- vars:

  Character vector of variables to check for missings in

- name:

  Optional function name to improve error message.

- finite:

  If `TRUE`, will also remove non-finite values.
