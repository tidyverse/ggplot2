# Calculate mean and standard error of the mean

For use with
[`stat_summary()`](https://ggplot2.tidyverse.org/reference/stat_summary.md)

## Usage

``` r
mean_se(x, mult = 1)
```

## Arguments

- x:

  numeric vector.

- mult:

  number of multiples of standard error.

## Value

A data frame with three columns:

- `y`:

  The mean.

- `ymin`:

  The mean minus the multiples of the standard error.

- `ymax`:

  The mean plus the multiples of the standard error.

## Examples

``` r
set.seed(1)
x <- rnorm(100)
mean_se(x)
#>           y       ymin      ymax
#> 1 0.1088874 0.01906743 0.1987073
```
