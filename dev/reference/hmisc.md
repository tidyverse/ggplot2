# A selection of summary functions from Hmisc

These are wrappers around functions from Hmisc designed to make them
easier to use with
[`stat_summary()`](https://ggplot2.tidyverse.org/dev/reference/stat_summary.md).
See the Hmisc documentation for more details:

- [`Hmisc::smean.cl.boot()`](https://rdrr.io/pkg/Hmisc/man/smean.sd.html)

- [`Hmisc::smean.cl.normal()`](https://rdrr.io/pkg/Hmisc/man/smean.sd.html)

- [`Hmisc::smean.sdl()`](https://rdrr.io/pkg/Hmisc/man/smean.sd.html)

- [`Hmisc::smedian.hilow()`](https://rdrr.io/pkg/Hmisc/man/smean.sd.html)

## Usage

``` r
mean_cl_boot(x, ...)

mean_cl_normal(x, ...)

mean_sdl(x, ...)

median_hilow(x, ...)
```

## Arguments

- x:

  a numeric vector

- ...:

  other arguments passed on to the respective Hmisc function.

## Value

A data frame with columns `y`, `ymin`, and `ymax`.

## Examples

``` r
if (requireNamespace("Hmisc", quietly = TRUE)) {
set.seed(1)
x <- rnorm(100)
mean_cl_boot(x)
mean_cl_normal(x)
mean_sdl(x)
median_hilow(x)
}
#>           y      ymin     ymax
#> 1 0.1139092 -1.671298 1.797468
```
