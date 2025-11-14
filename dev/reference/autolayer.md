# Create a ggplot layer appropriate to a particular data type

`autolayer()` uses ggplot2 to draw a particular layer for an object of a
particular class in a single command. This defines the S3 generic that
other classes and packages can extend.

## Usage

``` r
autolayer(object, ...)
```

## Arguments

- object:

  an object, whose class will determine the behaviour of autolayer

- ...:

  other arguments passed to specific methods

## Value

a ggplot layer

## See also

Other plotting automation topics:
[`automatic_plotting`](https://ggplot2.tidyverse.org/dev/reference/automatic_plotting.md),
[`autoplot()`](https://ggplot2.tidyverse.org/dev/reference/autoplot.md),
[`fortify()`](https://ggplot2.tidyverse.org/dev/reference/fortify.md)
