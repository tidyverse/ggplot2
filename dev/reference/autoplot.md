# Create a complete ggplot appropriate to a particular data type

`autoplot()` uses ggplot2 to draw a particular plot for an object of a
particular class in a single command. This defines the S3 generic that
other classes and packages can extend.

## Usage

``` r
autoplot(object, ...)
```

## Arguments

- object:

  an object, whose class will determine the behaviour of autoplot

- ...:

  other arguments passed to specific methods

## Value

a ggplot object

## See also

Other plotting automation topics:
[`autolayer()`](https://ggplot2.tidyverse.org/dev/reference/autolayer.md),
[`automatic_plotting`](https://ggplot2.tidyverse.org/dev/reference/automatic_plotting.md),
[`fortify()`](https://ggplot2.tidyverse.org/dev/reference/fortify.md)
