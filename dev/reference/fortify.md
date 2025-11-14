# Fortify a model with data.

Rather than using this function, I now recommend using the broom
package, which implements a much wider range of methods. `fortify()` may
be deprecated in the future.

## Usage

``` r
fortify(model, data, ...)
```

## Arguments

- model:

  model or other R object to convert to data frame

- data:

  original dataset, if needed

- ...:

  Arguments passed to methods.

## See also

[`fortify.lm()`](https://ggplot2.tidyverse.org/dev/reference/fortify.lm.md)

Other plotting automation topics:
[`autolayer()`](https://ggplot2.tidyverse.org/dev/reference/autolayer.md),
[`automatic_plotting`](https://ggplot2.tidyverse.org/dev/reference/automatic_plotting.md),
[`autoplot()`](https://ggplot2.tidyverse.org/dev/reference/autoplot.md)
