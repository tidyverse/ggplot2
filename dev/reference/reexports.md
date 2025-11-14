# Objects exported from other packages

These objects are imported from other packages. Follow the links below
to see their documentation.

- grid:

  [`arrow`](https://rdrr.io/r/grid/arrow.html),
  [`unit`](https://rdrr.io/r/grid/unit.html)

- scales:

  [`alpha`](https://scales.r-lib.org/reference/alpha.html)

## Examples

``` r
ggplot(mpg, aes(displ, hwy)) +
  geom_point(alpha = 0.5, colour = "blue")


ggplot(mpg, aes(displ, hwy)) +
  geom_point(colour = alpha("blue", 0.5))
```
