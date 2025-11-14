# Explicitly draw plot

Generally, you do not need to print or plot a ggplot2 plot explicitly:
the default top-level print method will do it for you. You will,
however, need to call [`print()`](https://rdrr.io/r/base/print.html)
explicitly if you want to draw a plot inside a function or for loop.

## Arguments

- x:

  plot to display

- newpage:

  draw new (empty) page first?

- vp:

  viewport to draw plot in

- ...:

  other arguments not used by this method

## Value

Invisibly returns the original plot.

## Examples

``` r
colours <- c("class", "drv", "fl")

# Doesn't seem to do anything!
for (colour in colours) {
  ggplot(mpg, aes(displ, hwy, colour = .data[[colour]])) +
    geom_point()
}

for (colour in colours) {
  print(ggplot(mpg, aes(displ, hwy, colour = .data[[colour]])) +
          geom_point())
}


```
