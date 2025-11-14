# Interpreter for graphical parameters

This is a wrapper for [`grid::gpar()`](https://rdrr.io/r/grid/gpar.html)
that applies ggplot2's interpretation of graphical parameters.

## Usage

``` r
gg_par(..., stroke = NULL, pointsize = NULL)
```

## Arguments

- ...:

  Named arguments passed on to `gpar()`.

- stroke:

  Linewidth for points. Populates the `lwd` grid parameter.

- pointsize:

  Size for points. Populates the `fontsize` grid parameter.

## Value

An object of class 'gpar'.
