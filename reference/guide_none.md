# Empty guide

This guide draws nothing.

## Usage

``` r
guide_none(title = waiver(), position = waiver())
```

## Arguments

- title:

  A character string or expression indicating a title of guide. If
  `NULL`, the title is not shown. By default
  ([`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.md)), the
  name of the scale object or the name specified in
  [`labs()`](https://ggplot2.tidyverse.org/reference/labs.md) is used
  for the title.

- position:

  Where this guide should be drawn: one of top, bottom, left, or right.
