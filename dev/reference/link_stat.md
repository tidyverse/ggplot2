# Parametrise a statistic computation

This is a helper function for
[`stat_chain()`](https://ggplot2.tidyverse.org/dev/reference/stat_chain.md)
to pass parameters and declare mappings.

## Usage

``` r
link_stat(stat, ..., after.stat = aes())
```

## Arguments

- stat:

  The statistical transformation to use on the data. The `stat` argument
  accepts the following:

  - A `Stat` ggproto subclass, for example `StatCount`.

  - A string naming the stat. To give the stat as a string, strip the
    function name of the `stat_` prefix. For example, for
    [`stat_count()`](https://ggplot2.tidyverse.org/dev/reference/geom_bar.md),
    give the string `"count"`.

- ...:

  Other arguments passed to the stat as a parameter.

- after.stat:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/dev/reference/aes.md) to be
  evaluated only after the stat has been computed.

## Value

A list bundling the stat, parameters and mapping.

## See also

[`stat_chain()`](https://ggplot2.tidyverse.org/dev/reference/stat_chain.md)

## Examples

``` r
# See `?stat_chain`
```
