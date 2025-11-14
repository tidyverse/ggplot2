# Extract tick information from guides

`get_guide_data()` builds a plot and extracts information from guide
keys. This information typically contains positions, values and/or
labels, depending on which aesthetic is queried or guide is used.

## Usage

``` r
get_guide_data(plot = get_last_plot(), aesthetic, panel = 1L)
```

## Arguments

- plot:

  A `ggplot` or `ggplot_build` object.

- aesthetic:

  A string that describes a single aesthetic for which to extract guide
  information. For example: `"colour"`, `"size"`, `"x"` or `"y.sec"`.

- panel:

  An integer giving a panel number for which to return position guide
  information.

## Value

One of the following:

- A `data.frame` representing the guide key, when the guide is unique
  for the aesthetic.

- A `list` when the coord does not support position axes or multiple
  guides match the aesthetic.

- `NULL` when no guide key could be found.

## Examples

``` r
# A standard plot
p <- ggplot(mtcars) +
  aes(mpg, disp, colour = drat, size = drat) +
  geom_point() +
  facet_wrap(vars(cyl), scales = "free_x")

# Guide information for legends
get_guide_data(p, "size")
#>       size .value .label
#> 1 2.662822    3.0    3.0
#> 2 3.919819    3.5    3.5
#> 3 4.779645    4.0    4.0
#> 4 5.477285    4.5    4.5

# Note that legend guides can be merged
merged <- p + guides(colour = "legend")
get_guide_data(merged, "size")
#>    colour .value .label     size
#> 1 #1A3855    3.0    3.0 2.662822
#> 2 #28557C    3.5    3.5 3.919819
#> 3 #3874A5    4.0    4.0 4.779645
#> 4 #4894D0    4.5    4.5 5.477285

# Guide information for positions
get_guide_data(p, "x", panel = 2)
#>           x .value .label y
#> 1 0.0959596     18     18 1
#> 2 0.3484848     19     19 1
#> 3 0.6010101     20     20 1
#> 4 0.8535354     21     21 1

# Coord polar doesn't support proper guides, so we get a list
polar <- p + coord_polar()
get_guide_data(polar, "theta", panel = 2)
#> $theta.range
#> [1] 17.8 21.4
#> 
#> $theta.major
#> [1] 18 19 20 21
#> 
#> $theta.minor
#> [1] 18.0 18.5 19.0 19.5 20.0 20.5 21.0
#> 
#> $theta.labels
#> [1] "18" "19" "20" "21"
#> 
```
