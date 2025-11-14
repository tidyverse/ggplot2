# Modify geom/stat aesthetic defaults for future plots

Functions to update or reset the default aesthetics of geoms and stats.

## Usage

``` r
update_geom_defaults(geom, new)

update_stat_defaults(stat, new)

reset_geom_defaults()

reset_stat_defaults()
```

## Arguments

- new:

  One of the following:

  - A named list of aesthetics to serve as new defaults.

  - `NULL` to reset the defaults.

- stat, geom:

  Name of geom/stat to modify (like `"point"` or `"bin"`), or a
  Geom/Stat object (like `GeomPoint` or `StatBin`).

## Note

Please note that geom defaults can be set *en masse* via the
`theme(geom)` argument. The guidelines for when to use which function
are as follows:

- If you want to change defaults for all geoms in all plots, use
  `theme_update(geom = element_geom(...))`.

- If you want to change defaults for all geoms in a single plot, use
  `+ theme(geom = element_geom(...))`.

- If you want to change defaults for one geom in all plots, use
  `update_geom_defaults()`.

- If you want to change settings for one geom in a single plot, use
  fixed aesthetic parameters in a layer, like so:
  `geom_point(colour = "red")`.

## Examples

``` r
# updating a geom's default aesthetic settings
# example: change geom_point()'s default color
GeomPoint$default_aes
#> Aesthetic mapping: 
#> * `shape`  -> `from_theme(pointshape)`
#> * `colour` -> `from_theme(colour %||% ink)`
#> * `fill`   -> `from_theme(fill %||% NA)`
#> * `size`   -> `from_theme(pointsize)`
#> * `alpha`  -> NA
#> * `stroke` -> `from_theme(borderwidth)`
update_geom_defaults("point", aes(color = "red"))
GeomPoint$default_aes
#> Aesthetic mapping: 
#> * `shape`  -> `from_theme(pointshape)`
#> * `colour` -> "red"
#> * `fill`   -> `from_theme(fill %||% NA)`
#> * `size`   -> `from_theme(pointsize)`
#> * `alpha`  -> NA
#> * `stroke` -> `from_theme(borderwidth)`
ggplot(mtcars, aes(mpg, wt)) + geom_point()


# reset single default
update_geom_defaults("point", NULL)

# reset all defaults
reset_geom_defaults()

# updating a stat's default aesthetic settings
# example: change stat_bin()'s default y-axis to the density scale
StatBin$default_aes
#> Aesthetic mapping: 
#> * `x`      -> `after_stat(count)`
#> * `y`      -> `after_stat(count)`
#> * `weight` -> 1
update_stat_defaults("bin", aes(y = after_stat(density)))
StatBin$default_aes
#> Aesthetic mapping: 
#> * `x`      -> `after_stat(count)`
#> * `y`      -> `after_stat(density)`
#> * `weight` -> 1
ggplot(data.frame(x = rnorm(1e3)), aes(x)) +
  geom_histogram() +
  geom_function(fun = dnorm, color = "red")
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.


# reset single default
update_stat_defaults("bin", NULL)

# reset all defaults
reset_stat_defaults()
```
