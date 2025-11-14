# Modify fill transparency

This works much like
[alpha()](https://scales.r-lib.org/reference/alpha.html) in that it
modifies the transparency of fill colours. It differs in that
`fill_alpha()` also attempts to set the transparency of `<GridPattern>`
objects.

## Usage

``` r
fill_alpha(fill, alpha)
```

## Arguments

- fill:

  A fill colour given as a `character` or `integer` vector, or as a
  (list of) `<GridPattern>` object(s).

- alpha:

  A transparency value between 0 (transparent) and 1 (opaque), parallel
  to `fill`.

## Value

A `character` vector of colours, or list of `<GridPattern>` objects.

## Examples

``` r
# Typical colour input
fill_alpha("red", 0.5)
#> [1] "#FF000080"

if (utils::packageVersion("grid") > "4.2") {
  # Pattern input
  fill_alpha(list(grid::linearGradient()), 0.5)
}
#> [[1]]
#> $x1
#> [1] 0npc
#> 
#> $y1
#> [1] 0npc
#> 
#> $x2
#> [1] 1npc
#> 
#> $y2
#> [1] 1npc
#> 
#> $stops
#> [1] 0 1
#> 
#> $colours
#> [1] "#00000080" "#FFFFFF80"
#> 
#> $extend
#> [1] "pad"
#> 
#> $group
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "GridLinearGradient" "GridPattern"       
#> 
```
