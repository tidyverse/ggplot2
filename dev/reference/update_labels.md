# Update axis/legend labels

Update axis/legend labels

## Usage

``` r
update_labels(p, labels)
```

## Arguments

- p:

  plot to modify

- labels:

  named list of new labels

## Examples

``` r
p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
update_labels(p, list(x = "New x"))

update_labels(p, list(x = expression(x / y ^ 2)))

update_labels(p, list(x = "New x", y = "New Y"))

update_labels(p, list(colour = "Fail silently"))
#> Ignoring unknown labels:
#> • colour : "Fail silently"
```
