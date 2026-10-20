# Benchmark plot creation time. Broken down into construct, build, render and draw times.

Benchmark plot creation time. Broken down into construct, build, render
and draw times.

## Usage

``` r
benchplot(x)
```

## Arguments

- x:

  code to create ggplot2 plot

## Examples

``` r
benchplot(ggplot(mtcars, aes(mpg, wt)) + geom_point())
#>        step user.self sys.self elapsed
#> 1 construct     0.004        0   0.005
#> 2     build     0.017        0   0.017
#> 3    render     0.024        0   0.024
#> 4      draw     0.010        0   0.011
#> 5     TOTAL     0.055        0   0.057
benchplot(ggplot(mtcars, aes(mpg, wt)) + geom_point() + facet_grid(. ~ cyl))
#>        step user.self sys.self elapsed
#> 1 construct     0.004        0   0.004
#> 2     build     0.017        0   0.017
#> 3    render     0.050        0   0.050
#> 4      draw     0.017        0   0.017
#> 5     TOTAL     0.088        0   0.088

# With tidy eval:
p <- expr(ggplot(mtcars, aes(mpg, wt)) + geom_point())
benchplot(!!p)

#>        step user.self sys.self elapsed
#> 1 construct     0.004        0   0.004
#> 2     build     0.016        0   0.016
#> 3    render     0.023        0   0.023
#> 4      draw     0.011        0   0.011
#> 5     TOTAL     0.054        0   0.054
```
