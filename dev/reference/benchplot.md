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
#> 1 construct     0.005        0   0.005
#> 2     build     0.025        0   0.025
#> 3    render     0.037        0   0.037
#> 4      draw     0.018        0   0.017
#> 5     TOTAL     0.085        0   0.084
benchplot(ggplot(mtcars, aes(mpg, wt)) + geom_point() + facet_grid(. ~ cyl))
#>        step user.self sys.self elapsed
#> 1 construct     0.005        0   0.004
#> 2     build     0.026        0   0.026
#> 3    render     0.083        0   0.083
#> 4      draw     0.031        0   0.030
#> 5     TOTAL     0.145        0   0.143

# With tidy eval:
p <- expr(ggplot(mtcars, aes(mpg, wt)) + geom_point())
benchplot(!!p)

#>        step user.self sys.self elapsed
#> 1 construct     0.004        0   0.004
#> 2     build     0.024        0   0.025
#> 3    render     0.036        0   0.036
#> 4      draw     0.017        0   0.017
#> 5     TOTAL     0.081        0   0.082
```
