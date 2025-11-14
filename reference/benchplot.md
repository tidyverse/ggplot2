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
#> 2     build     0.026        0   0.026
#> 3    render     0.039        0   0.039
#> 4      draw     0.020        0   0.019
#> 5     TOTAL     0.090        0   0.089
benchplot(ggplot(mtcars, aes(mpg, wt)) + geom_point() + facet_grid(. ~ cyl))
#>        step user.self sys.self elapsed
#> 1 construct     0.005        0   0.005
#> 2     build     0.028        0   0.028
#> 3    render     0.091        0   0.091
#> 4      draw     0.034        0   0.034
#> 5     TOTAL     0.158        0   0.158

# With tidy eval:
p <- expr(ggplot(mtcars, aes(mpg, wt)) + geom_point())
benchplot(!!p)

#>        step user.self sys.self elapsed
#> 1 construct     0.004    0.001   0.005
#> 2     build     0.026    0.000   0.026
#> 3    render     0.039    0.000   0.039
#> 4      draw     0.019    0.000   0.019
#> 5     TOTAL     0.088    0.001   0.089
```
