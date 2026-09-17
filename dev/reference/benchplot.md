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
#> 2     build     0.020        0   0.020
#> 3    render     0.030        0   0.029
#> 4      draw     0.014        0   0.013
#> 5     TOTAL     0.068        0   0.067
benchplot(ggplot(mtcars, aes(mpg, wt)) + geom_point() + facet_grid(. ~ cyl))
#>        step user.self sys.self elapsed
#> 1 construct     0.004        0   0.005
#> 2     build     0.021        0   0.022
#> 3    render     0.065        0   0.066
#> 4      draw     0.023        0   0.023
#> 5     TOTAL     0.113        0   0.116

# With tidy eval:
p <- expr(ggplot(mtcars, aes(mpg, wt)) + geom_point())
benchplot(!!p)

#>        step user.self sys.self elapsed
#> 1 construct     0.004        0   0.004
#> 2     build     0.020        0   0.020
#> 3    render     0.029        0   0.030
#> 4      draw     0.014        0   0.013
#> 5     TOTAL     0.067        0   0.067
```
