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
#> 2     build     0.027        0   0.027
#> 3    render     0.039        0   0.040
#> 4      draw     0.022        0   0.022
#> 5     TOTAL     0.092        0   0.094
benchplot(ggplot(mtcars, aes(mpg, wt)) + geom_point() + facet_grid(. ~ cyl))
#>        step user.self sys.self elapsed
#> 1 construct     0.005        0   0.005
#> 2     build     0.028        0   0.028
#> 3    render     0.090        0   0.090
#> 4      draw     0.036        0   0.036
#> 5     TOTAL     0.159        0   0.159

# With tidy eval:
p <- expr(ggplot(mtcars, aes(mpg, wt)) + geom_point())
benchplot(!!p)

#>        step user.self sys.self elapsed
#> 1 construct     0.005        0   0.004
#> 2     build     0.026        0   0.026
#> 3    render     0.039        0   0.039
#> 4      draw     0.020        0   0.019
#> 5     TOTAL     0.090        0   0.088
```
