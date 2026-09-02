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
#> 1 construct     0.005        0   0.004
#> 2     build     0.026        0   0.025
#> 3    render     0.037        0   0.037
#> 4      draw     0.017        0   0.017
#> 5     TOTAL     0.085        0   0.083
benchplot(ggplot(mtcars, aes(mpg, wt)) + geom_point() + facet_grid(. ~ cyl))
#>        step user.self sys.self elapsed
#> 1 construct     0.005        0   0.005
#> 2     build     0.027        0   0.027
#> 3    render     0.082        0   0.081
#> 4      draw     0.028        0   0.029
#> 5     TOTAL     0.142        0   0.142

# With tidy eval:
p <- expr(ggplot(mtcars, aes(mpg, wt)) + geom_point())
benchplot(!!p)

#>        step user.self sys.self elapsed
#> 1 construct     0.005        0   0.005
#> 2     build     0.025        0   0.025
#> 3    render     0.036        0   0.036
#> 4      draw     0.017        0   0.016
#> 5     TOTAL     0.083        0   0.082
```
