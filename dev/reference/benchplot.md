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
#> 1 construct     0.004        0   0.004
#> 2     build     0.021        0   0.021
#> 3    render     0.031        0   0.031
#> 4      draw     0.014        0   0.013
#> 5     TOTAL     0.070        0   0.069
benchplot(ggplot(mtcars, aes(mpg, wt)) + geom_point() + facet_grid(. ~ cyl))
#>        step user.self sys.self elapsed
#> 1 construct     0.004        0   0.004
#> 2     build     0.023        0   0.023
#> 3    render     0.071        0   0.071
#> 4      draw     0.023        0   0.024
#> 5     TOTAL     0.121        0   0.122

# With tidy eval:
p <- expr(ggplot(mtcars, aes(mpg, wt)) + geom_point())
benchplot(!!p)

#>        step user.self sys.self elapsed
#> 1 construct     0.003        0   0.003
#> 2     build     0.021        0   0.021
#> 3    render     0.030        0   0.030
#> 4      draw     0.013        0   0.013
#> 5     TOTAL     0.067        0   0.067
```
