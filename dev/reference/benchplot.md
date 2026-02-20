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
#> 2     build     0.024        0   0.024
#> 3    render     0.036        0   0.035
#> 4      draw     0.017        0   0.017
#> 5     TOTAL     0.082        0   0.080
benchplot(ggplot(mtcars, aes(mpg, wt)) + geom_point() + facet_grid(. ~ cyl))
#>        step user.self sys.self elapsed
#> 1 construct     0.004        0   0.005
#> 2     build     0.025        0   0.025
#> 3    render     0.081        0   0.081
#> 4      draw     0.030        0   0.030
#> 5     TOTAL     0.140        0   0.141

# With tidy eval:
p <- expr(ggplot(mtcars, aes(mpg, wt)) + geom_point())
benchplot(!!p)

#>        step user.self sys.self elapsed
#> 1 construct     0.003        0   0.003
#> 2     build     0.023        0   0.023
#> 3    render     0.035        0   0.035
#> 4      draw     0.017        0   0.017
#> 5     TOTAL     0.078        0   0.078
```
