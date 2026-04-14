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
#> 2     build     0.028        0   0.027
#> 3    render     0.041        0   0.041
#> 4      draw     0.021        0   0.021
#> 5     TOTAL     0.095        0   0.094
benchplot(ggplot(mtcars, aes(mpg, wt)) + geom_point() + facet_grid(. ~ cyl))
#>        step user.self sys.self elapsed
#> 1 construct     0.005        0   0.005
#> 2     build     0.030        0   0.030
#> 3    render     0.093        0   0.092
#> 4      draw     0.036        0   0.036
#> 5     TOTAL     0.164        0   0.163

# With tidy eval:
p <- expr(ggplot(mtcars, aes(mpg, wt)) + geom_point())
benchplot(!!p)

#>        step user.self sys.self elapsed
#> 1 construct     0.005        0   0.005
#> 2     build     0.028        0   0.028
#> 3    render     0.041        0   0.041
#> 4      draw     0.021        0   0.021
#> 5     TOTAL     0.095        0   0.095
```
