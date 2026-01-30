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
#> 2     build     0.027        0   0.028
#> 3    render     0.041        0   0.042
#> 4      draw     0.020        0   0.020
#> 5     TOTAL     0.093        0   0.095
benchplot(ggplot(mtcars, aes(mpg, wt)) + geom_point() + facet_grid(. ~ cyl))
#>        step user.self sys.self elapsed
#> 1 construct     0.005        0   0.005
#> 2     build     0.029        0   0.029
#> 3    render     0.098        0   0.098
#> 4      draw     0.035        0   0.035
#> 5     TOTAL     0.167        0   0.167

# With tidy eval:
p <- expr(ggplot(mtcars, aes(mpg, wt)) + geom_point())
benchplot(!!p)

#>        step user.self sys.self elapsed
#> 1 construct     0.004        0   0.004
#> 2     build     0.027        0   0.027
#> 3    render     0.042        0   0.042
#> 4      draw     0.020        0   0.020
#> 5     TOTAL     0.093        0   0.093
```
