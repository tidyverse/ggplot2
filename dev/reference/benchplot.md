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
#> 2     build     0.028        0   0.028
#> 3    render     0.042        0   0.042
#> 4      draw     0.020        0   0.021
#> 5     TOTAL     0.095        0   0.096
benchplot(ggplot(mtcars, aes(mpg, wt)) + geom_point() + facet_grid(. ~ cyl))
#>        step user.self sys.self elapsed
#> 1 construct     0.005        0   0.005
#> 2     build     0.030        0   0.030
#> 3    render     0.094        0   0.094
#> 4      draw     0.036        0   0.036
#> 5     TOTAL     0.165        0   0.165

# With tidy eval:
p <- expr(ggplot(mtcars, aes(mpg, wt)) + geom_point())
benchplot(!!p)

#>        step user.self sys.self elapsed
#> 1 construct     0.005        0   0.004
#> 2     build     0.027        0   0.028
#> 3    render     0.043        0   0.043
#> 4      draw     0.021        0   0.021
#> 5     TOTAL     0.096        0   0.096
```
