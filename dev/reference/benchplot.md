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
#> 2     build     0.025        0   0.025
#> 3    render     0.039        0   0.039
#> 4      draw     0.019        0   0.019
#> 5     TOTAL     0.087        0   0.087
benchplot(ggplot(mtcars, aes(mpg, wt)) + geom_point() + facet_grid(. ~ cyl))
#>        step user.self sys.self elapsed
#> 1 construct     0.005        0   0.005
#> 2     build     0.027        0   0.027
#> 3    render     0.088        0   0.088
#> 4      draw     0.033        0   0.034
#> 5     TOTAL     0.153        0   0.154

# With tidy eval:
p <- expr(ggplot(mtcars, aes(mpg, wt)) + geom_point())
benchplot(!!p)

#>        step user.self sys.self elapsed
#> 1 construct     0.004        0   0.004
#> 2     build     0.026        0   0.025
#> 3    render     0.037        0   0.038
#> 4      draw     0.019        0   0.019
#> 5     TOTAL     0.086        0   0.086
```
