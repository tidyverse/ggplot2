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
#> 2     build     0.028        0   0.027
#> 3    render     0.039        0   0.039
#> 4      draw     0.017        0   0.017
#> 5     TOTAL     0.088        0   0.088
benchplot(ggplot(mtcars, aes(mpg, wt)) + geom_point() + facet_grid(. ~ cyl))
#>        step user.self sys.self elapsed
#> 1 construct     0.006        0   0.006
#> 2     build     0.029        0   0.029
#> 3    render     0.085        0   0.086
#> 4      draw     0.030        0   0.030
#> 5     TOTAL     0.150        0   0.151

# With tidy eval:
p <- expr(ggplot(mtcars, aes(mpg, wt)) + geom_point())
benchplot(!!p)

#>        step user.self sys.self elapsed
#> 1 construct     0.004        0   0.005
#> 2     build     0.026        0   0.026
#> 3    render     0.037        0   0.037
#> 4      draw     0.017        0   0.017
#> 5     TOTAL     0.084        0   0.085
```
