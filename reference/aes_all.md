# Given a character vector, create a set of identity mappings

Given a character vector, create a set of identity mappings

## Usage

``` r
aes_all(vars)
```

## Arguments

- vars:

  vector of variable names

## Examples

``` r
aes_all(names(mtcars))
#> Aesthetic mapping: 
#> * `mpg`  -> `mpg`
#> * `cyl`  -> `cyl`
#> * `disp` -> `disp`
#> * `hp`   -> `hp`
#> * `drat` -> `drat`
#> * `wt`   -> `wt`
#> * `qsec` -> `qsec`
#> * `vs`   -> `vs`
#> * `am`   -> `am`
#> * `gear` -> `gear`
#> * `carb` -> `carb`
aes_all(c("x", "y", "col", "pch"))
#> Aesthetic mapping: 
#> * `x`      -> `x`
#> * `y`      -> `y`
#> * `colour` -> `col`
#> * `shape`  -> `pch`
```
