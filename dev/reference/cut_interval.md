# Discretise numeric data into categorical

`cut_interval()` makes `n` groups with equal range, `cut_number()` makes
`n` groups with (approximately) equal numbers of observations;
`cut_width()` makes groups of width `width`.

## Usage

``` r
cut_interval(x, n = NULL, length = NULL, ...)

cut_number(x, n = NULL, ...)

cut_width(x, width, center = NULL, boundary = NULL, closed = "right", ...)
```

## Arguments

- x:

  numeric vector

- n:

  number of intervals to create, OR

- length:

  length of each interval

- ...:

  Arguments passed on to
  [`base::cut.default`](https://rdrr.io/r/base/cut.html)

  `breaks`

  :   either a numeric vector of two or more unique cut points or a
      single number (greater than or equal to 2) giving the number of
      intervals into which `x` is to be cut.

  `labels`

  :   labels for the levels of the resulting category. By default,
      labels are constructed using `"(a,b]"` interval notation. If
      `labels = FALSE`, simple integer codes are returned instead of a
      factor.

  `right`

  :   logical, indicating if the intervals should be closed on the right
      (and open on the left) or vice versa.

  `dig.lab`

  :   integer which is used when labels are not given. It determines the
      number of digits used in formatting the break numbers.

  `ordered_result`

  :   logical: should the result be an ordered factor?

- width:

  The bin width.

- center, boundary:

  Specify either the position of edge or the center of a bin. Since all
  bins are aligned, specifying the position of a single bin (which
  doesn't need to be in the range of the data) affects the location of
  all bins. If not specified, uses the "tile layers algorithm", and sets
  the boundary to half of the binwidth.

  To center on integers, `width = 1` and `center = 0`. `boundary = 0.5`.

- closed:

  One of `"right"` or `"left"` indicating whether right or left edges of
  bins are included in the bin.

## Author

Randall Prium contributed most of the implementation of `cut_width()`.

## Examples

``` r
table(cut_interval(1:100, 10))
#> 
#>    [1,10.9] (10.9,20.8] (20.8,30.7] (30.7,40.6] (40.6,50.5] 
#>          10          10          10          10          10 
#> (50.5,60.4] (60.4,70.3] (70.3,80.2] (80.2,90.1]  (90.1,100] 
#>          10          10          10          10          10 
table(cut_interval(1:100, 11))
#> 
#>   [1,10]  (10,19]  (19,28]  (28,37]  (37,46]  (46,55]  (55,64] 
#>       10        9        9        9        9        9        9 
#>  (64,73]  (73,82]  (82,91] (91,100] 
#>        9        9        9        9 

set.seed(1)

table(cut_number(runif(1000), 10))
#> 
#> [0.00131,0.105]   (0.105,0.201]   (0.201,0.312]   (0.312,0.398] 
#>             100             100             100             100 
#>   (0.398,0.483]   (0.483,0.596]   (0.596,0.706]   (0.706,0.797] 
#>             100             100             100             100 
#>    (0.797,0.91]        (0.91,1] 
#>             100             100 

table(cut_width(runif(1000), 0.1))
#> 
#> [-0.05,0.05]  (0.05,0.15]  (0.15,0.25]  (0.25,0.35]  (0.35,0.45] 
#>           59          109          103           96          110 
#>  (0.45,0.55]  (0.55,0.65]  (0.65,0.75]  (0.75,0.85]  (0.85,0.95] 
#>           85           89           86          113           97 
#>  (0.95,1.05] 
#>           53 
table(cut_width(runif(1000), 0.1, boundary = 0))
#> 
#>   [0,0.1] (0.1,0.2] (0.2,0.3] (0.3,0.4] (0.4,0.5] (0.5,0.6] (0.6,0.7] 
#>       106       106       108       100        99       107        84 
#> (0.7,0.8] (0.8,0.9]   (0.9,1] 
#>        96        95        99 
table(cut_width(runif(1000), 0.1, center = 0))
#> 
#> [-0.05,0.05]  (0.05,0.15]  (0.15,0.25]  (0.25,0.35]  (0.35,0.45] 
#>           72          104           80          104          100 
#>  (0.45,0.55]  (0.55,0.65]  (0.65,0.75]  (0.75,0.85]  (0.85,0.95] 
#>           91           94           75          115          110 
#>  (0.95,1.05] 
#>           55 
table(cut_width(runif(1000), 0.1, labels = FALSE))
#> 
#>   1   2   3   4   5   6   7   8   9  10  11 
#>  49  92 100  98 112 102  88  89  97 116  57 
```
