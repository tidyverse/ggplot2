# FAQ: Faceting

## Panes

### What is the difference between `facet_wrap()` and `facet_grid()`?

The simplest answer is that you should use
[`facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.md)
when faceting by a single variable and
[`facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.md)
when faceting by two variables and want to create a grid of panes.

See example

[`facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.md)
is most commonly used to facet by a plot by a single categorical
variable.

``` r
ggplot(mpg, aes(x = cty)) +
  geom_histogram() +
  facet_wrap(~ drv)
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
```

![A histogram showing the city miles per gallon distribution for three
types of drive train, each in their own panel in a 1-row, 3-column
layout.](faq-faceting_files/figure-html/unnamed-chunk-2-1.png)

And
[`facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.md)
is commonly used to facet by a plot by two categorical variables.

``` r
ggplot(mpg, aes(x = cty)) +
  geom_histogram() +
  facet_grid(cyl ~ drv)
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
```

![A histogram showing the city miles per gallon distribution. The plot
has twelve panels in a 4-row, 3-column layout, showing three types of
drive train in the horizontal direction, and four numbers of cylinders
in the vertical direction. Several panels have no
data.](faq-faceting_files/figure-html/unnamed-chunk-3-1.png)

Notice that this results in some empty panes (e.g. 4-wheel drive and 5
cylinders) as there are no cars in the `mpg` dataset that fall into such
categories.

You can also use
[`facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.md)
with to facet by two categorical variables. This will only create facets
for combinations of the levels of variables for which data exists.

``` r
ggplot(mpg, aes(x = cty)) +
  geom_histogram() +
  facet_wrap(cyl ~ drv)
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
```

![A histogram showing the city miles per gallon distribution. The plot
has nine panels in a 3-row, 3-column layout, showing all existing
combinations of three types of drive train, and four numbers of
cylinders.](faq-faceting_files/figure-html/unnamed-chunk-4-1.png)

In
[`facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.md)
you can control the number of rows and/or columns of the resulting plot
layout using the `nrow` and `ncol` arguments, respectively. In
[`facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.md)
these values are determined by the number of levels of the variables
you’re faceting by.

Similarly, you can also use
[`facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.md)
to facet by a single categorical variable as well. In the formula
notation, you use a `.` to indicate that no faceting should be done
along that axis, i.e. `cyl ~ .` facets across the y-axis (within a
column) while `. ~ cyl` facets across the x-axis (within a row).

``` r
ggplot(mpg, aes(x = cty)) +
  geom_histogram() +
  facet_grid(cyl ~ .)
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
```

![A histogram showing the city miles per gallon distribution. The plot
has four panels in a 4-row, 1-column layout, showing four numbers of
cylinders.](faq-faceting_files/figure-html/unnamed-chunk-5-1.png)

``` r

ggplot(mpg, aes(x = cty)) +
  geom_histogram() +
  facet_grid(. ~ cyl)
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
```

![A histogram showing the city miles per gallon distribution. The plot
has four panels in a 1-row, 4-column layout, showing four numbers of
cylinders.](faq-faceting_files/figure-html/unnamed-chunk-5-2.png)

### How can I place a vertical lines (`geom_vline()`) in each pane of a faceted plot?

First, calculate where the lines should be placed and save this
information in a separate data frame. Then, add a
[`geom_vline()`](https://ggplot2.tidyverse.org/reference/geom_abline.md)
layer to your plot that uses the summarized data.

See example

Suppose you have the following plot, and you want to add a vertical line
at the mean value of `hwy` (highway mileage) for each pane.

``` r
ggplot(mpg, aes(x = hwy)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~ drv)
```

![A histogram showing the highway miles per gallon distribution for
three types of drive train, each in their own panel in a 1-row, 3-column
layout.](faq-faceting_files/figure-html/unnamed-chunk-6-1.png)

First, calculate these means and save them in a new data frame.

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

mpg_summary <- mpg |>
  group_by(drv) |>
  summarise(hwy_mean = mean(hwy))

mpg_summary
#> # A tibble: 3 × 2
#>   drv   hwy_mean
#>   <chr>    <dbl>
#> 1 4         19.2
#> 2 f         28.2
#> 3 r         21
```

Then, add a
[`geom_vline()`](https://ggplot2.tidyverse.org/reference/geom_abline.md)
layer to your plot that uses the summary data.

``` r
ggplot(mpg, aes(x = hwy)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~ drv) +
  geom_vline(data = mpg_summary, aes(xintercept = hwy_mean))
```

![A histogram showing the highway miles per gallon distribution for
three types of drive train, each in their own panel in a 1-row, 3-column
layout. Each panel has a vertical black line indicating the mean of the
distribution.](faq-faceting_files/figure-html/unnamed-chunk-8-1.png)

## Axes

### How can I set individual axis limits for facets?

Either let ggplot2 determine custom axis limits for the facets based on
the range of the data you’re plotting using the `scales` argument in
[`facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.md)
or
[`facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.md)
or, if that is not sufficient, use
[`expand_limits()`](https://ggplot2.tidyverse.org/reference/expand_limits.md)
to ensure limits include a single value or a range of values.

See example

Suppose you have the following faceted plot. By default, both x and y
scales are shared across the facets.

``` r
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point() +
  facet_grid(cyl ~ drv)
```

![A scatter plot showing city miles per gallon on the x-axis and highway
miles per gallon on the y-axis. The plot has twelve panels in a 4-row,
3-column layout, showing three types of drive train in the horizontal
direction and four numbers of cylinders in the vertical direction.
Several panels are empty. Every row has the same y-axis range, and every
column has the same x-axis
range.](faq-faceting_files/figure-html/unnamed-chunk-9-1.png)

You can control this behaviour with the `scales` argument of faceting
functions: varying scales across rows (`"free_x"`), columns
(`"free_y"`), or both rows and columns (`"free"`), e.g.

``` r
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point() +
  facet_grid(cyl ~ drv, scales = "free") 
```

![A scatter plot showing city miles per gallon on the x-axis and highway
miles per gallon on the y-axis. The plot has twelve panels in a 4-row,
3-column layout, showing three types of drive train in the horizontal
direction and four numbers of cylinders in the vertical direction.
Several panels are empty. Every row in the layout has an independent
y-axis range. Every column in the layout has an independent x-axis
range.](faq-faceting_files/figure-html/unnamed-chunk-10-1.png)

If you also want to make sure that a particular value or range is
included in each of the facets, you can set this with
[`expand_limits()`](https://ggplot2.tidyverse.org/reference/expand_limits.md),
e.g. ensure that 10 is included in the x-axis and values between 20 to
25 are included in the y-axis:

``` r
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point() +
  facet_grid(cyl ~ drv, scales = "free") +
  expand_limits(x = 10, y = c(20, 25))
```

![A scatter plot showing city miles per gallon on the x-axis and highway
miles per gallon on the y-axis. The plot has twelve panels in a 4-row,
3-column layout, showing three types of drive train in the horizontal
direction and four numbers of cylinders in the vertical direction.
Several panels are empty. Every row in the layout has an independent
y-axis range, but all include the 20-25 interval. Every column in the
layout has an independent x-axis range, but all include
10.](faq-faceting_files/figure-html/unnamed-chunk-11-1.png)

## Facet labels

### How can I remove the facet labels entirely?

Set the `strip.text` element in
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.md) to
[`element_blank()`](https://ggplot2.tidyverse.org/reference/element.md).

See example

Setting `strip.text` to
[`element_blank()`](https://ggplot2.tidyverse.org/reference/element.md)
will remove all facet labels.

``` r
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point() +
  facet_grid(cyl ~ drv) +
  theme(strip.text = element_blank())
```

![A scatter plot showing city miles per gallon on the x-axis and highway
miles per gallon on the y-axis. The plot has twelve panels in a 4-row,
3-column layout. The strips, or panel layout titles and their
backgrounds, are
missing.](faq-faceting_files/figure-html/unnamed-chunk-12-1.png)

You can also remove the labels across rows only with `strip.x.text` or
across columns only with `strip.y.text`.

``` r
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point() +
  facet_grid(cyl ~ drv) +
  theme(strip.text.x = element_blank())
```

![A scatter plot showing city miles per gallon on the x-axis and highway
miles per gallon on the y-axis. The plot has twelve panels in a 4-row,
3-column layout. In the vertical direction, the panels indicate four
numbers of cylinders. The strips of the horizontal direction are
missing.](faq-faceting_files/figure-html/unnamed-chunk-13-1.png)

### The facet labels in my plot are too long so they get cut off. How can I wrap facet label text so that long labels are spread across two rows?

Use
[`label_wrap_gen()`](https://ggplot2.tidyverse.org/reference/labellers.md)
in the `labeller` argument of your faceting function and set a `width`
(number of characters) for the maximum number of characters before
wrapping the strip.

See example

In the data frame below we have 100 observations, 50 of them come from
one group and 50 from another. These groups have very long names, and so
when you facet the ploy by group, the facet labels (strips) get cut off.

``` r
df <- data.frame(
  x = rnorm(100),
  group = c(rep("A long group name for the first group", 50),
            rep("A muuuuuuuuuuuuuch longer group name for the second group", 50))
)

ggplot(df, aes(x = x)) +
  geom_histogram(binwidth = 0.5) +
  facet_wrap(~ group)
```

![A histogram with two panels in a 1-row, 2-column layout of random
data. The first panel has as title 'A long group name for the first
group'. The second panel has a title 'A muuuuuuuuuuuuuch longer group
name for the second group'. However, the second title is clipped to the
panel width and doesn't show all the
text.](faq-faceting_files/figure-html/unnamed-chunk-14-1.png)

You can control the maximum width of the facet label by setting the
`width` in the
[`label_wrap_gen()`](https://ggplot2.tidyverse.org/reference/labellers.md)
function, which is then passed to the `labeller` argument of your
faceting function.

``` r
ggplot(df, aes(x = x)) +
  geom_histogram(binwidth = 0.5) +
  facet_wrap(~ group, labeller = labeller(group = label_wrap_gen(width = 25)))
```

![A histogram with two panels in a 1-row, 2-column layout of random
data. The first panel has as title 'A long group name for the first
group' in two lines of text. The second panel has a title 'A
muuuuuuuuuuuuuch longer group name for the second group' in three lines
of text. The width of the second title now fits within the panel
width.](faq-faceting_files/figure-html/unnamed-chunk-15-1.png)

### How can I set different axis labels for facets?

Use
[`as_labeller()`](https://ggplot2.tidyverse.org/reference/as_labeller.md)
in the `labeller` argument of your faceting function and then set
`strip.background` and `strip.placement` elements in the
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.md) to place
the facet labels where axis labels would go. This is a particularly
useful solution for plotting data on different scales without the use of
double y-axes.

See example

Suppose you have data price data on a given item over a few years from
two countries with very different currency scales.

``` r
df <- data.frame(
  year = rep(2016:2021, 2),
  price = c(10, 10, 13, 12, 14, 15, 1000, 1010, 1200, 1050, 1105, 1300),
  country = c(rep("US", 6), rep("Japan", 6))
)

df
#>    year price country
#> 1  2016    10      US
#> 2  2017    10      US
#> 3  2018    13      US
#> 4  2019    12      US
#> 5  2020    14      US
#> 6  2021    15      US
#> 7  2016  1000   Japan
#> 8  2017  1010   Japan
#> 9  2018  1200   Japan
#> 10 2019  1050   Japan
#> 11 2020  1105   Japan
#> 12 2021  1300   Japan
```

You can plot `price` versus `time` and facet by `country`, but the
resulting plot can be a bit difficult to read due to the shared y-axis
label.

``` r
ggplot(df, aes(x = year, y = price)) +
  geom_smooth() +
  facet_wrap(~ country, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = 2011:2020)
```

![A timeseries plot showing price over time for two countries, Japan and
the US, in two panels in a 2-row, 1-column layout. The countries are
indicated at the top of each panel. The two y-axes have different
ranges.](faq-faceting_files/figure-html/unnamed-chunk-17-1.png)

With the following you can customize the facet labels first with
[`as_labeller()`](https://ggplot2.tidyverse.org/reference/as_labeller.md),
turn off the default y-axis label, and then place the facet labels where
the y-axis label goes (`"outside"` and on the `"left"`).

``` r
ggplot(df, aes(x = year, y = price)) +
  geom_smooth() +
  facet_wrap(~ country, ncol = 1, scales = "free_y", 
             labeller = as_labeller(
               c(US = "US Dollars (USD)", Japan = "Japanese Yens (JPY)")), 
             strip.position = "left"
             ) +
  scale_x_continuous(breaks = 2011:2020) +
  labs(y = NULL) +
  theme(strip.background = element_blank(), strip.placement = "outside")
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

![A timeseries plot showing price over time for two countries and their
currencies, the Japanese Yen and the US Dollar, in two panels in a
2-row, 1-column layout. The countries and currency units are indicated
at the left of each panel. The two y-axes have different
ranges.](faq-faceting_files/figure-html/unnamed-chunk-18-1.png)
