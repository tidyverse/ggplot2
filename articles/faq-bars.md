# FAQ: Barplots

## Colors

### How can I change the color of the bars in my bar plot?

If using the same color for all bars, define the `fill` argument in
[`geom_bar()`](https://ggplot2.tidyverse.org/reference/geom_bar.md) (or
[`geom_col()`](https://ggplot2.tidyverse.org/reference/geom_bar.md)). If
assigning color based on another variable, map the variable to the
`fill` `aes`thetic, and if needed, use one of the `scale_fill_*()`
functions to set colors.

See example

You can set all bars to be a given color with the `fill` argument of
[`geom_bar()`](https://ggplot2.tidyverse.org/reference/geom_bar.md).

``` r
ggplot(mpg, aes(x = drv)) +
  geom_bar(fill = "blue")
```

![A bar chart showing the number of cars for each of three types of
drive train. All bars are
blue.](faq-bars_files/figure-html/unnamed-chunk-2-1.png)

Alternatively, if the colors should be based on a variable, this should
be should happen in the
[`aes()`](https://ggplot2.tidyverse.org/reference/aes.md) mapping.

``` r
ggplot(mpg, aes(x = drv, fill = drv)) +
  geom_bar()
```

![A bar chart showing the number of cars for each of three types of
drive train. From left-to-right, the bars appear red, green and
blue.](faq-bars_files/figure-html/unnamed-chunk-3-1.png)

And if you want to then customize the colors, one option is
[`scale_fill_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.md),
which allows you to manually assign colors to each bar. See other
`scale_fill_*()` functions for more options for color choices.

``` r
ggplot(mpg, aes(x = drv, fill = drv)) +
  geom_bar() +
  scale_fill_manual(values = c("purple", "orange", "darkblue"))
```

![A bar chart showing the number of cars for each of three types of
drive train. From left-to-right, the bars are purple, orange and dark
blue.](faq-bars_files/figure-html/unnamed-chunk-4-1.png)

## Spacing and widths

### How can I increase the space between the bars in my bar plot?

Set the `width` of
[`geom_bar()`](https://ggplot2.tidyverse.org/reference/geom_bar.md) to a
small value to obtain narrower bars with more space between them.

See example

By default, the `width` of bars is `0.9` (90% of the resolution of the
data). You can set this argument to a lower value to get bars that are
narrower with more space between them.

``` r
ggplot(mpg, aes(x = drv)) +
  geom_bar(width = 0.5)
```

![A bar chart showing the number of cars for each of three types of
drive train. The bars are somewhat narrower than the
default.](faq-bars_files/figure-html/unnamed-chunk-5-1.png)

``` r

ggplot(mpg, aes(x = drv)) +
  geom_bar(width = 0.1)
```

![A bar chart showing the number of cars for each of three types of
drive train. The bars are very
narrow.](faq-bars_files/figure-html/unnamed-chunk-5-2.png)

### How can I remove the space between the bars and the x-axis?

Adjust the `expand` argument in
[`scale_y_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.md),
e.g. add `scale_y_continuous(expand = expansion(mult = c(0, 0.05)))` to
remove the expansion on the lower end of the y-axis but keep the
expansion on the upper end of the y-axis at 0.05 (the default expansion
for continuous scales).

See example

By default ggplot2 expands the axes so the geoms aren’t flush against
the edges of the plot.

``` r
ggplot(mpg, aes(x = drv)) +
  geom_bar()
```

![A bar chart showing the number of cars for each of three types of
drive train. No parts of the bars touch the panel
edges.](faq-bars_files/figure-html/unnamed-chunk-6-1.png)

To remove the spacing between the bars and the x-axis, but keep the
spacing between the bars and the top of the plot, use the following.

``` r
ggplot(mpg, aes(x = drv)) +
  geom_bar() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
```

![A bar chart showing the number of cars for each of three types of
drive train. The bottom of the bars touch the
x-axis.](faq-bars_files/figure-html/unnamed-chunk-7-1.png)

To achieve the opposite, switch the values in `mult`. Note that the
tallest bar is now flush against top of the plot.

``` r
ggplot(mpg, aes(x = drv)) +
  geom_bar() +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0)))
```

![A bar chart showing the number of cars for each of three types of
drive train. The top of the highest bar touches the top of the
panel.](faq-bars_files/figure-html/unnamed-chunk-8-1.png)

To adjust spacing around the x-axis, adjust the `expand` argument in
[`scale_x_discrete()`](https://ggplot2.tidyverse.org/reference/scale_discrete.md).
Note that this places the bars flush against the left side and leaves
some space on the right side.

``` r
ggplot(mpg, aes(x = drv)) +
  geom_bar() +
  scale_x_discrete(expand = expansion(add = c(0, 0.6)))
```

![A bar chart showing the number of cars for each of three types of
drive train. The left of the leftmost bar touches the
y-axis.](faq-bars_files/figure-html/unnamed-chunk-9-1.png)

The default look of a bar plot can be achieved with the following.

``` r
ggplot(mpg, aes(x = drv)) +
  geom_bar() +
  scale_x_discrete(expand = expansion(add = 0.6)) +
  scale_y_continuous(expand = expansion(mult = 0.05))
```

![A bar chart showing the number of cars for each of three types of
drive train. No parts of the bars touch the panel
edges.](faq-bars_files/figure-html/unnamed-chunk-10-1.png)

### How do I ensure that bars on a dodged bar plot have the same width?

Set `position = position_dodge2(preserve = "single")` in
[`geom_bar()`](https://ggplot2.tidyverse.org/reference/geom_bar.md).

See example

In the following plot the bars have differing widths within each level
of `drv` as there are differing levels of `class` represented.

``` r
ggplot(mpg, aes(x = drv, fill = class)) +
  geom_bar(position = "dodge")
```

![A grouped bar chart showing car counts dodged and filled by 7 types of
cars for each of three types of drive train. The left group has 5
narrower bars, the middle group has 4 bars and the right group has 3
wider bars.](faq-bars_files/figure-html/unnamed-chunk-11-1.png)

You can use
[`position_dodge2()`](https://ggplot2.tidyverse.org/reference/position_dodge.md)
with `preserve = "single"` to address this.

``` r
ggplot(mpg, aes(x = drv, fill = class)) +
  geom_bar(position = position_dodge2(preserve = "single"))
```

![A grouped bar chart showing car counts dodged and filled by 7 types of
cars for each of three types of drive train. From left-to-right, each
groups has respectively 5, 4 and 3 equally wide
bars.](faq-bars_files/figure-html/unnamed-chunk-12-1.png)

## Stacked bar plots

### How can I create a stacked bar plot displaying a conditional distribution where each stack is scaled to sum to 100%?

Use `position = "fill"` in
[`geom_bar()`](https://ggplot2.tidyverse.org/reference/geom_bar.md) or
[`geom_col()`](https://ggplot2.tidyverse.org/reference/geom_bar.md). If
you also want to show percentages on the axis, use
[`scales::label_percent()`](https://scales.r-lib.org/reference/label_percent.html).

See example

The following plot is useful for comparing counts but not as useful for
comparing proportions, which is what you need if you want to be able to
make statements like “in this sample, it’s more likely to have a
two-seater car that has rear-wheel drive than an SUV that has rear-wheel
drive”.

``` r
ggplot(mpg, aes(y = class, fill = drv)) +
  geom_bar()
```

![A horizontal stacked bar chart showing car counts for 7 types of cars,
stacked and filled by 3 types of drive
train.](faq-bars_files/figure-html/unnamed-chunk-13-1.png)

`position = "fill"` will generate a bar plot with bars of equal length
and the stacks in each bar will show the proportion of `drv` for that
particular `class`.

``` r
ggplot(mpg, aes(y = class, fill = drv)) +
  geom_bar(position = "fill")
```

![A horizontal filled bar chart showing proportions of cars for 7 types
of cars. The fill colour represents 3 types of drive train. Every
stacked bar spans the width of the
panel.](faq-bars_files/figure-html/unnamed-chunk-14-1.png)

If you want to show percentages instead of proportions on the x-axis,
you can define this in
[`scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.md)
with
[`scales::label_percent()`](https://scales.r-lib.org/reference/label_percent.html).

``` r
ggplot(mpg, aes(y = class, fill = drv)) +
  geom_bar(position = "fill") +
  scale_x_continuous(name = "percentage", labels = scales::label_percent(accuracy = 1))
```

![A horizontal filled bar chart showing percentages of cars for 7 types
of cars. The fill colour represents 3 types of drive train. Every
stacked bar spans the width of the
panel.](faq-bars_files/figure-html/unnamed-chunk-15-1.png)

### How can I create a stacked bar plot based on data from a contingency table of to categorical variables?

First reshape the data (e.g. with
[`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html))
so that there is one row per each combination of the levels of the
categorical variables, then use
[`geom_col()`](https://ggplot2.tidyverse.org/reference/geom_bar.md) to
draw the bars.

See example

Suppose you have the following data from an opinion poll, where the
numbers in the cells represent the number of responses for each
party/opinion combination.

``` r
poll <- tribble(
  ~party,       ~agree, ~disagree, ~no_opinion,
  "Democrat",    20,    30,        20,
  "Republican",  15,    20,        10,
  "Independent", 10,    5,         0
)
```

You can first pivot the data longer to obtain a data frame with one row
per party/opinion combination and a new column, `n`, for the number of
responses that fall into that category.

``` r
poll_longer <- poll |>
  pivot_longer(
    cols = -party,
    names_to = "opinion",
    values_to = "n"
  )

poll_longer
#> # A tibble: 9 × 3
#>   party       opinion        n
#>   <chr>       <chr>      <dbl>
#> 1 Democrat    agree         20
#> 2 Democrat    disagree      30
#> 3 Democrat    no_opinion    20
#> 4 Republican  agree         15
#> 5 Republican  disagree      20
#> 6 Republican  no_opinion    10
#> 7 Independent agree         10
#> 8 Independent disagree       5
#> 9 Independent no_opinion     0
```

Then, you can pass this result to
[`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.md) and
create a bar for each `party` on the `y` (or `x`, if you prefer vertical
bars) axis and fill the bars in with number of responses for each
`opinion`.

``` r
ggplot(poll_longer, aes(y = party, fill = opinion, x = n)) +
  geom_col()
```

![A horizontal stacked bar chart showing opinion counts for 3 parties,
stacked and filled by 3 types of
opinions.](faq-bars_files/figure-html/unnamed-chunk-18-1.png)

To plot proportions (relative frequencies) instead of counts, use
`position = "fill"` in
[`geom_col()`](https://ggplot2.tidyverse.org/reference/geom_bar.md).

``` r
ggplot(poll_longer, aes(y = party, fill = opinion, x = n)) +
  geom_col(position = "fill") +
  xlab("proportion")
```

![A horizontal filled bar chart showing proportions of opinions for 3
parties. The fill colour represents 3 types of opinion. Every stacked
bar spans the width of the
panel.](faq-bars_files/figure-html/unnamed-chunk-19-1.png)

### How can I make a grouped bar plot?

Map the variable you want to group by to the `x` or `y` `aes`thetic, map
the variable you want to color the vars by to the `fill` aesthetic, and
set `position = "dodge"` in
[`geom_bar()`](https://ggplot2.tidyverse.org/reference/geom_bar.md).

See example

Suppose you have data from a survey with three questions, where
respondents select “Agree” or “Disagree” for each question.

``` r
survey <- tibble::tribble(
  ~respondent, ~q1,        ~q2,        ~q3,
  1,           "Agree",    "Agree",    "Disagree",
  2,           "Disagree", "Agree",    "Disagree",
  3,           "Agree",    "Agree",    "Disagree",
  4,           "Disagree", "Disagree", "Agree"
)
```

You’ll first want to reshape these data so that each row represents a
respondent / question pair. You can do this with
[`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html).
Then, pass the resulting longer data frame to
[`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.md) group
responses for each question together.

``` r
survey |>
  tidyr::pivot_longer(
    cols = -respondent,
    names_to = "question",
    values_to = "response"
  ) |>
  ggplot(aes(x = question, fill = response)) +
  geom_bar(position = "dodge")
```

![A grouped bar chart showing the number of responses to three
questions. Within each question, two bars denote an 'Agree' or
'Disagree' response.](faq-bars_files/figure-html/unnamed-chunk-21-1.png)

### How can I make a bar plot of group means?

Either calculate the group means first and use
[`geom_col()`](https://ggplot2.tidyverse.org/reference/geom_bar.md) to
draw the bars or let ggplot2 calculate the means with
[`stat_summary()`](https://ggplot2.tidyverse.org/reference/stat_summary.md)
with `fun = "mean"` and `geom = "bar"`.

See example

One option for calculating group means is using
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
followed by
[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html).
Then, you can pass the resulting data frame to
[`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.md) and plot
bars using
[`geom_col()`](https://ggplot2.tidyverse.org/reference/geom_bar.md).

``` r
mpg |>
  group_by(drv) |>
  summarise(mean_hwy = mean(hwy)) |>
  ggplot(aes(x = drv, y = mean_hwy)) +
  geom_col()
```

![A bar chart showing the average highway miles per gallon for three
types of drive
train.](faq-bars_files/figure-html/unnamed-chunk-22-1.png)

Alternatively, you can use
[`stat_summary()`](https://ggplot2.tidyverse.org/reference/stat_summary.md)
to let ggplot2 calculate and plot the means.

``` r
ggplot(mpg, aes(x = drv, y = hwy)) +
  stat_summary(fun = "mean", geom = "bar")
```

![A bar chart showing the average highway miles per gallon for three
types of drive
train.](faq-bars_files/figure-html/unnamed-chunk-23-1.png)

## Axes and axis limits

### Why do the bars on my plot disappear when I specify an axis range with `ylim()`? How can I get the bars to show up within a given axis range?

[`ylim()`](https://ggplot2.tidyverse.org/reference/lims.md) is a
shortcut for supplying the `limits` argument to individual scales. When
either of these is set, any values outside the limits specified are
replaced with `NA`. Since the bars naturally start at `y = 0`, replacing
part of the bars with `NA`s results in the bars entirely disappearing
from the plot. For changing axis limits without dropping data
observations, set limits in
[`coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.md)
instead. Also note that this will result in a deceiving bar plot, which
should be avoided in general.

See example

In the following plot the y-axis is limited to 20 to 120, and hence the
bars are not showing up.

``` r
ggplot(mpg, aes(x = drv)) +
  geom_bar() +
  ylim(c(20, 120))
#> Warning: Removed 3 rows containing missing values or values outside the scale range
#> (`geom_bar()`).
```

![A plot with axes and a panel, but no other
geometry.](faq-bars_files/figure-html/unnamed-chunk-24-1.png)

In order to obtain a bar plot with limited y-axis, you need to instead
set the limits in
[`coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.md).

``` r
ggplot(mpg, aes(x = drv)) +
  geom_bar() +
  coord_cartesian(ylim = c(20,110))
```

![A bar chart showing the number of cars for each of three types of
drive train. The y-axis starts at 20, and all bars touch the
x-axis.](faq-bars_files/figure-html/unnamed-chunk-25-1.png)

This is, indeed, a deceiving plot. If you’re using a bar plot to display
values that could not take the value of 0, you might choose a different
geom instead. For example, if you have the following data and plot.

``` r
df <- tibble::tribble(
  ~x,  ~y,
  "A", 1050,
  "B", 1100,
  "C", 1150
)

ggplot(df, aes(x = x, y = y)) +
  geom_col()
```

![A bar chart showing numbers for 3 arbitrary categories. The numbers
are far away from the x-axis and visually appear broadly similar in
height.](faq-bars_files/figure-html/unnamed-chunk-26-1.png)

Also suppose that you want to cut off the bars at `y = 1000` since you
know that the variable you’re plotting cannot take a value less than
1000, you might use
[`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.md)
instead.

``` r
# don't do this
ggplot(df, aes(x = x, y = y)) +
  geom_col() +
  coord_cartesian(ylim = c(1000, 1150))
```

![A bar chart showing numbers for 3 arbitrary categories. The y-axis
starts at 1000 and the bars all look different in height. This is not a
recommended way of plotting this
data.](faq-bars_files/figure-html/unnamed-chunk-27-1.png)

``` r

# do this
ggplot(df, aes(x = x, y = y)) +
  geom_point(size = 3)
```

![A scatter plot with 3 points showing numbers for 3 arbitrary
categories. The y-axis starts at 1000 and the points have visually
different values. This is a better way of plotting this
data.](faq-bars_files/figure-html/unnamed-chunk-27-2.png)
