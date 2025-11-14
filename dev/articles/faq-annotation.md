# FAQ: Annotation

### Why is annotation created with `geom_text()` pixellated? How can I make it more crisp?

You should use `annotate(geom = "text")` instead of
[`geom_text()`](https://ggplot2.tidyverse.org/dev/reference/geom_text.md)
for annotation.

See example

In the following visualisation we have annotated a histogram with a red
line and red text to mark the mean. Note that both the line and the text
appears pixellated/fuzzy.

``` r
mean_hwy <- round(mean(mpg$hwy), 2)

ggplot(mpg, aes(x = hwy)) +
  geom_histogram(binwidth = 2) +
  geom_segment(
    x = mean_hwy, xend = mean_hwy,
    y = 0, yend = 35,
    color = "red"
  ) +
  geom_text(
    x = mean_hwy, y = 40,
    label = paste("mean\n", mean_hwy),
    color = "red"
  )
```

![Histogram of highway miles per gallon for 234 cars. A red line is
placed at the position 23.44 and is adorned with the label 'mean 23.44'.
Both the line and the text appear pixellated due to
overplotting.](faq-annotation_files/figure-html/unnamed-chunk-2-1.png)

This is because
[`geom_text()`](https://ggplot2.tidyverse.org/dev/reference/geom_text.md)
draws the geom once per each row of the data frame, and plotting these
on top of each other. For annotation (as opposed to plotting the data
using text as geometric objects to represent each observation) use
[`annotate()`](https://ggplot2.tidyverse.org/dev/reference/annotate.md)
instead.

``` r
ggplot(mpg, aes(x = hwy)) +
  geom_histogram(binwidth = 2) +
  annotate("segment",
    x = mean_hwy, xend = mean_hwy, y = 0, yend = 35,
    color = "red"
  ) +
  annotate("text",
    x = mean_hwy, y = 40,
    label = paste("mean =", mean_hwy),
    color = "red"
  )
```

![Histogram of highway miles per gallon for 234 cars. A red line is
placed at the position 23.44 and is adorned with the label 'mean =
23.44'. Both the line and the text appear
crisp.](faq-annotation_files/figure-html/unnamed-chunk-3-1.png)

### How can I make sure all annotation created with `geom_text()` fits in the bounds of the plot?

Set `vjust = "inward"` and `hjust = "inward"` in
[`geom_text()`](https://ggplot2.tidyverse.org/dev/reference/geom_text.md).

See example

Suppose you have the following data frame and visualization. The labels
at the edges of the plot are cut off slightly.

``` r
df <- tibble::tribble(
  ~x, ~y, ~name,
  2,  2,  "two",
  3,  3,  "three",
  4,  4,  "four"
)

ggplot(df, aes(x = x, y = y, label = name)) +
  geom_text(size = 10)
```

![A plot showing the words 'two', 'three' and 'four' arranged
diagonally. The 'two' and 'four' labels have been clipped to the panel's
edge and are not displayed
completely.](faq-annotation_files/figure-html/unnamed-chunk-4-1.png)

You could manually extend axis limits to avoid this, but a more
straightforward approach is to set `vjust = "inward"` and
`hjust = "inward"` in
[`geom_text()`](https://ggplot2.tidyverse.org/dev/reference/geom_text.md).

``` r
ggplot(df, aes(x = x, y = y, label = name)) +
  geom_text(size = 10, vjust = "inward", hjust = "inward")
```

![A plot showing the words 'two', 'three' and 'four' arranged
diagonally. The 'two' and 'four' labels are aligned to the top-right and
bottom-left relative to their anchor points, and are displayed in their
entirety.](faq-annotation_files/figure-html/unnamed-chunk-5-1.png)

### How can I annotate my bar plot to display counts for each bar?

Either calculate the counts ahead of time and place them on bars using
[`geom_text()`](https://ggplot2.tidyverse.org/dev/reference/geom_text.md)
or let
[`ggplot()`](https://ggplot2.tidyverse.org/dev/reference/ggplot.md)
calculate them for you and then add them to the plot using
[`stat_count()`](https://ggplot2.tidyverse.org/dev/reference/geom_bar.md)
with `geom = "text"`.

See example

Suppose you have the following bar plot and you want to add the number
of cars that fall into each `drv` level on their respective bars.

``` r
ggplot(mpg, aes(x = drv)) +
  geom_bar()
```

![A bar chart showing the number of cars for each of three types of
drive train.](faq-annotation_files/figure-html/unnamed-chunk-6-1.png)

One option is to calculate the counts with
[`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html) and
then pass them to the `label` mapping in
[`geom_text()`](https://ggplot2.tidyverse.org/dev/reference/geom_text.md).
Note that we expanded the y axis limit to get the numbers to fit on the
plot.

``` r
mpg |>
  dplyr::count(drv) |>
  ggplot(aes(x = drv, y = n)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.5) +
  coord_cartesian(ylim = c(0, 110))
```

![A bar chart showing the number of cars for each of three types of
drive train. The count values are displayed on top of the bars as
text.](faq-annotation_files/figure-html/unnamed-chunk-7-1.png)

Another option is to let
[`ggplot()`](https://ggplot2.tidyverse.org/dev/reference/ggplot.md) do
the counting for you, and access these counts with `after_stat(count)`
that is mapped to the labels to be placed on the plot with
[`stat_count()`](https://ggplot2.tidyverse.org/dev/reference/geom_bar.md).

``` r
ggplot(mpg, aes(x = drv)) +
  geom_bar() + 
  stat_count(geom = "text", aes(label = ..count..), vjust = -0.5) +
  coord_cartesian(ylim = c(0, 110))
#> Warning: The dot-dot notation (`..count..`) was deprecated in ggplot2 3.4.0.
#> ℹ Please use `after_stat(count)` instead.
```

![A bar chart showing the number of cars for each of three types of
drive train. The count values are displayed on top of the bars as
text.](faq-annotation_files/figure-html/unnamed-chunk-8-1.png)

### How can I annotate my stacked bar plot to display counts for each segment?

First calculate the counts for each segment (e.g. with
[`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html))
and then place them on the bars with
[`geom_text()`](https://ggplot2.tidyverse.org/dev/reference/geom_text.md)
using `position_stack(vjust = 0.5)` in the `position` argument to place
the values in the middle of the segments.

See example

Suppose you have the following stacked bar plot.

``` r
ggplot(mpg, aes(x = class, fill = drv)) +
  geom_bar()
```

![A stacked bar chart showing the number of cars for each of seven types
of cars. The fill colour of the bars indicate the type of drive
train.](faq-annotation_files/figure-html/unnamed-chunk-9-1.png)

You can first calculate the counts for each segment with
[`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html),
which will place these values in a column called `n`.

``` r
mpg |>
  count(class, drv) 
#> # A tibble: 12 × 3
#>    class      drv       n
#>    <chr>      <chr> <int>
#>  1 2seater    r         5
#>  2 compact    4        12
#>  3 compact    f        35
#>  4 midsize    4         3
#>  5 midsize    f        38
#>  6 minivan    f        11
#>  7 pickup     4        33
#>  8 subcompact 4         4
#>  9 subcompact f        22
#> 10 subcompact r         9
#> 11 suv        4        51
#> 12 suv        r        11
```

You can then pass this result directly to
[`ggplot()`](https://ggplot2.tidyverse.org/dev/reference/ggplot.md),
draw the segments with appropriate heights with `y = n` in the
`aes`thetic mapping and
[`geom_col()`](https://ggplot2.tidyverse.org/dev/reference/geom_bar.md)
to draw the bars, and finally place the counts on the plot with
[`geom_text()`](https://ggplot2.tidyverse.org/dev/reference/geom_text.md).

``` r
mpg |>
  count(class, drv) |>
  ggplot(aes(x = class, fill = drv, y = n)) +
  geom_col() +
  geom_text(aes(label = n), size = 3, position = position_stack(vjust = 0.5))
```

![A stacked bar chart showing the number of cars for each of seven types
of cars. The fill colour of the bars indicate the type of drive train.
In the middle of each filled part, the count value is displayed as
text.](faq-annotation_files/figure-html/unnamed-chunk-11-1.png)

### How can I display proportions (relative frequencies) instead of counts on a bar plot?

Either calculate the proportions ahead of time and place them on bars
using
[`geom_text()`](https://ggplot2.tidyverse.org/dev/reference/geom_text.md)
or let
[`ggplot()`](https://ggplot2.tidyverse.org/dev/reference/ggplot.md)
calculate them for you and then add them to the plot using
[`stat_count()`](https://ggplot2.tidyverse.org/dev/reference/geom_bar.md)
with `geom = "text"`.

See example

Suppose you have the following bar plot but you want to display the
proportion of cars that fall into each `drv` level, instead of the
count.

``` r
ggplot(mpg, aes(x = drv)) +
  geom_bar()
```

![A bar chart showing the number of cars for each of three types of
drive train.](faq-annotation_files/figure-html/unnamed-chunk-12-1.png)

One option is to calculate the proportions with
[`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html) and
then use
[`geom_col()`](https://ggplot2.tidyverse.org/dev/reference/geom_bar.md)
to draw the bars

``` r
mpg |>
  dplyr::count(drv) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(x = drv, y = prop)) +
  geom_col()
```

![A bar chart showing the proportion of cars for each of three types of
drive train.](faq-annotation_files/figure-html/unnamed-chunk-13-1.png)

Another option is to let
[`ggplot()`](https://ggplot2.tidyverse.org/dev/reference/ggplot.md) do
the calculation of proportions for you, and access these counts with
`..prop..`. Note that we also need to the `group = 1` mapping for this
option.

``` r
ggplot(mpg, aes(x = drv, y = ..prop.., group = 1)) +
  geom_bar()
```

![A bar chart showing the proportion of cars for each of three types of
drive train.](faq-annotation_files/figure-html/unnamed-chunk-14-1.png)
