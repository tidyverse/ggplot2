# FAQ: Reordering

## Bar plots

### How can I reorder the bars in a bar plot by their value?

Change the order of the levels of the factor variable you’re creating
the bar plot for in the `aes`thetic `mapping`. The forcats package
offers a variety of options for doing this, such as
[`forcats::fct_infreq()`](https://forcats.tidyverse.org/reference/fct_inorder.html)
for ordering by the number of observations within each level.

See example

The following bar plot shows the number of cars that fall into each
`class` category. Classes are ordered alphabetically. You might prefer
them to be ordered by the number of cars in each class.

``` r
ggplot(mpg, aes(y = class)) +
  geom_bar()
```

![A horizontal bar plot showing counts on the x-axis and seven types of
cars on the y-axis. From bottom to top, the car types are in
alphabetical
order.](faq-reordering_files/figure-html/unnamed-chunk-2-1.png)

To do this, you can use
[`forcats::fct_infreq()`](https://forcats.tidyverse.org/reference/fct_inorder.html).

``` r
ggplot(mpg, aes(y = forcats::fct_infreq(class))) +
  geom_bar()
```

![A horizontal bar plot showing counts on the x-axis and seven types of
cars on the y-axis. From top to bottom, the car types are ordered by
increasing number of
cars.](faq-reordering_files/figure-html/unnamed-chunk-3-1.png)

If you’d like to plot the highest value first, you can also reverse the
order with
[`forcats::fct_rev()`](https://forcats.tidyverse.org/reference/fct_rev.html).
You might also want to simplify the axis label.

``` r
ggplot(mpg, aes(y = forcats::fct_rev(forcats::fct_infreq(class)))) +
  geom_bar() +
  labs(y = "class")
```

![A horizontal bar plot showing counts on the x-axis and seven types of
cars on the y-axis. From top to bottom, the car types are ordered in
decreasing number of
cars.](faq-reordering_files/figure-html/unnamed-chunk-4-1.png)

### How can I reorder the stacks in a stacked bar plot?

Change the order of the levels of the factor variable you’re creating
the stacks with in the `aes`thetic `mapping`. The forcats package offers
a variety of options for doing this, such as
[`forcats::fct_reorder()`](https://forcats.tidyverse.org/reference/fct_reorder.html)
to reorder the levels or
[`forcats::fct_rev()`](https://forcats.tidyverse.org/reference/fct_rev.html)
to reverse their order.

See example

Suppose you have the following stacked bar plot of `clarity` of
`diamonds` by their `cut`.

``` r
ggplot(diamonds, aes(x = cut, fill = clarity)) +
  geom_bar()
```

![A stacked bar plot showing counts on the y-axis and five cut qualities
of diamonds on the x-axis. Within every stacked bar, the fill colour
indicates an ordinal clarity of the diamond. The worst clarity has the
darkest colour and the best quality has the lightest colour. The best
clarity is the bar on the
bottom.](faq-reordering_files/figure-html/unnamed-chunk-5-1.png)

You can reverse the order `clarity` levels are displayed in the bars
with
[`forcats::fct_rev()`](https://forcats.tidyverse.org/reference/fct_rev.html).
This will also change the order they’re presented in the legend so the
two orders match.

``` r
ggplot(diamonds, aes(x = cut, fill = forcats::fct_rev(clarity))) +
  geom_bar() +
  labs(fill = "clarity")
```

![A stacked bar plot showing counts on the y-axis and five cut qualities
of diamonds on the x-axis. Within every stacked bar, the fill colour
indicates an ordinal clarity of the diamond. The worst clarity has the
lightest colour and the best quality has the darkest colour. The worst
clarity is the bar on the
bottom.](faq-reordering_files/figure-html/unnamed-chunk-6-1.png)

## Box plots

### How can I control the order of boxes in a side-by-side box plot?

Change the order of the levels of the factor variable you’re faceting
by. The forcats package offers a variety of options for doing this, such
as
[`forcats::fct_relevel()`](https://forcats.tidyverse.org/reference/fct_relevel.html)
for manual reordering or
[`forcats::fct_reorder()`](https://forcats.tidyverse.org/reference/fct_reorder.html)
for ordering by a particular value, e.g. group median.

See example

The order of the boxes is determined by the order of the levels of the
variable you’re grouping by. If the faceting variable is character, this
order is alphabetical by default.

``` r
ggplot(mpg, aes(x = class, y = hwy)) +
  geom_boxplot()
```

![A boxplot showing the highway miles per gallon on the y-axis for seven
types of car on the x-axis. The car types on the x-axis are in
alphabetical
order.](faq-reordering_files/figure-html/unnamed-chunk-7-1.png)

Suppose you’d like the boxes to be ordered in ascending order of their
medians. You can do this in a data transformation step prior to plotting
(e.g. with
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html))
or you can do it directly in the plotting code as shown below. You might
then want to customize the x-axis label as well.

``` r
ggplot(mpg, aes(x = forcats::fct_reorder(class, hwy, .fun = median), y = hwy)) +
  geom_boxplot() +
  labs(x = "class")
```

![A boxplot showing the highway miles per gallon on the y-axis for seven
types of car on the x-axis. The car types on the x-axis sorted from left
to right by increasing
medians.](faq-reordering_files/figure-html/unnamed-chunk-8-1.png)

## Facets

### How can I control the order of panes created with `facet_wrap()` or `facet_grid()`?

Change the order of the levels of the factor variable you’re faceting
by. The forcats package offers a variety of options for doing this, such
as
[`forcats::fct_relevel()`](https://forcats.tidyverse.org/reference/fct_relevel.html).

See example

The order of the panes is determined by the order of the levels of the
variable you’re faceting by. If the faceting variable is character, this
order is alphabetical by default.

``` r
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_wrap(~drv)
```

![A scatter plot showing the engine displacement on the x-axis and
highway miles per gallon on the y-axis of 234 cars. The plot has three
panels in a 1-row, 3-column layout for three types of drive train. The
drive trains are ordered alphabetically in the horizontal
direction.](faq-reordering_files/figure-html/unnamed-chunk-9-1.png)

Suppose you’d like the panes to be in the order `"r"`, `"f"` , `"4"`.
You can use
[`forcats::fct_relevel()`](https://forcats.tidyverse.org/reference/fct_relevel.html)
to reorder the levels of `drv`. You can do this in a data transformation
step prior to plotting (e.g. with
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html))
or you can do it directly in the plotting code as shown below.

``` r
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_wrap(~forcats::fct_relevel(drv, "r", "f", "4"))
```

![A scatter plot showing the engine displacement on the x-axis and
highway miles per gallon on the y-axis of 234 cars. The plot has three
panels in a 1-row, 3-column layout for three types of drive train. The
drive trains are in the order 'r', 'f' and '4' from left to
right.](faq-reordering_files/figure-html/unnamed-chunk-10-1.png)

## Overplotting

### How can I control the order of the points plotted?

If there is a specific point (or group of points) you want to make sure
is plotted on top of others, subset the data for those observations and
add as a new layer to your plot.

See example

Suppose you have the following data frame.

``` r
df <- tibble::tribble(
  ~id, ~x,   ~y, ~shape,            ~fill,
  1,   0.01, 0,  "circle filled",   "blue",
  2,   1,    0,  "square filled",   "red",
  3,   0.99, 0,  "asterisk",        "black",
  4,   0,    0,  "triangle filled", "yellow"
)
```

By default, this is how a scatter plot of these looks. Note that the
blue circle is partially covered by the yellow triangle since that
observation comes later in the dataset. Similarly the black asterisk
appears on top of the red square.

``` r
ggplot(df, aes(x = x, y = y, fill = fill, shape = shape)) +
  geom_point(size = 8) +
  scale_shape_identity() +
  scale_fill_identity()
```

![A scatter plot showing four points at the same y-positions but at four
x-positions, of which two are very distinct. Every point has a distinct
shape and colour. A yellow triangle is plotted on top of a blue circle.
A black asterisk is plotted on top of a red
square.](faq-reordering_files/figure-html/unnamed-chunk-12-1.png)

Suppose you arranged your data in ascending order of the x-coordinates
and plotted again. Now the blue circle is over the yellow triangle since
0.01 comes after 0 and similarly the red square is over the black
asterisk since 1 comes after 0.99.

``` r
df_arranged <- df |> dplyr::arrange(x)

df_arranged |>
  ggplot(aes(x = x, y = y, fill = fill, shape = shape)) +
  geom_point(size = 8) +
  scale_shape_identity() +
  scale_fill_identity()
```

![A scatter plot showing four points at the same y-positions but at four
x-positions, of which two are very distinct. Every point has a distinct
shape and colour. A blue circle is plotted on top of a yellow triangle.
A red square is plotted on top of a black
asterisk.](faq-reordering_files/figure-html/unnamed-chunk-13-1.png)

If you wanted to make sure that the observation identified with an
asterisk is always plotted on top, regardless of how the data are
arranged in the data frame, you can create an additional layer for that
observation.

``` r
ggplot(mapping = aes(x = x, y = y, fill = fill, shape = shape)) +
  geom_point(data = df |> filter(shape != "asterisk"), size = 8) +
  geom_point(data = df |> filter(shape == "asterisk"), size = 8) +
  scale_shape_identity() +
  scale_fill_identity()
```

![A scatter plot showing four points at the same y-positions but at four
x-positions, of which two are very distinct. Every point has a distinct
shape and colour. A yellow triangle is plotted on top of a blue circle.
A black asterisk is plotted on top of a red
square.](faq-reordering_files/figure-html/unnamed-chunk-14-1.png)

``` r

ggplot(mapping = aes(x = x, y = y, fill = fill, shape = shape)) +
  geom_point(data = df_arranged |> filter(shape != "asterisk"), size = 8) +
  geom_point(data = df_arranged |> filter(shape == "asterisk"), size = 8) +
  scale_shape_identity() +
  scale_fill_identity()
```

![A scatter plot showing four points at the same y-positions but at four
x-positions, of which two are very distinct. Every point has a distinct
shape and colour. A blue circle is plotted on top of a yellow triangle.
A black asterisk is plotted on top of a red
square.](faq-reordering_files/figure-html/unnamed-chunk-14-2.png)
