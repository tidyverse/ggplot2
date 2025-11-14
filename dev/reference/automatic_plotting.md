# Tailoring plots to particular data types

There are three functions to make plotting particular data types easier:
[`autoplot()`](https://ggplot2.tidyverse.org/dev/reference/autoplot.md),
[`autolayer()`](https://ggplot2.tidyverse.org/dev/reference/autolayer.md)
and
[`fortify()`](https://ggplot2.tidyverse.org/dev/reference/fortify.md).
These are S3 generics for which other packages can write methods to
display classes of data. The three functions are complementary and allow
different levels of customisation. Below we'll explore implementing this
series of methods to automate plotting of some class.

Let's suppose we are writing a packages that has a class called
'my_heatmap', that wraps a matrix and we'd like users to easily plot
this heatmap.

    my_heatmap <- function(...) {
      m <- matrix(...)
      class(m) <- c("my_heatmap", class(m))
      m
    }

    my_data <- my_heatmap(volcano)

## Automatic data shaping

One of the things we have to do is ensure that the data is shaped in the
long format so that it is compatible with ggplot2. This is the job of
the
[`fortify()`](https://ggplot2.tidyverse.org/dev/reference/fortify.md)
function. Because 'my_heatmap' wraps a matrix, we can let the fortify
method 'melt' the matrix to a long format. If your data is already based
on a long-format `<data.frame>`, you can skip implementing a
[`fortify()`](https://ggplot2.tidyverse.org/dev/reference/fortify.md)
method.

    fortify.my_heatmap <- function(model, ...) {
      data.frame(
        row = as.vector(row(model)),
        col = as.vector(col(model)),
        value = as.vector(model)
      )
    }

    fortify(my_data)

When you have implemented the
[`fortify()`](https://ggplot2.tidyverse.org/dev/reference/fortify.md)
method, it should be easier to construct a plot with the data than with
the matrix.

    ggplot(my_data, aes(x = col, y = row, fill = value)) +
      geom_raster()

## Automatic layers

A next step in automating plotting of your data type is to write an
[`autolayer()`](https://ggplot2.tidyverse.org/dev/reference/autolayer.md)
method. These are typically wrappers around geoms or stats that
automatically set aesthetics or other parameters. If you haven't
implemented a
[`fortify()`](https://ggplot2.tidyverse.org/dev/reference/fortify.md)
method for your data type, you might have to reshape the data in
[`autolayer()`](https://ggplot2.tidyverse.org/dev/reference/autolayer.md).

If you require multiple layers to display your data type, you can use an
[`autolayer()`](https://ggplot2.tidyverse.org/dev/reference/autolayer.md)
method that constructs a list of layers, which can be added to a plot.

    autolayer.my_heatmap <- function(object, ...) {
      geom_raster(
        mapping = aes(x = col, y = row, fill = value),
        data = object,
        ...,
        inherit.aes = FALSE
      )
    }

    ggplot() + autolayer(my_data)

As a quick tip: if you define a mapping in
[`autolayer()`](https://ggplot2.tidyverse.org/dev/reference/autolayer.md),
you might want to set `inherit.aes = FALSE` to not have aesthetics set
in other layers interfere with your layer.

## Automatic plots

The last step in automating plotting is to write an
[`autoplot()`](https://ggplot2.tidyverse.org/dev/reference/autoplot.md)
method for your data type. The expectation is that these return a
complete plot. In the example below, we're exploiting the
[`autolayer()`](https://ggplot2.tidyverse.org/dev/reference/autolayer.md)
method that we have already written to make a complete plot.

    autoplot.my_heatmap <- function(object, ..., option = "magma") {
      ggplot() +
        autolayer(my_data) +
        scale_fill_viridis_c(option = option) +
        theme_void()
    }

    autoplot(my_data)

If you don't have a wish to implement a base R plotting method, you can
set the plot method for your class to the autoplot method.

    plot.my_heatmap <- autoplot.my_heatmap
    plot(my_data)

## See also

Other plotting automation topics:
[`autolayer()`](https://ggplot2.tidyverse.org/dev/reference/autolayer.md),
[`autoplot()`](https://ggplot2.tidyverse.org/dev/reference/autoplot.md),
[`fortify()`](https://ggplot2.tidyverse.org/dev/reference/fortify.md)
