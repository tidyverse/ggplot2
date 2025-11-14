# Extract alt text from a plot

This function returns a text that can be used as alt-text in webpages
etc. Currently it will use the `alt` label, added with
`+ labs(alt = <...>)`, or a return an empty string, but in the future it
might try to generate an alt text from the information stored in the
plot.

## Usage

``` r
get_alt_text(p, ...)
```

## Arguments

- p:

  a ggplot object

- ...:

  Arguments passed to methods.

## Value

A text string

## Examples

``` r
p <- ggplot(mpg, aes(displ, hwy)) +
  geom_point()

# Returns an empty string
get_alt_text(p)
#> [1] ""

# A user provided alt text
p <- p + labs(
  alt = paste("A scatterplot showing the negative correlation between engine",
              "displacement as a function of highway miles per gallon")
)

get_alt_text(p)
#> [1] "A scatterplot showing the negative correlation between engine displacement as a function of highway miles per gallon"
```
