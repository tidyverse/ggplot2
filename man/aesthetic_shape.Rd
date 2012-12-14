\name{aesthetic_shape}
\alias{aesthetic-shape}
\alias{aesthetic_shape}
\alias{shape}
\title{Aesthetic: shape}
\description{
  This page demonstrates the usage of the shape aesthetic.
}
\examples{
# Shape examples
# Shape takes four types of values: an integer in [0, 25],
# a single character-- which uses that character as the plotting symbol,
# a . to draw the smallest rectangle that is visible (i.e., about one pixel)
# an NA to draw nothing
p <- ggplot(mtcars, aes(wt, mpg))
p + geom_point()
p + geom_point(shape = 5)
p + geom_point(shape = "k", size = 3)
p + geom_point(shape = ".")
p + geom_point(shape = NA)

# Shape can also be mapped from a variable
p + geom_point(aes(shape = factor(cyl)))

# A look at all 25 symbols
df2 <- data.frame(x = 1:5 , y = 1:25, z = 1:25)
s <- ggplot(df2, aes(x = x, y = y))
s + geom_point(aes(shape = z), size = 4) + scale_shape_identity()
# While all symbols have a foreground colour, symbols 19-25 also take a
# background colour (fill)
s + geom_point(aes(shape = z), size = 4, colour = "Red") +
  scale_shape_identity()
s + geom_point(aes(shape = z), size = 4, colour = "Red", fill = "Black") +
  scale_shape_identity()
}
\seealso{
  Other aesthetics: \code{\link{aesthetic_alpha}},
  \code{\link{aesthetic_colour_fill}},
  \code{\link{aesthetic_group_order}},
  \code{\link{aesthetic_linetype}},
  \code{\link{aesthetic_size}}
}

