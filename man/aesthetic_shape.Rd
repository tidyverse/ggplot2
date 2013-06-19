\name{aesthetic_shape}
\alias{aesthetic-shape}
\alias{aesthetic_shape}
\alias{shape}
\title{Aesthetic: shape}
\description{
  This page demonstrates the usage of the shape aesthetic.
}
\section{Geoms}{
  \Sexpr[results=rd,stage=build]{ggplot2:::rd_geom_with_aesthetic("shape")}
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

# A look at all 26 symbols
df2 <- data.frame(x = 1, y = 1, z = 0:25)
s <- ggplot(df2, aes(x = x, y = y)) +
  scale_x_continuous(breaks=c(1)) +
  scale_y_continuous(breaks=c(1)) +
  scale_shape_identity() +
  facet_wrap(~z) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())
s + geom_point(aes(shape = z), size = 10)
# While all symbols have a foreground colour, symbols 21-25 also take a
# background colour (fill)
s + geom_point(aes(shape = z), size = 10, colour = "Red")
s + geom_point(aes(shape = z), size = 10, colour = "Red", fill = "lightblue")
}
\seealso{
  Other aesthetics: \code{\link{aesthetic_alpha}},
  \code{\link{aesthetic_colour_fill}},
  \code{\link{aesthetic_group_order}},
  \code{\link{aesthetic_linetype}},
  \code{\link{aesthetic_size}}
}

