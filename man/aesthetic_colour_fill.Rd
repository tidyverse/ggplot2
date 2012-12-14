\name{aesthetic_colour_fill}
\alias{aesthetic-colour}
\alias{aesthetic-fill}
\alias{aesthetic_colour_fill}
\alias{color}
\alias{colour}
\alias{fill}
\title{Aesthetics related to colour: colour, fill}
\description{
  This page demonstrates the usage of a sub-group of
  aesthetics; colour and fill.
}
\examples{
\donttest{

# Bar chart example
c <- ggplot(mtcars, aes(factor(cyl)))
# Default plotting
c + geom_bar()
# To change the interior colouring use fill aesthetic
c + geom_bar(fill = "red")
# Compare with the colour aesthetic which changes just the bar outline
c + geom_bar(colour = "red")
# Combining both, you can see the changes more clearly
c + geom_bar(fill = "white", colour = "red")

# The aesthetic fill also takes different colouring scales
# setting fill equal to a factor varible uses a discrete colour scale
k <- ggplot(mtcars, aes(factor(cyl), fill = factor(vs)))
k + geom_bar()

# Fill aesthetic can also be used with a continuous variable
m <- ggplot(movies, aes(x = rating))
m + geom_histogram()
m + geom_histogram(aes(fill = ..count..))

# Some geoms don't use both aesthetics (i.e. geom_point or geom_line)
b <- ggplot(economics, aes(x = date, y = unemploy))
b + geom_line()
b + geom_line(colour = "green")
b + geom_point()
b + geom_point(colour = "red")
}
}
\seealso{
  Other aesthetics: \code{\link{aesthetic_alpha}},
  \code{\link{aesthetic_group_order}},
  \code{\link{aesthetic_linetype}},
  \code{\link{aesthetic_shape}},
  \code{\link{aesthetic_size}}
}

