\name{geom_point}
\alias{geom_point}
\title{Points, as for a scatterplot}
\usage{
  geom_point(mapping = NULL, data = NULL,
    stat = "identity", position = "identity",
    na.rm = FALSE, ...)
}
\arguments{
  \item{mapping}{The aesthetic mapping, usually constructed
  with \code{\link{aes}} or \code{\link{aes_string}}. Only
  needs to be set at the layer level if you are overriding
  the plot defaults.}

  \item{data}{A layer specific dataset - only needed if you
  want to override the plot defaults.}

  \item{stat}{The statistical transformation to use on the
  data for this layer.}

  \item{position}{The position adjustment to use for
  overlappling points on this layer}

  \item{na.rm}{If \code{FALSE} (the default), removes
  missing values with a warning.  If \code{TRUE} silently
  removes missing values.}

  \item{...}{other arguments passed on to
  \code{\link{layer}}. This can include aesthetics whose
  values you want to set, not map. See \code{\link{layer}}
  for more details.}
}
\description{
  The point geom is used to create scatterplots.
}
\details{
  The scatterplot is useful for displaying the relationship
  between two continuous variables, although it can also be
  used with one continuous and one categorical variable, or
  two categorical variables.  See \code{\link{geom_jitter}}
  for possibilities.

  The \emph{bubblechart} is a scatterplot with a third
  variable mapped to the size of points.  There are no
  special names for scatterplots where another variable is
  mapped to point shape or colour, however.

  The biggest potential problem with a scatterplot is
  overplotting: whenever you have more than a few points,
  points may be plotted on top of one another. This can
  severely distort the visual appearance of the plot. There
  is no one solution to this problem, but there are some
  techniques that can help.  You can add additional
  information with \code{\link{stat_smooth}},
  \code{\link{stat_quantile}} or
  \code{\link{stat_density2d}}.  If you have few unique x
  values, \code{\link{geom_boxplot}} may also be useful.
  Alternatively, you can summarise the number of points at
  each location and display that in some way, using
  \code{\link{stat_sum}}. Another technique is to use
  transparent points, \code{geom_point(alpha = 0.05)}.
}
\section{Aesthetics}{
  \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom",
  "point")}
}
\examples{
\donttest{
p <- ggplot(mtcars, aes(wt, mpg))
p + geom_point()

# Add aesthetic mappings
p + geom_point(aes(colour = qsec))
p + geom_point(aes(alpha = qsec))
p + geom_point(aes(colour = factor(cyl)))
p + geom_point(aes(shape = factor(cyl)))
p + geom_point(aes(size = qsec))

# Change scales
p + geom_point(aes(colour = cyl)) + scale_colour_gradient(low = "blue")
p + geom_point(aes(size = qsec)) + scale_area()
p + geom_point(aes(shape = factor(cyl))) + scale_shape(solid = FALSE)

# Set aesthetics to fixed value
p + geom_point(colour = "red", size = 3)
qplot(wt, mpg, data = mtcars, colour = I("red"), size = I(3))

# Varying alpha is useful for large datasets
d <- ggplot(diamonds, aes(carat, price))
d + geom_point(alpha = 1/10)
d + geom_point(alpha = 1/20)
d + geom_point(alpha = 1/100)

# You can create interesting shapes by layering multiple points of
# different sizes
p <- ggplot(mtcars, aes(mpg, wt))
p + geom_point(colour="grey50", size = 4) + geom_point(aes(colour = cyl))
p + aes(shape = factor(cyl)) +
  geom_point(aes(colour = factor(cyl)), size = 4) +
  geom_point(colour="grey90", size = 1.5)
p + geom_point(colour="black", size = 4.5) +
  geom_point(colour="pink", size = 4) +
  geom_point(aes(shape = factor(cyl)))

# These extra layers don't usually appear in the legend, but we can
# force their inclusion
p + geom_point(colour="black", size = 4.5, show_guide = TRUE) +
  geom_point(colour="pink", size = 4, show_guide = TRUE) +
  geom_point(aes(shape = factor(cyl)))

# Transparent points:
qplot(mpg, wt, data = mtcars, size = I(5), alpha = I(0.2))

# geom_point warns when missing values have been dropped from the data set
# and not plotted, you can turn this off by setting na.rm = TRUE
mtcars2 <- transform(mtcars, mpg = ifelse(runif(32) < 0.2, NA, mpg))
qplot(wt, mpg, data = mtcars2)
qplot(wt, mpg, data = mtcars2, na.rm = TRUE)

# Use qplot instead
qplot(wt, mpg, data = mtcars)
qplot(wt, mpg, data = mtcars, colour = factor(cyl))
qplot(wt, mpg, data = mtcars, colour = I("red"))
}
}
\seealso{
  \code{\link{scale_size}} to see scale area of points,
  instead of radius, \code{\link{geom_jitter}} to jitter
  points to reduce (mild) overplotting
}

