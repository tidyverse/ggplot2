\name{geom_abline}
\alias{geom_abline}
\title{Line specified by slope and intercept.}
\usage{
  geom_abline(mapping = NULL, data = NULL, stat = "abline",
    position = "identity", show_guide = FALSE, ...)
}
\arguments{
  \item{show_guide}{should a legend be drawn? (defaults to
  \code{FALSE})}

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

  \item{...}{other arguments passed on to
  \code{\link{layer}}. This can include aesthetics whose
  values you want to set, not map. See \code{\link{layer}}
  for more details.}
}
\description{
  The abline geom adds lines with a slope and intercept
  (see \code{\link{geom_hline}} and
  \code{\link{geom_vline}} for other types of lines).
}
\details{
  To specify the slope and intercept of the line with fixed
  values, use \code{annotate_abline}. With this method, the
  lines with be the same in every panel.

  To map variables from the data to the slope and
  y-intercept, use \code{geom_abline}. With this method,
  the slope and intercept can vary from panel to panel. See
  the examples for more ideas.
}
\examples{
p <- qplot(wt, mpg, data = mtcars)

# Specify a fixed slope and intercept
p + annotate_abline(intercept = 20, slope = 1)
p + annotate_abline(intercept = 20, slope = 1, colour = "red", size = 2)

# Calculate slope and intercept of line of best fit
coef(lm(mpg ~ wt, data = mtcars))
p + annotate_abline(intercept = 37, slope = -5)

# See ?stat_smooth for fitting smooth models to data
p + stat_smooth(method = "lm", se=FALSE)

# Mapping data to slopes and intercepts
df <- data.frame(a = rnorm(10, 25), b = rnorm(10, 0))
p + geom_abline(data = df, aes(intercept = a, slope = b), inherit.aes = FALSE)

# Slopes and intercepts from linear model
library(plyr)
coefs <- ddply(mtcars, .(cyl), function(df) {
  m <- lm(mpg ~ wt, data=df)
  data.frame(a = coef(m)[1], b = coef(m)[2])
})
str(coefs)
p + geom_abline(data = coefs, aes(intercept = a, slope = b),
  inherit.aes = FALSE)

# It's actually a bit easier to do this with stat_smooth
p + geom_smooth(aes(group = cyl), method = "lm", se = FALSE)
p + geom_smooth(aes(group = cyl), method = "lm", se = FALSE, fullrange = TRUE)

# With coordinate transforms
p + annotate_abline(intercept = 37, slope = -5) + coord_flip()
p + annotate_abline(intercept = 37, slope = -5) + coord_polar()
}
\seealso{
  \code{\link{stat_smooth}} to add lines derived from the
  data, \code{\link{geom_hline}} for horizontal lines,
  \code{\link{geom_vline}} for vertical lines
  \code{\link{geom_segment}}
}

