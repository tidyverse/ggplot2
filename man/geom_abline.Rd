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
  The abline geom adds a line with specified slope and
  intercept to the plot.
}
\details{
  With its siblings \code{geom_hline} and
  \code{geom_vline}, it's useful for annotating plots.  You
  can supply the parameters for geom_abline, intercept and
  slope, in two ways: either explicitly as fixed values, or
  in a data frame.  If you specify the fixed values
  (\code{geom_abline(intercept=0, slope=1)}) then the line
  will be the same in all panels.  If the intercept and
  slope are stored in the data, then they can vary from
  panel to panel.  See the examples for more ideas.
}
\section{Aesthetics}{
  \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom",
  "abline")}
}
\examples{
p <- qplot(wt, mpg, data = mtcars)

# Fixed slopes and intercepts
p + geom_abline() # Can't see it - outside the range of the data
p + geom_abline(intercept = 20)

# Calculate slope and intercept of line of best fit
coef(lm(mpg ~ wt, data = mtcars))
p + geom_abline(intercept = 37, slope = -5)
p + geom_abline(intercept = 10, colour = "red", size = 2)

# See ?stat_smooth for fitting smooth models to data
p + stat_smooth(method="lm", se=FALSE)

# Slopes and intercepts as data
p <- ggplot(mtcars, aes(x = wt, y=mpg), . ~ cyl) + geom_point()
df <- data.frame(a=rnorm(10, 25), b=rnorm(10, 0))
p + geom_abline(aes(intercept=a, slope=b), data=df)

# Slopes and intercepts from linear model
library(plyr)
coefs <- ddply(mtcars, .(cyl), function(df) {
  m <- lm(mpg ~ wt, data=df)
  data.frame(a = coef(m)[1], b = coef(m)[2])
})
str(coefs)
p + geom_abline(data=coefs, aes(intercept=a, slope=b))

# It's actually a bit easier to do this with stat_smooth
p + geom_smooth(aes(group=cyl), method="lm")
p + geom_smooth(aes(group=cyl), method="lm", fullrange=TRUE)

# With coordinate transforms
p + geom_abline(intercept = 37, slope = -5) + coord_flip()
p + geom_abline(intercept = 37, slope = -5) + coord_polar()
}
\seealso{
  \code{\link{stat_smooth}} to add lines derived from the
  data, \code{\link{geom_hline}} for horizontal lines,
  \code{\link{geom_vline}} for vertical lines
  \code{\link{geom_segment}}
}

