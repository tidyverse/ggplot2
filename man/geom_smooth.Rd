\name{geom_smooth}
\alias{geom_smooth}
\title{Add a smoothed conditional mean.}
\usage{
  geom_smooth(mapping = NULL, data = NULL, stat = "smooth",
    position = "identity", ...)
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

  \item{...}{other arguments passed on to
  \code{\link{layer}}. This can include aesthetics whose
  values you want to set, not map. See \code{\link{layer}}
  for more details.}
}
\description{
  Add a smoothed conditional mean.
}
\section{Aesthetics}{
  \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom",
  "smooth")}
}
\examples{
# See stat_smooth for examples of using built in model fitting
# if you need some more flexible, this example shows you how to
# plot the fits from any model of your choosing
qplot(wt, mpg, data=mtcars, colour=factor(cyl))

model <- lm(mpg ~ wt + factor(cyl), data=mtcars)
grid <- with(mtcars, expand.grid(
  wt = seq(min(wt), max(wt), length = 20),
  cyl = levels(factor(cyl))
))

grid$mpg <- stats::predict(model, newdata=grid)

qplot(wt, mpg, data=mtcars, colour=factor(cyl)) + geom_line(data=grid)

# or with standard errors

err <- stats::predict(model, newdata=grid, se = TRUE)
grid$ucl <- err$fit + 1.96 * err$se.fit
grid$lcl <- err$fit - 1.96 * err$se.fit

qplot(wt, mpg, data=mtcars, colour=factor(cyl)) +
  geom_smooth(aes(ymin = lcl, ymax = ucl), data=grid, stat="identity")
}
\seealso{
  The default stat for this geom is
  \code{\link{stat_smooth}} see that documentation for more
  options to control the underlying statistical
  transformation.
}

