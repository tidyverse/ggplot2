\name{geom_linerange}
\alias{geom_linerange}
\title{An interval represented by a vertical line.}
\usage{
  geom_linerange(mapping = NULL, data = NULL,
    stat = "identity", position = "identity", ...)
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
  An interval represented by a vertical line.
}
\section{Aesthetics}{
  \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom",
  "linerange")}
}
\examples{
# Generate data: means and standard errors of means for prices
# for each type of cut
dmod <- lm(price ~ cut, data=diamonds)
cuts <- data.frame(cut=unique(diamonds$cut), predict(dmod, data.frame(cut = unique(diamonds$cut)), se=TRUE)[c("fit","se.fit")])

qplot(cut, fit, data=cuts)
# With a bar chart, we are comparing lengths, so the y-axis is
# automatically extended to include 0
qplot(cut, fit, data=cuts, geom="bar")

# Display estimates and standard errors in various ways
se <- ggplot(cuts, aes(cut, fit,
  ymin = fit - se.fit, ymax=fit + se.fit, colour = cut))
se + geom_linerange()
se + geom_pointrange()
se + geom_errorbar(width = 0.5)
se + geom_crossbar(width = 0.5)

# Use coord_flip to flip the x and y axes
se + geom_linerange() + coord_flip()
}
\seealso{
  \code{\link{geom_errorbar}}: error bars;
  \code{\link{geom_pointrange}}: range indicated by
  straight line, with point in the middle;
  \code{\link{geom_crossbar}}: hollow bar with middle
  indicated by horizontal line; \code{\link{stat_summary}}:
  examples of these guys in use; \code{\link{geom_smooth}}:
  for continuous analog
}

