\name{geom_vline}
\alias{geom_vline}
\title{Line, vertical.}
\usage{
  geom_vline(mapping = NULL, data = NULL, stat = "vline",
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
  This geom allows you to annotate the plot with vertical
  lines (see \code{\link{geom_hline}} and
  \code{\link{geom_abline}} for other types of lines.
}
\details{
  There are two ways to use it.  You can either specify the
  intercept of the line in the call to the geom, in which
  case the line will be in the same position in every
  panel.  Alternatively, you can supply a different
  intercept for each panel using a data.frame.  See the
  examples for the differences.
}
\section{Aesthetics}{
  \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom",
  "vline")}
}
\examples{
# Fixed lines
p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
p + geom_vline(xintercept = 5)
p + geom_vline(xintercept = 1:5)
p + geom_vline(xintercept = 1:5, colour="green", linetype = "longdash")
p + geom_vline(aes(xintercept = wt))

# With coordinate transforms
p + geom_vline(aes(xintercept = wt)) + coord_equal()
p + geom_vline(aes(xintercept = wt)) + coord_flip()
p + geom_vline(aes(xintercept = wt)) + coord_polar()

p2 <- p + aes(colour = factor(cyl))
p2 + geom_vline(xintercept = 15)

# To display different lines in different facets, you need to
# create a data frame.
p <- qplot(mpg, wt, data=mtcars, facets = vs ~ am)
vline.data <- data.frame(z = c(15, 20, 25, 30), vs = c(0, 0, 1, 1), am = c(0, 1, 0, 1))
p + geom_vline(aes(xintercept = z), vline.data)
}
\seealso{
  \code{\link{geom_hline}} for horizontal lines,
  \code{\link{geom_abline}} for lines defined by a slope
  and intercept, \code{\link{geom_segment}} for a more
  general approach"
}

