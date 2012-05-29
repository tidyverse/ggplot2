\name{geom_hline}
\alias{geom_hline}
\title{Horizontal line.}
\usage{
  geom_hline(mapping = NULL, data = NULL, stat = "hline",
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
  This geom allows you to annotate the plot with horizontal
  lines (see \code{\link{geom_vline}} and
  \code{\link{geom_abline}} for other types of lines).
}
\details{
  There are two ways to use it. You can either specify the
  intercept of the line in the call to the geom, in which
  case the line will be in the same position in every
  panel. Alternatively, you can supply a different
  intercept for each panel using a data.frame. See the
  examples for the differences
}
\section{Aesthetics}{
  \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom",
  "hline")}
}
\examples{
p <- ggplot(mtcars, aes(x = wt, y=mpg)) + geom_point()

p + geom_hline(aes(yintercept=mpg))
p + geom_hline(yintercept=20)
p + geom_hline(yintercept=seq(10, 30, by=5))

# With coordinate transforms
p + geom_hline(aes(yintercept=mpg)) + coord_equal()
p + geom_hline(aes(yintercept=mpg)) + coord_flip()
p + geom_hline(aes(yintercept=mpg)) + coord_polar()

# To display different lines in different facets, you need to
# create a data frame.
p <- qplot(mpg, wt, data=mtcars, facets = vs ~ am)

hline.data <- data.frame(z = 1:4, vs = c(0,0,1,1), am = c(0,1,0,1))
p + geom_hline(aes(yintercept = z), hline.data)
}
\seealso{
  \code{\link{geom_vline}} for vertical lines,
  \code{\link{geom_abline}} for lines defined by a slope
  and intercept, \code{\link{geom_segment}} for a more
  general approach
}

