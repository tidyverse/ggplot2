\name{geom_vline}
\alias{geom_vline}
\title{Line, vertical.}
\usage{
  geom_vline(mapping = NULL, data = NULL,
    stat = "identity", position = "identity",
    show_guide = FALSE, ...)
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
  This geom allows you to add vertical lines (see
  \code{\link{geom_vline}} and \code{\link{geom_abline}}
  for other types of lines).
}
\details{
  To specify the intercept of the line directly, use
  \code{annotate_vline}. If you use this, the lines will be
  in the same position in each panel.

  To map variables from the data to the x-intercept
  position of the lines, use \code{geom_vline}. If you use
  this the lines can be in different positions in each
  panel.
}
\examples{
p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()

# Map a variable to yintercept
p + geom_vline(aes(xintercept = wt))

# Specify yintercept directly
p + annotate_vline(xintercept = 5)
p + annotate_vline(xintercept = 1:5)
p + annotate_vline(xintercept = 1:5, colour="green", linetype = "longdash")

# With coordinate transforms
p + geom_vline(aes(xintercept = wt)) + coord_equal()
p + geom_vline(aes(xintercept = wt)) + coord_flip()
p + geom_vline(aes(xintercept = wt)) + coord_polar()

p2 <- p + aes(colour = factor(cyl))
p2 + annotate_vline(xintercept = 15)

# To display different lines in different facets, you need to
# create a data frame.
p <- qplot(mpg, wt, data = mtcars, facets = vs ~ am)
vline.data <- data.frame(z = c(15, 20, 25, 30), vs = c(0, 0, 1, 1), am = c(0, 1, 0, 1))
p + geom_vline(aes(xintercept = z), vline.data, inherit.aes = FALSE)
}
\seealso{
  \code{\link{geom_hline}} for horizontal lines,
  \code{\link{geom_abline}} for lines defined by a slope
  and intercept, \code{\link{geom_segment}} for a more
  general approach
}

