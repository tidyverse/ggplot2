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
  This geom allows you to add horizontal lines (see
  \code{\link{geom_vline}} and \code{\link{geom_abline}}
  for other types of lines).
}
\details{
  To map variables from the data to the y-intercept
  position of the lines, use \code{geom_hline}. To specify
  the intercept of the line directly, use
  \code{annotate_hline}. In this case, the lines will be in
  the same position in every panel.
}
\examples{
p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()

# Map a variable to yintercept
p + geom_hline(aes(yintercept = mpg))

# Specify yintercept directly
p + annotate_hline(yintercept = 20)
p + annotate_hline(yintercept = seq(10, 30, by = 5))

# With coordinate transforms
p + geom_hline(aes(yintercept = mpg)) + coord_equal()
p + geom_hline(aes(yintercept = mpg)) + coord_flip()
p + geom_hline(aes(yintercept = mpg)) + coord_polar()

# To display different lines in different facets, you need to
# create a data frame.
p <- qplot(mpg, wt, data=mtcars, facets = vs ~ am)

hline.data <- data.frame(z = 1:4, vs = c(0,0,1,1), am = c(0,1,0,1))
p + geom_hline(aes(yintercept = z), hline.data, inherit.aes = FALSE)
}
\seealso{
  \code{\link{geom_vline}} for vertical lines,
  \code{\link{geom_abline}} for lines defined by a slope
  and intercept, \code{\link{geom_segment}} for a more
  general approach
}

