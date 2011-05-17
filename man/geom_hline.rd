\name{geom_hline}
\alias{geom_hline}
\title{Horizontal line.}

\description{
  Horizontal line.
}

\details{
  This geom allows you to annotate the plot with horizontal
  lines (see \code{\link{geom_vline}} and
  \code{\link{geom_abline}} for other types of lines).

  There are two ways to use it. You can either specify the
  intercept of the line in the call to the geom, in which
  case the line will be in the same position in every
  panel. Alternatively, you can supply a different
  intercept for each panel using a data.frame. See the
  examples for the differences
}
\seealso{\code{\link{geom_vline}} for vertical lines, 
\code{\link{geom_abline}} for lines defined by a slope and intercept,
\code{\link{geom_segment}} for a more general approach}
\examples{p <- ggplot(mtcars, aes(x = wt, y=mpg)) + geom_point()

p + geom_hline(aes(yintercept=mpg))
p + geom_hline(yintercept=20)
p + geom_hline(yintercept=seq(10, 30, by=5))

# To display different lines in different facets, you need to 
# create a data frame.
p <- qplot(mpg, wt, data=mtcars, facets = vs ~ am)

hline.data <- data.frame(z = 1:4, vs = c(0,0,1,1), am = c(0,1,0,1))
p + geom_hline(aes(yintercept = z), hline.data)}
