\name{geom_abline}
\alias{geom_abline}
\alias{geom_hline}
\alias{geom_vline}
\title{Horizontal, vertical, and sloped lines.}
\usage{
  geom_abline(mapping = NULL, data = NULL,
    stat = "identity", position = "identity",
    show_guide = FALSE, ...)

  geom_hline(mapping = NULL, data = NULL,
    stat = "identity", position = "identity",
    show_guide = FALSE, ...)

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
  You can either add lines at specified positions with
  \code{annotate(geom="hline")} (or \code{vline} or
  \code{abline}, or you can use variables from a data frame
  to specify the positions, using \code{geom_hline}.
}
\details{
  The \code{annotate} form is useful for adding individual
  lines to a plot, while the \code{geom} form is useful for
  drawing lines directly from the data

  For \code{geom_hline}, specify the y-intercept with
  \code{yintercept}.

  For \code{geom_vline}, specify the x-intercept with
  \code{xintercept}.

  For \code{geom_abline}, specify the y-intercept with
  \code{intercept} and the slope with \code{slope}.
}
\examples{
p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()

# Individual lines
p + annotate("hline", yintercept = 20)
p + annotate("vline", xintercept = 5, colour = "blue")
p + annotate("abline", intercept = 20, slope = 1)

# Using vectors to specify lines
p + annotate("hline", yintercept = seq(10, 30, by = 5))
p + annotate("vline", xintercept = 1:5, colour="darkgreen", linetype = "longdash")
p + annotate("abline", intercept = c(17, 22), slope = c(0.5, 1))

# Map a variable to line properties
p + geom_hline(aes(yintercept = mpg))
p + geom_vline(aes(xintercept = wt), colour = "blue")
p + geom_abline(aes(intercept = mpg, slope = wt))

# Calculate slope and intercept of line of best fit
coef(lm(mpg ~ wt, data = mtcars))
p + annotate_abline(intercept = 37, slope = -5)

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
  \code{\link{annotate}} for adding annotations.
}

