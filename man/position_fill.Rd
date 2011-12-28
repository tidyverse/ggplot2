\name{position_fill}
\alias{position_fill}
\title{Stack overlapping objects on top of one another, and standardise to have
equal height.}
\usage{
  position_fill(width = NULL, height = NULL)
}
\description{
  Stack overlapping objects on top of one another, and
  standardise to have equal height.
}
\examples{
# See ?geom_bar and ?geom_area for more examples
ggplot(mtcars, aes(x=factor(cyl), fill=factor(vs))) +
  geom_bar(position="fill")

cde <- geom_histogram(position="fill", binwidth = 500)

ggplot(diamonds, aes(x=price)) + cde
ggplot(diamonds, aes(x=price, fill=cut)) + cde
ggplot(diamonds, aes(x=price, fill=clarity)) + cde
ggplot(diamonds, aes(x=price, fill=color)) + cde
}

