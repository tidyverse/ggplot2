\name{geom_rect}
\alias{geom_rect}
\title{2d rectangles.}
\usage{geom_rect(mapping = NULL, data = NULL, stat = "identity",
    position = "identity", ...)}

\description{
  2d rectangles.
}
\examples{df <- data.frame(
x = sample(10, 20, replace = TRUE),
y = sample(10, 20, replace = TRUE)
)
ggplot(df, aes(xmin = x, xmax = x + 1, ymin = y, ymax = y + 2)) +
geom_rect()}

