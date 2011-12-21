\name{guides}
\alias{guides}
\title{Set guides for each scale.}
\usage{
  guides(...)
}
\arguments{
  \item{...}{List of scale guide pairs}
}
\value{
  A list containing mapping between scale and guide.
}
\description{
  Guides for each scale can be set in call of
  \code{scale_*} with argument \code{guide}, or in
  \code{guides}.
}
\examples{
# ggplot object

dat <- data.frame(x = 1:5, y = 1:5, p = 1:5, q = factor(1:5),
 r = factor(1:5))
p <- ggplot(dat, aes(x, y, colour = p, size = q, shape = r)) + geom_point()

# without guide specificatoin
p

# Show colorbar guide for colour.
# All these examples below have a same effect.

p + guides(colour = "colorbar", size = "legend", shape = "legend")
p + guides(colour = guide_colorbar(), size = guide_legend(),
  shape = guide_legend())
p +
 scale_colour_continuous(guide = "colorbar") +
 scale_size_discrete(guide = "legend") +
 scale_shape(guide = "legend")

# Guides are integrated where possible

p + guides(colour = guide_legend("title"), size = guide_legend("title"),
  shape = guide_legend("title"))
# same as
g <- guide_legend("title")
p + guides(colour = g, size = g, shape = g)

p + opts(legend.position = "bottom")

# position of guides

p + opts(legend.position = "bottom", legend.box = "horizontal")
}

