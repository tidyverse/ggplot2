\name{scale_colour_brewer}
\alias{scale_color_brewer}
\alias{scale_colour_brewer}
\alias{scale_fill_brewer}
\title{Sequential, diverging and qualitative colour scales from colorbrewer.org}
\usage{
  scale_colour_brewer(..., type = "seq", palette = 1)

  scale_fill_brewer(..., type = "seq", palette = 1)

  scale_color_brewer(..., type = "seq", palette = 1)
}
\arguments{
  \item{type}{One of seq (sequential), div (diverging) or
  qual (qualitative)}

  \item{palette}{If a string, will use that named palette.
  If a number, will index into the list of palettes of
  appropriate \code{type}}

  \item{...}{Other arguments passed on to
  \code{\link{continuous_scale}} to control name, limits,
  breaks, labels and so forth.}
}
\description{
  See \url{http://colorbrewer2.org} for more information.
}
\examples{
dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
(d <- qplot(carat, price, data=dsamp, colour=clarity))

# Change scale label
d + scale_colour_brewer()
d + scale_colour_brewer("clarity")
d + scale_colour_brewer(expression(clarity[beta]))

# Select brewer palette to use, see ?brewer.pal for more details
d + scale_colour_brewer(type="seq")
d + scale_colour_brewer(type="seq", palette=3)

RColorBrewer::display.brewer.all(n=8, exact.n=FALSE)

d + scale_colour_brewer(palette="Blues")
d + scale_colour_brewer(palette="Set1")

# scale_fill_brewer works just the same as
# scale_colour_brewer but for fill colours
ggplot(diamonds, aes(x=price, fill=cut)) +
  geom_histogram(position="dodge", binwidth=1000) +
  scale_fill_brewer()
}
\seealso{
  Other colour scales:
  \code{\link{scale_color_continuous}},
  \code{\link{scale_color_discrete}},
  \code{\link{scale_color_gradient}},
  \code{\link{scale_color_gradient2}},
  \code{\link{scale_color_gradientn}},
  \code{\link{scale_color_grey}},
  \code{\link{scale_color_hue}},
  \code{\link{scale_colour_continuous}},
  \code{\link{scale_colour_discrete}},
  \code{\link{scale_colour_gradient}},
  \code{\link{scale_colour_gradient2}},
  \code{\link{scale_colour_gradientn}},
  \code{\link{scale_colour_grey}},
  \code{\link{scale_colour_hue}},
  \code{\link{scale_fill_continuous}},
  \code{\link{scale_fill_discrete}},
  \code{\link{scale_fill_gradient}},
  \code{\link{scale_fill_gradient2}},
  \code{\link{scale_fill_gradientn}},
  \code{\link{scale_fill_grey}},
  \code{\link{scale_fill_hue}}
}

