\name{scale_colour_dichromat}
\alias{scale_colour_dichromat}
\alias{scale_fill_dichromat}
\title{Dichromat (colour-blind) scales}
\usage{
  scale_colour_dichromat(..., name = "BluetoOrange.10")

  scale_fill_dichromat(..., name = "BluetoOrange.10")
}
\arguments{
  \item{name}{Name of colour palette.  One of:
  {\code{BrowntoBlue.10}, \code{BrowntoBlue.12},
  \code{BluetoDarkOrange.12}, \code{BluetoDarkOrange.18},
  \code{DarkRedtoBlue.12}, \code{DarkRedtoBlue.18},
  \code{BluetoGreen.14}, \code{BluetoGray.8},
  \code{BluetoOrangeRed.14}, \code{BluetoOrange.10},
  \code{BluetoOrange.12}, \code{BluetoOrange.8},
  \code{LightBluetoDarkBlue.10},
  \code{LightBluetoDarkBlue.7}, \code{Categorical.12},
  \code{GreentoMagenta.16}, \code{SteppedSequential.5}}}

  \item{...}{Other arguments passed on to
  \code{\link{discrete_scale}} to control name, limits,
  breaks, labels and so forth.}
}
\description{
  Color schemes suitable for deficient or anomalous
  red-green vision. See \pkg{dichromat} for more
  information.
}
\examples{
dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
(d <- qplot(carat, price, data=dsamp, colour=clarity))
d + scale_colour_dichromat()

# Select dichromat palette to use, see ?scales::dichromat_pal for more details
d + scale_colour_dichromat(name="BluetoOrange.8")
d + scale_colour_dichromat(name="BluetoGray.8")
d + scale_colour_dichromat(name="Categorical.12")

# scale_fill_dichromat works just the same as
# scale_colour_dichromat but for fill colours
ggplot(diamonds, aes(x=price, fill=cut)) +
  geom_histogram(position="dodge", binwidth=1000) +
  scale_fill_dichromat()
}
\seealso{
  Other colour scales: \code{\link{scale_color_brewer}},
  \code{\link{scale_color_continuous}},
  \code{\link{scale_color_discrete}},
  \code{\link{scale_color_gradient}},
  \code{\link{scale_color_gradient2}},
  \code{\link{scale_color_gradientn}},
  \code{\link{scale_color_grey}},
  \code{\link{scale_color_hue}},
  \code{\link{scale_colour_brewer}},
  \code{\link{scale_colour_continuous}},
  \code{\link{scale_colour_discrete}},
  \code{\link{scale_colour_gradient}},
  \code{\link{scale_colour_gradient2}},
  \code{\link{scale_colour_gradientn}},
  \code{\link{scale_colour_grey}},
  \code{\link{scale_colour_hue}},
  \code{\link{scale_fill_brewer}},
  \code{\link{scale_fill_continuous}},
  \code{\link{scale_fill_discrete}},
  \code{\link{scale_fill_gradient}},
  \code{\link{scale_fill_gradient2}},
  \code{\link{scale_fill_gradientn}},
  \code{\link{scale_fill_grey}},
  \code{\link{scale_fill_hue}}
}

