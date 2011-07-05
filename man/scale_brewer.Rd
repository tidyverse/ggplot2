\name{scale_colour_brewer}
\alias{scale_colour_brewer}
\alias{scale_fill_brewer}
\title{Sequential, diverging and qualitative colour scales from colorbrewer...}
\usage{
  scale_colour_brewer(..., type = "seq", palette = 1)

  scale_fill_brewer(..., type = "seq", palette = 1)
}
\description{
  Sequential, diverging and qualitative colour scales from
  colorbrewer.org
}
\details{
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

