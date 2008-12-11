\name{scale_brewer}
\alias{scale_brewer}
\alias{scale_colour_brewer}
\alias{scale_fill_brewer}
\alias{ScaleBrewer}
\title{scale\_brewer}
\description{Sequential, diverging and qualitative colour scales from colorbrewer.org}
\details{
See <a href='http://colorbrewer.org'>colorbrewer.org</a> for more info

This page describes scale\_brewer, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{scale_colour_brewer(name=NULL, palette=1, type="qual", alpha=1, limits=NULL, breaks=NULL, labels=NULL, formatter=identity, ...)
scale_fill_brewer(name=NULL, palette=1, type="qual", alpha=1, limits=NULL, breaks=NULL, labels=NULL, formatter=identity, ...)}
\arguments{
 \item{name}{name of scale to appear in legend or on axis.  Maybe be an expression: see ?plotmath}
 \item{palette}{NULL}
 \item{type}{NULL}
 \item{alpha}{NULL}
 \item{limits}{numeric vector of length 2, giving the extent of the scale}
 \item{breaks}{numeric vector indicating where breaks should lie}
 \item{labels}{character vector giving labels associated with breaks}
 \item{formatter}{NULL}
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot2/scale_brewer.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
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

# One way to deal with overplotting - use transparency
# (only works with pdf, quartz and cairo devices)
d + scale_colour_brewer(alpha = 0.5)
d + scale_colour_brewer(alpha = 0.2)
  
# scale_fill_brewer works just the same as 
# scale_colour_brewer but for fill colours
ggplot(diamonds, aes(x=price, fill=cut)) + 
  geom_histogram(position="dodge", binwidth=1000) + 
  scale_fill_brewer()

}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
