\name{facet_wrap}
\alias{facet_wrap}
\alias{FacetWrap}
\title{facet\_wrap}
\description{Wrap a 1d ribbon of panels into 2d.}
\details{
This page describes facet\_wrap, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{facet_wrap(facets, nrow=NULL, ncol=NULL, scales="fixed", as.table=TRUE, ...)}
\arguments{
 \item{facets}{NULL}
 \item{nrow}{number of rows}
 \item{ncol}{number of colums}
 \item{scales}{should scales be fixed, free, or free in one dimension (\code{free_x}, \code{free_y}) }
 \item{as.table}{NULL}
 \item{...}{other arguments}
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot2/facet_wrap.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
d <- ggplot(diamonds, aes(carat, price, fill = ..density..)) + 
  xlim(0, 2) + stat_binhex() + opts(aspect.ratio = 1)
d + facet_wrap(~ color)
d + facet_wrap(~ color, ncol = 4)
d + facet_wrap(~ color, nrow = 4)

# Using multiple variables continues to wrap the long ribbon of 
# plots into 2d - the ribbon just gets longer
d + facet_wrap(~ color + cut)

# You can choose to keep the scales constant across all panels
# or vary the x scale, the y scale or both:
p <- qplot(price, data = diamonds, geom = "histogram", binwidth = 1000)
p + facet_wrap(~ color)
p + facet_wrap(~ color, scales = "free_y")

p <- qplot(displ, hwy, data = mpg)
p + facet_wrap(~ cyl)
p + facet_wrap(~ cyl, scales = "free") 
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
