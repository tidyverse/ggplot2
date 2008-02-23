\name{FacetGrid}
\alias{facet_grid}
\alias{FacetGrid}
\title{facet_grid}
\description{Lay out panels in a rectangular/tabular manner.}
\details{
This page describes \code{\link{facet_grid}}, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{facet_grid(...)}
\arguments{
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot/facet_grid.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
    # Facetting displays subsets of the data in different panels
    p <- ggplot(diamonds, aes(x=carat, y=price)) + geom_point() + geom_smooth(aes(colour=cut), method="lm") + scale_x_log10() + scale_y_log10()
    
    # With one variable
    p + facet_grid(. ~ cut)
    p + facet_grid(cut ~ .)
    p + facet_grid(cut ~ ., margins=TRUE)

    # With two variables
    p + facet_grid(clarity ~ cut)
    p + facet_grid(cut ~ clarity)
    p + facet_grid(cut ~ clarity, margins=TRUE)
    # and only useful if you have a reaaaaaallly long screen
    p + facet_grid(cut + clarity ~ .)
    
    # You can also use strings, which makes it a little easier
    # when writing functions that generate facetting specifications
    p + facet_grid("cut ~ .")
    
    # It still works even if not all panels have data in them
    airquality$week <- airquality$Day %/% 7
    qplot(Ozone, Wind, data=airquality, facets = Month ~ week)
    
    # see also ?plotmatrix
    
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
