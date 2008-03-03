\name{FacetGrid}
\alias{facet_grid}
\alias{FacetGrid}
\title{facet_grid}
\description{Lay out panels in a rectangular/tabular manner.}
\details{
This page describes \code{\link{facet_grid}}, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{facet_grid(facets=. ~ ., margins=FALSE, ...)}
\arguments{
 \item{facets}{NULL}
 \item{margins}{NULL}
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot/facet_grid.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
    # Facetting displays subsets of the data in different panels
    p <- ggplot(diamonds, aes(x=carat, y=..density..)) + geom_histogram(binwidth=0.2)
    
    # With one variable
    p + facet_grid(. ~ cut)
    p + facet_grid(cut ~ .)

    # With two variables
    p + facet_grid(clarity ~ cut)
    p + facet_grid(cut ~ clarity)
    p + facet_grid(cut ~ clarity, margins=TRUE)
    
    # You can also use strings, which makes it a little easier
    # when writing functions that generate facetting specifications
    # p + facet_grid("cut ~ .")
    
    # see also ?plotmatrix for the scatterplot matrix
    
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
