\name{facet_grid}
\alias{facet_grid}
\alias{FacetGrid}
\title{facet\_grid}
\description{Lay out panels in a rectangular/tabular manner.}
\details{
This page describes facet\_grid, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{facet_grid(facets=. ~ ., margins=FALSE, ...)}
\arguments{
 \item{facets}{a formula with the rows (of the tabular display) on the LHS and the columns (of the tabular display) on the RHS; the dot in the formula is used to indicate there should be no faceting on this dimension (either row or column); the formula can also be entered as a string instead of a classical formula object}
 \item{margins}{logical value, should marginal rows and columns be displayed}
 \item{...}{other arguments}
}
\seealso{\itemize{
  \item cast: the formula and margin arguments are the same as those used in the reshape package
  \item \url{http://had.co.nz/ggplot2/facet_grid.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
# faceting displays subsets of the data in different panels
p <- ggplot(diamonds, aes(x=carat, y=..density..)) + geom_histogram(binwidth=0.2)

# With one variable
p + facet_grid(. ~ cut)
p + facet_grid(cut ~ .)

# With two variables
p + facet_grid(clarity ~ cut)
p + facet_grid(cut ~ clarity)
p + facet_grid(cut ~ clarity, margins=TRUE)

# You can also use strings, which makes it a little easier
# when writing functions that generate faceting specifications
# p + facet_grid("cut ~ .")

# see also ?plotmatrix for the scatterplot matrix

}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
