\name{facet_grid}
\alias{facet_grid}
\alias{FacetGrid}
\title{facet\_grid}
\description{Lay out panels in a rectangular/tabular manner.}
\details{
This page describes facet\_grid, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{facet_grid(facets = . ~ ., margins = FALSE, scales = "fixed", 
    space = "fixed", labeller = "label_value", as.table = TRUE, 
    widths = NULL, heights = NULL, ...)}
\arguments{
 \item{facets}{a formula with the rows (of the tabular display) on the LHS and the columns (of the tabular display) on the RHS; the dot in the formula is used to indicate there should be no faceting on this dimension (either row or column); the formula can also be entered as a string instead of a classical formula object}
 \item{margins}{logical value, should marginal rows and columns be displayed}
 \item{scales}{NULL}
 \item{space}{NULL}
 \item{labeller}{NULL}
 \item{as.table}{NULL}
 \item{widths}{NULL}
 \item{heights}{NULL}
 \item{...}{other arguments}
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot2/facet_grid.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
# faceting displays subsets of the data in different panels
p <- ggplot(diamonds, aes(carat, ..density..)) +
 geom_histogram(binwidth = 1)

# With one variable
p + facet_grid(. ~ cut)
p + facet_grid(cut ~ .)

# With two variables
p + facet_grid(clarity ~ cut)
p + facet_grid(cut ~ clarity)
# p + facet_grid(cut ~ clarity, margins=TRUE)

qplot(mpg, wt, data=mtcars, facets = . ~ vs + am)
qplot(mpg, wt, data=mtcars, facets = vs + am ~ . )

# You can also use strings, which makes it a little easier
# when writing functions that generate faceting specifications
# p + facet_grid("cut ~ .")

# see also ?plotmatrix for the scatterplot matrix

# If there isn't any data for a given combination, that panel 
# will be empty
qplot(mpg, wt, data=mtcars) + facet_grid(cyl ~ vs)

# If you combine a facetted dataset with a dataset that lacks those
# facetting variables, the data will be repeated across the missing
# combinations:
p <- qplot(mpg, wt, data=mtcars, facets = vs ~ cyl)

df <- data.frame(mpg = 22, wt = 3)
p + geom_point(data = df, colour="red", size = 2)

df2 <- data.frame(mpg = c(19, 22), wt = c(2,4), vs = c(0, 1))
p + geom_point(data = df2, colour="red", size = 2)

df3 <- data.frame(mpg = c(19, 22), wt = c(2,4), vs = c(1, 1))
p + geom_point(data = df3, colour="red", size = 2)


# You can also choose whether the scales should be constant
# across all panels (the default), or whether they should be allowed
# to vary
mt <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) + geom_point()

mt + facet_grid(. ~ cyl, scales = "free")
# If scales and space are free, then the mapping between position
# and values in the data will be the same across all panels
mt + facet_grid(. ~ cyl, scales = "free", space = "free")

mt + facet_grid(vs ~ am, scales = "free")
mt + facet_grid(vs ~ am, scales = "free_x")
mt + facet_grid(vs ~ am, scales = "free_y")
mt + facet_grid(vs ~ am, scales = "free", space="free")

# You may need to set your own breaks for consitent display:
mt + facet_grid(. ~ cyl, scales = "free_x", space="free") + 
  scale_x_continuous(breaks = seq(10, 36, by = 2))
# Adding scale limits override free scales:
last_plot() + xlim(10, 15)

# Free scales are particularly useful for categorical variables
qplot(cty, model, data=mpg) + 
  facet_grid(manufacturer ~ ., scales = "free", space = "free")
# particularly when you reorder factor levels
mpg <- within(mpg, {
  model <- reorder(model, cty)
  manufacturer <- reorder(manufacturer, cty)
})
last_plot() %+% mpg + opts(strip.text.y = theme_text())
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
