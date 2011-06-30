\name{facet_null}
\alias{facet_null}
\title{Facet specification: a single panel.}
\usage{facet_null(shrink = TRUE)}

\description{
  Facet specification: a single panel.
}
\examples{# facet_null is the default facetting specification if you 
# don't override it with facet_grid or facet_wrap
ggplot(mtcars, aes(mpg, wt)) + geom_point()
qplot(mpg, wt, data = mtcars)}

