\name{label_parsed}
\alias{label_parsed}
\title{X mtcars$cyl2 <- factor(mtcars$cyl, labels = c("alpha", "beta", "gamma"))}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
X qplot(wt, mpg, data = mtcars) + facet_grid(. ~ cyl2)
}
\usage{label_parsed(variable, value)}
\arguments{
\item{variable}{}
\item{value}{}
}

\details{}

\examples{mtcars$cyl2 <- factor(mtcars$cyl, labels = c("alpha", "beta", "gamma"))
qplot(wt, mpg, data = mtcars) + facet_grid(. ~ cyl2)
qplot(wt, mpg, data = mtcars) + facet_grid(. ~ cyl2, 
labeller = label_parsed)}

