\name{ggpcp}
\alias{ggpcp}
\title{Make a parallel coordinates plot.}
\usage{
  ggpcp(data, vars = names(data), ...)
}
\arguments{
  \item{data}{data frame}

  \item{vars}{variables to include in parallel coordinates
  plot}

  \item{...}{other arguments passed on plot creation}
}
\description{
  One way to think about a parallel coordinates plot, is as
  plotting the data after it has been transformed to gain a
  new variable.  This function does this using
  \code{\link[reshape]{melt}}.
}
\details{
  This gives us enormous flexibility as we have separated
  out the type of drawing (lines by tradition) and can now
  use any of the existing geom functions.  In particular
  this makes it very easy to create parallel boxplots, as
  shown in the example.
}
\examples{
ggpcp(mtcars) + geom_line()
ggpcp(mtcars, vars=names(mtcars[2:6])) + geom_line()
ggpcp(mtcars) + geom_boxplot(aes(group=variable))

p <- ggpcp(mtcars, vars=names(mtcars[2:6]))
p + geom_line()
p + geom_line(aes(colour=mpg))
}

