\name{position_jitter}
\alias{position_jitter}
\title{Jitter points to avoid overplotting.}

\description{
  Jitter points to avoid overplotting.
}
\arguments{
  \item{width}{degree of jitter in x direction. Defaults to 40% of the
resolution of the data.}
  \item{height}{degree of jitter in y direction. Defaults to 40% of the
resolution of the data}
}
\examples{qplot(am, vs, data=mtcars)

# Default amount of jittering will generally be too much for 
# small datasets:
qplot(am, vs, data=mtcars, position="jitter")
# Control the amount as follows
qplot(am, vs, data=mtcars, position=position_jitter(w=0.1, h=0.1))

# The default works better for large datasets, where it will 
# take up as much space as a boxplot or a bar
qplot(cut, price, data=diamonds, geom=c("boxplot", "jitter"))}
