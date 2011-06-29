\name{geom_jitter}
\alias{geom_jitter}
\title{Points, jittered to reduce overplotting.}
\usage{geom_jitter(mapping, data, stat="identity",
    position="jitter", na.rm=FALSE, ...)}

\description{
  Points, jittered to reduce overplotting.
}

\details{
  The jitter geom is a convenient default for geom_point
  with position = 'jitter'.  See
  \code{\link{position_jitter}} to see how to adjust amount
  of jittering.
}
\seealso{\code{\link{geom_point}} for regular, unjittered points, 
\code{\link{geom_boxplot}} for another way of looking at the conditional
distribution of a variable,
\code{\link{position_jitter}} for examples of using jittering with other
geoms}
\examples{p <- ggplot(movies, aes(x=mpaa, y=rating)) 
p + geom_point()
p + geom_point(position = "jitter")

# Add aesthetic mappings
p + geom_jitter(aes(colour=rating))

# Vary parameters
p + geom_jitter(position=position_jitter(width=5))
p + geom_jitter(position=position_jitter(height=5))

# Use qplot instead
qplot(mpaa, rating, data=movies, geom="jitter")
qplot(mpaa, rating, data=movies, geom=c("boxplot","jitter"))
qplot(mpaa, rating, data=movies, geom=c("jitter", "boxplot"))}
