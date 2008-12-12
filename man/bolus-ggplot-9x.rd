\name{bolus.ggplot}
\alias{bolus.ggplot}
\alias{bolus}
\alias{bolus.proto}
\alias{digest.ggplot}
\alias{digest.proto}
\title{Create a bolus object}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
A bolus is a list suitable for digesting.
}
\usage{bolus.ggplot(x, ...)}
\arguments{
\item{x}{}
\item{...}{}
}

\details{Most ggplot objects have components that should be hashed when creating
a digest (especially since most ggplot objects are proto objects and
are also self-documenting).  The bolus methods ensure that only appropriate
components are digested.}

\examples{hash_tests <- list(
list(
ggplot() + scale_x_continuous() + scale_y_continuous(),
ggplot() + scale_y_continuous() + scale_x_continuous()
),
list(
qplot(mpg, wt, data=mtcars, na.rm = FALSE),
ggplot(mtcars, aes(y=wt, x=mpg)) + geom_point()
),
list(
qplot(mpg, wt, data=mtcars, xlab = "blah"),
qplot(mpg, wt, data=mtcars) + scale_x_continuous("blah")
)
)

lapply(hash_tests, function(equal) {
hashes <- lapply(equal, digest.ggplot)

if (length(unique(hashes)) != 1) {
lapply(equal, function(x) print(str(bolus(x))))
stop("Above plots not equal")
}
})}
\keyword{internal}
