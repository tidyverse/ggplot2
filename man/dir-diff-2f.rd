\name{dir_diff}
\alias{dir_diff}
\title{Functions for comparing images produced by two different versions of ggplot.}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
a <- "~/Desktop/test-1/"
}
\usage{dir_diff(a, b)}
\arguments{
\item{a}{path a}
\item{b}{path b}
\item{}{name of file}
\item{}{location of image a}
\item{}{location of image b}
\item{}{location where output should be saved}
\item{}{character vector of paths}
}
\value{
 \item{list with components only_a, only_b and both}
 \item{boolean}
}
\details{@arguments path a
@arguments path b
@value list with components only_a, only_b and both
Compare two images
Saves image displaying differences

@arguments name of file
@arguments location of image a
@arguments location of image b
@arguments location where output should be saved
@keyword internal
Test if all files are the same
Uses md5 checksum to rapidly check if multiple files are equal.}

\examples{}
\keyword{internal}
