\name{GeomCrossbar}
\alias{geom_crossbar}
\alias{GeomCrossbar}
\title{geom_crossbar}
\description{Hollow bar with middle indicated by horizontal line}
\details{
This page describes \code{\link{geom_crossbar}}, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\section{Aesthetics}{
The following aesthetics can be used with geom_crossbar.  Aesthetics are mapped to variables in the data with the \code{\link{aes}} function: \code{geom_crossbar(\code{\link{aes}}(x = var))}
\itemize{
  \item \code{x}: x position (\strong{required}) 
  \item \code{min}: minimum of interval (\strong{required}) 
  \item \code{max}: maximum of interval (\strong{required}) 
  \item \code{colour}: border colour 
  \item \code{fill}: internal colour 
  \item \code{width}: width of geom 
  \item \code{size}: size 
  \item \code{linetype}: line type 
  \item \code{min}: minimum of interval (\strong{required}) 
  \item \code{max}: maximum of interval (\strong{required}) 
}
}
\usage{geom_crossbar(...)}
\arguments{
 \item{...}{other arguments}
}
\seealso{\itemize{
  \item \code{\link{geom_errorbar}}: error bars
  \item \code{\link{geom_pointrange}}: range indicated by straight line, with point in the middle
  \item \code{\link{geom_linerange}}: range indicated by straight line + examples
  \item \code{\link{stat_summary}} : examples of these guys in use
  \item \code{\link{geom_smooth}}: for continuous analog
  \item \url{http://had.co.nz/ggplot/geom_crossbar.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
    # See geom_linerange for examples
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
