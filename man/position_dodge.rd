\name{PositionDodge}
\alias{position_dodge}
\alias{PositionDodge}
\title{position_dodge}
\description{Adjust position by dodging overlaps to the side}
\details{
This page describes \code{\link{position_dodge}}, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{position_dodge(width=NULL, ...)}
\arguments{
 \item{width}{NULL}
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot/position_dodge.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
    ggplot(mtcars, aes(x=factor(cyl), fill=factor(vs))) + geom_bar(position="dodge")
    ggplot(diamonds, aes(x=price, fill=cut)) + geom_bar(position="dodge")
    # see ?geom_boxplot and ?geom_bar for more examples
    
    df <- data.frame(x=c("a","a","b","b"), y=1:4)
    p <- qplot(x, y, data=df, position="dodge", geom="bar", stat="identity")
    p 
    p + geom_linerange(aes(min= y - 1, max = y+1), position="dodge")
    # Dodging things with different widths is tricky
    p + geom_errorbar(aes(min= y - 1, max = y+1), width=0.2, position="dodge")
    # You can specify the width to use for dodging (instead of the actual
    # width of the object) as follows
    p + geom_errorbar(aes(min= y - 1, max = y+1, width=0.2), position=position_dodge(width=0.90))
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
