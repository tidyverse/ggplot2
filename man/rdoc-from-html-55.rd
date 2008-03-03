\name{rdoc_from_html}
\alias{rdoc_from_html}
\title{Convert rdoc to html}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Crude regexp based conversion from html to rdoc.
}
\usage{rdoc_from_html(html, skip="")}
\arguments{
\item{html}{input rdoc string}
\item{skip}{pass to \code{\link{rdoc_auto_link}}}
}

\details{Assumes well-formed xhtml.  Also autolinks any ggplot functions.}

\examples{}
\keyword{internal}
