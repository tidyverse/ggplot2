\name{axis_vp_path}
\alias{axis_vp_path}
\title{Axis viewport path}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Compute viewport path for specified component of axis
}
\usage{axis_vp_path(position, name)}
\arguments{
\item{position}{position of axis}
\item{name}{component name}
\item{}{position of ticks}
\item{}{position of axis (top, bottom, left or right)}
\item{}{position of ticks}
\item{}{position of axis (top, bottom, left or right)}
\item{}{position of ticks}
\item{}{grob labels at ticks}
\item{}{position of axis (top, bottom, left or right)}
\item{}{position of axis (top, bottom, left or right)}
\item{}{labels at ticks}
\item{}{range of data values}
}

\details{@arguments position of axis
@arguments component name
@keyword hplot
@keyword internal
Grob axis line
Grob for axis baseline

@arguments position of ticks
@arguments position of axis (top, bottom, left or right)
@keyword hplot
@keyword internal
Grob axis ticks
Grob for axis ticks

@arguments position of ticks
@arguments position of axis (top, bottom, left or right)
@keyword hplot
@keyword internal
Grob axis labels
Grob for axis lables

@arguments position of ticks
@arguments grob labels at ticks
@arguments position of axis (top, bottom, left or right)
@keyword hplot
@keyword internal
switch(position,
top =    textGrob(labels, unit(at, "native"), unit(0.8, "npc"), just = c("centre","top"), rot = 0, check.overlap = TRUE, vp=vp, gp=gp, name="axis-labels"),
bottom = textGrob(labels, unit(at, "native"), unit(0.8, "npc"), just = c("centre","top"), rot = 0, check.overlap = TRUE, vp=vp, gp=gp, name="axis-labels"),
left =   textGrob(labels, unit(1, "npc"), unit(at, "native"), just = c("right","centre"), rot = 0, check.overlap = TRUE, vp=vp, gp=gp, name="axis-labels"),
right =  textGrob(labels, unit(1, "npc"), unit(at, "native"), just = c("right","centre"), rot = 0, check.overlap = TRUE, vp=vp, gp=gp, name="axis-labels")
)
Grob axis viewport
Generate viewport for axis grobs}

\examples{}
\keyword{hplot}
\keyword{internal}
\keyword{hplot}
\keyword{internal}
\keyword{hplot}
\keyword{internal}
\keyword{hplot}
\keyword{internal}
\keyword{hplot}
\keyword{internal}
