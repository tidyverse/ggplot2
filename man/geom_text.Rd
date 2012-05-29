\name{geom_text}
\alias{geom_text}
\title{Textual annotations.}
\usage{
  geom_text(mapping = NULL, data = NULL, stat = "identity",
    position = "identity", parse = FALSE, ...)
}
\arguments{
  \item{parse}{If TRUE, the labels will be parsed into
  expressions and displayed as described in ?plotmath}

  \item{mapping}{The aesthetic mapping, usually constructed
  with \code{\link{aes}} or \code{\link{aes_string}}. Only
  needs to be set at the layer level if you are overriding
  the plot defaults.}

  \item{data}{A layer specific dataset - only needed if you
  want to override the plot defaults.}

  \item{stat}{The statistical transformation to use on the
  data for this layer.}

  \item{position}{The position adjustment to use for
  overlappling points on this layer}

  \item{...}{other arguments passed on to
  \code{\link{layer}}. This can include aesthetics whose
  values you want to set, not map. See \code{\link{layer}}
  for more details.}
}
\description{
  Textual annotations.
}
\section{Aesthetics}{
  \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom",
  "text")}
}
\examples{
\donttest{
p <- ggplot(mtcars, aes(x=wt, y=mpg, label=rownames(mtcars)))

p + geom_text()
# Change size of the label
p + geom_text(size=10)
p <- p + geom_point()

# Set aesthetics to fixed value
p + geom_text()
p + geom_point() + geom_text(hjust=0, vjust=0)
p + geom_point() + geom_text(angle = 45)

# Add aesthetic mappings
p + geom_text(aes(colour=factor(cyl)))
p + geom_text(aes(colour=factor(cyl))) + scale_colour_discrete(l=40)

p + geom_text(aes(size=wt))
p + geom_text(aes(size=wt)) + scale_size(range=c(3,6))

# You can display expressions by setting parse = TRUE.  The
# details of the display are described in ?plotmath, but note that
# geom_text uses strings, not expressions.
p + geom_text(aes(label = paste(wt, "^(", cyl, ")", sep = "")),
  parse = TRUE)

# Add an annotation not from a variable source
c <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
c + geom_text(data = NULL, x = 5, y = 30, label = "plot mpg vs. wt")
# Or, you can use annotate
c + annotate("text", label = "plot mpg vs. wt", x = 2, y = 15, size = 8, colour = "red")

# Use qplot instead
qplot(wt, mpg, data = mtcars, label = rownames(mtcars),
   geom=c("point", "text"))
qplot(wt, mpg, data = mtcars, label = rownames(mtcars), size = wt) +
  geom_text(colour = "red")

# You can specify family, fontface and lineheight
p <- ggplot(mtcars, aes(x=wt, y=mpg, label=rownames(mtcars)))
p + geom_text(fontface=3)
p + geom_text(aes(fontface=am+1))
p + geom_text(aes(family=c("serif", "mono")[am+1]))
}
}

