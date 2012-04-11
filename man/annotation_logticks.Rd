\name{annotation_logticks}
\alias{annotation_logticks}
\title{Annotation: log tick marks}
\usage{
  annotation_logticks(base = 10, sides = "bl",
    scaled = TRUE, short = unit(0.1, "cm"),
    mid = unit(0.2, "cm"), long = unit(0.3, "cm"), ...)
}
\arguments{
  \item{base}{the base of the log (default 10)}

  \item{sides}{a string that controls which sides of the
  plot the log ticks appear on.  It can be set to a string
  containing any of \code{"trbl"}, for top, right, bottom,
  and left.}

  \item{short}{a \code{unit} object specifying the length
  of the short tick marks}

  \item{mid}{a \code{unit} object specifying the length of
  the middle tick marks.  In base 10, these are the "5"
  ticks.}

  \item{long}{a \code{unit} object specifying the length of
  the long tick marks.  In base 10, these are the "1" (or
  "10") ticks.}

  \item{scaled}{is the data already log-scaled? This should
  be \code{TRUE} (default) when the data is already
  transformed with \code{log10()} or when using
  \code{scale_y_log10}. It should be \code{FALSE} when
  using \code{coord_trans(y = "log10")}.}
}
\description{
  This annotation adds log tick marks with diminishing
  spacing. These tick marks probably make sense only for
  base 10.
}
\examples{
# Make a log-log plot (without log ticks)
library(MASS)
library(scales)
a <- ggplot(Animals, aes(x = body, y = brain)) + geom_point() +
     scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                   labels = trans_format("log10", math_format(10^.x))) +
     scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                   labels = trans_format("log10", math_format(10^.x))) +
     theme_bw()

a + annotation_logticks()                # Default: log ticks on bottom and left
a + annotation_logticks(sides = "lr")    # Log ticks for y, on left and right
a + annotation_logticks(sides = "trbl")  # All four sides

# Hide the minor grid lines because they don't align with the ticks
a + annotation_logticks(sides = "trbl") + opts(panel.grid.minor = theme_blank())


# Another way to get the same results as 'a' above: log-transform the data before
b <- ggplot(Animals, aes(x = log10(body), y = log10(brain))) + geom_point() +
     scale_x_continuous(name = "body", labels = math_format(10^.x)) +
     scale_y_continuous(name = "brain", labels = math_format(10^.x)) +
     theme_bw()+ opts(panel.grid.minor = theme_blank())

b + annotation_logticks()


# This shows log(x) on the axes
d <- ggplot(Animals, aes(x = log10(body), y = log10(brain))) + geom_point() +
     theme_bw()

d + annotation_logticks()


# Using a coordinate transform requires scaled = FALSE
t <- ggplot(Animals, aes(x = body, y = brain)) + geom_point() +
     coord_trans(xtrans = "log10", ytrans = "log10") + theme_bw()

t + annotation_logticks(scaled = FALSE)


# Change the length of the ticks
library(grid)
a + annotation_logticks(short = unit(.5,"mm"), mid = unit(3,"mm"), long = unit(4,"mm"))
}
\seealso{
  \code{\link{scale_y_continuous}},
  \code{\link{scale_y_log10}} for log scale
  transformations.

  \code{\link{coord_trans}} for log coordinate
  transformations.
}

