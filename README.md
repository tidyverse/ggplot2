# ggplot2

ggplot2 is a plotting system for R, based on the grammar of graphics,
which tries to take the good parts of base and lattice graphics and
avoid bad parts. It takes care of many of the fiddly details
that make plotting a hassle (like drawing legends) as well as
providing a powerful model of graphics that makes it easy to produce
complex multi-layered graphics.

To install or update, run:

    install.packages(c("ggplot2", "plyr"))

Find out more at http://had.co.nz/ggplot2, and check out the nearly 500
examples of ggplot in use.  If you're interested, you can also sign up to 
the ggplot2 mailing list at http://groups.google.com/group/ggplot2, or track
development at http://github.com/hadley/ggplot2

## Development

To install the development version of ggplot2, it's easiest to use the `devtools` package:

    # install.packages("devtools")
    library(devtools)
    install_github("ggplot2")