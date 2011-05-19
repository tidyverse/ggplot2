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

# Development

`ggplot2` follows the [git flow](http://object.io/site/2011/enter-git-flow/) branching model.  There are two main long-lived branches:

* `master`: main branch containing production-ready (i.e. releasable to CRAN)
  code. This branch should always pass `R CMD check`

* `develop`: where all development occurs. Small changes maybe be made
  directly in this branch, but any larger changes should be made in a feature
  branch created with `git flow feature start`

Other branches are described in more detail on the [main git flow page](http://nvie.com/posts/a-successful-git-branching-model/).

