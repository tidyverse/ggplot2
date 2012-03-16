vcontext("dotplot")

set.seed(112)
dat <- data.frame(x=rnorm(20), g=LETTERS[1:2])

# Basic dotplot with binning along x axis
ggplot(dat, aes(x)) + geom_dotplot(binwidth=.4)
save_vtest("basic", "Basic dotplot with dot-density binning, binwidth=.4")

ggplot(dat, aes(x)) + geom_dotplot(binwidth=.4, method="histodot")
save_vtest("histodot", "Histodot binning (equal bin spacing)")

ggplot(dat, aes(x)) + geom_dotplot(binwidth=.4, stackratio=.8)
save_vtest("stackratio", "Dots stacked closer: stackratio=.8")

ggplot(dat, aes(x)) + geom_dotplot(binwidth=.4, dotsize=1.4)
save_vtest("dotsize", "Larger dots: dotsize=1.4")


# Stacking methods
ggplot(dat, aes(x)) + geom_dotplot(binwidth=.4, stackdir="up")
save_vtest("stack-up", "Stack up")

ggplot(dat, aes(x)) + geom_dotplot(binwidth=.4, stackdir="down")
save_vtest("stack-down", "Stack down")

ggplot(dat, aes(x)) + geom_dotplot(binwidth=.4, stackdir="center")
save_vtest("stack-center", "Stack center")

ggplot(dat, aes(x)) + geom_dotplot(binwidth=.4, stackdir="centerwhole")
save_vtest("stack-centerwhole", "Stack centerwhole")


# Stacking methods with coord_flip
ggplot(dat, aes(x)) + geom_dotplot(binwidth=.4, stackdir="up") + coord_flip()
save_vtest("stack-up-flip", "Stack up with coord_flip")

ggplot(dat, aes(x)) + geom_dotplot(binwidth=.4, stackdir="down") + coord_flip()
save_vtest("stack-down-flip", "Stack down with coord_flip")

ggplot(dat, aes(x)) + geom_dotplot(binwidth=.4, stackdir="center") + coord_flip()
save_vtest("stack-center-flip", "Stack center with coord_flip")

ggplot(dat, aes(x)) + geom_dotplot(binwidth=.4, stackdir="centerwhole") + coord_flip()
save_vtest("stack-centerwhole-flip", "Stack centerwhole with coord_flip")


# Binning along x, with groups
ggplot(dat, aes(x, fill=g)) + geom_dotplot(binwidth=.4, alpha=.4)
save_vtest("multigroup", "Multiple groups, bins not aligned")

ggplot(dat, aes(x, fill=g)) +
  geom_dotplot(binwidth=.4, alpha=.4, binpositions="all")
save_vtest("multigroup-aligned", "Multiple groups, bins aligned")

ggplot(dat, aes(x, fill=g)) +
  geom_dotplot(binwidth=.4, alpha=.4, binpositions="all", position="stack")
save_vtest("multigroup-aligned-stack", "Multiple groups with aligned bins, stacked (currently doesn't work)")


# Binning along y axis
ggplot(dat, aes(x=0, y=x)) +
  geom_dotplot(binwidth=.4, binaxis="y", stackdir="center")
save_vtest("biny-center", "Bin along y, stack center")

ggplot(dat, aes(x=0, y=x)) +
  geom_dotplot(binwidth=.4, binaxis="y", stackdir="centerwhole")
save_vtest("biny-centerwhole", "Bin along y, stack centerwhole")

ggplot(dat, aes(x=0, y=x)) +
  geom_dotplot(binwidth=.4, binaxis="y", stackdir="centerwhole", method="histodot")
save_vtest("biny-centerwhole-histodot", "Bin along y, stack centerwhole, histodot")


# Binning along y, with multiple grouping factors
dat2 <- data.frame(x=LETTERS[1:3], y=rnorm(90), g=LETTERS[1:2])

ggplot(dat2, aes(x=x, y=y)) +
  geom_dotplot(binwidth=.25, binaxis="y", stackdir="centerwhole")
save_vtest("biny-groupx-centerwhole", "Bin y, three x groups, stack centerwhole")

ggplot(dat2, aes(x=x, y=y)) +
  geom_dotplot(binwidth=.25, binaxis="y", stackdir="center", binpositions="all") +
save_vtest("biny-groupx-aligned", "Bin y, three x groups, bins aligned across groups")

ggplot(dat2, aes(x=x, y=y)) +
  geom_dotplot(binwidth=.25, binaxis="y", stackdir="center", binpositions="all") +
  coord_flip() +
save_vtest("biny-groupx-aligned-flip", "Bin y, three x groups, bins aligned, coord_flip")

ggplot(dat2, aes(x="foo", y=y, fill=x)) + scale_y_continuous(breaks=seq(-4,4,.4)) +
  geom_dotplot(binwidth=.25, position="dodge", binaxis="y", stackdir="center")
save_vtest("biny-dodge", "Bin y, dodged")

ggplot(dat2, aes(x="foo", y=y, fill=x)) + scale_y_continuous(breaks=seq(-4,4,.4)) +
  geom_dotplot(binwidth=.25, position="dodge", binaxis="y", stackdir="center") +
  coord_flip()
save_vtest("biny-dodge-flip", "Bin y, dodged, coord_flip")


ggplot(dat2, aes(x=x, y=y, fill=g)) + scale_y_continuous(breaks=seq(-4,4,.4)) +
  geom_dotplot(binwidth=.2, position="dodge", binaxis="y", stackdir="center")
save_vtest("biny-groupx-dodge", "Bin y, three x groups, fill and dodge")

ggplot(dat2, aes(x=as.numeric(x), y=y, group=x)) +
  geom_dotplot(binwidth=.2, binaxis="y", stackdir="center")
save_vtest("biny-contx-groupx", "Bin y, continous x-axis, grouping by x")

ggplot(dat2, aes(x=as.numeric(x), y=y)) +
  geom_dotplot(binwidth=.2, binaxis="y", stackdir="center")
save_vtest("biny-contx-singlex", "Bin y, continous x-axis, single x group")

finish_vcontext()