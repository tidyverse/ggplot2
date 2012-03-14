set.seed(112)
dat <- data.frame(x=rnorm(20), g=LETTERS[1:2])

# Basic dotplot with binning along x axis
ggplot(dat, aes(x)) + geom_dotplot(binwidth=.4) +
  opts(title='Basic dotplot with dot-density\nbinning, binwidth=.4')

ggplot(dat, aes(x)) + geom_dotplot(binwidth=.4, method="histodot") +
  opts(title='Histodot binning (equal bin spacing)')

ggplot(dat, aes(x)) + geom_dotplot(binwidth=.4, stackratio=.8) +
  opts(title='Dots stacked closer: stackratio=.8')

ggplot(dat, aes(x)) + geom_dotplot(binwidth=.4, dotsize=1.4) +
  opts(title='Larger dots: dotsize=1.4')


# Stacking methods
ggplot(dat, aes(x)) + geom_dotplot(binwidth=.4, stackdir="up") +
  opts(title='Stack up')

ggplot(dat, aes(x)) + geom_dotplot(binwidth=.4, stackdir="down") +
  opts(title='Stack down')

ggplot(dat, aes(x)) + geom_dotplot(binwidth=.4, stackdir="center") +
  opts(title='Stack center')

ggplot(dat, aes(x)) + geom_dotplot(binwidth=.4, stackdir="centerwhole") +
  opts(title='Stack centerwhole')


# Stacking methods with coord_flip
ggplot(dat, aes(x)) + geom_dotplot(binwidth=.4, stackdir="up") + coord_flip() +
  opts(title='Stack up with coord_flip')

ggplot(dat, aes(x)) + geom_dotplot(binwidth=.4, stackdir="down") + coord_flip() +
  opts(title='Stack down with coord_flip')

ggplot(dat, aes(x)) + geom_dotplot(binwidth=.4, stackdir="center") + coord_flip() +
  opts(title='Stack center with coord_flip')

ggplot(dat, aes(x)) + geom_dotplot(binwidth=.4, stackdir="centerwhole") + coord_flip() +
  opts(title='Stack centerwhole with coord_flip')


# Binning along x, with groups
ggplot(dat, aes(x, fill=g)) + geom_dotplot(binwidth=.4, alpha=.4) +
  opts(title='Multiple groups, bins not aligned')

ggplot(dat, aes(x, fill=g)) +
  geom_dotplot(binwidth=.4, alpha=.4, binpositions="all") +
  opts(title='Multiple groups, aligned bins')

ggplot(dat, aes(x, fill=g)) +
  geom_dotplot(binwidth=.4, alpha=.4, binpositions="all", position="stack") +
  opts(title='Multiple groups with aligned bins,\n stacked (currently doesn\'t work)')


# Binning along y axis
ggplot(dat, aes(x=0, y=x)) +
  geom_dotplot(binwidth=.4, binaxis="y", stackdir="center") +
  opts(title='Bin along y, stack center')

ggplot(dat, aes(x=0, y=x)) +
  geom_dotplot(binwidth=.4, binaxis="y", stackdir="centerwhole") +
  opts(title='Bin along y, stack centerwhole')

ggplot(dat, aes(x=0, y=x)) +
  geom_dotplot(binwidth=.4, binaxis="y", stackdir="centerwhole", method="histodot") +
  opts(title='Bin along y, stack centerwhole,\nhistodot binning (fixed bin width)')


# Binning along y, with multiple grouping factors
dat2 <- data.frame(x=LETTERS[1:3], y=rnorm(90), g=LETTERS[1:2])

ggplot(dat2, aes(x=x, y=y)) +
  geom_dotplot(binwidth=.25, binaxis="y", stackdir="centerwhole") +
  opts(title='Bin y, three x groups, stack centerwhole')

ggplot(dat2, aes(x=x, y=y)) +
  geom_dotplot(binwidth=.25, binaxis="y", stackdir="center", binpositions="all") +
  opts(title='Bin y, three x groups, bins aligned across groups')

ggplot(dat2, aes(x=x, y=y)) +
  geom_dotplot(binwidth=.25, binaxis="y", stackdir="center", binpositions="all") +
  coord_flip() +
  opts(title='Bin y, three x groups, bins aligned, coord_flip')

ggplot(dat2, aes(x="foo", y=y, fill=x)) + scale_y_continuous(breaks=seq(-4,4,.4)) +
  geom_dotplot(binwidth=.25, position="dodge", binaxis="y", stackdir="center") +
  opts(title='Bin y, dodging')

ggplot(dat2, aes(x="foo", y=y, fill=x)) + scale_y_continuous(breaks=seq(-4,4,.4)) +
  geom_dotplot(binwidth=.25, position="dodge", binaxis="y", stackdir="center") +
  coord_flip() +
  opts(title='Bin y, dodging, coord_flip')

ggplot(dat2, aes(x=x, y=y, fill=g)) + scale_y_continuous(breaks=seq(-4,4,.4)) +
  geom_dotplot(binwidth=.2, position="dodge", binaxis="y", stackdir="center") +
  opts(title='Bin y, three x groups, fill and dodge')

ggplot(dat2, aes(x=as.numeric(x), y=y, group=x)) +
  geom_dotplot(binwidth=.2, binaxis="y", stackdir="center") +
  opts(title='Bin y, with continuous x axis,\ngrouping by x')

ggplot(dat2, aes(x=as.numeric(x), y=y)) +
  geom_dotplot(binwidth=.2, binaxis="y", stackdir="center") +
  opts(title='Bin y, with continuous x axis,\nsingle x group')
