set.seed(111)
dat <- data.frame(x=LETTERS[1:3], y=rnorm(90))
dat <- dat[ dat$x!="C" | c(T,F), ]  # Keep half the C's


ggplot(dat, aes(x=x, y=y)) + geom_violin() +
  opts(title="Basic violin")

ggplot(dat, aes(x=x, y=y)) + geom_violin() + geom_point(shape=21) +
  opts(title="Basic violin with overlaid points")

ggplot(dat, aes(x=x, y=y)) + geom_violin(scale="count") +
  opts(title="Scale area to sample size (C is smaller)")

ggplot(dat, aes(x=x, y=y)) + geom_violin(width=.5) +
  opts(title="Narrower (width=.5)")


ggplot(dat, aes(x=x, y=y)) + geom_violin(trim=FALSE) + geom_point(shape=21) +
  opts(title="With tails and points")

ggplot(dat, aes(x=x, y=y)) + geom_violin(adjust=.3) + geom_point(shape=21) +
  opts(title="With smaller bandwidth and points")


ggplot(dat, aes(x="foo", y=y, fill=x)) + geom_violin() +
  opts(title="Dodging")

ggplot(dat, aes(x=x, y=y)) + geom_violin() + coord_polar() +
  opts(title="coord_polar")

ggplot(dat, aes(x=x, y=y)) + geom_violin() + coord_flip() +
  opts(title="coord_flip")

ggplot(dat, aes(x="foo", y=y, fill=x)) + geom_violin() + coord_flip() +
  opts(title="Dodging and coord_flip")

ggplot(dat, aes(x=as.numeric(x), y=y)) + geom_violin() +
  opts(title="Continuous x axis, multiple groups\n(center should be 2)")

ggplot(dat, aes(x=as.numeric(1), y=y)) + geom_violin() +
  opts(title="Continuous x axis, single group")

ggplot(d2, aes(x=x, y=y, fill=g)) + geom_violin() +
  opts(title="Grouping on x and fill")

