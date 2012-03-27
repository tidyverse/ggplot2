vcontext("violin")

set.seed(111)
dat <- data.frame(x=LETTERS[1:3], y=rnorm(90))
dat <- dat[ dat$x!="C" | c(T,F), ]  # Keep half the C's


ggplot(dat, aes(x=x, y=y)) + geom_violin()
save_vtest("basic")

ggplot(dat, aes(x=x, y=y)) + geom_violin(scale="count")
save_vtest("scale area to sample size (C is smaller)")

ggplot(dat, aes(x=x, y=y)) + geom_violin(width=.5)
save_vtest("narrower (width=.5)")


ggplot(dat, aes(x=x, y=y)) + geom_violin(trim=FALSE) + geom_point(shape=21)
save_vtest("with tails and points")

ggplot(dat, aes(x=x, y=y)) + geom_violin(adjust=.3) + geom_point(shape=21)
save_vtest("with smaller bandwidth and points")


ggplot(dat, aes(x="foo", y=y, fill=x)) + geom_violin()
save_vtest("dodging")

ggplot(dat, aes(x=x, y=y)) + geom_violin() + coord_polar()
save_vtest("coord_polar")

ggplot(dat, aes(x=x, y=y)) + geom_violin() + coord_flip()
save_vtest("coord_flip")

ggplot(dat, aes(x="foo", y=y, fill=x)) + geom_violin() + coord_flip()
save_vtest("dodging and coord_flip")

ggplot(dat, aes(x=as.numeric(x), y=y)) + geom_violin()
save_vtest("continuous x axis, multiple groups (center should be at 2.0)")

ggplot(dat, aes(x=as.numeric(1), y=y)) + geom_violin()
save_vtest("continuous x axis, single group (center should be at 1.0)")

dat2 <- data.frame(x=LETTERS[1:3], y=rnorm(90), g=letters[5:6])

ggplot(dat2, aes(x=x, y=y, fill=g)) + geom_violin()
save_vtest("grouping on x and fill")

ggplot(dat2, aes(x=x, y=y, fill=g)) +
  geom_violin(position=position_dodge(width=.5))
save_vtest("grouping on x and fill, dodge width = 0.5")

end_vcontext()
