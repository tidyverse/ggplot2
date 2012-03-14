dat <- data.frame(x=1:3, y=1:3)

ggplot(dat, aes(x=x, y=y)) + geom_point() + scale_x_continuous(breaks=NA)
ggplot(dat, aes(x=x, y=y)) + geom_point() + scale_y_continuous(breaks=NA)
ggplot(dat, aes(x=1, y=y, alpha=x)) + geom_point() + scale_alpha_continuous(breaks=NA)
ggplot(dat, aes(x=1, y=y, size=x)) + geom_point() + scale_size_continuous(breaks=NA)
ggplot(dat, aes(x=1, y=y, fill=x)) + geom_point(shape=21) + scale_fill_continuous(breaks=NA)
ggplot(dat, aes(x=1, y=y, colour=x)) + geom_point() + scale_colour_continuous(breaks=NA)
