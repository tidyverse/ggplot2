vcontext("scale-breaks")
dat <- data.frame(x=1:3, y=1:3)

ggplot(dat, aes(x=x, y=y)) + geom_point() + scale_x_continuous(breaks=NULL)
save_vtest("no x breaks")

ggplot(dat, aes(x=x, y=y)) + geom_point() + scale_y_continuous(breaks=NULL)
save_vtest("no y breaks")

ggplot(dat, aes(x=1, y=y, alpha=x)) + geom_point() + scale_alpha_continuous(breaks=NULL)
save_vtest("no alpha breaks (no legend)")

ggplot(dat, aes(x=1, y=y, size=x)) + geom_point() + scale_size_continuous(breaks=NULL)
save_vtest("no size breaks (no legend)")

ggplot(dat, aes(x=1, y=y, fill=x)) + geom_point(shape=21) + scale_fill_continuous(breaks=NULL)
save_vtest("no fill breaks (no legend)")

ggplot(dat, aes(x=1, y=y, colour=x)) + geom_point() + scale_colour_continuous(breaks=NULL)
save_vtest("no colour breaks (no legend)")

end_vcontext()
