## aspect ratio
p <- ggplot(data.frame(x=1:8, y=1:8, f=gl(2,4), expand.grid(f1=1:2, f2=1:2, rep=1:2)), aes(x, y)) + geom_point()

p + opts(aspect.ratio=3)
p + facet_wrap(~f) + opts(aspect.ratio=3)
p + facet_grid(.~f) + opts(aspect.ratio=3)
p + facet_grid(f~.) + opts(aspect.ratio=3)
p + facet_grid(f1~f2) + opts(aspect.ratio=3)
p + opts(aspect.ratio=1/3)
p + facet_wrap(~f) + opts(aspect.ratio=1/3)
p + facet_grid(.~f) + opts(aspect.ratio=1/3)
p + facet_grid(f~.) + opts(aspect.ratio=1/3)
p + facet_grid(f1~f2) + opts(aspect.ratio=1/3)

