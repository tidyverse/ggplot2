vcontext("aspect-ratio")

p <- ggplot(data.frame(x=1:8, y=1:8, f=gl(2,4), expand.grid(f1=1:2, f2=1:2, rep=1:2)), aes(x, y)) + geom_point()

p + opts(aspect.ratio=3)
save_vtest("height is 3 times width")
p + facet_wrap(~f) + opts(aspect.ratio=3)
save_vtest("height is 3 times width, 2 wrap facets")
p + facet_grid(.~f) + opts(aspect.ratio=3)
save_vtest("height is 3 times width, 2 column facets")
p + facet_grid(f~.) + opts(aspect.ratio=3)
save_vtest("height is 3 times width, 2 row facets")
p + facet_grid(f1~f2) + opts(aspect.ratio=3)
save_vtest("height is 3 times width, 2x2 facets")


p + opts(aspect.ratio=1/3)
save_vtest("width is 3 times height")
p + facet_wrap(~f) + opts(aspect.ratio=1/3)
save_vtest("width is 3 times height, 2 wrap facets")
p + facet_grid(.~f) + opts(aspect.ratio=1/3)
save_vtest("width is 3 times height, 2 column facets")
p + facet_grid(f~.) + opts(aspect.ratio=1/3)
save_vtest("width is 3 times height, 2 row facets")
p + facet_grid(f1~f2) + opts(aspect.ratio=1/3)
save_vtest("width is 3 times height, 2x2 facets")

end_vcontext()
