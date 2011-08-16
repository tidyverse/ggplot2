## aspect ratio
p <- ggplot(data.frame(x=1:8, y=1:8, f=gl(2,4), expand.grid(f1=1:2, f2=1:2, rep=1:2)), aes(x, y)) + geom_point()

pl <- list()
pl[[1]] <- p + opts(aspect.ratio=3)
pl[[2]] <- p + facet_wrap(~f) + opts(aspect.ratio=3)
pl[[3]] <- p + facet_grid(.~f) + opts(aspect.ratio=3)
pl[[4]] <- p + facet_grid(f~.) + opts(aspect.ratio=3)
pl[[5]] <- p + facet_grid(f1~f2) + opts(aspect.ratio=3)
pl[[6]] <- p + opts(aspect.ratio=1/3)
pl[[7]] <- p + facet_wrap(~f) + opts(aspect.ratio=1/3)
pl[[8]] <- p + facet_grid(.~f) + opts(aspect.ratio=1/3)
pl[[9]] <- p + facet_grid(f~.) + opts(aspect.ratio=1/3)
pl[[10]] <- p + facet_grid(f1~f2) + opts(aspect.ratio=1/3)

l_ply(seq(pl), function(i) ggsave(sprintf("gtable_aspect_ratio_%02d.png", i), pl[[i]], width = 4, height = 4, dpi = 72))
