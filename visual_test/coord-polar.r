vcontext("coord-polar")

dat <- data.frame(x = 0:1, y=rep(1:80, each=2))

ggplot(dat, aes(x=x, y=y, group=factor(y))) + geom_line() + coord_polar()
save_vtest("Concentric circles at theta = 1:80")

ggplot(dat, aes(x=x, y=y-80, group=factor(y))) + geom_line() + coord_polar()
save_vtest("Concentric circles at theta = 1:80 - 80")

ggplot(dat, aes(x=x, y=y-40, group=factor(y))) + geom_line() + coord_polar()
save_vtest("Concentric circles at theta = 1:80 - 40")

ggplot(dat, aes(x=x, y=y+100, group=factor(y))) + geom_line() + coord_polar()
save_vtest("Concentric circles at theta = 1:80 + 100")

ggplot(dat, aes(x=x, y=y*100, group=factor(y))) + geom_line() + coord_polar()
save_vtest("Concentric circles at theta = 1:80 * 100")

end_vcontext()
