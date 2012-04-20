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


dat <- data.frame(x=LETTERS[1:6], y=11:16)
ggplot(dat, aes(x=x, y=y)) + geom_bar() + coord_polar()
save_vtest("rose plot with has equal spacing")

ggplot(dat, aes(x=as.numeric(x), y=y)) + geom_point() + coord_polar()
save_vtest("continuous theta has merged low/high values")

ggplot(dat, aes(x=as.numeric(x), y=y)) + geom_point() + coord_polar() +
  xlim(0, 6) + ylim(0,16)
save_vtest("continuous theta with xlim(0, 6) and ylim(0, 16)")

ggplot(dat, aes(x=x, y=y)) + geom_bar() + coord_polar(theta = "y")
save_vtest("racetrack plot with expand=F is closed and doesn't have center hole")

ggplot(dat, aes(x=x, y=y)) + geom_bar() + coord_polar(theta = "y") +
  scale_x_discrete(expand = c(0, 0.6))
save_vtest("racetrack plot with expand=T is closed and has center hole")

end_vcontext()
