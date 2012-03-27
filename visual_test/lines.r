vcontext("lines")

dat <- data.frame(x=LETTERS[1:5], y=1:5)

# geom_abline tests

ggplot(dat, aes(x=x, y=y)) + geom_bar() +
  geom_abline(intercept = 2, slope = 0, colour = "red")
save_vtest("geom_abline: intercept=2, slope=0")

ggplot(dat, aes(x=x, y=y)) + geom_bar() +
  geom_abline(intercept = 0, slope = 1, colour = "red")
save_vtest("geom_abline: intercept=0, slope=1 Should have same values as bars")

ggplot(dat, aes(x=x, y=y)) + geom_bar() +
  geom_abline(intercept = 2, slope = 0, colour = "red") +
  coord_flip()
save_vtest("geom_abline, coord_flip: intercept=2, slope=0")

ggplot(dat, aes(x=x, y=y)) + geom_bar() +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  coord_flip()
save_vtest("geom_abline, coord_flip: intercept=0, slope=1, should have same values as bars")


# geom_hline tests

ggplot(dat, aes(x=x, y=y)) + geom_bar() +
  geom_hline(yintercept = 2, colour = "red")
save_vtest("geom_hline: intercept=2")

ggplot(dat, aes(x=x, y=y)) + geom_bar() +
  geom_hline(yintercept = 2, colour = "red") +
  coord_flip()
save_vtest("geom_hline, coord_flip: intercept=2")

ggplot(dat, aes(x=x, y=y)) + geom_bar() +
  geom_hline(yintercept = 2, colour = "red") +
  coord_polar()
save_vtest("geom_hline, coord_polar: intercept=2, should have a circle at r=2")


# geom_vline tests

ggplot(dat, aes(x=x, y=y)) + geom_bar() +
  geom_vline(xintercept = 2, colour = "red")
save_vtest("geom_vline: intercept=2")

ggplot(dat, aes(x=x, y=y)) + geom_bar() +
  geom_vline(xintercept = 2, colour = "red") +
  coord_flip()
save_vtest("geom_vline, coord_flip: intercept=2")

ggplot(dat, aes(x=x, y=y)) + geom_bar() +
  geom_vline(xintercept = 2, colour = "red") +
  coord_polar()
save_vtest("geom_vline, coord_polar: intercept=2, should have a ray at 2")


# hline, vline, and abline tests with coord_map
library(maps)
library(mapproj)

nz <- data.frame(map("nz", plot=FALSE)[c("x","y")])
nzmap <- qplot(x, y, data=nz, geom="path")

nzmap + geom_hline(yintercept=-45) + coord_map()
save_vtest("geom_hline: intercept=-45, projection=mercator")

nzmap + geom_vline(xintercept=172) + coord_map()
save_vtest("geom_vline: intercept=172, projection=mercator")

nzmap + geom_abline(intercept=130, slope=-1) + coord_map()
save_vtest("geom_abline: intercept=130, slope=-1 projection=mercator")


nzmap + geom_hline(yintercept=-45) + coord_map(project="cylindrical")
save_vtest("geom_hline: intercept=-45, projection=cylindrical")

nzmap + geom_vline(xintercept=172) + coord_map(project="cylindrical")
save_vtest("geom_vline: intercept=172, projection=cylindrical")

nzmap + geom_abline(intercept=130, slope=-1) + coord_map(project="cylindrical")
save_vtest("geom_abline: intercept=130, slope=-1, projection=cylindrical")


nzmap + geom_hline(yintercept=-45) +
  coord_map(project='azequalarea', orientation=c(-36.92,174.6,0))
save_vtest("geom_hline: intercept=-45, projection=azequalarea")

nzmap + geom_vline(xintercept=172) +
  coord_map(project='azequalarea', orientation=c(-36.92,174.6,0))
save_vtest("geom_vline: intercept=172, projection=azequalara")

nzmap + geom_abline(intercept=130, slope=-1) +
  coord_map(project='azequalarea', orientation=c(-36.92,174.6,0))
save_vtest("geom_abline: intercept=130, slope=-1, projection=azequalarea")

end_vcontext()
