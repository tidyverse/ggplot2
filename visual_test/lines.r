dat <- data.frame(x=LETTERS[1:5], y=1:5)

# geom_abline tests

ggplot(dat, aes(x=x, y=y)) + geom_bar() +
  geom_abline(intercept = 2, slope = 0, colour = "red") +
  opts(title="geom_abline: intercept=2, slope=0")

ggplot(dat, aes(x=x, y=y)) + geom_bar() +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  opts(title="geom_abline: intercept=0, slope=1\nShould have same values as bars")

ggplot(dat, aes(x=x, y=y)) + geom_bar() +
  geom_abline(intercept = 2, slope = 0, colour = "red") +
  coord_flip() +
  opts(title="geom_abline, coord_flip: intercept=2, slope=0")

ggplot(dat, aes(x=x, y=y)) + geom_bar() +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  coord_flip() +
  opts(title="geom_abline, coord_flip: intercept=0, slope=1\nShould have same values as bars")


# geom_hline tests

ggplot(dat, aes(x=x, y=y)) + geom_bar() +
  geom_hline(yintercept = 2, colour = "red") +
  opts(title="geom_hline: intercept=2")

ggplot(dat, aes(x=x, y=y)) + geom_bar() +
  geom_hline(yintercept = 2, colour = "red") +
  coord_flip() +
  opts(title="geom_hline, coord_flip: intercept=2")

ggplot(dat, aes(x=x, y=y)) + geom_bar() +
  geom_hline(yintercept = 2, colour = "red") +
  coord_polar() +
  opts(title="geom_hline, coord_polar: intercept=2\nShould have a circle at r=2")


# geom_vline tests

ggplot(dat, aes(x=x, y=y)) + geom_bar() +
  geom_vline(xintercept = 2, colour = "red") +
  opts(title="geom_vline: intercept=2")

ggplot(dat, aes(x=x, y=y)) + geom_bar() +
  geom_vline(xintercept = 2, colour = "red") +
  coord_flip() +
  opts(title="geom_vline, coord_flip: intercept=2")

ggplot(dat, aes(x=x, y=y)) + geom_bar() +
  geom_vline(xintercept = 2, colour = "red") +
  coord_polar() +
  opts(title="geom_vline, coord_polar: intercept=2\nShould have a ray at 2")


# hline, vline, and abline tests with coord_map
library(maps)
library(mapproj)

nz <- data.frame(map("nz", plot=FALSE)[c("x","y")])
nzmap <- qplot(x, y, data=nz, geom="path")

nzmap + geom_hline(yintercept=-45) + coord_map() +
  opts(title="geom_hline: intercept=-45,\nprojection=mercator")
nzmap + geom_vline(xintercept=172) + coord_map() +
  opts(title="geom_vline: intercept=172,\nprojection=mercator")
nzmap + geom_abline(intercept=130, slope=-1) + coord_map() +
  opts(title="geom_abline: intercept=130, slope=-1\nprojection=mercator")

nzmap + geom_hline(yintercept=-45) + coord_map(project="cylindrical") +
  opts(title="geom_hline: intercept=-45,\nprojection=cylindrical")
nzmap + geom_vline(xintercept=172) + coord_map(project="cylindrical") +
  opts(title="geom_vline: intercept=172,\nprojection=cylindrical")
nzmap + geom_abline(intercept=130, slope=-1) + coord_map(project="cylindrical") +
  opts(title="geom_abline: intercept=130, slope=-1\nprojection=cylindrical")

nzmap + geom_hline(yintercept=-45) +
  coord_map(project='azequalarea',orientation=c(-36.92,174.6,0))  +
  opts(title="geom_hline: intercept=-45,\nprojection=azequalarea")
nzmap + geom_vline(xintercept=172) +
  coord_map(project='azequalarea',orientation=c(-36.92,174.6,0)) +
  opts(title="geom_vline: intercept=172,\nprojection=azequalara")
nzmap + geom_abline(intercept=130, slope=-1) +
  coord_map(project='azequalarea',orientation=c(-36.92,174.6,0)) +
  opts(title="geom_abline: intercept=130, slope=-1\nprojection=azequalarea")
