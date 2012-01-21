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
