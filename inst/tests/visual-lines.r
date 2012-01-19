dat <- data.frame(x=LETTERS[1:5], y=1:5)

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
