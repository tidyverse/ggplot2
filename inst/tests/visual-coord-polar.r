## test codes of polar coordinate

df <- data.frame(x = 0:1, y=rep(1:80, each=2))

ggplot(df, aes(x=x, y=y, group=factor(y))) +
  geom_line() +
  coord_polar()

ggplot(df, aes(x=x, y=y-80, group=factor(y))) +
  geom_line() +
  coord_polar()

ggplot(df, aes(x=x, y=y-40, group=factor(y))) +
  geom_line() +
  coord_polar()

ggplot(df, aes(x=x, y=y+100, group=factor(y))) +
  geom_line() +
  coord_polar()

ggplot(df, aes(x=x, y=y*100, group=factor(y))) +
  geom_line() +
  coord_polar()
