set.seed(1)

nCategory <- 5
nItem <- 6
df <- data.frame(category = rep(LETTERS[1:nCategory], 1, each = nItem),
                 item = paste("Item#", rep(1:nItem, nCategory, each = 1), sep = ''),
                 value = rep(1:nItem, nCategory, each = 1) + runif(nCategory * nItem) * 0.8)

ggplot(df) + geom_path(aes(x = value, y = category, group = item))
ggplot(df) + geom_path(aes(x = value, y = category, group = item, colour=item))

df2 <- df[c(1, 2, 7, 8, 13, 14, 3:6, 9:12, 15:nrow(df)), ]
ggplot(df2) + geom_path(aes(x = value, y = category, group = item))
ggplot(df2) + geom_path(aes(x = value, y = category, group = item, colour=item))
