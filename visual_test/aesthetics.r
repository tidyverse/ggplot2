vcontext("Aesthetics")

dat <- data.frame(xvar = letters[1:3], yvar = 7:9)

ggplot(dat, aes(x = xvar, y = yvar)) + geom_bar(stat = "identity")
save_vtest("stat='identity'")

ggplot(dat, aes(x = xvar, y = yvar)) + geom_bar(stat = "identity", width = 0.5)
save_vtest("stat='identity', width=0.5")

ggplot(dat, aes(x = xvar, y = yvar)) + geom_bar(stat = "identity", aes(width = 0.5))
save_vtest("stat='identity', aes(width=0.5)")


ggplot(dat, aes(x = xvar)) + geom_bar(stat = "count")
save_vtest("stat='count'")

ggplot(dat, aes(x = xvar)) + geom_bar(stat = "count", width = 0.5)
save_vtest("stat='count', width=0.5")

ggplot(dat, aes(x = xvar)) + geom_bar(stat = "count", aes(width = 0.5))
save_vtest("stat='count', aes(width=0.5)")

#TODO: Clean these up a bit

end_vcontext()
