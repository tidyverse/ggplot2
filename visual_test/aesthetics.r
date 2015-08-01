vcontext("Aesthetics")

dat <- data.frame(xvar = letters[1:3], yvar = 7:9)

ggplot(dat, aes(x = xvar, y = yvar)) + geom_bar(stat = "identity")
save_vtest("stat='identity'")

ggplot(dat, aes(x = xvar, y = yvar)) + geom_bar(stat = "identity", width = 0.5)
save_vtest("stat='identity', width=0.5")

ggplot(dat, aes(x = xvar, y = yvar)) + geom_bar(stat = "identity", aes(width = 0.5))
save_vtest("stat='identity', aes(width=0.5)")


ggplot(dat, aes(x = xvar)) + geom_bar(stat = "bin")
save_vtest("stat='bin'")

ggplot(dat, aes(x = xvar)) + geom_bar(stat = "bin", width = 0.5)
save_vtest("stat='bin', width=0.5")

ggplot(dat, aes(x = xvar)) + geom_bar(stat = "bin", aes(width = 0.5))
save_vtest("stat='bin', aes(width=0.5)")

#TODO: Clean these up a bit

end_vcontext()
