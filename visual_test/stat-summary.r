vcontext("stat-summary")

ggplot(mtcars, aes(x = cyl, y = mpg, colour = factor(vs))) +
  geom_point() +
  stat_summary(fun.y = mean, geom = "line", size = 2)
save_vtest("summary with color and lines")

ggplot(mtcars, aes(x = cyl, y = mpg)) +
  geom_point() +
  stat_summary(
    fun.data = mean_cl_boot,
    colour = "red",
    geom = "crossbar",
    width = 0.2
  )
save_vtest("summary with crossbars, no grouping")

ggplot(mtcars, aes(x = cyl, y = mpg, group = cyl)) +
  geom_point() +
  stat_summary(
    fun.data = mean_cl_boot,
    colour = "red",
    geom = "crossbar",
    width = 0.2
  )
save_vtest("summary with crossbars, manual grouping")

end_vcontext()
