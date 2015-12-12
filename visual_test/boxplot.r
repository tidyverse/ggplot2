vcontext("boxplot")

ggplot(mtcars, aes(x = factor(cyl), y = drat, colour = factor(cyl))) +
    geom_boxplot(outlier.size = 5)
save_vtest("outlier colours")

end_vcontext()
