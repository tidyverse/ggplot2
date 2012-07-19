vcontext("guide-axis")

## align the labels for facets

qplot(hwy, reorder(model, hwy), data = mpg) +
  facet_grid(manufacturer ~ ., scales = "free", space = "free") +
  theme(strip.text.y = element_text(angle=0))
save_vtest("align facet labels, facets horizontal")

qplot(reorder(model, hwy), hwy, data = mpg) +
  facet_grid(. ~ manufacturer, scales = "free", space = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
save_vtest("align facet labels, facets vertical")

qplot(wt, mpg, data=mtcars) +
  theme(axis.line = element_line(size=5, lineend="square"))
save_vtest("thick axis lines")

end_vcontext()
