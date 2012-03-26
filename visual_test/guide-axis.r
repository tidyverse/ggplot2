vcontext("guide-axis")

## align the labels for facets

qplot(hwy, reorder(model, hwy), data = mpg) +
  facet_grid(manufacturer ~ ., scales = "free", space = "free") +
  opts(strip.text.y = theme_text())
save_vtest("align facet labels, facets horizontal")

qplot(reorder(model, hwy), hwy, data = mpg) +
  facet_grid(. ~ manufacturer, scales = "free", space = "free") +
  opts(strip.text.x = theme_text(), axis.text.x = theme_text(angle = 90, hjust = 1))
save_vtest("align facet labels, facets vertical")

end_vcontext()
