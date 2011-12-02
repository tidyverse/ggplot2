## align the labels for facets

p <- qplot(hwy, reorder(model, hwy), data = mpg) + 
  facet_grid(manufacturer ~ ., scales = "free", space = "free") + 
  opts(strip.text.y = theme_text())

p <- qplot(reorder(model, hwy), hwy, data = mpg) + 
  facet_grid(. ~ manufacturer, scales = "free", space = "free") + 
  opts(strip.text.x = theme_text(), axis.text.x = theme_text(angle = 90, hjust = 1))
