## align the labels for facets

p <- qplot(hwy, reorder(model, hwy), data = mpg) + 
  facet_grid(manufacturer ~ ., scales = "free", space = "free") + 
  opts(strip.text.y = theme_text())

ggsave("guide_axis_label_01.png", p, width = 4, height = 8, dpi = 72)

p <- qplot(reorder(model, hwy), hwy, data = mpg) + 
  facet_grid(. ~ manufacturer, scales = "free", space = "free") + 
  opts(strip.text.x = theme_text(), axis.text.x = theme_text(angle = 90, hjust = 1))

ggsave("guide_axis_label_02.png", p, width = 8, height = 4, dpi = 72)
