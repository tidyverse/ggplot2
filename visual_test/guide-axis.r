vcontext("guide-axis")

## align the labels for facets

qplot(hwy, reorder(model, hwy), data = mpg) +
  facet_grid(manufacturer ~ ., scales = "free", space = "free") +
  theme(strip.text.y = element_text(angle=0))
save_vtest("align facet labels, facets horizontal")

qplot(reorder(model, hwy), hwy, data = mpg) +
  facet_grid(. ~ manufacturer, scales = "free", space = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
save_vtest("align facet labels, facets vertical")

end_vcontext()
