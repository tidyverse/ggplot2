vcontext("geom-polygon")

ggplot(faithful, aes(x = eruptions, y = waiting)) +
  stat_density_2d(aes(colour = ..level..), geom = "path") +
  xlim(0.5, 6) + ylim(40, 110)
save_vtest("stat_density2d with paths")

ggplot(faithful, aes(x = eruptions, y = waiting)) +
  stat_density2d(aes(fill = ..level..), geom = "polygon", colour = "white") +
  xlim(0.5, 6) + ylim(40, 110)
save_vtest("stat_density2d with filled polygons")

end_vcontext()
