vcontext("coord-map")

library(maps)

# World map
world_map <- map_data("world")
pworld <- ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon()


pworld
save_vtest("no projection")


pworld + coord_map(projection="mercator")
save_vtest("mercator projection")


pworld + coord_map(projection="ortho") +
  scale_y_continuous(breaks=(-2:2)*30) +
  scale_x_continuous(breaks=(-4:4)*45)
save_vtest("ortho projection, default orientation (centered on north pole)")

pworld + coord_map(projection="ortho", orientation=c(41,-74,0)) +
  scale_y_continuous(breaks=(-2:2)*30) +
  scale_x_continuous(breaks=(-4:4)*45)
save_vtest("ortho projection, custom orientation (centered on New York)")


# Need to set limits here so left-most longitude line shows up
pworld + coord_map(projection="aitoff") +
  scale_y_continuous(breaks=(-2:2)*30) +
  scale_x_continuous(breaks=(-4:4)*45, limits=c(-180, 180))
save_vtest("aitoff projection, default orientation")


# This drops half of the world, which probably isn't desirable.
# It might require rethinking about how limits work.
pworld + coord_map(projection="aitoff", orientation=c(90,180,0)) +
  scale_y_continuous(breaks=(-2:2)*30) +
  scale_x_continuous(breaks=(0:8)*45, limits=c(0, 360))
save_vtest("aitoff projection, custom orientation (centered on date line)")


# USA state map
states_map <- map_data("state")
pstate <- ggplot(states_map, aes(x=long, y=lat, group=group))

pstate +   geom_polygon() + coord_map("mercator")
save_vtest("USA map, mercator projection")


end_vcontext()
