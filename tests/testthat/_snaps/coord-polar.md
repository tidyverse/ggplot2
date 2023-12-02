# coord_radial warns about axes

    Code
      out <- ggplotGrob(p + coord_radial() + guides(theta = "axis"))
    Condition
      Warning:
      `guide_axis()` cannot be used for theta.
      i Use one of x, y, or r instead.
    Code
      out <- ggplotGrob(p + coord_radial(start = 0.1 * pi, end = 0.4 * pi,
      r_axis_inside = FALSE))
    Condition
      Warning:
      No appropriate placement found for `r_axis_inside`.
      i Axis will be placed at panel edge.

