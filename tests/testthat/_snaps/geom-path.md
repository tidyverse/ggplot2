# geom_path() throws meaningful error on bad combination of varying aesthetics

    Code
      ggplotGrob(p)
    Condition
      Error in `geom_path()`:
      ! Problem while converting geom to grob.
      i Error occurred in the 1st layer.
      Caused by error in `draw_panel()`:
      ! `geom_path()` can't have varying colour, linewidth, and/or alpha along the line when linetype isn't solid.

