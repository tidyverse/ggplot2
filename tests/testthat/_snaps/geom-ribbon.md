# geom_ribbon() checks the aesthetics

    Code
      ggplotGrob(p)
    Condition
      Error in `geom_ribbon()`:
      ! Problem while setting up geom.
      i Error occurred in the 1st layer.
      Caused by error in `setup_data()`:
      ! Either xmin or xmax must be given as an aesthetic.

---

    Code
      ggplotGrob(p)
    Condition
      Error in `geom_ribbon()`:
      ! Problem while setting up geom.
      i Error occurred in the 1st layer.
      Caused by error in `setup_data()`:
      ! Either ymin or ymax must be given as an aesthetic.

---

    Code
      ggplotGrob(p)
    Condition
      Error in `geom_ribbon()`:
      ! Problem while converting geom to grob.
      i Error occurred in the 1st layer.
      Caused by error in `draw_group()`:
      ! Aesthetics can not vary along a ribbon.

---

    Code
      geom_ribbon(aes(year, ymin = level - 5, ymax = level + 5), outline.type = "test")
    Condition
      Error in `geom_ribbon()`:
      ! `outline.type` must be one of "both", "upper", "lower", or "full", not "test".

