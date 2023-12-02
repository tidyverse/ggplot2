# errors are correctly triggered

    Code
      ggplotGrob(p)
    Condition
      Error in `geom_sf()`:
      ! Problem while converting geom to grob.
      i Error occurred in the 1st layer.
      Caused by error in `draw_panel()`:
      ! `geom_sf()` can only be used with `coord_sf()`.

---

    Code
      geom_sf_label(position = "jitter", nudge_x = 0.5)
    Condition
      Error in `geom_sf_label()`:
      ! Both `position` and `nudge_x`/`nudge_y` are supplied.
      i Only use one approach to alter the position.

---

    Code
      geom_sf_text(position = "jitter", nudge_x = 0.5)
    Condition
      Error in `geom_sf_text()`:
      ! both `position` and `nudge_x`/`nudge_y` are supplied.
      i Only use one approach to alter the position.

---

    Code
      sf_grob(pts, na.rm = FALSE)
    Condition
      Warning:
      Removed 1 row containing missing values or values outside the scale range (`geom_sf()`).
    Output
      polyline[GRID.polyline.8434] 

