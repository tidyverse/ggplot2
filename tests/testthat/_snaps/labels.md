# plot.tag.position rejects invalid input

    Code
      ggplotGrob(p + theme(plot.tag.position = TRUE))
    Condition
      Error in `mapply()`:
      ! The `plot.tag.position` theme element must be a <character/numeric/integer> object.

---

    Code
      ggplotGrob(p + theme(plot.tag.position = "foobar"))
    Condition
      Error in `theme()`:
      ! `plot.tag.position` must be one of "topleft", "top", "topright", "left", "right", "bottomleft", "bottom", or "bottomright", not "foobar".

