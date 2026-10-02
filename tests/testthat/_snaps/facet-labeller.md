# labeller() dispatches labellers

    Code
      ggplotGrob(p3)
    Condition
      Error in `resolve_labeller()`:
      ! Cannot supply both `rows` and `cols` to `facet_wrap()`.

---

    Code
      ggplotGrob(p5)
    Condition
      Error in `labeller()`:
      ! Conflict between `.cols` and `cyl`.

# resolve_labeller() provide meaningful errors

    Supply one of `rows` or `cols`.

---

    Cannot supply both `rows` and `cols` to `facet_wrap()`.

# labeller function catches overlap in names

    Conflict between `.rows` and `vs`.

