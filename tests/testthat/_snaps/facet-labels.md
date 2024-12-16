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

# old school labellers still work

    The `labeller` argument of `facet_()` is deprecated as of ggplot2 2.0.0.
    i Modern labellers do not take `variable` and `value` arguments anymore.

