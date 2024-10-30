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

    The `labeller` API has been updated. Labellers taking `variable` and `value` arguments are now deprecated.
    i See labellers documentation.

