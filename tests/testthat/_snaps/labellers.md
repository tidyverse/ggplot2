# resolve_labeller() provide meaningful errors

    Code
      resolve_labeller(NULL, NULL)
    Condition
      Error in `resolve_labeller()`:
      ! Supply one of `rows` or `cols`.

---

    Code
      resolve_labeller(prod, sum, structure(1:4, facet = "wrap"))
    Condition
      Error in `resolve_labeller()`:
      ! Cannot supply both `rows` and `cols` to `facet_wrap()`.

# labeller function catches overlap in names

    Code
      ggplotGrob(p)
    Condition
      Error in `labeller()`:
      ! Conflict between `.rows` and `vs`.

