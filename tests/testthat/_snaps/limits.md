# limits() throw meaningful errors

    Code
      lims(1:2)
    Condition
      Error in `lims()`:
      ! All arguments must be named.

---

    Code
      lims(linewidth = 1)
    Condition
      Error in `lims()`:
      ! `linewidth` must be a two-element vector.

