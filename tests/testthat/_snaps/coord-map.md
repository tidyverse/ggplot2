# coord map throws error when limits are badly specified

    Code
      ggplot() + coord_map(xlim = xlim(1, 1))
    Condition
      Error in `coord_map()`:
      ! `xlim` must be a vector of length 2, not a <ScaleContinuousPosition/ScaleContinuous/Scale/ggproto/gg> object.

---

    Code
      ggplot() + coord_cartesian(ylim = 1:3)
    Condition
      Error in `coord_cartesian()`:
      ! `ylim` must be a vector of length 2, not an integer vector of length 3.

