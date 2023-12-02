# flip coords throws error when limits are badly specified

    Code
      ggplot() + coord_flip(xlim(1, 1))
    Condition
      Error in `coord_flip()`:
      ! `xlim` must be a vector of length 2, not a <ScaleContinuousPosition/ScaleContinuous/Scale/ggproto/gg> object.

---

    Code
      ggplot() + coord_flip(ylim = 1:3)
    Condition
      Error in `coord_flip()`:
      ! `ylim` must be a vector of length 2, not an integer vector of length 3.

