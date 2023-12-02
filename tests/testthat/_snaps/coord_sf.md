# axis labels can be set manually

    Code
      p <- plot + scale_x_continuous(breaks = c(1000, 2000, 3000), labels = function(
        ...) c("A", "B"))
      ggplot_build(p)
    Condition
      Error in `fixup_graticule_labels()`:
      ! `breaks` and `labels` along `x` direction have different lengths.
    Code
      p <- plot + scale_y_continuous(breaks = c(1000, 2000, 3000), labels = function(
        ...) c("A", "B"))
      ggplot_build(p)
    Condition
      Error in `fixup_graticule_labels()`:
      ! `breaks` and `labels` along `y` direction have different lengths.

---

    Code
      coord_sf(label_graticule = 1:17)
    Condition
      Error in `coord_sf()`:
      ! Graticule labeling format not recognized.
    Code
      coord_sf(label_axes = 1:17)
    Condition
      Error in `coord_sf()`:
      ! Panel labeling format not recognized.

# default crs works

    Code
      ggplot_build(p + xlim(-Inf, 80))
    Condition
      Error in `calc_limits_bbox()`:
      ! Scale limits cannot be mapped onto spatial coordinates in `coord_sf()`.
      i Consider setting `lims_method = "geometry_bbox"` or `default_crs = NULL`.

# coord_sf() throws error when limits are badly specified

    Code
      ggplot() + coord_sf(xlim(1, 1))
    Condition
      Error in `coord_sf()`:
      ! `xlim` must be a vector of length 2, not a <ScaleContinuousPosition/ScaleContinuous/Scale/ggproto/gg> object.

---

    Code
      ggplot() + coord_sf(ylim = 1:3)
    Condition
      Error in `coord_sf()`:
      ! `ylim` must be a vector of length 2, not an integer vector of length 3.

