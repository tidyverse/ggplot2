# Changelog

## ggplot2 (development version)

## ggplot2 4.0.1

CRAN release: 2025-11-14

This is a smaller patch release focussed on fixing regressions from
4.0.0 and polishing the recent features.

### Bug fixes

- Fixed regression where
  [`geom_area()`](https://ggplot2.tidyverse.org/dev/reference/geom_ribbon.md)
  didn’t draw panels with single groups when `stat = "align"`
  ([@teunbrand](https://github.com/teunbrand),
  [\#6680](https://github.com/tidyverse/ggplot2/issues/6680))
- Fixed regression where `position_stack(vjust)` was ignored when there
  are only single groups
  ([\#6692](https://github.com/tidyverse/ggplot2/issues/6692))
- Fixed bug where `NA` handling in
  [`geom_path()`](https://ggplot2.tidyverse.org/dev/reference/geom_path.md)
  was ignoring panels ([@teunbrand](https://github.com/teunbrand),
  [\#6533](https://github.com/tidyverse/ggplot2/issues/6533))
- Fixed bug where `stat_bin(boundary)` was ignored
  ([\#6682](https://github.com/tidyverse/ggplot2/issues/6682)).
- [`geom_text()`](https://ggplot2.tidyverse.org/dev/reference/geom_text.md)
  and
  [`geom_label()`](https://ggplot2.tidyverse.org/dev/reference/geom_text.md)
  accept expressions as the `label` aesthetic
  ([@teunbrand](https://github.com/teunbrand),
  [\#6638](https://github.com/tidyverse/ggplot2/issues/6638))
- Fixed regression where
  [`draw_key_rect()`](https://ggplot2.tidyverse.org/dev/reference/draw_key.md)
  stopped using `fill` colours
  ([@mitchelloharawild](https://github.com/mitchelloharawild),
  [\#6609](https://github.com/tidyverse/ggplot2/issues/6609)).
- Fixed regression where `scale_{x,y}_*()` threw an error when an
  expression object is set to `labels` argument
  ([@yutannihilation](https://github.com/yutannihilation),
  [\#6617](https://github.com/tidyverse/ggplot2/issues/6617)).
- Fixed regression where the first (unnamed) argument to colour/fill
  scales was not passed as the `name` argument
  ([@teunbrand](https://github.com/teunbrand),
  [\#6623](https://github.com/tidyverse/ggplot2/issues/6623))
- Fixed issue where vectorised
  [`arrow()`](https://rdrr.io/r/grid/arrow.html)s caused errors in
  drawing the legend glyphs ([@teunbrand](https://github.com/teunbrand),
  [\#6594](https://github.com/tidyverse/ggplot2/issues/6594))
- Fixed regression where `NULL`-aesthetics contributed to plot labels
  too insistently. Now they contribute only as fallback labels
  ([@teunbrand](https://github.com/teunbrand),
  [\#6616](https://github.com/tidyverse/ggplot2/issues/6616))
- Fixed regression where empty arguments to colour/fill scale caused
  errors ([@jmbarbone](https://github.com/jmbarbone),
  [\#6710](https://github.com/tidyverse/ggplot2/issues/6710))
- Fixed axis misplacement in `coor_radial()` when labels are blank
  ([@teunbrand](https://github.com/teunbrand),
  [\#6574](https://github.com/tidyverse/ggplot2/issues/6574))

### Improvements

- Improved palette fallback mechanism in scales
  ([@teunbrand](https://github.com/teunbrand),
  [\#6669](https://github.com/tidyverse/ggplot2/issues/6669)).
- Allow `stat` in `geom_hline`, `geom_vline`, and `geom_abline`.
  ([@sierrajohnson](https://github.com/sierrajohnson),
  [\#6559](https://github.com/tidyverse/ggplot2/issues/6559))
- [`stat_boxplot()`](https://ggplot2.tidyverse.org/dev/reference/geom_boxplot.md)
  treats `width` as an optional aesthetic
  ([@Yunuuuu](https://github.com/Yunuuuu),
  [\#6575](https://github.com/tidyverse/ggplot2/issues/6575))
- The `theme(panel.widths, panel.heights)` setting attempts to preserve
  the plot’s aspect ratio when only one of the two settings is given,
  and the plot has a single panel
  ([@teunbrand](https://github.com/teunbrand),
  [\#6701](https://github.com/tidyverse/ggplot2/issues/6701)).
- Logical values for the linetype aesthetic will be interpreted
  numerically, so that `linetype = FALSE` becomes 0/‘blank’ and
  `linetype = TRUE` becomes 1/‘solid’
  ([@teunbrand](https://github.com/teunbrand),
  [\#6641](https://github.com/tidyverse/ggplot2/issues/6641))
- Out-of-bounds datapoints used as padding by
  [`stat_align()`](https://ggplot2.tidyverse.org/dev/reference/geom_ribbon.md)
  now get removed silently rather than verbosely
  ([@teunbrand](https://github.com/teunbrand),
  [\#6667](https://github.com/tidyverse/ggplot2/issues/6667))

## ggplot2 4.0.0

CRAN release: 2025-09-11

### User facing

#### Breaking changes

- The S3 parts of ggplot2 have been replaced with S7 bits
  ([\#6352](https://github.com/tidyverse/ggplot2/issues/6352)).
- (breaking) `geom_violin(quantiles)` now has actual quantiles based on
  the data, rather than inferred quantiles based on the computed
  density. The `quantiles` parameter that replaces `draw_quantiles` now
  belongs to
  [`stat_ydensity()`](https://ggplot2.tidyverse.org/dev/reference/geom_violin.md)
  instead of
  [`geom_violin()`](https://ggplot2.tidyverse.org/dev/reference/geom_violin.md)
  ([@teunbrand](https://github.com/teunbrand),
  [\#4120](https://github.com/tidyverse/ggplot2/issues/4120)).
- (Breaking) The defaults for all geoms can be set at one in the theme.
  ([@teunbrand](https://github.com/teunbrand) based on pioneering work
  by [@dpseidel](https://github.com/dpseidel),
  [\#2239](https://github.com/tidyverse/ggplot2/issues/2239))
  - A new `theme(geom)` argument is used to track these defaults.
  - The
    [`element_geom()`](https://ggplot2.tidyverse.org/dev/reference/element.md)
    function can be used to populate that argument.
  - The
    [`from_theme()`](https://ggplot2.tidyverse.org/dev/reference/aes_eval.md)
    function allows access to the theme default fields from inside the
    [`aes()`](https://ggplot2.tidyverse.org/dev/reference/aes.md)
    function.
- Moved the following packages in the description. If your package
  depended on ggplot2 to install these dependencies, you may need to
  list these in your own DESCRIPTION file now
  ([\#5986](https://github.com/tidyverse/ggplot2/issues/5986)).
  - Moved mgcv from Imports to Suggests
  - Moved tibble from Imports to Suggests
  - Removed glue dependency
- Default labels are derived in `build_ggplot()` (previously
  [`ggplot_build()`](https://ggplot2.tidyverse.org/dev/reference/ggplot_build.md))
  rather than in the layer method of
  [`update_ggplot()`](https://ggplot2.tidyverse.org/dev/reference/update_ggplot.md)
  (previously `ggplot_add.Layer()`). This may affect code that accessed
  the `plot$labels` property
  ([@teunbrand](https://github.com/teunbrand),
  [\#5894](https://github.com/tidyverse/ggplot2/issues/5894)).
- In binning stats, the default `boundary` is now chosen to better
  adhere to the `nbin` argument. This may affect plots that use default
  binning ([@teunbrand](https://github.com/teunbrand),
  [\#5882](https://github.com/tidyverse/ggplot2/issues/5882),
  [\#5036](https://github.com/tidyverse/ggplot2/issues/5036))

#### Lifecycle changes

- Deprecated functions and arguments prior to ggplot2 3.0.0 throw errors
  instead of warnings.

- Functions and arguments that were soft-deprecated up to ggplot2 3.4.0
  now throw warnings.

- [`annotation_borders()`](https://ggplot2.tidyverse.org/dev/reference/annotation_borders.md)
  replaces the now-deprecated
  [`borders()`](https://ggplot2.tidyverse.org/dev/reference/annotation_borders.md)
  ([@teunbrand](https://github.com/teunbrand),
  [\#6392](https://github.com/tidyverse/ggplot2/issues/6392))

- Turned off fallback for `size` to `linewidth` translation in
  [`geom_bar()`](https://ggplot2.tidyverse.org/dev/reference/geom_bar.md)/[`geom_col()`](https://ggplot2.tidyverse.org/dev/reference/geom_bar.md)
  ([\#4848](https://github.com/tidyverse/ggplot2/issues/4848)).

- The `fatten` argument has been deprecated in
  [`geom_boxplot()`](https://ggplot2.tidyverse.org/dev/reference/geom_boxplot.md),
  [`geom_crossbar()`](https://ggplot2.tidyverse.org/dev/reference/geom_linerange.md)
  and
  [`geom_pointrange()`](https://ggplot2.tidyverse.org/dev/reference/geom_linerange.md)
  ([@teunbrand](https://github.com/teunbrand),
  [\#4881](https://github.com/tidyverse/ggplot2/issues/4881)).

- The following methods have been deprecated:
  [`fortify.lm()`](https://ggplot2.tidyverse.org/dev/reference/fortify.lm.md),
  [`fortify.glht()`](https://ggplot2.tidyverse.org/dev/reference/fortify-multcomp.md),
  [`fortify.confint.glht()`](https://ggplot2.tidyverse.org/dev/reference/fortify-multcomp.md),
  [`fortify.summary.glht()`](https://ggplot2.tidyverse.org/dev/reference/fortify-multcomp.md)
  and
  [`fortify.cld()`](https://ggplot2.tidyverse.org/dev/reference/fortify-multcomp.md).
  It is recommend to use
  [`broom::augment()`](https://generics.r-lib.org/reference/augment.html)
  and [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
  instead ([@teunbrand](https://github.com/teunbrand),
  [\#3816](https://github.com/tidyverse/ggplot2/issues/3816)).

- [`geom_errorbarh()`](https://ggplot2.tidyverse.org/dev/reference/geom_linerange.md)
  is deprecated in favour of `geom_errorbar(orientation = "y")`
  ([@teunbrand](https://github.com/teunbrand),
  [\#5961](https://github.com/tidyverse/ggplot2/issues/5961)).

- Special getter and setter functions have been renamed for consistency,
  allowing for better tab-completion with `get_*`- and `set_*`-prefixes.
  The old names remain available for backward compatibility
  ([@teunbrand](https://github.com/teunbrand),
  [\#5568](https://github.com/tidyverse/ggplot2/issues/5568)).

  | New name                                                                            | Old name                                                                        |
  |-------------------------------------------------------------------------------------|---------------------------------------------------------------------------------|
  | [`get_theme()`](https://ggplot2.tidyverse.org/dev/reference/get_theme.md)           | [`theme_get()`](https://ggplot2.tidyverse.org/dev/reference/get_theme.md)       |
  | [`set_theme()`](https://ggplot2.tidyverse.org/dev/reference/get_theme.md)           | [`theme_set()`](https://ggplot2.tidyverse.org/dev/reference/get_theme.md)       |
  | [`replace_theme()`](https://ggplot2.tidyverse.org/dev/reference/get_theme.md)       | [`theme_replace()`](https://ggplot2.tidyverse.org/dev/reference/get_theme.md)   |
  | [`update_theme()`](https://ggplot2.tidyverse.org/dev/reference/get_theme.md)        | [`theme_update()`](https://ggplot2.tidyverse.org/dev/reference/get_theme.md)    |
  | [`get_last_plot()`](https://ggplot2.tidyverse.org/dev/reference/get_last_plot.md)   | [`last_plot()`](https://ggplot2.tidyverse.org/dev/reference/get_last_plot.md)   |
  | [`get_layer_data()`](https://ggplot2.tidyverse.org/dev/reference/ggplot_build.md)   | [`layer_data()`](https://ggplot2.tidyverse.org/dev/reference/ggplot_build.md)   |
  | [`get_layer_grob()`](https://ggplot2.tidyverse.org/dev/reference/ggplot_build.md)   | [`layer_grob()`](https://ggplot2.tidyverse.org/dev/reference/ggplot_build.md)   |
  | [`get_panel_scales()`](https://ggplot2.tidyverse.org/dev/reference/ggplot_build.md) | [`layer_scales()`](https://ggplot2.tidyverse.org/dev/reference/ggplot_build.md) |

- [`facet_wrap()`](https://ggplot2.tidyverse.org/dev/reference/facet_wrap.md)
  has new options for the `dir` argument for additional control over
  panel directions. They absorb interactions with the now-deprecated
  `as.table` argument. Internally `dir = "h"` or `dir = "v"` is
  deprecated ([@teunbrand](https://github.com/teunbrand),
  [\#5212](https://github.com/tidyverse/ggplot2/issues/5212)).

- [`coord_trans()`](https://ggplot2.tidyverse.org/dev/reference/coord_transform.md)
  was renamed to
  [`coord_transform()`](https://ggplot2.tidyverse.org/dev/reference/coord_transform.md)
  ([@nmercadeb](https://github.com/nmercadeb),
  [\#5825](https://github.com/tidyverse/ggplot2/issues/5825)).

#### Improvements

##### Themes

- The [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md)
  function offers new arguments:
  - `geom` to set defaults for layer aesthetics
    ([\#2239](https://github.com/tidyverse/ggplot2/issues/2239)).
  - `spacing`/`margins` as root elements that are inherited by all other
    spacings and (non-text) margins
    ([@teunbrand](https://github.com/teunbrand),
    [\#5622](https://github.com/tidyverse/ggplot2/issues/5622)).
  - `palette.{aes}.discrete` and `palette.{aes}.continuous` which
    determine the palettes used when scales have `palette = NULL`. This
    is the new default for generic scales like
    [`scale_colour_discrete()`](https://ggplot2.tidyverse.org/dev/reference/scale_colour_discrete.md)
    or
    [`scale_fill_continuous()`](https://ggplot2.tidyverse.org/dev/reference/scale_colour_continuous.md),
    see also the ‘Scales’ section
    ([\#4696](https://github.com/tidyverse/ggplot2/issues/4696)).
  - `panel.widths` and `panel.heights` to control the (absolute) size of
    the panels
    ([\#5338](https://github.com/tidyverse/ggplot2/issues/5338),
    [@teunbrand](https://github.com/teunbrand)).
  - `legend.key.justification` to control the alignment of legend keys
    ([@teunbrand](https://github.com/teunbrand),
    [\#3669](https://github.com/tidyverse/ggplot2/issues/3669))
- Built-in `theme_*()` functions have new arguments:
  - `ink`/`paper`/`accent` to control foreground, background and
    highlight colours respectively of the whole plot
    ([@teunbrand](https://github.com/teunbrand),
    [\#6063](https://github.com/tidyverse/ggplot2/issues/6063),
    [@EvaMaeRey](https://github.com/EvaMaeRey),
    [\#6438](https://github.com/tidyverse/ggplot2/issues/6438)).
  - `header_family` to easily set the font for headers and titles
    ([\#5886](https://github.com/tidyverse/ggplot2/issues/5886))
    - To accommodate, `plot.subtitle`, `plot.caption` and `plot.tag` now
      inherit from the root `text` element instead of the `title`
      element.
- New function family for setting parts of a theme. For example, you can
  now use `theme_sub_axis(line, text, ticks, ticks.length, line)` as a
  substitute for
  `theme(axis.line, axis.text, axis.ticks, axis.ticks.length, axis.line)`.
  This should allow slightly terser and more organised theme
  declarations ([@teunbrand](https://github.com/teunbrand),
  [\#5301](https://github.com/tidyverse/ggplot2/issues/5301)).
- Adjustments to margins
  ([\#6115](https://github.com/tidyverse/ggplot2/issues/6115)):
  - They can have NA-units, which indicate that the value should be
    inherited from the parent element.
  - New
    [`margin_part()`](https://ggplot2.tidyverse.org/dev/reference/element.md)
    function that comes pre-populated with NA-units, so you can change a
    single margin without worrying that the others look off.
  - New
    [`margin_auto()`](https://ggplot2.tidyverse.org/dev/reference/element.md)
    that recycles arguments in a CSS like fashion.
- The `fill` of the `panel.border` theme setting is ignored and forced
  to be transparent
  ([\#5782](https://github.com/tidyverse/ggplot2/issues/5782)).
- [`theme_classic()`](https://ggplot2.tidyverse.org/dev/reference/ggtheme.md)
  has the following changes ([@teunbrand](https://github.com/teunbrand),
  [\#5978](https://github.com/tidyverse/ggplot2/issues/5978) &
  [\#6320](https://github.com/tidyverse/ggplot2/issues/6320)):
  - Axis ticks are now black (`ink`-coloured) instead of dark gray.
  - Axis line ends are now `"square"`.
  - The panel grid is now blank at the `panel.grid` hierarchy level
    instead of the `panel.grid.major` and `panel.grid.minor` levels.
- The `theme(legend.spacing.{x/y})` setting now accepts `null`-units
  ([@teunbrand](https://github.com/teunbrand),
  [\#6417](https://github.com/tidyverse/ggplot2/issues/6417)).

##### Scales

- The default colour and fill scales have a new `palette` argument. The
  default, `palette = NULL` will retrieve palettes from the theme (see
  the Themes section). This replaces the old options-based `type`
  system, with some limited backward compatibility
  ([@teunbrand](https://github.com/teunbrand),
  [\#6064](https://github.com/tidyverse/ggplot2/issues/6064)).
- All scales now expose the `aesthetics` parameter
  ([@teunbrand](https://github.com/teunbrand),
  [\#5841](https://github.com/tidyverse/ggplot2/issues/5841))
- All position scales now use the same definition of `x` and `y`
  aesthetics. This lets uncommon aesthetics like `xintercept` expand
  scales as usual.
  ([\#3342](https://github.com/tidyverse/ggplot2/issues/3342),
  [\#4966](https://github.com/tidyverse/ggplot2/issues/4966),
  [@teunbrand](https://github.com/teunbrand))
- In continuous scales, when `breaks` is a function and `n.breaks` is
  set, the `n.breaks` will be passed to the `breaks` function.
  Previously, `n.breaks` only applied to the default break calculation
  ([@teunbrand](https://github.com/teunbrand),
  [\#5972](https://github.com/tidyverse/ggplot2/issues/5972)).
- Changes in discrete scales:
  - Added `palette` argument, which can be used to customise spacings
    between levels ([@teunbrand](https://github.com/teunbrand),
    [\#5770](https://github.com/tidyverse/ggplot2/issues/5770))
  - Added `continuous.limits` argument to control the display range
    ([@teunbrand](https://github.com/teunbrand),
    [\#4174](https://github.com/tidyverse/ggplot2/issues/4174),
    [\#6259](https://github.com/tidyverse/ggplot2/issues/6259)).
  - Added `minor_breaks` argument. This only makes sense in position
    scales, where it affects the placement of minor ticks and minor
    gridlines
    ([\#5434](https://github.com/tidyverse/ggplot2/issues/5434)).
  - Added `sec.axis` argument. Discrete scales don’t support
    transformations so it is recommended to use
    [`dup_axis()`](https://ggplot2.tidyverse.org/dev/reference/sec_axis.md)
    to set custom breaks or labels. Secondary discrete axes work with
    the continuous analogues of discrete breaks
    ([@teunbrand](https://github.com/teunbrand),
    [\#3171](https://github.com/tidyverse/ggplot2/issues/3171))
  - When `breaks` yields a named vector, the names will be used as
    `labels` by default ([@teunbrand](https://github.com/teunbrand),
    [\#6147](https://github.com/tidyverse/ggplot2/issues/6147)).
- Changes in date/time scales:
  - is silently cast to in date scales. Vice versa, is cast to in
    datetime scales ([@laurabrianna](https://github.com/laurabrianna),
    [\#3533](https://github.com/tidyverse/ggplot2/issues/3533))
  - Bare numeric provided to date or datetime scales get inversely
    transformed (i.e. cast to /) with a warning
    ([@teunbrand](https://github.com/teunbrand))
  - The `date_breaks`, `date_minor_breaks` and `date_labels` arguments
    have been copied over to `scale_{x/y}_time()`
    ([@teunbrand](https://github.com/teunbrand),
    [\#4335](https://github.com/tidyverse/ggplot2/issues/4335)).
- More stability for vctrs-based palettes
  ([@teunbrand](https://github.com/teunbrand),
  [\#6117](https://github.com/tidyverse/ggplot2/issues/6117)).
- Scale names, guide titles and aesthetic labels can now accept
  functions ([@teunbrand](https://github.com/teunbrand),
  [\#4313](https://github.com/tidyverse/ggplot2/issues/4313))

##### Coords

- Reversal of a dimension, typically ‘x’ or ‘y’, is now controlled by
  the `reverse` argument in
  [`coord_cartesian()`](https://ggplot2.tidyverse.org/dev/reference/coord_cartesian.md),
  [`coord_fixed()`](https://ggplot2.tidyverse.org/dev/reference/coord_fixed.md),
  [`coord_radial()`](https://ggplot2.tidyverse.org/dev/reference/coord_radial.md)
  and
  [`coord_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md).
  In
  [`coord_radial()`](https://ggplot2.tidyverse.org/dev/reference/coord_radial.md),
  this replaces the older `direction` argument
  ([\#4021](https://github.com/tidyverse/ggplot2/issues/4021),
  [@teunbrand](https://github.com/teunbrand)).
- `coord_*(expand)` can now take a logical vector to control expansion
  at any side of the panel (top, right, bottom, left)
  ([@teunbrand](https://github.com/teunbrand),
  [\#6020](https://github.com/tidyverse/ggplot2/issues/6020))
- New `coord_cartesian(ratio)` argument that absorbs the aspect ratio
  functionality from
  [`coord_equal()`](https://ggplot2.tidyverse.org/dev/reference/coord_fixed.md)
  and
  [`coord_fixed()`](https://ggplot2.tidyverse.org/dev/reference/coord_fixed.md),
  which are now wrappers for
  [`coord_cartesian()`](https://ggplot2.tidyverse.org/dev/reference/coord_cartesian.md).
- In non-orthogonal coordinate systems
  ([`coord_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md),
  [`coord_polar()`](https://ggplot2.tidyverse.org/dev/reference/coord_radial.md)
  and
  [`coord_radial()`](https://ggplot2.tidyverse.org/dev/reference/coord_radial.md)),
  using ‘AsIs’ variables escape transformation when both `x` and `y` is
  an ‘AsIs’ variable ([@teunbrand](https://github.com/teunbrand),
  [\#6205](https://github.com/tidyverse/ggplot2/issues/6205)).
- Axis labels are now preserved better when using
  `coord_sf(expand = TRUE)` and graticule lines are straight but do not
  meet the edge ([@teunbrand](https://github.com/teunbrand),
  [\#2985](https://github.com/tidyverse/ggplot2/issues/2985)).
- `coord_radial(clip = "on")` clips to the panel area when the graphics
  device supports clipping paths
  ([@teunbrand](https://github.com/teunbrand),
  [\#5952](https://github.com/tidyverse/ggplot2/issues/5952)).
- `coord_radial(r.axis.inside)` can now take a numeric value to control
  placement of internally placed radius axes
  ([@teunbrand](https://github.com/teunbrand),
  [\#5805](https://github.com/tidyverse/ggplot2/issues/5805)).
- Munching in
  [`coord_polar()`](https://ggplot2.tidyverse.org/dev/reference/coord_radial.md)
  and
  [`coord_radial()`](https://ggplot2.tidyverse.org/dev/reference/coord_radial.md)
  now adds more detail, particularly for data-points with a low radius
  near the center ([@teunbrand](https://github.com/teunbrand),
  [\#5023](https://github.com/tidyverse/ggplot2/issues/5023)).

##### Layers

- Position adjustments can now have auxiliary aesthetics
  ([@teunbrand](https://github.com/teunbrand)).
  - [`position_nudge()`](https://ggplot2.tidyverse.org/dev/reference/position_nudge.md)
    gains `nudge_x` and `nudge_y` aesthetics
    ([\#3026](https://github.com/tidyverse/ggplot2/issues/3026),
    [\#5445](https://github.com/tidyverse/ggplot2/issues/5445)).
  - [`position_dodge()`](https://ggplot2.tidyverse.org/dev/reference/position_dodge.md)
    gains `order` aesthetic
    ([\#3022](https://github.com/tidyverse/ggplot2/issues/3022),
    [\#3345](https://github.com/tidyverse/ggplot2/issues/3345))
- New
  [`stat_connect()`](https://ggplot2.tidyverse.org/dev/reference/stat_connect.md)
  to connect points via steps or other shapes
  ([@teunbrand](https://github.com/teunbrand),
  [\#6228](https://github.com/tidyverse/ggplot2/issues/6228))
- New stat:
  [`stat_manual()`](https://ggplot2.tidyverse.org/dev/reference/stat_manual.md)
  for arbitrary computations
  ([@teunbrand](https://github.com/teunbrand),
  [\#3501](https://github.com/tidyverse/ggplot2/issues/3501))
- [`geom_boxplot()`](https://ggplot2.tidyverse.org/dev/reference/geom_boxplot.md)
  gains additional arguments to style the colour, linetype and
  linewidths of the box, whiskers, median line and staples
  ([@teunbrand](https://github.com/teunbrand),
  [\#5126](https://github.com/tidyverse/ggplot2/issues/5126)).
- [`geom_violin()`](https://ggplot2.tidyverse.org/dev/reference/geom_violin.md)
  gains additional arguments to style the colour, linetype and
  linewidths of the quantiles, which replace the now-deprecated
  `draw_quantiles` argument
  ([\#5912](https://github.com/tidyverse/ggplot2/issues/5912)).
- New parameters for
  [`geom_label()`](https://ggplot2.tidyverse.org/dev/reference/geom_text.md)
  ([@teunbrand](https://github.com/teunbrand) and
  [@steveharoz](https://github.com/steveharoz),
  [\#5365](https://github.com/tidyverse/ggplot2/issues/5365)):
  - The `linewidth` aesthetic is now applied and replaces the
    `label.size` argument.
  - The `linetype` aesthetic is now applied.
  - New `border.colour` argument to set the colour of borders.
  - New `text.colour` argument to set the colour of text.
- New `layer(layout)` argument to interact with facets
  ([@teunbrand](https://github.com/teunbrand),
  [\#3062](https://github.com/tidyverse/ggplot2/issues/3062))
- New default `geom_qq_line(geom = "abline")` for better clipping in the
  vertical direction. In addition, `slope` and `intercept` are new
  computed variables in
  [`stat_qq_line()`](https://ggplot2.tidyverse.org/dev/reference/geom_qq.md)
  ([@teunbrand](https://github.com/teunbrand),
  [\#6087](https://github.com/tidyverse/ggplot2/issues/6087)).
- [`stat_ecdf()`](https://ggplot2.tidyverse.org/dev/reference/stat_ecdf.md)
  now has an optional `weight` aesthetic
  ([@teunbrand](https://github.com/teunbrand),
  [\#5058](https://github.com/tidyverse/ggplot2/issues/5058)).
- `stat_ellipse` now has an optional `weight`
  ([@teunbrand](https://github.com/teunbrand),
  [\#5272](https://github.com/tidyverse/ggplot2/issues/5272))
- [`stat_density()`](https://ggplot2.tidyverse.org/dev/reference/geom_density.md)
  has the new computed variable: `wdensity`, which is calculated as the
  density times the sum of weights
  ([@teunbrand](https://github.com/teunbrand),
  [\#4176](https://github.com/tidyverse/ggplot2/issues/4176)).
  - `linetype = NA` is now interpreted to mean ‘no line’ instead of
    raising errors ([@teunbrand](https://github.com/teunbrand),
    [\#6269](https://github.com/tidyverse/ggplot2/issues/6269)).
- [`position_dodge()`](https://ggplot2.tidyverse.org/dev/reference/position_dodge.md)
  and
  [`position_jitterdodge()`](https://ggplot2.tidyverse.org/dev/reference/position_jitterdodge.md)
  now have a `reverse` argument
  ([@teunbrand](https://github.com/teunbrand),
  [\#3610](https://github.com/tidyverse/ggplot2/issues/3610))
- [`position_jitterdodge()`](https://ggplot2.tidyverse.org/dev/reference/position_jitterdodge.md)
  now dodges by `group` ([@teunbrand](https://github.com/teunbrand),
  [\#3656](https://github.com/tidyverse/ggplot2/issues/3656))
- [`geom_rect()`](https://ggplot2.tidyverse.org/dev/reference/geom_tile.md)
  can now derive the required corners positions from `x`/`width` or
  `y`/`height` parameterisation
  ([@teunbrand](https://github.com/teunbrand),
  [\#5861](https://github.com/tidyverse/ggplot2/issues/5861)).
- `position_dodge(preserve = "single")` now handles multi-row geoms
  better, such as
  [`geom_violin()`](https://ggplot2.tidyverse.org/dev/reference/geom_violin.md)
  ([@teunbrand](https://github.com/teunbrand) based on
  [@clauswilke](https://github.com/clauswilke)’s work,
  [\#2801](https://github.com/tidyverse/ggplot2/issues/2801)).
- [`geom_point()`](https://ggplot2.tidyverse.org/dev/reference/geom_point.md)
  can be dodged vertically by using
  `position_dodge(..., orientation = "y")`
  ([@teunbrand](https://github.com/teunbrand),
  [\#5809](https://github.com/tidyverse/ggplot2/issues/5809)).
- The `arrow.fill` parameter is now applied to more line-based
  functions:
  [`geom_path()`](https://ggplot2.tidyverse.org/dev/reference/geom_path.md),
  [`geom_line()`](https://ggplot2.tidyverse.org/dev/reference/geom_path.md),
  [`geom_step()`](https://ggplot2.tidyverse.org/dev/reference/geom_path.md)
  [`geom_function()`](https://ggplot2.tidyverse.org/dev/reference/geom_function.md),
  line geometries in
  [`geom_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md) and
  [`element_line()`](https://ggplot2.tidyverse.org/dev/reference/element.md).
- [`geom_raster()`](https://ggplot2.tidyverse.org/dev/reference/geom_tile.md)
  now falls back to rendering as
  [`geom_rect()`](https://ggplot2.tidyverse.org/dev/reference/geom_tile.md)
  when coordinates are not linear
  ([\#5503](https://github.com/tidyverse/ggplot2/issues/5503)).
- [`geom_ribbon()`](https://ggplot2.tidyverse.org/dev/reference/geom_ribbon.md)
  can have varying `fill` or `alpha` in linear coordinate systems
  ([@teunbrand](https://github.com/teunbrand),
  [\#4690](https://github.com/tidyverse/ggplot2/issues/4690)).
- Standardised the calculation of `width`, which are now implemented as
  aesthetics ([@teunbrand](https://github.com/teunbrand),
  [\#2800](https://github.com/tidyverse/ggplot2/issues/2800),
  [\#3142](https://github.com/tidyverse/ggplot2/issues/3142),
  [\#5740](https://github.com/tidyverse/ggplot2/issues/5740),
  [\#3722](https://github.com/tidyverse/ggplot2/issues/3722)).
- All binning stats now use the `boundary`/`center` parametrisation
  rather than `origin`, following in
  [`stat_bin()`](https://ggplot2.tidyverse.org/dev/reference/geom_histogram.md)’s
  footsteps ([@teunbrand](https://github.com/teunbrand)).
- Reintroduced `drop` argument to
  [`stat_bin()`](https://ggplot2.tidyverse.org/dev/reference/geom_histogram.md)
  ([@teunbrand](https://github.com/teunbrand),
  [\#3449](https://github.com/tidyverse/ggplot2/issues/3449))
- [`stat_bin()`](https://ggplot2.tidyverse.org/dev/reference/geom_histogram.md)
  now accepts functions for argument `breaks`
  ([@aijordan](https://github.com/aijordan),
  [\#4561](https://github.com/tidyverse/ggplot2/issues/4561))
- [`after_stat()`](https://ggplot2.tidyverse.org/dev/reference/aes_eval.md)
  and
  [`after_scale()`](https://ggplot2.tidyverse.org/dev/reference/aes_eval.md)
  throw warnings when the computed aesthetics are not of the correct
  length ([\#5901](https://github.com/tidyverse/ggplot2/issues/5901)).
- [`geom_hline()`](https://ggplot2.tidyverse.org/dev/reference/geom_abline.md)
  and
  [`geom_vline()`](https://ggplot2.tidyverse.org/dev/reference/geom_abline.md)
  now have `position` argument
  ([@yutannihilation](https://github.com/yutannihilation),
  [\#4285](https://github.com/tidyverse/ggplot2/issues/4285)).
- [`geom_contour()`](https://ggplot2.tidyverse.org/dev/reference/geom_contour.md)
  should be able to recognise a rotated grid of points
  ([@teunbrand](https://github.com/teunbrand),
  [\#4320](https://github.com/tidyverse/ggplot2/issues/4320))

##### Other

- An attempt is made to use a variable’s label attribute as default
  label ([@teunbrand](https://github.com/teunbrand),
  [\#4631](https://github.com/tidyverse/ggplot2/issues/4631))
- `guide_*()` can now accept two inside legend theme elements:
  `legend.position.inside` and `legend.justification.inside`, allowing
  inside legends to be placed at different positions. Only inside
  legends with the same position and justification will be merged
  ([@Yunuuuu](https://github.com/Yunuuuu),
  [\#6210](https://github.com/tidyverse/ggplot2/issues/6210)).
- [`guide_bins()`](https://ggplot2.tidyverse.org/dev/reference/guide_bins.md),
  [`guide_colourbar()`](https://ggplot2.tidyverse.org/dev/reference/guide_colourbar.md)
  and
  [`guide_coloursteps()`](https://ggplot2.tidyverse.org/dev/reference/guide_coloursteps.md)
  gain an `angle` argument to overrule theme settings, similar to
  `guide_axis(angle)` ([@teunbrand](https://github.com/teunbrand),
  [\#4594](https://github.com/tidyverse/ggplot2/issues/4594)).
- New argument `labs(dictionary)` to label based on variable name rather
  than based on aesthetic ([@teunbrand](https://github.com/teunbrand),
  [\#5178](https://github.com/tidyverse/ggplot2/issues/5178))
- The [`summary()`](https://rdrr.io/r/base/summary.html) method for
  ggplots is now more terse about facets
  ([@teunbrand](https://github.com/teunbrand),
  [\#5989](https://github.com/tidyverse/ggplot2/issues/5989)).
- [`facet_wrap()`](https://ggplot2.tidyverse.org/dev/reference/facet_wrap.md)
  can have `space = "free_x"` with 1-row layouts and `space = "free_y"`
  with 1-column layouts ([@teunbrand](https://github.com/teunbrand))
- Layers can have names ([@teunbrand](https://github.com/teunbrand),
  [\#4066](https://github.com/tidyverse/ggplot2/issues/4066)).
- Axis labels are now justified across facet panels
  ([@teunbrand](https://github.com/teunbrand),
  [\#5820](https://github.com/tidyverse/ggplot2/issues/5820))
- `facet_grid(space = "free")` can now be combined with
  [`coord_fixed()`](https://ggplot2.tidyverse.org/dev/reference/coord_fixed.md)
  ([@teunbrand](https://github.com/teunbrand),
  [\#4584](https://github.com/tidyverse/ggplot2/issues/4584)).
- The ellipsis argument is now checked in
  [`fortify()`](https://ggplot2.tidyverse.org/dev/reference/fortify.md),
  [`get_alt_text()`](https://ggplot2.tidyverse.org/dev/reference/get_alt_text.md),
  [`labs()`](https://ggplot2.tidyverse.org/dev/reference/labs.md) and
  several guides. ([@teunbrand](https://github.com/teunbrand),
  [\#3196](https://github.com/tidyverse/ggplot2/issues/3196)).
- [`ggsave()`](https://ggplot2.tidyverse.org/dev/reference/ggsave.md)
  can write a multi-page pdf file when provided with a list of plots
  ([@teunbrand](https://github.com/teunbrand),
  [\#5093](https://github.com/tidyverse/ggplot2/issues/5093)).

#### Bug fixes

- Fixed a bug where the `guide_custom(order)` wasn’t working
  ([@teunbrand](https://github.com/teunbrand),
  [\#6195](https://github.com/tidyverse/ggplot2/issues/6195))
- Fixed bug in
  [`guide_custom()`](https://ggplot2.tidyverse.org/dev/reference/guide_custom.md)
  that would throw error with
  [`theme_void()`](https://ggplot2.tidyverse.org/dev/reference/ggtheme.md)
  ([@teunbrand](https://github.com/teunbrand),
  [\#5856](https://github.com/tidyverse/ggplot2/issues/5856)).
- [`guide_colourbar()`](https://ggplot2.tidyverse.org/dev/reference/guide_colourbar.md)
  now correctly hands off `position` and `available_aes` parameters
  downstream ([@teunbrand](https://github.com/teunbrand),
  [\#5930](https://github.com/tidyverse/ggplot2/issues/5930)).
- [`guide_axis()`](https://ggplot2.tidyverse.org/dev/reference/guide_axis.md)
  no longer reserves space for blank ticks
  ([@teunbrand](https://github.com/teunbrand),
  [\#4722](https://github.com/tidyverse/ggplot2/issues/4722),
  [\#6069](https://github.com/tidyverse/ggplot2/issues/6069)).
- Fixed regression in axes where `breaks = NULL` caused the axes to
  disappear instead of just rendering the axis line
  ([@teunbrand](https://github.com/teunbrand),
  [\#5816](https://github.com/tidyverse/ggplot2/issues/5816)).
- Better handling of the `guide_axis_logticks(negative.small)` parameter
  when scale limits have small maximum
  ([@teunbrand](https://github.com/teunbrand),
  [\#6121](https://github.com/tidyverse/ggplot2/issues/6121)).
- Fixed regression in `guide_bins(reverse = TRUE)`
  ([@teunbrand](https://github.com/teunbrand),
  [\#6183](https://github.com/tidyverse/ggplot2/issues/6183)).  
- Binned guides now accept expressions as labels
  ([@teunbrand](https://github.com/teunbrand),
  [\#6005](https://github.com/tidyverse/ggplot2/issues/6005))
- Fixed bug where binned scales wouldn’t simultaneously accept
  transformations and function-limits
  ([@teunbrand](https://github.com/teunbrand),
  [\#6144](https://github.com/tidyverse/ggplot2/issues/6144)).
- Fixed bug in out-of-bounds binned breaks
  ([@teunbrand](https://github.com/teunbrand),
  [\#6054](https://github.com/tidyverse/ggplot2/issues/6054))
- Fixed bug where binned guides would keep out-of-bounds breaks
  ([@teunbrand](https://github.com/teunbrand),
  [\#5870](https://github.com/tidyverse/ggplot2/issues/5870))
- Binned scales with zero-width data expand the default limits by 0.1
  ([@teunbrand](https://github.com/teunbrand),
  [\#5066](https://github.com/tidyverse/ggplot2/issues/5066))
- Date(time) scales now throw appropriate errors when `date_breaks`,
  `date_minor_breaks` or `date_labels` are not strings
  ([@RodDalBen](https://github.com/RodDalBen),
  [\#5880](https://github.com/tidyverse/ggplot2/issues/5880))
- Secondary axes respect `n.breaks` setting in continuous scales
  ([@teunbrand](https://github.com/teunbrand),
  [\#4483](https://github.com/tidyverse/ggplot2/issues/4483)).
- The size of the
  [`draw_key_polygon()`](https://ggplot2.tidyverse.org/dev/reference/draw_key.md)
  glyph now reflects the `linewidth` aesthetic which internally defaults
  to 0 ([\#4852](https://github.com/tidyverse/ggplot2/issues/4852)).
- [`draw_key_rect()`](https://ggplot2.tidyverse.org/dev/reference/draw_key.md)
  replaces a `NA` fill by the `colour` aesthetic
  ([@teunbrand](https://github.com/teunbrand),
  [\#5385](https://github.com/tidyverse/ggplot2/issues/5385),
  [\#5756](https://github.com/tidyverse/ggplot2/issues/5756)).
- Fixed bug where `na.value` was incorrectly mapped to non-`NA` values
  ([@teunbrand](https://github.com/teunbrand),
  [\#5756](https://github.com/tidyverse/ggplot2/issues/5756)).
- Missing values from discrete palettes are no longer inappropriately
  translated ([@teunbrand](https://github.com/teunbrand),
  [\#5929](https://github.com/tidyverse/ggplot2/issues/5929)).
- Fixed bug where empty discrete scales weren’t recognised as such
  ([@teunbrand](https://github.com/teunbrand),
  [\#5945](https://github.com/tidyverse/ggplot2/issues/5945)).
- Fixed regression with incorrectly drawn gridlines when using
  [`coord_flip()`](https://ggplot2.tidyverse.org/dev/reference/coord_flip.md)
  ([@teunbrand](https://github.com/teunbrand),
  [\#6293](https://github.com/tidyverse/ggplot2/issues/6293)).
- [`coord_radial()`](https://ggplot2.tidyverse.org/dev/reference/coord_radial.md)
  now displays no axis instead of throwing an error when a scale has no
  breaks ([@teunbrand](https://github.com/teunbrand),
  [\#6271](https://github.com/tidyverse/ggplot2/issues/6271)).
- [`coord_radial()`](https://ggplot2.tidyverse.org/dev/reference/coord_radial.md)
  displays minor gridlines now
  ([@teunbrand](https://github.com/teunbrand)).
- Position scales combined with
  [`coord_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md)
  can now use functions in the `breaks` argument. In addition,
  `n.breaks` works as intended and `breaks = NULL` removes grid lines
  and axes ([@teunbrand](https://github.com/teunbrand),
  [\#4622](https://github.com/tidyverse/ggplot2/issues/4622)).
- [`coord_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md) no
  longer errors when dealing with empty graticules
  ([@teunbrand](https://github.com/teunbrand),
  [\#6052](https://github.com/tidyverse/ggplot2/issues/6052))
- [`position_fill()`](https://ggplot2.tidyverse.org/dev/reference/position_stack.md)
  avoids stacking observations of zero
  ([@teunbrand](https://github.com/teunbrand),
  [\#6338](https://github.com/tidyverse/ggplot2/issues/6338))
- Fix a bug in
  [`position_jitterdodge()`](https://ggplot2.tidyverse.org/dev/reference/position_jitterdodge.md)
  where different jitters would be applied to different position
  aesthetics of the same axis
  ([@teunbrand](https://github.com/teunbrand),
  [\#5818](https://github.com/tidyverse/ggplot2/issues/5818)).
- Fixed bug in
  [`position_dodge2()`](https://ggplot2.tidyverse.org/dev/reference/position_dodge.md)’s
  identification of range overlaps
  ([@teunbrand](https://github.com/teunbrand),
  [\#5938](https://github.com/tidyverse/ggplot2/issues/5938),
  [\#4327](https://github.com/tidyverse/ggplot2/issues/4327)).
- [`geom_ribbon()`](https://ggplot2.tidyverse.org/dev/reference/geom_ribbon.md)
  now appropriately warns about, and removes, missing values
  ([@teunbrand](https://github.com/teunbrand),
  [\#6243](https://github.com/tidyverse/ggplot2/issues/6243)).
- Custom and raster annotation now respond to scale transformations, and
  can use AsIs variables for relative placement
  ([@teunbrand](https://github.com/teunbrand) based on
  [@yutannihilation](https://github.com/yutannihilation)’s prior work,
  [\#3120](https://github.com/tidyverse/ggplot2/issues/3120))
- [`geom_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md) now
  accepts shape names for point geometries
  ([@sierrajohnson](https://github.com/sierrajohnson),
  [\#5808](https://github.com/tidyverse/ggplot2/issues/5808))
- [`geom_step()`](https://ggplot2.tidyverse.org/dev/reference/geom_path.md)
  now supports the `orientation` argument
  ([@teunbrand](https://github.com/teunbrand),
  [\#5936](https://github.com/tidyverse/ggplot2/issues/5936)).
- [`geom_rug()`](https://ggplot2.tidyverse.org/dev/reference/geom_rug.md)
  prints a warning when `na.rm = FALSE`, as per documentation
  ([@pn317](https://github.com/pn317),
  [\#5905](https://github.com/tidyverse/ggplot2/issues/5905))
- [`geom_curve()`](https://ggplot2.tidyverse.org/dev/reference/geom_segment.md)
  now appropriately removes missing data instead of throwing errors
  ([@teunbrand](https://github.com/teunbrand),
  [\#5831](https://github.com/tidyverse/ggplot2/issues/5831)).
- Improved consistency of curve direction in
  [`geom_curve()`](https://ggplot2.tidyverse.org/dev/reference/geom_segment.md)
  ([@teunbrand](https://github.com/teunbrand),
  [\#5069](https://github.com/tidyverse/ggplot2/issues/5069)).
- [`geom_abline()`](https://ggplot2.tidyverse.org/dev/reference/geom_abline.md)
  clips to the panel range in the vertical direction too
  ([@teunbrand](https://github.com/teunbrand),
  [\#6086](https://github.com/tidyverse/ggplot2/issues/6086)).
- The default `se` parameter in layers with `geom = "smooth"` will be
  `TRUE` when the data has `ymin` and `ymax` parameters and `FALSE` if
  these are absent. Note that this does not affect the default of
  [`geom_smooth()`](https://ggplot2.tidyverse.org/dev/reference/geom_smooth.md)
  or
  [`stat_smooth()`](https://ggplot2.tidyverse.org/dev/reference/geom_smooth.md)
  ([@teunbrand](https://github.com/teunbrand),
  [\#5572](https://github.com/tidyverse/ggplot2/issues/5572)).
- The bounded density option in
  [`stat_density()`](https://ggplot2.tidyverse.org/dev/reference/geom_density.md)
  uses a wider range to prevent discontinuities
  ([\#5641](https://github.com/tidyverse/ggplot2/issues/5641)).
- Fixed bug in
  [`stat_function()`](https://ggplot2.tidyverse.org/dev/reference/geom_function.md)
  so x-axis title now produced automatically when no data added.
  ([@phispu](https://github.com/phispu),
  [\#5647](https://github.com/tidyverse/ggplot2/issues/5647)).
- [`stat_summary_2d()`](https://ggplot2.tidyverse.org/dev/reference/stat_summary_2d.md)
  and
  [`stat_bin_2d()`](https://ggplot2.tidyverse.org/dev/reference/geom_bin_2d.md)
  now deal with zero-range data more elegantly
  ([@teunbrand](https://github.com/teunbrand),
  [\#6207](https://github.com/tidyverse/ggplot2/issues/6207)).
- [`stat_summary_bin()`](https://ggplot2.tidyverse.org/dev/reference/stat_summary.md)
  no longer ignores `width` parameter
  ([@teunbrand](https://github.com/teunbrand),
  [\#4647](https://github.com/tidyverse/ggplot2/issues/4647)).
- Fixed bug where the `ggplot2::`-prefix did not work with
  [`stage()`](https://ggplot2.tidyverse.org/dev/reference/aes_eval.md)
  ([@teunbrand](https://github.com/teunbrand),
  [\#6104](https://github.com/tidyverse/ggplot2/issues/6104)).
- Passing empty unmapped aesthetics to layers raises a warning instead
  of throwing an error ([@teunbrand](https://github.com/teunbrand),
  [\#6009](https://github.com/tidyverse/ggplot2/issues/6009)).
- Staged expressions are handled more gracefully if legends cannot
  resolve them ([@teunbrand](https://github.com/teunbrand),
  [\#6264](https://github.com/tidyverse/ggplot2/issues/6264)).
- `theme(strip.clip)` now defaults to `"on"` and is independent of Coord
  clipping ([@teunbrand](https://github.com/teunbrand), 5952).
- Fixed bug in `facet_grid(margins = TRUE)` when using expresssions
  ([@teunbrand](https://github.com/teunbrand),
  [\#1864](https://github.com/tidyverse/ggplot2/issues/1864)).
- Prevented `facet_wrap(..., drop = FALSE)` from throwing spurious
  errors when a character facetting variable contained `NA`s
  ([@teunbrand](https://github.com/teunbrand),
  [\#5485](https://github.com/tidyverse/ggplot2/issues/5485)).

### Developer facing

#### Utilities

- New helper function
  [`gg_par()`](https://ggplot2.tidyverse.org/dev/reference/gg_par.md) to
  translate ggplot2’s interpretation of graphical parameters to {grid}’s
  interpretation ([@teunbrand](https://github.com/teunbrand),
  [\#5866](https://github.com/tidyverse/ggplot2/issues/5866)).
- New roxygen tag `@aesthetics` that takes a Geom, Stat or Position
  class and generates an ‘Aesthetics’ section.
- New
  [`make_constructor()`](https://ggplot2.tidyverse.org/dev/reference/make_constructor.md)
  function that builds a standard constructor for Geom and Stat classes
  ([@teunbrand](https://github.com/teunbrand),
  [\#6142](https://github.com/tidyverse/ggplot2/issues/6142)).
- New
  [`element_point()`](https://ggplot2.tidyverse.org/dev/reference/element.md)
  and
  [`element_polygon()`](https://ggplot2.tidyverse.org/dev/reference/element.md)
  that can be given to `theme(point, polygon)` as an extension point
  ([@teunbrand](https://github.com/teunbrand),
  [\#6248](https://github.com/tidyverse/ggplot2/issues/6248)).
- The helper function
  [`is_waiver()`](https://ggplot2.tidyverse.org/dev/reference/waiver.md)
  is now exported to help extensions to work with
  [`waiver()`](https://ggplot2.tidyverse.org/dev/reference/waiver.md)
  objects ([@arcresu](https://github.com/arcresu),
  [\#6173](https://github.com/tidyverse/ggplot2/issues/6173)).
- [`update_geom_defaults()`](https://ggplot2.tidyverse.org/dev/reference/update_defaults.md)
  and
  [`update_stat_defaults()`](https://ggplot2.tidyverse.org/dev/reference/update_defaults.md)
  have a reset mechanism when using `new = NULL` and invisible return
  the previous defaults
  ([\#4993](https://github.com/tidyverse/ggplot2/issues/4993)).
- New
  [`reset_geom_defaults()`](https://ggplot2.tidyverse.org/dev/reference/update_defaults.md)
  and
  [`reset_stat_defaults()`](https://ggplot2.tidyverse.org/dev/reference/update_defaults.md)
  to restore all geom or stat default aesthetics at once
  ([@teunbrand](https://github.com/teunbrand),
  [\#5975](https://github.com/tidyverse/ggplot2/issues/5975)).
- New function
  [`complete_theme()`](https://ggplot2.tidyverse.org/dev/reference/complete_theme.md)
  to replicate how themes are handled during plot building
  ([\#5801](https://github.com/tidyverse/ggplot2/issues/5801)).
- New function
  [`get_strip_labels()`](https://ggplot2.tidyverse.org/dev/reference/get_strip_labels.md)
  to retrieve facet labels ([@teunbrand](https://github.com/teunbrand),
  [\#4979](https://github.com/tidyverse/ggplot2/issues/4979))
- The ViewScale class has a `make_fixed_copy()` method to permit copying
  trained position scales
  ([\#3441](https://github.com/tidyverse/ggplot2/issues/3441)).

#### Internal changes

- Facet gains a new method `setup_panel_params` to interact with the
  panel_params setted by Coord object
  ([@Yunuuuu](https://github.com/Yunuuuu),
  [\#6397](https://github.com/tidyverse/ggplot2/issues/6397),
  [\#6380](https://github.com/tidyverse/ggplot2/issues/6380))
- [`continuous_scale()`](https://ggplot2.tidyverse.org/dev/reference/continuous_scale.md)
  and
  [`binned_scale()`](https://ggplot2.tidyverse.org/dev/reference/binned_scale.md)
  sort the `limits` argument internally
  ([@teunbrand](https://github.com/teunbrand)).
- `Scale$get_labels()` format expressions as lists.
- Using
  [`after_scale()`](https://ggplot2.tidyverse.org/dev/reference/aes_eval.md)
  in the `Geom*$default_aes` field is now evaluated in the context of
  data ([@teunbrand](https://github.com/teunbrand),
  [\#6135](https://github.com/tidyverse/ggplot2/issues/6135))
- Improvements to `pal_qualitative()`
  ([@teunbrand](https://github.com/teunbrand),
  [\#5013](https://github.com/tidyverse/ggplot2/issues/5013))
- Panel clipping responsibility moved from Facet class to Coord class
  through new `Coord$draw_panel()` method.
- Rearranged the code of `Facet$draw_panels()` method
  ([@teunbrand](https://github.com/teunbrand)).
- Added `gg` class to
  [`labs()`](https://ggplot2.tidyverse.org/dev/reference/labs.md)
  ([@phispu](https://github.com/phispu),
  [\#5553](https://github.com/tidyverse/ggplot2/issues/5553)).
- The plot’s layout now has a coord parameter that is used to prevent
  setting up identical panel parameters more than once
  ([\#5427](https://github.com/tidyverse/ggplot2/issues/5427))
- Applying defaults in
  [`geom_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md) has
  moved from the internal `sf_grob()` to `GeomSf$use_defaults()`
  ([@teunbrand](https://github.com/teunbrand)).
- New `Facet$draw_panel_content()` method for delegating panel assembly
  ([@Yunuuuu](https://github.com/Yunuuuu),
  [\#6406](https://github.com/tidyverse/ggplot2/issues/6406)).
- Layer data can be attenuated with parameter attributes
  ([@teunbrand](https://github.com/teunbrand),
  [\#3175](https://github.com/tidyverse/ggplot2/issues/3175)).
- When facets coerce the faceting variables to factors, the ‘ordered’
  class is dropped ([@teunbrand](https://github.com/teunbrand),
  [\#5666](https://github.com/tidyverse/ggplot2/issues/5666)).
- [`stat_align()`](https://ggplot2.tidyverse.org/dev/reference/geom_ribbon.md)
  skips computation when there is only 1 group and therefore alignment
  is not necessary
  ([\#5788](https://github.com/tidyverse/ggplot2/issues/5788)).
- [`position_stack()`](https://ggplot2.tidyverse.org/dev/reference/position_stack.md)
  skips computation when all `x` values are unique and therefore
  stacking is not necessary
  ([\#5788](https://github.com/tidyverse/ggplot2/issues/5788)).
- The summary function of
  [`stat_summary()`](https://ggplot2.tidyverse.org/dev/reference/stat_summary.md)
  and
  [`stat_summary_bin()`](https://ggplot2.tidyverse.org/dev/reference/stat_summary.md)
  is setup once in total instead of once per group
  ([@teunbrand](https://github.com/teunbrand),
  [\#5971](https://github.com/tidyverse/ggplot2/issues/5971))
- Removed barriers for using 2D structures as aesthetics
  ([@teunbrand](https://github.com/teunbrand),
  [\#4189](https://github.com/tidyverse/ggplot2/issues/4189)).
- Stricter check on `register_theme_elements(element_tree)`
  ([@teunbrand](https://github.com/teunbrand),
  [\#6162](https://github.com/tidyverse/ggplot2/issues/6162))
- The `legend.key.width` and `legend.key.height` calculations are no
  longer precomputed before guides are drawn
  ([@teunbrand](https://github.com/teunbrand),
  [\#6339](https://github.com/tidyverse/ggplot2/issues/6339))
- When `validate_subclass()` fails to find a class directly, it tries to
  retrieve the class via constructor functions
  ([@teunbrand](https://github.com/teunbrand)).

## ggplot2 3.5.2

CRAN release: 2025-04-09

This is a small release focusing on providing infrastructure for other
packages to gracefully prepare for changes in the next major release.

### Improvements

- Standardised test functions for important classes:
  [`is_ggproto()`](https://ggplot2.tidyverse.org/dev/reference/is_tests.md),
  [`is_ggplot()`](https://ggplot2.tidyverse.org/dev/reference/is_tests.md),
  [`is_mapping()`](https://ggplot2.tidyverse.org/dev/reference/is_tests.md),
  [`is_layer()`](https://ggplot2.tidyverse.org/dev/reference/is_tests.md),
  [`is_geom()`](https://ggplot2.tidyverse.org/dev/reference/is_tests.md),
  [`is_stat()`](https://ggplot2.tidyverse.org/dev/reference/is_tests.md),
  [`is_position()`](https://ggplot2.tidyverse.org/dev/reference/is_tests.md),
  [`is_coord()`](https://ggplot2.tidyverse.org/dev/reference/is_tests.md),
  [`is_facet()`](https://ggplot2.tidyverse.org/dev/reference/is_tests.md),
  [`is_scale()`](https://ggplot2.tidyverse.org/dev/reference/is_tests.md),
  [`is_guide()`](https://ggplot2.tidyverse.org/dev/reference/is_tests.md),
  [`is_guides()`](https://ggplot2.tidyverse.org/dev/reference/is_tests.md),
  [`is_margin()`](https://ggplot2.tidyverse.org/dev/reference/is_tests.md),
  [`is_theme_element()`](https://ggplot2.tidyverse.org/dev/reference/is_tests.md)
  and
  [`is_theme()`](https://ggplot2.tidyverse.org/dev/reference/is_tests.md).
- New
  [`get_labs()`](https://ggplot2.tidyverse.org/dev/reference/labs.md)
  function for retrieving completed plot labels
  ([@teunbrand](https://github.com/teunbrand),
  [\#6008](https://github.com/tidyverse/ggplot2/issues/6008)).
- New
  [`get_geom_defaults()`](https://ggplot2.tidyverse.org/dev/reference/get_geom_defaults.md)
  for retrieving resolved default aesthetics.
- A new
  [`ggplot_build()`](https://ggplot2.tidyverse.org/dev/reference/ggplot_build.md)
  S3 method for classes was added, which returns input unaltered
  ([@teunbrand](https://github.com/teunbrand),
  [\#5800](https://github.com/tidyverse/ggplot2/issues/5800)).

## ggplot2 3.5.1

CRAN release: 2024-04-23

This is a small release focusing on fixing regressions from 3.5.0 and
documentation updates.

### Bug fixes

- Fixed bug where discrete scales could not map aesthetics only
  consisting of `NA`s
  ([\#5623](https://github.com/tidyverse/ggplot2/issues/5623))
- Fixed spurious warnings from
  [`sec_axis()`](https://ggplot2.tidyverse.org/dev/reference/sec_axis.md)
  with `breaks = NULL`
  ([\#5713](https://github.com/tidyverse/ggplot2/issues/5713)).
- Patterns and gradients are now also enabled in
  [`geom_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md)
  ([@teunbrand](https://github.com/teunbrand),
  [\#5716](https://github.com/tidyverse/ggplot2/issues/5716)).
- The default behaviour of
  [`resolution()`](https://ggplot2.tidyverse.org/dev/reference/resolution.md)
  has been reverted to pre-3.5.0 behaviour. Whether mapped discrete
  vectors should be treated as having resolution of 1 is controlled by
  the new `discrete` argument.
- Fixed bug in
  [`guide_bins()`](https://ggplot2.tidyverse.org/dev/reference/guide_bins.md)
  and
  [`guide_coloursteps()`](https://ggplot2.tidyverse.org/dev/reference/guide_coloursteps.md)
  where discrete breaks, such as the levels produced by
  [`cut()`](https://rdrr.io/r/base/cut.html), were ordered incorrectly
  ([@teunbrand](https://github.com/teunbrand),
  [\#5757](https://github.com/tidyverse/ggplot2/issues/5757)).

### Improvements

- When facets coerce the faceting variables to factors, the ‘ordered’
  class is dropped ([@teunbrand](https://github.com/teunbrand),
  [\#5666](https://github.com/tidyverse/ggplot2/issues/5666)).
- [`coord_map()`](https://ggplot2.tidyverse.org/dev/reference/coord_map.md)
  and
  [`coord_polar()`](https://ggplot2.tidyverse.org/dev/reference/coord_radial.md)
  throw informative warnings when used with the guide system
  ([\#5707](https://github.com/tidyverse/ggplot2/issues/5707)).
- When passing a function to `stat_contour(breaks)`, that function is
  used to calculate the breaks even if `bins` and `binwidth` are missing
  ([@teunbrand](https://github.com/teunbrand),
  [\#5686](https://github.com/tidyverse/ggplot2/issues/5686)).
- [`geom_step()`](https://ggplot2.tidyverse.org/dev/reference/geom_path.md)
  now supports `lineend`, `linejoin` and `linemitre` parameters
  ([@teunbrand](https://github.com/teunbrand),
  [\#5705](https://github.com/tidyverse/ggplot2/issues/5705)).
- Fixed performance loss when the `.data` pronoun is used in
  [`aes()`](https://ggplot2.tidyverse.org/dev/reference/aes.md)
  ([\#5730](https://github.com/tidyverse/ggplot2/issues/5730)).
- Facet evaluation is better at dealing with inherited errors
  ([@teunbrand](https://github.com/teunbrand),
  [\#5670](https://github.com/tidyverse/ggplot2/issues/5670)).
- [`stat_bin()`](https://ggplot2.tidyverse.org/dev/reference/geom_histogram.md)
  deals with non-finite breaks better
  ([@teunbrand](https://github.com/teunbrand),
  [\#5665](https://github.com/tidyverse/ggplot2/issues/5665)).
- While axes in
  [`coord_radial()`](https://ggplot2.tidyverse.org/dev/reference/coord_radial.md)
  don’t neatly fit the top/right/bottom/left organisation, specifying
  `position = "top"` or `position = "right"` in the scale will flip the
  placement of the radial axis
  ([\#5735](https://github.com/tidyverse/ggplot2/issues/5735))
- Theme elements that do not exist now throw warnings instead of errors
  ([\#5719](https://github.com/tidyverse/ggplot2/issues/5719)).
- Fixed bug in
  [`coord_radial()`](https://ggplot2.tidyverse.org/dev/reference/coord_radial.md)
  where full circles were not treated as such
  ([@teunbrand](https://github.com/teunbrand),
  [\#5750](https://github.com/tidyverse/ggplot2/issues/5750)).
- When legends detect the presence of values in a layer, `NA` is now
  detected if the data contains values outside the given breaks
  ([@teunbrand](https://github.com/teunbrand),
  [\#5749](https://github.com/tidyverse/ggplot2/issues/5749)).
- [`annotate()`](https://ggplot2.tidyverse.org/dev/reference/annotate.md)
  now warns about `stat` or `position` arguments
  ([@teunbrand](https://github.com/teunbrand),
  [\#5151](https://github.com/tidyverse/ggplot2/issues/5151))
- `guide_coloursteps(even.steps = FALSE)` now works with discrete data
  that has been formatted by [`cut()`](https://rdrr.io/r/base/cut.html)
  ([@teunbrand](https://github.com/teunbrand),
  [\#3877](https://github.com/tidyverse/ggplot2/issues/3877)).
- [`ggsave()`](https://ggplot2.tidyverse.org/dev/reference/ggsave.md)
  now offers to install svglite if needed
  ([@eliocamp](https://github.com/eliocamp),
  [\#6166](https://github.com/tidyverse/ggplot2/issues/6166)).

## ggplot2 3.5.0

CRAN release: 2024-02-23

This is a minor release that turned out quite beefy. It is focused on
overhauling the guide system: the system responsible for displaying
information from scales in the guise of axes and legends. As part of
that overhaul, new guides have been implemented and existing guides have
been refined. The look and feel of guides has been mostly preserved, but
their internals and styling options have changed drastically.

Briefly summarising other highlights, we also welcome
[`coord_radial()`](https://ggplot2.tidyverse.org/dev/reference/coord_radial.md)
as a successor of
[`coord_polar()`](https://ggplot2.tidyverse.org/dev/reference/coord_radial.md).
Initial support for newer graphical features, such as pattern fills has
been added. The API has changed how
[`I()`](https://rdrr.io/r/base/AsIs.html)/`<AsIs>` vectors interact with
the scale system, namely: not at all.

### Breaking changes

- The guide system. As a whole. See ‘new features’ for more information.
  While the S3 guide generics are still in place, the S3 methods for
  [`guide_train()`](https://ggplot2.tidyverse.org/dev/reference/old_guide.md),
  [`guide_merge()`](https://ggplot2.tidyverse.org/dev/reference/old_guide.md),
  [`guide_geom()`](https://ggplot2.tidyverse.org/dev/reference/old_guide.md),
  [`guide_transform()`](https://ggplot2.tidyverse.org/dev/reference/old_guide.md),
  [`guide_gengrob()`](https://ggplot2.tidyverse.org/dev/reference/old_guide.md)
  have been superseded by the respective ggproto methods. In practice,
  this will mean that
  [`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) or
  sub-classing ggplot2’s guides with the S3 system will no longer work.

- By default,
  [`guide_legend()`](https://ggplot2.tidyverse.org/dev/reference/guide_legend.md)
  now only draws a key glyph for a layer when the value is in the
  layer’s data. To revert to the old behaviour, you can still set
  `show.legend = c({aesthetic} = TRUE)`
  ([@teunbrand](https://github.com/teunbrand),
  [\#3648](https://github.com/tidyverse/ggplot2/issues/3648)).

- In the `scale_{colour/fill}_gradient2()` and
  `scale_{colour/fill}_steps2()` functions, the `midpoint` argument is
  transformed by the scale transformation
  ([\#3198](https://github.com/tidyverse/ggplot2/issues/3198)).

- The `legend.key` theme element is set to inherit from the
  `panel.background` theme element. The default themes no longer set the
  `legend.key` element. This causes a visual change with the default
  [`theme_gray()`](https://ggplot2.tidyverse.org/dev/reference/ggtheme.md)
  ([\#5549](https://github.com/tidyverse/ggplot2/issues/5549)).

- The `scale_name` argument in
  [`continuous_scale()`](https://ggplot2.tidyverse.org/dev/reference/continuous_scale.md),
  [`discrete_scale()`](https://ggplot2.tidyverse.org/dev/reference/discrete_scale.md)
  and
  [`binned_scale()`](https://ggplot2.tidyverse.org/dev/reference/binned_scale.md)
  is soft-deprecated. If you have implemented custom scales, be advised
  to double-check that unnamed arguments ends up where they should
  ([@teunbrand](https://github.com/teunbrand),
  [\#1312](https://github.com/tidyverse/ggplot2/issues/1312)).

- The `legend.text.align` and `legend.title.align` arguments in
  [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md) are
  deprecated. The `hjust` setting of the `legend.text` and
  `legend.title` elements continues to fulfill the role of text
  alignment ([@teunbrand](https://github.com/teunbrand),
  [\#5347](https://github.com/tidyverse/ggplot2/issues/5347)).

- ‘lines’ units in
  [`geom_label()`](https://ggplot2.tidyverse.org/dev/reference/geom_text.md),
  often used in the `label.padding` argument, are now are relative to
  the text size. This causes a visual change, but fixes a misalignment
  issue between the textbox and text
  ([@teunbrand](https://github.com/teunbrand),
  [\#4753](https://github.com/tidyverse/ggplot2/issues/4753))

- [`coord_flip()`](https://ggplot2.tidyverse.org/dev/reference/coord_flip.md)
  has been marked as superseded. The recommended alternative is to swap
  the `x` and `y` aesthetic and/or using the `orientation` argument in a
  layer ([@teunbrand](https://github.com/teunbrand),
  [\#5130](https://github.com/tidyverse/ggplot2/issues/5130)).

- The `trans` argument in scales and secondary axes has been renamed to
  `transform`. The `trans` argument itself is deprecated. To access the
  transformation from the scale, a new `get_transformation()` method is
  added to Scale-classes
  ([\#5558](https://github.com/tidyverse/ggplot2/issues/5558)).

- Providing a numeric vector to `theme(legend.position)` has been
  deprecated. To set the default legend position inside the plot use
  `theme(legend.position = "inside", legend.position.inside = c(...))`
  instead.

### New features

- Plot scales now ignore `AsIs` objects constructed with `I(x)`, instead
  of invoking the identity scale. This allows these columns to co-exist
  with other layers that need a non-identity scale for the same
  aesthetic. Also, it makes it easy to specify relative positions
  ([@teunbrand](https://github.com/teunbrand),
  [\#5142](https://github.com/tidyverse/ggplot2/issues/5142)).

- The `fill` aesthetic in many geoms now accepts grid’s patterns and
  gradients. For developers of layer extensions, this feature can be
  enabled by switching from `fill = alpha(fill, alpha)` to
  `fill = fill_alpha(fill, alpha)` when providing fills to
  [`grid::gpar()`](https://rdrr.io/r/grid/gpar.html)
  ([@teunbrand](https://github.com/teunbrand),
  [\#3997](https://github.com/tidyverse/ggplot2/issues/3997)).

- New function
  [`check_device()`](https://ggplot2.tidyverse.org/dev/reference/check_device.md)
  for testing the availability of advanced graphics features introduced
  in R 4.1.0 onward ([@teunbrand](https://github.com/teunbrand),
  [\#5332](https://github.com/tidyverse/ggplot2/issues/5332)).

- [`coord_radial()`](https://ggplot2.tidyverse.org/dev/reference/coord_radial.md)
  is a successor to
  [`coord_polar()`](https://ggplot2.tidyverse.org/dev/reference/coord_radial.md)
  with more customisation options.
  [`coord_radial()`](https://ggplot2.tidyverse.org/dev/reference/coord_radial.md)
  can:

  - integrate with the new guide system via a dedicated
    [`guide_axis_theta()`](https://ggplot2.tidyverse.org/dev/reference/guide_axis_theta.md)
    to display the angle coordinate.
  - in addition to drawing full circles, also draw circle sectors by
    using the `end` argument.
  - avoid data vanishing in the center of the plot by setting the
    `donut` argument.
  - adjust the `angle` aesthetic of layers, such as
    [`geom_text()`](https://ggplot2.tidyverse.org/dev/reference/geom_text.md),
    to align with the coordinate system using the `rotate_angle`
    argument.

#### The guide system

The guide system encompassing axes and legends, as the last remaining
chunk of ggplot2, has been rewritten to use the `<ggproto>` system
instead of the S3 system. This change was a necessary step to officially
break open the guide system for extension package developers. The axes
and legends now inherit from a `<Guide>` class, which makes them
extensible in the same manner as geoms, stats, facets and coords
([\#3329](https://github.com/tidyverse/ggplot2/issues/3329),
[@teunbrand](https://github.com/teunbrand))

- The most user-facing change is that the styling of guides is rewired
  through the theme system. Guides now have a `theme` argument that can
  style individual guides, while
  [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md) has
  gained additional arguments to style guides. Theme elements declared
  in the guide override theme elements set through the plot. The new
  theme elements for guides are: `legend.key.spacing{.x/.y}`,
  `legend.frame`, `legend.axis.line`, `legend.ticks`,
  `legend.ticks.length`, `legend.text.position` and
  `legend.title.position`. Previous style options in the arguments of
  `guide_*()` functions are soft-deprecated.

- Unfortunately, we could not fully preserve the function of
  pre-existing guide extensions written in the S3 system. A fallback for
  these old guides is encapsulated in the `<GuideOld>` class, which
  calls the old S3 generics. The S3 methods have been removed as part of
  cleaning up, so the old guides will still work if the S3 methods are
  reimplemented, but we encourage to switch to the new system
  ([\#2728](https://github.com/tidyverse/ggplot2/issues/2728)).

- The `order` argument of guides now strictly needs to be a length-1
  integer ([\#4958](https://github.com/tidyverse/ggplot2/issues/4958)).

##### Axes

- New
  [`guide_axis_stack()`](https://ggplot2.tidyverse.org/dev/reference/guide_axis_stack.md)
  to combine other axis guides on top of one another.

- New
  [`guide_axis_theta()`](https://ggplot2.tidyverse.org/dev/reference/guide_axis_theta.md)
  to draw an axis in a circular arc in
  [`coord_radial()`](https://ggplot2.tidyverse.org/dev/reference/coord_radial.md).
  The guide can be controlled by adding
  `guides(theta = guide_axis_theta(...))` to a plot.

- New
  [`guide_axis_logticks()`](https://ggplot2.tidyverse.org/dev/reference/guide_axis_logticks.md)
  can be used to draw logarithmic tick marks as an axis. It supersedes
  the
  [`annotation_logticks()`](https://ggplot2.tidyverse.org/dev/reference/annotation_logticks.md)
  function ([@teunbrand](https://github.com/teunbrand),
  [\#5325](https://github.com/tidyverse/ggplot2/issues/5325)).

- [`guide_axis()`](https://ggplot2.tidyverse.org/dev/reference/guide_axis.md)
  gains a `minor.ticks` argument to draw minor ticks
  ([\#4387](https://github.com/tidyverse/ggplot2/issues/4387)).

- [`guide_axis()`](https://ggplot2.tidyverse.org/dev/reference/guide_axis.md)
  gains a `cap` argument that can be used to trim the axis line to
  extreme breaks
  ([\#4907](https://github.com/tidyverse/ggplot2/issues/4907)).

- Primary axis titles are now placed at the primary guide, so that
  `guides(x = guide_axis(position = "top"))` will display the title at
  the top by default
  ([\#4650](https://github.com/tidyverse/ggplot2/issues/4650)).

- The default `vjust` for the `axis.title.y.right` element is now 1
  instead of

  0.  

- Unknown secondary axis guide positions are now inferred as the
  opposite of the primary axis guide when the latter has a known
  `position`
  ([\#4650](https://github.com/tidyverse/ggplot2/issues/4650)).

##### Legends

- New
  [`guide_custom()`](https://ggplot2.tidyverse.org/dev/reference/guide_custom.md)
  function for drawing custom graphical objects (grobs) unrelated to
  scales in legend positions
  ([\#5416](https://github.com/tidyverse/ggplot2/issues/5416)).

- All legends have acquired a `position` argument, that allows
  individual guides to deviate from the `legend.position` set in the
  [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md)
  function. This means that legends can now be placed at multiple sides
  of the plot
  ([\#5488](https://github.com/tidyverse/ggplot2/issues/5488)).

- The spacing between legend keys and their labels, in addition to
  legends and their titles, is now controlled by the text’s `margin`
  setting. Not specifying margins will automatically add appropriate
  text margins. To control the spacing within a legend between keys, the
  new `legend.key.spacing.{x/y}` argument can be used in
  [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md).
  This leaves the `legend.spacing` theme setting dedicated to solely
  controlling the spacing between different guides
  ([\#5455](https://github.com/tidyverse/ggplot2/issues/5455)).

- [`guide_colourbar()`](https://ggplot2.tidyverse.org/dev/reference/guide_colourbar.md)
  and
  [`guide_coloursteps()`](https://ggplot2.tidyverse.org/dev/reference/guide_coloursteps.md)
  gain an `alpha` argument to set the transparency of the bar
  ([\#5085](https://github.com/tidyverse/ggplot2/issues/5085)).

- New `display` argument in
  [`guide_colourbar()`](https://ggplot2.tidyverse.org/dev/reference/guide_colourbar.md)
  supplants the `raster` argument. In R 4.1.0 and above,
  `display = "gradient"` will draw a gradient.

- Legend keys that can draw arrows have their size adjusted for arrows.

- When legend titles are larger than the legend, title justification
  extends to the placement of keys and labels
  ([\#1903](https://github.com/tidyverse/ggplot2/issues/1903)).

- Glyph drawing functions of the `draw_key_*()` family can now set
  `"width"` and `"height"` attributes (in centimetres) to the produced
  keys to control their displayed size in the legend.

- [`coord_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md)
  now uses customisable guides provided in the scales or
  [`guides()`](https://ggplot2.tidyverse.org/dev/reference/guides.md)
  function ([@teunbrand](https://github.com/teunbrand)).

### Improvements

- `guide_coloursteps(even.steps = FALSE)` now draws one rectangle per
  interval instead of many small ones
  ([\#5481](https://github.com/tidyverse/ggplot2/issues/5481)).

- [`draw_key_label()`](https://ggplot2.tidyverse.org/dev/reference/draw_key.md)
  now better reflects the appearance of labels
  ([\#5561](https://github.com/tidyverse/ggplot2/issues/5561)).

- [`position_stack()`](https://ggplot2.tidyverse.org/dev/reference/position_stack.md)
  no longer silently removes missing data, which is now handled by the
  geom instead of position
  ([\#3532](https://github.com/tidyverse/ggplot2/issues/3532)).

- The `minor_breaks` function argument in scales can now also take a
  function with two arguments: the scale’s limits and the scale’s major
  breaks ([\#3583](https://github.com/tidyverse/ggplot2/issues/3583)).

- Failing to fit or predict in
  [`stat_smooth()`](https://ggplot2.tidyverse.org/dev/reference/geom_smooth.md)
  now gives a warning and omits the failed group, instead of throwing an
  error ([@teunbrand](https://github.com/teunbrand),
  [\#5352](https://github.com/tidyverse/ggplot2/issues/5352)).

- [`labeller()`](https://ggplot2.tidyverse.org/dev/reference/labeller.md)
  now handles unspecified entries from lookup tables
  ([@92amartins](https://github.com/92amartins),
  [\#4599](https://github.com/tidyverse/ggplot2/issues/4599)).

- `fortify.default()` now accepts a data-frame-like object granted the
  object exhibits healthy [`dim()`](https://rdrr.io/r/base/dim.html),
  [`colnames()`](https://rdrr.io/r/base/colnames.html), and
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
  behaviours ([@hpages](https://github.com/hpages),
  [\#5390](https://github.com/tidyverse/ggplot2/issues/5390)).

- [`geom_violin()`](https://ggplot2.tidyverse.org/dev/reference/geom_violin.md)
  gains a `bounds` argument analogous to
  [`geom_density()`](https://ggplot2.tidyverse.org/dev/reference/geom_density.md)s
  ([@eliocamp](https://github.com/eliocamp),
  [\#5493](https://github.com/tidyverse/ggplot2/issues/5493)).

- To apply dodging more consistently in violin plots,
  [`stat_ydensity()`](https://ggplot2.tidyverse.org/dev/reference/geom_violin.md)
  now has a `drop` argument to keep or discard groups with 1
  observation.

- [`geom_boxplot()`](https://ggplot2.tidyverse.org/dev/reference/geom_boxplot.md)
  gains a new argument, `staplewidth` that can draw staples at the ends
  of whiskers ([@teunbrand](https://github.com/teunbrand),
  [\#5126](https://github.com/tidyverse/ggplot2/issues/5126))

- [`geom_boxplot()`](https://ggplot2.tidyverse.org/dev/reference/geom_boxplot.md)
  gains an `outliers` argument to switch outliers on or off, in a manner
  that does affects the scale range. For hiding outliers that does not
  affect the scale range, you can continue to use `outlier.shape = NA`
  ([@teunbrand](https://github.com/teunbrand),
  [\#4892](https://github.com/tidyverse/ggplot2/issues/4892)).

- Nicer error messages for xlim/ylim arguments in coord-\* functions
  ([@92amartins](https://github.com/92amartins),
  [\#4601](https://github.com/tidyverse/ggplot2/issues/4601),
  [\#5297](https://github.com/tidyverse/ggplot2/issues/5297)).

- You can now omit either `xend` or `yend` from
  [`geom_segment()`](https://ggplot2.tidyverse.org/dev/reference/geom_segment.md)
  as only one of these is now required. If one is missing, it will be
  filled from the `x` and `y` aesthetics respectively. This makes
  drawing horizontal or vertical segments a little bit more convenient
  ([@teunbrand](https://github.com/teunbrand),
  [\#5140](https://github.com/tidyverse/ggplot2/issues/5140)).

- When
  [`geom_path()`](https://ggplot2.tidyverse.org/dev/reference/geom_path.md)
  has aesthetics varying within groups, the
  [`arrow()`](https://rdrr.io/r/grid/arrow.html) is applied to groups
  instead of individual segments
  ([@teunbrand](https://github.com/teunbrand),
  [\#4935](https://github.com/tidyverse/ggplot2/issues/4935)).

- [`geom_text()`](https://ggplot2.tidyverse.org/dev/reference/geom_text.md)
  and
  [`geom_label()`](https://ggplot2.tidyverse.org/dev/reference/geom_text.md)
  gained a `size.unit` parameter that set the text size to millimetres,
  points, centimetres, inches or picas
  ([@teunbrand](https://github.com/teunbrand),
  [\#3799](https://github.com/tidyverse/ggplot2/issues/3799)).

- [`geom_label()`](https://ggplot2.tidyverse.org/dev/reference/geom_text.md)
  now uses the `angle` aesthetic
  ([@teunbrand](https://github.com/teunbrand),
  [\#2785](https://github.com/tidyverse/ggplot2/issues/2785))

- The `label.padding` argument in
  [`geom_label()`](https://ggplot2.tidyverse.org/dev/reference/geom_text.md)
  now supports inputs created with the
  [`margin()`](https://ggplot2.tidyverse.org/dev/reference/element.md)
  function ([\#5030](https://github.com/tidyverse/ggplot2/issues/5030)).

- `ScaleContinuous$get_breaks()` now only calls
  [`scales::zero_range()`](https://scales.r-lib.org/reference/zero_range.html)
  on limits in transformed space, rather than in data space
  ([\#5304](https://github.com/tidyverse/ggplot2/issues/5304)).

- Scales throw more informative messages
  ([@teunbrand](https://github.com/teunbrand),
  [\#4185](https://github.com/tidyverse/ggplot2/issues/4185),
  [\#4258](https://github.com/tidyverse/ggplot2/issues/4258))

- `scale_*_manual()` with a named `values` argument now emits a warning
  when none of those names match the values found in the data
  ([@teunbrand](https://github.com/teunbrand),
  [\#5298](https://github.com/tidyverse/ggplot2/issues/5298)).

- The `name` argument in most scales is now explicitly the first
  argument ([\#5535](https://github.com/tidyverse/ggplot2/issues/5535))

- The
  [`translate_shape_string()`](https://ggplot2.tidyverse.org/dev/reference/translate_shape_string.md)
  internal function is now exported for use in extensions of point
  layers ([@teunbrand](https://github.com/teunbrand),
  [\#5191](https://github.com/tidyverse/ggplot2/issues/5191)).

- To improve `width` calculation in bar plots with empty factor levels,
  [`resolution()`](https://ggplot2.tidyverse.org/dev/reference/resolution.md)
  considers `mapped_discrete` values as having resolution 1
  ([@teunbrand](https://github.com/teunbrand),
  [\#5211](https://github.com/tidyverse/ggplot2/issues/5211))

- In [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md),
  some elements can be specified with
  [`rel()`](https://ggplot2.tidyverse.org/dev/reference/element.md) to
  inherit from `unit`-class objects in a relative fashion
  ([@teunbrand](https://github.com/teunbrand),
  [\#3951](https://github.com/tidyverse/ggplot2/issues/3951)).

- [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md) now
  supports splicing a list of arguments
  ([\#5542](https://github.com/tidyverse/ggplot2/issues/5542)).

- In the theme element hierarchy, parent elements that are a strict
  subclass of child elements now confer their subclass upon the children
  ([\#5457](https://github.com/tidyverse/ggplot2/issues/5457)).

- New `plot.tag.location` in
  [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md) can
  control placement of the plot tag in the `"margin"`, `"plot"` or the
  new `"panel"` option
  ([\#4297](https://github.com/tidyverse/ggplot2/issues/4297)).

- [`coord_munch()`](https://ggplot2.tidyverse.org/dev/reference/coord_munch.md)
  can now close polygon shapes
  ([@teunbrand](https://github.com/teunbrand),
  [\#3271](https://github.com/tidyverse/ggplot2/issues/3271))

- Aesthetics listed in `geom_*()` and `stat_*()` layers now point to
  relevant documentation ([@teunbrand](https://github.com/teunbrand),
  [\#5123](https://github.com/tidyverse/ggplot2/issues/5123)).

- The new argument `axes` in
  [`facet_grid()`](https://ggplot2.tidyverse.org/dev/reference/facet_grid.md)
  and
  [`facet_wrap()`](https://ggplot2.tidyverse.org/dev/reference/facet_wrap.md)
  controls the display of axes at interior panel positions.
  Additionally, the `axis.labels` argument can be used to only draw tick
  marks or fully labelled axes
  ([@teunbrand](https://github.com/teunbrand),
  [\#4064](https://github.com/tidyverse/ggplot2/issues/4064)).

- [`coord_polar()`](https://ggplot2.tidyverse.org/dev/reference/coord_radial.md)
  can have free scales in facets
  ([@teunbrand](https://github.com/teunbrand),
  [\#2815](https://github.com/tidyverse/ggplot2/issues/2815)).

- The
  [`get_guide_data()`](https://ggplot2.tidyverse.org/dev/reference/get_guide_data.md)
  function can be used to extract position and label information from
  the plot ([\#5004](https://github.com/tidyverse/ggplot2/issues/5004)).

- Improve performance of layers without positional scales
  ([@zeehio](https://github.com/zeehio),
  [\#4990](https://github.com/tidyverse/ggplot2/issues/4990))

- More informative error for mismatched
  `direction`/`theme(legend.direction = ...)` arguments
  ([\#4364](https://github.com/tidyverse/ggplot2/issues/4364),
  [\#4930](https://github.com/tidyverse/ggplot2/issues/4930)).

### Bug fixes

- Fixed regression in
  [`guide_legend()`](https://ggplot2.tidyverse.org/dev/reference/guide_legend.md)
  where the `linewidth` key size wasn’t adapted to the width of the
  lines ([\#5160](https://github.com/tidyverse/ggplot2/issues/5160)).

- In
  [`guide_bins()`](https://ggplot2.tidyverse.org/dev/reference/guide_bins.md),
  the title no longer arbitrarily becomes offset from the guide when it
  has long labels.

- [`guide_colourbar()`](https://ggplot2.tidyverse.org/dev/reference/guide_colourbar.md)
  and
  [`guide_coloursteps()`](https://ggplot2.tidyverse.org/dev/reference/guide_coloursteps.md)
  merge properly when one of the aesthetics is dropped
  ([\#5324](https://github.com/tidyverse/ggplot2/issues/5324)).

- When using `geom_dotplot(binaxis = "x")` with a discrete y-variable,
  dots are now stacked from the y-position rather than from 0
  ([@teunbrand](https://github.com/teunbrand),
  [\#5462](https://github.com/tidyverse/ggplot2/issues/5462))

- [`stat_count()`](https://ggplot2.tidyverse.org/dev/reference/geom_bar.md)
  treats `x` as unique in the same manner
  [`unique()`](https://rdrr.io/r/base/unique.html) does
  ([\#4609](https://github.com/tidyverse/ggplot2/issues/4609)).

- The plot’s title, subtitle and caption now obey horizontal text
  margins ([\#5533](https://github.com/tidyverse/ggplot2/issues/5533)).

- Contour functions will not fail when `options("OutDec")` is not `.`
  ([@eliocamp](https://github.com/eliocamp),
  [\#5555](https://github.com/tidyverse/ggplot2/issues/5555)).

- Lines where `linewidth = NA` are now dropped in
  [`geom_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md)
  ([\#5204](https://github.com/tidyverse/ggplot2/issues/5204)).

- [`ggsave()`](https://ggplot2.tidyverse.org/dev/reference/ggsave.md) no
  longer sometimes creates new directories, which is now controlled by
  the new `create.dir` argument
  ([\#5489](https://github.com/tidyverse/ggplot2/issues/5489)).

- Legend titles no longer take up space if they’ve been removed by
  setting `legend.title = element_blank()`
  ([@teunbrand](https://github.com/teunbrand),
  [\#3587](https://github.com/tidyverse/ggplot2/issues/3587)).

- [`resolution()`](https://ggplot2.tidyverse.org/dev/reference/resolution.md)
  has a small tolerance, preventing spuriously small resolutions due to
  rounding errors ([@teunbrand](https://github.com/teunbrand),
  [\#2516](https://github.com/tidyverse/ggplot2/issues/2516)).

- [`stage()`](https://ggplot2.tidyverse.org/dev/reference/aes_eval.md)
  now works correctly, even with aesthetics that do not have scales
  ([\#5408](https://github.com/tidyverse/ggplot2/issues/5408))

- [`stat_ydensity()`](https://ggplot2.tidyverse.org/dev/reference/geom_violin.md)
  with incomplete groups calculates the default `width` parameter more
  stably ([@teunbrand](https://github.com/teunbrand),
  [\#5396](https://github.com/tidyverse/ggplot2/issues/5396))

- The `size` argument in
  [`annotation_logticks()`](https://ggplot2.tidyverse.org/dev/reference/annotation_logticks.md)
  has been deprecated in favour of the `linewidth` argument
  ([\#5292](https://github.com/tidyverse/ggplot2/issues/5292)).

- Binned scales now treat `NA`s in limits the same way continuous scales
  do ([\#5355](https://github.com/tidyverse/ggplot2/issues/5355)).

- Binned scales work better with `trans = "reverse"`
  ([\#5355](https://github.com/tidyverse/ggplot2/issues/5355)).

- Integers are once again valid input to theme arguments that expect
  numeric input ([@teunbrand](https://github.com/teunbrand),
  [\#5369](https://github.com/tidyverse/ggplot2/issues/5369))

- Legends in `scale_*_manual()` can show `NA` values again when the
  `values` is a named vector
  ([@teunbrand](https://github.com/teunbrand),
  [\#5214](https://github.com/tidyverse/ggplot2/issues/5214),
  [\#5286](https://github.com/tidyverse/ggplot2/issues/5286)).

- Fixed bug in
  [`coord_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md)
  where graticule lines didn’t obey `panel.grid.major`’s linewidth
  setting ([@teunbrand](https://github.com/teunbrand),
  [\#5179](https://github.com/tidyverse/ggplot2/issues/5179))

- Fixed bug in
  [`annotation_logticks()`](https://ggplot2.tidyverse.org/dev/reference/annotation_logticks.md)
  when no suitable tick positions could be found
  ([@teunbrand](https://github.com/teunbrand),
  [\#5248](https://github.com/tidyverse/ggplot2/issues/5248)).

- The default width of
  [`geom_bar()`](https://ggplot2.tidyverse.org/dev/reference/geom_bar.md)
  is now based on panel-wise resolution of the data, rather than global
  resolution ([@teunbrand](https://github.com/teunbrand),
  [\#4336](https://github.com/tidyverse/ggplot2/issues/4336)).

- [`stat_align()`](https://ggplot2.tidyverse.org/dev/reference/geom_ribbon.md)
  is now applied per panel instead of globally, preventing issues when
  facets have different ranges
  ([@teunbrand](https://github.com/teunbrand),
  [\#5227](https://github.com/tidyverse/ggplot2/issues/5227)).

- A stacking bug in
  [`stat_align()`](https://ggplot2.tidyverse.org/dev/reference/geom_ribbon.md)
  was fixed ([@teunbrand](https://github.com/teunbrand),
  [\#5176](https://github.com/tidyverse/ggplot2/issues/5176)).

- [`stat_contour()`](https://ggplot2.tidyverse.org/dev/reference/geom_contour.md)
  and
  [`stat_contour_filled()`](https://ggplot2.tidyverse.org/dev/reference/geom_contour.md)
  now warn about and remove duplicated coordinates
  ([@teunbrand](https://github.com/teunbrand),
  [\#5215](https://github.com/tidyverse/ggplot2/issues/5215)).

- [`guide_coloursteps()`](https://ggplot2.tidyverse.org/dev/reference/guide_coloursteps.md)
  and
  [`guide_bins()`](https://ggplot2.tidyverse.org/dev/reference/guide_bins.md)
  sort breaks
  ([\#5152](https://github.com/tidyverse/ggplot2/issues/5152)).

### Internal changes

- The `ScaleContinuous$get_breaks()` method no longer censors the
  computed breaks.

- The ggplot object now contains `$layout` which points to the `Layout`
  ggproto object and will be used by the `ggplot_build.ggplot` method.
  This was exposed so that package developers may extend the behaviour
  of the `Layout` ggproto object without needing to develop an entirely
  new `ggplot_build` method ([@jtlandis](https://github.com/jtlandis),
  [\#5077](https://github.com/tidyverse/ggplot2/issues/5077)).

- Guide building is now part of
  [`ggplot_build()`](https://ggplot2.tidyverse.org/dev/reference/ggplot_build.md)
  instead of
  [`ggplot_gtable()`](https://ggplot2.tidyverse.org/dev/reference/ggplot_gtable.md)
  to allow guides to observe unmapped data
  ([\#5483](https://github.com/tidyverse/ggplot2/issues/5483)).

- The `titleGrob()` function has been refactored to be faster and less
  complicated.

- The `scales_*()` functions related to managing the `<ScalesList>`
  class have been implemented as methods in the `<ScalesList>` class,
  rather than stray functions
  ([\#1310](https://github.com/tidyverse/ggplot2/issues/1310)).

## ggplot2 3.4.4

CRAN release: 2023-10-12

This hotfix release adapts to a change in r-devel’s
[`base::is.atomic()`](https://rdrr.io/r/base/is.recursive.html) and the
upcoming retirement of maptools.

- [`fortify()`](https://ggplot2.tidyverse.org/dev/reference/fortify.md)
  for sp objects (e.g., `SpatialPolygonsDataFrame`) is now deprecated
  and will be removed soon in support of [the upcoming retirement of
  rgdal, rgeos, and
  maptools](https://r-spatial.org/r/2023/05/15/evolution4.html). In
  advance of the whole removal,
  `fortify(<SpatialPolygonsDataFrame>, region = ...)` no longer works as
  of this version
  ([@yutannihilation](https://github.com/yutannihilation),
  [\#5244](https://github.com/tidyverse/ggplot2/issues/5244)).

## ggplot2 3.4.3

CRAN release: 2023-08-14

This hotfix release addresses a version comparison change in r-devel.
There are no user-facing or breaking changes.

## ggplot2 3.4.2

CRAN release: 2023-04-03

This is a hotfix release anticipating changes in r-devel, but folds in
upkeep changes and a few bug fixes as well.

### Minor improvements

- Various type checks and their messages have been standardised
  ([@teunbrand](https://github.com/teunbrand),
  [\#4834](https://github.com/tidyverse/ggplot2/issues/4834)).

- ggplot2 now uses
  [`scales::DiscreteRange`](https://scales.r-lib.org/reference/Range.html)
  and
  [`scales::ContinuousRange`](https://scales.r-lib.org/reference/Range.html),
  which are available to write scale extensions from scratch
  ([@teunbrand](https://github.com/teunbrand),
  [\#2710](https://github.com/tidyverse/ggplot2/issues/2710)).

- The
  [`layer_data()`](https://ggplot2.tidyverse.org/dev/reference/ggplot_build.md),
  [`layer_scales()`](https://ggplot2.tidyverse.org/dev/reference/ggplot_build.md)
  and
  [`layer_grob()`](https://ggplot2.tidyverse.org/dev/reference/ggplot_build.md)
  now have the default `plot = last_plot()`
  ([@teunbrand](https://github.com/teunbrand),
  [\#5166](https://github.com/tidyverse/ggplot2/issues/5166)).

- The
  [`datetime_scale()`](https://ggplot2.tidyverse.org/dev/reference/datetime_scale.md)
  scale constructor is now exported for use in extension packages
  ([@teunbrand](https://github.com/teunbrand),
  [\#4701](https://github.com/tidyverse/ggplot2/issues/4701)).

### Bug fixes

- [`update_geom_defaults()`](https://ggplot2.tidyverse.org/dev/reference/update_defaults.md)
  and
  [`update_stat_defaults()`](https://ggplot2.tidyverse.org/dev/reference/update_defaults.md)
  now return properly classed objects and have updated docs
  ([@dkahle](https://github.com/dkahle),
  [\#5146](https://github.com/tidyverse/ggplot2/issues/5146)).

- For the purposes of checking required or non-missing aesthetics,
  character vectors are no longer considered non-finite
  ([@teunbrand](https://github.com/teunbrand),
  [@4284](https://github.com/4284)).

- [`annotation_logticks()`](https://ggplot2.tidyverse.org/dev/reference/annotation_logticks.md)
  skips drawing ticks when the scale range is non-finite instead of
  throwing an error ([@teunbrand](https://github.com/teunbrand),
  [\#5229](https://github.com/tidyverse/ggplot2/issues/5229)).

- Fixed spurious warnings when the `weight` was used in
  [`stat_bin_2d()`](https://ggplot2.tidyverse.org/dev/reference/geom_bin_2d.md),
  [`stat_boxplot()`](https://ggplot2.tidyverse.org/dev/reference/geom_boxplot.md),
  [`stat_contour()`](https://ggplot2.tidyverse.org/dev/reference/geom_contour.md),
  [`stat_bin_hex()`](https://ggplot2.tidyverse.org/dev/reference/geom_hex.md)
  and
  [`stat_quantile()`](https://ggplot2.tidyverse.org/dev/reference/geom_quantile.md)
  ([@teunbrand](https://github.com/teunbrand),
  [\#5216](https://github.com/tidyverse/ggplot2/issues/5216)).

- To prevent changing the plotting order,
  [`stat_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md) is
  now computed per panel instead of per group
  ([@teunbrand](https://github.com/teunbrand),
  [\#4340](https://github.com/tidyverse/ggplot2/issues/4340)).

- Fixed bug in
  [`coord_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md)
  where graticule lines didn’t obey `panel.grid.major`’s linewidth
  setting ([@teunbrand](https://github.com/teunbrand),
  [\#5179](https://github.com/tidyverse/ggplot2/issues/5179)).

- [`geom_text()`](https://ggplot2.tidyverse.org/dev/reference/geom_text.md)
  drops observations where `angle = NA` instead of throwing an error
  ([@teunbrand](https://github.com/teunbrand),
  [\#2757](https://github.com/tidyverse/ggplot2/issues/2757)).

## ggplot2 3.4.1

CRAN release: 2023-02-10

This is a small release focusing on fixing regressions in the 3.4.0
release and minor polishes.

### Breaking changes

- The computed variable `y` in
  [`stat_ecdf()`](https://ggplot2.tidyverse.org/dev/reference/stat_ecdf.md)
  has been superseded by `ecdf` to prevent incorrect scale
  transformations ([@teunbrand](https://github.com/teunbrand),
  [\#5113](https://github.com/tidyverse/ggplot2/issues/5113) and
  [\#5112](https://github.com/tidyverse/ggplot2/issues/5112)).

### New features

- Added
  [`scale_linewidth_manual()`](https://ggplot2.tidyverse.org/dev/reference/scale_manual.md)
  and
  [`scale_linewidth_identity()`](https://ggplot2.tidyverse.org/dev/reference/scale_identity.md)
  to support the `linewidth` aesthetic
  ([@teunbrand](https://github.com/teunbrand),
  [\#5050](https://github.com/tidyverse/ggplot2/issues/5050)).

- [`ggsave()`](https://ggplot2.tidyverse.org/dev/reference/ggsave.md)
  warns when multiple `filename`s are given, and only writes to the
  first file ([@teunbrand](https://github.com/teunbrand),
  [\#5114](https://github.com/tidyverse/ggplot2/issues/5114)).

### Bug fixes

- Fixed a regression in
  [`geom_hex()`](https://ggplot2.tidyverse.org/dev/reference/geom_hex.md)
  where aesthetics were replicated across bins
  ([@thomasp85](https://github.com/thomasp85),
  [\#5037](https://github.com/tidyverse/ggplot2/issues/5037) and
  [\#5044](https://github.com/tidyverse/ggplot2/issues/5044)).

- Using two ordered factors as facetting variables in
  `facet_grid(..., as.table = FALSE)` now throws a warning instead of an
  error ([@teunbrand](https://github.com/teunbrand),
  [\#5109](https://github.com/tidyverse/ggplot2/issues/5109)).

- Fixed misbehaviour of
  [`draw_key_boxplot()`](https://ggplot2.tidyverse.org/dev/reference/draw_key.md)
  and
  [`draw_key_crossbar()`](https://ggplot2.tidyverse.org/dev/reference/draw_key.md)
  with skewed key aspect ratio
  ([@teunbrand](https://github.com/teunbrand),
  [\#5082](https://github.com/tidyverse/ggplot2/issues/5082)).

- Fixed spurious warning when `weight` aesthetic was used in
  [`stat_smooth()`](https://ggplot2.tidyverse.org/dev/reference/geom_smooth.md)
  ([@teunbrand](https://github.com/teunbrand) based on
  [@clauswilke](https://github.com/clauswilke)’s suggestion,
  [\#5053](https://github.com/tidyverse/ggplot2/issues/5053)).

- The `lwd` alias is now correctly replaced by `linewidth` instead of
  `size` ([@teunbrand](https://github.com/teunbrand) based on
  [@clauswilke](https://github.com/clauswilke)’s suggestion
  [\#5051](https://github.com/tidyverse/ggplot2/issues/5051)).

- Fixed a regression in `Coord$train_panel_guides()` where names of
  guides were dropped ([@maxsutton](https://github.com/maxsutton),
  [\#5063](https://github.com/tidyverse/ggplot2/issues/5063)).

In binned scales:

- Automatic breaks should no longer be out-of-bounds, and automatic
  limits are adjusted to include breaks
  ([@teunbrand](https://github.com/teunbrand),
  [\#5082](https://github.com/tidyverse/ggplot2/issues/5082)).

- Zero-range limits no longer throw an error and are treated akin to
  continuous scales with zero-range limits
  ([@teunbrand](https://github.com/teunbrand),
  [\#5066](https://github.com/tidyverse/ggplot2/issues/5066)).

- The `trans = "date"` and `trans = "time"` transformations were made
  compatible ([@teunbrand](https://github.com/teunbrand),
  [\#4217](https://github.com/tidyverse/ggplot2/issues/4217)).

## ggplot2 3.4.0

CRAN release: 2022-11-04

This is a minor release focusing on tightening up the internals and
ironing out some inconsistencies in the API. The biggest change is the
addition of the `linewidth` aesthetic that takes of sizing the width of
any line from `size`. This change, while attempting to be as
non-breaking as possible, has the potential to change the look of some
of your plots.

Other notable changes is a complete redo of the error and warning
messaging in ggplot2 using the cli package. Messaging is now better
contextualised and it should be easier to identify which layer an error
is coming from. Last, we have now made the switch to using the vctrs
package internally which means that support for vctrs classes as
variables should improve, along with some small gains in rendering
speed.

### Breaking changes

- A `linewidth` aesthetic has been introduced and supersedes the `size`
  aesthetic for scaling the width of lines in line based geoms. `size`
  will remain functioning but deprecated for these geoms and it is
  recommended to update all code to reflect the new aesthetic. For geoms
  that have *both* point sizing and linewidth sizing
  ([`geom_pointrange()`](https://ggplot2.tidyverse.org/dev/reference/geom_linerange.md)
  and `geom_sf`) `size` now **only** refers to sizing of points which
  can leads to a visual change in old code
  ([@thomasp85](https://github.com/thomasp85),
  [\#3672](https://github.com/tidyverse/ggplot2/issues/3672))

- The default line width for polygons in
  [`geom_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md)
  have been decreased to 0.2 to reflect that this is usually used for
  demarking borders where a thinner line is better suited. This change
  was made since we already induced a visual change in
  [`geom_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md)
  with the introduction of the `linewidth` aesthetic.

- The dot-dot notation (`..var..`) and
  [`stat()`](https://ggplot2.tidyverse.org/dev/reference/aes_eval.md),
  which have been superseded by
  [`after_stat()`](https://ggplot2.tidyverse.org/dev/reference/aes_eval.md),
  are now formally deprecated
  ([@yutannihilation](https://github.com/yutannihilation),
  [\#3693](https://github.com/tidyverse/ggplot2/issues/3693)).

- [`qplot()`](https://ggplot2.tidyverse.org/dev/reference/qplot.md) is
  now formally deprecated
  ([@yutannihilation](https://github.com/yutannihilation),
  [\#3956](https://github.com/tidyverse/ggplot2/issues/3956)).

- [`stage()`](https://ggplot2.tidyverse.org/dev/reference/aes_eval.md)
  now properly refers to the values without scale transformations for
  the stage of `after_stat`. If your code requires the scaled version of
  the values for some reason, you have to apply the same transformation
  by yourself, e.g. [`sqrt()`](https://rdrr.io/r/base/MathFun.html) for
  `scale_{x,y}_sqrt()`
  ([@yutannihilation](https://github.com/yutannihilation) and
  [@teunbrand](https://github.com/teunbrand),
  [\#4155](https://github.com/tidyverse/ggplot2/issues/4155)).

- Use [`rlang::hash()`](https://rlang.r-lib.org/reference/hash.html)
  instead of
  [`digest::digest()`](https://eddelbuettel.github.io/digest/man/digest.html).
  This update may lead to changes in the automatic sorting of legends.
  In order to enforce a specific legend order use the `order` argument
  in the guide. ([@thomasp85](https://github.com/thomasp85),
  [\#4458](https://github.com/tidyverse/ggplot2/issues/4458))

- referring to `x` in backquoted expressions with
  [`label_bquote()`](https://ggplot2.tidyverse.org/dev/reference/label_bquote.md)
  is no longer possible.

- The `ticks.linewidth` and `frame.linewidth` parameters of
  [`guide_colourbar()`](https://ggplot2.tidyverse.org/dev/reference/guide_colourbar.md)
  are now multiplied with `.pt` like elsewhere in ggplot2. It can cause
  visual changes when these arguments are not the defaults and these
  changes can be restored to their previous behaviour by adding `/ .pt`
  ([@teunbrand](https://github.com/teunbrand)
  [\#4314](https://github.com/tidyverse/ggplot2/issues/4314)).

- `scale_*_viridis_b()` now uses the full range of the viridis scales
  ([@gregleleu](https://github.com/gregleleu),
  [\#4737](https://github.com/tidyverse/ggplot2/issues/4737))

### New features

- [`geom_col()`](https://ggplot2.tidyverse.org/dev/reference/geom_bar.md)
  and
  [`geom_bar()`](https://ggplot2.tidyverse.org/dev/reference/geom_bar.md)
  gain a new `just` argument. This is set to `0.5` by default; use
  `just = 0`/`just = 1` to place columns on the left/right of the axis
  breaks. ([@wurli](https://github.com/wurli),
  [\#4899](https://github.com/tidyverse/ggplot2/issues/4899))

- [`geom_density()`](https://ggplot2.tidyverse.org/dev/reference/geom_density.md)
  and
  [`stat_density()`](https://ggplot2.tidyverse.org/dev/reference/geom_density.md)
  now support `bounds` argument to estimate density with boundary
  correction ([@echasnovski](https://github.com/echasnovski),
  [\#4013](https://github.com/tidyverse/ggplot2/issues/4013)).

- ggplot now checks during statistical transformations whether any data
  columns were dropped and warns about this. If stats intend to drop
  data columns they can declare them in the new field `dropped_aes`.
  ([@clauswilke](https://github.com/clauswilke),
  [\#3250](https://github.com/tidyverse/ggplot2/issues/3250))

- `...` supports
  [`rlang::list2`](https://rlang.r-lib.org/reference/list2.html) dynamic
  dots in all public functions. ([@mone27](https://github.com/mone27),
  [\#4764](https://github.com/tidyverse/ggplot2/issues/4764))

- [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md) now
  has a `strip.clip` argument, that can be set to `"off"` to prevent the
  clipping of strip text and background borders
  ([@teunbrand](https://github.com/teunbrand),
  [\#4118](https://github.com/tidyverse/ggplot2/issues/4118))

- [`geom_contour()`](https://ggplot2.tidyverse.org/dev/reference/geom_contour.md)
  now accepts a function in the `breaks` argument
  ([@eliocamp](https://github.com/eliocamp),
  [\#4652](https://github.com/tidyverse/ggplot2/issues/4652)).

### Minor improvements and bug fixes

- Fix a bug in
  [`position_jitter()`](https://ggplot2.tidyverse.org/dev/reference/position_jitter.md)
  where infinity values were dropped
  ([@javlon](https://github.com/javlon),
  [\#4790](https://github.com/tidyverse/ggplot2/issues/4790)).

- [`geom_linerange()`](https://ggplot2.tidyverse.org/dev/reference/geom_linerange.md)
  now respects the `na.rm` argument
  ([\#4927](https://github.com/tidyverse/ggplot2/issues/4927),
  [@thomasp85](https://github.com/thomasp85))

- Improve the support for
  [`guide_axis()`](https://ggplot2.tidyverse.org/dev/reference/guide_axis.md)
  on
  [`coord_trans()`](https://ggplot2.tidyverse.org/dev/reference/coord_transform.md)
  ([@yutannihilation](https://github.com/yutannihilation),
  [\#3959](https://github.com/tidyverse/ggplot2/issues/3959))

- Added
  [`stat_align()`](https://ggplot2.tidyverse.org/dev/reference/geom_ribbon.md)
  to align data without common x-coordinates prior to stacking. This is
  now the default stat for
  [`geom_area()`](https://ggplot2.tidyverse.org/dev/reference/geom_ribbon.md)
  ([@thomasp85](https://github.com/thomasp85),
  [\#4850](https://github.com/tidyverse/ggplot2/issues/4850))

- Fix a bug in
  [`stat_contour_filled()`](https://ggplot2.tidyverse.org/dev/reference/geom_contour.md)
  where break value differences below a certain number of digits would
  cause the computations to fail
  ([@thomasp85](https://github.com/thomasp85),
  [\#4874](https://github.com/tidyverse/ggplot2/issues/4874))

- Secondary axis ticks are now positioned more precisely, removing small
  visual artefacts with alignment between grid and ticks
  ([@thomasp85](https://github.com/thomasp85),
  [\#3576](https://github.com/tidyverse/ggplot2/issues/3576))

- Improve `stat_function` documentation regarding `xlim` argument.
  ([@92amartins](https://github.com/92amartins),
  [\#4474](https://github.com/tidyverse/ggplot2/issues/4474))

- Fix various issues with how `labels`, `breaks`, `limits`, and
  `show.limits` interact in the different binning guides
  ([@thomasp85](https://github.com/thomasp85),
  [\#4831](https://github.com/tidyverse/ggplot2/issues/4831))

- Automatic break calculation now squishes the scale limits to the
  domain of the transformation. This allows `scale_{x/y}_sqrt()` to find
  breaks at 0 when appropriate
  ([@teunbrand](https://github.com/teunbrand),
  [\#980](https://github.com/tidyverse/ggplot2/issues/980)).

- Using multiple modified aesthetics correctly will no longer trigger
  warnings. If used incorrectly, the warning will now report the
  duplicated aesthetic instead of `NA`
  ([@teunbrand](https://github.com/teunbrand),
  [\#4707](https://github.com/tidyverse/ggplot2/issues/4707)).

- [`aes()`](https://ggplot2.tidyverse.org/dev/reference/aes.md) now
  supports the `!!!` operator in its first two arguments
  ([\#2675](https://github.com/tidyverse/ggplot2/issues/2675)). Thanks
  to [@yutannihilation](https://github.com/yutannihilation) and
  [@teunbrand](https://github.com/teunbrand) for draft implementations.

- Require rlang \>= 1.0.0 ([@billybarc](https://github.com/billybarc),
  [\#4797](https://github.com/tidyverse/ggplot2/issues/4797))

- [`geom_violin()`](https://ggplot2.tidyverse.org/dev/reference/geom_violin.md)
  no longer issues “collapsing to unique ‘x’ values” warning
  ([@bersbersbers](https://github.com/bersbersbers),
  [\#4455](https://github.com/tidyverse/ggplot2/issues/4455))

- [`annotate()`](https://ggplot2.tidyverse.org/dev/reference/annotate.md)
  now documents unsupported geoms
  ([`geom_abline()`](https://ggplot2.tidyverse.org/dev/reference/geom_abline.md),
  [`geom_hline()`](https://ggplot2.tidyverse.org/dev/reference/geom_abline.md)
  and
  [`geom_vline()`](https://ggplot2.tidyverse.org/dev/reference/geom_abline.md)),
  and warns when they are requested
  ([@mikmart](https://github.com/mikmart),
  [\#4719](https://github.com/tidyverse/ggplot2/issues/4719))

- `presidential` dataset now includes Trump’s presidency
  ([@bkmgit](https://github.com/bkmgit),
  [\#4703](https://github.com/tidyverse/ggplot2/issues/4703)).

- [`position_stack()`](https://ggplot2.tidyverse.org/dev/reference/position_stack.md)
  now works fully with
  [`geom_text()`](https://ggplot2.tidyverse.org/dev/reference/geom_text.md)
  ([@thomasp85](https://github.com/thomasp85),
  [\#4367](https://github.com/tidyverse/ggplot2/issues/4367))

- [`geom_tile()`](https://ggplot2.tidyverse.org/dev/reference/geom_tile.md)
  now correctly recognises missing data in `xmin`, `xmax`, `ymin`, and
  `ymax` ([@thomasp85](https://github.com/thomasp85) and
  [@sigmapi](https://github.com/sigmapi),
  [\#4495](https://github.com/tidyverse/ggplot2/issues/4495))

- [`geom_hex()`](https://ggplot2.tidyverse.org/dev/reference/geom_hex.md)
  will now use the binwidth from
  [`stat_bin_hex()`](https://ggplot2.tidyverse.org/dev/reference/geom_hex.md)
  if present, instead of deriving it
  ([@thomasp85](https://github.com/thomasp85),
  [\#4580](https://github.com/tidyverse/ggplot2/issues/4580))

- [`geom_hex()`](https://ggplot2.tidyverse.org/dev/reference/geom_hex.md)
  now works on non-linear coordinate systems
  ([@thomasp85](https://github.com/thomasp85))

- Fixed a bug throwing errors when trying to render an empty plot with
  secondary axes ([@thomasp85](https://github.com/thomasp85),
  [\#4509](https://github.com/tidyverse/ggplot2/issues/4509))

- Axes are now added correctly in
  [`facet_wrap()`](https://ggplot2.tidyverse.org/dev/reference/facet_wrap.md)
  when `as.table = FALSE` ([@thomasp85](https://github.com/thomasp85),
  [\#4553](https://github.com/tidyverse/ggplot2/issues/4553))

- Better compatibility of custom device functions in
  [`ggsave()`](https://ggplot2.tidyverse.org/dev/reference/ggsave.md)
  ([@thomasp85](https://github.com/thomasp85),
  [\#4539](https://github.com/tidyverse/ggplot2/issues/4539))

- Binning scales are now more resilient to calculated limits that ends
  up being `NaN` after transformations
  ([@thomasp85](https://github.com/thomasp85),
  [\#4510](https://github.com/tidyverse/ggplot2/issues/4510))

- Strip padding in
  [`facet_grid()`](https://ggplot2.tidyverse.org/dev/reference/facet_grid.md)
  is now only in effect if `strip.placement = "outside"` *and* an axis
  is present between the strip and the panel
  ([@thomasp85](https://github.com/thomasp85),
  [\#4610](https://github.com/tidyverse/ggplot2/issues/4610))

- Aesthetics of length 1 are now recycled to 0 if the length of the data
  is 0 ([@thomasp85](https://github.com/thomasp85),
  [\#4588](https://github.com/tidyverse/ggplot2/issues/4588))

- Setting `size = NA` will no longer cause
  [`guide_legend()`](https://ggplot2.tidyverse.org/dev/reference/guide_legend.md)
  to error ([@thomasp85](https://github.com/thomasp85),
  [\#4559](https://github.com/tidyverse/ggplot2/issues/4559))

- Setting `stroke` to `NA` in
  [`geom_point()`](https://ggplot2.tidyverse.org/dev/reference/geom_point.md)
  will no longer impair the sizing of the points
  ([@thomasp85](https://github.com/thomasp85),
  [\#4624](https://github.com/tidyverse/ggplot2/issues/4624))

- [`stat_bin_2d()`](https://ggplot2.tidyverse.org/dev/reference/geom_bin_2d.md)
  now correctly recognises the `weight` aesthetic
  ([@thomasp85](https://github.com/thomasp85),
  [\#4646](https://github.com/tidyverse/ggplot2/issues/4646))

- All geoms now have consistent exposure of linejoin and lineend
  parameters, and the guide keys will now respect these settings
  ([@thomasp85](https://github.com/thomasp85),
  [\#4653](https://github.com/tidyverse/ggplot2/issues/4653))

- [`geom_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md) now
  respects `arrow` parameter for lines
  ([@jakeruss](https://github.com/jakeruss),
  [\#4659](https://github.com/tidyverse/ggplot2/issues/4659))

- Updated documentation for `print.ggplot` to reflect that it returns
  the original plot, not the result of
  [`ggplot_build()`](https://ggplot2.tidyverse.org/dev/reference/ggplot_build.md).
  ([@r2evans](https://github.com/r2evans),
  [\#4390](https://github.com/tidyverse/ggplot2/issues/4390))

- `scale_*_manual()` no longer displays extra legend keys, or changes
  their order, when a named `values` argument has more items than the
  data. To display all `values` on the legend instead, use
  `scale_*_manual(values = vals, limits = names(vals))`.
  ([@teunbrand](https://github.com/teunbrand),
  [@banfai](https://github.com/banfai),
  [\#4511](https://github.com/tidyverse/ggplot2/issues/4511),
  [\#4534](https://github.com/tidyverse/ggplot2/issues/4534))

- Updated documentation for
  [`geom_contour()`](https://ggplot2.tidyverse.org/dev/reference/geom_contour.md)
  to correctly reflect argument precedence between `bins` and
  `binwidth`. ([@eliocamp](https://github.com/eliocamp),
  [\#4651](https://github.com/tidyverse/ggplot2/issues/4651))

- Dots in
  [`geom_dotplot()`](https://ggplot2.tidyverse.org/dev/reference/geom_dotplot.md)
  are now correctly aligned to the baseline when `stackratio != 1` and
  `stackdir != "up"` ([@mjskay](https://github.com/mjskay),
  [\#4614](https://github.com/tidyverse/ggplot2/issues/4614))

- Key glyphs for
  [`geom_boxplot()`](https://ggplot2.tidyverse.org/dev/reference/geom_boxplot.md),
  [`geom_crossbar()`](https://ggplot2.tidyverse.org/dev/reference/geom_linerange.md),
  [`geom_pointrange()`](https://ggplot2.tidyverse.org/dev/reference/geom_linerange.md),
  and
  [`geom_linerange()`](https://ggplot2.tidyverse.org/dev/reference/geom_linerange.md)
  are now orientation-aware ([@mjskay](https://github.com/mjskay),
  [\#4732](https://github.com/tidyverse/ggplot2/issues/4732))

- Updated documentation for
  [`geom_smooth()`](https://ggplot2.tidyverse.org/dev/reference/geom_smooth.md)
  to more clearly describe effects of the `fullrange` parameter
  ([@thoolihan](https://github.com/thoolihan),
  [\#4399](https://github.com/tidyverse/ggplot2/issues/4399)).

## ggplot2 3.3.6

CRAN release: 2022-05-03

This is a very small release only applying an internal change to comply
with R 4.2 and its deprecation of
[`default.stringsAsFactors()`](https://rdrr.io/r/base/base-defunct.html).
There are no user facing changes and no breaking changes.

## ggplot2 3.3.5

CRAN release: 2021-06-25

This is a very small release focusing on fixing a couple of untenable
issues that surfaced with the 3.3.4 release

- Revert changes made in
  [\#4434](https://github.com/tidyverse/ggplot2/issues/4434) (apply
  transform to intercept in
  [`geom_abline()`](https://ggplot2.tidyverse.org/dev/reference/geom_abline.md))
  as it introduced undesirable issues far worse than the bug it fixed
  ([@thomasp85](https://github.com/thomasp85),
  [\#4514](https://github.com/tidyverse/ggplot2/issues/4514))
- Fixes an issue in
  [`ggsave()`](https://ggplot2.tidyverse.org/dev/reference/ggsave.md)
  when producing emf/wmf files
  ([@yutannihilation](https://github.com/yutannihilation),
  [\#4521](https://github.com/tidyverse/ggplot2/issues/4521))
- Warn when grDevices specific arguments are passed to ragg devices
  ([@thomasp85](https://github.com/thomasp85),
  [\#4524](https://github.com/tidyverse/ggplot2/issues/4524))
- Fix an issue where
  [`coord_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md)
  was reporting that it is non-linear even when data is provided in
  projected coordinates ([@clauswilke](https://github.com/clauswilke),
  [\#4527](https://github.com/tidyverse/ggplot2/issues/4527))

## ggplot2 3.3.4

CRAN release: 2021-06-16

This is a larger patch release fixing a huge number of bugs and
introduces a small selection of feature refinements.

### Features

- Alt-text can now be added to a plot using the `alt` label, i.e
  `+ labs(alt = ...)`. Currently this alt text is not automatically
  propagated, but we plan to integrate into Shiny, RMarkdown, and other
  tools in the future. ([@thomasp85](https://github.com/thomasp85),
  [\#4477](https://github.com/tidyverse/ggplot2/issues/4477))

- Add support for the BrailleR package for creating descriptions of the
  plot when rendered ([@thomasp85](https://github.com/thomasp85),
  [\#4459](https://github.com/tidyverse/ggplot2/issues/4459))

- [`coord_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md)
  now has an argument `default_crs` that specifies the coordinate
  reference system (CRS) for non-sf layers and scale/coord limits. This
  argument defaults to `NULL`, which means non-sf layers are assumed to
  be in projected coordinates, as in prior ggplot2 versions. Setting
  `default_crs = sf::st_crs(4326)` provides a simple way to interpret x
  and y positions as longitude and latitude, regardless of the CRS used
  by
  [`coord_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md).
  Authors of extension packages implementing
  [`stat_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md)-like
  functionality are encouraged to look at the source code of
  [`stat_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md)’s
  `compute_group()` function to see how to provide scale-limit hints to
  [`coord_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md)
  ([@clauswilke](https://github.com/clauswilke),
  [\#3659](https://github.com/tidyverse/ggplot2/issues/3659)).

- [`ggsave()`](https://ggplot2.tidyverse.org/dev/reference/ggsave.md)
  now uses ragg to render raster output if ragg is available. It also
  handles custom devices that sets a default unit
  (e.g. [`ragg::agg_png`](https://ragg.r-lib.org/reference/agg_png.html))
  correctly ([@thomasp85](https://github.com/thomasp85),
  [\#4388](https://github.com/tidyverse/ggplot2/issues/4388))

- [`ggsave()`](https://ggplot2.tidyverse.org/dev/reference/ggsave.md)
  now returns the saved file location invisibly
  ([\#3379](https://github.com/tidyverse/ggplot2/issues/3379),
  [@eliocamp](https://github.com/eliocamp)). Note that, as a side
  effect, an unofficial hack `<ggplot object> + ggsave()` no longer
  works ([\#4513](https://github.com/tidyverse/ggplot2/issues/4513)).

- The scale arguments `limits`, `breaks`, `minor_breaks`, `labels`,
  `rescaler` and `oob` now accept purrr style lambda notation
  ([@teunbrand](https://github.com/teunbrand),
  [\#4427](https://github.com/tidyverse/ggplot2/issues/4427)). The same
  is true for
  [`as_labeller()`](https://ggplot2.tidyverse.org/dev/reference/as_labeller.md)
  (and therefore also
  [`labeller()`](https://ggplot2.tidyverse.org/dev/reference/labeller.md))
  ([@netique](https://github.com/netique),
  [\#4188](https://github.com/tidyverse/ggplot2/issues/4188)).

- Manual scales now allow named vectors passed to `values` to contain
  fewer elements than existing in the data. Elements not present in
  values will be set to `NA`
  ([@thomasp85](https://github.com/thomasp85),
  [\#3451](https://github.com/tidyverse/ggplot2/issues/3451))

- Date and datetime position scales support out-of-bounds (oob)
  arguments to control how limits affect data outside those limits
  ([@teunbrand](https://github.com/teunbrand),
  [\#4199](https://github.com/tidyverse/ggplot2/issues/4199)).

### Fixes

- Fix a bug that
  [`after_stat()`](https://ggplot2.tidyverse.org/dev/reference/aes_eval.md)
  and
  [`after_scale()`](https://ggplot2.tidyverse.org/dev/reference/aes_eval.md)
  cannot refer to aesthetics if it’s specified in the plot-global
  mapping ([@yutannihilation](https://github.com/yutannihilation),
  [\#4260](https://github.com/tidyverse/ggplot2/issues/4260)).

- Fix bug in `annotate_logticks()` that would cause an error when used
  together with
  [`coord_flip()`](https://ggplot2.tidyverse.org/dev/reference/coord_flip.md)
  ([@thomasp85](https://github.com/thomasp85),
  [\#3954](https://github.com/tidyverse/ggplot2/issues/3954))

- Fix a bug in
  [`geom_abline()`](https://ggplot2.tidyverse.org/dev/reference/geom_abline.md)
  that resulted in `intercept` not being subjected to the transformation
  of the y scale ([@thomasp85](https://github.com/thomasp85),
  [\#3741](https://github.com/tidyverse/ggplot2/issues/3741))

- Extent the range of the line created by
  [`geom_abline()`](https://ggplot2.tidyverse.org/dev/reference/geom_abline.md)
  so that line ending is not visible for large linewidths
  ([@thomasp85](https://github.com/thomasp85),
  [\#4024](https://github.com/tidyverse/ggplot2/issues/4024))

- Fix bug in
  [`geom_dotplot()`](https://ggplot2.tidyverse.org/dev/reference/geom_dotplot.md)
  where dots would be positioned wrong with `stackgroups = TRUE`
  ([@thomasp85](https://github.com/thomasp85),
  [\#1745](https://github.com/tidyverse/ggplot2/issues/1745))

- Fix calculation of confidence interval for locfit smoothing in
  [`geom_smooth()`](https://ggplot2.tidyverse.org/dev/reference/geom_smooth.md)
  ([@topepo](https://github.com/topepo),
  [\#3806](https://github.com/tidyverse/ggplot2/issues/3806))

- Fix bug in
  [`geom_text()`](https://ggplot2.tidyverse.org/dev/reference/geom_text.md)
  where `"outward"` and `"inward"` justification for some `angle` values
  was reversed ([@aphalo](https://github.com/aphalo),
  [\#4169](https://github.com/tidyverse/ggplot2/issues/4169),
  [\#4447](https://github.com/tidyverse/ggplot2/issues/4447))

- [`ggsave()`](https://ggplot2.tidyverse.org/dev/reference/ggsave.md)
  now sets the default background to match the fill value of the
  `plot.background` theme element
  ([@karawoo](https://github.com/karawoo),
  [\#4057](https://github.com/tidyverse/ggplot2/issues/4057))

- It is now deprecated to specify `guides(<scale> = FALSE)` or
  `scale_*(guide = FALSE)` to remove a guide. Please use
  `guides(<scale> = "none")` or `scale_*(guide = "none")` instead
  ([@yutannihilation](https://github.com/yutannihilation),
  [\#4097](https://github.com/tidyverse/ggplot2/issues/4097))

- Fix a bug in
  [`guide_bins()`](https://ggplot2.tidyverse.org/dev/reference/guide_bins.md)
  where keys would disappear if the guide was reversed
  ([@thomasp85](https://github.com/thomasp85),
  [\#4210](https://github.com/tidyverse/ggplot2/issues/4210))

- Fix bug in
  [`guide_coloursteps()`](https://ggplot2.tidyverse.org/dev/reference/guide_coloursteps.md)
  that would repeat the terminal bins if the breaks coincided with the
  limits of the scale ([@thomasp85](https://github.com/thomasp85),
  [\#4019](https://github.com/tidyverse/ggplot2/issues/4019))

- Make sure that default labels from default mappings doesn’t overwrite
  default labels from explicit mappings
  ([@thomasp85](https://github.com/thomasp85),
  [\#2406](https://github.com/tidyverse/ggplot2/issues/2406))

- Fix bug in
  [`labeller()`](https://ggplot2.tidyverse.org/dev/reference/labeller.md)
  where parsing was turned off if `.multiline = FALSE`
  ([@thomasp85](https://github.com/thomasp85),
  [\#4084](https://github.com/tidyverse/ggplot2/issues/4084))

- Make sure
  [`label_bquote()`](https://ggplot2.tidyverse.org/dev/reference/label_bquote.md)
  has access to the calling environment when evaluating the labels
  ([@thomasp85](https://github.com/thomasp85),
  [\#4141](https://github.com/tidyverse/ggplot2/issues/4141))

- Fix a bug in the layer implementation that introduced a new state
  after the first render which could lead to a different look when
  rendered the second time ([@thomasp85](https://github.com/thomasp85),
  [\#4204](https://github.com/tidyverse/ggplot2/issues/4204))

- Fix a bug in legend justification where justification was lost of the
  legend dimensions exceeded the available size
  ([@thomasp85](https://github.com/thomasp85),
  [\#3635](https://github.com/tidyverse/ggplot2/issues/3635))

- Fix a bug in
  [`position_dodge2()`](https://ggplot2.tidyverse.org/dev/reference/position_dodge.md)
  where `NA` values in thee data would cause an error
  ([@thomasp85](https://github.com/thomasp85),
  [\#2905](https://github.com/tidyverse/ggplot2/issues/2905))

- Make sure
  [`position_jitter()`](https://ggplot2.tidyverse.org/dev/reference/position_jitter.md)
  creates the same jittering independent of whether it is called by name
  or with constructor ([@thomasp85](https://github.com/thomasp85),
  [\#2507](https://github.com/tidyverse/ggplot2/issues/2507))

- Fix a bug in
  [`position_jitter()`](https://ggplot2.tidyverse.org/dev/reference/position_jitter.md)
  where different jitters would be applied to different position
  aesthetics of the same axis
  ([@thomasp85](https://github.com/thomasp85),
  [\#2941](https://github.com/tidyverse/ggplot2/issues/2941))

- Fix a bug in
  [`qplot()`](https://ggplot2.tidyverse.org/dev/reference/qplot.md) when
  supplying `c(NA, NA)` as axis limits
  ([@thomasp85](https://github.com/thomasp85),
  [\#4027](https://github.com/tidyverse/ggplot2/issues/4027))

- Remove cross-inheritance of default discrete colour/fill scales and
  check the type and aesthetic of function output if `type` is a
  function ([@thomasp85](https://github.com/thomasp85),
  [\#4149](https://github.com/tidyverse/ggplot2/issues/4149))

- Fix bug in `scale_[x|y]_date()` where custom breaks functions that
  resulted in fractional dates would get misaligned
  ([@thomasp85](https://github.com/thomasp85),
  [\#3965](https://github.com/tidyverse/ggplot2/issues/3965))

- Fix bug in `scale_[x|y]_datetime()` where a specified timezone would
  be ignored by the scale ([@thomasp85](https://github.com/thomasp85),
  [\#4007](https://github.com/tidyverse/ggplot2/issues/4007))

- Fix issue in
  [`sec_axis()`](https://ggplot2.tidyverse.org/dev/reference/sec_axis.md)
  that would throw warnings in the absence of any secondary breaks
  ([@thomasp85](https://github.com/thomasp85),
  [\#4368](https://github.com/tidyverse/ggplot2/issues/4368))

- [`stat_bin()`](https://ggplot2.tidyverse.org/dev/reference/geom_histogram.md)’s
  computed variable `width` is now documented
  ([\#3522](https://github.com/tidyverse/ggplot2/issues/3522)).

- [`stat_count()`](https://ggplot2.tidyverse.org/dev/reference/geom_bar.md)
  now computes width based on the full dataset instead of per group
  ([@thomasp85](https://github.com/thomasp85),
  [\#2047](https://github.com/tidyverse/ggplot2/issues/2047))

- Extended
  [`stat_ecdf()`](https://ggplot2.tidyverse.org/dev/reference/stat_ecdf.md)
  to calculate the cdf from either x or y instead from y only
  ([@jgjl](https://github.com/jgjl),
  [\#4005](https://github.com/tidyverse/ggplot2/issues/4005))

- Fix a bug in
  [`stat_summary_bin()`](https://ggplot2.tidyverse.org/dev/reference/stat_summary.md)
  where one more than the requested number of bins would be created
  ([@thomasp85](https://github.com/thomasp85),
  [\#3824](https://github.com/tidyverse/ggplot2/issues/3824))

- Only drop groups in
  [`stat_ydensity()`](https://ggplot2.tidyverse.org/dev/reference/geom_violin.md)
  when there are fewer than two data points and throw a warning
  ([@andrewwbutler](https://github.com/andrewwbutler),
  [\#4111](https://github.com/tidyverse/ggplot2/issues/4111)).

- Fixed a bug in strip assembly when theme has
  `strip.text = element_blank()` and plots are faceted with
  multi-layered strips ([@teunbrand](https://github.com/teunbrand),
  [\#4384](https://github.com/tidyverse/ggplot2/issues/4384)).

- Using `theme(aspect.ratio = ...)` together with free space in
  [`facet_grid()`](https://ggplot2.tidyverse.org/dev/reference/facet_grid.md)
  now correctly throws an error
  ([@thomasp85](https://github.com/thomasp85),
  [\#3834](https://github.com/tidyverse/ggplot2/issues/3834))

- Fixed a bug in
  [`labeller()`](https://ggplot2.tidyverse.org/dev/reference/labeller.md)
  so that `.default` is passed to
  [`as_labeller()`](https://ggplot2.tidyverse.org/dev/reference/as_labeller.md)
  when labellers are specified by naming faceting variables.
  ([@waltersom](https://github.com/waltersom),
  [\#4031](https://github.com/tidyverse/ggplot2/issues/4031))

- Updated style for example code ([@rjake](https://github.com/rjake),
  [\#4092](https://github.com/tidyverse/ggplot2/issues/4092))

- ggplot2 now requires R \>= 3.3
  ([\#4247](https://github.com/tidyverse/ggplot2/issues/4247)).

- ggplot2 now uses
  [`rlang::check_installed()`](https://rlang.r-lib.org/reference/is_installed.html)
  to check if a suggested package is installed, which will offer to
  install the package before continuing
  ([\#4375](https://github.com/tidyverse/ggplot2/issues/4375),

  1.  

- Improved error with hint when piping a `ggplot` object into a facet
  function ([\#4379](https://github.com/tidyverse/ggplot2/issues/4379),
  [@mitchelloharawild](https://github.com/mitchelloharawild)).

## ggplot2 3.3.3

CRAN release: 2020-12-30

This is a small patch release mainly intended to address changes in R
and CRAN. It further changes the licensing model of ggplot2 to an MIT
license.

- Update the ggplot2 licence to an MIT license
  ([\#4231](https://github.com/tidyverse/ggplot2/issues/4231),
  [\#4232](https://github.com/tidyverse/ggplot2/issues/4232),
  [\#4233](https://github.com/tidyverse/ggplot2/issues/4233), and
  [\#4281](https://github.com/tidyverse/ggplot2/issues/4281))

- Use vdiffr conditionally so ggplot2 can be tested on systems without
  vdiffr

- Update tests to work with the new
  [`all.equal()`](https://rdrr.io/r/base/all.equal.html) defaults in R
  \>4.0.3

- Fixed a bug that
  [`guide_bins()`](https://ggplot2.tidyverse.org/dev/reference/guide_bins.md)
  mistakenly ignore `override.aes` argument
  ([@yutannihilation](https://github.com/yutannihilation),
  [\#4085](https://github.com/tidyverse/ggplot2/issues/4085)).

## ggplot2 3.3.2

CRAN release: 2020-06-19

This is a small release focusing on fixing regressions introduced in
3.3.1.

- Added an `outside` option to
  [`annotation_logticks()`](https://ggplot2.tidyverse.org/dev/reference/annotation_logticks.md)
  that places tick marks outside of the plot bounds.
  ([\#3783](https://github.com/tidyverse/ggplot2/issues/3783),
  [@kbodwin](https://github.com/kbodwin))

- [`annotation_raster()`](https://ggplot2.tidyverse.org/dev/reference/annotation_raster.md)
  adds support for native rasters. For large rasters, native rasters
  render significantly faster than arrays
  ([@kent37](https://github.com/kent37),
  [\#3388](https://github.com/tidyverse/ggplot2/issues/3388))

- Facet strips now have dedicated position-dependent theme elements
  (`strip.text.x.top`, `strip.text.x.bottom`, `strip.text.y.left`,
  `strip.text.y.right`) that inherit from `strip.text.x` and
  `strip.text.y`, respectively. As a consequence, some theme stylings
  now need to be applied to the position-dependent elements rather than
  to the parent elements. This change was already introduced in ggplot2
  3.3.0 but not listed in the changelog.
  ([@thomasp85](https://github.com/thomasp85),
  [\#3683](https://github.com/tidyverse/ggplot2/issues/3683))

- Facets now handle layers containing no data
  ([@yutannihilation](https://github.com/yutannihilation),
  [\#3853](https://github.com/tidyverse/ggplot2/issues/3853)).

- A newly added geom
  [`geom_density_2d_filled()`](https://ggplot2.tidyverse.org/dev/reference/geom_density_2d.md)
  and associated stat
  [`stat_density_2d_filled()`](https://ggplot2.tidyverse.org/dev/reference/geom_density_2d.md)
  can draw filled density contours
  ([@clauswilke](https://github.com/clauswilke),
  [\#3846](https://github.com/tidyverse/ggplot2/issues/3846)).

- A newly added
  [`geom_function()`](https://ggplot2.tidyverse.org/dev/reference/geom_function.md)
  is now recommended to use in conjunction with/instead of
  [`stat_function()`](https://ggplot2.tidyverse.org/dev/reference/geom_function.md).
  In addition,
  [`stat_function()`](https://ggplot2.tidyverse.org/dev/reference/geom_function.md)
  now works with transformed y axes,
  e.g. [`scale_y_log10()`](https://ggplot2.tidyverse.org/dev/reference/scale_continuous.md),
  and in plots containing no other data or layers
  ([@clauswilke](https://github.com/clauswilke),
  [\#3611](https://github.com/tidyverse/ggplot2/issues/3611),
  [\#3905](https://github.com/tidyverse/ggplot2/issues/3905),
  [\#3983](https://github.com/tidyverse/ggplot2/issues/3983)).

- Fixed a bug in
  [`geom_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md)
  that caused problems with legend-type autodetection
  ([@clauswilke](https://github.com/clauswilke),
  [\#3963](https://github.com/tidyverse/ggplot2/issues/3963)).

- Support graphics devices that use the `file` argument instead of
  `fileneame` in
  [`ggsave()`](https://ggplot2.tidyverse.org/dev/reference/ggsave.md)
  ([@bwiernik](https://github.com/bwiernik),
  [\#3810](https://github.com/tidyverse/ggplot2/issues/3810))

- Default discrete color scales are now configurable through the
  [`options()`](https://rdrr.io/r/base/options.html) of
  `ggplot2.discrete.colour` and `ggplot2.discrete.fill`. When set to a
  character vector of colour codes (or list of character vectors) with
  sufficient length, these colours are used for the default scale. See
  [`help(scale_colour_discrete)`](https://ggplot2.tidyverse.org/dev/reference/scale_colour_discrete.md)
  for more details and examples
  ([@cpsievert](https://github.com/cpsievert),
  [\#3833](https://github.com/tidyverse/ggplot2/issues/3833)).

- Default continuous colour scales (i.e., the
  [`options()`](https://rdrr.io/r/base/options.html)
  `ggplot2.continuous.colour` and `ggplot2.continuous.fill`, which
  inform the `type` argument of
  [`scale_fill_continuous()`](https://ggplot2.tidyverse.org/dev/reference/scale_colour_continuous.md)
  and
  [`scale_colour_continuous()`](https://ggplot2.tidyverse.org/dev/reference/scale_colour_continuous.md))
  now accept a function, which allows more control over these default
  [`continuous_scale()`](https://ggplot2.tidyverse.org/dev/reference/continuous_scale.md)s
  ([@cpsievert](https://github.com/cpsievert),
  [\#3827](https://github.com/tidyverse/ggplot2/issues/3827)).

- A bug was fixed in
  [`stat_contour()`](https://ggplot2.tidyverse.org/dev/reference/geom_contour.md)
  when calculating breaks based on the `bins` argument
  ([@clauswilke](https://github.com/clauswilke),
  [\#3879](https://github.com/tidyverse/ggplot2/issues/3879),
  [\#4004](https://github.com/tidyverse/ggplot2/issues/4004)).

- Data columns can now contain `Vector` S4 objects, which are widely
  used in the Bioconductor project.
  ([@teunbrand](https://github.com/teunbrand),
  [\#3837](https://github.com/tidyverse/ggplot2/issues/3837))

## ggplot2 3.3.1

CRAN release: 2020-05-28

This is a small release with no code change. It removes all malicious
links to a site that got hijacked from the readme and pkgdown site.

## ggplot2 3.3.0

CRAN release: 2020-03-05

This is a minor release but does contain a range of substantial new
features, along with the standard bug fixes. The release contains a few
visual breaking changes, along with breaking changes for extension
developers due to a shift in internal representation of the position
scales and their axes. No user breaking changes are included.

This release also adds Dewey Dunnington
([@paleolimbot](https://github.com/paleolimbot)) to the core team.

### Breaking changes

There are no user-facing breaking changes, but a change in some internal
representations that extension developers may have relied on, along with
a few breaking visual changes which may cause visual tests in downstream
packages to fail.

- The `panel_params` field in the `Layout` now contains a list of list
  of `ViewScale` objects, describing the trained coordinate system
  scales, instead of the list object used before. Any extensions that
  use this field will likely break, as will unit tests that checks
  aspects of this.

- [`element_text()`](https://ggplot2.tidyverse.org/dev/reference/element.md)
  now issues a warning when vectorized arguments are provided, as in
  `colour = c("red", "green", "blue")`. Such use is discouraged and not
  officially supported ([@clauswilke](https://github.com/clauswilke),
  [\#3492](https://github.com/tidyverse/ggplot2/issues/3492)).

- Changed
  [`theme_grey()`](https://ggplot2.tidyverse.org/dev/reference/ggtheme.md)
  setting for legend key so that it creates no border (`NA`) rather than
  drawing a white one. ([@annennenne](https://github.com/annennenne),
  [\#3180](https://github.com/tidyverse/ggplot2/issues/3180))

- [`geom_ribbon()`](https://ggplot2.tidyverse.org/dev/reference/geom_ribbon.md)
  now draws separate lines for the upper and lower intervals if `colour`
  is mapped. Similarly,
  [`geom_area()`](https://ggplot2.tidyverse.org/dev/reference/geom_ribbon.md)
  and
  [`geom_density()`](https://ggplot2.tidyverse.org/dev/reference/geom_density.md)
  now draw the upper lines only in the same case by default. If you want
  old-style full stroking, use `outline.type = "full"`
  ([@yutannihilation](https://github.com/yutannihilation),
  [\#3503](https://github.com/tidyverse/ggplot2/issues/3503) /
  [@thomasp85](https://github.com/thomasp85),
  [\#3708](https://github.com/tidyverse/ggplot2/issues/3708)).

### New features

- The evaluation time of aesthetics can now be controlled to a finer
  degree.
  [`after_stat()`](https://ggplot2.tidyverse.org/dev/reference/aes_eval.md)
  supersedes the use of
  [`stat()`](https://ggplot2.tidyverse.org/dev/reference/aes_eval.md)
  and `..var..`-notation, and is joined by
  [`after_scale()`](https://ggplot2.tidyverse.org/dev/reference/aes_eval.md)
  to allow for mapping to scaled aesthetic values. Remapping of the same
  aesthetic is now supported with
  [`stage()`](https://ggplot2.tidyverse.org/dev/reference/aes_eval.md),
  so you can map a data variable to a stat aesthetic, and remap the same
  aesthetic to something else after statistical transformation
  ([@thomasp85](https://github.com/thomasp85),
  [\#3534](https://github.com/tidyverse/ggplot2/issues/3534))

- All `coord_*()` functions with `xlim` and `ylim` arguments now accept
  vectors with `NA` as a placeholder for the minimum or maximum value
  (e.g., `ylim = c(0, NA)` would zoom the y-axis from 0 to the maximum
  value observed in the data). This mimics the behaviour of the `limits`
  argument in continuous scale functions
  ([@paleolimbot](https://github.com/paleolimbot),
  [\#2907](https://github.com/tidyverse/ggplot2/issues/2907)).

- Allowed reversing of discrete scales by re-writing `get_limits()`
  ([@AnneLyng](https://github.com/AnneLyng),
  [\#3115](https://github.com/tidyverse/ggplot2/issues/3115))

- All geoms and stats that had a direction (i.e. where the x and y axes
  had different interpretation), can now freely choose their direction,
  instead of relying on
  [`coord_flip()`](https://ggplot2.tidyverse.org/dev/reference/coord_flip.md).
  The direction is deduced from the aesthetic mapping, but can also be
  specified directly with the new `orientation` argument
  ([@thomasp85](https://github.com/thomasp85),
  [\#3506](https://github.com/tidyverse/ggplot2/issues/3506)).

- Position guides can now be customized using the new
  [`guide_axis()`](https://ggplot2.tidyverse.org/dev/reference/guide_axis.md),
  which can be passed to position `scale_*()` functions or via
  [`guides()`](https://ggplot2.tidyverse.org/dev/reference/guides.md).
  The new axis guide
  ([`guide_axis()`](https://ggplot2.tidyverse.org/dev/reference/guide_axis.md))
  comes with arguments `check.overlap` (automatic removal of overlapping
  labels), `angle` (easy rotation of axis labels), and `n.dodge` (dodge
  labels into multiple rows/columns)
  ([@paleolimbot](https://github.com/paleolimbot),
  [\#3322](https://github.com/tidyverse/ggplot2/issues/3322)).

- A new scale type has been added, that allows binning of aesthetics at
  the scale level. It has versions for both position and non-position
  aesthetics and comes with two new guides (`guide_bins` and
  `guide_coloursteps`) ([@thomasp85](https://github.com/thomasp85),
  [\#3096](https://github.com/tidyverse/ggplot2/issues/3096))

- [`scale_x_continuous()`](https://ggplot2.tidyverse.org/dev/reference/scale_continuous.md)
  and
  [`scale_y_continuous()`](https://ggplot2.tidyverse.org/dev/reference/scale_continuous.md)
  gains an `n.breaks` argument guiding the number of automatic generated
  breaks ([@thomasp85](https://github.com/thomasp85),
  [\#3102](https://github.com/tidyverse/ggplot2/issues/3102))

- Added
  [`stat_contour_filled()`](https://ggplot2.tidyverse.org/dev/reference/geom_contour.md)
  and
  [`geom_contour_filled()`](https://ggplot2.tidyverse.org/dev/reference/geom_contour.md),
  which compute and draw filled contours of gridded data
  ([@paleolimbot](https://github.com/paleolimbot),
  [\#3044](https://github.com/tidyverse/ggplot2/issues/3044)).
  [`geom_contour()`](https://ggplot2.tidyverse.org/dev/reference/geom_contour.md)
  and
  [`stat_contour()`](https://ggplot2.tidyverse.org/dev/reference/geom_contour.md)
  now use the isoband package to compute contour lines. The `complete`
  parameter (which was undocumented and has been unused for at least
  four years) was removed
  ([@paleolimbot](https://github.com/paleolimbot),
  [\#3044](https://github.com/tidyverse/ggplot2/issues/3044)).

- Themes have gained two new parameters, `plot.title.position` and
  `plot.caption.position`, that can be used to customize how plot
  title/subtitle and plot caption are positioned relative to the overall
  plot ([@clauswilke](https://github.com/clauswilke),
  [\#3252](https://github.com/tidyverse/ggplot2/issues/3252)).

### Extensions

- `Geom` now gains a `setup_params()` method in line with the other
  ggproto classes ([@thomasp85](https://github.com/thomasp85),
  [\#3509](https://github.com/tidyverse/ggplot2/issues/3509))

- The newly added function
  [`register_theme_elements()`](https://ggplot2.tidyverse.org/dev/reference/register_theme_elements.md)
  now allows developers of extension packages to define their own new
  theme elements and place them into the ggplot2 element tree
  ([@clauswilke](https://github.com/clauswilke),
  [\#2540](https://github.com/tidyverse/ggplot2/issues/2540)).

### Minor improvements and bug fixes

- [`coord_trans()`](https://ggplot2.tidyverse.org/dev/reference/coord_transform.md)
  now draws second axes and accepts `xlim`, `ylim`, and `expand`
  arguments to bring it up to feature parity with
  [`coord_cartesian()`](https://ggplot2.tidyverse.org/dev/reference/coord_cartesian.md).
  The `xtrans` and `ytrans` arguments that were deprecated in version
  1.0.1 in favour of `x` and `y` were removed
  ([@paleolimbot](https://github.com/paleolimbot),
  [\#2990](https://github.com/tidyverse/ggplot2/issues/2990)).

- [`coord_trans()`](https://ggplot2.tidyverse.org/dev/reference/coord_transform.md)
  now calculates breaks using the expanded range (previously these were
  calculated using the unexpanded range, which resulted in differences
  between plots made with
  [`coord_trans()`](https://ggplot2.tidyverse.org/dev/reference/coord_transform.md)
  and those made with
  [`coord_cartesian()`](https://ggplot2.tidyverse.org/dev/reference/coord_cartesian.md)).
  The expansion for discrete axes in
  [`coord_trans()`](https://ggplot2.tidyverse.org/dev/reference/coord_transform.md)
  was also updated such that it behaves identically to that in
  [`coord_cartesian()`](https://ggplot2.tidyverse.org/dev/reference/coord_cartesian.md)
  ([@paleolimbot](https://github.com/paleolimbot),
  [\#3338](https://github.com/tidyverse/ggplot2/issues/3338)).

- [`expand_scale()`](https://ggplot2.tidyverse.org/dev/reference/expansion.md)
  was deprecated in favour of
  [`expansion()`](https://ggplot2.tidyverse.org/dev/reference/expansion.md)
  for setting the `expand` argument of `x` and `y` scales
  ([@paleolimbot](https://github.com/paleolimbot)).

- [`geom_abline()`](https://ggplot2.tidyverse.org/dev/reference/geom_abline.md),
  [`geom_hline()`](https://ggplot2.tidyverse.org/dev/reference/geom_abline.md),
  and
  [`geom_vline()`](https://ggplot2.tidyverse.org/dev/reference/geom_abline.md)
  now issue more informative warnings when supplied with set aesthetics
  (i.e., `slope`, `intercept`, `yintercept`, and/or `xintercept`) and
  mapped aesthetics (i.e., `data` and/or `mapping`).

- Fix a bug in
  [`geom_raster()`](https://ggplot2.tidyverse.org/dev/reference/geom_tile.md)
  that squeezed the image when it went outside scale limits
  ([\#3539](https://github.com/tidyverse/ggplot2/issues/3539),
  [@thomasp85](https://github.com/thomasp85))

- [`geom_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md) now
  determines the legend type automatically
  ([@microly](https://github.com/microly),
  [\#3646](https://github.com/tidyverse/ggplot2/issues/3646)).

- [`geom_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md) now
  removes rows that can’t be plotted due to `NA` aesthetics
  ([\#3546](https://github.com/tidyverse/ggplot2/issues/3546),
  [@thomasp85](https://github.com/thomasp85))

- [`geom_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md) now
  applies alpha to linestring geometries
  ([\#3589](https://github.com/tidyverse/ggplot2/issues/3589),
  [@yutannihilation](https://github.com/yutannihilation)).

- [`gg_dep()`](https://ggplot2.tidyverse.org/dev/reference/gg_dep.md)
  was deprecated ([@perezp44](https://github.com/perezp44),
  [\#3382](https://github.com/tidyverse/ggplot2/issues/3382)).

- Added function `ggplot_add.by()` for lists created with
  [`by()`](https://rdrr.io/r/base/by.html), allowing such lists to be
  added to ggplot objects
  ([\#2734](https://github.com/tidyverse/ggplot2/issues/2734),
  [@Maschette](https://github.com/Maschette))

- ggplot2 no longer depends on reshape2, which means that it no longer
  (recursively) needs plyr, stringr, or stringi packages.

- Increase the default `nbin` of
  [`guide_colourbar()`](https://ggplot2.tidyverse.org/dev/reference/guide_colourbar.md)
  to place the ticks more precisely
  ([\#3508](https://github.com/tidyverse/ggplot2/issues/3508),
  [@yutannihilation](https://github.com/yutannihilation)).

- `manual_scale()` now matches `values` with the order of `breaks`
  whenever `values` is an unnamed vector. Previously, unnamed `values`
  would match with the limits of the scale and ignore the order of any
  `breaks` provided. Note that this may change the appearance of plots
  that previously relied on the unordered behaviour
  ([\#2429](https://github.com/tidyverse/ggplot2/issues/2429),
  [@idno0001](https://github.com/idno0001)).

- `scale_manual_*(limits = ...)` now actually limits the scale
  ([\#3262](https://github.com/tidyverse/ggplot2/issues/3262),
  [@yutannihilation](https://github.com/yutannihilation)).

- Fix a bug when `show.legend` is a named logical vector
  ([\#3461](https://github.com/tidyverse/ggplot2/issues/3461),
  [@yutannihilation](https://github.com/yutannihilation)).

- Added weight aesthetic option to
  [`stat_density()`](https://ggplot2.tidyverse.org/dev/reference/geom_density.md)
  and made scaling of weights the default
  ([@annennenne](https://github.com/annennenne),
  [\#2902](https://github.com/tidyverse/ggplot2/issues/2902))

- [`stat_density2d()`](https://ggplot2.tidyverse.org/dev/reference/geom_density_2d.md)
  can now take an `adjust` parameter to scale the default bandwidth.
  ([\#2860](https://github.com/tidyverse/ggplot2/issues/2860),
  [@haleyjeppson](https://github.com/haleyjeppson))

- [`stat_smooth()`](https://ggplot2.tidyverse.org/dev/reference/geom_smooth.md)
  uses `REML` by default, if `method = "gam"` and `gam`’s method is not
  specified ([@ikosmidis](https://github.com/ikosmidis),
  [\#2630](https://github.com/tidyverse/ggplot2/issues/2630)).

- stacking text when calculating the labels and the y axis with
  [`stat_summary()`](https://ggplot2.tidyverse.org/dev/reference/stat_summary.md)
  now works ([@ikosmidis](https://github.com/ikosmidis),
  [\#2709](https://github.com/tidyverse/ggplot2/issues/2709))

- [`stat_summary()`](https://ggplot2.tidyverse.org/dev/reference/stat_summary.md)
  and related functions now support rlang-style lambda functions
  ([\#3568](https://github.com/tidyverse/ggplot2/issues/3568),
  [@dkahle](https://github.com/dkahle)).

- The data mask pronoun, `.data`, is now stripped from default labels.

- Addition of partial themes to plots has been made more predictable;
  stepwise addition of individual partial themes is now equivalent to
  addition of multple theme elements at once
  ([@clauswilke](https://github.com/clauswilke),
  [\#3039](https://github.com/tidyverse/ggplot2/issues/3039)).

- Facets now don’t fail even when some variable in the spec are not
  available in all layers
  ([@yutannihilation](https://github.com/yutannihilation),
  [\#2963](https://github.com/tidyverse/ggplot2/issues/2963)).

## ggplot2 3.2.1

CRAN release: 2019-08-10

This is a patch release fixing a few regressions introduced in 3.2.0 as
well as fixing some unit tests that broke due to upstream changes.

- [`position_stack()`](https://ggplot2.tidyverse.org/dev/reference/position_stack.md)
  no longer changes the order of the input data. Changes to the internal
  behaviour of
  [`geom_ribbon()`](https://ggplot2.tidyverse.org/dev/reference/geom_ribbon.md)
  made this reordering problematic with ribbons that spanned `y = 0`
  ([\#3471](https://github.com/tidyverse/ggplot2/issues/3471))
- Using
  [`qplot()`](https://ggplot2.tidyverse.org/dev/reference/qplot.md) with
  a single positional aesthetic will no longer title the non-specified
  scale as `"NULL"`
  ([\#3473](https://github.com/tidyverse/ggplot2/issues/3473))
- Fixes unit tests for sf graticule labels caused by changes to sf

## ggplot2 3.2.0

CRAN release: 2019-06-16

This is a minor release with an emphasis on internal changes to make
ggplot2 faster and more consistent. The few interface changes will only
affect the aesthetics of the plot in minor ways, and will only
potentially break code of extension developers if they have relied on
internals that have been changed. This release also sees the addition of
Hiroaki Yutani ([@yutannihilation](https://github.com/yutannihilation))
to the core developer team.

With the release of R 3.6, ggplot2 now requires the R version to be at
least 3.2, as the tidyverse is committed to support 5 major versions of
R.

### Breaking changes

- Two patches
  ([\#2996](https://github.com/tidyverse/ggplot2/issues/2996) and
  [\#3050](https://github.com/tidyverse/ggplot2/issues/3050)) fixed
  minor rendering problems. In most cases, the visual changes are so
  subtle that they are difficult to see with the naked eye. However,
  these changes are detected by the vdiffr package, and therefore any
  package developers who use vdiffr to test for visual correctness of
  ggplot2 plots will have to regenerate all reference images.

- In some cases, ggplot2 now produces a warning or an error for code
  that previously produced plot output. In all these cases, the previous
  plot output was accidental, and the plotting code uses the ggplot2 API
  in a way that would lead to undefined behavior. Examples include a
  missing `group` aesthetic in
  [`geom_boxplot()`](https://ggplot2.tidyverse.org/dev/reference/geom_boxplot.md)
  ([\#3316](https://github.com/tidyverse/ggplot2/issues/3316)),
  annotations across multiple facets
  ([\#3305](https://github.com/tidyverse/ggplot2/issues/3305)), and not
  using aesthetic mappings when drawing ribbons with
  [`geom_ribbon()`](https://ggplot2.tidyverse.org/dev/reference/geom_ribbon.md)
  ([\#3318](https://github.com/tidyverse/ggplot2/issues/3318)).

### New features

- This release includes a range of internal changes that speeds up plot
  generation. None of the changes are user facing and will not break any
  code, but in general ggplot2 should feel much faster. The changes
  includes, but are not limited to:

  - Caching ascent and descent dimensions of text to avoid recalculating
    it for every title.

  - Using a faster data.frame constructor as well as faster indexing
    into data.frames

  - Removing the plyr dependency, replacing plyr functions with faster
    equivalents.

- [`geom_polygon()`](https://ggplot2.tidyverse.org/dev/reference/geom_polygon.md)
  can now draw polygons with holes using the new `subgroup` aesthetic.
  This functionality requires R 3.6.0
  ([@thomasp85](https://github.com/thomasp85),
  [\#3128](https://github.com/tidyverse/ggplot2/issues/3128))

- Aesthetic mappings now accept functions that return `NULL`
  ([@yutannihilation](https://github.com/yutannihilation),
  [\#2997](https://github.com/tidyverse/ggplot2/issues/2997)).

- [`stat_function()`](https://ggplot2.tidyverse.org/dev/reference/geom_function.md)
  now accepts rlang/purrr style anonymous functions for the `fun`
  parameter ([@dkahle](https://github.com/dkahle),
  [\#3159](https://github.com/tidyverse/ggplot2/issues/3159)).

- [`geom_rug()`](https://ggplot2.tidyverse.org/dev/reference/geom_rug.md)
  gains an “outside” option to allow for moving the rug tassels to
  outside the plot area ([@njtierney](https://github.com/njtierney),
  [\#3085](https://github.com/tidyverse/ggplot2/issues/3085)) and a
  `length` option to allow for changing the length of the rug lines
  ([@daniel-wells](https://github.com/daniel-wells),
  [\#3109](https://github.com/tidyverse/ggplot2/issues/3109)).

- All geoms now take a `key_glyph` paramter that allows users to
  customize how legend keys are drawn
  ([@clauswilke](https://github.com/clauswilke),
  [\#3145](https://github.com/tidyverse/ggplot2/issues/3145)). In
  addition, a new key glyph `timeseries` is provided to draw nice
  legends for time series
  ([@mitchelloharawild](https://github.com/mitchelloharawild),
  [\#3145](https://github.com/tidyverse/ggplot2/issues/3145)).

### Extensions

- Layers now have a new member function `setup_layer()` which is called
  at the very beginning of the plot building process and which has
  access to the original input data and the plot object being built.
  This function allows the creation of custom layers that autogenerate
  aesthetic mappings based on the input data or that filter the input
  data in some form. For the time being, this feature is not exported,
  but it has enabled the development of a new layer type,
  [`layer_sf()`](https://ggplot2.tidyverse.org/dev/reference/layer_sf.md)
  (see next item). Other special-purpose layer types may be added in the
  future ([@clauswilke](https://github.com/clauswilke),
  [\#2872](https://github.com/tidyverse/ggplot2/issues/2872)).

- A new layer type
  [`layer_sf()`](https://ggplot2.tidyverse.org/dev/reference/layer_sf.md)
  can auto-detect and auto-map sf geometry columns in the data. It
  should be used by extension developers who are writing new sf-based
  geoms or stats ([@clauswilke](https://github.com/clauswilke),
  [\#3232](https://github.com/tidyverse/ggplot2/issues/3232)).

- `x0` and `y0` are now recognized positional aesthetics so they will
  get scaled if used in extension geoms and stats
  ([@thomasp85](https://github.com/thomasp85),
  [\#3168](https://github.com/tidyverse/ggplot2/issues/3168))

- Continuous scale limits now accept functions which accept the default
  limits and return adjusted limits. This makes it possible to write a
  function that e.g. ensures the limits are always a multiple of 100,
  regardless of the data ([@econandrew](https://github.com/econandrew),
  [\#2307](https://github.com/tidyverse/ggplot2/issues/2307)).

### Minor improvements and bug fixes

- [`cut_width()`](https://ggplot2.tidyverse.org/dev/reference/cut_interval.md)
  now accepts `...` to pass further arguments to
  [`base::cut.default()`](https://rdrr.io/r/base/cut.html) like
  [`cut_number()`](https://ggplot2.tidyverse.org/dev/reference/cut_interval.md)
  and
  [`cut_interval()`](https://ggplot2.tidyverse.org/dev/reference/cut_interval.md)
  already did ([@cderv](https://github.com/cderv),
  [\#3055](https://github.com/tidyverse/ggplot2/issues/3055))

- [`coord_map()`](https://ggplot2.tidyverse.org/dev/reference/coord_map.md)
  now can have axes on the top and right
  ([@karawoo](https://github.com/karawoo),
  [\#3042](https://github.com/tidyverse/ggplot2/issues/3042)).

- [`coord_polar()`](https://ggplot2.tidyverse.org/dev/reference/coord_radial.md)
  now correctly rescales the secondary axis
  ([@linzi-sg](https://github.com/linzi-sg),
  [\#3278](https://github.com/tidyverse/ggplot2/issues/3278))

- [`coord_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md),
  [`coord_map()`](https://ggplot2.tidyverse.org/dev/reference/coord_map.md),
  and
  [`coord_polar()`](https://ggplot2.tidyverse.org/dev/reference/coord_radial.md)
  now squash `-Inf` and `Inf` into the min and max of the plot
  ([@yutannihilation](https://github.com/yutannihilation),
  [\#2972](https://github.com/tidyverse/ggplot2/issues/2972)).

- [`coord_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md)
  graticule lines are now drawn in the same thickness as panel grid
  lines in
  [`coord_cartesian()`](https://ggplot2.tidyverse.org/dev/reference/coord_cartesian.md),
  and seting panel grid lines to
  [`element_blank()`](https://ggplot2.tidyverse.org/dev/reference/element.md)
  now also works in
  [`coord_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md)
  ([@clauswilke](https://github.com/clauswilke),
  [\#2991](https://github.com/tidyverse/ggplot2/issues/2991),
  [\#2525](https://github.com/tidyverse/ggplot2/issues/2525)).

- `economics` data has been regenerated. This leads to some changes in
  the values of all columns (especially in `psavert`), but more
  importantly, strips the grouping attributes from `economics_long`.

- [`element_line()`](https://ggplot2.tidyverse.org/dev/reference/element.md)
  now fills closed arrows
  ([@yutannihilation](https://github.com/yutannihilation),
  [\#2924](https://github.com/tidyverse/ggplot2/issues/2924)).

- Facet strips on the left side of plots now have clipping turned on,
  preventing text from running out of the strip and borders from looking
  thicker than for other strips ([@karawoo](https://github.com/karawoo),
  [\#2772](https://github.com/tidyverse/ggplot2/issues/2772) and
  [\#3061](https://github.com/tidyverse/ggplot2/issues/3061)).

- ggplot2 now works in Turkish locale
  ([@yutannihilation](https://github.com/yutannihilation),
  [\#3011](https://github.com/tidyverse/ggplot2/issues/3011)).

- Clearer error messages for inappropriate aesthetics
  ([@clairemcwhite](https://github.com/clairemcwhite),
  [\#3060](https://github.com/tidyverse/ggplot2/issues/3060)).

- ggplot2 no longer attaches any external packages when using functions
  that depend on packages that are suggested but not imported by
  ggplot2. The affected functions include
  [`geom_hex()`](https://ggplot2.tidyverse.org/dev/reference/geom_hex.md),
  [`stat_binhex()`](https://ggplot2.tidyverse.org/dev/reference/geom_hex.md),
  [`stat_summary_hex()`](https://ggplot2.tidyverse.org/dev/reference/stat_summary_2d.md),
  [`geom_quantile()`](https://ggplot2.tidyverse.org/dev/reference/geom_quantile.md),
  [`stat_quantile()`](https://ggplot2.tidyverse.org/dev/reference/geom_quantile.md),
  and
  [`map_data()`](https://ggplot2.tidyverse.org/dev/reference/map_data.md)
  ([@clauswilke](https://github.com/clauswilke),
  [\#3126](https://github.com/tidyverse/ggplot2/issues/3126)).

- [`geom_area()`](https://ggplot2.tidyverse.org/dev/reference/geom_ribbon.md)
  and
  [`geom_ribbon()`](https://ggplot2.tidyverse.org/dev/reference/geom_ribbon.md)
  now sort the data along the x-axis in the `setup_data()` method rather
  than as part of `draw_group()`
  ([@thomasp85](https://github.com/thomasp85),
  [\#3023](https://github.com/tidyverse/ggplot2/issues/3023))

- [`geom_hline()`](https://ggplot2.tidyverse.org/dev/reference/geom_abline.md),
  [`geom_vline()`](https://ggplot2.tidyverse.org/dev/reference/geom_abline.md),
  and
  [`geom_abline()`](https://ggplot2.tidyverse.org/dev/reference/geom_abline.md)
  now throw a warning if the user supplies both an `xintercept`,
  `yintercept`, or `slope` value and a mapping
  ([@RichardJActon](https://github.com/RichardJActon),
  [\#2950](https://github.com/tidyverse/ggplot2/issues/2950)).

- [`geom_rug()`](https://ggplot2.tidyverse.org/dev/reference/geom_rug.md)
  now works with
  [`coord_flip()`](https://ggplot2.tidyverse.org/dev/reference/coord_flip.md)
  ([@has2k1](https://github.com/has2k1),
  [\#2987](https://github.com/tidyverse/ggplot2/issues/2987)).

- [`geom_violin()`](https://ggplot2.tidyverse.org/dev/reference/geom_violin.md)
  no longer throws an error when quantile lines fall outside the violin
  polygon ([@thomasp85](https://github.com/thomasp85),
  [\#3254](https://github.com/tidyverse/ggplot2/issues/3254)).

- [`guide_legend()`](https://ggplot2.tidyverse.org/dev/reference/guide_legend.md)
  and
  [`guide_colorbar()`](https://ggplot2.tidyverse.org/dev/reference/guide_colourbar.md)
  now use appropriate spacing between legend key glyphs and legend text
  even if the legend title is missing
  ([@clauswilke](https://github.com/clauswilke),
  [\#2943](https://github.com/tidyverse/ggplot2/issues/2943)).

- Default labels are now generated more consistently; e.g., symbols no
  longer get backticks, and long expressions are abbreviated with `...`
  ([@yutannihilation](https://github.com/yutannihilation),
  [\#2981](https://github.com/tidyverse/ggplot2/issues/2981)).

- All-`Inf` layers are now ignored for picking the scale
  ([@yutannihilation](https://github.com/yutannihilation),
  [\#3184](https://github.com/tidyverse/ggplot2/issues/3184)).

- Diverging Brewer colour palette now use the correct mid-point colour
  ([@dariyasydykova](https://github.com/dariyasydykova),
  [\#3072](https://github.com/tidyverse/ggplot2/issues/3072)).

- [`scale_color_continuous()`](https://ggplot2.tidyverse.org/dev/reference/scale_colour_continuous.md)
  now points to
  [`scale_colour_continuous()`](https://ggplot2.tidyverse.org/dev/reference/scale_colour_continuous.md)
  so that it will handle `type = "viridis"` as the documentation states
  ([@hlendway](https://github.com/hlendway),
  [\#3079](https://github.com/tidyverse/ggplot2/issues/3079)).

- [`scale_shape_identity()`](https://ggplot2.tidyverse.org/dev/reference/scale_identity.md)
  now works correctly with `guide = "legend"` (1,
  [\#3029](https://github.com/tidyverse/ggplot2/issues/3029))

- `scale_continuous` will now draw axis line even if the length of
  breaks is 0 ([@thomasp85](https://github.com/thomasp85),
  [\#3257](https://github.com/tidyverse/ggplot2/issues/3257))

- [`stat_bin()`](https://ggplot2.tidyverse.org/dev/reference/geom_histogram.md)
  will now error when the number of bins exceeds 1e6 to avoid
  accidentally freezing the user session
  ([@thomasp85](https://github.com/thomasp85)).

- [`sec_axis()`](https://ggplot2.tidyverse.org/dev/reference/sec_axis.md)
  now places ticks accurately when using nonlinear transformations
  ([@dpseidel](https://github.com/dpseidel),
  [\#2978](https://github.com/tidyverse/ggplot2/issues/2978)).

- [`facet_wrap()`](https://ggplot2.tidyverse.org/dev/reference/facet_wrap.md)
  and
  [`facet_grid()`](https://ggplot2.tidyverse.org/dev/reference/facet_grid.md)
  now automatically remove NULL from facet specs, and accept empty specs
  ([@yutannihilation](https://github.com/yutannihilation),
  [\#3070](https://github.com/tidyverse/ggplot2/issues/3070),
  [\#2986](https://github.com/tidyverse/ggplot2/issues/2986)).

- [`stat_bin()`](https://ggplot2.tidyverse.org/dev/reference/geom_histogram.md)
  now handles data with only one unique value
  ([@yutannihilation](https://github.com/yutannihilation)
  [\#3047](https://github.com/tidyverse/ggplot2/issues/3047)).

- [`sec_axis()`](https://ggplot2.tidyverse.org/dev/reference/sec_axis.md)
  now accepts functions as well as formulas
  ([@yutannihilation](https://github.com/yutannihilation),
  [\#3031](https://github.com/tidyverse/ggplot2/issues/3031)).

- New theme elements allowing different ticks lengths for each axis. For
  instance, this can be used to have inwards ticks on the x-axis
  (`axis.ticks.length.x`) and outwards ticks on the y-axis
  (`axis.ticks.length.y`) ([@pank](https://github.com/pank),
  [\#2935](https://github.com/tidyverse/ggplot2/issues/2935)).

- The arguments of `Stat*$compute_layer()` and
  `Position*$compute_layer()` are now renamed to always match the ones
  of `Stat$compute_layer()` and `Position$compute_layer()`
  ([@yutannihilation](https://github.com/yutannihilation),
  [\#3202](https://github.com/tidyverse/ggplot2/issues/3202)).

- `geom_*()` and `stat_*()` now accepts purrr-style lambda notation
  ([@yutannihilation](https://github.com/yutannihilation),
  [\#3138](https://github.com/tidyverse/ggplot2/issues/3138)).

- [`geom_tile()`](https://ggplot2.tidyverse.org/dev/reference/geom_tile.md)
  and
  [`geom_rect()`](https://ggplot2.tidyverse.org/dev/reference/geom_tile.md)
  now draw rectangles without notches at the corners. The style of the
  corner can be controlled by `linejoin` parameters
  ([@yutannihilation](https://github.com/yutannihilation),
  [\#3050](https://github.com/tidyverse/ggplot2/issues/3050)).

## ggplot2 3.1.0

CRAN release: 2018-10-25

### Breaking changes

This is a minor release and breaking changes have been kept to a
minimum. End users of ggplot2 are unlikely to encounter any issues.
However, there are a few items that developers of ggplot2 extensions
should be aware of. For additional details, see also the discussion
accompanying issue
[\#2890](https://github.com/tidyverse/ggplot2/issues/2890).

- In non-user-facing internal code (specifically in the
  [`aes()`](https://ggplot2.tidyverse.org/dev/reference/aes.md) function
  and in the `aesthetics` argument of scale functions), ggplot2 now
  always uses the British spelling for aesthetics containing the word
  “colour”. When users specify a “color” aesthetic it is automatically
  renamed to “colour”. This renaming is also applied to non-standard
  aesthetics that contain the word “color”. For example, “point_color”
  is renamed to “point_colour”. This convention makes it easier to
  support both British and American spelling for novel, non-standard
  aesthetics, but it may require some adjustment for packages that have
  previously introduced non-standard color aesthetics using American
  spelling. A new function
  [`standardise_aes_names()`](https://ggplot2.tidyverse.org/dev/reference/standardise_aes_names.md)
  is provided in case extension writers need to perform this renaming in
  their own code ([@clauswilke](https://github.com/clauswilke),
  [\#2649](https://github.com/tidyverse/ggplot2/issues/2649)).

- Functions that generate other functions (closures) now force the
  arguments that are used from the generated functions, to avoid
  hard-to-catch errors. This may affect some users of manual scales
  (such as
  [`scale_colour_manual()`](https://ggplot2.tidyverse.org/dev/reference/scale_manual.md),
  [`scale_fill_manual()`](https://ggplot2.tidyverse.org/dev/reference/scale_manual.md),
  etc.) who depend on incorrect behavior
  ([@krlmlr](https://github.com/krlmlr),
  [\#2807](https://github.com/tidyverse/ggplot2/issues/2807)).

- `Coord` objects now have a function `backtransform_range()` that
  returns the panel range in data coordinates. This change may affect
  developers of custom coords, who now should implement this function.
  It may also affect developers of custom geoms that use the
  [`range()`](https://rdrr.io/r/base/range.html) function. In some
  applications, `backtransform_range()` may be more appropriate
  ([@clauswilke](https://github.com/clauswilke),
  [\#2821](https://github.com/tidyverse/ggplot2/issues/2821)).

### New features

- [`coord_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md)
  has much improved customization of axis tick labels. Labels can now be
  set manually, and there are two new parameters, `label_graticule` and
  `label_axes`, that can be used to specify which graticules to label on
  which side of the plot ([@clauswilke](https://github.com/clauswilke),
  [\#2846](https://github.com/tidyverse/ggplot2/issues/2846),
  [\#2857](https://github.com/tidyverse/ggplot2/issues/2857),
  [\#2881](https://github.com/tidyverse/ggplot2/issues/2881)).

- Two new geoms
  [`geom_sf_label()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md)
  and
  [`geom_sf_text()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md)
  can draw labels and text on sf objects. Under the hood, a new
  [`stat_sf_coordinates()`](https://ggplot2.tidyverse.org/dev/reference/stat_sf_coordinates.md)
  calculates the x and y coordinates from the coordinates of the sf
  geometries. You can customize the calculation method via
  `fun.geometry` argument
  ([@yutannihilation](https://github.com/yutannihilation),
  [\#2761](https://github.com/tidyverse/ggplot2/issues/2761)).

### Minor improvements and fixes

- [`benchplot()`](https://ggplot2.tidyverse.org/dev/reference/benchplot.md)
  now uses tidy evaluation ([@dpseidel](https://github.com/dpseidel),
  [\#2699](https://github.com/tidyverse/ggplot2/issues/2699)).

- The error message in `compute_aesthetics()` now only provides the
  names of aesthetics with mismatched lengths, rather than all
  aesthetics ([@karawoo](https://github.com/karawoo),
  [\#2853](https://github.com/tidyverse/ggplot2/issues/2853)).

- For faceted plots, data is no longer internally reordered. This makes
  it safer to feed data columns into
  [`aes()`](https://ggplot2.tidyverse.org/dev/reference/aes.md) or into
  parameters of geoms or stats. However, doing so remains discouraged
  ([@clauswilke](https://github.com/clauswilke),
  [\#2694](https://github.com/tidyverse/ggplot2/issues/2694)).

- [`coord_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md)
  now also understands the `clip` argument, just like the other coords
  ([@clauswilke](https://github.com/clauswilke),
  [\#2938](https://github.com/tidyverse/ggplot2/issues/2938)).

- [`fortify()`](https://ggplot2.tidyverse.org/dev/reference/fortify.md)
  now displays a more informative error message for
  [`grouped_df()`](https://dplyr.tidyverse.org/reference/grouped_df.html)
  objects when dplyr is not installed
  ([@jimhester](https://github.com/jimhester),
  [\#2822](https://github.com/tidyverse/ggplot2/issues/2822)).

- All `geom_*()` now display an informative error message when required
  aesthetics are missing ([@dpseidel](https://github.com/dpseidel),
  [\#2637](https://github.com/tidyverse/ggplot2/issues/2637) and
  [\#2706](https://github.com/tidyverse/ggplot2/issues/2706)).

- [`geom_boxplot()`](https://ggplot2.tidyverse.org/dev/reference/geom_boxplot.md)
  now understands the `width` parameter even when used with a
  non-standard stat, such as
  [`stat_identity()`](https://ggplot2.tidyverse.org/dev/reference/stat_identity.md)
  ([@clauswilke](https://github.com/clauswilke),
  [\#2893](https://github.com/tidyverse/ggplot2/issues/2893)).

- [`geom_hex()`](https://ggplot2.tidyverse.org/dev/reference/geom_hex.md)
  now understands the `size` and `linetype` aesthetics
  ([@mikmart](https://github.com/mikmart),
  [\#2488](https://github.com/tidyverse/ggplot2/issues/2488)).

- [`geom_hline()`](https://ggplot2.tidyverse.org/dev/reference/geom_abline.md),
  [`geom_vline()`](https://ggplot2.tidyverse.org/dev/reference/geom_abline.md),
  and
  [`geom_abline()`](https://ggplot2.tidyverse.org/dev/reference/geom_abline.md)
  now work properly with
  [`coord_trans()`](https://ggplot2.tidyverse.org/dev/reference/coord_transform.md)
  ([@clauswilke](https://github.com/clauswilke),
  [\#2149](https://github.com/tidyverse/ggplot2/issues/2149),
  [\#2812](https://github.com/tidyverse/ggplot2/issues/2812)).

- `geom_text(..., parse = TRUE)` now correctly renders the expected
  number of items instead of silently dropping items that are empty
  expressions, e.g. the empty string ““. If an expression spans multiple
  lines, we take just the first line and drop the rest. This same issue
  is also fixed for
  [`geom_label()`](https://ggplot2.tidyverse.org/dev/reference/geom_text.md)
  and the axis labels for
  [`geom_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md)
  ([@slowkow](https://github.com/slowkow),
  [\#2867](https://github.com/tidyverse/ggplot2/issues/2867)).

- [`geom_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md) now
  respects `lineend`, `linejoin`, and `linemitre` parameters for lines
  and polygons ([@alistaire47](https://github.com/alistaire47),
  [\#2826](https://github.com/tidyverse/ggplot2/issues/2826)).

- [`ggsave()`](https://ggplot2.tidyverse.org/dev/reference/ggsave.md)
  now exits without creating a new graphics device if previously none
  was open ([@clauswilke](https://github.com/clauswilke),
  [\#2363](https://github.com/tidyverse/ggplot2/issues/2363)).

- [`labs()`](https://ggplot2.tidyverse.org/dev/reference/labs.md) now
  has named arguments `title`, `subtitle`, `caption`, and `tag`. Also,
  [`labs()`](https://ggplot2.tidyverse.org/dev/reference/labs.md) now
  accepts tidyeval
  ([@yutannihilation](https://github.com/yutannihilation),
  [\#2669](https://github.com/tidyverse/ggplot2/issues/2669)).

- [`position_nudge()`](https://ggplot2.tidyverse.org/dev/reference/position_nudge.md)
  is now more robust and nudges only in the direction requested. This
  enables, for example, the horizontal nudging of boxplots
  ([@clauswilke](https://github.com/clauswilke),
  [\#2733](https://github.com/tidyverse/ggplot2/issues/2733)).

- [`sec_axis()`](https://ggplot2.tidyverse.org/dev/reference/sec_axis.md)
  and
  [`dup_axis()`](https://ggplot2.tidyverse.org/dev/reference/sec_axis.md)
  now return appropriate breaks for the secondary axis when applied to
  log transformed scales ([@dpseidel](https://github.com/dpseidel),
  [\#2729](https://github.com/tidyverse/ggplot2/issues/2729)).

- [`sec_axis()`](https://ggplot2.tidyverse.org/dev/reference/sec_axis.md)
  now works as expected when used in combination with tidy eval
  ([@dpseidel](https://github.com/dpseidel),
  [\#2788](https://github.com/tidyverse/ggplot2/issues/2788)).

- `scale_*_date()`, `scale_*_time()` and `scale_*_datetime()` can now
  display a secondary axis that is a **one-to-one** transformation of
  the primary axis, implemented using the `sec.axis` argument to the
  scale constructor ([@dpseidel](https://github.com/dpseidel),
  [\#2244](https://github.com/tidyverse/ggplot2/issues/2244)).

- [`stat_contour()`](https://ggplot2.tidyverse.org/dev/reference/geom_contour.md),
  [`stat_density2d()`](https://ggplot2.tidyverse.org/dev/reference/geom_density_2d.md),
  [`stat_bin2d()`](https://ggplot2.tidyverse.org/dev/reference/geom_bin_2d.md),
  [`stat_binhex()`](https://ggplot2.tidyverse.org/dev/reference/geom_hex.md)
  now calculate normalized statistics including `nlevel`, `ndensity`,
  and `ncount`. Also,
  [`stat_density()`](https://ggplot2.tidyverse.org/dev/reference/geom_density.md)
  now includes the calculated statistic `nlevel`, an alias for `scaled`,
  to better match the syntax of
  [`stat_bin()`](https://ggplot2.tidyverse.org/dev/reference/geom_histogram.md)
  ([@bjreisman](https://github.com/bjreisman),
  [\#2679](https://github.com/tidyverse/ggplot2/issues/2679)).

## ggplot2 3.0.0

CRAN release: 2018-07-03

### Breaking changes

- ggplot2 now supports/uses tidy evaluation (as described below). This
  is a major change and breaks a number of packages; we made this
  breaking change because it is important to make ggplot2 more
  programmable, and to be more consistent with the rest of the
  tidyverse. The best general (and detailed) introduction to tidy
  evaluation can be found in the meta programming chapters in [Advanced
  R](https://adv-r.hadley.nz).

  The primary developer facing change is that
  [`aes()`](https://ggplot2.tidyverse.org/dev/reference/aes.md) now
  contains quosures (expression + environment pairs) rather than
  symbols, and you’ll need to take a different approach to extracting
  the information you need. A common symptom of this change are errors
  “undefined columns selected” or “invalid ‘type’ (list) of argument”
  ([\#2610](https://github.com/tidyverse/ggplot2/issues/2610)). As in
  the previous version, constants (like `aes(x = 1)` or
  `aes(colour = "smoothed")`) are stored as is.

  In this version of ggplot2, if you need to describe a mapping in a
  string, use
  [`quo_name()`](https://ggplot2.tidyverse.org/dev/reference/tidyeval.md)
  (to generate single-line strings; longer expressions may be
  abbreviated) or `quo_text()` (to generate non-abbreviated strings that
  may span multiple lines). If you do need to extract the value of a
  variable instead use
  [`rlang::eval_tidy()`](https://rlang.r-lib.org/reference/eval_tidy.html).
  You may want to condition on `(packageVersion("ggplot2") <= "2.2.1")`
  so that your code can work with both released and development versions
  of ggplot2.

  We recognise that this is a big change and if you’re not already
  familiar with rlang, there’s a lot to learn. If you are stuck, or need
  any help, please reach out on <https://forum.posit.co/>.

- Error: Column `y` must be a 1d atomic vector or a list

  Internally, ggplot2 now uses `as.data.frame(tibble::as_tibble(x))` to
  convert a list into a data frame. This improves ggplot2’s support for
  list-columns (needed for sf support), at a small cost: you can no
  longer use matrix-columns. Note that unlike tibble we still allow
  column vectors such as returned by
  [`base::scale()`](https://rdrr.io/r/base/scale.html) because of their
  widespread use.

- Error: More than one expression parsed

  Previously `aes_string(x = c("a", "b", "c"))` silently returned
  `aes(x = a)`. Now this is a clear error.

- Error: `data` must be uniquely named but has duplicate columns

  If layer data contains columns with identical names an error will be
  thrown. In earlier versions the first occurring column was chosen
  silently, potentially masking that the wrong data was chosen.

- Error: Aesthetics must be either length 1 or the same as the data

  Layers are stricter about the columns they will combine into a single
  data frame. Each aesthetic now must be either the same length as the
  data frame or a single value. This makes silent recycling errors much
  less likely.

- Error: `coord_*` doesn’t support free scales

  Free scales only work with selected coordinate systems; previously
  you’d get an incorrect plot.

- Error in f(…) : unused argument (range = c(0, 1))

  This is because the `oob` argument to scale has been set to a function
  that only takes a single argument; it needs to take two arguments
  (`x`, and `range`).

- Error: unused argument (output)

  The function
  [`guide_train()`](https://ggplot2.tidyverse.org/dev/reference/old_guide.md)
  now has an optional parameter `aesthetic` that allows you to override
  the `aesthetic` setting in the scale. To make your code work with the
  both released and development versions of ggplot2 appropriate, add
  `aesthetic = NULL` to the
  [`guide_train()`](https://ggplot2.tidyverse.org/dev/reference/old_guide.md)
  method signature.

  ``` r
  # old
  guide_train.legend <- function(guide, scale) {...}

  # new
  guide_train.legend <- function(guide, scale, aesthetic = NULL) {...}
  ```

  Then, inside the function, replace `scale$aesthetics[1]`,
  `aesthetic %||% scale$aesthetics[1]`. (The %\|\|% operator is defined
  in the rlang package).

  ``` r
  # old
  setNames(list(scale$map(breaks)), scale$aesthetics[1])

  # new
  setNames(list(scale$map(breaks)), aesthetic %||% scale$aesthetics[1])
  ```

- The long-deprecated `subset` argument to
  [`layer()`](https://ggplot2.tidyverse.org/dev/reference/layer.md) has
  been removed.

### Tidy evaluation

- [`aes()`](https://ggplot2.tidyverse.org/dev/reference/aes.md) now
  supports quasiquotation so that you can use `!!`, `!!!`, and `:=`.
  This replaces
  [`aes_()`](https://ggplot2.tidyverse.org/dev/reference/aes_.md) and
  [`aes_string()`](https://ggplot2.tidyverse.org/dev/reference/aes_.md)
  which are now soft-deprecated (but will remain around for a long
  time).

- [`facet_wrap()`](https://ggplot2.tidyverse.org/dev/reference/facet_wrap.md)
  and
  [`facet_grid()`](https://ggplot2.tidyverse.org/dev/reference/facet_grid.md)
  now support
  [`vars()`](https://ggplot2.tidyverse.org/dev/reference/vars.md)
  inputs. Like
  [`dplyr::vars()`](https://dplyr.tidyverse.org/reference/vars.html),
  this helper quotes its inputs and supports quasiquotation. For
  instance, you can now supply faceting variables like this:
  `facet_wrap(vars(am, cyl))` instead of `facet_wrap(~am + cyl)`. Note
  that the formula interface is not going away and will not be
  deprecated.
  [`vars()`](https://ggplot2.tidyverse.org/dev/reference/vars.md) is
  simply meant to make it easier to create functions around
  [`facet_wrap()`](https://ggplot2.tidyverse.org/dev/reference/facet_wrap.md)
  and
  [`facet_grid()`](https://ggplot2.tidyverse.org/dev/reference/facet_grid.md).

  The first two arguments of
  [`facet_grid()`](https://ggplot2.tidyverse.org/dev/reference/facet_grid.md)
  become `rows` and `cols` and now support
  [`vars()`](https://ggplot2.tidyverse.org/dev/reference/vars.md)
  inputs. Note however that we took special care to ensure complete
  backward compatibility. With this change
  `facet_grid(vars(cyl), vars(am, vs))` is equivalent to
  `facet_grid(cyl ~ am + vs)`, and `facet_grid(cols = vars(am, vs))` is
  equivalent to `facet_grid(. ~ am + vs)`.

  One nice aspect of the new interface is that you can now easily supply
  names: `facet_grid(vars(Cylinder = cyl), labeller = label_both)` will
  give nice label titles to the facets. Of course, those names can be
  unquoted with the usual tidy eval syntax.

#### sf

- ggplot2 now has full support for sf with
  [`geom_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md) and
  [`coord_sf()`](https://ggplot2.tidyverse.org/dev/reference/ggsf.md):

  ``` r
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  ggplot(nc) +
    geom_sf(aes(fill = AREA))
  ```

  It supports all simple features, automatically aligns CRS across
  layers, sets up the correct aspect ratio, and draws a graticule.

### New features

- ggplot2 now works on R 3.1 onwards, and uses the
  [vdiffr](https://github.com/r-lib/vdiffr) package for visual testing.

- In most cases, accidentally using `%>%` instead of `+` will generate
  an informative error
  ([\#2400](https://github.com/tidyverse/ggplot2/issues/2400)).

- New syntax for calculated aesthetics. Instead of using
  `aes(y = ..count..)` you can (and should!) use `aes(y = stat(count))`.
  [`stat()`](https://ggplot2.tidyverse.org/dev/reference/aes_eval.md) is
  a real function with documentation which hopefully will make this part
  of ggplot2 less confusing
  ([\#2059](https://github.com/tidyverse/ggplot2/issues/2059)).

  [`stat()`](https://ggplot2.tidyverse.org/dev/reference/aes_eval.md) is
  particularly nice for more complex calculations because you only need
  to specify it once: `aes(y = stat(count / max(count)))`, rather than
  `aes(y = ..count.. / max(..count..))`

- New `tag` label for adding identification tags to plots, typically
  used for labelling a subplot with a letter. Add a tag with
  `labs(tag = "A")`, style it with the `plot.tag` theme element, and
  control position with the `plot.tag.position` theme setting
  ([@thomasp85](https://github.com/thomasp85)).

#### Layers: geoms, stats, and position adjustments

- [`geom_segment()`](https://ggplot2.tidyverse.org/dev/reference/geom_segment.md)
  and
  [`geom_curve()`](https://ggplot2.tidyverse.org/dev/reference/geom_segment.md)
  have a new `arrow.fill` parameter which allows you to specify a
  separate fill colour for closed arrowheads
  ([@hrbrmstr](https://github.com/hrbrmstr) and
  [@clauswilke](https://github.com/clauswilke),
  [\#2375](https://github.com/tidyverse/ggplot2/issues/2375)).

- [`geom_point()`](https://ggplot2.tidyverse.org/dev/reference/geom_point.md)
  and friends can now take shapes as strings instead of integers,
  e.g. `geom_point(shape = "diamond")`
  ([@daniel-barnett](https://github.com/daniel-barnett),
  [\#2075](https://github.com/tidyverse/ggplot2/issues/2075)).

- [`position_dodge()`](https://ggplot2.tidyverse.org/dev/reference/position_dodge.md)
  gains a `preserve` argument that allows you to control whether the
  `total` width at each `x` value is preserved (the current default), or
  ensure that the width of a `single` element is preserved (what many
  people want)
  ([\#1935](https://github.com/tidyverse/ggplot2/issues/1935)).

- New
  [`position_dodge2()`](https://ggplot2.tidyverse.org/dev/reference/position_dodge.md)
  provides enhanced dodging for boxplots. Compared to
  [`position_dodge()`](https://ggplot2.tidyverse.org/dev/reference/position_dodge.md),
  [`position_dodge2()`](https://ggplot2.tidyverse.org/dev/reference/position_dodge.md)
  compares `xmin` and `xmax` values to determine which elements overlap,
  and spreads overlapping elements evenly within the region of overlap.
  [`position_dodge2()`](https://ggplot2.tidyverse.org/dev/reference/position_dodge.md)
  is now the default position adjustment for
  [`geom_boxplot()`](https://ggplot2.tidyverse.org/dev/reference/geom_boxplot.md),
  because it handles `varwidth = TRUE`, and will be considered for other
  geoms in the future.

  The `padding` parameter adds a small amount of padding between
  elements ([@karawoo](https://github.com/karawoo),
  [\#2143](https://github.com/tidyverse/ggplot2/issues/2143)) and a
  `reverse` parameter allows you to reverse the order of placement
  ([@karawoo](https://github.com/karawoo),
  [\#2171](https://github.com/tidyverse/ggplot2/issues/2171)).

- New
  [`stat_qq_line()`](https://ggplot2.tidyverse.org/dev/reference/geom_qq.md)
  makes it easy to add a simple line to a Q-Q plot, which makes it
  easier to judge the fit of the theoretical distribution
  ([@nicksolomon](https://github.com/nicksolomon)).

#### Scales and guides

- Improved support for mapping date/time variables to `alpha`, `size`,
  `colour`, and `fill` aesthetics, including `date_breaks` and
  `date_labels` arguments ([@karawoo](https://github.com/karawoo),
  [\#1526](https://github.com/tidyverse/ggplot2/issues/1526)), and new
  [`scale_alpha()`](https://ggplot2.tidyverse.org/dev/reference/scale_alpha.md)
  variants ([@karawoo](https://github.com/karawoo),
  [\#1526](https://github.com/tidyverse/ggplot2/issues/1526)).

- Improved support for ordered factors. Ordered factors throw a warning
  when mapped to shape (unordered factors do not), and do not throw
  warnings when mapped to size or alpha (unordered factors do). Viridis
  is used as the default colour and fill scale for ordered factors
  ([@karawoo](https://github.com/karawoo),
  [\#1526](https://github.com/tidyverse/ggplot2/issues/1526)).

- The `expand` argument of `scale_*_continuous()` and
  `scale_*_discrete()` now accepts separate expansion values for the
  lower and upper range limits. The expansion limits can be specified
  using the convenience function
  [`expand_scale()`](https://ggplot2.tidyverse.org/dev/reference/expansion.md).

  Separate expansion limits may be useful for bar charts, e.g. if one
  wants the bottom of the bars to be flush with the x axis but still
  leave some (automatically calculated amount of) space above them:

  ``` r
  ggplot(mtcars) +
      geom_bar(aes(x = factor(cyl))) +
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))
  ```

  It can also be useful for line charts, e.g. for counts over time,
  where one wants to have a ’hard’ lower limit of y = 0 but leave the
  upper limit unspecified (and perhaps differing between panels), with
  some extra space above the highest point on the line (with symmetrical
  limits, the extra space above the highest point could in some cases
  cause the lower limit to be negative).

  The old syntax for the `expand` argument will, of course, continue to
  work ([@huftis](https://github.com/huftis),
  [\#1669](https://github.com/tidyverse/ggplot2/issues/1669)).

- [`scale_colour_continuous()`](https://ggplot2.tidyverse.org/dev/reference/scale_colour_continuous.md)
  and
  [`scale_colour_gradient()`](https://ggplot2.tidyverse.org/dev/reference/scale_gradient.md)
  are now controlled by global options `ggplot2.continuous.colour` and
  `ggplot2.continuous.fill`. These can be set to `"gradient"` (the
  default) or `"viridis"` ([@karawoo](https://github.com/karawoo)).

- New
  [`scale_colour_viridis_c()`](https://ggplot2.tidyverse.org/dev/reference/scale_viridis.md)/[`scale_fill_viridis_c()`](https://ggplot2.tidyverse.org/dev/reference/scale_viridis.md)
  (continuous) and
  [`scale_colour_viridis_d()`](https://ggplot2.tidyverse.org/dev/reference/scale_viridis.md)/[`scale_fill_viridis_d()`](https://ggplot2.tidyverse.org/dev/reference/scale_viridis.md)
  (discrete) make it easy to use Viridis colour scales
  ([@karawoo](https://github.com/karawoo),
  [\#1526](https://github.com/tidyverse/ggplot2/issues/1526)).

- Guides for
  [`geom_text()`](https://ggplot2.tidyverse.org/dev/reference/geom_text.md)
  now accept custom labels with
  `guide_legend(override.aes = list(label = "foo"))`
  ([@brianwdavis](https://github.com/brianwdavis),
  [\#2458](https://github.com/tidyverse/ggplot2/issues/2458)).

#### Margins

- Strips gain margins on all sides by default. This means that to fully
  justify text to the edge of a strip, you will need to also set the
  margins to 0 ([@karawoo](https://github.com/karawoo)).

- Rotated strip labels now correctly understand `hjust` and `vjust`
  parameters at all angles ([@karawoo](https://github.com/karawoo)).

- Strip labels now understand justification relative to the direction of
  the text, meaning that in y facets, the strip text can be placed at
  either end of the strip using `hjust`
  ([@karawoo](https://github.com/karawoo)).

- Legend titles and labels get a little extra space around them, which
  prevents legend titles from overlapping the legend at large font sizes
  ([@karawoo](https://github.com/karawoo),
  [\#1881](https://github.com/tidyverse/ggplot2/issues/1881)).

### Extension points

- New
  [`autolayer()`](https://ggplot2.tidyverse.org/dev/reference/autolayer.md)
  S3 generic
  ([@mitchelloharawild](https://github.com/mitchelloharawild),
  [\#1974](https://github.com/tidyverse/ggplot2/issues/1974)). This is
  similar to
  [`autoplot()`](https://ggplot2.tidyverse.org/dev/reference/autoplot.md)
  but produces layers rather than complete plots.

- Custom objects can now be added using `+` if a `ggplot_add` method has
  been defined for the class of the object
  ([@thomasp85](https://github.com/thomasp85)).

- Theme elements can now be subclassed. Add a `merge_element` method to
  control how properties are inherited from the parent element. Add an
  `element_grob` method to define how elements are rendered into grobs
  ([@thomasp85](https://github.com/thomasp85),
  [\#1981](https://github.com/tidyverse/ggplot2/issues/1981)).

- Coords have gained new extension mechanisms.

  If you have an existing coord extension, you will need to revise the
  specification of the `train()` method. It is now called
  `setup_panel_params()` (better reflecting what it actually does) and
  now has arguments `scale_x`, and `scale_y` (the x and y scales
  respectively) and `param`, a list of plot specific parameters
  generated by `setup_params()`.

  What was formerly called `scale_details` (in coords), `panel_ranges`
  (in layout) and `panel_scales` (in geoms) are now consistently called
  `panel_params`
  ([\#1311](https://github.com/tidyverse/ggplot2/issues/1311)). These
  are parameters of the coord that vary from panel to panel.

- [`ggplot_build()`](https://ggplot2.tidyverse.org/dev/reference/ggplot_build.md)
  and
  [`ggplot_gtable()`](https://ggplot2.tidyverse.org/dev/reference/ggplot_gtable.md)
  are now generics, so ggplot-subclasses can define additional behavior
  during the build stage.

- [`guide_train()`](https://ggplot2.tidyverse.org/dev/reference/old_guide.md),
  [`guide_merge()`](https://ggplot2.tidyverse.org/dev/reference/old_guide.md),
  [`guide_geom()`](https://ggplot2.tidyverse.org/dev/reference/old_guide.md),
  and
  [`guide_gengrob()`](https://ggplot2.tidyverse.org/dev/reference/old_guide.md)
  are now exported as they are needed if you want to design your own
  guide. They are not currently documented; use at your own risk
  ([\#2528](https://github.com/tidyverse/ggplot2/issues/2528)).

- [`scale_type()`](https://ggplot2.tidyverse.org/dev/reference/scale_type.md)
  generic is now exported and documented. Use this if you want to extend
  ggplot2 to work with a new type of vector.

### Minor bug fixes and improvements

#### Faceting

- [`facet_grid()`](https://ggplot2.tidyverse.org/dev/reference/facet_grid.md)
  gives a more informative error message if you try to use a variable in
  both rows and cols
  ([\#1928](https://github.com/tidyverse/ggplot2/issues/1928)).

- [`facet_grid()`](https://ggplot2.tidyverse.org/dev/reference/facet_grid.md)
  and
  [`facet_wrap()`](https://ggplot2.tidyverse.org/dev/reference/facet_wrap.md)
  both give better error messages if you attempt to use an unsupported
  coord with free scales
  ([\#2049](https://github.com/tidyverse/ggplot2/issues/2049)).

- [`label_parsed()`](https://ggplot2.tidyverse.org/dev/reference/labellers.md)
  works once again
  ([\#2279](https://github.com/tidyverse/ggplot2/issues/2279)).

- You can now style the background of horizontal and vertical strips
  independently with `strip.background.x` and `strip.background.y` theme
  settings ([\#2249](https://github.com/tidyverse/ggplot2/issues/2249)).

#### Scales

- [`discrete_scale()`](https://ggplot2.tidyverse.org/dev/reference/discrete_scale.md)
  documentation now inherits shared definitions from
  [`continuous_scale()`](https://ggplot2.tidyverse.org/dev/reference/continuous_scale.md)
  ([@alistaire47](https://github.com/alistaire47),
  [\#2052](https://github.com/tidyverse/ggplot2/issues/2052)).

- [`guide_colorbar()`](https://ggplot2.tidyverse.org/dev/reference/guide_colourbar.md)
  shows all colours of the scale ([@has2k1](https://github.com/has2k1),
  [\#2343](https://github.com/tidyverse/ggplot2/issues/2343)).

- `scale_identity()` once again produces legends by default
  ([\#2112](https://github.com/tidyverse/ggplot2/issues/2112)).

- Tick marks for secondary axes with strong transformations are more
  accurately placed ([@thomasp85](https://github.com/thomasp85),
  [\#1992](https://github.com/tidyverse/ggplot2/issues/1992)).

- Missing line types now reliably generate missing lines (with standard
  warning) ([\#2206](https://github.com/tidyverse/ggplot2/issues/2206)).

- Legends now ignore set aesthetics that are not length one
  ([\#1932](https://github.com/tidyverse/ggplot2/issues/1932)).

- All colour and fill scales now have an `aesthetics` argument that can
  be used to set the aesthetic(s) the scale works with. This makes it
  possible to apply a colour scale to both colour and fill aesthetics at
  the same time, via `aesthetics = c("colour", "fill")`
  ([@clauswilke](https://github.com/clauswilke)).

- Three new generic scales work with any aesthetic or set of aesthetics:
  [`scale_continuous_identity()`](https://ggplot2.tidyverse.org/dev/reference/scale_identity.md),
  [`scale_discrete_identity()`](https://ggplot2.tidyverse.org/dev/reference/scale_identity.md),
  and
  [`scale_discrete_manual()`](https://ggplot2.tidyverse.org/dev/reference/scale_manual.md)
  ([@clauswilke](https://github.com/clauswilke)).

- `scale_*_gradient2()` now consistently omits points outside limits by
  rescaling after the limits are enforced
  ([@foo-bar-baz-qux](https://github.com/foo-bar-baz-qux),
  [\#2230](https://github.com/tidyverse/ggplot2/issues/2230)).

#### Layers

- [`geom_label()`](https://ggplot2.tidyverse.org/dev/reference/geom_text.md)
  now correctly produces unbordered labels when `label.size` is 0, even
  when saving to PDF ([@bfgray3](https://github.com/bfgray3),
  [\#2407](https://github.com/tidyverse/ggplot2/issues/2407)).

- [`layer()`](https://ggplot2.tidyverse.org/dev/reference/layer.md)
  gives considerably better error messages for incorrectly specified
  `geom`, `stat`, or `position`
  ([\#2401](https://github.com/tidyverse/ggplot2/issues/2401)).

- In all layers that use it, `linemitre` now defaults to 10 (instead
  of 1) to better match base R.

- [`geom_boxplot()`](https://ggplot2.tidyverse.org/dev/reference/geom_boxplot.md)
  now supplies a default value if no `x` aesthetic is present
  ([@foo-bar-baz-qux](https://github.com/foo-bar-baz-qux),
  [\#2110](https://github.com/tidyverse/ggplot2/issues/2110)).

- [`geom_density()`](https://ggplot2.tidyverse.org/dev/reference/geom_density.md)
  drops groups with fewer than two data points and throws a warning. For
  groups with two data points, density values are now calculated with
  [`stats::density`](https://rdrr.io/r/stats/density.html)
  ([@karawoo](https://github.com/karawoo),
  [\#2127](https://github.com/tidyverse/ggplot2/issues/2127)).

- [`geom_segment()`](https://ggplot2.tidyverse.org/dev/reference/geom_segment.md)
  now also takes a `linejoin` parameter. This allows more control over
  the appearance of the segments, which is especially useful for
  plotting thick arrows ([@Ax3man](https://github.com/Ax3man),
  [\#774](https://github.com/tidyverse/ggplot2/issues/774)).

- [`geom_smooth()`](https://ggplot2.tidyverse.org/dev/reference/geom_smooth.md)
  now reports the formula used when `method = "auto"`
  ([@davharris](https://github.com/davharris)
  [\#1951](https://github.com/tidyverse/ggplot2/issues/1951)).
  [`geom_smooth()`](https://ggplot2.tidyverse.org/dev/reference/geom_smooth.md)
  now orders by the `x` aesthetic, making it easier to pass pre-computed
  values without manual ordering ([@izahn](https://github.com/izahn),
  [\#2028](https://github.com/tidyverse/ggplot2/issues/2028)). It also
  now knows it has `ymin` and `ymax` aesthetics
  ([\#1939](https://github.com/tidyverse/ggplot2/issues/1939)). The
  legend correctly reflects the status of the `se` argument when used
  with stats other than the default
  ([@clauswilke](https://github.com/clauswilke),
  [\#1546](https://github.com/tidyverse/ggplot2/issues/1546)).

- [`geom_tile()`](https://ggplot2.tidyverse.org/dev/reference/geom_tile.md)
  now once again interprets `width` and `height` correctly (1,
  [\#2510](https://github.com/tidyverse/ggplot2/issues/2510)).

- [`position_jitter()`](https://ggplot2.tidyverse.org/dev/reference/position_jitter.md)
  and
  [`position_jitterdodge()`](https://ggplot2.tidyverse.org/dev/reference/position_jitterdodge.md)
  gain a `seed` argument that allows the specification of a random seed
  for reproducible jittering ([@krlmlr](https://github.com/krlmlr),
  [\#1996](https://github.com/tidyverse/ggplot2/issues/1996) and
  [@slowkow](https://github.com/slowkow),
  [\#2445](https://github.com/tidyverse/ggplot2/issues/2445)).

- [`stat_density()`](https://ggplot2.tidyverse.org/dev/reference/geom_density.md)
  has better behaviour if all groups are dropped because they are too
  small ([\#2282](https://github.com/tidyverse/ggplot2/issues/2282)).

- [`stat_summary_bin()`](https://ggplot2.tidyverse.org/dev/reference/stat_summary.md)
  now understands the `breaks` parameter
  ([@karawoo](https://github.com/karawoo),
  [\#2214](https://github.com/tidyverse/ggplot2/issues/2214)).

- [`stat_bin()`](https://ggplot2.tidyverse.org/dev/reference/geom_histogram.md)
  now accepts functions for `binwidth`. This allows better binning when
  faceting along variables with different ranges
  ([@botanize](https://github.com/botanize)).

- [`stat_bin()`](https://ggplot2.tidyverse.org/dev/reference/geom_histogram.md)
  and
  [`geom_histogram()`](https://ggplot2.tidyverse.org/dev/reference/geom_histogram.md)
  now sum correctly when using the `weight` aesthetic
  ([@jiho](https://github.com/jiho),
  [\#1921](https://github.com/tidyverse/ggplot2/issues/1921)).

- [`stat_bin()`](https://ggplot2.tidyverse.org/dev/reference/geom_histogram.md)
  again uses correct scaling for the computed variable `ndensity`
  ([@timgoodman](https://github.com/timgoodman),
  [\#2324](https://github.com/tidyverse/ggplot2/issues/2324)).

- [`stat_bin()`](https://ggplot2.tidyverse.org/dev/reference/geom_histogram.md)
  and
  [`stat_bin_2d()`](https://ggplot2.tidyverse.org/dev/reference/geom_bin_2d.md)
  now properly handle the `breaks` parameter when the scales are
  transformed ([@has2k1](https://github.com/has2k1),
  [\#2366](https://github.com/tidyverse/ggplot2/issues/2366)).

- [`update_geom_defaults()`](https://ggplot2.tidyverse.org/dev/reference/update_defaults.md)
  and
  [`update_stat_defaults()`](https://ggplot2.tidyverse.org/dev/reference/update_defaults.md)
  allow American spelling of aesthetic parameters
  ([@foo-bar-baz-qux](https://github.com/foo-bar-baz-qux),
  [\#2299](https://github.com/tidyverse/ggplot2/issues/2299)).

- The `show.legend` parameter now accepts a named logical vector to
  hide/show only some aesthetics in the legend
  ([@tutuchan](https://github.com/tutuchan),
  [\#1798](https://github.com/tidyverse/ggplot2/issues/1798)).

- Layers now silently ignore unknown aesthetics with value `NULL`
  ([\#1909](https://github.com/tidyverse/ggplot2/issues/1909)).

#### Coords

- Clipping to the plot panel is now configurable, through a `clip`
  argument to coordinate systems, e.g. `coord_cartesian(clip = "off")`
  ([@clauswilke](https://github.com/clauswilke),
  [\#2536](https://github.com/tidyverse/ggplot2/issues/2536)).

- Like scales, coordinate systems now give you a message when you’re
  replacing an existing coordinate system
  ([\#2264](https://github.com/tidyverse/ggplot2/issues/2264)).

- [`coord_polar()`](https://ggplot2.tidyverse.org/dev/reference/coord_radial.md)
  now draws secondary axis ticks and labels
  ([@dylan-stark](https://github.com/dylan-stark),
  [\#2072](https://github.com/tidyverse/ggplot2/issues/2072)), and can
  draw the radius axis on the right
  ([@thomasp85](https://github.com/thomasp85),
  [\#2005](https://github.com/tidyverse/ggplot2/issues/2005)).

- [`coord_trans()`](https://ggplot2.tidyverse.org/dev/reference/coord_transform.md)
  now generates a warning when a transformation generates non-finite
  values ([@foo-bar-baz-qux](https://github.com/foo-bar-baz-qux),
  [\#2147](https://github.com/tidyverse/ggplot2/issues/2147)).

#### Themes

- Complete themes now always override all elements of the default theme
  ([@has2k1](https://github.com/has2k1),
  [\#2058](https://github.com/tidyverse/ggplot2/issues/2058),
  [\#2079](https://github.com/tidyverse/ggplot2/issues/2079)).

- Themes now set default grid colour in `panel.grid` rather than
  individually in `panel.grid.major` and `panel.grid.minor`
  individually. This makes it slightly easier to customise the theme
  ([\#2352](https://github.com/tidyverse/ggplot2/issues/2352)).

- Fixed bug when setting strips to
  [`element_blank()`](https://ggplot2.tidyverse.org/dev/reference/element.md)
  ([@thomasp85](https://github.com/thomasp85)).

- Axes positioned on the top and to the right can now customize their
  ticks and lines separately
  ([@thomasp85](https://github.com/thomasp85),
  [\#1899](https://github.com/tidyverse/ggplot2/issues/1899)).

- Built-in themes gain parameters `base_line_size` and `base_rect_size`
  which control the default sizes of line and rectangle elements
  ([@karawoo](https://github.com/karawoo),
  [\#2176](https://github.com/tidyverse/ggplot2/issues/2176)).

- Default themes use
  [`rel()`](https://ggplot2.tidyverse.org/dev/reference/element.md) to
  set line widths ([@baptiste](https://github.com/baptiste)).

- Themes were tweaked for visual consistency and more graceful behavior
  when changing the base font size. All absolute heights or widths were
  replaced with heights or widths that are proportional to the base font
  size. One relative font size was eliminated
  ([@clauswilke](https://github.com/clauswilke)).

- The height of descenders is now calculated solely on font metrics and
  doesn’t change with the specific letters in the string. This fixes
  minor alignment issues with plot titles, subtitles, and legend titles
  ([\#2288](https://github.com/tidyverse/ggplot2/issues/2288),
  [@clauswilke](https://github.com/clauswilke)).

#### Guides

- [`guide_colorbar()`](https://ggplot2.tidyverse.org/dev/reference/guide_colourbar.md)
  is more configurable: tick marks and color bar frame can now by styled
  with arguments `ticks.colour`, `ticks.linewidth`, `frame.colour`,
  `frame.linewidth`, and `frame.linetype`
  ([@clauswilke](https://github.com/clauswilke)).

- [`guide_colorbar()`](https://ggplot2.tidyverse.org/dev/reference/guide_colourbar.md)
  now uses `legend.spacing.x` and `legend.spacing.y` correctly, and it
  can handle multi-line titles. Minor tweaks were made to
  [`guide_legend()`](https://ggplot2.tidyverse.org/dev/reference/guide_legend.md)
  to make sure the two legend functions behave as similarly as possible
  ([@clauswilke](https://github.com/clauswilke),
  [\#2397](https://github.com/tidyverse/ggplot2/issues/2397) and
  [\#2398](https://github.com/tidyverse/ggplot2/issues/2398)).

- The theme elements `legend.title` and `legend.text` now respect the
  settings of `margin`, `hjust`, and `vjust`
  ([@clauswilke](https://github.com/clauswilke),
  [\#2465](https://github.com/tidyverse/ggplot2/issues/2465),
  [\#1502](https://github.com/tidyverse/ggplot2/issues/1502)).

- Non-angle parameters of `label.theme` or `title.theme` can now be set
  in
  [`guide_legend()`](https://ggplot2.tidyverse.org/dev/reference/guide_legend.md)
  and
  [`guide_colorbar()`](https://ggplot2.tidyverse.org/dev/reference/guide_colourbar.md)
  ([@clauswilke](https://github.com/clauswilke),
  [\#2544](https://github.com/tidyverse/ggplot2/issues/2544)).

#### Other

- [`fortify()`](https://ggplot2.tidyverse.org/dev/reference/fortify.md)
  gains a method for tbls ([@karawoo](https://github.com/karawoo),
  [\#2218](https://github.com/tidyverse/ggplot2/issues/2218)).

- `ggplot` gains a method for `grouped_df`s that adds a `.group`
  variable, which computes a unique value for each group. Use it with
  `aes(group = .group)`
  ([\#2351](https://github.com/tidyverse/ggplot2/issues/2351)).

- [`ggproto()`](https://ggplot2.tidyverse.org/dev/reference/ggproto.md)
  produces objects with class `c("ggproto", "gg")`, allowing for a more
  informative error message when adding layers, scales, or other ggproto
  objects ([@jrnold](https://github.com/jrnold),
  [\#2056](https://github.com/tidyverse/ggplot2/issues/2056)).

- [`ggsave()`](https://ggplot2.tidyverse.org/dev/reference/ggsave.md)’s
  DPI argument now supports 3 string options: “retina” (320 DPI),
  “print” (300 DPI), and “screen” (72 DPI)
  ([@foo-bar-baz-qux](https://github.com/foo-bar-baz-qux),
  [\#2156](https://github.com/tidyverse/ggplot2/issues/2156)).
  [`ggsave()`](https://ggplot2.tidyverse.org/dev/reference/ggsave.md)
  now uses full argument names to avoid partial match warnings
  ([\#2355](https://github.com/tidyverse/ggplot2/issues/2355)), and
  correctly restores the previous graphics device when several graphics
  devices are open
  ([\#2363](https://github.com/tidyverse/ggplot2/issues/2363)).

- [`print.ggplot()`](https://ggplot2.tidyverse.org/dev/reference/print.ggplot.md)
  now returns the original ggplot object, instead of the output from
  [`ggplot_build()`](https://ggplot2.tidyverse.org/dev/reference/ggplot_build.md).
  Also, the object returned from
  [`ggplot_build()`](https://ggplot2.tidyverse.org/dev/reference/ggplot_build.md)
  now has the class `"ggplot_built"`
  ([\#2034](https://github.com/tidyverse/ggplot2/issues/2034)).

- [`map_data()`](https://ggplot2.tidyverse.org/dev/reference/map_data.md)
  now works even when purrr is loaded (tidyverse#66).

- New functions
  [`summarise_layout()`](https://ggplot2.tidyverse.org/dev/reference/summarise_plot.md),
  [`summarise_coord()`](https://ggplot2.tidyverse.org/dev/reference/summarise_plot.md),
  and
  [`summarise_layers()`](https://ggplot2.tidyverse.org/dev/reference/summarise_plot.md)
  summarise the layout, coordinate systems, and layers of a built ggplot
  object ([\#2034](https://github.com/tidyverse/ggplot2/issues/2034),
  [@wch](https://github.com/wch)). This provides a tested API that
  (e.g.) shiny can depend on.

- Updated startup messages reflect new resources
  ([\#2410](https://github.com/tidyverse/ggplot2/issues/2410),
  [@mine-cetinkaya-rundel](https://github.com/mine-cetinkaya-rundel)).

## ggplot2 2.2.1

CRAN release: 2016-12-30

- Fix usage of `structure(NULL)` for R-devel compatibility
  ([\#1968](https://github.com/tidyverse/ggplot2/issues/1968)).

## ggplot2 2.2.0

CRAN release: 2016-11-11

### Major new features

#### Subtitle and caption

Thanks to [@hrbrmstr](https://github.com/hrbrmstr) plots now have
subtitles and captions, which can be set with the `subtitle` and
`caption` arguments to
[`ggtitle()`](https://ggplot2.tidyverse.org/dev/reference/labs.md) and
[`labs()`](https://ggplot2.tidyverse.org/dev/reference/labs.md). You can
control their appearance with the theme settings `plot.caption` and
`plot.subtitle`. The main plot title is now left-aligned to better work
better with a subtitle. The caption is right-aligned
([@hrbrmstr](https://github.com/hrbrmstr)).

#### Stacking

[`position_stack()`](https://ggplot2.tidyverse.org/dev/reference/position_stack.md)
and
[`position_fill()`](https://ggplot2.tidyverse.org/dev/reference/position_stack.md)
now sort the stacking order to match grouping order. This allows you to
control the order through grouping, and ensures that the default legend
matches the plot
([\#1552](https://github.com/tidyverse/ggplot2/issues/1552),
[\#1593](https://github.com/tidyverse/ggplot2/issues/1593)). If you want
the opposite order (useful if you have horizontal bars and horizontal
legend), you can request reverse stacking by using
`position = position_stack(reverse = TRUE)`
([\#1837](https://github.com/tidyverse/ggplot2/issues/1837)).

[`position_stack()`](https://ggplot2.tidyverse.org/dev/reference/position_stack.md)
and
[`position_fill()`](https://ggplot2.tidyverse.org/dev/reference/position_stack.md)
now accepts negative values which will create stacks extending below the
x-axis ([\#1691](https://github.com/tidyverse/ggplot2/issues/1691)).

[`position_stack()`](https://ggplot2.tidyverse.org/dev/reference/position_stack.md)
and
[`position_fill()`](https://ggplot2.tidyverse.org/dev/reference/position_stack.md)
gain a `vjust` argument which makes it easy to (e.g.) display labels in
the middle of stacked bars
([\#1821](https://github.com/tidyverse/ggplot2/issues/1821)).

#### Layers

[`geom_col()`](https://ggplot2.tidyverse.org/dev/reference/geom_bar.md)
was added to complement
[`geom_bar()`](https://ggplot2.tidyverse.org/dev/reference/geom_bar.md)
([@hrbrmstr](https://github.com/hrbrmstr)). It uses `stat="identity"` by
default, making the `y` aesthetic mandatory. It does not support any
other `stat_()` and does not provide fallback support for the `binwidth`
parameter. Examples and references in other functions were updated to
demonstrate
[`geom_col()`](https://ggplot2.tidyverse.org/dev/reference/geom_bar.md)
usage.

When creating a layer, ggplot2 will warn if you use an unknown aesthetic
or an unknown parameter. Compared to the previous version, this is
stricter for aesthetics (previously there was no message), and less
strict for parameters (previously this threw an error)
([\#1585](https://github.com/tidyverse/ggplot2/issues/1585)).

#### Facetting

The facet system, as well as the internal panel class, has been
rewritten in ggproto. Facets are now extendable in the same manner as
geoms and stats, as described in
[`vignette("extending-ggplot2")`](https://ggplot2.tidyverse.org/dev/articles/extending-ggplot2.md).

We have also added the following new features.

- [`facet_grid()`](https://ggplot2.tidyverse.org/dev/reference/facet_grid.md)
  and
  [`facet_wrap()`](https://ggplot2.tidyverse.org/dev/reference/facet_wrap.md)
  now allow expressions in their faceting formulas
  ([@DanRuderman](https://github.com/DanRuderman),
  [\#1596](https://github.com/tidyverse/ggplot2/issues/1596)).

- When
  [`facet_wrap()`](https://ggplot2.tidyverse.org/dev/reference/facet_wrap.md)
  results in an uneven number of panels, axes will now be drawn
  underneath the hanging panels (fixes
  [\#1607](https://github.com/tidyverse/ggplot2/issues/1607))

- Strips can now be freely positioned in
  [`facet_wrap()`](https://ggplot2.tidyverse.org/dev/reference/facet_wrap.md)
  using the `strip.position` argument (deprecates `switch`).

- The relative order of panel, strip, and axis can now be controlled
  with the theme setting `strip.placement` that takes either `inside`
  (strip between panel and axis) or `outside` (strip after axis).

- The theme option `panel.margin` has been deprecated in favour of
  `panel.spacing` to more clearly communicate intent.

#### Extensions

Unfortunately there was a major oversight in the construction of ggproto
which lead to extensions capturing the super object at package build
time, instead of at package run time
([\#1826](https://github.com/tidyverse/ggplot2/issues/1826)). This
problem has been fixed, but requires re-installation of all extension
packages.

### Scales

- The position of x and y axes can now be changed using the `position`
  argument in `scale_x_*`and `scale_y_*` which can take `top` and
  `bottom`, and `left` and `right` respectively. The themes of top and
  right axes can be modified using the `.top` and `.right` modifiers to
  `axis.text.*` and `axis.title.*`.

#### Continuous scales

- [`scale_x_continuous()`](https://ggplot2.tidyverse.org/dev/reference/scale_continuous.md)
  and
  [`scale_y_continuous()`](https://ggplot2.tidyverse.org/dev/reference/scale_continuous.md)
  can now display a secondary axis that is a **one-to-one**
  transformation of the primary axis (e.g. degrees Celcius to degrees
  Fahrenheit). The secondary axis will be positioned opposite to the
  primary axis and can be controlled with the `sec.axis` argument to the
  scale constructor.

- Scales worry less about having breaks. If no breaks can be computed,
  the plot will work instead of throwing an uninformative error
  ([\#791](https://github.com/tidyverse/ggplot2/issues/791)). This is
  particularly helpful when you have facets with free scales, and not
  all panels contain data.

- Scales now warn when transformation introduces infinite values
  ([\#1696](https://github.com/tidyverse/ggplot2/issues/1696)).

#### Date time

- `scale_*_datetime()` now supports time zones. It will use the timezone
  attached to the variable by default, but can be overridden with the
  `timezone` argument.

- New
  [`scale_x_time()`](https://ggplot2.tidyverse.org/dev/reference/scale_date.md)
  and
  [`scale_y_time()`](https://ggplot2.tidyverse.org/dev/reference/scale_date.md)
  generate reasonable default breaks and labels for hms vectors
  ([\#1752](https://github.com/tidyverse/ggplot2/issues/1752)).

#### Discrete scales

The treatment of missing values by discrete scales has been thoroughly
overhauled ([\#1584](https://github.com/tidyverse/ggplot2/issues/1584)).
The underlying principle is that we can naturally represent missing
values on discrete variables (by treating just like another level), so
by default we should.

This principle applies to:

- character vectors
- factors with implicit NA
- factors with explicit NA

And to all scales (both position and non-position.)

Compared to the previous version of ggplot2, there are three main
changes:

1.  [`scale_x_discrete()`](https://ggplot2.tidyverse.org/dev/reference/scale_discrete.md)
    and
    [`scale_y_discrete()`](https://ggplot2.tidyverse.org/dev/reference/scale_discrete.md)
    always show discrete NA, regardless of their source

2.  If present, `NA`s are shown in discrete legends.

3.  All discrete scales gain a `na.translate` argument that allows you
    to control whether `NA`s are translated to something that can be
    visualised, or should be left as missing. Note that if you don’t
    translate (i.e. `na.translate = FALSE)` the missing values will
    passed on to the layer, which will warning that it’s dropping
    missing values. To suppress the warnings, you’ll also need to add
    `na.rm = TRUE` to the layer call.

There were also a number of other smaller changes

- Correctly use scale expansion factors.
- Don’t preserve space for dropped levels
  ([\#1638](https://github.com/tidyverse/ggplot2/issues/1638)).
- Only issue one warning when when asking for too many levels
  ([\#1674](https://github.com/tidyverse/ggplot2/issues/1674)).
- Unicode labels work better on Windows
  ([\#1827](https://github.com/tidyverse/ggplot2/issues/1827)).
- Warn when used with only continuous data
  ([\#1589](https://github.com/tidyverse/ggplot2/issues/1589))

### Themes

- The [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md)
  constructor now has named arguments rather than ellipses. This should
  make autocomplete substantially more useful. The documentation
  (including examples) has been considerably improved.

- Built-in themes are more visually homogeneous, and match `theme_grey`
  better. ([@jiho](https://github.com/jiho),
  [\#1679](https://github.com/tidyverse/ggplot2/issues/1679))

- When computing the height of titles, ggplot2 now includes the height
  of the descenders (i.e. the bits of `g` and `y` that hang beneath the
  baseline). This improves the margins around titles, particularly the y
  axis label
  ([\#1712](https://github.com/tidyverse/ggplot2/issues/1712)). I have
  also very slightly increased the inner margins of axis titles, and
  removed the outer margins.

- Theme element inheritance is now easier to work with as modification
  now overrides default `element_blank` elements
  ([\#1555](https://github.com/tidyverse/ggplot2/issues/1555),
  [\#1557](https://github.com/tidyverse/ggplot2/issues/1557),
  [\#1565](https://github.com/tidyverse/ggplot2/issues/1565),
  [\#1567](https://github.com/tidyverse/ggplot2/issues/1567))

- Horizontal legends (i.e. legends on the top or bottom) are
  horizontally aligned by default
  ([\#1842](https://github.com/tidyverse/ggplot2/issues/1842)). Use
  `legend.box = "vertical"` to switch back to the previous behaviour.

- [`element_line()`](https://ggplot2.tidyverse.org/dev/reference/element.md)
  now takes an `arrow` argument to specify arrows at the end of lines
  ([\#1740](https://github.com/tidyverse/ggplot2/issues/1740))

There were a number of tweaks to the theme elements that control
legends:

- `legend.justification` now controls appearance will plotting the
  legend outside of the plot area. For example, you can use
  `theme(legend.justification = "top")` to make the legend align with
  the top of the plot.

- `panel.margin` and `legend.margin` have been renamed to
  `panel.spacing` and `legend.spacing` respectively, to better
  communicate intent (they only affect spacing between legends and
  panels, not the margins around them)

- `legend.margin` now controls margin around individual legends.

- New `legend.box.background`, `legend.box.spacing`, and
  `legend.box.margin` control the background, spacing, and margin of the
  legend box (the region that contains all legends).

### Bug fixes and minor improvements

- ggplot2 now imports tibble. This ensures that all built-in datasets
  print compactly even if you haven’t explicitly loaded tibble or dplyr
  ([\#1677](https://github.com/tidyverse/ggplot2/issues/1677)).

- Class of aesthetic mapping is preserved when adding
  [`aes()`](https://ggplot2.tidyverse.org/dev/reference/aes.md) objects
  ([\#1624](https://github.com/tidyverse/ggplot2/issues/1624)).

- `+.gg` now works for lists that include data frames.

- `annotation_x()` now works in the absense of global data
  ([\#1655](https://github.com/tidyverse/ggplot2/issues/1655))

- `geom_*(show.legend = FALSE)` now works for `guide_colorbar`.

- [`geom_boxplot()`](https://ggplot2.tidyverse.org/dev/reference/geom_boxplot.md)
  gains new `outlier.alpha`
  ([@jonathan-g](https://github.com/jonathan-g)) and `outlier.fill`
  ([@schloerke](https://github.com/schloerke),
  [\#1787](https://github.com/tidyverse/ggplot2/issues/1787)) parameters
  to control the alpha/fill of outlier points independently of the alpha
  of the boxes.

- [`position_jitter()`](https://ggplot2.tidyverse.org/dev/reference/position_jitter.md)
  (and hence
  [`geom_jitter()`](https://ggplot2.tidyverse.org/dev/reference/geom_jitter.md))
  now correctly computes the jitter width/jitter when supplied by the
  user ([\#1775](https://github.com/tidyverse/ggplot2/issues/1775),
  [@has2k1](https://github.com/has2k1)).

- [`geom_contour()`](https://ggplot2.tidyverse.org/dev/reference/geom_contour.md)
  more clearly describes what inputs it needs
  ([\#1577](https://github.com/tidyverse/ggplot2/issues/1577)).

- [`geom_curve()`](https://ggplot2.tidyverse.org/dev/reference/geom_segment.md)
  respects the `lineend` parameter
  ([\#1852](https://github.com/tidyverse/ggplot2/issues/1852)).

- [`geom_histogram()`](https://ggplot2.tidyverse.org/dev/reference/geom_histogram.md)
  and
  [`stat_bin()`](https://ggplot2.tidyverse.org/dev/reference/geom_histogram.md)
  understand the `breaks` parameter once more.
  ([\#1665](https://github.com/tidyverse/ggplot2/issues/1665)). The
  floating point adjustment for histogram bins is now actually used - it
  was previously inadvertently ignored
  ([\#1651](https://github.com/tidyverse/ggplot2/issues/1651)).

- [`geom_violin()`](https://ggplot2.tidyverse.org/dev/reference/geom_violin.md)
  no longer transforms quantile lines with the alpha aesthetic
  ([@mnbram](https://github.com/mnbram),
  [\#1714](https://github.com/tidyverse/ggplot2/issues/1714)). It no
  longer errors when quantiles are requested but data have zero range
  ([\#1687](https://github.com/tidyverse/ggplot2/issues/1687)). When
  `trim = FALSE` it once again has a nice range that allows the density
  to reach zero (by extending the range 3 bandwidths to either side of
  the data)
  ([\#1700](https://github.com/tidyverse/ggplot2/issues/1700)).

- [`geom_dotplot()`](https://ggplot2.tidyverse.org/dev/reference/geom_dotplot.md)
  works better when faceting and binning on the y-axis.
  ([\#1618](https://github.com/tidyverse/ggplot2/issues/1618),
  [@has2k1](https://github.com/has2k1)).

- `geom_hexbin()` once again supports `..density..`
  ([@mikebirdgeneau](https://github.com/mikebirdgeneau),
  [\#1688](https://github.com/tidyverse/ggplot2/issues/1688)).

- [`geom_step()`](https://ggplot2.tidyverse.org/dev/reference/geom_path.md)
  gives useful warning if only one data point in layer
  ([\#1645](https://github.com/tidyverse/ggplot2/issues/1645)).

- [`layer()`](https://ggplot2.tidyverse.org/dev/reference/layer.md)
  gains new `check.aes` and `check.param` arguments. These allow
  geom/stat authors to optional suppress checks for known
  aesthetics/parameters. Currently this is used only in
  [`geom_blank()`](https://ggplot2.tidyverse.org/dev/reference/geom_blank.md)
  which powers
  [`expand_limits()`](https://ggplot2.tidyverse.org/dev/reference/expand_limits.md)
  ([\#1795](https://github.com/tidyverse/ggplot2/issues/1795)).

- All `stat_*()` display a better error message when required aesthetics
  are missing.

- [`stat_bin()`](https://ggplot2.tidyverse.org/dev/reference/geom_histogram.md)
  and
  [`stat_summary_hex()`](https://ggplot2.tidyverse.org/dev/reference/stat_summary_2d.md)
  now accept length 1 `binwidth`
  ([\#1610](https://github.com/tidyverse/ggplot2/issues/1610))

- [`stat_density()`](https://ggplot2.tidyverse.org/dev/reference/geom_density.md)
  gains new argument `n`, which is passed to underlying function
  [`stats::density`](https://rdrr.io/r/stats/density.html) (“number of
  equally spaced points at which the density is to be estimated”).
  ([@hbuschme](https://github.com/hbuschme))

- [`stat_binhex()`](https://ggplot2.tidyverse.org/dev/reference/geom_hex.md)
  now again returns `count` rather than `value`
  ([\#1747](https://github.com/tidyverse/ggplot2/issues/1747))

- [`stat_ecdf()`](https://ggplot2.tidyverse.org/dev/reference/stat_ecdf.md)
  respects `pad` argument
  ([\#1646](https://github.com/tidyverse/ggplot2/issues/1646)).

- [`stat_smooth()`](https://ggplot2.tidyverse.org/dev/reference/geom_smooth.md)
  once again informs you about the method it has chosen. It also
  correctly calculates the size of the largest group within facets.

- `x` and `y` scales are now symmetric regarding the list of aesthetics
  they accept: `xmin_final`, `xmax_final`, `xlower`, `xmiddle` and
  `xupper` are now valid `x` aesthetics.

- `Scale` extensions can now override the `make_title` and
  `make_sec_title` methods to let the scale modify the axis/legend
  titles.

- The random stream is now reset after calling `.onAttach()`
  ([\#2409](https://github.com/tidyverse/ggplot2/issues/2409)).

## ggplot2 2.1.0

CRAN release: 2016-03-01

### New features

- When mapping an aesthetic to a constant (e.g.
  `geom_smooth(aes(colour = "loess")))`), the default guide title is the
  name of the aesthetic (i.e. “colour”), not the value (i.e. “loess”)
  ([\#1431](https://github.com/tidyverse/ggplot2/issues/1431)).

- [`layer()`](https://ggplot2.tidyverse.org/dev/reference/layer.md) now
  accepts a function as the data argument. The function will be applied
  to the data passed to the
  [`ggplot()`](https://ggplot2.tidyverse.org/dev/reference/ggplot.md)
  function and must return a data.frame
  ([\#1527](https://github.com/tidyverse/ggplot2/issues/1527),
  [@thomasp85](https://github.com/thomasp85)). This is a more general
  version of the deprecated `subset` argument.

- [`theme_update()`](https://ggplot2.tidyverse.org/dev/reference/get_theme.md)
  now uses the `+` operator instead of `%+replace%`, so that unspecified
  values will no longer be `NULL`ed out.
  [`theme_replace()`](https://ggplot2.tidyverse.org/dev/reference/get_theme.md)
  preserves the old behaviour if desired
  ([@oneillkza](https://github.com/oneillkza),
  [\#1519](https://github.com/tidyverse/ggplot2/issues/1519)).

- [`stat_bin()`](https://ggplot2.tidyverse.org/dev/reference/geom_histogram.md)
  has been overhauled to use the same algorithm as ggvis, which has been
  considerably improved thanks to the advice of Randy Prium
  ([@rpruim](https://github.com/rpruim)). This includes:

  - Better arguments and a better algorithm for determining the origin.
    You can now specify either `boundary` or the `center` of a bin.
    `origin` has been deprecated in favour of these arguments.

  - `drop` is deprecated in favour of `pad`, which adds extra 0-count
    bins at either end (needed for frequency polygons).
    [`geom_histogram()`](https://ggplot2.tidyverse.org/dev/reference/geom_histogram.md)
    defaults to `pad = FALSE` which considerably improves the default
    limits for the histogram, especially when the bins are big
    ([\#1477](https://github.com/tidyverse/ggplot2/issues/1477)).

  - The default algorithm does a (somewhat) better job at picking nice
    widths and origins across a wider range of input data.

  - `bins = n` now gives a histogram with `n` bins, not `n + 1`
    ([\#1487](https://github.com/tidyverse/ggplot2/issues/1487)).

### Bug fixes

- All `\donttest{}` examples run.

- All `geom_()` and `stat_()` functions now have consistent argument
  order: data + mapping, then geom/stat/position, then `...`, then
  specific arguments, then arguments common to all layers
  ([\#1305](https://github.com/tidyverse/ggplot2/issues/1305)). This may
  break code if you were previously relying on partial name matching,
  but in the long-term should make ggplot2 easier to use. In particular,
  you can now set the `n` parameter in
  [`geom_density2d()`](https://ggplot2.tidyverse.org/dev/reference/geom_density_2d.md)
  without it partially matching `na.rm`
  ([\#1485](https://github.com/tidyverse/ggplot2/issues/1485)).

- For geoms with both `colour` and `fill`, `alpha` once again only
  affects fill (Reverts
  [\#1371](https://github.com/tidyverse/ggplot2/issues/1371),
  [\#1523](https://github.com/tidyverse/ggplot2/issues/1523)). This was
  causing problems for people.

- [`facet_wrap()`](https://ggplot2.tidyverse.org/dev/reference/facet_wrap.md)/[`facet_grid()`](https://ggplot2.tidyverse.org/dev/reference/facet_grid.md)
  works with multiple empty panels of data
  ([\#1445](https://github.com/tidyverse/ggplot2/issues/1445)).

- [`facet_wrap()`](https://ggplot2.tidyverse.org/dev/reference/facet_wrap.md)
  correctly swaps `nrow` and `ncol` when faceting vertically
  ([\#1417](https://github.com/tidyverse/ggplot2/issues/1417)).

- `ggsave("x.svg")` now uses svglite to produce the svg
  ([\#1432](https://github.com/tidyverse/ggplot2/issues/1432)).

- [`geom_boxplot()`](https://ggplot2.tidyverse.org/dev/reference/geom_boxplot.md)
  now understands `outlier.color`
  ([\#1455](https://github.com/tidyverse/ggplot2/issues/1455)).

- [`geom_path()`](https://ggplot2.tidyverse.org/dev/reference/geom_path.md)
  knows that “solid” (not just 1) represents a solid line
  ([\#1534](https://github.com/tidyverse/ggplot2/issues/1534)).

- [`geom_ribbon()`](https://ggplot2.tidyverse.org/dev/reference/geom_ribbon.md)
  preserves missing values so they correctly generate a gap in the
  ribbon ([\#1549](https://github.com/tidyverse/ggplot2/issues/1549)).

- [`geom_tile()`](https://ggplot2.tidyverse.org/dev/reference/geom_tile.md)
  once again accepts `width` and `height` parameters
  ([\#1513](https://github.com/tidyverse/ggplot2/issues/1513)). It uses
  [`draw_key_polygon()`](https://ggplot2.tidyverse.org/dev/reference/draw_key.md)
  for better a legend, including a coloured outline
  ([\#1484](https://github.com/tidyverse/ggplot2/issues/1484)).

- [`layer()`](https://ggplot2.tidyverse.org/dev/reference/layer.md) now
  automatically adds a `na.rm` parameter if none is explicitly supplied.

- [`position_jitterdodge()`](https://ggplot2.tidyverse.org/dev/reference/position_jitterdodge.md)
  now works on all possible dodge aesthetics, e.g. `color`, `linetype`
  etc. instead of only based on `fill`
  ([@bleutner](https://github.com/bleutner))

- `position = "nudge"` now works (although it doesn’t do anything
  useful) ([\#1428](https://github.com/tidyverse/ggplot2/issues/1428)).

- The default scale for columns of class “AsIs” is now “identity”
  ([\#1518](https://github.com/tidyverse/ggplot2/issues/1518)).

- `scale_*_discrete()` has better defaults when used with purely
  continuous data
  ([\#1542](https://github.com/tidyverse/ggplot2/issues/1542)).

- [`scale_size()`](https://ggplot2.tidyverse.org/dev/reference/scale_size.md)
  warns when used with categorical data.

- [`scale_size()`](https://ggplot2.tidyverse.org/dev/reference/scale_size.md),
  `scale_colour()`, and `scale_fill()` gain date and date-time variants
  ([\#1526](https://github.com/tidyverse/ggplot2/issues/1526)).

- [`stat_bin_hex()`](https://ggplot2.tidyverse.org/dev/reference/geom_hex.md)
  and `stat_bin_summary()` now use the same underlying algorithm so
  results are consistent
  ([\#1383](https://github.com/tidyverse/ggplot2/issues/1383)).
  [`stat_bin_hex()`](https://ggplot2.tidyverse.org/dev/reference/geom_hex.md)
  now accepts a `weight` aesthetic. To be consistent with related stats,
  the output variable from
  [`stat_bin_hex()`](https://ggplot2.tidyverse.org/dev/reference/geom_hex.md)
  is now value instead of count.

- [`stat_density()`](https://ggplot2.tidyverse.org/dev/reference/geom_density.md)
  gains a `bw` parameter which makes it easy to get consistent smoothing
  between facets ([@jiho](https://github.com/jiho))

- `stat-density-2d()` no longer ignores the `h` parameter, and now
  accepts `bins` and `binwidth` parameters to control the number of
  contours ([\#1448](https://github.com/tidyverse/ggplot2/issues/1448),
  [@has2k1](https://github.com/has2k1)).

- [`stat_ecdf()`](https://ggplot2.tidyverse.org/dev/reference/stat_ecdf.md)
  does a better job of adding padding to -Inf/Inf, and gains an argument
  `pad` to suppress the padding if not needed
  ([\#1467](https://github.com/tidyverse/ggplot2/issues/1467)).

- [`stat_function()`](https://ggplot2.tidyverse.org/dev/reference/geom_function.md)
  gains an `xlim` parameter
  ([\#1528](https://github.com/tidyverse/ggplot2/issues/1528)). It once
  again works with discrete x values
  ([\#1509](https://github.com/tidyverse/ggplot2/issues/1509)).

- [`stat_summary()`](https://ggplot2.tidyverse.org/dev/reference/stat_summary.md)
  preserves sorted x order which avoids artefacts when display results
  with
  [`geom_smooth()`](https://ggplot2.tidyverse.org/dev/reference/geom_smooth.md)
  ([\#1520](https://github.com/tidyverse/ggplot2/issues/1520)).

- All elements should now inherit correctly for all themes except
  [`theme_void()`](https://ggplot2.tidyverse.org/dev/reference/ggtheme.md).
  ([@Katiedaisey](https://github.com/Katiedaisey),
  [\#1555](https://github.com/tidyverse/ggplot2/issues/1555))

- [`theme_void()`](https://ggplot2.tidyverse.org/dev/reference/ggtheme.md)
  was completely void of text but facets and legends still need labels.
  They are now visible ([@jiho](https://github.com/jiho)).

- You can once again set legend key and height width to unit arithmetic
  objects (like `2 * unit(1, "cm")`)
  ([\#1437](https://github.com/tidyverse/ggplot2/issues/1437)).

- Eliminate spurious warning if you have a layer with no data and no
  aesthetics
  ([\#1451](https://github.com/tidyverse/ggplot2/issues/1451)).

- Removed a superfluous comma in `theme-defaults.r` code
  ([@jschoeley](https://github.com/jschoeley))

- Fixed a compatibility issue with `ggproto` and R versions prior to
  3.1.2. ([\#1444](https://github.com/tidyverse/ggplot2/issues/1444))

- Fixed issue where
  [`coord_map()`](https://ggplot2.tidyverse.org/dev/reference/coord_map.md)
  fails when given an explicit `parameters` argument
  ([@tdmcarthur](https://github.com/tdmcarthur),
  [\#1729](https://github.com/tidyverse/ggplot2/issues/1729))

- Fixed issue where
  [`geom_errorbarh()`](https://ggplot2.tidyverse.org/dev/reference/geom_linerange.md)
  had a required `x` aesthetic
  ([\#1933](https://github.com/tidyverse/ggplot2/issues/1933))

## ggplot2 2.0.0

CRAN release: 2015-12-18

### Major changes

- ggplot no longer throws an error if your plot has no layers. Instead
  it automatically adds
  [`geom_blank()`](https://ggplot2.tidyverse.org/dev/reference/geom_blank.md)
  ([\#1246](https://github.com/tidyverse/ggplot2/issues/1246)).

- New
  [`cut_width()`](https://ggplot2.tidyverse.org/dev/reference/cut_interval.md)
  is a convenient replacement for the verbose `plyr::round_any()`, with
  the additional benefit of offering finer control.

- New
  [`geom_count()`](https://ggplot2.tidyverse.org/dev/reference/geom_count.md)
  is a convenient alias to
  [`stat_sum()`](https://ggplot2.tidyverse.org/dev/reference/geom_count.md).
  Use it when you have overlapping points on a scatterplot.
  [`stat_sum()`](https://ggplot2.tidyverse.org/dev/reference/geom_count.md)
  now defaults to using counts instead of proportions.

- New
  [`geom_curve()`](https://ggplot2.tidyverse.org/dev/reference/geom_segment.md)
  adds curved lines, with a similar specification to
  [`geom_segment()`](https://ggplot2.tidyverse.org/dev/reference/geom_segment.md)
  ([@veraanadi](https://github.com/veraanadi),
  [\#1088](https://github.com/tidyverse/ggplot2/issues/1088)).

- Date and datetime scales now have `date_breaks`, `date_minor_breaks`
  and `date_labels` arguments so that you never need to use the long
  [`scales::date_breaks()`](https://scales.r-lib.org/reference/date_breaks.html)
  or
  [`scales::date_format()`](https://scales.r-lib.org/reference/date_format.html).

- [`geom_bar()`](https://ggplot2.tidyverse.org/dev/reference/geom_bar.md)
  now has it’s own stat, distinct from
  [`stat_bin()`](https://ggplot2.tidyverse.org/dev/reference/geom_histogram.md)
  which was also used by
  [`geom_histogram()`](https://ggplot2.tidyverse.org/dev/reference/geom_histogram.md).
  [`geom_bar()`](https://ggplot2.tidyverse.org/dev/reference/geom_bar.md)
  now uses
  [`stat_count()`](https://ggplot2.tidyverse.org/dev/reference/geom_bar.md)
  which counts values at each distinct value of x (i.e. it does not bin
  the data first). This can be useful when you want to show exactly
  which values are used in a continuous variable.

- [`geom_point()`](https://ggplot2.tidyverse.org/dev/reference/geom_point.md)
  gains a `stroke` aesthetic which controls the border width of shapes
  21-25 ([\#1133](https://github.com/tidyverse/ggplot2/issues/1133),
  [@SeySayux](https://github.com/SeySayux)). `size` and `stroke` are
  additive so a point with `size = 5` and `stroke = 5` will have a
  diameter of 10mm.
  ([\#1142](https://github.com/tidyverse/ggplot2/issues/1142))

- New
  [`position_nudge()`](https://ggplot2.tidyverse.org/dev/reference/position_nudge.md)
  allows you to slightly offset labels (or other geoms) from their
  corresponding points
  ([\#1109](https://github.com/tidyverse/ggplot2/issues/1109)).

- [`scale_size()`](https://ggplot2.tidyverse.org/dev/reference/scale_size.md)
  now maps values to *area*, not radius. Use
  [`scale_radius()`](https://ggplot2.tidyverse.org/dev/reference/scale_size.md)
  if you want the old behaviour (not recommended, except perhaps for
  lines).

- New
  [`stat_summary_bin()`](https://ggplot2.tidyverse.org/dev/reference/stat_summary.md)
  works like
  [`stat_summary()`](https://ggplot2.tidyverse.org/dev/reference/stat_summary.md)
  but on binned data. It’s a generalisation of
  [`stat_bin()`](https://ggplot2.tidyverse.org/dev/reference/geom_histogram.md)
  that can compute any aggregate, not just counts
  ([\#1274](https://github.com/tidyverse/ggplot2/issues/1274)). Both
  default to
  [`mean_se()`](https://ggplot2.tidyverse.org/dev/reference/mean_se.md)
  if no aggregation functions are supplied
  ([\#1386](https://github.com/tidyverse/ggplot2/issues/1386)).

- Layers are now much stricter about their arguments - you will get an
  error if you’ve supplied an argument that isn’t an aesthetic or a
  parameter. This is likely to cause some short-term pain but in the
  long-term it will make it much easier to spot spelling mistakes and
  other errors
  ([\#1293](https://github.com/tidyverse/ggplot2/issues/1293)).

  This change does break a handful of geoms/stats that used `...` to
  pass additional arguments on to the underlying computation. Now
  [`geom_smooth()`](https://ggplot2.tidyverse.org/dev/reference/geom_smooth.md)/[`stat_smooth()`](https://ggplot2.tidyverse.org/dev/reference/geom_smooth.md)
  and
  [`geom_quantile()`](https://ggplot2.tidyverse.org/dev/reference/geom_quantile.md)/[`stat_quantile()`](https://ggplot2.tidyverse.org/dev/reference/geom_quantile.md)
  use `method.args` instead
  ([\#1245](https://github.com/tidyverse/ggplot2/issues/1245),
  [\#1289](https://github.com/tidyverse/ggplot2/issues/1289)); and
  [`stat_summary()`](https://ggplot2.tidyverse.org/dev/reference/stat_summary.md)
  ([\#1242](https://github.com/tidyverse/ggplot2/issues/1242)),
  [`stat_summary_hex()`](https://ggplot2.tidyverse.org/dev/reference/stat_summary_2d.md),
  and
  [`stat_summary2d()`](https://ggplot2.tidyverse.org/dev/reference/stat_summary_2d.md)
  use `fun.args`.

#### Extensibility

There is now an official mechanism for defining Stats, Geoms, and
Positions in other packages. See
[`vignette("extending-ggplot2")`](https://ggplot2.tidyverse.org/dev/articles/extending-ggplot2.md)
for details.

- All Geoms, Stats and Positions are now exported, so you can inherit
  from them when making your own objects
  ([\#989](https://github.com/tidyverse/ggplot2/issues/989)).

- ggplot2 no longer uses proto or reference classes. Instead, we now use
  ggproto, a new OO system designed specifically for ggplot2. Unlike
  proto and RC, ggproto supports clean cross-package inheritance.
  Creating a new OO system isn’t usually the right way to solve a
  problem, but I’m pretty sure it was necessary here. Read more about it
  in the vignette.

- [`aes_()`](https://ggplot2.tidyverse.org/dev/reference/aes_.md)
  replaces
  [`aes_q()`](https://ggplot2.tidyverse.org/dev/reference/aes_.md). It
  also supports formulas, so the most concise SE version of
  `aes(carat, price)` is now `aes_(~carat, ~price)`. You may want to use
  this form in packages, as it will avoid spurious `R CMD check`
  warnings about undefined global variables.

#### Text

- [`geom_text()`](https://ggplot2.tidyverse.org/dev/reference/geom_text.md)
  has been overhauled to make labelling your data a little easier. It:

  - `nudge_x` and `nudge_y` arguments let you offset labels from their
    corresponding points
    ([\#1120](https://github.com/tidyverse/ggplot2/issues/1120)).

  - `check_overlap = TRUE` provides a simple way to avoid overplotting
    of labels: labels that would otherwise overlap are omitted
    ([\#1039](https://github.com/tidyverse/ggplot2/issues/1039)).

  - `hjust` and `vjust` can now be character vectors: “left”, “center”,
    “right”, “bottom”, “middle”, “top”. New options include “inward” and
    “outward” which align text towards and away from the center of the
    plot respectively.

- [`geom_label()`](https://ggplot2.tidyverse.org/dev/reference/geom_text.md)
  works like
  [`geom_text()`](https://ggplot2.tidyverse.org/dev/reference/geom_text.md)
  but draws a rounded rectangle underneath each label
  ([\#1039](https://github.com/tidyverse/ggplot2/issues/1039)). This is
  useful when you want to label plots that are dense with data.

#### Deprecated features

- The little used
  [`aes_auto()`](https://ggplot2.tidyverse.org/dev/reference/aes_auto.md)
  has been deprecated.

- [`aes_q()`](https://ggplot2.tidyverse.org/dev/reference/aes_.md) has
  been replaced with
  [`aes_()`](https://ggplot2.tidyverse.org/dev/reference/aes_.md) to be
  consistent with SE versions of NSE functions in other packages.

- The `order` aesthetic is officially deprecated. It never really
  worked, and was poorly documented.

- The `stat` and `position` arguments to
  [`qplot()`](https://ggplot2.tidyverse.org/dev/reference/qplot.md) have
  been deprecated.
  [`qplot()`](https://ggplot2.tidyverse.org/dev/reference/qplot.md) is
  designed for quick plots - if you need to specify position or stat,
  use
  [`ggplot()`](https://ggplot2.tidyverse.org/dev/reference/ggplot.md)
  instead.

- The theme setting `axis.ticks.margin` has been deprecated: now use the
  margin property of `axis.text`.

- `stat_abline()`, `stat_hline()` and `stat_vline()` have been removed:
  these were never suitable for use other than with
  [`geom_abline()`](https://ggplot2.tidyverse.org/dev/reference/geom_abline.md)
  etc and were not documented.

- `show_guide` has been renamed to `show.legend`: this more accurately
  reflects what it does (controls appearance of layer in legend), and
  uses the same convention as other ggplot2 arguments (i.e. a `.`
  between names). (Yes, I know that’s inconsistent with function names
  with use `_`, but it’s too late to change now.)

A number of geoms have been renamed to be internally consistent:

- [`stat_binhex()`](https://ggplot2.tidyverse.org/dev/reference/geom_hex.md)
  and
  [`stat_bin2d()`](https://ggplot2.tidyverse.org/dev/reference/geom_bin_2d.md)
  have been renamed to
  [`stat_bin_hex()`](https://ggplot2.tidyverse.org/dev/reference/geom_hex.md)
  and
  [`stat_bin_2d()`](https://ggplot2.tidyverse.org/dev/reference/geom_bin_2d.md)
  ([\#1274](https://github.com/tidyverse/ggplot2/issues/1274)).
  [`stat_summary2d()`](https://ggplot2.tidyverse.org/dev/reference/stat_summary_2d.md)
  has been renamed to
  [`stat_summary_2d()`](https://ggplot2.tidyverse.org/dev/reference/stat_summary_2d.md),
  [`geom_density2d()`](https://ggplot2.tidyverse.org/dev/reference/geom_density_2d.md)/[`stat_density2d()`](https://ggplot2.tidyverse.org/dev/reference/geom_density_2d.md)
  has been renamed to
  [`geom_density_2d()`](https://ggplot2.tidyverse.org/dev/reference/geom_density_2d.md)/[`stat_density_2d()`](https://ggplot2.tidyverse.org/dev/reference/geom_density_2d.md).

- [`stat_spoke()`](https://ggplot2.tidyverse.org/dev/reference/geom_spoke.md)
  is now
  [`geom_spoke()`](https://ggplot2.tidyverse.org/dev/reference/geom_spoke.md)
  since I realised it’s a reparameterisation of
  [`geom_segment()`](https://ggplot2.tidyverse.org/dev/reference/geom_segment.md).

- `stat_bindot()` has been removed because it’s so tightly coupled to
  [`geom_dotplot()`](https://ggplot2.tidyverse.org/dev/reference/geom_dotplot.md).
  If you happened to use `stat_bindot()`, just change to
  [`geom_dotplot()`](https://ggplot2.tidyverse.org/dev/reference/geom_dotplot.md)
  ([\#1194](https://github.com/tidyverse/ggplot2/issues/1194)).

All defunct functions have been removed.

#### Default appearance

- The default
  [`theme_grey()`](https://ggplot2.tidyverse.org/dev/reference/ggtheme.md)
  background colour has been changed from “grey90” to “grey92”: this
  makes the background a little less visually prominent.

- Labels and titles have been tweaked for readability:

  - Axes labels are darker.

  - Legend and axis titles are given the same visual treatment.

  - The default font size dropped from 12 to 11. You might be surprised
    that I’ve made the default text size smaller as it was already hard
    for many people to read. It turns out there was a bug in RStudio
    (fixed in 0.99.724), that shrunk the text of all grid based
    graphics. Once that was resolved the defaults seemed too big to my
    eyes.

  - More spacing between titles and borders.

  - Default margins scale with the theme font size, so the appearance at
    larger font sizes should be considerably improved
    ([\#1228](https://github.com/tidyverse/ggplot2/issues/1228)).

- `alpha` now affects both fill and colour aesthetics
  ([\#1371](https://github.com/tidyverse/ggplot2/issues/1371)).

- [`element_text()`](https://ggplot2.tidyverse.org/dev/reference/element.md)
  gains a margins argument which allows you to add additional padding
  around text elements. To help see what’s going on use `debug = TRUE`
  to display the text region and anchors.

- The default font size in
  [`geom_text()`](https://ggplot2.tidyverse.org/dev/reference/geom_text.md)
  has been decreased from 5mm (14 pts) to 3.8 mm (11 pts) to match the
  new default theme sizes.

- A diagonal line is no longer drawn on bar and rectangle legends.
  Instead, the border has been tweaked to be more visible, and more
  closely match the size of line drawn on the plot.

- [`geom_pointrange()`](https://ggplot2.tidyverse.org/dev/reference/geom_linerange.md)
  and
  [`geom_linerange()`](https://ggplot2.tidyverse.org/dev/reference/geom_linerange.md)
  get vertical (not horizontal) lines in the legend
  ([\#1389](https://github.com/tidyverse/ggplot2/issues/1389)).

- The default line `size` for
  [`geom_smooth()`](https://ggplot2.tidyverse.org/dev/reference/geom_smooth.md)
  has been increased from 0.5 to 1 to make it easier to see when
  overlaid on data.

- [`geom_bar()`](https://ggplot2.tidyverse.org/dev/reference/geom_bar.md)
  and
  [`geom_rect()`](https://ggplot2.tidyverse.org/dev/reference/geom_tile.md)
  use a slightly paler shade of grey so they aren’t so visually heavy.

- [`geom_boxplot()`](https://ggplot2.tidyverse.org/dev/reference/geom_boxplot.md)
  now colours outliers the same way as the boxes.

- [`geom_point()`](https://ggplot2.tidyverse.org/dev/reference/geom_point.md)
  now uses shape 19 instead of 16. This looks much better on the default
  Linux graphics device. (It’s very slightly smaller than the old point,
  but it shouldn’t affect any graphics significantly)

- Sizes in ggplot2 are measured in mm. Previously they were converted to
  pts (for use in grid) by multiplying by 72 / 25.4. However, grid uses
  printer’s points, not Adobe (big pts), so sizes are now correctly
  multiplied by 72.27 / 25.4. This is unlikely to noticeably affect
  display, but it’s technically correct
  (<https://youtu.be/hou0lU8WMgo>).

- The default legend will now allocate multiple rows (if vertical) or
  columns (if horizontal) in order to make a legend that is more likely
  to fit on the screen. You can override with the `nrow`/`ncol`
  arguments to
  [`guide_legend()`](https://ggplot2.tidyverse.org/dev/reference/guide_legend.md)

  ``` r
  p <- ggplot(mpg, aes(displ,hwy, colour = model)) + geom_point()
  p
  p + theme(legend.position = "bottom")
  # Previous behaviour
  p + guides(colour = guide_legend(ncol = 1))
  ```

#### New and updated themes

- New
  [`theme_void()`](https://ggplot2.tidyverse.org/dev/reference/ggtheme.md)
  is completely empty. It’s useful for plots with non- standard
  coordinates or for drawings ([@jiho](https://github.com/jiho),
  [\#976](https://github.com/tidyverse/ggplot2/issues/976)).

- New
  [`theme_dark()`](https://ggplot2.tidyverse.org/dev/reference/ggtheme.md)
  has a dark background designed to make colours pop out
  ([@jiho](https://github.com/jiho),
  [\#1018](https://github.com/tidyverse/ggplot2/issues/1018))

- [`theme_minimal()`](https://ggplot2.tidyverse.org/dev/reference/ggtheme.md)
  became slightly more minimal by removing the axis ticks: labels now
  line up directly beneath grid lines
  ([@tomschloss](https://github.com/tomschloss),
  [\#1084](https://github.com/tidyverse/ggplot2/issues/1084))

- New theme setting `panel.ontop` (logical) make it possible to place
  background elements (i.e., gridlines) on top of data. Best used with
  transparent `panel.background`
  ([@noamross](https://github.com/noamross).
  [\#551](https://github.com/tidyverse/ggplot2/issues/551)).

#### Labelling

The facet labelling system was updated with many new features and a more
flexible interface ([@lionel-](https://github.com/lionel-)). It now
works consistently across grid and wrap facets. The most important user
visible changes are:

- [`facet_wrap()`](https://ggplot2.tidyverse.org/dev/reference/facet_wrap.md)
  gains a `labeller` option
  ([\#25](https://github.com/tidyverse/ggplot2/issues/25)).

- [`facet_grid()`](https://ggplot2.tidyverse.org/dev/reference/facet_grid.md)
  and
  [`facet_wrap()`](https://ggplot2.tidyverse.org/dev/reference/facet_wrap.md)
  gain a `switch` argument to display the facet titles near the axes.
  When switched, the labels become axes subtitles. `switch` can be set
  to “x”, “y” or “both” (the latter only for grids) to control which
  margin is switched.

The labellers (such as
[`label_value()`](https://ggplot2.tidyverse.org/dev/reference/labellers.md)
or
[`label_both()`](https://ggplot2.tidyverse.org/dev/reference/labellers.md))
also get some new features:

- They now offer the `multi_line` argument to control whether to display
  composite facets (those specified as `~var1 + var2`) on one or
  multiple lines.

- In
  [`label_bquote()`](https://ggplot2.tidyverse.org/dev/reference/label_bquote.md)
  you now refer directly to the names of variables. With this change,
  you can create math expressions that depend on more than one variable.
  This math expression can be specified either for the rows or the
  columns and you can also provide different expressions to each margin.

  As a consequence of these changes, referring to `x` in backquoted
  expressions is deprecated.

- Similarly to
  [`label_bquote()`](https://ggplot2.tidyverse.org/dev/reference/label_bquote.md),
  [`labeller()`](https://ggplot2.tidyverse.org/dev/reference/labeller.md)
  now take `.rows` and `.cols` arguments. In addition, it also takes
  `.default`.
  [`labeller()`](https://ggplot2.tidyverse.org/dev/reference/labeller.md)
  is useful to customise how particular variables are labelled. The
  three additional arguments specify how to label the variables are not
  specifically mentioned, respectively for rows, columns or both. This
  makes it especially easy to set up a project-wide labeller dispatcher
  that can be reused across all your plots. See the documentation for an
  example.

- The new labeller
  [`label_context()`](https://ggplot2.tidyverse.org/dev/reference/labellers.md)
  adapts to the number of factors facetted over. With a single factor,
  it displays only the values, just as before. But with multiple factors
  in a composite margin (e.g. with `~cyl + am`), the labels are passed
  over to
  [`label_both()`](https://ggplot2.tidyverse.org/dev/reference/labellers.md).
  This way the variables names are displayed with the values to help
  identifying them.

On the programming side, the labeller API has been rewritten in order to
offer more control when faceting over multiple factors (e.g. with
formulae such as `~cyl + am`). This also means that if you have written
custom labellers, you will need to update them for this version of
ggplot.

- Previously, a labeller function would take `variable` and `value`
  arguments and return a character vector. Now, they take a data frame
  of character vectors and return a list. The input data frame has one
  column per factor facetted over and each column in the returned list
  becomes one line in the strip label. See documentation for more
  details.

- The labels received by a labeller now contain metadata: their margin
  (in the “type” attribute) and whether they come from a wrap or a grid
  facet (in the “facet” attribute).

- Note that the new
  [`as_labeller()`](https://ggplot2.tidyverse.org/dev/reference/as_labeller.md)
  function operator provides an easy way to transform an existing
  function to a labeller function. The existing function just needs to
  take and return a character vector.

### Documentation

- Improved documentation for
  [`aes()`](https://ggplot2.tidyverse.org/dev/reference/aes.md),
  [`layer()`](https://ggplot2.tidyverse.org/dev/reference/layer.md) and
  much much more.

- I’ve tried to reduce the use of `...` so that you can see all the
  documentation in one place rather than having to integrate multiple
  pages. In some cases this has involved adding additional arguments to
  geoms to make it more clear what you can do:

  - [`geom_smooth()`](https://ggplot2.tidyverse.org/dev/reference/geom_smooth.md)
    gains explicit `method`, `se` and `formula` arguments.

  - [`geom_histogram()`](https://ggplot2.tidyverse.org/dev/reference/geom_histogram.md)
    gains `binwidth`, `bins`, `origin` and `right` arguments.

  - [`geom_jitter()`](https://ggplot2.tidyverse.org/dev/reference/geom_jitter.md)
    gains `width` and `height` arguments to make it easier to control
    the amount of jittering without using the lengthy
    [`position_jitter()`](https://ggplot2.tidyverse.org/dev/reference/position_jitter.md)
    function
    ([\#1116](https://github.com/tidyverse/ggplot2/issues/1116))

- Use of
  [`qplot()`](https://ggplot2.tidyverse.org/dev/reference/qplot.md) in
  examples has been minimised
  ([\#1123](https://github.com/tidyverse/ggplot2/issues/1123),
  [@hrbrmstr](https://github.com/hrbrmstr)). This is inline with the 2nd
  edition of the ggplot2 box, which minimises the use of
  [`qplot()`](https://ggplot2.tidyverse.org/dev/reference/qplot.md) in
  favour of
  [`ggplot()`](https://ggplot2.tidyverse.org/dev/reference/ggplot.md).

- Tightly linked geoms and stats
  (e.g. [`geom_boxplot()`](https://ggplot2.tidyverse.org/dev/reference/geom_boxplot.md)
  and
  [`stat_boxplot()`](https://ggplot2.tidyverse.org/dev/reference/geom_boxplot.md))
  are now documented in the same file so you can see all the arguments
  in one place. Variations of the same idea
  (e.g. [`geom_path()`](https://ggplot2.tidyverse.org/dev/reference/geom_path.md),
  [`geom_line()`](https://ggplot2.tidyverse.org/dev/reference/geom_path.md),
  and
  [`geom_step()`](https://ggplot2.tidyverse.org/dev/reference/geom_path.md))
  are also documented together.

- It’s now obvious that you can set the `binwidth` parameter for
  [`stat_bin_hex()`](https://ggplot2.tidyverse.org/dev/reference/geom_hex.md),
  [`stat_summary_hex()`](https://ggplot2.tidyverse.org/dev/reference/stat_summary_2d.md),
  [`stat_bin_2d()`](https://ggplot2.tidyverse.org/dev/reference/geom_bin_2d.md),
  and
  [`stat_summary_2d()`](https://ggplot2.tidyverse.org/dev/reference/stat_summary_2d.md).

- The internals of positions have been cleaned up considerably. You’re
  unlikely to notice any external changes, although the documentation
  should be a little less confusing since positions now don’t list
  parameters they never use.

### Data

- All datasets have class `tbl_df` so if you also use dplyr, you get a
  better print method.

- `economics` has been brought up to date to 2015-04-01.

- New `economics_long` is the economics data in long form.

- New `txhousing` dataset containing information about the Texas housing
  market. Useful for examples that need multiple time series, and for
  demonstrating model+vis methods.

- New `luv_colours` dataset which contains the locations of all built-in
  [`colors()`](https://rdrr.io/r/grDevices/colors.html) in Luv space.

- `movies` has been moved into its own package, ggplot2movies, because
  it was large and not terribly useful. If you’ve used the movies
  dataset, you’ll now need to explicitly load the package with
  [`library(ggplot2movies)`](https://rdrr.io/r/base/library.html).

### Bug fixes and minor improvements

- All partially matched arguments and `$` have been been replaced with
  full matches ([@jimhester](https://github.com/jimhester),
  [\#1134](https://github.com/tidyverse/ggplot2/issues/1134)).

- ggplot2 now exports
  [`alpha()`](https://scales.r-lib.org/reference/alpha.html) from the
  scales package
  ([\#1107](https://github.com/tidyverse/ggplot2/issues/1107)), and
  [`arrow()`](https://rdrr.io/r/grid/arrow.html) and
  [`unit()`](https://rdrr.io/r/grid/unit.html) from grid
  ([\#1225](https://github.com/tidyverse/ggplot2/issues/1225)). This
  means you don’t need attach scales/grid or do `scales::`/`grid::` for
  these commonly used functions.

- [`aes_string()`](https://ggplot2.tidyverse.org/dev/reference/aes_.md)
  now only parses character inputs. This fixes bugs when using it with
  numbers and non default `OutDec` settings
  ([\#1045](https://github.com/tidyverse/ggplot2/issues/1045)).

- [`annotation_custom()`](https://ggplot2.tidyverse.org/dev/reference/annotation_custom.md)
  automatically adds a unique id to each grob name, making it easier to
  plot multiple grobs with the same name (e.g. grobs of ggplot2
  graphics) in the same plot
  ([\#1256](https://github.com/tidyverse/ggplot2/issues/1256)).

- [`borders()`](https://ggplot2.tidyverse.org/dev/reference/annotation_borders.md)
  now accepts xlim and ylim arguments for specifying the geographical
  region of interest
  ([@markpayneatwork](https://github.com/markpayneatwork),
  [\#1392](https://github.com/tidyverse/ggplot2/issues/1392)).

- [`coord_cartesian()`](https://ggplot2.tidyverse.org/dev/reference/coord_cartesian.md)
  applies the same expansion factor to limits as for scales. You can
  suppress with `expand = FALSE`
  ([\#1207](https://github.com/tidyverse/ggplot2/issues/1207)).

- [`coord_trans()`](https://ggplot2.tidyverse.org/dev/reference/coord_transform.md)
  now works when breaks are suppressed
  ([\#1422](https://github.com/tidyverse/ggplot2/issues/1422)).

- [`cut_number()`](https://ggplot2.tidyverse.org/dev/reference/cut_interval.md)
  gives error message if the number of requested bins can be created
  because there are two few unique values
  ([\#1046](https://github.com/tidyverse/ggplot2/issues/1046)).

- Character labels in
  [`facet_grid()`](https://ggplot2.tidyverse.org/dev/reference/facet_grid.md)
  are no longer (incorrectly) coerced into factors. This caused problems
  with custom label functions
  ([\#1070](https://github.com/tidyverse/ggplot2/issues/1070)).

- [`facet_wrap()`](https://ggplot2.tidyverse.org/dev/reference/facet_wrap.md)
  and
  [`facet_grid()`](https://ggplot2.tidyverse.org/dev/reference/facet_grid.md)
  now allow you to use non-standard variable names by surrounding them
  with backticks
  ([\#1067](https://github.com/tidyverse/ggplot2/issues/1067)).

- [`facet_wrap()`](https://ggplot2.tidyverse.org/dev/reference/facet_wrap.md)
  more carefully checks its `nrow` and `ncol` arguments to ensure that
  they’re specified correctly
  ([@richierocks](https://github.com/richierocks),
  [\#962](https://github.com/tidyverse/ggplot2/issues/962))

- [`facet_wrap()`](https://ggplot2.tidyverse.org/dev/reference/facet_wrap.md)
  gains a `dir` argument to control the direction the panels are wrapped
  in. The default is “h” for horizontal. Use “v” for vertical layout
  ([\#1260](https://github.com/tidyverse/ggplot2/issues/1260)).

- [`geom_abline()`](https://ggplot2.tidyverse.org/dev/reference/geom_abline.md),
  [`geom_hline()`](https://ggplot2.tidyverse.org/dev/reference/geom_abline.md)
  and
  [`geom_vline()`](https://ggplot2.tidyverse.org/dev/reference/geom_abline.md)
  have been rewritten to have simpler behaviour and be more consistent:

  - `stat_abline()`, `stat_hline()` and `stat_vline()` have been
    removed: these were never suitable for use other than with
    [`geom_abline()`](https://ggplot2.tidyverse.org/dev/reference/geom_abline.md)
    etc and were not documented.

  - [`geom_abline()`](https://ggplot2.tidyverse.org/dev/reference/geom_abline.md),
    [`geom_vline()`](https://ggplot2.tidyverse.org/dev/reference/geom_abline.md)
    and
    [`geom_hline()`](https://ggplot2.tidyverse.org/dev/reference/geom_abline.md)
    are bound to
    [`stat_identity()`](https://ggplot2.tidyverse.org/dev/reference/stat_identity.md)
    and
    [`position_identity()`](https://ggplot2.tidyverse.org/dev/reference/position_identity.md)

  - Intercept parameters can no longer be set to a function.

  - They are all documented in one file, since they are so closely
    related.

- [`geom_bin2d()`](https://ggplot2.tidyverse.org/dev/reference/geom_bin_2d.md)
  will now let you specify one dimension’s breaks exactly, without
  touching the other dimension’s default breaks at all
  ([\#1126](https://github.com/tidyverse/ggplot2/issues/1126)).

- [`geom_crossbar()`](https://ggplot2.tidyverse.org/dev/reference/geom_linerange.md)
  sets grouping correctly so you can display multiple crossbars on one
  plot. It also makes the default `fatten` argument a little bigger to
  make the middle line more obvious
  ([\#1125](https://github.com/tidyverse/ggplot2/issues/1125)).

- [`geom_histogram()`](https://ggplot2.tidyverse.org/dev/reference/geom_histogram.md)
  and
  [`geom_smooth()`](https://ggplot2.tidyverse.org/dev/reference/geom_smooth.md)
  now only inform you about the default values once per layer, rather
  than once per panel
  ([\#1220](https://github.com/tidyverse/ggplot2/issues/1220)).

- [`geom_pointrange()`](https://ggplot2.tidyverse.org/dev/reference/geom_linerange.md)
  gains `fatten` argument so you can control the size of the point
  relative to the size of the line.

- [`geom_segment()`](https://ggplot2.tidyverse.org/dev/reference/geom_segment.md)
  annotations were not transforming with scales
  ([@BrianDiggs](https://github.com/BrianDiggs),
  [\#859](https://github.com/tidyverse/ggplot2/issues/859)).

- [`geom_smooth()`](https://ggplot2.tidyverse.org/dev/reference/geom_smooth.md)
  is no longer so chatty. If you want to know what the default smoothing
  method is, look it up in the documentation!
  ([\#1247](https://github.com/tidyverse/ggplot2/issues/1247))

- [`geom_violin()`](https://ggplot2.tidyverse.org/dev/reference/geom_violin.md)
  now has the ability to draw quantile lines
  ([@DanRuderman](https://github.com/DanRuderman)).

- [`ggplot()`](https://ggplot2.tidyverse.org/dev/reference/ggplot.md)
  now captures the parent frame to use for evaluation, rather than
  always defaulting to the global environment. This should make ggplot
  more suitable to use in more situations (e.g. with knitr)

- [`ggsave()`](https://ggplot2.tidyverse.org/dev/reference/ggsave.md)
  has been simplified a little to make it easier to maintain. It no
  longer checks that you’re printing a ggplot2 object (so now also works
  with any grid grob)
  ([\#970](https://github.com/tidyverse/ggplot2/issues/970)), and always
  requires a filename. Parameter `device` now supports character
  argument to specify which supported device to use (‘pdf’, ‘png’,
  ‘jpeg’, etc.), for when it cannot be correctly inferred from the file
  extension (for example when a temporary filename is supplied server
  side in shiny apps) ([@sebkopf](https://github.com/sebkopf),
  [\#939](https://github.com/tidyverse/ggplot2/issues/939)). It no
  longer opens a graphics device if one isn’t already open - this is
  annoying when you’re running from a script
  ([\#1326](https://github.com/tidyverse/ggplot2/issues/1326)).

- [`guide_colorbar()`](https://ggplot2.tidyverse.org/dev/reference/guide_colourbar.md)
  creates correct legend if only one color
  ([@krlmlr](https://github.com/krlmlr),
  [\#943](https://github.com/tidyverse/ggplot2/issues/943)).

- [`guide_colorbar()`](https://ggplot2.tidyverse.org/dev/reference/guide_colourbar.md)
  no longer fails when the legend is empty - previously this often
  masked misspecifications elsewhere in the plot
  ([\#967](https://github.com/tidyverse/ggplot2/issues/967)).

- New
  [`layer_data()`](https://ggplot2.tidyverse.org/dev/reference/ggplot_build.md)
  function extracts the data used for plotting for a given layer. It’s
  mostly useful for testing.

- User supplied `minor_breaks` can now be supplied on the same scale as
  the data, and will be automatically transformed with by scale
  ([\#1385](https://github.com/tidyverse/ggplot2/issues/1385)).

- You can now suppress the appearance of an axis/legend title (and the
  space that would allocated for it) with `NULL` in the `scale_`
  function. To use the default label, use
  [`waiver()`](https://ggplot2.tidyverse.org/dev/reference/waiver.md)
  ([\#1145](https://github.com/tidyverse/ggplot2/issues/1145)).

- Position adjustments no longer warn about potentially varying ranges
  because the problem rarely occurs in practice and there are currently
  a lot of false positives since I don’t understand exactly what FP
  criteria I should be testing.

- [`scale_fill_grey()`](https://ggplot2.tidyverse.org/dev/reference/scale_grey.md)
  now uses red for missing values. This matches
  [`scale_colour_grey()`](https://ggplot2.tidyverse.org/dev/reference/scale_grey.md)
  and makes it obvious where missing values lie. Override with
  `na.value`.

- `scale_*_gradient2()` defaults to using Lab colour space.

- `scale_*_gradientn()` now allows `colours` or `colors`
  ([\#1290](https://github.com/tidyverse/ggplot2/issues/1290))

- [`scale_y_continuous()`](https://ggplot2.tidyverse.org/dev/reference/scale_continuous.md)
  now also transforms the `lower`, `middle` and `upper` aesthetics used
  by
  [`geom_boxplot()`](https://ggplot2.tidyverse.org/dev/reference/geom_boxplot.md):
  this only affects `geom_boxplot(stat = "identity")`
  ([\#1020](https://github.com/tidyverse/ggplot2/issues/1020)).

- Legends no longer inherit aesthetics if `inherit.aes` is FALSE
  ([\#1267](https://github.com/tidyverse/ggplot2/issues/1267)).

- [`lims()`](https://ggplot2.tidyverse.org/dev/reference/lims.md) makes
  it easy to set the limits of any axis
  ([\#1138](https://github.com/tidyverse/ggplot2/issues/1138)).

- `labels = NULL` now works with
  [`guide_legend()`](https://ggplot2.tidyverse.org/dev/reference/guide_legend.md)
  and
  [`guide_colorbar()`](https://ggplot2.tidyverse.org/dev/reference/guide_colourbar.md).
  ([\#1175](https://github.com/tidyverse/ggplot2/issues/1175),
  [\#1183](https://github.com/tidyverse/ggplot2/issues/1183)).

- `override.aes` now works with American aesthetic spelling, e.g. color

- Scales no longer round data points to improve performance of colour
  palettes. Instead the scales package now uses a much faster colour
  interpolation algorithm
  ([\#1022](https://github.com/tidyverse/ggplot2/issues/1022)).

- `scale_*_brewer()` and `scale_*_distiller()` add new `direction`
  argument of
  [`scales::brewer_pal`](https://scales.r-lib.org/reference/pal_brewer.html),
  making it easier to change the order of colours
  ([@jiho](https://github.com/jiho),
  [\#1139](https://github.com/tidyverse/ggplot2/issues/1139)).

- [`scale_x_date()`](https://ggplot2.tidyverse.org/dev/reference/scale_date.md)
  now clips dates outside the limits in the same way as
  [`scale_x_continuous()`](https://ggplot2.tidyverse.org/dev/reference/scale_continuous.md)
  ([\#1090](https://github.com/tidyverse/ggplot2/issues/1090)).

- [`stat_bin()`](https://ggplot2.tidyverse.org/dev/reference/geom_histogram.md)
  gains `bins` arguments, which denotes the number of bins. Now you can
  set `bins=100` instead of `binwidth=0.5`. Note that `breaks` or
  `binwidth` will override it ([@tmshn](https://github.com/tmshn),
  [\#1158](https://github.com/tidyverse/ggplot2/issues/1158),
  [\#102](https://github.com/tidyverse/ggplot2/issues/102)).

- [`stat_boxplot()`](https://ggplot2.tidyverse.org/dev/reference/geom_boxplot.md)
  warns if a continuous variable is used for the `x` aesthetic without
  also supplying a `group` aesthetic
  ([\#992](https://github.com/tidyverse/ggplot2/issues/992),
  [@krlmlr](https://github.com/krlmlr)).

- [`stat_summary_2d()`](https://ggplot2.tidyverse.org/dev/reference/stat_summary_2d.md)
  and
  [`stat_bin_2d()`](https://ggplot2.tidyverse.org/dev/reference/geom_bin_2d.md)
  now share exactly the same code for determining breaks from `bins`,
  `binwidth`, and `origin`.

- [`stat_summary_2d()`](https://ggplot2.tidyverse.org/dev/reference/stat_summary_2d.md)
  and
  [`stat_bin_2d()`](https://ggplot2.tidyverse.org/dev/reference/geom_bin_2d.md)
  now output in tile/raster compatible form instead of rect compatible
  form.

- Automatically computed breaks do not lead to an error for
  transformations like “probit” where the inverse can map to infinity
  ([\#871](https://github.com/tidyverse/ggplot2/issues/871),
  [@krlmlr](https://github.com/krlmlr))

- [`stat_function()`](https://ggplot2.tidyverse.org/dev/reference/geom_function.md)
  now always evaluates the function on the original scale. Previously it
  computed the function on transformed scales, giving incorrect values
  ([@BrianDiggs](https://github.com/BrianDiggs),
  [\#1011](https://github.com/tidyverse/ggplot2/issues/1011)).

- `strip_dots` works with anonymous functions within calculated
  aesthetics (e.g. `aes(sapply(..density.., function(x) mean(x))))`
  ([\#1154](https://github.com/tidyverse/ggplot2/issues/1154),
  [@NikNakk](https://github.com/NikNakk))

- [`theme()`](https://ggplot2.tidyverse.org/dev/reference/theme.md)
  gains `validate = FALSE` parameter to turn off validation, and hence
  store arbitrary additional data in the themes.
  ([@tdhock](https://github.com/tdhock),
  [\#1121](https://github.com/tidyverse/ggplot2/issues/1121))

- Improved the calculation of segments needed to draw the curve
  representing a line when plotted in polar coordinates. In some cases,
  the last segment of a multi-segment line was not drawn
  ([@BrianDiggs](https://github.com/BrianDiggs),
  [\#952](https://github.com/tidyverse/ggplot2/issues/952))
