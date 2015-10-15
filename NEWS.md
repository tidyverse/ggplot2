# ggplot2 1.1.0

## Major changes

* ggplot no longer throws an error if you your plot doesn't have any
  layers. Instead it automatically adds `geom_blank()` (#1246)
  
* New `cut_width()` is a convenient replacement for the verbose
  `plyr::round_any()`, with the additional benefit of offering more
  control.

* New `geom_count()` is a convenient alias to `stat_sum()`. Use it when you
  have overlapping points on a scatterplot. `stat_sum()` now defaults to 
  using counts instead of proportions.

* New `geom_curve()` adds curved lines, similar to `geom_segment()`
  (@veraanadi, #1088).

* Date and datetime scales now have `date_breaks`, `date_minor_breaks` and
  `date_labels` arguments so that you never need to use the long form
  `scales::date_breaks()` or `scales::date_format()`.

* `geom_bar()` and `geom_histogram()` no longer share `stat_bin()`. Now
  `geom_bar()` has its own stat, `stat_count()`. This is useful if you want to
  display a bar chart of continuous data, drawing a bar at each unique
  location.

* `geom_point()` gains a `stroke` aesthetic which controls the border width of 
  shape 21 - 25 (#1133, @SeySayux). Like `size`, the unit of stroke is `mm`. 
  A point with `size = 5, stroke = 5` will be 10 mm wide. (#1142)

* New `position_nudge()` allows you to slightly off setting labels (or other 
  geoms) from their corresponding points (#1109).

* `scale_size()` now maps values to _area_, not radius. Use `scale_radius()`
  if you want the old behaviour (not recommended, except perhaps for lines).

* New `stat_summary_bin()` that works like `stat_summary()` but on binned
  data. It's a generalisation of `stat_bin()` that can compute any aggregate.
  (#1274)

* Layers are now much stricter about their arguments - you will get an error
  if you've supplied an argument that isn't an aesthetic or a parameter.
  This is likely to cause some short-term pain but in the long-term will make
  it much easier to spot spelling mistakes and other errors (#1293).
  
    This unfortunately breaks a handful of geoms/stats that used `...` to pass 
    additional arguments on to the underlying computation: 
    `geom_smooth()`/`stat_smooth()` and `geom_quantile()`/`stat_quantile()` 
    now use  `method.args` (#1245, #1289); `stat_summary()` (#1242), 
    `stat_summary_hex()`, and `stat_summary2d()` use `fun.args`.

### Extensibility

There is now an official mechanism for defining Stats, Geoms, and Positions in other packages. See `vignette("extending-ggplot2")` for details.

* `Geom`, `Stat` and `Position` are now exported, making it easier to create
  new geoms, stats and positions in other packages (#989).

* Switched away from proto and reference class objects to ggproto. This makes it
  easier to extend ggplot2, because ggproto supports clean cross-package
  inheritance.

* `aes_auto()` has been deprecated. `aes_()` added as alias to old `aes_q()`.
  `aes_()` can now use formulas, so the most concise SE version of
  `aes(carat, price)` is now `aes_(~carat, ~price)`.

### Text

* `geom_text()` gains `nudge_x` and `nudge_y` arguments to offset labels from
  the x & y position (#1120). `check_overlap = TRUE` provides a simple
  resolution to label overplotting (#1039). `hjust` and `vjust` can now
  be character vectors: "left", "center", "right", "bottom", "middle", "top".
  New options include "inward" and "outward" which align text towards and
  away from the center of the plot respectively.

* `geom_label()` works like `geom_text()` but draws a solid background
  behind each label (#1039).

### Deprecated features

* `order` aesthetic is officially deprecated. It never really worked, and was 
  poorly documented.

* The `stat` and `position` arguments to `qplot()` have been deprecated.

* Theme setting `axis.ticks.margin` has been deprecated: now use the margin 
  property of `axis.ticks`.
  
* `show_guide` has been renamed to `show.legend`: this more accurately
  reflects what it does (control appearance in legend), and uses the same
  convention as other ggplot2 arguments (i.e. a `.` between names).

A number of geoms have been renamed to be more consistent (the existing
functions will continue to work but will be deprecated in the future):

* `stat_binhex()` and `stat_bin2d()` have been renamed to `stat_bin_hex()` 
  and `stat_bin_2d()` (#1274). `stat_summary2d()` has been renamed to `stat_summary_2d()`

* `stat_spoke()` is now `geom_spoke()` since I realised it's a
  reparameterisation of `geom_segment().

* `stat_bindot()` has been removed because it's so tightly coupled to
  `geom_dotplot()`. If you happened to use `stat_bindot()`, just change to
  `geom_dotplot()` (#1194).

## Visual appearance

### Changes to defaults

* The default `theme_grey()` background colour has been tweaked from grey90 to 
  grey92: this makes the background recede into the background a little more.

* Default text appearance has been tweaked for readability:

    * Axes labels are darker
    
    * Legend and axis titles are given the same treatment
    
    * The default font size dropped from 12 to 11. 
    
    * More spacing between titles and borders
    
    * Default margins scale with the theme font size, so the appearance at 
      larger font sizes should be considerably improved (#1228). 

* `element_text()` gains a margins argument which allows you to add additional
  margins around text elements in the plot. To help see what's going on use 
  `debug = TRUE` to see the text region and anchors.

* The default font size in `geom_text()` has been decreased from 5mm (14 pts)
  to 3.8 mm (11 pts) to match the new default theme sizes.

* A diagonal line is no longer drawn on bar and rectangle legends. Instead, the
  border has been tweaked to be more visible, and more closely match the size of 
  line drawn on the plot.

* The default line `size` for `geom_smooth()` has been bumped up to 1 to make
  it easier to see when overlaid on data. To continue to use the previous size, 
  set `size = 0.5`.
  
* `geom_bar()` and `geom_rect()` use a slightly paler shade of grey so they
  don't look so visually heavy.

* `geom_point()` now uses shape 19 instead of 16 - this looks much better on 
  the default linux graphics device. (It's very slightly smaller than the old 
  point, but it shouldn't affect any graphics significantly)

* Sizes in ggplot2 are measured in mm. Previously they were converted to pts 
  (for use in grid) by multiplying by 72 / 25.4. However, grid uses printer's 
  points, not Adobe (big pts), so sizes are now correctly multiplied by 
  72.27 / 25.4.

* The default legend will now allocate multiple rows (if vertical) or
  columns (if horizontal) in order to make a legend that is more likely to
  fit on the screen. You can override with the `nrow`/`ncol` arguments
  to `guide_legend()`

    ```R
    p <- ggplot(mpg, aes(displ,hwy, colour = model)) + geom_point()
    p
    p + theme(legend.position = "bottom")
    p + guides(colour = guide_legend(ncol = 1))
    ```

### New and updated themes

* New `theme_void()` is completely empty. It's useful for plots with non
  standard coordinates or for drawings (@jiho, #976).

* New `theme_dark()` has a dark background designed to make colours pop out
  (@jiho, #1018)

* `theme_minimal()` got slightly more minimal by removing the axis ticks:
  labels now line up beneath the grid lines (@tomschloss, #1084)

* New theme setting `panel.ontop` (logical) allows placing background elements
  (e.g., gridlines) on top of data. Usually used with blank or transparent
  `panel.background`.  (@noamross. #551)

### Labelling

* `facet_wrap()` gains a `labeller` option. (@lionel-, #25)

* The labeller API has been updated to offer more control when facetting over 
  multiple factors (e.g. with formulae such as `~cyl + am`). Previously, a 
  labeller function would take `variable` and `value` arguments and return a 
  character vector. 
  
  Now, they take a data frame of character vectors and return a list. The input 
  data frame has one column per factor facetted over. Each column in the 
  returned list becomes one line in the strip label. See documentation for more
  details. 
  
* Labellers offer the `multi_line` argument to control whether to display one 
  or multiple lines. 
  
* Referring to `x` in backquoted expressions with `label_bquote()` is 
  deprecated. You can now refer to the variable names instead. (@lionel-)

* `label_bquote()` and `labeller()` now take `rows` and `cols` arguments. They 
  allow rows and columns labels to have specific plotmath expressions or 
  labellers.

* New labeller `label_context()` which behaviour differently based on the 
  on the number of factors facetted over. With a single factor, it displays 
  only the values, just as before. But with multiple factors (e.g. with 
  `~cyl + am`), the labels are dispatched to `label_both()` to display both the 
  variables and the values and make the plot clearer. (@lionel-)

## Documentation

* Improved documentation for `aes()`, `layer()` and many geoms and scales. 

* I've tried to reduce the use of `...` so that you can see all the 
  documentation in one place rather than having to navigate through multiple 
  pages. In some cases this has involved adding additional arguments to the
  `geom_()` to make it more clear what you can do:
  
    *  `geom_smooth()` gains explicit `method`, `se` and `formula` arguments.
    
    * `geom_histogram()` gains `binwidth`, `bins`, origin` and `right` 
      arguments.
      
    * `geom_jitter()` gains `width` and `height` arguments to make it easier
      to control the amount of jittering without using the lengthy 
      `position_jitter()` function (#1116)

* Use of `qplot()` in examples has been minimised (#1123, @hrbrmstr). 

* Tighly linked geoms and stats (e.g. `geom_boxplot()` and `stat_boxplot()`) 
  are now documented in the same file so you can see all the arguments in one
  place.

* It's now obvious that you can set the `binwidth` parameter for
  `stat_bin_hex()`, `stat_summary_hex()`, `stat_bin_2d()`, and
  `stat_summary_2d()`. `stat_summary_2d()` and `stat_bin_2d()` now share
  exactly the same code for determining breaks from `bins`, `binwidth`, and
  `origin`. `stat_summary_2d()` and `stat_bin_2d()` now output in tile/raster
  compatible form instead of rect form.

* The internals of positions have been cleaned up considerably. You're unlikely
  to notice any external changes, although the documentation should be a little
  less confusing since positions now don't list parameters they never use.

## Data

* All datasets have class `tbl_df` so if you also use dplyr, you get a better
  print method.

* `economics` has been brought up to date to 2015-04-01.

* New `economics_long` is the economics data in long form.

* New `txhousing` dataset containing information about the Texas housing
  market. Useful for examples that need multiple time series, and for
  demonstrating model+vis methods.

* New `luv_colours` dataset which contains the locations of all
  built-in `colors()` in Luv space.

* `movies` has been moved into its own package, ggplot2movies, because it was 
  large and not terribly useful. If you've used the movies dataset, you'll now 
  need to explicitly load the package with `library(ggplot2movies)`.

## Bug fixes and minor improvements

* All instances of partial matched dollar expressions have been removed 
  (@jimhester, #1134)

* All defunct functions and arguments have been removed.

* ggplot2 now exports `alpha()` from the scales package (#1107), and `arrow()` 
  and `unit()` from grid (#1225). This means you don't need attach scales/grid 
  or do `scales::`/`grid::` for these commonly used functions.

* `aes_string()` now only parses character inputs. This problems bugs when
  using it with numbers and non default `OutDec` settings (#1045).

* `annotation_custom()` automatically adds a unique id to each grob name,
  making it easier to plot multiple grobs with the same name (e.g. grobs of
  ggplot2 graphics) in the same plot (#1256).

* `coord_cartesian()` applies the same expansion factor to limits as scales. 
  You can suppress with `expand = FALSE` (#1207).

* `cut_number()` gives error message if insufficient data values to
  produce requested bins (#1046).

* Character labels in `facet_grid()` are no longer (incorrectly) coerced into
  factors. This caused problems with custom label functions (#1070).

* `facet_wrap()` and `facet_grid()` now allow you to use non-standard
  variable names (provided that they surrounded by backticks) (#1067)

* `facet_grid()` and `facet_wrap()` gain a `switch` argument that
  allows the facet titles to be displayed near the axes. They then act
  as axes subtitles. Can be set to "x", "y" or "both" (the latter only
  for grids) to control which label strips are switched. (@lionel-)

* `facet_wrap()` more carefully checks its `nrow` and `ncol` arguments
  to ensure that they're specified correctly (@richierocks, #962)

* `facet_wrap()` gains a `dir` argument to control the direction the
  panels are wrapped in. The default is "h" for horizontal. Use "v" for
  vertical layout (#1260).

* `geom_abline()`, `geom_hline()` and `geom_vline()` have been rewritten to
  have simpler behaviour and be more consistent:

    * `stat_abline()`, `stat_hline()` and `stat_vline()` have been removed:
      these were never suitable for use other than with `geom_abline()` etc
      and were not documented.

    * `geom_abline()`, `geom_vline()` and `geom_hline()` are bound to
      `stat_identity()` and `position_identity()`

    * Intercept parameters can no longer be set to a function.

    * They are all documented in one file, since they are so closely related.

* `geom_bin2d()` will now let you specify one dimension's breaks exactly,
  without touching the other dimension's default breaks at all (#1126).

* `geom_crossbar()` sets grouping correctly so you can display multiple
  crossbars on one plot. It also makes the default `fatten` argument a little
  bigger to make the middle line more obvious (#1125).

* `geom_histogram()` and `geom_smooth()` now only inform you about the
  default values once per layer, rather than once per panel (#1220).

* `geom_pointrange()` gains `fatten` argument so you can control the
  size of the point relative to the size of the line.

* `geom_segment()` annotations were not transforming with scales 
  (@BrianDiggs, #859).

* `geom_smooth()` is no longer so chatty. If you want to know what the deafult
  smoothing method is, look it up in the documentation! (#1247)

* `geom_violin()` now has the ability to draw quantile lines (@DanRuderman).

* `ggplot()` now captures the parent frame to use for evaluation,
  rather than always defaulting to the global environment. This should
  make ggplot more suitable to use in more situations (e.g. with knitr)

* `ggsave()` has been simplified a little to make it easier to maintain.
  It no longer checks that you're printing a ggplot2 object (so now also
  works with any grid grob) (#970), and always requires a filename.
  Parameter `device` now supports character argument to specify which supported
  device to use ('pdf', 'png', 'jpeg', etc.), for when it cannot be correctly
  inferred from the file extension (for example when a temporary filename is
  supplied server side in shiny apps) (@sebkopf, #939). It no longer opens
  a graphics device if one isn't already open - this is annoying when you're
  running from a script (#1326)

* `guide_colorbar()` creates correct legend if only one color (@krlmlr, #943)

* `guide_colorbar()` no longer fails when the legend is empty - previously
  this often masked misspecifications elsewhere in the plot (#967)

* New `layer_data()` function extracts the data used for plotting for a given
  layer. It's mostly useful for testing.

* You can now suppress the appearance of an axis/legend title (and the space
  that would allocated for it) with `NULL` in the `scale_` function. To
  use the default lable, use `waiver()` (#1145).

* Position adjustments no longer warn about potentially varying ranges
  because the problem rarely occurs in practice and there are currently a
  lot of false positives since I don't understand exactly what FP criteria
  I should be testing.

* `scale_fill_grey()` now uses red for missing values. This matches
  `scale_colour_grey()` and makes it obvious where missing values lie.
  Control with `na.value`.

* `scale_*_gradient2()` defaults to using Lab colour space.

* `scale_*_gradientn()` etc now allows `colours` or `colors` (#1290)

* `scale_y_continuous()` now also transforms the `lower`, `middle` and `upper`
  aesthetics used by `geom_boxplot()`: this only affects
  `geom_boxplot(stat = "identity")` (#1020).

* Legends no longer inherit aesthetics if `inherit.aes` is FALSE (#1267).

* `lims()` makes it easy to set the limits of any axis (#1138).

* `labels = NULL` now works with `guide_legend()` and `guide_colorbar()`.
  (#1175, #1183).

* `override.aes` now works with American aesthetic spelling, e.g. color

* Scales no longer round data points to improve performance of colour
  palettes. Instead the scales package now uses a much faster colour
  interpolation algorithm (#1022).

* `scale_*_brewer()` and `scale_*_distiller()` add new `direction` argument of 
  `scales::brewer_pal` and simplify changing the order of colours (@jiho, #1139)

* `scale_x_date()` now clips dates outside the limits in the same way as
  `scale_x_continuous()` (#1090)

* `stat_bin()` gains `bins` arguments, which denotes the number of bins. Now
  you can set `bins=100` instead of `binwidth=0.5`. Note that `breaks` or
  `binwidth` will override it. (@tmshn, #1158, #102)

* `stat_boxplot()` warns if a continuous variable is used for the `x` aesthetic
  without also supplying a `group` aesthetic (#992, @krlmlr)

* Automatically computed breaks do not lead to an error for transformations like
  "probit" where the inverse can map to infinity (#871, @krlmlr)

* `stat_function` computed the function with the transformed (not
  original data) values. This caused the computed values to be wrong
  if a different scale, other than untransformed continuous, was
  used. Function is now evaluted in original data scale
  values. (@BrianDiggs, #1011)

* `strip_dots` works with anonymous functions within calculated aesthetics 
  (e.g. `aes(sapply(..density.., function(x) mean(x))))` (#1154, @NikNakk)

* `theme()` gains `validate = FALSE` parameter to turn off validation, and 
  hence store arbitrary additional data in the themes. (@tdhock, #1121)

* Improved the calculation of segments needed to draw the curve representing
  a line when plotted in polar coordinates. In some cases, the last segment
  of a multi-segment line was not drawn (@BrianDiggs, #952)
