# ggplot2 2.1.0

## New features

* When mapping an aesthetic to a constant (e.g. 
  `geom_smooth(aes(colour = "loess")))`), the default guide title is the name 
  of the aesthetic (i.e. "colour"), not the value (i.e. "loess") (#1431).

* `layer()` now accepts a function as the data argument. The function will be
  applied to the data passed to the `ggplot()` function and must return a
  data.frame (#1527, @thomasp85). This is a more general version of the 
  deprecated `subset` argument.

* `theme_update()` now uses the `+` operator instead of `%+replace%`, so that
  unspecified values will no longer be `NULL`ed out. `theme_replace()`
  preserves the old behaviour if desired (@oneillkza, #1519). 

* `stat_bin()` has been overhauled to use the same algorithm as ggvis, which 
  has been considerably improved thanks to the advice of Randy Prium (@rpruim).
  This includes:
  
    * Better arguments and a better algorithm for determining the origin.
      You can now specify either `boundary` or the `center` of a bin.
      `origin` has been deprecated in favour of these arguments.
      
    * `drop` is deprecated in favour of `pad`, which adds extra 0-count bins
      at either end (needed for frequency polygons). `geom_histogram()` defaults 
      to `pad = FALSE` which considerably improves the default limits for 
      the histogram, especially when the bins are big (#1477).
      
    * The default algorithm does a (somewhat) better job at picking nice widths 
      and origins across a wider range of input data.
      
    * `bins = n` now gives a histogram with `n` bins, not `n + 1` (#1487).

## Bug fixes

* All `\donttest{}` examples run.

* All `geom_()` and `stat_()` functions now have consistent argument order:
  data + mapping, then geom/stat/position, then `...`, then specific arguments, 
  then arguments common to all layers (#1305). This may break code if you were
  previously relying on partial name matching, but in the long-term should make 
  ggplot2 easier to use. In particular, you can now set the `n` parameter
  in `geom_density2d()` without it partially matching `na.rm` (#1485).

* For geoms with both `colour` and `fill`, `alpha` once again only affects
  fill (Reverts #1371, #1523). This was causing problems for people.

* `facet_wrap()`/`facet_grid()` works with multiple empty panels of data 
  (#1445).

* `facet_wrap()` correctly swaps `nrow` and `ncol` when facetting vertically
  (#1417).

* `ggsave("x.svg")` now uses svglite to produce the svg (#1432).

* `geom_boxplot()` now understands `outlier.color` (#1455).

* `geom_path()` knows that "solid" (not just 1) represents a solid line (#1534).

* `geom_ribbon()` preserves missing values so they correctly generate a 
  gap in the ribbon (#1549).

* `geom_tile()` once again accepts `width` and `height` parameters (#1513). 
  It uses `draw_key_polygon()` for better a legend, including a coloured 
  outline (#1484).

* `layer()` now automatically adds a `na.rm` parameter if none is explicitly
  supplied.

* `position_jitterdodge()` now works on all possible dodge aesthetics, 
  e.g. `color`, `linetype` etc. instead of only based on `fill` (@bleutner)

* `position = "nudge"` now works (although it doesn't do anything useful)
  (#1428).

* The default scale for columns of class "AsIs" is now "identity" (#1518).

* `scale_*_discrete()` has better defaults when used with purely continuous
  data (#1542).

* `scale_size()` warns when used with categorical data.

* `scale_size()`, `scale_colour()`, and `scale_fill()` gain date and date-time
  variants (#1526).

* `stat_bin_hex()` and `stat_bin_summary()` now use the same underlying 
  algorithm so results are consistent (#1383). `stat_bin_hex()` now accepts
  a `weight` aesthetic. To be consistent with related stats, the output variable 
  from `stat_bin_hex()` is now value instead of count.

* `stat_density()` gains a `bw` parameter which makes it easy to get consistent 
   smoothing between facets (@jiho)

* `stat-density-2d()` no longer ignores the `h` parameter, and now accepts 
  `bins` and `binwidth` parameters to control the number of contours 
  (#1448, @has2k1).

* `stat_ecdf()` does a better job of adding padding to -Inf/Inf, and gains
  an argument `pad` to suppress the padding if not needed (#1467).

* `stat_function()` gains an `xlim` parameter (#1528). It once again works 
  with discrete x values (#1509).

* `stat_summary()` preserves sorted x order which avoids artefacts when
  display results with `geom_smooth()` (#1520).

* All elements should now inherit correctly for all themes except `theme_void()`.
  (@Katiedaisey, #1555) 

* `theme_void()` was completely void of text but facets and legends still
  need labels. They are now visible (@jiho). 

* You can once again set legend key and height width to unit arithmetic
  objects (like `2 * unit(1, "cm")`) (#1437).

* Eliminate spurious warning if you have a layer with no data and no aesthetics
  (#1451).

* Removed a superfluous comma in `theme-defaults.r` code (@jschoeley)

* Fixed a compatibility issue with `ggproto` and R versions prior to 3.1.2.
  (#1444)

# ggplot2 2.0.0

## Major changes

* ggplot no longer throws an error if you your plot has no layers. Instead it 
  automatically adds `geom_blank()` (#1246).
  
* New `cut_width()` is a convenient replacement for the verbose
  `plyr::round_any()`, with the additional benefit of offering finer
  control.

* New `geom_count()` is a convenient alias to `stat_sum()`. Use it when you
  have overlapping points on a scatterplot. `stat_sum()` now defaults to 
  using counts instead of proportions.

* New `geom_curve()` adds curved lines, with a similar specification to 
  `geom_segment()` (@veraanadi, #1088).

* Date and datetime scales now have `date_breaks`, `date_minor_breaks` and
  `date_labels` arguments so that you never need to use the long
  `scales::date_breaks()` or `scales::date_format()`.
  
* `geom_bar()` now has it's own stat, distinct from `stat_bin()` which was
  also used by `geom_histogram()`. `geom_bar()` now uses `stat_count()` 
  which counts values at each distinct value of x (i.e. it does not bin
  the data first). This can be useful when you want to show exactly which 
  values are used in a continuous variable.

* `geom_point()` gains a `stroke` aesthetic which controls the border width of 
  shapes 21-25 (#1133, @SeySayux). `size` and `stroke` are additive so a point 
  with `size = 5` and `stroke = 5` will have a diameter of 10mm. (#1142)

* New `position_nudge()` allows you to slightly offset labels (or other 
  geoms) from their corresponding points (#1109).

* `scale_size()` now maps values to _area_, not radius. Use `scale_radius()`
  if you want the old behaviour (not recommended, except perhaps for lines).

* New `stat_summary_bin()` works like `stat_summary()` but on binned data. 
  It's a generalisation of `stat_bin()` that can compute any aggregate,
  not just counts (#1274). Both default to `mean_se()` if no aggregation
  functions are supplied (#1386).

* Layers are now much stricter about their arguments - you will get an error
  if you've supplied an argument that isn't an aesthetic or a parameter.
  This is likely to cause some short-term pain but in the long-term it will make
  it much easier to spot spelling mistakes and other errors (#1293).
  
    This change does break a handful of geoms/stats that used `...` to pass 
    additional arguments on to the underlying computation. Now 
    `geom_smooth()`/`stat_smooth()` and `geom_quantile()`/`stat_quantile()` 
    use `method.args` instead (#1245, #1289); and `stat_summary()` (#1242), 
    `stat_summary_hex()`, and `stat_summary2d()` use `fun.args`.

### Extensibility

There is now an official mechanism for defining Stats, Geoms, and Positions in other packages. See `vignette("extending-ggplot2")` for details.

* All Geoms, Stats and Positions are now exported, so you can inherit from them
  when making your own objects (#989).

* ggplot2 no longer uses proto or reference classes. Instead, we now use 
  ggproto, a new OO system designed specifically for ggplot2. Unlike proto
  and RC, ggproto supports clean cross-package inheritance. Creating a new OO
  system isn't usually the right way to solve a problem, but I'm pretty sure
  it was necessary here. Read more about it in the vignette.

* `aes_()` replaces `aes_q()`. It also supports formulas, so the most concise 
  SE version of `aes(carat, price)` is now `aes_(~carat, ~price)`. You may
  want to use this form in packages, as it will avoid spurious `R CMD check` 
  warnings about undefined global variables.

### Text

* `geom_text()` has been overhauled to make labelling your data a little
  easier. It:
  
    * `nudge_x` and `nudge_y` arguments let you offset labels from their
      corresponding points (#1120). 
      
    * `check_overlap = TRUE` provides a simple way to avoid overplotting 
      of labels: labels that would otherwise overlap are omitted (#1039).
      
    * `hjust` and `vjust` can now be character vectors: "left", "center", 
      "right", "bottom", "middle", "top". New options include "inward" and 
      "outward" which align text towards and away from the center of the plot 
      respectively.

* `geom_label()` works like `geom_text()` but draws a rounded rectangle 
  underneath each label (#1039). This is useful when you want to label plots
  that are dense with data.

### Deprecated features

* The little used `aes_auto()` has been deprecated. 

* `aes_q()` has been replaced with `aes_()` to be consistent with SE versions
  of NSE functions in other packages.

* The `order` aesthetic is officially deprecated. It never really worked, and 
  was poorly documented.

* The `stat` and `position` arguments to `qplot()` have been deprecated.
  `qplot()` is designed for quick plots - if you need to specify position
  or stat, use `ggplot()` instead.

* The theme setting `axis.ticks.margin` has been deprecated: now use the margin 
  property of `axis.ticks`.
  
* `stat_abline()`, `stat_hline()` and `stat_vline()` have been removed:
  these were never suitable for use other than with `geom_abline()` etc
  and were not documented.

* `show_guide` has been renamed to `show.legend`: this more accurately
  reflects what it does (controls appearance of layer in legend), and uses the 
  same convention as other ggplot2 arguments (i.e. a `.` between names).
  (Yes, I know that's inconsistent with function names with use `_`, but it's
  too late to change now.)

A number of geoms have been renamed to be internally consistent:

* `stat_binhex()` and `stat_bin2d()` have been renamed to `stat_bin_hex()` 
  and `stat_bin_2d()` (#1274). `stat_summary2d()` has been renamed to 
  `stat_summary_2d()`, `geom_density2d()`/`stat_density2d()` has been renamed 
  to `geom_density_2d()`/`stat_density_2d()`.

* `stat_spoke()` is now `geom_spoke()` since I realised it's a
  reparameterisation of `geom_segment().

* `stat_bindot()` has been removed because it's so tightly coupled to
  `geom_dotplot()`. If you happened to use `stat_bindot()`, just change to
  `geom_dotplot()` (#1194).

All defunct functions have been removed.

### Default appearance

* The default `theme_grey()` background colour has been changed from "grey90" 
  to "grey92": this makes the background a little less visually prominent.

* Labels and titles have been tweaked for readability:

    * Axes labels are darker.
    
    * Legend and axis titles are given the same visual treatment.
    
    * The default font size dropped from 12 to 11. You might be surprised that 
      I've made the default text size smaller as it was already hard for
      many people to read. It turns out there was a bug in RStudio (fixed in 
      0.99.724), that shrunk the text of all grid based graphics. Once that
      was resolved the defaults seemed too big to my eyes.
    
    * More spacing between titles and borders.
    
    * Default margins scale with the theme font size, so the appearance at 
      larger font sizes should be considerably improved (#1228). 

* `alpha` now affects both fill and colour aesthetics (#1371).

* `element_text()` gains a margins argument which allows you to add additional
  padding around text elements. To help see what's going on use `debug = TRUE` 
  to display the text region and anchors.

* The default font size in `geom_text()` has been decreased from 5mm (14 pts)
  to 3.8 mm (11 pts) to match the new default theme sizes.

* A diagonal line is no longer drawn on bar and rectangle legends. Instead, the
  border has been tweaked to be more visible, and more closely match the size of 
  line drawn on the plot.

* `geom_pointrange()` and `geom_linerange()` get vertical (not horizontal)
  lines in the legend (#1389).

* The default line `size` for `geom_smooth()` has been increased from 0.5 to 1 
  to make it easier to see when overlaid on data.
  
* `geom_bar()` and `geom_rect()` use a slightly paler shade of grey so they
  aren't so visually heavy.
  
* `geom_boxplot()` now colours outliers the same way as the boxes.

* `geom_point()` now uses shape 19 instead of 16. This looks much better on 
  the default Linux graphics device. (It's very slightly smaller than the old 
  point, but it shouldn't affect any graphics significantly)

* Sizes in ggplot2 are measured in mm. Previously they were converted to pts 
  (for use in grid) by multiplying by 72 / 25.4. However, grid uses printer's 
  points, not Adobe (big pts), so sizes are now correctly multiplied by 
  72.27 / 25.4. This is unlikely to noticeably affect display, but it's
  technically correct (<https://youtu.be/hou0lU8WMgo>).

* The default legend will now allocate multiple rows (if vertical) or
  columns (if horizontal) in order to make a legend that is more likely to
  fit on the screen. You can override with the `nrow`/`ncol` arguments
  to `guide_legend()`

    ```R
    p <- ggplot(mpg, aes(displ,hwy, colour = model)) + geom_point()
    p
    p + theme(legend.position = "bottom")
    # Previous behaviour
    p + guides(colour = guide_legend(ncol = 1))
    ```

### New and updated themes

* New `theme_void()` is completely empty. It's useful for plots with non-
  standard coordinates or for drawings (@jiho, #976).

* New `theme_dark()` has a dark background designed to make colours pop out
  (@jiho, #1018)

* `theme_minimal()` became slightly more minimal by removing the axis ticks:
  labels now line up directly beneath grid lines (@tomschloss, #1084)

* New theme setting `panel.ontop` (logical) make it possible to place 
  background elements (i.e., gridlines) on top of data. Best used with 
  transparent `panel.background` (@noamross. #551).

### Labelling

The facet labelling system was updated with many new features and a
more flexible interface (@lionel-). It now works consistently across
grid and wrap facets. The most important user visible changes are:

* `facet_wrap()` gains a `labeller` option (#25).

* `facet_grid()` and `facet_wrap()` gain a `switch` argument to
  display the facet titles near the axes. When switched, the labels
  become axes subtitles. `switch` can be set to "x", "y" or "both"
  (the latter only for grids) to control which margin is switched.

The labellers (such as `label_value()` or `label_both()`) also get
some new features:

* They now offer the `multi_line` argument to control whether to
  display composite facets (those specified as `~var1 + var2`) on one
  or multiple lines.

* In `label_bquote()` you now refer directly to the names of
  variables. With this change, you can create math expressions that
  depend on more than one variable. This math expression can be
  specified either for the rows or the columns and you can also
  provide different expressions to each margin.

  As a consequence of these changes, referring to `x` in backquoted
  expressions is deprecated.

* Similarly to `label_bquote()`, `labeller()` now take `.rows` and
  `.cols` arguments. In addition, it also takes `.default`.
  `labeller()` is useful to customise how particular variables are
  labelled. The three additional arguments specify how to label the
  variables are not specifically mentioned, respectively for rows,
  columns or both. This makes it especially easy to set up a
  project-wide labeller dispatcher that can be reused across all your
  plots. See the documentation for an example.

* The new labeller `label_context()` adapts to the number of factors
  facetted over. With a single factor, it displays only the values,
  just as before. But with multiple factors in a composite margin
  (e.g. with `~cyl + am`), the labels are passed over to
  `label_both()`. This way the variables names are displayed with the
  values to help identifying them.

On the programming side, the labeller API has been rewritten in order
to offer more control when facetting over multiple factors (e.g. with
formulae such as `~cyl + am`). This also means that if you have
written custom labellers, you will need to update them for this
version of ggplot.

* Previously, a labeller function would take `variable` and `value`
  arguments and return a character vector. Now, they take a data frame
  of character vectors and return a list. The input data frame has one
  column per factor facetted over and each column in the returned list
  becomes one line in the strip label. See documentation for more
  details.

* The labels received by a labeller now contain metadata: their margin
  (in the "type" attribute) and whether they come from a wrap or a
  grid facet (in the "facet" attribute).

* Note that the new `as_labeller()` function operator provides an easy
  way to transform an existing function to a labeller function. The
  existing function just needs to take and return a character vector.

## Documentation

* Improved documentation for `aes()`, `layer()` and much much more.

* I've tried to reduce the use of `...` so that you can see all the 
  documentation in one place rather than having to integrate multiple pages.
  In some cases this has involved adding additional arguments to geoms
  to make it more clear what you can do:
  
    *  `geom_smooth()` gains explicit `method`, `se` and `formula` arguments.
    
    * `geom_histogram()` gains `binwidth`, `bins`, origin` and `right` 
      arguments.
      
    * `geom_jitter()` gains `width` and `height` arguments to make it easier
      to control the amount of jittering without using the lengthy 
      `position_jitter()` function (#1116)

* Use of `qplot()` in examples has been minimised (#1123, @hrbrmstr). This is
  inline with the 2nd edition of the ggplot2 box, which minimises the use of 
  `qplot()` in favour of `ggplot()`.

* Tighly linked geoms and stats (e.g. `geom_boxplot()` and `stat_boxplot()`) 
  are now documented in the same file so you can see all the arguments in one
  place. Variations of the same idea (e.g. `geom_path()`, `geom_line()`, and
  `geom_step()`) are also documented together.

* It's now obvious that you can set the `binwidth` parameter for
  `stat_bin_hex()`, `stat_summary_hex()`, `stat_bin_2d()`, and
  `stat_summary_2d()`. 

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

* All partially matched arguments and `$` have been been replaced with 
  full matches (@jimhester, #1134).

* ggplot2 now exports `alpha()` from the scales package (#1107), and `arrow()` 
  and `unit()` from grid (#1225). This means you don't need attach scales/grid 
  or do `scales::`/`grid::` for these commonly used functions.

* `aes_string()` now only parses character inputs. This fixes bugs when
  using it with numbers and non default `OutDec` settings (#1045).

* `annotation_custom()` automatically adds a unique id to each grob name,
  making it easier to plot multiple grobs with the same name (e.g. grobs of
  ggplot2 graphics) in the same plot (#1256).

* `borders()` now accepts xlim and ylim arguments for specifying the geographical 
  region of interest (@markpayneatwork, #1392).

* `coord_cartesian()` applies the same expansion factor to limits as for scales. 
  You can suppress with `expand = FALSE` (#1207).

* `coord_trans()` now works when breaks are suppressed (#1422).

* `cut_number()` gives error message if the number of requested bins can
  be created because there are two few unique values (#1046).

* Character labels in `facet_grid()` are no longer (incorrectly) coerced into
  factors. This caused problems with custom label functions (#1070).

* `facet_wrap()` and `facet_grid()` now allow you to use non-standard
  variable names by surrounding them with backticks (#1067).

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
  running from a script (#1326).

* `guide_colorbar()` creates correct legend if only one color (@krlmlr, #943).

* `guide_colorbar()` no longer fails when the legend is empty - previously
  this often masked misspecifications elsewhere in the plot (#967).

* New `layer_data()` function extracts the data used for plotting for a given
  layer. It's mostly useful for testing.

* User supplied `minor_breaks` can now be supplied on the same scale as 
  the data, and will be automatically transformed with by scale (#1385).

* You can now suppress the appearance of an axis/legend title (and the space
  that would allocated for it) with `NULL` in the `scale_` function. To
  use the default lable, use `waiver()` (#1145).

* Position adjustments no longer warn about potentially varying ranges
  because the problem rarely occurs in practice and there are currently a
  lot of false positives since I don't understand exactly what FP criteria
  I should be testing.

* `scale_fill_grey()` now uses red for missing values. This matches
  `scale_colour_grey()` and makes it obvious where missing values lie.
  Override with `na.value`.

* `scale_*_gradient2()` defaults to using Lab colour space.

* `scale_*_gradientn()` now allows `colours` or `colors` (#1290)

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
  `scales::brewer_pal`, making it easier to change the order of colours 
  (@jiho, #1139).

* `scale_x_date()` now clips dates outside the limits in the same way as
  `scale_x_continuous()` (#1090).

* `stat_bin()` gains `bins` arguments, which denotes the number of bins. Now
  you can set `bins=100` instead of `binwidth=0.5`. Note that `breaks` or
  `binwidth` will override it (@tmshn, #1158, #102).

* `stat_boxplot()` warns if a continuous variable is used for the `x` aesthetic
  without also supplying a `group` aesthetic (#992, @krlmlr).

* `stat_summary_2d()` and `stat_bin_2d()` now share exactly the same code for 
  determining breaks from `bins`, `binwidth`, and `origin`. 
  
* `stat_summary_2d()` and `stat_bin_2d()` now output in tile/raster compatible 
  form instead of rect compatible form. 

* Automatically computed breaks do not lead to an error for transformations like
  "probit" where the inverse can map to infinity (#871, @krlmlr)

* `stat_function()` now always evaluates the function on the original scale.
  Previously it computed the function on transformed scales, giving incorrect
  values (@BrianDiggs, #1011).

* `strip_dots` works with anonymous functions within calculated aesthetics 
  (e.g. `aes(sapply(..density.., function(x) mean(x))))` (#1154, @NikNakk)

* `theme()` gains `validate = FALSE` parameter to turn off validation, and 
  hence store arbitrary additional data in the themes. (@tdhock, #1121)

* Improved the calculation of segments needed to draw the curve representing
  a line when plotted in polar coordinates. In some cases, the last segment
  of a multi-segment line was not drawn (@BrianDiggs, #952)
