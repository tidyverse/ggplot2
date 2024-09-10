# activAnalyzer (patchwork)

# actxps (thematic)

# AeRobiology (unknown)

```
# <details>
# 
# * Version: 2.0.1
# * GitHub: NA
# * Source code: https://github.com/cran/AeRobiology
# * Date/Publication: 2019-06-03 06:20:03 UTC
# * Number of recursive dependencies: 98
# 
# Run `revdepcheck::cloud_details(, "AeRobiology")` for more info
# 
# </details>
# 
# ## Newly broken
# 
# *   checking re-building of vignette outputs ... NOTE
#     ```
#     Error(s) in re-building vignettes:
#     --- re-building ‘my-vignette.Rmd’ using rmarkdown
#     ```
# 
# ## In both
# 
# *   checking running R code from vignettes ... ERROR
#     ```
#     Errors in running code in vignettes:
#     when running code in ‘my-vignette.Rmd’
#       ...
#     +     export.plot = FALSE, export.result = FALSE, n.types = 3, 
#     +     y.start = 2011, y.end = .... [TRUNCATED] 
#     
#     > iplot_abundance(munich_pollen, interpolation = FALSE, 
#     +     export.plot = FALSE, export.result = FALSE, n.types = 3, 
#     +     y.start = 2011, y.end = .... [TRUNCATED] 
#     
#       When sourcing ‘my-vignette.R’:
#     Error: subscript out of bounds
#     Execution halted
#     
#       ‘my-vignette.Rmd’ using ‘UTF-8’... failed
#     ```
```

# agricolaeplotr (missing labels)

# AnalysisLin (plotly)

# animbook (plotly)

# ANN2 (missing labels)

# aplot (patchwork)

# applicable (missing labels)

# ASRgenomics (factoextra)

# autoplotly (plotly)

# autoReg (patchwork)

# bartMan (ggnewscale)

# bayesAB (missing labels)

# BayesGrowth (ggdist)

# BayesianReasoning (missing labels)

# BayesMallows (missing labels)

# bayesplot (missing labels)

# bayestestR (patchwork)

# beastt (ggdist)

# besthr (patchwork)

# biclustermd (xintercept/yintercept class)

# biodosetools (missing labels)

# boxly (plotly)

# braidReports (annotation_logticks)

# breathtestcore (plot slots)

# brolgar (facet params: as.table)

# cartograflow (plotly)

# cartographr (unknown)

```
# <details>
# 
# * Version: 0.2.2
# * GitHub: https://github.com/da-wi/cartographr
# * Source code: https://github.com/cran/cartographr
# * Date/Publication: 2024-06-28 14:50:09 UTC
# * Number of recursive dependencies: 99
# 
# Run `revdepcheck::cloud_details(, "cartographr")` for more info
# 
# </details>
# 
# ## Newly broken
# 
# *   checking tests ... ERROR
#     ```
#       Running ‘testthat.R’
#     Running the tests in ‘tests/testthat.R’ failed.
#     Complete output:
#       > # This file is part of the standard setup for testthat.
#       > # It is recommended that you do not modify it.
#       > #
#       > # Where should you do additional test configuration?
#       > # Learn more about the roles of various files in:
#       > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
#       > # * https://testthat.r-lib.org/articles/special-files.html
#     ...
#        21. │                   └─base::stop(...)
#        22. └─base::.handleSimpleError(...)
#        23.   └─rlang (local) h(simpleError(msg, call))
#        24.     └─handlers[[1L]](cnd)
#        25.       └─cli::cli_abort(...)
#        26.         └─rlang::abort(...)
#       
#       [ FAIL 1 | WARN 0 | SKIP 0 | PASS 106 ]
#       Error: Test failures
#       Execution halted
#     ```
# 
# ## In both
# 
# *   checking installed package size ... NOTE
#     ```
#       installed size is  5.3Mb
#       sub-directories of 1Mb or more:
#         data   3.5Mb
#     ```
```

# cats (plotly)

# cheem (plotly)

# chillR (patchwork)

# chronicle (plotly)

# circhelp (patchwork)

# clifro (missing labels)

# clinDataReview (plotly)

# clinUtils (plotly)

# CohortPlat (plotly)

# CoreMicrobiomeR (plotly)

# correlationfunnel (plotly)

# corrViz (plotly)

# countfitteR (missing labels)

# covidcast (unknown)

```
# <details>
# 
# * Version: 0.5.2
# * GitHub: https://github.com/cmu-delphi/covidcast
# * Source code: https://github.com/cran/covidcast
# * Date/Publication: 2023-07-12 23:40:06 UTC
# * Number of recursive dependencies: 93
# 
# Run `revdepcheck::cloud_details(, "covidcast")` for more info
# 
# </details>
# 
# ## Newly broken
# 
# *   checking tests ... ERROR
#     ```
#       Running ‘testthat.R’
#     Running the tests in ‘tests/testthat.R’ failed.
#     Complete output:
#       > library(testthat)
#       > library(covidcast)
#       We encourage COVIDcast API users to register on our mailing list:
#       https://lists.andrew.cmu.edu/mailman/listinfo/delphi-covidcast-api
#       We'll send announcements about new data sources, package updates,
#       server maintenance, and new features.
#       > 
#     ...
#       • plot/default-county-choropleth.svg
#       • plot/default-hrr-choropleth-with-include.svg
#       • plot/default-msa-choropleth-with-include.svg
#       • plot/default-state-choropleth-with-include.svg
#       • plot/default-state-choropleth-with-range.svg
#       • plot/state-choropleth-with-no-metadata.svg
#       • plot/state-line-graph-with-range.svg
#       • plot/state-line-graph-with-stderrs.svg
#       Error: Test failures
#       Execution halted
#     ```
# 
# *   checking running R code from vignettes ... ERROR
#     ```
#     Errors in running code in vignettes:
#     when running code in ‘plotting-signals.Rmd’
#       ...
#     > knitr::opts_chunk$set(fig.width = 6, fig.height = 4)
#     
#     > plot(dv)
#     
#       When sourcing ‘plotting-signals.R’:
#     Error: Problem while setting up geom aesthetics.
#     ℹ Error occurred in the 6th layer.
#     Caused by error in `$<-.data.frame`:
#     ! replacement has 1 row, data has 0
#     Execution halted
#     
#       ‘correlation-utils.Rmd’ using ‘UTF-8’... OK
#       ‘covidcast.Rmd’ using ‘UTF-8’... OK
#       ‘external-data.Rmd’ using ‘UTF-8’... OK
#       ‘multi-signals.Rmd’ using ‘UTF-8’... OK
#       ‘plotting-signals.Rmd’ using ‘UTF-8’... failed
#     ```
# 
# *   checking re-building of vignette outputs ... NOTE
#     ```
#     Error(s) in re-building vignettes:
#     --- re-building ‘correlation-utils.Rmd’ using rmarkdown
#     --- finished re-building ‘correlation-utils.Rmd’
#     
#     --- re-building ‘covidcast.Rmd’ using rmarkdown
#     ```
# 
# ## In both
# 
# *   checking data for non-ASCII characters ... NOTE
#     ```
#       Note: found 20 marked UTF-8 strings
#     ```
```

# crosshap (plotly)

# cubble (patchwork)

# deeptime (ggnewscale)

# distributional (ggdist)

# dittoViz (plotly)

# EGM (missing labels)

# entropart (default access)

# epiCleanr (ggdist)

# esci (ggdist)

# evalITR (unknown)

```
# <details>
# 
# * Version: 1.0.0
# * GitHub: https://github.com/MichaelLLi/evalITR
# * Source code: https://github.com/cran/evalITR
# * Date/Publication: 2023-08-25 23:10:06 UTC
# * Number of recursive dependencies: 167
# 
# Run `revdepcheck::cloud_details(, "evalITR")` for more info
# 
# </details>
# 
# ## Newly broken
# 
# *   checking re-building of vignette outputs ... NOTE
#     ```
#     Error(s) in re-building vignettes:
#     --- re-building ‘cv_multiple_alg.Rmd’ using rmarkdown
#     ```
# 
# ## In both
# 
# *   checking running R code from vignettes ... ERROR
#     ```
#     Errors in running code in vignettes:
#     when running code in ‘cv_multiple_alg.Rmd’
#       ...
#         intersect, setdiff, setequal, union
#     
#     
#     > load("../data/star.rda")
#     Warning in readChar(con, 5L, useBytes = TRUE) :
#       cannot open compressed file '../data/star.rda', probable reason 'No such file or directory'
#     
#     ...
#     Execution halted
#     
#       ‘cv_multiple_alg.Rmd’ using ‘UTF-8’... failed
#       ‘cv_single_alg.Rmd’ using ‘UTF-8’... failed
#       ‘install.Rmd’ using ‘UTF-8’... OK
#       ‘paper_alg1.Rmd’ using ‘UTF-8’... OK
#       ‘sample_split.Rmd’ using ‘UTF-8’... failed
#       ‘sample_split_caret.Rmd’ using ‘UTF-8’... failed
#       ‘user_itr.Rmd’ using ‘UTF-8’... failed
#       ‘user_itr_algs.Rmd’ using ‘UTF-8’... failed
#     ```
# 
# *   checking dependencies in R code ... NOTE
#     ```
#     Namespaces in Imports field not imported from:
#       ‘forcats’ ‘rqPen’ ‘utils’
#       All declared Imports should be used.
#     ```
```

# eventstudyr (missing labels)

# EvoPhylo (patchwork)

# expirest (missing labels)

# explainer (plotly)

# ezEDA (missing labels)

# ezplot (0-length width)

# fable.prophet (ggdist)

# fabletools (ggdist)

# factoextra (0-length width)

# fairmodels (missing labels)

# fddm (ggforce)

# feasts (missing labels)

# ffp (ggdist)

# fido (ggdist)

# flipr (unknown)

```
# <details>
# 
# * Version: 0.3.3
# * GitHub: https://github.com/LMJL-Alea/flipr
# * Source code: https://github.com/cran/flipr
# * Date/Publication: 2023-08-23 09:00:02 UTC
# * Number of recursive dependencies: 106
# 
# Run `revdepcheck::cloud_details(, "flipr")` for more info
# 
# </details>
# 
# ## Newly broken
# 
# *   checking re-building of vignette outputs ... NOTE
#     ```
#     Error(s) in re-building vignettes:
#     --- re-building ‘alternative.Rmd’ using rmarkdown
#     --- finished re-building ‘alternative.Rmd’
#     
#     --- re-building ‘exactness.Rmd’ using rmarkdown
#     
#     Quitting from lines 142-177 [unnamed-chunk-1] (exactness.Rmd)
#     Error: processing vignette 'exactness.Rmd' failed with diagnostics:
#     subscript out of bounds
#     --- failed re-building ‘exactness.Rmd’
#     
#     --- re-building ‘flipr.Rmd’ using rmarkdown
#     ```
# 
# ## In both
# 
# *   checking running R code from vignettes ... ERROR
#     ```
#     Errors in running code in vignettes:
#     when running code in ‘exactness.Rmd’
#       ...
#     
#     > library(flipr)
#     
#     > load("../R/sysdata.rda")
#     Warning in readChar(con, 5L, useBytes = TRUE) :
#       cannot open compressed file '../R/sysdata.rda', probable reason 'No such file or directory'
#     
#     ...
#       cannot open compressed file '../R/sysdata.rda', probable reason 'No such file or directory'
#     
#       When sourcing ‘plausibility.R’:
#     Error: cannot open the connection
#     Execution halted
#     
#       ‘alternative.Rmd’ using ‘UTF-8’... OK
#       ‘exactness.Rmd’ using ‘UTF-8’... failed
#       ‘flipr.Rmd’ using ‘UTF-8’... failed
#       ‘plausibility.Rmd’ using ‘UTF-8’... failed
#     ```
# 
# *   checking installed package size ... NOTE
#     ```
#       installed size is 11.0Mb
#       sub-directories of 1Mb or more:
#         doc    9.1Mb
#         libs   1.2Mb
#     ```
```

# foqat (ggnewscale)

# forestly (patchwork)

# frailtyEM (plotly)

# funcharts (patchwork)

# geomtextpath (default access)

# GGally (missing labels)

# gganimate (gganimate)

# ggbrain (ggnewscale)

# ggbreak (patchwork)

# ggdark (default access)

# ggdist (ggdist)

# ggDoubleHeat (ggnewscale)

# ggeasy (missing labels)

# ggedit (saved to disk)

# ggESDA (passing NULL aesthetic mapping)

# ggfixest (visual differences)

# ggforce (ggforce)

Note: facet_zoom/facet_col/facet_row

# ggformula (docs)

# ggfortify (tests)

# gggenomes (patchwork)

# ggh4x (unknown)

```
# <details>
# 
# * Version: 0.2.8
# * GitHub: https://github.com/teunbrand/ggh4x
# * Source code: https://github.com/cran/ggh4x
# * Date/Publication: 2024-01-23 21:00:02 UTC
# * Number of recursive dependencies: 77
# 
# Run `revdepcheck::cloud_details(, "ggh4x")` for more info
# 
# </details>
# 
# ## Newly broken
# 
# *   checking examples ... ERROR
#     ```
#     Running examples in ‘ggh4x-Ex.R’ failed
#     The error most likely occurred in:
#     
#     > ### Name: guide_stringlegend
#     > ### Title: String legend
#     > ### Aliases: guide_stringlegend
#     > 
#     > ### ** Examples
#     > 
#     > p <- ggplot(mpg, aes(displ, hwy)) +
#     +   geom_point(aes(colour = manufacturer))
#     > 
#     > # String legend can be set in the `guides()` function
#     > p + guides(colour = guide_stringlegend(ncol = 2))
#     Error in (function (layer, df)  : 
#       argument "theme" is missing, with no default
#     Calls: <Anonymous> ... use_defaults -> eval_from_theme -> %||% -> calc_element
#     Execution halted
#     ```
# 
# *   checking tests ... ERROR
#     ```
#       Running ‘testthat.R’
#     Running the tests in ‘tests/testthat.R’ failed.
#     Complete output:
#       > library(testthat)
#       > library(ggh4x)
#       Loading required package: ggplot2
#       > 
#       > test_check("ggh4x")
#       [ FAIL 2 | WARN 20 | SKIP 18 | PASS 753 ]
#       
#     ...
#        25.                                     └─ggplot2 (local) compute_geom_2(..., self = self)
#        26.                                       └─self$geom$use_defaults(...)
#        27.                                         └─ggplot2 (local) use_defaults(..., self = self)
#        28.                                           └─ggplot2:::eval_from_theme(default_aes, theme)
#        29.                                             ├─calc_element("geom", theme) %||% .default_geom_element
#        30.                                             └─ggplot2::calc_element("geom", theme)
#       
#       [ FAIL 2 | WARN 20 | SKIP 18 | PASS 753 ]
#       Error: Test failures
#       Execution halted
#     ```
# 
# *   checking re-building of vignette outputs ... NOTE
#     ```
#     Error(s) in re-building vignettes:
#     --- re-building ‘Facets.Rmd’ using rmarkdown
#     ```
# 
# ## In both
# 
# *   checking running R code from vignettes ... ERROR
#     ```
#     Errors in running code in vignettes:
#     when running code in ‘Miscellaneous.Rmd’
#       ...
#     
#     > ggplot(diamonds, aes(price, carat, colour = clarity)) + 
#     +     geom_point(shape = ".") + scale_colour_brewer(palette = "Dark2", 
#     +     guide = "stri ..." ... [TRUNCATED] 
#     Warning: The S3 guide system was deprecated in ggplot2 3.5.0.
#     ℹ It has been replaced by a ggproto system that can be extended.
#     
#     ...
#     ℹ Error occurred in the 1st layer.
#     Caused by error in `setup_params()`:
#     ! A discrete 'nbinom' distribution cannot be fitted to continuous data.
#     Execution halted
#     
#       ‘Facets.Rmd’ using ‘UTF-8’... OK
#       ‘Miscellaneous.Rmd’ using ‘UTF-8’... failed
#       ‘PositionGuides.Rmd’ using ‘UTF-8’... OK
#       ‘Statistics.Rmd’ using ‘UTF-8’... failed
#       ‘ggh4x.Rmd’ using ‘UTF-8’... OK
#     ```
```

# gghighlight (default access)

# ggHoriPlot (patchwork)

# ggiraph (additional params)

# ggiraphExtra (additional params)

# ggmice (plotly)

# ggmulti (custom use_defaults)

# ggnewscale (ggnewscale)

# ggparallel (unknown)

```
# <details>
# 
# * Version: 0.4.0
# * GitHub: https://github.com/heike/ggparallel
# * Source code: https://github.com/cran/ggparallel
# * Date/Publication: 2024-03-09 22:00:02 UTC
# * Number of recursive dependencies: 51
# 
# Run `revdepcheck::cloud_details(, "ggparallel")` for more info
# 
# </details>
# 
# ## Newly broken
# 
# *   checking tests ... ERROR
#     ```
#       Running ‘testthat.R’
#     Running the tests in ‘tests/testthat.R’ failed.
#     Complete output:
#       > # This file is part of the standard setup for testthat.
#       > # It is recommended that you do not modify it.
#       > #
#       > # Where should you do additional test configuration?
#       > # Learn more about the roles of various files in:
#       > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
#       > # * https://testthat.r-lib.org/articles/special-files.html
#     ...
#        12.                     └─self$get_layer_key(params, layers[include], data[include], theme)
#        13.                       └─ggplot2 (local) get_layer_key(...)
#        14.                         └─base::Map(...)
#        15.                           └─base::mapply(FUN = f, ..., SIMPLIFY = FALSE)
#        16.                             └─ggplot2 (local) `<fn>`(layer = dots[[1L]][[1L]], df = dots[[2L]][[1L]])
#        17.                               └─layer$compute_geom_2(key, single_params, theme)
#       
#       [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
#       Error: Test failures
#       Execution halted
#     ```
```

# ggpicrust2 (patchwork)

# ggpie (ggnewscale)

# ggplotlyExtra (plotly)

# ggpol (default access)

# ggpubr (update tests)

# ggraph (guide theme passing)

# ggredist (unknown)

```
# <details>
# 
# * Version: 0.0.2
# * GitHub: https://github.com/alarm-redist/ggredist
# * Source code: https://github.com/cran/ggredist
# * Date/Publication: 2022-11-23 11:20:02 UTC
# * Number of recursive dependencies: 67
# 
# Run `revdepcheck::cloud_details(, "ggredist")` for more info
# 
# </details>
# 
# ## Newly broken
# 
# *   checking examples ... ERROR
#     ```
#     Running examples in ‘ggredist-Ex.R’ failed
#     The error most likely occurred in:
#     
#     > ### Name: geom_district_text
#     > ### Title: Label Map Regions
#     > ### Aliases: geom_district_text geom_district_label
#     > ###   stat_district_coordinates StatDistrictCoordinates GeomDistrictText
#     > ### Keywords: datasets
#     > 
#     > ### ** Examples
#     ...
#      22. │                       └─coord$transform(data, panel_params)
#      23. │                         └─ggplot2 (local) transform(..., self = self)
#      24. │                           └─ggplot2:::sf_rescale01(...)
#      25. │                             └─sf::st_normalize(x, c(x_range[1], y_range[1], x_range[2], y_range[2]))
#      26. └─base::.handleSimpleError(...)
#      27.   └─rlang (local) h(simpleError(msg, call))
#      28.     └─handlers[[1L]](cnd)
#      29.       └─cli::cli_abort(...)
#      30.         └─rlang::abort(...)
#     Execution halted
#     ```
```

# ggRtsy (missing labels)

# ggseqplot (length 0 width)

# ggside (unknown)

```
# <details>
# 
# * Version: 0.3.1
# * GitHub: https://github.com/jtlandis/ggside
# * Source code: https://github.com/cran/ggside
# * Date/Publication: 2024-03-01 09:12:37 UTC
# * Number of recursive dependencies: 76
# 
# Run `revdepcheck::cloud_details(, "ggside")` for more info
# 
# </details>
# 
# ## Newly broken
# 
# *   checking tests ... ERROR
#     ```
#       Running ‘testthat.R’
#     Running the tests in ‘tests/testthat.R’ failed.
#     Complete output:
#       > library(testthat)
#       > library(ggplot2)
#       > library(ggside)
#       Registered S3 method overwritten by 'ggside':
#         method from   
#         +.gg   ggplot2
#       > 
#     ...
#       • ops_meaningful/alpha-0-5-from-function.svg
#       • side_layers/boxplot2.svg
#       • vdiff_irisScatter/collapsed-histo.svg
#       • vdiff_irisScatter/facetgrid-collapsed-density.svg
#       • vdiff_irisScatter/facetgrid-histo.svg
#       • vdiff_irisScatter/facetgrid-side-density.svg
#       • vdiff_irisScatter/stacked-side-density.svg
#       • vdiff_irisScatter/yside-histo.svg
#       Error: Test failures
#       Execution halted
#     ```
# 
# *   checking for code/documentation mismatches ... WARNING
#     ```
#     Codoc mismatches from documentation object 'geom_xsideabline':
#     geom_xsidehline
#       Code: function(mapping = NULL, data = NULL, position = "identity",
#                      ..., yintercept, na.rm = FALSE, show.legend = NA)
#       Docs: function(mapping = NULL, data = NULL, ..., yintercept, na.rm =
#                      FALSE, show.legend = NA)
#       Argument names in code not in docs:
#         position
#       Mismatches in argument names (first 3):
#         Position: 3 Code: position Docs: ...
#     ...
#       Docs: function(mapping = NULL, data = NULL, stat = "identity",
#                      position = "identity", ..., lineend = "butt", linejoin
#                      = "round", linemitre = 10, arrow = NULL, na.rm =
#                      FALSE, show.legend = NA, inherit.aes = TRUE)
#       Argument names in code not in docs:
#         arrow.fill
#       Mismatches in argument names:
#         Position: 10 Code: arrow.fill Docs: na.rm
#         Position: 11 Code: na.rm Docs: show.legend
#         Position: 12 Code: show.legend Docs: inherit.aes
#     ```
```

# ggspatial (missing defaults)

# ggtern (ggtern)

# ggupset (unknown)

```
# <details>
# 
# * Version: 0.4.0
# * GitHub: https://github.com/const-ae/ggupset
# * Source code: https://github.com/cran/ggupset
# * Date/Publication: 2024-06-24 10:10:04 UTC
# * Number of recursive dependencies: 46
# 
# Run `revdepcheck::cloud_details(, "ggupset")` for more info
# 
# </details>
# 
# ## Newly broken
# 
# *   checking examples ... ERROR
#     ```
#     Running examples in ‘ggupset-Ex.R’ failed
#     The error most likely occurred in:
#     
#     > ### Name: axis_combmatrix
#     > ### Title: Convert delimited text labels into a combination matrix axis
#     > ### Aliases: axis_combmatrix
#     > 
#     > ### ** Examples
#     > 
#     >   library(ggplot2)
#     ...
#     Datsun 710        Cyl: 4_Gears: 4
#     Hornet 4 Drive    Cyl: 6_Gears: 3
#     Hornet Sportabout Cyl: 8_Gears: 3
#     Valiant           Cyl: 6_Gears: 3
#     >   ggplot(mtcars, aes(x=combined)) +
#     +     geom_bar() +
#     +     axis_combmatrix(sep = "_")
#     Error in as.unit(e2) : object is not coercible to a unit
#     Calls: <Anonymous> ... polylineGrob -> is.unit -> unit.c -> Ops.unit -> as.unit
#     Execution halted
#     ```
```

# ggVennDiagram (plotly)

# greatR (patchwork)

# Greymodels (plotly)

# gtExtras (unknown)

```
# <details>
# 
# * Version: 0.5.0
# * GitHub: https://github.com/jthomasmock/gtExtras
# * Source code: https://github.com/cran/gtExtras
# * Date/Publication: 2023-09-15 22:32:06 UTC
# * Number of recursive dependencies: 105
# 
# Run `revdepcheck::cloud_details(, "gtExtras")` for more info
# 
# </details>
# 
# ## Newly broken
# 
# *   checking tests ... ERROR
#     ```
#       Running ‘testthat.R’
#     Running the tests in ‘tests/testthat.R’ failed.
#     Complete output:
#       > library(testthat)
#       > library(gtExtras)
#       Loading required package: gt
#       
#       Attaching package: 'gt'
#       
#       The following object is masked from 'package:testthat':
#     ...
#       ══ Failed tests ════════════════════════════════════════════════════════════════
#       ── Failure ('test-gt_plt_bar.R:44:3'): gt_plt_bar svg is created and has specific values ──
#       `bar_neg_vals` (`actual`) not equal to c("49.19", "32.79", "16.40", "16.40", "32.79", "49.19") (`expected`).
#       
#       `actual`:   "49.19" "32.79" "16.40" "0.00"  "0.00"  "0.00" 
#       `expected`: "49.19" "32.79" "16.40" "16.40" "32.79" "49.19"
#       
#       [ FAIL 1 | WARN 14 | SKIP 23 | PASS 115 ]
#       Error: Test failures
#       Execution halted
#     ```
```

# HaploCatcher (patchwork)

# healthyR (plotly)

# healthyR.ts (plotly)

# heatmaply (plotly)

# hermiter (patchwork)

# hesim (missing labels)

# hidecan (ggnewscale)

# HVT (plotly)

# hypsoLoop (namespace conflict)

# ICvectorfields (ggnewscale)

# idopNetwork (patchwork)

# inferCSN (plotly)

# insurancerating (patchwork)

# inTextSummaryTable (default access)

# inventorize (unknown)

```
# <details>
# 
# * Version: 1.1.1
# * GitHub: NA
# * Source code: https://github.com/cran/inventorize
# * Date/Publication: 2022-05-31 22:20:09 UTC
# * Number of recursive dependencies: 71
# 
# Run `revdepcheck::cloud_details(, "inventorize")` for more info
# 
# </details>
# 
# ## Newly broken
# 
# *   checking whether package ‘inventorize’ can be installed ... ERROR
#     ```
#     Installation failed.
#     See ‘/tmp/workdir/inventorize/new/inventorize.Rcheck/00install.out’ for details.
#     ```
# 
# ## Installation
# 
# ### Devel
# 
# ```
# * installing *source* package ‘inventorize’ ...
# ** package ‘inventorize’ successfully unpacked and MD5 sums checked
# ** using staged installation
# ** R
# ** byte-compile and prepare package for lazy loading
# Error in pm[[2]] : subscript out of bounds
# Error: unable to load R code in package ‘inventorize’
# Execution halted
# ERROR: lazy loading failed for package ‘inventorize’
# * removing ‘/tmp/workdir/inventorize/new/inventorize.Rcheck/inventorize’
# 
# 
# ```
# ### CRAN
# 
# ```
# * installing *source* package ‘inventorize’ ...
# ** package ‘inventorize’ successfully unpacked and MD5 sums checked
# ** using staged installation
# ** R
# ** byte-compile and prepare package for lazy loading
# Warning in qgamma(service_level, alpha, beta) : NaNs produced
# Warning in qgamma(service_level, alpha, beta) : NaNs produced
# ** help
# *** installing help indices
# ** building package indices
# ** testing if installed package can be loaded from temporary location
# ** testing if installed package can be loaded from final location
# ** testing if installed package keeps a record of temporary installation path
# * DONE (inventorize)
# 
# 
# ```
```
# karel (gganimate)

# kDGLM (plotly)

# latentcor (plotly)

# lcars (device issue)

# lemon (resolve theme)

# lfproQC (plotly)

# LMoFit (saved to disk)

# manydata (plot slots)

# MARVEL (ggnewscale)

# MBNMAdose (cannot reproduce)

# MBNMAtime (ggdist)

# MetaNet (ggnewscale)

# metR (fixed in dev)

# migraph (missing labels)

# MiMIR (plotly)

# miRetrieve (plotly)

# misspi (plotly)

# mizer (cannot reproduce)

# mlr3spatiotempcv (patchwork)

# mlr3viz (patchwork)

# modeltime.resample (plotly)

# move (false positive)

# mtb (missing labels)

# neatmaps (plotly)

# NetFACS (false positive)

# NeuralSens (ggnewscale)

# NHSRplotthedots (missing labels)

# NIMAA (plotly)

# OBIC (patchwork)

# OmicNavigator (plotly)

# oncomsm (patchwork)

# pafr (missing labels)

# patchwork (patchwork)

# pathviewr (missing labels)

# pcutils (patchwork)

# pdxTrees (gganimate)

# personalized (plotly)

# phylepic (ggnewscale)

# Plasmidprofiler (plotly)

# platetools (faulty tests)

# plotDK (missing labels)

# plotly (plotly)

# pmartR (unknown)

```
# <details>
# 
# * Version: 2.4.5
# * GitHub: https://github.com/pmartR/pmartR
# * Source code: https://github.com/cran/pmartR
# * Date/Publication: 2024-05-21 15:50:02 UTC
# * Number of recursive dependencies: 149
# 
# Run `revdepcheck::cloud_details(, "pmartR")` for more info
# 
# </details>
# 
# ## Newly broken
# 
# *   checking tests ... ERROR
#     ```
#       Running ‘testthat.R’
#     Running the tests in ‘tests/testthat.R’ failed.
#     Complete output:
#       > library(testthat)
#       > library(pmartR)
#       > 
#       > test_check("pmartR")
#       [ FAIL 1 | WARN 1 | SKIP 11 | PASS 2375 ]
#       
#       ══ Skipped tests (11) ══════════════════════════════════════════════════════════
#     ...
#       • plots/plot-spansres-color-high-color-low.svg
#       • plots/plot-spansres.svg
#       • plots/plot-statres-anova-volcano.svg
#       • plots/plot-statres-anova.svg
#       • plots/plot-statres-combined-volcano.svg
#       • plots/plot-statres-combined.svg
#       • plots/plot-statres-gtest.svg
#       • plots/plot-totalcountfilt.svg
#       Error: Test failures
#       Execution halted
#     ```
# 
# ## In both
# 
# *   checking installed package size ... NOTE
#     ```
#       installed size is 10.4Mb
#       sub-directories of 1Mb or more:
#         R      1.5Mb
#         help   1.5Mb
#         libs   6.3Mb
#     ```
```

# pmxTools (ggdist)

# posterior (ggdist)

# PPQplan (plotly)

# ppseq (plotly)

# precrec (patchwork)

# priorsense (ggdist)

# ProAE (ggnewscale)

# probably (missing labels)

# processmapR (plotly)

# psborrow (missing labels)

# r2dii.plot (missing labels)

# Radviz (accessing defaults)

# rassta (plotly)

# REddyProc (false positive)

# redist (patchwork)

# reReg (length 0 width)

# reservr (patchwork)

# rKOMICS (unknown)

```
# <details>
# 
# * Version: 1.3
# * GitHub: NA
# * Source code: https://github.com/cran/rKOMICS
# * Date/Publication: 2023-06-29 22:40:03 UTC
# * Number of recursive dependencies: 128
# 
# Run `revdepcheck::cloud_details(, "rKOMICS")` for more info
# 
# </details>
# 
# ## Newly broken
# 
# *   checking examples ... ERROR
#     ```
#     Running examples in ‘rKOMICS-Ex.R’ failed
#     The error most likely occurred in:
#     
#     > ### Name: msc.pca
#     > ### Title: Prinicple Component Analysis based on MSC
#     > ### Aliases: msc.pca
#     > 
#     > ### ** Examples
#     > 
#     > data(matrices)
#     ...
#      11.       │ └─base::withCallingHandlers(...)
#      12.       └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
#      13.         └─l$compute_geom_2(d, theme = plot$theme)
#      14.           └─ggplot2 (local) compute_geom_2(..., self = self)
#      15.             └─self$geom$use_defaults(...)
#      16.               └─ggplot2 (local) use_defaults(..., self = self)
#      17.                 └─ggplot2:::check_aesthetics(new_params, nrow(data))
#      18.                   └─cli::cli_abort(...)
#      19.                     └─rlang::abort(...)
#     Execution halted
#     ```
# 
# ## In both
# 
# *   checking installed package size ... NOTE
#     ```
#       installed size is 24.8Mb
#       sub-directories of 1Mb or more:
#         extdata  24.0Mb
#     ```
# 
# *   checking re-building of vignette outputs ... NOTE
#     ```
#     Error(s) in re-building vignettes:
#     --- re-building ‘example.Rnw’ using Sweave
#     Loading required package: viridisLite
#     Warning: Removed 95 rows containing non-finite outside the scale range
#     (`stat_boxplot()`).
#     Warning: Removed 89 rows containing non-finite outside the scale range
#     (`stat_boxplot()`).
#     Warning: Removed 149 rows containing non-finite outside the scale range
#     (`stat_boxplot()`).
#     Warning: Removed 286 rows containing non-finite outside the scale range
#     ...
#     l.5 \usepackage
#                    {xcolor}^^M
#     !  ==> Fatal error occurred, no output PDF file produced!
#     --- failed re-building ‘example.Rnw’
#     
#     SUMMARY: processing the following file failed:
#       ‘example.Rnw’
#     
#     Error: Vignette re-building failed.
#     Execution halted
#     ```
```

# RKorAPClient (missing labels)

# RNAseqQC (patchwork)

# roahd (plotly)

# romic (plotly)

# roptions (plotly)

# santaR (plot slots)

# scdtb (missing labels)

# scoringutils (ggdist)

# scUtils (missing labels)

# SCVA (plotly)

# SDMtune (missing labels)

# SeaVal (plotly)

# sgsR (missing labels)

# SHAPforxgboost (ggforce)

# SHELF (unknown)

```
# <details>
# 
# * Version: 1.10.0
# * GitHub: https://github.com/OakleyJ/SHELF
# * Source code: https://github.com/cran/SHELF
# * Date/Publication: 2024-05-07 14:20:03 UTC
# * Number of recursive dependencies: 126
# 
# Run `revdepcheck::cloud_details(, "SHELF")` for more info
# 
# </details>
# 
# ## Newly broken
# 
# *   checking re-building of vignette outputs ... NOTE
#     ```
#     Error(s) in re-building vignettes:
#     --- re-building ‘Dirichlet-elicitation.Rmd’ using rmarkdown
#     ```
```

# shinipsum (plot slots)

# SimNPH (missing labels)

# smallsets (patchwork)

# spbal (empty sf)

# spinifex (plotly)

# sport (missing labels)

# SqueakR (false positive)

# statgenGWAS (device issue)

# surveyexplorer (ggupset)

# Sysrecon (patchwork)

# tabledown (plotly)

# TCIU (plotly)

# tensorEVD (ggnewscale)

# thematic (thematic)

# tidybayes (ggdist)

# tidycat (ggforce)

# tidyCDISC (plotly)

# tidydr (uses internals)

# tidysdm (patchwork)

# tidytreatment (ggdist)

# timetk (plotly)

# tinyarray (patchwork)

# tornado (length 0 width)

# TOSTER (ggdist)

# TreatmentPatterns (plotly)

# trelliscopejs (plotly)

# tricolore (ggtern)

# triptych (patchwork)

# tsnet (ggdist)

# umiAnalyzer (plotly)

# valr (missing labels)

# vivaldi (plotly)

# vivid (ggnewscale)

# vvshiny (plotly)

# wilson (plotly)

# xaringanthemer (default access)

# yamlet (missing labels)

