# actxps

<details>

* Version: 1.6.0
* GitHub: https://github.com/mattheaphy/actxps
* Source code: https://github.com/cran/actxps
* Date/Publication: 2025-01-07 13:00:02 UTC
* Number of recursive dependencies: 130

Run `revdepcheck::cloud_details(, "actxps")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘actxps.Rmd’ using rmarkdown
    Warning: thematic was unable to resolve `bg='auto'`. Try providing an actual color (or `NA`) to the `bg` argument of `thematic_on()`. By the way, 'auto' is only officially supported in `shiny::renderPlot()`, some rmarkdown scenarios (specifically, `html_document()` with `theme!=NULL`), in RStudio, or if `auto_config_set()` is used.
    Warning: thematic was unable to resolve `fg='auto'`. Try providing an actual color (or `NA`) to the `fg` argument of `thematic_on()`. By the way, 'auto' is only officially supported in `shiny::renderPlot()`, some rmarkdown scenarios (specifically, `html_document()` with `theme!=NULL`), in RStudio, or if `auto_config_set()` is used.
    Warning: thematic was unable to resolve `accent='auto'`. Try providing an actual color (or `NA`) to the `accent` argument of `thematic_on()`. By the way, 'auto' is only officially supported in `shiny::renderPlot()`, some rmarkdown scenarios (specifically, `html_document()` with `theme!=NULL`), in RStudio, or if `auto_config_set()` is used.
    
    Quitting from actxps.Rmd:128-130 [plot]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `adjust_color()`:
    ...
    
    Error: processing vignette 'transactions.Rmd' failed with diagnostics:
    Internal error: adjust_color() expects an input of length 1
    --- failed re-building ‘transactions.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘actxps.Rmd’ ‘misc.Rmd’ ‘transactions.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# adklakedata

<details>

* Version: 0.6.1
* GitHub: https://github.com/lawinslow/adklakedata
* Source code: https://github.com/cran/adklakedata
* Date/Publication: 2018-02-16 19:08:16 UTC
* Number of recursive dependencies: 62

Run `revdepcheck::cloud_details(, "adklakedata")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘adklakedata-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: adk_lake_shapes
    > ### Title: Return path to Lake Polygons Shapefile
    > ### Aliases: adk_lake_shapes
    > 
    > ### ** Examples
    > 
    > library(sf)
    Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
    > bl = read_sf(adklakedata::adk_shape())
    Error in process_cpl_read_ogr(x, quiet, check_ring_dir = check_ring_dir,  : 
      package tibble not available: install first?
    Calls: read_sf ... st_read -> st_read.character -> process_cpl_read_ogr
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# adw

<details>

* Version: 0.4.0
* GitHub: https://github.com/PanfengZhang/adw
* Source code: https://github.com/cran/adw
* Date/Publication: 2024-04-15 19:10:16 UTC
* Number of recursive dependencies: 60

Run `revdepcheck::cloud_details(, "adw")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘adw-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: adw
    > ### Title: Angular Distance Weighting Interpolation.
    > ### Aliases: adw
    > 
    > ### ** Examples
    > 
    > set.seed(2)
    ...
    4 111.75 31.25 -0.47570806
    5 112.25 31.25 -1.57521787
    6 112.75 31.25 -1.10541492
    > 
    > # example 2
    > hmap <- cnmap::getMap(code = "410000") |> sf::st_make_valid() # return a 'sf' object.
    Error in process_cpl_read_ogr(x, quiet, check_ring_dir = check_ring_dir,  : 
      package tibble not available: install first?
    Calls: <Anonymous> ... st_read -> st_read.character -> process_cpl_read_ogr
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Introduction.Rmd’ using rmarkdown
    
    Quitting from Introduction.Rmd:47-74 [unnamed-chunk-3]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `process_cpl_read_ogr()`:
    ! package tibble not available: install first?
    ---
    Backtrace:
    ...
    
    Error: processing vignette 'Introduction.Rmd' failed with diagnostics:
    package tibble not available: install first?
    --- failed re-building ‘Introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# AeRobiology

<details>

* Version: 2.0.1
* GitHub: NA
* Source code: https://github.com/cran/AeRobiology
* Date/Publication: 2019-06-03 06:20:03 UTC
* Number of recursive dependencies: 95

Run `revdepcheck::cloud_details(, "AeRobiology")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘my-vignette.Rmd’ using rmarkdown
    ```

# afex

<details>

* Version: 1.4-1
* GitHub: https://github.com/singmann/afex
* Source code: https://github.com/cran/afex
* Date/Publication: 2024-09-01 16:10:02 UTC
* Number of recursive dependencies: 238

Run `revdepcheck::cloud_details(, "afex")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘afex_analysing_accuracy_data.Rmd’ using rmarkdown
    ```

# AgroR

<details>

* Version: 1.3.6
* GitHub: NA
* Source code: https://github.com/cran/AgroR
* Date/Publication: 2024-04-24 02:20:18 UTC
* Number of recursive dependencies: 118

Run `revdepcheck::cloud_details(, "AgroR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘AgroR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: logistic
    > ### Title: Analysis: Logistic regression
    > ### Aliases: logistic
    > 
    > ### ** Examples
    > 
    > data("emerg")
    ...
     27. └─vctrs (local) `<fn>`()
     28.   └─vctrs::vec_default_ptype2(...)
     29.     ├─base::withRestarts(...)
     30.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     31.     │   └─base (local) doWithOneRestart(return(expr), restart)
     32.     └─vctrs::stop_incompatible_type(...)
     33.       └─vctrs:::stop_incompatible(...)
     34.         └─vctrs:::stop_vctrs(...)
     35.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

# alookr

<details>

* Version: 0.3.9
* GitHub: https://github.com/choonghyunryu/alookr
* Source code: https://github.com/cran/alookr
* Date/Publication: 2024-02-11 07:30:02 UTC
* Number of recursive dependencies: 169

Run `revdepcheck::cloud_details(, "alookr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘alookr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: compare_plot
    > ### Title: Comparison plot of train set and test set
    > ### Aliases: compare_plot
    > 
    > ### ** Examples
    > 
    > library(dplyr)
    ...
    > sb %>%
    +   compare_plot()
    Warning: `unite_()` was deprecated in tidyr 1.2.0.
    ℹ Please use `unite()` instead.
    ℹ The deprecated feature was likely used in the ggmosaic package.
      Please report the issue at <https://github.com/haleyjeppson/ggmosaic>.
    Error in make_title(..., self = self) : 
      unused arguments ("dataset class", "x")
    Calls: %>% ... labels -> <Anonymous> -> resolve_label -> <Anonymous>
    Execution halted
    ```

# AmpliconDuo

<details>

* Version: 1.1.1
* GitHub: NA
* Source code: https://github.com/cran/AmpliconDuo
* Date/Publication: 2020-05-25 22:20:02 UTC
* Number of recursive dependencies: 27

Run `revdepcheck::cloud_details(, "AmpliconDuo")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘AmpliconDuo-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: AmpliconDuo-package
    > ### Title: Statistical Analysis Of Amplicon Data Of The Same Sample To
    > ###   Identify Spurious Amplicons
    > ### Aliases: AmpliconDuo-package AmpliconDuo
    > ### Keywords: package
    > 
    > ### ** Examples
    ...
      3.     └─ggplot2:::scale_backward_compatibility(...)
      4.       ├─rlang::exec(scale, !!!args)
      5.       └─ggplot2 (local) `<fn>`(...)
      6.         ├─ggplot2::discrete_scale(...)
      7.         │ └─ggplot2::ggproto(...)
      8.         │   └─rlang::list2(...)
      9.         └─ggplot2:::pal_qualitative(type = type)
     10.           └─ggplot2:::stop_input_type(type, "a character vector or list of character vectors")
     11.             └─rlang::abort(message, ..., call = call, arg = arg)
    Execution halted
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) AmpliconDuo-package.Rd:37-39: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) AmpliconDuo-package.Rd:40-42: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) AmpliconDuo-package.Rd:43-46: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) AmpliconDuo-package.Rd:47-49: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) AmpliconDuo-package.Rd:50-52: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) AmpliconDuo-package.Rd:53-55: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) AmpliconDuo-package.Rd:56-58: Lost braces in \itemize; meant \describe ?
    ```

# AnalysisLin

<details>

* Version: 0.1.2
* GitHub: NA
* Source code: https://github.com/cran/AnalysisLin
* Date/Publication: 2024-01-30 00:10:10 UTC
* Number of recursive dependencies: 119

Run `revdepcheck::cloud_details(, "AnalysisLin")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘AnalysisLin-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: bar_plot
    > ### Title: Bar Plots for Categorical Variables
    > ### Aliases: bar_plot
    > 
    > ### ** Examples
    > 
    > data(iris)
    > bar_plot(iris)
    Error in pm[[2]] : subscript out of bounds
    Calls: bar_plot ... plotly_build -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# animbook

<details>

* Version: 1.0.0
* GitHub: https://github.com/KrisanatA/animbook
* Source code: https://github.com/cran/animbook
* Date/Publication: 2023-12-05 17:50:07 UTC
* Number of recursive dependencies: 86

Run `revdepcheck::cloud_details(, "animbook")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘animbook-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: anim_animate
    > ### Title: Modified the ggplot object
    > ### Aliases: anim_animate
    > 
    > ### ** Examples
    > 
    > animbook <- anim_prep(data = osiris, id = ID, values = sales, time = year, group = japan)
    ...
              transform it into an animated object
    > 
    > animate <- anim_animate(plot)
    You can now pass it to gganimate::animate().
                       The recommended setting is nframes = 89
    > 
    > plotly::ggplotly(animate)
    Error in pm[[2]] : subscript out of bounds
    Calls: <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# ANN2

<details>

* Version: 2.3.4
* GitHub: https://github.com/bflammers/ANN2
* Source code: https://github.com/cran/ANN2
* Date/Publication: 2020-12-01 10:00:02 UTC
* Number of recursive dependencies: 49

Run `revdepcheck::cloud_details(, "ANN2")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ANN2)
      > 
      > # Only test if not on mac
      > if (tolower(Sys.info()[["sysname"]]) != "darwin") {
      +   test_check("ANN2")
      + }
    ...
      ── Failure ('test-plotting.R:59:3'): the reconstruction_plot.ANN() function works correctly ──
      p_AE$labels$colour not equal to "col".
      target is NULL, current is character
      ── Failure ('test-plotting.R:77:3'): the compression_plot.ANN() function works correctly ──
      p_AE$labels$colour not equal to "col".
      target is NULL, current is character
      
      [ FAIL 5 | WARN 2 | SKIP 4 | PASS 143 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking C++ specification ... NOTE
    ```
      Specified C++11: please drop specification unless essential
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 48.9Mb
      sub-directories of 1Mb or more:
        cereal   1.4Mb
        libs    47.4Mb
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# APackOfTheClones

<details>

* Version: 1.2.4
* GitHub: https://github.com/Qile0317/APackOfTheClones
* Source code: https://github.com/cran/APackOfTheClones
* Date/Publication: 2024-11-18 22:30:02 UTC
* Number of recursive dependencies: 175

Run `revdepcheck::cloud_details(, "APackOfTheClones")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘APackOfTheClones-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: APOTCPlot
    > ### Title: Various variations of visualizations of clonal expansion
    > ###   post-RunAPOTC
    > ### Aliases: APOTCPlot
    > 
    > ### ** Examples
    > 
    ...
    by .GlobalEnv when processing object ‘combined_pbmc’
    Warning: namespace ‘colorspace’ is not available and has been replaced
    by .GlobalEnv when processing object ‘combined_pbmc’
    > 
    > combined_pbmc <- RunAPOTC(
    +     combined_pbmc, run_id = "run1", verbose = FALSE
    + )
    > 
    > # plotting with default arguments will plot the latest "run1"
    > clonal_packing_plot <- APOTCPlot(combined_pbmc)
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/tests.html
    ...
       17. │                     └─vctrs:::stop_names(...)
       18. │                       └─vctrs:::stop_vctrs(...)
       19. │                         └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
       20. ├─APackOfTheClones:::name_latest_legend_layer(.)
       21. │ └─plt %>% name_latest_layer(.ApotcLegendLayerName)
       22. └─APackOfTheClones:::name_latest_layer(., .ApotcLegendLayerName)
      
      [ FAIL 5 | WARN 57 | SKIP 10 | PASS 239 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘APackOfTheClones-install.Rmd’ using rmarkdown
    --- finished re-building ‘APackOfTheClones-install.Rmd’
    
    --- re-building ‘APackOfTheClones-runs.Rmd’ using rmarkdown
    ```

## In both

*   checking contents of ‘data’ directory ... WARNING
    ```
    Output for data("combined_pbmc", package = "APackOfTheClones"):
      Warning: namespace ‘colorspace’ is not available and has been replaced
      by .GlobalEnv when processing object ‘combined_pbmc’
      Warning: namespace ‘colorspace’ is not available and has been replaced
      by .GlobalEnv when processing object ‘combined_pbmc’
      Warning: namespace ‘colorspace’ is not available and has been replaced
      by .GlobalEnv when processing object ‘combined_pbmc’
      Warning: namespace ‘colorspace’ is not available and has been replaced
      by .GlobalEnv when processing object ‘combined_pbmc’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.6Mb
      sub-directories of 1Mb or more:
        help   1.1Mb
        libs   5.3Mb
    ```

# APCtools

<details>

* Version: 1.0.4
* GitHub: https://github.com/bauer-alex/APCtools
* Source code: https://github.com/cran/APCtools
* Date/Publication: 2023-01-13 23:30:02 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::cloud_details(, "APCtools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘APCtools-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_densityMatrix
    > ### Title: Create a matrix of density plots
    > ### Aliases: plot_densityMatrix
    > 
    > ### ** Examples
    > 
    > library(APCtools)
    ...
      2.2.0 and is now defunct.
    ℹ Please use the `rows` argument instead.
    Backtrace:
        ▆
     1. └─APCtools::plot_densityMatrix(...)
     2.   └─ggplot2::facet_grid(facets = facet_formula, switch = "y")
     3.     └─lifecycle::deprecate_stop("2.2.0", "facet_grid(facets)", "facet_grid(rows)")
     4.       └─lifecycle:::deprecate_stop0(msg)
     5.         └─rlang::cnd_signal(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(APCtools)
      > 
      > test_check("APCtools")
      Loading required package: nlme
      This is mgcv 1.9-1. For overview type 'help("mgcv-package")'.
      Excluding 9565 missing observations of mainTrip_distance...
    ...
          ▆
       1. └─APCtools::plot_densityMatrix(...) at test-plots_descriptive.R:148:3
       2.   └─ggplot2::facet_grid(facets = facet_formula, switch = "y")
       3.     └─lifecycle::deprecate_stop("2.2.0", "facet_grid(facets)", "facet_grid(rows)")
       4.       └─lifecycle:::deprecate_stop0(msg)
       5.         └─rlang::cnd_signal(...)
      
      [ FAIL 3 | WARN 39 | SKIP 0 | PASS 69 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘main_functionality.Rmd’ using rmarkdown
    ```

# applicable

<details>

* Version: 0.1.1
* GitHub: https://github.com/tidymodels/applicable
* Source code: https://github.com/cran/applicable
* Date/Publication: 2024-04-25 00:00:04 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::cloud_details(, "applicable")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(applicable)
      Loading required package: ggplot2
      > 
      > test_check("applicable")
      Loading required package: dplyr
    ...
      `expected` is a character vector ('percentile')
      ── Failure ('test-plot.R:36:3'): output of autoplot.apd_pca is correct when options=distance are provided ──
      ad_plot$labels$y (`actual`) not equal to "percentile" (`expected`).
      
      `actual` is NULL
      `expected` is a character vector ('percentile')
      
      [ FAIL 3 | WARN 0 | SKIP 22 | PASS 90 ]
      Error: Test failures
      Execution halted
    ```

# arulesViz

<details>

* Version: 1.5.3
* GitHub: https://github.com/mhahsler/arulesViz
* Source code: https://github.com/cran/arulesViz
* Date/Publication: 2024-04-26 09:20:02 UTC
* Number of recursive dependencies: 123

Run `revdepcheck::cloud_details(, "arulesViz")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘arulesViz-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_arulesViz
    > ### Title: Visualize Association Rules and Itemsets
    > ### Aliases: plot_arulesViz plot.rules plot plot.itemsets
    > ###   plot.grouped_matrix plotly guide_edge_colourbar
    > ### Keywords: hplot
    > 
    > ### ** Examples
    ...
     23.   └─vctrs::vec_default_cast(...)
     24.     ├─base::withRestarts(...)
     25.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     26.     │   └─base (local) doWithOneRestart(return(expr), restart)
     27.     └─vctrs::stop_incompatible_cast(...)
     28.       └─vctrs::stop_incompatible_type(...)
     29.         └─vctrs:::stop_incompatible(...)
     30.           └─vctrs:::stop_vctrs(...)
     31.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘arulesViz.Rnw’ using Sweave
    Loading required package: arules
    Loading required package: Matrix
    
    Attaching package: ‘arules’
    
    The following objects are masked from ‘package:base’:
    
    ...
    To reduce overplotting, jitter is added! Use jitter = 0 to prevent jitter.
    Error: processing vignette 'arulesViz.Rnw' failed with diagnostics:
    Can't convert <double> to <character>.
    --- failed re-building ‘arulesViz.Rnw’
    
    SUMMARY: processing the following file failed:
      ‘arulesViz.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# autocogs

<details>

* Version: 0.1.4
* GitHub: https://github.com/schloerke/autocogs
* Source code: https://github.com/cran/autocogs
* Date/Publication: 2021-05-29 17:00:05 UTC
* Number of recursive dependencies: 71

Run `revdepcheck::cloud_details(, "autocogs")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(autocogs)
      > 
      > test_check("autocogs")
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 223 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
        8.         └─autocogs (local) FUN(X[[i]], ...)
        9.           └─base::lapply(...)
       10.             └─autocogs (local) FUN(X[[i]], ...)
       11.               ├─base::do.call(fn, args)
       12.               └─autocogs (local) `<fn>`(...)
       13.                 └─base::do.call(loess, c(core_params, params$method.args))
      
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 223 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘MASS’ ‘broom’ ‘diptest’ ‘ggplot2’ ‘hexbin’ ‘moments’
      All declared Imports should be used.
    ```

# autoplotly

<details>

* Version: 0.1.4
* GitHub: https://github.com/terrytangyuan/autoplotly
* Source code: https://github.com/cran/autoplotly
* Date/Publication: 2021-04-18 06:50:11 UTC
* Number of recursive dependencies: 86

Run `revdepcheck::cloud_details(, "autoplotly")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘autoplotly-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: autoplotly
    > ### Title: Automatic Visualization of Popular Statistical Results Using
    > ###   'plotly.js' and 'ggplot2'
    > ### Aliases: autoplotly
    > 
    > ### ** Examples
    > 
    ...
    > p <- autoplotly(prcomp(iris[c(1, 2, 3, 4)]), data = iris,
    +                 colour = 'Species', label = TRUE, label.size = 3, frame = TRUE)
    Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
    ℹ Please use tidy evaluation idioms with `aes()`.
    ℹ See also `vignette("ggplot2-in-packages")` for more information.
    ℹ The deprecated feature was likely used in the ggfortify package.
      Please report the issue at <https://github.com/sinhrks/ggfortify/issues>.
    Error in pm[[2]] : subscript out of bounds
    Calls: autoplotly ... autoplotly.default -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(autoplotly)
      > 
      > test_check("autoplotly")
      [ FAIL 3 | WARN 1 | SKIP 0 | PASS 1 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
          ▆
       1. ├─autoplotly::autoplotly(...) at test_all.R:26:3
       2. └─autoplotly:::autoplotly.default(...)
       3.   ├─plotly::ggplotly(...)
       4.   └─plotly:::ggplotly.ggplot(...)
       5.     └─plotly::gg2list(...)
      
      [ FAIL 3 | WARN 1 | SKIP 0 | PASS 1 ]
      Error: Test failures
      Execution halted
    ```

# bakeoff

<details>

* Version: 0.2.0
* GitHub: https://github.com/apreshill/bakeoff
* Source code: https://github.com/cran/bakeoff
* Date/Publication: 2022-10-21 15:25:10 UTC
* Number of recursive dependencies: 31

Run `revdepcheck::cloud_details(, "bakeoff")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘bakeoff-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: bakeoff_palette
    > ### Title: A *bakeoff* palette generator
    > ### Aliases: bakeoff_palette
    > 
    > ### ** Examples
    > 
    > bakeoff_palette("showstopper")
    ...
     18. └─vctrs (local) `<fn>`()
     19.   └─vctrs::vec_default_ptype2(...)
     20.     ├─base::withRestarts(...)
     21.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     22.     │   └─base (local) doWithOneRestart(return(expr), restart)
     23.     └─vctrs::stop_incompatible_type(...)
     24.       └─vctrs:::stop_incompatible(...)
     25.         └─vctrs:::stop_vctrs(...)
     26.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 260 marked UTF-8 strings
    ```

# bartMan

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/bartMan
* Date/Publication: 2024-07-24 12:10:02 UTC
* Number of recursive dependencies: 134

Run `revdepcheck::cloud_details(, "bartMan")` for more info

</details>

## Newly broken

*   checking whether package ‘bartMan’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/bartMan/new/bartMan.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘bartMan’ ...
** package ‘bartMan’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error in eval(exprs[i], envir) : object 'justify_grobs' not found
Error: unable to load R code in package ‘bartMan’
Execution halted
ERROR: lazy loading failed for package ‘bartMan’
* removing ‘/tmp/workdir/bartMan/new/bartMan.Rcheck/bartMan’


```
### CRAN

```
* installing *source* package ‘bartMan’ ...
** package ‘bartMan’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (bartMan)


```
# bayesAB

<details>

* Version: 1.1.3
* GitHub: https://github.com/FrankPortman/bayesAB
* Source code: https://github.com/cran/bayesAB
* Date/Publication: 2021-06-25 00:50:02 UTC
* Number of recursive dependencies: 70

Run `revdepcheck::cloud_details(, "bayesAB")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(bayesAB)
      > 
      > test_check("bayesAB")
      [ FAIL 1 | WARN 5 | SKIP 0 | PASS 140 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-dists.R:34:3'): Success ──────────────────────────────────────
      plotNormalInvGamma(3, 1, 1, 1)$labels$y not equal to "sig_sq".
      target is NULL, current is character
      
      [ FAIL 1 | WARN 5 | SKIP 0 | PASS 140 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) plotDistributions.Rd:32: Lost braces
        32 | plot{...} functions are generated programmatically so the function calls in
           |     ^
    ```

# BayesERtools

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/BayesERtools
* Date/Publication: 2025-02-12 11:40:22 UTC
* Number of recursive dependencies: 216

Run `revdepcheck::cloud_details(, "BayesERtools")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
      > # * https://testthat.r-lib.org/articles/special-files.html
    ...
      ── Failure ('test-plot_ermod.R:183:5'): plot_er show caption ───────────────────
      `plot_er(...)` produced warnings.
      ── Failure ('test-plot_ermod.R:194:3'): plot_er show caption ───────────────────
      `plot_er(...)` produced warnings.
      ── Failure ('test-plot_ermod.R:209:5'): plot_er_gof ────────────────────────────
      `plot_er_gof(ermod_bin, show_coef_exp = TRUE, show_caption = TRUE)` produced warnings.
      
      [ FAIL 8 | WARN 3 | SKIP 0 | PASS 143 ]
      Error: Test failures
      Execution halted
    ```

# BayesianReasoning

<details>

* Version: 0.4.2
* GitHub: https://github.com/gorkang/BayesianReasoning
* Source code: https://github.com/cran/BayesianReasoning
* Date/Publication: 2023-11-14 11:33:20 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "BayesianReasoning")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(BayesianReasoning)
      > 
      > test_check("BayesianReasoning")
      
       Plot created in: ./FP_10_sens_100_screening_1667_diagnostic_44.png
      
    ...
      `names(expected)` is absent
      ── Failure ('test-PPV_heatmap.R:1097:3'): Plot with line overlay ───────────────
      vapply(p$result$layers, function(x) class(x$geom)[1], "") (`actual`) not identical to c("GeomTile", "GeomSegment", "GeomPoint", "GeomMarkRect") (`expected`).
      
      `names(actual)` is a character vector ('geom_tile', 'annotate', 'annotate...3', 'geom_mark_rect')
      `names(expected)` is absent
      
      [ FAIL 8 | WARN 16 | SKIP 4 | PASS 115 ]
      Error: Test failures
      Execution halted
    ```

# BayesMallows

<details>

* Version: 2.2.3
* GitHub: https://github.com/ocbe-uio/BayesMallows
* Source code: https://github.com/cran/BayesMallows
* Date/Publication: 2025-01-14 11:30:02 UTC
* Number of recursive dependencies: 79

Run `revdepcheck::cloud_details(, "BayesMallows")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
      > # * https://testthat.r-lib.org/articles/special-files.html
    ...
      `expected` is a character vector ('interaction(chain, cluster)')
      ── Failure ('test-assess_convergence.R:217:3'): assess_convergence.BayesMallowsMixtures works ──
      p$labels$colour (`actual`) not equal to "cluster" (`expected`).
      
      `actual` is NULL
      `expected` is a character vector ('cluster')
      
      [ FAIL 10 | WARN 0 | SKIP 10 | PASS 363 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 23.8Mb
      sub-directories of 1Mb or more:
        doc    2.7Mb
        libs  20.2Mb
    ```

# BayesMultiMode

<details>

* Version: 0.7.3
* GitHub: https://github.com/paullabonne/BayesMultiMode
* Source code: https://github.com/cran/BayesMultiMode
* Date/Publication: 2024-10-31 15:30:06 UTC
* Number of recursive dependencies: 105

Run `revdepcheck::cloud_details(, "BayesMultiMode")` for more info

</details>

## Newly broken

*   checking whether package ‘BayesMultiMode’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
    See ‘/tmp/workdir/BayesMultiMode/new/BayesMultiMode.Rcheck/00install.out’ for details.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        help   6.1Mb
    ```

# bayesplot

<details>

* Version: 1.12.0
* GitHub: https://github.com/stan-dev/bayesplot
* Source code: https://github.com/cran/bayesplot
* Date/Publication: 2025-04-10 10:10:06 UTC
* Number of recursive dependencies: 127

Run `revdepcheck::cloud_details(, "bayesplot")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(bayesplot)
      This is bayesplot version 1.12.0
      - Online documentation and vignettes at mc-stan.org/bayesplot
      - bayesplot theme set to bayesplot::theme_default()
         * Does _not_ affect other ggplot2 plots
         * See ?bayesplot_theme_set for details on theme setting
    ...
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-mcmc-traces.R:59:3'): mcmc_trace options work ────────────────
      all(c("xmin", "xmax", "ymin", "ymax") %in% names(ll)) is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      
      [ FAIL 1 | WARN 4 | SKIP 74 | PASS 1032 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.2Mb
      sub-directories of 1Mb or more:
        R     4.0Mb
        doc   3.5Mb
    ```

# BCClong

<details>

* Version: 1.0.3
* GitHub: NA
* Source code: https://github.com/cran/BCClong
* Date/Publication: 2024-06-24 00:00:02 UTC
* Number of recursive dependencies: 148

Run `revdepcheck::cloud_details(, "BCClong")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘BCClong-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.BCC
    > ### Title: Generic plot method for BCC objects
    > ### Aliases: plot.BCC
    > 
    > ### ** Examples
    > 
    > # get data from the package
    ...
     22. └─vctrs (local) `<fn>`()
     23.   └─vctrs::vec_default_ptype2(...)
     24.     ├─base::withRestarts(...)
     25.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     26.     │   └─base (local) doWithOneRestart(return(expr), restart)
     27.     └─vctrs::stop_incompatible_type(...)
     28.       └─vctrs:::stop_incompatible(...)
     29.         └─vctrs:::stop_vctrs(...)
     30.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.4Mb
      sub-directories of 1Mb or more:
        data   3.3Mb
        doc    1.2Mb
        libs   6.8Mb
    ```

# BCEA

<details>

* Version: 2.4.7
* GitHub: https://github.com/n8thangreen/BCEA
* Source code: https://github.com/cran/BCEA
* Date/Publication: 2025-01-14 12:30:08 UTC
* Number of recursive dependencies: 124

Run `revdepcheck::cloud_details(, "BCEA")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘BCEA-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: eib.plot.bcea
    > ### Title: Expected Incremental Benefit (EIB) Plot
    > ### Aliases: eib.plot.bcea eib.plot
    > ### Keywords: hplot
    > 
    > ### ** Examples
    > 
    ...
     20. └─vctrs (local) `<fn>`()
     21.   └─vctrs::vec_default_ptype2(...)
     22.     ├─base::withRestarts(...)
     23.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     24.     │   └─base (local) doWithOneRestart(return(expr), restart)
     25.     └─vctrs::stop_incompatible_type(...)
     26.       └─vctrs:::stop_incompatible(...)
     27.         └─vctrs:::stop_vctrs(...)
     28.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘CEriskav.Rmd’ using rmarkdown
    ```

# biclustermd

<details>

* Version: 0.2.3
* GitHub: https://github.com/jreisner/biclustermd
* Source code: https://github.com/cran/biclustermd
* Date/Publication: 2021-06-17 15:10:06 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "biclustermd")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(biclustermd)
      Loading required package: ggplot2
      Loading required package: tidyr
      
      Attaching package: 'tidyr'
      
    ...
      ── Failure ('test-autoplot_biclustermd.R:6:3'): autoplot_biclustermd() correctly plots cluster lines ──
      ap$data[[3]]$xintercept[-1] not equal to cumsum(colSums(sbc$P)) + 0.5.
      Classes differ: 'mapped_discrete'/'numeric' is not 'numeric'
      ── Failure ('test-autoplot_biclustermd.R:7:3'): autoplot_biclustermd() correctly plots cluster lines ──
      ap$data[[4]]$yintercept[-1] not equal to cumsum(colSums(sbc$Q)) + 0.5.
      Classes differ: 'mapped_discrete'/'numeric' is not 'numeric'
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 66 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘nycflights13’
      All declared Imports should be used.
    ```

# BIGL

<details>

* Version: 1.9.3
* GitHub: https://github.com/openanalytics/BIGL
* Source code: https://github.com/cran/BIGL
* Date/Publication: 2024-08-01 08:20:02 UTC
* Number of recursive dependencies: 95

Run `revdepcheck::cloud_details(, "BIGL")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘analysis.Rmd’ using rmarkdown
    ```

# BiVariAn

<details>

* Version: 1.0.1
* GitHub: https://github.com/AndresFloresG/BiVariAn
* Source code: https://github.com/cran/BiVariAn
* Date/Publication: 2025-03-05 13:10:02 UTC
* Number of recursive dependencies: 199

Run `revdepcheck::cloud_details(, "BiVariAn")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘BiVariAn-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: auto_bar_cont
    > ### Title: Automatic barplot of continous variables
    > ### Aliases: auto_bar_cont
    > 
    > ### ** Examples
    > 
    > data <- data.frame(group = rep(letters[1:2], 30),
    ...
    Backtrace:
        ▆
     1. └─BiVariAn::auto_bar_cont(...)
     2.   └─ggplot2::scale_y_continuous(...)
     3.     └─ggplot2::continuous_scale(...)
     4.       └─ggplot2:::check_continuous_limits(limits, call = call)
     5.         └─ggplot2:::check_length(limits, 2L, arg = arg, call = call)
     6.           └─cli::cli_abort(msg, call = call, arg = arg)
     7.             └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
    ...
      ── Failure ('test-auto_viol_cont.R:13:3'): auto_violin_cont works ──────────────
      capture.output(violinplotlist$var1$layers[[1]])[1] (`actual`) not equal to "geom_violin: draw_quantiles = NULL, na.rm = FALSE, orientation = NA" (`expected`).
      
      actual vs expected
      - "geom_violin: na.rm = FALSE, orientation = NA, quantile_gp = list(colour = NULL, linetype = 0, linewidth = NULL)"
      + "geom_violin: draw_quantiles = NULL, na.rm = FALSE, orientation = NA"
      
      [ FAIL 3 | WARN 2 | SKIP 7 | PASS 119 ]
      Error: Test failures
      Execution halted
    ```

# boxly

<details>

* Version: 0.1.1
* GitHub: https://github.com/Merck/boxly
* Source code: https://github.com/cran/boxly
* Date/Publication: 2023-10-24 02:40:02 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "boxly")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/tests.html
      > # * https://testthat.r-lib.org/reference/test_package.html#special-files
    ...
       16.   ├─plotly::add_trace(...)
       17.   │ └─plotly::add_data(p, data)
       18.   │   └─plotly:::is.plotly(p)
       19.   ├─plotly::ggplotly(p, tooltip = "text", dynamicTicks = TRUE)
       20.   └─plotly:::ggplotly.ggplot(p, tooltip = "text", dynamicTicks = TRUE)
       21.     └─plotly::gg2list(...)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 25 ]
      Error: Test failures
      Execution halted
    ```

# brolgar

<details>

* Version: 1.0.1
* GitHub: https://github.com/njtierney/brolgar
* Source code: https://github.com/cran/brolgar
* Date/Publication: 2024-05-10 14:50:34 UTC
* Number of recursive dependencies: 101

Run `revdepcheck::cloud_details(, "brolgar")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘brolgar-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: facet_sample
    > ### Title: Facet data into groups to facilitate exploration
    > ### Aliases: facet_sample
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    > ggplot(heights,
    + aes(x = year,
    +     y = height_cm,
    +     group = country)) +
    +   geom_line() +
    +   facet_sample()
    Error in if (params$as.table) { : argument is of length zero
    Calls: <Anonymous> ... <Anonymous> -> setup -> <Anonymous> -> compute_layout
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘exploratory-modelling.Rmd’ using rmarkdown
    
    Quitting from exploratory-modelling.Rmd:46-56 [use-gg-highlight]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'exploratory-modelling.Rmd' failed with diagnostics:
    ℹ In index: 1.
    ℹ With name: geom_line.
    Caused by error in `aes_param_name %in% names(non_null_default_aes) && is.na(non_null_default_aes[[
        aes_param_name]])`:
    ! 'length = 2' in coercion to 'logical(1)'
    --- failed re-building ‘exploratory-modelling.Rmd’
    
    --- re-building ‘finding-features.Rmd’ using rmarkdown
    ```

# bullseye

<details>

* Version: 1.0.0
* GitHub: https://github.com/cbhurley/bullseye
* Source code: https://github.com/cran/bullseye
* Date/Publication: 2025-05-09 10:10:02 UTC
* Number of recursive dependencies: 159

Run `revdepcheck::cloud_details(, "bullseye")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘bullseye-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: add_nobs_to_pairwise
    > ### Title: Adds number of observations column to pairwise tibble
    > ### Aliases: add_nobs_to_pairwise
    > 
    > ### ** Examples
    > 
    > irisc <- pairwise_scores(iris[40:150,], by= "Species")
    ...
     17. │               └─base::lapply(...)
     18. │                 └─ggplot2 (local) FUN(X[[i]], ...)
     19. │                   ├─rlang::inject(self$draw_panel(data, panel_params, coord, !!!params))
     20. │                   └─self$draw_panel(...)
     21. └─base::.handleSimpleError(...)
     22.   └─rlang (local) h(simpleError(msg, call))
     23.     └─handlers[[1L]](cnd)
     24.       └─cli::cli_abort(...)
     25.         └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘calc_pairwise.Rmd’ using rmarkdown
    --- finished re-building ‘calc_pairwise.Rmd’
    
    --- re-building ‘integrating.Rmd’ using rmarkdown
    ```

# canvasXpress

<details>

* Version: 1.56.1
* GitHub: https://github.com/neuhausi/canvasXpress
* Source code: https://github.com/cran/canvasXpress
* Date/Publication: 2025-04-08 19:30:02 UTC
* Number of recursive dependencies: 147

Run `revdepcheck::cloud_details(, "canvasXpress")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(canvasXpress)
      > require(canvasXpress.data)
      Loading required package: canvasXpress.data
      > 
      > test_check("canvasXpress")
      Loading required package: htmlwidgets
    ...
          ▆
       1. ├─base::suppressWarnings(ggplot.as.list(gplot)) at test-other-ggplot_as_list.R:131:5
       2. │ └─base::withCallingHandlers(...)
       3. └─canvasXpress::ggplot.as.list(gplot)
       4.   └─canvasXpress:::gg_cxplot(o, "canvas", ...)
       5.     └─canvasXpress:::gg_proc_layer(o, i, bld)
      
      [ FAIL 1 | WARN 1 | SKIP 480 | PASS 97 ]
      Error: Test failures
      Execution halted
    ```

# cartograflow

<details>

* Version: 1.0.5
* GitHub: https://github.com/fbahoken/cartogRaflow
* Source code: https://github.com/cran/cartograflow
* Date/Publication: 2023-10-17 22:40:21 UTC
* Number of recursive dependencies: 99

Run `revdepcheck::cloud_details(, "cartograflow")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘cartograflow-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: flowgini
    > ### Title: Analysis of flow concentration (Gini coefficient)
    > ### Aliases: flowgini
    > 
    > ### ** Examples
    > 
    > library(cartograflow)
    ...
    ℹ Use `flowcum` instead.
    Warning: Use of `x$linkcum` is discouraged.
    ℹ Use `linkcum` instead.
    Warning: Use of `x$flowcum` is discouraged.
    ℹ Use `flowcum` instead.
    Warning: Use of `x$flowcum` is discouraged.
    ℹ Use `flowcum` instead.
    Error in pm[[2]] : subscript out of bounds
    Calls: flowgini ... %>% -> layout -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# cats

<details>

* Version: 1.0.2
* GitHub: NA
* Source code: https://github.com/cran/cats
* Date/Publication: 2022-03-11 10:20:07 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::cloud_details(, "cats")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘cats-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: trial_ocs
    > ### Title: Calculates the operating characteristics of the cohort trial
    > ### Aliases: trial_ocs
    > 
    > ### ** Examples
    > 
    > 
    ...
    + safety_prob = safety_prob, Bayes_Sup1 = Bayes_Sup1, Bayes_Sup2 = Bayes_Sup2,
    + cohort_offset = cohort_offset, sr_first_pos = sr_first_pos,
    + missing_prob = missing_prob, cohort_fixed = cohort_fixed, accrual_type = accrual_type,
    + accrual_param = accrual_param, hist_lag = hist_lag, analysis_times = analysis_times,
    + time_trend = time_trend, cohorts_start = cohorts_start, cohorts_sim = cohorts_sim,
    + iter = 2, coresnum = 1, save = FALSE, ret_list = TRUE, plot_ocs = TRUE
    + )
    Error in pm[[2]] : subscript out of bounds
    Calls: trial_ocs -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘epitools’ ‘forcats’ ‘purrr’
      All declared Imports should be used.
    ```

# centerline

<details>

* Version: 0.2.2
* GitHub: https://github.com/atsyplenkov/centerline
* Source code: https://github.com/cran/centerline
* Date/Publication: 2025-03-16 04:40:02 UTC
* Number of recursive dependencies: 109

Run `revdepcheck::cloud_details(, "centerline")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘centerline-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_cnt_text
    > ### Title: Plot label or text on centerline with ggplot2
    > ### Aliases: geom_cnt_text geom_cnt_label
    > 
    > ### ** Examples
    > 
    > ## Don't show: 
    ...
     21. │                       ├─rlang::inject(self$draw_panel(data, panel_params, coord, !!!params))
     22. │                       └─self$draw_panel(...)
     23. │                         └─geomtextpath (local) draw_panel(...)
     24. │                           └─geomtextpath:::sf_textgrob(...)
     25. └─base::.handleSimpleError(...)
     26.   └─rlang (local) h(simpleError(msg, call))
     27.     └─handlers[[1L]](cnd)
     28.       └─cli::cli_abort(...)
     29.         └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
      > # * https://testthat.r-lib.org/articles/special-files.html
    ...
        6.           └─base::lapply(...)
        7.             └─ggplot2 (local) FUN(X[[i]], ...)
        8.               ├─rlang::inject(self$draw_panel(data, panel_params, coord, !!!params))
        9.               └─self$draw_panel(...)
       10.                 └─geomtextpath (local) draw_panel(...)
       11.                   └─geomtextpath:::sf_textgrob(...)
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 245 ]
      Error: Test failures
      Execution halted
    ```

# Certara.Xpose.NLME

<details>

* Version: 2.0.2
* GitHub: NA
* Source code: https://github.com/cran/Certara.Xpose.NLME
* Date/Publication: 2025-01-28 15:50:10 UTC
* Number of recursive dependencies: 100

Run `revdepcheck::cloud_details(, "Certara.Xpose.NLME")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Certara.Xpose.NLME-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: eta_vs_cov
    > ### Title: ETAs vs covariate Plot
    > ### Aliases: eta_vs_cov
    > 
    > ### ** Examples
    > 
    > eta_vs_cov(xpose::xpdb_ex_pk,
    ...
    ℹ Please use tidy evaluation idioms with `aes()`.
    ℹ See also `vignette("ggplot2-in-packages")` for more information.
    ℹ The deprecated feature was likely used in the Certara.Xpose.NLME package.
      Please report the issue to the authors.
    `geom_smooth()` using formula = 'y ~ x'
    `geom_smooth()` using formula = 'y ~ x'
    Error in label_variable(labels, multi_line = multi_line) : 
      could not find function "label_variable"
    Calls: <Anonymous> ... list2 -> labeller -> lapply -> FUN -> .default -> x
    Execution halted
    ```

# cheem

<details>

* Version: 0.4.0.0
* GitHub: https://github.com/nspyrison/cheem
* Source code: https://github.com/cran/cheem
* Date/Publication: 2023-11-08 21:30:02 UTC
* Number of recursive dependencies: 149

Run `revdepcheck::cloud_details(, "cheem")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(cheem)
      --------------------------------------------------------
      cheem --- version 0.4.0.0
      Please share bugs, suggestions, and feature requests at:
      https://github.com/nspyrison/cheem/issues/
      --------------------------------------------------------
    ...
       13. │   ├─utils::modifyList(x %||% list(), y %||% list(), ...)
       14. │   │ └─base::stopifnot(is.list(x), is.list(val))
       15. │   └─x %||% list()
       16. ├─plotly::ggplotly(...)
       17. └─plotly:::ggplotly.ggplot(...)
       18.   └─plotly::gg2list(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 10 ]
      Error: Test failures
      Execution halted
    ```

# choroplethr

<details>

* Version: 4.0.0
* GitHub: https://github.com/eastnile/choroplethr
* Source code: https://github.com/cran/choroplethr
* Date/Publication: 2025-04-11 22:10:08 UTC
* Number of recursive dependencies: 123

Run `revdepcheck::cloud_details(, "choroplethr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘choroplethr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: country_choropleth
    > ### Title: Create a country-level choropleth
    > ### Aliases: country_choropleth
    > 
    > ### ** Examples
    > 
    > # demonstrate default options
    ...
    Backtrace:
        ▆
     1. └─choroplethr::country_choropleth(...)
     2.   └─c$render()
     3.     └─self$get_scale()
     4.       └─ggplot2::scale_fill_continuous(...)
     5.         └─ggplot2:::scale_backward_compatibility(...)
     6.           └─cli::cli_abort("Unknown scale type: {.val {scale}}")
     7.             └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 10 marked UTF-8 strings
    ```

# chronicle

<details>

* Version: 0.3
* GitHub: NA
* Source code: https://github.com/cran/chronicle
* Date/Publication: 2021-06-25 05:00:02 UTC
* Number of recursive dependencies: 143

Run `revdepcheck::cloud_details(, "chronicle")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘chronicle-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: make_barplot
    > ### Title: Create a bar plot from a data frame through ggplotly
    > ### Aliases: make_barplot
    > 
    > ### ** Examples
    > 
    > make_barplot(dt = iris, bars = 'Species', value = 'Sepal.Length')
    Error in pm[[2]] : subscript out of bounds
    Calls: make_barplot -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘chronicle.Rmd’ using rmarkdown
    
    Quitting from chronicle.Rmd:37-67 [unnamed-chunk-3]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'chronicle.Rmd' failed with diagnostics:
    ...
    ℹ In index: 1.
    Caused by error in `pm[[2]]`:
    ! subscript out of bounds
    --- failed re-building ‘chronicle.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘chronicle.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DT’ ‘dplyr’ ‘prettydoc’ ‘rmdformats’ ‘skimr’
      All declared Imports should be used.
    ```

# classmap

<details>

* Version: 1.2.4
* GitHub: NA
* Source code: https://github.com/cran/classmap
* Date/Publication: 2025-05-13 22:00:02 UTC
* Number of recursive dependencies: 133

Run `revdepcheck::cloud_details(, "classmap")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘classmap-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: vcr.forest.newdata
    > ### Title: Prepare for visualization of a random forest classification on
    > ###   new data.
    > ### Aliases: vcr.forest.newdata
    > 
    > ### ** Examples
    > 
    ...
     20. └─vctrs (local) `<fn>`()
     21.   └─vctrs::vec_default_ptype2(...)
     22.     ├─base::withRestarts(...)
     23.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     24.     │   └─base (local) doWithOneRestart(return(expr), restart)
     25.     └─vctrs::stop_incompatible_type(...)
     26.       └─vctrs:::stop_incompatible(...)
     27.         └─vctrs:::stop_vctrs(...)
     28.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 9 marked UTF-8 strings
    ```

# clinDataReview

<details>

* Version: 1.6.2
* GitHub: https://github.com/openanalytics/clinDataReview
* Source code: https://github.com/cran/clinDataReview
* Date/Publication: 2025-04-11 22:10:02 UTC
* Number of recursive dependencies: 118

Run `revdepcheck::cloud_details(, "clinDataReview")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘clinDataReview-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: scatterplotClinData
    > ### Title: Scatterplot of variables of interest for clinical data
    > ###   visualization.
    > ### Aliases: scatterplotClinData
    > 
    > ### ** Examples
    > 
    ...
    + 	data = dataPlot, 
    + 	xVar = "ADY",
    + 	yVar = "LBSTRESN",
    + 	aesPointVar = list(color = "TRTP", fill = "TRTP"),
    + 	aesLineVar = list(group = "USUBJID", color = "TRTP"),
    + 	labelVars = labelVars
    + )
    Error in pm[[2]] : subscript out of bounds
    Calls: scatterplotClinData -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(clinDataReview)
      > 
      > test_check("clinDataReview")
        adding: report.html (deflated 63%)
        adding: report_dependencies171f62574784/ (stored 0%)
        adding: report_dependencies171f62574784/file171fd3e203d.html (deflated 8%)
    ...
      Backtrace:
          ▆
       1. └─clinDataReview::scatterplotClinData(...) at test_scatterplotClinData.R:1001:3
       2.   ├─plotly::ggplotly(p = gg, width = width, height = height, tooltip = if (!is.null(hoverVars)) "text")
       3.   └─plotly:::ggplotly.ggplot(...)
       4.     └─plotly::gg2list(...)
      
      [ FAIL 31 | WARN 0 | SKIP 31 | PASS 466 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘clinDataReview-dataPreprocessing.Rmd’ using rmarkdown
    --- finished re-building ‘clinDataReview-dataPreprocessing.Rmd’
    
    --- re-building ‘clinDataReview-dataVisualization.Rmd’ using rmarkdown
    
    Quitting from clinDataReview-dataVisualization.Rmd:169-211 [timeProfiles]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `pm[[2]]`:
    ...
    --- failed re-building ‘clinDataReview-dataVisualization.Rmd’
    
    --- re-building ‘clinDataReview-reporting.Rmd’ using rmarkdown
    --- finished re-building ‘clinDataReview-reporting.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘clinDataReview-dataVisualization.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        doc   4.3Mb
    ```

# clinUtils

<details>

* Version: 0.2.0
* GitHub: https://github.com/openanalytics/clinUtils
* Source code: https://github.com/cran/clinUtils
* Date/Publication: 2024-05-17 14:50:06 UTC
* Number of recursive dependencies: 108

Run `revdepcheck::cloud_details(, "clinUtils")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘clinUtils-vignette.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.7Mb
      sub-directories of 1Mb or more:
        doc   6.5Mb
    ```

# clockSim

<details>

* Version: 0.1.2
* GitHub: https://github.com/yeyuan98/clockSim
* Source code: https://github.com/cran/clockSim
* Date/Publication: 2025-04-22 20:10:01 UTC
* Number of recursive dependencies: 79

Run `revdepcheck::cloud_details(, "clockSim")` for more info

</details>

## Newly broken

*   checking whether package ‘clockSim’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
    See ‘/tmp/workdir/clockSim/new/clockSim.Rcheck/00install.out’ for details.
    ```

# clustcurv

<details>

* Version: 2.0.2
* GitHub: https://github.com/noramvillanueva/clustcurv
* Source code: https://github.com/cran/clustcurv
* Date/Publication: 2024-10-25 08:20:07 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::cloud_details(, "clustcurv")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘clustcurv.Rmd’ using rmarkdown
    
    Quitting from clustcurv.Rmd:92-94 [unnamed-chunk-5]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `pm[[2]]`:
    ! subscript out of bounds
    ---
    Backtrace:
    ...
    
    Error: processing vignette 'clustcurv.Rmd' failed with diagnostics:
    subscript out of bounds
    --- failed re-building ‘clustcurv.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘clustcurv.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# clustTMB

<details>

* Version: 0.1.0
* GitHub: https://github.com/Andrea-Havron/clustTMB
* Source code: https://github.com/cran/clustTMB
* Date/Publication: 2024-10-14 11:50:46 UTC
* Number of recursive dependencies: 153

Run `revdepcheck::cloud_details(, "clustTMB")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘CovarianceStructure.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 82.4Mb
      sub-directories of 1Mb or more:
        libs  81.9Mb
    ```

# cnmap

<details>

* Version: 0.1.0
* GitHub: https://github.com/PanfengZhang/cnmap
* Source code: https://github.com/cran/cnmap
* Date/Publication: 2024-04-02 12:42:06 UTC
* Number of recursive dependencies: 59

Run `revdepcheck::cloud_details(, "cnmap")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘cnmap-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: getMap
    > ### Title: China Map Data from AutoNavi Map
    > ### Aliases: getMap
    > 
    > ### ** Examples
    > 
    > library(cnmap)
    > 
    > map1 <- getMap(code = "110000") # get the map data of Beijing City
    Error in process_cpl_read_ogr(x, quiet, check_ring_dir = check_ring_dir,  : 
      package tibble not available: install first?
    Calls: getMap ... st_read -> st_read.character -> process_cpl_read_ogr
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Introduction.Rmd’ using rmarkdown
    
    Quitting from Introduction.Rmd:28-35 [unnamed-chunk-3]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `process_cpl_read_ogr()`:
    ! package tibble not available: install first?
    ---
    Backtrace:
    ...
    
    Error: processing vignette 'Introduction.Rmd' failed with diagnostics:
    package tibble not available: install first?
    --- failed re-building ‘Introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# cocktailApp

<details>

* Version: 0.2.3
* GitHub: https://github.com/shabbychef/cocktailApp
* Source code: https://github.com/cran/cocktailApp
* Date/Publication: 2023-07-19 13:40:09 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::cloud_details(, "cocktailApp")` for more info

</details>

## Newly broken

*   checking whether package ‘cocktailApp’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: The `size` argument of `element_rect()` is deprecated as of ggplot2 3.4.0.
    See ‘/tmp/workdir/cocktailApp/new/cocktailApp.Rcheck/00install.out’ for details.
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 14729 marked UTF-8 strings
    ```

# coda.plot

<details>

* Version: 0.1.9
* GitHub: NA
* Source code: https://github.com/cran/coda.plot
* Date/Publication: 2025-04-20 21:30:02 UTC
* Number of recursive dependencies: 42

Run `revdepcheck::cloud_details(, "coda.plot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘coda.plot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: balance_dendrogram
    > ### Title: Compositional Balance Dendrogram
    > ### Aliases: balance_dendrogram
    > 
    > ### ** Examples
    > 
    > # Simulated compositional data and balances
    > X = matrix(runif(50, 1, 10), ncol = 5)
    > colnames(X) = LETTERS[1:5]
    > B = pb_basis(X, method = 'exact')
    > balance_dendrogram(X, B)
    Error in validate_theme(theme) : could not find function "validate_theme"
    Calls: <Anonymous> ... ggplot_gtable -> ggplot_gtable.ggplot_built -> <Anonymous>
    Execution halted
    ```

*   checking whether package ‘coda.plot’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: The `size` argument of `element_rect()` is deprecated as of ggplot2 3.4.0.
    See ‘/tmp/workdir/coda.plot/new/coda.plot.Rcheck/00install.out’ for details.
    ```

# cogmapr

<details>

* Version: 0.9.3
* GitHub: NA
* Source code: https://github.com/cran/cogmapr
* Date/Publication: 2022-01-04 15:40:07 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::cloud_details(, "cogmapr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘cogmapr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggCMap
    > ### Title: Plot a social cognitive map using ggplot2
    > ### Aliases: ggCMap
    > 
    > ### ** Examples
    > 
    > project_name <- "a_new_project"
    ...
      3.     └─ggplot2:::scale_backward_compatibility(...)
      4.       ├─rlang::exec(scale, !!!args)
      5.       └─ggplot2 (local) `<fn>`(...)
      6.         ├─ggplot2::discrete_scale(...)
      7.         │ └─ggplot2::ggproto(...)
      8.         │   └─rlang::list2(...)
      9.         └─ggplot2:::pal_qualitative(type = type)
     10.           └─ggplot2:::stop_input_type(type, "a character vector or list of character vectors")
     11.             └─rlang::abort(message, ..., call = call, arg = arg)
    Execution halted
    ```

# CohortPlat

<details>

* Version: 1.0.5
* GitHub: NA
* Source code: https://github.com/cran/CohortPlat
* Date/Publication: 2022-02-14 09:30:02 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "CohortPlat")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘CohortPlat-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_trial
    > ### Title: Plots the cohort trial study overview given stage data.
    > ### Aliases: plot_trial
    > 
    > ### ** Examples
    > 
    > 
    ...
    + stage_data = stage_data, cohort_random = cohort_random, cohorts_max = cohorts_max,
    + sr_drugs_pos = sr_drugs_pos, target_rr = target_rr, sharing_type = sharing_type,
    + safety_prob = safety_prob, Bayes_Sup = Bayes_Sup, prob_rr_transform = prob_rr_transform,
    + cohort_offset = cohort_offset, Bayes_Fut = Bayes_Fut, sr_first_pos = sr_first_pos
    + )
    > 
    > plot_trial(res_list, unit = "n")
    Error in pm[[2]] : subscript out of bounds
    Calls: plot_trial -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘my-vignette.Rmd’ using rmarkdown
    
    Quitting from my-vignette.Rmd:1042-1073 [unnamed-chunk-20]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `pm[[2]]`:
    ! subscript out of bounds
    ---
    Backtrace:
    ...
    
    Error: processing vignette 'my-vignette.Rmd' failed with diagnostics:
    subscript out of bounds
    --- failed re-building ‘my-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘my-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# colorrepel

<details>

* Version: 0.4.1
* GitHub: https://github.com/raysinensis/color_repel
* Source code: https://github.com/cran/colorrepel
* Date/Publication: 2025-01-19 04:50:02 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "colorrepel")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘colorrepel-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggplotly_background
    > ### Title: Prepare ggplot object to ggplotly-compatible layer and image
    > ###   layer
    > ### Aliases: ggplotly_background
    > 
    > ### ** Examples
    > 
    > a <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy)) +
    +   ggplot2::geom_point(ggplot2::aes(color = as.factor(cyl)))
    > new_colors <- color_repel(a)
    > b <- ggplotly_background(a, filename = NULL)
    Error in pm[[2]] : subscript out of bounds
    Calls: ggplotly_background -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# contsurvplot

<details>

* Version: 0.2.1
* GitHub: https://github.com/RobinDenz1/contsurvplot
* Source code: https://github.com/cran/contsurvplot
* Date/Publication: 2023-08-15 08:00:03 UTC
* Number of recursive dependencies: 157

Run `revdepcheck::cloud_details(, "contsurvplot")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(contsurvplot)
      Please cite as: 
      
      Denz R, Timmesfeld N (2023). "Visualizing the (Causal) Effect of a Continuous Variable on a Time-To-Event Outcome." Epidemiology, 34 (5). doi: 10.1097/EDE.0000000000001630.
      > library(survival)
      > library(testthat)
      > 
    ...
      • plot_surv_contour/plot-change-horizon.svg
      • plot_surv_contour/plot-cif.svg
      • plot_surv_contour/plot-custom-colors.svg
      • plot_surv_contour/plot-defaults.svg
      • plot_surv_contour/plot-lots-of-stuff.svg
      • plot_surv_contour/plot-panel-border-axis-dist.svg
      • plot_surv_contour/plot-with-group.svg
      • plot_surv_heatmap/plot-contour-lines.svg
      Error: Test failures
      Execution halted
    ```

# CoreMicrobiomeR

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/CoreMicrobiomeR
* Date/Publication: 2024-04-03 20:03:02 UTC
* Number of recursive dependencies: 89

Run `revdepcheck::cloud_details(, "CoreMicrobiomeR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘CoreMicrobiomeR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: group_bar_plots
    > ### Title: Grouped Bar Plots Based on Sample Size
    > ### Aliases: group_bar_plots
    > 
    > ### ** Examples
    > 
    > #To run input data
    ...
    +  top_percentage = 10  # Adjust the percentage as needed for core/non-core OTUs
    + )
    Warning encountered during diversity analysis:you have empty rows: their dissimilarities may be
                     meaningless in method “bray”
    > #To run grouped bar plot function
    > plot_group_bar <- group_bar_plots(core_1$final_otu_table_bef_filter,
    + core_1$final_otu_aft_filter, 10)
    Error in pm[[2]] : subscript out of bounds
    Calls: group_bar_plots -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# correlationfunnel

<details>

* Version: 0.2.0
* GitHub: https://github.com/business-science/correlationfunnel
* Source code: https://github.com/cran/correlationfunnel
* Date/Publication: 2020-06-09 04:40:03 UTC
* Number of recursive dependencies: 114

Run `revdepcheck::cloud_details(, "correlationfunnel")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(dplyr)
      
      Attaching package: 'dplyr'
      
      The following object is masked from 'package:testthat':
      
    ...
          ▆
       1. ├─correlationfunnel::plot_correlation_funnel(...) at test-plot_correlation_funnel.R:23:1
       2. └─correlationfunnel:::plot_correlation_funnel.data.frame(...)
       3.   ├─plotly::ggplotly(g, tooltip = "text")
       4.   └─plotly:::ggplotly.ggplot(g, tooltip = "text")
       5.     └─plotly::gg2list(...)
      
      [ FAIL 1 | WARN 3 | SKIP 0 | PASS 17 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

# corrViz

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/corrViz
* Date/Publication: 2023-06-30 11:40:07 UTC
* Number of recursive dependencies: 127

Run `revdepcheck::cloud_details(, "corrViz")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘corrViz-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: animSolar
    > ### Title: animSolar
    > ### Aliases: animSolar
    > 
    > ### ** Examples
    > 
    > cm <- cor(mtcars)
    ...
      All aesthetics have length 1, but the data has 250 rows.
    ℹ Please consider using `annotate()` or provide this layer with data containing
      a single row.
    Warning in geom_text(data = solar_system, aes(x = 0, y = 0, label = sun),  :
      All aesthetics have length 1, but the data has 250 rows.
    ℹ Please consider using `annotate()` or provide this layer with data containing
      a single row.
    Error in pm[[2]] : subscript out of bounds
    Calls: animSolar -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘corrViz.Rmd’ using rmarkdown
    
    Quitting from corrViz.Rmd:75-81 [heatmap]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `pm[[2]]`:
    ! subscript out of bounds
    ---
    Backtrace:
    ...
    
    Error: processing vignette 'corrViz.Rmd' failed with diagnostics:
    subscript out of bounds
    --- failed re-building ‘corrViz.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘corrViz.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.2Mb
      sub-directories of 1Mb or more:
        doc   6.7Mb
    ```

# CoSMoS

<details>

* Version: 2.1.0
* GitHub: https://github.com/TycheLab/CoSMoS
* Source code: https://github.com/cran/CoSMoS
* Date/Publication: 2021-05-29 23:20:08 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::cloud_details(, "CoSMoS")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘vignette.Rmd’ using rmarkdown
    ```

# countfitteR

<details>

* Version: 1.4
* GitHub: https://github.com/BioGenies/countfitteR
* Source code: https://github.com/cran/countfitteR
* Date/Publication: 2020-09-30 21:30:02 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "countfitteR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(countfitteR)
      > 
      > test_check("countfitteR")
      [ FAIL 1 | WARN 6 | SKIP 0 | PASS 32 ]
      
    ...
      Error in `p$labels[[1]]`: subscript out of bounds
      Backtrace:
          ▆
       1. └─testthat::expect_equal(p$labels[[1]], "x") at testing.R:45:3
       2.   └─testthat::quasi_label(enquo(object), label, arg = "object")
       3.     └─rlang::eval_bare(expr, quo_get_env(quo))
      
      [ FAIL 1 | WARN 6 | SKIP 0 | PASS 32 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) summary_fitlist.Rd:15: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) summary_fitlist.Rd:16: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) summary_fitlist.Rd:17: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) summary_fitlist.Rd:18: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) summary_fitlist.Rd:19: Lost braces in \itemize; \value handles \item{}{} directly
    ```

# covidcast

<details>

* Version: 0.5.2
* GitHub: https://github.com/cmu-delphi/covidcast
* Source code: https://github.com/cran/covidcast
* Date/Publication: 2023-07-12 23:40:06 UTC
* Number of recursive dependencies: 90

Run `revdepcheck::cloud_details(, "covidcast")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(covidcast)
      We encourage COVIDcast API users to register on our mailing list:
      https://lists.andrew.cmu.edu/mailman/listinfo/delphi-covidcast-api
      We'll send announcements about new data sources, package updates,
      server maintenance, and new features.
      > 
    ...
      • plot/default-county-choropleth.svg
      • plot/default-hrr-choropleth-with-include.svg
      • plot/default-msa-choropleth-with-include.svg
      • plot/default-state-choropleth-with-include.svg
      • plot/default-state-choropleth-with-range.svg
      • plot/state-choropleth-with-no-metadata.svg
      • plot/state-line-graph-with-range.svg
      • plot/state-line-graph-with-stderrs.svg
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘correlation-utils.Rmd’ using rmarkdown
    --- finished re-building ‘correlation-utils.Rmd’
    
    --- re-building ‘covidcast.Rmd’ using rmarkdown
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 20 marked UTF-8 strings
    ```

# crmPack

<details>

* Version: 1.0.6
* GitHub: https://github.com/openpharma/crmPack
* Source code: https://github.com/cran/crmPack
* Date/Publication: 2024-06-26 15:00:14 UTC
* Number of recursive dependencies: 56

Run `revdepcheck::cloud_details(, "crmPack")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘guidelines.Rmd’ using rmarkdown
    --- finished re-building ‘guidelines.Rmd’
    
    --- re-building ‘example.Rnw’ using Sweave
    Loading required package: ggplot2
    Registered S3 method overwritten by 'crmPack':
      method       from  
      print.gtable gtable
    Type crmPackHelp() to open help browser
    ...
    Error in `vec_slice<-`(`*tmp*`, is.na(match(pal_names, limits)), value = na_value) : 
      Can't convert `na_value` <character> to <double>.
    
    --- failed re-building ‘example.Rnw’
    
    SUMMARY: processing the following file failed:
      ‘example.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## Newly fixed

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘guidelines.Rmd’ using rmarkdown
    --- finished re-building ‘guidelines.Rmd’
    
    --- re-building ‘example.Rnw’ using Sweave
    Loading required package: ggplot2
    Registered S3 method overwritten by 'crmPack':
      method       from  
      print.gtable gtable
    Type crmPackHelp() to open help browser
    ...
    l.7 ^^M
           
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘example.Rnw’
    
    SUMMARY: processing the following file failed:
      ‘example.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ctrialsgov

<details>

* Version: 0.2.5
* GitHub: NA
* Source code: https://github.com/cran/ctrialsgov
* Date/Publication: 2021-10-18 16:00:02 UTC
* Number of recursive dependencies: 97

Run `revdepcheck::cloud_details(, "ctrialsgov")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ctrialsgov)
      > 
      > test_check("ctrialsgov")
      [NCT04553939] ible Local Advanved |Bladder| Cancer
      [NCT03517995]  of Sulforaphane in |Bladder| Cancer Chemoprevent
      [NCT04210479]       Comparison of |Bladder| Filling vs. Non-Fil
    ...
          ▆
       1. ├─ctrialsgov::ctgov_to_plotly(p) at test-plot.R:12:3
       2. └─ctrialsgov:::ctgov_to_plotly.ctgov_bar_plot(p)
       3.   ├─plotly::ggplotly(p, tooltip = "text")
       4.   └─plotly:::ggplotly.ggplot(p, tooltip = "text")
       5.     └─plotly::gg2list(...)
      
      [ FAIL 1 | WARN 6 | SKIP 0 | PASS 43 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1350 marked UTF-8 strings
    ```

# cubble

<details>

* Version: 1.0.0
* GitHub: https://github.com/huizezhang-sherry/cubble
* Source code: https://github.com/cran/cubble
* Date/Publication: 2024-08-27 15:20:02 UTC
* Number of recursive dependencies: 131

Run `revdepcheck::cloud_details(, "cubble")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘cb1class.Rmd’ using rmarkdown
    --- finished re-building ‘cb1class.Rmd’
    
    --- re-building ‘cb2create.Rmd’ using rmarkdown
    --- finished re-building ‘cb2create.Rmd’
    
    --- re-building ‘cb3tsibblesf.Rmd’ using rmarkdown
    --- finished re-building ‘cb3tsibblesf.Rmd’
    
    --- re-building ‘cb4glyph.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.6Mb
      sub-directories of 1Mb or more:
        data   6.0Mb
        doc    1.4Mb
    ```

# cvms

<details>

* Version: 1.7.0
* GitHub: https://github.com/ludvigolsen/cvms
* Source code: https://github.com/cran/cvms
* Date/Publication: 2025-03-07 11:30:07 UTC
* Number of recursive dependencies: 152

Run `revdepcheck::cloud_details(, "cvms")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(cvms)
      > 
      > if (require("xpectr")) {
      +   test_check("cvms")
      + }
      Loading required package: xpectr
    ...
      ── Failure ('test_plotting_functions.R:349:3'): plot_confusion_matrix() with sum tiles, class order, and intensity_by percentage ──
      sapply(p2$layers, function(x) class(x$geom)[1]) not equal to c(...).
      names for target but not for current
      ── Failure ('test_plotting_functions.R:359:3'): plot_confusion_matrix() with sum tiles, class order, and intensity_by percentage ──
      `labels` not equal to list(...).
      Length mismatch: comparison on first 4 components
      
      [ FAIL 12 | WARN 0 | SKIP 71 | PASS 3605 ]
      Error: Test failures
      Execution halted
    ```

# cylcop

<details>

* Version: 0.2.0
* GitHub: https://github.com/r-lib/devtools
* Source code: https://github.com/cran/cylcop
* Date/Publication: 2022-10-29 22:00:21 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "cylcop")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘cylcop-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_circ_hist
    > ### Title: Circular Histogram of Turn Angles
    > ### Aliases: plot_circ_hist
    > 
    > ### ** Examples
    > 
    > set.seed(123)
    ...
     22. │               └─l$compute_geom_2(d, theme = plot$theme)
     23. │                 └─ggplot2 (local) compute_geom_2(..., self = self)
     24. │                   └─self$geom$use_defaults(...)
     25. │                     └─ggplot2 (local) use_defaults(..., self = self)
     26. │                       └─ggplot2:::check_aesthetics(new_params, nrow(data))
     27. │                         └─vctrs::list_sizes(x)
     28. └─vctrs:::stop_scalar_type(`<fn>`(`<expression>`), "x$label", `<env>`)
     29.   └─vctrs:::stop_vctrs(...)
     30.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) gammamix.Rd:47-48: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) gammamix.Rd:49-51: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) gammamix.Rd:52-53: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) gammamix.Rd:54-55: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) joint.Rd:41-42: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) joint.Rd:43-45: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) joint.Rd:46-47: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) lnormmix.Rd:44-45: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) lnormmix.Rd:46-48: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) lnormmix.Rd:49-50: Lost braces in \itemize; \value handles \item{}{} directly
    ...
    checkRd: (-1) vonmisesmix.Rd:45-46: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) vonmisesmix.Rd:47-48: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) weibullmix.Rd:44-45: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) weibullmix.Rd:46-48: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) weibullmix.Rd:49-50: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) weibullmix.Rd:51-52: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) wrappedcauchy.Rd:43-44: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) wrappedcauchy.Rd:45-47: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) wrappedcauchy.Rd:48-49: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) wrappedcauchy.Rd:50-51: Lost braces in \itemize; \value handles \item{}{} directly
    ```

# cystiSim

<details>

* Version: 0.1.0
* GitHub: https://github.com/brechtdv/cystiSim
* Source code: https://github.com/cran/cystiSim
* Date/Publication: 2016-05-15 21:46:33
* Number of recursive dependencies: 31

Run `revdepcheck::cloud_details(, "cystiSim")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘cystiSim-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: cystiRun
    > ### Title: 'cystiRun' object
    > ### Aliases: cystiRun initiate update.cystiRun print.cystiRun plot.cystiRun
    > ###   prevalence
    > 
    > ### ** Examples
    > 
    ...
     20. └─vctrs (local) `<fn>`()
     21.   └─vctrs::vec_default_ptype2(...)
     22.     ├─base::withRestarts(...)
     23.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     24.     │   └─base (local) doWithOneRestart(return(expr), restart)
     25.     └─vctrs::stop_incompatible_type(...)
     26.       └─vctrs:::stop_incompatible(...)
     27.         └─vctrs:::stop_vctrs(...)
     28.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) fit.Rd:37: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) fit.Rd:38: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) fit.Rd:39: Lost braces in \itemize; meant \describe ?
    ```

# D2MCS

<details>

* Version: 1.0.1
* GitHub: https://github.com/drordas/D2MCS
* Source code: https://github.com/cran/D2MCS
* Date/Publication: 2022-08-23 11:40:02 UTC
* Number of recursive dependencies: 178

Run `revdepcheck::cloud_details(, "D2MCS")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘test_all.R’
    Running the tests in ‘tests/test_all.R’ failed.
    Complete output:
      > testthat::test_check("D2MCS")
      Loading required package: D2MCS
      [ FAIL 6 | WARN 5 | SKIP 15 | PASS 702 ]
      
      ══ Skipped tests (15) ══════════════════════════════════════════════════════════
      • On CRAN (2): 'test_D2MCS.R:129:3', 'test_D2MCS.R:213:3'
      • {ranger} is not installed (13): 'test_D2MCS.R:411:3',
    ...
        5.   └─BinaryPlot$new()$plot(binary.summary)
        6.     └─super$plot(summary)
        7.       └─ggplot2::scale_color_continuous(...)
        8.         └─ggplot2:::scale_backward_compatibility(...)
        9.           └─cli::cli_abort("Unknown scale type: {.val {scale}}")
       10.             └─rlang::abort(...)
      
      [ FAIL 6 | WARN 5 | SKIP 15 | PASS 702 ]
      Error: Test failures
      Execution halted
    ```

# daiquiri

<details>

* Version: 1.1.1
* GitHub: https://github.com/ropensci/daiquiri
* Source code: https://github.com/cran/daiquiri
* Date/Publication: 2023-07-18 16:50:09 UTC
* Number of recursive dependencies: 105

Run `revdepcheck::cloud_details(, "daiquiri")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(daiquiri)
      > 
      > test_check("daiquiri")
      
      Quitting from report_htmldoc.Rmd:466-528 [daiquiri-individual-fields]
      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ...
       36.                 └─rlang::abort(message, ..., call = call, arg = arg)
      
      [ FAIL 4 | WARN 0 | SKIP 8 | PASS 472 ]
      Deleting unused snapshots:
      • aggregate_data/test_[ALL_FIELDS_COMBINED].csv
      • aggregate_data/test_[DUPLICATES].csv
      • aggregate_data/test_col1.csv
      • aggregate_data/test_col2.csv
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘daiquiri.Rmd’ using rmarkdown
    ```

# DAISIEprep

<details>

* Version: 1.0.0
* GitHub: https://github.com/joshwlambert/DAISIEprep
* Source code: https://github.com/cran/DAISIEprep
* Date/Publication: 2024-12-18 00:20:02 UTC
* Number of recursive dependencies: 146

Run `revdepcheck::cloud_details(, "DAISIEprep")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(DAISIEprep)
      > 
      > test_check("DAISIEprep")
      [ FAIL 4 | WARN 2 | SKIP 14 | PASS 2235 ]
      
      ══ Skipped tests (14) ══════════════════════════════════════════════════════════
    ...
      ── Failure ('test-plot_phylod.R:8:3'): plot_phylod runs silent without error ───
      `plot_phylod(phylod = phylod, node_pies = FALSE)` produced warnings.
      ── Failure ('test-plot_phylod.R:13:3'): plot_phylod runs silent without error ──
      `plot_phylod(phylod = phylod, node_pies = TRUE)` produced warnings.
      ── Failure ('test-plot_phylod.R:18:3'): plot_phylod runs silent without error ──
      `plot_phylod(phylod = phylod, node_pies = TRUE)` produced warnings.
      
      [ FAIL 4 | WARN 2 | SKIP 14 | PASS 2235 ]
      Error: Test failures
      Execution halted
    ```

# dampack

<details>

* Version: 1.0.2.1000
* GitHub: https://github.com/DARTH-git/dampack
* Source code: https://github.com/cran/dampack
* Date/Publication: 2024-09-30 17:00:06 UTC
* Number of recursive dependencies: 86

Run `revdepcheck::cloud_details(, "dampack")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘dampack-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: calc_exp_loss
    > ### Title: Calculate the expected loss at a range of willingness-to-pay
    > ###   thresholds
    > ### Aliases: calc_exp_loss
    > 
    > ### ** Examples
    > 
    ...
      5.       └─ggplot2:::scale_backward_compatibility(...)
      6.         ├─rlang::exec(scale, !!!args)
      7.         └─ggplot2 (local) `<fn>`(...)
      8.           ├─ggplot2::discrete_scale(...)
      9.           │ └─ggplot2::ggproto(...)
     10.           │   └─rlang::list2(...)
     11.           └─ggplot2:::pal_qualitative(type = type)
     12.             └─ggplot2:::stop_input_type(type, "a character vector or list of character vectors")
     13.               └─rlang::abort(message, ..., call = call, arg = arg)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(dampack)
      Loading required package: ggplot2
      > 
      > test_check("dampack")
      [ FAIL 12 | WARN 0 | SKIP 1 | PASS 122 ]
      
    ...
        8.           ├─ggplot2::discrete_scale(...)
        9.           │ └─ggplot2::ggproto(...)
       10.           │   └─rlang::list2(...)
       11.           └─ggplot2:::pal_qualitative(type = type)
       12.             └─ggplot2:::stop_input_type(type, "a character vector or list of character vectors")
       13.               └─rlang::abort(message, ..., call = call, arg = arg)
      
      [ FAIL 12 | WARN 0 | SKIP 1 | PASS 122 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘basic_cea.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        data   3.0Mb
        doc    1.8Mb
    ```

# deeptime

<details>

* Version: 2.1.0
* GitHub: https://github.com/willgearty/deeptime
* Source code: https://github.com/cran/deeptime
* Date/Publication: 2024-10-25 23:30:02 UTC
* Number of recursive dependencies: 195

Run `revdepcheck::cloud_details(, "deeptime")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘deeptime-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: facet_grid_color
    > ### Title: Lay out panels in a grid with colored strips
    > ### Aliases: facet_grid_color FacetGridColor
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    > df <- data.frame(x = 1:10, y = 1:10, period = c("Permian", "Triassic"))
    > ggplot(df) +
    +   geom_point(aes(x, y)) +
    +   facet_grid_color(cols = vars(period), colors = periods)
    Error in match.fun(params$labeller) : 
      'params$labeller' is not a function, character or symbol
    Calls: <Anonymous> ... attach_strips -> <Anonymous> -> format_strip_labels -> match.fun
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(deeptime)
      > 
      > test_check("deeptime")
      Scale for y is already present.
      Adding another scale for y, which will replace the existing scale.
      Scale for y is already present.
    ...
      • patterns/geo-pattern2-new.svg
      • patterns/scale-fill-geopattern-labels-new.svg
      • patterns/scale-fill-geopattern-limits-new.svg
      • patterns/scale-fill-geopattern-na-new.svg
      • patterns/scale-fill-geopattern-na2-new.svg
      • points_range/geom-points-range-aes-new.svg
      • points_range/geom-points-range-bg-new.svg
      • points_range/geom-points-range-h-new.svg
      Error: Test failures
      Execution halted
    ```

# Dforest

<details>

* Version: 0.4.2
* GitHub: NA
* Source code: https://github.com/cran/Dforest
* Date/Publication: 2017-11-28 22:03:57 UTC
* Number of recursive dependencies: 27

Run `revdepcheck::cloud_details(, "Dforest")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Dforest-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: DF_easy
    > ### Title: Simple pre-defined pipeline for Decision forest
    > ### Aliases: DF_easy
    > 
    > ### ** Examples
    > 
    >   # data(demo_simple)
    ...
    ! Unknown scale type:
    Backtrace:
        ▆
     1. └─Dforest::DF_easy(Train_X, Train_Y, Test_X, Test_Y)
     2.   └─Dforest::DF_ConfPlot(Pred_result, Test_Y)
     3.     └─ggplot2::scale_color_continuous(...)
     4.       └─ggplot2:::scale_backward_compatibility(...)
     5.         └─cli::cli_abort("Unknown scale type: {.val {scale}}")
     6.           └─rlang::abort(...)
    Execution halted
    ```

# DImodelsVis

<details>

* Version: 1.0.1
* GitHub: NA
* Source code: https://github.com/cran/DImodelsVis
* Date/Publication: 2024-02-26 14:10:15 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::cloud_details(, "DImodelsVis")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘DImodelsVis-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: conditional_ternary
    > ### Title: Conditional ternary diagrams
    > ### Aliases: conditional_ternary
    > 
    > ### ** Examples
    > 
    > library(DImodels)
    ...
     11.       │ └─base::withCallingHandlers(...)
     12.       └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13.         └─l$compute_position(d, layout)
     14.           └─ggplot2 (local) compute_position(..., self = self)
     15.             └─self$position$use_defaults(data, self$aes_params)
     16.               └─ggplot2 (local) use_defaults(..., self = self)
     17.                 └─ggplot2:::check_aesthetics(new, nrow(data))
     18.                   └─cli::cli_abort(...)
     19.                     └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘DImodelsVis-with-complex-models.Rmd’ using rmarkdown
    ```

# directlabels

<details>

* Version: 2024.1.21
* GitHub: https://github.com/tdhock/directlabels
* Source code: https://github.com/cran/directlabels
* Date/Publication: 2024-01-24 19:20:07 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "directlabels")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘examples.Rmd’ using knitr
    ```

# distributions3

<details>

* Version: 0.2.2
* GitHub: https://github.com/alexpghayes/distributions3
* Source code: https://github.com/cran/distributions3
* Date/Publication: 2024-09-16 16:20:02 UTC
* Number of recursive dependencies: 89

Run `revdepcheck::cloud_details(, "distributions3")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(distributions3)
      
      Attaching package: 'distributions3'
      
      The following object is masked from 'package:stats':
      
    ...
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-plot.R:50:3'): ggplot2 implementation works ──────────────────
      `print(gg1)` produced warnings.
      ── Failure ('test-plot.R:56:3'): ggplot2 implementation works ──────────────────
      `print(gg3)` produced warnings.
      
      [ FAIL 2 | WARN 14 | SKIP 0 | PASS 2766 ]
      Error: Test failures
      Execution halted
    ```

# dittoViz

<details>

* Version: 1.0.3
* GitHub: https://github.com/dtm2451/dittoViz
* Source code: https://github.com/cran/dittoViz
* Date/Publication: 2025-02-25 18:30:02 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "dittoViz")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘dittoViz-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: barPlot
    > ### Title: Outputs a stacked bar plot to show the percent composition of
    > ###   samples, groups, clusters, or other groupings
    > ### Aliases: barPlot
    > 
    > ### ** Examples
    > 
    ...
    15     3        D    12                          32 0.3750000
    16     4        D     8                          32 0.2500000
    > # through hovering the cursor over the relevant parts of the plot
    > if (requireNamespace("plotly", quietly = TRUE)) {
    +     barPlot(example_df, "clustering", group.by = "groups",
    +         do.hover = TRUE)
    +     }
    Error in pm[[2]] : subscript out of bounds
    Calls: barPlot -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(dittoViz)
      Loading required package: ggplot2
      > test_check("dittoViz")
      Picking joint bandwidth of 11.2
      Picking joint bandwidth of 11.2
      Picking joint bandwidth of 11.2
    ...
        6.   └─dittoViz:::.yPlot_add_data_y_direction(...)
        7.     └─ggplot2::geom_violin(draw_quantiles = vlnplot.quantiles)
        8.       └─ggplot2:::check_numeric(draw_quantiles)
        9.         └─ggplot2:::check_object(x, is.numeric, what, ..., arg = arg, call = call)
       10.           └─ggplot2:::stop_input_type(...)
       11.             └─rlang::abort(message, ..., call = call, arg = arg)
      
      [ FAIL 47 | WARN 19 | SKIP 0 | PASS 199 ]
      Error: Test failures
      Execution halted
    ```

# dotsViolin

<details>

* Version: 0.0.1
* GitHub: NA
* Source code: https://github.com/cran/dotsViolin
* Date/Publication: 2023-10-30 13:20:02 UTC
* Number of recursive dependencies: 37

Run `revdepcheck::cloud_details(, "dotsViolin")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘dotsViolin-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: dots_and_violin
    > ### Title: Makes a composite dot-plot and violin-plot
    > ### Aliases: dots_and_violin
    > ### Keywords: dot-plot violin-plot
    > 
    > ### ** Examples
    > 
    ...
     29. │                                   └─ggplot2 (local) draw_group(...)
     30. │                                     ├─ggplot2:::ggname(...)
     31. │                                     │ └─grid::grobName(grob, prefix)
     32. │                                     └─ggplot2:::dotstackGrob(...)
     33. └─base::.handleSimpleError(...)
     34.   └─rlang (local) h(simpleError(msg, call))
     35.     └─handlers[[1L]](cnd)
     36.       └─cli::cli_abort(...)
     37.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

# duke

<details>

* Version: 0.0.3
* GitHub: https://github.com/aidangildea/duke
* Source code: https://github.com/cran/duke
* Date/Publication: 2023-12-15 21:50:16 UTC
* Number of recursive dependencies: 86

Run `revdepcheck::cloud_details(, "duke")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/tests.html
      > # * https://testthat.r-lib.org/reference/test_package.html#special-files
    ...
       2. │ └─testthat:::expect_condition_matching(...)
       3. │   └─testthat:::quasi_capture(...)
       4. │     ├─testthat (local) .capture(...)
       5. │     │ └─base::withCallingHandlers(...)
       6. │     └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
       7. └─testthat::expect_equal(ggplot2::layer_data(p3)$fill[9], "#B5B5B5")
      
      [ FAIL 1 | WARN 10 | SKIP 0 | PASS 27 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘duke.Rmd’ using rmarkdown
    ```

# easysurv

<details>

* Version: 2.0.1
* GitHub: https://github.com/Maple-Health-Group/easysurv
* Source code: https://github.com/cran/easysurv
* Date/Publication: 2024-06-21 10:30:06 UTC
* Number of recursive dependencies: 153

Run `revdepcheck::cloud_details(, "easysurv")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘easysurv-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: get_km
    > ### Title: Generate Kaplan-Meier estimates
    > ### Aliases: get_km
    > 
    > ### ** Examples
    > 
    > km_results <- get_km(
    ...
    Poor     Poor     228    145 3.101736 0.1772520 2.183562 1.978082 2.619178
           Median follow-up
    Good           4.452055
    Medium         4.712329
    Poor           4.115068
    
    Error in .construct_risktable(x, geom_blank.times = NULL, geom_blank.risktable_stats = "{n.risk}",  : 
      argument "risktable_height" is missing, with no default
    Calls: <Anonymous> ... ggsurvfit_build -> <Anonymous> -> .construct_risktable
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘easysurv.Rmd’ using rmarkdown
    
    Quitting from easysurv.Rmd:148-157 [km]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `.construct_risktable()`:
    ! argument "risktable_height" is missing, with no default
    ---
    Backtrace:
    ...
    
    Error: processing vignette 'easysurv.Rmd' failed with diagnostics:
    argument "risktable_height" is missing, with no default
    --- failed re-building ‘easysurv.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘easysurv.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# EbayesThresh

<details>

* Version: 1.4-12
* GitHub: https://github.com/stephenslab/EbayesThresh
* Source code: https://github.com/cran/EbayesThresh
* Date/Publication: 2017-08-08 04:02:13 UTC
* Number of recursive dependencies: 64

Run `revdepcheck::cloud_details(, "EbayesThresh")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ebayesthresh.Rmd’ using rmarkdown
    ```

# ecocbo

<details>

* Version: 0.12.0
* GitHub: NA
* Source code: https://github.com/cran/ecocbo
* Date/Publication: 2024-08-21 08:00:02 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::cloud_details(, "ecocbo")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ecocbo-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ecocbo-package
    > ### Title: ecocbo: Calculating Optimum Sampling Effort in Community Ecology
    > ### Aliases: ecocbo-package ecocbo
    > ### Keywords: package
    > 
    > ### ** Examples
    > 
    ...
     39.   └─vctrs::vec_default_cast(...)
     40.     ├─base::withRestarts(...)
     41.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     42.     │   └─base (local) doWithOneRestart(return(expr), restart)
     43.     └─vctrs::stop_incompatible_cast(...)
     44.       └─vctrs::stop_incompatible_type(...)
     45.         └─vctrs:::stop_incompatible(...)
     46.           └─vctrs:::stop_vctrs(...)
     47.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/tests.html
      > # * https://testthat.r-lib.org/reference/test_package.html#special-files
    ...
      ── Failure ('test-plot_power.R:6:3'): plots are plotted ────────────────────────
      Expected `plot_power(epiBetaR, m = 4, method = "both")` to run without any conditions.
      i Actually got a <purrr_error_indexed> with text:
        i In index: 1.
        Caused by error in `vec_slice<-`:
        ! Can't convert `na_value` <character> to <integer>.
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 26 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ecocbo-guide.Rmd’ using rmarkdown
    
    Quitting from ecocbo-guide.Rmd:164-167 [step4]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'ecocbo-guide.Rmd' failed with diagnostics:
    ...
    ℹ In index: 1.
    Caused by error in `vec_slice<-`:
    ! Can't convert `na_value` <character> to <integer>.
    --- failed re-building ‘ecocbo-guide.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ecocbo-guide.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ecolRxC

<details>

* Version: 0.1.1-10
* GitHub: NA
* Source code: https://github.com/cran/ecolRxC
* Date/Publication: 2023-03-31 07:50:02 UTC
* Number of recursive dependencies: 26

Run `revdepcheck::cloud_details(, "ecolRxC")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ecolRxC-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.ecolRxC
    > ### Title: Graphical representation of a RxC ecological inference (vote
    > ###   transfer) matrix
    > ### Aliases: plot.ecolRxC
    > 
    > ### ** Examples
    > 
    ...
    ! Unknown scale type:
    Backtrace:
        ▆
     1. ├─base::plot(example, show.plot = FALSE)
     2. └─ecolRxC:::plot.ecolRxC(example, show.plot = FALSE)
     3.   └─ggplot2::scale_fill_continuous(...)
     4.     └─ggplot2:::scale_backward_compatibility(...)
     5.       └─cli::cli_abort("Unknown scale type: {.val {scale}}")
     6.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) ecolRxC.Rd:161-166: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) ecolRxC.Rd:167-175: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) ecolRxC.Rd:176-178: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) ecolRxC.Rd:179-185: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) ecolRxC.Rd:186-192: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) ecolRxC.Rd:193-201: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) ecolRxC.Rd:202-208: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) ecolRxC.Rd:209-216: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) ecolRxC.Rd:217-221: Lost braces in \itemize; meant \describe ?
    ```

# EGM

<details>

* Version: 0.1.0
* GitHub: https://github.com/shah-in-boots/EGM
* Source code: https://github.com/cran/EGM
* Date/Publication: 2024-05-23 16:10:05 UTC
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "EGM")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(EGM)
      Loading required package: vctrs
      Loading required package: data.table
      > EGM::set_wfdb_path("/usr/local/bin")
      > 
      > test_check("EGM")
    ...
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-ggm.R:63:2'): theming works ──────────────────────────────────
      g$labels$x (`actual`) not equal to "sample" (`expected`).
      
      `actual` is NULL
      `expected` is a character vector ('sample')
      
      [ FAIL 1 | WARN 9 | SKIP 19 | PASS 43 ]
      Error: Test failures
      Execution halted
    ```

# eiCircles

<details>

* Version: 0.0.1-12
* GitHub: NA
* Source code: https://github.com/cran/eiCircles
* Date/Publication: 2025-04-11 11:20:01 UTC
* Number of recursive dependencies: 28

Run `revdepcheck::cloud_details(, "eiCircles")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘eiCircles-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.BPF
    > ### Title: Graphical representation of a RxC ecological inference (vote
    > ###   transfer) matrix
    > ### Aliases: plot.BPF
    > 
    > ### ** Examples
    > 
    ...
    ! Unknown scale type:
    Backtrace:
        ▆
     1. ├─base::plot(example, show.plot = FALSE)
     2. └─eiCircles:::plot.BPF(example, show.plot = FALSE)
     3.   └─ggplot2::scale_fill_continuous(...)
     4.     └─ggplot2:::scale_backward_compatibility(...)
     5.       └─cli::cli_abort("Unknown scale type: {.val {scale}}")
     6.         └─rlang::abort(...)
    Execution halted
    ```

# eks

<details>

* Version: 1.1.0
* GitHub: NA
* Source code: https://github.com/cran/eks
* Date/Publication: 2025-05-18 17:30:02 UTC
* Number of recursive dependencies: 79

Run `revdepcheck::cloud_details(, "eks")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘eks-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: tidyst_kdr
    > ### Title: Tidy and geospatial kernel density ridge estimates
    > ### Aliases: tidy_kdr st_kdr
    > ### Keywords: smooth
    > 
    > ### ** Examples
    > ## tidy density ridge estimate
    ...
     20. └─vctrs (local) `<fn>`()
     21.   └─vctrs::vec_default_ptype2(...)
     22.     ├─base::withRestarts(...)
     23.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     24.     │   └─base (local) doWithOneRestart(return(expr), restart)
     25.     └─vctrs::stop_incompatible_type(...)
     26.       └─vctrs:::stop_incompatible(...)
     27.         └─vctrs:::stop_vctrs(...)
     28.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘tidysf_kde.Rmd’ using rmarkdown
    ```

# enrichR

<details>

* Version: 3.4
* GitHub: NA
* Source code: https://github.com/cran/enrichR
* Date/Publication: 2025-02-02 22:50:06 UTC
* Number of recursive dependencies: 54

Run `revdepcheck::cloud_details(, "enrichR")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘enrichR.Rmd’ using rmarkdown
    
    Quitting from enrichR.Rmd:159-164 [unnamed-chunk-15]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'enrichR.Rmd' failed with diagnostics:
    Unknown scale type:
    --- failed re-building ‘enrichR.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘enrichR.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# epiCleanr

<details>

* Version: 0.2.0
* GitHub: https://github.com/truenomad/epiCleanr
* Source code: https://github.com/cran/epiCleanr
* Date/Publication: 2023-09-28 12:20:05 UTC
* Number of recursive dependencies: 128

Run `revdepcheck::cloud_details(, "epiCleanr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘epiCleanr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: handle_outliers
    > ### Title: Detect and Handle Outliers in Dataset
    > ### Aliases: handle_outliers
    > 
    > ### ** Examples
    > 
    > 
    ...
     10. │           └─ggplot2 (local) FUN(X[[i]], ...)
     11. │             └─scale$map_df(df = df)
     12. │               └─ggplot2 (local) map_df(..., self = self)
     13. │                 └─base::lapply(aesthetics, function(j) self$map(df[[j]]))
     14. │                   └─ggplot2 (local) FUN(X[[i]], ...)
     15. │                     └─self$map(df[[j]])
     16. │                       └─ggplot2 (local) map(..., self = self)
     17. │                         └─vctrs::`vec_slice<-`(`*tmp*`, is.na(x), value = na_value)
     18. └─rlang::cnd_signal(x)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        doc    2.9Mb
        help   2.5Mb
    ```

# epiphy

<details>

* Version: 0.5.0
* GitHub: https://github.com/chgigot/epiphy
* Source code: https://github.com/cran/epiphy
* Date/Publication: 2023-11-16 11:20:10 UTC
* Number of recursive dependencies: 89

Run `revdepcheck::cloud_details(, "epiphy")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘epiphy-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: clump
    > ### Title: Regroup observational data into even clumps of individuals.
    > ### Aliases: clump clump.intensity
    > 
    > ### ** Examples
    > 
    > my_incidence <- incidence(tomato_tswv$field_1929)
    ...
     18. │               └─ggplot2 (local) setup_params(...)
     19. │                 └─ggplot2:::make_summary_fun(...)
     20. │                   └─rlang::as_function(fun.data)
     21. │                     └─base::get(x, envir = env, mode = "function")
     22. └─base::.handleSimpleError(...)
     23.   └─rlang (local) h(simpleError(msg, call))
     24.     └─handlers[[1L]](cnd)
     25.       └─cli::cli_abort(...)
     26.         └─rlang::abort(...)
    Execution halted
    ```

# EQUALSTATS

<details>

* Version: 0.5.0
* GitHub: NA
* Source code: https://github.com/cran/EQUALSTATS
* Date/Publication: 2024-09-23 08:30:02 UTC
* Number of recursive dependencies: 129

Run `revdepcheck::cloud_details(, "EQUALSTATS")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘EQUALSTATS-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: function.Survival_Analysis
    > ### Title: Perform Survival Analysis
    > ### Aliases: function.Survival_Analysis
    > 
    > ### ** Examples
    > 
    > # Create simulated data ####
    ...
    > rv$second_menu_choice <- NA
    > rv$entry[[1]] <- "Admission to care home"
    > rv$entry[[2]] <- "Follow-up"
    > rv$entry[[3]] <- "Treatment"
    > # Final function ####
    > Results <- function.Survival_Analysis(Predefined_lists, rv)
    Error in .construct_risktable(x, geom_blank.times = NULL, geom_blank.risktable_stats = c("{n.risk}",  : 
      argument "risktable_height" is missing, with no default
    Calls: function.Survival_Analysis
    Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘lmerTest’
    ```

# equatiomatic

<details>

* Version: 0.3.6
* GitHub: https://github.com/datalorax/equatiomatic
* Source code: https://github.com/cran/equatiomatic
* Date/Publication: 2025-03-10 16:50:09 UTC
* Number of recursive dependencies: 146

Run `revdepcheck::cloud_details(, "equatiomatic")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘colors.Rmd’ using rmarkdown
    --- finished re-building ‘colors.Rmd’
    
    --- re-building ‘equatiomatic.Rmd’ using rmarkdown
    --- finished re-building ‘equatiomatic.Rmd’
    
    --- re-building ‘forecast-arima.Rmd’ using rmarkdown
    --- finished re-building ‘forecast-arima.Rmd’
    
    --- re-building ‘lme4-lmer.Rmd’ using rmarkdown
    --- finished re-building ‘lme4-lmer.Rmd’
    
    --- re-building ‘plotting-integration.Rmd’ using rmarkdown
    ```

# errors

<details>

* Version: 0.4.3
* GitHub: https://github.com/r-quantities/errors
* Source code: https://github.com/cran/errors
* Date/Publication: 2025-01-18 18:10:05 UTC
* Number of recursive dependencies: 66

Run `revdepcheck::cloud_details(, "errors")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘errors-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_errors
    > ### Title: Errorbars for 'errors' objects
    > ### Aliases: geom_errors
    > 
    > ### ** Examples
    > 
    > if (requireNamespace("ggplot2", quietly=TRUE)) {
    ...
     20. │                   └─self$draw_panel(data, panel_params, coord, width = 0.05, height = 0.05)
     21. │                     └─errors (local) draw_panel(...)
     22. │                       ├─base::append(...)
     23. │                       └─ggplot2::GeomErrorbarh$draw_panel(...)
     24. └─base::.handleSimpleError(...)
     25.   └─rlang (local) h(simpleError(msg, call))
     26.     └─handlers[[1L]](cnd)
     27.       └─cli::cli_abort(...)
     28.         └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘rjournal.Rmd’ using rmarkdown
    
    Quitting from rjournal.Rmd:252-272 [plot]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'rjournal.Rmd' failed with diagnostics:
    ...
    ℹ Error occurred in the 2nd layer.
    Caused by error in `draw_panel()`:
    ! unused argument (height = NULL)
    --- failed re-building ‘rjournal.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘rjournal.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# eventstudyr

<details>

* Version: 1.1.3
* GitHub: https://github.com/JMSLab/eventstudyr
* Source code: https://github.com/cran/eventstudyr
* Date/Publication: 2024-03-04 15:00:02 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "eventstudyr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(eventstudyr)
      > 
      > test_check("eventstudyr")
      Defaulting to strongest lead of differenced policy variable: proxyIV = z_fd_lead3. To specify a different proxyIV use the proxyIV argument.
      Defaulting to strongest lead of differenced policy variable: proxyIV = z_fd_lead3. To specify a different proxyIV use the proxyIV argument.
      Defaulting to strongest lead of differenced policy variable: proxyIV = z_fd_lead3. To specify a different proxyIV use the proxyIV argument.
    ...
      `expected` is a character vector ('ci_lower')
      ── Failure ('test-EventStudyPlot.R:128:5'): confidence intervals are appropriately present or absent ──
      p_ci$labels$ymax (`actual`) not equal to "ci_upper" (`expected`).
      
      `actual` is NULL
      `expected` is a character vector ('ci_upper')
      
      [ FAIL 6 | WARN 0 | SKIP 0 | PASS 258 ]
      Error: Test failures
      Execution halted
    ```

# explainer

<details>

* Version: 1.0.2
* GitHub: https://github.com/PERSIMUNE/explainer
* Source code: https://github.com/cran/explainer
* Date/Publication: 2024-09-30 17:30:02 UTC
* Number of recursive dependencies: 185

Run `revdepcheck::cloud_details(, "explainer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘explainer-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: eDecisionCurve
    > ### Title: Decision Curve Plot
    > ### Aliases: eDecisionCurve
    > 
    > ### ** Examples
    > 
    > library("explainer")
    ...
    > mylrn$train(maintask, splits$train)
    > myplot <- eDecisionCurve(
    +   task = maintask,
    +   trained_model = mylrn,
    +   splits = splits,
    +   seed = seed
    + )
    Error in pm[[2]] : subscript out of bounds
    Calls: eDecisionCurve -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ggpmisc’
      All declared Imports should be used.
    ```

# ez

<details>

* Version: 4.4-0
* GitHub: https://github.com/mike-lawrence/ez
* Source code: https://github.com/cran/ez
* Date/Publication: 2016-11-02 18:17:31
* Number of recursive dependencies: 63

Run `revdepcheck::cloud_details(, "ez")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ez-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ezPlot
    > ### Title: Plot data from a factorial experiment
    > ### Aliases: ezPlot
    > 
    > ### ** Examples
    > 
    > #Read in the ANT data (see ?ANT).
    ...
    Backtrace:
        ▆
     1. └─ez::ezPlot(...)
     2.   ├─base::eval(parse(text = p))
     3.   │ └─base::eval(parse(text = p))
     4.   └─ggplot2::facet_grid(facets = . ~ group, scales = "free_y")
     5.     └─lifecycle::deprecate_stop("2.2.0", "facet_grid(facets)", "facet_grid(rows)")
     6.       └─lifecycle:::deprecate_stop0(msg)
     7.         └─rlang::cnd_signal(...)
    Execution halted
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) ez-package.Rd:19: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) ez-package.Rd:20: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) ez-package.Rd:21: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) ez-package.Rd:22: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) ez-package.Rd:23: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) ez-package.Rd:24: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) ez-package.Rd:25: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) ez-package.Rd:26: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) ez-package.Rd:27: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) ez-package.Rd:28: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) ez-package.Rd:29: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) ez-package.Rd:30: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) ez-package.Rd:32: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) ez-package.Rd:33: Lost braces in \itemize; meant \describe ?
    ```

# ezEDA

<details>

* Version: 0.1.1
* GitHub: https://github.com/kviswana/ezEDA
* Source code: https://github.com/cran/ezEDA
* Date/Publication: 2021-06-29 04:40:10 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::cloud_details(, "ezEDA")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ezEDA)
      > 
      > test_check("ezEDA")
      [ FAIL 22 | WARN 0 | SKIP 0 | PASS 57 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      ── Error ('test_two_measures_relationship.R:19:3'): y axis is labeled 'hwy' ────
      Error in `expect_match(p$labels$y, "hwy")`: is.character(act$val) is not TRUE
      Backtrace:
          ▆
       1. └─testthat::expect_match(p$labels$y, "hwy") at test_two_measures_relationship.R:19:3
       2.   └─base::stopifnot(is.character(act$val))
      
      [ FAIL 22 | WARN 0 | SKIP 0 | PASS 57 ]
      Error: Test failures
      Execution halted
    ```

# fabletools

<details>

* Version: 0.5.0
* GitHub: https://github.com/tidyverts/fabletools
* Source code: https://github.com/cran/fabletools
* Date/Publication: 2024-09-17 07:30:02 UTC
* Number of recursive dependencies: 105

Run `revdepcheck::cloud_details(, "fabletools")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(dplyr)
      
      Attaching package: 'dplyr'
      
      The following object is masked from 'package:testthat':
      
    ...
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-graphics.R:327:3'): autoplot_dcmp_ts() ───────────────────────
      `print(p)` produced warnings.
      ── Failure ('test-graphics.R:346:3'): autoplot_dcmp_ts() ───────────────────────
      `print(p)` produced warnings.
      
      [ FAIL 2 | WARN 5 | SKIP 1 | PASS 292 ]
      Error: Test failures
      Execution halted
    ```

# fairmodels

<details>

* Version: 1.2.1
* GitHub: https://github.com/ModelOriented/fairmodels
* Source code: https://github.com/cran/fairmodels
* Date/Publication: 2022-08-23 19:50:06 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::cloud_details(, "fairmodels")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(fairmodels)
      > 
      > 
      > test_check("fairmodels")
      Welcome to DALEX (version: 2.4.3).
      Find examples and detailed introduction at: http://ema.drwhy.ai/
    ...
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 312 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test_plot_density.R:14:3'): Test plot_density ─────────────────────
      plt$labels$x not equal to "probability".
      target is NULL, current is character
      
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 312 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) choose_metric.Rd:35: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) choose_metric.Rd:36: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) choose_metric.Rd:37: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) confusion_matrx.Rd:20: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) confusion_matrx.Rd:21: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) confusion_matrx.Rd:22: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) confusion_matrx.Rd:23: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) disparate_impact_remover.Rd:28: Lost braces
        28 | pigeonholing. The number of pigeonholes is fixed and equal to min{101, unique(a)}, where a is vector with values for subgroup. So if some subgroup is not numerous and
           |                                                                  ^
    ...
    checkRd: (-1) group_metric.Rd:30: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) group_metric.Rd:31: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) group_metric.Rd:32: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) group_metric.Rd:33: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) metric_scores.Rd:18: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) metric_scores.Rd:19: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) performance_and_fairness.Rd:20: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) performance_and_fairness.Rd:21: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) performance_and_fairness.Rd:22: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) performance_and_fairness.Rd:23: Lost braces in \itemize; \value handles \item{}{} directly
    ```

# FARS

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/FARS
* Date/Publication: 2025-05-08 10:40:06 UTC
* Number of recursive dependencies: 137

Run `revdepcheck::cloud_details(, "FARS")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘introduction.Rmd’ using rmarkdown
    
    Quitting from introduction.Rmd:174-202 [unnamed-chunk-11]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `pm[[2]]`:
    ! subscript out of bounds
    ---
    ...
    
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    subscript out of bounds
    --- failed re-building ‘introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# faux

<details>

* Version: 1.2.2
* GitHub: https://github.com/debruine/faux
* Source code: https://github.com/cran/faux
* Date/Publication: 2025-01-15 09:10:01 UTC
* Number of recursive dependencies: 128

Run `revdepcheck::cloud_details(, "faux")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘faux-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: norm2likert
    > ### Title: Convert normal to likert
    > ### Aliases: norm2likert
    > 
    > ### ** Examples
    > 
    > 
    ...
      2.   └─plt$build()
      3.     └─private$addLimits(margThemed)
      4.       └─ggplot2::scale_x_continuous(limits = limits, oob = scales::squish)
      5.         └─ggplot2::continuous_scale(...)
      6.           └─ggplot2:::check_continuous_limits(limits, call = call)
      7.             └─ggplot2:::check_numeric(limits, arg = arg, call = call, allow_na = TRUE)
      8.               └─ggplot2:::check_object(x, is.numeric, what, ..., arg = arg, call = call)
      9.                 └─ggplot2:::stop_input_type(...)
     10.                   └─rlang::abort(message, ..., call = call, arg = arg)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(faux)
      
      ************
      Welcome to faux. For support and examples visit:
      https://debruine.github.io/faux/
      - Get and set global package options with: faux_options()
    ...
      Backtrace:
          ▆
       1. └─global expect_equal(p_value_st$facet$params$labeller(df), value_labs) at test-plot_design.R:420:3
       2.   └─testthat::expect_equal(..., check.environment = FALSE)
       3.     └─testthat::quasi_label(enquo(object), label, arg = "object")
       4.       └─rlang::eval_bare(expr, quo_get_env(quo))
      
      [ FAIL 13 | WARN 0 | SKIP 22 | PASS 1354 ]
      Error: Test failures
      Execution halted
    ```

# fdANOVA

<details>

* Version: 0.1.2
* GitHub: NA
* Source code: https://github.com/cran/fdANOVA
* Date/Publication: 2018-08-29 19:54:26 UTC
* Number of recursive dependencies: 69

Run `revdepcheck::cloud_details(, "fdANOVA")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘fdANOVA-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.fanovatests
    > ### Title: Plot P-values of Tests Based on Random Projections for FANOVA
    > ###   Problem
    > ### Aliases: plot.fanovatests
    > ### Keywords: Plot
    > 
    > ### ** Examples
    ...
     20. └─vctrs (local) `<fn>`()
     21.   └─vctrs::vec_default_ptype2(...)
     22.     ├─base::withRestarts(...)
     23.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     24.     │   └─base (local) doWithOneRestart(return(expr), restart)
     25.     └─vctrs::stop_incompatible_type(...)
     26.       └─vctrs:::stop_incompatible(...)
     27.         └─vctrs:::stop_vctrs(...)
     28.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

# feasts

<details>

* Version: 0.4.1
* GitHub: https://github.com/tidyverts/feasts
* Source code: https://github.com/cran/feasts
* Date/Publication: 2024-09-25 23:40:02 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "feasts")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(feasts)
      Loading required package: fabletools
      > 
      > test_check("feasts")
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 101 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test-graphics.R:192:3'): gg_tsdisplay() plots ───────────────────────
      Error in `p + ggplot2::labs(x = "x", y = "y", title = "title")`: non-numeric argument to binary operator
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 101 ]
      Error: Test failures
      Execution halted
    ```

# fgeo.plot

<details>

* Version: 1.1.11
* GitHub: https://github.com/forestgeo/fgeo.plot
* Source code: https://github.com/cran/fgeo.plot
* Date/Publication: 2022-09-03 18:30:02 UTC
* Number of recursive dependencies: 97

Run `revdepcheck::cloud_details(, "fgeo.plot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘fgeo.plot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: autoplot_by_species.sp_elev
    > ### Title: List plots of species distribution and topography (good for pdf
    > ###   output).
    > ### Aliases: autoplot_by_species.sp_elev autoplot_by_species.sp
    > 
    > ### ** Examples
    > 
    ...
      5. │     └─fgeo.plot (local) FUN(X[[i]], ...)
      6. │       └─fgeo.plot:::map_pure_elev(...)
      7. │         └─... %>% ...
      8. ├─fgeo.plot:::best_elev_legend(., hide_color_legend = hide_color_legend)
      9. └─fgeo.plot:::add_elevation_contours(...)
     10.   └─ggplot2::scale_colour_continuous(low = low, high = high)
     11.     └─ggplot2:::scale_backward_compatibility(...)
     12.       └─cli::cli_abort("Unknown scale type: {.val {scale}}")
     13.         └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(fgeo.plot)
      > 
      > test_check("fgeo.plot")
      [ FAIL 6 | WARN 5 | SKIP 0 | PASS 114 ]
      
    ...
        9. ├─fgeo.plot:::best_elev_legend(., hide_color_legend = hide_color_legend)
       10. └─fgeo.plot:::add_elevation_contours(...)
       11.   └─ggplot2::scale_colour_continuous(low = low, high = high)
       12.     └─ggplot2:::scale_backward_compatibility(...)
       13.       └─cli::cli_abort("Unknown scale type: {.val {scale}}")
       14.         └─rlang::abort(...)
      
      [ FAIL 6 | WARN 5 | SKIP 0 | PASS 114 ]
      Error: Test failures
      Execution halted
    ```

# fitdistrplus

<details>

* Version: 1.2-2
* GitHub: https://github.com/lbbe-software/fitdistrplus
* Source code: https://github.com/cran/fitdistrplus
* Date/Publication: 2025-01-07 16:00:02 UTC
* Number of recursive dependencies: 110

Run `revdepcheck::cloud_details(, "fitdistrplus")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘fitdistrplus-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: CIcdfplot
    > ### Title: Empirical cumulative distribution function with pointwise
    > ###   confidence intervals on probabilities or on quantiles
    > ### Aliases: CIcdfplot
    > ### Keywords: distribution
    > 
    > ### ** Examples
    ...
     20. └─vctrs (local) `<fn>`()
     21.   └─vctrs::vec_default_ptype2(...)
     22.     ├─base::withRestarts(...)
     23.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     24.     │   └─base (local) doWithOneRestart(return(expr), restart)
     25.     └─vctrs::stop_incompatible_type(...)
     26.       └─vctrs:::stop_incompatible(...)
     27.         └─vctrs:::stop_vctrs(...)
     28.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘t-CIcdfplot.R’
    Running the tests in ‘tests/t-CIcdfplot.R’ failed.
    Complete output:
      > require("fitdistrplus")
      Loading required package: fitdistrplus
      Loading required package: MASS
      Loading required package: survival
      > 
      > nbboot <- 201
      > nbboot <- 10
    ...
       26.       └─vctrs:::stop_incompatible(...)
       27.         └─vctrs:::stop_vctrs(...)
       28.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
      Warning message:
      Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
      ℹ Please use `linewidth` instead.
      ℹ The deprecated feature was likely used in the fitdistrplus package.
        Please report the issue at
        <https://github.com/lbbe-software/fitdistrplus/issues>. 
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘FAQ.Rmd’ using rmarkdown
    ```

# flipr

<details>

* Version: 0.3.3
* GitHub: https://github.com/LMJL-Alea/flipr
* Source code: https://github.com/cran/flipr
* Date/Publication: 2023-08-23 09:00:02 UTC
* Number of recursive dependencies: 105

Run `revdepcheck::cloud_details(, "flipr")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘alternative.Rmd’ using rmarkdown
    --- finished re-building ‘alternative.Rmd’
    
    --- re-building ‘exactness.Rmd’ using rmarkdown
    
    Quitting from exactness.Rmd:141-177 [unnamed-chunk-1]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `pm[[2]]`:
    ...
     5. ├─plotly::ggplotly(.)
     6. └─plotly:::ggplotly.ggplot(.)
     7.   └─plotly::gg2list(...)
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'exactness.Rmd' failed with diagnostics:
    subscript out of bounds
    --- failed re-building ‘exactness.Rmd’
    
    --- re-building ‘flipr.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.2Mb
      sub-directories of 1Mb or more:
        doc    9.1Mb
        libs   1.4Mb
    ```

# foqat

<details>

* Version: 2.0.8.2
* GitHub: https://github.com/tianshu129/foqat
* Source code: https://github.com/cran/foqat
* Date/Publication: 2023-09-30 06:10:02 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::cloud_details(, "foqat")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Air_Quality.Rmd’ using rmarkdown
    --- finished re-building ‘Air_Quality.Rmd’
    
    --- re-building ‘Atmospheric_Radiation.Rmd’ using rmarkdown
    --- finished re-building ‘Atmospheric_Radiation.Rmd’
    
    --- re-building ‘Basic_Functions.Rmd’ using rmarkdown
    --- finished re-building ‘Basic_Functions.Rmd’
    
    --- re-building ‘Particle_Size_Distribution.Rmd’ using rmarkdown
    ```

# forestPSD

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/forestPSD
* Date/Publication: 2024-11-11 16:50:05 UTC
* Number of recursive dependencies: 45

Run `revdepcheck::cloud_details(, "forestPSD")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘forestPSD-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: psdfun
    > ### Title: Regression analysis for survival curves.
    > ### Aliases: psdfun
    > 
    > ### ** Examples
    > 
    > data(Npop)
    ...
     13. │       └─l$compute_geom_2(d, theme = plot$theme)
     14. │         └─ggplot2 (local) compute_geom_2(..., self = self)
     15. │           └─self$geom$use_defaults(...)
     16. │             └─ggplot2 (local) use_defaults(..., self = self)
     17. │               └─ggplot2:::check_aesthetics(new_params, nrow(data))
     18. │                 └─vctrs::list_sizes(x)
     19. └─vctrs:::stop_scalar_type(`<fn>`(`<expression>`), "x$label", `<env>`)
     20.   └─vctrs:::stop_vctrs(...)
     21.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggplot2’ ‘reshape2’
      All declared Imports should be used.
    ```

# frailtyEM

<details>

* Version: 1.0.1
* GitHub: https://github.com/tbalan/frailtyEM
* Source code: https://github.com/cran/frailtyEM
* Date/Publication: 2019-09-22 13:00:10 UTC
* Number of recursive dependencies: 76

Run `revdepcheck::cloud_details(, "frailtyEM")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘frailtyEM-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: summary.emfrail
    > ### Title: Summary for 'emfrail' objects
    > ### Aliases: summary.emfrail
    > 
    > ### ** Examples
    > 
    > data("bladder")
    ...
        filter
    
    The following object is masked from ‘package:graphics’:
    
        layout
    
    > ggplotly(pl2)
    Error in pm[[2]] : subscript out of bounds
    Calls: ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘frailtyEM_manual.Rnw’ using Sweave
    Loading required package: survival
    Loading required package: gridExtra
    Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
    ℹ Please use tidy evaluation idioms with `aes()`.
    ℹ See also `vignette("ggplot2-in-packages")` for more
      information.
    ℹ The deprecated feature was likely used in the frailtyEM
      package.
    ...
    l.179   \RequirePackage{grfext}\relax
                                         ^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘frailtyEM_manual.Rnw’
    
    SUMMARY: processing the following file failed:
      ‘frailtyEM_manual.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) emfrail_control.Rd:49: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) emfrail_control.Rd:50: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) emfrail_control.Rd:51: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) emfrail_control.Rd:52: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) emfrail_control.Rd:53-54: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) emfrail_control.Rd:55-57: Lost braces in \itemize; meant \describe ?
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# func2vis

<details>

* Version: 1.0-3
* GitHub: NA
* Source code: https://github.com/cran/func2vis
* Date/Publication: 2023-03-16 17:30:02 UTC
* Number of recursive dependencies: 114

Run `revdepcheck::cloud_details(, "func2vis")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘func2vis-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_pathways
    > ### Title: Plot clean enriched pathways as a bubble plot
    > ### Aliases: plot_pathways
    > 
    > ### ** Examples
    > 
    > data("t.tests.treatment.sign")
    ...
    Error in `scale_backward_compatibility()`:
    ! Unknown scale type:
    Backtrace:
        ▆
     1. └─func2vis::plot_pathways(revised_pathway)
     2.   └─ggplot2::scale_color_continuous(...)
     3.     └─ggplot2:::scale_backward_compatibility(...)
     4.       └─cli::cli_abort("Unknown scale type: {.val {scale}}")
     5.         └─rlang::abort(...)
    Execution halted
    ```

# gapminder

<details>

* Version: 1.0.0
* GitHub: https://github.com/jennybc/gapminder
* Source code: https://github.com/cran/gapminder
* Date/Publication: 2023-03-10 09:50:08 UTC
* Number of recursive dependencies: 63

Run `revdepcheck::cloud_details(, "gapminder")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gapminder-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: country_colors
    > ### Title: Gapminder color schemes.
    > ### Aliases: country_colors continent_colors
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    ...
      and is now defunct.
    ℹ Please use the `show.legend` argument instead.
    Backtrace:
        ▆
     1. └─ggplot2::geom_line(lwd = 1, show_guide = FALSE)
     2.   └─ggplot2::layer(...)
     3.     └─lifecycle::deprecate_stop("2.0.0", "layer(show_guide)", "layer(show.legend)")
     4.       └─lifecycle:::deprecate_stop0(msg)
     5.         └─rlang::cnd_signal(...)
    Execution halted
    ```

# GCalignR

<details>

* Version: 1.0.7
* GitHub: https://github.com/mottensmann/GCalignR
* Source code: https://github.com/cran/GCalignR
* Date/Publication: 2024-07-03 18:00:01 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::cloud_details(, "GCalignR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘GCalignR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gc_heatmap
    > ### Title: Visualises peak alignments in form of a heatmap
    > ### Aliases: gc_heatmap
    > 
    > ### ** Examples
    > 
    > 
    ...
    Error in `scale_backward_compatibility()`:
    ! Unknown scale type:
    Backtrace:
        ▆
     1. └─GCalignR::gc_heatmap(aligned_peak_data, algorithm_step = "aligned")
     2.   └─ggplot2::scale_fill_continuous(...)
     3.     └─ggplot2:::scale_backward_compatibility(...)
     4.       └─cli::cli_abort("Unknown scale type: {.val {scale}}")
     5.         └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(GCalignR)
      > 
      > test_check("GCalignR")
      Run GCalignR
      Start: 2025-05-20 10:24:04
      
    ...
          ▆
       1. └─GCalignR::gc_heatmap(x) at test-gc_heatmap.R:6:1
       2.   └─ggplot2::scale_fill_continuous(...)
       3.     └─ggplot2:::scale_backward_compatibility(...)
       4.       └─cli::cli_abort("Unknown scale type: {.val {scale}}")
       5.         └─rlang::abort(...)
      
      [ FAIL 1 | WARN 2 | SKIP 1 | PASS 32 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘GCalignR_How_does_the_Algorithm_work.Rmd’ using rmarkdown
    ```

# geneSLOPE

<details>

* Version: 0.38.2
* GitHub: https://github.com/psobczyk/geneSLOPE
* Source code: https://github.com/cran/geneSLOPE
* Date/Publication: 2023-08-16 09:12:37 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::cloud_details(, "geneSLOPE")` for more info

</details>

## Newly broken

*   checking whether package ‘geneSLOPE’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
    See ‘/tmp/workdir/geneSLOPE/new/geneSLOPE.Rcheck/00install.out’ for details.
    ```

# geoheatmap

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/geoheatmap
* Date/Publication: 2024-09-05 15:40:02 UTC
* Number of recursive dependencies: 103

Run `revdepcheck::cloud_details(, "geoheatmap")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘geoheatmap.Rmd’ using rmarkdown
    
    Quitting from geoheatmap.Rmd:75-81 [unnamed-chunk-5]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'geoheatmap.Rmd' failed with diagnostics:
    Unknown scale type:
    --- failed re-building ‘geoheatmap.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘geoheatmap.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# geomtextpath

<details>

* Version: 0.1.5
* GitHub: https://github.com/AllanCameron/geomtextpath
* Source code: https://github.com/cran/geomtextpath
* Date/Publication: 2025-01-14 17:40:02 UTC
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "geomtextpath")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘geomtextpath-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_textsf
    > ### Title: Visualise sf objects with labels
    > ### Aliases: geom_textsf geom_labelsf
    > 
    > ### ** Examples
    > 
    > ggplot(waterways) +
    ...
     19. │                   ├─rlang::inject(self$draw_panel(data, panel_params, coord, !!!params))
     20. │                   └─self$draw_panel(...)
     21. │                     └─geomtextpath (local) draw_panel(...)
     22. │                       └─geomtextpath:::sf_textgrob(...)
     23. └─base::.handleSimpleError(...)
     24.   └─rlang (local) h(simpleError(msg, call))
     25.     └─handlers[[1L]](cnd)
     26.       └─cli::cli_abort(...)
     27.         └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(geomtextpath)
      Loading required package: ggplot2
      > 
      > test_check("geomtextpath")
      [ FAIL 2 | WARN 0 | SKIP 4 | PASS 462 ]
      
    ...
      `expected` is an expression vector
      ── Error ('test-sf.R:91:3'): We can make grobs from sf features ────────────────
      Error in `(x$boxlinewidth %||% defaults$linewidth[type_ind]) * 3.779528`: non-numeric argument to binary operator
      Backtrace:
          ▆
       1. └─geomtextpath:::sf_textgrob(river, as_textbox = TRUE) at test-sf.R:91:3
      
      [ FAIL 2 | WARN 0 | SKIP 4 | PASS 462 ]
      Error: Test failures
      Execution halted
    ```

# gg1d

<details>

* Version: 0.1.0
* GitHub: https://github.com/selkamand/gg1d
* Source code: https://github.com/cran/gg1d
* Date/Publication: 2024-12-09 19:40:02 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "gg1d")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gg1d-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gg1d
    > ### Title: AutoPlot an entire data.frame
    > ### Aliases: gg1d
    > 
    > ### ** Examples
    > 
    > path_gg1d <- system.file("example.csv", package = "gg1d")
    ...
     28. │                             └─base::lapply(...)
     29. │                               └─ggplot2 (local) FUN(X[[i]], ...)
     30. │                                 ├─rlang::inject(self$draw_panel(data, panel_params, coord, !!!params))
     31. │                                 └─self$draw_panel(...)
     32. └─base::.handleSimpleError(...)
     33.   └─rlang (local) h(simpleError(msg, call))
     34.     └─handlers[[1L]](cnd)
     35.       └─cli::cli_abort(...)
     36.         └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/tests.html
      > # * https://testthat.r-lib.org/reference/test_package.html#special-files
    ...
      Expected `gg1d(...)` to run without any errors.
      i Actually got a <rlang_error> with text:
        Problem while converting geom to grob.
        i Error occurred in the 1st layer.
        Caused by error in `draw_panel()`:
        ! unused arguments (lineend = "butt", linejoin = "mitre")
      
      [ FAIL 9 | WARN 0 | SKIP 2 | PASS 47 ]
      Error: Test failures
      Execution halted
    ```

# ggalign

<details>

* Version: 1.0.2
* GitHub: https://github.com/Yunuuuu/ggalign
* Source code: https://github.com/cran/ggalign
* Date/Publication: 2025-05-14 14:00:08 UTC
* Number of recursive dependencies: 57

Run `revdepcheck::cloud_details(, "ggalign")` for more info

</details>

## Newly broken

*   checking for code/documentation mismatches ... WARNING
    ```
    Codoc mismatches from Rd file 'scheme_theme.Rd':
    scheme_theme
      Code: function(..., line, rect, text, title, point, polygon, geom,
                     spacing, margins, aspect.ratio, axis.title,
                     axis.title.x, axis.title.x.top, axis.title.x.bottom,
                     axis.title.y, axis.title.y.left, axis.title.y.right,
                     axis.text, axis.text.x, axis.text.x.top,
                     axis.text.x.bottom, axis.text.y, axis.text.y.left,
                     axis.text.y.right, axis.text.theta, axis.text.r,
                     axis.ticks, axis.ticks.x, axis.ticks.x.top,
    ...
                     strip.text.y.right, strip.switch.pad.grid,
                     strip.switch.pad.wrap, complete = FALSE, validate =
                     TRUE)
      Argument names in code not in docs:
        point polygon geom spacing margins legend.key.justification
        panel.widths panel.heights
      Mismatches in argument names (first 3):
        Position: 6 Code: point Docs: aspect.ratio
        Position: 7 Code: polygon Docs: axis.title
        Position: 8 Code: geom Docs: axis.title.x
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      'patchwork', 'ggrastr', 'maftools'
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘ape’, ‘patchwork’, ‘maftools’, ‘ComplexHeatmap’, ‘pheatmap’
    ```

# GGally

<details>

* Version: 2.2.1
* GitHub: https://github.com/ggobi/ggally
* Source code: https://github.com/cran/GGally
* Date/Publication: 2024-02-14 00:53:32 UTC
* Number of recursive dependencies: 144

Run `revdepcheck::cloud_details(, "GGally")` for more info

</details>

## Newly broken

*   checking whether package ‘GGally’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/GGally/new/GGally.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘GGally’ ...
** package ‘GGally’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in eval(exprs[i], envir) : object 'is.rel' not found
Error: unable to load R code in package ‘GGally’
Execution halted
ERROR: lazy loading failed for package ‘GGally’
* removing ‘/tmp/workdir/GGally/new/GGally.Rcheck/GGally’


```
### CRAN

```
* installing *source* package ‘GGally’ ...
** package ‘GGally’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (GGally)


```
# gganimate

<details>

* Version: 1.0.9
* GitHub: https://github.com/thomasp85/gganimate
* Source code: https://github.com/cran/gganimate
* Date/Publication: 2024-02-27 14:00:03 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::cloud_details(, "gganimate")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(gganimate)
      Loading required package: ggplot2
      > 
      > test_check("gganimate")
      [ FAIL 2 | WARN 4 | SKIP 1 | PASS 4 ]
      
    ...
       22.                       └─self$extract_key(...)
       23.                         └─ggplot2 (local) extract_key(...)
       24.                           └─Guide$extract_key(scale, aesthetic, ...)
       25.                             └─ggplot2 (local) extract_key(...)
       26.                               └─scale$map(breaks)
       27.                                 └─ggplot2 (local) map(..., self = self)
      
      [ FAIL 2 | WARN 4 | SKIP 1 | PASS 4 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘gganimate.Rmd’ using rmarkdown
    ```

# ggborderline

<details>

* Version: 0.2.0
* GitHub: https://github.com/wurli/ggborderline
* Source code: https://github.com/cran/ggborderline
* Date/Publication: 2022-10-25 13:45:14 UTC
* Number of recursive dependencies: 43

Run `revdepcheck::cloud_details(, "ggborderline")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggborderline-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_borderpath
    > ### Title: Connect observations
    > ### Aliases: geom_borderpath geom_borderline geom_borderstep
    > 
    > ### ** Examples
    > 
    > require(ggplot2)
    ...
      2. └─ggplot2:::print.ggplot(x)
      3.   ├─ggplot2::ggplot_build(x)
      4.   └─ggplot2:::ggplot_build.ggplot(x)
      5.     └─npscales$set_palettes(plot$theme)
      6.       └─ggplot2 (local) set_palettes(..., self = self)
      7.         ├─scales::as_discrete_pal(elem)
      8.         └─scales:::as_discrete_pal.default(elem)
      9.           └─cli::cli_abort("Cannot convert {.arg x} to a discrete palette.")
     10.             └─rlang::abort(...)
    Execution halted
    ```

# ggcorset

<details>

* Version: 0.5.0
* GitHub: https://github.com/kbelisar/ggcorset
* Source code: https://github.com/cran/ggcorset
* Date/Publication: 2024-04-07 13:33:01 UTC
* Number of recursive dependencies: 51

Run `revdepcheck::cloud_details(, "ggcorset")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘corset_plot_intro.Rmd’ using rmarkdown
    
    Quitting from corset_plot_intro.Rmd:35-73 [unnamed-chunk-1]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'corset_plot_intro.Rmd' failed with diagnostics:
    Can't combine `..1` <palette> and `..2` <character>.
    --- failed re-building ‘corset_plot_intro.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘corset_plot_intro.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ggdark

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/ggdark
* Date/Publication: 2019-01-11 17:30:06 UTC
* Number of recursive dependencies: 43

Run `revdepcheck::cloud_details(, "ggdark")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggdark-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: dark_mode
    > ### Title: Activate dark mode on a 'ggplot2' theme
    > ### Aliases: dark_mode
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
    > 
    > p1 <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
    +   geom_point()
    > 
    > p1  # theme returned by theme_get()
    > p1 + dark_mode()  # activate dark mode on theme returned by theme_get()
    Error in match(x, table, nomatch = 0L) : 
      'match' requires vector arguments
    Calls: dark_mode -> %in%
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggdark)
      > 
      > test_check("ggdark")
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 0 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      ── Error ('test_dark_mode.R:10:1'): (code run outside of `test_that()`) ────────
      Error in `match(x, table, nomatch = 0L)`: 'match' requires vector arguments
      Backtrace:
          ▆
       1. └─ggdark::dark_mode(light_theme) at test_dark_mode.R:10:1
       2.   └─geoms[["GeomPoint"]]$default_aes$colour %in% ...
      
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# ggdemetra

<details>

* Version: 0.2.8
* GitHub: https://github.com/AQLT/ggdemetra
* Source code: https://github.com/cran/ggdemetra
* Date/Publication: 2024-02-04 14:50:02 UTC
* Number of recursive dependencies: 52

Run `revdepcheck::cloud_details(, "ggdemetra")` for more info

</details>

## Newly broken

*   checking whether package ‘ggdemetra’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: `aes_()` was deprecated in ggplot2 3.0.0.
    See ‘/tmp/workdir/ggdemetra/new/ggdemetra.Rcheck/00install.out’ for details.
    ```

# ggDoubleHeat

<details>

* Version: 0.1.2
* GitHub: https://github.com/PursuitOfDataScience/ggDoubleHeat
* Source code: https://github.com/cran/ggDoubleHeat
* Date/Publication: 2023-08-24 21:00:04 UTC
* Number of recursive dependencies: 56

Run `revdepcheck::cloud_details(, "ggDoubleHeat")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggDoubleHeat-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_heat_circle
    > ### Title: Heatcircle
    > ### Aliases: geom_heat_circle
    > 
    > ### ** Examples
    > 
    > 
    ...
    +                    y = rep(c("d", "e", "f"), 3),
    +                    outside_values = rep(c(1,5,7),3),
    +                    inside_values = rep(c(2,3,4),3))
    > 
    > ggplot(data, aes(x,y)) +
    + geom_heat_circle(outside = outside_values,
    +                  inside = inside_values)
    Error in scale_title(title) : object 'outside_values' not found
    Calls: <Anonymous> ... extract_params -> <Anonymous> -> make_title -> scale_title
    Execution halted
    ```

# ggEDA

<details>

* Version: 0.1.0
* GitHub: https://github.com/CCICB/ggEDA
* Source code: https://github.com/cran/ggEDA
* Date/Publication: 2025-05-07 12:00:02 UTC
* Number of recursive dependencies: 93

Run `revdepcheck::cloud_details(, "ggEDA")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggEDA-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggstack
    > ### Title: AutoPlot an entire data.frame
    > ### Aliases: ggstack
    > 
    > ### ** Examples
    > 
    > 
    ...
     28. │                             └─base::lapply(...)
     29. │                               └─ggplot2 (local) FUN(X[[i]], ...)
     30. │                                 ├─rlang::inject(self$draw_panel(data, panel_params, coord, !!!params))
     31. │                                 └─self$draw_panel(...)
     32. └─base::.handleSimpleError(...)
     33.   └─rlang (local) h(simpleError(msg, call))
     34.     └─handlers[[1L]](cnd)
     35.       └─cli::cli_abort(...)
     36.         └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/tests.html
      > # * https://testthat.r-lib.org/reference/test_package.html#special-files
    ...
      Expected `ggstack(...)` to run without any errors.
      i Actually got a <rlang_error> with text:
        Problem while converting geom to grob.
        i Error occurred in the 1st layer.
        Caused by error in `draw_panel()`:
        ! unused arguments (lineend = "butt", linejoin = "mitre")
      
      [ FAIL 11 | WARN 9 | SKIP 2 | PASS 114 ]
      Error: Test failures
      Execution halted
    ```

# ggedit

<details>

* Version: 0.4.1
* GitHub: https://github.com/yonicd/ggedit
* Source code: https://github.com/cran/ggedit
* Date/Publication: 2024-03-04 14:40:02 UTC
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "ggedit")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggedit-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: compare
    > ### Title: compare
    > ### Aliases: compare
    > 
    > ### ** Examples
    > 
    > compare(ggplot2::theme_bw(),ggplot2::theme_get())
    [1] "theme(panel.background=element_rect(fill='white'),panel.grid=element_line(arrow.fill=''#'*EBEBEBFF',colour=''#'*EBEBEBFF'),strip.background=element_rect(colour=''#'*333333FF'))"
    > compare(ggplot2::theme_bw(),ggplot2::theme_get(),verbose=FALSE)
    Error in parse(text = out) : <text>:2:0: unexpected end of input
    1: theme(panel.background=element_rect(fill='white'),panel.grid=element_line(arrow.fill=''#'*EBEBEBFF',colour=''#'*EBEBEBFF'),strip.background=element_rect(colour=''#'*333333FF'))
       ^
    Calls: compare -> eval -> parse
    Execution halted
    ```

# ggenealogy

<details>

* Version: 1.0.3
* GitHub: NA
* Source code: https://github.com/cran/ggenealogy
* Date/Publication: 2024-02-21 16:00:02 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "ggenealogy")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggenealogy-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotDegMatrix
    > ### Title: Returns the image object to show the heat map of degrees between
    > ###   the inputted set of vertices
    > ### Aliases: plotDegMatrix
    > 
    > ### ** Examples
    > 
    ...
    > p + ggplot2::scale_fill_continuous(low = "white", high = "darkgreen")
    Error in `scale_backward_compatibility()`:
    ! Unknown scale type:
    Backtrace:
        ▆
     1. └─ggplot2::scale_fill_continuous(low = "white", high = "darkgreen")
     2.   └─ggplot2:::scale_backward_compatibility(...)
     3.     └─cli::cli_abort("Unknown scale type: {.val {scale}}")
     4.       └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ggenealogy.Rnw’ using Sweave
    Warning: `qplot()` was deprecated in ggplot2 3.4.0.
    ℹ The deprecated feature was likely used in the ggenealogy
      package.
      Please report the issue to the authors.
    Warning: Using `size` aesthetic for lines was deprecated in ggplot2
    3.4.0.
    ℹ Please use `linewidth` instead.
    ℹ The deprecated feature was likely used in the ggenealogy
    ...
    Error in scale_backward_compatibility(..., guide = guide, na.value = na.value,  : 
      Unknown scale type:
    
    --- failed re-building ‘ggenealogy.Rnw’
    
    SUMMARY: processing the following file failed:
      ‘ggenealogy.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## Newly fixed

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ggenealogy.Rnw’ using Sweave
    Warning: Removed 1 row containing missing values or values outside
    the scale range (`geom_segment()`).
    Warning: Removed 1 row containing missing values or values outside
    the scale range (`geom_segment()`).
    Warning: Removed 1 row containing missing values or values outside
    the scale range (`geom_segment()`).
    Warning: Removed 1 row containing missing values or values outside
    the scale range (`geom_segment()`).
    ...
    l.4 \usepackage
                   [T1]{fontenc}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘ggenealogy.Rnw’
    
    SUMMARY: processing the following file failed:
      ‘ggenealogy.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2356 marked UTF-8 strings
    ```

# ggfixest

<details>

* Version: 0.3.0
* GitHub: https://github.com/grantmcdermott/ggfixest
* Source code: https://github.com/cran/ggfixest
* Date/Publication: 2025-05-14 02:20:02 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::cloud_details(, "ggfixest")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘tinytest.R’
    Running the tests in ‘tests/tinytest.R’ failed.
    Complete output:
      > ## Throttle CPU threads if R CMD check (for CRAN)
      > 
      > if (any(grepl("_R_CHECK", names(Sys.getenv()), fixed = TRUE))) {
      +     # fixest
      +     if (requireNamespace("fixest", quietly = TRUE)) {
      +         library(fixest)
      +         setFixest_nthreads(1)
    ...
       call| expect_snapshot_plot(p19a, label = "ggiplot_multi_complex_kitchen_iid")
       diff| 1774
       info| Diff plot saved to: _tinysnapshot_review/ggiplot_multi_complex_kitchen_iid.png
      ----- FAILED[]: test_ggiplot.R<193--193>
       call| expect_snapshot_plot(p19b, label = "ggiplot_multi_complex_kitchen_iid")
       diff| 1774
       info| Diff plot saved to: _tinysnapshot_review/ggiplot_multi_complex_kitchen_iid.png
      Error: 5 out of 111 tests failed
      In addition: There were 11 warnings (use warnings() to see them)
      Execution halted
    ```

# ggfocus

<details>

* Version: 1.0.0
* GitHub: https://github.com/Freguglia/ggfocus
* Source code: https://github.com/cran/ggfocus
* Date/Publication: 2020-01-23 13:20:02 UTC
* Number of recursive dependencies: 53

Run `revdepcheck::cloud_details(, "ggfocus")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggfocus-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggfocus
    > ### Title: (Deprecated) Sets focus scales to an existing 'ggplot' object
    > ### Aliases: ggfocus
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
    > p <- ggplot(iris,aes(x=Sepal.Length,y=Petal.Length)) + geom_point()
    > ggfocus(p, Species, "versicolor")
    The function 'ggfocus()' is deprecated, consider using the family scale_*_focus() instead.
    Warning: `select_()` was deprecated in dplyr 0.7.0.
    ℹ Please use `select()` instead.
    ℹ The deprecated feature was likely used in the ggfocus package.
      Please report the issue at <https://github.com/Freguglia/ggfocus/issues>.
    Error in scale_title(title) : object 'Species' not found
    Calls: <Anonymous> ... extract_params -> <Anonymous> -> make_title -> scale_title
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# ggfootball

<details>

* Version: 0.2.1
* GitHub: https://github.com/aymennasri/ggfootball
* Source code: https://github.com/cran/ggfootball
* Date/Publication: 2025-03-22 23:00:02 UTC
* Number of recursive dependencies: 110

Run `revdepcheck::cloud_details(, "ggfootball")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggfootball-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: xg_map
    > ### Title: Plot shots xG of a football match
    > ### Aliases: xg_map
    > 
    > ### ** Examples
    > 
    > xg_map(26631, title = "xG Map")
    Error in self$params$labeller(labels_data) : 
      attempt to apply non-function
    Calls: xg_map ... <Anonymous> -> render -> <Anonymous> -> draw_panels
    Execution halted
    ```

# ggforce

<details>

* Version: 0.4.2
* GitHub: https://github.com/thomasp85/ggforce
* Source code: https://github.com/cran/ggforce
* Date/Publication: 2024-02-19 11:00:02 UTC
* Number of recursive dependencies: 68

Run `revdepcheck::cloud_details(, "ggforce")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggforce-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: facet_matrix
    > ### Title: Facet by different data columns
    > ### Aliases: facet_matrix
    > 
    > ### ** Examples
    > 
    > # Standard use:
    > ggplot(mpg) +
    +   geom_point(aes(x = .panel_x, y = .panel_y)) +
    +   facet_matrix(vars(displ, cty, hwy))
    Error in match.fun(params$labeller) : 
      'params$labeller' is not a function, character or symbol
    Calls: <Anonymous> ... attach_strips -> <Anonymous> -> format_strip_labels -> match.fun
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 27.8Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        help   1.2Mb
        libs  25.0Mb
    ```

# ggformula

<details>

* Version: 0.12.0
* GitHub: https://github.com/ProjectMOSAIC/ggformula
* Source code: https://github.com/cran/ggformula
* Date/Publication: 2023-11-09 12:30:07 UTC
* Number of recursive dependencies: 120

Run `revdepcheck::cloud_details(, "ggformula")` for more info

</details>

## Newly broken

*   checking for code/documentation mismatches ... WARNING
    ```
    Codoc mismatches from Rd file 'gf_lines.Rd':
    gf_hline
      Code: function(object = NULL, gformula = NULL, data = NULL, ...,
                     yintercept, color, linetype, linewidth, alpha, xlab,
                     ylab, title, subtitle, caption, position = "identity",
                     show.legend = NA, show.help = NULL, inherit = FALSE,
                     environment = parent.frame())
      Docs: function(object = NULL, gformula = NULL, data = NULL, ...,
                     yintercept, color, linetype, linewidth, alpha, xlab,
                     ylab, title, subtitle, caption, show.legend = NA,
    ...
                     xintercept, color, linetype, linewidth, alpha, xlab,
                     ylab, title, subtitle, caption, show.legend = NA,
                     show.help = NULL, inherit = FALSE, environment =
                     parent.frame())
      Argument names in code not in docs:
        position
      Mismatches in argument names (first 3):
        Position: 15 Code: position Docs: show.legend
        Position: 16 Code: show.legend Docs: show.help
        Position: 17 Code: show.help Docs: inherit
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘akima’, ‘ggforce’
    ```

# ggfortify

<details>

* Version: 0.4.17
* GitHub: https://github.com/sinhrks/ggfortify
* Source code: https://github.com/cran/ggfortify
* Date/Publication: 2024-04-17 04:30:04 UTC
* Number of recursive dependencies: 122

Run `revdepcheck::cloud_details(, "ggfortify")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Complete output:
      > library(testthat)
      > 
      > suppressWarnings(RNGversion("3.5.0"))
      > set.seed(1, sample.kind = "Rejection")
      > 
      > test_check('ggfortify')
      Loading required package: ggfortify
    ...
      
      x[3]: "#595959FF"
      y[3]: "grey35"
      
      x[4]: "#595959FF"
      y[4]: "grey35"
      
      [ FAIL 5 | WARN 13 | SKIP 48 | PASS 734 ]
      Error: Test failures
      Execution halted
    ```

# ggfun

<details>

* Version: 0.1.8
* GitHub: https://github.com/YuLab-SMU/ggfun
* Source code: https://github.com/cran/ggfun
* Date/Publication: 2024-12-03 10:20:02 UTC
* Number of recursive dependencies: 60

Run `revdepcheck::cloud_details(, "ggfun")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ggfun.Rmd’ using rmarkdown
    ```

# gggenomes

<details>

* Version: 1.0.1
* GitHub: https://github.com/thackl/gggenomes
* Source code: https://github.com/cran/gggenomes
* Date/Publication: 2024-08-30 11:40:02 UTC
* Number of recursive dependencies: 110

Run `revdepcheck::cloud_details(, "gggenomes")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gggenomes-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pick
    > ### Title: Pick bins and seqs by name or position
    > ### Aliases: pick pick_seqs pick_seqs_within pick_by_tree
    > 
    > ### ** Examples
    > 
    > s0 <- tibble::tibble(
    ...
     24. │                           ├─rlang::inject(self$draw_panel(data, panel_params, coord, !!!params))
     25. │                           └─self$draw_panel(...)
     26. │                             └─ggtree (local) draw_panel(...)
     27. │                               └─ggplot2:::empty(data)
     28. └─base::.handleSimpleError(...)
     29.   └─rlang (local) h(simpleError(msg, call))
     30.     └─handlers[[1L]](cnd)
     31.       └─cli::cli_abort(...)
     32.         └─rlang::abort(...)
    Execution halted
    ```

# ggh4x

<details>

* Version: 0.3.0
* GitHub: https://github.com/teunbrand/ggh4x
* Source code: https://github.com/cran/ggh4x
* Date/Publication: 2024-12-15 17:20:02 UTC
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "ggh4x")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggh4x-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: guide_stringlegend
    > ### Title: String legend
    > ### Aliases: guide_stringlegend
    > 
    > ### ** Examples
    > 
    > p <- ggplot(mpg, aes(displ, hwy)) +
    +   geom_point(aes(colour = manufacturer))
    > 
    > # String legend can be set in the `guides()` function
    > p + guides(colour = guide_stringlegend(ncol = 2))
    Error in get_layer_key(...) : 
      unused argument (list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", 19, 1.5, 0.5, TRUE), list("white", "black", 0.5, 1, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, NULL, NULL, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, 
        NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "#4D4D4DFF", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL,
    Calls: <Anonymous> ... <Anonymous> -> <Anonymous> -> process_layers -> <Anonymous>
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggh4x)
      Loading required package: ggplot2
      > 
      > test_check("ggh4x")
      [ FAIL 2 | WARN 296 | SKIP 15 | PASS 661 ]
      
    ...
       11.             └─base::Map(...)
       12.               └─base::mapply(FUN = f, ..., SIMPLIFY = FALSE)
       13.                 └─ggplot2 (local) `<fn>`(guide = dots[[1L]][[1L]], param = dots[[2L]][[1L]])
       14.                   └─guide$process_layers(param, layers, data, theme)
       15.                     └─ggplot2 (local) process_layers(..., self = self)
       16.                       └─self$get_layer_key(params, layers[include], data[include], theme)
      
      [ FAIL 2 | WARN 296 | SKIP 15 | PASS 661 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Facets.Rmd’ using rmarkdown
    ```

# gghalves

<details>

* Version: 0.1.4
* GitHub: https://github.com/erocoar/gghalves
* Source code: https://github.com/cran/gghalves
* Date/Publication: 2022-11-20 11:40:02 UTC
* Number of recursive dependencies: 50

Run `revdepcheck::cloud_details(, "gghalves")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gghalves-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_half_point
    > ### Title: Points with jitter for half geoms.
    > ### Aliases: geom_half_point
    > 
    > ### ** Examples
    > 
    > ggplot(iris, aes(x = Species, y = Petal.Width, fill = Species)) + 
    ...
     29. │                                     └─base::lapply(...)
     30. │                                       └─ggplot2 (local) FUN(X[[i]], ...)
     31. │                                         └─ggplot2 (local) apply_fun(cur_data)
     32. │                                           └─ggplot2 (local) fun(x, ...)
     33. └─base::.handleSimpleError(...)
     34.   └─rlang (local) h(simpleError(msg, call))
     35.     └─handlers[[1L]](cnd)
     36.       └─cli::cli_abort(...)
     37.         └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘gghalves.Rmd’ using rmarkdown
    
    Quitting from gghalves.Rmd:39-42 [unnamed-chunk-2]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'gghalves.Rmd' failed with diagnostics:
    ...
    ℹ Error occurred in the 1st layer.
    Caused by error in `fun()`:
    ! argument "layout" is missing, with no default
    --- failed re-building ‘gghalves.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘gghalves.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘grDevices’ ‘gtable’
      All declared Imports should be used.
    ```

# gghighlight

<details>

* Version: 0.4.1
* GitHub: https://github.com/yutannihilation/gghighlight
* Source code: https://github.com/cran/gghighlight
* Date/Publication: 2023-12-16 01:00:02 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::cloud_details(, "gghighlight")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gghighlight-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gghighlight
    > ### Title: Highlight Data With Predicate
    > ### Aliases: gghighlight
    > 
    > ### ** Examples
    > 
    > d <- data.frame(
    ...
      8. │           ├─purrr:::with_indexed_errors(...)
      9. │           │ └─base::withCallingHandlers(...)
     10. │           ├─purrr:::call_with_cleanup(...)
     11. │           └─gghighlight (local) .f(.x[[i]], .y[[i]], ...)
     12. │             └─gghighlight:::get_default_aes_param(nm, layer$geom, layer$mapping)
     13. └─base::.handleSimpleError(...)
     14.   └─purrr (local) h(simpleError(msg, call))
     15.     └─cli::cli_abort(...)
     16.       └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(gghighlight)
      Loading required package: ggplot2
      > 
      > test_check("gghighlight")
      label_key: type
      label_key: type
    ...
       18.     └─cli::cli_abort(...)
       19.       └─rlang::abort(...)
      
      [ FAIL 7 | WARN 6 | SKIP 0 | PASS 159 ]
      Deleting unused snapshots:
      • vdiffr/simple-bar-chart-with-facet.svg
      • vdiffr/simple-line-chart.svg
      • vdiffr/simple-point-chart.svg
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘gghighlight.Rmd’ using rmarkdown
    ```

# gghourglass

<details>

* Version: 0.0.2
* GitHub: https://github.com/pepijn-devries/gghourglass
* Source code: https://github.com/cran/gghourglass
* Date/Publication: 2025-04-05 14:20:02 UTC
* Number of recursive dependencies: 97

Run `revdepcheck::cloud_details(, "gghourglass")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gghourglass-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: AnnotateLunarphase
    > ### Title: Annotate ggplot with lunar phases
    > ### Aliases: AnnotateLunarphase annotate_lunarphase
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    ...
     16. │             └─vctrs (local) `<fn>`()
     17. │               ├─x[i = i]
     18. │               └─grid:::`[.unit`(x = x, i = i)
     19. │                 └─base::stop(...)
     20. └─base::.handleSimpleError(...)
     21.   └─rlang (local) h(simpleError(msg, call))
     22.     └─handlers[[1L]](cnd)
     23.       └─cli::cli_abort(...)
     24.         └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(gghourglass)
      > 
      > test_check("gghourglass")
      NULL
      [ FAIL 1 | WARN 2 | SKIP 4 | PASS 3 ]
      
    ...
       23. │                       └─base::stop(...)
       24. └─base::.handleSimpleError(...)
       25.   └─rlang (local) h(simpleError(msg, call))
       26.     └─handlers[[1L]](cnd)
       27.       └─cli::cli_abort(...)
       28.         └─rlang::abort(...)
      
      [ FAIL 1 | WARN 2 | SKIP 4 | PASS 3 ]
      Error: Test failures
      Execution halted
    ```

# ggiraph

<details>

* Version: 0.8.13
* GitHub: https://github.com/davidgohel/ggiraph
* Source code: https://github.com/cran/ggiraph
* Date/Publication: 2025-03-28 10:20:02 UTC
* Number of recursive dependencies: 87

Run `revdepcheck::cloud_details(, "ggiraph")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggiraph-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_bar_interactive
    > ### Title: Create interactive bars
    > ### Aliases: geom_bar_interactive geom_col_interactive
    > 
    > ### ** Examples
    > 
    > # add interactive bar -------
    ...
     20. │                 └─base::lapply(...)
     21. │                   └─ggplot2 (local) FUN(X[[i]], ...)
     22. │                     ├─rlang::inject(self$draw_panel(data, panel_params, coord, !!!params))
     23. │                     └─self$draw_panel(...)
     24. └─base::.handleSimpleError(...)
     25.   └─rlang (local) h(simpleError(msg, call))
     26.     └─handlers[[1L]](cnd)
     27.       └─cli::cli_abort(...)
     28.         └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘tinytest.R’
    Running the tests in ‘tests/tinytest.R’ failed.
    Complete output:
      > if (requireNamespace("tinytest", quietly = TRUE)) {
      +   tinytest::test_package("ggiraph")
      + }
      
      test-annotate_interactive.R...    0 tests    
      test-annotate_interactive.R...    0 tests    
      test-annotate_interactive.R...    0 tests    
    ...
       30. │                             └─base::lapply(...)
       31. │                               └─ggplot2 (local) FUN(X[[i]], ...)
       32. │                                 ├─rlang::inject(self$draw_panel(data, panel_params, coord, !!!params))
       33. │                                 └─self$draw_panel(...)
       34. └─base::.handleSimpleError(...)
       35.   └─rlang (local) h(simpleError(msg, call))
       36.     └─handlers[[1L]](cnd)
       37.       └─cli::cli_abort(...)
       38.         └─rlang::abort(...)
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.1Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        libs   5.2Mb
    ```

# ggiraphExtra

<details>

* Version: 0.3.0
* GitHub: https://github.com/cardiomoon/ggiraphExtra
* Source code: https://github.com/cran/ggiraphExtra
* Date/Publication: 2020-10-06 07:00:02 UTC
* Number of recursive dependencies: 108

Run `revdepcheck::cloud_details(, "ggiraphExtra")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggiraphExtra-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggBar
    > ### Title: Draw an interactive barplot
    > ### Aliases: ggBar
    > 
    > ### ** Examples
    > 
    > require(moonBook)
    ...
     21. │                   └─base::lapply(...)
     22. │                     └─ggplot2 (local) FUN(X[[i]], ...)
     23. │                       ├─rlang::inject(self$draw_panel(data, panel_params, coord, !!!params))
     24. │                       └─self$draw_panel(...)
     25. └─base::.handleSimpleError(...)
     26.   └─rlang (local) h(simpleError(msg, call))
     27.     └─handlers[[1L]](cnd)
     28.       └─cli::cli_abort(...)
     29.         └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ggPredict.Rmd’ using rmarkdown
    ```

# gglgbtq

<details>

* Version: 0.2.0
* GitHub: https://github.com/turtletopia/gglgbtq
* Source code: https://github.com/cran/gglgbtq
* Date/Publication: 2024-06-25 22:20:02 UTC
* Number of recursive dependencies: 67

Run `revdepcheck::cloud_details(, "gglgbtq")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gglgbtq-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: lgbtq-scales
    > ### Title: Apply gglgbtq scales
    > ### Aliases: lgbtq-scales scale_color_lgbtq scale_colour_lgbtq
    > ###   scale_fill_lgbtq
    > 
    > ### ** Examples
    > 
    ...
     20. └─vctrs (local) `<fn>`()
     21.   └─vctrs::vec_default_ptype2(...)
     22.     ├─base::withRestarts(...)
     23.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     24.     │   └─base (local) doWithOneRestart(return(expr), restart)
     25.     └─vctrs::stop_incompatible_type(...)
     26.       └─vctrs:::stop_incompatible(...)
     27.         └─vctrs:::stop_vctrs(...)
     28.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(gglgbtq)
      > 
      > test_check("gglgbtq")
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 165 ]
      
    ...
       29.     └─vctrs::stop_incompatible_type(...)
       30.       └─vctrs:::stop_incompatible(...)
       31.         └─vctrs:::stop_vctrs(...)
       32.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 165 ]
      Deleting unused snapshots:
      • scales/scale-color.svg
      Error: Test failures
      Execution halted
    ```

# ggmap

<details>

* Version: 4.0.1
* GitHub: https://github.com/dkahle/ggmap
* Source code: https://github.com/cran/ggmap
* Date/Publication: 2025-04-07 19:40:02 UTC
* Number of recursive dependencies: 63

Run `revdepcheck::cloud_details(, "ggmap")` for more info

</details>

## Newly broken

*   checking whether package ‘ggmap’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: `aes_()` was deprecated in ggplot2 3.0.0.
    See ‘/tmp/workdir/ggmap/new/ggmap.Rcheck/00install.out’ for details.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.4Mb
      sub-directories of 1Mb or more:
        data   7.0Mb
    ```

# ggmice

<details>

* Version: 0.1.0
* GitHub: https://github.com/amices/ggmice
* Source code: https://github.com/cran/ggmice
* Date/Publication: 2023-08-07 14:20:02 UTC
* Number of recursive dependencies: 120

Run `revdepcheck::cloud_details(, "ggmice")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ggmice.Rmd’ using rmarkdown
    ```

# ggmosaic

<details>

* Version: 0.3.3
* GitHub: https://github.com/haleyjeppson/ggmosaic
* Source code: https://github.com/cran/ggmosaic
* Date/Publication: 2021-02-23 19:50:02 UTC
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "ggmosaic")` for more info

</details>

## Newly broken

*   checking whether package ‘ggmosaic’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ggmosaic/new/ggmosaic.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    ':::' call which should be '::': ‘scales:::censor’
      See the note in ?`:::` about the use of this operator.
    Unexported object imported by a ':::' call: ‘productplots:::bound’
      See the note in ?`:::` about the use of this operator.
    ```

## Installation

### Devel

```
* installing *source* package ‘ggmosaic’ ...
** package ‘ggmosaic’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in get(x, envir = ns, inherits = FALSE) : 
  object 'is.waive' not found
Error: unable to load R code in package ‘ggmosaic’
Execution halted
ERROR: lazy loading failed for package ‘ggmosaic’
* removing ‘/tmp/workdir/ggmosaic/new/ggmosaic.Rcheck/ggmosaic’


```
### CRAN

```
* installing *source* package ‘ggmosaic’ ...
** package ‘ggmosaic’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (ggmosaic)


```
# ggmulti

<details>

* Version: 1.0.7
* GitHub: NA
* Source code: https://github.com/cran/ggmulti
* Date/Publication: 2024-04-09 09:40:05 UTC
* Number of recursive dependencies: 124

Run `revdepcheck::cloud_details(, "ggmulti")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggmulti-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: coord_radial
    > ### Title: Radial axes
    > ### Aliases: coord_radial
    > 
    > ### ** Examples
    > 
    > if(require("dplyr")) {
    ...
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    Warning: Duplicated aesthetics after name standardisation: PTS
    Error in use_defaults(..., self = self) : 
      unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", 19, 1.5, 0.5, TRUE), list("white", "black", 0.5, 1, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, NULL, NULL, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, 
        NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "#4D4D4DFF", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), li
    Calls: <Anonymous> ... <Anonymous> -> <Anonymous> -> compute_geom_2 -> <Anonymous>
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > 
      > 
      > library(testthat)
      > library(ggmulti)
      Loading required package: ggplot2
      
      Attaching package: 'ggmulti'
    ...
       16.                         └─base::Map(...)
       17.                           └─base::mapply(FUN = f, ..., SIMPLIFY = FALSE)
       18.                             └─ggplot2 (local) `<fn>`(layer = dots[[1L]][[1L]], df = dots[[2L]][[1L]])
       19.                               └─layer$compute_geom_2(key, single_params, theme)
       20.                                 └─ggplot2 (local) compute_geom_2(..., self = self)
       21.                                   └─self$geom$use_defaults(...)
      
      [ FAIL 4 | WARN 5 | SKIP 0 | PASS 30 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘glyph.Rmd’ using rmarkdown
    ```

# ggnewscale

<details>

* Version: 0.5.1
* GitHub: https://github.com/eliocamp/ggnewscale
* Source code: https://github.com/cran/ggnewscale
* Date/Publication: 2025-02-24 09:00:02 UTC
* Number of recursive dependencies: 59

Run `revdepcheck::cloud_details(, "ggnewscale")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggnewscale)
      > 
      > test_check("ggnewscale")
      `new_stat_bin2d()` using `bins = 30`. Pick better value `binwidth`.
      `new_stat_bin2d()` using `bins = 30`. Pick better value `binwidth`.
      `new_stat_bin2d()` using `bins = 30`. Pick better value `binwidth`.
    ...
       13.         └─scales:::as_discrete_pal.default(elem)
       14.           └─cli::cli_abort("Cannot convert {.arg x} to a discrete palette.")
       15.             └─rlang::abort(...)
      
      [ FAIL 1 | WARN 0 | SKIP 5 | PASS 3 ]
      Deleting unused snapshots:
      • newscale/guides-outisde-of-scales.svg
      • newscale/respects-override-aes-2.svg
      Error: Test failures
      Execution halted
    ```

# ggpackets

<details>

* Version: 0.2.1
* GitHub: https://github.com/dgkf/ggpackets
* Source code: https://github.com/cran/ggpackets
* Date/Publication: 2022-10-10 23:30:02 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::cloud_details(, "ggpackets")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggpackets-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggpacket
    > ### Title: A container for lazy ggplot layers
    > ### Aliases: ggpacket
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
    +     geom_line(...) %+%
    +     geom_point(color = "red", ...)
    + }
    > 
    > ggplot(mtcars, aes(x = wt, y = mpg)) +
    +   ggpk_func(color = "purple", size = 2, point.size = 4)
    Error in if (grepl("^Ignoring unknown (parameters|aesthetics):", w$message)) invokeRestart("muffleWarning") : 
      the condition has length > 1
    Calls: +.gg ... withOneRestart -> doWithOneRestart -> signalCondition -> <Anonymous>
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘composing-functions.Rmd’ using rmarkdown
    ```

# ggparallel

<details>

* Version: 0.4.0
* GitHub: https://github.com/heike/ggparallel
* Source code: https://github.com/cran/ggparallel
* Date/Publication: 2024-03-09 22:00:02 UTC
* Number of recursive dependencies: 48

Run `revdepcheck::cloud_details(, "ggparallel")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
      > # * https://testthat.r-lib.org/articles/special-files.html
    ...
       12. │       └─ggplot2::layer_data(..., self = self)
       13. └─base::.handleSimpleError(...)
       14.   └─rlang (local) h(simpleError(msg, call))
       15.     └─handlers[[1L]](cnd)
       16.       └─cli::cli_abort(...)
       17.         └─rlang::abort(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

# ggparty

<details>

* Version: 1.0.0
* GitHub: https://github.com/martin-borkovec/ggparty
* Source code: https://github.com/cran/ggparty
* Date/Publication: 2019-07-18 10:54:06 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::cloud_details(, "ggparty")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ggparty-graphic-partying.Rmd’ using rmarkdown
    
    Quitting from ggparty-graphic-partying.Rmd:41-122 [unnamed-chunk-2]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'ggparty-graphic-partying.Rmd' failed with diagnostics:
    `type` must be a character vector or list of character vectors, not `NULL`.
    --- failed re-building ‘ggparty-graphic-partying.Rmd’
    
    --- re-building ‘on-the-edge.Rmd’ using rmarkdown
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘survival’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# ggplot2.utils

<details>

* Version: 0.3.2
* GitHub: https://github.com/insightsengineering/ggplot2.utils
* Source code: https://github.com/cran/ggplot2.utils
* Date/Publication: 2024-06-25 21:10:02 UTC
* Number of recursive dependencies: 71

Run `revdepcheck::cloud_details(, "ggplot2.utils")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggplot2.utils)
      Loading required package: ggplot2
      > 
      > test_check("ggplot2.utils")
      
      Attaching package: 'dplyr'
    ...
      ── Failure ('test-geom_km_ticks.R:10:3'): geom_km_ticks works as expected ──────
      Names of `first_layer` ('time', 'survival', 'n.risk', 'n.censor', 'n.event', 'PANEL', 'group', 'x', 'y', 'shape', 'colour', 'size', 'alpha', 'stroke', 'fill') don't match 'x', 'y', 'time', 'survival', 'n.risk', 'n.censor', 'n.event', 'PANEL', 'group', 'shape', 'colour', 'size', 'alpha', 'stroke', 'fill'
      ── Failure ('test-stat_km.R:10:3'): stat_km works as expected ──────────────────
      Names of `first_layer` ('time', 'survival', 'PANEL', 'group', 'x', 'y', 'colour', 'fill', 'linewidth', 'linetype', 'weight', 'alpha') don't match 'x', 'y', 'time', 'survival', 'PANEL', 'group', 'colour', 'fill', 'linewidth', 'linetype', 'weight', 'alpha'
      ── Failure ('test-stat_km_ticks.R:10:3'): stat_km_ticks works as expected ──────
      Names of `first_layer` ('time', 'survival', 'n.risk', 'n.censor', 'n.event', 'PANEL', 'group', 'x', 'y', 'shape', 'colour', 'size', 'alpha', 'stroke', 'fill') don't match 'x', 'y', 'time', 'survival', 'n.risk', 'n.censor', 'n.event', 'PANEL', 'group', 'shape', 'colour', 'size', 'alpha', 'stroke', 'fill'
      
      [ FAIL 4 | WARN 0 | SKIP 7 | PASS 44 ]
      Error: Test failures
      Execution halted
    ```

# ggpointdensity

<details>

* Version: 0.2.0
* GitHub: https://github.com/LKremer/ggpointdensity
* Source code: https://github.com/cran/ggpointdensity
* Date/Publication: 2025-05-16 15:30:02 UTC
* Number of recursive dependencies: 48

Run `revdepcheck::cloud_details(, "ggpointdensity")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggpointdensity-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_pointdensity
    > ### Title: A cross between a scatter plot and a 2D density plot
    > ### Aliases: geom_pointdensity
    > 
    > ### ** Examples
    > 
    > library(ggpointdensity)
    ...
    +   scale_colour_continuous(low = "red", high = "black")
    Error in `scale_backward_compatibility()`:
    ! Unknown scale type:
    Backtrace:
        ▆
     1. └─ggplot2::scale_colour_continuous(low = "red", high = "black")
     2.   └─ggplot2:::scale_backward_compatibility(...)
     3.     └─cli::cli_abort("Unknown scale type: {.val {scale}}")
     4.       └─rlang::abort(...)
    Execution halted
    ```

# ggpol

<details>

* Version: 0.0.7
* GitHub: https://github.com/erocoar/ggpol
* Source code: https://github.com/cran/ggpol
* Date/Publication: 2020-11-08 13:40:02 UTC
* Number of recursive dependencies: 52

Run `revdepcheck::cloud_details(, "ggpol")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggpol-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: GeomConfmat
    > ### Title: Confusion Matrix
    > ### Aliases: GeomConfmat geom_confmat stat_confmat
    > 
    > ### ** Examples
    > 
    > x <- sample(LETTERS[seq(4)], 50, replace = TRUE)
    ...
     21. │                     └─ggpol (local) draw_panel(...)
     22. │                       └─base::lapply(GeomText$default_aes[missing_aes], rlang::eval_tidy)
     23. │                         └─rlang (local) FUN(X[[i]], ...)
     24. ├─ggplot2::from_theme(fontsize)
     25. └─base::.handleSimpleError(...)
     26.   └─rlang (local) h(simpleError(msg, call))
     27.     └─handlers[[1L]](cnd)
     28.       └─cli::cli_abort(...)
     29.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘grDevices’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# ggpp

<details>

* Version: 0.5.8-1
* GitHub: https://github.com/aphalo/ggpp
* Source code: https://github.com/cran/ggpp
* Date/Publication: 2024-07-01 07:40:02 UTC
* Number of recursive dependencies: 79

Run `revdepcheck::cloud_details(, "ggpp")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggpp-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_label_s
    > ### Title: Linked Text
    > ### Aliases: geom_label_s geom_text_s
    > 
    > ### ** Examples
    > 
    > 
    ...
      2.   └─ggplot2:::scale_backward_compatibility(...)
      3.     ├─rlang::exec(scale, !!!args)
      4.     └─ggplot2 (local) `<fn>`(l = 40, na.value = "grey50", call = `<fn>`(scale_colour_discrete(l = 40)))
      5.       ├─ggplot2::discrete_scale(...)
      6.       │ └─ggplot2::ggproto(...)
      7.       │   └─rlang::list2(...)
      8.       └─ggplot2:::pal_qualitative(type = type)
      9.         └─ggplot2:::stop_input_type(type, "a character vector or list of character vectors")
     10.           └─rlang::abort(message, ..., call = call, arg = arg)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘grammar-extensions.Rmd’ using rmarkdown
    ```

# ggpubr

<details>

* Version: 0.6.0
* GitHub: https://github.com/kassambara/ggpubr
* Source code: https://github.com/cran/ggpubr
* Date/Publication: 2023-02-10 16:20:02 UTC
* Number of recursive dependencies: 89

Run `revdepcheck::cloud_details(, "ggpubr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggpubr)
      Loading required package: ggplot2
      > 
      > test_check("ggpubr")
      [ FAIL 2 | WARN 6 | SKIP 0 | PASS 183 ]
      
    ...
      [6]   6 - 10 == -4
      [7]  19 -  9 == 10
      [9]   1 -  7 == -6
      [10]  6 -  7 == -1
      [11] 13 -  6 ==  7
      ...
      
      [ FAIL 2 | WARN 6 | SKIP 0 | PASS 183 ]
      Error: Test failures
      Execution halted
    ```

# ggraph

<details>

* Version: 2.2.1
* GitHub: https://github.com/thomasp85/ggraph
* Source code: https://github.com/cran/ggraph
* Date/Publication: 2024-03-07 12:40:02 UTC
* Number of recursive dependencies: 114

Run `revdepcheck::cloud_details(, "ggraph")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggraph-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_conn_bundle
    > ### Title: Create hierarchical edge bundles between node connections
    > ### Aliases: geom_conn_bundle geom_conn_bundle2 geom_conn_bundle0
    > 
    > ### ** Examples
    > 
    > # Create a graph of the flare class system
    ...
    +   ) +
    +   geom_node_point(aes(filter = leaf, colour = class)) +
    +   scale_edge_colour_distiller('', direction = 1, guide = 'edge_direction') +
    +   coord_fixed() +
    +   ggforce::theme_no_axes()
    Error in get_layer_key(...) : 
      unused argument (list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", 19, 1.5, 0.5, TRUE), list("white", "black", 0.5, 1, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, NULL, NULL, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, list(), list(NULL, NULL, NULL, 
        NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE)
    Calls: <Anonymous> ... <Anonymous> -> <Anonymous> -> process_layers -> <Anonymous>
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Edges.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.0Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        doc    3.9Mb
        libs   2.8Mb
    ```

# ggredist

<details>

* Version: 0.0.2
* GitHub: https://github.com/alarm-redist/ggredist
* Source code: https://github.com/cran/ggredist
* Date/Publication: 2022-11-23 11:20:02 UTC
* Number of recursive dependencies: 65

Run `revdepcheck::cloud_details(, "ggredist")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggredist-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_district_text
    > ### Title: Label Map Regions
    > ### Aliases: geom_district_text geom_district_label
    > ###   stat_district_coordinates StatDistrictCoordinates GeomDistrictText
    > ### Keywords: datasets
    > 
    > ### ** Examples
    ...
     22. │                       └─coord$transform(data, panel_params)
     23. │                         └─ggplot2 (local) transform(..., self = self)
     24. │                           └─ggplot2:::sf_rescale01(data[[geom_column(data)]], x_range, y_range)
     25. │                             └─sf::st_normalize(x, c(x_range[1], y_range[1], x_range[2], y_range[2]))
     26. └─base::.handleSimpleError(...)
     27.   └─rlang (local) h(simpleError(msg, call))
     28.     └─handlers[[1L]](cnd)
     29.       └─cli::cli_abort(...)
     30.         └─rlang::abort(...)
    Execution halted
    ```

# ggResidpanel

<details>

* Version: 0.3.0
* GitHub: NA
* Source code: https://github.com/cran/ggResidpanel
* Date/Publication: 2019-05-31 23:20:04 UTC
* Number of recursive dependencies: 112

Run `revdepcheck::cloud_details(, "ggResidpanel")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggResidpanel-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: resid_interact
    > ### Title: Panel of Interactive Versions of Diagnostic Residual Plots.
    > ### Aliases: resid_interact
    > 
    > ### ** Examples
    > 
    > 
    > # Fit a model to the penguin data
    > penguin_model <- lme4::lmer(heartrate ~ depth + duration + (1|bird), data = penguins)
    > 
    > # Create the default interactive panel
    > resid_interact(penguin_model)
    Error in pm[[2]] : subscript out of bounds
    Calls: resid_interact ... %>% -> layout -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggResidpanel)
      > 
      > test_check("ggResidpanel")
      [ FAIL 1 | WARN 4 | SKIP 9 | PASS 18 ]
      
      ══ Skipped tests (9) ═══════════════════════════════════════════════════════════
    ...
       39. │                                             └─ggplot2 (local) draw_group(..., self = self)
       40. └─base::.handleSimpleError(...)
       41.   └─rlang (local) h(simpleError(msg, call))
       42.     └─handlers[[1L]](cnd)
       43.       └─cli::cli_abort(...)
       44.         └─rlang::abort(...)
      
      [ FAIL 1 | WARN 4 | SKIP 9 | PASS 18 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘introduction.Rmd’ using rmarkdown
    ```

# ggRtsy

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/ggRtsy
* Date/Publication: 2023-09-15 19:12:05 UTC
* Number of recursive dependencies: 66

Run `revdepcheck::cloud_details(, "ggRtsy")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggplot2)
      > library(dplyr)
      
      Attaching package: 'dplyr'
      
      The following object is masked from 'package:testthat':
    ...
       13. │         │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
       14. │         └─vctrs::vec_as_location(i, n, names = names, arg = arg, call = call)
       15. └─vctrs (local) `<fn>`()
       16.   └─vctrs:::stop_subscript_oob(...)
       17.     └─vctrs:::stop_subscript(...)
       18.       └─rlang::abort(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 3 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Vignette.Rmd’ using rmarkdown
    
    Quitting from Vignette.Rmd:47-49 [unnamed-chunk-2]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'Vignette.Rmd' failed with diagnostics:
    ...
    Can't extract rows past the end.
    ℹ Location 1 doesn't exist.
    ℹ There are only 0 rows.
    --- failed re-building ‘Vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 19 marked UTF-8 strings
    ```

# ggseqplot

<details>

* Version: 0.8.6
* GitHub: https://github.com/maraab23/ggseqplot
* Source code: https://github.com/cran/ggseqplot
* Date/Publication: 2025-05-06 22:10:02 UTC
* Number of recursive dependencies: 127

Run `revdepcheck::cloud_details(, "ggseqplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggseqplot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggseqtrplot
    > ### Title: Sequence Transition Rate Plot
    > ### Aliases: ggseqtrplot
    > 
    > ### ** Examples
    > 
    > # Use example data from TraMineR: biofam data set
    ...
         8  7           7        Divorced
     [>] sum of weights: 330.07 - min/max: 0/6.02881860733032
     [>] 300 sequences in the data set
     [>] min/max sequence length: 16/16
    > 
    > # Basic transition rate plot (with adjusted x-axis labels)
    > ggseqtrplot(biofam.seq, x_n.dodge = 2)
    Error in ggseqtrplot(biofam.seq, x_n.dodge = 2) : 
      labsize must be a single number
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggseqplot)
      Loading required package: TraMineR
      
      TraMineR stable version 2.2-11 (Built: 2024-12-09)
      Website: http://traminer.unige.ch
      Please type 'citation("TraMineR")' for citation information.
    ...
      Backtrace:
          ▆
       1. ├─testthat::expect_s3_class(ggseqtrplot(biofam.seq), "ggplot") at test-ggseqtrplot.R:35:3
       2. │ └─testthat::quasi_label(enquo(object), arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. └─ggseqplot::ggseqtrplot(biofam.seq)
      
      [ FAIL 1 | WARN 16 | SKIP 0 | PASS 131 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ggseqplot.Rmd’ using rmarkdown
    ```

# ggside

<details>

* Version: 0.3.1
* GitHub: https://github.com/jtlandis/ggside
* Source code: https://github.com/cran/ggside
* Date/Publication: 2024-03-01 09:12:37 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::cloud_details(, "ggside")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggside-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_xsidebar
    > ### Title: Side bar Charts
    > ### Aliases: geom_xsidebar geom_*sidebar geom_ysidebar geom_xsidecol
    > ###   geom_ysidecol
    > 
    > ### ** Examples
    > 
    ...
     11.       │ └─base::withCallingHandlers(...)
     12.       └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13.         └─l$compute_statistic(d, layout)
     14.           └─ggside (local) compute_statistic(..., self = self)
     15.             └─ggplot2 (local) ggproto_parent_method(self = self, data = data, layout = layout)
     16.               └─self$stat$setup_params(data, self$stat_params)
     17.                 └─ggplot2 (local) setup_params(..., self = self)
     18.                   └─cli::cli_abort("{.fn {snake_class(self)}} must only have an {.field x} {.emph or} {.field y} aesthetic.")
     19.                     └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggplot2)
      > library(ggside)
      Registered S3 method overwritten by 'ggside':
        method from   
        +.gg   ggplot2
      > 
    ...
      • side_layers/label.svg
      • side_layers/violin.svg
      • vdiff_irisScatter/collapsed-histo.svg
      • vdiff_irisScatter/facetgrid-collapsed-density.svg
      • vdiff_irisScatter/facetgrid-histo.svg
      • vdiff_irisScatter/facetgrid-side-density.svg
      • vdiff_irisScatter/stacked-side-density.svg
      • vdiff_irisScatter/yside-histo.svg
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ggside_aes_mapping.Rmd’ using rmarkdown
    
    Quitting from ggside_aes_mapping.Rmd:42-46 [ggside_updated_aes_usage]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'ggside_aes_mapping.Rmd' failed with diagnostics:
    Problem while setting up geom.
    ℹ Error occurred in the 2nd layer.
    Caused by error in `ggproto_parent_method()`:
    ! `geom_xsidedensity()` requires the following missing aesthetics: y.
    --- failed re-building ‘ggside_aes_mapping.Rmd’
    
    --- re-building ‘ggside_basic_usage.Rmd’ using rmarkdown
    ```

*   checking for code/documentation mismatches ... WARNING
    ```
    Codoc mismatches from Rd file 'geom_xsideabline.Rd':
    geom_xsidehline
      Code: function(mapping = NULL, data = NULL, position = "identity",
                     ..., yintercept, na.rm = FALSE, show.legend = NA)
      Docs: function(mapping = NULL, data = NULL, ..., yintercept, na.rm =
                     FALSE, show.legend = NA)
      Argument names in code not in docs:
        position
      Mismatches in argument names (first 3):
        Position: 3 Code: position Docs: ...
    ...
                     FALSE, show.legend = NA, inherit.aes = TRUE,
                     orientation = "y")
      Argument names in code not in docs:
        quantile.colour quantile.color quantile.linetype quantile.linewidth
      Mismatches in argument names (first 3):
        Position: 6 Code: trim Docs: draw_quantiles
        Position: 7 Code: bounds Docs: trim
        Position: 8 Code: quantile.colour Docs: bounds
      Mismatches in argument default values:
        Name: 'draw_quantiles' Code: deprecated() Docs: NULL
    ```

*   checking R code for possible problems ... NOTE
    ```
    geom_xsidelabel: no visible global function definition for ‘deprecated’
    geom_xsideviolin: no visible global function definition for
      ‘deprecated’
    geom_ysidelabel: no visible global function definition for ‘deprecated’
    geom_ysideviolin: no visible global function definition for
      ‘deprecated’
    Undefined global functions or variables:
      deprecated
    ```

# ggswissmaps

<details>

* Version: 0.1.1
* GitHub: https://github.com/gibonet/ggswissmaps
* Source code: https://github.com/cran/ggswissmaps
* Date/Publication: 2016-10-29 10:48:24
* Number of recursive dependencies: 69

Run `revdepcheck::cloud_details(, "ggswissmaps")` for more info

</details>

## Newly broken

*   checking whether package ‘ggswissmaps’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
    See ‘/tmp/workdir/ggswissmaps/new/ggswissmaps.Rcheck/00install.out’ for details.
    ```

*   checking whether the namespace can be loaded with stated dependencies ... NOTE
    ```
    Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
    ℹ Please use tidy evaluation idioms with `aes()`.
    ℹ See also `vignette("ggplot2-in-packages")` for more information.
    ℹ The deprecated feature was likely used in the ggswissmaps package.
      Please report the issue to the authors.
    
    A namespace must be able to be loaded with just the base namespace
    loaded: otherwise if the namespace gets loaded by a saved object, the
    session will be unable to start.
    
    Probably some imports need to be declared in the NAMESPACE file.
    ```

# ggtern

<details>

* Version: 3.5.0
* GitHub: NA
* Source code: https://github.com/cran/ggtern
* Date/Publication: 2024-03-24 21:50:02 UTC
* Number of recursive dependencies: 40

Run `revdepcheck::cloud_details(, "ggtern")` for more info

</details>

## Newly broken

*   checking whether package ‘ggtern’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ggtern/new/ggtern.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘chemometrics’
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘sp’
    ```

## Installation

### Devel

```
* installing *source* package ‘ggtern’ ...
** package ‘ggtern’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** demo
** inst
** byte-compile and prepare package for lazy loading
Error in get(x, envir = ns, inherits = FALSE) : 
  object 'is.zero' not found
Error: unable to load R code in package ‘ggtern’
Execution halted
ERROR: lazy loading failed for package ‘ggtern’
* removing ‘/tmp/workdir/ggtern/new/ggtern.Rcheck/ggtern’


```
### CRAN

```
* installing *source* package ‘ggtern’ ...
** package ‘ggtern’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** demo
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (ggtern)


```
# ggTimeSeries

<details>

* Version: 1.0.2
* GitHub: https://github.com/thecomeonman/ggTimeSeries
* Source code: https://github.com/cran/ggTimeSeries
* Date/Publication: 2022-01-23 16:22:42 UTC
* Number of recursive dependencies: 53

Run `revdepcheck::cloud_details(, "ggTimeSeries")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggTimeSeries-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggplot_calendar_heatmap
    > ### Title: Plots a calendar heatmap
    > ### Aliases: ggplot_calendar_heatmap
    > 
    > ### ** Examples
    > 
    > {
    ...
      <https://github.com/thecomeonman/ggTimeSeries/issues>.
    Error in `scale_backward_compatibility()`:
    ! Unknown scale type:
    Backtrace:
        ▆
     1. └─ggplot2::scale_colour_continuous(low = "red", high = "green")
     2.   └─ggplot2:::scale_backward_compatibility(...)
     3.     └─cli::cli_abort("Unknown scale type: {.val {scale}}")
     4.       └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ggTimeSeries.Rmd’ using rmarkdown
    ```

# ggVennDiagram

<details>

* Version: 1.5.2
* GitHub: https://github.com/gaospecial/ggVennDiagram
* Source code: https://github.com/cran/ggVennDiagram
* Date/Publication: 2024-02-20 08:10:02 UTC
* Number of recursive dependencies: 95

Run `revdepcheck::cloud_details(, "ggVennDiagram")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘VennCalculator.Rmd’ using rmarkdown
    --- finished re-building ‘VennCalculator.Rmd’
    
    --- re-building ‘fully-customed.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.1Mb
      sub-directories of 1Mb or more:
        doc    9.5Mb
        help   1.1Mb
    ```

# gprofiler2

<details>

* Version: 0.2.3
* GitHub: NA
* Source code: https://github.com/cran/gprofiler2
* Date/Publication: 2024-02-23 21:50:02 UTC
* Number of recursive dependencies: 72

Run `revdepcheck::cloud_details(, "gprofiler2")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gprofiler2-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gostplot
    > ### Title: Manhattan plot of functional enrichment results.
    > ### Aliases: gostplot
    > 
    > ### ** Examples
    > 
    >  gostres <- gost(c("Klf4", "Pax5", "Sox2", "Nanog"), organism = "mmusculus")
    ...
    In addition: Warning messages:
    1: The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
    ℹ Please use the `linewidth` argument instead.
    ℹ The deprecated feature was likely used in the gprofiler2 package.
      Please report the issue at <https://biit.cs.ut.ee/gprofiler/page/contact>. 
    2: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ℹ Please use `linewidth` instead.
    ℹ The deprecated feature was likely used in the gprofiler2 package.
      Please report the issue at <https://biit.cs.ut.ee/gprofiler/page/contact>. 
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘gprofiler2.Rmd’ using rmarkdown
    
    Quitting from gprofiler2.Rmd:245-247 [unnamed-chunk-14]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `plot$facet$params$labeller()`:
    ! attempt to apply non-function
    ---
    Backtrace:
    ...
    
    Error: processing vignette 'gprofiler2.Rmd' failed with diagnostics:
    attempt to apply non-function
    --- failed re-building ‘gprofiler2.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘gprofiler2.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# grafify

<details>

* Version: 5.0.0.1
* GitHub: https://github.com/ashenoy-cmbi/grafify
* Source code: https://github.com/cran/grafify
* Date/Publication: 2025-03-10 22:50:02 UTC
* Number of recursive dependencies: 112

Run `revdepcheck::cloud_details(, "grafify")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(grafify)
      Loading required package: ggplot2
      > library(rlang)
      
      Attaching package: 'rlang'
      
    ...
      Backtrace:
          ▆
       1. └─testthat::expect_match(db2$labels$y, "PI") at test-plot_befafter_colors.R:37:3
       2.   └─base::stopifnot(is.character(act$val))
      ── Failure ('test-plot_qqline.R:14:3'): Check QQ plots ─────────────────────────
      as.character(db1$labels$y) is empty.
      
      [ FAIL 5 | WARN 0 | SKIP 0 | PASS 180 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        help   5.3Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘gratia’
    ```

# Greymodels

<details>

* Version: 2.0.1
* GitHub: https://github.com/havishaJ/Greymodels
* Source code: https://github.com/cran/Greymodels
* Date/Publication: 2022-12-05 12:42:35 UTC
* Number of recursive dependencies: 87

Run `revdepcheck::cloud_details(, "Greymodels")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Greymodels-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Plots
    > ### Title: plots
    > ### Aliases: plots plotrm plotsmv1 plotsmv2 plotsigndgm plots_mdbgm12
    > 
    > ### ** Examples
    > 
    >   # Plots - EPGM (1, 1) model
    ...
    +     geom_point(data = set4, aes(x = CI, y = y), shape = 23, color = "black") +
    +     geom_line(data = xy1, aes(x = x, y = y,color = "Raw Data")) +
    +     geom_line(data = xy2, aes(x = x, y = y,color = "Fitted&Forecasts")) +
    +     geom_line(data = set3, aes(x = CI, y = y,color = "LowerBound"), linetype=2) +
    +     geom_line(data = set4, aes(x = CI, y = y,color = "UpperBound"), linetype=2) +
    +     scale_color_manual(name = "Label",values = colors)
    >   r <- ggplotly(p)
    Error in pm[[2]] : subscript out of bounds
    Calls: ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# hatchR

<details>

* Version: 0.3.2
* GitHub: https://github.com/bmait101/hatchR
* Source code: https://github.com/cran/hatchR
* Date/Publication: 2025-03-05 15:40:02 UTC
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "hatchR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
      > # * https://testthat.r-lib.org/articles/special-files.html
    ...
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-plot_check_temp.R:13:3'): plot_check_temp() works ────────────
      `plot_geoms` (`actual`) not identical to c("GeomPoint", "GeomLine", "GeomHline", "GeomHline") (`expected`).
      
      `names(actual)` is a character vector ('geom_point', 'geom_line', 'geom_hline', 'geom_hline...4')
      `names(expected)` is absent
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 38 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.7Mb
      sub-directories of 1Mb or more:
        data   6.5Mb
        doc    1.9Mb
    ```

# healthyR

<details>

* Version: 0.2.2
* GitHub: https://github.com/spsanderson/healthyR
* Source code: https://github.com/cran/healthyR
* Date/Publication: 2024-07-01 13:20:02 UTC
* Number of recursive dependencies: 146

Run `revdepcheck::cloud_details(, "healthyR")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘getting-started.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.6Mb
      sub-directories of 1Mb or more:
        data   2.5Mb
        doc    3.7Mb
    ```

# healthyR.ai

<details>

* Version: 0.1.1
* GitHub: https://github.com/spsanderson/healthyR.ai
* Source code: https://github.com/cran/healthyR.ai
* Date/Publication: 2025-04-24 11:40:17 UTC
* Number of recursive dependencies: 204

Run `revdepcheck::cloud_details(, "healthyR.ai")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘healthyR.ai-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pca_your_recipe
    > ### Title: Perform PCA
    > ### Aliases: pca_your_recipe
    > 
    > ### ** Examples
    > 
    > suppressPackageStartupMessages(library(timetk))
    ...
    +   step_rm(matches("(iso$)|(xts$)|(hour)|(min)|(sec)|(am.pm)"))
    > 
    > output_list <- pca_your_recipe(rec_obj, .data = data_tbl)
    Warning: !  The following columns have zero variance so scaling cannot be used:
      date_col_day, date_col_mday, date_col_mweek, and date_col_mday7.
    ℹ Consider using ?step_zv (`?recipes::step_zv()`) to remove those columns
      before normalizing.
    Error in pm[[2]] : subscript out of bounds
    Calls: pca_your_recipe -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘auto-kmeans.Rmd’ using rmarkdown
    --- finished re-building ‘auto-kmeans.Rmd’
    
    --- re-building ‘getting-started.Rmd’ using rmarkdown
    
    Quitting from getting-started.Rmd:106-113 [pca_your_rec]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `pm[[2]]`:
    ...
     2.   ├─plotly::ggplotly(loadings_plt)
     3.   └─plotly:::ggplotly.ggplot(loadings_plt)
     4.     └─plotly::gg2list(...)
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'getting-started.Rmd' failed with diagnostics:
    subscript out of bounds
    --- failed re-building ‘getting-started.Rmd’
    
    --- re-building ‘kmeans-umap.Rmd’ using rmarkdown
    ```

# healthyR.ts

<details>

* Version: 0.3.1
* GitHub: https://github.com/spsanderson/healthyR.ts
* Source code: https://github.com/cran/healthyR.ts
* Date/Publication: 2024-10-11 23:00:03 UTC
* Number of recursive dependencies: 210

Run `revdepcheck::cloud_details(, "healthyR.ts")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘healthyR.ts-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: tidy_fft
    > ### Title: Tidy Style FFT
    > ### Aliases: tidy_fft
    > 
    > ### ** Examples
    > 
    > suppressPackageStartupMessages(library(dplyr))
    ...
    > a <- tidy_fft(
    +   .data = data_tbl,
    +   .value_col = value,
    +   .date_col = date_col,
    +   .harmonics = 3,
    +   .frequency = 12
    + )
    Error in pm[[2]] : subscript out of bounds
    Calls: tidy_fft -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘getting-started.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        doc   5.3Mb
    ```

# heatmaply

<details>

* Version: 1.5.0
* GitHub: https://github.com/talgalili/heatmaply
* Source code: https://github.com/cran/heatmaply
* Date/Publication: 2023-10-06 20:50:02 UTC
* Number of recursive dependencies: 109

Run `revdepcheck::cloud_details(, "heatmaply")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(heatmaply)
      Loading required package: plotly
      Loading required package: ggplot2
      
      Attaching package: 'plotly'
      
    ...
       4. │   │ └─base::withCallingHandlers(...)
       5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
       6. ├─heatmaply:::predict_colors(ggplotly(g), plot_method = "ggplot")
       7. ├─plotly::ggplotly(g)
       8. └─plotly:::ggplotly.ggplot(g)
       9.   └─plotly::gg2list(...)
      
      [ FAIL 58 | WARN 0 | SKIP 0 | PASS 193 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘heatmaply.Rmd’ using rmarkdown
    
    Quitting from heatmaply.Rmd:108-111 [unnamed-chunk-5]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `pm[[2]]`:
    ! subscript out of bounds
    ---
    Backtrace:
    ...
    
    Error: processing vignette 'heatmaply.Rmd' failed with diagnostics:
    subscript out of bounds
    --- failed re-building ‘heatmaply.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘heatmaply.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        doc   5.1Mb
    ```

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) heatmaply.Rd:282: Lost braces
       282 | row and column dendrograms. The default uses stats{reorder.dendrogram}}
           |                                                   ^
    checkRd: (-1) heatmapr.Rd:93: Lost braces
        93 | \item{reorderfun}{function(d, w) of dendrogram and weights for reordering the row and column dendrograms. The default uses stats{reorder.dendrogram}}
           |                                                                                                                                 ^
    checkRd: (-1) is.plotly.Rd:16: Lost braces
        16 | Helpful for the plot_method in link{heatmaply}.
           |                                    ^
    ```

# hesim

<details>

* Version: 0.5.5
* GitHub: https://github.com/hesim-dev/hesim
* Source code: https://github.com/cran/hesim
* Date/Publication: 2024-09-18 23:10:02 UTC
* Number of recursive dependencies: 104

Run `revdepcheck::cloud_details(, "hesim")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(hesim)
      > 
      > test_check("hesim")
      sample = 1
      sample = 2
      [ FAIL 4 | WARN 0 | SKIP 0 | PASS 1121 ]
    ...
      ── Failure ('test-plot.R:95:3'): autoplot.stateprobs() allows confidence intervals ──
      p$labels$fill not equal to "strategy_id".
      target is NULL, current is character
      ── Failure ('test-plot.R:99:3'): autoplot.stateprobs() allows confidence intervals ──
      p$labels$fill not equal to "strategy_id".
      target is NULL, current is character
      
      [ FAIL 4 | WARN 0 | SKIP 0 | PASS 1121 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 35.9Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        doc    2.2Mb
        libs  30.8Mb
    ```

# hmsidwR

<details>

* Version: 1.1.2
* GitHub: https://github.com/Fgazzelloni/hmsidwR
* Source code: https://github.com/cran/hmsidwR
* Date/Publication: 2024-11-13 15:00:02 UTC
* Number of recursive dependencies: 175

Run `revdepcheck::cloud_details(, "hmsidwR")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘hmsidwR.Rmd’ using rmarkdown
    
    Quitting from hmsidwR.Rmd:51-84 [unnamed-chunk-6]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `pm[[2]]`:
    ! subscript out of bounds
    ---
    ...
    
    Error: processing vignette 'hmsidwR.Rmd' failed with diagnostics:
    subscript out of bounds
    --- failed re-building ‘hmsidwR.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘hmsidwR.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.2Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        doc    4.0Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1686 marked UTF-8 strings
    ```

# HVT

<details>

* Version: 25.2.3
* GitHub: https://github.com/Mu-Sigma/HVT
* Source code: https://github.com/cran/HVT
* Date/Publication: 2025-03-27 11:40:13 UTC
* Number of recursive dependencies: 208

Run `revdepcheck::cloud_details(, "HVT")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘HVT-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: clustHVT
    > ### Title: Performing Hierarchical Clustering Analysis
    > ### Aliases: clustHVT
    > ### Keywords: Clustering_Analysis
    > 
    > ### ** Examples
    > 
    ...
    > scoring <- scoreHVT(dataset, hvt.results, analysis.plots = TRUE, names.column = dataset[,1])
    Scale for colour is already present.
    Adding another scale for colour, which will replace the existing scale.
    Scale for x is already present.
    Adding another scale for x, which will replace the existing scale.
    Scale for y is already present.
    Adding another scale for y, which will replace the existing scale.
    Error in pm[[2]] : subscript out of bounds
    Calls: scoreHVT
    Execution halted
    ```

# hypsoLoop

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/hypsoLoop
* Date/Publication: 2022-02-08 09:00:02 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::cloud_details(, "hypsoLoop")` for more info

</details>

## Newly broken

*   checking whether package ‘hypsoLoop’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::set_theme’ by ‘sjPlot::set_theme’ when loading ‘hypsoLoop’
    See ‘/tmp/workdir/hypsoLoop/new/hypsoLoop.Rcheck/00install.out’ for details.
    ```

# ibdsim2

<details>

* Version: 2.2.0
* GitHub: https://github.com/magnusdv/ibdsim2
* Source code: https://github.com/cran/ibdsim2
* Date/Publication: 2025-03-03 13:30:02 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "ibdsim2")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ibdsim2-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotSegmentDistribution
    > ### Title: Scatter plots of IBD segment distributions
    > ### Aliases: plotSegmentDistribution
    > 
    > ### ** Examples
    > 
    > 
    ...
     20. └─vctrs (local) `<fn>`()
     21.   └─vctrs::vec_default_ptype2(...)
     22.     ├─base::withRestarts(...)
     23.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     24.     │   └─base (local) doWithOneRestart(return(expr), restart)
     25.     └─vctrs::stop_incompatible_type(...)
     26.       └─vctrs:::stop_incompatible(...)
     27.         └─vctrs:::stop_vctrs(...)
     28.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

# ichimoku

<details>

* Version: 1.5.6
* GitHub: https://github.com/shikokuchuo/ichimoku
* Source code: https://github.com/cran/ichimoku
* Date/Publication: 2025-03-14 18:00:05 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::cloud_details(, "ichimoku")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ichimoku)
      > 
      > test_check("ichimoku")
      Data verified by SHA256: 02a4376885505a161032f4f4d4240798c6e776943dbaa54f374f92b2fccea1a9
      Data verified by SHA256: 02a4376885505a161032f4f4d4240798c6e776943dbaa54f374f92b2fccea1a9
      Loading required package: shiny
    ...
       31.     │   └─base (local) doWithOneRestart(return(expr), restart)
       32.     └─vctrs::stop_incompatible_cast(...)
       33.       └─vctrs::stop_incompatible_type(...)
       34.         └─vctrs:::stop_incompatible(...)
       35.           └─vctrs:::stop_vctrs(...)
       36.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 208 ]
      Error: Test failures
      Execution halted
    ```

# implicitMeasures

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/implicitMeasures
* Date/Publication: 2022-02-16 13:40:13 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::cloud_details(, "implicitMeasures")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(implicitMeasures)
      > 
      > test_check("implicitMeasures")
      [ FAIL 3 | WARN 2 | SKIP 2 | PASS 73 ]
      
    ...
        5. └─implicitMeasures::multi_dscore(iat_data, ds = "error-inflation")
        6.   └─ggplot2::geom_violin(draw_quantiles = TRUE)
        7.     └─ggplot2:::check_numeric(draw_quantiles)
        8.       └─ggplot2:::check_object(x, is.numeric, what, ..., arg = arg, call = call)
        9.         └─ggplot2:::stop_input_type(...)
       10.           └─rlang::abort(message, ..., call = call, arg = arg)
      
      [ FAIL 3 | WARN 2 | SKIP 2 | PASS 73 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘IAT-example.Rmd’ using rmarkdown
    ```

# infer

<details>

* Version: 1.0.8
* GitHub: https://github.com/tidymodels/infer
* Source code: https://github.com/cran/infer
* Date/Publication: 2025-04-14 17:20:02 UTC
* Number of recursive dependencies: 125

Run `revdepcheck::cloud_details(, "infer")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/tests.html
      > # * https://testthat.r-lib.org/reference/test_package.html#special-files
    ...
      • visualize/viz-assume-z-p-val-right.svg
      • visualize/viz-assume-z.svg
      • visualize/viz-fit-bare.svg
      • visualize/viz-fit-conf-int.svg
      • visualize/viz-fit-no-h0.svg
      • visualize/viz-fit-p-val-both.svg
      • visualize/viz-fit-p-val-left.svg
      • visualize/viz-fit-p-val-right.svg
      Error: Test failures
      Execution halted
    ```

# inferCSN

<details>

* Version: 1.1.7
* GitHub: https://github.com/mengxu98/inferCSN
* Source code: https://github.com/cran/inferCSN
* Date/Publication: 2025-03-30 17:00:02 UTC
* Number of recursive dependencies: 187

Run `revdepcheck::cloud_details(, "inferCSN")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘inferCSN-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_dynamic_networks
    > ### Title: Plot dynamic networks
    > ### Aliases: plot_dynamic_networks
    > 
    > ### ** Examples
    > 
    > data("example_matrix")
    ...
    + )
    > 
    > plot_dynamic_networks(
    +   network,
    +   celltypes_order = celltypes_order,
    +   plot_type = "ggplotly"
    + )
    Error in pm[[2]] : subscript out of bounds
    Calls: plot_dynamic_networks -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 27.7Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        libs  25.1Mb
    ```

# injurytools

<details>

* Version: 1.0.3
* GitHub: https://github.com/lzumeta/injurytools
* Source code: https://github.com/cran/injurytools
* Date/Publication: 2023-11-14 17:20:05 UTC
* Number of recursive dependencies: 157

Run `revdepcheck::cloud_details(, "injurytools")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘estimate-epi-measures.Rmd’ using rmarkdown
    --- finished re-building ‘estimate-epi-measures.Rmd’
    
    --- re-building ‘model-injury-data-i.Rmd’ using rmarkdown
    ```

# inTextSummaryTable

<details>

* Version: 3.3.3
* GitHub: https://github.com/openanalytics/inTextSummaryTable
* Source code: https://github.com/cran/inTextSummaryTable
* Date/Publication: 2024-06-12 18:30:02 UTC
* Number of recursive dependencies: 110

Run `revdepcheck::cloud_details(, "inTextSummaryTable")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(inTextSummaryTable)
      > 
      > test_check("inTextSummaryTable")
      [ FAIL 59 | WARN 0 | SKIP 0 | PASS 881 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      Error in `geom_point(mapping = do.call(aes, c(aesBase, list(y = sym("meanVar")), 
          if (!is.null(colorVar) & useShape) list(shape = sym("colorVar")))), 
          position = pd, size = sizePoint, data = data)`: Problem while setting up geom aesthetics.
      ℹ Error occurred in the 2nd layer.
      Caused by error in `list_sizes()`:
      ! `x$size` must be a vector, not a <quosure> object.
      
      [ FAIL 59 | WARN 0 | SKIP 0 | PASS 881 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘inTextSummaryTable-advanced.Rmd’ using rmarkdown
    --- finished re-building ‘inTextSummaryTable-advanced.Rmd’
    
    --- re-building ‘inTextSummaryTable-aesthetics.Rmd’ using rmarkdown
    
    Quitting from inTextSummaryTable-aesthetics.Rmd:210-224 [aesthetics-defaultsVisualization]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ...
    Caused by error in `list_sizes()`:
    ! `x$size` must be a vector, not a <quosure> object.
    --- failed re-building ‘inTextSummaryTable-visualization.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘inTextSummaryTable-aesthetics.Rmd’
      ‘inTextSummaryTable-visualization.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.2Mb
      sub-directories of 1Mb or more:
        doc   9.7Mb
    ```

# inventorize

<details>

* Version: 1.1.1
* GitHub: NA
* Source code: https://github.com/cran/inventorize
* Date/Publication: 2022-05-31 22:20:09 UTC
* Number of recursive dependencies: 69

Run `revdepcheck::cloud_details(, "inventorize")` for more info

</details>

## Newly broken

*   checking whether package ‘inventorize’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/inventorize/new/inventorize.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘inventorize’ ...
** package ‘inventorize’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error in pm[[2]] : subscript out of bounds
Error: unable to load R code in package ‘inventorize’
Execution halted
ERROR: lazy loading failed for package ‘inventorize’
* removing ‘/tmp/workdir/inventorize/new/inventorize.Rcheck/inventorize’


```
### CRAN

```
* installing *source* package ‘inventorize’ ...
** package ‘inventorize’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Warning in qgamma(service_level, alpha, beta) : NaNs produced
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (inventorize)


```
# iNZightTS

<details>

* Version: 2.0.0
* GitHub: https://github.com/iNZightVIT/iNZightTS
* Source code: https://github.com/cran/iNZightTS
* Date/Publication: 2024-01-17 06:20:02 UTC
* Number of recursive dependencies: 91

Run `revdepcheck::cloud_details(, "iNZightTS")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(iNZightTS)
      > 
      > test_check("iNZightTS")
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 107 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
        9.     └─ggplot2::coord_fixed(...)
       10.       └─ggplot2::coord_cartesian(ratio = ratio)
       11.         └─ggplot2:::check_number_decimal(...)
       12.           └─ggplot2:::.stop_not_number(...)
       13.             └─ggplot2:::stop_input_type(...)
       14.               └─rlang::abort(message, ..., call = call, arg = arg)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 107 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘plotly’
    ```

# IRon

<details>

* Version: 0.1.4
* GitHub: https://github.com/nunompmoniz/IRon
* Source code: https://github.com/cran/IRon
* Date/Publication: 2023-01-20 07:20:06 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "IRon")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘IRon-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: sera
    > ### Title: Squared Error-Relevance Area (SERA)
    > ### Aliases: sera
    > 
    > ### ** Examples
    > 
    > library(IRon)
    ...
     16. │             └─self$stat$setup_params(data, self$stat_params)
     17. │               └─ggplot2 (local) setup_params(...)
     18. │                 └─base::match.fun(method)
     19. │                   └─base::get(as.character(FUN), mode = "function", envir = envir)
     20. └─base::.handleSimpleError(...)
     21.   └─rlang (local) h(simpleError(msg, call))
     22.     └─handlers[[1L]](cnd)
     23.       └─cli::cli_abort(...)
     24.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.7Mb
      sub-directories of 1Mb or more:
        data   6.5Mb
    ```

# karel

<details>

* Version: 0.1.1
* GitHub: https://github.com/mpru/karel
* Source code: https://github.com/cran/karel
* Date/Publication: 2022-03-26 21:50:02 UTC
* Number of recursive dependencies: 87

Run `revdepcheck::cloud_details(, "karel")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘karel-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: acciones
    > ### Title: Acciones que Karel puede realizar
    > ### Aliases: acciones avanzar girar_izquierda poner_coso juntar_coso
    > ###   girar_derecha darse_vuelta
    > 
    > ### ** Examples
    > 
    ...
     1. └─karel::ejecutar_acciones()
     2.   ├─base::suppressWarnings(...)
     3.   │ └─base::withCallingHandlers(...)
     4.   ├─gganimate::animate(...)
     5.   └─gganimate:::animate.gganim(...)
     6.     └─args$renderer(frames_vars$frame_source, args$fps)
     7.       └─gganimate:::png_dim(frames[1])
     8.         └─cli::cli_abort("Provided file ({file}) does not exist")
     9.           └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(karel)
      > 
      > test_check("karel")
      [ FAIL 2 | WARN 2 | SKIP 0 | PASS 78 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
        5.     ├─gganimate::animate(...)
        6.     └─gganimate:::animate.gganim(...)
        7.       └─args$renderer(frames_vars$frame_source, args$fps)
        8.         └─gganimate:::png_dim(frames[1])
        9.           └─cli::cli_abort("Provided file ({file}) does not exist")
       10.             └─rlang::abort(...)
      
      [ FAIL 2 | WARN 2 | SKIP 0 | PASS 78 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘gifski’
      All declared Imports should be used.
    ```

# kDGLM

<details>

* Version: 1.2.7
* GitHub: https://github.com/silvaneojunior/kDGLM
* Source code: https://github.com/cran/kDGLM
* Date/Publication: 2025-03-20 01:00:03 UTC
* Number of recursive dependencies: 133

Run `revdepcheck::cloud_details(, "kDGLM")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘kDGLM-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: forecast.fitted_dlm
    > ### Title: Auxiliary function for forecasting
    > ### Aliases: forecast.fitted_dlm
    > 
    > ### ** Examples
    > 
    > 
    ...
    Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
    ℹ Please use tidy evaluation idioms with `aes()`.
    ℹ See also `vignette("ggplot2-in-packages")` for more information.
    ℹ The deprecated feature was likely used in the kDGLM package.
      Please report the issue at <https://github.com/silvaneojunior/kDGLM/issues>.
    Scale for y is already present.
    Adding another scale for y, which will replace the existing scale.
    Error in pm[[2]] : subscript out of bounds
    Calls: forecast ... lapply -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘example1.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.9Mb
      sub-directories of 1Mb or more:
        doc   9.9Mb
    ```

# khroma

<details>

* Version: 1.16.0
* GitHub: NA
* Source code: https://github.com/cran/khroma
* Date/Publication: 2025-02-25 17:40:05 UTC
* Number of recursive dependencies: 72

Run `revdepcheck::cloud_details(, "khroma")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘khroma-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: scale_colour_land
    > ### Title: AVHRR Global Land Cover Classification Color Scheme for
    > ###   'ggplot2' and 'ggraph'
    > ### Aliases: scale_colour_land scale_color_land scale_fill_land
    > ###   scale_edge_colour_land scale_edge_color_land scale_edge_fill_land
    > 
    > ### ** Examples
    ...
     21.   └─vctrs::vec_default_cast(...)
     22.     ├─base::withRestarts(...)
     23.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     24.     │   └─base (local) doWithOneRestart(return(expr), restart)
     25.     └─vctrs::stop_incompatible_cast(...)
     26.       └─vctrs::stop_incompatible_type(...)
     27.         └─vctrs:::stop_incompatible(...)
     28.           └─vctrs:::stop_vctrs(...)
     29.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

# L2E

<details>

* Version: 2.0
* GitHub: NA
* Source code: https://github.com/cran/L2E
* Date/Publication: 2022-09-08 21:13:00 UTC
* Number of recursive dependencies: 63

Run `revdepcheck::cloud_details(, "L2E")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘l2e-intro.Rmd’ using rmarkdown
    
    Quitting from l2e-intro.Rmd:278-294 [unnamed-chunk-14]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'l2e-intro.Rmd' failed with diagnostics:
    Can't combine `..1` <double> and `..2` <character>.
    --- failed re-building ‘l2e-intro.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘l2e-intro.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘robustbase’
      All declared Imports should be used.
    ```

# lans2r

<details>

* Version: 1.2.0
* GitHub: https://github.com/KopfLab/lans2r
* Source code: https://github.com/cran/lans2r
* Date/Publication: 2023-02-19 07:20:02 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::cloud_details(, "lans2r")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(dplyr)
      
      Attaching package: 'dplyr'
      
      The following object is masked from 'package:testthat':
      
    ...
       4. ├─methods::is(plot_maps(data), "ggplot")
       5. └─lans2r::plot_maps(data)
       6.   └─ggplot2::scale_fill_continuous(low = color_scale[1], high = color_scale[2])
       7.     └─ggplot2:::scale_backward_compatibility(...)
       8.       └─cli::cli_abort("Unknown scale type: {.val {scale}}")
       9.         └─rlang::abort(...)
      
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 143 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘lans2r-calculate.Rmd’ using rmarkdown
    ```

# latentcor

<details>

* Version: 2.0.1
* GitHub: NA
* Source code: https://github.com/cran/latentcor
* Date/Publication: 2022-09-05 20:50:02 UTC
* Number of recursive dependencies: 142

Run `revdepcheck::cloud_details(, "latentcor")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘latentcor-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: latentcor
    > ### Title: Estimate latent correlation for mixed types.
    > ### Aliases: latentcor
    > 
    > ### ** Examples
    > 
    > # Example 1 - truncated data type, same type for all variables
    ...
    > R_approx = latentcor(X = X, types = "tru", method = "approx")$R
    > proc.time() - start_time
       user  system elapsed 
      0.025   0.000   0.025 
    > # Heatmap for latent correlation matrix.
    > Heatmap_R_approx = latentcor(X = X, types = "tru", method = "approx",
    +                              showplot = TRUE)$plotR
    Error in pm[[2]] : subscript out of bounds
    Calls: latentcor ... %>% -> layout -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) evaluation.Rd:38: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) evaluation.Rd:39: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) evaluation.Rd:40: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) evaluation.Rd:41: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) evaluation.Rd:42: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) evaluation.Rd:43: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) evaluation.Rd:44: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) evaluation.Rd:45: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) evaluation.Rd:46: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) evaluation.Rd:47: Lost braces in \itemize; \value handles \item{}{} directly
    ...
    checkRd: (-1) gen_data.Rd:35: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) gen_data.Rd:36-37: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) get_types.Rd:17: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) interpolation.Rd:23: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) interpolation.Rd:24: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) latentcor.Rd:38: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) latentcor.Rd:39: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) latentcor.Rd:40: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) latentcor.Rd:41: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) latentcor.Rd:42: Lost braces in \itemize; \value handles \item{}{} directly
    ```

# latrend

<details>

* Version: 1.6.1
* GitHub: https://github.com/philips-software/latrend
* Source code: https://github.com/cran/latrend
* Date/Publication: 2024-05-15 11:50:02 UTC
* Number of recursive dependencies: 237

Run `revdepcheck::cloud_details(, "latrend")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘latrend-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: qqPlot
    > ### Title: Quantile-quantile plot
    > ### Aliases: qqPlot
    > 
    > ### ** Examples
    > 
    > data(latrendData)
    ...
     23. │                         └─ggplot2 (local) FUN(X[[i]], ...)
     24. │                           └─self$draw_group(group, panel_params, coord, ...)
     25. │                             └─ggplot2 (local) draw_group(...)
     26. │                               └─ggplot2 (local) draw_group(..., self = self)
     27. └─base::.handleSimpleError(...)
     28.   └─rlang (local) h(simpleError(msg, call))
     29.     └─handlers[[1L]](cnd)
     30.       └─cli::cli_abort(...)
     31.         └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘demo.Rmd’ using rmarkdown
    ```

# lcars

<details>

* Version: 0.4.0
* GitHub: https://github.com/leonawicz/lcars
* Source code: https://github.com/cran/lcars
* Date/Publication: 2024-09-11 22:52:42 UTC
* Number of recursive dependencies: 85

Run `revdepcheck::cloud_details(, "lcars")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘lcars-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: lcars_border
    > ### Title: LCARS border plot
    > ### Aliases: lcars_border
    > 
    > ### ** Examples
    > 
    > lcars_border()
    ...
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family '0.5' not found in PostScript font database
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family '0.5' not found in PostScript font database
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family '0.5' not found in PostScript font database
    Error in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  : 
      invalid font type
    Calls: lcars_border ... drawDetails -> drawDetails.text -> grid.Call.graphics
    Execution halted
    ```

# legendry

<details>

* Version: 0.2.1
* GitHub: https://github.com/teunbrand/legendry
* Source code: https://github.com/cran/legendry
* Date/Publication: 2025-03-04 22:50:02 UTC
* Number of recursive dependencies: 68

Run `revdepcheck::cloud_details(, "legendry")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘legendry-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: compose_crux
    > ### Title: Compose guides in a cross
    > ### Aliases: compose_crux
    > 
    > ### ** Examples
    > 
    > # Roughly recreating a colour bar with extra text on top and bottom
    ...
     12.                   └─legendry (local) draw(..., self = self)
     13.                     └─self$setup_elements(params, self$elements, theme)
     14.                       └─legendry (local) setup_elements(...)
     15.                         └─Guide$setup_elements(params, elements, theme)
     16.                           └─ggplot2 (local) setup_elements(...)
     17.                             └─base::lapply(elements[is_char], calc_element, theme = theme)
     18.                               └─ggplot2 (local) FUN(X[[i]], ...)
     19.                                 └─cli::cli_abort(...)
     20.                                   └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
      > # * https://testthat.r-lib.org/articles/special-files.html
    ...
      • primitive-fence/primitive-fence-radial.svg
      • primitive-labels/primitive-labels-radial.svg
      • primitive-line/primitive-line-radial.svg
      • primitive-segments/primitive-segments-radial.svg
      • primitive-spacer/primitive-spacer-radial.svg
      • primitive-ticks/primitive-ticks-radial.svg
      • primitive-title/primitive-title-radial.svg
      • scale-dendro/scale-dendro-radial.svg
      Error: Test failures
      Execution halted
    ```

# lemon

<details>

* Version: 0.5.0
* GitHub: https://github.com/stefanedwards/lemon
* Source code: https://github.com/cran/lemon
* Date/Publication: 2024-11-10 18:20:02 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::cloud_details(, "lemon")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘lemon-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: annotate_y_axis
    > ### Title: Annotations on the axis
    > ### Aliases: annotate_y_axis annotate_x_axis
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    > 
    > p <- ggplot(mtcars, aes(mpg, hp, colour=disp)) + geom_point()
    > 
    > l <- p + annotate_y_axis('mark at', y=200, tick=TRUE)
    > l
    Error in identicalUnits(x) : object is not a unit
    Calls: <Anonymous> ... polylineGrob -> is.unit -> unit.c -> identicalUnits
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(lemon)
      > 
      > 
      > if (TRUE) {
      +   test_check("lemon")
      + } #else {
    ...
       13.                   └─gtable::gtable_add_row_space(panel_table, theme$panel.spacing.y %||% theme$panel.spacing)
       14.                     └─cli::cli_abort("{.arg height} must be of length 1 or nrow - 1")
       15.                       └─rlang::abort(...)
      
      [ FAIL 2 | WARN 13 | SKIP 3 | PASS 138 ]
      Deleting unused snapshots:
      • facet/facet-rep-wrap-spacing.svg
      • facet_aux/facet-rep-wrap.svg
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘capped-axes.Rmd’ using rmarkdown
    ```

# lfproQC

<details>

* Version: 1.4.0
* GitHub: https://github.com/kabilansbio/lfproQC
* Source code: https://github.com/cran/lfproQC
* Date/Publication: 2024-10-10 13:10:02 UTC
* Number of recursive dependencies: 143

Run `revdepcheck::cloud_details(, "lfproQC")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘lfproQC-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Boxplot_data
    > ### Title: Creating Boxplot for a dataset
    > ### Aliases: Boxplot_data
    > 
    > ### ** Examples
    > 
    > Boxplot_data(yeast_data)
    Using Majority protein IDs as id variables
    Warning: Removed 269 rows containing non-finite outside the scale range
    (`stat_boxplot()`).
    Error in pm[[2]] : subscript out of bounds
    Calls: Boxplot_data -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘user_guide.Rmd’ using rmarkdown
    
    Quitting from user_guide.Rmd:54-56 [unnamed-chunk-8]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `pm[[2]]`:
    ! subscript out of bounds
    ---
    Backtrace:
    ...
    
    Error: processing vignette 'user_guide.Rmd' failed with diagnostics:
    subscript out of bounds
    --- failed re-building ‘user_guide.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘user_guide.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# LMoFit

<details>

* Version: 0.1.7
* GitHub: NA
* Source code: https://github.com/cran/LMoFit
* Date/Publication: 2024-05-14 07:33:23 UTC
* Number of recursive dependencies: 60

Run `revdepcheck::cloud_details(, "LMoFit")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘LMoFit.Rmd’ using rmarkdown
    
    Quitting from LMoFit.Rmd:235-237 [unnamed-chunk-15]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'LMoFit.Rmd' failed with diagnostics:
    ...
    ℹ Error occurred in the 1st layer.
    Caused by error in `is.waive()`:
    ! could not find function "is.waive"
    --- failed re-building ‘LMoFit.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘LMoFit.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        data   6.0Mb
    ```

# lnmixsurv

<details>

* Version: 3.1.6
* GitHub: NA
* Source code: https://github.com/cran/lnmixsurv
* Date/Publication: 2024-09-03 15:20:08 UTC
* Number of recursive dependencies: 196

Run `revdepcheck::cloud_details(, "lnmixsurv")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/tests.html
      > # * https://testthat.r-lib.org/reference/test_package.html#special-files
    ...
      <subscriptOutOfBoundsError/error/condition>
      Error in `pm[[2]]`: subscript out of bounds
      Backtrace:
          ▆
       1. └─testthat::expect_snapshot(plot(mod1)) at test-survival_ln_mixture_em-methods.R:25:3
       2.   └─rlang::cnd_signal(state$error)
      
      [ FAIL 1 | WARN 0 | SKIP 7 | PASS 50 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘compare.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.2Mb
      sub-directories of 1Mb or more:
        doc    4.0Mb
        libs   5.8Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘purrr’ ‘readr’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘rstanarm’
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# LorenzRegression

<details>

* Version: 2.1.0
* GitHub: https://github.com/AlJacq/LorenzRegression
* Source code: https://github.com/cran/LorenzRegression
* Date/Publication: 2024-10-11 16:50:02 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "LorenzRegression")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘LorenzRegression-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Lorenz.Reg
    > ### Title: Fits a Lorenz regression
    > ### Aliases: Lorenz.Reg
    > 
    > ### ** Examples
    > 
    > data(Data.Incomes)
    ...
     23.   └─vctrs::vec_default_cast(...)
     24.     ├─base::withRestarts(...)
     25.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     26.     │   └─base (local) doWithOneRestart(return(expr), restart)
     27.     └─vctrs::stop_incompatible_cast(...)
     28.       └─vctrs::stop_incompatible_type(...)
     29.         └─vctrs:::stop_incompatible(...)
     30.           └─vctrs:::stop_vctrs(...)
     31.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

# lpdensity

<details>

* Version: 2.5
* GitHub: NA
* Source code: https://github.com/cran/lpdensity
* Date/Publication: 2024-10-06 06:50:02 UTC
* Number of recursive dependencies: 26

Run `revdepcheck::cloud_details(, "lpdensity")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘lpdensity-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: lpdensity
    > ### Title: Local Polynomial Density Estimation and Inference
    > ### Aliases: lpdensity
    > 
    > ### ** Examples
    > 
    > # Generate a random sample
    ...
     20. └─vctrs (local) `<fn>`()
     21.   └─vctrs::vec_default_ptype2(...)
     22.     ├─base::withRestarts(...)
     23.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     24.     │   └─base (local) doWithOneRestart(return(expr), restart)
     25.     └─vctrs::stop_incompatible_type(...)
     26.       └─vctrs:::stop_incompatible(...)
     27.         └─vctrs:::stop_vctrs(...)
     28.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

# lphom

<details>

* Version: 0.3.5-6
* GitHub: NA
* Source code: https://github.com/cran/lphom
* Date/Publication: 2025-04-11 22:00:06 UTC
* Number of recursive dependencies: 28

Run `revdepcheck::cloud_details(, "lphom")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘lphom-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.lphom
    > ### Title: Graphical representation of a RxC ecological inference (vote
    > ###   transfer) matrix
    > ### Aliases: plot.lphom
    > 
    > ### ** Examples
    > 
    ...
    Backtrace:
        ▆
     1. ├─base::plot(mt.ns, show.plot = FALSE)
     2. └─lphom:::plot.lphom(mt.ns, show.plot = FALSE)
     3.   └─lphom (local) pintar_con(...)
     4.     └─ggplot2::scale_fill_continuous(...)
     5.       └─ggplot2:::scale_backward_compatibility(...)
     6.         └─cli::cli_abort("Unknown scale type: {.val {scale}}")
     7.           └─rlang::abort(...)
    Execution halted
    ```

# lspartition

<details>

* Version: 0.5
* GitHub: NA
* Source code: https://github.com/cran/lspartition
* Date/Publication: 2025-04-29 10:30:06 UTC
* Number of recursive dependencies: 32

Run `revdepcheck::cloud_details(, "lspartition")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘lspartition-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: lsprobust.plot
    > ### Title: Graphic Presentation of Results for 'lspartition' Package
    > ### Aliases: lsprobust.plot
    > 
    > ### ** Examples
    > 
    > x   <- runif(500)
    ...
     20. └─vctrs (local) `<fn>`()
     21.   └─vctrs::vec_default_ptype2(...)
     22.     ├─base::withRestarts(...)
     23.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     24.     │   └─base (local) doWithOneRestart(return(expr), restart)
     25.     └─vctrs::stop_incompatible_type(...)
     26.       └─vctrs:::stop_incompatible(...)
     27.         └─vctrs:::stop_vctrs(...)
     28.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

# manynet

<details>

* Version: 1.3.2
* GitHub: https://github.com/stocnet/manynet
* Source code: https://github.com/cran/manynet
* Date/Publication: 2024-11-05 20:50:02 UTC
* Number of recursive dependencies: 143

Run `revdepcheck::cloud_details(, "manynet")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        R           1.5Mb
        tutorials   1.9Mb
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘Rgraphviz’
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 7 marked UTF-8 strings
    ```

# mapindia

<details>

* Version: 1.0.1
* GitHub: https://github.com/shubhamdutta26/mapindia
* Source code: https://github.com/cran/mapindia
* Date/Publication: 2024-11-14 16:10:07 UTC
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "mapindia")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘getting_started.Rmd’ using rmarkdown
    ```

# marquee

<details>

* Version: 1.0.0
* GitHub: https://github.com/r-lib/marquee
* Source code: https://github.com/cran/marquee
* Date/Publication: 2025-01-20 16:01:56 UTC
* Number of recursive dependencies: 86

Run `revdepcheck::cloud_details(, "marquee")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘marquee-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: guide_marquee
    > ### Title: Marquee subtitle guide
    > ### Aliases: guide_marquee
    > 
    > ### ** Examples
    > 
    > ## Don't show: 
    ...
    Warning in split.default(glyphs, rep(seq_len(n_breaks), each = n_layers)) :
      data length is not a multiple of split variable
    Warning in max(unlist(width)) :
      no non-missing arguments to max; returning -Inf
    Warning in max(unlist(height)) :
      no non-missing arguments to max; returning -Inf
    Error in gridGTreeCoords(unname(lapply(x$children[x$childrenOrder], grobCoords,  : 
      Invalid gTree coordinates
    Calls: <Anonymous> ... lapply -> FUN -> grobCoords.gTree -> gridGTreeCoords
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.9Mb
      sub-directories of 1Mb or more:
        doc    2.4Mb
        libs   2.6Mb
    ```

# mcStats

<details>

* Version: 0.1.2
* GitHub: NA
* Source code: https://github.com/cran/mcStats
* Date/Publication: 2020-02-26 06:50:02 UTC
* Number of recursive dependencies: 53

Run `revdepcheck::cloud_details(, "mcStats")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(mcStats)
      > 
      > test_check("mcStats")
      [ FAIL 3 | WARN 2 | SKIP 0 | PASS 5 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      * Removed 420 rows containing missing values or values outside the scale range
      (`geom_area()`).
      * Removed 442 rows containing missing values or values outside the scale range
      (`geom_area()`).
      * Removed 184 rows containing missing values or values outside the scale range
      (`geom_area()`).
      
      [ FAIL 3 | WARN 2 | SKIP 0 | PASS 5 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# MendelianRandomization

<details>

* Version: 0.10.0
* GitHub: NA
* Source code: https://github.com/cran/MendelianRandomization
* Date/Publication: 2024-04-12 10:10:02 UTC
* Number of recursive dependencies: 86

Run `revdepcheck::cloud_details(, "MendelianRandomization")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘MendelianRandomization-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: mr_plot
    > ### Title: Draw a scatter plot of the genetic associations and/or causal
    > ###   estimates
    > ### Aliases: mr_plot mr_plot,MRInput-method mr_plot,MRAll-method
    > ###   mr_plot,MRMVInput-method
    > 
    > ### ** Examples
    > 
    > mr_plot(mr_input(bx = ldlc, bxse = ldlcse, by = chdlodds, byse = chdloddsse),
    +   line="egger", orientate = TRUE)
    Error in pm[[2]] : subscript out of bounds
    Calls: mr_plot ... plotly_build -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        libs   4.2Mb
    ```

# MetAlyzer

<details>

* Version: 1.1.0
* GitHub: https://github.com/nilsmechtel/MetAlyzer
* Source code: https://github.com/cran/MetAlyzer
* Date/Publication: 2024-12-06 14:00:02 UTC
* Number of recursive dependencies: 95

Run `revdepcheck::cloud_details(, "MetAlyzer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘MetAlyzer-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotly_scatter
    > ### Title: Plotly Log2FC Scatter Plot
    > ### Aliases: plotly_scatter
    > 
    > ### ** Examples
    > 
    > 
    ...
    Warning: Partial NA coefficients for 2 probe(s)
    > 
    > p_scatter <- plotly_scatter(metalyzer_se)
    Warning in geom_rect(data = rects_df, inherit.aes = FALSE, aes(xmin = .data$Start,  :
      Ignoring unknown aesthetics: text
    Warning in geom_point(size = 0.5, aes(text = paste0(.data$Metabolite, "\nClass: ",  :
      Ignoring unknown aesthetics: text
    Error in pm[[2]] : subscript out of bounds
    Calls: plotly_scatter -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# MetaNet

<details>

* Version: 0.2.5
* GitHub: https://github.com/Asa12138/MetaNet
* Source code: https://github.com/cran/MetaNet
* Date/Publication: 2025-04-10 06:30:02 UTC
* Number of recursive dependencies: 174

Run `revdepcheck::cloud_details(, "MetaNet")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘MetaNet-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: as.ggig
    > ### Title: Transfer an igraph object to a ggig
    > ### Aliases: as.ggig
    > 
    > ### ** Examples
    > 
    > as.ggig(co_net, coors = c_net_layout(co_net)) -> ggig
    ...
      2. └─ggplot2:::print.ggplot(x)
      3.   ├─ggplot2::ggplot_build(x)
      4.   └─ggplot2:::ggplot_build.ggplot(x)
      5.     └─npscales$set_palettes(plot$theme)
      6.       └─ggplot2 (local) set_palettes(..., self = self)
      7.         ├─scales::as_continuous_pal(elem)
      8.         └─scales:::as_continuous_pal.default(elem)
      9.           └─cli::cli_abort("Cannot convert {.arg x} to a continuous palette.")
     10.             └─rlang::abort(...)
    Execution halted
    ```

# metaquant

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/metaquant
* Date/Publication: 2025-02-11 17:00:02 UTC
* Number of recursive dependencies: 76

Run `revdepcheck::cloud_details(, "metaquant")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘metaquant-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotdist
    > ### Title: Visualising Densities using Quantiles
    > ### Aliases: plotdist
    > 
    > ### ** Examples
    > 
    > #Example dataset of 3-point summaries (min, med, max) for 2 groups
    ...
    +   display.legend = TRUE,
    +   pooled.dist = TRUE
    + )
    Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ℹ Please use `linewidth` instead.
    ℹ The deprecated feature was likely used in the metaquant package.
      Please report the issue to the authors.
    Error in pm[[2]] : subscript out of bounds
    Calls: plotdist -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# MetBrewer

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/MetBrewer
* Date/Publication: 2022-03-21 13:30:02 UTC
* Number of recursive dependencies: 26

Run `revdepcheck::cloud_details(, "MetBrewer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘MetBrewer-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: met.brewer
    > ### Title: Met Palette Generator
    > ### Aliases: met.brewer
    > ### Keywords: colors
    > 
    > ### ** Examples
    > 
    ...
     20. └─vctrs (local) `<fn>`()
     21.   └─vctrs::vec_default_ptype2(...)
     22.     ├─base::withRestarts(...)
     23.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     24.     │   └─base (local) doWithOneRestart(return(expr), restart)
     25.     └─vctrs::stop_incompatible_type(...)
     26.       └─vctrs:::stop_incompatible(...)
     27.         └─vctrs:::stop_vctrs(...)
     28.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

# metR

<details>

* Version: 0.18.1
* GitHub: https://github.com/eliocamp/metR
* Source code: https://github.com/cran/metR
* Date/Publication: 2025-05-13 06:30:01 UTC
* Number of recursive dependencies: 119

Run `revdepcheck::cloud_details(, "metR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘metR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_contour_fill
    > ### Title: Filled 2d contours of a 3d surface
    > ### Aliases: geom_contour_fill stat_contour_fill StatContourFill
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    ...
    > ggplot(surface, aes(Var1, Var2, z = value)) + geom_contour_fill() + geom_contour(color = "black", 
    +     size = 0.1)
    Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ℹ Please use `linewidth` instead.
    > ggplot(surface, aes(Var1, Var2, z = value)) + geom_contour_fill(aes(fill = after_stat(level)))
    > ggplot(surface, aes(Var1, Var2, z = value)) + geom_contour_fill(aes(fill = after_stat(level_d)))
    Error in train(..., self = self) : 
      unused argument (call = scale_fill_discretised())
    Calls: <Anonymous> ... <Anonymous> -> train_df -> <Anonymous> -> train -> <Anonymous>
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(metR)
      > # library(vdiffr)
      > 
      > on_cran <- !isTRUE(as.logical(Sys.getenv("NOT_CRAN", "false")))
      > if (on_cran) data.table::setDTthreads(2)
      > 
    ...
       15.                       ├─ggplot2::guide_gengrob(params, theme)
       16.                       └─metR:::guide_gengrob.colorstrip(params, theme)
       17.                         └─grid::convertWidth(...)
       18.                           └─grid::convertUnit(...)
       19.                             ├─grid:::upgradeUnit(x)
       20.                             └─grid:::upgradeUnit.default(x)
      
      [ FAIL 1 | WARN 2 | SKIP 19 | PASS 184 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Visualization-tools.Rmd’ using knitr
    
    Quitting from Visualization-tools.Rmd:181-184 [unnamed-chunk-12]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `train()`:
    ! unused argument (call = scale_fill_discretised())
    ---
    Backtrace:
    ...
    --- failed re-building ‘Visualization-tools.Rmd’
    
    --- re-building ‘Working-with-data.Rmd’ using knitr
    --- finished re-building ‘Working-with-data.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Visualization-tools.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.6Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        data   2.0Mb
        doc    1.8Mb
    ```

# mgcViz

<details>

* Version: 0.2.0
* GitHub: https://github.com/mfasiolo/mgcViz
* Source code: https://github.com/cran/mgcViz
* Date/Publication: 2025-04-11 11:30:07 UTC
* Number of recursive dependencies: 111

Run `revdepcheck::cloud_details(, "mgcViz")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘mgcviz.rmd’ using rmarkdown
    ```

# MIC

<details>

* Version: 1.0.2
* GitHub: https://github.com/agerada/MIC
* Source code: https://github.com/cran/MIC
* Date/Publication: 2025-02-07 09:40:02 UTC
* Number of recursive dependencies: 145

Run `revdepcheck::cloud_details(, "MIC")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘MIC-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.mic_validation
    > ### Title: Plot MIC validation results
    > ### Aliases: plot.mic_validation
    > 
    > ### ** Examples
    > 
    > gold_standard <- c("<0.25", "8", "64", ">64")
    ...
      3.   ├─ggplot2::ggplot_gtable(data)
      4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
      5.     └─layout$render(geom_grobs, data, theme, plot$labels)
      6.       └─ggplot2 (local) render(..., self = self)
      7.         └─self$facet$draw_panels(...)
      8.           └─lemon (local) draw_panels(...)
      9.             └─gtable::gtable_add_col_space(panel_table, theme$panel.spacing.x %||% theme$panel.spacing)
     10.               └─cli::cli_abort("{.arg width} must be of length 1 or ncol - 1")
     11.                 └─rlang::abort(...)
    Execution halted
    ```

# migraph

<details>

* Version: 1.4.5
* GitHub: https://github.com/stocnet/migraph
* Source code: https://github.com/cran/migraph
* Date/Publication: 2024-12-02 10:00:10 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "migraph")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(manynet)
      > library(migraph)
      > 
      > test_check("migraph")
      Starting 2 test processes
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 46 ]
    ...
      `expected` is a character vector ('Statistic')
      ── Failure ('test-model_tests.R:73:3'): qap plot works ─────────────────────────
      qapplot$labels$x (`actual`) not identical to "Statistic" (`expected`).
      
      `actual` is NULL
      `expected` is a character vector ('Statistic')
      
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 46 ]
      Error: Test failures
      Execution halted
    ```

# mikropml

<details>

* Version: 1.6.1
* GitHub: https://github.com/SchlossLab/mikropml
* Source code: https://github.com/cran/mikropml
* Date/Publication: 2023-08-21 15:10:05 UTC
* Number of recursive dependencies: 128

Run `revdepcheck::cloud_details(, "mikropml")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(mikropml)
      > test_check("mikropml")
      Fraction of data in the training set: 0.778 
      	Groups in the training set: A C D 
      	Groups in the testing set: B
      Fraction of data in the training set: 0.778 
    ...
      `names(expected)` is absent
      ── Failure ('test-plot.R:140:3'): plot_mean_prc uses geom ribbon, line, and hline ──
      ... %>% unlist() (`actual`) not equal to c(...) (`expected`).
      
      `names(actual)` is a character vector ('geom_ribbon1', 'geom_ribbon2', 'geom_ribbon3', 'geom_ribbon4', 'geom_line1', ...)
      `names(expected)` is absent
      
      [ FAIL 2 | WARN 19 | SKIP 12 | PASS 314 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) create_grouped_data_partition.Rd:60: Lost braces; missing escapes or markup?
        60 | Zena Lapp, {zenalapp@umich.edu}
           |            ^
    checkRd: (-1) create_grouped_data_partition.Rd:62: Lost braces; missing escapes or markup?
        62 | Kelly Sovacool, {sovacool@umich.edu}
           |                 ^
    checkRd: (-1) create_grouped_k_multifolds.Rd:30: Lost braces; missing escapes or markup?
        30 | Zena Lapp, {zenalapp@umich.edu}
           |            ^
    checkRd: (-1) get_partition_indices.Rd:58: Lost braces; missing escapes or markup?
        58 | Kelly Sovacool, {sovacool@umich.edu}
           |                 ^
    checkRd: (-1) set_hparams_glmnet.Rd:16: Lost braces; missing escapes or markup?
        16 | Zena Lapp, {zenalapp@umich.edu}
           |            ^
    ```

# MiMIR

<details>

* Version: 1.5
* GitHub: NA
* Source code: https://github.com/cran/MiMIR
* Date/Publication: 2024-02-01 08:50:02 UTC
* Number of recursive dependencies: 193

Run `revdepcheck::cloud_details(, "MiMIR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘MiMIR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: LOBOV_accuracies
    > ### Title: LOBOV_accuracies
    > ### Aliases: LOBOV_accuracies
    > 
    > ### ** Examples
    > 
    > require(pROC)
    ...
      56 metabolites x  500 samples 
    | Pruning samples on5SD:
      56 metabolites x  500 samples 
    | Performing scaling ...  DONE!
    | Imputation ...  DONE!
    > p_avail<-colnames(b_p)[c(1:5)]
    > LOBOV_accuracies(sur$surrogates, b_p, p_avail, MiMIR::acc_LOBOV)
    Error in pm[[2]] : subscript out of bounds
    Calls: LOBOV_accuracies -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# miRetrieve

<details>

* Version: 1.3.4
* GitHub: NA
* Source code: https://github.com/cran/miRetrieve
* Date/Publication: 2021-09-18 17:30:02 UTC
* Number of recursive dependencies: 124

Run `revdepcheck::cloud_details(, "miRetrieve")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(miRetrieve)
      > 
      > test_check("miRetrieve")
      [ FAIL 1 | WARN 15 | SKIP 0 | PASS 202 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      Backtrace:
          ▆
       1. └─miRetrieve::compare_mir_terms_scatter(df_merged, "miR-21", title = "Test_title") at test-comparemirterms.R:56:1
       2.   ├─plotly::ggplotly(plot)
       3.   └─plotly:::ggplotly.ggplot(plot)
       4.     └─plotly::gg2list(...)
      
      [ FAIL 1 | WARN 15 | SKIP 0 | PASS 202 ]
      Error: Test failures
      Execution halted
    ```

# MiscMetabar

<details>

* Version: 0.14.2
* GitHub: https://github.com/adrientaudiere/MiscMetabar
* Source code: https://github.com/cran/MiscMetabar
* Date/Publication: 2025-03-20 15:20:02 UTC
* Number of recursive dependencies: 419

Run `revdepcheck::cloud_details(, "MiscMetabar")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(MiscMetabar)
      Loading required package: phyloseq
      Loading required package: ggplot2
      Loading required package: dada2
      Loading required package: Rcpp
    ...
        'test_tuckey.R:26:3', 'test_targets.R:5:3', 'test_targets.R:56:3',
        'test_targets.R:84:3', 'test_targets.R:101:3', 'test_targets.R:111:3'
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test_figures_beta_div.R:72:5'): graph_test_pq works ───────────────
      `graph_test_pq(data_fungi_mini, fact = "Tree_name")` produced warnings.
      
      [ FAIL 1 | WARN 4 | SKIP 74 | PASS 81 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        data      2.0Mb
        extdata   1.2Mb
    ```

# misspi

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/misspi
* Date/Publication: 2023-10-17 09:50:02 UTC
* Number of recursive dependencies: 86

Run `revdepcheck::cloud_details(, "misspi")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘misspi-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: evaliq
    > ### Title: Evaluate the Imputation Quality
    > ### Aliases: evaliq
    > 
    > ### ** Examples
    > 
    > # A very quick example
    ...
    > # Default plot
    > er.eval <- evaliq(x.true[na.idx], x.est[na.idx])
    `geom_smooth()` using formula = 'y ~ x'
    > 
    > # Interactive plot
    > er.eval <- evaliq(x.true[na.idx], x.est[na.idx], interactive = TRUE)
    `geom_smooth()` using formula = 'y ~ x'
    Error in pm[[2]] : subscript out of bounds
    Calls: evaliq -> print -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# misty

<details>

* Version: 0.7.1
* GitHub: NA
* Source code: https://github.com/cran/misty
* Date/Publication: 2025-03-10 17:40:07 UTC
* Number of recursive dependencies: 86

Run `revdepcheck::cloud_details(, "misty")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘misty-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: na.pattern
    > ### Title: Missing Data Pattern
    > ### Aliases: na.pattern
    > 
    > ### ** Examples
    > 
    > # Example 1: Compute a summary of missing data patterns
    ...
     23.                               └─ggplot2 (local) extract_key(...)
     24.                                 └─Guide$extract_key(scale, aesthetic, ...)
     25.                                   └─ggplot2 (local) extract_key(...)
     26.                                     └─scale$get_labels(breaks)
     27.                                       └─ggplot2 (local) get_labels(..., self = self)
     28.                                         └─self$scale$get_labels(breaks)
     29.                                           └─ggplot2 (local) get_labels(..., self = self)
     30.                                             └─cli::cli_abort(...)
     31.                                               └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        R      4.0Mb
        help   1.5Mb
    ```

# mizer

<details>

* Version: 2.5.3
* GitHub: https://github.com/sizespectrum/mizer
* Source code: https://github.com/cran/mizer
* Date/Publication: 2024-10-17 07:10:09 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::cloud_details(, "mizer")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
    ...
      • plots/plot-feeding-level.svg
      • plots/plot-single-growth-curve.svg
      • plots/plot-spectra.svg
      • plots/plot-yield-by-gear.svg
      • plots/plot-yield.svg
      • plots/plotfishing-mortality.svg
      • plots/plotfmort-truncated.svg
      • plots/plotpredation-mortality.svg
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        help   1.8Mb
    ```

# MKpower

<details>

* Version: 1.0
* GitHub: https://github.com/stamats/MKpower
* Source code: https://github.com/cran/MKpower
* Date/Publication: 2024-09-23 14:30:01 UTC
* Number of recursive dependencies: 123

Run `revdepcheck::cloud_details(, "MKpower")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘MKpower-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: qqunif
    > ### Title: qq-Plots for Uniform Distribution
    > ### Aliases: qqunif qqunif.default qqunif.sim.power.ttest
    > ###   qqunif.sim.power.wtest
    > ### Keywords: hplot
    > 
    > ### ** Examples
    ...
     23. │                         └─ggplot2 (local) FUN(X[[i]], ...)
     24. │                           └─self$draw_group(group, panel_params, coord, ...)
     25. │                             └─ggplot2 (local) draw_group(...)
     26. │                               └─ggplot2 (local) draw_group(..., self = self)
     27. └─base::.handleSimpleError(...)
     28.   └─rlang (local) h(simpleError(msg, call))
     29.     └─handlers[[1L]](cnd)
     30.       └─cli::cli_abort(...)
     31.         └─rlang::abort(...)
    Execution halted
    ```

# modeltime.resample

<details>

* Version: 0.2.3
* GitHub: https://github.com/business-science/modeltime.resample
* Source code: https://github.com/cran/modeltime.resample
* Date/Publication: 2023-04-12 15:50:02 UTC
* Number of recursive dependencies: 235

Run `revdepcheck::cloud_details(, "modeltime.resample")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > 
      > # Machine Learning
      > library(tidymodels)
      ── Attaching packages ────────────────────────────────────── tidymodels 1.3.0 ──
      ✔ broom        1.0.8          ✔ recipes      1.3.0     
      ✔ dials        1.4.0          ✔ rsample      1.3.0     
    ...
          ▆
       1. ├─m750_models_resample %>% ... at test-modeltime_fit_resamples.R:116:5
       2. └─modeltime.resample::plot_modeltime_resamples(., .interactive = TRUE)
       3.   ├─plotly::ggplotly(g)
       4.   └─plotly:::ggplotly.ggplot(g)
       5.     └─plotly::gg2list(...)
      
      [ FAIL 1 | WARN 4 | SKIP 0 | PASS 16 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘crayon’ ‘dials’ ‘glue’ ‘parsnip’
      All declared Imports should be used.
    ```

# modgo

<details>

* Version: 1.0.1
* GitHub: NA
* Source code: https://github.com/cran/modgo
* Date/Publication: 2024-09-11 16:20:02 UTC
* Number of recursive dependencies: 71

Run `revdepcheck::cloud_details(, "modgo")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘modgo-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: distr_plots
    > ### Title: Plots distribution of original and simulated data
    > ### Aliases: distr_plots
    > 
    > ### ** Examples
    > 
    > data("Cleveland",package="modgo")
    ...
     26. └─vctrs (local) `<fn>`()
     27.   └─vctrs::vec_default_ptype2(...)
     28.     ├─base::withRestarts(...)
     29.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     30.     │   └─base (local) doWithOneRestart(return(expr), restart)
     31.     └─vctrs::stop_incompatible_type(...)
     32.       └─vctrs:::stop_incompatible(...)
     33.         └─vctrs:::stop_vctrs(...)
     34.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘modgo_vignette.Rmd’ using rmarkdown
    ```

# mpactr

<details>

* Version: 0.2.1
* GitHub: https://github.com/mums2/mpactr
* Source code: https://github.com/cran/mpactr
* Date/Publication: 2025-03-29 00:30:05 UTC
* Number of recursive dependencies: 143

Run `revdepcheck::cloud_details(, "mpactr")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘downstream_analyses.Rmd’ using rmarkdown
    Warning: Duplicated chunk option(s) 'warning' in both chunk header and pipe comments of the chunk 'unnamed-chunk-18'.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.0Mb
      sub-directories of 1Mb or more:
        doc       4.9Mb
        extdata   1.9Mb
        libs      1.6Mb
    ```

# MPTmultiverse

<details>

* Version: 0.4-2
* GitHub: https://github.com/mpt-network/MPTmultiverse
* Source code: https://github.com/cran/MPTmultiverse
* Date/Publication: 2020-06-24 09:40:11 UTC
* Number of recursive dependencies: 97

Run `revdepcheck::cloud_details(, "MPTmultiverse")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘MPTmultiverse-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: fit_mpt
    > ### Title: Multiverse Analysis for MPT Models
    > ### Aliases: fit_mpt
    > 
    > ### ** Examples
    > 
    > 
    ...
    Backtrace:
        ▆
     1. ├─base::plot(fit_all, which = "est")
     2. └─MPTmultiverse:::plot.multiverseMPT(fit_all, which = "est")
     3.   └─MPTmultiverse:::plot_est(x, shapes = shapes)
     4.     └─ggplot2::facet_grid(facets = ". ~ condition")
     5.       └─lifecycle::deprecate_stop("2.2.0", "facet_grid(facets)", "facet_grid(rows)")
     6.         └─lifecycle:::deprecate_stop0(msg)
     7.           └─rlang::cnd_signal(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘introduction-bayen_kuhlmann_2011.rmd’ using rmarkdown
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) fit_mpt.Rd:139-142: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) fit_mpt.Rd:143-148: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) fit_mpt.Rd:149-159: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) fit_mpt.Rd:167-168: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) fit_mpt.Rd:169: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) fit_mpt.Rd:170: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) fit_mpt.Rd:171: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) fit_mpt.Rd:176-177: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) fit_mpt.Rd:178-179: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) fit_mpt.Rd:180-181: Lost braces in \itemize; meant \describe ?
    ...
    checkRd: (-1) mpt_options.Rd:18: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) mpt_options.Rd:19: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) mpt_options.Rd:20: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) mpt_options.Rd:21: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) mpt_options.Rd:22: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) mpt_options.Rd:23: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) mpt_options.Rd:24: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) mpt_options.Rd:25: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) mpt_options.Rd:26: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) mpt_options.Rd:27: Lost braces in \itemize; meant \describe ?
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# MRZero

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/MRZero
* Date/Publication: 2024-04-14 09:30:03 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "MRZero")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘MRZero-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: mr_plot
    > ### Title: Draw a scatter plot of the genetic associations and/or causal
    > ###   estimates
    > ### Aliases: mr_plot mr_plot,MRInput-method mr_plot,MRAll-method
    > ###   mr_plot,MRMVInput-method
    > 
    > ### ** Examples
    > 
    > mr_plot(mr_input(bx = ldlc, bxse = ldlcse, by = chdlodds, byse = chdloddsse),
    +   line="egger", orientate = TRUE)
    Error in pm[[2]] : subscript out of bounds
    Calls: mr_plot ... plotly_build -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# mstate

<details>

* Version: 0.3.3
* GitHub: https://github.com/hputter/mstate
* Source code: https://github.com/cran/mstate
* Date/Publication: 2024-07-11 21:30:06 UTC
* Number of recursive dependencies: 114

Run `revdepcheck::cloud_details(, "mstate")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mstate-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: vis.multiple.pt
    > ### Title: Visualise multiple probtrans objects
    > ### Aliases: vis.multiple.pt
    > 
    > ### ** Examples
    > 
    > 
    ...
     20. └─vctrs (local) `<fn>`()
     21.   └─vctrs::vec_default_ptype2(...)
     22.     ├─base::withRestarts(...)
     23.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     24.     │   └─base (local) doWithOneRestart(return(expr), restart)
     25.     └─vctrs::stop_incompatible_type(...)
     26.       └─vctrs:::stop_incompatible(...)
     27.         └─vctrs:::stop_vctrs(...)
     28.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘visuals_demo.Rmd’ using rmarkdown
    ```

# MultiTraits

<details>

* Version: 0.5.0
* GitHub: NA
* Source code: https://github.com/cran/MultiTraits
* Date/Publication: 2025-04-20 18:40:02 UTC
* Number of recursive dependencies: 137

Run `revdepcheck::cloud_details(, "MultiTraits")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘MultiTraits-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: CSR_plot
    > ### Title: Create a ternary plot of CSR strategies
    > ### Aliases: CSR_plot
    > 
    > ### ** Examples
    > 
    > data(PFF)
    ...
    5  51   14  31  4.068212  0.000000 95.93179    R
    6  66   27  13  5.131786 73.958953 20.90926 S/SR
    > CSR_plot(data=result)
    Warning: The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
    ℹ Please use the `linewidth` argument instead.
    ℹ The deprecated feature was likely used in the ggtern package.
      Please report the issue to the authors.
    Error in validate_theme(theme) : could not find function "validate_theme"
    Calls: <Anonymous> ... ggplot_build.ggplot -> layers_add_or_remove_mask -> <Anonymous>
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘MultiTraits_tutorial.Rmd’ using rmarkdown
    
    Quitting from MultiTraits_tutorial.Rmd:105-120 [unnamed-chunk-4]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `validate_theme()`:
    ! could not find function "validate_theme"
    ---
    Backtrace:
    ...
    
    Error: processing vignette 'MultiTraits_tutorial.Rmd' failed with diagnostics:
    could not find function "validate_theme"
    --- failed re-building ‘MultiTraits_tutorial.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘MultiTraits_tutorial.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking whether package ‘MultiTraits’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: The `size` argument of `element_rect()` is deprecated as of ggplot2 3.4.0.
    See ‘/tmp/workdir/MultiTraits/new/MultiTraits.Rcheck/00install.out’ for details.
    ```

# mvdalab

<details>

* Version: 1.7
* GitHub: NA
* Source code: https://github.com/cran/mvdalab
* Date/Publication: 2022-10-05 23:00:14 UTC
* Number of recursive dependencies: 68

Run `revdepcheck::cloud_details(, "mvdalab")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mvdalab-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.plusminus
    > ### Title: 2D Graph of the PCA scores associated with a plusminusFit
    > ### Aliases: plot.plusminus
    > 
    > ### ** Examples
    > 
    > ###  PLUS-Minus CLASSIFIER WITH validation = 'none', i.e. no CV ###
    ...
     12. │             └─ggplot2 (local) FUN(X[[i]], ...)
     13. │               └─scale$map_df(df = df)
     14. │                 └─ggplot2 (local) map_df(..., self = self)
     15. │                   └─base::lapply(aesthetics, function(j) self$map(df[[j]]))
     16. │                     └─ggplot2 (local) FUN(X[[i]], ...)
     17. │                       └─self$map(df[[j]])
     18. │                         └─ggplot2 (local) map(..., self = self)
     19. │                           └─vctrs::`vec_slice<-`(`*tmp*`, is.na(x), value = na_value)
     20. └─rlang::cnd_signal(x)
    Execution halted
    ```

# NatParksPalettes

<details>

* Version: 0.2.0
* GitHub: https://github.com/kevinsblake/NatParksPalettes
* Source code: https://github.com/cran/NatParksPalettes
* Date/Publication: 2022-10-09 21:10:02 UTC
* Number of recursive dependencies: 26

Run `revdepcheck::cloud_details(, "NatParksPalettes")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘NatParksPalettes-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: natparks.pals
    > ### Title: National Parks Palette Generator
    > ### Aliases: natparks.pals
    > ### Keywords: colors
    > 
    > ### ** Examples
    > 
    ...
     20. └─vctrs (local) `<fn>`()
     21.   └─vctrs::vec_default_ptype2(...)
     22.     ├─base::withRestarts(...)
     23.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     24.     │   └─base (local) doWithOneRestart(return(expr), restart)
     25.     └─vctrs::stop_incompatible_type(...)
     26.       └─vctrs:::stop_incompatible(...)
     27.         └─vctrs:::stop_vctrs(...)
     28.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

# neatmaps

<details>

* Version: 2.1.0
* GitHub: https://github.com/PhilBoileau/neatmaps
* Source code: https://github.com/cran/neatmaps
* Date/Publication: 2019-05-12 19:10:03 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "neatmaps")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘neatmaps-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: consClustResTable
    > ### Title: Consensus Cluster Results in a Table
    > ### Aliases: consClustResTable
    > 
    > ### ** Examples
    > 
    > # create the data frame using the network, node and edge attributes
    ...
    > # run the neatmap code on df
    > neat_res <- neatmap(df, scale_df = "ecdf", max_k = 3, reps = 100, 
    +                     xlab = "vars", ylab = "nets", xlab_cex = 1, ylab_cex = 1)
    Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ℹ Please use `linewidth` instead.
    ℹ The deprecated feature was likely used in the dendextend package.
      Please report the issue at <https://github.com/talgalili/dendextend/issues>.
    Error in pm[[2]] : subscript out of bounds
    Calls: neatmap ... %>% -> layout -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
    ```

# NHSRplotthedots

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/NHSRplotthedots
* Date/Publication: 2021-11-03 20:20:10 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::cloud_details(, "NHSRplotthedots")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(NHSRplotthedots)
      > 
      > test_check("NHSRplotthedots")
      [ FAIL 1 | WARN 733 | SKIP 3 | PASS 431 ]
      
    ...
      
      `actual$type` is absent
      `expected$type` is a character vector ('type')
      
      `actual$text` is absent
      `expected$text` is a character vector ('text')
      
      [ FAIL 1 | WARN 733 | SKIP 3 | PASS 431 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘NHSRdatasets’ ‘grid’ ‘utils’
      All declared Imports should be used.
    ```

# nic

<details>

* Version: 0.0.2
* GitHub: https://github.com/thiyangt/nic
* Source code: https://github.com/cran/nic
* Date/Publication: 2023-03-13 11:30:02 UTC
* Number of recursive dependencies: 56

Run `revdepcheck::cloud_details(, "nic")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘color-palette-demo.Rmd’ using rmarkdown
    ```

# NIMAA

<details>

* Version: 0.2.1
* GitHub: https://github.com/jafarilab/NIMAA
* Source code: https://github.com/cran/NIMAA
* Date/Publication: 2022-04-11 14:12:45 UTC
* Number of recursive dependencies: 180

Run `revdepcheck::cloud_details(, "NIMAA")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘NIMAA-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: extractSubMatrix
    > ### Title: Extract the non-missing submatrices from a given matrix.
    > ### Aliases: extractSubMatrix
    > 
    > ### ** Examples
    > 
    > # load part of the beatAML data
    ...
    ℹ See also `vignette("ggplot2-in-packages")` for more information.
    ℹ The deprecated feature was likely used in the NIMAA package.
      Please report the issue at <https://github.com/jafarilab/NIMAA/issues>.
    Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ℹ Please use `linewidth` instead.
    ℹ The deprecated feature was likely used in the NIMAA package.
      Please report the issue at <https://github.com/jafarilab/NIMAA/issues>.
    Error in pm[[2]] : subscript out of bounds
    Calls: extractSubMatrix ... plotSubmatrix -> print -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(NIMAA)
      > 
      > test_check("NIMAA")
      binmatnest.temperature 
                    13.21232 
      Size of Square: 	 66 rows x  66 columns 
    ...
       1. └─NIMAA::extractSubMatrix(...) at test-extract-nonmissing-submatrix.R:5:3
       2.   └─NIMAA:::plotSubmatrix(...)
       3.     ├─base::print(plotly::ggplotly(p))
       4.     ├─plotly::ggplotly(p)
       5.     └─plotly:::ggplotly.ggplot(p)
       6.       └─plotly::gg2list(...)
      
      [ FAIL 1 | WARN 8 | SKIP 0 | PASS 7 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘NIMAA-vignette.Rmd’ using rmarkdown
    
    Quitting from NIMAA-vignette.Rmd:48-57 [plotIncMatrix function]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `pm[[2]]`:
    ! subscript out of bounds
    ---
    Backtrace:
    ...
    
    Error: processing vignette 'NIMAA-vignette.Rmd' failed with diagnostics:
    subscript out of bounds
    --- failed re-building ‘NIMAA-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘NIMAA-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        doc    4.0Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 24 marked UTF-8 strings
    ```

# nlmixr2plot

<details>

* Version: 3.0.1
* GitHub: https://github.com/nlmixr2/nlmixr2plot
* Source code: https://github.com/cran/nlmixr2plot
* Date/Publication: 2025-02-14 15:10:02 UTC
* Number of recursive dependencies: 160

Run `revdepcheck::cloud_details(, "nlmixr2plot")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(nlmixr2plot)
      > 
      > test_check("nlmixr2plot")
      > loading into symengine environment...
      > pruning branches (`if`/`else`) of saem model...
      v done
    ...
      ══ Skipped tests (2) ═══════════════════════════════════════════════════════════
      • On CRAN (2): 'test-plots-cens.R:2:3', 'test-plots-multiple-endpoints.R:2:3'
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test-add.R:41:3'): test addition operator ───────────────────────────
      Error in `pl + ggplot2::xlab("cool")`: non-numeric argument to binary operator
      
      [ FAIL 1 | WARN 5 | SKIP 2 | PASS 30 ]
      Error: Test failures
      Execution halted
    ```

# nonmem2R

<details>

* Version: 0.2.5
* GitHub: NA
* Source code: https://github.com/cran/nonmem2R
* Date/Publication: 2024-03-11 17:30:02 UTC
* Number of recursive dependencies: 62

Run `revdepcheck::cloud_details(, "nonmem2R")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘nonmem2R-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: basic.GOF4
    > ### Title: Basic 4- and 6-panel GOF
    > ### Aliases: basic.GOF4 basic.GOF6
    > 
    > ### ** Examples
    > 
    > # Get path to the example files included in nonmem2R package
    ...
     21. │           └─l$compute_position(d, layout)
     22. │             └─ggplot2 (local) compute_position(..., self = self)
     23. │               └─self$position$setup_data(data, params)
     24. │                 └─ggplot2 (local) setup_data(..., self = self)
     25. └─base::.handleSimpleError(...)
     26.   └─rlang (local) h(simpleError(msg, call))
     27.     └─handlers[[1L]](cnd)
     28.       └─cli::cli_abort(...)
     29.         └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘GOFvignette.Rmd’ using rmarkdown
    ```

# normfluodbf

<details>

* Version: 2.0.0
* GitHub: https://github.com/AlphaPrime7/normfluodbf
* Source code: https://github.com/cran/normfluodbf
* Date/Publication: 2024-09-27 23:10:03 UTC
* Number of recursive dependencies: 127

Run `revdepcheck::cloud_details(, "normfluodbf")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
      > # * https://testthat.r-lib.org/articles/special-files.html
    ...
          ▆
       1. ├─... %>% ... at test_plot_dev.R:13:3
       2. └─normfluodbf:::plot_dev_with_custom_legends(...)
       3.   ├─plotly::ggplotly(p)
       4.   └─plotly:::ggplotly.ggplot(p)
       5.     └─plotly::gg2list(...)
      
      [ FAIL 4 | WARN 613 | SKIP 0 | PASS 15 ]
      Error: Test failures
      Execution halted
    ```

# nprobust

<details>

* Version: 0.5.0
* GitHub: NA
* Source code: https://github.com/cran/nprobust
* Date/Publication: 2025-04-14 07:50:06 UTC
* Number of recursive dependencies: 28

Run `revdepcheck::cloud_details(, "nprobust")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘nprobust-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: nprobust.plot
    > ### Title: Graphical Presentation of Results from 'nprobust' Package.
    > ### Aliases: nprobust.plot
    > 
    > ### ** Examples
    > 
    > x   <- runif(500) 
    ...
     20. └─vctrs (local) `<fn>`()
     21.   └─vctrs::vec_default_ptype2(...)
     22.     ├─base::withRestarts(...)
     23.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     24.     │   └─base (local) doWithOneRestart(return(expr), restart)
     25.     └─vctrs::stop_incompatible_type(...)
     26.       └─vctrs:::stop_incompatible(...)
     27.         └─vctrs:::stop_vctrs(...)
     28.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

# nuts

<details>

* Version: 1.1.0
* GitHub: https://github.com/ropensci/nuts
* Source code: https://github.com/cran/nuts
* Date/Publication: 2024-07-13 10:50:02 UTC
* Number of recursive dependencies: 167

Run `revdepcheck::cloud_details(, "nuts")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘nuts.Rmd’ using knitr
    ```

# nzelect

<details>

* Version: 0.4.0
* GitHub: NA
* Source code: https://github.com/cran/nzelect
* Date/Publication: 2017-10-02 20:35:23 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "nzelect")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘README.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        data   5.0Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6409 marked UTF-8 strings
    ```

# ofpetrial

<details>

* Version: 0.1.2
* GitHub: https://github.com/DIFM-Brain/ofpetrial
* Source code: https://github.com/cran/ofpetrial
* Date/Publication: 2024-12-11 23:00:02 UTC
* Number of recursive dependencies: 148

Run `revdepcheck::cloud_details(, "ofpetrial")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ofpetrial-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: check_ortho_with_chars
    > ### Title: Check the orthogonality with field/topographic characteristics
    > ### Aliases: check_ortho_with_chars
    > 
    > ### ** Examples
    > 
    > data(td_single_input)
    ...
     30.   └─plt$build()
     31.     └─private$addLimits(margThemed)
     32.       └─ggplot2::scale_x_continuous(limits = limits, oob = scales::squish)
     33.         └─ggplot2::continuous_scale(...)
     34.           └─ggplot2:::check_continuous_limits(limits, call = call)
     35.             └─ggplot2:::check_numeric(limits, arg = arg, call = call, allow_na = TRUE)
     36.               └─ggplot2:::check_object(x, is.numeric, what, ..., arg = arg, call = call)
     37.                 └─ggplot2:::stop_input_type(...)
     38.                   └─rlang::abort(message, ..., call = call, arg = arg)
    Execution halted
    ```

# OmicNavigator

<details>

* Version: 1.13.13
* GitHub: https://github.com/abbvie-external/OmicNavigator
* Source code: https://github.com/cran/OmicNavigator
* Date/Publication: 2023-08-25 20:40:02 UTC
* Number of recursive dependencies: 86

Run `revdepcheck::cloud_details(, "OmicNavigator")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘tinytest.R’
    Running the tests in ‘tests/tinytest.R’ failed.
    Complete output:
      > # Test files in inst/tinytest/
      > if (requireNamespace("tinytest", quietly = TRUE)) {
      +   suppressMessages(tinytest::test_package("OmicNavigator"))
      + }
      
      testAdd.R.....................    0 tests    
      testAdd.R.....................    0 tests    
    ...
      testPlot.R....................  140 tests [0;31m1 fails[0m 
      testPlot.R....................  140 tests [0;31m1 fails[0m 
      testPlot.R....................  141 tests [0;31m1 fails[0m 
      testPlot.R....................  141 tests [0;31m1 fails[0m 
      testPlot.R....................  141 tests [0;31m1 fails[0m 
      testPlot.R....................  142 tests [0;31m1 fails[0m 
      testPlot.R....................  142 tests [0;31m1 fails[0m 
      testPlot.R....................  143 tests [0;31m1 fails[0m Error in pm[[2]] : subscript out of bounds
      Calls: suppressMessages ... plotStudy -> f -> <Anonymous> -> ggplotly.ggplot -> gg2list
      Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘OmicNavigatorAPI.Rnw’ using Sweave
    OmicNavigator R package version: 1.13.13
    The app is not installed. Install it with installApp()
    Installing study "ABC" in /tmp/RtmpZZJnkc/file1cc0178f2c1
    Exporting study "ABC" as an R package
    Note: No maintainer email was specified. Using the placeholder: Unknown <unknown@unknown>
    Calculating pairwise overlaps. This may take a while...
    Exported study to /tmp/RtmpZZJnkc/ONstudyABC
    Success!
    ...
    l.14 ^^M
            
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘OmicNavigatorUsersGuide.Rnw’
    
    SUMMARY: processing the following files failed:
      ‘OmicNavigatorAPI.Rnw’ ‘OmicNavigatorUsersGuide.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# onewaytests

<details>

* Version: 3.0
* GitHub: NA
* Source code: https://github.com/cran/onewaytests
* Date/Publication: 2023-10-01 22:00:02 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::cloud_details(, "onewaytests")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘onewaytests-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gplot
    > ### Title: Box-and-Whisker, Violin Plots and Error Bars
    > ### Aliases: gplot
    > ### Keywords: functions
    > 
    > ### ** Examples
    > 
    ...
     20. └─vctrs (local) `<fn>`()
     21.   └─vctrs::vec_default_ptype2(...)
     22.     ├─base::withRestarts(...)
     23.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     24.     │   └─base (local) doWithOneRestart(return(expr), restart)
     25.     └─vctrs::stop_incompatible_type(...)
     26.       └─vctrs:::stop_incompatible(...)
     27.         └─vctrs:::stop_vctrs(...)
     28.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

# ordinalsimr

<details>

* Version: 0.2.0
* GitHub: https://github.com/NeuroShepherd/ordinalsimr
* Source code: https://github.com/cran/ordinalsimr
* Date/Publication: 2025-03-07 10:10:02 UTC
* Number of recursive dependencies: 119

Run `revdepcheck::cloud_details(, "ordinalsimr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
      > # * https://testthat.r-lib.org/articles/special-files.html
    ...
      ── Error ('test-utils.R:70:3'): test the plot_power() function ─────────────────
      Error in `expect_match(plot_obj$labels$ymin, "lower_power_bound")`: is.character(act$val) is not TRUE
      Backtrace:
          ▆
       1. └─testthat::expect_match(plot_obj$labels$ymin, "lower_power_bound") at test-utils.R:70:3
       2.   └─base::stopifnot(is.character(act$val))
      
      [ FAIL 1 | WARN 0 | SKIP 1 | PASS 69 ]
      Error: Test failures
      Execution halted
    ```

# OTclust

<details>

* Version: 1.0.6
* GitHub: NA
* Source code: https://github.com/cran/OTclust
* Date/Publication: 2023-10-06 14:40:07 UTC
* Number of recursive dependencies: 69

Run `revdepcheck::cloud_details(, "OTclust")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘OTclust-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: otplot
    > ### Title: Visualize a partition on 2 dimensional space
    > ### Aliases: otplot
    > 
    > ### ** Examples
    > 
    > data(sim1)
    ...
     20. └─vctrs (local) `<fn>`()
     21.   └─vctrs::vec_default_ptype2(...)
     22.     ├─base::withRestarts(...)
     23.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     24.     │   └─base (local) doWithOneRestart(return(expr), restart)
     25.     └─vctrs::stop_incompatible_type(...)
     26.       └─vctrs:::stop_incompatible(...)
     27.         └─vctrs:::stop_vctrs(...)
     28.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘OTclust.Rmd’ using rmarkdown
    
    Quitting from OTclust.Rmd:86-91 [unnamed-chunk-7]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'OTclust.Rmd' failed with diagnostics:
    Can't combine `..1` <double> and `..2` <character>.
    --- failed re-building ‘OTclust.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘OTclust.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# pafr

<details>

* Version: 0.0.2
* GitHub: https://github.com/dwinter/pafr
* Source code: https://github.com/cran/pafr
* Date/Publication: 2020-12-08 10:20:12 UTC
* Number of recursive dependencies: 111

Run `revdepcheck::cloud_details(, "pafr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(pafr)
      Loading required package: ggplot2
      > 
      > test_check("pafr")
      [ FAIL 6 | WARN 2 | SKIP 0 | PASS 70 ]
      
    ...
      ── Failure ('test_plot.r:11:5'): dotplot works produces a plot ─────────────────
      unname(labs["xintercept"]) not equal to "xintercept".
      target is NULL, current is character
      ── Failure ('test_plot.r:12:5'): dotplot works produces a plot ─────────────────
      unname(labs["yintercept"]) not equal to "yintercept".
      target is NULL, current is character
      
      [ FAIL 6 | WARN 2 | SKIP 0 | PASS 70 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# paletteer

<details>

* Version: 1.6.0
* GitHub: https://github.com/EmilHvitfeldt/paletteer
* Source code: https://github.com/cran/paletteer
* Date/Publication: 2024-01-21 18:00:02 UTC
* Number of recursive dependencies: 89

Run `revdepcheck::cloud_details(, "paletteer")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(paletteer)
      > 
      > test_check("paletteer")
      NULL
      NULL
      NULL
    ...
      • vdiffr_palette_check/vangogh.svg
      • vdiffr_palette_check/vapeplot.svg
      • vdiffr_palette_check/vaporwave.svg
      • vdiffr_palette_check/viridis.svg
      • vdiffr_palette_check/warhol.svg
      • vdiffr_palette_check/werpals.svg
      • vdiffr_palette_check/wesanderson.svg
      • vdiffr_palette_check/yarrr.svg
      Error: Test failures
      Execution halted
    ```

# palettes

<details>

* Version: 0.2.1
* GitHub: https://github.com/mccarthy-m-g/palettes
* Source code: https://github.com/cran/palettes
* Date/Publication: 2024-07-13 23:10:01 UTC
* Number of recursive dependencies: 111

Run `revdepcheck::cloud_details(, "palettes")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘palettes-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: scale_colour_palette_d
    > ### Title: Colour scales from colour vectors and colour palettes
    > ### Aliases: scale_colour_palette_d scale_fill_palette_d
    > ###   scale_colour_palette_c scale_fill_palette_c scale_colour_palette_b
    > ###   scale_fill_palette_b scale_color_palette_d scale_color_palette_c
    > ###   scale_color_palette_b
    > 
    ...
     23.   └─vctrs::vec_default_cast(...)
     24.     ├─base::withRestarts(...)
     25.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     26.     │   └─base (local) doWithOneRestart(return(expr), restart)
     27.     └─vctrs::stop_incompatible_cast(...)
     28.       └─vctrs::stop_incompatible_type(...)
     29.         └─vctrs:::stop_incompatible(...)
     30.           └─vctrs:::stop_vctrs(...)
     31.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘biscale.Rmd’ using rmarkdown
    ```

# PAMscapes

<details>

* Version: 0.11.3
* GitHub: NA
* Source code: https://github.com/cran/PAMscapes
* Date/Publication: 2025-04-02 20:30:05 UTC
* Number of recursive dependencies: 130

Run `revdepcheck::cloud_details(, "PAMscapes")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘PAMscapes-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: markNA
    > ### Title: Mark NA Values by Time and Frequency
    > ### Aliases: markNA
    > 
    > ### ** Examples
    > 
    > manta <- loadSoundscapeData(system.file('extdata/MANTAExampleSmall1.csv', package='PAMscapes'))
    ...
      Found 1 non-standard hybrid millidecade frequencies (HMD_24000) these will be removed. Run with "dropNonHmd=FALSE" to keep them.
    > naDf <- data.frame(start=min(manta$UTC),
    +                    end=max(manta$UTC),
    +                    freqMin=100,
    +                    freqMax=500)
    > plotHourlyLevel(manta)
    Warning in scale_x_continuous(trans = "log10", breaks = major, minor_breaks = minor,  :
      log-10 transformation introduced infinite values.
    Error: Cannot create zero-length unit vector ("unit" subsetting)
    Execution halted
    ```

# pathfindR

<details>

* Version: 2.4.2
* GitHub: https://github.com/egeulgen/pathfindR
* Source code: https://github.com/cran/pathfindR
* Date/Publication: 2025-02-17 09:30:02 UTC
* Number of recursive dependencies: 143

Run `revdepcheck::cloud_details(, "pathfindR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat-active_snw.R’
      Running ‘testthat-clustering.R’
      Running ‘testthat-comparison.R’
      Running ‘testthat-core.R’
      Running ‘testthat-data_generation.R’
      Running ‘testthat-enrichment.R’
      Running ‘testthat-scoring.R’
    Running the tests in ‘tests/testthat-scoring.R’ failed.
    Complete output:
      > library(testthat)
    ...
      ── Failure ('test-scoring.R:113:9'): `plot_scores()` -- creates term score heatmap ggplot object with correct labels ──
      g$labels$x not identical to "Sample".
      target is NULL, current is character
      ── Failure ('test-scoring.R:114:9'): `plot_scores()` -- creates term score heatmap ggplot object with correct labels ──
      g$labels$y not identical to "Term".
      target is NULL, current is character
      
      [ FAIL 8 | WARN 0 | SKIP 0 | PASS 37 ]
      Error: Test failures
      Execution halted
    ```

# pathviewr

<details>

* Version: 1.1.7
* GitHub: https://github.com/ropensci/pathviewr
* Source code: https://github.com/cran/pathviewr
* Date/Publication: 2023-03-08 08:10:05 UTC
* Number of recursive dependencies: 189

Run `revdepcheck::cloud_details(, "pathviewr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(pathviewr)
      > #library(vdiffr)
      > 
      > test_check("pathviewr")
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 286 ]
      
    ...
      ── Error ('test-plot_by_subject.R:168:3'): elev views wrangled correctly via tidyverse ──
      Error in `expect_match(elev_all_plots[[3]][[4]][["labels"]][["x"]], "position_height")`: is.character(act$val) is not TRUE
      Backtrace:
          ▆
       1. └─testthat::expect_match(...) at test-plot_by_subject.R:168:3
       2.   └─base::stopifnot(is.character(act$val))
      
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 286 ]
      Error: Test failures
      Execution halted
    ```

# pdxTrees

<details>

* Version: 0.4.0
* GitHub: https://github.com/mcconvil/pdxTrees
* Source code: https://github.com/cran/pdxTrees
* Date/Publication: 2020-08-17 14:00:02 UTC
* Number of recursive dependencies: 103

Run `revdepcheck::cloud_details(, "pdxTrees")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘pdxTrees-vignette.Rmd’ using rmarkdown
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# personalized

<details>

* Version: 0.2.7
* GitHub: https://github.com/jaredhuling/personalized
* Source code: https://github.com/cran/personalized
* Date/Publication: 2022-06-27 20:20:03 UTC
* Number of recursive dependencies: 91

Run `revdepcheck::cloud_details(, "personalized")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > Sys.setenv("R_TESTS" = "")
      > library(testthat)
      > library(personalized)
      Loading required package: glmnet
      Loading required package: Matrix
      Loaded glmnet 4.1-8
      Loading required package: mgcv
    ...
       4. └─personalized:::plot.subgroup_validated(subgrp.val, type = "stability")
       5.   ├─plotly::subplot(...)
       6.   │ └─plotly:::dots2plots(...)
       7.   ├─plotly::ggplotly(p.primary, tooltip = paste0("tooltip", 1:4))
       8.   └─plotly:::ggplotly.ggplot(...)
       9.     └─plotly::gg2list(...)
      
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 215 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) fit.subgroup.Rd:56: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) fit.subgroup.Rd:57: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) fit.subgroup.Rd:58: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) fit.subgroup.Rd:59: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) fit.subgroup.Rd:60: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) fit.subgroup.Rd:61: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) fit.subgroup.Rd:62: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) fit.subgroup.Rd:63: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) fit.subgroup.Rd:64: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) fit.subgroup.Rd:65: Lost braces in \itemize; meant \describe ?
    ...
    checkRd: (-1) fit.subgroup.Rd:179-181: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) fit.subgroup.Rd:182: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) fit.subgroup.Rd:183: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) fit.subgroup.Rd:184: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) validate.subgroup.Rd:77-81: Lost braces in \enumerate; meant \describe ?
    checkRd: (-1) validate.subgroup.Rd:82-86: Lost braces in \enumerate; meant \describe ?
    checkRd: (-1) validate.subgroup.Rd:89-92: Lost braces in \enumerate; meant \describe ?
    checkRd: (-1) weighted.ksvm.Rd:21: Lost braces; missing escapes or markup?
        21 | \item{y}{The response vector (either a character vector, factor vector, or numeric vector with values in {-1, 1})}
           |                                                                                                          ^
    ```

# PieGlyph

<details>

* Version: 1.0.0
* GitHub: https://github.com/rishvish/PieGlyph
* Source code: https://github.com/cran/PieGlyph
* Date/Publication: 2024-06-28 12:00:02 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "PieGlyph")` for more info

</details>

## Newly broken

*   checking whether package ‘PieGlyph’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/PieGlyph/new/PieGlyph.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘PieGlyph’ ...
** package ‘PieGlyph’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in get(x, envir = ns, inherits = FALSE) : 
  object 'is.waive' not found
Error: unable to load R code in package ‘PieGlyph’
Execution halted
ERROR: lazy loading failed for package ‘PieGlyph’
* removing ‘/tmp/workdir/PieGlyph/new/PieGlyph.Rcheck/PieGlyph’


```
### CRAN

```
* installing *source* package ‘PieGlyph’ ...
** package ‘PieGlyph’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (PieGlyph)


```
# plantTracker

<details>

* Version: 1.1.0
* GitHub: https://github.com/aestears/plantTracker
* Source code: https://github.com/cran/plantTracker
* Date/Publication: 2023-05-05 18:20:02 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::cloud_details(, "plantTracker")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Suggested_plantTracker_Workflow.Rmd’ using rmarkdown
    Warning in options[opts_class] <- Map(code_folding_class, options[opts_class],  :
      number of items to replace is not a multiple of replacement length
    Warning in options[opts_attr] <- Map(code_folding_attr, options[opts_attr],  :
      number of items to replace is not a multiple of replacement length
    Warning in options[opts_class] <- Map(code_folding_class, options[opts_class],  :
      number of items to replace is not a multiple of replacement length
    Warning in options[opts_attr] <- Map(code_folding_attr, options[opts_attr],  :
      number of items to replace is not a multiple of replacement length
    Warning in options[opts_class] <- Map(code_folding_class, options[opts_class],  :
      number of items to replace is not a multiple of replacement length
    Warning in options[opts_attr] <- Map(code_folding_attr, options[opts_attr],  :
      number of items to replace is not a multiple of replacement length
    ```

# Plasmidprofiler

<details>

* Version: 0.1.6
* GitHub: NA
* Source code: https://github.com/cran/Plasmidprofiler
* Date/Publication: 2017-01-06 01:10:47
* Number of recursive dependencies: 81

Run `revdepcheck::cloud_details(, "Plasmidprofiler")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Plasmidprofiler-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: main
    > ### Title: Main: Run everything
    > ### Aliases: main
    > 
    > ### ** Examples
    > 
    > main(blastdata,
    ...
    Saving 12 x 7 in image
    Warning: Vectorized input to `element_text()` is not officially supported.
    ℹ Results may be unexpected or may change in future versions of ggplot2.
    Warning in geom_tile(aes(x = Plasmid, y = Sample, label = AMR_gene, fill = Inc_group,  :
      Ignoring unknown aesthetics: label and text
    Warning: Use of `report$Sureness` is discouraged.
    ℹ Use `Sureness` instead.
    Error in pm[[2]] : subscript out of bounds
    Calls: main ... <Anonymous> -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# plotDK

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/plotDK
* Date/Publication: 2021-10-01 08:00:02 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::cloud_details(, "plotDK")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(plotDK)
      > 
      > test_check("plotDK")
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 46 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      Error in `expect_setequal(c("x", "y", "group", "subgroup", "text", "fill"), 
          names(labels))`: `object` and `expected` must both be vectors
      Backtrace:
          ▆
       1. └─testthat::expect_setequal(c("x", "y", "group", "subgroup", "text", "fill"), names(labels)) at test-plotDK.R:67:5
       2.   └─rlang::abort("`object` and `expected` must both be vectors")
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 46 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘mapproj’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 12992 marked UTF-8 strings
    ```

# plothelper

<details>

* Version: 0.1.9
* GitHub: https://github.com/githubwwwjjj/plothelper
* Source code: https://github.com/cran/plothelper
* Date/Publication: 2020-05-08 08:40:10 UTC
* Number of recursive dependencies: 43

Run `revdepcheck::cloud_details(, "plothelper")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘plothelper-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: sunshinexy
    > ### Title: Generating Lines Which Link One Points to Many
    > ### Aliases: sunshinexy
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
    + 	scale_color_continuous(low="blue", high="red")
    Error in `scale_backward_compatibility()`:
    ! Unknown scale type:
    Backtrace:
        ▆
     1. └─ggplot2::scale_color_continuous(low = "blue", high = "red")
     2.   └─ggplot2:::scale_backward_compatibility(...)
     3.     └─cli::cli_abort("Unknown scale type: {.val {scale}}")
     4.       └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# plotly

<details>

* Version: 4.10.4
* GitHub: https://github.com/plotly/plotly.R
* Source code: https://github.com/cran/plotly
* Date/Publication: 2024-01-13 22:40:02 UTC
* Number of recursive dependencies: 135

Run `revdepcheck::cloud_details(, "plotly")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘plotly-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: style
    > ### Title: Modify trace(s)
    > ### Aliases: style
    > 
    > ### ** Examples
    > 
    > ## Don't show: 
    ...
    + # this clobbers the previously supplied marker.line.color
    + style(p, marker.line = list(width = 2.5), marker.size = 10)
    + ## Don't show: 
    + }) # examplesIf
    > (p <- ggplotly(qplot(data = mtcars, wt, mpg, geom = c("point", "smooth"))))
    Warning: `qplot()` was deprecated in ggplot2 3.4.0.
    `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
    Error in pm[[2]] : subscript out of bounds
    Calls: <Anonymous> ... eval -> eval -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library("testthat")
      > library("plotly")
      Loading required package: ggplot2
      
      Attaching package: 'plotly'
      
      The following object is masked from 'package:ggplot2':
    ...
      • plotly-subplot/subplot-bump-axis-annotation.svg
      • plotly-subplot/subplot-bump-axis-image.svg
      • plotly-subplot/subplot-bump-axis-shape-shared.svg
      • plotly-subplot/subplot-bump-axis-shape.svg
      • plotly-subplot/subplot-reposition-annotation.svg
      • plotly-subplot/subplot-reposition-image.svg
      • plotly-subplot/subplot-reposition-shape-fixed.svg
      • plotly-subplot/subplot-reposition-shape.svg
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        R             1.0Mb
        htmlwidgets   4.0Mb
    ```

# plotor

<details>

* Version: 0.5.2
* GitHub: https://github.com/craig-parylo/plotor
* Source code: https://github.com/cran/plotor
* Date/Publication: 2025-02-09 11:50:02 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "plotor")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
      > # * https://testthat.r-lib.org/articles/special-files.html
    ...
      > test_check("plotor")
      [ FAIL 1 | WARN 20 | SKIP 0 | PASS 0 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-plot_or.R:3:3'): plotor output does not produce messages or warnings ──
      `{ ... }` produced warnings.
      
      [ FAIL 1 | WARN 20 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

# plotROC

<details>

* Version: 2.3.1
* GitHub: https://github.com/sachsmc/plotROC
* Source code: https://github.com/cran/plotROC
* Date/Publication: 2023-10-06 12:40:02 UTC
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "plotROC")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘plotROC-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: calc_auc
    > ### Title: Calculate the Area under the ROC curve
    > ### Aliases: calc_auc
    > 
    > ### ** Examples
    > 
    > D.ex <- rbinom(50, 1, .5)
    ...
    > ggroc <- ggplot(rocdata, aes(m = M, d = D)) + geom_roc()
    > calc_auc(ggroc)
    Error in `rlang::quo_get_expr()`:
    ! `quo` must be a quosure
    Backtrace:
        ▆
     1. ├─plotROC::calc_auc(ggroc)
     2. │ └─rlang::quo_get_expr(l2$plot$mapping[current_name][[1]])
     3. └─rlang::abort(message = message)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(plotROC)
      Loading required package: ggplot2
      > 
      > test_check("plotROC")
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 13 ]
      
    ...
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. ├─base::nrow(calc_auc(ggroc_p))
       5. ├─plotROC::calc_auc(ggroc_p)
       6. │ └─rlang::quo_get_expr(l2$plot$mapping[current_name][[1]])
       7. └─rlang::abort(message = message)
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 13 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘examples.Rmd’ using rmarkdown
    ```

# plotthis

<details>

* Version: 0.6.1
* GitHub: https://github.com/pwwang/plotthis
* Source code: https://github.com/cran/plotthis
* Date/Publication: 2025-04-07 09:00:02 UTC
* Number of recursive dependencies: 212

Run `revdepcheck::cloud_details(, "plotthis")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘plotthis-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: QQPlot
    > ### Title: QQ plot
    > ### Aliases: QQPlot
    > 
    > ### ** Examples
    > 
    > set.seed(8525)
    ...
     23. │                         └─ggplot2 (local) FUN(X[[i]], ...)
     24. │                           └─self$draw_group(group, panel_params, coord, ...)
     25. │                             └─ggplot2 (local) draw_group(...)
     26. │                               └─ggplot2 (local) draw_group(..., self = self)
     27. └─base::.handleSimpleError(...)
     28.   └─rlang (local) h(simpleError(msg, call))
     29.     └─handlers[[1L]](cnd)
     30.       └─cli::cli_abort(...)
     31.         └─rlang::abort(...)
    Execution halted
    ```

# pmartR

<details>

* Version: 2.5.0
* GitHub: https://github.com/pmartR/pmartR
* Source code: https://github.com/cran/pmartR
* Date/Publication: 2025-04-23 18:00:02 UTC
* Number of recursive dependencies: 144

Run `revdepcheck::cloud_details(, "pmartR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(pmartR)
      > 
      > test_check("pmartR")
      [ FAIL 1 | WARN 11 | SKIP 11 | PASS 2380 ]
      
      ══ Skipped tests (11) ══════════════════════════════════════════════════════════
    ...
      • plots/plot-spansres-n-biomolecule-bar.svg
      • plots/plot-spansres.svg
      • plots/plot-statres-anova-volcano.svg
      • plots/plot-statres-anova.svg
      • plots/plot-statres-combined-volcano.svg
      • plots/plot-statres-combined.svg
      • plots/plot-statres-gtest.svg
      • plots/plot-totalcountfilt.svg
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.7Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        help   1.5Mb
        libs   6.6Mb
    ```

# pmxTools

<details>

* Version: 1.3
* GitHub: https://github.com/kestrel99/pmxTools
* Source code: https://github.com/cran/pmxTools
* Date/Publication: 2023-02-21 16:00:08 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "pmxTools")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(pmxTools)
      Loading required package: patchwork
      > 
      > test_check("pmxTools")
      [ FAIL 1 | WARN 1 | SKIP 12 | PASS 110 ]
      
    ...
       41.     └─handlers[[1L]](cnd)
       42.       └─cli::cli_abort(...)
       43.         └─rlang::abort(...)
      
      [ FAIL 1 | WARN 1 | SKIP 12 | PASS 110 ]
      Deleting unused snapshots:
      • plot/conditioned-distplot.svg
      • plot/perc.svg
      Error: Test failures
      Execution halted
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) plot_scm.Rd:106: Lost braces; missing escapes or markup?
       106 | \item{lookupDF}{A data frame containing a lookup table for node labels. By default, {plot_scm} will use the PSN model names. If a lookup table containing the fields `Model` and `Alias` is provided, model names in `Model` will be replaced in the output plots by mtaching labels in `Alias`.}
           |                                                                                     ^
    checkRd: (-1) rnm.Rd:56: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) rnm.Rd:57: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) rnm.Rd:58: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) rnm.Rd:59: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) rnm.Rd:60: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) rnm.Rd:61: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) rnm.Rd:62: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) rnm.Rd:63: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) rnm.Rd:64: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) rnm.Rd:65: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) rnm.Rd:66: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) rnm.Rd:67: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) rnm.Rd:68: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) rnm.Rd:69: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) rnm.Rd:70: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) rnm.Rd:71: Lost braces in \itemize; meant \describe ?
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘DiagrammeR’
    ```

# PoweREST

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/PoweREST
* Date/Publication: 2024-09-09 09:30:02 UTC
* Number of recursive dependencies: 181

Run `revdepcheck::cloud_details(, "PoweREST")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘PoweREST-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: vis_XGBoost
    > ### Title: Visualization of the power estimations from XGBoost
    > ### Aliases: vis_XGBoost
    > 
    > ### ** Examples
    > 
    > data(power_example)
    ...
    # Good: !!myquosure * rhs
    Backtrace:
        ▆
     1. └─PoweREST::vis_XGBoost(...)
     2.   ├─base::eval(parse(text = txt3))
     3.   │ └─base::eval(parse(text = txt3))
     4.   └─rayshader::plot_gg(mtplot, width = 3.5, raytrace = FALSE, preview = TRUE)
     5.     └─rlang:::Ops.quosure(ggplotobj2$layers[[i]]$geom$default_aes$size, pointcontract)
     6.       └─rlang::abort(...)
    Execution halted
    ```

# PPQplan

<details>

* Version: 1.1.0
* GitHub: https://github.com/allenzhuaz/PPQplan
* Source code: https://github.com/cran/PPQplan
* Date/Publication: 2020-10-08 04:30:06 UTC
* Number of recursive dependencies: 116

Run `revdepcheck::cloud_details(, "PPQplan")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘PPQnote.Rmd’ using rmarkdown
    --- finished re-building ‘PPQnote.Rmd’
    
    --- re-building ‘PPQplan-vignette.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 12.1Mb
      sub-directories of 1Mb or more:
        doc  12.0Mb
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# ppseq

<details>

* Version: 0.2.5
* GitHub: https://github.com/zabore/ppseq
* Source code: https://github.com/cran/ppseq
* Date/Publication: 2024-09-04 22:20:02 UTC
* Number of recursive dependencies: 102

Run `revdepcheck::cloud_details(, "ppseq")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘one_sample_expansion.Rmd’ using rmarkdown
    
    Quitting from one_sample_expansion.Rmd:182-188 [unnamed-chunk-13]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `pm[[2]]`:
    ! subscript out of bounds
    ---
    Backtrace:
    ...
    
    Error: processing vignette 'two_sample_randomized.Rmd' failed with diagnostics:
    subscript out of bounds
    --- failed re-building ‘two_sample_randomized.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘one_sample_expansion.Rmd’ ‘two_sample_randomized.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.0Mb
      sub-directories of 1Mb or more:
        doc  10.5Mb
    ```

# precintcon

<details>

* Version: 2.3.0
* GitHub: https://github.com/lucasvenez/precintcon
* Source code: https://github.com/cran/precintcon
* Date/Publication: 2016-07-17 13:49:19
* Number of recursive dependencies: 26

Run `revdepcheck::cloud_details(, "precintcon")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘precintcon-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pplot.deciles
    > ### Title: Plot deciles
    > ### Aliases: pplot.deciles precintcon.plot.deciles
    > ### Keywords: deciles precipitation
    > 
    > ### ** Examples
    > 
    ...
     1. └─precintcon::pplot.deciles(monthly)
     2.   └─precintcon::precintcon.plot.deciles(...)
     3.     └─base::mapply(...)
     4.       └─precintcon (local) `<fn>`(...)
     5.         └─ggplot2::geom_boxplot(...)
     6.           └─ggplot2::layer(...)
     7.             └─lifecycle::deprecate_stop("2.0.0", "layer(show_guide)", "layer(show.legend)")
     8.               └─lifecycle:::deprecate_stop0(msg)
     9.                 └─rlang::cnd_signal(...)
    Execution halted
    ```

# predictMe

<details>

* Version: 0.1
* GitHub: https://github.com/mmiche/predictMe
* Source code: https://github.com/cran/predictMe
* Date/Publication: 2022-05-24 09:40:02 UTC
* Number of recursive dependencies: 54

Run `revdepcheck::cloud_details(, "predictMe")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘predictMe-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: makeTablePlot
    > ### Title: Tabularize the essential result of the predictMe package.
    > ### Aliases: makeTablePlot
    > 
    > ### ** Examples
    > 
    > # Simulate data set with continuous outcome (use all default values)
    ...
    Error in `scale_backward_compatibility()`:
    ! Unknown scale type:
    Backtrace:
        ▆
     1. └─predictMe::makeTablePlot(...)
     2.   └─ggplot2::scale_fill_continuous(high = "#132B43", low = "#56B1F7")
     3.     └─ggplot2:::scale_backward_compatibility(...)
     4.       └─cli::cli_abort("Unknown scale type: {.val {scale}}")
     5.         └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘predictMe.Rmd’ using rmarkdown
    
    Quitting from predictMe.Rmd:94-99 [unnamed-chunk-4]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'predictMe.Rmd' failed with diagnostics:
    Unknown scale type:
    --- failed re-building ‘predictMe.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘predictMe.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# prevR

<details>

* Version: 5.0.0
* GitHub: https://github.com/larmarange/prevR
* Source code: https://github.com/cran/prevR
* Date/Publication: 2023-05-15 18:50:03 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "prevR")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘intro_prevR.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.4Mb
      sub-directories of 1Mb or more:
        data   7.5Mb
    ```

# probably

<details>

* Version: 1.0.3
* GitHub: https://github.com/tidymodels/probably
* Source code: https://github.com/cran/probably
* Date/Publication: 2024-02-23 03:20:02 UTC
* Number of recursive dependencies: 130

Run `revdepcheck::cloud_details(, "probably")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/tests.html
      > # * https://testthat.r-lib.org/reference/test_package.html#special-files
    ...
      
      `actual$ymin` is absent
      `expected$ymin` is a character vector ('lower')
      
      `actual$ymax` is absent
      `expected$ymax` is a character vector ('upper')
      
      [ FAIL 2 | WARN 0 | SKIP 46 | PASS 466 ]
      Error: Test failures
      Execution halted
    ```

# processmapR

<details>

* Version: 0.5.6
* GitHub: https://github.com/bupaverse/processmapr
* Source code: https://github.com/cran/processmapR
* Date/Publication: 2024-12-03 12:50:02 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::cloud_details(, "processmapR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(processmapR)
      
      Attaching package: 'processmapR'
      
      The following object is masked from 'package:stats':
      
    ...
       10.   └─processmapR:::return_plotly(p, plotly)
       11.     ├─plotly::ggplotly(p)
       12.     └─plotly:::ggplotly.ggplot(p)
       13.       └─plotly::gg2list(...)
      ── Failure ('test_trace_explorer.R:240:3'): test trace_explorer on eventlog with param `plotly` ──
      `chart` inherits from 'gg'/'ggplot' not 'plotly'.
      
      [ FAIL 6 | WARN 0 | SKIP 11 | PASS 105 ]
      Error: Test failures
      Execution halted
    ```

# profoc

<details>

* Version: 1.3.3
* GitHub: https://github.com/BerriJ/profoc
* Source code: https://github.com/cran/profoc
* Date/Publication: 2024-09-21 22:10:02 UTC
* Number of recursive dependencies: 70

Run `revdepcheck::cloud_details(, "profoc")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘class.Rmd’ using rmarkdown
    0%   10   20   30   40   50   60   70   80   90   100%
    [----|----|----|----|----|----|----|----|----|----|
    **************************************************|
    --- finished re-building ‘class.Rmd’
    
    --- re-building ‘production.Rmd’ using rmarkdown
    0%   10   20   30   40   50   60   70   80   90   100%
    [----|----|----|----|----|----|----|----|----|----|
    **************************************************|
    --- finished re-building ‘production.Rmd’
    
    --- re-building ‘profoc.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 20.6Mb
      sub-directories of 1Mb or more:
        doc    1.4Mb
        libs  18.4Mb
    ```

# psborrow

<details>

* Version: 0.2.2
* GitHub: NA
* Source code: https://github.com/cran/psborrow
* Date/Publication: 2025-02-19 13:40:02 UTC
* Number of recursive dependencies: 104

Run `revdepcheck::cloud_details(, "psborrow")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(psborrow)
      > 
      > test_check("psborrow")
      Input all is found in pred. Any other input is disregarded.
      [ FAIL 10 | WARN 0 | SKIP 1 | PASS 144 ]
      
    ...
      `expected` is a character vector ('ref')
      ── Failure ('test-plots.R:126:5'): Ensure output is producing a ggplot2 object with appropriate parameters ──
      p1$labels$yintercept (`actual`) not equal to "ref" (`expected`).
      
      `actual` is NULL
      `expected` is a character vector ('ref')
      
      [ FAIL 10 | WARN 0 | SKIP 1 | PASS 144 ]
      Error: Test failures
      Execution halted
    ```

*   checking whether package ‘psborrow’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
    See ‘/tmp/workdir/psborrow/new/psborrow.Rcheck/00install.out’ for details.
    ```

# pscore

<details>

* Version: 0.4.0
* GitHub: https://github.com/JWiley/score-project
* Source code: https://github.com/cran/pscore
* Date/Publication: 2022-05-13 22:30:02 UTC
* Number of recursive dependencies: 157

Run `revdepcheck::cloud_details(, "pscore")` for more info

</details>

## Newly broken

*   checking whether package ‘pscore’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
    See ‘/tmp/workdir/pscore/new/pscore.Rcheck/00install.out’ for details.
    ```

# pubh

<details>

* Version: 2.0.0
* GitHub: https://github.com/josie-athens/pubh
* Source code: https://github.com/cran/pubh
* Date/Publication: 2024-10-08 05:00:03 UTC
* Number of recursive dependencies: 159

Run `revdepcheck::cloud_details(, "pubh")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘pubh-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: axis_labs
    > ### Title: Apply labels from variables to axis-labels in plots.
    > ### Aliases: axis_labs
    > 
    > ### ** Examples
    > 
    > data(kfm, package = "ISwR")
    ...
    > kfm |>
    +   gf_point(weight ~ dl.milk) |>
    +   gf_lm(col = 2, interval = "confidence", col = 2) |>
    +   axis_labs()
    Warning in rep(yes, length.out = len) :
      'x' is NULL so the result will be NULL
    Error in ans[ypos] <- rep(yes, length.out = len)[ypos] : 
      replacement has length zero
    Calls: axis_labs -> ifelse
    Execution halted
    ```

# qlifetable

<details>

* Version: 0.0.2-6
* GitHub: NA
* Source code: https://github.com/cran/qlifetable
* Date/Publication: 2025-04-11 22:30:02 UTC
* Number of recursive dependencies: 27

Run `revdepcheck::cloud_details(, "qlifetable")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘qlifetable-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.qlifetable
    > ### Title: Graphical representation in a 4x4 raster of a qlifetable data
    > ###   frame.
    > ### Aliases: plot.qlifetable
    > 
    > ### ** Examples
    > 
    ...
    ! Unknown scale type:
    Backtrace:
        ▆
     1. ├─base::plot(out, show.plot = FALSE)
     2. └─qlifetable:::plot.qlifetable(out, show.plot = FALSE)
     3.   └─ggplot2::scale_fill_continuous(...)
     4.     └─ggplot2:::scale_backward_compatibility(...)
     5.       └─cli::cli_abort("Unknown scale type: {.val {scale}}")
     6.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        data   5.0Mb
    ```

# quantities

<details>

* Version: 0.2.3
* GitHub: https://github.com/r-quantities/quantities
* Source code: https://github.com/cran/quantities
* Date/Publication: 2025-01-18 21:20:02 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::cloud_details(, "quantities")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘introduction.Rmd’ using rmarkdown
    ```

# QurvE

<details>

* Version: 1.1.1
* GitHub: https://github.com/NicWir/QurvE
* Source code: https://github.com/cran/QurvE
* Date/Publication: 2024-01-26 12:40:14 UTC
* Number of recursive dependencies: 151

Run `revdepcheck::cloud_details(, "QurvE")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘QurvE-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.grid
    > ### Title: Plot a matrix of growth curve panels
    > ### Aliases: plot.grid
    > 
    > ### ** Examples
    > 
    > # Create random growth data set
    ...
    Error in `scale_backward_compatibility()`:
    ! Unknown scale type:
    Backtrace:
        ▆
     1. └─QurvE::plot.grid(res, param = "mu.spline")
     2.   └─ggplot2::scale_fill_continuous(...)
     3.     └─ggplot2:::scale_backward_compatibility(...)
     4.       └─cli::cli_abort("Unknown scale type: {.val {scale}}")
     5.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        R           1.5Mb
        doc         2.1Mb
        shiny_app   1.2Mb
    ```

# R2sample

<details>

* Version: 4.0.1
* GitHub: NA
* Source code: https://github.com/cran/R2sample
* Date/Publication: 2025-04-30 15:50:02 UTC
* Number of recursive dependencies: 69

Run `revdepcheck::cloud_details(, "R2sample")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘R2sample.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.8Mb
      sub-directories of 1Mb or more:
        libs   7.4Mb
    ```

# r6qualitytools

<details>

* Version: 1.0.1
* GitHub: https://github.com/Fabianenc/r6qualitytools
* Source code: https://github.com/cran/r6qualitytools
* Date/Publication: 2024-10-03 19:30:02 UTC
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "r6qualitytools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘r6qualitytools-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: dotPlot
    > ### Title: dotPlot: Function to create a dot plot
    > ### Aliases: dotPlot
    > 
    > ### ** Examples
    > 
    > # Create some data and grouping
    ...
     30.   └─vctrs::vec_default_cast(...)
     31.     ├─base::withRestarts(...)
     32.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     33.     │   └─base (local) doWithOneRestart(return(expr), restart)
     34.     └─vctrs::stop_incompatible_cast(...)
     35.       └─vctrs::stop_incompatible_type(...)
     36.         └─vctrs:::stop_incompatible(...)
     37.           └─vctrs:::stop_vctrs(...)
     38.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

# Radviz

<details>

* Version: 0.9.4
* GitHub: https://github.com/yannabraham/Radviz
* Source code: https://github.com/cran/Radviz
* Date/Publication: 2025-05-09 06:30:02 UTC
* Number of recursive dependencies: 62

Run `revdepcheck::cloud_details(, "Radviz")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Radviz-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Radviz-package
    > ### Title: Radviz: Project Multidimensional Data in 2D Space
    > ### Aliases: Radviz Radviz-package
    > ### Keywords: internal
    > 
    > ### ** Examples
    > 
    ...
    Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
    ℹ Please use tidy evaluation idioms with `aes()`.
    ℹ See also `vignette("ggplot2-in-packages")` for more information.
    ℹ The deprecated feature was likely used in the Radviz package.
      Please report the issue at <https://github.com/yannabraham/Radviz/issues>.
    > plot(rv,anchors.only=FALSE)
    Error in plot.radviz(rv, anchors.only = FALSE) : 
      'language' object cannot be coerced to type 'double'
    Calls: plot -> plot.radviz
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘multivariate_analysis.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.6Mb
      sub-directories of 1Mb or more:
        doc    3.9Mb
        libs   2.5Mb
    ```

# rassta

<details>

* Version: 1.0.6
* GitHub: https://github.com/bafuentes/rassta
* Source code: https://github.com/cran/rassta
* Date/Publication: 2024-08-19 06:20:13 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "rassta")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rassta-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: select_functions
    > ### Title: Select Constrained Univariate Distribution Functions
    > ### Aliases: select_functions
    > 
    > ### ** Examples
    > 
    > require(terra)
    ...
    > tvars <- terra::rast(tf)
    > # Single-layer SpatRaster of topographic classification units
    > ## 5 classification units
    > tcf <- list.files(path = p, pattern = "topography.tif", full.names = TRUE)
    > tcu <- terra::rast(tcf)
    > # Automatic selection of distribution functions
    > tdif <- select_functions(cu.rast = tcu, var.rast = tvars, fun = mean)
    Error in pm[[2]] : subscript out of bounds
    Calls: select_functions -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘tinytest.R’
    Running the tests in ‘tests/tinytest.R’ failed.
    Complete output:
      > 
      > if ( requireNamespace("tinytest", quietly=TRUE) ){
      +   tinytest::test_package("rassta")
      + }
      
      Attaching package: 'rassta'
      
    ...
      test_select_functions.R.......    0 tests    
      test_select_functions.R.......    0 tests    
      test_select_functions.R.......    0 tests    
      test_select_functions.R.......    0 tests    
      test_select_functions.R.......    0 tests    
      test_select_functions.R.......    0 tests    
      test_select_functions.R.......    0 tests    
      test_select_functions.R.......    0 tests    Error in pm[[2]] : subscript out of bounds
      Calls: <Anonymous> ... select_functions -> <Anonymous> -> ggplotly.ggplot -> gg2list
      Execution halted
    ```

# rasterdiv

<details>

* Version: 0.3.6
* GitHub: https://github.com/mattmar/rasterdiv
* Source code: https://github.com/cran/rasterdiv
* Date/Publication: 2024-11-06 11:20:03 UTC
* Number of recursive dependencies: 100

Run `revdepcheck::cloud_details(, "rasterdiv")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘rasterdiv_01_Basics.Rmd’ using rmarkdown
    ```

# rater

<details>

* Version: 1.3.1
* GitHub: https://github.com/jeffreypullin/rater
* Source code: https://github.com/cran/rater
* Date/Publication: 2023-09-11 17:40:02 UTC
* Number of recursive dependencies: 89

Run `revdepcheck::cloud_details(, "rater")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(rater)
      * The rater package uses `Stan` to fit bayesian models.
      * If you are working on a local, multicore CPU with excess RAM please call:
      * options(mc.cores = parallel::detectCores())
      * This will allow Stan to run inference on multiple cores in parallel.
      > 
    ...
      ── Failure ('test_plotting.R:65:3'): plot_theta_points output has correct type ──
      get_geoms(ds_plot) (`actual`) not equal to c("GeomPoint", "GeomErrorbar") (`expected`).
      
      `names(actual)` is a character vector ('geom_point', 'geom_errorbar')
      `names(expected)` is absent
      
      [ FAIL 4 | WARN 0 | SKIP 6 | PASS 375 ]
      Error: Test failures
      In addition: There were 12 warnings (use warnings() to see them)
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 124.5Mb
      sub-directories of 1Mb or more:
        libs  123.7Mb
    ```

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) as_mcmc.list.Rd:5: Lost braces; missing escapes or markup?
         5 | \title{Convert a rater_fit object to a {coda} \code{mcmc.list} object.}
           |                                        ^
    checkRd: (-1) as_mcmc.list.Rd:16: Lost braces; missing escapes or markup?
        16 | Convert a rater_fit object to a {coda} \code{mcmc.list} object.
           |                                 ^
    checkRd: (-1) as_mcmc.list.Rd:13: Lost braces; missing escapes or markup?
        13 | A {coda} mcmc.list object.
           |   ^
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# ratlas

<details>

* Version: 0.1.0
* GitHub: https://github.com/atlas-aai/ratlas
* Source code: https://github.com/cran/ratlas
* Date/Publication: 2024-11-06 16:20:02 UTC
* Number of recursive dependencies: 136

Run `revdepcheck::cloud_details(, "ratlas")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ratlas)
      NOTE: Either Arial Narrow or Montserrat fonts are required to use these themes.
            Please use ratlas::import_montserrat() to install Montserrat and
            if Arial Narrow is not on your system, please see https://bit.ly/arialnarrow
      > 
    ...
      • okabeito-color-scales/okabeito-fill.svg
      • okabeito-color-scales/okabeito-lighten.svg
      • stat-bin/bins30-bound0.svg
      • stat-bin/no-color-hist.svg
      • stat-bin/one-bin.svg
      • stat-bin/trimmed-hist-bound0.svg
      • stat-bin/trimmed-hist-center0.svg
      • themes/theme-atlas.svg
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘plotting.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        doc       2.6Mb
        rstudio   1.6Mb
    ```

# RavenR

<details>

* Version: 2.2.2
* GitHub: https://github.com/rchlumsk/RavenR
* Source code: https://github.com/cran/RavenR
* Date/Publication: 2024-05-07 03:30:02 UTC
* Number of recursive dependencies: 132

Run `revdepcheck::cloud_details(, "RavenR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘RavenR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: rvn_met_recordplot
    > ### Title: EC Climate Gauge Record Overlap Visualization
    > ### Aliases: rvn_met_recordplot
    > 
    > ### ** Examples
    > 
    > # load metadata from RavenR sample data
    ...
    Error in `scale_backward_compatibility()`:
    ! Unknown scale type:
    Backtrace:
        ▆
     1. └─RavenR::rvn_met_recordplot(...)
     2.   └─ggplot2::scale_color_continuous(...)
     3.     └─ggplot2:::scale_backward_compatibility(...)
     4.       └─cli::cli_abort("Unknown scale type: {.val {scale}}")
     5.         └─rlang::abort(...)
    Execution halted
    ```

# RBesT

<details>

* Version: 1.8-2
* GitHub: https://github.com/Novartis/RBesT
* Source code: https://github.com/cran/RBesT
* Date/Publication: 2025-04-25 18:30:02 UTC
* Number of recursive dependencies: 144

Run `revdepcheck::cloud_details(, "RBesT")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘RBesT-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: mix
    > ### Title: Mixture Distributions
    > ### Aliases: mix dmix pmix qmix rmix [[.mix
    > 
    > ### ** Examples
    > 
    > ## a beta mixture
    ...
     20. └─vctrs (local) `<fn>`()
     21.   └─vctrs::vec_default_ptype2(...)
     22.     ├─base::withRestarts(...)
     23.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     24.     │   └─base (local) doWithOneRestart(return(expr), restart)
     25.     └─vctrs::stop_incompatible_type(...)
     26.       └─vctrs:::stop_incompatible(...)
     27.         └─vctrs:::stop_vctrs(...)
     28.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 94.6Mb
      sub-directories of 1Mb or more:
        libs  93.2Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# RcmdrPlugin.RiskDemo

<details>

* Version: 3.2
* GitHub: NA
* Source code: https://github.com/cran/RcmdrPlugin.RiskDemo
* Date/Publication: 2024-02-06 09:20:02 UTC
* Number of recursive dependencies: 200

Run `revdepcheck::cloud_details(, "RcmdrPlugin.RiskDemo")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘RcmdrPlugin.RiskDemo-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: drawIncidenceFin
    > ### Title: Plotting incidence curves of an epidemic with Finnish data
    > ### Aliases: drawIncidenceFin
    > 
    > ### ** Examples
    > 
    > data(dataCovidFin)
    ...
     20. └─vctrs (local) `<fn>`()
     21.   └─vctrs::vec_default_ptype2(...)
     22.     ├─base::withRestarts(...)
     23.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     24.     │   └─base (local) doWithOneRestart(return(expr), restart)
     25.     └─vctrs::stop_incompatible_type(...)
     26.       └─vctrs:::stop_incompatible(...)
     27.         └─vctrs:::stop_vctrs(...)
     28.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

# rddensity

<details>

* Version: 2.6
* GitHub: NA
* Source code: https://github.com/cran/rddensity
* Date/Publication: 2024-10-06 08:30:02 UTC
* Number of recursive dependencies: 27

Run `revdepcheck::cloud_details(, "rddensity")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rddensity-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: rddensity
    > ### Title: Manipulation Testing Using Local Polynomial Density Estimation
    > ### Aliases: rddensity
    > 
    > ### ** Examples
    > 
    > ### Continuous Density
    ...
     21. └─vctrs (local) `<fn>`()
     22.   └─vctrs::vec_default_ptype2(...)
     23.     ├─base::withRestarts(...)
     24.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     25.     │   └─base (local) doWithOneRestart(return(expr), restart)
     26.     └─vctrs::stop_incompatible_type(...)
     27.       └─vctrs:::stop_incompatible(...)
     28.         └─vctrs:::stop_vctrs(...)
     29.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

# registr

<details>

* Version: 2.1.0
* GitHub: NA
* Source code: https://github.com/cran/registr
* Date/Publication: 2022-10-02 21:40:02 UTC
* Number of recursive dependencies: 87

Run `revdepcheck::cloud_details(, "registr")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘incomplete_curves.Rmd’ using rmarkdown
    ```

## In both

*   checking C++ specification ... NOTE
    ```
      Specified C++11: please drop specification unless essential
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        doc    1.8Mb
        libs   3.1Mb
    ```

# regtomean

<details>

* Version: 1.2
* GitHub: NA
* Source code: https://github.com/cran/regtomean
* Date/Publication: 2024-12-17 15:00:02 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::cloud_details(, "regtomean")` for more info

</details>

## Newly broken

*   checking whether package ‘regtomean’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::set_theme’ by ‘sjPlot::set_theme’ when loading ‘regtomean’
    See ‘/tmp/workdir/regtomean/new/regtomean.Rcheck/00install.out’ for details.
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘plyr’
    ```

# rempsyc

<details>

* Version: 0.1.9
* GitHub: https://github.com/rempsyc/rempsyc
* Source code: https://github.com/cran/rempsyc
* Date/Publication: 2025-02-01 23:40:05 UTC
* Number of recursive dependencies: 176

Run `revdepcheck::cloud_details(, "rempsyc")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rempsyc-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: nice_normality
    > ### Title: Easy normality check per group
    > ### Aliases: nice_normality
    > ### Keywords: QQ density distribution normality plots
    > 
    > ### ** Examples
    > 
    ...
     30. │                                     └─ggplot2 (local) FUN(X[[i]], ...)
     31. │                                       └─self$draw_group(group, panel_params, coord, ...)
     32. │                                         └─ggplot2 (local) draw_group(...)
     33. │                                           └─ggplot2 (local) draw_group(..., self = self)
     34. └─base::.handleSimpleError(...)
     35.   └─rlang (local) h(simpleError(msg, call))
     36.     └─handlers[[1L]](cnd)
     37.       └─cli::cli_abort(...)
     38.         └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(rempsyc)
      Suggested APA citation: Thériault, R. (2023). rempsyc: Convenience functions for psychology. 
      Journal of Open Source Software, 8(87), 5466. https://doi.org/10.21105/joss.05466
      > 
      > test_check("rempsyc")
      [ FAIL 2 | WARN 2 | SKIP 20 | PASS 44 ]
    ...
       30. │                                     └─ggplot2 (local) draw_group(..., self = self)
       31. └─base::.handleSimpleError(...)
       32.   └─rlang (local) h(simpleError(msg, call))
       33.     └─handlers[[1L]](cnd)
       34.       └─cli::cli_abort(...)
       35.         └─rlang::abort(...)
      
      [ FAIL 2 | WARN 2 | SKIP 20 | PASS 44 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘assumptions.Rmd’ using rmarkdown
    
    Quitting from assumptions.Rmd:134-140 [unnamed-chunk-11]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'assumptions.Rmd' failed with diagnostics:
    Problem while converting geom to grob.
    ℹ Error occurred in the 1st layer.
    Caused by error in `check_linewidth()`:
    ! could not find function "check_linewidth"
    --- failed re-building ‘assumptions.Rmd’
    
    --- re-building ‘contrasts.Rmd’ using rmarkdown
    ```

# ReturnCurves

<details>

* Version: 1.0.1
* GitHub: https://github.com/lidiamandre/ReturnCurves
* Source code: https://github.com/cran/ReturnCurves
* Date/Publication: 2025-02-05 17:40:02 UTC
* Number of recursive dependencies: 97

Run `revdepcheck::cloud_details(, "ReturnCurves")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ReturnCurves-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: adf_est
    > ### Title: Estimation of the Angular Dependence Function (ADF)
    > ### Aliases: adf_est
    > 
    > ### ** Examples
    > 
    > library(ReturnCurves)
    ...
     21.   └─vctrs::vec_default_cast(...)
     22.     ├─base::withRestarts(...)
     23.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     24.     │   └─base (local) doWithOneRestart(return(expr), restart)
     25.     └─vctrs::stop_incompatible_cast(...)
     26.       └─vctrs::stop_incompatible_type(...)
     27.         └─vctrs:::stop_incompatible(...)
     28.           └─vctrs:::stop_vctrs(...)
     29.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ReturnCurves.Rmd’ using rmarkdown
    
    Quitting from ReturnCurves.Rmd:309-312 [plotsadfest]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'ReturnCurves.Rmd' failed with diagnostics:
    Can't convert `na_value` <character> to <double>.
    --- failed re-building ‘ReturnCurves.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ReturnCurves.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# rLakeHabitat

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/rLakeHabitat
* Date/Publication: 2025-04-15 19:30:02 UTC
* Number of recursive dependencies: 101

Run `revdepcheck::cloud_details(, "rLakeHabitat")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rLakeHabitat-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: bathyMap
    > ### Title: Plot Bathymetry Map
    > ### Aliases: bathyMap
    > 
    > ### ** Examples
    > 
    > #load raster
    ...
    Error in `scale_backward_compatibility()`:
    ! Unknown scale type:
    Backtrace:
        ▆
     1. └─rLakeHabitat::bathyMap(DEM, contours = TRUE, units = "m", labels = TRUE)
     2.   └─ggplot2::scale_fill_continuous(...)
     3.     └─ggplot2:::scale_backward_compatibility(...)
     4.       └─cli::cli_abort("Unknown scale type: {.val {scale}}")
     5.         └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
      > # * https://testthat.r-lib.org/articles/special-files.html
    ...
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. └─rLakeHabitat::bathyMap(dat, units = "m", plotTitle = "Lake Monona")
       5.   └─ggplot2::scale_fill_continuous(...)
       6.     └─ggplot2:::scale_backward_compatibility(...)
       7.       └─cli::cli_abort("Unknown scale type: {.val {scale}}")
       8.         └─rlang::abort(...)
      
      [ FAIL 2 | WARN 3 | SKIP 0 | PASS 211 ]
      Error: Test failures
      Execution halted
    ```

# RNAseqQC

<details>

* Version: 0.2.1
* GitHub: https://github.com/frederikziebell/RNAseqQC
* Source code: https://github.com/cran/RNAseqQC
* Date/Publication: 2024-07-15 14:40:02 UTC
* Number of recursive dependencies: 160

Run `revdepcheck::cloud_details(, "RNAseqQC")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘data.Rmd’ using rmarkdown
    --- finished re-building ‘data.Rmd’
    
    --- re-building ‘introduction.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.0Mb
      sub-directories of 1Mb or more:
        data   4.5Mb
        doc    2.3Mb
    ```

# roahd

<details>

* Version: 1.4.3
* GitHub: https://github.com/astamm/roahd
* Source code: https://github.com/cran/roahd
* Date/Publication: 2021-11-04 00:10:02 UTC
* Number of recursive dependencies: 85

Run `revdepcheck::cloud_details(, "roahd")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘roahd-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.depthgram
    > ### Title: Specialized method to plot 'depthgram' objects
    > ### Aliases: plot.depthgram
    > 
    > ### ** Examples
    > 
    > N <- 50
    ...
    +   N,
    +   centerline = sin(2 * pi * grid),
    +   Cov = Cov
    + )
    > names <- paste0("id_", 1:nrow(Data[[1]]))
    > DG <- depthgram(Data, marginal_outliers = TRUE, ids = names)
    > plot(DG)
    Error in pm[[2]] : subscript out of bounds
    Calls: plot ... plotly_build -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.4Mb
      sub-directories of 1Mb or more:
        data   5.0Mb
        doc    1.7Mb
    ```

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) fData.Rd:22: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) fData.Rd:23-24: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) fData.Rd:25: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) fData.Rd:26: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) fData.Rd:27-28: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) mfData.Rd:22: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) mfData.Rd:23: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) mfData.Rd:24-25: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) mfData.Rd:26: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) mfData.Rd:27: Lost braces in \itemize; \value handles \item{}{} directly
    ...
    checkRd: (-1) outliergram.Rd:49-51: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) outliergram.Rd:52-54: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) outliergram.Rd:55-58: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) outliergram.Rd:59-62: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) outliergram.Rd:63-65: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) outliergram.Rd:66-69: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) outliergram.Rd:70-71: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) outliergram.Rd:94: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) outliergram.Rd:95-96: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) outliergram.Rd:97: Lost braces in \itemize; \value handles \item{}{} directly
    ```

# romic

<details>

* Version: 1.1.3
* GitHub: NA
* Source code: https://github.com/cran/romic
* Date/Publication: 2023-09-21 05:40:02 UTC
* Number of recursive dependencies: 110

Run `revdepcheck::cloud_details(, "romic")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
      > # * https://testthat.r-lib.org/articles/special-files.html
    ...
       3. │ │ └─base::withCallingHandlers(...)
       4. │ ├─plotly::ggplotly(heatmap_plot) %>% plotly::layout(margin = 0)
       5. │ ├─plotly::ggplotly(heatmap_plot)
       6. │ └─plotly:::ggplotly.ggplot(heatmap_plot)
       7. │   └─plotly::gg2list(...)
       8. └─plotly::layout(., margin = 0)
      
      [ FAIL 1 | WARN 0 | SKIP 7 | PASS 66 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) update_tidy_omic.Rd:16-17: Lost braces
        16 | in \code{updated_tidy_data} (names) and the table {features, samples,
           |                                                   ^
    ```

# roptions

<details>

* Version: 1.0.3
* GitHub: NA
* Source code: https://github.com/cran/roptions
* Date/Publication: 2020-05-11 11:10:06 UTC
* Number of recursive dependencies: 68

Run `revdepcheck::cloud_details(, "roptions")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘roptions-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: box.spread
    > ### Title: Box Spread Strategy Function
    > ### Aliases: box.spread
    > 
    > ### ** Examples
    > 
    > box.spread(100, 105, 95, 110, 3.2, 2.6, 1.1, 2.4)
    ...
    35         5.7
    36         5.7
    37         5.7
    38         5.7
    39         5.7
    40         5.7
    41         5.7
    Error in pm[[2]] : subscript out of bounds
    Calls: box.spread -> print -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# SAMprior

<details>

* Version: 2.0.0
* GitHub: NA
* Source code: https://github.com/cran/SAMprior
* Date/Publication: 2025-01-17 14:50:06 UTC
* Number of recursive dependencies: 136

Run `revdepcheck::cloud_details(, "SAMprior")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘SAMprior-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: SAM_prior
    > ### Title: Calculating SAM priors
    > ### Aliases: SAM_prior SAM_prior.betaMix SAM_prior.gammaMix
    > ###   SAM_prior.normMix
    > 
    > ### ** Examples
    > 
    ...
     20. └─vctrs (local) `<fn>`()
     21.   └─vctrs::vec_default_ptype2(...)
     22.     ├─base::withRestarts(...)
     23.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     24.     │   └─base (local) doWithOneRestart(return(expr), restart)
     25.     └─vctrs::stop_incompatible_type(...)
     26.       └─vctrs:::stop_incompatible(...)
     27.         └─vctrs:::stop_vctrs(...)
     28.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

# saros

<details>

* Version: 1.5.1
* GitHub: https://github.com/NIFU-NO/saros
* Source code: https://github.com/cran/saros
* Date/Publication: 2025-02-12 22:50:01 UTC
* Number of recursive dependencies: 118

Run `revdepcheck::cloud_details(, "saros")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘saros-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: girafe
    > ### Title: Pull global plotting settings before displaying plot
    > ### Aliases: girafe
    > 
    > ### ** Examples
    > 
    > plot <- makeme(data = ex_survey, dep = b_1)
    ...
     21. │                   └─base::lapply(...)
     22. │                     └─ggplot2 (local) FUN(X[[i]], ...)
     23. │                       ├─rlang::inject(self$draw_panel(data, panel_params, coord, !!!params))
     24. │                       └─self$draw_panel(...)
     25. └─base::.handleSimpleError(...)
     26.   └─rlang (local) h(simpleError(msg, call))
     27.     └─handlers[[1L]](cnd)
     28.       └─cli::cli_abort(...)
     29.         └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(saros)
      > 
      > testthat::test_check("saros")
      Starting 2 test processes
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 211 ]
    ...
       24. │                         └─self$draw_panel(...)
       25. └─base::.handleSimpleError(...)
       26.   └─rlang (local) h(simpleError(msg, call))
       27.     └─handlers[[1L]](cnd)
       28.       └─cli::cli_abort(...)
       29.         └─rlang::abort(...)
      
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 211 ]
      Error: Test failures
      Execution halted
    ```

# scoringutils

<details>

* Version: 2.1.0
* GitHub: https://github.com/epiforecasts/scoringutils
* Source code: https://github.com/cran/scoringutils
* Date/Publication: 2025-03-03 18:10:02 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::cloud_details(, "scoringutils")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Deprecated-functions.Rmd’ using rmarkdown
    --- finished re-building ‘Deprecated-functions.Rmd’
    
    --- re-building ‘Deprecated-visualisations.Rmd’ using rmarkdown
    ```

# scplot

<details>

* Version: 0.5.1
* GitHub: NA
* Source code: https://github.com/cran/scplot
* Date/Publication: 2025-03-01 13:10:03 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::cloud_details(, "scplot")` for more info

</details>

## Newly broken

*   checking whether package ‘scplot’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: The `size` argument of `element_rect()` is deprecated as of ggplot2 3.4.0.
    See ‘/tmp/workdir/scplot/new/scplot.Rcheck/00install.out’ for details.
    ```

# scRNAstat

<details>

* Version: 0.1.1.1
* GitHub: NA
* Source code: https://github.com/cran/scRNAstat
* Date/Publication: 2025-03-08 08:58:55 UTC
* Number of recursive dependencies: 155

Run `revdepcheck::cloud_details(, "scRNAstat")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  8.1Mb
      sub-directories of 1Mb or more:
        data   8.0Mb
    ```

## In both

*   checking whether package ‘scRNAstat’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: namespace ‘colorspace’ is not available and has been replaced
    See ‘/tmp/workdir/scRNAstat/new/scRNAstat.Rcheck/00install.out’ for details.
    ```

# scUtils

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/scUtils
* Date/Publication: 2020-06-25 16:20:02 UTC
* Number of recursive dependencies: 49

Run `revdepcheck::cloud_details(, "scUtils")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(scUtils)
      > 
      > test_check("scUtils")
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 32 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-plots.R:59:3'): all kinds of colnames are allowed ────────────
      p$labels not equal to list(y = "Dim2", x = "Dim1", colour = "expression").
      Length mismatch: comparison on first 2 components
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 32 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# SCVA

<details>

* Version: 1.3.1
* GitHub: NA
* Source code: https://github.com/cran/SCVA
* Date/Publication: 2020-01-09 22:50:10 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::cloud_details(, "SCVA")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘SCVA-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: graphly
    > ### Title: Interactive plot of single-case data
    > ### Aliases: graphly
    > ### Keywords: Single-case design Graph
    > 
    > ### ** Examples
    > 
    > data(AB)
    > graphly(design = "AB", data = AB)
    Warning: `qplot()` was deprecated in ggplot2 3.4.0.
    ℹ The deprecated feature was likely used in the SCVA package.
      Please report the issue to the authors.
    Error in pm[[2]] : subscript out of bounds
    Calls: graphly -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# SDMtune

<details>

* Version: 1.3.2
* GitHub: https://github.com/ConsBiol-unibern/SDMtune
* Source code: https://github.com/cran/SDMtune
* Date/Publication: 2024-12-16 16:50:06 UTC
* Number of recursive dependencies: 122

Run `revdepcheck::cloud_details(, "SDMtune")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(SDMtune)
      
         _____  ____   __  ___ __
        / ___/ / __ \ /  |/  // /_ __  __ ____   ___
        \__ \ / / / // /|_/ // __// / / // __ \ / _ \
       ___/ // /_/ // /  / // /_ / /_/ // / / //  __/
    ...
      `expected` is a character vector ('Var2')
      ── Failure ('test-plotCor.R:6:3'): The plot has the correct labels and text size ──
      p$labels$y (`actual`) not equal to "Var1" (`expected`).
      
      `actual` is NULL
      `expected` is a character vector ('Var1')
      
      [ FAIL 2 | WARN 0 | SKIP 55 | PASS 315 ]
      Error: Test failures
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        R      3.0Mb
        libs   1.0Mb
    ```

# seAMLess

<details>

* Version: 0.1.1
* GitHub: https://github.com/eonurk/seAMLess
* Source code: https://github.com/cran/seAMLess
* Date/Publication: 2024-11-11 12:50:02 UTC
* Number of recursive dependencies: 50

Run `revdepcheck::cloud_details(, "seAMLess")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘seAMLess-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ternaryPlot
    > ### Title: Given the immune compositions (ICs) of bulk-RNA samples, this
    > ###   function creates a ternary plot similar to ALOT tube from EuroFlow
    > ###   analysis and Figure 1E of our paper.
    > ### Aliases: ternaryPlot
    > 
    > ### ** Examples
    ...
    > 
    > data(minRes)
    > ternaryPlot(minRes)
    Warning: The `size` argument of `element_rect()` is deprecated as of ggplot2 3.4.0.
    ℹ Please use the `linewidth` argument instead.
    ℹ The deprecated feature was likely used in the ggtern package.
      Please report the issue to the authors.
    Error in validate_theme(theme) : could not find function "validate_theme"
    Calls: <Anonymous> ... ggplot_build.ggplot -> layers_add_or_remove_mask -> <Anonymous>
    Execution halted
    ```

# see

<details>

* Version: 0.11.0
* GitHub: https://github.com/easystats/see
* Source code: https://github.com/cran/see
* Date/Publication: 2025-03-11 16:20:02 UTC
* Number of recursive dependencies: 246

Run `revdepcheck::cloud_details(, "see")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘see-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_from_list
    > ### Title: Create ggplot2 geom(s) from a list
    > ### Aliases: geom_from_list geoms_from_list
    > 
    > ### ** Examples
    > 
    > ## Don't show: 
    ...
     11.           │ │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
     12.           │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
     13.           │ └─base::withCallingHandlers(...)
     14.           └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     15.             └─l$map_statistic(d, plot)
     16.               └─ggplot2 (local) map_statistic(..., self = self)
     17.                 └─ggplot2:::check_nondata_cols(...)
     18.                   └─cli::cli_abort(c(problem, issues, i = hint), call = NULL)
     19.                     └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # graphics engine changed in R 4.4, and so snapshots generated on
      > # previous R version won't work
      > if (getRversion() >= "4.4.0") {
      +   library(testthat)
      +   library(see)
      + 
      +   test_check("see")
    ...
       30. │                                     └─ggplot2 (local) draw_group(..., self = self)
       31. └─base::.handleSimpleError(...)
       32.   └─rlang (local) h(simpleError(msg, call))
       33.     └─handlers[[1L]](cnd)
       34.       └─cli::cli_abort(...)
       35.         └─rlang::abort(...)
      
      [ FAIL 1 | WARN 8 | SKIP 23 | PASS 34 ]
      Error: Test failures
      Execution halted
    ```

# seedreg

<details>

* Version: 1.0.3
* GitHub: NA
* Source code: https://github.com/cran/seedreg
* Date/Publication: 2022-07-07 21:20:02 UTC
* Number of recursive dependencies: 125

Run `revdepcheck::cloud_details(, "seedreg")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘seedreg-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: BC_model
    > ### Title: Analysis: Logistic regression Brain-Cousens hormesis models
    > ### Aliases: BC_model
    > 
    > ### ** Examples
    > 
    > library(seedreg)
    ...
     28.     └─vctrs::stop_incompatible_type(...)
     29.       └─vctrs:::stop_incompatible(...)
     30.         └─vctrs:::stop_vctrs(...)
     31.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Warning message:
    Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ℹ Please use `linewidth` instead.
    ℹ The deprecated feature was likely used in the seedreg package.
      Please report the issue to the authors. 
    Execution halted
    ```

# sglg

<details>

* Version: 0.2.2
* GitHub: NA
* Source code: https://github.com/cran/sglg
* Date/Publication: 2022-09-04 03:50:01 UTC
* Number of recursive dependencies: 93

Run `revdepcheck::cloud_details(, "sglg")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘sglg-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: deviance_residuals
    > ### Title: Deviance Residuals for a Generalized Log-gamma Regression Model
    > ### Aliases: deviance_residuals
    > 
    > ### ** Examples
    > 
    > # Example 1
    > n <- 300
    > error <- rglg(n,0,1,1)
    > y <- 0.5 + error
    > fit <- glg(y~1,data=as.data.frame(y))
    > deviance_residuals(fit)
    Error in pm[[2]] : subscript out of bounds
    Calls: deviance_residuals ... dots2plots -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# sgsR

<details>

* Version: 1.4.5
* GitHub: https://github.com/tgoodbody/sgsR
* Source code: https://github.com/cran/sgsR
* Date/Publication: 2024-03-03 15:10:02 UTC
* Number of recursive dependencies: 120

Run `revdepcheck::cloud_details(, "sgsR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/tests.html
    ...
      `expected` is a character vector ('zq90')
      ── Failure ('test-utils-plot.R:19:3'): scatter messages ────────────────────────
      o1$labels$x (`actual`) not equal to "pzabove2" (`expected`).
      
      `actual` is NULL
      `expected` is a character vector ('pzabove2')
      
      [ FAIL 2 | WARN 112 | SKIP 19 | PASS 508 ]
      Error: Test failures
      Execution halted
    ```

# SHAPforxgboost

<details>

* Version: 0.1.3
* GitHub: https://github.com/liuyanguu/SHAPforxgboost
* Source code: https://github.com/cran/SHAPforxgboost
* Date/Publication: 2023-05-29 17:20:07 UTC
* Number of recursive dependencies: 114

Run `revdepcheck::cloud_details(, "SHAPforxgboost")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘SHAPforxgboost-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: shap.plot.force_plot
    > ### Title: Make the SHAP force plot
    > ### Aliases: shap.plot.force_plot
    > 
    > ### ** Examples
    > 
    > 
    ...
    ℹ Please use tidy evaluation idioms with `aes()`.
    ℹ See also `vignette("ggplot2-in-packages")` for more information.
    ℹ The deprecated feature was likely used in the SHAPforxgboost package.
      Please report the issue at
      <https://github.com/liuyanguu/SHAPforxgboost/issues>.
    Data has N = 150 | zoom in length is 50 at location 90.
    
    Error in upgradeUnit.default(x) : Not a unit object
    Calls: <Anonymous> ... is.unit -> convertUnit -> upgradeUnit -> upgradeUnit.default
    Execution halted
    ```

# shapviz

<details>

* Version: 0.9.7
* GitHub: https://github.com/ModelOriented/shapviz
* Source code: https://github.com/cran/shapviz
* Date/Publication: 2025-01-19 19:20:02 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::cloud_details(, "shapviz")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘shapviz-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: sv_force
    > ### Title: SHAP Force Plot
    > ### Aliases: sv_force sv_force.default sv_force.shapviz sv_force.mshapviz
    > 
    > ### ** Examples
    > 
    > dtrain <- xgboost::xgb.DMatrix(
    ...
     20. └─vctrs (local) `<fn>`()
     21.   └─vctrs::vec_default_ptype2(...)
     22.     ├─base::withRestarts(...)
     23.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     24.     │   └─base (local) doWithOneRestart(return(expr), restart)
     25.     └─vctrs::stop_incompatible_type(...)
     26.       └─vctrs:::stop_incompatible(...)
     27.         └─vctrs:::stop_vctrs(...)
     28.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      'fastshap', 'h2o', 'lightgbm'
    ```

# simDAG

<details>

* Version: 0.3.0
* GitHub: https://github.com/RobinDenz1/siMDAG
* Source code: https://github.com/cran/simDAG
* Date/Publication: 2025-03-30 16:00:01 UTC
* Number of recursive dependencies: 150

Run `revdepcheck::cloud_details(, "simDAG")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘v_cookbook.Rmd’ using rmarkdown
    
    Quitting from v_cookbook.Rmd:224-237 [unnamed-chunk-11]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error:
    ! An error occured when processing node 'Y'. The message was:
    Error: An error occured when processing node '.'. The message was:
    Error in rowSums(mapply("*", data, betas)): 'x' must be an array of at least two dimensions
    ...
    
    Error: processing vignette 'v_cookbook.Rmd' failed with diagnostics:
    An error occured when processing node 'Y'. The message was:
    Error: An error occured when processing node '.'. The message was:
    Error in rowSums(mapply("*", data, betas)): 'x' must be an array of at least two dimensions
    
    
    --- failed re-building ‘v_cookbook.Rmd’
    
    --- re-building ‘v_covid_example.Rmd’ using rmarkdown
    ```

# sjPlot

<details>

* Version: 2.8.17
* GitHub: https://github.com/strengejacke/sjPlot
* Source code: https://github.com/cran/sjPlot
* Date/Publication: 2024-11-29 11:20:03 UTC
* Number of recursive dependencies: 194

Run `revdepcheck::cloud_details(, "sjPlot")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘blackwhitefigures.Rmd’ using rmarkdown
    ```

# skewlmm

<details>

* Version: 1.1.2
* GitHub: https://github.com/fernandalschumacher/skewlmm
* Source code: https://github.com/cran/skewlmm
* Date/Publication: 2024-12-15 00:50:02 UTC
* Number of recursive dependencies: 79

Run `revdepcheck::cloud_details(, "skewlmm")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘skewlmm-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot
    > ### Title: Plot a smn.lmm or smsn.lmm object
    > ### Aliases: plot.SMN plot.SMSN
    > ### Keywords: hplot
    > 
    > ### ** Examples
    > 
    ...
    ! Unknown scale type:
    Backtrace:
        ▆
     1. ├─base::plot(fm1)
     2. └─skewlmm:::plot.SMN(fm1)
     3.   └─ggplot2::scale_color_continuous(high = "#132B43", low = "#56B1F7")
     4.     └─ggplot2:::scale_backward_compatibility(...)
     5.       └─cli::cli_abort("Unknown scale type: {.val {scale}}")
     6.         └─rlang::abort(...)
    Execution halted
    ```

# smer

<details>

* Version: 0.0.1
* GitHub: https://github.com/lcrawlab/sme
* Source code: https://github.com/cran/smer
* Date/Publication: 2025-01-16 15:50:01 UTC
* Number of recursive dependencies: 111

Run `revdepcheck::cloud_details(, "smer")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘smer.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 45.7Mb
      sub-directories of 1Mb or more:
        libs  42.7Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# soc.ca

<details>

* Version: 0.8.0
* GitHub: https://github.com/Rsoc/soc.ca
* Source code: https://github.com/cran/soc.ca
* Date/Publication: 2021-09-02 22:50:02 UTC
* Number of recursive dependencies: 128

Run `revdepcheck::cloud_details(, "soc.ca")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘soc.ca-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: map.ind
    > ### Title: Map the individuals of a soc.ca analysis
    > ### Aliases: map.ind
    > 
    > ### ** Examples
    > 
    > example(soc.ca)
    ...
    > map + scale_color_continuous(low = "white", high = "red")
    Error in `scale_backward_compatibility()`:
    ! Unknown scale type:
    Backtrace:
        ▆
     1. └─ggplot2::scale_color_continuous(low = "white", high = "red")
     2.   └─ggplot2:::scale_backward_compatibility(...)
     3.     └─cli::cli_abort("Unknown scale type: {.val {scale}}")
     4.       └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘FactoMineR’ ‘flextable’ ‘htmlTable’ ‘stringr’
      All declared Imports should be used.
    ```

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) map.active.Rd:82: Lost braces
        82 | \link{guides}, \link{theme} and link{guide_legend} functions from the
           |                                     ^
    checkRd: (-1) map.add.Rd:99: Lost braces
        99 | \link{guides}, \link{theme} and link{guide_legend} functions from the
           |                                     ^
    checkRd: (-1) map.csa.mca.Rd:35: Lost braces
        35 | \link{soc.csa}, \link{map.csa.all}, link{map.csa.mca.array}
           |                                         ^
    checkRd: (-1) map.ctr.Rd:85: Lost braces
    ...
           |                                     ^
    checkRd: (-1) map.ind.Rd:80: Lost braces
        80 | \link{guides}, \link{theme} and link{guide_legend} functions from the
           |                                     ^
    checkRd: (-1) map.mod.Rd:82: Lost braces
        82 | \link{guides}, \link{theme} and link{guide_legend} functions from the
           |                                     ^
    checkRd: (-1) map.sup.Rd:82: Lost braces
        82 | \link{guides}, \link{theme} and link{guide_legend} functions from the
           |                                     ^
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 938 marked UTF-8 strings
    ```

# SoupX

<details>

* Version: 1.6.2
* GitHub: https://github.com/constantAmateur/SoupX
* Source code: https://github.com/cran/SoupX
* Date/Publication: 2022-11-01 14:00:03 UTC
* Number of recursive dependencies: 200

Run `revdepcheck::cloud_details(, "SoupX")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘pbmcTutorial.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        data   3.9Mb
        doc    1.0Mb
    ```

# spectralR

<details>

* Version: 0.1.3
* GitHub: https://github.com/olehprylutskyi/spectralR
* Source code: https://github.com/cran/spectralR
* Date/Publication: 2023-08-24 09:20:02 UTC
* Number of recursive dependencies: 118

Run `revdepcheck::cloud_details(, "spectralR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘tinytest.R’
    Running the tests in ‘tests/tinytest.R’ failed.
    Complete output:
      > if ( requireNamespace("tinytest", quietly=TRUE) ){
      +   tinytest::test_package("spectralR")
      + }
      
      test_get.pixel.data.R.........    1 tests [0;32mOK[0m [0;36m91ms[0m
      
      test_prepare.vector.data.R....    1 tests [0;32mOK[0m 
    ...
      test_prepare.vector.data.R....    8 tests [0;32mOK[0m 
      test_prepare.vector.data.R....    9 tests [0;32mOK[0m 
      test_prepare.vector.data.R....   10 tests [0;32mOK[0m 
      test_prepare.vector.data.R....   11 tests [0;32mOK[0m 
      test_prepare.vector.data.R....   12 tests [0;32mOK[0m [0;36m45ms[0m
      
      test_spectral.curves.plot.R...    1 tests [0;32mOK[0m Joining with `by = join_by(variable)`
      Error in if (msg != "") { : the condition has length > 1
      Calls: <Anonymous> ... lapply -> FUN -> eval -> eval -> expect_silent -> fun
      Execution halted
    ```

# spinifex

<details>

* Version: 0.3.8
* GitHub: https://github.com/nspyrison/spinifex
* Source code: https://github.com/cran/spinifex
* Date/Publication: 2025-01-08 22:10:02 UTC
* Number of recursive dependencies: 152

Run `revdepcheck::cloud_details(, "spinifex")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(spinifex)
      Loading required package: tourr
      --------------------------------------------------------
      spinifex --- version 0.3.8
      Please share bugs, suggestions, and feature requests at:
    ...
       2. │ └─base::withCallingHandlers(...)
       3. └─spinifex::play_tour_path(tour_path = tpath, data = dat_std, angle = 1)
       4.   └─spinifex (local) render_type(frames = tour_df, ...)
       5.     ├─plotly::ggplotly(p = gg, tooltip = "tooltip")
       6.     └─plotly:::ggplotly.ggplot(p = gg, tooltip = "tooltip")
       7.       └─plotly::gg2list(...)
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 80 ]
      Error: Test failures
      Execution halted
    ```

# sport

<details>

* Version: 0.2.1
* GitHub: https://github.com/gogonzo/sport
* Source code: https://github.com/cran/sport
* Date/Publication: 2024-01-08 23:50:02 UTC
* Number of recursive dependencies: 68

Run `revdepcheck::cloud_details(, "sport")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > 
      > test_check("sport")
      Loading required package: sport
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 238 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test_output.R:30:3'): Scale is labelled 'r' ───────────────────────
      p$labels$y not identical to "r".
      target is NULL, current is character
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 238 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 7504 marked UTF-8 strings
    ```

# SqueakR

<details>

* Version: 1.3.0
* GitHub: https://github.com/osimon81/SqueakR
* Source code: https://github.com/cran/SqueakR
* Date/Publication: 2022-06-28 09:20:04 UTC
* Number of recursive dependencies: 147

Run `revdepcheck::cloud_details(, "SqueakR")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘SqueakR.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.8Mb
      sub-directories of 1Mb or more:
        doc   8.2Mb
    ```

# statgenGWAS

<details>

* Version: 1.0.11
* GitHub: https://github.com/Biometris/statgenGWAS
* Source code: https://github.com/cran/statgenGWAS
* Date/Publication: 2025-03-31 13:30:06 UTC
* Number of recursive dependencies: 71

Run `revdepcheck::cloud_details(, "statgenGWAS")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘tinytest.R’
    Running the tests in ‘tests/tinytest.R’ failed.
    Complete output:
      > 
      > if ( requireNamespace("tinytest", quietly=TRUE) ){
      +   tinytest::test_package("statgenGWAS")
      + }
      
      test_GWAS.R...................    0 tests    
      test_GWAS.R...................    0 tests    
    ...
      ℹ Please use `linewidth` instead.
      ℹ The deprecated feature was likely used in the statgenGWAS package.
        Please report the issue at <https://github.com/Biometris/statgenGWAS/issues>. 
      2: In grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
        for '← 2@3' in 'mbcsToSbcs': <- substituted for ← (U+2190)
      3: `aes_()` was deprecated in ggplot2 3.0.0.
      ℹ Please use tidy evaluation idioms with `aes()`
      ℹ The deprecated feature was likely used in the statgenGWAS package.
        Please report the issue at <https://github.com/Biometris/statgenGWAS/issues>. 
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 15.2Mb
      sub-directories of 1Mb or more:
        data   7.0Mb
        libs   7.2Mb
    ```

# statgenGxE

<details>

* Version: 1.0.9
* GitHub: https://github.com/Biometris/statgenGxE
* Source code: https://github.com/cran/statgenGxE
* Date/Publication: 2024-09-18 07:10:03 UTC
* Number of recursive dependencies: 86

Run `revdepcheck::cloud_details(, "statgenGxE")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(statgenGxE)
      > 
      > test_check("statgenGxE")
      [ FAIL 8 | WARN 3 | SKIP 17 | PASS 440 ]
      
      ══ Skipped tests (17) ══════════════════════════════════════════════════════════
    ...
      ── Failure ('test-plots.R:354:3'): Option colorGenoBy in scatterFit plot functions correctly ──
      p1$labels$colour not equal to "family".
      target is NULL, current is character
      ── Failure ('test-plots.R:405:3'): VarCov plot gives correct output types ──────
      `geoms` not equal to "GeomTile".
      names for target but not for current
      
      [ FAIL 8 | WARN 3 | SKIP 17 | PASS 440 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘asreml’
    ```

# statgenHTP

<details>

* Version: 1.0.8
* GitHub: https://github.com/Biometris/statgenHTP
* Source code: https://github.com/cran/statgenHTP
* Date/Publication: 2025-04-29 10:30:02 UTC
* Number of recursive dependencies: 132

Run `revdepcheck::cloud_details(, "statgenHTP")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘tinytest.R’
    Running the tests in ‘tests/tinytest.R’ failed.
    Complete output:
      > 
      > if ( requireNamespace("tinytest", quietly=TRUE) ){
      +   tinytest::test_package("statgenHTP")
      + }
      
      test_TP.R.....................    0 tests    
      test_TP.R.....................    0 tests    
    ...
      test_detectSerieOut.R.........    7 tests [0;32mOK[0m 
      test_detectSerieOut.R.........    8 tests [0;32mOK[0m 
      test_detectSerieOut.R.........    9 tests [0;32mOK[0m Error in inherits(w, class) && grepl(pattern, w$message, ...) : 
        'length = 2' in coercion to 'logical(1)'
      Calls: <Anonymous> ... eval -> expect_warning -> fun -> sapply -> lapply -> FUN
      In addition: Warning messages:
      1:  125 failed to parse. 
      2: Ignoring unknown labels:
      • `colour = ""` 
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘asreml’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.0Mb
      sub-directories of 1Mb or more:
        data   6.5Mb
    ```

# statgenIBD

<details>

* Version: 1.0.8
* GitHub: https://github.com/Biometris/statgenIBD
* Source code: https://github.com/cran/statgenIBD
* Date/Publication: 2025-02-05 12:00:02 UTC
* Number of recursive dependencies: 59

Run `revdepcheck::cloud_details(, "statgenIBD")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘tinytest.R’
    Running the tests in ‘tests/tinytest.R’ failed.
    Complete output:
      > 
      > if ( requireNamespace("tinytest", quietly=TRUE) ){
      +   tinytest::test_package("statgenIBD")
      + }
      
      test_IBDprob.R................    0 tests    
      test_IBDprob.R................    0 tests    
    ...
      test_IBDprob.R................    6 tests [0;32mOK[0m 
      test_IBDprob.R................    7 tests [0;32mOK[0m 
      test_IBDprob.R................    8 tests [0;32mOK[0m 
      test_IBDprob.R................    9 tests [0;32mOK[0m 
      test_IBDprob.R................    9 tests [0;32mOK[0m 
      test_IBDprob.R................   10 tests [0;32mOK[0m 
      test_IBDprob.R................   11 tests [0;32mOK[0m 
      test_IBDprob.R................   12 tests [0;32mOK[0m Error in if (msg != "") { : the condition has length > 1
      Calls: <Anonymous> ... lapply -> FUN -> eval -> eval -> expect_silent -> fun
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.7Mb
      sub-directories of 1Mb or more:
        extdata   2.4Mb
        libs      6.4Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘statgenGWAS’
      All declared Imports should be used.
    ```

# stats19

<details>

* Version: 3.3.1
* GitHub: https://github.com/ropensci/stats19
* Source code: https://github.com/cran/stats19
* Date/Publication: 2025-01-15 08:00:02 UTC
* Number of recursive dependencies: 158

Run `revdepcheck::cloud_details(, "stats19")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘blog.Rmd’ using rmarkdown
    [WARNING] Citeproc: citation sarkar_street_2018 not found
    --- finished re-building ‘blog.Rmd’
    
    --- re-building ‘stats19-training-setup.Rmd’ using rmarkdown
    --- finished re-building ‘stats19-training-setup.Rmd’
    
    --- re-building ‘stats19-training.Rmd’ using rmarkdown
    ```

# tabledown

<details>

* Version: 1.0.0
* GitHub: https://github.com/masiraji/tabledown
* Source code: https://github.com/cran/tabledown
* Date/Publication: 2024-05-02 13:40:03 UTC
* Number of recursive dependencies: 164

Run `revdepcheck::cloud_details(, "tabledown")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tabledown-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggreliability_plotly
    > ### Title: A Function for Creating Item Response Theory based reliability
    > ###   plot based on plotly.
    > ### Aliases: ggreliability_plotly
    > 
    > ### ** Examples
    > 
    ...
    Iteration: 17, Log-Lik: -5351.363, Max-Change: 0.00011
    Iteration: 18, Log-Lik: -5351.363, Max-Change: 0.00054
    Iteration: 19, Log-Lik: -5351.363, Max-Change: 0.00012
    Iteration: 20, Log-Lik: -5351.363, Max-Change: 0.00035
    Iteration: 21, Log-Lik: -5351.363, Max-Change: 0.00010
    > 
    > plot <- ggreliability_plotly(data, model)
    Error in pm[[2]] : subscript out of bounds
    Calls: ggreliability_plotly -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 551 marked UTF-8 strings
    ```

# TCIU

<details>

* Version: 1.2.7
* GitHub: https://github.com/SOCR/TCIU
* Source code: https://github.com/cran/TCIU
* Date/Publication: 2024-09-15 02:40:02 UTC
* Number of recursive dependencies: 166

Run `revdepcheck::cloud_details(, "TCIU")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘TCIU-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: fmri_image
    > ### Title: interactive graph object of the fMRI image
    > ### Aliases: fmri_image
    > 
    > ### ** Examples
    > 
    > fmri_generate = fmri_simulate_func(dim_data = c(64, 64, 40), mask = mask)
    > fmri_image(fmri_generate$fmri_data, option='manually', voxel_location = c(40,22,33), time = 4)
    Error in pm[[2]] : subscript out of bounds
    Calls: fmri_image ... add_trace -> add_data -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘tciu-LT-kimesurface.Rmd’ using rmarkdown
    
    Quitting from tciu-LT-kimesurface.Rmd:158-160 [unnamed-chunk-5]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'tciu-LT-kimesurface.Rmd' failed with diagnostics:
    Problem while computing layer data.
    ...
    
    Error: processing vignette 'tciu-fMRI-analytics.Rmd' failed with diagnostics:
    subscript out of bounds
    --- failed re-building ‘tciu-fMRI-analytics.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘tciu-LT-kimesurface.Rmd’ ‘tciu-fMRI-analytics.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 14.1Mb
      sub-directories of 1Mb or more:
        data   1.5Mb
        doc   12.0Mb
    ```

# telraamStats

<details>

* Version: 1.1.2
* GitHub: https://github.com/KetsiaGuichard/telraamStats
* Source code: https://github.com/cran/telraamStats
* Date/Publication: 2024-05-27 17:40:02 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::cloud_details(, "telraamStats")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘telraamStats-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gg_car_speed_histogram
    > ### Title: Histogram of car speed over a period, for a segment or a subset
    > ###   of segment.
    > ### Aliases: gg_car_speed_histogram
    > 
    > ### ** Examples
    > 
    ...
     20. └─vctrs (local) `<fn>`()
     21.   └─vctrs::vec_default_ptype2(...)
     22.     ├─base::withRestarts(...)
     23.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     24.     │   └─base (local) doWithOneRestart(return(expr), restart)
     25.     └─vctrs::stop_incompatible_type(...)
     26.       └─vctrs:::stop_incompatible(...)
     27.         └─vctrs:::stop_vctrs(...)
     28.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘data-details.Rmd’ using rmarkdown
    --- finished re-building ‘data-details.Rmd’
    
    --- re-building ‘data-visualization.Rmd’ using rmarkdown
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 3271 marked UTF-8 strings
    ```

# thematic

<details>

* Version: 0.1.6
* GitHub: https://github.com/rstudio/thematic
* Source code: https://github.com/cran/thematic
* Date/Publication: 2024-07-29 15:50:02 UTC
* Number of recursive dependencies: 103

Run `revdepcheck::cloud_details(, "thematic")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘thematic-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: sequential_gradient
    > ### Title: Control parameters of the sequential colorscale
    > ### Aliases: sequential_gradient
    > 
    > ### ** Examples
    > 
    > 
    > # Gradient from fg to accent
    > fg <- sequential_gradient(1, 0)
    > thematic_on("black", "white", "salmon", sequential = fg)
    > ggplot2::qplot(1:10, 1:10, color = 1:10)
    Warning: `qplot()` was deprecated in ggplot2 3.4.0.
    Error in adjust_color(user_default$colour, bg, fg, accent) : 
      Internal error: adjust_color() expects an input of length 1
    Calls: <Anonymous> ... <Anonymous> -> <Anonymous> -> update_defaults -> adjust_color
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(thematic)
      > 
      > test_check("thematic")
      [ FAIL 9 | WARN 1 | SKIP 7 | PASS 27 ]
      
      ══ Skipped tests (7) ═══════════════════════════════════════════════════════════
    ...
       10.             └─base::Map(...)
       11.               └─base::mapply(FUN = f, ..., SIMPLIFY = FALSE)
       12.                 └─thematic (local) `<fn>`(dots[[1L]][[1L]], dots[[2L]][[1L]])
       13.                   ├─ggplot2::update_geom_defaults(...)
       14.                   │ └─ggplot2:::update_defaults(geom, "Geom", new, env = parent.frame())
       15.                   └─thematic:::adjust_color(user_default$colour, bg, fg, accent)
      
      [ FAIL 9 | WARN 1 | SKIP 7 | PASS 27 ]
      Error: Test failures
      Execution halted
    ```

# tidycat

<details>

* Version: 0.1.2
* GitHub: https://github.com/guyabel/tidycat
* Source code: https://github.com/cran/tidycat
* Date/Publication: 2021-08-02 04:20:01 UTC
* Number of recursive dependencies: 68

Run `revdepcheck::cloud_details(, "tidycat")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tidycat-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: tidy_categorical
    > ### Title: Expand broom::tidy() Outputs for Categorical Parameter Estimates
    > ### Aliases: tidy_categorical
    > 
    > ### ** Examples
    > 
    > # strip ordering in factors (currently ordered factor not supported)
    ...
    > ggplot(data = d0,
    +        mapping = aes(x = level, colour = reference,
    +                      y = estimate, ymin = conf.low, ymax = conf.high)) +
    +   facet_row(facets = vars(variable), scales = "free_x", space = "free") +
    +   geom_hline(yintercept = 0, linetype = "dashed") +
    +   geom_pointrange() +
    +   theme(axis.text.x = element_text(angle = 45, hjust = 1))
    Error in space$x : $ operator is invalid for atomic vectors
    Calls: <Anonymous> ... <Anonymous> -> draw_panels -> <Anonymous> -> init_gtable
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘intro.Rmd’ using rmarkdown
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

# tidyCDISC

<details>

* Version: 0.2.1
* GitHub: https://github.com/Biogen-Inc/tidyCDISC
* Source code: https://github.com/cran/tidyCDISC
* Date/Publication: 2023-03-16 14:20:02 UTC
* Number of recursive dependencies: 132

Run `revdepcheck::cloud_details(, "tidyCDISC")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(tidyCDISC)
      > library(shinyjs)
      
      Attaching package: 'shinyjs'
      
    ...
        6. ├─plotly::config(...)
        7. │ └─plotly:::modify_list(p$x$config, args)
        8. │   ├─utils::modifyList(x %||% list(), y %||% list(), ...)
        9. │   │ └─base::stopifnot(is.list(x), is.list(val))
       10. │   └─x %||% list()
       11. └─plotly::layout(...)
      
      [ FAIL 1 | WARN 1 | SKIP 15 | PASS 91 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        data   2.0Mb
        doc    1.8Mb
    ```

# tidydr

<details>

* Version: 0.0.5
* GitHub: https://github.com/YuLab-SMU/tidydr
* Source code: https://github.com/cran/tidydr
* Date/Publication: 2023-03-08 09:20:02 UTC
* Number of recursive dependencies: 76

Run `revdepcheck::cloud_details(, "tidydr")` for more info

</details>

## Newly broken

*   checking whether package ‘tidydr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/tidydr/new/tidydr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘tidydr’ ...
** package ‘tidydr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in get(x, envir = ns, inherits = FALSE) : 
  object 'len0_null' not found
Error: unable to load R code in package ‘tidydr’
Execution halted
ERROR: lazy loading failed for package ‘tidydr’
* removing ‘/tmp/workdir/tidydr/new/tidydr.Rcheck/tidydr’


```
### CRAN

```
* installing *source* package ‘tidydr’ ...
** package ‘tidydr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (tidydr)


```
# tidyposterior

<details>

* Version: 1.0.1
* GitHub: https://github.com/tidymodels/tidyposterior
* Source code: https://github.com/cran/tidyposterior
* Date/Publication: 2023-10-11 18:50:02 UTC
* Number of recursive dependencies: 177

Run `revdepcheck::cloud_details(, "tidyposterior")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(tidyposterior)
      > 
      > test_check("tidyposterior")
      
      Attaching package: 'rsample'
      
    ...
      `expected`: ".upper"
      ── Failure ('test_perf_mod.R:288:3'): workflow sets ────────────────────────────
      as.character(p_rope$labels$colour) (`actual`) not equal to "workflow" (`expected`).
      
      `actual`:             
      `expected`: "workflow"
      
      [ FAIL 12 | WARN 32 | SKIP 5 | PASS 116 ]
      Error: Test failures
      Execution halted
    ```

# timetk

<details>

* Version: 2.9.0
* GitHub: https://github.com/business-science/timetk
* Source code: https://github.com/cran/timetk
* Date/Publication: 2023-10-31 22:30:02 UTC
* Number of recursive dependencies: 205

Run `revdepcheck::cloud_details(, "timetk")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
      > # * https://testthat.r-lib.org/articles/special-files.html
    ...
        7. └─timetk:::plot_time_series.grouped_df(...)
        8.   ├─timetk::plot_time_series(...)
        9.   └─timetk:::plot_time_series.data.frame(...)
       10.     ├─plotly::ggplotly(g, dynamicTicks = TRUE)
       11.     └─plotly:::ggplotly.ggplot(g, dynamicTicks = TRUE)
       12.       └─plotly::gg2list(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 406 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2750 marked UTF-8 strings
    ```

# track2KBA

<details>

* Version: 1.1.2
* GitHub: https://github.com/BirdLifeInternational/track2kba
* Source code: https://github.com/cran/track2KBA
* Date/Publication: 2024-07-01 10:40:07 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::cloud_details(, "track2KBA")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘tinytest.R’
    Running the tests in ‘tests/tinytest.R’ failed.
    Complete output:
      > if ( requireNamespace("tinytest", quietly=TRUE) ){
      +   tinytest::test_package("track2KBA")
      + }
      
      test_estSpaceUse.R............    0 tests    
      Attaching package: 'lubridate'
      
    ...
          with cells of 0.411 square km
      
      test_mapKDE.R.................    1 tests [0;32mOK[0m 
      test_mapKDE.R.................    2 tests [0;32mOK[0m Error in if (msg != "") { : the condition has length > 1
      Calls: <Anonymous> ... lapply -> FUN -> eval -> eval -> expect_silent -> fun
      In addition: Warning message:
      In Matching::ks.boot(WI, BW, alternative = "two.sided", nboots = iterations) :
        For publication quality p-values it is recommended that 'nboots'
       be set equal to at least 500 (preferably 1000)
      Execution halted
    ```

# tradeoffaucdim

<details>

* Version: 0.1.0
* GitHub: https://github.com/luisgarcez11/tradeoffaucdim
* Source code: https://github.com/cran/tradeoffaucdim
* Date/Publication: 2025-05-02 09:40:02 UTC
* Number of recursive dependencies: 142

Run `revdepcheck::cloud_details(, "tradeoffaucdim")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tradeoffaucdim-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: compare_test
    > ### Title: Compare test
    > ### Aliases: compare_test
    > 
    > ### ** Examples
    > 
    > compare_test(obj5)
    ...
     11. │     │ └─base::withCallingHandlers(...)
     12. │     └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13. │       └─l$layer_data(plot$data)
     14. │         └─ggplot2::layer_data(..., self = self)
     15. └─base::.handleSimpleError(...)
     16.   └─rlang (local) h(simpleError(msg, call))
     17.     └─handlers[[1L]](cnd)
     18.       └─cli::cli_abort(...)
     19.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘speedglm’
      All declared Imports should be used.
    ```

# trelliscopejs

<details>

* Version: 0.2.6
* GitHub: https://github.com/hafen/trelliscopejs
* Source code: https://github.com/cran/trelliscopejs
* Date/Publication: 2021-02-01 08:00:02 UTC
* Number of recursive dependencies: 103

Run `revdepcheck::cloud_details(, "trelliscopejs")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(trelliscopejs)
      > 
      > test_check("trelliscopejs")
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 0 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
       4. └─base::lapply(...)
       5.   └─trelliscopejs (local) FUN(X[[i]], ...)
       6.     ├─base::do.call(plotly::ggplotly, c(list(p = q), plotly_args))
       7.     ├─plotly (local) `<fn>`(p = `<gg>`)
       8.     └─plotly:::ggplotly.ggplot(p = `<gg>`)
       9.       └─plotly::gg2list(...)
      
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

# tricolore

<details>

* Version: 1.2.4
* GitHub: https://github.com/jschoeley/tricolore
* Source code: https://github.com/cran/tricolore
* Date/Publication: 2024-05-15 15:00:02 UTC
* Number of recursive dependencies: 105

Run `revdepcheck::cloud_details(, "tricolore")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tricolore-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ColorKeySextant
    > ### Title: Sextant Scheme Legend
    > ### Aliases: ColorKeySextant
    > ### Keywords: internal
    > 
    > ### ** Examples
    > 
    > # NOTE: only intended for internal use and not part of the API
    > tricolore:::ColorKeySextant(center = prop.table(runif(3)),
    +                             values = c('#01A0C6', '#B8B3D8', '#F11D8C',
    +                                        '#FFB3B3', '#FFFF00', '#B3DCC3'),
    +                             label_as = 'pct_diff', show_center = TRUE)
    Error in validate_theme(theme) : could not find function "validate_theme"
    Calls: <Anonymous> ... ggplot_build.ggplot -> layers_add_or_remove_mask -> <Anonymous>
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘choropleth_maps_with_tricolore.Rmd’ using rmarkdown
    
    Quitting from choropleth_maps_with_tricolore.Rmd:60-72 [unnamed-chunk-4]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `validate_theme()`:
    ! could not find function "validate_theme"
    ---
    Backtrace:
    ...
    
    Error: processing vignette 'choropleth_maps_with_tricolore.Rmd' failed with diagnostics:
    could not find function "validate_theme"
    --- failed re-building ‘choropleth_maps_with_tricolore.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘choropleth_maps_with_tricolore.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking whether package ‘tricolore’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: The `size` argument of `element_rect()` is deprecated as of ggplot2 3.4.0.
    See ‘/tmp/workdir/tricolore/new/tricolore.Rcheck/00install.out’ for details.
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

# twowaytests

<details>

* Version: 1.5
* GitHub: NA
* Source code: https://github.com/cran/twowaytests
* Date/Publication: 2024-11-11 14:20:02 UTC
* Number of recursive dependencies: 68

Run `revdepcheck::cloud_details(, "twowaytests")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘twowaytests-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gplotTwoWay
    > ### Title: Box-and-Whisker, Violin Plots and Error Bars for Two-Way Layout
    > ### Aliases: gplotTwoWay
    > ### Keywords: functions
    > 
    > ### ** Examples
    > 
    ...
     20. └─vctrs (local) `<fn>`()
     21.   └─vctrs::vec_default_ptype2(...)
     22.     ├─base::withRestarts(...)
     23.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     24.     │   └─base (local) doWithOneRestart(return(expr), restart)
     25.     └─vctrs::stop_incompatible_type(...)
     26.       └─vctrs:::stop_incompatible(...)
     27.         └─vctrs:::stop_vctrs(...)
     28.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

# umiAnalyzer

<details>

* Version: 1.0.0
* GitHub: https://github.com/sfilges/umiAnalyzer
* Source code: https://github.com/cran/umiAnalyzer
* Date/Publication: 2021-11-25 08:40:02 UTC
* Number of recursive dependencies: 112

Run `revdepcheck::cloud_details(, "umiAnalyzer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘umiAnalyzer-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: AmpliconPlot
    > ### Title: Generate Amplicon plots
    > ### Aliases: AmpliconPlot
    > 
    > ### ** Examples
    > 
    > library(umiAnalyzer)
    ...
    ℹ Please use tidy evaluation idioms with `aes()`
    ℹ The deprecated feature was likely used in the umiAnalyzer package.
      Please report the issue at <https://github.com/sfilges/umiAnalyzer/issues>.
    Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ℹ Please use `linewidth` instead.
    ℹ The deprecated feature was likely used in the umiAnalyzer package.
      Please report the issue at <https://github.com/sfilges/umiAnalyzer/issues>.
    Error in pm[[2]] : subscript out of bounds
    Calls: AmpliconPlot -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# usmap

<details>

* Version: 0.7.1
* GitHub: https://github.com/pdil/usmap
* Source code: https://github.com/cran/usmap
* Date/Publication: 2024-03-21 04:20:02 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "usmap")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘usmap-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: .east_north_central
    > ### Title: East North Central census division
    > ### Aliases: .east_north_central
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    > plot_usmap(include = .east_north_central, labels = TRUE)
    Error in process_cpl_read_ogr(x, quiet, check_ring_dir = check_ring_dir,  : 
      package tibble not available: install first?
    Calls: plot_usmap ... st_read -> st_read.character -> process_cpl_read_ogr
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
    ...
      • data/statepov.svg
      • plot/arizona-county-map-with-labels-and-fill.svg
      • plot/example-data-state-map-with-custom-linewidth.svg
      • plot/new-england-state-map-with-labels-excluding-maine.svg
      • plot/southeastern-states-map-with-labels.svg
      • plot/state-map-with-labels.svg
      • plot/state-map-with-major-rivers.svg
      • plot/state-population-map-with-blue-outlines.svg
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘usmap1.Rmd’ using rmarkdown
    
    Quitting from usmap1.Rmd:25-27 [unnamed-chunk-1]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `process_cpl_read_ogr()`:
    ! package tibble not available: install first?
    ---
    Backtrace:
    ...
    
    Error: processing vignette 'usmap3.Rmd' failed with diagnostics:
    package tibble not available: install first?
    --- failed re-building ‘usmap3.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘usmap1.Rmd’ ‘usmap2.Rmd’ ‘usmap3.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘usmapdata:::alaska_bbox’ ‘usmapdata:::ea_crs’
      ‘usmapdata:::hawaii_bbox’ ‘usmapdata:::transform_alaska’
      ‘usmapdata:::transform_hawaii’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 17 marked UTF-8 strings
    ```

# vaccineff

<details>

* Version: 1.0.0
* GitHub: https://github.com/epiverse-trace/vaccineff
* Source code: https://github.com/cran/vaccineff
* Date/Publication: 2024-11-29 09:30:02 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::cloud_details(, "vaccineff")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/tests.html
    ...
      `expected` is a character vector ('coverage * max(dose_plot)')
      ── Failure ('test-utils_coverage.R:50:3'): `plot_coverage`: cumulative plot ────
      plt$labels$y (`actual`) not identical to "coverage * max(dose_plot)" (`expected`).
      
      `actual` is NULL
      `expected` is a character vector ('coverage * max(dose_plot)')
      
      [ FAIL 2 | WARN 0 | SKIP 6 | PASS 100 ]
      Error: Test failures
      Execution halted
    ```

# valr

<details>

* Version: 0.8.3
* GitHub: https://github.com/rnabioco/valr
* Source code: https://github.com/cran/valr
* Date/Publication: 2025-01-11 15:40:02 UTC
* Number of recursive dependencies: 146

Run `revdepcheck::cloud_details(, "valr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/tests.html
      > # * https://testthat.r-lib.org/reference/test_package.html#special-files
    ...
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test_glyph.r:13:3'): glyph labels are applied ─────────────────────
      res$labels$label (`actual`) not equal to "id" (`expected`).
      
      `actual` is NULL
      `expected` is a character vector ('id')
      
      [ FAIL 1 | WARN 0 | SKIP 4 | PASS 479 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 15.1Mb
      sub-directories of 1Mb or more:
        libs  13.9Mb
    ```

# vangogh

<details>

* Version: 0.1.1
* GitHub: https://github.com/cherylisabella/vangogh
* Source code: https://github.com/cran/vangogh
* Date/Publication: 2022-05-27 08:00:02 UTC
* Number of recursive dependencies: 60

Run `revdepcheck::cloud_details(, "vangogh")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘vangogh.Rmd’ using rmarkdown
    ```

# vDiveR

<details>

* Version: 2.0.1
* GitHub: NA
* Source code: https://github.com/cran/vDiveR
* Date/Publication: 2024-11-22 08:20:02 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "vDiveR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘vDiveR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_conservation_level
    > ### Title: Conservation Levels Distribution Plot
    > ### Aliases: plot_conservation_level
    > 
    > ### ** Examples
    > 
    > plot_conservation_level(proteins_1host, conservation_label = 1,alpha=0.8, base_size = 15)
    ...
     29. │                                     └─base::lapply(...)
     30. │                                       └─ggplot2 (local) FUN(X[[i]], ...)
     31. │                                         └─ggplot2 (local) apply_fun(cur_data)
     32. │                                           └─ggplot2 (local) fun(x, ...)
     33. └─base::.handleSimpleError(...)
     34.   └─rlang (local) h(simpleError(msg, call))
     35.     └─handlers[[1L]](cnd)
     36.       └─cli::cli_abort(...)
     37.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘maps’
      All declared Imports should be used.
    ```

# vecmatch

<details>

* Version: 1.1.0
* GitHub: https://github.com/Polymerase3/vecmatch
* Source code: https://github.com/cran/vecmatch
* Date/Publication: 2025-04-24 12:20:02 UTC
* Number of recursive dependencies: 124

Run `revdepcheck::cloud_details(, "vecmatch")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘vecmatch-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: estimate_gps
    > ### Title: Calculate treatment allocation probabilities
    > ### Aliases: estimate_gps
    > 
    > ### ** Examples
    > 
    > 
    ...
     19. └─vctrs (local) `<fn>`()
     20.   └─vctrs::vec_default_ptype2(...)
     21.     ├─base::withRestarts(...)
     22.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     23.     │   └─base (local) doWithOneRestart(return(expr), restart)
     24.     └─vctrs::stop_incompatible_type(...)
     25.       └─vctrs:::stop_incompatible(...)
     26.         └─vctrs:::stop_vctrs(...)
     27.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
    ...
       33.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
       34.     │   └─base (local) doWithOneRestart(return(expr), restart)
       35.     └─vctrs::stop_incompatible_type(...)
       36.       └─vctrs:::stop_incompatible(...)
       37.         └─vctrs:::stop_vctrs(...)
       38.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
      
      [ FAIL 10 | WARN 0 | SKIP 0 | PASS 207 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘vecmatch.Rmd’ using rmarkdown
    
    Quitting from vecmatch.Rmd:50-63 [unnamed-chunk-2]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'vecmatch.Rmd' failed with diagnostics:
    Can't combine `..1` <palette> and `..2` <character>.
    --- failed re-building ‘vecmatch.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘vecmatch.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘MatchIt’
    ```

# vivaldi

<details>

* Version: 1.0.1
* GitHub: https://github.com/GreshamLab/vivaldi
* Source code: https://github.com/cran/vivaldi
* Date/Publication: 2023-03-21 20:10:02 UTC
* Number of recursive dependencies: 100

Run `revdepcheck::cloud_details(, "vivaldi")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘vivaldi-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: snv_location
    > ### Title: snv_location
    > ### Aliases: snv_location
    > 
    > ### ** Examples
    > 
    > # Example 1:
    ...
    6      m2   PB1 234     G     A    minor     0.010     0.990
    7      m2   PB1 266     G     A    minor     0.022     0.978
    8      m2   PB2 199     A     G    minor     0.043     0.957
    9      m2   PB2  88     G     A    major     0.055     0.945
    10     m2   PB2 180     C     T    minor     0.011     0.989
    > 
    > snv_location(df)
    Error in pm[[2]] : subscript out of bounds
    Calls: snv_location -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/tests.html
      > # * https://testthat.r-lib.org/reference/test_package.html#special-files
    ...
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-snv_location.R:13:3'): expect output ─────────────────────────
      Expected `snv_location(df)` to run without any errors.
      i Actually got a <subscriptOutOfBoundsError> with text:
        subscript out of bounds
      
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 28 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘vignette.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        doc       5.4Mb
        extdata   1.1Mb
    ```

# voluModel

<details>

* Version: 0.2.2
* GitHub: https://github.com/hannahlowens/voluModel
* Source code: https://github.com/cran/voluModel
* Date/Publication: 2024-08-20 22:50:01 UTC
* Number of recursive dependencies: 131

Run `revdepcheck::cloud_details(, "voluModel")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘voluModel-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pointCompMap
    > ### Title: Comparative point mapping
    > ### Aliases: pointCompMap
    > ### Keywords: plotting
    > 
    > ### ** Examples
    > 
    ...
      6.       └─ggplot2 (local) setup(..., self = self)
      7.         └─self$coord$setup_params(data)
      8.           └─ggplot2 (local) setup_params(..., self = self)
      9.             └─ggproto_parent(Coord, self)$setup_params(data)
     10.               └─ggplot2 (local) setup_params(..., self = self)
     11.                 └─ggplot2:::parse_coord_expand(expand = self$expand %||% TRUE)
     12.                   └─ggplot2:::check_logical(expand)
     13.                     └─ggplot2:::stop_input_type(...)
     14.                       └─rlang::abort(message, ..., call = call, arg = arg)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘a_Introduction.Rmd’ using rmarkdown
    
    Quitting from a_Introduction.Rmd:43-58 [show points]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'a_Introduction.Rmd' failed with diagnostics:
    `expand` must be a logical vector, not the number 0.05.
    --- failed re-building ‘a_Introduction.Rmd’
    
    --- re-building ‘b_RasterProcessing.Rmd’ using rmarkdown
    ```

# vvshiny

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/vvshiny
* Date/Publication: 2023-07-19 15:30:02 UTC
* Number of recursive dependencies: 134

Run `revdepcheck::cloud_details(, "vvshiny")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
      > # * https://testthat.r-lib.org/articles/special-files.html
    ...
       1. ├─vvshiny::ggplotly_with_legend(p, color = "grp", mapping_table = list(grp = "Group")) at test-ggplotly_with_legend.R:15:3
       2. │ ├─plotly::ggplotly(plot) %>% ...
       3. │ ├─plotly::ggplotly(plot)
       4. │ └─plotly:::ggplotly.ggplot(plot)
       5. │   └─plotly::gg2list(...)
       6. └─plotly::layout(...)
      
      [ FAIL 1 | WARN 4 | SKIP 0 | PASS 60 ]
      Error: Test failures
      Execution halted
    ```

# wilson

<details>

* Version: 2.4.2
* GitHub: https://github.com/loosolab/wilson
* Source code: https://github.com/cran/wilson
* Date/Publication: 2021-04-19 09:40:02 UTC
* Number of recursive dependencies: 203

Run `revdepcheck::cloud_details(, "wilson")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(wilson)
      
      Attaching package: 'wilson'
      
      The following object is masked from 'package:stats':
      
    ...
      Backtrace:
          ▆
       1. └─wilson::create_geneview(...) at test-interactive-plots.R:21:3
       2.   ├─plotly::ggplotly(...)
       3.   └─plotly:::ggplotly.ggplot(...)
       4.     └─plotly::gg2list(...)
      
      [ FAIL 3 | WARN 11 | SKIP 1 | PASS 74 ]
      Error: Test failures
      Execution halted
    ```

# wpa

<details>

* Version: 1.9.1
* GitHub: https://github.com/microsoft/wpa
* Source code: https://github.com/cran/wpa
* Date/Publication: 2024-06-06 13:20:02 UTC
* Number of recursive dependencies: 121

Run `revdepcheck::cloud_details(, "wpa")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/tests.html
      > # * https://testthat.r-lib.org/reference/test_package.html#special-files
    ...
       3. │   └─plot_data_long %>% ...
       4. └─wpa::plot_hourly_pat(...)
       5.   └─ggplot2::scale_fill_continuous(...)
       6.     └─ggplot2:::scale_backward_compatibility(...)
       7.       └─cli::cli_abort("Unknown scale type: {.val {scale}}")
       8.         └─rlang::abort(...)
      
      [ FAIL 1 | WARN 250 | SKIP 0 | PASS 9 ]
      Error: Test failures
      Execution halted
    ```

# xaringanthemer

<details>

* Version: 0.4.3
* GitHub: https://github.com/gadenbuie/xaringanthemer
* Source code: https://github.com/cran/xaringanthemer
* Date/Publication: 2024-09-15 14:00:02 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::cloud_details(, "xaringanthemer")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ggplot2-themes.Rmd’ using rmarkdown
    ```

# xpose

<details>

* Version: 0.4.19
* GitHub: https://github.com/UUPharmacometrics/xpose
* Source code: https://github.com/cran/xpose
* Date/Publication: 2025-01-07 20:00:02 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "xpose")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘xpose-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: amt_vs_idv
    > ### Title: Compartment kinetics
    > ### Aliases: amt_vs_idv
    > 
    > ### ** Examples
    > 
    > amt_vs_idv(xpdb_ex_pk, nrow = 2, ncol = 1)
    Using data from $prob no.1
    Tidying data by ID, SEX, MED1, MED2, AMT ... and 20 more variables
    Error in label_variable(labels, multi_line = multi_line) : 
      could not find function "label_variable"
    Calls: <Anonymous> ... list2 -> labeller -> lapply -> FUN -> .default -> x
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(xpose)
      Loading required package: ggplot2
      
      Attaching package: 'xpose'
      
      The following object is masked from 'package:stats':
    ...
       26.                             │   └─rlang::list2(...)
       27.                             └─ggplot2::labeller(labels)
       28.                               └─base::lapply(...)
       29.                                 └─ggplot2 (local) FUN(X[[i]], ...)
       30.                                   └─ggplot2 (local) .default(labels[label])
       31.                                     └─ggplot2 (local) x(labels)
      
      [ FAIL 3 | WARN 1 | SKIP 7 | PASS 517 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘access_xpdb_data.Rmd’ using rmarkdown
    --- finished re-building ‘access_xpdb_data.Rmd’
    
    --- re-building ‘customize_plots.Rmd’ using rmarkdown
    ```

# xpose.xtras

<details>

* Version: 0.0.2
* GitHub: https://github.com/jprybylski/xpose.xtras
* Source code: https://github.com/cran/xpose.xtras
* Date/Publication: 2024-11-21 17:20:02 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::cloud_details(, "xpose.xtras")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘xpose.xtras-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: grab_xpose_plot
    > ### Title: Grab processed 'xpose_plot'
    > ### Aliases: grab_xpose_plot
    > 
    > ### ** Examples
    > 
    > 
    ...
    + eta_vs_catcov(etavar = ETA1) %>%
    + grab_xpose_plot()
    Using data from $prob no.1
    Removing duplicated rows based on: ID
    Tidying data by ID, DOSE, AMT, SS, II ... and 23 more variables
    Warning: attributes are not identical across measure variables; they will be dropped
    Error in label_variable(labels, multi_line = multi_line) : 
      could not find function "label_variable"
    Calls: %>% ... list2 -> labeller -> lapply -> FUN -> .default -> x
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
      > # * https://testthat.r-lib.org/articles/special-files.html
    ...
       21.                         └─scales[[i]][[method]](vec_slice(data[[var]], scale_index[[i]]))
       22.                           └─ggplot2 (local) train(..., self = self)
       23.                             └─self$range$train(x, call = self$call)
       24.                               └─scales::train_continuous(x, self$range, call = call)
       25.                                 └─cli::cli_abort(...)
       26.                                   └─rlang::abort(...)
      
      [ FAIL 2 | WARN 0 | SKIP 6 | PASS 653 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘a01-the-xp_xtra-object.Rmd’ using rmarkdown
    
    Quitting from a01-the-xp_xtra-object.Rmd:94-96 [plot_cont]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `label_variable()`:
    ! could not find function "label_variable"
    ---
    Backtrace:
    ...
     62.                             └─ggplot2 (local) FUN(X[[i]], ...)
     63.                               └─ggplot2 (local) .default(labels[label])
     64.                                 └─ggplot2 (local) x(labels)
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'a01-the-xp_xtra-object.Rmd' failed with diagnostics:
    could not find function "label_variable"
    --- failed re-building ‘a01-the-xp_xtra-object.Rmd’
    
    --- re-building ‘a02-xpose-sets.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.9Mb
      sub-directories of 1Mb or more:
        data   2.5Mb
        doc    2.4Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

# xray

<details>

* Version: 0.2
* GitHub: https://github.com/sicarul/xray
* Source code: https://github.com/cran/xray
* Date/Publication: 2017-12-08 05:15:59 UTC
* Number of recursive dependencies: 35

Run `revdepcheck::cloud_details(, "xray")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘xray-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: distributions
    > ### Title: Analyze each variable and generate a histogram describing it's
    > ###   distribution.
    > ### Aliases: distributions
    > 
    > ### ** Examples
    > 
    > library(xray)
    > distributions(mtcars)
    ================================================================================Error in { : 
      task 1 failed - "`type` must be a character vector or list of character vectors, not `NULL`."
    Calls: distributions -> %do% -> <Anonymous>
    Execution halted
    ```

