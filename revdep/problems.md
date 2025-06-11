# activAnalyzer

<details>

* Version: 2.1.2
* GitHub: https://github.com/pydemull/activAnalyzer
* Source code: https://github.com/cran/activAnalyzer
* Date/Publication: 2024-09-23 23:40:02 UTC
* Number of recursive dependencies: 145

Run `revdepcheck::cloud_details(, "activAnalyzer")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘activAnalyzer.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        R         1.5Mb
        doc       1.0Mb
        extdata   2.0Mb
    ```

# actxps

<details>

* Version: 1.6.0
* GitHub: https://github.com/mattheaphy/actxps
* Source code: https://github.com/cran/actxps
* Date/Publication: 2025-01-07 13:00:02 UTC
* Number of recursive dependencies: 129

Run `revdepcheck::cloud_details(, "actxps")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘actxps.Rmd’ using rmarkdown
    
    Quitting from actxps.Rmd:10-18 [setup]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `get()`:
    ! object 'ggplot_build.ggplot' not found
    ---
    Backtrace:
    ...
    
    Error: processing vignette 'transactions.Rmd' failed with diagnostics:
    object 'ggplot_build.ggplot' not found
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
* Number of recursive dependencies: 61

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

# adsoRptionCMF

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/adsoRptionCMF
* Date/Publication: 2025-06-05 10:00:05 UTC
* Number of recursive dependencies: 46

Run `revdepcheck::cloud_details(, "adsoRptionCMF")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘adsoRptionCMF-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: fit_freundlichLM
    > ### Title: Freundlich Isotherm Linear Analysis
    > ### Aliases: fit_freundlichLM
    > 
    > ### ** Examples
    > 
    > Ce <- c(0.01353, 0.04648, 0.13239, 0.27714, 0.41600, 0.63607, 0.80435, 1.10327, 1.58223)
    ...
     17. │             └─l$compute_geom_2(d, theme = plot@theme)
     18. │               └─ggplot2 (local) compute_geom_2(..., self = self)
     19. │                 └─self$geom$use_defaults(...)
     20. │                   └─ggplot2 (local) use_defaults(..., self = self)
     21. │                     └─ggplot2:::check_aesthetics(new_params, nrow(data))
     22. │                       └─vctrs::list_sizes(x)
     23. └─vctrs:::stop_scalar_type(`<fn>`(`<expression>`), "x$label", `<env>`)
     24.   └─vctrs:::stop_vctrs(...)
     25.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

# adw

<details>

* Version: 0.4.0
* GitHub: https://github.com/PanfengZhang/adw
* Source code: https://github.com/cran/adw
* Date/Publication: 2024-04-15 19:10:16 UTC
* Number of recursive dependencies: 59

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
* Number of recursive dependencies: 94

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
* Number of recursive dependencies: 237

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
* Number of recursive dependencies: 117

Run `revdepcheck::cloud_details(, "AgroR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘AgroR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: TBARPLOT.reverse
    > ### Title: Graph: Reverse graph of DICT, DBCT and DQL output when
    > ###   geom="bar"
    > ### Aliases: TBARPLOT.reverse
    > ### Keywords: Experimental
    > 
    > ### ** Examples
    ...
    3  7.822709e-05   0.59104221 0.46541352     0.3244232  4.189412
    4  1.496061e-02   0.09984009 0.09849058     0.1332682  6.462590
    5  1.757687e-04   0.67552390 0.42726077     0.3008609  2.566743
    6  1.138255e-04   0.70461554 0.37578092     0.6357482  2.093636
    > TBARPLOT.reverse(a)
    Warning in geom_text(aes(label = letra, y = media + desvio + sup), position = position_dodge(0.9),  :
      Ignoring empty aesthetics: `size` and `family`.
    Error in if (colo == "gray") { : argument is of length zero
    Calls: TBARPLOT.reverse
    Execution halted
    ```

# AgroReg

<details>

* Version: 1.2.10
* GitHub: NA
* Source code: https://github.com/cran/AgroReg
* Date/Publication: 2024-01-16 12:50:16 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::cloud_details(, "AgroReg")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘AgroReg-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: adjust_scale
    > ### Title: Utils: Adjust y and x scale
    > ### Aliases: adjust_scale
    > 
    > ### ** Examples
    > 
    > library(AgroReg)
    ...
    > data("aristolochia")
    > attach(aristolochia)
    > a=LM(trat,resp)
    
    > b=LL(trat,resp,npar = "LL.3")
    > a=plot_arrange(list(a,b),gray = TRUE)
    Error in equation[[i]] <- plots[[i]][[3]]$plot$s : 
      replacement has length zero
    Calls: plot_arrange
    Execution halted
    ```

# alookr

<details>

* Version: 0.3.9
* GitHub: https://github.com/choonghyunryu/alookr
* Source code: https://github.com/cran/alookr
* Date/Publication: 2024-02-11 07:30:02 UTC
* Number of recursive dependencies: 168

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

# AnalysisLin

<details>

* Version: 0.1.2
* GitHub: NA
* Source code: https://github.com/cran/AnalysisLin
* Date/Publication: 2024-01-30 00:10:10 UTC
* Number of recursive dependencies: 118

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

# andurinha

<details>

* Version: 0.0.2
* GitHub: https://github.com/noemiallefs/andurinha
* Source code: https://github.com/cran/andurinha
* Date/Publication: 2020-08-13 08:40:02 UTC
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "andurinha")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(andurinha)
      > 
      > test_check("andurinha")
        sample absMax
      1      A  0.465
      2      B  0.402
    ...
      ── Failure ('test_funtionsOutput.R:29:3'): plotPeaks return a ggplot objetc ────
      class(...) not equal to c("gg", "ggplot").
      Lengths differ: 5 is not 2
      ── Failure ('test_funtionsOutput.R:32:3'): plotPeaks return a ggplot objetc ────
      class(...) not equal to c("gg", "ggplot").
      Lengths differ: 5 is not 2
      
      [ FAIL 4 | WARN 0 | SKIP 0 | PASS 43 ]
      Error: Test failures
      Execution halted
    ```

# animbook

<details>

* Version: 1.0.0
* GitHub: https://github.com/KrisanatA/animbook
* Source code: https://github.com/cran/animbook
* Date/Publication: 2023-12-05 17:50:07 UTC
* Number of recursive dependencies: 85

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
    > 
    > plot <- wallaby_plot(animbook)
    You can now use the animbook::anim_animate() function to
              transform it into an animated object
    > 
    > animate <- anim_animate(plot)
    Error in plot$plot_env : object of type 'object' is not subsettable
    Calls: anim_animate
    Execution halted
    ```

# ANN2

<details>

* Version: 2.3.4
* GitHub: https://github.com/bflammers/ANN2
* Source code: https://github.com/cran/ANN2
* Date/Publication: 2020-12-01 10:00:02 UTC
* Number of recursive dependencies: 48

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
* Number of recursive dependencies: 174

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
       27. │                             └─vctrs:::stop_names(...)
       28. │                               └─vctrs:::stop_vctrs(...)
       29. │                                 └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
       30. ├─APackOfTheClones:::name_latest_legend_layer(.)
       31. │ └─plt %>% name_latest_layer(.ApotcLegendLayerName)
       32. └─APackOfTheClones:::name_latest_layer(., .ApotcLegendLayerName)
      
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
* Number of recursive dependencies: 112

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
      
      [ FAIL 2 | WARN 51 | SKIP 0 | PASS 84 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘main_functionality.Rmd’ using rmarkdown
    ```

# aplot

<details>

* Version: 0.2.5
* GitHub: https://github.com/YuLab-SMU/aplot
* Source code: https://github.com/cran/aplot
* Date/Publication: 2025-02-27 03:50:02 UTC
* Number of recursive dependencies: 47

Run `revdepcheck::cloud_details(, "aplot")` for more info

</details>

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.axisAlign:
      function(object, plot, object_name)
    
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.alab:
      function(object, plot, object_name)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# applicable

<details>

* Version: 0.1.1
* GitHub: https://github.com/tidymodels/applicable
* Source code: https://github.com/cran/applicable
* Date/Publication: 2024-04-25 00:00:04 UTC
* Number of recursive dependencies: 114

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

# arena2r

<details>

* Version: 1.0.0
* GitHub: https://github.com/pedroliman/arena2r
* Source code: https://github.com/cran/arena2r
* Date/Publication: 2018-10-19 15:30:03 UTC
* Number of recursive dependencies: 86

Run `revdepcheck::cloud_details(, "arena2r")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(arena2r)
      > 
      > test_check("arena2r")
      [ FAIL 3 | WARN 6 | SKIP 0 | PASS 4 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      ── Failure ('tests_arena2r.R:31:3'): plots_sao_gerados ─────────────────────────
      `plot_conf` has type 'object', not 'list'.
      ── Failure ('tests_arena2r.R:32:3'): plots_sao_gerados ─────────────────────────
      `plot_scat` has type 'object', not 'list'.
      ── Failure ('tests_arena2r.R:33:3'): plots_sao_gerados ─────────────────────────
      `plot_box` has type 'object', not 'list'.
      
      [ FAIL 3 | WARN 6 | SKIP 0 | PASS 4 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘purrr’ ‘shinyBS’ ‘shinydashboard’ ‘shinyjs’
      All declared Imports should be used.
    ```

# ARUtools

<details>

* Version: 0.7.2
* GitHub: https://github.com/ARUtools/ARUtools
* Source code: https://github.com/cran/ARUtools
* Date/Publication: 2025-03-19 19:20:05 UTC
* Number of recursive dependencies: 140

Run `revdepcheck::cloud_details(, "ARUtools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ARUtools-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: calc_selection_weights
    > ### Title: Calculate Selection Weights
    > ### Aliases: calc_selection_weights
    > 
    > ### ** Examples
    > 
    > s <- clean_site_index(example_sites_clean,
    ...
    +   calc_sun()
    Extracting ARU info...
    Extracting Dates and Times...
    Joining by columns `date_time_start` and `date_time_end`
    > 
    > params <- sim_selection_weights()
    Error: Can't find method for generic `&(e1, e2)`:
    - e1: <patchwork>
    - e2: <theme>
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ARUtools)
      > 
      > test_check("ARUtools")
      sh: 1: sox: not found
      Extracting Dates and Times...
      ! Omitted 1 extra, non-json/GPS files
    ...
       13.   │ └─base::force(code)
       14.   └─S7:::Ops.S7_object((p1 + p2)/p3, ggplot2::theme_minimal(base_size = 14))
      
      [ FAIL 1 | WARN 0 | SKIP 5 | PASS 567 ]
      Deleting unused snapshots:
      • 08_selections/sim-selection-weights1.svg
      • 08_selections/sim-selection-weights2.svg
      • 08_selections/sim-selection-weights3.svg
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ARUtools.Rmd’ using rmarkdown
    --- finished re-building ‘ARUtools.Rmd’
    
    --- re-building ‘Misc.Rmd’ using rmarkdown
    --- finished re-building ‘Misc.Rmd’
    
    --- re-building ‘SubSample.Rmd’ using rmarkdown
    
    Quitting from SubSample.Rmd:115-124 [unnamed-chunk-5]
    ...
    --- finished re-building ‘spatial.Rmd’
    
    --- re-building ‘timezones.Rmd’ using rmarkdown
    --- finished re-building ‘timezones.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘SubSample.Rmd’ ‘multisampling.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ASRgenomics

<details>

* Version: 1.1.4
* GitHub: NA
* Source code: https://github.com/cran/ASRgenomics
* Date/Publication: 2024-01-29 21:20:02 UTC
* Number of recursive dependencies: 136

Run `revdepcheck::cloud_details(, "ASRgenomics")` for more info

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
      `expected[2:2]`:                                                      "ggplot"
      ── Failure ('test-qc.R:58:3'): filters work ────────────────────────────────────
      class(M_filter$plot.maf) (`actual`) not equal to c("gg", "ggplot") (`expected`).
      
      `actual`:        "ggplot2::ggplot" "ggplot" "ggplot2::gg" "S7_object" "gg"    
      `expected[2:2]`:                                                      "ggplot"
      
      [ FAIL 5 | WARN 7 | SKIP 0 | PASS 258 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.9Mb
      sub-directories of 1Mb or more:
        data   8.5Mb
    ```

# assignPOP

<details>

* Version: 1.3.0
* GitHub: https://github.com/alexkychen/assignPOP
* Source code: https://github.com/cran/assignPOP
* Date/Publication: 2024-03-13 08:30:02 UTC
* Number of recursive dependencies: 138

Run `revdepcheck::cloud_details(, "assignPOP")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(assignPOP)
      > 
      > test_check("assignPOP")
      
        Correct assignment rates were estimated!!
        A total of 3 assignment tests for 3 pops.
    ...
      ── Failure ('test_accuracy.R:8:3'): Calculate assignment accuracy for Monte-Carlo results ──
      `plot` has type 'object', not 'list'.
      ── Failure ('test_accuracy.R:18:3'): Calculate assignment accuracy for K-fold results ──
      `plot` has type 'object', not 'list'.
      ── Failure ('test_membership.R:5:3'): Plot membership probability ──────────────
      `plot` has type 'object', not 'list'.
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 39 ]
      Error: Test failures
      Execution halted
    ```

# autocogs

<details>

* Version: 0.1.4
* GitHub: https://github.com/schloerke/autocogs
* Source code: https://github.com/cran/autocogs
* Date/Publication: 2021-05-29 17:00:05 UTC
* Number of recursive dependencies: 70

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
* Number of recursive dependencies: 85

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

# autoReg

<details>

* Version: 0.3.3
* GitHub: https://github.com/cardiomoon/autoReg
* Source code: https://github.com/cran/autoReg
* Date/Publication: 2023-11-14 05:53:27 UTC
* Number of recursive dependencies: 216

Run `revdepcheck::cloud_details(, "autoReg")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘autoReg-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: modelPlot
    > ### Title: Draw coefficients/odds ratio/hazard ratio plot
    > ### Aliases: modelPlot
    > 
    > ### ** Examples
    > 
    > fit=lm(mpg~wt*hp+am,data=mtcars)
    > modelPlot(fit,widths=c(1,0,2,3))
    Error: Can't find method for generic `&(e1, e2)`:
    - e1: <patchwork>
    - e2: <theme>
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Automatic_Regression_Modeling.Rmd’ using rmarkdown
    
    Quitting from Automatic_Regression_Modeling.Rmd:141-143 [unnamed-chunk-15]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error:
    ! Can't find method for generic `&(e1, e2)`:
    - e1: <patchwork>
    - e2: <theme>
    ...
    Error: processing vignette 'Getting_started.Rmd' failed with diagnostics:
    Can't find method for generic `&(e1, e2)`:
    - e1: <patchwork>
    - e2: <theme>
    --- failed re-building ‘Getting_started.Rmd’
    
    --- re-building ‘Statiastical_test_in_gaze.Rmd’ using rmarkdown
    --- finished re-building ‘Statiastical_test_in_gaze.Rmd’
    
    --- re-building ‘Survival.Rmd’ using rmarkdown
    ```

# bartMan

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/bartMan
* Date/Publication: 2024-07-24 12:10:02 UTC
* Number of recursive dependencies: 133

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
* Number of recursive dependencies: 69

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

# bayesassurance

<details>

* Version: 0.1.0
* GitHub: https://github.com/jpan928/bayesassurance_rpackage
* Source code: https://github.com/cran/bayesassurance
* Date/Publication: 2022-06-17 10:40:17 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::cloud_details(, "bayesassurance")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(bayesassurance)
      > 
      > test_check("bayesassurance")
      [1] 20 10
      [1] 30 10
      [1] 40 10
    ...
      Error in `out$plot$labels`: object of type 'object' is not subsettable
      Backtrace:
          ▆
       1. └─testthat::expect_identical(out$plot$labels$y, "Power/Assurance") at test-pwr_curves.R:114:3
       2.   └─testthat::quasi_label(enquo(object), label, arg = "object")
       3.     └─rlang::eval_bare(expr, quo_get_env(quo))
      
      [ FAIL 15 | WARN 92 | SKIP 0 | PASS 125 ]
      Error: Test failures
      Execution halted
    ```

# BayesCVI

<details>

* Version: 1.0.1
* GitHub: NA
* Source code: https://github.com/cran/BayesCVI
* Date/Publication: 2024-09-04 15:50:02 UTC
* Number of recursive dependencies: 30

Run `revdepcheck::cloud_details(, "BayesCVI")` for more info

</details>

## Newly broken

*   checking whether package ‘BayesCVI’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘e1071::element’ by ‘ggplot2::element’ when loading ‘BayesCVI’
    See ‘/tmp/workdir/BayesCVI/new/BayesCVI.Rcheck/00install.out’ for details.
    ```

# BayesERtools

<details>

* Version: 0.2.2
* GitHub: NA
* Source code: https://github.com/cran/BayesERtools
* Date/Publication: 2025-06-06 20:20:02 UTC
* Number of recursive dependencies: 215

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
      
      [ FAIL 8 | WARN 3 | SKIP 0 | PASS 168 ]
      Error: Test failures
      Execution halted
    ```

# BayesianReasoning

<details>

* Version: 0.4.2
* GitHub: https://github.com/gorkang/BayesianReasoning
* Source code: https://github.com/cran/BayesianReasoning
* Date/Publication: 2023-11-14 11:33:20 UTC
* Number of recursive dependencies: 105

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
* Number of recursive dependencies: 78

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
* Number of recursive dependencies: 104

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
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        help   6.1Mb
    ```

# bayesplot

<details>

* Version: 1.12.0
* GitHub: https://github.com/stan-dev/bayesplot
* Source code: https://github.com/cran/bayesplot
* Date/Publication: 2025-04-10 10:10:06 UTC
* Number of recursive dependencies: 126

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
      
      [ FAIL 1 | WARN 2 | SKIP 74 | PASS 1032 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.2Mb
      sub-directories of 1Mb or more:
        R     5.0Mb
        doc   3.5Mb
    ```

# BCEA

<details>

* Version: 2.4.7
* GitHub: https://github.com/n8thangreen/BCEA
* Source code: https://github.com/cran/BCEA
* Date/Publication: 2025-01-14 12:30:08 UTC
* Number of recursive dependencies: 123

Run `revdepcheck::cloud_details(, "BCEA")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘BCEA-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.bcea
    > ### Title: Summary Plot of the Health Economic Analysis
    > ### Aliases: plot.bcea
    > ### Keywords: hplot
    > 
    > ### ** Examples
    > 
    ...
     21.                   │ │ └─base (local) tryCatchList(expr, classes, parentenv, handlers)
     22.                   │ │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
     23.                   │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
     24.                   │ └─base::withCallingHandlers(...)
     25.                   └─ggplot2::merge_element(t2[[item]], t1[[item]])
     26.                     ├─S7::S7_dispatch()
     27.                     └─ggplot2 (local) `method(merge_element, list(class_any, class_any))`(new = `<named list>`, old = `<ggpl2::_>`, ...)
     28.                       └─cli::cli_abort("No method for merging {.cls {class(new)[1]}} into {.cls {class(old)[1]}}.")
     29.                         └─rlang::abort(...)
    Execution halted
    ```

# bdsm

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/bdsm
* Date/Publication: 2025-05-02 18:50:02 UTC
* Number of recursive dependencies: 110

Run `revdepcheck::cloud_details(, "bdsm")` for more info

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
      [2] "ggplot"          -                
      [3] "ggplot2::gg"     -                
      [4] "S7_object"       -                
      [5] "gg"              | "gg"        [1]
                            - "ggplot"    [2]
      [6] "ggarrange"       | "ggarrange" [3]
      
      [ FAIL 10 | WARN 0 | SKIP 6 | PASS 69 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘bdsm_vignette.Rnw’ using Sweave
    Error: processing vignette 'bdsm_vignette.Rnw' failed with diagnostics:
    Running 'texi2dvi' on 'bdsm_vignette.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `vmargin.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    ...
    l.5 \usepackage
                   {amsmath}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘bdsm_vignette.Rnw’
    
    SUMMARY: processing the following file failed:
      ‘bdsm_vignette.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# BeeBDC

<details>

* Version: 1.2.1
* GitHub: https://github.com/jbdorey/BeeBDC
* Source code: https://github.com/cran/BeeBDC
* Date/Publication: 2024-11-04 04:10:02 UTC
* Number of recursive dependencies: 217

Run `revdepcheck::cloud_details(, "BeeBDC")` for more info

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
      attributes(testOut)$class (`actual`) not equal to c("gg", "ggplot") (`expected`).
      
      `actual`:        "ggplot2::ggplot" "ggplot" "ggplot2::gg" "S7_object" "gg"    
      `expected[2:2]`:                                                      "ggplot"
      ── Failure ('test-summaryMaps.R:23:3'): summaryMaps expected class ─────────────
      `testMap` has type 'object', not 'list'.
      
      [ FAIL 8 | WARN 5 | SKIP 0 | PASS 242 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 219 marked UTF-8 strings
    ```

# benchr

<details>

* Version: 0.2.5
* GitHub: NA
* Source code: https://github.com/cran/benchr
* Date/Publication: 2020-03-07 06:30:03 UTC
* Number of recursive dependencies: 28

Run `revdepcheck::cloud_details(, "benchr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘tinytest.R’
    Running the tests in ‘tests/tinytest.R’ failed.
    Complete output:
      > if (requireNamespace("tinytest", quietly = TRUE)) {
      +   tinytest::test_package("benchr")
      + }
      
      test-benchmark.R..............    0 tests    0%   10   20   30   40   50   60   70   80   90   100%
      [----|----|----|----|----|----|----|----|----|----|
      **************************************************|
    ...
       call| expect_equal(class(pp2), c("gg", "ggplot"))
       diff| Lengths (2, 5) differ (string compare on first 2)
       diff| 1 string mismatch
      ----- FAILED[attr]: test-plot.R<62--62>
       call| expect_equal(pp2$labels, list(x = "replications", y = "time", 
       call| -->    colour = NULL))
       diff| Attributes: < names for current but not for target >
       diff| Attributes: < Length mismatch: comparison on first 0 components >
      Error: 10 out of 176 tests failed
      Execution halted
    ```

## In both

*   checking C++ specification ... NOTE
    ```
      Specified C++11: please drop specification unless essential
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# biclustermd

<details>

* Version: 0.2.3
* GitHub: https://github.com/jreisner/biclustermd
* Source code: https://github.com/cran/biclustermd
* Date/Publication: 2021-06-17 15:10:06 UTC
* Number of recursive dependencies: 81

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

# biometryassist

<details>

* Version: 1.2.2
* GitHub: https://github.com/biometryhub/biometryassist
* Source code: https://github.com/cran/biometryassist
* Date/Publication: 2025-04-23 15:00:07 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::cloud_details(, "biometryassist")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(biometryassist)
      > 
      > test_check("biometryassist")
      Starting 2 test processes
      [ FAIL 1 | WARN 4 | SKIP 122 | PASS 170 ]
      
    ...
      • resplot/resplot-for-asreml-pt-1.svg
      • resplot/resplot-for-asreml-pt-2.svg
      • resplot/resplot-for-asreml-pt-3.svg
      • resplot/resplot-for-asreml-single.svg
      • resplot/resplot-for-lme4.svg
      • resplot/resplot-for-sommer-mmer.svg
      • resplot/resplot-for-sommer-mmes.svg
      • resplot/resplot-with-smaller-call.svg
      Error: Test failures
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      'asreml', 'ARTool', 'lme4', 'sommer'
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘lme4’
    ```

# BiVariAn

<details>

* Version: 1.0.1
* GitHub: https://github.com/AndresFloresG/BiVariAn
* Source code: https://github.com/cran/BiVariAn
* Date/Publication: 2025-03-05 13:10:02 UTC
* Number of recursive dependencies: 198

Run `revdepcheck::cloud_details(, "BiVariAn")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘BiVariAn-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: auto_bar_categ
    > ### Title: Automatic generation of barplot with percentages
    > ### Aliases: auto_bar_categ
    > 
    > ### ** Examples
    > 
    > data<-data.frame(categ = rep(letters[1:2], 10),
    ...
     1. └─BiVariAn::auto_bar_categ(data = data, groupvar = "categ", lang_labs = "EN")
     2.   └─BiVariAn (local) theme_func()
     3.     ├─... %+replace% ...
     4.     │ └─ggplot2::is_theme(e1)
     5.     │   └─S7::S7_inherits(x, class_theme)
     6.     └─ggprism::theme_prism(...)
     7.       └─parent %+replace% t
     8.         └─cli::cli_abort("{.code %+replace%} requires two theme objects")
     9.           └─rlang::abort(...)
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
       4.     │ └─ggplot2::is_theme(e1)
       5.     │   └─S7::S7_inherits(x, class_theme)
       6.     └─ggprism::theme_prism(...)
       7.       └─parent %+replace% t
       8.         └─cli::cli_abort("{.code %+replace%} requires two theme objects")
       9.           └─rlang::abort(...)
      
      [ FAIL 8 | WARN 2 | SKIP 7 | PASS 100 ]
      Error: Test failures
      Execution halted
    ```

# blockCV

<details>

* Version: 3.1-5
* GitHub: https://github.com/rvalavi/blockCV
* Source code: https://github.com/cran/blockCV
* Date/Publication: 2024-11-01 08:20:02 UTC
* Number of recursive dependencies: 145

Run `revdepcheck::cloud_details(, "blockCV")` for more info

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
      ── Failure ('test-cv_plot.R:20:13'): test that cv_plot function works ──────────
      `plt` has type 'object', not 'list'.
      ── Failure ('test-cv_similarity.R:23:13'): test that cv_similarity function works with cv_spatil ──
      `plt` has type 'object', not 'list'.
      ── Failure ('test-cv_similarity.R:39:13'): test that cv_similarity function works with cv_buffer ──
      `plt` has type 'object', not 'list'.
      
      [ FAIL 3 | WARN 0 | SKIP 1 | PASS 316 ]
      Error: Test failures
      Execution halted
    ```

# boxly

<details>

* Version: 0.1.1
* GitHub: https://github.com/Merck/boxly
* Source code: https://github.com/cran/boxly
* Date/Publication: 2023-10-24 02:40:02 UTC
* Number of recursive dependencies: 87

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
* Number of recursive dependencies: 100

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
* Number of recursive dependencies: 158

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
     20. │                   └─base::lapply(...)
     21. │                     └─ggplot2 (local) FUN(X[[i]], ...)
     22. │                       ├─rlang::inject(self$draw_panel(data, panel_params, coord, !!!params))
     23. │                       └─self$draw_panel(...)
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
    --- re-building ‘calc_pairwise.Rmd’ using rmarkdown
    --- finished re-building ‘calc_pairwise.Rmd’
    
    --- re-building ‘integrating.Rmd’ using rmarkdown
    ```

# calibmsm

<details>

* Version: 1.1.2
* GitHub: NA
* Source code: https://github.com/cran/calibmsm
* Date/Publication: 2025-06-06 11:10:02 UTC
* Number of recursive dependencies: 171

Run `revdepcheck::cloud_details(, "calibmsm")` for more info

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
      [2] "ggplot"          -                
      [3] "ggplot2::gg"     -                
      [4] "S7_object"       -                
      [5] "gg"              | "gg"        [1]
                            - "ggplot"    [2]
      [6] "ggarrange"       | "ggarrange" [3]
      
      [ FAIL 5 | WARN 0 | SKIP 15 | PASS 210 ]
      Error: Test failures
      Execution halted
    ```

# calmr

<details>

* Version: 0.7.0
* GitHub: https://github.com/victor-navarro/calmr
* Source code: https://github.com/cran/calmr
* Date/Publication: 2025-05-11 21:30:02 UTC
* Number of recursive dependencies: 105

Run `revdepcheck::cloud_details(, "calmr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(calmr)
      
      Attaching package: 'calmr'
      
      The following object is masked from 'package:stats':
    ...
      ── Failure ('test-RSA.R:64:3'): plotting RSA with a test works ─────────────────
      `plt` does not have names.
      ── Failure ('test-graphs.R:15:3'): calmr_model_graph works ─────────────────────
      `g` does not have names.
      ── Failure ('test-graphs.R:25:3'): calmr_model_graph takes a trial ─────────────
      `g` does not have names.
      
      [ FAIL 4 | WARN 5 | SKIP 0 | PASS 246 ]
      Error: Test failures
      Execution halted
    ```

# canvasXpress

<details>

* Version: 1.56.1
* GitHub: https://github.com/neuhausi/canvasXpress
* Source code: https://github.com/cran/canvasXpress
* Date/Publication: 2025-04-08 19:30:02 UTC
* Number of recursive dependencies: 146

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
       2. │ └─base::withCallingHandlers(...)
       3. └─canvasXpress::ggplot.as.list(gplot)
       4.   └─canvasXpress:::gg_cxplot(o, "canvas", ...)
       5.     └─canvasXpress:::gg_theme(o)
       6.       ├─base::as.character(e[[a]])
       7.       └─base::as.character.default(e[[a]])
      
      [ FAIL 9 | WARN 1 | SKIP 479 | PASS 58 ]
      Error: Test failures
      Execution halted
    ```

# carbonr

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/carbonr
* Date/Publication: 2024-10-16 18:10:09 UTC
* Number of recursive dependencies: 99

Run `revdepcheck::cloud_details(, "carbonr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(carbonr)
      > 
      > test_check("carbonr")
      [ FAIL 3 | WARN 7 | SKIP 0 | PASS 117 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      ── Failure ('test-output_display.R:15:3'): output_display generates gg/ggplot object ──
      class(output1) not equal to c("gg", "ggplot").
      Lengths differ: 5 is not 2
      ── Failure ('test-output_display.R:16:3'): output_display generates gg/ggplot object ──
      class(output2) not equal to c("gg", "ggplot").
      Lengths differ: 5 is not 2
      
      [ FAIL 3 | WARN 7 | SKIP 0 | PASS 117 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘sp’ ‘tibble’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 710 marked UTF-8 strings
    ```

# cartograflow

<details>

* Version: 1.0.5
* GitHub: https://github.com/fbahoken/cartogRaflow
* Source code: https://github.com/cran/cartograflow
* Date/Publication: 2023-10-17 22:40:21 UTC
* Number of recursive dependencies: 98

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

# cartographr

<details>

* Version: 0.2.2
* GitHub: https://github.com/da-wi/cartographr
* Source code: https://github.com/cran/cartographr
* Date/Publication: 2024-06-28 14:50:09 UTC
* Number of recursive dependencies: 95

Run `revdepcheck::cloud_details(, "cartographr")` for more info

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
      > test_check("cartographr")
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 106 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-utils.R:13:3'): add_attribution returns correct ggplot2 labs object ──
      `result` inherits from 'ggplot2::labels'/'gg'/'S7_object' not 'labels'.
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 106 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        data   3.5Mb
    ```

# cats

<details>

* Version: 1.0.2
* GitHub: NA
* Source code: https://github.com/cran/cats
* Date/Publication: 2022-03-11 10:20:07 UTC
* Number of recursive dependencies: 80

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

# causact

<details>

* Version: 0.5.7
* GitHub: https://github.com/flyaflya/causact
* Source code: https://github.com/cran/causact
* Date/Publication: 2025-01-15 21:20:01 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "causact")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(causact)
      WARNING: The 'r-causact' Conda environment does not exist. To use the 'dag_numpyro()' function, you need to set up the 'r-causact' environment. Run install_causact_deps() when ready to set up the 'r-causact' environment.
      
      Attaching package: 'causact'
      
      The following objects are masked from 'package:stats':
    ...
      WARNING: The 'r-causact' Conda environment does not exist. To use the 'dag_numpyro()' function, you need to set up the 'r-causact' environment. Run install_causact_deps() when ready to set up the 'r-causact' environment.
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 60 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-dag_plot.R:4:3'): dag plot creates graph ─────────────────────
      `plotGr` has type 'object', not 'list'.
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 60 ]
      Error: Test failures
      Execution halted
    ```

# CausalImpact

<details>

* Version: 1.3.0
* GitHub: NA
* Source code: https://github.com/cran/CausalImpact
* Date/Publication: 2022-11-09 08:40:40 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::cloud_details(, "CausalImpact")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘CausalImpact_import_test.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # Copyright 2014-2022 Google Inc. All rights reserved.
      > #
      > # Licensed under the Apache License, Version 2.0 (the "License");
      > # you may not use this file except in compliance with the License.
      > # You may obtain a copy of the License at
      > #
    ...
      Lengths differ: 5 is not 2
      ── Failure ('test-impact-plot.R:147:3'): CreateImpactPlot ──────────────────────
      !isTRUE(all.equal(q1, q2, check.environment = FALSE)) is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      
      [ FAIL 3 | WARN 4 | SKIP 0 | PASS 738 ]
      Error: Test failures
      Execution halted
    ```

# centerline

<details>

* Version: 0.2.2
* GitHub: https://github.com/atsyplenkov/centerline
* Source code: https://github.com/cran/centerline
* Date/Publication: 2025-03-16 04:40:02 UTC
* Number of recursive dependencies: 108

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
     24. │                           ├─rlang::inject(self$draw_panel(data, panel_params, coord, !!!params))
     25. │                           └─self$draw_panel(...)
     26. │                             └─geomtextpath (local) draw_panel(...)
     27. │                               └─geomtextpath:::sf_textgrob(...)
     28. └─base::.handleSimpleError(...)
     29.   └─rlang (local) h(simpleError(msg, call))
     30.     └─handlers[[1L]](cnd)
     31.       └─cli::cli_abort(...)
     32.         └─rlang::abort(...)
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
* Number of recursive dependencies: 99

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
      4. ├─purrr::map_if(...)
      5. │ └─purrr:::where_if(.x, .p)
      6. │   └─rlang::is_logical(.p)
      7. ├─stringr::str_detect(., "@")
      8. │ └─stringr:::check_lengths(string, pattern)
      9. │   └─vctrs::vec_size_common(...)
     10. └─vctrs:::stop_scalar_type(`<fn>`(`<ggplt2::>`), "string", `<env>`)
     11.   └─vctrs:::stop_vctrs(...)
     12.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

# cheem

<details>

* Version: 0.4.0.0
* GitHub: https://github.com/nspyrison/cheem
* Source code: https://github.com/cran/cheem
* Date/Publication: 2023-11-08 21:30:02 UTC
* Number of recursive dependencies: 148

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
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 8 ]
      Error: Test failures
      Execution halted
    ```

# ChemoSpec

<details>

* Version: 6.1.11
* GitHub: https://github.com/bryanhanson/ChemoSpec
* Source code: https://github.com/cran/ChemoSpec
* Date/Publication: 2025-04-15 15:00:09 UTC
* Number of recursive dependencies: 160

Run `revdepcheck::cloud_details(, "ChemoSpec")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ChemoSpec.Rmd’ using rmarkdown
    
    Quitting from ChemoSpec.Rmd:464-468 [load1]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error:
    ! Can't find method for generic `&(e1, e2)`:
    - e1: <patchwork>
    - e2: <ggplot2::labels>
    ...
    Can't find method for generic `&(e1, e2)`:
    - e1: <patchwork>
    - e2: <ggplot2::labels>
    --- failed re-building ‘ChemoSpec.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ChemoSpec.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ChemoSpecUtils

<details>

* Version: 1.0.5
* GitHub: https://github.com/bryanhanson/ChemoSpecUtils
* Source code: https://github.com/cran/ChemoSpecUtils
* Date/Publication: 2025-04-12 15:30:04 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "ChemoSpecUtils")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ChemoSpecUtils-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: GraphicsOptions
    > ### Title: Graphic Output Options in ChemoSpec and ChemoSpec2D
    > ### Aliases: GraphicsOptions
    > ### Keywords: utilities
    > 
    > ### ** Examples
    > 
    ...
    ℹ Adding new coordinate system, which will replace the existing one.
    Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
    ℹ Please use tidy evaluation idioms with `aes()`.
    ℹ See also `vignette("ggplot2-in-packages")` for more information.
    ℹ The deprecated feature was likely used in the ChemoSpec package.
      Please report the issue at <https://github.com/bryanhanson/ChemoSpec/issues>.
    Error: Can't find method for generic `&(e1, e2)`:
    - e1: <patchwork>
    - e2: <theme>
    Execution halted
    ```

# chillR

<details>

* Version: 0.76
* GitHub: NA
* Source code: https://github.com/cran/chillR
* Date/Publication: 2024-11-14 09:40:02 UTC
* Number of recursive dependencies: 133

Run `revdepcheck::cloud_details(, "chillR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘chillR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_scenarios
    > ### Title: Plot historic and future scenarios for climate-related metrics
    > ###   (ggplot2 version)
    > ### Aliases: plot_scenarios
    > 
    > ### ** Examples
    > 
    ...
    > # Plot the climate scenarios
    > 
    > plot_scenarios(climate_scenario_list, metric = 'Chill_Portions',
    +                add_historic = TRUE, size = 2, shape = 3, color = 'blue',
    +                outlier_shape = 12, historic_color = 'skyblue',
    +                group_by = c("Year", "Scenario"))
    Error: Can't find method for generic `&(e1, e2)`:
    - e1: <patchwork>
    - e2: <theme>
    Execution halted
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

# circumplex

<details>

* Version: 1.0.0
* GitHub: https://github.com/jmgirard/circumplex
* Source code: https://github.com/cran/circumplex
* Date/Publication: 2024-10-28 04:30:02 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::cloud_details(, "circumplex")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(circumplex)
      > 
      > test_check("circumplex")
      [ FAIL 4 | WARN 8 | SKIP 20 | PASS 97 ]
      
      ══ Skipped tests (20) ══════════════════════════════════════════════════════════
    ...
      [ FAIL 4 | WARN 8 | SKIP 20 | PASS 97 ]
      Deleting unused snapshots:
      • ssm_plot/many-circle-repel.svg
      • ssm_plot/many-curve-plots.svg
      • ssm_plot/measure-contrast-circle-ssm.svg
      • ssm_plot/measure-contrast-curve-ssm.svg
      • ssm_plot/single-group-mean-ssm-no-palette.svg
      • ssm_plot/single-group-mean-ssm-with-labels.svg
      Error: Test failures
      Execution halted
    ```

# classmap

<details>

* Version: 1.2.4
* GitHub: NA
* Source code: https://github.com/cran/classmap
* Date/Publication: 2025-05-13 22:00:02 UTC
* Number of recursive dependencies: 132

Run `revdepcheck::cloud_details(, "classmap")` for more info

</details>

## Newly broken

*   checking whether package ‘classmap’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::element’ by ‘e1071::element’ when loading ‘classmap’
    See ‘/tmp/workdir/classmap/new/classmap.Rcheck/00install.out’ for details.
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 9 marked UTF-8 strings
    ```

# clifro

<details>

* Version: 3.2-5
* GitHub: https://github.com/ropensci/clifro
* Source code: https://github.com/cran/clifro
* Date/Publication: 2021-05-24 05:50:02 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "clifro")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Complete output:
      > library(testthat)
      > library(clifro)
      > 
      > test_check("clifro")
      [ FAIL 1 | WARN 1 | SKIP 4 | PASS 10 ]
      
    ...
      • On CRAN (4): 'test-cf_find_station.R:4:3', 'test-cf_last_query.R:4:3',
        'test-cf_query.R:4:3', 'test-cf_station.R:4:3'
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-windrose.R:15:3'): windrose ──────────────────────────────────
      tt$labels inherits from `'ggplot2::labels'/'gg'/'S7_object'` not `'character'`.
      
      [ FAIL 1 | WARN 1 | SKIP 4 | PASS 10 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) cf_save_kml.Rd:68: Lost braces
        68 | \code{\link{cf_find_station}} and code{vignette("choose-station")} for
           |                                       ^
    ```

# climwin

<details>

* Version: 1.2.31
* GitHub: https://github.com/LiamDBailey/climwin
* Source code: https://github.com/cran/climwin
* Date/Publication: 2025-06-08 16:40:02 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "climwin")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(climwin)
      > 
      > test_check("climwin")
      
        |                                                                            
        |                                                                      |   0%
    ...
      `expected`: TRUE 
      ── Failure ('testplotwin.R:15:3'): plotwin produces a graph ────────────────────
      attr(test, "class")[1] == "gg" is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      
      [ FAIL 7 | WARN 761 | SKIP 3 | PASS 790 ]
      Error: Test failures
      Execution halted
    ```

# clinDataReview

<details>

* Version: 1.6.2
* GitHub: https://github.com/openanalytics/clinDataReview
* Source code: https://github.com/cran/clinDataReview
* Date/Publication: 2025-04-11 22:10:02 UTC
* Number of recursive dependencies: 117

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
        adding: report_dependencies172d6f8f416/ (stored 0%)
        adding: report_dependencies172d6f8f416/file172d481ba5e0.html (deflated 8%)
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
* Number of recursive dependencies: 107

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
* Number of recursive dependencies: 78

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
* Number of recursive dependencies: 106

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

# cmcR

<details>

* Version: 0.1.11
* GitHub: NA
* Source code: https://github.com/cran/cmcR
* Date/Publication: 2022-12-10 14:00:02 UTC
* Number of recursive dependencies: 120

Run `revdepcheck::cloud_details(, "cmcR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(cmcR)
      > 
      > test_check("cmcR",reporter = SummaryReporter)
        adding: bindata/ (stored 0%)
        adding: bindata/data.bin (deflated 58%)
        adding: main.xml (deflated 62%)
    ...
      ── 2. Failure ('test-diagnosticTools.R:104:3'): diagnosticTools functions work a
      all(unlist(purrr::map(cmcPlt_list, ~class(.) == c("gg", "ggplot")))) is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      
      ══ DONE ════════════════════════════════════════════════════════════════════════
      I believe in you!
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        data   2.5Mb
        doc    2.3Mb
    ```

# cnmap

<details>

* Version: 0.1.0
* GitHub: https://github.com/PanfengZhang/cnmap
* Source code: https://github.com/cran/cnmap
* Date/Publication: 2024-04-02 12:42:06 UTC
* Number of recursive dependencies: 58

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

# CNVreg

<details>

* Version: 1.0
* GitHub: NA
* Source code: https://github.com/cran/CNVreg
* Date/Publication: 2025-03-10 16:50:21 UTC
* Number of recursive dependencies: 118

Run `revdepcheck::cloud_details(, "CNVreg")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘CNVReg_vig.Rmd’ using rmarkdown
    
    Quitting from CNVReg_vig.Rmd:322-378 [unnamed-chunk-17]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error:
    ! Can't find method for generic `&(e1, e2)`:
    - e1: <patchwork>
    - e2: <theme>
    ...
    Can't find method for generic `&(e1, e2)`:
    - e1: <patchwork>
    - e2: <theme>
    --- failed re-building ‘CNVReg_vig.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘CNVReg_vig.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# cocktailApp

<details>

* Version: 0.2.3
* GitHub: https://github.com/shabbychef/cocktailApp
* Source code: https://github.com/cran/cocktailApp
* Date/Publication: 2023-07-19 13:40:09 UTC
* Number of recursive dependencies: 93

Run `revdepcheck::cloud_details(, "cocktailApp")` for more info

</details>

## Newly broken

*   checking whether package ‘cocktailApp’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/cocktailApp/new/cocktailApp.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 14729 marked UTF-8 strings
    ```

## Installation

### Devel

```
* installing *source* package ‘cocktailApp’ ...
** package ‘cocktailApp’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'ggtern', details:
  call: NULL
  error: <ggplot2::element_line> object properties are invalid:
- @lineend must be <character> or <NULL>, not S3<arrow>
Execution halted
ERROR: lazy loading failed for package ‘cocktailApp’
* removing ‘/tmp/workdir/cocktailApp/new/cocktailApp.Rcheck/cocktailApp’


```
### CRAN

```
* installing *source* package ‘cocktailApp’ ...
** package ‘cocktailApp’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (cocktailApp)


```
# coda.plot

<details>

* Version: 0.1.9
* GitHub: NA
* Source code: https://github.com/cran/coda.plot
* Date/Publication: 2025-04-20 21:30:02 UTC
* Number of recursive dependencies: 41

Run `revdepcheck::cloud_details(, "coda.plot")` for more info

</details>

## Newly broken

*   checking whether package ‘coda.plot’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/coda.plot/new/coda.plot.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘coda.plot’ ...
** package ‘coda.plot’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'ggtern', details:
  call: NULL
  error: <ggplot2::element_line> object properties are invalid:
- @lineend must be <character> or <NULL>, not S3<arrow>
Execution halted
ERROR: lazy loading failed for package ‘coda.plot’
* removing ‘/tmp/workdir/coda.plot/new/coda.plot.Rcheck/coda.plot’


```
### CRAN

```
* installing *source* package ‘coda.plot’ ...
** package ‘coda.plot’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (coda.plot)


```
# CohortPlat

<details>

* Version: 1.0.5
* GitHub: NA
* Source code: https://github.com/cran/CohortPlat
* Date/Publication: 2022-02-14 09:30:02 UTC
* Number of recursive dependencies: 79

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
* Number of recursive dependencies: 87

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

# conquestr

<details>

* Version: 1.5.1
* GitHub: NA
* Source code: https://github.com/cran/conquestr
* Date/Publication: 2025-05-18 07:50:02 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::cloud_details(, "conquestr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(conquestr)
      > 
      > test_check("conquestr")
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 25 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-ConQuestRout.R:10:3'): Rout file is of correct type ──────────
      `myRoutPlot` has type 'object', not 'list'.
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 25 ]
      Error: Test failures
      Execution halted
    ```

# constructive

<details>

* Version: 1.1.0
* GitHub: https://github.com/cynkra/constructive
* Source code: https://github.com/cran/constructive
* Date/Publication: 2025-01-10 14:10:01 UTC
* Number of recursive dependencies: 111

Run `revdepcheck::cloud_details(, "constructive")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(constructive)
      > 
      > test_check("constructive")
      [ FAIL 6 | WARN 2 | SKIP 113 | PASS 24 ]
      
      ══ Skipped tests (113) ═════════════════════════════════════════════════════════
    ...
       13. │       └─constructive:::strip_theme(x)
       14. │         └─constructive:::`$<-`(`*tmp*`, size, value = NULL)
       15. │           └─constructive:::`[[<-`(...)
       16. └─rlang (local) `<fn>`(`<ntSbsttE>`)
       17.   └─handlers[[1L]](cnd)
       18.     └─rlang::abort(...)
      
      [ FAIL 6 | WARN 2 | SKIP 113 | PASS 24 ]
      Error: Test failures
      Execution halted
    ```

# contsurvplot

<details>

* Version: 0.2.1
* GitHub: https://github.com/RobinDenz1/contsurvplot
* Source code: https://github.com/cran/contsurvplot
* Date/Publication: 2023-08-15 08:00:03 UTC
* Number of recursive dependencies: 156

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
* Number of recursive dependencies: 88

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
* Number of recursive dependencies: 113

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
* Number of recursive dependencies: 126

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
* Number of recursive dependencies: 77

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
* Number of recursive dependencies: 87

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
       1. ├─testthat::expect_equal(p$labels[[1]], "x") at testing.R:45:3
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. ├─p$labels[[1]]
       5. ├─S7:::`[[.S7_object`(p$labels, 1)
       6. └─base::NextMethod()
      
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

# countries

<details>

* Version: 1.2.1
* GitHub: https://github.com/fbellelli/countries
* Source code: https://github.com/cran/countries
* Date/Publication: 2025-02-22 14:10:02 UTC
* Number of recursive dependencies: 79

Run `revdepcheck::cloud_details(, "countries")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(countries)
      > 
      > test_check("countries")
      
      In total 1 unique country names were provided
      1/1 have been matched with EXACT matching
    ...
      `expected[2:2]`:                                                      "ggplot"
      ── Failure ('test_function_output.R:319:3'): output from quick_map() are as expected ──
      class(quick_map(example, "test", col_na = "black")) (`actual`) not equal to c("gg", "ggplot") (`expected`).
      
      `actual`:        "ggplot2::ggplot" "ggplot" "ggplot2::gg" "S7_object" "gg"    
      `expected[2:2]`:                                                      "ggplot"
      
      [ FAIL 9 | WARN 0 | SKIP 2 | PASS 137 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 9044 marked UTF-8 strings
    ```

# covidcast

<details>

* Version: 0.5.2
* GitHub: https://github.com/cmu-delphi/covidcast
* Source code: https://github.com/cran/covidcast
* Date/Publication: 2023-07-12 23:40:06 UTC
* Number of recursive dependencies: 89

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

# cowplot

<details>

* Version: 1.1.3
* GitHub: https://github.com/wilkelab/cowplot
* Source code: https://github.com/cran/cowplot
* Date/Publication: 2024-01-22 23:22:51 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "cowplot")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(cowplot)
      > 
      > test_check("cowplot")
      [ FAIL 1 | WARN 4 | SKIP 7 | PASS 54 ]
      
      ══ Skipped tests (7) ═══════════════════════════════════════════════════════════
    ...
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test_ggdraw.R:7:3'): basic ggdraw usage ───────────────────────────
      methods::is(g) not equal to "gg".
      1/1 mismatches
      x[1]: "ggplot2::ggplot"
      y[1]: "gg"
      
      [ FAIL 1 | WARN 4 | SKIP 7 | PASS 54 ]
      Error: Test failures
      Execution halted
    ```

# Coxmos

<details>

* Version: 1.1.3
* GitHub: https://github.com/BiostatOmics/Coxmos
* Source code: https://github.com/cran/Coxmos
* Date/Publication: 2025-06-02 18:02:02 UTC
* Number of recursive dependencies: 201

Run `revdepcheck::cloud_details(, "Coxmos")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Coxmos-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_evaluation
    > ### Title: plot_evaluation
    > ### Aliases: plot_evaluation
    > 
    > ### ** Examples
    > 
    > data("X_proteomic")
    ...
    > Y_test <- Y_proteomic[-index_train,]
    > coxEN.model <- coxEN(X_train, Y_train, x.center = TRUE, x.scale = TRUE)
    Warning: from glmnet C++ code (error code -10013); Number of nonzero coefficients along the path exceeds pmax=7 at 13th lambda value; solutions for larger lambdas returned
    > eval_results <- eval_Coxmos_models(lst_models = list("coxEN" = coxEN.model), X_test = X_test,
    + Y_test = Y_test)
    > plot_eval_results <- plot_evaluation(eval_results)
    Error: Can't find method for generic `&(e1, e2)`:
    - e1: <patchwork>
    - e2: <theme>
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.4Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        data   2.5Mb
        doc    2.8Mb
    ```

# cry

<details>

* Version: 0.5.1
* GitHub: NA
* Source code: https://github.com/cran/cry
* Date/Publication: 2022-10-10 08:00:05 UTC
* Number of recursive dependencies: 50

Run `revdepcheck::cloud_details(, "cry")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘cry-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_SHELX
    > ### Title: Plot SHELXC log files
    > ### Aliases: plot_SHELX
    > 
    > ### ** Examples
    > 
    > datadir <- system.file("extdata",package="cry")
    > ## SHELXC
    > shelxc_log <- file.path(datadir,"shelxc.log")
    > shelxc <- read_SHELX_log(shelxc_log)
    > plot_shelxc <- plot_SHELX(filename = shelxc, var = shelxc$I_sig,
    + type = "shelxc", title_chart = "SHELXC")
    Error in rep(yes, length.out = len) : 
      attempt to replicate an object of type 'object'
    Calls: plot_SHELX -> ifelse -> ifelse -> ifelse
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘MTZ.Rmd’ using rmarkdown
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) calculate_cell_volume.Rd:16: Lost braces; missing escapes or markup?
        16 | V A real number. The volume (in angstroms^3 or angstroms^{-3})
           |                                                          ^
    ```

# ctrialsgov

<details>

* Version: 0.2.5
* GitHub: NA
* Source code: https://github.com/cran/ctrialsgov
* Date/Publication: 2021-10-18 16:00:02 UTC
* Number of recursive dependencies: 96

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
* Number of recursive dependencies: 130

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

# cutpointr

<details>

* Version: 1.2.0
* GitHub: https://github.com/thie1e/cutpointr
* Source code: https://github.com/cran/cutpointr
* Date/Publication: 2024-12-10 22:50:02 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::cloud_details(, "cutpointr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(cutpointr)
      > 
      > test_check("cutpointr")
      [ FAIL 105 | WARN 14 | SKIP 0 | PASS 337 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      class(tempplot5) not identical to c("gg", "ggplot").
      Lengths differ: 5 is not 2
      Backtrace:
          ▆
       1. └─cutpointr (local) test_ggplot_functions(cp, do_plot_metric = FALSE) at test-cutpointr.R:1156:5
       2.   └─testthat::expect_identical(class(tempplot5), c("gg", "ggplot")) at test-cutpointr.R:22:5
      
      [ FAIL 105 | WARN 14 | SKIP 0 | PASS 337 ]
      Error: Test failures
      Execution halted
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
      names for target but not for current
      ── Failure ('test_plotting_functions.R:359:3'): plot_confusion_matrix() with sum tiles, class order, and intensity_by percentage ──
      `labels` not equal to list(...).
      Attributes: < names for target but not for current >
      Attributes: < Length mismatch: comparison on first 0 components >
      Length mismatch: comparison on first 4 components
      
      [ FAIL 15 | WARN 0 | SKIP 71 | PASS 3602 ]
      Error: Test failures
      Execution halted
    ```

# cylcop

<details>

* Version: 0.2.0
* GitHub: https://github.com/r-lib/devtools
* Source code: https://github.com/cran/cylcop
* Date/Publication: 2022-10-29 22:00:21 UTC
* Number of recursive dependencies: 95

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
     23. │                   └─l$compute_geom_2(d, theme = plot@theme)
     24. │                     └─ggplot2 (local) compute_geom_2(..., self = self)
     25. │                       └─self$geom$use_defaults(...)
     26. │                         └─ggplot2 (local) use_defaults(..., self = self)
     27. │                           └─ggplot2:::check_aesthetics(new_params, nrow(data))
     28. │                             └─vctrs::list_sizes(x)
     29. └─vctrs:::stop_scalar_type(`<fn>`(`<expression>`), "x$label", `<env>`)
     30.   └─vctrs:::stop_vctrs(...)
     31.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
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
      [ FAIL 5 | WARN 6 | SKIP 15 | PASS 755 ]
      
      ══ Skipped tests (15) ══════════════════════════════════════════════════════════
      • On CRAN (2): 'test_D2MCS.R:129:3', 'test_D2MCS.R:213:3'
      • {ranger} is not installed (13): 'test_D2MCS.R:411:3',
    ...
      ── Failure ('test_clustering.strategies.TypeBasedStrategy.R:226:3'): TypeBasedStrategy works ──
      c("gg", "ggplot") not equal to class(strategyNoReal$plot()).
      Lengths differ: 2 is not 5
      ── Failure ('test_clustering.strategies.TypeBasedStrategy.R:234:3'): TypeBasedStrategy works ──
      c("gg", "ggplot") not equal to class(strategyNoBinary$plot()).
      Lengths differ: 2 is not 5
      
      [ FAIL 5 | WARN 6 | SKIP 15 | PASS 755 ]
      Error: Test failures
      Execution halted
    ```

# dabestr

<details>

* Version: 2025.3.14
* GitHub: https://github.com/ACCLAB/dabestr
* Source code: https://github.com/cran/dabestr
* Date/Publication: 2025-02-26 12:50:02 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "dabestr")` for more info

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
      • 001_plotter/two-groups-unpaired-mean-diff-colour-float-true.svg
      • 001_plotter/two-groups-unpaired-mean-diff-float-false.svg
      • 001_plotter/two-groups-unpaired-mean-diff-float-true.svg
      • 002_forest_plot/deltadelta-forest-plot-mean-diff.svg
      • 002_forest_plot/deltadelta-forest-plot-with-plot-kwargs.svg
      • 002_forest_plot/minimeta-forest-plot-mean-diff.svg
      • 002_forest_plot/minimeta-forest-plot-median-diff.svg
      • 002_forest_plot/minimeta-forest-plot-with-plot-kwargs.svg
      Error: Test failures
      Execution halted
    ```

# daiquiri

<details>

* Version: 1.1.1
* GitHub: https://github.com/ropensci/daiquiri
* Source code: https://github.com/cran/daiquiri
* Date/Publication: 2023-07-18 16:50:09 UTC
* Number of recursive dependencies: 104

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
* Number of recursive dependencies: 145

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

# daltoolbox

<details>

* Version: 1.2.707
* GitHub: https://github.com/cefet-rj-dal/daltoolbox
* Source code: https://github.com/cran/daltoolbox
* Date/Publication: 2025-05-13 06:20:13 UTC
* Number of recursive dependencies: 99

Run `revdepcheck::cloud_details(, "daltoolbox")` for more info

</details>

## Newly broken

*   checking whether package ‘daltoolbox’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘e1071::element’ by ‘ggplot2::element’ when loading ‘daltoolbox’
    See ‘/tmp/workdir/daltoolbox/new/daltoolbox.Rcheck/00install.out’ for details.
    ```

# dams

<details>

* Version: 0.3.0
* GitHub: https://github.com/jsta/dams
* Source code: https://github.com/cran/dams
* Date/Publication: 2020-05-20 16:00:03 UTC
* Number of recursive dependencies: 87

Run `revdepcheck::cloud_details(, "dams")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘dams.Rmd’ using rmarkdown
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 5 marked UTF-8 strings
    ```

# Deducer

<details>

* Version: 0.9-0
* GitHub: NA
* Source code: https://github.com/cran/Deducer
* Date/Publication: 2025-05-07 13:50:06 UTC
* Number of recursive dependencies: 114

Run `revdepcheck::cloud_details(, "Deducer")` for more info

</details>

## Newly broken

*   checking whether package ‘Deducer’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘e1071::element’ by ‘ggplot2::element’ when loading ‘Deducer’
    See ‘/tmp/workdir/Deducer/new/Deducer.Rcheck/00install.out’ for details.
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

*   checking whether package ‘deeptime’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/deeptime/new/deeptime.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘deeptime’ ...
** package ‘deeptime’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
...
** testing if installed package can be loaded from temporary location
Error: package or namespace load failed for ‘deeptime’:
 .onLoad failed in loadNamespace() for 'deeptime', details:
  call: NULL
  error: <ggplot2::element_text> object properties are invalid:
- @hjust must be <NULL>, <integer>, or <double>, not <logical>
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/tmp/workdir/deeptime/new/deeptime.Rcheck/deeptime’


```
### CRAN

```
* installing *source* package ‘deeptime’ ...
** package ‘deeptime’ successfully unpacked and MD5 sums checked
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
* DONE (deeptime)


```
# dendextend

<details>

* Version: 1.19.0
* GitHub: https://github.com/talgalili/dendextend
* Source code: https://github.com/cran/dendextend
* Date/Publication: 2024-11-15 10:40:06 UTC
* Number of recursive dependencies: 128

Run `revdepcheck::cloud_details(, "dendextend")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > 
      > library(dendextend)
      
      ---------------------
      Welcome to dendextend version 1.19.0
      Type citation('dendextend') for how to cite the package.
    ...
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 872 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-ggdend.R:168:4'): ggplot doesn't have warnings for dendrograms ──
      names(ggplot_build(g)) not identical to c("data", "layout", "plot").
      target is NULL, current is character
      
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 872 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      'ggdendro', 'dendroextras', 'Hmisc', 'WGCNA', 'moduleColor',
      'distory', 'phangorn', 'zoo'
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.9Mb
      sub-directories of 1Mb or more:
        doc   5.8Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘WGCNA’, ‘dendroextras’, ‘moduleColor’, ‘distory’, ‘phangorn’, ‘ggdendro’, ‘zoo’
    ```

# densityratio

<details>

* Version: 0.2.0
* GitHub: https://github.com/thomvolker/densityratio
* Source code: https://github.com/cran/densityratio
* Date/Publication: 2025-05-19 13:30:05 UTC
* Number of recursive dependencies: 64

Run `revdepcheck::cloud_details(, "densityratio")` for more info

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
      ── Failure ('test-ulsif.R:78:3'): multidimensional ULSIF estimation, prediction works ──
      plot_univariate(...) has type 'object', not 'list'.
      ── Failure ('test-ulsif.R:83:3'): multidimensional ULSIF estimation, prediction works ──
      plot_univariate(...) has type 'object', not 'list'.
      ── Failure ('test-lhss.R:29:3'): multidimensional lhss estimation, prediction and plotting works ──
      suppressWarnings(plot(dr)) has type 'object', not 'list'.
      
      [ FAIL 10 | WARN 0 | SKIP 0 | PASS 270 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.6Mb
      sub-directories of 1Mb or more:
        data   1.5Mb
        libs   8.9Mb
    ```

# diceR

<details>

* Version: 3.0.0
* GitHub: https://github.com/AlineTalhouk/diceR
* Source code: https://github.com/cran/diceR
* Date/Publication: 2025-02-05 09:40:01 UTC
* Number of recursive dependencies: 151

Run `revdepcheck::cloud_details(, "diceR")` for more info

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
      ── Failure ('test-graphs.R:18:3'): graph_cdf object can have added/modified ggplot layers ──
      isTRUE(all.equal(p1, p2)) is not FALSE
      
      `actual`:   TRUE 
      `expected`: FALSE
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 113 ]
      Error: Test failures
      Execution halted
    ```

# DImodelsVis

<details>

* Version: 1.0.1
* GitHub: NA
* Source code: https://github.com/cran/DImodelsVis
* Date/Publication: 2024-02-26 14:10:15 UTC
* Number of recursive dependencies: 112

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
     14.           │ └─base::withCallingHandlers(...)
     15.           └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     16.             └─l$compute_position(d, layout)
     17.               └─ggplot2 (local) compute_position(..., self = self)
     18.                 └─self$position$use_defaults(data, self$aes_params)
     19.                   └─ggplot2 (local) use_defaults(..., self = self)
     20.                     └─ggplot2:::check_aesthetics(new, nrow(data))
     21.                       └─cli::cli_abort(...)
     22.                         └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘DImodelsVis-with-complex-models.Rmd’ using rmarkdown
    ```

# directlabels

<details>

* Version: 2025.5.20
* GitHub: https://github.com/tdhock/directlabels
* Source code: https://github.com/cran/directlabels
* Date/Publication: 2025-05-20 10:50:02 UTC
* Number of recursive dependencies: 79

Run `revdepcheck::cloud_details(, "directlabels")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘examples.Rmd’ using knitr
    ```

# DiSCos

<details>

* Version: 0.1.1
* GitHub: https://github.com/Davidvandijcke/DiSCos
* Source code: https://github.com/cran/DiSCos
* Date/Publication: 2024-07-23 03:30:03 UTC
* Number of recursive dependencies: 118

Run `revdepcheck::cloud_details(, "DiSCos")` for more info

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
      `expected`: TRUE 
      ── Failure ('test-DiSCoTEA.R:142:3'): cdfDiff works ────────────────────────────
      typeof(discot$plot) == "list" is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      
      [ FAIL 8 | WARN 2 | SKIP 1 | PASS 107 ]
      Error: Test failures
      Execution halted
    ```

# distributions3

<details>

* Version: 0.2.2
* GitHub: https://github.com/alexpghayes/distributions3
* Source code: https://github.com/cran/distributions3
* Date/Publication: 2024-09-16 16:20:02 UTC
* Number of recursive dependencies: 88

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
* Number of recursive dependencies: 95

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

# dndR

<details>

* Version: 3.0.0
* GitHub: https://github.com/njlyon0/dndR
* Source code: https://github.com/cran/dndR
* Date/Publication: 2025-04-02 18:00:02 UTC
* Number of recursive dependencies: 67

Run `revdepcheck::cloud_details(, "dndR")` for more info

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
      `expected[2:2]`:                                                      "ggplot"
      ── Failure ('test-probability_plot.R:22:3'): Outputs are as expected ───────────
      class(my_plot) (`actual`) not equal to c("gg", "ggplot") (`expected`).
      
      `actual`:        "ggplot2::ggplot" "ggplot" "ggplot2::gg" "S7_object" "gg"    
      `expected[2:2]`:                                                      "ggplot"
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 102 ]
      Error: Test failures
      Execution halted
    ```

# duke

<details>

* Version: 0.0.3
* GitHub: https://github.com/aidangildea/duke
* Source code: https://github.com/cran/duke
* Date/Publication: 2023-12-15 21:50:16 UTC
* Number of recursive dependencies: 85

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

# easyalluvial

<details>

* Version: 0.3.2
* GitHub: https://github.com/erblast/easyalluvial
* Source code: https://github.com/cran/easyalluvial
* Date/Publication: 2023-12-07 13:40:06 UTC
* Number of recursive dependencies: 148

Run `revdepcheck::cloud_details(, "easyalluvial")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(easyalluvial)
      > 
      > test_check("easyalluvial")
      [ FAIL 3 | WARN 6 | SKIP 18 | PASS 41 ]
      
      ══ Skipped tests (18) ══════════════════════════════════════════════════════════
    ...
      • plot_marginal_histograms/mod-num-num.svg
      • plot_marginal_histograms/mod-num-pred-train.svg
      • plot_marginal_histograms/plot-hist-long-cat-fill.svg
      • plot_marginal_histograms/plot-hist-long-cat.svg
      • plot_marginal_histograms/plot-hist-long-num-has-fill.svg
      • plot_marginal_histograms/plot-hist-long-num-is-fill.svg
      • plot_marginal_histograms/plot-hist-long-num.svg
      • plot_marginal_histograms/plot-hist-wide-num.svg
      Error: Test failures
      Execution halted
    ```

# easysurv

<details>

* Version: 2.0.1
* GitHub: https://github.com/Maple-Health-Group/easysurv
* Source code: https://github.com/cran/easysurv
* Date/Publication: 2024-06-21 10:30:06 UTC
* Number of recursive dependencies: 152

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

# ecocbo

<details>

* Version: 0.12.0
* GitHub: NA
* Source code: https://github.com/cran/ecocbo
* Date/Publication: 2024-08-21 08:00:02 UTC
* Number of recursive dependencies: 114

Run `revdepcheck::cloud_details(, "ecocbo")` for more info

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
        |======================================================================| 100%
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 26 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-plot_power.R:5:3'): plots are plotted ────────────────────────
      plot_power(epiBetaR, n = 4, m = 4, method = "power") has type 'object', not 'list'.
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 26 ]
      Error: Test failures
      Execution halted
    ```

# EGM

<details>

* Version: 0.1.0
* GitHub: https://github.com/shah-in-boots/EGM
* Source code: https://github.com/cran/EGM
* Date/Publication: 2024-05-23 16:10:05 UTC
* Number of recursive dependencies: 73

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

# eks

<details>

* Version: 1.1.0
* GitHub: NA
* Source code: https://github.com/cran/eks
* Date/Publication: 2025-05-18 17:30:02 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::cloud_details(, "eks")` for more info

</details>

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.scale_rem:
      function(object, plot, object_name)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# ENMTools

<details>

* Version: 1.1.2
* GitHub: NA
* Source code: https://github.com/cran/ENMTools
* Date/Publication: 2024-01-16 12:50:11 UTC
* Number of recursive dependencies: 285

Run `revdepcheck::cloud_details(, "ENMTools")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ENMTools)
      Loading required package: dismo
      Loading required package: raster
      Loading required package: sp
      > 
      > test_check("ENMTools")
    ...
      `actual`:   FALSE
      `expected`: TRUE 
      Backtrace:
          ▆
       1. └─ENMTools (local) expect_enmtools_model(cyreni.glm.raster2) at test_ENMTools.R:249:3
       2.   └─testthat::expect_true(...) at test_ENMTools.R:66:3
      
      [ FAIL 9 | WARN 0 | SKIP 3 | PASS 167 ]
      Error: Test failures
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
     13. │               └─ggplot2 (local) FUN(X[[i]], ...)
     14. │                 └─scale$map_df(df = df)
     15. │                   └─ggplot2 (local) map_df(..., self = self)
     16. │                     └─base::lapply(aesthetics, function(j) self$map(df[[j]]))
     17. │                       └─ggplot2 (local) FUN(X[[i]], ...)
     18. │                         └─self$map(df[[j]])
     19. │                           └─ggplot2 (local) map(..., self = self)
     20. │                             └─vctrs::`vec_slice<-`(`*tmp*`, is.na(x), value = na_value)
     21. └─rlang::cnd_signal(x)
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
* Number of recursive dependencies: 88

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
     21. │                   └─ggplot2 (local) setup_params(...)
     22. │                     └─ggplot2:::make_summary_fun(...)
     23. │                       └─rlang::as_function(fun.data)
     24. │                         └─base::get(x, envir = env, mode = "function")
     25. └─base::.handleSimpleError(...)
     26.   └─rlang (local) h(simpleError(msg, call))
     27.     └─handlers[[1L]](cnd)
     28.       └─cli::cli_abort(...)
     29.         └─rlang::abort(...)
    Execution halted
    ```

# epos

<details>

* Version: 1.1
* GitHub: https://github.com/bernd-mueller/epos
* Source code: https://github.com/cran/epos
* Date/Publication: 2024-03-15 10:10:02 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::cloud_details(, "epos")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(epos)
      > 
      > test_check("epos")
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 13 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      [1] 1 - 11 == -10
      Backtrace:
          ▆
       1. └─testthat::expect_that(length(tanimotobaseline), equals(11)) at test_createTanimotoBaseline.R:47:3
       2.   └─testthat (local) condition(object)
       3.     └─testthat::expect_equal(x, expected, ..., expected.label = label)
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 13 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘testthat’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 15 marked UTF-8 strings
    ```

# EQUALSTATS

<details>

* Version: 0.5.0
* GitHub: NA
* Source code: https://github.com/cran/EQUALSTATS
* Date/Publication: 2024-09-23 08:30:02 UTC
* Number of recursive dependencies: 128

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
* Number of recursive dependencies: 145

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
* Number of recursive dependencies: 65

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
     23. │                       └─self$draw_panel(data, panel_params, coord, width = 0.05, height = 0.05)
     24. │                         └─errors (local) draw_panel(...)
     25. │                           ├─base::append(...)
     26. │                           └─ggplot2::GeomErrorbarh$draw_panel(...)
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

# eudract

<details>

* Version: 1.0.4
* GitHub: https://github.com/shug0131/eudraCT
* Source code: https://github.com/cran/eudract
* Date/Publication: 2025-03-20 17:20:02 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "eudract")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘eudract-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: dot_plot
    > ### Title: creates a dot-plot of safety data showing the absolute and
    > ###   relative risks
    > ### Aliases: dot_plot
    > 
    > ### ** Examples
    > 
    ...
    
    $reference
    [1] "Control"
    
    > fig <- dot_plot(safety_statistics, type="non_serious", base=4)
    > fig
    Error: Can't find method for generic `&(e1, e2)`:
    - e1: <patchwork>
    - e2: <theme>
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(eudract)
      > 
      > test_check("eudract")
      [ FAIL 1 | WARN 3 | SKIP 4 | PASS 75 ]
      
      ══ Skipped tests (4) ═══════════════════════════════════════════════════════════
    ...
      
      [ FAIL 1 | WARN 3 | SKIP 4 | PASS 75 ]
      Deleting unused snapshots:
      • dotplot/dotplot-1group.svg
      • dotplot/dotplot-3groups.svg
      • dotplot/dotplot-ref-rr.svg
      • dotplot/dotplot-ref.svg
      • dotplot/dotplot.svg
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘eudract.Rmd’ using rmarkdown
    --- finished re-building ‘eudract.Rmd’
    
    --- re-building ‘standard_reporting.Rmd’ using rmarkdown
    
    Quitting from standard_reporting.Rmd:65-67 [dotplot_sae]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error:
    ...
    Can't find method for generic `&(e1, e2)`:
    - e1: <patchwork>
    - e2: <theme>
    --- failed re-building ‘standard_reporting.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘standard_reporting.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# eventstudyr

<details>

* Version: 1.1.3
* GitHub: https://github.com/JMSLab/eventstudyr
* Source code: https://github.com/cran/eventstudyr
* Date/Publication: 2024-03-04 15:00:02 UTC
* Number of recursive dependencies: 97

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

# EvoPhylo

<details>

* Version: 0.3.2
* GitHub: https://github.com/tiago-simoes/EvoPhylo
* Source code: https://github.com/cran/EvoPhylo
* Date/Publication: 2022-11-03 17:00:02 UTC
* Number of recursive dependencies: 146

Run `revdepcheck::cloud_details(, "EvoPhylo")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘EvoPhylo-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_treerates_sgn
    > ### Title: Plot Bayesian evolutionary tree with rate thresholds for
    > ###   selection mode
    > ### Aliases: plot_treerates_sgn
    > 
    > ### ** Examples
    > 
    ...
    Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
    ℹ Please use tidy evaluation idioms with `aes()`.
    ℹ See also `vignette("ggplot2-in-packages")` for more information.
    ℹ The deprecated feature was likely used in the ggtree package.
      Please report the issue at <https://github.com/YuLab-SMU/ggtree/issues>.
    Error: .onLoad failed in loadNamespace() for 'deeptime', details:
      call: NULL
      error: <ggplot2::element_text> object properties are invalid:
    - @hjust must be <NULL>, <integer>, or <double>, not <logical>
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘char-part.Rmd’ using rmarkdown
    
    Quitting from char-part.Rmd:28-30 [unnamed-chunk-2]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'char-part.Rmd' failed with diagnostics:
    The package "deeptime" (>= 0.2.0) is required.
    ...
    Error: processing vignette 'rates-selection_MrBayes.Rmd' failed with diagnostics:
    The package "deeptime" (>= 0.2.0) is required.
    --- failed re-building ‘rates-selection_MrBayes.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘char-part.Rmd’ ‘fbd-params.Rmd’ ‘offset_handling.Rmd’
      ‘rates-selection_BEAST2.Rmd’ ‘rates-selection_MrBayes.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        data      2.5Mb
        doc       1.6Mb
        extdata   2.4Mb
    ```

# explainer

<details>

* Version: 1.0.2
* GitHub: https://github.com/PERSIMUNE/explainer
* Source code: https://github.com/cran/explainer
* Date/Publication: 2024-09-30 17:30:02 UTC
* Number of recursive dependencies: 184

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
* Number of recursive dependencies: 62

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
* Number of recursive dependencies: 76

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

# EZFragility

<details>

* Version: 1.0.3
* GitHub: NA
* Source code: https://github.com/cran/EZFragility
* Date/Publication: 2025-04-10 14:40:09 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::cloud_details(, "EZFragility")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘EZFragility-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotFragHeatmap
    > ### Title: Visualization functions (raw signal, fragility matrix)
    > ### Aliases: plotFragHeatmap plotFragQuantile plotFragDistribution
    > 
    > ### ** Examples
    > 
    > 
    ...
    > 
    > ## precomputed fragility object
    > data("pt01Frag")
    > 
    > ## plot the fragility heatmap
    > plotFragHeatmap(frag = pt01Frag, sozIndex = sozIndex)
    Warning: Ignoring unknown labels:
    • size : "2"
    Error: `object` must be an <S7_object>, not a S3<element_markdown/element_text/element>
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Intro_to_EZFragility.Rmd’ using rmarkdown
    ```

# ezplot

<details>

* Version: 0.7.13
* GitHub: NA
* Source code: https://github.com/cran/ezplot
* Date/Publication: 2024-01-28 11:30:05 UTC
* Number of recursive dependencies: 105

Run `revdepcheck::cloud_details(, "ezplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ezplot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: variable_plot
    > ### Title: variable_plot
    > ### Aliases: variable_plot
    > 
    > ### ** Examples
    > 
    > suppressPackageStartupMessages(library(tsibble))
    > library(tsibbledata)
    > variable_plot(ansett, "Week", "Passengers", facet_x = "Class", yoy = TRUE)
    Error: <ggplot2::element_rect> object properties are invalid:
    - @linewidth must be <NULL>, <integer>, or <double>, not <logical>
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ezplot)
      > suppressPackageStartupMessages(library(tsibble))
      > 
      > test_check("ezplot")
      [ FAIL 2 | WARN 6 | SKIP 0 | PASS 90 ]
      
    ...
       2.   ├─ggplot2::theme(...)
       3.   │ └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
       4.   │   └─base::mget(args, envir = env)
       5.   └─ggplot2::element_rect(fill = NA, colour = NA, linewidth = NA)
       6.     └─S7::new_object(...)
       7.       └─S7::validate(object, recursive = !parent_validated)
      
      [ FAIL 2 | WARN 6 | SKIP 0 | PASS 90 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘bar_plot.Rmd’ using rmarkdown
    ```

# fabletools

<details>

* Version: 0.5.0
* GitHub: https://github.com/tidyverts/fabletools
* Source code: https://github.com/cran/fabletools
* Date/Publication: 2024-09-17 07:30:02 UTC
* Number of recursive dependencies: 104

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
* Number of recursive dependencies: 83

Run `revdepcheck::cloud_details(, "fairmodels")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘fairmodels-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: fairness_heatmap
    > ### Title: Fairness heatmap
    > ### Aliases: fairness_heatmap
    > 
    > ### ** Examples
    > 
    > 
    ...
    [32m Fairness object created succesfully [39m 
    > 
    > 
    > fh <- fairness_heatmap(fobject)
    > 
    > plot(fh)
    Error in rep(yes, length.out = len) : 
      attempt to replicate an object of type 'object'
    Calls: plot -> plot.fairness_heatmap -> ifelse
    Execution halted
    ```

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
        5. │ └─base::class(object) %in% class
        6. ├─fairmodels::plot_fairmodels(fc, type = "fairness_heatmap")
        7. └─fairmodels:::plot_fairmodels.fairness_object(fc, type = "fairness_heatmap")
        8.   └─fairmodels:::plot_fairmodels.default(x, type, ...)
        9.     └─fairmodels:::plot.fairness_heatmap(fairness_heatmap(x, ...))
       10.       └─base::ifelse(...)
      
      [ FAIL 3 | WARN 2 | SKIP 0 | PASS 299 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Advanced_tutorial.Rmd’ using rmarkdown
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

# fairness

<details>

* Version: 1.2.2
* GitHub: https://github.com/kozodoi/fairness
* Source code: https://github.com/cran/fairness
* Date/Publication: 2021-04-14 15:00:02 UTC
* Number of recursive dependencies: 149

Run `revdepcheck::cloud_details(, "fairness")` for more info

</details>

## Newly broken

*   checking whether package ‘fairness’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘e1071::element’ by ‘ggplot2::element’ when loading ‘fairness’
    See ‘/tmp/workdir/fairness/new/fairness.Rcheck/00install.out’ for details.
    ```

# faux

<details>

* Version: 1.2.2
* GitHub: https://github.com/debruine/faux
* Source code: https://github.com/cran/faux
* Date/Publication: 2025-01-15 09:10:01 UTC
* Number of recursive dependencies: 127

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
      class(p) not equal to c("gg", "ggplot").
      Lengths differ: 5 is not 2
      Backtrace:
          ▆
       1. └─global expect_equal(class(p), c("gg", "ggplot")) at test-plot_design.R:288:3
       2.   └─testthat::expect_equal(..., check.environment = FALSE)
      
      [ FAIL 19 | WARN 0 | SKIP 22 | PASS 1351 ]
      Error: Test failures
      Execution halted
    ```

# fChange

<details>

* Version: 2.0.0
* GitHub: NA
* Source code: https://github.com/cran/fChange
* Date/Publication: 2025-03-27 18:00:09 UTC
* Number of recursive dependencies: 206

Run `revdepcheck::cloud_details(, "fChange")` for more info

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
      `expected`: "gg"             
      ── Failure ('test-plot_qq.R:3:3'): QQ Plot ─────────────────────────────────────
      class(tmp)[1] (`actual`) not equal to "gg" (`expected`).
      
      `actual`:   "ggplot2::ggplot"
      `expected`: "gg"             
      
      [ FAIL 13 | WARN 0 | SKIP 9 | PASS 177 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        libs   3.5Mb
    ```

# feasts

<details>

* Version: 0.4.1
* GitHub: https://github.com/tidyverts/feasts
* Source code: https://github.com/cran/feasts
* Date/Publication: 2024-09-25 23:40:02 UTC
* Number of recursive dependencies: 97

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
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 101 ]
      
    ...
          ▆
       1. ├─ggplot2::ggplot_build(p[[1]]) at test-graphics.R:194:3
       2. ├─ggplot2:::ggplot_build.default(p[[1]])
       3. │ └─ggplot2::build_ggplot(plot)
       4. │   └─S7::S7_dispatch()
       5. └─S7:::method_lookup_error("build_ggplot", `<named list>`)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 101 ]
      Error: Test failures
      Execution halted
    ```

# fec16

<details>

* Version: 0.1.4
* GitHub: https://github.com/baumer-lab/fec16
* Source code: https://github.com/cran/fec16
* Date/Publication: 2023-08-09 10:50:14 UTC
* Number of recursive dependencies: 91

Run `revdepcheck::cloud_details(, "fec16")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘fec_vignette.Rmd’ using rmarkdown
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 20 marked UTF-8 strings
    ```

# fitbitViz

<details>

* Version: 1.0.6
* GitHub: https://github.com/mlampros/fitbitViz
* Source code: https://github.com/cran/fitbitViz
* Date/Publication: 2024-02-08 09:30:02 UTC
* Number of recursive dependencies: 153

Run `revdepcheck::cloud_details(, "fitbitViz")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(fitbitViz)
      > 
      > test_check("fitbitViz")
      [ FAIL 3 | WARN 3 | SKIP 0 | PASS 4 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
       4.   ├─ggplot2::theme(...)
       5.   │ └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
       6.   │   └─base::mget(args, envir = env)
       7.   └─ggplot2::element_text(...)
       8.     └─S7::new_object(...)
       9.       └─S7::validate(object, recursive = !parent_validated)
      
      [ FAIL 3 | WARN 3 | SKIP 0 | PASS 4 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        doc   4.0Mb
    ```

# flextable

<details>

* Version: 0.9.9
* GitHub: https://github.com/davidgohel/flextable
* Source code: https://github.com/cran/flextable
* Date/Publication: 2025-05-31 09:40:02 UTC
* Number of recursive dependencies: 131

Run `revdepcheck::cloud_details(, "flextable")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘flextable-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gg_chunk
    > ### Title: 'ggplots' chunk wrapper
    > ### Aliases: gg_chunk
    > 
    > ### ** Examples
    > 
    > library(data.table)
    ...
    Loading required package: ggplot2
    a flextable object.
    col_keys: `Species`, `gg` 
    header has 1 row(s) 
    body has 3 row(s) 
    original dataset sample: 
    Error in format.default(unlist(xx), ...) : 
      c("Found no format() method for class \"ggplot2::ggplot\"", "Found no format() method for class \"ggplot\"", "Found no format() method for class \"ggplot2::gg\"", "Found no format() method for class \"S7_object\"", "Found no format() method for class \"gg\"")
    Calls: <Anonymous> ... format -> format.default -> lapply -> FUN -> format.default
    Execution halted
    ```

# flipr

<details>

* Version: 0.3.3
* GitHub: https://github.com/LMJL-Alea/flipr
* Source code: https://github.com/cran/flipr
* Date/Publication: 2023-08-23 09:00:02 UTC
* Number of recursive dependencies: 104

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
     6. ├─plotly::ggplotly(.)
     7. └─plotly:::ggplotly.ggplot(.)
     8.   └─plotly::gg2list(...)
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

# fmf

<details>

* Version: 1.1.1
* GitHub: NA
* Source code: https://github.com/cran/fmf
* Date/Publication: 2020-09-03 07:32:12 UTC
* Number of recursive dependencies: 175

Run `revdepcheck::cloud_details(, "fmf")` for more info

</details>

## Newly broken

*   checking whether package ‘fmf’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘e1071::element’ by ‘ggplot2::element’ when loading ‘fmf’
    See ‘/tmp/workdir/fmf/new/fmf.Rcheck/00install.out’ for details.
    ```

## In both

*   checking C++ specification ... NOTE
    ```
      Specified C++11: please drop specification unless essential
    ```

# foqat

<details>

* Version: 2.0.8.2
* GitHub: https://github.com/tianshu129/foqat
* Source code: https://github.com/cran/foqat
* Date/Publication: 2023-09-30 06:10:02 UTC
* Number of recursive dependencies: 72

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

# forestly

<details>

* Version: 0.1.2
* GitHub: https://github.com/Merck/forestly
* Source code: https://github.com/cran/forestly
* Date/Publication: 2025-01-10 16:20:14 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "forestly")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘forest-plot-static.Rmd’ using rmarkdown
    ```

# forestPSD

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/forestPSD
* Date/Publication: 2024-11-11 16:50:05 UTC
* Number of recursive dependencies: 44

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
     16. │           └─l$compute_geom_2(d, theme = plot@theme)
     17. │             └─ggplot2 (local) compute_geom_2(..., self = self)
     18. │               └─self$geom$use_defaults(...)
     19. │                 └─ggplot2 (local) use_defaults(..., self = self)
     20. │                   └─ggplot2:::check_aesthetics(new_params, nrow(data))
     21. │                     └─vctrs::list_sizes(x)
     22. └─vctrs:::stop_scalar_type(`<fn>`(`<expression>`), "x$label", `<env>`)
     23.   └─vctrs:::stop_vctrs(...)
     24.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggplot2’ ‘reshape2’
      All declared Imports should be used.
    ```

# formods

<details>

* Version: 0.2.0
* GitHub: https://github.com/john-harrold/formods
* Source code: https://github.com/cran/formods
* Date/Publication: 2025-01-07 16:20:06 UTC
* Number of recursive dependencies: 187

Run `revdepcheck::cloud_details(, "formods")` for more info

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
      ── Failure ('test-FG_module.R:13:5'): FG -- Forcing builds of figures ──────────
      class(ggplot2::ggplot_build(sess_res[["state"]][["FG"]][["figs"]][[fid]][["fobj"]])) (`actual`) not equal to "ggplot_built" (`expected`).
      
      `actual`:   "ggplot2::ggplot_built" "ggplot_built" "ggplot2::gg" "S7_object"
      `expected`: "ggplot_built"                                                  
      
      [ FAIL 1 | WARN 32 | SKIP 0 | PASS 39 ]
      Error: Test failures
      Execution halted
    ```

# fqar

<details>

* Version: 0.5.4
* GitHub: https://github.com/equitable-equations/fqar
* Source code: https://github.com/cran/fqar
* Date/Publication: 2024-10-06 18:00:02 UTC
* Number of recursive dependencies: 72

Run `revdepcheck::cloud_details(, "fqar")` for more info

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
      ── Failure ('test-species_profile_plot.R:21:3'): species_profile_plot works ────
      class(p) (`actual`) not equal to c("gg", "ggplot") (`expected`).
      
      `actual`:        "ggplot2::ggplot" "ggplot" "ggplot2::gg" "S7_object" "gg"    
      `expected[2:2]`:                                                      "ggplot"
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 365 ]
      Error: Test failures
      Execution halted
    ```

# frailtyEM

<details>

* Version: 1.0.1
* GitHub: https://github.com/tbalan/frailtyEM
* Source code: https://github.com/cran/frailtyEM
* Date/Publication: 2019-09-22 13:00:10 UTC
* Number of recursive dependencies: 75

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

# funcharts

<details>

* Version: 1.7.0
* GitHub: https://github.com/unina-sfere/funcharts
* Source code: https://github.com/cran/funcharts
* Date/Publication: 2025-03-17 17:30:02 UTC
* Number of recursive dependencies: 126

Run `revdepcheck::cloud_details(, "funcharts")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘funcharts-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: control_charts_pca_mfd_real_time
    > ### Title: Real-time T2 and SPE control charts for multivariate functional
    > ###   data
    > ### Aliases: control_charts_pca_mfd_real_time
    > 
    > ### ** Examples
    > 
    ...
    > 
    > cclist <- control_charts_pca_mfd_real_time(
    +   pca_list = pca_list,
    +   components_list = 1:3,
    +   mfdobj_x_test = mfdobj_x2_list)
    > plot_control_charts_real_time(cclist, 1)
    Error: Can't find method for generic `&(e1, e2)`:
    - e1: <patchwork>
    - e2: <theme>
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(funcharts)
      Loading required package: robustbase
      > 
      > test_check("funcharts")
      [1] "The Fisher omnibus combining function is considered."
      [1] "The Fisher omnibus combining function is considered."
    ...
      - e1: <patchwork>
      - e2: <theme>
      Backtrace:
          ▆
       1. └─funcharts::plot_control_charts_real_time(cclist, 1) at test_phaseII.R:104:3
       2.   └─S7:::Ops.S7_object(...)
      
      [ FAIL 3 | WARN 0 | SKIP 1 | PASS 111 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        libs   3.4Mb
    ```

# FunnelPlotR

<details>

* Version: 0.5.0
* GitHub: https://github.com/nhs-r-community/FunnelPlotR
* Source code: https://github.com/cran/FunnelPlotR
* Date/Publication: 2024-04-12 08:40:02 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::cloud_details(, "FunnelPlotR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘FunnelPlotR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: funnel_plot
    > ### Title: Funnel plots for comparing institutional performance
    > ### Aliases: funnel_plot
    > 
    > ### ** Examples
    > 
    > # We will use the 'medpar' dataset from the 'COUNT' package.
    ...
    > 
    > # Get predicted values for building ratio
    > medpar$prds<- predict(mod, type="response")
    > 
    > # Draw plot, returning just the plot object
    > fp<-funnel_plot(medpar, denominator=prds, numerator=los,
    + group = provnum, limit=95, title="An example funnel plot")
    Error in validate_funnel_plot(rtn) : Invalid ggplot object
    Calls: funnel_plot -> validate_funnel_plot
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(FunnelPlotR)
      > 
      > test_check("FunnelPlotR")
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 34 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      ── Error ('test-funnel_plot.R:7:3'): `funnel_plot()` works with input and returns expected list ──
      Error in `validate_funnel_plot(rtn)`: Invalid ggplot object
      Backtrace:
          ▆
       1. └─FunnelPlotR::funnel_plot(dt, num, denom, group) at test-funnel_plot.R:7:3
       2.   └─FunnelPlotR:::validate_funnel_plot(rtn)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 34 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘changing_funnel_plot_options.Rmd’ using rmarkdown
    
    Quitting from changing_funnel_plot_options.Rmd:32-49 [dtsetup]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `validate_funnel_plot()`:
    ! Invalid ggplot object
    ---
    Backtrace:
    ...
        ▆
     1. └─FunnelPlotR::funnel_plot(...)
     2.   └─FunnelPlotR:::validate_funnel_plot(rtn)
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'changing_funnel_plot_options.Rmd' failed with diagnostics:
    Invalid ggplot object
    --- failed re-building ‘changing_funnel_plot_options.Rmd’
    
    --- re-building ‘funnel_plots.Rmd’ using rmarkdown
    ```

# gapfill

<details>

* Version: 0.9.6-1
* GitHub: https://github.com/florafauna/gapfill
* Source code: https://github.com/cran/gapfill
* Date/Publication: 2021-02-12 10:10:05 UTC
* Number of recursive dependencies: 67

Run `revdepcheck::cloud_details(, "gapfill")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘run-all.R’
    Running the tests in ‘tests/run-all.R’ failed.
    Complete output:
      > library(testthat)
      > test_check('gapfill')
      Loading required package: gapfill
      Loading required package: ggplot2
      --> See ?Gapfill and https://doi.org/10.1109/TGRS.2017.2785240 <--
      [ FAIL 5 | WARN 6 | SKIP 0 | PASS 870 ]
      
    ...
      ── Failure ('test-Image.R:10:4'): Image ────────────────────────────────────────
      class(Image(unname(ndvi))) not equal to c("gg", "ggplot").
      Lengths differ: 5 is not 2
      ── Failure ('test-Image.R:11:4'): Image ────────────────────────────────────────
      class(Image(ndvi, zlim = c(1, 2))) not equal to c("gg", "ggplot").
      Lengths differ: 5 is not 2
      
      [ FAIL 5 | WARN 6 | SKIP 0 | PASS 870 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      'raster', 'doParallel', 'doMPI'
    ```

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) Gapfill.Rd:66-68: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) Gapfill.Rd:69-70: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) Gapfill.Rd:71-77: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) Gapfill.Rd:72: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) Gapfill.Rd:73: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) Gapfill.Rd:74: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) Gapfill.Rd:75: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) Gapfill.Rd:78: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) ndvi.Rd:10: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) ndvi.Rd:11: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) ndvi.Rd:12: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) ndvi.Rd:13: Lost braces in \itemize; meant \describe ?
    ```

# gapminder

<details>

* Version: 1.0.0
* GitHub: https://github.com/jennybc/gapminder
* Source code: https://github.com/cran/gapminder
* Date/Publication: 2023-03-10 09:50:08 UTC
* Number of recursive dependencies: 62

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

# genekitr

<details>

* Version: 1.2.8
* GitHub: https://github.com/GangLiLab/genekitr
* Source code: https://github.com/cran/genekitr
* Date/Publication: 2024-09-06 13:00:06 UTC
* Number of recursive dependencies: 196

Run `revdepcheck::cloud_details(, "genekitr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘genekitr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotVenn
    > ### Title: Venn plot for groups of genes
    > ### Aliases: plotVenn
    > 
    > ### ** Examples
    > 
    > k1 = requireNamespace("ComplexUpset",quietly = TRUE)
    ...
     32. │                           └─ggplot2 (local) `method(merge_element, list(ggplot2::element, class_any))`(...)
     33. │                             └─S7::props(old, idx)
     34. │                               └─S7::check_is_S7(object)
     35. │                                 └─base::stop(msg, call. = FALSE)
     36. └─base::.handleSimpleError(...)
     37.   └─rlang (local) h(simpleError(msg, call))
     38.     └─handlers[[1L]](cnd)
     39.       └─cli::cli_abort(...)
     40.         └─rlang::abort(...)
    Execution halted
    ```

# geneSLOPE

<details>

* Version: 0.38.2
* GitHub: https://github.com/psobczyk/geneSLOPE
* Source code: https://github.com/cran/geneSLOPE
* Date/Publication: 2023-08-16 09:12:37 UTC
* Number of recursive dependencies: 76

Run `revdepcheck::cloud_details(, "geneSLOPE")` for more info

</details>

## Newly broken

*   checking whether package ‘geneSLOPE’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
    See ‘/tmp/workdir/geneSLOPE/new/geneSLOPE.Rcheck/00install.out’ for details.
    ```

# geofacet

<details>

* Version: 0.2.1
* GitHub: https://github.com/hafen/geofacet
* Source code: https://github.com/cran/geofacet
* Date/Publication: 2023-11-30 08:00:11 UTC
* Number of recursive dependencies: 93

Run `revdepcheck::cloud_details(, "geofacet")` for more info

</details>

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.facet_geo_spec:
      function(object, plot, object_name)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 646 marked UTF-8 strings
    ```

# geoheatmap

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/geoheatmap
* Date/Publication: 2024-09-05 15:40:02 UTC
* Number of recursive dependencies: 102

Run `revdepcheck::cloud_details(, "geoheatmap")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘geoheatmap.Rmd’ using rmarkdown
    ```

# geomtextpath

<details>

* Version: 0.1.5
* GitHub: https://github.com/AllanCameron/geomtextpath
* Source code: https://github.com/cran/geomtextpath
* Date/Publication: 2025-01-14 17:40:02 UTC
* Number of recursive dependencies: 91

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
     22. │                       ├─rlang::inject(self$draw_panel(data, panel_params, coord, !!!params))
     23. │                       └─self$draw_panel(...)
     24. │                         └─geomtextpath (local) draw_panel(...)
     25. │                           └─geomtextpath:::sf_textgrob(...)
     26. └─base::.handleSimpleError(...)
     27.   └─rlang (local) h(simpleError(msg, call))
     28.     └─handlers[[1L]](cnd)
     29.       └─cli::cli_abort(...)
     30.         └─rlang::abort(...)
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

# gfoRmulaICE

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/gfoRmulaICE
* Date/Publication: 2024-12-02 12:50:08 UTC
* Number of recursive dependencies: 89

Run `revdepcheck::cloud_details(, "gfoRmulaICE")` for more info

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
      `expected[2:2]`:                                                      "ggplot"
      ── Failure ('test_plot.R:534:5'): plot complicated scenario 3 - intervention-specific time options ──
      class(plots) (`actual`) not equal to c("gg", "ggplot") (`expected`).
      
      `actual`:        "ggplot2::ggplot" "ggplot" "ggplot2::gg" "S7_object" "gg"    
      `expected[2:2]`:                                                      "ggplot"
      
      [ FAIL 15 | WARN 176 | SKIP 0 | PASS 47 ]
      Error: Test failures
      Execution halted
    ```

# gg1d

<details>

* Version: 0.1.0
* GitHub: https://github.com/selkamand/gg1d
* Source code: https://github.com/cran/gg1d
* Date/Publication: 2024-12-09 19:40:02 UTC
* Number of recursive dependencies: 87

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
     31. │                                 └─base::lapply(...)
     32. │                                   └─ggplot2 (local) FUN(X[[i]], ...)
     33. │                                     ├─rlang::inject(self$draw_panel(data, panel_params, coord, !!!params))
     34. │                                     └─self$draw_panel(...)
     35. └─base::.handleSimpleError(...)
     36.   └─rlang (local) h(simpleError(msg, call))
     37.     └─handlers[[1L]](cnd)
     38.       └─cli::cli_abort(...)
     39.         └─rlang::abort(...)
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
      
      [ FAIL 9 | WARN 1 | SKIP 2 | PASS 47 ]
      Error: Test failures
      Execution halted
    ```

# ggalign

<details>

* Version: 1.0.2
* GitHub: https://github.com/Yunuuuu/ggalign
* Source code: https://github.com/cran/ggalign
* Date/Publication: 2025-05-14 14:00:08 UTC
* Number of recursive dependencies: 56

Run `revdepcheck::cloud_details(, "ggalign")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggalign-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: align_dendro
    > ### Title: Plot dendrogram tree
    > ### Aliases: align_dendro
    > 
    > ### ** Examples
    > 
    > # align_dendro will always add a plot area
    ...
     37. │                           ├─S7::S7_dispatch()
     38. │                           └─ggplot2 (local) `method(+, list(ggplot2::ggplot, class_any))`(...)
     39. │                             └─ggplot2:::add_ggplot(e1, e2, e2name)
     40. │                               ├─ggplot2::ggplot_add(object, p, objectname)
     41. │                               └─ggalign:::ggplot_add.ggalign_design(object, p, objectname)
     42. └─base::.handleSimpleError(...)
     43.   └─rlang (local) h(simpleError(msg, call))
     44.     └─handlers[[1L]](cnd)
     45.       └─rlang::cnd_signal(e)
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
      • plot-align/reorder-left-reverse.svg
      • plot-align/reorder-left.svg
      • plot-align/reorder-right.svg
      • plot-align/reorder-top-reverse.svg
      • plot-align/reorder-top-within-group.svg
      • plot-align/reorder-top.svg
      • plot-align/stack-no-data-dendrogram-input.svg
      • plot-align/stack-no-data-hclust-input.svg
      Error: Test failures
      Execution halted
    ```

*   checking S3 generic/method consistency ... WARNING
    ```
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.ggalign_default_expansion:
      function(object, plot, object_name)
    
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.FacetSector:
      function(object, plot, object_name)
    
    ...
      function(plot, ...)
    ggplot_build.ggalign_facet_sector_plot:
      function(plot)
    
    ggplot_build:
      function(plot, ...)
    ggplot_build.ggalign_heatmap:
      function(plot)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking Rd cross-references ... WARNING
    ```
    Packages unavailable to check Rd xrefs: ‘ape’, ‘patchwork’, ‘maftools’, ‘ComplexHeatmap’, ‘pheatmap’
    Missing link or links in Rd file 'scheme_theme.Rd':
      ‘[ggplot2:+.gg]{+.gg()}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

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

## Newly fixed

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘ape’, ‘patchwork’, ‘maftools’, ‘ComplexHeatmap’, ‘pheatmap’
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      'patchwork', 'ggrastr', 'maftools'
    ```

# GGally

<details>

* Version: 2.2.1
* GitHub: https://github.com/ggobi/ggally
* Source code: https://github.com/cran/GGally
* Date/Publication: 2024-02-14 00:53:32 UTC
* Number of recursive dependencies: 143

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
* Number of recursive dependencies: 93

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

*   checking S3 generic/method consistency ... WARNING
    ```
    ggplot_build:
      function(plot, ...)
    ggplot_build.gganim:
      function(plot)
    
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.ExitFactory:
      function(object, plot, object_name)
    
    ...
      function(object, plot, ...)
    ggplot_add.View:
      function(object, plot, object_name)
    
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.EaseAes:
      function(object, plot, object_name)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# ggblend

<details>

* Version: 0.1.0
* GitHub: https://github.com/mjskay/ggblend
* Source code: https://github.com/cran/ggblend
* Date/Publication: 2023-05-22 08:30:05 UTC
* Number of recursive dependencies: 59

Run `revdepcheck::cloud_details(, "ggblend")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggblend)
      > 
      > test_check("ggblend")
      [ FAIL 3 | WARN 2 | SKIP 7 | PASS 99 ]
      
      ══ Skipped tests (7) ═══════════════════════════════════════════════════════════
    ...
      `actual$mapping` is an S3 object of class <uneval>, a list
      `expected$mapping` is an S7 object of class <ggplot2::mapping>
      Backtrace:
          ▆
       1. └─ggblend:::expect_equal_layer(...) at test-operation-adjust.R:19:3
       2.   └─testthat::expect_equal(...) at tests/testthat/helper-layer.R:26:3
      
      [ FAIL 3 | WARN 2 | SKIP 7 | PASS 99 ]
      Error: Test failures
      Execution halted
    ```

# ggborderline

<details>

* Version: 0.2.0
* GitHub: https://github.com/wurli/ggborderline
* Source code: https://github.com/cran/ggborderline
* Date/Publication: 2022-10-25 13:45:14 UTC
* Number of recursive dependencies: 42

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
      5.     └─ggplot2::build_ggplot(plot)
      6.       ├─S7::S7_dispatch()
      7.       └─ggplot2 (local) `method(build_ggplot, ggplot2::ggplot)`(...)
      8.         └─npscales$set_palettes(plot@theme)
      9.           └─ggplot2 (local) set_palettes(..., self = self)
     10.             ├─scales::as_discrete_pal(elem)
     11.             └─scales:::as_discrete_pal.default(elem)
     12.               └─cli::cli_abort("Cannot convert {.arg x} to a discrete palette.")
     13.                 └─rlang::abort(...)
    Execution halted
    ```

# ggbrain

<details>

* Version: 0.9.0
* GitHub: https://github.com/michaelhallquist/ggbrain
* Source code: https://github.com/cran/ggbrain
* Date/Publication: 2025-03-20 01:30:02 UTC
* Number of recursive dependencies: 71

Run `revdepcheck::cloud_details(, "ggbrain")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ggbrain_aesthetics.Rmd’ using rmarkdown
    --- finished re-building ‘ggbrain_aesthetics.Rmd’
    
    --- re-building ‘ggbrain_introduction.Rmd’ using rmarkdown
    ```

*   checking S3 generic/method consistency ... WARNING
    ```
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.ggbrain_panel:
      function(object, plot, object_name)
    
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.ggbrain_label:
      function(object, plot, object_name)
    
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.ggbrain_layer:
      function(object, plot, object_name)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.1Mb
      sub-directories of 1Mb or more:
        doc       1.6Mb
        extdata   2.2Mb
        libs      5.5Mb
    ```

# ggbreak

<details>

* Version: 0.1.4
* GitHub: https://github.com/YuLab-SMU/ggbreak
* Source code: https://github.com/cran/ggbreak
* Date/Publication: 2025-02-04 17:50:10 UTC
* Number of recursive dependencies: 61

Run `revdepcheck::cloud_details(, "ggbreak")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggbreak-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: scale_wrap
    > ### Title: scale-wrap
    > ### Aliases: scale_wrap
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    > library(ggbreak)
    > p <- ggplot(economics, aes(x=date, y = unemploy, colour = uempmed)) +
    +      geom_line()
    > p + scale_wrap(n=4)
    Error: Can't find method for generic `&(e1, e2)`:
    - e1: <patchwork>
    - e2: <theme>
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ggbreak.Rmd’ using rmarkdown
    
    Quitting from ggbreak.Rmd:61-78 [unnamed-chunk-2]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `UseMethod()`:
    ! no applicable method for 'as.grob' applied to an object of class "c('LayerInstance', 'Layer', 'ggproto', 'gg')"
    ---
    Backtrace:
    ...
    
    Error: processing vignette 'ggbreak.Rmd' failed with diagnostics:
    no applicable method for 'as.grob' applied to an object of class "c('LayerInstance', 'Layer', 'ggproto', 'gg')"
    --- failed re-building ‘ggbreak.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ggbreak.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking S3 generic/method consistency ... WARNING
    ```
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.wrap_params:
      function(object, plot, object_name)
    
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.ggcut_params:
      function(object, plot, object_name)
    
    ...
      function(object, plot, ...)
    ggplot_add.ggbreak:
      function(object, plot, object_name)
    
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.gg:
      function(object, plot, object_name)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# ggbump

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/ggbump
* Date/Publication: 2020-04-24 16:00:02 UTC
* Number of recursive dependencies: 50

Run `revdepcheck::cloud_details(, "ggbump")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggbump)
      > 
      > test_check("ggbump")
      [ FAIL 3 | WARN 1 | SKIP 0 | PASS 1 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      ── Failure ('test-numeric_inputs.R:21:3'): multiplication works ────────────────
      ... %>% class() not equal to c("gg", "ggplot").
      Lengths differ: 5 is not 2
      ── Failure ('test-numeric_inputs.R:26:3'): multiplication works ────────────────
      ... %>% class() not equal to c("gg", "ggplot").
      Lengths differ: 5 is not 2
      
      [ FAIL 3 | WARN 1 | SKIP 0 | PASS 1 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# ggdark

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/ggdark
* Date/Publication: 2019-01-11 17:30:06 UTC
* Number of recursive dependencies: 42

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
* Number of recursive dependencies: 51

Run `revdepcheck::cloud_details(, "ggdemetra")` for more info

</details>

## Newly broken

*   checking whether package ‘ggdemetra’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: `aes_()` was deprecated in ggplot2 3.0.0.
    See ‘/tmp/workdir/ggdemetra/new/ggdemetra.Rcheck/00install.out’ for details.
    ```

# ggDoE

<details>

* Version: 0.8
* GitHub: https://github.com/toledo60/ggDoE
* Source code: https://github.com/cran/ggDoE
* Date/Publication: 2024-02-10 04:50:02 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::cloud_details(, "ggDoE")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggDoE-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gg_lm
    > ### Title: Regression Diagnostic Plots with ggplot2
    > ### Aliases: gg_lm
    > 
    > ### ** Examples
    > 
    > model <- lm(mpg ~ wt + am + gear, data = mtcars)
    > gg_lm(model)
    Warning: `fortify(<lm>)` was deprecated in ggplot2 3.6.0.
    ℹ Please use `broom::augment(<lm>)` instead.
    ℹ The deprecated feature was likely used in the ggplot2 package.
      Please report the issue at <https://github.com/tidyverse/ggplot2/issues>.
    Error: Can't find method for generic `&(e1, e2)`:
    - e1: <patchwork>
    - e2: <theme>
    Execution halted
    ```

# ggDoubleHeat

<details>

* Version: 0.1.2
* GitHub: https://github.com/PursuitOfDataScience/ggDoubleHeat
* Source code: https://github.com/cran/ggDoubleHeat
* Date/Publication: 2023-08-24 21:00:04 UTC
* Number of recursive dependencies: 55

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

# ggeasy

<details>

* Version: 0.1.5
* GitHub: https://github.com/jonocarroll/ggeasy
* Source code: https://github.com/cran/ggeasy
* Date/Publication: 2024-11-03 05:50:02 UTC
* Number of recursive dependencies: 90

Run `revdepcheck::cloud_details(, "ggeasy")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggeasy-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: easy_add_legend_title
    > ### Title: Easily add legend title(s)
    > ### Aliases: easy_add_legend_title
    > 
    > ### ** Examples
    > 
    > 
    ...
    > # Add legend title to a specific aesthetic
    > ggplot(mtcars, aes(wt, mpg, colour = cyl, size = hp)) +
    +   geom_point() + easy_add_legend_title(col = "Number of Cylinders")
    > 
    > # Add legend title to all aesthetics
    > ggplot(mtcars, aes(wt, mpg, colour = cyl)) +
    +   geom_point() + easy_add_legend_title("Number of Cylinders")
    Error: <ggplot2::labels> object is invalid:
    - labels cannot contain duplicate names (colour, linetype, shape, and size).
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggeasy)
      > 
      > test_check("ggeasy")
      [ FAIL 6 | WARN 0 | SKIP 1 | PASS 151 ]
      
      ══ Skipped tests (1) ═══════════════════════════════════════════════════════════
    ...
       8.         └─S7::validate(object, recursive = !parent_validated)
      
      [ FAIL 6 | WARN 0 | SKIP 1 | PASS 151 ]
      Deleting unused snapshots:
      • labs/labels-attrib.svg
      • labs/labels-manual.svg
      • labs/labels-mytitle.svg
      • labs/labels-y-col.svg
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘shortcuts.Rmd’ using rmarkdown
    ```

*   checking S3 generic/method consistency ... WARNING
    ```
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.easy_labs:
      function(object, plot, object_name)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# ggEDA

<details>

* Version: 0.1.0
* GitHub: https://github.com/CCICB/ggEDA
* Source code: https://github.com/cran/ggEDA
* Date/Publication: 2025-05-07 12:00:02 UTC
* Number of recursive dependencies: 92

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
     31. │                                 └─base::lapply(...)
     32. │                                   └─ggplot2 (local) FUN(X[[i]], ...)
     33. │                                     ├─rlang::inject(self$draw_panel(data, panel_params, coord, !!!params))
     34. │                                     └─self$draw_panel(...)
     35. └─base::.handleSimpleError(...)
     36.   └─rlang (local) h(simpleError(msg, call))
     37.     └─handlers[[1L]](cnd)
     38.       └─cli::cli_abort(...)
     39.         └─rlang::abort(...)
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
      
      [ FAIL 11 | WARN 12 | SKIP 2 | PASS 114 ]
      Error: Test failures
      Execution halted
    ```

# ggedit

<details>

* Version: 0.4.1
* GitHub: https://github.com/yonicd/ggedit
* Source code: https://github.com/cran/ggedit
* Date/Publication: 2024-03-04 14:40:02 UTC
* Number of recursive dependencies: 91

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
    Error in as.vector(x, "list") : 
      cannot coerce type 'object' to vector of type 'list'
    Calls: compare ... themeFetchFull -> lapply -> as.list -> as.list.default
    Execution halted
    ```

# ggfields

<details>

* Version: 0.0.6
* GitHub: https://github.com/pepijn-devries/ggfields
* Source code: https://github.com/cran/ggfields
* Date/Publication: 2024-02-26 14:40:03 UTC
* Number of recursive dependencies: 100

Run `revdepcheck::cloud_details(, "ggfields")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggfields-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: angle_correction
    > ### Title: Calculate correction for angle in the plot coordinate system
    > ### Aliases: angle_correction
    > 
    > ### ** Examples
    > 
    > ## Create a data.frame with some xy-coordinates and all angles pointing North (0 degrees)
    ...
    +       default_crs = 4326
    +     )
    +   )
    > 
    > ## When plotting as lon-lat, the angle correction will be zero
    > angle_correction(d, params_mockup, ggplot2::coord_sf(default_crs = 4326))
    Error in panel_params$guides$get_guide(aesthetics) : 
      attempt to apply non-function
    Calls: angle_correction ... train_panel_guides -> <Anonymous> -> train_panel_guides
    Execution halted
    ```

# ggfixest

<details>

* Version: 0.3.0
* GitHub: https://github.com/grantmcdermott/ggfixest
* Source code: https://github.com/cran/ggfixest
* Date/Publication: 2025-05-14 02:20:02 UTC
* Number of recursive dependencies: 77

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
* Number of recursive dependencies: 52

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

*   checking S3 generic/method consistency ... WARNING
    ```
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.ggfocus_fill:
      function(object, plot, object_name)
    
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.ggfocus_shape:
      function(object, plot, object_name)
    
    ...
      function(object, plot, ...)
    ggplot_add.ggfocus_size:
      function(object, plot, object_name)
    
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.ggfocus_linetype:
      function(object, plot, object_name)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# ggforce

<details>

* Version: 0.4.2
* GitHub: https://github.com/thomasp85/ggforce
* Source code: https://github.com/cran/ggforce
* Date/Publication: 2024-02-19 11:00:02 UTC
* Number of recursive dependencies: 67

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
    Error in get(x, envir = ns, inherits = FALSE) : 
      object 'check_labeller' not found
    Calls: facet_matrix -> <Anonymous> -> get
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 27.3Mb
      sub-directories of 1Mb or more:
        R      1.0Mb
        help   1.2Mb
        libs  25.0Mb
    ```

# ggformula

<details>

* Version: 0.12.0
* GitHub: https://github.com/ProjectMOSAIC/ggformula
* Source code: https://github.com/cran/ggformula
* Date/Publication: 2023-11-09 12:30:07 UTC
* Number of recursive dependencies: 119

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
* Number of recursive dependencies: 121

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
* Number of recursive dependencies: 59

Run `revdepcheck::cloud_details(, "ggfun")` for more info

</details>

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.facet_set:
      function(object, plot, object_name)
    
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.scatter_rect:
      function(object, plot, object_name)
    
    ...
      function(object, plot, ...)
    ggplot_add.segmentC:
      function(object, plot, object_name)
    
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.volpoint:
      function(object, plot, object_name)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# ggfx

<details>

* Version: 1.0.1
* GitHub: https://github.com/thomasp85/ggfx
* Source code: https://github.com/cran/ggfx
* Date/Publication: 2022-08-22 08:00:06 UTC
* Number of recursive dependencies: 62

Run `revdepcheck::cloud_details(, "ggfx")` for more info

</details>

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
    ggplot_gtable:
      function(plot)
    ggplot_gtable.filtered_gtable:
      function(data)
    
    ggplot_build:
      function(plot, ...)
    ggplot_build.filtered_ggplot:
      function(plot)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) render_context.Rd:42: Lost braces; missing escapes or markup?
        42 | \item{xmin, ymin, xmax, ymax}{Boundaries of the area in pixels. {0,0} is the
           |                                                                 ^
    ```

# gggenomes

<details>

* Version: 1.0.1
* GitHub: https://github.com/thackl/gggenomes
* Source code: https://github.com/cran/gggenomes
* Date/Publication: 2024-08-30 11:40:02 UTC
* Number of recursive dependencies: 109

Run `revdepcheck::cloud_details(, "gggenomes")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gggenomes-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: add_feats
    > ### Title: Add different types of tracks
    > ### Aliases: add_feats add_links add_subfeats add_sublinks add_clusters
    > ###   add_tracks
    > 
    > ### ** Examples
    > 
    ...
    +   add_feats(repeats = emale_tirs) +
    +   geom_seq() + geom_feat()
    Warning: The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
    ℹ Please use the `linewidth` argument instead.
    ℹ The deprecated feature was likely used in the gggenomes package.
      Please report the issue at <https://github.com/thackl/gggenomes/issues>.
    Error in UseMethod("add_feats") : 
      no applicable method for 'add_feats' applied to an object of class "NULL"
    Calls: %>% -> add_feats
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘emales.Rmd’ using rmarkdown
    --- finished re-building ‘emales.Rmd’
    
    --- re-building ‘flip.Rmd’ using rmarkdown
    
    Quitting from flip.Rmd:16-44 [unnamed-chunk-2]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `UseMethod()`:
    ...
    
    Error: processing vignette 'gggenomes.Rmd' failed with diagnostics:
    no applicable method for 'track_info' applied to an object of class "NULL"
    --- failed re-building ‘gggenomes.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘flip.Rmd’ ‘gggenomes.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ggghost

<details>

* Version: 0.2.2
* GitHub: https://github.com/jonocarroll/ggghost
* Source code: https://github.com/cran/ggghost
* Date/Publication: 2025-04-15 05:50:07 UTC
* Number of recursive dependencies: 46

Run `revdepcheck::cloud_details(, "ggghost")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggghost-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: -.gg
    > ### Title: Remove a call from a ggghost object
    > ### Aliases: -.gg
    > 
    > ### ** Examples
    > 
    > ## create a ggghost object
    ...
    > ## remove the geom_smooth
    > z - geom_smooth()
    > 
    > ## remove the labels
    > ## NOTE: argument must be present and able to be
    > ## evaluated in scope
    > z - labs(TRUE) # works
    Error: <ggplot2::labels> object is invalid:
    - every label must be named.
    Execution halted
    ```

# gghalves

<details>

* Version: 0.1.4
* GitHub: https://github.com/erocoar/gghalves
* Source code: https://github.com/cran/gghalves
* Date/Publication: 2022-11-20 11:40:02 UTC
* Number of recursive dependencies: 49

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
     32. │                                         └─base::lapply(...)
     33. │                                           └─ggplot2 (local) FUN(X[[i]], ...)
     34. │                                             └─ggplot2 (local) apply_fun(cur_data)
     35. │                                               └─ggplot2 (local) fun(x, ...)
     36. └─base::.handleSimpleError(...)
     37.   └─rlang (local) h(simpleError(msg, call))
     38.     └─handlers[[1L]](cnd)
     39.       └─cli::cli_abort(...)
     40.         └─rlang::abort(...)
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
* Number of recursive dependencies: 82

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
     15. │               ├─purrr:::with_indexed_errors(...)
     16. │               │ └─base::withCallingHandlers(...)
     17. │               ├─purrr:::call_with_cleanup(...)
     18. │               └─gghighlight (local) .f(.x[[i]], .y[[i]], ...)
     19. │                 └─gghighlight:::get_default_aes_param(nm, layer$geom, layer$mapping)
     20. └─base::.handleSimpleError(...)
     21.   └─purrr (local) h(simpleError(msg, call))
     22.     └─cli::cli_abort(...)
     23.       └─rlang::abort(...)
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
       25.     └─cli::cli_abort(...)
       26.       └─rlang::abort(...)
      
      [ FAIL 7 | WARN 24 | SKIP 0 | PASS 159 ]
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

*   checking S3 generic/method consistency ... WARNING
    ```
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.gg_highlighter:
      function(object, plot, object_name)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# gghourglass

<details>

* Version: 0.0.2
* GitHub: https://github.com/pepijn-devries/gghourglass
* Source code: https://github.com/cran/gghourglass
* Date/Publication: 2025-04-05 14:20:02 UTC
* Number of recursive dependencies: 96

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
     19. │                 └─vctrs (local) `<fn>`()
     20. │                   ├─x[i = i]
     21. │                   └─grid:::`[.unit`(x = x, i = i)
     22. │                     └─base::stop(...)
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
      > library(gghourglass)
      > 
      > test_check("gghourglass")
      NULL
      [ FAIL 1 | WARN 2 | SKIP 4 | PASS 3 ]
      
    ...
       26. │                           └─base::stop(...)
       27. └─base::.handleSimpleError(...)
       28.   └─rlang (local) h(simpleError(msg, call))
       29.     └─handlers[[1L]](cnd)
       30.       └─cli::cli_abort(...)
       31.         └─rlang::abort(...)
      
      [ FAIL 1 | WARN 2 | SKIP 4 | PASS 3 ]
      Error: Test failures
      Execution halted
    ```

# ggimage

<details>

* Version: 0.3.3
* GitHub: https://github.com/GuangchuangYu/ggimage
* Source code: https://github.com/cran/ggimage
* Date/Publication: 2023-06-19 04:10:02 UTC
* Number of recursive dependencies: 56

Run `revdepcheck::cloud_details(, "ggimage")` for more info

</details>

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.bgimage:
      function(object, plot, object_name)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# ggiraph

<details>

* Version: 0.8.13
* GitHub: https://github.com/davidgohel/ggiraph
* Source code: https://github.com/cran/ggiraph
* Date/Publication: 2025-03-28 10:20:02 UTC
* Number of recursive dependencies: 86

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
     23. │                     └─base::lapply(...)
     24. │                       └─ggplot2 (local) FUN(X[[i]], ...)
     25. │                         ├─rlang::inject(self$draw_panel(data, panel_params, coord, !!!params))
     26. │                         └─self$draw_panel(...)
     27. └─base::.handleSimpleError(...)
     28.   └─rlang (local) h(simpleError(msg, call))
     29.     └─handlers[[1L]](cnd)
     30.       └─cli::cli_abort(...)
     31.         └─rlang::abort(...)
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
       33. │                                 └─base::lapply(...)
       34. │                                   └─ggplot2 (local) FUN(X[[i]], ...)
       35. │                                     ├─rlang::inject(self$draw_panel(data, panel_params, coord, !!!params))
       36. │                                     └─self$draw_panel(...)
       37. └─base::.handleSimpleError(...)
       38.   └─rlang (local) h(simpleError(msg, call))
       39.     └─handlers[[1L]](cnd)
       40.       └─cli::cli_abort(...)
       41.         └─rlang::abort(...)
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
* Number of recursive dependencies: 107

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
     24. │                       └─base::lapply(...)
     25. │                         └─ggplot2 (local) FUN(X[[i]], ...)
     26. │                           ├─rlang::inject(self$draw_panel(data, panel_params, coord, !!!params))
     27. │                           └─self$draw_panel(...)
     28. └─base::.handleSimpleError(...)
     29.   └─rlang (local) h(simpleError(msg, call))
     30.     └─handlers[[1L]](cnd)
     31.       └─cli::cli_abort(...)
     32.         └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ggPredict.Rmd’ using rmarkdown
    ```

# gglogger

<details>

* Version: 0.1.5
* GitHub: https://github.com/pwwang/gglogger
* Source code: https://github.com/cran/gglogger
* Date/Publication: 2024-10-25 09:10:02 UTC
* Number of recursive dependencies: 42

Run `revdepcheck::cloud_details(, "gglogger")` for more info

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
      y[1]:  displ, y = hwy))\n"
      ── Failure ('test-gglogger.R:124:5'): gglogger stringify works ─────────────────
      `code` not equal to "ggplot2::ggplot(ggplot2::mpg) +\n  geom_point(aes(x = displ, y = hwy))".
      1/1 mismatches
      x[1]: "ggplot2::ggplot(ggplot2::mpg)"
      y[1]: "ggplot2::ggplot(ggplot2::mpg) +\n  geom_point(aes(x = displ, y = hwy))"
      
      [ FAIL 16 | WARN 0 | SKIP 0 | PASS 38 ]
      Error: Test failures
      Execution halted
    ```

# ggmap

<details>

* Version: 4.0.1
* GitHub: https://github.com/dkahle/ggmap
* Source code: https://github.com/cran/ggmap
* Date/Publication: 2025-04-07 19:40:02 UTC
* Number of recursive dependencies: 62

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
* Number of recursive dependencies: 119

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
* Number of recursive dependencies: 73

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
* Number of recursive dependencies: 123

Run `revdepcheck::cloud_details(, "ggmulti")` for more info

</details>

## Newly broken

*   checking whether package ‘ggmulti’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ggmulti/new/ggmulti.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ggmulti’ ...
** package ‘ggmulti’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in get(x, envir = ns, inherits = FALSE) : 
  object 'new_aes' not found
Error: unable to load R code in package ‘ggmulti’
Execution halted
ERROR: lazy loading failed for package ‘ggmulti’
* removing ‘/tmp/workdir/ggmulti/new/ggmulti.Rcheck/ggmulti’


```
### CRAN

```
* installing *source* package ‘ggmulti’ ...
** package ‘ggmulti’ successfully unpacked and MD5 sums checked
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
* DONE (ggmulti)


```
# ggnewscale

<details>

* Version: 0.5.1
* GitHub: https://github.com/eliocamp/ggnewscale
* Source code: https://github.com/cran/ggnewscale
* Date/Publication: 2025-02-24 09:00:02 UTC
* Number of recursive dependencies: 58

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
       16.             └─scales:::as_discrete_pal.default(elem)
       17.               └─cli::cli_abort("Cannot convert {.arg x} to a discrete palette.")
       18.                 └─rlang::abort(...)
      
      [ FAIL 1 | WARN 0 | SKIP 5 | PASS 3 ]
      Deleting unused snapshots:
      • newscale/guides-outisde-of-scales.svg
      • newscale/respects-override-aes-2.svg
      Error: Test failures
      Execution halted
    ```

*   checking S3 generic/method consistency ... WARNING
    ```
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.new_aes:
      function(object, plot, object_name)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# ggpackets

<details>

* Version: 0.2.1
* GitHub: https://github.com/dgkf/ggpackets
* Source code: https://github.com/cran/ggpackets
* Date/Publication: 2022-10-10 23:30:02 UTC
* Number of recursive dependencies: 72

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
    Calls: Ops.S7_object ... withOneRestart -> doWithOneRestart -> signalCondition -> <Anonymous>
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘composing-functions.Rmd’ using rmarkdown
    ```

*   checking S3 generic/method consistency ... WARNING
    ```
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.ggpacket:
      function(object, plot, object_name)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# ggparallel

<details>

* Version: 0.4.0
* GitHub: https://github.com/heike/ggparallel
* Source code: https://github.com/cran/ggparallel
* Date/Publication: 2024-03-09 22:00:02 UTC
* Number of recursive dependencies: 47

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
          ▆
       1. ├─ggplot2::ggplot_build(test_mtcars_plot) at test-ggparallel.R:12:3
       2. ├─ggplot2:::ggplot_build.default(test_mtcars_plot)
       3. │ └─ggplot2::build_ggplot(plot)
       4. │   └─S7::S7_dispatch()
       5. └─S7:::method_lookup_error("build_ggplot", `<list>`)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

# ggpath

<details>

* Version: 1.0.2
* GitHub: https://github.com/mrcaseb/ggpath
* Source code: https://github.com/cran/ggpath
* Date/Publication: 2024-08-20 09:30:02 UTC
* Number of recursive dependencies: 64

Run `revdepcheck::cloud_details(, "ggpath")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggpath-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: element_path
    > ### Title: Theme Elements for Image Grobs
    > ### Aliases: element_path element_raster
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
    +   theme(
    +     plot.caption = element_path(hjust = 1, size = 0.6),
    +     axis.text.y = element_path(size = 1),
    +     axis.title.x = element_path(),
    +     axis.title.y = element_path(vjust = 0.9),
    +     plot.title = element_path(hjust = 0, size = 2, alpha = 0.5),
    +     plot.subtitle = element_path(hjust = 0.9, angle = 45),
    +   )
    Error: `object` must be an <S7_object>, not a S3<element_path/element_text/element>
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggpath)
      > 
      > test_check("ggpath")
      [ FAIL 1 | WARN 0 | SKIP 5 | PASS 3 ]
      
      ══ Skipped tests (5) ═══════════════════════════════════════════════════════════
    ...
      • geom_lines/p1.svg
      • geom_lines/p2.svg
      • geom_lines/p3.svg
      • geom_lines/p4.svg
      • geom_lines/p5.svg
      • theme-elements/p1.svg
      • theme-elements/p2.svg
      • theme-elements/p3.svg
      Error: Test failures
      Execution halted
    ```

# ggpedigree

<details>

* Version: 0.7.0
* GitHub: https://github.com/R-Computing-Lab/ggpedigree
* Source code: https://github.com/cran/ggpedigree
* Date/Publication: 2025-06-10 00:10:02 UTC
* Number of recursive dependencies: 146

Run `revdepcheck::cloud_details(, "ggpedigree")` for more info

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
      ── Failure ('test-ggPedigreeInteractive.R:279:3'): ggPedigreeInteractive handles inbreeding ──
      `p_widget_nozyg` inherits from 'ggplot2::ggplot'/'ggplot'/'ggplot2::gg'/'S7_object'/'gg' not 'htmlwidget'.
      ── Failure ('test-ggPedigreeInteractive.R:294:3'): ggPedigreeInteractive handles inbreeding ──
      `p_nozyg` inherits from 'ggplot2::ggplot'/'ggplot'/'ggplot2::gg'/'S7_object'/'gg' not 'plotly'.
      ── Failure ('test-ggPedigreeInteractive.R:295:3'): ggPedigreeInteractive handles inbreeding ──
      `p_nozyg` inherits from 'ggplot2::ggplot'/'ggplot'/'ggplot2::gg'/'S7_object'/'gg' not 'htmlwidget'.
      
      [ FAIL 16 | WARN 14 | SKIP 0 | PASS 281 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘v00_plots.Rmd’ using rmarkdown
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘OpenMx’
    ```

# ggplot2.utils

<details>

* Version: 0.3.2
* GitHub: https://github.com/insightsengineering/ggplot2.utils
* Source code: https://github.com/cran/ggplot2.utils
* Date/Publication: 2024-06-25 21:10:02 UTC
* Number of recursive dependencies: 70

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

# ggPMX

<details>

* Version: 1.2.11
* GitHub: https://github.com/ggPMXdevelopment/ggPMX
* Source code: https://github.com/cran/ggPMX
* Date/Publication: 2023-11-30 16:10:06 UTC
* Number of recursive dependencies: 173

Run `revdepcheck::cloud_details(, "ggPMX")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ggPMX-guide.Rmd’ using rmarkdown
    
    Quitting from ggPMX-guide.Rmd:24-37 [load_package]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `+.gg`:
    ! 'ggmatrix' does not know how to add objects that do not have class 'theme', 'labels' or 'ggproto'. Received object with class: 'character'
    ---
    Backtrace:
    ...
    
    Error: processing vignette 'ggPMX-guide.Rmd' failed with diagnostics:
    'ggmatrix' does not know how to add objects that do not have class 'theme', 'labels' or 'ggproto'. Received object with class: 'character'
    --- failed re-building ‘ggPMX-guide.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ggPMX-guide.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘lixoftConnectors’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  9.0Mb
      sub-directories of 1Mb or more:
        doc        1.1Mb
        help       2.4Mb
        testdata   4.8Mb
    ```

# ggpol

<details>

* Version: 0.0.7
* GitHub: https://github.com/erocoar/ggpol
* Source code: https://github.com/cran/ggpol
* Date/Publication: 2020-11-08 13:40:02 UTC
* Number of recursive dependencies: 51

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
     24. │                         └─ggpol (local) draw_panel(...)
     25. │                           └─base::lapply(GeomText$default_aes[missing_aes], rlang::eval_tidy)
     26. │                             └─rlang (local) FUN(X[[i]], ...)
     27. ├─ggplot2::from_theme(fontsize)
     28. └─base::.handleSimpleError(...)
     29.   └─rlang (local) h(simpleError(msg, call))
     30.     └─handlers[[1L]](cnd)
     31.       └─cli::cli_abort(...)
     32.         └─rlang::abort(...)
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

# ggprism

<details>

* Version: 1.0.6
* GitHub: https://github.com/csdaw/ggprism
* Source code: https://github.com/cran/ggprism
* Date/Publication: 2025-05-17 10:50:02 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "ggprism")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggprism-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: annotation_ticks
    > ### Title: Add ticks as ggplot annotation
    > ### Aliases: annotation_ticks
    > 
    > ### ** Examples
    > 
    > ## Generally it is better to use the guide_prism_minor function.
    ...
      Please report the issue at <https://github.com/csdaw/ggprism/issues>.
    Error in `parent %+replace% t`:
    ! `%+replace%` requires two theme objects
    Backtrace:
        ▆
     1. └─ggprism::theme_prism(border = TRUE)
     2.   └─parent %+replace% t
     3.     └─cli::cli_abort("{.code %+replace%} requires two theme objects")
     4.       └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘tinytest.R’
    Running the tests in ‘tests/tinytest.R’ failed.
    Complete output:
      > 
      > if ( requireNamespace("tinytest", quietly=TRUE) ){
      +   tinytest::test_package("ggprism")
      + }
      
      test-add_pvalue.R.............    0 tests    
      test-add_pvalue.R.............    0 tests    
    ...
      ℹ It has been replaced by a ggproto system that can be extended. 
      3: The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
      ℹ Please use the `linewidth` argument instead.
      ℹ The deprecated feature was likely used in the ggprism package.
        Please report the issue at <https://github.com/csdaw/ggprism/issues>. 
      4: The `size` argument of `element_rect()` is deprecated as of ggplot2 3.4.0.
      ℹ Please use the `linewidth` argument instead.
      ℹ The deprecated feature was likely used in the ggprism package.
        Please report the issue at <https://github.com/csdaw/ggprism/issues>. 
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘axes.Rmd’ using rmarkdown
    
    Quitting from axes.Rmd:37-48 [unnamed-chunk-2]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'axes.Rmd' failed with diagnostics:
    `%+replace%` requires two theme objects
    ...
    
    Error: processing vignette 'themes.Rmd' failed with diagnostics:
    `%+replace%` requires two theme objects
    --- failed re-building ‘themes.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘axes.Rmd’ ‘colours.Rmd’ ‘ggprism.Rmd’ ‘pvalues.Rmd’ ‘themes.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ggpubr

<details>

* Version: 0.6.0
* GitHub: https://github.com/kassambara/ggpubr
* Source code: https://github.com/cran/ggpubr
* Date/Publication: 2023-02-10 16:20:02 UTC
* Number of recursive dependencies: 88

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

# ggquickeda

<details>

* Version: 0.3.1
* GitHub: https://github.com/smouksassi/ggquickeda
* Source code: https://github.com/cran/ggquickeda
* Date/Publication: 2024-01-15 10:20:02 UTC
* Number of recursive dependencies: 174

Run `revdepcheck::cloud_details(, "ggquickeda")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggquickeda-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggcontinuousexpdist
    > ### Title: Create a continuous exposure fit plot
    > ### Aliases: ggcontinuousexpdist
    > 
    > ### ** Examples
    > 
    > # Example 1
    ...
    Joining with `by = join_by(loopvariable, DOSE, quant_25)`
    Joining with `by = join_by(loopvariable, DOSE, quant_75)`
    Joining with `by = join_by(loopvariable, DOSE, medexp)`
    > a / b +
    + plot_layout(guides = "collect") &
    +  theme(legend.position = "top")
    Error: Can't find method for generic `&(e1, e2)`:
    - e1: <patchwork>
    - e2: <theme>
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DT’ ‘Formula’ ‘GGally’ ‘Hmisc’ ‘RPostgres’ ‘colourpicker’ ‘ggpmisc’
      ‘ggpubr’ ‘glue’ ‘gridExtra’ ‘markdown’ ‘patchwork’ ‘plotly’
      ‘quantreg’ ‘shinyFiles’ ‘shinyjqui’ ‘shinyjs’ ‘table1’ ‘zoo’
      All declared Imports should be used.
    ```

# ggraph

<details>

* Version: 2.2.1
* GitHub: https://github.com/thomasp85/ggraph
* Source code: https://github.com/cran/ggraph
* Date/Publication: 2024-03-07 12:40:02 UTC
* Number of recursive dependencies: 113

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
      unused argument (list(<object>, <object>, <object>, <object>, <object>, <object>, <object>, 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, <object>, <object>, <object>, NULL, <object>, NULL, <object>, <object>, <object>, <object>, NULL, <object>, NULL, <object>, NULL, <object>, <object>, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.5, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, <object>, NULL, NULL, NULL, NULL, NULL, NULL, 
        NULL, NULL, <object>, NULL, 2, NULL, NULL, NULL, 1.2, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.2, NULL, <object>, NULL, <object>, NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0, <object>, 2, <object>, <object>, NULL, NULL, NULL, <object>, NULL, <object>, NULL, NULL, NULL, NULL, FALSE, NULL, NULL, <object>, <object>, "panel", <object>, <object>, "panel", <object>, "topleft", NULL, NULL, <object>, NULL, NULL, "on", "
    Calls: <Anonymous> ... <Anonymous> -> <Anonymous> -> process_layers -> <Anonymous>
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Edges.Rmd’ using rmarkdown
    ```

*   checking S3 generic/method consistency ... WARNING
    ```
    ggplot_build:
      function(plot, ...)
    ggplot_build.ggraph:
      function(plot)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
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
* Number of recursive dependencies: 64

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
     25. │                           └─coord$transform(data, panel_params)
     26. │                             └─ggplot2 (local) transform(..., self = self)
     27. │                               └─ggplot2:::sf_rescale01(data[[geom_column(data)]], x_range, y_range)
     28. │                                 └─sf::st_normalize(x, c(x_range[1], y_range[1], x_range[2], y_range[2]))
     29. └─base::.handleSimpleError(...)
     30.   └─rlang (local) h(simpleError(msg, call))
     31.     └─handlers[[1L]](cnd)
     32.       └─cli::cli_abort(...)
     33.         └─rlang::abort(...)
    Execution halted
    ```

# ggResidpanel

<details>

* Version: 0.3.0
* GitHub: NA
* Source code: https://github.com/cran/ggResidpanel
* Date/Publication: 2019-05-31 23:20:04 UTC
* Number of recursive dependencies: 111

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
       42. │                                                 └─ggplot2 (local) draw_group(..., self = self)
       43. └─base::.handleSimpleError(...)
       44.   └─rlang (local) h(simpleError(msg, call))
       45.     └─handlers[[1L]](cnd)
       46.       └─cli::cli_abort(...)
       47.         └─rlang::abort(...)
      
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
* Number of recursive dependencies: 65

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

# ggseas

<details>

* Version: 0.5.4
* GitHub: https://github.com/ellisp/ggseas
* Source code: https://github.com/cran/ggseas
* Date/Publication: 2018-06-12 13:33:33 UTC
* Number of recursive dependencies: 62

Run `revdepcheck::cloud_details(, "ggseas")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggseas)
      Loading required package: ggplot2
      > 
      > test_check("ggseas")
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 9 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-tsdf.R:22:4'): ggplot can draw graphic with tsdf output ──────
      class(tmp2) not equal to c("gg", "ggplot").
      Lengths differ: 5 is not 2
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 9 ]
      Error: Test failures
      Execution halted
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
    
    > ### Name: ggseqrfplot
    > ### Title: Relative Frequency Sequence Plot
    > ### Aliases: ggseqrfplot
    > 
    > ### ** Examples
    > 
    > # Load additional library for fine-tuning the plots
    ...
    > 
    > # ... with ggseqrfplot
    > ggseqrfplot(biofam.seq, weighted = FALSE, diss = diss, k = 12, grp.meth="first")
     [>] Using k=12 frequency groups with grp.meth='first'
     [>] Pseudo/medoid-based-R2: 0.4620155
     [>] Pseudo/medoid-based-F statistic: 6.870317, p-value: 3.09994e-08
    Error: Can't find method for generic `&(e1, e2)`:
    - e1: <patchwork>
    - e2: <theme>
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
      
      [ FAIL 3 | WARN 16 | SKIP 0 | PASS 117 ]
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
* Number of recursive dependencies: 72

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
     14.           │ └─base::withCallingHandlers(...)
     15.           └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     16.             └─l$compute_statistic(d, layout)
     17.               └─ggside (local) compute_statistic(..., self = self)
     18.                 └─ggplot2 (local) ggproto_parent_method(self = self, data = data, layout = layout)
     19.                   └─self$stat$setup_params(data, self$stat_params)
     20.                     └─ggplot2 (local) setup_params(..., self = self)
     21.                       └─cli::cli_abort("{.fn {snake_class(self)}} must only have an {.field x} {.emph or} {.field y} aesthetic.")
     22.                         └─rlang::abort(...)
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

*   checking S3 generic/method consistency ... WARNING
    ```
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.ggside_scale:
      function(object, plot, object_name)
    
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.ggside_layer:
      function(object, plot, object_name)
    
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.ggside_options:
      function(object, plot, object_name)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
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

# ggspatial

<details>

* Version: 1.1.9
* GitHub: https://github.com/paleolimbot/ggspatial
* Source code: https://github.com/cran/ggspatial
* Date/Publication: 2023-08-17 15:32:38 UTC
* Number of recursive dependencies: 104

Run `revdepcheck::cloud_details(, "ggspatial")` for more info

</details>

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.fixed_plot_aspect:
      function(object, plot, object_name)
    
    ggplot_build:
      function(plot, ...)
    ggplot_build.gg_fixed_plot_aspect:
      function(plot)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# ggstatsplot

<details>

* Version: 0.13.1
* GitHub: https://github.com/IndrajeetPatil/ggstatsplot
* Source code: https://github.com/cran/ggstatsplot
* Date/Publication: 2025-05-09 23:40:02 UTC
* Number of recursive dependencies: 172

Run `revdepcheck::cloud_details(, "ggstatsplot")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # graphics engine changed in this version, and so snapshots generated on
      > # previous R version won't work
      > if (getRversion() >= "4.4.0") {
      +   library(testthat)
      +   suppressPackageStartupMessages(library(ggstatsplot))
      +   test_check("ggstatsplot")
      + }
    ...
      • pairwise-ggsignif/within-non-parametric-all.svg
      • pairwise-ggsignif/within-non-parametric-only-non-significant.svg
      • pairwise-ggsignif/within-non-parametric-only-significant.svg
      • pairwise-ggsignif/within-parametric-all.svg
      • pairwise-ggsignif/within-parametric-only-significant.svg
      • pairwise-ggsignif/within-robust-all.svg
      • pairwise-ggsignif/within-robust-only-non-significant.svg
      • pairwise-ggsignif/within-robust-only-significant.svg
      Error: Test failures
      Execution halted
    ```

# ggstream

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/ggstream
* Date/Publication: 2021-05-06 07:50:03 UTC
* Number of recursive dependencies: 51

Run `revdepcheck::cloud_details(, "ggstream")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggstream)
      > 
      > test_check("ggstream")
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-geom_stream.R:4:3'): geom_stream ─────────────────────────────
      class(...) not equal to c("gg", "ggplot").
      Lengths differ: 5 is not 2
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

# ggswissmaps

<details>

* Version: 0.1.1
* GitHub: https://github.com/gibonet/ggswissmaps
* Source code: https://github.com/cran/ggswissmaps
* Date/Publication: 2016-10-29 10:48:24
* Number of recursive dependencies: 68

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

# ggtangle

<details>

* Version: 0.0.6
* GitHub: NA
* Source code: https://github.com/cran/ggtangle
* Date/Publication: 2024-12-18 14:30:06 UTC
* Number of recursive dependencies: 76

Run `revdepcheck::cloud_details(, "ggtangle")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ggtangle.Rmd’ using rmarkdown
    ```

*   checking S3 generic/method consistency ... WARNING
    ```
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.cnet_label:
      function(object, plot, object_name)
    
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.layer_edge:
      function(object, plot, object_name)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# ggtern

<details>

* Version: 3.5.0
* GitHub: NA
* Source code: https://github.com/cran/ggtern
* Date/Publication: 2024-03-24 21:50:02 UTC
* Number of recursive dependencies: 39

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
# ggtext

<details>

* Version: 0.1.2
* GitHub: https://github.com/wilkelab/ggtext
* Source code: https://github.com/cran/ggtext
* Date/Publication: 2022-09-16 11:36:07 UTC
* Number of recursive dependencies: 76

Run `revdepcheck::cloud_details(, "ggtext")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘plotting_text.Rmd’ using rmarkdown
    ```

# ggtibble

<details>

* Version: 1.0.1
* GitHub: NA
* Source code: https://github.com/cran/ggtibble
* Date/Publication: 2024-06-19 12:50:02 UTC
* Number of recursive dependencies: 72

Run `revdepcheck::cloud_details(, "ggtibble")` for more info

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
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. ├─ggtibble::new_gglist(list(ggplot2::labs("foo")))
       5. └─ggplot2::labs("foo")
       6.   └─ggplot2::class_labels(args)
       7.     └─S7::new_object(labels)
       8.       └─S7::validate(object, recursive = !parent_validated)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 59 ]
      Error: Test failures
      Execution halted
    ```

# ggtime

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/ggtime
* Date/Publication: 2025-06-10 09:10:02 UTC
* Number of recursive dependencies: 70

Run `revdepcheck::cloud_details(, "ggtime")` for more info

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
       1. ├─ggplot2::ggplot_build(p[[1]]) at test-plot-helpers.R:192:3
       2. ├─ggplot2:::ggplot_build.default(p[[1]])
       3. │ └─ggplot2::build_ggplot(plot)
       4. │   └─S7::S7_dispatch()
       5. └─S7:::method_lookup_error("build_ggplot", `<named list>`)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 32 ]
      Error: Test failures
      Execution halted
    ```

# ggVennDiagram

<details>

* Version: 1.5.2
* GitHub: https://github.com/gaospecial/ggVennDiagram
* Source code: https://github.com/cran/ggVennDiagram
* Date/Publication: 2024-02-20 08:10:02 UTC
* Number of recursive dependencies: 94

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

# ggview

<details>

* Version: 0.2.1
* GitHub: https://github.com/idmn/ggview
* Source code: https://github.com/cran/ggview
* Date/Publication: 2024-10-02 17:00:10 UTC
* Number of recursive dependencies: 44

Run `revdepcheck::cloud_details(, "ggview")` for more info

</details>

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.canvas:
      function(object, plot, object_name)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# grafify

<details>

* Version: 5.0.0.1
* GitHub: https://github.com/ashenoy-cmbi/grafify
* Source code: https://github.com/cran/grafify
* Date/Publication: 2025-03-10 22:50:02 UTC
* Number of recursive dependencies: 111

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

# graphPAF

<details>

* Version: 2.0.0
* GitHub: https://github.com/johnfergusonNUIG/graphPAF
* Source code: https://github.com/cran/graphPAF
* Date/Publication: 2023-12-21 00:50:06 UTC
* Number of recursive dependencies: 47

Run `revdepcheck::cloud_details(, "graphPAF")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘graphPAF-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.SAF_summary
    > ### Title: Produce plots of sequential and average PAF
    > ### Aliases: plot.SAF_summary
    > 
    > ### ** Examples
    > 
    > library(splines)
    ...
    + riskfactor_vec = c("urban.rural","occupational.exposure"),ci=FALSE)
    > plot(out)
    Warning in fortify(data, ...) : Arguments in `...` must be used.
    ✖ Problematic argument:
    • size = point.size
    ℹ Did you misspell an argument name?
    Error: Can't find method for generic `+(e1, e2)`:
    - e1: <ggplot2::element_text>
    - e2: <theme>
    Execution halted
    ```

# gratia

<details>

* Version: 0.10.0
* GitHub: https://github.com/gavinsimpson/gratia
* Source code: https://github.com/cran/gratia
* Date/Publication: 2024-12-19 19:10:02 UTC
* Number of recursive dependencies: 153

Run `revdepcheck::cloud_details(, "gratia")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gratia-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: appraise
    > ### Title: Model diagnostic plots
    > ### Aliases: appraise appraise.gam appraise.lm
    > 
    > ### ** Examples
    > 
    > load_mgcv()
    ...
    > ## change the ggplot theme for all panels
    > library("ggplot2")
    > appraise(mod, seed = 42,
    +   point_col = "steelblue", point_alpha = 0.4,
    +   line_col = "black"
    + ) & theme_minimal()
    Error: Can't find method for generic `&(e1, e2)`:
    - e1: <patchwork>
    - e2: <theme>
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Complete output:
      > ## Test `gratia` using the `testthat` package
      > 
      > ## Setup
      > library("testthat")
      > library("gratia")
      > 
      > ## Runs the tests in tests/testthat
    ...
      • penalty/draw-penalty-single-smooths-user-continuous-fill.svg
      • rootograms/draw-gaussian-rootogram.svg
      • rootograms/draw-neg-bin-rootogram.svg
      • rootograms/draw-neg-bin-sqrt-rootogram.svg
      • rootograms/draw-neg-bin-standing-rootogram.svg
      • rootograms/draw-neg-bin-suspended-rootogram.svg
      • soap-films/draw-gam-so-soap-film.svg
      • soap-films/draw-smooth-estimates-so-soap-film.svg
      Error: Test failures
      Execution halted
    ```

# Greymodels

<details>

* Version: 2.0.1
* GitHub: https://github.com/havishaJ/Greymodels
* Source code: https://github.com/cran/Greymodels
* Date/Publication: 2022-12-05 12:42:35 UTC
* Number of recursive dependencies: 86

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

# GRShiny

<details>

* Version: 1.0.0
* GitHub: https://github.com/sooyongl/GRShiny
* Source code: https://github.com/cran/GRShiny
* Date/Publication: 2023-05-03 18:40:06 UTC
* Number of recursive dependencies: 152

Run `revdepcheck::cloud_details(, "GRShiny")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(GRShiny)
      > 
      > test_check("GRShiny")
      
      F1 =~ NA*y1+l1*y1+l2*y2
      
    ...
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-gendata.r:73:3'): Plotting functions work ────────────────────
      `res` is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 6 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) runGRM.Rd:24: Lost braces
        24 |    from \code{\link{mirt}} or code{\linkS4class{lavaan}} from from
           |                                   ^
    ```

# grwat

<details>

* Version: 0.0.4
* GitHub: https://github.com/tsamsonov/grwat
* Source code: https://github.com/cran/grwat
* Date/Publication: 2023-10-27 11:40:12 UTC
* Number of recursive dependencies: 97

Run `revdepcheck::cloud_details(, "grwat")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > suppressPackageStartupMessages(library(grwat))
      > 
      > test_check("grwat")
      [ FAIL 7 | WARN 4 | SKIP 1 | PASS 165 ]
      
      ══ Skipped tests (1) ═══════════════════════════════════════════════════════════
    ...
      ── Failure ('test-gr_plot_matrix.R:17:3'): Matrix ggplot has the correct content ──
      `plt` has type 'object', not 'list'.
      ── Failure ('test-gr_plot_ridge.R:9:3'): Ridgeline plot has the correct content ──
      `plt` has type 'object', not 'list'.
      ── Failure ('test-gr_plot_tests.R:10:3'): Tests plot has the correct content ───
      `plt` has type 'object', not 'list'.
      
      [ FAIL 7 | WARN 4 | SKIP 1 | PASS 165 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        doc    1.6Mb
        libs   2.6Mb
    ```

# gtExtras

<details>

* Version: 0.6.0
* GitHub: https://github.com/jthomasmock/gtExtras
* Source code: https://github.com/cran/gtExtras
* Date/Publication: 2025-05-29 04:50:02 UTC
* Number of recursive dependencies: 105

Run `revdepcheck::cloud_details(, "gtExtras")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(gtExtras)
      Loading required package: gt
      
      Attaching package: 'gt'
      
      The following object is masked from 'package:testthat':
    ...
       44. │                                                     └─base::stop(...)
       45. └─base::.handleSimpleError(...)
       46.   └─rlang (local) h(simpleError(msg, call))
       47.     └─handlers[[1L]](cnd)
       48.       └─cli::cli_abort(...)
       49.         └─rlang::abort(...)
      
      [ FAIL 6 | WARN 4 | SKIP 19 | PASS 104 ]
      Error: Test failures
      Execution halted
    ```

# guideR

<details>

* Version: 0.4.0
* GitHub: https://github.com/larmarange/guideR
* Source code: https://github.com/cran/guideR
* Date/Publication: 2025-04-22 12:00:02 UTC
* Number of recursive dependencies: 154

Run `revdepcheck::cloud_details(, "guideR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘guideR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_inertia_from_tree
    > ### Title: Plot inertia, absolute loss and relative loss from a
    > ###   classification tree
    > ### Aliases: plot_inertia_from_tree get_inertia_from_tree
    > ### Keywords: tree
    > 
    > ### ** Examples
    ...
    11    11    38.5        -2.96       -0.0713 
    12    12    36.8        -1.68       -0.0436 
    13    13    36.7        -0.113      -0.00307
    14    14    32.7        -4.02       -0.109  
    15    15    31.6        -1.10       -0.0336 
    > plot_inertia_from_tree(hc)
    Error: Can't find method for generic `&(e1, e2)`:
    - e1: <patchwork>
    - e2: <theme>
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
      ── Failure ('test-plot_inertia_from_tree.R:6:3'): plot_inertia_from_tree() does not produce an error ──
      Expected `plot_inertia_from_tree(hc)` to run without any errors.
      i Actually got a <S7_error_method_not_found> with text:
        Can't find method for generic `&(e1, e2)`:
        - e1: <patchwork>
        - e2: <theme>
      
      [ FAIL 1 | WARN 34 | SKIP 4 | PASS 60 ]
      Error: Test failures
      Execution halted
    ```

# harmony

<details>

* Version: 1.2.3
* GitHub: https://github.com/immunogenomics/harmony
* Source code: https://github.com/cran/harmony
* Date/Publication: 2024-11-27 23:50:02 UTC
* Number of recursive dependencies: 210

Run `revdepcheck::cloud_details(, "harmony")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Seurat.Rmd’ using rmarkdown
    Calculating gene variances
    0%   10   20   30   40   50   60   70   80   90   100%
    [----|----|----|----|----|----|----|----|----|----|
    **************************************************|
    Calculating feature variances of standardized and clipped values
    0%   10   20   30   40   50   60   70   80   90   100%
    [----|----|----|----|----|----|----|----|----|----|
    **************************************************|
    ...
    **************************************************|
    0%   10   20   30   40   50   60   70   80   90   100%
    [----|----|----|----|----|----|----|----|----|----|
    **************************************************|
    0%   10   20   30   40   50   60   70   80   90   100%
    [----|----|----|----|----|----|----|----|----|----|
    **************************************************|
    0%   10   20   30   40   50   60   70   80   90   100%
    [----|----|----|----|----|----|----|----|----|----|
    **************************************************|
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 14.8Mb
      sub-directories of 1Mb or more:
        data   5.0Mb
        doc    2.5Mb
        libs   7.1Mb
    ```

# hatchR

<details>

* Version: 0.3.2
* GitHub: https://github.com/bmait101/hatchR
* Source code: https://github.com/cran/hatchR
* Date/Publication: 2025-03-05 15:40:02 UTC
* Number of recursive dependencies: 91

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
      `plot_geoms` (`actual`) not identical to c("GeomPoint", "GeomLine", "GeomHline", "GeomHline") (`expected`).
      
      `names(actual)` is a character vector ('geom_point', 'geom_line', 'geom_hline', 'geom_hline...4')
      `names(expected)` is absent
      ── Failure ('test-plot_phenology.R:10:3'): plot_phenology works ────────────────
      `p` has type 'object', not 'list'.
      
      [ FAIL 4 | WARN 0 | SKIP 0 | PASS 35 ]
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
* Number of recursive dependencies: 145

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
* Number of recursive dependencies: 203

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
* Number of recursive dependencies: 209

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
* Number of recursive dependencies: 108

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
* Number of recursive dependencies: 103

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

# hrbrthemes

<details>

* Version: 0.8.7
* GitHub: NA
* Source code: https://github.com/cran/hrbrthemes
* Date/Publication: 2024-03-04 00:20:02 UTC
* Number of recursive dependencies: 67

Run `revdepcheck::cloud_details(, "hrbrthemes")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Complete output:
      > library(testthat)
      > test_check("hrbrthemes")
      Loading required package: hrbrthemes
      [ FAIL 1 | WARN 0 | SKIP 2 | PASS 9 ]
      
      ══ Skipped tests (2) ═══════════════════════════════════════════════════════════
      • On CRAN (2): 'test-hrbrthemes.R:71:3', 'test-themes.R:4:3'
    ...
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-hrbrthemes.R:16:3'): we can do something ─────────────────────
      `gg_check(gg_tmp)` did not produce any messages.
      
      [ FAIL 1 | WARN 0 | SKIP 2 | PASS 9 ]
      Deleting unused snapshots:
      • themes/theme-ipsum-rc.svg
      • themes/theme-ipsum.svg
      Error: Test failures
      Execution halted
    ```

# HVT

<details>

* Version: 25.2.4
* GitHub: https://github.com/Mu-Sigma/HVT
* Source code: https://github.com/cran/HVT
* Date/Publication: 2025-06-10 12:40:21 UTC
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

# hyperoverlap

<details>

* Version: 1.1.1
* GitHub: NA
* Source code: https://github.com/cran/hyperoverlap
* Date/Publication: 2021-08-10 08:30:05 UTC
* Number of recursive dependencies: 105

Run `revdepcheck::cloud_details(, "hyperoverlap")` for more info

</details>

## Newly broken

*   checking whether package ‘hyperoverlap’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘e1071::element’ by ‘ggplot2::element’ when loading ‘hyperoverlap’
    See ‘/tmp/workdir/hyperoverlap/new/hyperoverlap.Rcheck/00install.out’ for details.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        doc   6.3Mb
    ```

# hypervolume

<details>

* Version: 3.1.5
* GitHub: https://github.com/bblonder/hypervolume
* Source code: https://github.com/cran/hypervolume
* Date/Publication: 2025-01-17 20:50:16 UTC
* Number of recursive dependencies: 142

Run `revdepcheck::cloud_details(, "hypervolume")` for more info

</details>

## Newly broken

*   checking whether package ‘hypervolume’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘e1071::element’ by ‘ggplot2::element’ when loading ‘hypervolume’
    See ‘/tmp/workdir/hypervolume/new/hypervolume.Rcheck/00install.out’ for details.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        doc    2.8Mb
        libs   2.4Mb
    ```

# hypsoLoop

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/hypsoLoop
* Date/Publication: 2022-02-08 09:00:02 UTC
* Number of recursive dependencies: 93

Run `revdepcheck::cloud_details(, "hypsoLoop")` for more info

</details>

## Newly broken

*   checking whether package ‘hypsoLoop’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::set_theme’ by ‘sjPlot::set_theme’ when loading ‘hypsoLoop’
    See ‘/tmp/workdir/hypsoLoop/new/hypsoLoop.Rcheck/00install.out’ for details.
    ```

# ImHD

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/ImHD
* Date/Publication: 2023-09-12 06:12:44 UTC
* Number of recursive dependencies: 38

Run `revdepcheck::cloud_details(, "ImHD")` for more info

</details>

## Newly broken

*   checking whether package ‘ImHD’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘e1071::element’ by ‘ggplot2::element’ when loading ‘ImHD’
    See ‘/tmp/workdir/ImHD/new/ImHD.Rcheck/00install.out’ for details.
    ```

# implicitMeasures

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/implicitMeasures
* Date/Publication: 2022-02-16 13:40:13 UTC
* Number of recursive dependencies: 83

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
      [ FAIL 7 | WARN 2 | SKIP 2 | PASS 69 ]
      
    ...
        5. └─implicitMeasures::multi_dscore(iat_data, ds = "error-inflation")
        6.   └─ggplot2::geom_violin(draw_quantiles = TRUE)
        7.     └─ggplot2:::check_numeric(draw_quantiles)
        8.       └─ggplot2:::check_object(x, is.numeric, what, ..., arg = arg, call = call)
        9.         └─ggplot2:::stop_input_type(...)
       10.           └─rlang::abort(message, ..., call = call, arg = arg)
      
      [ FAIL 7 | WARN 2 | SKIP 2 | PASS 69 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘IAT-example.Rmd’ using rmarkdown
    ```

# imputeTS

<details>

* Version: 3.3
* GitHub: https://github.com/SteffenMoritz/imputeTS
* Source code: https://github.com/cran/imputeTS
* Date/Publication: 2022-09-09 06:52:55 UTC
* Number of recursive dependencies: 110

Run `revdepcheck::cloud_details(, "imputeTS")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > test_check("imputeTS")
      Loading required package: imputeTS
      [ FAIL 43 | WARN 18 | SKIP 52 | PASS 411 ]
      
      ══ Skipped tests (52) ══════════════════════════════════════════════════════════
      • On CRAN (52): 'test-ggplot_na_distribution.R:59:3',
    ...
      `expected`: TRUE 
      ── Failure ('test-ggplot_na_imputations.R:81:5'): Check that all parameters of plot run without error ──
      is.list(...) is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      
      [ FAIL 43 | WARN 18 | SKIP 52 | PASS 411 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Cheat_Sheet_imputeTS.pdf.asis’ using asis
    --- finished re-building ‘Cheat_Sheet_imputeTS.pdf.asis’
    
    --- re-building ‘imputeTS-Time-Series-Missing-Value-Imputation-in-R.ltx’ using tex
    Error: processing vignette 'imputeTS-Time-Series-Missing-Value-Imputation-in-R.ltx' failed with diagnostics:
    Running 'texi2dvi' on 'imputeTS-Time-Series-Missing-Value-Imputation-in-R.ltx' failed.
    LaTeX errors:
    ! LaTeX Error: File `etex.sty' not found.
    
    ...
    
    ! Emergency stop.
    <read *> 
             
    l.5 \usepackage
                   [utf8]{inputenc}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘imputeTS-Time-Series-Missing-Value-Imputation-in-R.ltx’
    
    --- re-building ‘gallery_visualizations.Rmd’ using rmarkdown
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        doc    1.1Mb
        libs   1.9Mb
    ```

# incidence

<details>

* Version: 1.7.5
* GitHub: https://github.com/reconhub/incidence
* Source code: https://github.com/cran/incidence
* Date/Publication: 2024-05-31 10:10:02 UTC
* Number of recursive dependencies: 65

Run `revdepcheck::cloud_details(, "incidence")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘incidence-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: get_fit
    > ### Title: Accessors for 'incidence_fit' objects
    > ### Aliases: get_fit get_fit.incidence_fit get_fit.incidence_fit_list
    > ###   get_info get_info.incidence_fit get_info.incidence_fit_list
    > 
    > ### ** Examples
    > 
    ...
    + })}
    Loading required package: outbreaks
    > dat <- ebola_sim$linelist$date_of_onset
    > sex <- ebola_sim$linelist$gender
    > i.sex <- incidence(dat, interval = 7, group = sex)
    > fits <- fit_optim_split(i.sex, separate_split = TRUE)
    Error in names(out$plot) <- names(res) : 
      invalid to use names()<- on an S4 object of class 'ggplot2::ggplot'
    Calls: withAutoprint ... source -> withVisible -> eval -> eval -> fit_optim_split
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(incidence)
      > 
      > test_check("incidence")
      [ FAIL 1 | WARN 0 | SKIP 18 | PASS 213 ]
      
      ══ Skipped tests (18) ══════════════════════════════════════════════════════════
    ...
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test-accessors.R:156:1'): (code run outside of `test_that()`) ───────
      Error in `names(out$plot) <- names(res)`: invalid to use names()<- on an S4 object of class 'ggplot2::ggplot'
      Backtrace:
          ▆
       1. └─incidence::fit_optim_split(i.sex.o) at test-accessors.R:156:1
      
      [ FAIL 1 | WARN 0 | SKIP 18 | PASS 213 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘conversions.Rmd’ using rmarkdown
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

*   checking S3 generic/method consistency ... WARNING
    ```
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.infer_layer:
      function(object, plot, object_name)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# inferCSN

<details>

* Version: 1.1.7
* GitHub: https://github.com/mengxu98/inferCSN
* Source code: https://github.com/cran/inferCSN
* Date/Publication: 2025-03-30 17:00:02 UTC
* Number of recursive dependencies: 186

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

# inTextSummaryTable

<details>

* Version: 3.3.3
* GitHub: https://github.com/openanalytics/inTextSummaryTable
* Source code: https://github.com/cran/inTextSummaryTable
* Date/Publication: 2024-06-12 18:30:02 UTC
* Number of recursive dependencies: 109

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
    Error: processing vignette 'inTextSummaryTable-visualization.Rmd' failed with diagnostics:
    The `plot.margin` theme element must be a <unit> vector of length 4
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

* Version: 1.1.2
* GitHub: NA
* Source code: https://github.com/cran/inventorize
* Date/Publication: 2025-05-28 13:20:02 UTC
* Number of recursive dependencies: 68

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
* Number of recursive dependencies: 90

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
      [ FAIL 6 | WARN 3 | SKIP 0 | PASS 91 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
          ▆
       1. ├─base::plot(tm, names(tm)[-1], xlim = c(2000, 2011)) at test_subset.R:44:5
       2. └─iNZightTS:::plot.inz_ts(tm, names(tm)[-1], xlim = c(2000, 2011))
       3.   ├─(function(.) structure(., use.plotly = ggplotable(.)))(...)
       4.   │ └─base::structure(., use.plotly = ggplotable(.))
       5.   └─S7:::Ops.S7_object(...)
      
      [ FAIL 6 | WARN 3 | SKIP 0 | PASS 91 ]
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
* Number of recursive dependencies: 81

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
     19. │                 └─self$stat$setup_params(data, self$stat_params)
     20. │                   └─ggplot2 (local) setup_params(...)
     21. │                     └─base::match.fun(method)
     22. │                       └─base::get(as.character(FUN), mode = "function", envir = envir)
     23. └─base::.handleSimpleError(...)
     24.   └─rlang (local) h(simpleError(msg, call))
     25.     └─handlers[[1L]](cnd)
     26.       └─cli::cli_abort(...)
     27.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.7Mb
      sub-directories of 1Mb or more:
        data   6.5Mb
    ```

# isoorbi

<details>

* Version: 1.3.1
* GitHub: https://github.com/isoverse/isoorbi
* Source code: https://github.com/cran/isoorbi
* Date/Publication: 2024-08-27 05:10:03 UTC
* Number of recursive dependencies: 120

Run `revdepcheck::cloud_details(, "isoorbi")` for more info

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
      ── Failure ('test_plotting_functions.R:107:3'): orbi_plot_raw_data() tests ─────
      orbi_plot_raw_data(df, y = "ratio") has type 'object', not 'list'.
      ── Failure ('test_plotting_functions.R:139:3'): orbi_plot_isotopocule_coverage() tests ──
      `fig` has type 'object', not 'list'.
      ── Failure ('test_shotnoise_functions.R:94:3'): orbi_plot_shot_noise() tests ───
      orbi_plot_shot_noise(df, "time.min") has type 'object', not 'list'.
      
      [ FAIL 5 | WARN 1 | SKIP 0 | PASS 350 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        doc       2.0Mb
        extdata   3.3Mb
    ```

# jskm

<details>

* Version: 0.5.11
* GitHub: https://github.com/jinseob2kim/jstable
* Source code: https://github.com/cran/jskm
* Date/Publication: 2025-03-20 03:10:02 UTC
* Number of recursive dependencies: 108

Run `revdepcheck::cloud_details(, "jskm")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘jskm.Rmd’ using rmarkdown
    ```

# karel

<details>

* Version: 0.1.1
* GitHub: https://github.com/mpru/karel
* Source code: https://github.com/cran/karel
* Date/Publication: 2022-03-26 21:50:02 UTC
* Number of recursive dependencies: 86

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
* Number of recursive dependencies: 132

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

# lares

<details>

* Version: 5.2.13
* GitHub: https://github.com/laresbernardo/lares
* Source code: https://github.com/cran/lares
* Date/Publication: 2025-02-19 15:20:02 UTC
* Number of recursive dependencies: 95

Run `revdepcheck::cloud_details(, "lares")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘lares-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: clusterKmeans
    > ### Title: Automated K-Means Clustering + PCA/t-SNE
    > ### Aliases: clusterKmeans
    > 
    > ### ** Examples
    > 
    > Sys.unsetenv("LARES_FONT") # Temporal
    ...
    > 
    > # If dataset has +5 columns, feel free to reduce dimenstionalities
    > # with reduce_pca() or reduce_tsne() first
    > 
    > # Find optimal k
    > check_k <- clusterKmeans(df, limit = 10)
    >>> Removed duplicate obserations: 1
    Error: <ggplot2::element_text> object properties are invalid:
    - @family must be <NULL> or <character>, not <logical>
    Execution halted
    ```

# latentcor

<details>

* Version: 2.0.1
* GitHub: NA
* Source code: https://github.com/cran/latentcor
* Date/Publication: 2022-09-05 20:50:02 UTC
* Number of recursive dependencies: 141

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
      0.025   0.000   0.024 
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
     26. │                             └─ggplot2 (local) FUN(X[[i]], ...)
     27. │                               └─self$draw_group(group, panel_params, coord, ...)
     28. │                                 └─ggplot2 (local) draw_group(...)
     29. │                                   └─ggplot2 (local) draw_group(..., self = self)
     30. └─base::.handleSimpleError(...)
     31.   └─rlang (local) h(simpleError(msg, call))
     32.     └─handlers[[1L]](cnd)
     33.       └─cli::cli_abort(...)
     34.         └─rlang::abort(...)
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
* Number of recursive dependencies: 84

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
    > 
    > len_frac <- c(0.3, 0.5, 0.2, 0.4, 0.3, 0.2, 0.1, 0.3)
    > n_seg <- c(1, 2, 0, 8)
    > 
    > library(ggplot2)
    > g <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
    +   geom_point() + facet_wrap(~Species, 2) + theme_lcars_light()
    Error: <ggplot2::element_text> object properties are invalid:
    - @family must be <NULL> or <character>, not <double>
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘lcars.Rmd’ using rmarkdown
    
    Quitting from lcars.Rmd:20-54 [ex-1]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error:
    ! <ggplot2::element_text> object properties are invalid:
    - @family must be <NULL> or <character>, not <double>
    ---
    ...
    Error: processing vignette 'lcars.Rmd' failed with diagnostics:
    <ggplot2::element_text> object properties are invalid:
    - @family must be <NULL> or <character>, not <double>
    --- failed re-building ‘lcars.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘lcars.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# legendry

<details>

* Version: 0.2.2
* GitHub: https://github.com/teunbrand/legendry
* Source code: https://github.com/cran/legendry
* Date/Publication: 2025-05-30 09:20:09 UTC
* Number of recursive dependencies: 67

Run `revdepcheck::cloud_details(, "legendry")` for more info

</details>

## Newly broken

*   checking whether the package can be loaded with stated dependencies ... WARNING
    ```
    Loading required package: ggplot2
    Error: package or namespace load failed for ‘legendry’:
     .onLoad failed in loadNamespace() for 'legendry', details:
      call: getFromNamespace("class_mapping", "ggplot2")
      error: could not find function "getFromNamespace"
    Execution halted
    
    It looks like this package (or one of its dependent packages) has an
    unstated dependence on a standard package.  All dependencies must be
    declared in DESCRIPTION.
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

*   checking whether the package can be unloaded cleanly ... WARNING
    ```
    Error: package or namespace load failed for ‘legendry’:
     .onLoad failed in loadNamespace() for 'legendry', details:
      call: getFromNamespace("class_mapping", "ggplot2")
      error: could not find function "getFromNamespace"
    Execution halted
    ```

*   checking whether the namespace can be loaded with stated dependencies ... WARNING
    ```
    Error: .onLoad failed in loadNamespace() for 'legendry', details:
      call: getFromNamespace("class_mapping", "ggplot2")
      error: could not find function "getFromNamespace"
    Execution halted
    
    A namespace must be able to be loaded with just the base namespace
    loaded: otherwise if the namespace gets loaded by a saved object, the
    session will be unable to start.
    
    Probably some imports need to be declared in the NAMESPACE file.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Error: .onLoad failed in loadNamespace() for 'legendry', details:
      call: getFromNamespace("class_mapping", "ggplot2")
      error: could not find function "getFromNamespace"
    Call sequence:
    3: stop(gettextf("%s failed in %s() for '%s', details:\n  call: %s\n  error: %s", 
           hookname, "loadNamespace", pkgname, deparse(conditionCall(res))[1L], 
           conditionMessage(res)), call. = FALSE, domain = NA)
    2: runHook(".onLoad", env, package.lib, package)
    1: loadNamespace(package, lib.loc)
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    Error: .onLoad failed in loadNamespace() for 'legendry', details:
      call: getFromNamespace("class_mapping", "ggplot2")
      error: could not find function "getFromNamespace"
    Execution halted
    ```

# lemon

<details>

* Version: 0.5.0
* GitHub: https://github.com/stefanedwards/lemon
* Source code: https://github.com/cran/lemon
* Date/Publication: 2024-11-10 18:20:02 UTC
* Number of recursive dependencies: 72

Run `revdepcheck::cloud_details(, "lemon")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘lemon-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: brackets_horizontal
    > ### Title: Axis brackets instead of axis ticks and lines
    > ### Aliases: brackets_horizontal brackets_horisontal brackets_vertical
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
    +   geom_point(position=position_jitter(width=0.3)) +
    +   theme_bw() +
    +   theme(panel.border = element_blank(), axis.line = element_line())
    > p
    > 
    > p <- p + coord_flex_cart(bottom=brackets_horizontal(length=unit(0.08, 'npc')))
    > p
    Error in as.unit(e2) : object is not coercible to a unit
    Calls: <Anonymous> ... render_axis_h -> bottom -> bottom -> unit.c -> Ops.unit -> as.unit
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
       16.                       └─gtable::gtable_add_row_space(panel_table, theme$panel.spacing.y %||% theme$panel.spacing)
       17.                         └─cli::cli_abort("{.arg height} must be of length 1 or nrow - 1")
       18.                           └─rlang::abort(...)
      
      [ FAIL 2 | WARN 14 | SKIP 3 | PASS 138 ]
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

*   checking S3 generic/method consistency ... WARNING
    ```
    ggplot_build:
      function(plot, ...)
    ggplot_build.lemon_plot:
      function(plot)
    
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.axis_annotation:
      function(object, plot, object_name)
    
    ggplot_gtable:
      function(plot)
    ggplot_gtable.built_lemon:
      function(data)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# lfproQC

<details>

* Version: 1.4.0
* GitHub: https://github.com/kabilansbio/lfproQC
* Source code: https://github.com/cran/lfproQC
* Date/Publication: 2024-10-10 13:10:02 UTC
* Number of recursive dependencies: 142

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

# lgpr

<details>

* Version: 1.2.4
* GitHub: https://github.com/jtimonen/lgpr
* Source code: https://github.com/cran/lgpr
* Date/Publication: 2023-09-24 06:50:02 UTC
* Number of recursive dependencies: 100

Run `revdepcheck::cloud_details(, "lgpr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # Short tests
      > #  - focus on testing that everything runs
      > #  - should take less than minute
      > library(testthat)
      > library(lgpr)
      Attached lgpr 1.2.4, using rstan 2.32.7. Type ?lgpr to get started.
      > 
    ...
      ── Failure ('test_misc-invgamma.R:34:3'): plotting the inverse gamma distribution works ──
      `c1` not equal to c("gg", "ggplot").
      Lengths differ: 5 is not 2
      ── Failure ('test_misc-invgamma.R:35:3'): plotting the inverse gamma distribution works ──
      `c2` not equal to c("gg", "ggplot").
      Lengths differ: 5 is not 2
      
      [ FAIL 2 | WARN 2 | SKIP 0 | PASS 446 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 153.2Mb
      sub-directories of 1Mb or more:
        R       1.5Mb
        libs  151.3Mb
    ```

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) prior_to_num.Rd:13: Lost braces; missing escapes or markup?
        13 |   {'uniform', 'normal', 'student-t', 'gamma', 'inv-gamma', 'log-normal'}
           |   ^
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# lionfish

<details>

* Version: 1.0.27
* GitHub: NA
* Source code: https://github.com/cran/lionfish
* Date/Publication: 2025-03-13 20:50:02 UTC
* Number of recursive dependencies: 125

Run `revdepcheck::cloud_details(, "lionfish")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Bioprocess-Optimization.Rmd’ using knitr
    --- finished re-building ‘Bioprocess-Optimization.Rmd’
    
    --- re-building ‘General-interactivity.Rmd’ using knitr
    --- finished re-building ‘General-interactivity.Rmd’
    
    --- re-building ‘Market-segmentation.Rmd’ using knitr
    ```

# listdown

<details>

* Version: 0.5.7
* GitHub: https://github.com/kaneplusplus/listdown
* Source code: https://github.com/cran/listdown
* Date/Publication: 2023-04-03 00:30:02 UTC
* Number of recursive dependencies: 70

Run `revdepcheck::cloud_details(, "listdown")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘listdown-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: as_ld_yml
    > ### Title: Turn a Computational Component List into YAML with Class
    > ###   Information
    > ### Aliases: as_ld_yml
    > 
    > ### ** Examples
    > 
    ...
    +   as_ld_yml(cc_list)
    + }
    Loading required package: ggplot2
    Error in parse(text = paste("x", loc, " <<- list(paste(", deparse(class(elem)),  : 
      <text>:2:1: unexpected symbol
    1: x [[1]]  <<- list(paste( c("ggplot2::ggplot", "ggplot", "ggplot2::gg", "S7_object", "gg" , collapse = ":"))
    2: x
       ^
    Calls: as_ld_yml -> depth_first_copy -> eval -> parse
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.r’
    Running the tests in ‘tests/testthat.r’ failed.
    Complete output:
      > library(testthat)
      > library(listdown)
      > 
      > test_check("listdown")
      [ FAIL 2 | WARN 0 | SKIP 1 | PASS 34 ]
      
      ══ Skipped tests (1) ═══════════════════════════════════════════════════════════
    ...
       5.   └─listdown (local) depth_first_copy()
       6.     ├─base::eval(...)
       7.     └─base::parse(...)
      ── Failure ('test-cc-dendro.r:20:3'): Dendrograms work. ────────────────────────
      ld_cc_dendro(cc_list) not equal to read_reference("cc-dendro.rds").
      4 string mismatches
      
      [ FAIL 2 | WARN 0 | SKIP 1 | PASS 34 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘workflowr’
    ```

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) ld_build_html_site.Rd:41: Lost braces
        41 | directory specified by `rmd_dir`. Note that this is an {{rmarkdown}}
           |                                                        ^
    checkRd: (-1) ld_create_doc.Rd:29: Lost braces
        29 | directory specified by `rmd_dir`. Note that this is an {{rmarkdown}}
           |                                                        ^
    ```

# LMD

<details>

* Version: 1.0.0
* GitHub: https://github.com/shubhra-opensource/LMD
* Source code: https://github.com/cran/LMD
* Date/Publication: 2022-09-20 09:56:07 UTC
* Number of recursive dependencies: 90

Run `revdepcheck::cloud_details(, "LMD")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘LMD-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_lmd
    > ### Title: LMD Plot
    > ### Aliases: plot_lmd
    > ### Keywords: LMD PF Residue
    > 
    > ### ** Examples
    > 
    ...
    > y = (2 / 3 )* sin(x * 30) + (2 / 3) * sin(x * 17.5) + (4 / 5) *cos(x * 2)
    > plot_lmd(lmd(y))
    Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ℹ Please use `linewidth` instead.
    ℹ The deprecated feature was likely used in the LMD package.
      Please report the issue at
      <https://github.com/shubhra-opensource/LMD/issues>.
    Error: <ggplot2::labels> object is invalid:
    - every label must be named.
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Getting_Started_with_LMD.Rmd’ using rmarkdown
    ```

# lnmixsurv

<details>

* Version: 3.1.6
* GitHub: NA
* Source code: https://github.com/cran/lnmixsurv
* Date/Publication: 2024-09-03 15:20:08 UTC
* Number of recursive dependencies: 195

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

# longevity

<details>

* Version: 1.2
* GitHub: https://github.com/lbelzile/longevity
* Source code: https://github.com/cran/longevity
* Date/Publication: 2025-05-12 02:10:02 UTC
* Number of recursive dependencies: 52

Run `revdepcheck::cloud_details(, "longevity")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘longevity-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: autoplot.elife_par
    > ### Title: Goodness-of-fit plots for parametric models
    > ### Aliases: autoplot.elife_par plot.elife_par
    > 
    > ### ** Examples
    > 
    > set.seed(1234)
    ...
    +  thresh = 0,
    +  event = ifelse(samp$rcens, 0L, 1L),
    +  type = "right",
    +  family = "exp",
    +  export = TRUE)
    > plot(fitted, plot.type = "ggplot")
    Error in get("print.ggplot", envir = loadNamespace("ggplot2")) : 
      object 'print.ggplot' not found
    Calls: plot -> plot.elife_par -> lapply -> match.fun -> get
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘tinytest.R’
    Running the tests in ‘tests/tinytest.R’ failed.
    Complete output:
      > 
      > if ( requireNamespace("tinytest", quietly=TRUE) ){
      +   tinytest::test_package("longevity")
      + }
      
      test-S3methods.R..............    0 tests    
      test-S3methods.R..............    0 tests    
    ...
      test-S3methods.R..............    0 tests    
      test-S3methods.R..............    0 tests    
      test-S3methods.R..............    0 tests    
      test-S3methods.R..............    0 tests    
      test-S3methods.R..............    0 tests    
      test-S3methods.R..............    0 tests    
      test-S3methods.R..............    0 tests    Error in get("print.ggplot", envir = loadNamespace("ggplot2")) : 
        object 'print.ggplot' not found
      Calls: <Anonymous> ... plot -> plot.elife_par -> lapply -> match.fun -> get
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.0Mb
      sub-directories of 1Mb or more:
        data   7.0Mb
        libs   2.3Mb
    ```

# LSX

<details>

* Version: 1.4.4
* GitHub: https://github.com/koheiw/LSX
* Source code: https://github.com/cran/LSX
* Date/Publication: 2025-05-23 09:02:06 UTC
* Number of recursive dependencies: 90

Run `revdepcheck::cloud_details(, "LSX")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > require(testthat)
      Loading required package: testthat
      > require(quanteda)
      Loading required package: quanteda
      Package version: 4.3.0
      Unicode version: 15.1
    ...
      ── Failure ('test-textplot.R:84:5'): textplot_components() works ───────────────
      class(textplot_components(lss_svd, 3, scale = "relative")) not equal to c("gg", "ggplot").
      Lengths differ: 5 is not 2
      ── Failure ('test-textplot.R:109:5'): textplot_terms works even when frequency has zeros (#85) ──
      class(textplot_terms(lss)) not equal to c("gg", "ggplot").
      Lengths differ: 5 is not 2
      
      [ FAIL 24 | WARN 0 | SKIP 10 | PASS 221 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2096 marked UTF-8 strings
    ```

# ltertools

<details>

* Version: 2.0.0
* GitHub: https://github.com/lter/ltertools
* Source code: https://github.com/cran/ltertools
* Date/Publication: 2025-03-26 18:30:02 UTC
* Number of recursive dependencies: 90

Run `revdepcheck::cloud_details(, "ltertools")` for more info

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
      ── Failure ('test-site_timeline.R:15:3'): Outputs are correct ──────────────────
      class(time_gg) (`actual`) not equal to c("gg", "ggplot") (`expected`).
      
      `actual`:        "ggplot2::ggplot" "ggplot" "ggplot2::gg" "S7_object" "gg"    
      `expected[2:2]`:                                                      "ggplot"
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 36 ]
      Error: Test failures
      Execution halted
    ```

# manydata

<details>

* Version: 1.0.2
* GitHub: https://github.com/globalgov/manydata
* Source code: https://github.com/cran/manydata
* Date/Publication: 2025-06-03 08:40:02 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::cloud_details(, "manydata")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(manydata)
      Loading required package: cli
      Loading required package: dplyr
      
      Attaching package: 'dplyr'
      
    ...
      ── Failure ('test_compare.R:6:3'): plot for compare_categories returns the correct output format ──
      `db` has length 1, not length 11.
      ── Failure ('test_compare.R:48:3'): compare_overlap() and return the correct output format ──
      `pl` has type 'object', not 'list'.
      ── Failure ('test_compare.R:60:3'): compare_missing() and plot_missing() returns the correct output format ──
      `pl` has type 'object', not 'list'.
      
      [ FAIL 5 | WARN 1 | SKIP 3 | PASS 122 ]
      Error: Test failures
      Execution halted
    ```

# manymodelr

<details>

* Version: 0.3.9
* GitHub: https://github.com/Nelson-Gon/manymodelr
* Source code: https://github.com/cran/manymodelr
* Date/Publication: 2025-03-20 07:20:02 UTC
* Number of recursive dependencies: 126

Run `revdepcheck::cloud_details(, "manymodelr")` for more info

</details>

## Newly broken

*   checking whether package ‘manymodelr’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘e1071::element’ by ‘ggplot2::element’ when loading ‘manymodelr’
    See ‘/tmp/workdir/manymodelr/new/manymodelr.Rcheck/00install.out’ for details.
    ```

# manynet

<details>

* Version: 1.3.2
* GitHub: https://github.com/stocnet/manynet
* Source code: https://github.com/cran/manynet
* Date/Publication: 2024-11-05 20:50:02 UTC
* Number of recursive dependencies: 142

Run `revdepcheck::cloud_details(, "manynet")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘manynet-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: make_play
    > ### Title: Making diffusion models on networks
    > ### Aliases: make_play play_diffusion play_diffusions
    > 
    > ### ** Examples
    > 
    >   smeg <- generate_smallworld(15, 0.025)
    ...
    ! Only know how to add <ggplot> and/or <grob> objects
    Backtrace:
        ▆
     1. ├─base::plot(play_diffusion(smeg, recovery = 0.4))
     2. └─manynet:::plot.diff_model(play_diffusion(smeg, recovery = 0.4))
     3.   └─manynet:::`+.ggplot`(...)
     4.     └─patchwork::wrap_plots(e1, e2, ...)
     5.       └─cli::cli_abort("Only know how to add {.cls ggplot} and/or {.cls grob} objects")
     6.         └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(manynet)
      > 
      > test_check("manynet")
      Starting 2 test processes
      [ FAIL 3 | WARN 0 | SKIP 21 | PASS 548 ]
      
    ...
       1. ├─base::plot(node_degree(ison_adolescents)) at test-measure_centrality.R:87:3
       2. └─manynet:::plot.node_measure(node_degree(ison_adolescents))
       3.   └─manynet:::`+.ggplot`(...)
       4.     └─patchwork::wrap_plots(e1, e2, ...)
       5.       └─cli::cli_abort("Only know how to add {.cls ggplot} and/or {.cls grob} objects")
       6.         └─rlang::abort(...)
      
      [ FAIL 3 | WARN 0 | SKIP 21 | PASS 548 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘Rgraphviz’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        R           1.5Mb
        tutorials   1.9Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 7 marked UTF-8 strings
    ```

# marquee

<details>

* Version: 1.0.0
* GitHub: https://github.com/r-lib/marquee
* Source code: https://github.com/cran/marquee
* Date/Publication: 2025-01-20 16:01:56 UTC
* Number of recursive dependencies: 85

Run `revdepcheck::cloud_details(, "marquee")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘marquee-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: element_marquee
    > ### Title: ggplot2 theme element supporting marquee syntax
    > ### Aliases: element_marquee
    > 
    > ### ** Examples
    > 
    > ## Don't show: 
    ...
    + What more could you _possibly_ want?") +
    +   theme(title = element_marquee())
    + ## Don't show: 
    + }) # examplesIf
    > library(ggplot2)
    > p <- ggplot(mtcars) + geom_point(aes(mpg, disp)) + labs(title = "A {.red *marquee*} title\n* Look at this bullet list\n\n* great, huh?") + 
    +     theme_gray(base_size = 6) + theme(title = element_marquee())
    > plot(p)
    Error: `object` must be an <S7_object>, not a S3<element_marquee/element_text/element>
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

# MaxWiK

<details>

* Version: 1.0.5
* GitHub: NA
* Source code: https://github.com/cran/MaxWiK
* Date/Publication: 2024-11-25 11:40:13 UTC
* Number of recursive dependencies: 54

Run `revdepcheck::cloud_details(, "MaxWiK")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘User-Guide.Rmd’ using rmarkdown
    # Input data
    
    ## Data format
    
    The data format of input should be numerical only in the form of data frame, for example:
    
    
    ``` r
    ...
    
    Error: processing vignette 'User-Guide.Rmd' failed with diagnostics:
    `object` must be an <S7_object>, not a S3<element_text/element>
    --- failed re-building ‘User-Guide.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘User-Guide.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# MBMethPred

<details>

* Version: 0.1.4.2
* GitHub: https://github.com/sharifrahmanie/MBMethPred
* Source code: https://github.com/cran/MBMethPred
* Date/Publication: 2023-09-18 14:10:09 UTC
* Number of recursive dependencies: 138

Run `revdepcheck::cloud_details(, "MBMethPred")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(MBMethPred)
      > 
      > test_check("MBMethPred")
              y_pred
      y_true   Group3 SHH Group4
        Group3      1   0      0
    ...
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 11 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-BoxPlot.R:8:3'): MakeBoxPlot returns correct type ────────────
      BoxPlot(File = data) has type 'object', not 'list'.
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 11 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.2Mb
      sub-directories of 1Mb or more:
        R      3.0Mb
        data   7.0Mb
    ```

# mcp

<details>

* Version: 0.3.4
* GitHub: https://github.com/lindeloev/mcp
* Source code: https://github.com/cran/mcp
* Date/Publication: 2024-03-17 20:10:02 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "mcp")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mcp-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_pars
    > ### Title: Plot individual parameters
    > ### Aliases: plot_pars
    > 
    > ### ** Examples
    > 
    > # Typical usage. demo_fit is an mcpfit object.
    > plot_pars(demo_fit)
    Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ℹ Please use `linewidth` instead.
    ℹ The deprecated feature was likely used in the bayesplot package.
      Please report the issue at <https://github.com/stan-dev/bayesplot/issues/>.
    Error: Can't find method for generic `&(e1, e2)`:
    - e1: <patchwork>
    - e2: <theme>
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(mcp)
      > 
      > test_check("mcp")
      [ FAIL 24 | WARN 2 | SKIP 6 | PASS 749 ]
      
      ══ Skipped tests (6) ═══════════════════════════════════════════════════════════
    ...
      Backtrace:
          ▆
       1. └─mcp:::test_runs(model, ...) at tests/testthat/helper-runs.R:338:7
       2.   └─mcp:::test_plot_pars(fit) at tests/testthat/helper-runs.R:112:7
       3.     └─mcp::plot_pars(fit, type = "dens_overlay") at tests/testthat/helper-runs.R:171:3
       4.       └─S7:::Ops.S7_object(...)
      
      [ FAIL 24 | WARN 2 | SKIP 6 | PASS 749 ]
      Error: Test failures
      Execution halted
    ```

# mcStats

<details>

* Version: 0.1.2
* GitHub: NA
* Source code: https://github.com/cran/mcStats
* Date/Publication: 2020-02-26 06:50:02 UTC
* Number of recursive dependencies: 52

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

# meme

<details>

* Version: 0.2.3
* GitHub: https://github.com/GuangchuangYu/meme
* Source code: https://github.com/cran/meme
* Date/Publication: 2021-04-23 10:00:02 UTC
* Number of recursive dependencies: 63

Run `revdepcheck::cloud_details(, "meme")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘meme.Rmd’ using rmarkdown
    ```

# MendelianRandomization

<details>

* Version: 0.10.0
* GitHub: NA
* Source code: https://github.com/cran/MendelianRandomization
* Date/Publication: 2024-04-12 10:10:02 UTC
* Number of recursive dependencies: 85

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
      installed size is  5.9Mb
      sub-directories of 1Mb or more:
        R      1.0Mb
        libs   4.2Mb
    ```

# metagam

<details>

* Version: 0.4.0
* GitHub: https://github.com/Lifebrain/metagam
* Source code: https://github.com/cran/metagam
* Date/Publication: 2023-05-05 18:20:06 UTC
* Number of recursive dependencies: 116

Run `revdepcheck::cloud_details(, "metagam")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘dominance.Rmd’ using rmarkdown
    ```

# MetAlyzer

<details>

* Version: 1.1.0
* GitHub: https://github.com/nilsmechtel/MetAlyzer
* Source code: https://github.com/cran/MetAlyzer
* Date/Publication: 2024-12-06 14:00:02 UTC
* Number of recursive dependencies: 94

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
* Number of recursive dependencies: 173

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
      5.     └─ggplot2::build_ggplot(plot)
      6.       ├─S7::S7_dispatch()
      7.       └─ggplot2 (local) `method(build_ggplot, ggplot2::ggplot)`(...)
      8.         └─npscales$set_palettes(plot@theme)
      9.           └─ggplot2 (local) set_palettes(..., self = self)
     10.             ├─scales::as_continuous_pal(elem)
     11.             └─scales:::as_continuous_pal.default(elem)
     12.               └─cli::cli_abort("Cannot convert {.arg x} to a continuous palette.")
     13.                 └─rlang::abort(...)
    Execution halted
    ```

# metaquant

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/metaquant
* Date/Publication: 2025-02-11 17:00:02 UTC
* Number of recursive dependencies: 75

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
       18.                           ├─ggplot2::guide_gengrob(params, theme)
       19.                           └─metR:::guide_gengrob.colorstrip(params, theme)
       20.                             └─grid::convertWidth(...)
       21.                               └─grid::convertUnit(...)
       22.                                 ├─grid:::upgradeUnit(x)
       23.                                 └─grid:::upgradeUnit.default(x)
      
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
* Number of recursive dependencies: 110

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

* Version: 1.1.0
* GitHub: https://github.com/agerada/MIC
* Source code: https://github.com/cran/MIC
* Date/Publication: 2025-06-05 04:20:06 UTC
* Number of recursive dependencies: 144

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
      6.       ├─S7::S7_dispatch()
      7.       └─ggplot2 (local) `method(gtable_ggplot, ggplot2::ggplot_built)`(data = `<ggpl2::_>`)
      8.         └─layout$render(geom_grobs, data, theme, plot@labels)
      9.           └─ggplot2 (local) render(..., self = self)
     10.             └─self$facet$draw_panels(...)
     11.               └─lemon (local) draw_panels(...)
     12.                 └─gtable::gtable_add_col_space(panel_table, theme$panel.spacing.x %||% theme$panel.spacing)
     13.                   └─cli::cli_abort("{.arg width} must be of length 1 or ncol - 1")
     14.                     └─rlang::abort(...)
    Execution halted
    ```

# migraph

<details>

* Version: 1.4.5
* GitHub: https://github.com/stocnet/migraph
* Source code: https://github.com/cran/migraph
* Date/Publication: 2024-12-02 10:00:10 UTC
* Number of recursive dependencies: 95

Run `revdepcheck::cloud_details(, "migraph")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘migraph-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: regression
    > ### Title: Linear and logistic regression for network data
    > ### Aliases: regression net_regression
    > 
    > ### ** Examples
    > 
    > networkers <- ison_networkers %>% to_subgraph(Discipline == "Sociology")
    ...
    4 sim Citations     39.4       3.13     0.1 
    > glance(model1)
    # A tibble: 1 × 8
      r.squared adj.r.squared sigma statistic  p.value    df df.residual  nobs
          <dbl>         <dbl> <dbl>     <dbl>    <dbl> <dbl>       <int> <int>
    1    0.0674        0.0570  48.9      6.46 0.000309     3         268   272
    > plot(model1)
    Error in loadNamespace(x) : there is no package called ‘patchwork’
    Calls: plot ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
    Execution halted
    ```

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
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 34 ]
    ...
       2. ├─migraph:::plot.network_test(cugtest)
       3. │ └─manynet:::`+.ggplot`(...)
       4. └─base::loadNamespace(x)
       5.   └─base::withRestarts(stop(cond), retry_loadNamespace = function() NULL)
       6.     └─base (local) withOneRestart(expr, restarts[[1L]])
       7.       └─base (local) doWithOneRestart(return(expr), restart)
      
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 34 ]
      Error: Test failures
      Execution halted
    ```

# mikropml

<details>

* Version: 1.6.1
* GitHub: https://github.com/SchlossLab/mikropml
* Source code: https://github.com/cran/mikropml
* Date/Publication: 2023-08-21 15:10:05 UTC
* Number of recursive dependencies: 127

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
* Number of recursive dependencies: 192

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
* Number of recursive dependencies: 123

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
      [ FAIL 7 | WARN 15 | SKIP 0 | PASS 196 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      y[1]: "list"
      ── Failure ('test-plotscore.R:44:5'): Tests plotting scores ────────────────────
      typeof(plot_own) not equal to "list".
      1/1 mismatches
      x[1]: "object"
      y[1]: "list"
      
      [ FAIL 7 | WARN 15 | SKIP 0 | PASS 196 ]
      Error: Test failures
      Execution halted
    ```

# MiscMetabar

<details>

* Version: 0.14.2
* GitHub: https://github.com/adrientaudiere/MiscMetabar
* Source code: https://github.com/cran/MiscMetabar
* Date/Publication: 2025-03-20 15:20:02 UTC
* Number of recursive dependencies: 420

Run `revdepcheck::cloud_details(, "MiscMetabar")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘MiscMetabar-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: upset_pq
    > ### Title: Make upset plot for phyloseq object.
    > ### Aliases: upset_pq
    > 
    > ### ** Examples
    > 
    > if (requireNamespace("ComplexUpset")) {
    ...
     27. │                     └─ggplot2 (local) `method(merge_element, list(ggplot2::element, class_any))`(...)
     28. │                       └─S7::props(old, idx)
     29. │                         └─S7::check_is_S7(object)
     30. │                           └─base::stop(msg, call. = FALSE)
     31. └─base::.handleSimpleError(...)
     32.   └─rlang (local) h(simpleError(msg, call))
     33.     └─handlers[[1L]](cnd)
     34.       └─cli::cli_abort(...)
     35.         └─rlang::abort(...)
    Execution halted
    ```

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
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test_figures_beta_div.R:72:5'): graph_test_pq works ───────────────
      `graph_test_pq(data_fungi_mini, fact = "Tree_name")` produced warnings.
      ── Failure ('test_figures_beta_div.R:258:5'): upset_pq works with data_fungi dataset ──
      `suppressMessages(upset_pq(data_fungi_mini, "Height"))` produced warnings.
      
      [ FAIL 2 | WARN 4 | SKIP 74 | PASS 80 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘MiscMetabar.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        data      2.0Mb
        extdata   1.2Mb
    ```

# miscset

<details>

* Version: 1.1.0
* GitHub: https://github.com/setempler/miscset
* Source code: https://github.com/cran/miscset
* Date/Publication: 2017-02-24 16:46:57
* Number of recursive dependencies: 109

Run `revdepcheck::cloud_details(, "miscset")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘miscset.Rmd’ using rmarkdown
    ```

# misspi

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/misspi
* Date/Publication: 2023-10-17 09:50:02 UTC
* Number of recursive dependencies: 85

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

# mistral

<details>

* Version: 2.2.2
* GitHub: NA
* Source code: https://github.com/cran/mistral
* Date/Publication: 2024-01-17 13:40:02 UTC
* Number of recursive dependencies: 71

Run `revdepcheck::cloud_details(, "mistral")` for more info

</details>

## Newly broken

*   checking whether package ‘mistral’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘e1071::element’ by ‘ggplot2::element’ when loading ‘mistral’
    See ‘/tmp/workdir/mistral/new/mistral.Rcheck/00install.out’ for details.
    ```

# misty

<details>

* Version: 0.7.2
* GitHub: NA
* Source code: https://github.com/cran/misty
* Date/Publication: 2025-05-20 16:20:02 UTC
* Number of recursive dependencies: 85

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
     26.                                   └─ggplot2 (local) extract_key(...)
     27.                                     └─Guide$extract_key(scale, aesthetic, ...)
     28.                                       └─ggplot2 (local) extract_key(...)
     29.                                         └─scale$get_labels(breaks)
     30.                                           └─ggplot2 (local) get_labels(..., self = self)
     31.                                             └─self$scale$get_labels(breaks)
     32.                                               └─ggplot2 (local) get_labels(..., self = self)
     33.                                                 └─cli::cli_abort(...)
     34.                                                   └─rlang::abort(...)
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
* Number of recursive dependencies: 122

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
     26. │                             └─ggplot2 (local) FUN(X[[i]], ...)
     27. │                               └─self$draw_group(group, panel_params, coord, ...)
     28. │                                 └─ggplot2 (local) draw_group(...)
     29. │                                   └─ggplot2 (local) draw_group(..., self = self)
     30. └─base::.handleSimpleError(...)
     31.   └─rlang (local) h(simpleError(msg, call))
     32.     └─handlers[[1L]](cnd)
     33.       └─cli::cli_abort(...)
     34.         └─rlang::abort(...)
    Execution halted
    ```

# mlr3spatiotempcv

<details>

* Version: 2.3.2
* GitHub: https://github.com/mlr-org/mlr3spatiotempcv
* Source code: https://github.com/cran/mlr3spatiotempcv
* Date/Publication: 2024-11-29 13:10:02 UTC
* Number of recursive dependencies: 165

Run `revdepcheck::cloud_details(, "mlr3spatiotempcv")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > if (requireNamespace("testthat", quietly = TRUE)) {
      +   library("checkmate")
      +   library("testthat")
      +   library("mlr3spatiotempcv")
      +   test_check("mlr3spatiotempcv")
      + }
      Loading required package: mlr3
    ...
      • 2-autoplot/sptcvcstf-2d-time-var-fold-1-rep-2.svg
      • 2-autoplot/sptcvcstf-2d-time-var-fold-1-sample-fold-n.svg
      • 2-autoplot/sptcvcstf-2d-time-var-fold-1.svg
      • 2-autoplot/sptcvcstf-2d-time-var-sample-fold-n.svg
      • 2-autoplot/sptcvcstf-3d-time-var-fold-1-2-sample-fold-n.svg
      • 2-autoplot/sptcvcstf-3d-time-var-fold-1-2.svg
      • 2-autoplot/sptcvcstf-3d-time-var-fold-1-sample-fold-n.svg
      • autoplot_buffer/spcvbuffer-fold-1-2.svg
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.9Mb
      sub-directories of 1Mb or more:
        data   3.5Mb
    ```

# modeldb

<details>

* Version: 0.3.0
* GitHub: https://github.com/tidymodels/modeldb
* Source code: https://github.com/cran/modeldb
* Date/Publication: 2023-11-01 14:30:02 UTC
* Number of recursive dependencies: 87

Run `revdepcheck::cloud_details(, "modeldb")` for more info

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
      ── Failure ('test_kmeans_viz.R:5:3'): plot_kmeans() returns a ggplot2 object ───
      class(plot_kmeans(mtcars, mpg, wt, group = am)) (`actual`) not equal to c("gg", "ggplot") (`expected`).
      
      `actual`:        "ggplot2::ggplot" "ggplot" "ggplot2::gg" "S7_object" "gg"    
      `expected[2:2]`:                                                      "ggplot"
      
      [ FAIL 1 | WARN 33 | SKIP 0 | PASS 18 ]
      Error: Test failures
      Execution halted
    ```

# modeltime.resample

<details>

* Version: 0.2.3
* GitHub: https://github.com/business-science/modeltime.resample
* Source code: https://github.com/cran/modeltime.resample
* Date/Publication: 2023-04-12 15:50:02 UTC
* Number of recursive dependencies: 234

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
      ✔ broom        1.0.8          ✔ recipes      1.3.1     
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

# Momocs

<details>

* Version: 1.4.1
* GitHub: https://github.com/MomX/Momocs
* Source code: https://github.com/cran/Momocs
* Date/Publication: 2023-11-13 11:13:30 UTC
* Number of recursive dependencies: 144

Run `revdepcheck::cloud_details(, "Momocs")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Momocs-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_CV
    > ### Title: Plots a cross-validation table as an heatmap
    > ### Aliases: plot_CV plot_CV.default plot_CV.LDA
    > 
    > ### ** Examples
    > 
    > h <- hearts %>%
    ...
    > h %>% plot_CV(freq=FALSE, rm0=FALSE, fill=FALSE)
    > # you can customize the returned gg with some ggplot2 functions
    > h %>% plot_CV(labels=FALSE, fill=TRUE, axis.size=5) + ggplot2::ggtitle("A confusion matrix")
    > 
    > # or build your own using the prepared data_frame:
    > df <- h %>% plot_CV() %$% data
    Error in eval(substitute(expr), data, enclos = parent.frame()) : 
      invalid 'envir' argument of type 'object'
    Calls: %$% -> <Anonymous> -> with.default -> eval
    Execution halted
    ```

# mpactr

<details>

* Version: 0.2.1
* GitHub: https://github.com/mums2/mpactr
* Source code: https://github.com/cran/mpactr
* Date/Publication: 2025-03-29 00:30:05 UTC
* Number of recursive dependencies: 142

Run `revdepcheck::cloud_details(, "mpactr")` for more info

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
      `expected[2:2]`:                                                      "ggplot"
      ── Failure ('test-quality_control.R:44:3'): qc plot returns a generates a plot ──
      class(plot) (`actual`) not equal to c("gg", "ggplot") (`expected`).
      
      `actual`:        "ggplot2::ggplot" "ggplot" "ggplot2::gg" "S7_object" "gg"    
      `expected[2:2]`:                                                      "ggplot"
      
      [ FAIL 2 | WARN 0 | SKIP 1 | PASS 165 ]
      Error: Test failures
      Execution halted
    ```

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

# MRZero

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/MRZero
* Date/Publication: 2024-04-14 09:30:03 UTC
* Number of recursive dependencies: 79

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

# mshap

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/mshap
* Date/Publication: 2021-06-17 08:40:02 UTC
* Number of recursive dependencies: 123

Run `revdepcheck::cloud_details(, "mshap")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(mshap)
      > 
      > test_check("mshap")
      [ FAIL 8 | WARN 2 | SKIP 0 | PASS 19 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      `p2_all` has type 'object', not 'list'.
      ── Failure ('test-plots.R:62:3'): Observation Plot Works with all arguments ────
      class(p2_all) (`actual`) not equal to c("gg", "ggplot") (`expected`).
      
      `actual`:        "ggplot2::ggplot" "ggplot" "ggplot2::gg" "S7_object" "gg"    
      `expected[2:2]`:                                                      "ggplot"
      
      [ FAIL 8 | WARN 2 | SKIP 0 | PASS 19 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyselect’
      All declared Imports should be used.
    ```

# mtdesign

<details>

* Version: 0.1.2
* GitHub: https://github.com/openpharma/mtdesign
* Source code: https://github.com/cran/mtdesign
* Date/Publication: 2024-11-05 14:10:15 UTC
* Number of recursive dependencies: 56

Run `revdepcheck::cloud_details(, "mtdesign")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(mtdesign)
      > 
      > test_check("mtdesign")
      testthat 2025-06-11 09:02:22 DEBUG test_check: Entry
      testthat 2025-06-11 09:02:22 DEBUG test_check: Entry
      testthat 2025-06-11 09:02:22 DEBUG test_check: Exit
    ...
      `expected[2:2]`:                                                      "ggplot"
      ── Failure ('test-powerPlot.R:6:3'): multiplication works ──────────────────────
      class(powerPlot(simonGrid)) (`actual`) not equal to c("gg", "ggplot") (`expected`).
      
      `actual`:        "ggplot2::ggplot" "ggplot" "ggplot2::gg" "S7_object" "gg"    
      `expected[2:2]`:                                                      "ggplot"
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 127 ]
      Error: Test failures
      Execution halted
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

*   checking whether package ‘MultiTraits’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/MultiTraits/new/MultiTraits.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘MultiTraits’ ...
** package ‘MultiTraits’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'ggtern', details:
  call: NULL
  error: <ggplot2::element_line> object properties are invalid:
- @lineend must be <character> or <NULL>, not S3<arrow>
Execution halted
ERROR: lazy loading failed for package ‘MultiTraits’
* removing ‘/tmp/workdir/MultiTraits/new/MultiTraits.Rcheck/MultiTraits’


```
### CRAN

```
* installing *source* package ‘MultiTraits’ ...
** package ‘MultiTraits’ successfully unpacked and MD5 sums checked
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
* DONE (MultiTraits)


```
# mvdalab

<details>

* Version: 1.7
* GitHub: NA
* Source code: https://github.com/cran/mvdalab
* Date/Publication: 2022-10-05 23:00:14 UTC
* Number of recursive dependencies: 67

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
     15. │                 └─ggplot2 (local) FUN(X[[i]], ...)
     16. │                   └─scale$map_df(df = df)
     17. │                     └─ggplot2 (local) map_df(..., self = self)
     18. │                       └─base::lapply(aesthetics, function(j) self$map(df[[j]]))
     19. │                         └─ggplot2 (local) FUN(X[[i]], ...)
     20. │                           └─self$map(df[[j]])
     21. │                             └─ggplot2 (local) map(..., self = self)
     22. │                               └─vctrs::`vec_slice<-`(`*tmp*`, is.na(x), value = na_value)
     23. └─rlang::cnd_signal(x)
    Execution halted
    ```

# mverse

<details>

* Version: 0.2.0
* GitHub: https://github.com/mverseanalysis/mverse
* Source code: https://github.com/cran/mverse
* Date/Publication: 2025-04-24 08:10:02 UTC
* Number of recursive dependencies: 135

Run `revdepcheck::cloud_details(, "mverse")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘mverse_gettingstarted.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.0Mb
      sub-directories of 1Mb or more:
        data   7.0Mb
    ```

# mxnorm

<details>

* Version: 1.0.3
* GitHub: NA
* Source code: https://github.com/cran/mxnorm
* Date/Publication: 2023-05-01 17:10:02 UTC
* Number of recursive dependencies: 169

Run `revdepcheck::cloud_details(, "mxnorm")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(mxnorm)
      > 
      > test_check("mxnorm")
      boundary (singular) fit: see help('isSingular')
      boundary (singular) fit: see help('isSingular')
      boundary (singular) fit: see help('isSingular')
    ...
      `expected[2:2]`:                                                      "ggplot"
      ── Failure ('test-plot_mx_umap.R:16:5'): plotting works ────────────────────────
      class(plot_mx_umap(mx_data)) (`actual`) not equal to c("gg", "ggplot") (`expected`).
      
      `actual`:        "ggplot2::ggplot" "ggplot" "ggplot2::gg" "S7_object" "gg"    
      `expected[2:2]`:                                                      "ggplot"
      
      [ FAIL 2 | WARN 2 | SKIP 6 | PASS 85 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘e1071’ ‘glue’
      All declared Imports should be used.
    ```

# neatmaps

<details>

* Version: 2.1.0
* GitHub: https://github.com/PhilBoileau/neatmaps
* Source code: https://github.com/cran/neatmaps
* Date/Publication: 2019-05-12 19:10:03 UTC
* Number of recursive dependencies: 97

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
* Number of recursive dependencies: 82

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
      [ FAIL 2 | WARN 733 | SKIP 3 | PASS 430 ]
      
    ...
      `attr(expected, 'S7_class')` is absent
      ── Failure ('test-ptd_create_ggplot.R:79:3'): it returns a ggplot object ───────
      p$labels (`actual`) not equal to list(...) (`expected`).
      
      `actual` is an S7 object of class <ggplot2::labels>
      `expected` is a list
      
      [ FAIL 2 | WARN 733 | SKIP 3 | PASS 430 ]
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

# nichetools

<details>

* Version: 0.3.2
* GitHub: https://github.com/benjaminhlina/nichetools
* Source code: https://github.com/cran/nichetools
* Date/Publication: 2024-09-30 21:20:02 UTC
* Number of recursive dependencies: 117

Run `revdepcheck::cloud_details(, "nichetools")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘using-nichetools-with-the-package-SIBER.Rmd’ using rmarkdown
    
    Quitting from using-nichetools-with-the-package-SIBER.Rmd:96-118 [unnamed-chunk-6]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error:
    ! `object` must be an <S7_object>, not a S3<element_markdown/element_text/element>
    ---
    Backtrace:
    ...
     61.                                                                       └─ggplot2 (local) f(init, x[[i]])
     62.                                                                         └─S7::props(e2)
     63.                                                                           └─S7::check_is_S7(object)
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'using-nichetools-with-the-package-SIBER.Rmd' failed with diagnostics:
    `object` must be an <S7_object>, not a S3<element_markdown/element_text/element>
    --- failed re-building ‘using-nichetools-with-the-package-SIBER.Rmd’
    
    --- re-building ‘using-nichetools-with-the-package-nicheROVER.Rmd’ using rmarkdown
    ```

# NiLeDAM

<details>

* Version: 0.3
* GitHub: NA
* Source code: https://github.com/cran/NiLeDAM
* Date/Publication: 2023-09-18 06:00:02 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::cloud_details(, "NiLeDAM")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(NiLeDAM)
      
      ***********************************************************
      
            Welcome to the NiLeDAM package
    ...
      `expected[2:2]`:                                                      "ggplot"
      ── Failure ('test-popline.R:22:3'): test result on 'ageTests' plotted correctly ──
      typeof(p) (`actual`) not equal to "list" (`expected`).
      
      `actual`:   "object"
      `expected`: "list"  
      
      [ FAIL 8 | WARN 10 | SKIP 0 | PASS 49 ]
      Error: Test failures
      Execution halted
    ```

# NIMAA

<details>

* Version: 0.2.1
* GitHub: https://github.com/jafarilab/NIMAA
* Source code: https://github.com/cran/NIMAA
* Date/Publication: 2022-04-11 14:12:45 UTC
* Number of recursive dependencies: 179

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
                    13.21346 
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
* Number of recursive dependencies: 159

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
      `expected`: TRUE 
      ── Failure ('test-add.R:45:3'): test addition operator ─────────────────────────
      inherits(p2[[1]], "gg") is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      
      [ FAIL 2 | WARN 4 | SKIP 2 | PASS 33 ]
      Error: Test failures
      Execution halted
    ```

# nonmem2R

<details>

* Version: 0.2.5
* GitHub: NA
* Source code: https://github.com/cran/nonmem2R
* Date/Publication: 2024-03-11 17:30:02 UTC
* Number of recursive dependencies: 61

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
     22. │               └─l$compute_position(d, layout)
     23. │                 └─ggplot2 (local) compute_position(..., self = self)
     24. │                   └─self$position$setup_data(data, params)
     25. │                     └─ggplot2 (local) setup_data(..., self = self)
     26. └─base::.handleSimpleError(...)
     27.   └─rlang (local) h(simpleError(msg, call))
     28.     └─handlers[[1L]](cnd)
     29.       └─cli::cli_abort(...)
     30.         └─rlang::abort(...)
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
* Number of recursive dependencies: 126

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

# nzelect

<details>

* Version: 0.4.0
* GitHub: NA
* Source code: https://github.com/cran/nzelect
* Date/Publication: 2017-10-02 20:35:23 UTC
* Number of recursive dependencies: 79

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

# occCite

<details>

* Version: 0.5.9
* GitHub: https://github.com/ropensci/occCite
* Source code: https://github.com/cran/occCite
* Date/Publication: 2024-10-28 13:30:06 UTC
* Number of recursive dependencies: 131

Run `revdepcheck::cloud_details(, "occCite")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(occCite)
      > 
      > test_check("occCite")
      	
       OccCite query occurred on: 20 June, 2024
      	
    ...
      ── Failure ('test-sumFig.R:40:3'): sumFig works when plotting only source by species ──
      class(test[[1]][[1]]) not equal to "ggplot_built".
      Lengths differ: 4 is not 1
      ── Failure ('test-sumFig.R:48:3'): sumFig works when plotting only aggregator by species ──
      class(test[[1]][[1]]) not equal to "ggplot_built".
      Lengths differ: 4 is not 1
      
      [ FAIL 9 | WARN 1 | SKIP 20 | PASS 114 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ape’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 7 marked UTF-8 strings
    ```

# ofpetrial

<details>

* Version: 0.1.2
* GitHub: https://github.com/DIFM-Brain/ofpetrial
* Source code: https://github.com/cran/ofpetrial
* Date/Publication: 2024-12-11 23:00:02 UTC
* Number of recursive dependencies: 147

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

# ogrdbstats

<details>

* Version: 0.5.2
* GitHub: https://github.com/airr-community/ogrdbstats
* Source code: https://github.com/cran/ogrdbstats
* Date/Publication: 2024-11-03 06:10:02 UTC
* Number of recursive dependencies: 130

Run `revdepcheck::cloud_details(, "ogrdbstats")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ogrdbstats-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: make_novel_base_grobs
    > ### Title: Create plots showing base usage at selected locations in
    > ###   sequences based on novel alleles
    > ### Aliases: make_novel_base_grobs
    > 
    > ### ** Examples
    > 
    > base_grobs = make_novel_base_grobs(
    +                  example_rep$inferred_seqs,
    +                  example_rep$input_sequences,
    +                  'V',
    +                  FALSE
    +              )
    Error: <ggplot2::labels> object is invalid:
    - every label must be named.
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Using_ogrdbstats.Rmd’ using rmarkdown
    
    Quitting from Using_ogrdbstats.Rmd:277-296 [unnamed-chunk-1]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error:
    ! <ggplot2::labels> object is invalid:
    - every label must be named.
    ---
    ...
    Error: processing vignette 'Using_ogrdbstats.Rmd' failed with diagnostics:
    <ggplot2::labels> object is invalid:
    - every label must be named.
    --- failed re-building ‘Using_ogrdbstats.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Using_ogrdbstats.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# OmicNavigator

<details>

* Version: 1.15.0
* GitHub: https://github.com/abbvie-external/OmicNavigator
* Source code: https://github.com/cran/OmicNavigator
* Date/Publication: 2025-05-28 19:40:02 UTC
* Number of recursive dependencies: 85

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
    OmicNavigator R package version: 1.15.0
    The app is not installed. Install it with installApp()
    Installing study "ABC" in /tmp/RtmppgkVeU/file1ce750eab033
    Exporting study "ABC" as an R package
    Note: No maintainer email was specified. Using the placeholder: Unknown <unknown@unknown>
    Calculating pairwise overlaps. This may take a while...
    Exported study to /tmp/RtmppgkVeU/ONstudyABC
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

# ordinalsimr

<details>

* Version: 0.2.1
* GitHub: https://github.com/NeuroShepherd/ordinalsimr
* Source code: https://github.com/cran/ordinalsimr
* Date/Publication: 2025-06-04 07:10:02 UTC
* Number of recursive dependencies: 118

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

# ordr

<details>

* Version: 0.1.1
* GitHub: https://github.com/corybrunson/ordr
* Source code: https://github.com/cran/ordr
* Date/Publication: 2022-10-20 20:52:35 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "ordr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ordr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: biplot-stats
    > ### Title: Convenience stats for row and column matrix factors
    > ### Aliases: biplot-stats stat_rows_ellipse stat_cols_ellipse
    > ###   stat_rows_center stat_cols_center stat_rows_star stat_cols_star
    > ###   stat_rows_chull stat_cols_chull stat_rows_cone stat_cols_cone
    > ###   stat_rows_scale stat_cols_scale stat_rows_spantree stat_cols_spantree
    > 
    ...
    +     type = "qual", palette = 2,
    +     aesthetics = c("color", "fill")
    +   ) +
    +   ggtitle(
    +     "Row-principal PCA biplot of Anderson iris measurements",
    +     "Overlaid with 95% confidence disks"
    +   )
    Error: <ggplot2::ggplot> object properties are invalid:
    - @mapping must be <ggplot2::mapping>, not <list>
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ordr)
      Loading required package: ggplot2
      > 
      > test_check("ordr")
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 181 ]
      
    ...
          ▆
       1. └─ordr::ggbiplot(m) at test-stat-spantree.r:10:5
       2.   ├─base::`$<-`(`*tmp*`, "mapping", value = `<named list>`)
       3.   └─ggplot2:::`$<-.ggplot2::gg`(`*tmp*`, "mapping", value = `<named list>`)
       4.     └─S7::`props<-`(`*tmp*`, value = `[[<-`(S7::props(x), i, value))
       5.       └─S7::validate(object)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 181 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘cmds-variables.rmd’ using rmarkdown
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) lra-ord.Rd:54: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) lra-ord.Rd:55-56: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) lra-ord.Rd:57-58: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) lra-ord.Rd:59: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) lra-ord.Rd:60: Lost braces in \itemize; \value handles \item{}{} directly
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4 marked UTF-8 strings
    ```

# pacu

<details>

* Version: 0.1.63
* GitHub: https://github.com/cldossantos/pacu
* Source code: https://github.com/cran/pacu
* Date/Publication: 2025-05-29 21:50:02 UTC
* Number of recursive dependencies: 168

Run `revdepcheck::cloud_details(, "pacu")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘pacu.Rmd’ using rmarkdown
    --- finished re-building ‘pacu.Rmd’
    
    --- re-building ‘pacu_faq.Rmd’ using rmarkdown
    --- finished re-building ‘pacu_faq.Rmd’
    
    --- re-building ‘pacu_sat.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        extdata   4.8Mb
    ```

# pafr

<details>

* Version: 0.0.2
* GitHub: https://github.com/dwinter/pafr
* Source code: https://github.com/cran/pafr
* Date/Publication: 2020-12-08 10:20:12 UTC
* Number of recursive dependencies: 110

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

*   checking S3 generic/method consistency ... WARNING
    ```
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.dotplot_hl:
      function(object, plot, object_name)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
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
     26.   └─vctrs::vec_default_cast(...)
     27.     ├─base::withRestarts(...)
     28.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     29.     │   └─base (local) doWithOneRestart(return(expr), restart)
     30.     └─vctrs::stop_incompatible_cast(...)
     31.       └─vctrs::stop_incompatible_type(...)
     32.         └─vctrs:::stop_incompatible(...)
     33.           └─vctrs:::stop_vctrs(...)
     34.             └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘biscale.Rmd’ using rmarkdown
    ```

# paramix

<details>

* Version: 0.0.2
* GitHub: NA
* Source code: https://github.com/cran/paramix
* Date/Publication: 2025-06-10 14:50:02 UTC
* Number of recursive dependencies: 76

Run `revdepcheck::cloud_details(, "paramix")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘intro.Rmd’ using rmarkdown
    ```

# patchwork

<details>

* Version: 1.3.0
* GitHub: https://github.com/thomasp85/patchwork
* Source code: https://github.com/cran/patchwork
* Date/Publication: 2024-09-16 09:30:02 UTC
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "patchwork")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘patchwork-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_arithmetic
    > ### Title: Plot arithmetic
    > ### Aliases: plot_arithmetic -.ggplot /.ggplot |.ggplot *.gg &.gg
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
    > # Stacking and packing
    > (p1 | p2 | p3) /
    +       p4
    > 
    > # Add elements to the same nesting level
    > (p1 + (p2 + p3) + p4 + plot_layout(ncol = 1)) * theme_bw()
    Error: Can't find method for generic `*(e1, e2)`:
    - e1: <patchwork>
    - e2: <theme>
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(patchwork)
      > 
      > test_check("patchwork")
      [ FAIL 1 | WARN 1 | SKIP 11 | PASS 0 ]
      
      ══ Skipped tests (11) ══════════════════════════════════════════════════════════
    ...
      • layout/insets-can-be-changed.svg
      • layout/other-alignments-work.svg
      • layout/patchworks-can-be-inset.svg
      • layout/setting-heights-as-units.svg
      • layout/setting-heights.svg
      • layout/setting-nrow.svg
      • layout/setting-widths-as-units.svg
      • layout/setting-widths.svg
      Error: Test failures
      Execution halted
    ```

*   checking S3 generic/method consistency ... WARNING
    ```
    ggplot_gtable:
      function(plot)
    ggplot_gtable.fixed_dim_build:
      function(data)
    
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.formula:
      function(object, plot, object_name)
    
    ...
      function(object, plot, ...)
    ggplot_add.grob:
      function(object, plot, object_name)
    
    ggplot_build:
      function(plot, ...)
    ggplot_build.fixed_dim_ggplot:
      function(plot)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# pathfindR

<details>

* Version: 2.4.2
* GitHub: https://github.com/egeulgen/pathfindR
* Source code: https://github.com/cran/pathfindR
* Date/Publication: 2025-02-17 09:30:02 UTC
* Number of recursive dependencies: 142

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
* Number of recursive dependencies: 188

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

# PatientLevelPrediction

<details>

* Version: 6.4.1
* GitHub: https://github.com/OHDSI/PatientLevelPrediction
* Source code: https://github.com/cran/PatientLevelPrediction
* Date/Publication: 2025-04-20 09:40:02 UTC
* Number of recursive dependencies: 215

Run `revdepcheck::cloud_details(, "PatientLevelPrediction")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘PatientLevelPrediction-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: outcomeSurvivalPlot
    > ### Title: Plot the outcome incidence over time
    > ### Aliases: outcomeSurvivalPlot
    > 
    > ### ** Examples
    > 
    > ## Don't show: 
    ...
    Warning: Ignoring unknown labels:
    • fill : "Strata"
    • linetype : "1"
    Warning: Ignoring unknown labels:
    • fill : "Strata"
    • linetype : "1"
    Warning: Ignoring unknown labels:
    • colour : "Strata"
    Error: `object` must be an <S7_object>, not a S3<element_markdown/element_text/element>
    Execution halted
    ```

# patientProfilesVis

<details>

* Version: 2.0.9
* GitHub: https://github.com/openanalytics/patientProfilesVis
* Source code: https://github.com/cran/patientProfilesVis
* Date/Publication: 2024-06-18 09:00:02 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "patientProfilesVis")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘patientProfiles-template-SDTM.Rmd’ using rmarkdown
    --- finished re-building ‘patientProfiles-template-SDTM.Rmd’
    
    --- re-building ‘patientProfilesVis-introduction.Rmd’ using rmarkdown
    
    Quitting from patientProfilesVis-introduction.Rmd:167-171 [text-wideFormat-include]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ...
    
    Error: processing vignette 'patientProfilesVis-introduction.Rmd' failed with diagnostics:
    The `plot.margin` theme element must be a <unit> vector of length 4
    --- failed re-building ‘patientProfilesVis-introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘patientProfilesVis-introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(patientProfilesVis)
      > 
      > test_check("patientProfilesVis")
      [ FAIL 21 | WARN 23 | SKIP 20 | PASS 330 ]
      
      == Skipped tests (20) ==================================================================================================================================================================================
    ...
       24.                           \-ggplot2:::plot_theme(plot)
       25.                             \-ggplot2:::check_theme(theme)
       26.                               \-base::mapply(...)
       27.                                 \-ggplot2 (local) `<fn>`(...)
       28.                                   \-cli::cli_abort(...)
       29.                                     \-rlang::abort(...)
      
      [ FAIL 21 | WARN 23 | SKIP 20 | PASS 330 ]
      Error: Test failures
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        doc   5.2Mb
    ```

# pcr

<details>

* Version: 1.2.2
* GitHub: https://github.com/MahShaaban/pcr
* Source code: https://github.com/cran/pcr
* Date/Publication: 2020-04-01 06:10:02 UTC
* Number of recursive dependencies: 68

Run `revdepcheck::cloud_details(, "pcr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(pcr)
      > 
      > test_check("pcr")
      [ FAIL 12 | WARN 1 | SKIP 0 | PASS 74 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      ── Failure ('test_plotting_fun.R:68:3'): .pcr_plot_assess returns an efficiency plot ──
      class(gg) not identical to c("gg", "ggplot").
      Lengths differ: 5 is not 2
      ── Failure ('test_plotting_fun.R:79:3'): .pcr_plot_assess returns standard_curve plots ──
      class(gg) not identical to c("gg", "ggplot").
      Lengths differ: 5 is not 2
      
      [ FAIL 12 | WARN 1 | SKIP 0 | PASS 74 ]
      Error: Test failures
      Execution halted
    ```

# pdxTrees

<details>

* Version: 0.4.0
* GitHub: https://github.com/mcconvil/pdxTrees
* Source code: https://github.com/cran/pdxTrees
* Date/Publication: 2020-08-17 14:00:02 UTC
* Number of recursive dependencies: 102

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

# pedbp

<details>

* Version: 2.0.2
* GitHub: https://github.com/dewittpe/pedbp
* Source code: https://github.com/cran/pedbp
* Date/Publication: 2025-01-07 20:40:01 UTC
* Number of recursive dependencies: 116

Run `revdepcheck::cloud_details(, "pedbp")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘test-bmi_for_age.R’
      Running ‘test-bp-by-source.R’
      Running ‘test-bp_cdf.R’
    Running the tests in ‘tests/test-bp_cdf.R’ failed.
    Complete output:
      > library(pedbp)
      > 
      > ################################################################################
      > x <- bp_cdf(age = 96, male = 1, sbp = 103, dbp = 55)
      > stopifnot(identical(class(x), c("gg", "ggplot")))
      Error: identical(class(x), c("gg", "ggplot")) is not TRUE
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        libs   5.3Mb
    ```

# personalized

<details>

* Version: 0.2.7
* GitHub: https://github.com/jaredhuling/personalized
* Source code: https://github.com/cran/personalized
* Date/Publication: 2022-06-27 20:20:03 UTC
* Number of recursive dependencies: 90

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
      Loaded glmnet 4.1-9
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

# phylepic

<details>

* Version: 0.2.0
* GitHub: https://github.com/cidm-ph/phylepic
* Source code: https://github.com/cran/phylepic
* Date/Publication: 2024-05-31 19:10:02 UTC
* Number of recursive dependencies: 85

Run `revdepcheck::cloud_details(, "phylepic")` for more info

</details>

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
    ggplot_gtable:
      function(plot)
    ggplot_gtable.phylepic_ggplot_build:
      function(data)
    
    ggplot_build:
      function(plot, ...)
    ggplot_build.phylepic_ggplot:
      function(plot)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# PieGlyph

<details>

* Version: 1.0.0
* GitHub: https://github.com/rishvish/PieGlyph
* Source code: https://github.com/cran/PieGlyph
* Date/Publication: 2024-06-28 12:00:02 UTC
* Number of recursive dependencies: 87

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
* Number of recursive dependencies: 80

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
* Number of recursive dependencies: 80

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

# plotdap

<details>

* Version: 1.0.3
* GitHub: https://github.com/rmendels/plotdap
* Source code: https://github.com/cran/plotdap
* Date/Publication: 2023-10-17 22:00:15 UTC
* Number of recursive dependencies: 109

Run `revdepcheck::cloud_details(, "plotdap")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘plotdap-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: add_ggplot
    > ### Title: Add ggplot2 elements to a plotdap object
    > ### Aliases: add_ggplot
    > 
    > ### ** Examples
    > 
    > 
    ...
    +   crs = "+proj=laea +y_0=0 +lon_0=155 +lat_0=-90 +ellps=WGS84 +no_defs")
    Loading required package: maps
    > p <- add_ggplot(
    +  p,
    +  ggplot2::theme_bw()
    + )
    Error in .Primitive("@")(NULL, properties) : 
      no applicable method for `@` applied to an object of class "NULL"
    Calls: add_ggplot ... <Anonymous> -> validate -> validate_properties -> @ -> do.call
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘using_plotdap.Rmd’ using rmarkdown
    
    Quitting from using_plotdap.Rmd:57-61 [world]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error:
    ! no applicable method for `@` applied to an object of class "NULL"
    ---
    Backtrace:
    ...
    
    Error: processing vignette 'using_plotdap.Rmd' failed with diagnostics:
    no applicable method for `@` applied to an object of class "NULL"
    --- failed re-building ‘using_plotdap.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘using_plotdap.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# plotDK

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/plotDK
* Date/Publication: 2021-10-01 08:00:02 UTC
* Number of recursive dependencies: 82

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
      [ FAIL 4 | WARN 0 | SKIP 0 | PASS 46 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      ── Failure ('test-plotDK.R:72:5'): The returned plot has the correct labels ────
      labels$fill not equal to "test_label".
      target is NULL, current is character
      ── Failure ('test-plotDK.R:81:5'): The returned plot has the correct labels ────
      labels$fill not equal to "fill".
      target is NULL, current is character
      
      [ FAIL 4 | WARN 0 | SKIP 0 | PASS 46 ]
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

# PlotFTIR

<details>

* Version: 1.2.0
* GitHub: https://github.com/NRCan/PlotFTIR
* Source code: https://github.com/cran/PlotFTIR
* Date/Publication: 2025-03-31 15:30:06 UTC
* Number of recursive dependencies: 104

Run `revdepcheck::cloud_details(, "PlotFTIR")` for more info

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
      `expected` is a character vector ('Transmittance (a.u.)')
      ── Failure ('test-plot_ftir.R:28:3'): Plots are generated ──────────────────────
      p5$label$y (`actual`) not equal to "Normalized Absorbance" (`expected`).
      
      `actual` is NULL
      `expected` is a character vector ('Normalized Absorbance')
      
      [ FAIL 6 | WARN 58 | SKIP 0 | PASS 513 ]
      Error: Test failures
      Execution halted
    ```

# plotly

<details>

* Version: 4.10.4
* GitHub: https://github.com/plotly/plotly.R
* Source code: https://github.com/cran/plotly
* Date/Publication: 2024-01-13 22:40:02 UTC
* Number of recursive dependencies: 134

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

* Version: 0.6.0
* GitHub: https://github.com/craig-parylo/plotor
* Source code: https://github.com/cran/plotor
* Date/Publication: 2025-05-28 11:50:02 UTC
* Number of recursive dependencies: 139

Run `revdepcheck::cloud_details(, "plotor")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘plotor-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: table_or
    > ### Title: Table OR
    > ### Aliases: table_or
    > 
    > ### ** Examples
    > 
    > # get some data
    ...
     40. │                                               └─self$draw_panel(...)
     41. │                                                 └─ggplot2 (local) draw_panel(..., self = self)
     42. │                                                   └─grid:::Ops.unit(data$linewidth, 0)
     43. │                                                     └─base::stop(...)
     44. └─base::.handleSimpleError(...)
     45.   └─rlang (local) h(simpleError(msg, call))
     46.     └─handlers[[1L]](cnd)
     47.       └─cli::cli_abort(...)
     48.         └─rlang::abort(...)
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
      ── Failure ('test-plot_or.R:7:3'): `plot_or()` does not produce messages or warnings ──
      `{ ... }` produced warnings.
      ── Failure ('test-plot_or.R:13:3'): `plot_or()` does not produce messages or warnings ──
      `{ ... }` produced warnings.
      
      [ FAIL 2 | WARN 24 | SKIP 1 | PASS 26 ]
      Deleting unused snapshots:
      • plot_or/plot-diabetes.svg
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘using_plotor.Rmd’ using rmarkdown
    ```

# plotROC

<details>

* Version: 2.3.1
* GitHub: https://github.com/sachsmc/plotROC
* Source code: https://github.com/cran/plotROC
* Date/Publication: 2023-10-06 12:40:02 UTC
* Number of recursive dependencies: 73

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

* Version: 0.7.0
* GitHub: https://github.com/pwwang/plotthis
* Source code: https://github.com/cran/plotthis
* Date/Publication: 2025-05-31 05:00:02 UTC
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
     26. │                             └─ggplot2 (local) FUN(X[[i]], ...)
     27. │                               └─self$draw_group(group, panel_params, coord, ...)
     28. │                                 └─ggplot2 (local) draw_group(...)
     29. │                                   └─ggplot2 (local) draw_group(..., self = self)
     30. └─base::.handleSimpleError(...)
     31.   └─rlang (local) h(simpleError(msg, call))
     32.     └─handlers[[1L]](cnd)
     33.       └─cli::cli_abort(...)
     34.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        help   3.3Mb
    ```

# pmartR

<details>

* Version: 2.5.0
* GitHub: https://github.com/pmartR/pmartR
* Source code: https://github.com/cran/pmartR
* Date/Publication: 2025-04-23 18:00:02 UTC
* Number of recursive dependencies: 143

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
* Number of recursive dependencies: 81

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
       44.     └─handlers[[1L]](cnd)
       45.       └─cli::cli_abort(...)
       46.         └─rlang::abort(...)
      
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

# pollster

<details>

* Version: 0.1.6
* GitHub: NA
* Source code: https://github.com/cran/pollster
* Date/Publication: 2023-05-12 19:00:05 UTC
* Number of recursive dependencies: 67

Run `revdepcheck::cloud_details(, "pollster")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘crosstab3way.Rmd’ using rmarkdown
    
    Quitting from crosstab3way.Rmd:40-54 [unnamed-chunk-3]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error:
    ! <ggplot2::labels> object is invalid:
    - every label must be named.
    ---
    ...
     3.     └─S7::new_object(labels)
     4.       └─S7::validate(object, recursive = !parent_validated)
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'crosstab3way.Rmd' failed with diagnostics:
    <ggplot2::labels> object is invalid:
    - every label must be named.
    --- failed re-building ‘crosstab3way.Rmd’
    
    --- re-building ‘crosstabs.Rmd’ using rmarkdown
    ```

# poppr

<details>

* Version: 2.9.6
* GitHub: https://github.com/grunwaldlab/poppr
* Source code: https://github.com/cran/poppr
* Date/Publication: 2024-03-15 17:40:02 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::cloud_details(, "poppr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘poppr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: genotype_curve
    > ### Title: Produce a genotype accumulation curve
    > ### Aliases: genotype_curve
    > 
    > ### ** Examples
    > 
    > data(nancycats)
    > nan_geno <- genotype_curve(nancycats)
    Error: <ggplot2::labels> object is invalid:
    - every label must be named.
    Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘algo.Rnw’ using knitr
    Error: processing vignette 'algo.Rnw' failed with diagnostics:
    Running 'texi2dvi' on 'algo.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `colortbl.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    ...
    l.4 \makeatletter
                     ^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘algo.Rnw’
    
    SUMMARY: processing the following file failed:
      ‘algo.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# PoweREST

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/PoweREST
* Date/Publication: 2024-09-09 09:30:02 UTC
* Number of recursive dependencies: 180

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
* Number of recursive dependencies: 101

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
* Number of recursive dependencies: 25

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

# predint

<details>

* Version: 2.2.1
* GitHub: https://github.com/MaxMenssen/predint
* Source code: https://github.com/cran/predint
* Date/Publication: 2024-03-04 15:10:03 UTC
* Number of recursive dependencies: 68

Run `revdepcheck::cloud_details(, "predint")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(predint)
      Loading required package: ggplot2
      Loading required package: lme4
      Loading required package: Matrix
      Loading required package: MASS
      
    ...
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-plot.R:16:9'): plot must be a ggplot ─────────────────────────
      is.list(qp_plot) is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 304 ]
      Error: Test failures
      Execution halted
    ```

# PredPsych

<details>

* Version: 0.4
* GitHub: NA
* Source code: https://github.com/cran/PredPsych
* Date/Publication: 2019-07-23 08:20:05 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::cloud_details(, "PredPsych")` for more info

</details>

## Newly broken

*   checking whether package ‘PredPsych’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘e1071::element’ by ‘ggplot2::element’ when loading ‘PredPsych’
    See ‘/tmp/workdir/PredPsych/new/PredPsych.Rcheck/00install.out’ for details.
    ```

# predRupdate

<details>

* Version: 0.2.0
* GitHub: https://github.com/GlenMartin31/predRupdate
* Source code: https://github.com/cran/predRupdate
* Date/Publication: 2024-08-23 14:20:02 UTC
* Number of recursive dependencies: 112

Run `revdepcheck::cloud_details(, "predRupdate")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(predRupdate)
      > 
      > test_check("predRupdate")
      $OE_ratio
      [1] 1.900612
      
    ...
      `expected[2:2]`:                                                      "ggplot"
      ── Failure ('test-flex_calplot.R:28:3'): flex_calplot works ────────────────────
      class(...) (`actual`) not equal to c("gg", "ggplot") (`expected`).
      
      `actual`:        "ggplot2::ggplot" "ggplot" "ggplot2::gg" "S7_object" "gg"    
      `expected[2:2]`:                                                      "ggplot"
      
      [ FAIL 2 | WARN 0 | SKIP 4 | PASS 229 ]
      Error: Test failures
      Execution halted
    ```

# priorsense

<details>

* Version: 1.1.0
* GitHub: https://github.com/n-kall/priorsense
* Source code: https://github.com/cran/priorsense
* Date/Publication: 2025-04-10 14:00:02 UTC
* Number of recursive dependencies: 120

Run `revdepcheck::cloud_details(, "priorsense")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(priorsense)
      > 
      > test_check("priorsense")
      [ FAIL 1 | WARN 0 | SKIP 2 | PASS 91 ]
      
      ══ Skipped tests (2) ═══════════════════════════════════════════════════════════
      • On CRAN (2): 'test_cmdstan.R:5:3', 'test_moment_matching.R:15:3'
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test_plots.R:103:5'): pagination of plots works as expected ───────
      powerscale_plot_quantities(ps, variables_per_page = Inf) has length 1, not length 11.
      
      [ FAIL 1 | WARN 0 | SKIP 2 | PASS 91 ]
      Error: Test failures
      Execution halted
    ```

# probably

<details>

* Version: 1.1.0
* GitHub: https://github.com/tidymodels/probably
* Source code: https://github.com/cran/probably
* Date/Publication: 2025-05-21 13:30:02 UTC
* Number of recursive dependencies: 129

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
      `expected` is a list
      ── Failure ('test-cal-plot.R:256:3'): Binary logistic functions work with group argument ──
      res$labels (`actual`) not equal to list(x = "Probability", y = "Predicted Event Rate") (`expected`).
      
      `actual` is an S7 object of class <ggplot2::labels>
      `expected` is a list
      
      [ FAIL 2 | WARN 0 | SKIP 84 | PASS 549 ]
      Error: Test failures
      Execution halted
    ```

# processmapR

<details>

* Version: 0.5.6
* GitHub: https://github.com/bupaverse/processmapr
* Source code: https://github.com/cran/processmapR
* Date/Publication: 2024-12-03 12:50:02 UTC
* Number of recursive dependencies: 114

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
      `chart` inherits from 'ggplot2::ggplot'/'ggplot'/'ggplot2::gg'/'S7_object'/'gg' not 'plotly'.
      
      [ FAIL 6 | WARN 0 | SKIP 11 | PASS 105 ]
      Error: Test failures
      Execution halted
    ```

# psborrow

<details>

* Version: 0.2.2
* GitHub: NA
* Source code: https://github.com/cran/psborrow
* Date/Publication: 2025-02-19 13:40:02 UTC
* Number of recursive dependencies: 103

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
      [ FAIL 20 | WARN 0 | SKIP 1 | PASS 134 ]
      
    ...
      `expected`: "gg"             
      ── Failure ('test-plots.R:126:5'): Ensure output is producing a ggplot2 object with appropriate parameters ──
      p1$labels$yintercept (`actual`) not equal to "ref" (`expected`).
      
      `actual` is NULL
      `expected` is a character vector ('ref')
      
      [ FAIL 20 | WARN 0 | SKIP 1 | PASS 134 ]
      Error: Test failures
      Execution halted
    ```

*   checking whether package ‘psborrow’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
    See ‘/tmp/workdir/psborrow/new/psborrow.Rcheck/00install.out’ for details.
    ```

# psc

<details>

* Version: 1.3.0
* GitHub: https://github.com/richJJackson/psc
* Source code: https://github.com/cran/psc
* Date/Publication: 2025-05-29 15:10:02 UTC
* Number of recursive dependencies: 190

Run `revdepcheck::cloud_details(, "psc")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘psc-vignette.Rmd’ using rmarkdown
    
    Quitting from psc-vignette.Rmd:176-178 [unnamed-chunk-5]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'psc-vignette.Rmd' failed with diagnostics:
    ...
    ℹ In index: 1.
    Caused by error:
    ! Can't find method for `build_ggplot(S3<gg/ggplot>)`.
    --- failed re-building ‘psc-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘psc-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# pscore

<details>

* Version: 0.4.0
* GitHub: https://github.com/JWiley/score-project
* Source code: https://github.com/cran/pscore
* Date/Publication: 2022-05-13 22:30:02 UTC
* Number of recursive dependencies: 156

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
* Number of recursive dependencies: 158

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

# qad

<details>

* Version: 1.0.4
* GitHub: https://github.com/griefl/qad
* Source code: https://github.com/cran/qad
* Date/Publication: 2022-12-14 16:50:02 UTC
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "qad")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(qad)
      > 
      > test_check("qad")
      Intervalls for the variable x2 
        Interval lowerBound upperBound
      1       I1          1          4
    ...
      `expected[2:2]`:                                                      "ggplot"
      ── Failure ('test-utilityfunctions.R:33:3'): plot_density ──────────────────────
      class(plot_density(fit$mass_matrix)) (`actual`) not equal to c("gg", "ggplot") (`expected`).
      
      `actual`:        "ggplot2::ggplot" "ggplot" "ggplot2::gg" "S7_object" "gg"    
      `expected[2:2]`:                                                      "ggplot"
      
      [ FAIL 10 | WARN 26 | SKIP 0 | PASS 71 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) ECBC.Rd:39: Lost braces; missing escapes or markup?
        39 | Let (u_i,v_i) := (F_n(x_i),G_n(y_i)) be the pseudo-observations for i in \{1,\ldots,n\} and (u_1',v_1'),\ldots, (u_m',v_m') the distinct pairs of pseudo-observations with m leq n. Moreover set S_1:=\{0, u_1, \ldots, u_{m_1}\} and S_2:=\{0, v_1,\ldots, v_{m_2}\} and define the quantities t_i,r_i and s_i for i=1,\ldots, m by
           |                                                                                                                                                                                                                           ^
    checkRd: (-1) ECBC.Rd:39: Lost braces; missing escapes or markup?
        39 | Let (u_i,v_i) := (F_n(x_i),G_n(y_i)) be the pseudo-observations for i in \{1,\ldots,n\} and (u_1',v_1'),\ldots, (u_m',v_m') the distinct pairs of pseudo-observations with m leq n. Moreover set S_1:=\{0, u_1, \ldots, u_{m_1}\} and S_2:=\{0, v_1,\ldots, v_{m_2}\} and define the quantities t_i,r_i and s_i for i=1,\ldots, m by
           |                                                                                                                                                                                                                                                               ^
    checkRd: (-1) ECBC.Rd:49: Lost braces; missing escapes or markup?
        49 | and set the measure of the empirical copula mu_{A_n}^B := 1/n sum_{i=1}^m t_i mu_B^{w_i}, where B denotes the product copula.
           |                                                ^
    checkRd: (-1) ECBC.Rd:49: Lost braces; missing escapes or markup?
    ...
           |                                                                                                                                                                                                                                                               ^
    checkRd: (-1) emp_c_copula.Rd:52: Lost braces; missing escapes or markup?
        52 | and set the measure of the empirical copula mu_{A_n}^B := 1/n sum_{i=1}^m t_i mu_B^{w_i}, where B denotes the product copula.
           |                                                ^
    checkRd: (-1) emp_c_copula.Rd:52: Lost braces; missing escapes or markup?
        52 | and set the measure of the empirical copula mu_{A_n}^B := 1/n sum_{i=1}^m t_i mu_B^{w_i}, where B denotes the product copula.
           |                                                                   ^
    checkRd: (-1) emp_c_copula.Rd:52: Lost braces; missing escapes or markup?
        52 | and set the measure of the empirical copula mu_{A_n}^B := 1/n sum_{i=1}^m t_i mu_B^{w_i}, where B denotes the product copula.
           |                                                                                    ^
    ```

# qdap

<details>

* Version: 2.4.6
* GitHub: https://github.com/trinker/qdap
* Source code: https://github.com/cran/qdap
* Date/Publication: 2023-05-11 06:10:02 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::cloud_details(, "qdap")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library("testthat")
      > library("qdap")
      Loading required package: qdapDictionaries
      Loading required package: qdapRegex
      Loading required package: qdapTools
      Loading required package: RColorBrewer
      
    ...
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-dispersion_plot.R:6:5'): dispersion_plot outputs a ggplot object ──
      all(class(m) == c("gg", "ggplot")) is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      
      [ FAIL 1 | WARN 3 | SKIP 0 | PASS 217 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        data   2.0Mb
        help   1.5Mb
    ```

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) check_text.Rd:19: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) check_text.Rd:20: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) check_text.Rd:21: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) check_text.Rd:22: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) check_text.Rd:23: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) check_text.Rd:24: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) check_text.Rd:25: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) check_text.Rd:26: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) check_text.Rd:27: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) check_text.Rd:28: Lost braces in \itemize; \value handles \item{}{} directly
    ...
    checkRd: (-1) word_stats.Rd:70: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) word_stats.Rd:71: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) word_stats.Rd:72: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) word_stats.Rd:73: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) word_stats.Rd:74: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) word_stats.Rd:75: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) word_stats.Rd:76: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) word_stats.Rd:77: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) word_stats.Rd:78: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) word_stats.Rd:79: Lost braces in \itemize; \value handles \item{}{} directly
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘gplots’
    ```

# qgcompint

<details>

* Version: 1.0.0
* GitHub: https://github.com/alexpkeil1/qgcompint
* Source code: https://github.com/cran/qgcompint
* Date/Publication: 2025-03-12 16:40:02 UTC
* Number of recursive dependencies: 155

Run `revdepcheck::cloud_details(, "qgcompint")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘qgcompint-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.qgcompemmfit
    > ### Title: Default plotting method for a qgcompemmfit object
    > ### Aliases: plot.qgcompemmfit
    > 
    > ### ** Examples
    > 
    > set.seed(50)
    ...
    > pp1 = plot(qfit4, emmval=1, suppressprint=TRUE)
    > pp2 = plot(qfit4, emmval=2, suppressprint=TRUE)
    > pp1 + theme_linedraw() # can use with other ggplot functions
    > 
    > # overlay (fussy to work with)
    > #ppom <- ggplot_build(pp0 + pp1[2] + pp2[[2]] + scale_color_discrete(guide="none"))
    > ppom <- ggplot_build(pp0ee + pp1ee[2] + pp2ee[[2]] + scale_color_discrete(guide="none"))
    Error in S7::prop(x, "meta")[[i]] : subscript out of bounds
    Calls: ggplot_build -> [[ -> [[.ggplot2::gg
    Execution halted
    ```

# qicharts2

<details>

* Version: 0.8.0
* GitHub: https://github.com/anhoej/qicharts2
* Source code: https://github.com/cran/qicharts2
* Date/Publication: 2025-05-15 08:00:02 UTC
* Number of recursive dependencies: 67

Run `revdepcheck::cloud_details(, "qicharts2")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘qicharts2-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: bchart
    > ### Title: Bernoulli CUSUM chart for binary data (EXPERIMENTAL)
    > ### Aliases: bchart
    > 
    > ### ** Examples
    > 
    > # Generate 1000 random successes and failures with success rate = 0.02
    ...
    ℹ Please use tidy evaluation idioms with `aes()`
    ℹ The deprecated feature was likely used in the qicharts2 package.
      Please report the issue to the authors.
    Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ℹ Please use `linewidth` instead.
    ℹ The deprecated feature was likely used in the qicharts2 package.
      Please report the issue to the authors.
    Error: <ggplot2::labels> object is invalid:
    - every label must be named.
    Execution halted
    ```

# quantities

<details>

* Version: 0.2.3
* GitHub: https://github.com/r-quantities/quantities
* Source code: https://github.com/cran/quantities
* Date/Publication: 2025-01-18 21:20:02 UTC
* Number of recursive dependencies: 72

Run `revdepcheck::cloud_details(, "quantities")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘introduction.Rmd’ using rmarkdown
    ```

# quickPlot

<details>

* Version: 1.0.2
* GitHub: https://github.com/PredictiveEcology/quickPlot
* Source code: https://github.com/cran/quickPlot
* Date/Publication: 2023-07-02 05:20:02 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::cloud_details(, "quickPlot")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Complete output:
      > library(testthat)
      > test_check("quickPlot")
      Loading required package: quickPlot
      Error in pgmatrix(grobToPlot, col, real, size, minv, maxv, legend, legendText,  : 
        object 'nrowLegText' not found
      Error in pgmatrix(grobToPlot, col, real, size, minv, maxv, legend, legendText,  : 
        object 'nrowLegText' not found
    ...
      • Plot/test46.png
      • Plot/test47.png
      • Plot/test5.png
      • Plot/test50.png
      • Plot/test6.png
      • Plot/test7.png
      • Plot/test8.png
      • Plot/test9.png
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘iii-plotting.Rmd’ using rmarkdown
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘fastshp’
    ```

# quickReg

<details>

* Version: 1.5.0
* GitHub: NA
* Source code: https://github.com/cran/quickReg
* Date/Publication: 2017-09-28 06:41:12 UTC
* Number of recursive dependencies: 71

Run `revdepcheck::cloud_details(, "quickReg")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘quickReg.Rmd’ using rmarkdown
    
    Quitting from quickReg.Rmd:130-162 [plot]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error:
    ! <ggplot2::labels> object is invalid:
    - every label must be named.
    ---
    ...
    Error: processing vignette 'quickReg.Rmd' failed with diagnostics:
    <ggplot2::labels> object is invalid:
    - every label must be named.
    --- failed re-building ‘quickReg.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘quickReg.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘psych’
      All declared Imports should be used.
    ```

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) display_table.Rd:37: Lost braces; missing escapes or markup?
        37 | \item{normtest}{A character indicating test of normality, the default method is \code{\link{shapiro.test}} when sample size no more than 5000, otherwise \code{\link[nortest]{lillie.test}} {Kolmogorov-Smirnov} is used, see package \strong{nortest} for more methods.Use 'shapiro.test', 'lillie.test', 'ad.test', etc to specify methods.}
           |                                                                                                                                                                                             ^
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘PredictABEL’
    ```

# R4GoodPersonalFinances

<details>

* Version: 1.0.0
* GitHub: https://github.com/R4GoodAcademy/R4GoodPersonalFinances
* Source code: https://github.com/cran/R4GoodPersonalFinances
* Date/Publication: 2025-06-04 11:00:09 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::cloud_details(, "R4GoodPersonalFinances")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘R4GoodPersonalFinances-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_retirement_ruin
    > ### Title: Plotting retirement ruin
    > ### Aliases: plot_retirement_ruin
    > 
    > ### ** Examples
    > 
    > plot_retirement_ruin(
    ...
    +   portfolio_return_mean = 0.034,
    +   portfolio_return_sd   = 0.15,
    +   age                   = 65,
    +   gompertz_mode         = 88,
    +   gompertz_dispersion   = 10,
    +   portfolio_value       = 1000000,
    +   monthly_spendings     = 3000
    + )
    Error: `object` must be an <S7_object>, not a S3<element_markdown/element_text/element>
    Execution halted
    ```

# Radviz

<details>

* Version: 0.9.4
* GitHub: https://github.com/yannabraham/Radviz
* Source code: https://github.com/cran/Radviz
* Date/Publication: 2025-05-09 06:30:02 UTC
* Number of recursive dependencies: 61

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

# RamanMP

<details>

* Version: 1.0
* GitHub: NA
* Source code: https://github.com/cran/RamanMP
* Date/Publication: 2021-07-09 08:10:04 UTC
* Number of recursive dependencies: 59

Run `revdepcheck::cloud_details(, "RamanMP")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘RamanMP-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: peak.finder
    > ### Title: Peaks identification
    > ### Aliases: peak.finder
    > ### Keywords: peaks
    > 
    > ### ** Examples
    > 
    > data("MPdatabase")
    > peak.data<-peak.finder(MPdatabase[,c(1,7)], threshold = 500, m=7)
    Error: <ggplot2::element_text> object properties are invalid:
    - @size must be <NULL>, <integer>, or <double>, not <character>
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.1Mb
      sub-directories of 1Mb or more:
        data   8.0Mb
    ```

# randomForestExplainer

<details>

* Version: 0.10.1
* GitHub: https://github.com/ModelOriented/randomForestExplainer
* Source code: https://github.com/cran/randomForestExplainer
* Date/Publication: 2020-07-11 20:30:02 UTC
* Number of recursive dependencies: 89

Run `revdepcheck::cloud_details(, "randomForestExplainer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘randomForestExplainer-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_importance_ggpairs
    > ### Title: Plot importance measures with ggpairs
    > ### Aliases: plot_importance_ggpairs
    > 
    > ### ** Examples
    > 
    > forest <- randomForest::randomForest(Species ~ ., data = iris, localImp = TRUE, ntree = 200)
    > frame <- measure_importance(forest, measures = c("mean_min_depth", "times_a_root"))
    > plot_importance_ggpairs(frame, measures = c("mean_min_depth", "times_a_root"))
    Error in `+.gg`(e1, item) : 
      'ggmatrix' does not know how to add objects that do not have class 'theme', 'labels' or 'ggproto'. Received object with class: 'character'
    Calls: plot_importance_ggpairs -> +.gg -> add_list_to_ggmatrix -> +.gg
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(randomForestExplainer)
      > 
      > test_check("randomForestExplainer")
      [ FAIL 12 | WARN 71 | SKIP 0 | PASS 49 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      Backtrace:
          ▆
       1. └─randomForestExplainer::plot_importance_rankings(ranger_s) at test_ranger.R:185:3
       2.   └─GGally:::`+.gg`(plot, ggtitle(main))
       3.     └─GGally:::add_list_to_ggmatrix(e1, e2)
       4.       └─GGally:::`+.gg`(e1, item)
      
      [ FAIL 12 | WARN 71 | SKIP 0 | PASS 49 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘randomForestExplainer.Rmd’ using rmarkdown
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# rassta

<details>

* Version: 1.0.6
* GitHub: https://github.com/bafuentes/rassta
* Source code: https://github.com/cran/rassta
* Date/Publication: 2024-08-19 06:20:13 UTC
* Number of recursive dependencies: 105

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
* Number of recursive dependencies: 99

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
* Number of recursive dependencies: 88

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

# rbioacc

<details>

* Version: 1.2.1
* GitHub: NA
* Source code: https://github.com/cran/rbioacc
* Date/Publication: 2024-02-27 01:40:02 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "rbioacc")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(rbioacc)
      > 
      > test_check("rbioacc")
      
      SAMPLING FOR MODEL 'TK' NOW (CHAIN 1).
      Chain 1: 
    ...
      `expected`: TRUE 
      ── Failure ('test-ppc.R:16:5'): plot ppc ───────────────────────────────────────
      all(class(ppc(fit_MGSG)) == c("gg", "ggplot")) is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      
      [ FAIL 20 | WARN 103 | SKIP 9 | PASS 60 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 94.5Mb
      sub-directories of 1Mb or more:
        libs  93.8Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# rbiom

<details>

* Version: 2.2.0
* GitHub: https://github.com/cmmr/rbiom
* Source code: https://github.com/cran/rbiom
* Date/Publication: 2025-04-04 20:20:02 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "rbiom")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rbiom-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: bdiv_heatmap
    > ### Title: Display beta diversities in an all vs all grid.
    > ### Aliases: bdiv_heatmap
    > 
    > ### ** Examples
    > 
    >     library(rbiom)
    ...
     18.                   │ │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
     19.                   │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
     20.                   │ └─base::withCallingHandlers(...)
     21.                   └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     22.                     └─l$layer_data(plot@data)
     23.                       └─ggplot2::layer_data(..., self = self)
     24.                         └─ggplot2:::unrowname(data)
     25.                           └─cli::cli_abort("Can only remove rownames from {.cls data.frame} and {.cls matrix} objects.")
     26.                             └─rlang::abort(...)
    Execution halted
    ```

# reda

<details>

* Version: 0.5.4
* GitHub: https://github.com/wenjie2wang/reda
* Source code: https://github.com/cran/reda
* Date/Publication: 2022-07-08 21:50:02 UTC
* Number of recursive dependencies: 50

Run `revdepcheck::cloud_details(, "reda")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘tinytest.R’
    Running the tests in ‘tests/tinytest.R’ failed.
    Complete output:
      > if (requireNamespace("tinytest", quietly = TRUE) &&
      +     utils::packageVersion("tinytest") >= "1.1.0") {
      + 
      +     ## Set a seed to make the test deterministic
      +     set.seed(808)
      + 
      +     tinytest::test_package("reda", ncpu = NULL,
    ...
       call| -->    lty = 1:4, col = 1:4)), c("gg", "ggplot"))
       diff| Lengths (2, 5) differ (string compare on first 2)
       diff| 1 string mismatch
      ----- FAILED[data]: test_rateReg.R<152--152>
       call| expect_equivalent(class(plot(mcf_splineFit)), c("gg", "ggplot"))
       diff| Lengths (2, 5) differ (string compare on first 2)
       diff| 1 string mismatch
      Error: 8 out of 191 tests failed
      In addition: There were 16 warnings (use warnings() to see them)
      Execution halted
    ```

## In both

*   checking C++ specification ... NOTE
    ```
      Specified C++11: please drop specification unless essential
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.0Mb
      sub-directories of 1Mb or more:
        doc    3.3Mb
        libs   4.0Mb
    ```

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) simEvent.Rd:112: Lost braces; missing escapes or markup?
       112 | and another argument named {zCoef} for covariate coefficient vector.
           |                            ^
    ```

# regtomean

<details>

* Version: 1.2
* GitHub: NA
* Source code: https://github.com/cran/regtomean
* Date/Publication: 2024-12-17 15:00:02 UTC
* Number of recursive dependencies: 83

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
* Number of recursive dependencies: 175

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
     33. │                                         └─ggplot2 (local) FUN(X[[i]], ...)
     34. │                                           └─self$draw_group(group, panel_params, coord, ...)
     35. │                                             └─ggplot2 (local) draw_group(...)
     36. │                                               └─ggplot2 (local) draw_group(..., self = self)
     37. └─base::.handleSimpleError(...)
     38.   └─rlang (local) h(simpleError(msg, call))
     39.     └─handlers[[1L]](cnd)
     40.       └─cli::cli_abort(...)
     41.         └─rlang::abort(...)
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
       33. │                                         └─ggplot2 (local) draw_group(..., self = self)
       34. └─base::.handleSimpleError(...)
       35.   └─rlang (local) h(simpleError(msg, call))
       36.     └─handlers[[1L]](cnd)
       37.       └─cli::cli_abort(...)
       38.         └─rlang::abort(...)
      
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

# RevGadgets

<details>

* Version: 1.2.1
* GitHub: https://github.com/revbayes/RevGadgets
* Source code: https://github.com/cran/RevGadgets
* Date/Publication: 2023-11-29 20:30:02 UTC
* Number of recursive dependencies: 131

Run `revdepcheck::cloud_details(, "RevGadgets")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(RevGadgets)
      > 
      > test_check("RevGadgets")
      
        |                                              
        |                                        |   0%
    ...
      Backtrace:
          ▆
       1. ├─RevGadgets::plotFBDTree(...) at test_plotFBDTree.R:12:3
       2. │ └─RevGadgets:::plotTreeFull(...)
       3. └─base::loadNamespace(x)
       4.   └─base (local) runHook(".onLoad", env, package.lib, package)
      
      [ FAIL 1 | WARN 11 | SKIP 0 | PASS 138 ]
      Error: Test failures
      Execution halted
    ```

# rifreg

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/rifreg
* Date/Publication: 2024-05-01 18:42:07 UTC
* Number of recursive dependencies: 79

Run `revdepcheck::cloud_details(, "rifreg")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(rifreg)
      Loading required package: ggplot2
      > 
      > test_check("rifreg")
      Bootstrapping Standard Errors...
      
    ...
      `expected[2:2]`:                                                      "ggplot"
      ── Failure ('test-plot.R:190:3'): Generic plot method generates a plot for RIF of interquantile ratio ──
      class(rifreg_plot) (`actual`) not equal to c("gg", "ggplot") (`expected`).
      
      `actual`:        "ggplot2::ggplot" "ggplot" "ggplot2::gg" "S7_object" "gg"    
      `expected[2:2]`:                                                      "ggplot"
      
      [ FAIL 10 | WARN 3525 | SKIP 0 | PASS 66 ]
      Error: Test failures
      Execution halted
    ```

# rmweather

<details>

* Version: 0.2.62
* GitHub: https://github.com/skgrange/rmweather
* Source code: https://github.com/cran/rmweather
* Date/Publication: 2025-02-21 00:20:02 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::cloud_details(, "rmweather")` for more info

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
      ── Failure ('test_02_rmw_functions.R:83:3'): Test training function ────────────
      class(rmw_plot_importance(df_importance)) not identical to c("gg", "ggplot").
      Lengths differ: 5 is not 2
      ── Failure ('test_02_rmw_functions.R:90:3'): Test training function ────────────
      class(plot_test) not identical to c("gg", "ggplot").
      Lengths differ: 5 is not 2
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 40 ]
      Error: Test failures
      Execution halted
    ```

# RNAseqQC

<details>

* Version: 0.2.1
* GitHub: https://github.com/frederikziebell/RNAseqQC
* Source code: https://github.com/cran/RNAseqQC
* Date/Publication: 2024-07-15 14:40:02 UTC
* Number of recursive dependencies: 159

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
* Number of recursive dependencies: 84

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

# robCompositions

<details>

* Version: 2.4.1
* GitHub: NA
* Source code: https://github.com/cran/robCompositions
* Date/Publication: 2023-08-25 15:30:06 UTC
* Number of recursive dependencies: 139

Run `revdepcheck::cloud_details(, "robCompositions")` for more info

</details>

## Newly broken

*   checking whether package ‘robCompositions’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘e1071::element’ by ‘ggplot2::element’ when loading ‘robCompositions’
    See ‘/tmp/workdir/robCompositions/new/robCompositions.Rcheck/00install.out’ for details.
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘imputation.Rnw’ using knitr
    Error: processing vignette 'imputation.Rnw' failed with diagnostics:
    Running 'texi2dvi' on 'imputation.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `scrartcl.cls' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: cls)
    
    ...
    l.3 \usepackage
                   [pdftex]{hyperref}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘robCompositions-overview.Rnw’
    
    SUMMARY: processing the following files failed:
      ‘imputation.Rnw’ ‘robCompositions-overview.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 24.0Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        libs  20.8Mb
    ```

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) GDPsatis.Rd:21: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) GDPsatis.Rd:22: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) GDPsatis.Rd:23: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) GDPsatis.Rd:24: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) GDPsatis.Rd:25: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) GDPsatis.Rd:26: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) GDPsatis.Rd:27: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) GDPsatis.Rd:28: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) aDist.Rd:19: Lost braces
        19 | sets, or a distance matrix in case code{y} is not supplied.
    ...
           |               ^
    checkRd: (-1) weightedPivotCoord.Rd:25: Lost braces; missing escapes or markup?
        25 | `cut = min(#r_{j}=>0/#r_{j}, #r_{j}<0/#r_{j}`, with Gaussian Kernel function and bandwidth `h=0.05`.}
           |                         ^
    checkRd: (-1) weightedPivotCoord.Rd:25: Lost braces; missing escapes or markup?
        25 | `cut = min(#r_{j}=>0/#r_{j}, #r_{j}<0/#r_{j}`, with Gaussian Kernel function and bandwidth `h=0.05`.}
           |                                 ^
    checkRd: (-1) weightedPivotCoord.Rd:25: Lost braces; missing escapes or markup?
        25 | `cut = min(#r_{j}=>0/#r_{j}, #r_{j}<0/#r_{j}`, with Gaussian Kernel function and bandwidth `h=0.05`.}
           |                                          ^
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# romic

<details>

* Version: 1.1.3
* GitHub: NA
* Source code: https://github.com/cran/romic
* Date/Publication: 2023-09-21 05:40:02 UTC
* Number of recursive dependencies: 109

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
* Number of recursive dependencies: 67

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

# rPBK

<details>

* Version: 0.2.4
* GitHub: NA
* Source code: https://github.com/cran/rPBK
* Date/Publication: 2024-02-26 17:00:02 UTC
* Number of recursive dependencies: 79

Run `revdepcheck::cloud_details(, "rPBK")` for more info

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
      `expected`: TRUE 
      ── Failure ('test-ppc.R:7:5'): ppc ─────────────────────────────────────────────
      all(class(ppcPBK_C4) == c("gg", "ggplot")) is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 6 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking whether package ‘rPBK’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: namespace ‘colorspace’ is not available and has been replaced
    See ‘/tmp/workdir/rPBK/new/rPBK.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 98.4Mb
      sub-directories of 1Mb or more:
        data   1.5Mb
        libs  96.5Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rstantools’
      All declared Imports should be used.
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# rrr

<details>

* Version: 1.0.0
* GitHub: https://github.com/chrisaddy/rrr
* Source code: https://github.com/cran/rrr
* Date/Publication: 2016-12-09 15:15:55
* Number of recursive dependencies: 94

Run `revdepcheck::cloud_details(, "rrr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rrr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: residuals
    > ### Title: Reduced-Rank Regression Residuals
    > ### Aliases: residuals
    > 
    > ### ** Examples
    > 
    > data(tobacco)
    ...
    > galaxy <- as_data_frame(COMBO17)
    > galaxy <- select(galaxy, -starts_with("e."), -Nr, -UFS:-IFD)
    > galaxy <- na.omit(galaxy)
    > galaxy_x <- select(galaxy, -Rmag:-chi2red)
    > galaxy_y <- select(galaxy, Rmag:chi2red)
    > residuals(galaxy_x, galaxy_y, type = "cva", rank = 2, k = 0.001)
    Error in `+.gg`(e1, item) : 
      'ggmatrix' does not know how to add objects that do not have class 'theme', 'labels' or 'ggproto'. Received object with class: 'character'
    Calls: residuals ... cva_residual_plot -> +.gg -> add_list_to_ggmatrix -> +.gg
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘rrr.Rmd’ using rmarkdown
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Rcpp’
      All declared Imports should be used.
    ```

# RSDA

<details>

* Version: 3.2.4
* GitHub: NA
* Source code: https://github.com/cran/RSDA
* Date/Publication: 2025-06-02 19:10:02 UTC
* Number of recursive dependencies: 161

Run `revdepcheck::cloud_details(, "RSDA")` for more info

</details>

## Newly broken

*   checking whether package ‘RSDA’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘e1071::element’ by ‘ggplot2::element’ when loading ‘RSDA’
    See ‘/tmp/workdir/RSDA/new/RSDA.Rcheck/00install.out’ for details.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        doc   4.6Mb
    ```

# RTIGER

<details>

* Version: 2.1.0
* GitHub: NA
* Source code: https://github.com/cran/RTIGER
* Date/Publication: 2023-03-29 09:20:02 UTC
* Number of recursive dependencies: 158

Run `revdepcheck::cloud_details(, "RTIGER")` for more info

</details>

## Newly broken

*   checking whether package ‘RTIGER’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘e1071::element’ by ‘ggplot2::element’ when loading ‘RTIGER’
    See ‘/tmp/workdir/RTIGER/new/RTIGER.Rcheck/00install.out’ for details.
    ```

# RVA

<details>

* Version: 0.0.5
* GitHub: https://github.com/THERMOSTATS/RVA
* Source code: https://github.com/cran/RVA
* Date/Publication: 2021-11-01 21:40:02 UTC
* Number of recursive dependencies: 208

Run `revdepcheck::cloud_details(, "RVA")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘RVA.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.8Mb
      sub-directories of 1Mb or more:
        data   7.5Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘XML’
      All declared Imports should be used.
    ```

# RVenn

<details>

* Version: 1.1.0
* GitHub: NA
* Source code: https://github.com/cran/RVenn
* Date/Publication: 2019-07-18 21:40:02 UTC
* Number of recursive dependencies: 72

Run `revdepcheck::cloud_details(, "RVenn")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(RVenn)
      > 
      > test_check("RVenn")
      [ FAIL 4 | WARN 1 | SKIP 0 | PASS 31 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      ── Failure ('test_ggvenn.R:16:3'): ggvenn: slices ──────────────────────────────
      class(ggvenn(v1, slice = c(2, 4))) not equal to c("gg", "ggplot").
      Lengths differ: 5 is not 2
      ── Failure ('test_ggvenn.R:17:3'): ggvenn: slices ──────────────────────────────
      class(ggvenn(v1, slice = c("A", "B", "C"))) not equal to c("gg", "ggplot").
      Lengths differ: 5 is not 2
      
      [ FAIL 4 | WARN 1 | SKIP 0 | PASS 31 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# Rwtss

<details>

* Version: 0.9.2
* GitHub: https://github.com/e-sensing/Rwtss
* Source code: https://github.com/cran/Rwtss
* Date/Publication: 2022-04-25 08:50:05 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::cloud_details(, "Rwtss")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(Rwtss)
      Rwtss - R interface to Web Time Series Service.
      Loaded Rwtss v0.9.2.
              See ?Rwtss for help, citation("Rwtss") for use in publication.
              See demo(package = "Rwtss") for examples.
    ...
      Backtrace:
          ▆
       1. ├─vcr::use_cassette(...) at test_wtss.R:53:5
       2. │ └─cassette$call_block(...)
       3. └─testthat::expect_true(unname(summary(g)[1, 2]) == "gg") at test_wtss.R:60:9
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 57 ]
      Error: Test failures
      Execution halted
      Ran 14/14 deferred expressions
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) wtss_to_ts.Rd:31: Lost braces; missing escapes or markup?
        31 | This period can be either {"month", "week", "day"}, 
           |                           ^
    checkRd: (-1) wtss_to_ts.Rd:32: Lost braces; missing escapes or markup?
        32 | {"months", "weeks", "days"} or
           | ^
    checkRd: (-1) wtss_to_ts.Rd:33: Lost braces; missing escapes or markup?
        33 | {12, 52, 365}. This function creates a new time series with the required 
           | ^
    ```

# saros

<details>

* Version: 1.5.4
* GitHub: https://github.com/NIFU-NO/saros
* Source code: https://github.com/cran/saros
* Date/Publication: 2025-06-04 12:10:06 UTC
* Number of recursive dependencies: 117

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
     24. │                       └─base::lapply(...)
     25. │                         └─ggplot2 (local) FUN(X[[i]], ...)
     26. │                           ├─rlang::inject(self$draw_panel(data, panel_params, coord, !!!params))
     27. │                           └─self$draw_panel(...)
     28. └─base::.handleSimpleError(...)
     29.   └─rlang (local) h(simpleError(msg, call))
     30.     └─handlers[[1L]](cnd)
     31.       └─cli::cli_abort(...)
     32.         └─rlang::abort(...)
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
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 224 ]
    ...
       27. │                             └─self$draw_panel(...)
       28. └─base::.handleSimpleError(...)
       29.   └─rlang (local) h(simpleError(msg, call))
       30.     └─handlers[[1L]](cnd)
       31.       └─cli::cli_abort(...)
       32.         └─rlang::abort(...)
      
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 224 ]
      Error: Test failures
      Execution halted
    ```

# scatterpie

<details>

* Version: 0.2.4
* GitHub: NA
* Source code: https://github.com/cran/scatterpie
* Date/Publication: 2024-08-28 17:20:02 UTC
* Number of recursive dependencies: 64

Run `revdepcheck::cloud_details(, "scatterpie")` for more info

</details>

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.layer_scatterpie:
      function(object, plot, object_name)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# scCustomize

<details>

* Version: 3.0.1
* GitHub: https://github.com/samuel-marsh/scCustomize
* Source code: https://github.com/cran/scCustomize
* Date/Publication: 2024-12-18 18:40:02 UTC
* Number of recursive dependencies: 275

Run `revdepcheck::cloud_details(, "scCustomize")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘scCustomize-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Plot_Median_Genes
    > ### Title: Plot Median Genes per Cell per Sample
    > ### Aliases: Plot_Median_Genes
    > 
    > ### ** Examples
    > 
    > library(Seurat)
    ...
     1. └─scCustomize::Plot_Median_Genes(...)
     2.   └─scCustomize::theme_ggprism_mod()
     3.     ├─... %+replace% ...
     4.     │ └─ggplot2::is_theme(e1)
     5.     │   └─S7::S7_inherits(x, class_theme)
     6.     └─ggprism::theme_prism(...)
     7.       └─parent %+replace% t
     8.         └─cli::cli_abort("{.code %+replace%} requires two theme objects")
     9.           └─rlang::abort(...)
    Execution halted
    ```

# schtools

<details>

* Version: 0.4.1
* GitHub: https://github.com/SchlossLab/schtools
* Source code: https://github.com/cran/schtools
* Date/Publication: 2023-08-21 14:50:06 UTC
* Number of recursive dependencies: 116

Run `revdepcheck::cloud_details(, "schtools")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘introduction.Rmd’ using rmarkdown
    
    Quitting from introduction.Rmd:76-94 [italic-genus]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error:
    ! `object` must be an <S7_object>, not a S3<element_markdown/element_text/element>
    ---
    Backtrace:
    ...
    
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    `object` must be an <S7_object>, not a S3<element_markdown/element_text/element>
    --- failed re-building ‘introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# scoringutils

<details>

* Version: 2.1.0
* GitHub: https://github.com/epiforecasts/scoringutils
* Source code: https://github.com/cran/scoringutils
* Date/Publication: 2025-03-03 18:10:02 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "scoringutils")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘scoringutils-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: get_pit_histogram.forecast_quantile
    > ### Title: Probability integral transformation histogram
    > ### Aliases: get_pit_histogram.forecast_quantile
    > ###   get_pit_histogram.forecast_sample get_pit_histogram
    > ###   get_pit_histogram.default
    > ### Keywords: scoring
    > 
    ...
    > library("ggplot2")
    > 
    > result <- get_pit_histogram(example_sample_continuous, by = "model")
    > ggplot(result,  aes(x = mid, y = density)) +
    +   geom_col() +
    +   facet_wrap(. ~ model) +
    +   labs(x = "Quantile", "Density")
    Error: <ggplot2::labels> object is invalid:
    - every label must be named.
    Execution halted
    ```

# scplot

<details>

* Version: 0.5.1
* GitHub: NA
* Source code: https://github.com/cran/scplot
* Date/Publication: 2025-03-01 13:10:03 UTC
* Number of recursive dependencies: 114

Run `revdepcheck::cloud_details(, "scplot")` for more info

</details>

## Newly broken

*   checking whether package ‘scplot’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/scplot/new/scplot.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘scplot’ ...
** package ‘scplot’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Warning: The `size` argument of `element_rect()` is deprecated as of ggplot2 3.4.0.
ℹ Please use the `linewidth` argument instead.
ℹ The deprecated feature was likely used in the scplot package.
  Please report the issue to the authors.
Error in `class<-`(`*tmp*`, value = "list") : 
  cannot coerce type 'object' to vector of type 'list'
Error: unable to load R code in package ‘scplot’
Execution halted
ERROR: lazy loading failed for package ‘scplot’
* removing ‘/tmp/workdir/scplot/new/scplot.Rcheck/scplot’


```
### CRAN

```
* installing *source* package ‘scplot’ ...
** package ‘scplot’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (scplot)


```
# SCpubr

<details>

* Version: 2.0.2
* GitHub: https://github.com/enblacar/SCpubr
* Source code: https://github.com/cran/SCpubr
* Date/Publication: 2023-10-11 09:50:02 UTC
* Number of recursive dependencies: 299

Run `revdepcheck::cloud_details(, "SCpubr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # nolint start
      > library(testthat)
      > library(SCpubr)
      
      
      ── SCpubr 2.0.2 ────────────────────────────────────────────────────────────────
      
    ...
      - e1: <patchwork>
      - e2: <ggplot2::labels>
      Backtrace:
          ▆
       1. └─SCpubr::do_FeaturePlot(sample, features = "EPC1") at test-utils.R:775:5
       2.   └─S7:::Ops.S7_object(p, ggplot2::ggtitle(""))
      
      [ FAIL 55 | WARN 16 | SKIP 391 | PASS 25 ]
      Error: Test failures
      Execution halted
    ```

# scRNAstat

<details>

* Version: 0.1.1.1
* GitHub: NA
* Source code: https://github.com/cran/scRNAstat
* Date/Publication: 2025-03-08 08:58:55 UTC
* Number of recursive dependencies: 154

Run `revdepcheck::cloud_details(, "scRNAstat")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘scRNAstat-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: basic_filter
    > ### Title: basic_filter
    > ### Aliases: basic_filter
    > 
    > ### ** Examples
    > 
    > basic_filter(AJ064_small_sce)
    ...
      Please report the issue at <https://github.com/satijalab/seurat/issues>.
    Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
    ℹ Please use tidy evaluation idioms with `aes()`.
    ℹ See also `vignette("ggplot2-in-packages")` for more information.
    ℹ The deprecated feature was likely used in the Seurat package.
      Please report the issue at <https://github.com/satijalab/seurat/issues>.
    Error: Can't find method for generic `&(e1, e2)`:
    - e1: <patchwork>
    - e2: <theme>
    Execution halted
    ```

## In both

*   checking whether package ‘scRNAstat’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: namespace ‘colorspace’ is not available and has been replaced
    See ‘/tmp/workdir/scRNAstat/new/scRNAstat.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.1Mb
      sub-directories of 1Mb or more:
        data   8.0Mb
    ```

# scrutiny

<details>

* Version: 0.5.0
* GitHub: https://github.com/lhdjung/scrutiny
* Source code: https://github.com/cran/scrutiny
* Date/Publication: 2024-09-22 08:10:02 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::cloud_details(, "scrutiny")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘scrutiny-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: debit_plot
    > ### Title: Visualize DEBIT results
    > ### Aliases: debit_plot
    > 
    > ### ** Examples
    > 
    > # Run `debit_plot()` on the output
    > # of `debit_map()`:
    > pigs3 %>%
    +   debit_map() %>%
    +   debit_plot()
    Error: <ggplot2::element_line> object properties are invalid:
    - @colour cannot be a decimal number, but could be an integer.
    - @arrow.fill cannot be a decimal number, but could be an integer.
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
      > library(scrutiny)
      > 
      > test_check("scrutiny")
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 523 ]
    ...
       3.   ├─ggplot2::theme(...)
       4.   │ └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
       5.   │   └─base::mget(args, envir = env)
       6.   └─ggplot2::element_line(seq(0, 0.5, 0.1))
       7.     └─S7::new_object(...)
       8.       └─S7::validate(object, recursive = !parent_validated)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 523 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘consistency-tests-in-depth.Rmd’ using rmarkdown
    --- finished re-building ‘consistency-tests-in-depth.Rmd’
    
    --- re-building ‘consistency-tests-simple.Rmd’ using rmarkdown
    --- finished re-building ‘consistency-tests-simple.Rmd’
    
    --- re-building ‘debit.Rmd’ using rmarkdown
    
    Quitting from debit.Rmd:116-124 [unnamed-chunk-8]
    ...
    - @arrow.fill cannot be a decimal number, but could be an integer.
    --- failed re-building ‘debit.Rmd’
    
    --- re-building ‘devtools.Rmd’ using rmarkdown
    --- finished re-building ‘devtools.Rmd’
    
    --- re-building ‘duplicates.Rmd’ using rmarkdown
    --- finished re-building ‘duplicates.Rmd’
    
    --- re-building ‘grim.Rmd’ using rmarkdown
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘janitor’
    ```

# scUtils

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/scUtils
* Date/Publication: 2020-06-25 16:20:02 UTC
* Number of recursive dependencies: 48

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
      Attributes: < names for target but not for current >
      Attributes: < Length mismatch: comparison on first 0 components >
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
* Number of recursive dependencies: 77

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

## In both

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
* Number of recursive dependencies: 49

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
    > library(seAMLess)
    > 
    > data(minRes)
    > ternaryPlot(minRes)
    Error: .onLoad failed in loadNamespace() for 'ggtern', details:
      call: NULL
      error: <ggplot2::element_line> object properties are invalid:
    - @lineend must be <character> or <NULL>, not S3<arrow>
    Execution halted
    ```

# sedproxy

<details>

* Version: 0.7.5
* GitHub: https://github.com/EarthSystemDiagnostics/sedproxy
* Source code: https://github.com/cran/sedproxy
* Date/Publication: 2023-02-26 10:50:02 UTC
* Number of recursive dependencies: 68

Run `revdepcheck::cloud_details(, "sedproxy")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(sedproxy)
      > 
      > test_check("sedproxy")
      [ FAIL 1 | WARN 13 | SKIP 0 | PASS 36 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-ClimToProxyClim.R:332:3'): example from paper works ──────────
      class(p) not equal to c("gg", "ggplot").
      Lengths differ: 5 is not 2
      
      [ FAIL 1 | WARN 13 | SKIP 0 | PASS 36 ]
      Error: Test failures
      Execution halted
    ```

# see

<details>

* Version: 0.11.0
* GitHub: https://github.com/easystats/see
* Source code: https://github.com/cran/see
* Date/Publication: 2025-03-11 16:20:02 UTC
* Number of recursive dependencies: 245

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
     14.               │ │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
     15.               │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
     16.               │ └─base::withCallingHandlers(...)
     17.               └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     18.                 └─l$map_statistic(d, plot)
     19.                   └─ggplot2 (local) map_statistic(..., self = self)
     20.                     └─ggplot2:::check_nondata_cols(...)
     21.                       └─cli::cli_abort(c(problem, issues, i = hint), call = NULL)
     22.                         └─rlang::abort(...)
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
       33. │                                         └─ggplot2 (local) draw_group(..., self = self)
       34. └─base::.handleSimpleError(...)
       35.   └─rlang (local) h(simpleError(msg, call))
       36.     └─handlers[[1L]](cnd)
       37.       └─cli::cli_abort(...)
       38.         └─rlang::abort(...)
      
      [ FAIL 1 | WARN 11 | SKIP 23 | PASS 34 ]
      Error: Test failures
      Execution halted
    ```

# seedreg

<details>

* Version: 1.0.3
* GitHub: NA
* Source code: https://github.com/cran/seedreg
* Date/Publication: 2022-07-07 21:20:02 UTC
* Number of recursive dependencies: 124

Run `revdepcheck::cloud_details(, "seedreg")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘seedreg-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: lineplot
    > ### Title: Graph: line chart
    > ### Aliases: lineplot
    > 
    > ### ** Examples
    > 
    > data("substrate")
    ...
     19. │                 └─ggplot2 (local) setup_params(...)
     20. │                   └─ggplot2:::make_summary_fun(...)
     21. │                     └─rlang::as_function(fun.data)
     22. │                       └─base::get(x, envir = env, mode = "function")
     23. └─base::.handleSimpleError(...)
     24.   └─rlang (local) h(simpleError(msg, call))
     25.     └─handlers[[1L]](cnd)
     26.       └─cli::cli_abort(...)
     27.         └─rlang::abort(...)
    Execution halted
    ```

# seqHMM

<details>

* Version: 2.0.0
* GitHub: https://github.com/helske/seqHMM
* Source code: https://github.com/cran/seqHMM
* Date/Publication: 2025-05-17 00:10:02 UTC
* Number of recursive dependencies: 125

Run `revdepcheck::cloud_details(, "seqHMM")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘seqHMM-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: stacked_sequence_plot
    > ### Title: Stacked Sequence Plots of Multichannel Sequences and/or Most
    > ###   Probable Paths from Hidden Markov Models
    > ### Aliases: stacked_sequence_plot
    > 
    > ### ** Examples
    > 
    ...
    +   plots = "both", 
    +   type = "d", 
    +   legend_position = c("right", "right", "right", "none")
    + )
    > library("ggplot2")
    > p & theme(plot.margin = unit(c(1, 1, 0, 2), "mm"))
    Error: Can't find method for generic `&(e1, e2)`:
    - e1: <patchwork>
    - e2: <theme>
    Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘seqHMM.Rnw’ using knitr
    
    Quitting from seqHMM.Rnw:417-429 [graphicalillustrations2]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error:
    ! Can't find method for generic `&(e1, e2)`:
    - e1: <patchwork>
    - e2: <ggplot2::labels>
    ...
    Can't find method for generic `&(e1, e2)`:
    - e1: <patchwork>
    - e2: <ggplot2::labels>
    --- failed re-building ‘seqHMM_visualization.Rnw’
    
    SUMMARY: processing the following files failed:
      ‘seqHMM.Rnw’ ‘seqHMM_algorithms.Rnw’ ‘seqHMM_visualization.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 41.6Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        data   1.5Mb
        libs  37.5Mb
    ```

# Seurat

<details>

* Version: 5.3.0
* GitHub: https://github.com/satijalab/seurat
* Source code: https://github.com/cran/Seurat
* Date/Publication: 2025-04-23 22:10:02 UTC
* Number of recursive dependencies: 278

Run `revdepcheck::cloud_details(, "Seurat")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Seurat-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: CombinePlots
    > ### Title: Combine ggplot2-based plots into a single plot
    > ### Aliases: CombinePlots
    > 
    > ### ** Examples
    > 
    > data("pbmc_small")
    ...
    +   split.by = 'group'
    + )
    Warning: The `slot` argument of `FetchData()` is deprecated as of SeuratObject 5.0.0.
    ℹ Please use the `layer` argument instead.
    ℹ The deprecated feature was likely used in the Seurat package.
      Please report the issue at <https://github.com/satijalab/seurat/issues>.
    Error: Can't find method for generic `&(e1, e2)`:
    - e1: <patchwork>
    - e2: <theme>
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 16.7Mb
      sub-directories of 1Mb or more:
        R      3.5Mb
        help   1.5Mb
        libs  11.4Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘Signac’
    ```

# sglg

<details>

* Version: 0.2.2
* GitHub: NA
* Source code: https://github.com/cran/sglg
* Date/Publication: 2022-09-04 03:50:01 UTC
* Number of recursive dependencies: 92

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
* Number of recursive dependencies: 119

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
      
      [ FAIL 2 | WARN 115 | SKIP 19 | PASS 508 ]
      Error: Test failures
      Execution halted
    ```

# shadowtext

<details>

* Version: 0.1.4
* GitHub: https://github.com/GuangchuangYu/shadowtext
* Source code: https://github.com/cran/shadowtext
* Date/Publication: 2024-07-18 07:00:01 UTC
* Number of recursive dependencies: 47

Run `revdepcheck::cloud_details(, "shadowtext")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    '::' or ':::' import not declared from: ‘S7’
    ```

# SHAPforxgboost

<details>

* Version: 0.1.3
* GitHub: https://github.com/liuyanguu/SHAPforxgboost
* Source code: https://github.com/cran/SHAPforxgboost
* Date/Publication: 2023-05-29 17:20:07 UTC
* Number of recursive dependencies: 113

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

# shiny

<details>

* Version: 1.10.0
* GitHub: https://github.com/rstudio/shiny
* Source code: https://github.com/cran/shiny
* Date/Publication: 2024-12-14 00:10:02 UTC
* Number of recursive dependencies: 91

Run `revdepcheck::cloud_details(, "shiny")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(shiny)
      > 
      > test_check("shiny")
      [ FAIL 96 | WARN 1 | SKIP 19 | PASS 1529 ]
      
      ══ Skipped tests (19) ══════════════════════════════════════════════════════════
    ...
      m5$panels has length 0, not length 1.
      ── Failure ('test-plot-coordmap.R:464:3'): ggplot coordmap maintains discrete limits ──
      m5$panels[[1]]$domain$discrete_limits (`actual`) not equal to list(x = c("e", "f")) (`expected`).
      
      `actual` is NULL
      `expected` is a list
      
      [ FAIL 96 | WARN 1 | SKIP 19 | PASS 1529 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 16.6Mb
      sub-directories of 1Mb or more:
        R      3.5Mb
        help   1.7Mb
        www   10.4Mb
    ```

# shinyMixR

<details>

* Version: 0.5.0
* GitHub: NA
* Source code: https://github.com/cran/shinyMixR
* Date/Publication: 2024-11-14 16:10:03 UTC
* Number of recursive dependencies: 199

Run `revdepcheck::cloud_details(, "shinyMixR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # see https://github.com/rstudio/shinytest2/issues/351 
      > options(chromote.timeout = 120)
      > 
      > library(testthat)
      > library(shinyMixR)
      Loading required package: shiny
      Loading required package: ggplot2
    ...
      Backtrace:
          ▆
       1. ├─base::suppressWarnings(gof_plot(res, ptype = "all", type = "xpose")) at test-gof_plot.R:10:3
       2. │ └─base::withCallingHandlers(...)
       3. └─shinyMixR::gof_plot(res, ptype = "all", type = "xpose")
       4.   └─S7:::Ops.S7_object(...)
      
      [ FAIL 4 | WARN 1 | SKIP 11 | PASS 28 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘getting_started.Rmd’ using rmarkdown
    
    Quitting from getting_started.Rmd:234-237 [unnamed-chunk-18]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error:
    ! Can't find method for generic `&(e1, e2)`:
    - e1: <patchwork>
    - e2: <theme>
    ...
    Can't find method for generic `&(e1, e2)`:
    - e1: <patchwork>
    - e2: <theme>
    --- failed re-building ‘getting_started.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘getting_started.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# shinyMobile

<details>

* Version: 2.0.1
* GitHub: https://github.com/RinteRface/shinyMobile
* Source code: https://github.com/cran/shinyMobile
* Date/Publication: 2024-10-04 17:30:02 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "shinyMobile")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘shinyMobile-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: f7SplitLayout
    > ### Title: Framework7 split layout
    > ### Aliases: f7SplitLayout
    > 
    > ### ** Examples
    > 
    > library(shiny)
    ...
    > fruits <- data.frame(
    +   name = c("Apples", "Oranges", "Bananas", "Berries"),
    +   value = c(44, 55, 67, 83)
    + )
    > 
    > thematic_shiny(font = "auto")
    Error in get(x, envir = ns, inherits = FALSE) : 
      object 'ggplot_build.ggplot' not found
    Calls: thematic_shiny ... thematic_on -> ggplot_build_set -> getFromNamespace -> get
    Execution halted
    ```

# simmr

<details>

* Version: 0.5.1.217
* GitHub: https://github.com/andrewcparnell/simmr
* Source code: https://github.com/cran/simmr
* Date/Publication: 2024-10-16 15:10:02 UTC
* Number of recursive dependencies: 114

Run `revdepcheck::cloud_details(, "simmr")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘advanced_plotting.Rmd’ using rmarkdown
    ```

# SimNPH

<details>

* Version: 0.5.7
* GitHub: https://github.com/SimNPH/SimNPH
* Source code: https://github.com/cran/SimNPH
* Date/Publication: 2025-04-08 10:30:02 UTC
* Number of recursive dependencies: 130

Run `revdepcheck::cloud_details(, "SimNPH")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(SimNPH)
      Loading required package: SimDesign
      Loading required package: survival
      > 
      > test_check("SimNPH")
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 403 ]
    ...
       1. ├─withr::with_package(...) at test-shhr_gg.R:5:3
       2. │ └─base::force(code)
       3. ├─withr::with_package(...) at test-shhr_gg.R:6:5
       4. │ └─base::force(code)
       5. └─SimNPH::shhr_gg(A, B) at test-shhr_gg.R:7:7
       6.   └─S7:::Ops.S7_object(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 403 ]
      Error: Test failures
      Execution halted
    ```

# simplecolors

<details>

* Version: 0.1.2
* GitHub: https://github.com/rjake/simplecolors
* Source code: https://github.com/cran/simplecolors
* Date/Publication: 2023-08-31 04:40:07 UTC
* Number of recursive dependencies: 118

Run `revdepcheck::cloud_details(, "simplecolors")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(simplecolors)
      > 
      > test_check("simplecolors")
      [ FAIL 3 | WARN 1 | SKIP 0 | PASS 22 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      y[1]: "gg"
      ── Failure ('test-show_colors.R:5:3'): show_colors retuns gglot ────────────────
      is(x) not equal to "gg".
      1/1 mismatches
      x[1]: "ggplot2::ggplot"
      y[1]: "gg"
      
      [ FAIL 3 | WARN 1 | SKIP 0 | PASS 22 ]
      Error: Test failures
      Execution halted
    ```

# sjPlot

<details>

* Version: 2.8.17
* GitHub: https://github.com/strengejacke/sjPlot
* Source code: https://github.com/cran/sjPlot
* Date/Publication: 2024-11-29 11:20:03 UTC
* Number of recursive dependencies: 193

Run `revdepcheck::cloud_details(, "sjPlot")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘blackwhitefigures.Rmd’ using rmarkdown
    ```

# smallsets

<details>

* Version: 2.0.0
* GitHub: https://github.com/lydialucchesi/smallsets
* Source code: https://github.com/cran/smallsets
* Date/Publication: 2023-12-05 00:00:02 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::cloud_details(, "smallsets")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘smallsets-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Smallset_Timeline
    > ### Title: Smallset Timeline
    > ### Aliases: Smallset_Timeline
    > 
    > ### ** Examples
    > 
    > set.seed(145)
    > 
    > Smallset_Timeline(
    +   data = s_data,
    +   code = system.file("s_data_preprocess.R", package = "smallsets")
    + )
    Error: Can't find property <ggplot2::element_blank>@hjust
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
        9. │       └─ggplot2:::fix_theme_deprecations(elements)
       10. │         ├─base::`$<-`(`*tmp*`, "hjust", value = `<dbl>`)
       11. │         └─ggplot2:::`$<-.ggplot2::element`(`*tmp*`, "hjust", value = `<dbl>`)
       12. │           └─S7::`props<-`(`*tmp*`, value = `[[<-`(S7::props(x), i, value))
       13. │             └─S7::`prop<-`(`*tmp*`, name, check = FALSE, value = value[[name]])
       14. └─S7 (local) `<fn>`(...)
      
      [ FAIL 15 | WARN 0 | SKIP 0 | PASS 16 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘smallsets.Rmd’ using rmarkdown
    
    Quitting from smallsets.Rmd:35-42 [timeline1]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error:
    ! Can't find property <ggplot2::element_blank>@hjust
    ---
    Backtrace:
    ...
    
    Error: processing vignette 'smallsets.Rmd' failed with diagnostics:
    Can't find property <ggplot2::element_blank>@hjust
    --- failed re-building ‘smallsets.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘smallsets.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘gurobi’
    ```

# smer

<details>

* Version: 0.0.1
* GitHub: https://github.com/lcrawlab/sme
* Source code: https://github.com/cran/smer
* Date/Publication: 2025-01-16 15:50:01 UTC
* Number of recursive dependencies: 110

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

# SOMbrero

<details>

* Version: 1.4-2
* GitHub: https://github.com/tuxette/SOMbrero
* Source code: https://github.com/cran/SOMbrero
* Date/Publication: 2024-01-25 22:10:10 UTC
* Number of recursive dependencies: 112

Run `revdepcheck::cloud_details(, "SOMbrero")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library("testthat")
      > library("SOMbrero")
      Loading required package: igraph
      
      Attaching package: 'igraph'
      
      The following object is masked from 'package:testthat':
    ...
      ── Failure ('test-missing.R:58:3'): All 'add' plots are produced for inputs with missing entries ──
      class(plot(nsom, what = "add", type = "barplot", variable = add4)) not equal to c("gg", "ggplot").
      Lengths differ: 5 is not 2
      ── Failure ('test-missing.R:61:3'): All 'add' plots are produced for inputs with missing entries ──
      class(plot(nsom, what = "add", type = "boxplot", variable = add4)) not equal to c("gg", "ggplot").
      Lengths differ: 5 is not 2
      
      [ FAIL 14 | WARN 0 | SKIP 0 | PASS 58 ]
      Error: Test failures
      Execution halted
    ```

# SoupX

<details>

* Version: 1.6.2
* GitHub: https://github.com/constantAmateur/SoupX
* Source code: https://github.com/cran/SoupX
* Date/Publication: 2022-11-01 14:00:03 UTC
* Number of recursive dependencies: 199

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

# spacejamr

<details>

* Version: 0.2.1
* GitHub: https://github.com/dscolby/spacejamr
* Source code: https://github.com/cran/spacejamr
* Date/Publication: 2022-04-01 20:10:02 UTC
* Number of recursive dependencies: 133

Run `revdepcheck::cloud_details(, "spacejamr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(spacejamr)
      > 
      > test_check("spacejamr")
          Density Mean Degree Mean Closeness Mean Betweenness Largest Component Size
      pl        0           0            NaN                0                      1
      apl       0           0            NaN                0                      1
    ...
      [1] "ggraph"          - "gg"     [2]
      [2] "ggplot2::ggplot" -             
      [3] "ggplot"          -             
      [4] "ggplot2::gg"     -             
      [5] "S7_object"       -             
      [6] "gg"              - "ggplot" [3]
      
      [ FAIL 3 | WARN 3 | SKIP 0 | PASS 20 ]
      Error: Test failures
      Execution halted
    ```

# spatialwarnings

<details>

* Version: 3.1.0
* GitHub: https://github.com/spatial-ews/spatialwarnings
* Source code: https://github.com/cran/spatialwarnings
* Date/Publication: 2024-09-06 14:50:02 UTC
* Number of recursive dependencies: 93

Run `revdepcheck::cloud_details(, "spatialwarnings")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > 
      > library(spatialwarnings)
      Loading required package: future
      This is spatialwarnings 3.1.0
      Use plan(multisession) to set up parallel processing
      > if ( require("testthat") ) { 
      +   test_check("spatialwarnings")
    ...
      {
          ...
      } is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      
      [ FAIL 6 | WARN 17 | SKIP 2 | PASS 161 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.3Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        libs   5.8Mb
    ```

# spectralR

<details>

* Version: 0.1.3
* GitHub: https://github.com/olehprylutskyi/spectralR
* Source code: https://github.com/cran/spectralR
* Date/Publication: 2023-08-24 09:20:02 UTC
* Number of recursive dependencies: 117

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
      
      test_get.pixel.data.R.........    1 tests [0;32mOK[0m [0;34m0.1s[0m
      
      test_prepare.vector.data.R....    1 tests [0;32mOK[0m 
    ...
      test_prepare.vector.data.R....    8 tests [0;32mOK[0m 
      test_prepare.vector.data.R....    9 tests [0;32mOK[0m 
      test_prepare.vector.data.R....   10 tests [0;32mOK[0m 
      test_prepare.vector.data.R....   11 tests [0;32mOK[0m 
      test_prepare.vector.data.R....   12 tests [0;32mOK[0m [0;36m79ms[0m
      
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
* Number of recursive dependencies: 151

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
      
      [ FAIL 9 | WARN 85 | SKIP 0 | PASS 72 ]
      Error: Test failures
      Execution halted
    ```

# sport

<details>

* Version: 0.2.1
* GitHub: https://github.com/gogonzo/sport
* Source code: https://github.com/cran/sport
* Date/Publication: 2024-01-08 23:50:02 UTC
* Number of recursive dependencies: 67

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

# sportyR

<details>

* Version: 2.2.2
* GitHub: https://github.com/sportsdataverse/sportyR
* Source code: https://github.com/cran/sportyR
* Date/Publication: 2024-02-15 10:10:02 UTC
* Number of recursive dependencies: 91

Run `revdepcheck::cloud_details(, "sportyR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(sportyR)
      > 
      > test_check("sportyR")
      [ FAIL 28 | WARN 0 | SKIP 0 | PASS 101 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      `expected`: "gg"             
      ── Failure ('test-plots-and-features-volleyball.R:18:5'): geom_volleyball() can successfully transform coordinates ──
      class(fivb_court)[1] (`actual`) not equal to "gg" (`expected`).
      
      `actual`:   "ggplot2::ggplot"
      `expected`: "gg"             
      
      [ FAIL 28 | WARN 0 | SKIP 0 | PASS 101 ]
      Error: Test failures
      Execution halted
    ```

# SPUTNIK

<details>

* Version: 1.4.2
* GitHub: https://github.com/paoloinglese/SPUTNIK
* Source code: https://github.com/cran/SPUTNIK
* Date/Publication: 2024-04-16 14:30:05 UTC
* Number of recursive dependencies: 85

Run `revdepcheck::cloud_details(, "SPUTNIK")` for more info

</details>

## Newly broken

*   checking whether package ‘SPUTNIK’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘e1071::element’ by ‘ggplot2::element’ when loading ‘SPUTNIK’
    See ‘/tmp/workdir/SPUTNIK/new/SPUTNIK.Rcheck/00install.out’ for details.
    ```

# SqueakR

<details>

* Version: 1.3.0
* GitHub: https://github.com/osimon81/SqueakR
* Source code: https://github.com/cran/SqueakR
* Date/Publication: 2022-06-28 09:20:04 UTC
* Number of recursive dependencies: 146

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

# ssd4mosaic

<details>

* Version: 1.0.3
* GitHub: NA
* Source code: https://github.com/cran/ssd4mosaic
* Date/Publication: 2025-03-11 14:40:02 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::cloud_details(, "ssd4mosaic")` for more info

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
      plot_cens_ci$layers has length 11, not length 7.
      ── Error ('test-fct_options_plots.R:8:3'): Uncensored data can be coloured by group ──
      Error in `order(p$layers[[1]]$data$group)`: argument 1 is not a vector
      Backtrace:
          ▆
       1. └─base::order(p$layers[[1]]$data$group) at test-fct_options_plots.R:8:3
      
      [ FAIL 3 | WARN 1 | SKIP 1 | PASS 66 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# sssc

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/sssc
* Date/Publication: 2018-06-15 11:22:54 UTC
* Number of recursive dependencies: 31

Run `revdepcheck::cloud_details(, "sssc")` for more info

</details>

## Newly broken

*   checking whether package ‘sssc’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘e1071::element’ by ‘ggplot2::element’ when loading ‘sssc’
    See ‘/tmp/workdir/sssc/new/sssc.Rcheck/00install.out’ for details.
    ```

# starvz

<details>

* Version: 0.8.2
* GitHub: https://github.com/schnorr/starvz
* Source code: https://github.com/cran/starvz
* Date/Publication: 2024-09-08 19:00:02 UTC
* Number of recursive dependencies: 101

Run `revdepcheck::cloud_details(, "starvz")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(starvz)
      > 
      > test_check("starvz")
      [ FAIL 1 | WARN 0 | SKIP 2 | PASS 1 ]
      
      ══ Skipped tests (2) ═══════════════════════════════════════════════════════════
    ...
      • On CRAN (2): 'test_lu.R:10:3', 'test_qr.R:5:3'
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test_lu.R:8:3'): starvz_plot works ────────────────────────────────
      class(pl) not equal to c("patchwork", "gg", "ggplot").
      Lengths differ: 6 is not 3
      
      [ FAIL 1 | WARN 0 | SKIP 2 | PASS 1 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        libs   3.9Mb
    ```

# states

<details>

* Version: 0.3.2
* GitHub: https://github.com/andybega/states
* Source code: https://github.com/cran/states
* Date/Publication: 2023-09-05 12:20:02 UTC
* Number of recursive dependencies: 90

Run `revdepcheck::cloud_details(, "states")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library("testthat")
      > library("states")
      
      Attaching package: 'states'
      
      The following object is masked from 'package:testthat':
      
    ...
      ── Failure ('test-plot-missing.R:72:3'): plot_missing accepts all input options ──
      plot_missing(...) has type 'object', not 'list'.
      ── Failure ('test-plot-missing.R:77:3'): plot_missing accepts all input options ──
      plot_missing(...) has type 'object', not 'list'.
      ── Failure ('test-plot-missing.R:82:3'): plot_missing accepts all input options ──
      plot_missing(...) has type 'object', not 'list'.
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 106 ]
      Error: Test failures
      Execution halted
    ```

# statgenGWAS

<details>

* Version: 1.0.11
* GitHub: https://github.com/Biometris/statgenGWAS
* Source code: https://github.com/cran/statgenGWAS
* Date/Publication: 2025-03-31 13:30:06 UTC
* Number of recursive dependencies: 70

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
* Number of recursive dependencies: 85

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
* Number of recursive dependencies: 131

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
      test_detectSerieOut.R.........    4 tests [0;32mOK[0m 
      test_detectSerieOut.R.........    5 tests [0;32mOK[0m 
      test_detectSerieOut.R.........    6 tests [0;32mOK[0m 
      test_detectSerieOut.R.........    7 tests [0;32mOK[0m 
      test_detectSerieOut.R.........    8 tests [0;32mOK[0m 
      test_detectSerieOut.R.........    9 tests [0;32mOK[0m Error in inherits(w, class) && grepl(pattern, w$message, ...) : 
        'length = 2' in coercion to 'logical(1)'
      Calls: <Anonymous> ... eval -> expect_warning -> fun -> sapply -> lapply -> FUN
      In addition: There were 22 warnings (use warnings() to see them)
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
* Number of recursive dependencies: 58

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

# statgenSTA

<details>

* Version: 1.0.14
* GitHub: https://github.com/Biometris/statgenSTA
* Source code: https://github.com/cran/statgenSTA
* Date/Publication: 2024-10-14 07:40:02 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::cloud_details(, "statgenSTA")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(statgenSTA)
      > 
      > test_check("statgenSTA")
      [ FAIL 1 | WARN 2 | SKIP 6 | PASS 777 ]
      
      ══ Skipped tests (6) ═══════════════════════════════════════════════════════════
    ...
      • {asreml} is not installed (3): 'test-STA.R:43:3', 'test-STA.R:57:3',
        'test-STA.R:113:3'
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-layoutPlot.R:104:3'): plot works correctly if no trial is defined ──
      `plot(testTD2, plotType = "layout")` produced warnings.
      
      [ FAIL 1 | WARN 2 | SKIP 6 | PASS 777 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘asreml’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘mapproj’
      All declared Imports should be used.
    ```

# stRoke

<details>

* Version: 24.10.1
* GitHub: https://github.com/agdamsbo/stRoke
* Source code: https://github.com/cran/stRoke
* Date/Publication: 2024-10-25 06:50:01 UTC
* Number of recursive dependencies: 130

Run `revdepcheck::cloud_details(, "stRoke")` for more info

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
      ── Failure ('test-index_plot.R:2:3'): index_plot() works! ──────────────────────
      index_plot(stRoke::score[score$event == "A", ]) has type 'object', not 'list'.
      ── Failure ('test-index_plot.R:5:3'): index_plot() works! ──────────────────────
      index_plot(stRoke::score[score$event == "A", ], sub_plot = "_per") has type 'object', not 'list'.
      ── Failure ('test-index_plot.R:17:3'): index_plot() works! ─────────────────────
      index_plot(stRoke::score, sub_plot = "_per", facet.by = "event") has type 'object', not 'list'.
      
      [ FAIL 3 | WARN 2 | SKIP 0 | PASS 47 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 705 marked UTF-8 strings
    ```

# Superpower

<details>

* Version: 0.2.3
* GitHub: https://github.com/arcaldwell49/Superpower
* Source code: https://github.com/cran/Superpower
* Date/Publication: 2025-05-15 07:20:02 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::cloud_details(, "Superpower")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(Superpower)
      > 
      > 
      > test_check("Superpower")
      [1] "condition_one" "sad_1"        
    ...
      y[1]: "gg"
      ── Failure ('test-morey.R:109:3'): No error messages ftest ─────────────────────
      class(test)[1] not equal to "gg".
      1/1 mismatches
      x[1]: "ggplot2::ggplot"
      y[1]: "gg"
      
      [ FAIL 2 | WARN 18 | SKIP 13 | PASS 398 ]
      Error: Test failures
      Execution halted
    ```

# survHE

<details>

* Version: 2.0.4
* GitHub: https://github.com/giabaio/survHE
* Source code: https://github.com/cran/survHE
* Date/Publication: 2025-05-15 08:20:02 UTC
* Number of recursive dependencies: 127

Run `revdepcheck::cloud_details(, "survHE")` for more info

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
      ── Failure ('test-make.surv.R:20:3'): output plots don't throw an error and return ggplot2 list ──
      psa.plot(p.mle3) has type 'object', not 'list'.
      ── Failure ('test-make.surv.R:22:3'): output plots don't throw an error and return ggplot2 list ──
      plot(mle, add.km = TRUE) has type 'object', not 'list'.
      ── Failure ('test-make.surv.R:23:3'): output plots don't throw an error and return ggplot2 list ──
      plot(mle, add.km = TRUE, sim = 10) has type 'object', not 'list'.
      
      [ FAIL 7 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

# synthpop

<details>

* Version: 1.9-1.1
* GitHub: NA
* Source code: https://github.com/cran/synthpop
* Date/Publication: 2025-06-03 05:53:06 UTC
* Number of recursive dependencies: 63

Run `revdepcheck::cloud_details(, "synthpop")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘synthpop-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: compare.synds
    > ### Title: Compare univariate distributions of synthesised and observed
    > ###   data
    > ### Aliases: compare.synds compare.data.frame compare.list
    > ###   print.compare.synds
    > 
    > ### ** Examples
    ...
    > 
    > ### synthetic data provided as a 'synds' object
    > compare(s1, ods, vars = "ls")
    Calculations done for ls 
    
    Comparing percentages observed with synthetic
    
    Error in S7::prop(x, "meta")[[i]] : subscript out of bounds
    Calls: <Anonymous> ... print.compare.synds -> print -> [[ -> [[.ggplot2::gg
    Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘disclosure.Rnw’ using Sweave
    New version of synthpop (1.9-0) with disclosure functions
    see disclosure.pdf for details and NEWS file for other changes
    
    Find out more at https://www.synthpop.org.uk/
    Error: processing vignette 'disclosure.Rnw' failed with diagnostics:
    Running 'texi2dvi' on 'disclosure.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `authblk.sty' not found.
    ...
    l.6 \usepackage
                   {framed}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building 'utility.Rnw'
    
    SUMMARY: processing the following files failed:
      ‘disclosure.Rnw’ ‘inference.Rnw’ ‘utility.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# tabledown

<details>

* Version: 1.0.0
* GitHub: https://github.com/masiraji/tabledown
* Source code: https://github.com/cran/tabledown
* Date/Publication: 2024-05-02 13:40:03 UTC
* Number of recursive dependencies: 163

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

# tastypie

<details>

* Version: 0.1.1
* GitHub: https://github.com/PaoloDalena/tastypie
* Source code: https://github.com/cran/tastypie
* Date/Publication: 2023-09-06 18:00:06 UTC
* Number of recursive dependencies: 76

Run `revdepcheck::cloud_details(, "tastypie")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(tastypie)
                  $$  $$  $$
                __||__||__||__
               | * * * * * * *|
               |* * * * * * * |
               | * * * * * * *|
    ...
      * S7_object
      * gg
      Backtrace:
          ▆
       1. └─testthat::expect_match(class(b), "gg") at test-templates.R:37:3
       2.   └─testthat:::expect_match_(...)
      
      [ FAIL 45 | WARN 38 | SKIP 0 | PASS 74 ]
      Error: Test failures
      Execution halted
    ```

# TcGSA

<details>

* Version: 0.12.10
* GitHub: https://github.com/sistm/TcGSA
* Source code: https://github.com/cran/TcGSA
* Date/Publication: 2022-02-28 21:40:02 UTC
* Number of recursive dependencies: 131

Run `revdepcheck::cloud_details(, "TcGSA")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in Rd file 'plot1GS.Rd':
      ‘+.gg’
    
    Missing link or links in Rd file 'plotFit.GS.Rd':
      ‘+.gg’
    
    Missing link or links in Rd file 'plotMultipleGS.Rd':
      ‘+.gg’
    
    Missing link or links in Rd file 'plotPat.1GS.Rd':
      ‘+.gg’
    
    Missing link or links in Rd file 'plotSelect.GS.Rd':
      ‘+.gg’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# TCIU

<details>

* Version: 1.2.7
* GitHub: https://github.com/SOCR/TCIU
* Source code: https://github.com/cran/TCIU
* Date/Publication: 2024-09-15 02:40:02 UTC
* Number of recursive dependencies: 165

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
    --- finished re-building ‘tciu-LT-kimesurface.Rmd’
    
    --- re-building ‘tciu-fMRI-analytics.Rmd’ using rmarkdown
    
    Quitting from tciu-fMRI-analytics.Rmd:183-185 [unnamed-chunk-5]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `pm[[2]]`:
    ...
    
    Error: processing vignette 'tciu-fMRI-analytics.Rmd' failed with diagnostics:
    subscript out of bounds
    --- failed re-building ‘tciu-fMRI-analytics.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘tciu-fMRI-analytics.Rmd’
    
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

# teal.widgets

<details>

* Version: 0.4.3
* GitHub: https://github.com/insightsengineering/teal.widgets
* Source code: https://github.com/cran/teal.widgets
* Date/Publication: 2025-01-31 17:50:01 UTC
* Number of recursive dependencies: 104

Run `revdepcheck::cloud_details(, "teal.widgets")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > pkg_name <- "teal.widgets"
      > library(pkg_name, character.only = TRUE)
      > testthat::test_check(pkg_name)
      [ FAIL 3 | WARN 0 | SKIP 21 | PASS 203 ]
      
      ══ Skipped tests (21) ══════════════════════════════════════════════════════════
      • testing depth 3 is below current testing specification 5 (21):
    ...
      ── Failure ('test-ggplot2_args.R:267:5'): parse_ggplot2_args, when resolve_ggplot2_args priorotizes input in the order: user_plot, user_default, teal.ggplot2_args and module_plot ──
      deparse(parsed_all$theme, 500) not identical to "ggplot2::theme(axis.text = structure(list(), class = c(\"element_blank\", \"element\")))".
      1/1 mismatches
      x[1]: "ggplot2::theme(axis.text = <object>)"
      y[1]: "ggplot2::theme(axis.text = structure(list(), class = c(\"element_blank\",
      y[1]:  \"element\")))"
      
      [ FAIL 3 | WARN 0 | SKIP 21 | PASS 203 ]
      Error: Test failures
      Execution halted
    ```

# TestGardener

<details>

* Version: 3.3.5
* GitHub: NA
* Source code: https://github.com/cran/TestGardener
* Date/Publication: 2024-09-18 17:40:02 UTC
* Number of recursive dependencies: 122

Run `revdepcheck::cloud_details(, "TestGardener")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘TestGardener-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Ffuns_plot
    > ### Title: Plot a selection of fit criterion F functions and their first
    > ###   two derivatives.
    > ### Aliases: Ffuns_plot
    > 
    > ### ** Examples
    > 
    ...
    > #  short SweSAT multiple choice test with 24 items and 1000 examinees
    > chcemat   <- Quant_13B_problem_dataList$chcemat
    > index     <- Quant_13B_problem_parmList$index
    > SfdList   <- Quant_13B_problem_parmList$SfdList
    > plotindex <- 1:3
    > indfine   <- seq(0,100,len=101)
    > Ffuns_plot(indfine, index, SfdList, chcemat, plotindex)
    Error: <ggplot2::labels> object is invalid:
    - every label must be named.
    Execution halted
    ```

# thematic

<details>

* Version: 0.1.6
* GitHub: https://github.com/rstudio/thematic
* Source code: https://github.com/cran/thematic
* Date/Publication: 2024-07-29 15:50:02 UTC
* Number of recursive dependencies: 102

Run `revdepcheck::cloud_details(, "thematic")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘thematic-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: auto_config
    > ### Title: Configure auto theming behavior
    > ### Aliases: auto_config auto_config_set auto_config_get
    > 
    > ### ** Examples
    > 
    > old_config <- auto_config_set(auto_config("black", "white"))
    > thematic_with_theme(
    +   thematic_theme(), {
    +     plot(1:10, 1:10)
    +  })
    Error in get(x, envir = ns, inherits = FALSE) : 
      object 'ggplot_build.ggplot' not found
    Calls: thematic_with_theme ... <Anonymous> -> ggplot_build_set -> getFromNamespace -> get
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
      [ FAIL 17 | WARN 0 | SKIP 5 | PASS 13 ]
      
      ══ Skipped tests (5) ═══════════════════════════════════════════════════════════
    ...
      Backtrace:
          ▆
       1. └─thematic::thematic_on("black", "white", "green") at test-state.R:54:3
       2.   └─thematic:::ggplot_build_set()
       3.     └─utils::getFromNamespace("ggplot_build.ggplot", "ggplot2")
       4.       └─base::get(x, envir = ns, inherits = FALSE)
      
      [ FAIL 17 | WARN 0 | SKIP 5 | PASS 13 ]
      Error: Test failures
      Execution halted
    ```

# tidycat

<details>

* Version: 0.1.2
* GitHub: https://github.com/guyabel/tidycat
* Source code: https://github.com/cran/tidycat
* Date/Publication: 2021-08-02 04:20:01 UTC
* Number of recursive dependencies: 67

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
* Number of recursive dependencies: 131

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
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        R      1.0Mb
        data   2.0Mb
        doc    1.8Mb
    ```

# tidydr

<details>

* Version: 0.0.5
* GitHub: https://github.com/YuLab-SMU/tidydr
* Source code: https://github.com/cran/tidydr
* Date/Publication: 2023-03-08 09:20:02 UTC
* Number of recursive dependencies: 75

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
# tidyEdSurvey

<details>

* Version: 0.1.3
* GitHub: NA
* Source code: https://github.com/cran/tidyEdSurvey
* Date/Publication: 2024-05-14 20:20:03 UTC
* Number of recursive dependencies: 110

Run `revdepcheck::cloud_details(, "tidyEdSurvey")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(EdSurvey)
      Loading required package: car
      Loading required package: carData
      Loading required package: lfactors
      lfactors v1.0.4
      
    ...
      ── Failure ('test-ggplot.R:13:3'): density plot with facets and PVs ────────────
      class(p) not equal to c("gg", "ggplot").
      Lengths differ: 5 is not 2
      ── Failure ('test-ggplot.R:21:3'): boxplot with facets and PVs ─────────────────
      class(p) not equal to c("gg", "ggplot").
      Lengths differ: 5 is not 2
      
      [ FAIL 2 | WARN 0 | SKIP 1 | PASS 10 ]
      Error: Test failures
      Execution halted
    ```

# tidypaleo

<details>

* Version: 0.1.3
* GitHub: https://github.com/paleolimbot/tidypaleo
* Source code: https://github.com/cran/tidypaleo
* Date/Publication: 2023-01-18 08:20:03 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::cloud_details(, "tidypaleo")` for more info

</details>

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.rotate_facet_label_spec:
      function(object, plot, object_name)
    
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.sequential_layer_facet_spec:
      function(object, plot, object_name)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# tidyposterior

<details>

* Version: 1.0.1
* GitHub: https://github.com/tidymodels/tidyposterior
* Source code: https://github.com/cran/tidyposterior
* Date/Publication: 2023-10-11 18:50:02 UTC
* Number of recursive dependencies: 176

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

# tidysdm

<details>

* Version: 1.0.0
* GitHub: https://github.com/EvolEcolGroup/tidysdm
* Source code: https://github.com/cran/tidysdm
* Date/Publication: 2025-03-05 17:40:02 UTC
* Number of recursive dependencies: 191

Run `revdepcheck::cloud_details(, "tidysdm")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tidysdm-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_pres_vs_bg
    > ### Title: Plot presences vs background
    > ### Aliases: plot_pres_vs_bg
    > 
    > ### ** Examples
    > 
    > data("bradypus", package = "maxnet")
    ...
    +     ),
    +     ref = "presence"
    +   )) %>%
    +   select(-ecoreg)
    > 
    > bradypus_tb %>% plot_pres_vs_bg(presence)
    Error: Can't find method for generic `&(e1, e2)`:
    - e1: <patchwork>
    - e2: <theme>
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘a0_tidysdm_overview.Rmd’ using rmarkdown
    ```

# timetk

<details>

* Version: 2.9.0
* GitHub: https://github.com/business-science/timetk
* Source code: https://github.com/cran/timetk
* Date/Publication: 2023-10-31 22:30:02 UTC
* Number of recursive dependencies: 204

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

# tinyarray

<details>

* Version: 2.4.3
* GitHub: https://github.com/xjsun1221/tinyarray
* Source code: https://github.com/cran/tinyarray
* Date/Publication: 2025-03-05 13:20:02 UTC
* Number of recursive dependencies: 248

Run `revdepcheck::cloud_details(, "tinyarray")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tinyarray-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: box_surv
    > ### Title: box_surv
    > ### Aliases: box_surv
    > 
    > ### ** Examples
    > 
    > if(requireNamespace("ggpubr",quietly = TRUE)) {
    ...
    Please use `theme()` to construct themes.
    Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
    ℹ Please use tidy evaluation idioms with `aes()`.
    ℹ See also `vignette("ggplot2-in-packages")` for more information.
    ℹ The deprecated feature was likely used in the tinyarray package.
      Please report the issue at <https://github.com/xjsun1221/tinyarray/issues>.
    Error: Can't find method for generic `&(e1, e2)`:
    - e1: <patchwork>
    - e2: <theme>
    Execution halted
    ```

# tmt

<details>

* Version: 0.3.4-0
* GitHub: https://github.com/jansteinfeld/tmt
* Source code: https://github.com/cran/tmt
* Date/Publication: 2024-05-03 15:00:02 UTC
* Number of recursive dependencies: 76

Run `revdepcheck::cloud_details(, "tmt")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(tmt)
      - tmt 0.3.4-0 (2024-05-02)
      > 
      > test_check("tmt")
      The following items are specified in the dataset, but not in the submitted mstdesign: ii1 
      The following items are specified in the mstdesign, but not in the dataset: i1 
    ...
      [1] 1 - 11 == -10
      Backtrace:
          ▆
       1. └─testthat::expect_that(length(p), equals(11)) at test-tmt_gmc.R:34:5
       2.   └─testthat (local) condition(object)
       3.     └─testthat::expect_equal(x, expected, ..., expected.label = label)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 362 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘dexterMST’
    ```

# TPMplt

<details>

* Version: 0.1.6
* GitHub: https://github.com/CubicZebra/TPMplt
* Source code: https://github.com/cran/TPMplt
* Date/Publication: 2024-10-01 13:20:05 UTC
* Number of recursive dependencies: 90

Run `revdepcheck::cloud_details(, "TPMplt")` for more info

</details>

## Newly broken

*   checking whether package ‘TPMplt’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘e1071::element’ by ‘ggplot2::element’ when loading ‘TPMplt’
    See ‘/tmp/workdir/TPMplt/new/TPMplt.Rcheck/00install.out’ for details.
    ```

# track2KBA

<details>

* Version: 1.1.2
* GitHub: https://github.com/BirdLifeInternational/track2kba
* Source code: https://github.com/cran/track2KBA
* Date/Publication: 2024-07-01 10:40:07 UTC
* Number of recursive dependencies: 93

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
* Number of recursive dependencies: 141

Run `revdepcheck::cloud_details(, "tradeoffaucdim")` for more info

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
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-05-plot_curve.R:11:3'): output ───────────────────────────────
      plot_curve(obj4)$plot_performance has type 'object', not 'list'.
      ── Failure ('test-05-plot_curve.R:12:3'): output ───────────────────────────────
      plot_curve(obj4)$plot_time has type 'object', not 'list'.
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 38 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘speedglm’
      All declared Imports should be used.
    ```

# trajmsm

<details>

* Version: 0.1.3
* GitHub: https://github.com/awamaeva/R-package-trajmsm
* Source code: https://github.com/cran/trajmsm
* Date/Publication: 2024-10-05 19:00:02 UTC
* Number of recursive dependencies: 34

Run `revdepcheck::cloud_details(, "trajmsm")` for more info

</details>

## Newly broken

*   checking whether package ‘trajmsm’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘e1071::element’ by ‘ggplot2::element’ when loading ‘trajmsm’
    See ‘/tmp/workdir/trajmsm/new/trajmsm.Rcheck/00install.out’ for details.
    ```

# TransProR

<details>

* Version: 1.0.3
* GitHub: https://github.com/SSSYDYSSS/TransProR
* Source code: https://github.com/cran/TransProR
* Date/Publication: 2025-02-18 09:00:05 UTC
* Number of recursive dependencies: 209

Run `revdepcheck::cloud_details(, "TransProR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘TransProR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: highlight_by_node
    > ### Title: Highlight Nodes in a Phylogenetic Tree with Custom Fill Colors
    > ###   and Transparency
    > ### Aliases: highlight_by_node
    > 
    > ### ** Examples
    > 
    ...
    > p2 <- highlight_by_node(
    +   p2_plot,
    +   nodes,
    +   fill_colors,
    +   alpha_values,
    +   extend_values
    + )
    Error in ggtree_object + layer : non-numeric argument to binary operator
    Calls: highlight_by_node
    Execution halted
    ```

# trelliscopejs

<details>

* Version: 0.2.6
* GitHub: https://github.com/hafen/trelliscopejs
* Source code: https://github.com/cran/trelliscopejs
* Date/Publication: 2021-02-01 08:00:02 UTC
* Number of recursive dependencies: 102

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
       7.     ├─plotly (local) `<fn>`(p = `<ggplt2::>`)
       8.     └─plotly:::ggplotly.ggplot(p = `<ggplt2::>`)
       9.       └─plotly::gg2list(...)
      
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

*   checking S3 generic/method consistency ... WARNING
    ```
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.facet_trelliscope:
      function(object, plot, object_name)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# tricolore

<details>

* Version: 1.2.4
* GitHub: https://github.com/jschoeley/tricolore
* Source code: https://github.com/cran/tricolore
* Date/Publication: 2024-05-15 15:00:02 UTC
* Number of recursive dependencies: 104

Run `revdepcheck::cloud_details(, "tricolore")` for more info

</details>

## Newly broken

*   checking whether package ‘tricolore’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/tricolore/new/tricolore.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

## Installation

### Devel

```
* installing *source* package ‘tricolore’ ...
** package ‘tricolore’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'ggtern', details:
  call: NULL
  error: <ggplot2::element_line> object properties are invalid:
- @lineend must be <character> or <NULL>, not S3<arrow>
Execution halted
ERROR: lazy loading failed for package ‘tricolore’
* removing ‘/tmp/workdir/tricolore/new/tricolore.Rcheck/tricolore’


```
### CRAN

```
* installing *source* package ‘tricolore’ ...
** package ‘tricolore’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (tricolore)


```
# triplot

<details>

* Version: 1.3.0
* GitHub: https://github.com/ModelOriented/triplot
* Source code: https://github.com/cran/triplot
* Date/Publication: 2020-07-13 17:00:03 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::cloud_details(, "triplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘triplot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.triplot
    > ### Title: Plots triplot
    > ### Aliases: plot.triplot
    > 
    > ### ** Examples
    > 
    > library(DALEX)
    ...
    > explainer_apartments <- explain(model = apartments_num_lm_model,
    +                                 data = apartments_num[,-1],
    +                                 y = apartments_num[, 1],
    +                                 verbose = FALSE)
    > apartments_tri <- calculate_triplot(x = explainer_apartments,
    +  new_observation = apartments_num_new_observation[-1])
    > plot(apartments_tri)
    Error: <ggplot2::ggplot> object properties are invalid:
    - @labels must be <ggplot2::labels>, not <character>
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(triplot)
      > 
      > test_check("triplot")
      Welcome to DALEX (version: 2.4.3).
      Find examples and detailed introduction at: http://ema.drwhy.ai/
      Additional features will be available after installation of: ggpubr.
    ...
       14.             └─ggplot2::update_ggplot(object = object, plot = plot, ...)
       15.               ├─S7::S7_dispatch()
       16.               └─ggplot2 (local) `method(update_ggplot, list(ggplot2::theme, ggplot2::ggplot))`(...)
       17.                 └─S7::set_props(plot, theme = add_theme(plot@theme, object))
       18.                   └─S7::`props<-`(`*tmp*`, value = list(...))
       19.                     └─S7::validate(object)
      
      [ FAIL 2 | WARN 3 | SKIP 0 | PASS 74 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# triptych

<details>

* Version: 0.1.3
* GitHub: https://github.com/aijordan/triptych
* Source code: https://github.com/cran/triptych
* Date/Publication: 2024-06-13 15:50:02 UTC
* Number of recursive dependencies: 61

Run `revdepcheck::cloud_details(, "triptych")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘triptych-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.triptych
    > ### Title: Plot methods for the triptych classes
    > ### Aliases: plot.triptych autoplot.triptych plot.triptych_murphy
    > ###   autoplot.triptych_murphy plot.triptych_reliability
    > ###   autoplot.triptych_reliability plot.triptych_roc autoplot.triptych_roc
    > ###   plot.triptych_mcbdsc autoplot.triptych_mcbdsc
    > 
    ...
    > ### ** Examples
    > 
    > data(ex_binary, package = "triptych")
    > tr <- triptych(ex_binary)
    > 
    > dplyr::slice(tr, 1, 3, 6, 9) |> autoplot()
    Error: Can't find method for generic `&(e1, e2)`:
    - e1: <patchwork>
    - e2: <theme>
    Execution halted
    ```

# tsnet

<details>

* Version: 0.1.0
* GitHub: https://github.com/bsiepe/tsnet
* Source code: https://github.com/cran/tsnet
* Date/Publication: 2024-02-28 11:30:02 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::cloud_details(, "tsnet")` for more info

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
      Chain 1: 
      [ FAIL 1 | WARN 16 | SKIP 0 | PASS 107 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-centrality.R:52:3'): plot_centrality returns expected output ──
      `result` has type 'object', not 'list'.
      
      [ FAIL 1 | WARN 16 | SKIP 0 | PASS 107 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 159.7Mb
      sub-directories of 1Mb or more:
        libs  158.7Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# ulrb

<details>

* Version: 0.1.6
* GitHub: https://github.com/pascoalf/ulrb
* Source code: https://github.com/cran/ulrb
* Date/Publication: 2025-04-07 09:50:02 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "ulrb")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Glossary.Rmd’ using rmarkdown
    --- finished re-building ‘Glossary.Rmd’
    
    --- re-building ‘eco-analysis.Rmd’ using rmarkdown
    ```

# umiAnalyzer

<details>

* Version: 1.0.0
* GitHub: https://github.com/sfilges/umiAnalyzer
* Source code: https://github.com/cran/umiAnalyzer
* Date/Publication: 2021-11-25 08:40:02 UTC
* Number of recursive dependencies: 111

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

# UnalR

<details>

* Version: 1.0.0
* GitHub: https://github.com/estadisticaun/UnalR
* Source code: https://github.com/cran/UnalR
* Date/Publication: 2024-05-25 17:20:05 UTC
* Number of recursive dependencies: 167

Run `revdepcheck::cloud_details(, "UnalR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(UnalR)
      > 
      > test_check("UnalR")
      Starting 2 test processes
      [ FAIL 1 | WARN 4 | SKIP 2 | PASS 73 ]
      
    ...
      ══ Skipped tests (2) ═══════════════════════════════════════════════════════════
      • On CRAN (2): 'test-Plot_Boxplot.R:48:3', 'test-Tabla.R:40:3'
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-Plot_Series.R:110:3'): Plot.Series() captura de algunos valores claves a retornar ──
      `output_PlotSeries` has type 'object', not 'list'.
      
      [ FAIL 1 | WARN 4 | SKIP 2 | PASS 73 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.0Mb
      sub-directories of 1Mb or more:
        R      2.3Mb
        data   2.0Mb
        help   2.7Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 312859 marked UTF-8 strings
    ```

# usmap

<details>

* Version: 0.8.0
* GitHub: https://github.com/pdil/usmap
* Source code: https://github.com/cran/usmap
* Date/Publication: 2025-05-28 08:00:02 UTC
* Number of recursive dependencies: 87

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

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 17 marked UTF-8 strings
    ```

# utile.visuals

<details>

* Version: 0.3.3
* GitHub: https://github.com/efinite/utile.visuals
* Source code: https://github.com/cran/utile.visuals
* Date/Publication: 2023-01-24 01:00:02 UTC
* Number of recursive dependencies: 37

Run `revdepcheck::cloud_details(, "utile.visuals")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘utile.visuals-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: append_table
    > ### Title: Append a ggplot2 table to the bottom of a ggplot2 plot
    > ### Aliases: append_table
    > 
    > ### ** Examples
    > 
    > library(survival)
    ...
    +     aesthetics = c('colour', 'fill')) +
    +   theme_basic()
    > 
    > # Risk Table
    > tbl_risk <- ggrisktable(fit, c(0, 10, 20, 30, 40, 50)) +
    +   coord_cartesian(c(0, 50)) +
    +   scale_x_continuous(expand = c(0.02,0)) +
    +   theme_risk()
    Error: Can't find property <ggplot2::element_blank>@hjust
    Execution halted
    ```

# vaccine

<details>

* Version: 1.3.0
* GitHub: https://github.com/Avi-Kenny/vaccine
* Source code: https://github.com/cran/vaccine
* Date/Publication: 2025-01-07 18:00:01 UTC
* Number of recursive dependencies: 151

Run `revdepcheck::cloud_details(, "vaccine")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(vaccine)
      vaccine (version 1.3.0).
      Type ?vaccine to get started.
      > 
      > test_check("vaccine")
      Loading required package: nnls
    ...
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-test-core-functions.R:320:3'): plot_ce ───────────────────────
      class(p) (`actual`) not equal to c("gg", "ggplot") (`expected`).
      
      `actual`:        "ggplot2::ggplot" "ggplot" "ggplot2::gg" "S7_object" "gg"    
      `expected[2:2]`:                                                      "ggplot"
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 165 ]
      Error: Test failures
      Execution halted
    ```

# vaccineff

<details>

* Version: 1.0.0
* GitHub: https://github.com/epiverse-trace/vaccineff
* Source code: https://github.com/cran/vaccineff
* Date/Publication: 2024-11-29 09:30:02 UTC
* Number of recursive dependencies: 72

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

# vanquish

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/vanquish
* Date/Publication: 2018-09-05 14:50:04 UTC
* Number of recursive dependencies: 31

Run `revdepcheck::cloud_details(, "vanquish")` for more info

</details>

## Newly broken

*   checking whether package ‘vanquish’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘e1071::element’ by ‘ggplot2::element’ when loading ‘vanquish’
    See ‘/tmp/workdir/vanquish/new/vanquish.Rcheck/00install.out’ for details.
    ```

# VarSelLCM

<details>

* Version: 2.1.3.1
* GitHub: NA
* Source code: https://github.com/cran/VarSelLCM
* Date/Publication: 2020-10-14 16:34:36 UTC
* Number of recursive dependencies: 60

Run `revdepcheck::cloud_details(, "VarSelLCM")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘VarSelLCM.Rmd’ using rmarkdown
    
    Quitting from VarSelLCM.Rmd:112-115 [unnamed-chunk-10]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `rep()`:
    ! attempt to replicate an object of type 'object'
    ---
    Backtrace:
    ...
    
    Error: processing vignette 'VarSelLCM.Rmd' failed with diagnostics:
    attempt to replicate an object of type 'object'
    --- failed re-building ‘VarSelLCM.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘VarSelLCM.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 17.3Mb
      sub-directories of 1Mb or more:
        libs  16.5Mb
    ```

# vDiveR

<details>

* Version: 2.0.1
* GitHub: NA
* Source code: https://github.com/cran/vDiveR
* Date/Publication: 2024-11-22 08:20:02 UTC
* Number of recursive dependencies: 105

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
     32. │                                         └─base::lapply(...)
     33. │                                           └─ggplot2 (local) FUN(X[[i]], ...)
     34. │                                             └─ggplot2 (local) apply_fun(cur_data)
     35. │                                               └─ggplot2 (local) fun(x, ...)
     36. └─base::.handleSimpleError(...)
     37.   └─rlang (local) h(simpleError(msg, call))
     38.     └─handlers[[1L]](cnd)
     39.       └─cli::cli_abort(...)
     40.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘maps’
      All declared Imports should be used.
    ```

# vip

<details>

* Version: 0.4.1
* GitHub: https://github.com/koalaverse/vip
* Source code: https://github.com/cran/vip
* Date/Publication: 2023-08-21 09:20:02 UTC
* Number of recursive dependencies: 102

Run `revdepcheck::cloud_details(, "vip")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘tinytest.R’
    Running the tests in ‘tests/tinytest.R’ failed.
    Complete output:
      > if (requireNamespace("tinytest", quietly = TRUE)) {
      +   home <- length(unclass(packageVersion("vip"))[[1L]]) == 4
      +   tinytest::test_package("vip", at_home = home)
      + }
      
      Attaching package: 'vip'
      
    ...
      test_vip.R....................    0 tests    
      test_vip.R....................    1 tests [0;32mOK[0m 
      test_vip.R....................    2 tests [0;32mOK[0m 
      test_vip.R....................    3 tests [0;32mOK[0m [0;36m8ms[0m
      ----- FAILED[data]: test_pkg_nnet.R<47--50>
       call| expect_identical(current = class(p), target = c("gg", "ggplot"))
       diff| Lengths (2, 5) differ (string compare on first 2)
       diff| 1 string mismatch
      Error: 1 out of 25 tests failed
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      'C50', 'caret', 'Cubist', 'earth', 'gbm', 'glmnet', 'h2o',
      'lightgbm', 'mixOmics', 'mlr', 'mlr3', 'neuralnet', 'parsnip',
      'partykit', 'pls', 'randomForest', 'ranger', 'RSNNS', 'sparklyr',
      'tidymodels', 'workflows', 'xgboost'
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘randomForest’, ‘glmnet’, ‘C50’, ‘Cubist’, ‘caret’, ‘partykit’, ‘earth’, ‘gbm’, ‘h2o’, ‘sparklyr’, ‘ranger’, ‘xgboost’, ‘lightgbm’
    ```

# visualpred

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/visualpred
* Date/Publication: 2024-11-07 12:30:02 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::cloud_details(, "visualpred")` for more info

</details>

## Newly broken

*   checking whether package ‘visualpred’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘e1071::element’ by ‘ggplot2::element’ when loading ‘visualpred’
    See ‘/tmp/workdir/visualpred/new/visualpred.Rcheck/00install.out’ for details.
    ```

# vivaldi

<details>

* Version: 1.0.1
* GitHub: https://github.com/GreshamLab/vivaldi
* Source code: https://github.com/cran/vivaldi
* Date/Publication: 2023-03-21 20:10:02 UTC
* Number of recursive dependencies: 99

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
* Number of recursive dependencies: 130

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
      9.           └─ggplot2 (local) setup(..., self = self)
     10.             └─self$coord$setup_params(data)
     11.               └─ggplot2 (local) setup_params(..., self = self)
     12.                 └─ggproto_parent(Coord, self)$setup_params(data)
     13.                   └─ggplot2 (local) setup_params(..., self = self)
     14.                     └─ggplot2:::parse_coord_expand(expand = self$expand %||% TRUE)
     15.                       └─ggplot2:::check_logical(expand)
     16.                         └─ggplot2:::stop_input_type(...)
     17.                           └─rlang::abort(message, ..., call = call, arg = arg)
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
* Number of recursive dependencies: 133

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
      
      [ FAIL 3 | WARN 4 | SKIP 0 | PASS 58 ]
      Error: Test failures
      Execution halted
    ```

# washi

<details>

* Version: 0.2.0
* GitHub: https://github.com/WA-Department-of-Agriculture/washi
* Source code: https://github.com/cran/washi
* Date/Publication: 2023-09-07 23:50:02 UTC
* Number of recursive dependencies: 85

Run `revdepcheck::cloud_details(, "washi")` for more info

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
      `expected`: 0.0
      ── Failure ('test-theme.R:31:3'): washi_theme() works ──────────────────────────
      length(theme$panel.grid.major.y) (`actual`) not equal to 0 (`expected`).
      
        `actual`: 1.0
      `expected`: 0.0
      
      [ FAIL 2 | WARN 4 | SKIP 5 | PASS 13 ]
      Error: Test failures
      Execution halted
    ```

# wilson

<details>

* Version: 2.4.2
* GitHub: https://github.com/loosolab/wilson
* Source code: https://github.com/cran/wilson
* Date/Publication: 2021-04-19 09:40:02 UTC
* Number of recursive dependencies: 202

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

# windfarmGA

<details>

* Version: 4.0.0
* GitHub: https://github.com/YsoSirius/windfarmGA
* Source code: https://github.com/cran/windfarmGA
* Date/Publication: 2025-01-18 11:00:02 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::cloud_details(, "windfarmGA")` for more info

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
      `expected`: TRUE 
      ── Failure ('test_plots.R:126:3'): Test Plotting Functions ─────────────────────
      class(windr_res)[1] == "gg" is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      
      [ FAIL 2 | WARN 0 | SKIP 2 | PASS 703 ]
      Error: Test failures
      Execution halted
    ```

# wql

<details>

* Version: 1.0.2
* GitHub: https://github.com/jsta/wql
* Source code: https://github.com/cran/wql
* Date/Publication: 2025-01-23 10:10:02 UTC
* Number of recursive dependencies: 52

Run `revdepcheck::cloud_details(, "wql")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘wql-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: eofNum
    > ### Title: Plot EOF percent variance
    > ### Aliases: eofNum
    > ### Keywords: Graphics ts
    > 
    > ### ** Examples
    > 
    > 
    > # Create an annual time series data matrix from sfbay chlorophyll data
    > # Average over each year
    > chla1 <- aggregate(sfbayChla, 1, mean, na.rm = TRUE)  
    > chla1 <- chla1[, 1:12]  # remove stations with missing years
    > eofNum(chla1)
    Error: <ggplot2::labels> object is invalid:
    - every label must be named.
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘wql-package.Rmd’ using rmarkdown
    ```

# xaringanthemer

<details>

* Version: 0.4.3
* GitHub: https://github.com/gadenbuie/xaringanthemer
* Source code: https://github.com/cran/xaringanthemer
* Date/Publication: 2024-09-15 14:00:02 UTC
* Number of recursive dependencies: 72

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
* Number of recursive dependencies: 105

Run `revdepcheck::cloud_details(, "xpose")` for more info

</details>

## Newly broken

*   checking whether package ‘xpose’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/xpose/new/xpose.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘xpose’ ...
** package ‘xpose’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
Error in get("print.ggplot", envir = asNamespace("ggplot2")) : 
  object 'print.ggplot' not found
Warning: namespace ‘xpose’ is not available and has been replaced
by .GlobalEnv when processing object ‘xpdb_ex_pk’
...
Warning: namespace ‘xpose’ is not available and has been replaced
by .GlobalEnv when processing object ‘xpdb_ex_pk’
** inst
** byte-compile and prepare package for lazy loading
Error in get("print.ggplot", envir = asNamespace("ggplot2")) : 
  object 'print.ggplot' not found
Error: unable to load R code in package ‘xpose’
Execution halted
ERROR: lazy loading failed for package ‘xpose’
* removing ‘/tmp/workdir/xpose/new/xpose.Rcheck/xpose’


```
### CRAN

```
* installing *source* package ‘xpose’ ...
** package ‘xpose’ successfully unpacked and MD5 sums checked
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
* DONE (xpose)


```
# xpose.xtras

<details>

* Version: 0.0.2
* GitHub: https://github.com/jprybylski/xpose.xtras
* Source code: https://github.com/cran/xpose.xtras
* Date/Publication: 2024-11-21 17:20:02 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "xpose.xtras")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘xpose.xtras-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: catdv_vs_dvprobs
    > ### Title: Non-simulation based likelihood model diagnostic
    > ### Aliases: catdv_vs_dvprobs
    > 
    > ### ** Examples
    > 
    > 
    ...
      4. ├─purrr::map_if(...)
      5. │ └─purrr:::where_if(.x, .p)
      6. │   └─rlang::is_logical(.p)
      7. ├─stringr::str_detect(., "@")
      8. │ └─stringr:::check_lengths(string, pattern)
      9. │   └─vctrs::vec_size_common(...)
     10. └─vctrs:::stop_scalar_type(`<fn>`(`<ggplt2::>`), "string", `<env>`)
     11.   └─vctrs:::stop_vctrs(...)
     12.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
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
       10. ├─stringr::str_detect(., "@")
       11. │ └─stringr:::check_lengths(string, pattern)
       12. │   └─vctrs::vec_size_common(...)
       13. └─vctrs:::stop_scalar_type(`<fn>`(`<ggplt2::>`), "string", `<env>`)
       14.   └─vctrs:::stop_vctrs(...)
       15.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
      
      [ FAIL 10 | WARN 0 | SKIP 4 | PASS 622 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘a01-the-xp_xtra-object.Rmd’ using rmarkdown
    
    Quitting from a01-the-xp_xtra-object.Rmd:94-96 [plot_cont]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'a01-the-xp_xtra-object.Rmd' failed with diagnostics:
    `string` must be a vector, not a <ggplot2::labels/gg/S7_object> object.
    ...
    Error: processing vignette 'a03-useful_plots.Rmd' failed with diagnostics:
    `string` must be a vector, not a <ggplot2::labels/gg/S7_object> object.
    --- failed re-building ‘a03-useful_plots.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘a01-the-xp_xtra-object.Rmd’ ‘a02-xpose-sets.Rmd’
      ‘a03-useful_plots.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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

# yamlet

<details>

* Version: 1.2.1
* GitHub: https://github.com/bergsmat/yamlet
* Source code: https://github.com/cran/yamlet
* Date/Publication: 2025-04-18 12:30:02 UTC
* Number of recursive dependencies: 100

Run `revdepcheck::cloud_details(, "yamlet")` for more info

</details>

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.ggplot_isometric:
      function(object, plot, object_name)
    
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.ggplot_symmetric:
      function(object, plot, object_name)
    
    ggplot_build:
      function(plot, ...)
    ggplot_build.decorated_ggplot:
      function(plot)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘scripted-html.Rmd’ using rmarkdown
    ```

# ZetaSuite

<details>

* Version: 1.0.1
* GitHub: NA
* Source code: https://github.com/cran/ZetaSuite
* Date/Publication: 2022-05-24 19:40:02 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::cloud_details(, "ZetaSuite")` for more info

</details>

## Newly broken

*   checking whether package ‘ZetaSuite’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘e1071::element’ by ‘ggplot2::element’ when loading ‘ZetaSuite’
    See ‘/tmp/workdir/ZetaSuite/new/ZetaSuite.Rcheck/00install.out’ for details.
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

