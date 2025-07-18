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

## In both

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(afex)
      Loading required package: lme4
      Loading required package: Matrix
      ************
      Welcome to afex. For support visit: http://afex.singmann.science/
      - Functions for ANOVAs: aov_car(), aov_ez(), and aov_4()
    ...
      • afex_plot-default-support/afex-plots-nlme-3.svg
      • afex_plot-default-support/afex-plots-nlme-4.svg
      • afex_plot-default-support/afex-plots-nlme-5.svg
      • afex_plot-default-support/afex-plots-poisson-glm-1.svg
      • afex_plot-default-support/afex-plots-poisson-glm-2.svg
      • afex_plot-vignette/afex-plot-glmmtmb-1.svg
      • afex_plot-vignette/afex-plot-glmmtmb-2.svg
      • afex_plot-vignette/afex-plot-glmmtmb-3.svg
      Error: Test failures
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
    
    > ### Name: qq_plot
    > ### Title: QQ Plots for Numerical Variables
    > ### Aliases: qq_plot
    > 
    > ### ** Examples
    > 
    > data(mtcars)
    > qq_plot(mtcars)
    Error in eval(substitute(expr), data, enclos = parent.frame()) : 
      object 'x' not found
    Calls: qq_plot ... to_basic.GeomAbline -> with -> with.default -> eval -> eval
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
      installed size is 49.0Mb
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
    > data("combined_pbmc")
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
      
      [ FAIL 5 | WARN 53 | SKIP 10 | PASS 239 ]
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

*   checking installed package size ... NOTE
    ```
      installed size is  7.6Mb
      sub-directories of 1Mb or more:
        help   1.1Mb
        libs   5.3Mb
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

# AquaBEHER

<details>

* Version: 1.4.0
* GitHub: https://github.com/RobelTakele/AquaBEHER
* Source code: https://github.com/cran/AquaBEHER
* Date/Publication: 2024-09-24 22:10:05 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "AquaBEHER")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        doc    2.4Mb
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
      installed size is  6.9Mb
      sub-directories of 1Mb or more:
        data   6.5Mb
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

# autograph

<details>

* Version: 0.1.2
* GitHub: https://github.com/stocnet/autograph
* Source code: https://github.com/cran/autograph
* Date/Publication: 2025-07-02 15:30:06 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::cloud_details(, "autograph")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘autograph-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: map_measure
    > ### Title: Plotting logical marks Plotting numeric measures
    > ### Aliases: map_measure plot.node_measure plot.tie_measure
    > ###   plot.network_measures
    > 
    > ### ** Examples
    > 
    ...
    ! Only know how to add <ggplot> and/or <grob> objects
    Backtrace:
        ▆
     1. ├─base::plot(manynet::node_deg(ison_karateka))
     2. └─autograph:::plot.node_measure(manynet::node_deg(ison_karateka))
     3.   └─manynet:::`+.ggplot`(...)
     4.     └─patchwork::wrap_plots(e1, e2, ...)
     5.       └─cli::cli_abort("Only know how to add {.cls ggplot} and/or {.cls grob} objects")
     6.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘RSiena’
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

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(autoplotly)
      > 
      > test_check("autoplotly")
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 5 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      `expected`: TRUE 
      ── Failure ('test_all.R:16:3'): autoplotly is composable with additional ggplot2 elements ──
      inherits(p$ggplot_obj, "ggplot") is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 5 ]
      Error: Test failures
      Execution halted
    ```

# bartMan

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/bartMan
* Date/Publication: 2024-07-24 12:10:02 UTC
* Number of recursive dependencies: 132

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

# BayesianReasoning

<details>

* Version: 0.4.2
* GitHub: https://github.com/gorkang/BayesianReasoning
* Source code: https://github.com/cran/BayesianReasoning
* Date/Publication: 2023-11-14 11:33:20 UTC
* Number of recursive dependencies: 104

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
      
      [ FAIL 4 | WARN 0 | SKIP 0 | PASS 246 ]
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
* Number of recursive dependencies: 172

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
      `expected[2:2]`:                                                      "ggplot"
      ── Failure ('test-2_visualization.r:66:3'): radial_cheem_tour ──────────────────
      class(r_ggt) (`actual`) not equal to c("gg", "ggplot") (`expected`).
      
      `actual`:        "ggplot2::ggplot" "ggplot" "ggplot2::gg" "S7_object" "gg"    
      `expected[2:2]`:                                                      "ggplot"
      
      [ FAIL 6 | WARN 0 | SKIP 0 | PASS 10 ]
      Error: Test failures
      Execution halted
    ```

# circumplex

<details>

* Version: 1.0.0
* GitHub: https://github.com/jmgirard/circumplex
* Source code: https://github.com/cran/circumplex
* Date/Publication: 2024-10-28 04:30:02 UTC
* Number of recursive dependencies: 93

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
     22. │               └─l$compute_geom_2(d, theme = plot@theme)
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

# dendextend

<details>

* Version: 1.19.1
* GitHub: https://github.com/talgalili/dendextend
* Source code: https://github.com/cran/dendextend
* Date/Publication: 2025-07-15 08:50:09 UTC
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
      Welcome to dendextend version 1.19.1
      Type citation('dendextend') for how to cite the package.
    ...
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 872 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-ggdend.R:172:7'): ggplot doesn't have warnings for dendrograms ──
      names(built_plot) not identical to c("data", "layout", "plot").
      target is NULL, current is character
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 872 ]
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

# DImodelsVis

<details>

* Version: 1.0.1
* GitHub: NA
* Source code: https://github.com/cran/DImodelsVis
* Date/Publication: 2024-02-26 14:10:15 UTC
* Number of recursive dependencies: 111

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
    
    > ### Name: freqPlot
    > ### Title: Plot discrete observation frequencies per sample and per
    > ###   grouping
    > ### Aliases: freqPlot
    > 
    > ### ** Examples
    > 
    ...
        ▆
     1. └─dittoViz::freqPlot(...)
     2.   └─dittoViz::yPlot(...)
     3.     └─dittoViz:::.yPlot_add_data_y_direction(...)
     4.       └─ggplot2::geom_violin(draw_quantiles = vlnplot.quantiles)
     5.         └─ggplot2:::check_numeric(draw_quantiles)
     6.           └─ggplot2:::check_object(x, is.numeric, what, ..., arg = arg, call = call)
     7.             └─ggplot2:::stop_input_type(...)
     8.               └─rlang::abort(message, ..., call = call, arg = arg)
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
      
      [ FAIL 19 | WARN 19 | SKIP 0 | PASS 281 ]
      Error: Test failures
      Execution halted
    ```

# door

<details>

* Version: 0.0.2
* GitHub: NA
* Source code: https://github.com/cran/door
* Date/Publication: 2025-07-08 07:10:02 UTC
* Number of recursive dependencies: 54

Run `revdepcheck::cloud_details(, "door")` for more info

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
      ── Failure ('test-plots.R:46:3'): Partial credit plot ──────────────────────────
      partial_credit_biplot(pc_obj1) has type 'object', not 'list'.
      ── Failure ('test-plots.R:47:3'): Partial credit plot ──────────────────────────
      partial_credit_biplot(pc_obj2) has type 'object', not 'list'.
      ── Failure ('test-plots.R:48:3'): Partial credit plot ──────────────────────────
      partial_credit_contour_plot(y1 = y1t, y2 = y2t) has type 'object', not 'list'.
      
      [ FAIL 8 | WARN 10 | SKIP 0 | PASS 25 ]
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

# ENMTools

<details>

* Version: 1.1.4
* GitHub: https://github.com/danlwarren/ENMTools
* Source code: https://github.com/cran/ENMTools
* Date/Publication: 2025-07-14 00:10:05 UTC
* Number of recursive dependencies: 284

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

# extraSuperpower

<details>

* Version: 1.5.0
* GitHub: https://github.com/luisrmacias/extraSuperpower
* Source code: https://github.com/cran/extraSuperpower
* Date/Publication: 2025-07-15 15:20:02 UTC
* Number of recursive dependencies: 128

Run `revdepcheck::cloud_details(, "extraSuperpower")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘extraSuperpower-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: calculate_mean_matrix
    > ### Title: Create input for simulation based two-way factorial experiments
    > ### Aliases: calculate_mean_matrix
    > 
    > ### ** Examples
    > 
    > refmean <- 1
    ...
    > 
    > ## Plot should look the same, structure within data can be checked once simulated
    > effects_treat_time$meansplot
    > 
    > n <- 20
    > repeatedmeasures_experiment <- twoway_simulation_correlated(group_size = n,
    +                                           matrices_obj = effects_treat_time)
    Error in S7::prop(x, "meta")[[i]] : subscript out of bounds
    Calls: twoway_simulation_correlated -> [[ -> [[.ggplot2::gg
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
      Error in `S7::prop(x, "meta")[[i]]`: subscript out of bounds
      Backtrace:
          ▆
       1. └─extraSuperpower::twoway_simulation_correlated(...) at test-twoway_simulation_testing.R:104:3
       2.   ├─matrices_obj[[2]][[2]]
       3.   └─ggplot2 (local) `[[.ggplot2::gg`(matrices_obj[[2]], 2)
      
      [ FAIL 8 | WARN 4 | SKIP 0 | PASS 218 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘eSp_balanced_design_powertest.Rmd’ using rmarkdown
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
      `print(p)` produced messages.
      ── Failure ('test-graphics.R:346:3'): autoplot_dcmp_ts() ───────────────────────
      `print(p)` produced messages.
      
      [ FAIL 2 | WARN 0 | SKIP 1 | PASS 292 ]
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
      
      [ FAIL 3 | WARN 1 | SKIP 0 | PASS 299 ]
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
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test-graphics.R:194:3'): gg_tsdisplay() plots ───────────────────────
      Error in `UseMethod("ggplot_build")`: no applicable method for 'ggplot_build' applied to an object of class "NULL"
      Backtrace:
          ▆
       1. └─ggplot2::ggplot_build(p[[1]]) at test-graphics.R:194:3
      
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
     13. │       └─l$compute_geom_2(d, theme = plot@theme)
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

* Version: 0.38.3
* GitHub: https://github.com/psobczyk/geneSLOPE
* Source code: https://github.com/cran/geneSLOPE
* Date/Publication: 2025-06-20 04:40:02 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::cloud_details(, "geneSLOPE")` for more info

</details>

## Newly broken

*   checking whether package ‘geneSLOPE’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
    See ‘/tmp/workdir/geneSLOPE/new/geneSLOPE.Rcheck/00install.out’ for details.
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
      2. └─ggplot2 (local) `print.ggplot2::ggplot`(x)
      3.   ├─ggplot2::ggplot_build(x)
      4.   └─ggplot2 (local) `ggplot_build.ggplot2::ggplot`(x)
      5.     └─npscales$set_palettes(plot@theme)
      6.       └─ggplot2 (local) set_palettes(..., self = self)
      7.         ├─scales::as_discrete_pal(elem)
      8.         └─scales:::as_discrete_pal.default(elem)
      9.           └─cli::cli_abort("Cannot convert {.arg x} to a discrete palette.")
     10.             └─rlang::abort(...)
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
      installed size is 10.2Mb
      sub-directories of 1Mb or more:
        doc       1.6Mb
        extdata   2.2Mb
        libs      5.6Mb
    ```

# ggbreak

<details>

* Version: 0.1.5
* GitHub: https://github.com/YuLab-SMU/ggbreak
* Source code: https://github.com/cran/ggbreak
* Date/Publication: 2025-07-02 04:50:07 UTC
* Number of recursive dependencies: 61

Run `revdepcheck::cloud_details(, "ggbreak")` for more info

</details>

## Newly broken

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

* Version: 0.4.18
* GitHub: https://github.com/sinhrks/ggfortify
* Source code: https://github.com/cran/ggfortify
* Date/Publication: 2025-07-03 06:40:02 UTC
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
      
      [ FAIL 1 | WARN 13 | SKIP 48 | PASS 727 ]
      Error: Test failures
      Execution halted
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
* Number of recursive dependencies: 85

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
* Number of recursive dependencies: 106

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
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test-ggparallel.R:12:3'): ggparallel works ──────────────────────────
      Error in `UseMethod("ggplot_build")`: no applicable method for 'ggplot_build' applied to an object of class "c('gg', 'ggplot')"
      Backtrace:
          ▆
       1. └─ggplot2::ggplot_build(test_mtcars_plot) at test-ggparallel.R:12:3
      
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

# ggPMX

<details>

* Version: 1.3.0
* GitHub: https://github.com/ggPMXdevelopment/ggPMX
* Source code: https://github.com/cran/ggPMX
* Date/Publication: 2025-07-16 10:20:02 UTC
* Number of recursive dependencies: 160

Run `revdepcheck::cloud_details(, "ggPMX")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggPMX)
      > library(data.table)
      > setDTthreads(1)
      > test_check("ggPMX")
      convergence of SAEM parameter estimates  file does not exist.
      convergence of SAEM parameter estimates  file does not exist.
    ...
      Error: 'build_ggplot' is not an exported object from 'namespace:ggplot2'
      Backtrace:
          ▆
       1. └─testthat::expect_identical(...) at test-plot-individual.R:353:7
       2.   └─testthat::quasi_label(enquo(object), label, arg = "object")
       3.     └─rlang::eval_bare(expr, quo_get_env(quo))
      
      [ FAIL 1 | WARN 0 | SKIP 16 | PASS 1031 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘lixoftConnectors’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.0Mb
      sub-directories of 1Mb or more:
        help       2.4Mb
        testdata   3.0Mb
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

# ggraph

<details>

* Version: 2.2.1
* GitHub: https://github.com/thomasp85/ggraph
* Source code: https://github.com/cran/ggraph
* Date/Publication: 2024-03-07 12:40:02 UTC
* Number of recursive dependencies: 112

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
        NULL, NULL, <object>, NULL, 2, NULL, NULL, NULL, 1.2, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.2, NULL, <object>, NULL, <object>, NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, c(0, 0, 0, 0), <object>, 2, <object>, <object>, NULL, NULL, NULL, <object>, NULL, <object>, NULL, NULL, NULL, NULL, FALSE, NULL, NULL, <object>, <object>, "panel", <object>, <object>, "panel", <object>, "topleft", NULL, NULL, <object>, NULL, N
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
    
    > ### Name: resid_panel
    > ### Title: Panel of Diagnostic Residual Plots.
    > ### Aliases: resid_panel
    > 
    > ### ** Examples
    > 
    > 
    ...
     33. │                                       └─ggplot2 (local) FUN(X[[i]], ...)
     34. │                                         └─self$draw_group(group, panel_params, coord, ...)
     35. │                                           └─ggplot2 (local) draw_group(...)
     36. │                                             └─ggplot2 (local) draw_group(..., self = self)
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
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test-plot-helpers.R:192:3'): gg_tsdisplay() plots ───────────────────
      Error in `UseMethod("ggplot_build")`: no applicable method for 'ggplot_build' applied to an object of class "NULL"
      Backtrace:
          ▆
       1. └─ggplot2::ggplot_build(p[[1]]) at test-plot-helpers.R:192:3
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 32 ]
      Error: Test failures
      Execution halted
    ```

# ggtranslate

<details>

* Version: 0.1.0
* GitHub: https://github.com/mathiasleroy/ggtranslate
* Source code: https://github.com/cran/ggtranslate
* Date/Publication: 2025-07-10 15:00:02 UTC
* Number of recursive dependencies: 42

Run `revdepcheck::cloud_details(, "ggtranslate")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggtranslate-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggtranslate
    > ### Title: Translate all text elements in a ggplot2 object
    > ### Aliases: ggtranslate
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
    +   "Tuesday" = "Mardi",
    +   "Weekly Report" = "Rapport Hebdomadaire"
    + )
    > p_en <- ggplot(df, aes(x = day, y = value)) +
    +   geom_col() +
    +   labs(title = "Weekly Report")
    > p_fr <- ggtranslate(p_en, translation_fr)
    Error: <ggplot2::ggplot> object properties are invalid:
    - @labels must be <ggplot2::labels>, not <list>
    Execution halted
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
       41. │                                                 └─base::stop(...)
       42. └─base::.handleSimpleError(...)
       43.   └─rlang (local) h(simpleError(msg, call))
       44.     └─handlers[[1L]](cnd)
       45.       └─cli::cli_abort(...)
       46.         └─rlang::abort(...)
      
      [ FAIL 6 | WARN 4 | SKIP 19 | PASS 104 ]
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
      installed size is 14.9Mb
      sub-directories of 1Mb or more:
        data   5.0Mb
        doc    2.5Mb
        libs   7.2Mb
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

# hmde

<details>

* Version: 1.2.1
* GitHub: https://github.com/traitecoevo/hmde
* Source code: https://github.com/cran/hmde
* Date/Publication: 2025-07-04 19:20:06 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::cloud_details(, "hmde")` for more info

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
      ── Failure ('test-hmde_plot_de_pieces.R:21:3'): Execution and output: plot_de_pieces function ──
      `plot` has type 'object', not 'list'.
      ── Failure ('test-hmde_plot_obs_est_inds.R:5:3'): Execution and output: plot_obs_est_inds function ──
      `plot` does not have names.
      ── Failure ('test-hmde_plot_obs_est_inds.R:9:3'): Execution and output: plot_obs_est_inds function ──
      `plot` has type 'object', not 'list'.
      
      [ FAIL 4 | WARN 0 | SKIP 0 | PASS 85 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 175.3Mb
      sub-directories of 1Mb or more:
        article    3.0Mb
        doc        1.1Mb
        libs     170.5Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
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

* Version: 0.3.0
* GitHub: NA
* Source code: https://github.com/cran/implicitMeasures
* Date/Publication: 2025-07-10 14:50:05 UTC
* Number of recursive dependencies: 126

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
      [ FAIL 4 | WARN 1 | SKIP 2 | PASS 75 ]
      
    ...
      y[1]: "gg"
      ── Failure ('test-d_density_d_plot.R:99:3'): d_density produces a ggplot fot the SC-IAT ──
      class(d_density(d_sciat1))[[1]] not equal to "gg".
      1/1 mismatches
      x[1]: "ggplot2::ggplot"
      y[1]: "gg"
      
      [ FAIL 4 | WARN 1 | SKIP 2 | PASS 75 ]
      Error: Test failures
      Execution halted
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

* Version: 0.5.13
* GitHub: https://github.com/jinseob2kim/jstable
* Source code: https://github.com/cran/jskm
* Date/Publication: 2025-06-12 15:20:06 UTC
* Number of recursive dependencies: 108

Run `revdepcheck::cloud_details(, "jskm")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘jskm.Rmd’ using rmarkdown
    ```

# latrend

<details>

* Version: 1.6.2
* GitHub: https://github.com/niekdt/latrend
* Source code: https://github.com/cran/latrend
* Date/Publication: 2025-07-04 14:40:02 UTC
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
       13.                   └─gtable::gtable_add_row_space(panel_table, theme$panel.spacing.y %||% theme$panel.spacing)
       14.                     └─cli::cli_abort("{.arg height} must be of length 1 or nrow - 1")
       15.                       └─rlang::abort(...)
      
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
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
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
      
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 446 ]
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

# longevity

<details>

* Version: 1.2.1
* GitHub: https://github.com/lbelzile/longevity
* Source code: https://github.com/cran/longevity
* Date/Publication: 2025-07-03 21:20:02 UTC
* Number of recursive dependencies: 58

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
    Error in get(x = ifelse(packageVersion("ggplot2") >= "3.5.2.9001", "print.ggplot2::ggplot",  : 
      object 'print.ggplot2::ggplot' not found
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
      test-S3methods.R..............    0 tests    Error in get(x = ifelse(packageVersion("ggplot2") >= "3.5.2.9001", "print.ggplot2::ggplot",  : 
        object 'print.ggplot2::ggplot' not found
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

* Version: 1.5.1
* GitHub: https://github.com/stocnet/manynet
* Source code: https://github.com/cran/manynet
* Date/Publication: 2025-06-23 08:30:02 UTC
* Number of recursive dependencies: 135

Run `revdepcheck::cloud_details(, "manynet")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘manynet-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: make_random
    > ### Title: Making unconditional and conditional random networks
    > ### Aliases: make_random generate_random generate_configuration
    > ###   generate_man generate_utilities generate_permutation
    > 
    > ### ** Examples
    > 
    ...
    Backtrace:
        ▆
     1. └─manynet::graphr(generate_random(12, 0.4))
     2.   └─manynet:::.graph_layout(...)
     3.     └─ggraph::ggraph(lo)
     4.       └─manynet:::`+.ggplot`(...)
     5.         └─patchwork::wrap_plots(e1, e2, ...)
     6.           └─cli::cli_abort("Only know how to add {.cls ggplot} and/or {.cls grob} objects")
     7.             └─rlang::abort(...)
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
      [ FAIL 2 | WARN 0 | SKIP 20 | PASS 562 ]
      
    ...
       3.   └─manynet:::.graph_layout(...)
       4.     └─ggraph::ggraph(lo)
       5.       └─manynet:::`+.ggplot`(...)
       6.         └─patchwork::wrap_plots(e1, e2, ...)
       7.           └─cli::cli_abort("Only know how to add {.cls ggplot} and/or {.cls grob} objects")
       8.             └─rlang::abort(...)
      
      [ FAIL 2 | WARN 0 | SKIP 20 | PASS 562 ]
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
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        R           1.5Mb
        tutorials   1.6Mb
    ```

# MAPCtools

<details>

* Version: 0.1.0
* GitHub: https://github.com/LarsVatten/MAPCtools
* Source code: https://github.com/cran/MAPCtools
* Date/Publication: 2025-06-25 15:40:02 UTC
* Number of recursive dependencies: 110

Run `revdepcheck::cloud_details(, "MAPCtools")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        extdata   4.2Mb
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
* Number of recursive dependencies: 64

Run `revdepcheck::cloud_details(, "meme")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘meme.Rmd’ using rmarkdown
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
      3.   ├─ggplot2::ggplot_gtable(data)
      4.   └─ggplot2 (local) `ggplot_gtable.ggplot2::ggplot_built`(data)
      5.     └─layout$render(geom_grobs, data, theme, plot@labels)
      6.       └─ggplot2 (local) render(..., self = self)
      7.         └─self$facet$draw_panels(...)
      8.           └─lemon (local) draw_panels(...)
      9.             └─gtable::gtable_add_col_space(panel_table, theme$panel.spacing.x %||% theme$panel.spacing)
     10.               └─cli::cli_abort("{.arg width} must be of length 1 or ncol - 1")
     11.                 └─rlang::abort(...)
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
    
    > ### Name: MetaboWAS
    > ### Title: MetaboWAS
    > ### Aliases: MetaboWAS
    > 
    > ### ** Examples
    > 
    > require(MiMIR)
    ...
    Backtrace:
        ▆
     1. └─MiMIR::MetaboWAS(...)
     2.   └─ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, ylim))
     3.     └─ggplot2::continuous_scale(...)
     4.       └─ggplot2:::check_continuous_limits(limits, call = call)
     5.         └─ggplot2:::check_length(limits, 2L, arg = arg, call = call)
     6.           └─cli::cli_abort(msg, call = call, arg = arg)
     7.             └─rlang::abort(...)
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
      [ FAIL 6 | WARN 15 | SKIP 0 | PASS 204 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      y[1]: "list"
      ── Failure ('test-plotscore.R:44:5'): Tests plotting scores ────────────────────
      typeof(plot_own) not equal to "list".
      1/1 mismatches
      x[1]: "object"
      y[1]: "list"
      
      [ FAIL 6 | WARN 15 | SKIP 0 | PASS 204 ]
      Error: Test failures
      Execution halted
    ```

# MiscMetabar

<details>

* Version: 0.14.3
* GitHub: https://github.com/adrientaudiere/MiscMetabar
* Source code: https://github.com/cran/MiscMetabar
* Date/Publication: 2025-06-21 14:00:02 UTC
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
      
      [ FAIL 2 | WARN 2 | SKIP 74 | PASS 81 ]
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

# mistral

<details>

* Version: 2.2.3
* GitHub: NA
* Source code: https://github.com/cran/mistral
* Date/Publication: 2025-07-16 05:00:02 UTC
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

# mlr3spatiotempcv

<details>

* Version: 2.3.3
* GitHub: https://github.com/mlr-org/mlr3spatiotempcv
* Source code: https://github.com/cran/mlr3spatiotempcv
* Date/Publication: 2025-07-10 15:20:10 UTC
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
      installed size is  5.7Mb
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

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.0Mb
      sub-directories of 1Mb or more:
        doc       4.9Mb
        extdata   1.9Mb
        libs      1.6Mb
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
      testthat 2025-07-18 12:24:01 DEBUG test_check: Entry
      testthat 2025-07-18 12:24:01 DEBUG test_check: Entry
      testthat 2025-07-18 12:24:01 DEBUG test_check: Exit
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
# mverse

<details>

* Version: 0.2.2
* GitHub: https://github.com/mverseanalysis/mverse
* Source code: https://github.com/cran/mverse
* Date/Publication: 2025-06-21 10:40:06 UTC
* Number of recursive dependencies: 134

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
      Error in python_config_impl(python) : 
        Error running '/root/.cache/R/reticulate/uv/cache/builds-v0/.tmpRFGVnf/bin/python': No such file.
      Error in python_config_impl(python) : 
    ...
      `expected[2:2]`:                                                      "ggplot"
      ── Failure ('test-plot_mx_umap.R:16:5'): plotting works ────────────────────────
      class(plot_mx_umap(mx_data)) (`actual`) not equal to c("gg", "ggplot") (`expected`).
      
      `actual`:        "ggplot2::ggplot" "ggplot" "ggplot2::gg" "S7_object" "gg"    
      `expected[2:2]`:                                                      "ggplot"
      
      [ FAIL 2 | WARN 8 | SKIP 6 | PASS 85 ]
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

# mycolorsTB

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/mycolorsTB
* Date/Publication: 2025-07-16 17:10:02 UTC
* Number of recursive dependencies: 66

Run `revdepcheck::cloud_details(, "mycolorsTB")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘use-mycolors.Rmd’ using rmarkdown
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

# nlmixr2plot

<details>

* Version: 3.0.2
* GitHub: https://github.com/nlmixr2/nlmixr2plot
* Source code: https://github.com/cran/nlmixr2plot
* Date/Publication: 2025-06-25 05:30:02 UTC
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
      ggplot2::is_ggplot(p2[[1]]) is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      
      [ FAIL 2 | WARN 4 | SKIP 2 | PASS 33 ]
      Error: Test failures
      Execution halted
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
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test_plot.R:21:3'): test normfluodbf plot ─────────────────────────
      "gg" (`actual`) not equal to class(plt.obj)[1] (`expected`).
      
      `actual`:   "gg"             
      `expected`: "ggplot2::ggplot"
      
      [ FAIL 1 | WARN 617 | SKIP 0 | PASS 23 ]
      Error: Test failures
      Execution halted
    ```

# nzelect

<details>

* Version: 0.4.0
* GitHub: NA
* Source code: https://github.com/cran/nzelect
* Date/Publication: 2017-10-02 20:35:23 UTC
* Number of recursive dependencies: 78

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
* Number of recursive dependencies: 146

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
        Please report the issue to the authors. 
      2: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
      ℹ Please use `linewidth` instead.
      ℹ The deprecated feature was likely used in the UpSetR package.
        Please report the issue to the authors. 
      3: The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
      ℹ Please use the `linewidth` argument instead.
      ℹ The deprecated feature was likely used in the UpSetR package.
        Please report the issue to the authors. 
      Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘OmicNavigatorAPI.Rnw’ using Sweave
    OmicNavigator R package version: 1.15.0
    The app is not installed. Install it with installApp()
    Installing study "ABC" in /tmp/Rtmpj47Bsp/file1e09511743e0
    Exporting study "ABC" as an R package
    Note: No maintainer email was specified. Using the placeholder: Unknown <unknown@unknown>
    Calculating pairwise overlaps. This may take a while...
    Exported study to /tmp/Rtmpj47Bsp/ONstudyABC
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

# patchwork

<details>

* Version: 1.3.1
* GitHub: https://github.com/thomasp85/patchwork
* Source code: https://github.com/cran/patchwork
* Date/Publication: 2025-06-21 13:20:05 UTC
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "patchwork")` for more info

</details>

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.formula:
      function(object, plot, object_name)
    
    ggplot_add:
      function(object, plot, ...)
    ggplot_add.nativeRaster:
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
       21.                       \-ggplot2:::plot_theme(plot)
       22.                         \-ggplot2:::check_theme(theme)
       23.                           \-base::mapply(...)
       24.                             \-ggplot2 (local) `<fn>`(...)
       25.                               \-cli::cli_abort(...)
       26.                                 \-rlang::abort(...)
      
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

# phylepic

<details>

* Version: 0.2.0
* GitHub: https://github.com/cidm-ph/phylepic
* Source code: https://github.com/cran/phylepic
* Date/Publication: 2024-05-31 19:10:02 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::cloud_details(, "phylepic")` for more info

</details>

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
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
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 48 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      ── Failure ('test-plotDK.R:72:5'): The returned plot has the correct labels ────
      labels$fill not equal to "test_label".
      target is NULL, current is character
      ── Failure ('test-plotDK.R:81:5'): The returned plot has the correct labels ────
      labels$fill not equal to "fill".
      target is NULL, current is character
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 48 ]
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

* Version: 0.7.2
* GitHub: https://github.com/pwwang/plotthis
* Source code: https://github.com/cran/plotthis
* Date/Publication: 2025-07-13 02:30:12 UTC
* Number of recursive dependencies: 219

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

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        help   3.3Mb
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

# psborrow

<details>

* Version: 0.2.3
* GitHub: NA
* Source code: https://github.com/cran/psborrow
* Date/Publication: 2025-06-23 14:30:02 UTC
* Number of recursive dependencies: 103

Run `revdepcheck::cloud_details(, "psborrow")` for more info

</details>

## Newly broken

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
    Caused by error in `UseMethod()`:
    ! no applicable method for 'ggplot_build' applied to an object of class "c('gg', 'ggplot')"
    --- failed re-building ‘psc-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘psc-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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

* Version: 1.0.5
* GitHub: https://github.com/griefl/qad
* Source code: https://github.com/cran/qad
* Date/Publication: 2025-07-02 08:40:02 UTC
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
    Calls: ggplot_build -> try_prop -> <Anonymous> -> [[ -> [[.ggplot2::gg
    Execution halted
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

# reda

<details>

* Version: 0.5.5
* GitHub: https://github.com/wenjie2wang/reda
* Source code: https://github.com/cran/reda
* Date/Publication: 2025-06-14 04:50:02 UTC
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
      Error: 5 out of 191 tests failed
      In addition: There were 16 warnings (use warnings() to see them)
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.5Mb
      sub-directories of 1Mb or more:
        doc    3.6Mb
        libs   4.2Mb
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
      Ignoring unknown labels:
    ...
       30. │                                     └─ggplot2 (local) draw_group(..., self = self)
       31. └─base::.handleSimpleError(...)
       32.   └─rlang (local) h(simpleError(msg, call))
       33.     └─handlers[[1L]](cnd)
       34.       └─cli::cli_abort(...)
       35.         └─rlang::abort(...)
      
      [ FAIL 2 | WARN 0 | SKIP 20 | PASS 44 ]
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
* Number of recursive dependencies: 70

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
      `expected`: TRUE 
      Backtrace:
          ▆
       1. ├─vcr::use_cassette(...) at test_wtss.R:53:5
       2. │ └─cassette$call_block(...)
       3. └─testthat::expect_true(unname(summary(g)[1, 2]) == "gg") at test_wtss.R:60:9
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 57 ]
      Error: Test failures
      Execution halted
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
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 224 ]
    ...
       24. │                         └─self$draw_panel(...)
       25. └─base::.handleSimpleError(...)
       26.   └─rlang (local) h(simpleError(msg, call))
       27.     └─handlers[[1L]](cnd)
       28.       └─cli::cli_abort(...)
       29.         └─rlang::abort(...)
      
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 224 ]
      Error: Test failures
      Execution halted
    ```

# scCustomize

<details>

* Version: 3.0.1
* GitHub: https://github.com/samuel-marsh/scCustomize
* Source code: https://github.com/cran/scCustomize
* Date/Publication: 2024-12-18 18:40:02 UTC
* Number of recursive dependencies: 276

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

# scoringutils

<details>

* Version: 2.1.1
* GitHub: https://github.com/epiforecasts/scoringutils
* Source code: https://github.com/cran/scoringutils
* Date/Publication: 2025-06-25 22:20:02 UTC
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
      ── Failure ('test-do_TFActivityPlot.R:6:5'): do_TFActivityPlot: PASS - minimal input ──
      `out` has type 'object', not 'list'.
      ── Failure ('test-do_ViolinPlot.R:6:5'): do_ViolinPlot: CRAN essentials ────────
      `p` has type 'object', not 'list'.
      ── Failure ('test-do_VolcanoPlot.R:8:5'): do_VolcanoPlot: CRAN essentials ──────
      `p` has type 'object', not 'list'.
      
      [ FAIL 58 | WARN 16 | SKIP 391 | PASS 29 ]
      Error: Test failures
      Execution halted
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
      
      [ FAIL 1 | WARN 3 | SKIP 23 | PASS 34 ]
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
    
    > ### Name: multicurve
    > ### Title: Graph: Merge multiple curves into a single graph
    > ### Aliases: multicurve
    > 
    > ### ** Examples
    > 
    > library(seedreg)
    ...
    9                      r-squared   0.8400000
    10                          RMSE  12.5937903
    
    [[3]]
    
    > multicurve(list(a,b,c,d))
    Error in equation[[i]] <- plots[[i]][[3]]$plot$s : 
      replacement has length zero
    Calls: multicurve
    Execution halted
    ```

# shinyMixR

<details>

* Version: 0.5.1
* GitHub: https://github.com/RichardHooijmaijers/shinyMixR
* Source code: https://github.com/cran/shinyMixR
* Date/Publication: 2025-07-18 09:20:02 UTC
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
      1/1 mismatches
      [1] 1 - 11 == -10
      ── Failure ('test-fit_plot.R:26:3'): fit_plot works as expected ────────────────
      length(plot) not equal to 11.
      1/1 mismatches
      [1] 1 - 11 == -10
      
      [ FAIL 3 | WARN 1 | SKIP 11 | PASS 54 ]
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

# spacejamr

<details>

* Version: 0.2.1
* GitHub: https://github.com/dscolby/spacejamr
* Source code: https://github.com/cran/spacejamr
* Date/Publication: 2022-04-01 20:10:02 UTC
* Number of recursive dependencies: 132

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
      
      test_get.pixel.data.R.........    1 tests [0;32mOK[0m [0;36m83ms[0m
      
      test_prepare.vector.data.R....    1 tests [0;32mOK[0m 
    ...
      test_prepare.vector.data.R....    8 tests [0;32mOK[0m 
      test_prepare.vector.data.R....    9 tests [0;32mOK[0m 
      test_prepare.vector.data.R....   10 tests [0;32mOK[0m 
      test_prepare.vector.data.R....   11 tests [0;32mOK[0m 
      test_prepare.vector.data.R....   12 tests [0;32mOK[0m [0;36m60ms[0m
      
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
* Number of recursive dependencies: 175

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
      `expected[2:2]`:                                                      "ggplot"
      ── Failure ('test-zDepricated_3_visualize.r:94:3'): view_manip_space: gganimate class and length ──
      class(ret_heavy) (`actual`) not equal to c("gg", "ggplot") (`expected`).
      
      `actual`:        "ggplot2::ggplot" "ggplot" "ggplot2::gg" "S7_object" "gg"    
      `expected[2:2]`:                                                      "ggplot"
      
      [ FAIL 11 | WARN 0 | SKIP 0 | PASS 80 ]
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
      
      [ FAIL 3 | WARN 0 | SKIP 1 | PASS 66 ]
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

* Version: 0.2.4
* GitHub: https://github.com/arcaldwell49/Superpower
* Source code: https://github.com/cran/Superpower
* Date/Publication: 2025-06-24 19:30:02 UTC
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
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-morey.R:109:3'): No error messages ftest ─────────────────────
      class(test)[1] not equal to "gg".
      1/1 mismatches
      x[1]: "ggplot2::ggplot"
      y[1]: "gg"
      
      [ FAIL 1 | WARN 18 | SKIP 13 | PASS 399 ]
      Error: Test failures
      Execution halted
    ```

# tastypie

<details>

* Version: 0.1.1
* GitHub: https://github.com/PaoloDalena/tastypie
* Source code: https://github.com/cran/tastypie
* Date/Publication: 2023-09-06 18:00:06 UTC
* Number of recursive dependencies: 77

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

# topolow

<details>

* Version: 1.0.0
* GitHub: https://github.com/omid-arhami/topolow
* Source code: https://github.com/cran/topolow
* Date/Publication: 2025-07-11 12:30:02 UTC
* Number of recursive dependencies: 127

Run `revdepcheck::cloud_details(, "topolow")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘topolow-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_cluster_mapping
    > ### Title: Create Clustered Mapping Plots
    > ### Aliases: plot_cluster_mapping
    > 
    > ### ** Examples
    > 
    > # Basic usage with default configurations
    ...
    > data <- data.frame(
    +   V1 = rnorm(100), V2 = rnorm(100), V3 = rnorm(100), name = 1:100,
    +   antigen = rep(c(0,1), 50), antiserum = rep(c(1,0), 50),
    +   cluster = rep(1:5, each=20)
    + )
    > p1 <- plot_cluster_mapping(data, ndim=3)
    Error in new_layout_config() : 
      inherits(plot_margin, "margin") is not TRUE
    Calls: plot_cluster_mapping -> new_layout_config -> stopifnot
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
      ── Error ('test-visualization.R:47:3'): plot configuration objects work correctly ──
      Error in `new_layout_config(width = 8, height = 6)`: inherits(plot_margin, "margin") is not TRUE
      Backtrace:
          ▆
       1. └─topolow::new_layout_config(width = 8, height = 6) at test-visualization.R:47:3
       2.   └─base::stopifnot(...)
      
      [ FAIL 4 | WARN 0 | SKIP 0 | PASS 50 ]
      Error: Test failures
      Execution halted
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

# TransProR

<details>

* Version: 1.0.5
* GitHub: https://github.com/SSSYDYSSS/TransProR
* Source code: https://github.com/cran/TransProR
* Date/Publication: 2025-06-16 08:20:02 UTC
* Number of recursive dependencies: 200

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

# vaccine

<details>

* Version: 1.3.0
* GitHub: https://github.com/Avi-Kenny/vaccine
* Source code: https://github.com/cran/vaccine
* Date/Publication: 2025-01-07 18:00:01 UTC
* Number of recursive dependencies: 152

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
* Number of recursive dependencies: 59

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
      installed size is 17.4Mb
      sub-directories of 1Mb or more:
        libs  16.7Mb
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
      test_vip.R....................    3 tests [0;32mOK[0m [0;36m7ms[0m
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
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-basic_plot.R:15:3'): basic_plot function test ────────────────
      `test_plot1` has type 'object', not 'list'.
      ── Failure ('test-basic_plot.R:19:3'): basic_plot function test ────────────────
      `test_plot2` has type 'object', not 'list'.
      
      [ FAIL 2 | WARN 2 | SKIP 0 | PASS 59 ]
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

# xpose

<details>

* Version: 0.4.19
* GitHub: https://github.com/UUPharmacometrics/xpose
* Source code: https://github.com/cran/xpose
* Date/Publication: 2025-01-07 20:00:02 UTC
* Number of recursive dependencies: 104

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
* Number of recursive dependencies: 104

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
      
      [ FAIL 7 | WARN 0 | SKIP 6 | PASS 647 ]
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

