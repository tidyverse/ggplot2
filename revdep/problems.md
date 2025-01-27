# ABHgenotypeR

<details>

* Version: 1.0.1
* GitHub: https://github.com/StefanReuscher/ABHgenotypeR
* Source code: https://github.com/cran/ABHgenotypeR
* Date/Publication: 2016-02-04 11:27:29
* Number of recursive dependencies: 54

Run `revdepcheck::cloud_details(, "ABHgenotypeR")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ABHgenotypeR-vignette.Rmd’
      ...
    +     "preprefall025TestData.csv", package = "ABHgenotypeR"), nameA = "NB", 
    +     nameB = "OL ..." ... [TRUNCATED] 
    
    > plotGenos(genotypes)
    
      When sourcing ‘ABHgenotypeR-vignette.R’:
    Error: The `panel.margin` argument of `theme()` was deprecated in ggplot2 2.2.0
    and is now defunct.
    ℹ Please use the `panel.spacing` argument instead.
    Execution halted
    
      ‘ABHgenotypeR-vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ABHgenotypeR-vignette.Rmd’ using rmarkdown
    
    Quitting from lines 70-72 [unnamed-chunk-3] (ABHgenotypeR-vignette.Rmd)
    Error: processing vignette 'ABHgenotypeR-vignette.Rmd' failed with diagnostics:
    The `panel.margin` argument of `theme()` was deprecated in ggplot2 2.2.0
    and is now defunct.
    ℹ Please use the `panel.spacing` argument instead.
    --- failed re-building ‘ABHgenotypeR-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ABHgenotypeR-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘actxps.Rmd’
      ...
    # ℹ 2 more variables: ae_expected_1 <dbl>, ae_expected_2 <dbl>
    
    > autoplot(exp_res)
    Warning: thematic was unable to resolve `bg='auto'`. Try providing an actual color (or `NA`) to the `bg` argument of `thematic_on()`. By the way, 'auto' is only officially supported in `shiny::renderPlot()`, some rmarkdown scenarios (specifically, `html_document()` with `theme!=NULL`), in RStudio, or if `auto_config_set()` is used.
    Warning: thematic was unable to resolve `fg='auto'`. Try providing an actual color (or `NA`) to the `fg` argument of `thematic_on()`. By the way, 'auto' is only officially supported in `shiny::renderPlot()`, some rmarkdown scenarios (specifically, `html_document()` with `theme!=NULL`), in RStudio, or if `auto_config_set()` is used.
    Warning: thematic was unable to resolve `accent='auto'`. Try providing an actual color (or `NA`) to the `accent` argument of `thematic_on()`. By the way, 'auto' is only officially supported in `shiny::renderPlot()`, some rmarkdown scenarios (specifically, `html_document()` with `theme!=NULL`), in RStudio, or if `auto_config_set()` is used.
    
    ...
    
      When sourcing ‘transactions.R’:
    Error: Internal error: adjust_color() expects an input of length 1
    Execution halted
    
      ‘actxps.Rmd’ using ‘UTF-8’... failed
      ‘exp_summary.Rmd’ using ‘UTF-8’... OK
      ‘exposures.Rmd’ using ‘UTF-8’... OK
      ‘misc.Rmd’ using ‘UTF-8’... failed
      ‘transactions.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘actxps.Rmd’ using rmarkdown
    Warning: thematic was unable to resolve `bg='auto'`. Try providing an actual color (or `NA`) to the `bg` argument of `thematic_on()`. By the way, 'auto' is only officially supported in `shiny::renderPlot()`, some rmarkdown scenarios (specifically, `html_document()` with `theme!=NULL`), in RStudio, or if `auto_config_set()` is used.
    Warning: thematic was unable to resolve `fg='auto'`. Try providing an actual color (or `NA`) to the `fg` argument of `thematic_on()`. By the way, 'auto' is only officially supported in `shiny::renderPlot()`, some rmarkdown scenarios (specifically, `html_document()` with `theme!=NULL`), in RStudio, or if `auto_config_set()` is used.
    Warning: thematic was unable to resolve `accent='auto'`. Try providing an actual color (or `NA`) to the `accent` argument of `thematic_on()`. By the way, 'auto' is only officially supported in `shiny::renderPlot()`, some rmarkdown scenarios (specifically, `html_document()` with `theme!=NULL`), in RStudio, or if `auto_config_set()` is used.
    
    Quitting from lines 129-130 [plot] (actxps.Rmd)
    Error: processing vignette 'actxps.Rmd' failed with diagnostics:
    Internal error: adjust_color() expects an input of length 1
    --- failed re-building ‘actxps.Rmd’
    ...
    Quitting from lines 205-211 [trx-plot] (transactions.Rmd)
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
* Number of recursive dependencies: 64

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

# adobeanalyticsr

<details>

* Version: 0.5.0
* GitHub: https://github.com/benrwoodard/adobeanalyticsr
* Source code: https://github.com/cran/adobeanalyticsr
* Date/Publication: 2025-01-16 06:10:02 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::cloud_details(, "adobeanalyticsr")` for more info

</details>

## Newly broken

*   checking whether package ‘adobeanalyticsr’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(limit[i], ': unused argument (c("within", "after")) 
    See ‘/tmp/workdir/adobeanalyticsr/new/adobeanalyticsr.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    seg_then: possible error in is.element(limit[i], c("within", "after")):
      unused argument (c("within", "after"))
    ```

# adw

<details>

* Version: 0.4.0
* GitHub: https://github.com/PanfengZhang/adw
* Source code: https://github.com/cran/adw
* Date/Publication: 2024-04-15 19:10:16 UTC
* Number of recursive dependencies: 62

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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Introduction.Rmd’
      ...
    
    > tavg <- data.frame(lon = runif(100, min = 110, max = 117), 
    +     lat = runif(100, min = 31, max = 37), value = runif(100, 
    +         min = 20, max  .... [TRUNCATED] 
    
    > hmap <- getMap(name = "河南省", returnClass = "sf")
    
      When sourcing ‘Introduction.R’:
    Error: package tibble not available: install first?
    Execution halted
    
      ‘Introduction.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Introduction.Rmd’ using rmarkdown
    
    Quitting from lines 48-74 [unnamed-chunk-3] (Introduction.Rmd)
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
* Number of recursive dependencies: 97

Run `revdepcheck::cloud_details(, "AeRobiology")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘my-vignette.Rmd’
      ...
    Warning: Ignoring unknown labels:
    • `size = "14"`
    
    > iplot_abundance(munich_pollen, interpolation = FALSE, 
    +     export.plot = FALSE, export.result = FALSE, n.types = 3, 
    +     y.start = 2011, y.end = .... [TRUNCATED] 
    
      When sourcing ‘my-vignette.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘my-vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘my-vignette.Rmd’ using rmarkdown
    ```

# agricolaeplotr

<details>

* Version: 0.6.0
* GitHub: https://github.com/jensharbers/agricolaeplotr
* Source code: https://github.com/cran/agricolaeplotr
* Date/Publication: 2025-01-22 13:20:02 UTC
* Number of recursive dependencies: 143

Run `revdepcheck::cloud_details(, "agricolaeplotr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(agricolaeplotr)
      
      Type 'citation("agricolaeplotr")' for citing this R package in publications.
      
      Attaching package: 'agricolaeplotr'
      
    ...
      `expected` is a character vector ('ROW')
      ── Failure ('testall.R:847:3'): plot a plot design from FielDHub package shows COLUMN as x axis ──
      p$labels$x (`actual`) not identical to "COLUMN" (`expected`).
      
      `actual` is NULL
      `expected` is a character vector ('COLUMN')
      
      [ FAIL 30 | WARN 92 | SKIP 0 | PASS 107 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘FielDHub.Rmd’
      ...
    3  3   IBAGUE  103   1     ND-10
    4  4   IBAGUE  104   1     ND-13
    5  5   IBAGUE  105   1      ND-6
    6  6   IBAGUE  106   1     ND-14
    
    > plt <- plot(rcbd2)
    
      When sourcing ‘FielDHub.R’:
    Error: unused argument (dn)
    Execution halted
    
      ‘FielDHub.Rmd’ using ‘UTF-8’... failed
      ‘vignette.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘FielDHub.Rmd’ using rmarkdown
    
    Quitting from lines 40-44 [unnamed-chunk-2] (FielDHub.Rmd)
    Error: processing vignette 'FielDHub.Rmd' failed with diagnostics:
    unused argument (dn)
    --- failed re-building ‘FielDHub.Rmd’
    
    --- re-building ‘vignette.Rmd’ using rmarkdown
    ```

# agridat

<details>

* Version: 1.24
* GitHub: https://github.com/kwstat/agridat
* Source code: https://github.com/cran/agridat
* Date/Publication: 2024-10-27 17:00:02 UTC
* Number of recursive dependencies: 226

Run `revdepcheck::cloud_details(, "agridat")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘agridat-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: connolly.potato
    > ### Title: Potato yields in single-drill plots
    > ### Aliases: connolly.potato
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    ...
    > dat <- connolly.potato
    > 
    > # Field plan
    > libs(desplot)
    > desplot(dat, yield~col*row,
    +         out1=rep, # aspect unknown
    +         main="connolly.potato yields (reps not contiguous)")
    Error in is.element(x, dn) : unused argument (dn)
    Calls: desplot -> checkvars
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘agridat_graphical_gems.Rmd’
      ...
    
    > dat <- gomez.stripsplitplot
    
    > if (require("desplot")) {
    +     desplot(dat, gen ~ col * row, out1 = rep, col = nitro, text = planting, 
    +         cex = 1, main = "gomez.stripsplit ..." ... [TRUNCATED] 
    
      When sourcing ‘agridat_graphical_gems.R’:
    Error: unused argument (dn)
    Execution halted
    
      ‘agridat_data.Rmd’ using ‘UTF-8’... OK
      ‘agridat_graphical_gems.Rmd’ using ‘UTF-8’... failed
      ‘agridat_intro.Rmd’ using ‘UTF-8’... OK
      ‘agridat_mixed_model_example.Rmd’ using ‘UTF-8’... OK
      ‘agridat_uniformity_data.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘agridat_data.Rmd’ using rmarkdown
    --- finished re-building ‘agridat_data.Rmd’
    
    --- re-building ‘agridat_graphical_gems.Rmd’ using rmarkdown
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
* Number of recursive dependencies: 88

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
* Number of recursive dependencies: 51

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
      
      [ FAIL 5 | WARN 1 | SKIP 4 | PASS 143 ]
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
      installed size is 48.8Mb
      sub-directories of 1Mb or more:
        cereal   1.4Mb
        libs    47.2Mb
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# APCI

<details>

* Version: 1.0.8
* GitHub: NA
* Source code: https://github.com/cran/APCI
* Date/Publication: 2024-09-02 20:20:06 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::cloud_details(, "APCI")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘1_tests.R’
    Running the tests in ‘tests/1_tests.R’ failed.
    Complete output:
      > # install the package and use this script to test the package
      > library("APCI")
      > # or: remotes::install_github("jiahui1902/APCI")
      > test_data <- APCI::women9017
      > test_data$acc <- as.factor(test_data$acc)
      > test_data$pcc <- as.factor(test_data$pcc)
      > test_data$educc <- as.factor(test_data$educc)
    ...
       -0.330681588   0.124972726   0.151208824  -0.350044237   0.160268300 
          acc8:pcc3     acc9:pcc3     acc1:pcc4     acc2:pcc4     acc3:pcc4 
       -0.124023962   0.184278037  -0.415952378   0.038103374   0.173343774 
          acc4:pcc4     acc5:pcc4     acc6:pcc4     acc7:pcc4     acc8:pcc4 
        0.274042725   0.250469135  -0.098811727  -0.063004131  -0.355449149 
          acc9:pcc4     acc1:pcc5     acc2:pcc5     acc3:pcc5     acc4:pcc5 
        0.147098521   0.009963326   0.321744801  -0.080742516  -0.199308789 
          acc5:pcc5     acc6:pcc5     acc7:pcc5     acc8:pcc5     acc9:pcc5 
       -0.509084301  -0.328667274   0.352292245   0.698949422  -0.291285834 
      Killed
    ```

# APCtools

<details>

* Version: 1.0.4
* GitHub: https://github.com/bauer-alex/APCtools
* Source code: https://github.com/cran/APCtools
* Date/Publication: 2023-01-13 23:30:02 UTC
* Number of recursive dependencies: 114

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
      This is mgcv 1.8-42. For overview type 'help("mgcv-package")'.
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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘main_functionality.Rmd’
      ...
    
    > plot_densityMatrix(dat = travel, y_var = "mainTrip_distance", 
    +     age_groups = age_groups, period_groups = period_groups, log_scale = TRUE)
    Excluding 9149 missing observations of mainTrip_distance...
    
      When sourcing ‘main_functionality.R’:
    Error: The `facets` argument of `facet_grid()` was deprecated in ggplot2 2.2.0
    and is now defunct.
    ℹ Please use the `rows` argument instead.
    Execution halted
    
      ‘main_functionality.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
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
* Number of recursive dependencies: 116

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
      
      Attaching package: 'ggplot2'
      
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

# ARPALData

<details>

* Version: 1.6.1
* GitHub: NA
* Source code: https://github.com/cran/ARPALData
* Date/Publication: 2025-01-10 14:00:10 UTC
* Number of recursive dependencies: 140

Run `revdepcheck::cloud_details(, "ARPALData")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ARPALData-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: is_ARPALdf
    > ### Title: Check if a given object belongs to class 'ARPALdf'
    > ### Aliases: is_ARPALdf
    > 
    > ### ** Examples
    > 
    > d <- get_ARPA_Lombardia_AQ_registry()
    ...
    dbl  (5): IDSensor, IDStation, Altitude, Latitude, Longitude
    date (2): DateStart, DateStop
    
    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    > is_ARPALdf(d)
    Error in is.element("ARPALdf", attr(Data, "class")) : 
      unused argument (attr(Data, "class"))
    Calls: is_ARPALdf
    Execution halted
    ```

*   checking whether package ‘ARPALData’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element("try-error", ': unused argument (attr(Meteo2, "class")) 
      Note: possible error in 'is.element("ARPALdf", ': unused argument (attr(Data, "class")) 
      Note: possible error in 'is.element("ARPALdf_AQ", ': unused argument (attr(Data, "class")) 
      Note: possible error in 'is.element("ARPALdf_AQ_mun", ': unused argument (attr(Data, "class")) 
      Note: possible error in 'is.element("ARPALdf_W", ': unused argument (attr(Data, "class")) 
    See ‘/tmp/workdir/ARPALData/new/ARPALData.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    get_ARPA_Lombardia_W_data: possible error in is.element("try-error",
      attr(Meteo2, "class")): unused argument (attr(Meteo2, "class"))
    is_ARPALdf: possible error in is.element("ARPALdf", attr(Data,
      "class")): unused argument (attr(Data, "class"))
    is_ARPALdf_AQ: possible error in is.element("ARPALdf_AQ", attr(Data,
      "class")): unused argument (attr(Data, "class"))
    is_ARPALdf_AQ_mun: possible error in is.element("ARPALdf_AQ_mun",
      attr(Data, "class")): unused argument (attr(Data, "class"))
    is_ARPALdf_W: possible error in is.element("ARPALdf_W", attr(Data,
      "class")): unused argument (attr(Data, "class"))
    ```

# arulesViz

<details>

* Version: 1.5.3
* GitHub: https://github.com/mhahsler/arulesViz
* Source code: https://github.com/cran/arulesViz
* Date/Publication: 2024-04-26 09:20:02 UTC
* Number of recursive dependencies: 124

Run `revdepcheck::cloud_details(, "arulesViz")` for more info

</details>

## Newly broken

*   checking whether package ‘arulesViz’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘arules::is.element’ by ‘ggplot2::is.element’ when loading ‘arulesViz’
    See ‘/tmp/workdir/arulesViz/new/arulesViz.Rcheck/00install.out’ for details.
    ```

# asremlPlus

<details>

* Version: 4.4.43
* GitHub: https://github.com/briencj/asremlPlus
* Source code: https://github.com/cran/asremlPlus
* Date/Publication: 2024-12-10 08:30:01 UTC
* Number of recursive dependencies: 176

Run `revdepcheck::cloud_details(, "asremlPlus")` for more info

</details>

## Newly broken

*   checking whether package ‘asremlPlus’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(fixed.labels[[1]], ': unused argument (fixed.labels[[i]]) 
      Note: possible error in 'is.element(sparse.labels[[1]], ': unused argument (sparse.labels[[i]]) 
    See ‘/tmp/workdir/asremlPlus/new/asremlPlus.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    REMLRT.asreml: possible error in is.element(fixed.labels[[1]],
      fixed.labels[[i]]): unused argument (fixed.labels[[i]])
    REMLRT.asreml: possible error in is.element(sparse.labels[[1]],
      sparse.labels[[i]]): unused argument (sparse.labels[[i]])
    bootREMLRT.asreml: possible error in is.element(fixed.labels[[1]],
      fixed.labels[[i]]): unused argument (fixed.labels[[i]])
    bootREMLRT.asreml: possible error in is.element(sparse.labels[[1]],
      sparse.labels[[i]]): unused argument (sparse.labels[[i]])
    ```

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘asremlPlus-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: LSD.frame
    > ### Title: Description of an LSD frame
    > ### Aliases: LSD.frame
    > ### Keywords: asreml htest
    > 
    > ### ** Examples
    > 
    ...
    +   }
    Warning in check_dep_version() :
      ABI version mismatch: 
    lme4 was built with Matrix ABI version 1
    Current Matrix ABI version is 0
    Please re-install lme4 from source or restore original ‘Matrix’ package
    Error in initializePtr() : 
      function 'cholmod_factor_ldetA' not provided by package 'Matrix'
    Calls: <Anonymous> ... initialize -> <Anonymous> -> initializePtr -> .Call
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘asreml’
    ```

# autocogs

<details>

* Version: 0.1.4
* GitHub: https://github.com/schloerke/autocogs
* Source code: https://github.com/cran/autocogs
* Date/Publication: 2021-05-29 17:00:05 UTC
* Number of recursive dependencies: 73

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
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 228 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
        8.         └─autocogs (local) FUN(X[[i]], ...)
        9.           └─base::lapply(...)
       10.             └─autocogs (local) FUN(X[[i]], ...)
       11.               ├─base::do.call(fn, args)
       12.               └─autocogs (local) `<fn>`(...)
       13.                 └─base::do.call(loess, c(core_params, params$method.args))
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 228 ]
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
* Number of recursive dependencies: 87

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

# autoTS

<details>

* Version: 0.9.11
* GitHub: https://github.com/vivienroussez/autots
* Source code: https://github.com/cran/autoTS
* Date/Publication: 2020-06-05 12:20:06 UTC
* Number of recursive dependencies: 119

Run `revdepcheck::cloud_details(, "autoTS")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘autoTS_vignette.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘autoTS_vignette.Rmd’
      ...
    Warning in unzip(zipfile = "../inst/extdata/namq_10_gdp.zip", exdir = tmp_dir) :
      error 1 in extracting from zip file
    
    > dat <- read.csv(paste0(tmp_dir, "/namq_10_gdp_1_Data.csv"))
    Warning in file(file, "rt") :
      cannot open file '/tmp/RtmpJMEUNu/namq_10_gdp_1_Data.csv': No such file or directory
    
      When sourcing ‘autoTS_vignette.R’:
    Error: cannot open the connection
    Execution halted
    
      ‘autoTS_vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘plotly’ ‘shinycssloaders’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# backShift

<details>

* Version: 0.1.4.3
* GitHub: https://github.com/christinaheinze/backShift
* Source code: https://github.com/cran/backShift
* Date/Publication: 2020-05-06 11:30:03 UTC
* Number of recursive dependencies: 95

Run `revdepcheck::cloud_details(, "backShift")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘backShift-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: metricsThreshold
    > ### Title: Performance metrics for estimate of connectiviy matrix A.
    > ### Aliases: metricsThreshold
    > 
    > ### ** Examples
    > 
    > # true A
    ...
    > diag(A.est) <- 0
    > A.est[1,2] <- 0.76
    > A.est[2,3] <- -0.68
    > A.est[3,1] <- 0.83
    >  
    > # compute metrics with threshold 0.25
    > metricsThreshold(A, A.est, thres = 0.25)
    Error in is.element(-1, errs$diff) : unused argument (errs$diff)
    Calls: metricsThreshold -> unlist -> metrics
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘exampleBackShift.Rmd’
      ...
    
    > plotGraphEdgeAttr(estimate = Ahat.structure, plotStabSelec = TRUE, 
    +     labels = colnames(A), thres.point = thres.pe, edgeWeights = Ahat, 
    +     t .... [TRUNCATED] 
    
    > metricsThresholdedA <- metricsThreshold(A, Ahat, thres = thres.pe)
    
      When sourcing ‘exampleBackShift.R’:
    Error: unused argument (errs$diff)
    Execution halted
    
      ‘exampleBackShift.Rmd’ using ‘UTF-8’... failed
    ```

*   checking whether package ‘backShift’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(-1, errs$diff)': unused argument (errs$diff) 
      Note: possible error in 'is.element(1, errs$diff)': unused argument (errs$diff) 
    See ‘/tmp/workdir/backShift/new/backShift.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    metrics: possible error in is.element(-1, errs$diff): unused argument
      (errs$diff)
    metrics: possible error in is.element(1, errs$diff): unused argument
      (errs$diff)
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘exampleBackShift.Rmd’ using rmarkdown
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘InvariantCausalPrediction’, ‘CompareCausalNetworks’
    ```

# bartMan

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/bartMan
* Date/Publication: 2024-07-24 12:10:02 UTC
* Number of recursive dependencies: 135

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
* Number of recursive dependencies: 73

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
      [ FAIL 1 | WARN 4 | SKIP 0 | PASS 140 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-dists.R:34:3'): Success ──────────────────────────────────────
      plotNormalInvGamma(3, 1, 1, 1)$labels$y not equal to "sig_sq".
      target is NULL, current is character
      
      [ FAIL 1 | WARN 4 | SKIP 0 | PASS 140 ]
      Error: Test failures
      Execution halted
    ```

# bayesforecast

<details>

* Version: 1.0.1
* GitHub: NA
* Source code: https://github.com/cran/bayesforecast
* Date/Publication: 2021-06-17 10:00:02 UTC
* Number of recursive dependencies: 111

Run `revdepcheck::cloud_details(, "bayesforecast")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ARIMA.Rmd’
      ...
    ma       -0.4547 0.0033   -0.6145   -0.2713 1008.0532 1.0008
    sar      -0.2735 0.0023   -0.3902   -0.1508 1034.6555 0.9992
    sma      -0.3322 0.0027   -0.4979   -0.2091 1022.9639 1.0007
    loglik -443.0979 0.0588 -446.7919 -440.8711  935.8408 0.9999
    
    > mcmc_plot(object = sf1)
    
    ...
    [1] "df [  ] ~ gamma ( shape =  2 ,rate =  0.1 )"
    
    > mcmc_plot(sf1)
    
      When sourcing ‘GARCH.R’:
    Error: unused argument (code)
    Execution halted
    
      ‘ARIMA.Rmd’ using ‘UTF-8’... failed
      ‘GARCH.Rmd’ using ‘UTF-8’... failed
    ```

*   checking whether package ‘bayesforecast’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(NA, code)': unused argument (code) 
    See ‘/tmp/workdir/bayesforecast/new/bayesforecast.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    mcmc_plot.varstan: possible error in is.element(NA, code): unused
      argument (code)
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ARIMA.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 163.5Mb
      sub-directories of 1Mb or more:
        libs  162.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RcppParallel’ ‘StanHeaders’ ‘astsa’
      All declared Imports should be used.
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# BayesGrowth

<details>

* Version: 1.0.0
* GitHub: https://github.com/jonathansmart/BayesGrowth
* Source code: https://github.com/cran/BayesGrowth
* Date/Publication: 2023-11-21 18:10:08 UTC
* Number of recursive dependencies: 109

Run `revdepcheck::cloud_details(, "BayesGrowth")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘MCMC-example.Rmd’
      ...
    > ggplot(growth_curve, aes(Age, LAA)) + geom_point(data = example_data, 
    +     aes(Age, Length), alpha = 0.3) + geom_lineribbon(aes(ymin = .lower, 
    +  .... [TRUNCATED] 
    Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ℹ Please use `linewidth` instead.
    
      When sourcing ‘MCMC-example.R’:
    Error: unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 14, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, FALSE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, 
        NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "#4D4D4DFF", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, 
    Execution halted
    
      ‘MCMC-example.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘MCMC-example.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 82.3Mb
      sub-directories of 1Mb or more:
        data   1.5Mb
        libs  80.1Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# BayesianMCPMod

<details>

* Version: 1.0.1
* GitHub: https://github.com/Boehringer-Ingelheim/BayesianMCPMod
* Source code: https://github.com/cran/BayesianMCPMod
* Date/Publication: 2024-04-05 13:53:00 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::cloud_details(, "BayesianMCPMod")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘BayesianMCPMod-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: getBootstrapQuantiles
    > ### Title: getBootstrapQuantiles
    > ### Aliases: getBootstrapQuantiles
    > 
    > ### ** Examples
    > 
    > posterior_list <- list(Ctrl = RBesT::mixnorm(comp1 = c(w = 1, m = 0, s = 1), sigma = 2),
    ...
    +                        DG_4 = RBesT::mixnorm(comp1 = c(w = 1, m = 6.5, s = 1.1), sigma = 2))
    > models         <- c("exponential", "linear")
    > dose_levels    <- c(0, 1, 2, 4, 8)
    > fit            <- getModelFits(models      = models,
    +                                posterior   = posterior_list,
    +                                dose_levels = dose_levels,
    +                                simple      = TRUE)
    Error in is.element(modelNum, 1:4) : unused argument (1:4)
    Calls: getModelFits -> lapply -> FUN -> <Anonymous> -> fitMod.raw
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
      > # * https://r-pkgs.org/tests.html
    ...
       10. │                 └─testthat::source_file(path, env = env, chdir = chdir, wrap = wrap)
       11. │                   ├─base::withCallingHandlers(...)
       12. │                   └─base::eval(exprs, env)
       13. │                     └─base::eval(exprs, env)
       14. │                       └─DoseFinding::Mods(linear = NULL, doses = dose_levels) at tests/testthat/setup.R:106:1
       15. │                         └─DoseFinding:::fullMod(...)
       16. └─base::.handleSimpleError(...)
       17.   └─testthat (local) h(simpleError(msg, call))
       18.     └─rlang::abort(...)
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Simulation_Example.Rmd’
      ...
    > sigemax2 <- DoseFinding::guesst(d = c(2, 4), p = c(0.3, 
    +     0.8), model = "sigEmax")
    
    > mods <- DoseFinding::Mods(linear = NULL, emax = emax, 
    +     exponential = exp, sigEmax = rbind(sigemax, sigemax2), doses = dose_levels, 
    +     maxE .... [TRUNCATED] 
    
    ...
    > mods <- DoseFinding::Mods(linear = NULL, emax = emax_guesst, 
    +     exponential = exp_guesst, doses = dose_levels, maxEff = -1, 
    +     placEff = -12 .... [TRUNCATED] 
    
      When sourcing ‘analysis_normal.R’:
    Error: unused argument (c("emax", "quadratic", "exponential"))
    Execution halted
    
      ‘Simulation_Example.Rmd’ using ‘UTF-8’... failed
      ‘analysis_normal.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Simulation_Example.Rmd’ using rmarkdown
    
    Quitting from lines 93-124 [unnamed-chunk-2] (Simulation_Example.Rmd)
    Error: processing vignette 'Simulation_Example.Rmd' failed with diagnostics:
    unused argument (c("emax", "quadratic", "exponential"))
    --- failed re-building ‘Simulation_Example.Rmd’
    
    --- re-building ‘analysis_normal.Rmd’ using rmarkdown
    ...
    Quitting from lines 130-148 [Pre-Specification of candidate models] (analysis_normal.Rmd)
    Error: processing vignette 'analysis_normal.Rmd' failed with diagnostics:
    unused argument (c("emax", "quadratic", "exponential"))
    --- failed re-building ‘analysis_normal.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘Simulation_Example.Rmd’ ‘analysis_normal.Rmd’
    
    Error: Vignette re-building failed.
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
      
      Attaching package: 'ggplot2'
      
    ...
      `names(expected)` is absent
      ── Failure ('test-PPV_heatmap.R:1097:3'): Plot with line overlay ───────────────
      vapply(p$result$layers, function(x) class(x$geom)[1], "") (`actual`) not identical to c("GeomTile", "GeomSegment", "GeomPoint", "GeomMarkRect") (`expected`).
      
      `names(actual)` is a character vector ('geom_tile', 'annotate', 'annotate...3', 'geom_mark_rect')
      `names(expected)` is absent
      
      [ FAIL 8 | WARN 56 | SKIP 4 | PASS 115 ]
      Error: Test failures
      Execution halted
    ```

# BayesMallows

<details>

* Version: 2.2.3
* GitHub: https://github.com/ocbe-uio/BayesMallows
* Source code: https://github.com/cran/BayesMallows
* Date/Publication: 2025-01-14 11:30:02 UTC
* Number of recursive dependencies: 81

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
      installed size is 23.6Mb
      sub-directories of 1Mb or more:
        doc    2.7Mb
        libs  20.0Mb
    ```

# bayesplot

<details>

* Version: 1.11.1
* GitHub: https://github.com/stan-dev/bayesplot
* Source code: https://github.com/cran/bayesplot
* Date/Publication: 2024-02-15 05:30:11 UTC
* Number of recursive dependencies: 128

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
      This is bayesplot version 1.11.1
      - Online documentation and vignettes at mc-stan.org/bayesplot
      - bayesplot theme set to bayesplot::theme_default()
         * Does _not_ affect other ggplot2 plots
         * See ?bayesplot_theme_set for details on theme setting
    ...
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-mcmc-traces.R:55:3'): mcmc_trace options work ────────────────
      all(c("xmin", "xmax", "ymin", "ymax") %in% names(ll)) is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      
      [ FAIL 1 | WARN 1 | SKIP 73 | PASS 1024 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘visual-mcmc-diagnostics.Rmd’
      ...
    
    > schools_dat <- list(J = 8, y = c(28, 8, -3, 7, -1, 
    +     1, 18, 12), sigma = c(15, 10, 16, 11, 9, 11, 10, 18))
    
    > fit_cp <- sampling(schools_mod_cp, data = schools_dat, 
    +     seed = 803214055, control = list(adapt_delta = 0.9))
    
      When sourcing ‘visual-mcmc-diagnostics.R’:
    Error: error in evaluating the argument 'object' in selecting a method for function 'sampling': object 'schools_mod_cp' not found
    Execution halted
    
      ‘graphical-ppcs.Rmd’ using ‘UTF-8’... OK
      ‘plotting-mcmc-draws.Rmd’ using ‘UTF-8’... OK
      ‘visual-mcmc-diagnostics.Rmd’ using ‘UTF-8’... failed
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.6Mb
      sub-directories of 1Mb or more:
        R     4.0Mb
        doc   3.8Mb
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
    
    > ### Name: bcea
    > ### Title: Create Bayesian Cost-Effectiveness Analysis Object
    > ### Aliases: bcea bcea.default bcea.rjags bcea.rstan bcea.bugs
    > ### Keywords: manip
    > 
    > ### ** Examples
    > 
    ...
    +                             #  with each intervention
    +       Kmax=50000,           # maximum value possible for the willingness 
    +                             #  to pay threshold; implies that k is chosen 
    +                             #  in a grid from the interval (0, Kmax)
    +       plot=TRUE             # plots the results
    + )
    Error in is.element("point", names(extra_args)) : 
      unused argument (names(extra_args))
    Calls: bcea -> bcea.default -> plot -> plot.bcea
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(BCEA)
      Registered S3 method overwritten by 'BCEA':
        method     from
        plot.evppi voi 
      The BCEA version loaded is: 2.4.7
      
    ...
        6. │   │     ├─withr::with_output_sink(path, withVisible(code))
        7. │   │     │ └─base::force(code)
        8. │   │     └─base::withVisible(code)
        9. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
       10. ├─base::summary(smoke_bcea)
       11. └─BCEA:::summary.bcea(smoke_bcea)
      
      [ FAIL 3 | WARN 0 | SKIP 3 | PASS 89 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘bcea.Rmd’
      ...
        is.element
    
    
    > library(purrr)
    
    > plot(bcea_smoke)
    
    ...
    
      ‘CEriskav.Rmd’ using ‘UTF-8’... OK
      ‘bcea.Rmd’ using ‘UTF-8’... failed
      ‘ceac.Rmd’ using ‘UTF-8’... OK
      ‘ceef.Rmd’ using ‘UTF-8’... OK
      ‘ceplane.Rmd’ using ‘UTF-8’... OK
      ‘contour.Rmd’ using ‘UTF-8’... OK
      ‘eib.Rmd’ using ‘UTF-8’... OK
      ‘paired_vs_multiple_comps.Rmd’ using ‘UTF-8’... OK
      ‘Set_bcea_parameters.Rmd’ using ‘UTF-8’... OK
    ```

*   checking whether package ‘BCEA’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(wtp, he$k)': unused argument (he$k) 
      Note: possible error in 'is.element("point", names(extra_args))': unused argument (names(extra_args)) 
      Note: possible error in 'is.element("color", names(extra_args$point))': unused argument (names(extra_args$point)) 
      Note: possible error in 'is.element(names(Table), ': unused argument (paste0("U", c(he$ref, he$comp))) 
    See ‘/tmp/workdir/BCEA/new/BCEA.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ib_plot_base: possible error in is.element(wtp, he$k): unused argument
      (he$k)
    ib_plot_ggplot: possible error in is.element(wtp, he$k): unused
      argument (he$k)
    plot.bcea: possible error in is.element("point", names(extra_args)):
      unused argument (names(extra_args))
    plot.bcea: possible error in is.element("color",
      names(extra_args$point)): unused argument (names(extra_args$point))
    sim_table.bcea: possible error in is.element(wtp, he$k): unused
      argument (he$k)
    summary.bcea: possible error in is.element(wtp, he$k): unused argument
      (he$k)
    summary.bcea: possible error in is.element(names(Table), paste0("U",
      c(he$ref, he$comp))): unused argument (paste0("U", c(he$ref,
      he$comp)))
    summary.pairwise: possible error in is.element(wtp, he$k): unused
      argument (he$k)
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘CEriskav.Rmd’ using rmarkdown
    ```

# BEAMR

<details>

* Version: 1.1.0
* GitHub: https://github.com/annaSeffernick/BEAMR
* Source code: https://github.com/cran/BEAMR
* Date/Publication: 2024-07-27 16:00:06 UTC
* Number of recursive dependencies: 152

Run `revdepcheck::cloud_details(, "BEAMR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘BEAMR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: check_beam_specs
    > ### Title: Check that beam.specs satisfies all necessary conditions
    > ### Aliases: check_beam_specs
    > 
    > ### ** Examples
    > 
    > data(beam_dat)
    > data(beam_specs)
    > test_specs <- check_beam_specs(beam_specs, names(beam_dat$mtx.data))
    Error in is.element(beam.specs[, "mtx"], mtx.names) : 
      unused argument (mtx.names)
    Calls: check_beam_specs
    Execution halted
    ```

*   checking whether package ‘BEAMR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(beam.specs[, ': unused argument (mtx.names) 
      Note: possible error in 'is.element("name", colnames(beam.specs))': unused argument (colnames(beam.specs)) 
      Note: possible error in 'is.element(cls, required.class)': unused argument (required.class) 
      Note: possible error in 'is.element(obj.cls, "list")': unused argument ("list") 
      Note: possible error in 'is.element(set.data$mtx.id, ': unused argument (specs.mtx) 
      Note: possible error in 'is.element(colnames(set.anns), ': unused argument ("set.id") 
      Note: possible error in 'is.element(c("set.id", ': unused argument (colnames(set.data)) 
      Note: possible error in 'is.element(set.index$mtx.id[i], ': unused argument (mtx.names) 
      Note: possible error in 'is.element(set.data$row.id[row.index], ': unused argument (row.ids) 
    See ‘/tmp/workdir/BEAMR/new/BEAMR.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    check_beam_specs: possible error in is.element(beam.specs[, "mtx"],
      mtx.names): unused argument (mtx.names)
    check_beam_specs: possible error in is.element("name",
      colnames(beam.specs)): unused argument (colnames(beam.specs))
    check_list_class: possible error in is.element(cls, required.class):
      unused argument (required.class)
    check_list_class: possible error in is.element(obj.cls, "list"): unused
      argument ("list")
    compute_set_pvalues: possible error in is.element(set.data$mtx.id,
      specs.mtx): unused argument (specs.mtx)
    prep_beam_data: possible error in is.element(colnames(set.anns),
      "set.id"): unused argument ("set.id")
    prep_beam_data: possible error in is.element(c("set.id", "mtx.id",
      "row.id"), colnames(set.data)): unused argument (colnames(set.data))
    prep_beam_data: possible error in is.element(set.index$mtx.id[i],
      mtx.names): unused argument (mtx.names)
    prep_beam_data: possible error in
      is.element(set.data$row.id[row.index], row.ids): unused argument
      (row.ids)
    ```

# beastt

<details>

* Version: 0.0.1
* GitHub: https://github.com/GSK-Biostatistics/beastt
* Source code: https://github.com/cran/beastt
* Date/Publication: 2024-06-20 15:50:16 UTC
* Number of recursive dependencies: 99

Run `revdepcheck::cloud_details(, "beastt")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘beastt-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_dist
    > ### Title: Plot Distribution
    > ### Aliases: plot_dist
    > 
    > ### ** Examples
    > 
    > library(distributional)
    ...
     12. │     └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13. │       └─l$compute_geom_2(d, theme = plot$theme)
     14. │         └─ggplot2 (local) compute_geom_2(..., self = self)
     15. │           └─self$geom$use_defaults(...)
     16. └─base::.handleSimpleError(...)
     17.   └─rlang (local) h(simpleError(msg, call))
     18.     └─handlers[[1L]](cnd)
     19.       └─cli::cli_abort(...)
     20.         └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘binary.Rmd’
      ...
    
    > plot_dist(pwr_prior)
    
      When sourcing ‘binary.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `use_defaults()`:
    ...
      When sourcing ‘continuous.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `use_defaults()`:
    ! unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, 
        NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "#4D4D4DFF", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 
    Execution halted
    
      ‘binary.Rmd’ using ‘UTF-8’... failed
      ‘continuous.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘binary.Rmd’ using rmarkdown
    ```

# biclust

<details>

* Version: 2.0.3.1
* GitHub: NA
* Source code: https://github.com/cran/biclust
* Date/Publication: 2023-05-19 07:18:34 UTC
* Number of recursive dependencies: 41

Run `revdepcheck::cloud_details(, "biclust")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘biclust-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: BCPlaid
    > ### Title: The Plaid Model Bicluster algorithm
    > ### Aliases: BCPlaid plaid 'plaid model' BCPlaid BCPlaid-class 'turner
    > ###   biclustering' biclust,matrix,BCPlaid-method
    > ### Keywords: cluster classif models
    > 
    > ### ** Examples
    > 
    >   #Random matrix with embedded bicluster
    >   test <- matrix(rnorm(5000),100,50)
    >   test[11:20,11:20] <- rnorm(100,3,0.3)
    >   res<-biclust(test, method=BCPlaid())
    Error in is.element("a", model) : unused argument (model)
    Calls: biclust -> biclust -> <Anonymous> -> plaid -> fitLayer
    Execution halted
    ```

*   checking whether package ‘biclust’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element("a", model)': unused argument (model) 
      Note: possible error in 'is.element("b", model)': unused argument (model) 
      Note: possible error in 'is.element("c", model)': unused argument (model) 
      Note: possible error in 'is.element("a", fit.model)': unused argument (fit.model) 
      Note: possible error in 'is.element("b", fit.model)': unused argument (fit.model) 
      Note: possible error in 'is.element("c", fit.model)': unused argument (fit.model) 
      Note: possible error in 'is.element(cluster, c("r", ': unused argument (c("r", "b")) 
      Note: possible error in 'is.element(cluster, c("c", ': unused argument (c("c", "b")) 
    See ‘/tmp/workdir/biclust/new/biclust.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    fitLayer: possible error in is.element("a", model): unused argument
      (model)
    fitLayer: possible error in is.element("b", model): unused argument
      (model)
    fitLayer: possible error in is.element("c", model): unused argument
      (model)
    plaid: possible error in is.element("a", fit.model): unused argument
      (fit.model)
    plaid: possible error in is.element("b", fit.model): unused argument
      (fit.model)
    ...
    updatePlaid: possible error in is.element(cluster, c("r", "b")): unused
      argument (c("r", "b"))
    updatePlaid: possible error in is.element(cluster, c("c", "b")): unused
      argument (c("c", "b"))
    updatePlaid: possible error in is.element("a", model): unused argument
      (model)
    updatePlaid: possible error in is.element("b", model): unused argument
      (model)
    updatePlaid: possible error in is.element("c", model): unused argument
      (model)
    ```

# biclustermd

<details>

* Version: 0.2.3
* GitHub: https://github.com/jreisner/biclustermd
* Source code: https://github.com/cran/biclustermd
* Date/Publication: 2021-06-17 15:10:06 UTC
* Number of recursive dependencies: 83

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
      
      Attaching package: 'ggplot2'
      
      The following object is masked from 'package:base':
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

# biodosetools

<details>

* Version: 3.6.1
* GitHub: https://github.com/biodosetools-team/biodosetools
* Source code: https://github.com/cran/biodosetools
* Date/Publication: 2022-11-16 16:00:02 UTC
* Number of recursive dependencies: 118

Run `revdepcheck::cloud_details(, "biodosetools")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(biodosetools)
      > 
      > test_check("biodosetools")
      ! Problem with `glm()` -> constraint ML optimization will be used instead
      ! Problem with `glm()` -> constraint ML optimization will be used instead
      number of iterations= 43 
    ...
          actual                 | expected                  
      [2] "Estimation"           | "Estimation"           [2]
      [3] "Dose (Gy)"            | "Dose (Gy)"            [3]
      [4] "Translocations/cells" | "Translocations/cells" [4]
                                 - "yield_low"            [5]
                                 - "yield_upp"            [6]
      
      [ FAIL 4 | WARN 0 | SKIP 1 | PASS 232 ]
      Error: Test failures
      Execution halted
    ```

# boxly

<details>

* Version: 0.1.1
* GitHub: https://github.com/Merck/boxly
* Source code: https://github.com/cran/boxly
* Date/Publication: 2023-10-24 02:40:02 UTC
* Number of recursive dependencies: 90

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
* Number of recursive dependencies: 103

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
    ...
    
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

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘exploratory-modelling.Rmd’
      ...
    Warning in is.na(non_null_default_aes[[aes_param_name]]) :
      is.na() applied to non-(list or vector) of type 'language'
    
      When sourcing ‘exploratory-modelling.R’:
    Error: ℹ In index: 1.
    ℹ With name: geom_line.
    Caused by error in `aes_param_name %in% names(non_null_default_aes) && is.na(non_null_default_aes[[
    ...
    Error: argument is of length zero
    Execution halted
    
      ‘exploratory-modelling.Rmd’ using ‘UTF-8’... failed
      ‘finding-features.Rmd’ using ‘UTF-8’... failed
      ‘getting-started.Rmd’ using ‘UTF-8’... failed
      ‘id-interesting-obs.Rmd’ using ‘UTF-8’... OK
      ‘longitudinal-data-structures.Rmd’ using ‘UTF-8’... OK
      ‘mixed-effects-models.Rmd’ using ‘UTF-8’... failed
      ‘visualisation-gallery.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘exploratory-modelling.Rmd’ using rmarkdown
    
    Quitting from lines 47-56 [use-gg-highlight] (exploratory-modelling.Rmd)
    Error: processing vignette 'exploratory-modelling.Rmd' failed with diagnostics:
    ℹ In index: 1.
    ℹ With name: geom_line.
    Caused by error in `aes_param_name %in% names(non_null_default_aes) && is.na(non_null_default_aes[[
        aes_param_name]])`:
    ! 'length = 2' in coercion to 'logical(1)'
    --- failed re-building ‘exploratory-modelling.Rmd’
    
    --- re-building ‘finding-features.Rmd’ using rmarkdown
    ```

# cartograflow

<details>

* Version: 1.0.5
* GitHub: https://github.com/fbahoken/cartogRaflow
* Source code: https://github.com/cran/cartograflow
* Date/Publication: 2023-10-17 22:40:21 UTC
* Number of recursive dependencies: 101

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
* Number of recursive dependencies: 83

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

# CausalImpact

<details>

* Version: 1.3.0
* GitHub: NA
* Source code: https://github.com/cran/CausalImpact
* Date/Publication: 2022-11-09 08:40:40 UTC
* Number of recursive dependencies: 76

Run `revdepcheck::cloud_details(, "CausalImpact")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘CausalImpact-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: CausalImpact
    > ### Title: Inferring causal impact using structural time-series models
    > ### Aliases: CausalImpact
    > 
    > ### ** Examples
    > 
    > # Example 1
    ...
    > y <- 1.2 * x1 + rnorm(52)
    > y[41:52] <- y[41:52] + 10
    > data <- cbind(y, x1)
    > pre.period <- c(1, 40)
    > post.period <- c(41, 52)
    > impact <- CausalImpact(data, pre.period, post.period)
    Error in is.element(letter, c("B", "M", "K")) : 
      unused argument (c("B", "M", "K"))
    Calls: CausalImpact ... InterpretSummaryTable -> IdentifyNumberAbbreviation
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘CausalImpact_import_test.R’
    Running the tests in ‘tests/CausalImpact_import_test.R’ failed.
    Complete output:
      > # Copyright 2014-2022 Google Inc. All rights reserved.
      > #
      > # Licensed under the Apache License, Version 2.0 (the "License");
      > # you may not use this file except in compliance with the License.
      > # You may obtain a copy of the License at
      > #
      > # http://www.apache.org/licenses/LICENSE-2.0
    ...
      > y <- 1.2 * x1 + rnorm(100)
      > y[71:100] <- y[71:100] + 10
      > data <- cbind(y, x1)
      > 
      > # Test that a `CausalImpact` object is created when the package is not loaded.
      > impact <- CausalImpact::CausalImpact(data, c(1, 70), c(71, 100))
      Error in is.element(letter, c("B", "M", "K")) : 
        unused argument (c("B", "M", "K"))
      Calls: <Anonymous> ... InterpretSummaryTable -> IdentifyNumberAbbreviation
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘CausalImpact.Rmd’
      ...
    
    > pre.period <- c(1, 70)
    
    > post.period <- c(71, 100)
    
    > impact <- CausalImpact(data, pre.period, post.period)
    
      When sourcing ‘CausalImpact.R’:
    Error: unused argument (c("B", "M", "K"))
    Execution halted
    
      ‘CausalImpact.Rmd’ using ‘UTF-8’... failed
    ```

*   checking whether package ‘CausalImpact’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(letter, c("B", ': unused argument (c("B", "M", "K")) 
    See ‘/tmp/workdir/CausalImpact/new/CausalImpact.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    IdentifyNumberAbbreviation: possible error in is.element(letter, c("B",
      "M", "K")): unused argument (c("B", "M", "K"))
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘CausalImpact.Rmd’ using rmarkdown
    ```

# celltrackR

<details>

* Version: 1.2.1
* GitHub: NA
* Source code: https://github.com/cran/celltrackR
* Date/Publication: 2024-08-26 12:20:02 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::cloud_details(, "celltrackR")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘QC.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘clustering.Rmd’
      ...
    +     squareDisplacement, maxDisplacement, outreachRatio), method = "MDS", 
    +     labels = r .... [TRUNCATED] 
    
    > trackFeatureMap(all.tracks, c(speed, meanTurningAngle, 
    +     squareDisplacement, maxDisplacement, outreachRatio), method = "UMAP", 
    +     labels =  .... [TRUNCATED] 
    
    ...
      When sourcing ‘data-QC.R’:
    Error: unused argument (names(TCells))
    Execution halted
    
      ‘QC.Rmd’ using ‘UTF-8’... OK
      ‘ana-methods.Rmd’ using ‘UTF-8’... OK
      ‘clustering.Rmd’ using ‘UTF-8’... failed
      ‘data-QC.Rmd’ using ‘UTF-8’... failed
      ‘reading-converting-data.Rmd’ using ‘UTF-8’... OK
      ‘simulation.Rmd’ using ‘UTF-8’... OK
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        doc   3.6Mb
    ```

# cellularautomata

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/cellularautomata
* Date/Publication: 2024-11-20 19:10:06 UTC
* Number of recursive dependencies: 72

Run `revdepcheck::cloud_details(, "cellularautomata")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘basic-usage.Rmd’
      ...
    
    > plot(ca(45))
    
    > plot(ca(195))
    
    > plot(ca(30, ncols = 20, steps = 30), animate = TRUE)
    
      When sourcing ‘basic-usage.R’:
    Error: argument "theme" is missing, with no default
    Execution halted
    
      ‘basic-usage.Rmd’ using ‘UTF-8’... failed
    ```

# CensMFM

<details>

* Version: 3.1
* GitHub: NA
* Source code: https://github.com/cran/CensMFM
* Date/Publication: 2024-05-14 07:33:36 UTC
* Number of recursive dependencies: 41

Run `revdepcheck::cloud_details(, "CensMFM")` for more info

</details>

## Newly broken

*   checking whether package ‘CensMFM’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(NA, y)': unused argument (y) 
    See ‘/tmp/workdir/CensMFM/new/CensMFM.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    fit.EM.MSNC: possible error in is.element(NA, y): unused argument (y)
    ```

# CFO

<details>

* Version: 2.2.0
* GitHub: NA
* Source code: https://github.com/cran/CFO
* Date/Publication: 2024-11-15 12:00:09 UTC
* Number of recursive dependencies: 34

Run `revdepcheck::cloud_details(, "CFO")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘CFO-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: CFO.oc
    > ### Title: Generate operating characteristics of phase I trials single-drug
    > ###   trials in multiple simulations
    > ### Aliases: CFO.oc
    > 
    > ### ** Examples
    > 
    ...
    Percentage of patients allocated to the MTD: 0.340 
    Percentage of selecting a dose above the MTD: 0.200  
    Percentage of allocating patients at dose levels above the MTD: 0.240  
    Percentage of the patients suffering DLT: 0.227  
    Average trial duration: 17.9  
    > plot(faCFOoc)
    Error in is.element(strpattern, c("none", names(objectPlot))) : 
      unused argument (c("none", names(objectPlot)))
    Calls: plot -> plot.cfo
    Execution halted
    ```

*   checking whether package ‘CFO’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(strpattern, ': unused argument (c("none", names(objectPlot))) 
    See ‘/tmp/workdir/CFO/new/CFO.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    plot.cfo: possible error in is.element(strpattern, c("none",
      names(objectPlot))): unused argument (c("none", names(objectPlot)))
    ```

# changepoint.geo

<details>

* Version: 1.0.2
* GitHub: https://github.com/grundy95/changepoint.geo
* Source code: https://github.com/cran/changepoint.geo
* Date/Publication: 2023-09-23 22:50:09 UTC
* Number of recursive dependencies: 50

Run `revdepcheck::cloud_details(, "changepoint.geo")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(changepoint.geo)
      Loading required package: changepoint
      Loading required package: zoo
      
      Attaching package: 'zoo'
      
    ...
      Angle Changepoints	:  60 90 
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 152 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test-geomcp.R:74:2'): (code run outside of `test_that()`) ───────────
      Error in `is.element(NA, data[[d]])`: unused argument (data[[d]])
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 152 ]
      Error: Test failures
      Execution halted
    ```

# cheem

<details>

* Version: 0.4.0.0
* GitHub: https://github.com/nspyrison/cheem
* Source code: https://github.com/cran/cheem
* Date/Publication: 2023-11-08 21:30:02 UTC
* Number of recursive dependencies: 151

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

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘getting-started-with-cheem.Rmd’
      ...
    
    > knitr::opts_chunk$set(echo = TRUE, include = TRUE, 
    +     results = "show", eval = FALSE, message = FALSE, warning = FALSE, 
    +     error = FALSE, co .... [TRUNCATED] 
    
    > knitr::include_graphics("../inst/shiny_apps/cheem/www/lime_nonlinear.png")
    
      When sourcing ‘getting-started-with-cheem.R’:
    Error: Cannot find the file(s): "../inst/shiny_apps/cheem/www/lime_nonlinear.png"
    Execution halted
    
      ‘getting-started-with-cheem.Rmd’ using ‘UTF-8’... failed
    ```

# chillR

<details>

* Version: 0.76
* GitHub: NA
* Source code: https://github.com/cran/chillR
* Date/Publication: 2024-11-14 09:40:02 UTC
* Number of recursive dependencies: 137

Run `revdepcheck::cloud_details(, "chillR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘chillR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: interpolate_gaps_hourly
    > ### Title: Interpolate gaps in hourly temperature records
    > ### Aliases: interpolate_gaps_hourly
    > ### Keywords: utility
    > 
    > ### ** Examples
    > 
    > 
    > 
    > Winters_gaps<-make_JDay(Winters_hours_gaps[1:2000,])
    Error in is.element(c("Day", "Month", "Year"), colnames(dateframe)) : 
      unused argument (colnames(dateframe))
    Calls: make_JDay
    Execution halted
    ```

*   checking whether package ‘chillR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(c("Day", "Month", ': unused argument (colnames(dateframe)) 
    See ‘/tmp/workdir/chillR/new/chillR.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    make_JDay: possible error in is.element(c("Day", "Month", "Year"),
      colnames(dateframe)): unused argument (colnames(dateframe))
    ```

# chronicle

<details>

* Version: 0.3
* GitHub: NA
* Source code: https://github.com/cran/chronicle
* Date/Publication: 2021-06-25 05:00:02 UTC
* Number of recursive dependencies: 145

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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘chronicle.Rmd’
      ...
    +     filename = "quick_demo", title = "A quick chronicle demo", 
    +     author =  .... [TRUNCATED] 
    
    Quitting from lines 34-46 [unnamed-chunk-3] (quick_demo.Rmd)
    
      When sourcing ‘chronicle.R’:
    Error: ℹ In index: 1.
    Caused by error in `pm[[2]]`:
    ! subscript out of bounds
    Execution halted
    
      ‘chronicle.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘chronicle.Rmd’ using rmarkdown
    
    Quitting from lines 38-67 [unnamed-chunk-3] (chronicle.Rmd)
    Error: processing vignette 'chronicle.Rmd' failed with diagnostics:
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

# cjoint

<details>

* Version: 2.1.1
* GitHub: NA
* Source code: https://github.com/cran/cjoint
* Date/Publication: 2023-08-22 07:50:07 UTC
* Number of recursive dependencies: 85

Run `revdepcheck::cloud_details(, "cjoint")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘cjoint-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: japan2014conjoint
    > ### Title: Japan 2014 Conjoint Experiment Dataset from Horiuchi et. al.
    > ###   (2014)
    > ### Aliases: japan2014conjoint
    > ### Keywords: datasets
    > 
    > ### ** Examples
    ...
    > # Run AMCE estimator using all attributes and uniform design
    > results <- amce(selected ~ `Consumption tax` + `Employment` + `Monetary and fiscal policy` + 
    +                   `Economic growth strategy` + `Nuclear power` + `TPP` + 
    +                   `Collective self-defense` + `Constitutional revision` + 
    +                   `National assembly seat reduction`,  data=japan2014conjoint, cluster=TRUE, 
    +                   respondent.id="respondentIndex", weights="wgt", design="uniform")
    Error in is.element(full_terms, orig_effects) : 
      unused argument (orig_effects)
    Calls: amce
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(cjoint)
      Loading required package: sandwich
      Loading required package: lmtest
      Loading required package: zoo
      
      Attaching package: 'zoo'
    ...
       2.   └─cjoint::amce(...) at test-main.r:16:3
      ── Error ('test-plot.R:3:1'): (code run outside of `test_that()`) ──────────────
      Error in `is.element(full_terms, orig_effects)`: unused argument (orig_effects)
      Backtrace:
          ▆
       1. └─cjoint::amce(...) at test-plot.R:3:1
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

*   checking whether package ‘cjoint’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(full_terms, ': unused argument (orig_effects) 
      Note: possible error in 'is.element(unique_vars, ': unused argument (respondent_vars) 
      Note: possible error in 'is.element(y, respondent_vars)': unused argument (respondent_vars) 
      Note: possible error in 'is.element(all_prof, ': unused argument (all_run_vars) 
      Note: possible error in 'is.element(substrings, ': unused argument (y) 
      Note: possible error in 'is.element(all_depends, ': unused argument (profile_effects[i]) 
      Note: possible error in 'is.element(substrings_d, ': unused argument (substrings) 
      Note: possible error in 'is.element(all_depends, ': unused argument (resp_effects[i]) 
      Note: possible error in 'is.element(respondent_vars, ': unused argument (substrings) 
    ...
      Note: possible error in 'is.element(facet.names, ': unused argument (amce_obj$respondent.varying) 
      Note: possible error in 'is.element(raw_attributes, ': unused argument (attr_remove) 
      Note: possible error in 'is.element(subs, names(amce_obj$estimates))': unused argument (names(amce_obj$estimates)) 
      Note: possible error in 'is.element(facet.name, ': unused argument (names(amce_obj$estimates)) 
      Note: possible error in 'is.element(all.vars, ': unused argument (this.var) 
      Note: possible error in 'is.element(subs, all_prof)': unused argument (all_prof) 
    See ‘/tmp/workdir/cjoint/new/cjoint.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    amce: possible error in is.element(full_terms, orig_effects): unused
      argument (orig_effects)
    amce: possible error in is.element(unique_vars, respondent_vars):
      unused argument (respondent_vars)
    amce : <anonymous>: possible error in is.element(y, respondent_vars):
      unused argument (respondent_vars)
    amce: possible error in is.element(all_prof, all_run_vars): unused
      argument (all_run_vars)
    amce : <anonymous>: possible error in is.element(substrings, y): unused
      argument (y)
    ...
    plot.amce : <anonymous>: possible error in is.element(subs,
      names(amce_obj$estimates)): unused argument
      (names(amce_obj$estimates))
    plot.amce: possible error in is.element(facet.name,
      names(amce_obj$estimates)): unused argument
      (names(amce_obj$estimates))
    print.summary.amce: possible error in is.element(all.vars, this.var):
      unused argument (this.var)
    summary.amce : <anonymous>: possible error in is.element(subs,
      all_prof): unused argument (all_prof)
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 29 marked UTF-8 strings
    ```

# clickstream

<details>

* Version: 1.3.3
* GitHub: NA
* Source code: https://github.com/cran/clickstream
* Date/Publication: 2023-09-27 14:50:02 UTC
* Number of recursive dependencies: 43

Run `revdepcheck::cloud_details(, "clickstream")` for more info

</details>

## Newly broken

*   checking whether package ‘clickstream’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘arules::is.element’ by ‘ggplot2::is.element’ when loading ‘clickstream’
    See ‘/tmp/workdir/clickstream/new/clickstream.Rcheck/00install.out’ for details.
    ```

# clinDataReview

<details>

* Version: 1.6.1
* GitHub: https://github.com/openanalytics/clinDataReview
* Source code: https://github.com/cran/clinDataReview
* Date/Publication: 2024-06-18 09:10:05 UTC
* Number of recursive dependencies: 120

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
        adding: report_dependencies16a96c1c4532/ (stored 0%)
        adding: report_dependencies16a96c1c4532/file16a9248230f.html (deflated 8%)
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

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘clinDataReview-dataPreprocessing.Rmd’ using rmarkdown
    --- finished re-building ‘clinDataReview-dataPreprocessing.Rmd’
    
    --- re-building ‘clinDataReview-dataVisualization.Rmd’ using rmarkdown
    
    Quitting from lines 167-208 [timeProfiles] (clinDataReview-dataVisualization.Rmd)
    Error: processing vignette 'clinDataReview-dataVisualization.Rmd' failed with diagnostics:
    subscript out of bounds
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
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        doc   4.3Mb
    ```

# clinDR

<details>

* Version: 2.4.1
* GitHub: NA
* Source code: https://github.com/cran/clinDR
* Date/Publication: 2023-08-09 04:20:05 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "clinDR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘clinDR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: "Extract.emaxsim"
    > ### Title: Extract a simulation from the output of emaxsim
    > ### Aliases: [.emaxsim
    > ### Keywords: nonlinear
    > 
    > ### ** Examples
    > 
    ...
    > meanlev<-emaxfun(doselev,pop)  
    > 
    > ###FixedMean is specialized constructor function for emaxsim
    > gen.parm<-FixedMean(n,doselev,meanlev,sdy)  
    > 
    > D1 <- emaxsim(nsim=2,gen.parm,modType=3,nproc=1)
    Error in is.element(x, c("emax", "quadratic", "exponential")) : 
      unused argument (c("emax", "quadratic", "exponential"))
    Calls: emaxsim -> Mods -> fullMod -> modCount -> lapply -> FUN
    Execution halted
    ```

# clinUtils

<details>

* Version: 0.2.0
* GitHub: https://github.com/openanalytics/clinUtils
* Source code: https://github.com/cran/clinUtils
* Date/Publication: 2024-05-17 14:50:06 UTC
* Number of recursive dependencies: 110

Run `revdepcheck::cloud_details(, "clinUtils")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘clinUtils-vignette.Rmd’
      ...
    
        layout
    
    
    > listPlotsInteractiveLB <- sapply(listPlotsLB, function(ggplot) ggplotly(ggplot) %>% 
    +     partial_bundle(), simplify = FALSE)
    
      When sourcing ‘clinUtils-vignette.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘clinUtils-vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘clinUtils-vignette.Rmd’ using rmarkdown
    ```

## Newly fixed

*   checking running R code from vignettes ... WARNING
    ```
    Errors in running code in vignettes:
    when running code in ‘clinUtils-vignette.Rmd’
      ...
    
    
    
    
    
    Quitting from lines 2-4 [lab-hist-interactive1]
    
      When sourcing ‘clinUtils-vignette.R’:
    Error: there is no package called 'webshot'
    Execution halted
    
      ‘clinUtils-vignette.Rmd’ using ‘UTF-8’... failed
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.7Mb
      sub-directories of 1Mb or more:
        doc   6.5Mb
    ```

# CLONETv2

<details>

* Version: 2.2.1
* GitHub: NA
* Source code: https://github.com/cran/CLONETv2
* Date/Publication: 2021-10-13 20:40:14 UTC
* Number of recursive dependencies: 34

Run `revdepcheck::cloud_details(, "CLONETv2")` for more info

</details>

## Newly broken

*   checking whether package ‘CLONETv2’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘arules::is.element’ by ‘ggplot2::is.element’ when loading ‘CLONETv2’
    See ‘/tmp/workdir/CLONETv2/new/CLONETv2.Rcheck/00install.out’ for details.
    ```

# ClusROC

<details>

* Version: 1.0.2
* GitHub: https://github.com/toduckhanh/ClusROC
* Source code: https://github.com/cran/ClusROC
* Date/Publication: 2022-11-17 15:00:02 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::cloud_details(, "ClusROC")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ClusROC-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: clus_lme
    > ### Title: Linear Mixed-Effects Models for a continuous diagnostic test or
    > ###   a biomarker (or a classifier).
    > ### Aliases: clus_lme
    > 
    > ### ** Examples
    > 
    ...
    3       1 -0.5174383 2  0.36460260  1
    4       2  2.0831761 1  2.55955171  0
    5       2  2.5393980 2 -0.57296509  1
    6       2 -1.6553751 1  0.06360181  0
    > ## A model with two covariate
    > out1 <- clus_lme(fixed_formula = Y ~ X1 + X2, name_class = "D",
    +                  name_clust = "id_Clus", data = data_3class)
    Error in is.element(name_test, names_vars) : unused argument (names_vars)
    Calls: clus_lme -> check_fixed_formula
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ClusROC)
      Loading required package: nlme
      Loading required package: Rcpp
      > library(rgl)
      > 
      > options(rgl.useNULL = TRUE)
    ...
                without newdata ──
      Error in `is.element(name_test, names_vars)`: unused argument (names_vars)
      Backtrace:
          ▆
       1. └─ClusROC::clus_lme(...) at test-clus_vus.R:31:3
       2.   └─ClusROC:::check_fixed_formula(fixed_formula, call, names_vars)
      
      [ FAIL 31 | WARN 0 | SKIP 0 | PASS 12 ]
      Error: Test failures
      Execution halted
    ```

*   checking whether package ‘ClusROC’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(name_class, ': unused argument (names_vars) 
      Note: possible error in 'is.element(name_clust, ': unused argument (names_vars) 
      Note: possible error in 'is.element(name_clust, ': unused argument (c(name_test, names_covars, name_class)) 
      Note: possible error in 'is.element(name_test, ': unused argument (names_vars) 
      Note: possible error in 'is.element(names_covars, ': unused argument (names_vars) 
      Note: possible error in 'is.element(x, ok_method)': unused argument (ok_method) 
    See ‘/tmp/workdir/ClusROC/new/ClusROC.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    check_class: possible error in is.element(name_class, names_vars):
      unused argument (names_vars)
    check_clust: possible error in is.element(name_clust, names_vars):
      unused argument (names_vars)
    check_clust: possible error in is.element(name_clust, c(name_test,
      names_covars, name_class)): unused argument (c(name_test,
      names_covars, name_class))
    check_fixed_formula: possible error in is.element(name_test,
      names_vars): unused argument (names_vars)
    check_fixed_formula: possible error in is.element(names_covars,
      names_vars): unused argument (names_vars)
    get_method_opt_thres3 : <anonymous>: possible error in is.element(x,
      ok_method): unused argument (ok_method)
    ```

# clustcurv

<details>

* Version: 2.0.2
* GitHub: https://github.com/noramvillanueva/clustcurv
* Source code: https://github.com/cran/clustcurv
* Date/Publication: 2024-10-25 08:20:07 UTC
* Number of recursive dependencies: 109

Run `revdepcheck::cloud_details(, "clustcurv")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘clustcurv.Rmd’
      ...
    > autoplot(res, groups_by_colour = FALSE, interactive = TRUE)
    Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
    ℹ Please use tidy evaluation idioms with `aes()`.
    ℹ See also `vignette("ggplot2-in-packages")` for more information.
    ℹ The deprecated feature was likely used in the ggfortify package.
      Please report the issue at <https://github.com/sinhrks/ggfortify/issues>.
    
      When sourcing ‘clustcurv.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘clustcurv.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘clustcurv.Rmd’ using rmarkdown
    
    Quitting from lines 93-94 [unnamed-chunk-5] (clustcurv.Rmd)
    Error: processing vignette 'clustcurv.Rmd' failed with diagnostics:
    subscript out of bounds
    --- failed re-building ‘clustcurv.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘clustcurv.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# clustMD

<details>

* Version: 1.2.1
* GitHub: NA
* Source code: https://github.com/cran/clustMD
* Date/Publication: 2017-05-08 17:19:20 UTC
* Number of recursive dependencies: 42

Run `revdepcheck::cloud_details(, "clustMD")` for more info

</details>

## Newly broken

*   checking whether package ‘clustMD’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(model, c("EII", ': unused argument (c("EII", "VII", "EEI", "VEI", "EVI", "VVI", "BD")) 
      Note: possible error in 'is.element(covModel, ': unused argument (resParallel$models) 
      Note: possible error in 'is.element(nClus, resParallel$G)': unused argument (resParallel$G) 
    See ‘/tmp/workdir/clustMD/new/clustMD.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    M.step: possible error in is.element(model, c("EII", "VII", "EEI",
      "VEI", "EVI", "VVI", "BD")): unused argument (c("EII", "VII", "EEI",
      "VEI", "EVI", "VVI", "BD"))
    getOutput_clustMDparallel: possible error in is.element(covModel,
      resParallel$models): unused argument (resParallel$models)
    getOutput_clustMDparallel: possible error in is.element(nClus,
      resParallel$G): unused argument (resParallel$G)
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) dtmvnom.Rd:30: Escaped LaTeX specials: \&
    ```

# cnmap

<details>

* Version: 0.1.0
* GitHub: https://github.com/PanfengZhang/cnmap
* Source code: https://github.com/cran/cnmap
* Date/Publication: 2024-04-02 12:42:06 UTC
* Number of recursive dependencies: 61

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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Introduction.Rmd’
      ...
    > library(sf)
    Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
    
    > library(cnmap)
    
    > map1 <- getMap(name = "中国")
    
      When sourcing ‘Introduction.R’:
    Error: package tibble not available: install first?
    Execution halted
    
      ‘Introduction.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Introduction.Rmd’ using rmarkdown
    
    Quitting from lines 29-35 [unnamed-chunk-3] (Introduction.Rmd)
    Error: processing vignette 'Introduction.Rmd' failed with diagnostics:
    package tibble not available: install first?
    --- failed re-building ‘Introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# codaredistlm

<details>

* Version: 0.1.0
* GitHub: https://github.com/tystan/codaredistlm
* Source code: https://github.com/cran/codaredistlm
* Date/Publication: 2022-12-22 19:50:06 UTC
* Number of recursive dependencies: 66

Run `revdepcheck::cloud_details(, "codaredistlm")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘codaredistlm-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_delta_comp
    > ### Title: Plot redistributed time-use predictions from compositional ilr
    > ###   multiple linear regression model fit
    > ### Aliases: plot_delta_comp
    > 
    > ### ** Examples
    > 
    ...
    |     |      x|
    |:----|------:|
    |0%   | 1439.9|
    |25%  | 1440.0|
    |50%  | 1440.0|
    |75%  | 1440.0|
    |100% | 1440.1|
    ---
    
    ---
    ```

# codez

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/codez
* Date/Publication: 2022-09-23 15:10:02 UTC
* Number of recursive dependencies: 136

Run `revdepcheck::cloud_details(, "codez")` for more info

</details>

## Newly broken

*   checking whether package ‘codez’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::is.scale’ by ‘greybox::is.scale’ when loading ‘codez’
    See ‘/tmp/workdir/codez/new/codez.Rcheck/00install.out’ for details.
    ```

# CohortPlat

<details>

* Version: 1.0.5
* GitHub: NA
* Source code: https://github.com/cran/CohortPlat
* Date/Publication: 2022-02-14 09:30:02 UTC
* Number of recursive dependencies: 82

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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘my-vignette.Rmd’
      ...
    
    > set.seed(50)
    
    > ocs1 <- trial_ocs(n_int = n_int, n_fin = n_fin, rr_comb = rr_comb, 
    +     rr_mono = rr_mono, rr_back = rr_back, rr_plac = rr_plac, 
    +     rr_transfo .... [TRUNCATED] 
    
      When sourcing ‘my-vignette.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘my-vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘my-vignette.Rmd’ using rmarkdown
    
    Quitting from lines 1043-1073 [unnamed-chunk-20] (my-vignette.Rmd)
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
* Number of recursive dependencies: 89

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

# CoreMicrobiomeR

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/CoreMicrobiomeR
* Date/Publication: 2024-04-03 20:03:02 UTC
* Number of recursive dependencies: 91

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
* Number of recursive dependencies: 115

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
* Number of recursive dependencies: 128

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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘corrViz.Rmd’
      ...
    > library(corrViz)
    
    > cm <- cor(mtcars)
    
    > corrHeatmap(mat = cm, display = "all", reorder = TRUE, 
    +     pal = colorRampPalette(c("darkblue", "white", "darkred"))(100))
    
      When sourcing ‘corrViz.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘corrViz.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘corrViz.Rmd’ using rmarkdown
    
    Quitting from lines 76-81 [heatmap] (corrViz.Rmd)
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
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "CoSMoS")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘vignette.Rmd’
      ...
    
    > ggplot(data = dta, mapping = aes(x = time, y = value)) + 
    +     geom_line() + facet_grid(facets = variable ~ ., scales = "free_y") + 
    +     theme_li .... [TRUNCATED] 
    
      When sourcing ‘vignette.R’:
    Error: The `facets` argument of `facet_grid()` was deprecated in ggplot2 2.2.0
    and is now defunct.
    ℹ Please use the `rows` argument instead.
    Execution halted
    
      ‘vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
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
* Number of recursive dependencies: 90

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

# covidcast

<details>

* Version: 0.5.2
* GitHub: https://github.com/cmu-delphi/covidcast
* Source code: https://github.com/cran/covidcast
* Date/Publication: 2023-07-12 23:40:06 UTC
* Number of recursive dependencies: 92

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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘plotting-signals.Rmd’
      ...
    • horizontal = TRUE
    ℹ Did you misspell an argument name?
    
      When sourcing ‘plotting-signals.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 5th layer.
    Caused by error in `check_aesthetics()`:
    ! Aesthetics must be either length 1 or the same as the data (3078).
    ✖ Fix the following mappings: `fill`.
    Execution halted
    
      ‘correlation-utils.Rmd’ using ‘UTF-8’... OK
      ‘covidcast.Rmd’ using ‘UTF-8’... OK
      ‘external-data.Rmd’ using ‘UTF-8’... OK
      ‘multi-signals.Rmd’ using ‘UTF-8’... OK
      ‘plotting-signals.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
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

# creditmodel

<details>

* Version: 1.3.1
* GitHub: NA
* Source code: https://github.com/cran/creditmodel
* Date/Publication: 2022-01-07 11:32:41 UTC
* Number of recursive dependencies: 71

Run `revdepcheck::cloud_details(, "creditmodel")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘creditmodel-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: char_to_num
    > ### Title: character to number
    > ### Aliases: char_to_num
    > 
    > ### ** Examples
    > 
    > dat_sub = lendingclub[c('dti_joint',	'emp_length')]
    > str(dat_sub)
    'data.frame':	31766 obs. of  2 variables:
     $ dti_joint : chr  "Missing" "Missing" "9.02" "Missing" ...
     $ emp_length: chr  "3 years" "2 years" "2 years" "10+ years" ...
    > #variables that are converted to numbers containing strings
    > dat_sub = char_to_num(dat_sub)
    Error in is.element(class(x), types) : unused argument (types)
    Calls: char_to_num -> get_names -> unique -> sapply -> lapply -> FUN
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘introduction.Rmd’
      ...
    > library(creditmodel)
    Package 'creditmodel' version 1.3.1
    
    > B_model = training_model(dat = UCICreditCard, model_name = "UCICreditCard", 
    +     target = "default.payment.next.month", x_list = NULL, occur_time  .... [TRUNCATED] 
    ── Building ─────────────────────────────────────────────────── UCICreditCard ──
    
      When sourcing ‘introduction.R’:
    Error: unused argument (c("LR", "XGB", "GBM", "RF"))
    Execution halted
    
      ‘introduction.Rmd’ using ‘UTF-8’... failed
    ```

*   checking whether package ‘creditmodel’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(x, names(mat_nas_shadow))': unused argument (names(mat_nas_shadow)) 
      Note: possible error in 'is.element(x, names(corr_random_num))': unused argument (names(corr_random_num)) 
      Note: possible error in 'is.element(x, colnames(corr_random_char))': unused argument (colnames(corr_random_char)) 
      Note: possible error in 'is.element(class(dm_x), ': unused argument (c("Date", "numeric", "integer", "double")) 
      Note: possible error in 'is.element(x, c("cyan", ': unused argument (c("cyan", "grey", "green", "red", "blue", "purple")) 
      Note: possible error in 'is.element(class(dat), ': unused argument (c("data.table", "list", "tbl_df", "tbl", "matrix")) 
      Note: possible error in 'is.element(occur_time, ': unused argument (names(dat)) 
      Note: possible error in 'is.element(class(dat_x), ': unused argument (c("integer", "numeric", "double")) 
      Note: possible error in 'is.element(class(dat_x), ': unused argument (c("Date")) 
    ...
      Note: possible error in 'is.element("mins", unit)': unused argument (unit) 
      Note: possible error in 'is.element("hours", unit)': unused argument (unit) 
      Note: possible error in 'is.element("weeks", unit)': unused argument (unit) 
      Note: possible error in 'is.element(split_type, ': unused argument (c("OOT", "Random", "byRow")) 
      Note: possible error in 'is.element(algorithm, ': unused argument (c("LR", "XGB", "GBM", "RF")) 
      Note: possible error in 'is.element(feature_filter[["filter"]], ': unused argument (c("IV", "PSI", "XGB", "COR")) 
    See ‘/tmp/workdir/creditmodel/new/creditmodel.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    analysis_nas : <anonymous>: possible error in is.element(x,
      names(mat_nas_shadow)): unused argument (names(mat_nas_shadow))
    analysis_nas : <anonymous>: possible error in is.element(x,
      names(corr_random_num)): unused argument (names(corr_random_num))
    analysis_nas : <anonymous>: possible error in is.element(x,
      colnames(corr_random_char)): unused argument
      (colnames(corr_random_char))
    analysis_outliers: possible error in is.element(class(dm_x), c("Date",
      "numeric", "integer", "double")): unused argument (c("Date",
      "numeric", "integer", "double"))
    ...
      "GBM", "RF")): unused argument (c("LR", "XGB", "GBM", "RF"))
    training_model: possible error in is.element(missing_proc, c("median",
      "avg_dist", "default")): unused argument (c("median", "avg_dist",
      "default"))
    training_model: possible error in
      is.element(feature_filter[["filter"]], c("IV", "PSI", "XGB", "COR")):
      unused argument (c("IV", "PSI", "XGB", "COR"))
    xgb_params_search: possible error in is.element(method,
      c("random_search", "grid_search", "local_search")): unused argument
      (c("random_search", "grid_search", "local_search"))
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘introduction.Rmd’ using rmarkdown
    
    Quitting from lines 31-79 [unnamed-chunk-1] (introduction.Rmd)
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    unused argument (c("LR", "XGB", "GBM", "RF"))
    --- failed re-building ‘introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.6Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        data   5.5Mb
    ```

# cricketr

<details>

* Version: 0.0.26
* GitHub: https://github.com/tvganesh/cricketr
* Source code: https://github.com/cran/cricketr
* Date/Publication: 2021-03-23 05:30:15 UTC
* Number of recursive dependencies: 58

Run `revdepcheck::cloud_details(, "cricketr")` for more info

</details>

## Newly broken

*   checking whether package ‘cricketr’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(1, homeOrAway)': unused argument (homeOrAway) 
      Note: possible error in 'is.element(2, homeOrAway)': unused argument (homeOrAway) 
      Note: possible error in 'is.element(3, homeOrAway)': unused argument (homeOrAway) 
      Note: possible error in 'is.element(1, result)': unused argument (result) 
      Note: possible error in 'is.element(2, result)': unused argument (result) 
      Note: possible error in 'is.element(3, result)': unused argument (result) 
      Note: possible error in 'is.element(4, result)': unused argument (result) 
      Note: possible error in 'is.element(5, result)': unused argument (result) 
    See ‘/tmp/workdir/cricketr/new/cricketr.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    getTeamData: possible error in is.element(1, homeOrAway): unused
      argument (homeOrAway)
    getTeamData: possible error in is.element(2, homeOrAway): unused
      argument (homeOrAway)
    getTeamData: possible error in is.element(3, homeOrAway): unused
      argument (homeOrAway)
    getTeamData: possible error in is.element(1, result): unused argument
      (result)
    getTeamData: possible error in is.element(2, result): unused argument
      (result)
    getTeamData: possible error in is.element(3, result): unused argument
      (result)
    getTeamData: possible error in is.element(4, result): unused argument
      (result)
    getTeamData: possible error in is.element(5, result): unused argument
      (result)
    ```

# crosshap

<details>

* Version: 1.4.0
* GitHub: https://github.com/jacobimarsh/crosshap
* Source code: https://github.com/cran/crosshap
* Date/Publication: 2024-03-31 15:40:02 UTC
* Number of recursive dependencies: 116

Run `revdepcheck::cloud_details(, "crosshap")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘crosshap-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: build_bot_halfeyeplot
    > ### Title: Bot hap-pheno raincloud plot
    > ### Aliases: build_bot_halfeyeplot
    > 
    > ### ** Examples
    > 
    > 
    ...
     12. │     └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13. │       └─l$compute_geom_2(d, theme = plot$theme)
     14. │         └─ggplot2 (local) compute_geom_2(..., self = self)
     15. │           └─self$geom$use_defaults(...)
     16. └─base::.handleSimpleError(...)
     17.   └─rlang (local) h(simpleError(msg, call))
     18.     └─handlers[[1L]](cnd)
     19.       └─cli::cli_abort(...)
     20.         └─rlang::abort(...)
    Execution halted
    ```

# CRTgeeDR

<details>

* Version: 2.0.1
* GitHub: NA
* Source code: https://github.com/cran/CRTgeeDR
* Date/Publication: 2022-09-06 07:28:24 UTC
* Number of recursive dependencies: 28

Run `revdepcheck::cloud_details(, "CRTgeeDR")` for more info

</details>

## Newly broken

*   checking whether package ‘CRTgeeDR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(id, dropid)': unused argument (dropid) 
    See ‘/tmp/workdir/CRTgeeDR/new/CRTgeeDR.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    geeDREstimation: possible error in is.element(id, dropid): unused
      argument (dropid)
    ```

# ctrialsgov

<details>

* Version: 0.2.5
* GitHub: NA
* Source code: https://github.com/cran/ctrialsgov
* Date/Publication: 2021-10-18 16:00:02 UTC
* Number of recursive dependencies: 99

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
* Number of recursive dependencies: 132

Run `revdepcheck::cloud_details(, "cubble")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘cb6interactive.Rmd’
      ...
    +     ymax .... [TRUNCATED] 
    Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ℹ Please use `linewidth` instead.
    
    > ts_interactive <- highlight(ggplotly(ts_static, width = 600, 
    +     height = 300), on = "plotly_selected", opacityDim = 0.012)
    
    ...
    Error: subscript out of bounds
    Execution halted
    
      ‘cb1class.Rmd’ using ‘UTF-8’... OK
      ‘cb2create.Rmd’ using ‘UTF-8’... OK
      ‘cb3tsibblesf.Rmd’ using ‘UTF-8’... OK
      ‘cb4glyph.Rmd’ using ‘UTF-8’... OK
      ‘cb5match.Rmd’ using ‘UTF-8’... OK
      ‘cb6interactive.Rmd’ using ‘UTF-8’... failed
      ‘cb7misc.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
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
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        data   3.0Mb
        doc    1.4Mb
    ```

# DAISIE

<details>

* Version: 4.4.1
* GitHub: https://github.com/rsetienne/DAISIE
* Source code: https://github.com/cran/DAISIE
* Date/Publication: 2023-10-21 23:10:10 UTC
* Number of recursive dependencies: 118

Run `revdepcheck::cloud_details(, "DAISIE")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘DAISIE-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: DAISIE_plot_input
    > ### Title: DAISIE tree plot
    > ### Aliases: DAISIE_plot_input
    > 
    > ### ** Examples
    > 
    > 
    ...
    ℹ Did you misspell an argument name?
    Scale for y is already present.
    Adding another scale for y, which will replace the existing scale.
    Joining with `by = join_by(label)`
    Warning: Removed 1 row containing missing values or values outside the scale range
    (`geom_point()`).
    Error in grid.Call.graphics(C_rect, x$x, x$y, x$width, x$height, resolveHJust(x$just,  : 
      invalid hex digit in 'color' or 'lty'
    Calls: <Anonymous> ... drawDetails -> drawDetails.rect -> grid.Call.graphics
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘doMC’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 37.6Mb
      sub-directories of 1Mb or more:
        libs  35.5Mb
    ```

# DAISIEprep

<details>

* Version: 1.0.0
* GitHub: https://github.com/joshwlambert/DAISIEprep
* Source code: https://github.com/cran/DAISIEprep
* Date/Publication: 2024-12-18 00:20:02 UTC
* Number of recursive dependencies: 148

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
      [ FAIL 4 | WARN 4 | SKIP 14 | PASS 2235 ]
      
      ══ Skipped tests (14) ══════════════════════════════════════════════════════════
    ...
      ── Failure ('test-plot_phylod.R:8:3'): plot_phylod runs silent without error ───
      `plot_phylod(phylod = phylod, node_pies = FALSE)` produced warnings.
      ── Failure ('test-plot_phylod.R:13:3'): plot_phylod runs silent without error ──
      `plot_phylod(phylod = phylod, node_pies = TRUE)` produced warnings.
      ── Failure ('test-plot_phylod.R:18:3'): plot_phylod runs silent without error ──
      `plot_phylod(phylod = phylod, node_pies = TRUE)` produced warnings.
      
      [ FAIL 4 | WARN 4 | SKIP 14 | PASS 2235 ]
      Error: Test failures
      Execution halted
    ```

# daltoolbox

<details>

* Version: 1.1.727
* GitHub: https://github.com/cefet-rj-dal/daltoolbox
* Source code: https://github.com/cran/daltoolbox
* Date/Publication: 2024-11-25 05:10:02 UTC
* Number of recursive dependencies: 108

Run `revdepcheck::cloud_details(, "daltoolbox")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘daltoolbox-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_ts_pred
    > ### Title: Plot a time series chart with predictions
    > ### Aliases: plot_ts_pred
    > 
    > ### ** Examples
    > 
    > data(sin_data)
    ...
    > samp <- ts_sample(ts, test_size= 5)
    > io_train <- ts_projection(samp$train)
    > io_test <- ts_projection(samp$test)
    > 
    > model <- ts_arima()
    > model <- fit(model, x=io_train$input, y=io_train$output)
    Error in is.element("drift", names(obj$model$coef)) : 
      unused argument (names(obj$model$coef))
    Calls: fit -> fit.ts_arima
    Execution halted
    ```

*   checking whether package ‘daltoolbox’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element("drift", names(obj$model$coef))': unused argument (names(obj$model$coef)) 
    See ‘/tmp/workdir/daltoolbox/new/daltoolbox.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    fit.ts_arima: possible error in is.element("drift",
      names(obj$model$coef)): unused argument (names(obj$model$coef))
    ```

# dawaR

<details>

* Version: 0.2.7
* GitHub: https://github.com/aleksanderbl29/dawaR
* Source code: https://github.com/cran/dawaR
* Date/Publication: 2024-12-08 00:00:06 UTC
* Number of recursive dependencies: 150

Run `revdepcheck::cloud_details(, "dawaR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘dawaR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: get_data
    > ### Title: Get data from section
    > ### Aliases: get_data
    > 
    > ### ** Examples
    > 
    > if (connection_check()) {
    ...
    +   x <- get_data("regioner")
    +   head(x)
    + }
    ✖ Request failed: HTTP 504 Gateway Timeout.
    ✖ The API returned a  error.
    ✖ No content will be returned
    Error in do.call(rbind.data.frame, response) : 
      second argument must be a list
    Calls: get_data -> do.call
    Execution halted
    ```

## Newly fixed

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
      ── Error ('test-get_data.R:55:5'): get_data(section = 'regioner') snapshot ─────
      Error in `do.call(rbind.data.frame, response)`: second argument must be a list
      Backtrace:
          ▆
       1. └─testthat::expect_snapshot(get_data(section = "regioner")) at test-get_data.R:55:5
       2.   └─rlang::cnd_signal(state$error)
      
      [ FAIL 1 | WARN 0 | SKIP 48 | PASS 10 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘dawaR.Rmd’ using rmarkdown
    
    Quitting from lines 62-70 [municipality_map] (dawaR.Rmd)
    Error: processing vignette 'dawaR.Rmd' failed with diagnostics:
    Problem while computing aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error:
    ! object 'regionsnavn' not found
    ...
    --- finished re-building ‘renv-issues.Rmd’
    
    --- re-building ‘status.Rmd’ using rmarkdown
    --- finished re-building ‘status.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘dawaR.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# DDPNA

<details>

* Version: 0.3.3
* GitHub: https://github.com/liukf10/DDPNA
* Source code: https://github.com/cran/DDPNA
* Date/Publication: 2024-03-14 03:20:15 UTC
* Number of recursive dependencies: 153

Run `revdepcheck::cloud_details(, "DDPNA")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘DDPNA-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: MaxQprotein
    > ### Title: read proteomic quantification data and seperate the protein
    > ###   information and quantification information.
    > ### Aliases: MaxQprotein
    > 
    > ### ** Examples
    > 
    > data(ProteomicData)
    > # example for MaxQ Data
    > MaxQdata <- MaxQprotein(ProteomicData$MaxQ)
    Error in is.element("Potential.contaminant", colnames(infile)) : 
      unused argument (colnames(infile))
    Calls: MaxQprotein -> removeConRev
    Execution halted
    ```

*   checking whether package ‘DDPNA’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element("Potential.contaminant", ': unused argument (colnames(infile)) 
      Note: possible error in 'is.element("+", unique(infile$Potential.contaminant))': unused argument (unique(infile$Potential.contaminant)) 
      Note: possible error in 'is.element("Reverse", ': unused argument (colnames(infile)) 
      Note: possible error in 'is.element("+", unique(infile$Reverse))': unused argument (unique(infile$Reverse)) 
      Note: possible error in 'is.element(IDname, colname)': unused argument (colname) 
      Note: possible error in 'is.element(QuanCol, colname)': unused argument (colname) 
    See ‘/tmp/workdir/DDPNA/new/DDPNA.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    MaxQprotein : removeConRev: possible error in
      is.element("Potential.contaminant", colnames(infile)): unused
      argument (colnames(infile))
    MaxQprotein : removeConRev: possible error in is.element("+",
      unique(infile$Potential.contaminant)): unused argument
      (unique(infile$Potential.contaminant))
    MaxQprotein : removeConRev: possible error in is.element("Reverse",
      colnames(infile)): unused argument (colnames(infile))
    MaxQprotein : removeConRev: possible error in is.element("+",
      unique(infile$Reverse)): unused argument (unique(infile$Reverse))
    MaxQprotein: possible error in is.element(IDname, colname): unused
      argument (colname)
    MaxQprotein: possible error in is.element(QuanCol, colname): unused
      argument (colname)
    ```

# Deducer

<details>

* Version: 0.7-9
* GitHub: NA
* Source code: https://github.com/cran/Deducer
* Date/Publication: 2015-12-29 22:16:31
* Number of recursive dependencies: 118

Run `revdepcheck::cloud_details(, "Deducer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Deducer-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: sort.data.frame
    > ### Title: Sort Data
    > ### Aliases: sort.data.frame
    > 
    > ### ** Examples
    > 
    > data(mtcars)
    > 
    > #sort by the number of cylenders
    > sort(mtcars, by= ~cyl)
    Error in is.element(substring(formc, 1, 1), c("+", "-")) : 
      unused argument (c("+", "-"))
    Calls: sort -> sort -> sort.data.frame
    Execution halted
    ```

*   checking whether package ‘Deducer’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(substring(formc, ': unused argument (c("+", "-")) 
    See ‘/tmp/workdir/Deducer/new/Deducer.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    sort.data.frame: possible error in is.element(substring(formc, 1, 1),
      c("+", "-")): unused argument (c("+", "-"))
    ```

# deeptime

<details>

* Version: 2.1.0
* GitHub: https://github.com/willgearty/deeptime
* Source code: https://github.com/cran/deeptime
* Date/Publication: 2024-10-25 23:30:02 UTC
* Number of recursive dependencies: 197

Run `revdepcheck::cloud_details(, "deeptime")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘deeptime-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: facet_wrap_color
    > ### Title: Wrap a 1d ribbon of panels into 2d with colored strips
    > ### Aliases: facet_wrap_color FacetWrapColor
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    ...
        is.element
    
    > df <- data.frame(x = 1:10, y = 1:10, period = c("Permian", "Triassic"))
    > ggplot(df) +
    +   geom_point(aes(x, y)) +
    +   facet_wrap_color(vars(period), colors = periods)
    Error in asNamespace("ggplot2")$wrap_as_facets_list(...) : 
      attempt to apply non-function
    Calls: facet_wrap_color -> wrap_as_facets_list
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

# desplot

<details>

* Version: 1.10
* GitHub: https://github.com/kwstat/desplot
* Source code: https://github.com/cran/desplot
* Date/Publication: 2023-03-09 23:20:06 UTC
* Number of recursive dependencies: 68

Run `revdepcheck::cloud_details(, "desplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘desplot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: desplot
    > ### Title: Plot the layout/data of a field experiment.
    > ### Aliases: desplot ggdesplot
    > 
    > ### ** Examples
    > 
    > if(require(agridat)){
    ...
    + desplot(yates.oats, 
    +         block ~ col+row, 
    +         col=nitro, text=gen, cex=1, out1=block,
    +         out2=gen, out2.gpar=list(col = "gray50", lwd = 1, lty = 1))
    + 
    + }
    Loading required package: agridat
    Error in is.element(x, dn) : unused argument (dn)
    Calls: desplot -> checkvars
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(desplot)
      > 
      > test_check("desplot")
      [ FAIL 7 | WARN 0 | SKIP 0 | PASS 10 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      ── Error ('test_desplot.R:142:1'): (code run outside of `test_that()`) ─────────
      Error in `is.element(x, dn)`: unused argument (dn)
      Backtrace:
          ▆
       1. └─desplot::desplot(...) at test_desplot.R:142:1
       2.   └─desplot (local) checkvars(col.string, dn)
      
      [ FAIL 7 | WARN 0 | SKIP 0 | PASS 10 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘desplot_examples.Rmd’
      ...
    > if (is.element("x", names(yates.oats))) yates.oats <- transform(yates.oats, 
    +     col = x, row = y)
    
    > desplot(yates.oats, block ~ col + row, col = nitro, 
    +     text = gen, cex = 1, out1 = block, out2 = gen, out2.gpar = list(col = "gray50", 
    +        .... [TRUNCATED] 
    
      When sourcing ‘desplot_examples.R’:
    Error: unused argument (dn)
    Execution halted
    
      ‘desplot_examples.Rmd’ using ‘UTF-8’... failed
    ```

*   checking whether package ‘desplot’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(x, dn)': unused argument (dn) 
    See ‘/tmp/workdir/desplot/new/desplot.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    desplot : checkvars: possible error in is.element(x, dn): unused
      argument (dn)
    ggdesplot : checkvars: possible error in is.element(x, dn): unused
      argument (dn)
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘desplot_examples.Rmd’ using rmarkdown
    
    Quitting from lines 30-39 [yates] (desplot_examples.Rmd)
    Error: processing vignette 'desplot_examples.Rmd' failed with diagnostics:
    unused argument (dn)
    --- failed re-building ‘desplot_examples.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘desplot_examples.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# diceR

<details>

* Version: 2.2.0
* GitHub: https://github.com/AlineTalhouk/diceR
* Source code: https://github.com/cran/diceR
* Date/Publication: 2024-01-22 21:22:46 UTC
* Number of recursive dependencies: 155

Run `revdepcheck::cloud_details(, "diceR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘diceR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: graphs
    > ### Title: Graphical Displays
    > ### Aliases: graphs graph_cdf graph_delta_area graph_heatmap graph_tracking
    > ###   graph_all
    > 
    > ### ** Examples
    > 
    ...
      7. │       ├─purrr:::call_with_cleanup(...)
      8. │       └─diceR (local) .f(...)
      9. ├─base::loadNamespace(x)
     10. │ └─base (local) runHook(".onLoad", env, package.lib, package)
     11. │   └─base::stop(...)
     12. └─base::.handleSimpleError(...)
     13.   └─purrr (local) h(simpleError(msg, call))
     14.     └─cli::cli_abort(...)
     15.       └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(diceR)
      > 
      > test_check("diceR")
      Selecting k and imputing non-clustered cases
      Computing consensus functions
      Evaluating output with consensus function results
    ...
       18. │ └─base (local) runHook(".onLoad", env, package.lib, package)
       19. │   └─base::stop(...)
       20. └─base::.handleSimpleError(...)
       21.   └─purrr (local) h(simpleError(msg, call))
       22.     └─cli::cli_abort(...)
       23.       └─rlang::abort(...)
      
      [ FAIL 9 | WARN 7 | SKIP 0 | PASS 91 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘overview.Rmd’
      ...
    > hm <- graph_heatmap(pam.4)
    
      When sourcing ‘overview.R’:
    Error: ℹ In index: 1.
    ℹ With name: PAM_Euclidean k=4.
    Caused by error:
    ! .onLoad failed in loadNamespace() for 'NMF', details:
      call: is.element(models, models.wraps)
      error: unused argument (models.wraps)
    Execution halted
    
      ‘overview.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘overview.Rmd’ using rmarkdown
    
    Quitting from lines 169-170 [graph_heatmap] (overview.Rmd)
    Error: processing vignette 'overview.Rmd' failed with diagnostics:
    ℹ In index: 1.
    ℹ With name: PAM_Euclidean k=4.
    Caused by error:
    ! .onLoad failed in loadNamespace() for 'NMF', details:
      call: is.element(models, models.wraps)
      error: unused argument (models.wraps)
    --- failed re-building ‘overview.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘overview.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# directlabels

<details>

* Version: 2024.1.21
* GitHub: https://github.com/tdhock/directlabels
* Source code: https://github.com/cran/directlabels
* Date/Publication: 2024-01-24 19:20:07 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::cloud_details(, "directlabels")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘directlabels-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: qp.labels
    > ### Title: Make a Positioning Method for non-overlapping lineplot labels
    > ### Aliases: qp.labels
    > 
    > ### ** Examples
    > 
    > SegCost$error <- factor(SegCost$error,c("FP","FN","E","I"))
    ...
    ! The `panel.margin` argument of `theme()` was deprecated in ggplot2
      2.2.0 and is now defunct.
    ℹ Please use the `panel.spacing` argument instead.
    Backtrace:
        ▆
     1. └─ggplot2::theme(panel.margin = grid::unit(0, "lines"))
     2.   └─lifecycle::deprecate_stop("2.2.0", "theme(panel.margin)", "theme(panel.spacing)")
     3.     └─lifecycle:::deprecate_stop0(msg)
     4.       └─rlang::cnd_signal(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘examples.Rmd’
      ...
    Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ℹ Please use `linewidth` instead.
    `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
    
      When sourcing ‘examples.R’:
    Error: Problem while computing stat.
    ℹ Error occurred in the 3rd layer.
    Caused by error in `get()`:
    ! object 'last.qp' of mode 'function' was not found
    Execution halted
    
      ‘examples.Rmd’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘examples.Rmd’ using knitr
    ```

# distributional

<details>

* Version: 0.5.0
* GitHub: https://github.com/mitchelloharawild/distributional
* Source code: https://github.com/cran/distributional
* Date/Publication: 2024-09-17 06:20:02 UTC
* Number of recursive dependencies: 120

Run `revdepcheck::cloud_details(, "distributional")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘distributional-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: dist_truncated
    > ### Title: Truncate a distribution
    > ### Aliases: dist_truncated
    > 
    > ### ** Examples
    > 
    > dist <- dist_truncated(dist_normal(2,1), lower = 0)
    ...
     12. │     └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13. │       └─l$compute_geom_2(d, theme = plot$theme)
     14. │         └─ggplot2 (local) compute_geom_2(..., self = self)
     15. │           └─self$geom$use_defaults(...)
     16. └─base::.handleSimpleError(...)
     17.   └─rlang (local) h(simpleError(msg, call))
     18.     └─handlers[[1L]](cnd)
     19.       └─cli::cli_abort(...)
     20.         └─rlang::abort(...)
    Execution halted
    ```

# dittoViz

<details>

* Version: 1.0.1
* GitHub: https://github.com/dtm2451/dittoViz
* Source code: https://github.com/cran/dittoViz
* Date/Publication: 2024-02-02 00:00:12 UTC
* Number of recursive dependencies: 98

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
      
      Attaching package: 'ggplot2'
      
      The following object is masked from 'package:base':
    ...
       2. └─dittoViz::freqPlot(...)
       3.   └─dittoViz::yPlot(...)
       4.     └─dittoViz:::.warn_or_apply_plotly(p, plots)
       5.       ├─plotly::ggplotly(p, tooltip = "text")
       6.       └─plotly:::ggplotly.ggplot(p, tooltip = "text")
       7.         └─plotly::gg2list(...)
      
      [ FAIL 29 | WARN 0 | SKIP 0 | PASS 252 ]
      Error: Test failures
      Execution halted
    ```

# divent

<details>

* Version: 0.4-4
* GitHub: https://github.com/EricMarcon/divent
* Source code: https://github.com/cran/divent
* Date/Publication: 2024-11-06 16:10:08 UTC
* Number of recursive dependencies: 95

Run `revdepcheck::cloud_details(, "divent")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘divent-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: accum_div_phylo
    > ### Title: Phylogenetic Diversity Accumulation of a Community
    > ### Aliases: accum_div_phylo accum_ent_phylo accum_ent_phylo.numeric
    > ###   accum_ent_phylo.abundances accum_div_phylo.numeric
    > ###   accum_div_phylo.abundances
    > 
    > ### ** Examples
    ...
     11.       │ └─base::withCallingHandlers(...)
     12.       └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13.         └─l$compute_geom_2(d, theme = plot$theme)
     14.           └─ggplot2 (local) compute_geom_2(..., self = self)
     15.             └─self$geom$use_defaults(...)
     16.               └─ggplot2 (local) use_defaults(..., self = self)
     17.                 └─ggplot2:::check_aesthetics(new_params, nrow(data))
     18.                   └─cli::cli_abort(...)
     19.                     └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘divent.Rmd’
      ...
    
    > autoplot(paracou_6_abd[1, ])
    
      When sourcing ‘divent.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `check_aesthetics()`:
    ! Aesthetics must be either length 1 or the same as the data (189).
    ✖ Fix the following mappings: `shape` and `size`.
    Execution halted
    
      ‘divent.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘divent.Rmd’ using rmarkdown
    
    Quitting from lines 59-60 [plot_paracou6] (divent.Rmd)
    Error: processing vignette 'divent.Rmd' failed with diagnostics:
    Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `check_aesthetics()`:
    ! Aesthetics must be either length 1 or the same as the data (189).
    ✖ Fix the following mappings: `shape` and `size`.
    --- failed re-building ‘divent.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘divent.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘dbmss’
    ```

# doBy

<details>

* Version: 4.6.24
* GitHub: https://github.com/hojsgaard/doBy
* Source code: https://github.com/cran/doBy
* Date/Publication: 2024-10-07 23:20:12 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::cloud_details(, "doBy")` for more info

</details>

## Newly broken

*   checking whether package ‘doBy’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(substring(formc, ': unused argument (c("+", "-")) 
    See ‘/tmp/workdir/doBy/new/doBy.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    orderBy: possible error in is.element(substring(formc, 1, 1), c("+",
      "-")): unused argument (c("+", "-"))
    ```

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘doBy-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: by-order
    > ### Title: Ordering (sorting) rows of a data frame
    > ### Aliases: by-order order_by orderBy
    > ### Keywords: utilities
    > 
    > ### ** Examples
    > 
    > 
    > orderBy(~ conc + Treatment, CO2)
    Error in is.element(substring(formc, 1, 1), c("+", "-")) : 
      unused argument (c("+", "-"))
    Calls: orderBy
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘doby.rmd’
      ...
    > myfun1 <- function(x) {
    +     c(m = mean(x), s = sd(x))
    + }
    
    > summaryBy(cbind(mpg, cyl, lh = log(hp)) ~ vs, data = mtcars, 
    +     FUN = myfun1)
    
    ...
    > fm.mix <- lmer(uptake ~ conc + Treatment + Type + 
    +     (1 | Plant), data = CO2.ubal)
    
      When sourcing ‘linest_lsmeans.R’:
    Error: function 'cholmod_factor_ldetA' not provided by package 'Matrix'
    Execution halted
    
      ‘doby.rmd’ using ‘UTF-8’... failed
      ‘linest_lsmeans.rnw’ using ‘UTF-8’... failed
      ‘section_fun.rmd’ using ‘UTF-8’... OK
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.2Mb
      sub-directories of 1Mb or more:
        data   7.0Mb
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘doby.rmd’ using knitr
    
    Quitting from lines 70-75 [unnamed-chunk-4] (doby.rmd)
    Error: processing vignette 'doby.rmd' failed with diagnostics:
    unused argument (c("+", "-"))
    --- failed re-building ‘doby.rmd’
    
    --- re-building ‘linest_lsmeans.rnw’ using knitr
    ...
    --- failed re-building ‘linest_lsmeans.rnw’
    
    --- re-building ‘section_fun.rmd’ using knitr
    --- finished re-building ‘section_fun.rmd’
    
    SUMMARY: processing the following files failed:
      ‘doby.rmd’ ‘linest_lsmeans.rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# DoseFinding

<details>

* Version: 1.2-1
* GitHub: https://github.com/openpharma/DoseFinding
* Source code: https://github.com/cran/DoseFinding
* Date/Publication: 2024-08-23 16:20:09 UTC
* Number of recursive dependencies: 103

Run `revdepcheck::cloud_details(, "DoseFinding")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘DoseFinding-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: DoseFinding-package
    > ### Title: DoseFinding: Planning and Analyzing Dose Finding Experiments
    > ### Aliases: DoseFinding DoseFinding-package
    > ### Keywords: internal
    > 
    > ### ** Examples
    > 
    ...
    > data(IBScovars)
    > 
    > ## perform (model based) multiple contrast test
    > ## define candidate dose-response shapes
    > models <- Mods(linear = NULL, emax = 0.2, quadratic = -0.17,
    +                doses = c(0, 1, 2, 3, 4))
    Error in is.element(x, c("emax", "quadratic", "exponential")) : 
      unused argument (c("emax", "quadratic", "exponential"))
    Calls: Mods -> fullMod -> modCount -> lapply -> FUN
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testgFit.R’
    Running the tests in ‘tests/testgFit.R’ failed.
    Complete output:
      > require("DoseFinding")
      Loading required package: DoseFinding
      > data(IBScovars)
      > lmfit <- lm(resp~factor(dose)+gender, data=IBScovars)
      > cf <- coef(lmfit)[-c(6)]
      > vcv <- vcov(lmfit)[-c(6), -c(6)]
      > lmfit2 <- lm(resp~as.factor(dose)-1+gender, data=IBScovars)
      > cf2 <- coef(lmfit2)[-c(6)]
      > vcv2 <- vcov(lmfit2)[-c(6), -c(6)]
      > dose <- c(0:4)
      > 
      > ## test fitting all available models
      > fitMod(dose[-1], cf[-1], S=vcv[-1,-1], model="linear", placAdj=TRUE,type="general")
      Error in is.element(modelNum, 1:4) : unused argument (1:4)
      Calls: fitMod -> fitMod.raw
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘analysis_normal.Rmd’
      ...
    
    > doses <- c(0, 12.5, 25, 50, 100)
    
    > mods <- Mods(emax = c(2.6, 12.5), sigEmax = c(30.5, 
    +     3.5), quadratic = -0.00776, placEff = 1.25, maxEff = 0.15, 
    +     doses = doses)
    
    ...
      When sourcing ‘sample_size.R’:
    Error: unused argument (c("emax", "quadratic", "exponential"))
    Execution halted
    
      ‘analysis_normal.Rmd’ using ‘UTF-8’... failed
      ‘binary_data.Rmd’ using ‘UTF-8’... failed
      ‘faq.Rmd’ using ‘UTF-8’... OK
      ‘mult_regimen.Rmd’ using ‘UTF-8’... failed
      ‘overview.Rmd’ using ‘UTF-8’... failed
      ‘sample_size.Rmd’ using ‘UTF-8’... failed
    ```

*   checking whether package ‘DoseFinding’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(modelNum, ': unused argument (1:4) 
      Note: possible error in 'is.element(names(data), ': unused argument (usedVars) 
      Note: possible error in 'is.element(nm, c("emax", ': unused argument (c("emax", "exponential", "quadratic")) 
      Note: possible error in 'is.element(nm, c("logistic", ': unused argument (c("logistic", "betaMod", "sigEmax")) 
      Note: possible error in 'is.element(type, c("TD", ': unused argument (c("TD", "ED")) 
      Note: possible error in 'is.element(x, c("emax", ': unused argument (c("emax", "quadratic", "exponential")) 
      Note: possible error in 'is.element(x, c("sigEmax", ': unused argument (c("sigEmax", "logistic", "betaMod")) 
      Note: possible error in 'is.element(optimizer, ': unused argument (c("Nelder-Mead", "nlminb")) 
      Note: possible error in 'is.element(model, "linInt")': unused argument ("linInt") 
    ...
      Note: possible error in 'is.element("S", namargs)': unused argument (namargs) 
      Note: possible error in 'is.element("df", namargs)': unused argument (namargs) 
      Note: possible error in 'is.element("n", namargs)': unused argument (namargs) 
      Note: possible error in 'is.element("sigma", namargs)': unused argument (namargs) 
      Note: possible error in 'is.element(tms, names(newdata))': unused argument (names(newdata)) 
      Note: possible error in 'is.element(model, c("logistic", ': unused argument (c("logistic", "linlog")) 
    See ‘/tmp/workdir/DoseFinding/new/DoseFinding.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    fitMod.raw: possible error in is.element(modelNum, 1:4): unused
      argument (1:4)
    fitMod.raw: possible error in is.element(names(data), usedVars): unused
      argument (usedVars)
    fullMod: possible error in is.element(nm, c("emax", "exponential",
      "quadratic")): unused argument (c("emax", "exponential",
      "quadratic"))
    fullMod: possible error in is.element(nm, c("logistic", "betaMod",
      "sigEmax")): unused argument (c("logistic", "betaMod", "sigEmax"))
    getSimEst: possible error in is.element(type, c("TD", "ED")): unused
    ...
    sampSizeMCT: possible error in is.element("placAdj", namargs): unused
      argument (namargs)
    sampSizeMCT: possible error in is.element("S", namargs): unused
      argument (namargs)
    sampSizeMCT: possible error in is.element("df", namargs): unused
      argument (namargs)
    sampSizeMCT: possible error in is.element("n", namargs): unused
      argument (namargs)
    sampSizeMCT: possible error in is.element("sigma", namargs): unused
      argument (namargs)
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘analysis_normal.Rmd’ using rmarkdown
    ```

# dotsViolin

<details>

* Version: 0.0.1
* GitHub: NA
* Source code: https://github.com/cran/dotsViolin
* Date/Publication: 2023-10-30 13:20:02 UTC
* Number of recursive dependencies: 39

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

# dr4pl

<details>

* Version: 2.0.0
* GitHub: NA
* Source code: https://github.com/cran/dr4pl
* Date/Publication: 2021-08-17 16:10:18 UTC
* Number of recursive dependencies: 149

Run `revdepcheck::cloud_details(, "dr4pl")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘dr4pl-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: IC
    > ### Title: Obtain Inhibitory Concentrations (IC) of a dose-response curve
    > ### Aliases: IC
    > 
    > ### ** Examples
    > 
    > data.test <- data.frame(x = c(0.0001, 0.001, 0.01, 0.1, 1),
    +                         y = c(10, 9, 5, 1, 0))
    > obj.dr4pl <- dr4pl(y ~ x,
    +                    data = data.test)
    Error in is.element(method.init, types.method.init) : 
      unused argument (types.method.init)
    Calls: dr4pl -> dr4pl.formula -> dr4pl.default
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘walk_through_in_R.Rmd’
      ...
    + }, warning = function(war) {
    +     print(paste(sep = " ", war))
    + }, error .... [TRUNCATED] 
    [1] "simpleWarning in log(dose/parmMat[, 4]): NaNs produced\n"
    
    > dr4pl.error.1 <- dr4pl(Response ~ Dose, data = drc_error_1)
    
      When sourcing ‘walk_through_in_R.R’:
    Error: unused argument (types.method.init)
    Execution halted
    
      ‘dr4pl_derivatives.Rmd’ using ‘UTF-8’... OK
      ‘walk_through_in_R.Rmd’ using ‘UTF-8’... failed
    ```

*   checking whether package ‘dr4pl’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(trend, types.trend)': unused argument (types.trend) 
      Note: possible error in 'is.element(method.init, ': unused argument (types.method.init) 
      Note: possible error in 'is.element(method.robust, ': unused argument (types.method.robust) 
      Note: possible error in 'is.element(method.optim, ': unused argument (types.method.optim) 
      Note: possible error in 'is.element(method.robust, ': unused argument (c("absolute", "Huber")) 
    See ‘/tmp/workdir/dr4pl/new/dr4pl.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    FindInitialParms: possible error in is.element(trend, types.trend):
      unused argument (types.trend)
    FindInitialParms: possible error in is.element(method.init,
      types.method.init): unused argument (types.method.init)
    FindInitialParms: possible error in is.element(method.robust,
      types.method.robust): unused argument (types.method.robust)
    dr4pl.default: possible error in is.element(method.init,
      types.method.init): unused argument (types.method.init)
    dr4pl.default: possible error in is.element(method.optim,
      types.method.optim): unused argument (types.method.optim)
    dr4pl.default: possible error in is.element(trend, types.trend): unused
      argument (types.trend)
    dr4pl.default: possible error in is.element(method.robust,
      c("absolute", "Huber")): unused argument (c("absolute", "Huber"))
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘dr4pl_derivatives.Rmd’ using rmarkdown
    --- finished re-building ‘dr4pl_derivatives.Rmd’
    
    --- re-building ‘walk_through_in_R.Rmd’ using rmarkdown
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) confint.dr4pl.Rd:49: Escaped LaTeX specials: \&
    checkRd: (-1) gof-dr4pl.Rd:40: Escaped LaTeX specials: \&
    checkRd: (-1) vcov.dr4pl.Rd:61: Escaped LaTeX specials: \&
    ```

# DRomics

<details>

* Version: 2.6-2
* GitHub: https://github.com/lbbe-software/DRomics
* Source code: https://github.com/cran/DRomics
* Date/Publication: 2024-10-16 09:50:02 UTC
* Number of recursive dependencies: 151

Run `revdepcheck::cloud_details(, "DRomics")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘DRomics-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: bmdfilter
    > ### Title: Filtering BMDs according to estimation quality
    > ### Aliases: bmdfilter
    > 
    > ### ** Examples
    > 
    > 
    ...
    > 
    > ### (1.a) Examples on BMD.xfold (with some undefined BMD.xfold values)
    > 
    > # Plot of BMDs with no filtering
    > subres <- bmdfilter(b$res, BMDfilter = "none")
    > bmdplot(subres, BMDtype = "xfold", point.size = 3, add.CI = TRUE)
    Error in is.element(c("id", "BMD.xfold"), cnames) : 
      unused argument (cnames)
    Calls: bmdplot
    Execution halted
    ```

*   checking whether package ‘DRomics’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(c("BMD.zSD"), ': unused argument (cnames) 
      Note: possible error in 'is.element(c("BMD.zSD.upper", ': unused argument (cnames) 
      Note: possible error in 'is.element(c("BMD.xfold"), ': unused argument (cnames) 
      Note: possible error in 'is.element(c("BMD.xfold.upper", ': unused argument (cnames) 
      Note: possible error in 'is.element(c("id", "BMD.zSD"), ': unused argument (cnames) 
      Note: possible error in 'is.element(c("id", "BMD.xfold"), ': unused argument (cnames) 
      Note: possible error in 'is.element(shapeby, cnames)': unused argument (cnames) 
      Note: possible error in 'is.element(colorby, cnames)': unused argument (cnames) 
      Note: possible error in 'is.element(facetby, cnames)': unused argument (cnames) 
    ...
      Note: possible error in 'is.element(c("id", "model", ': unused argument (cnames) 
      Note: possible error in 'is.element(c("BMD.zSD", ': unused argument (cnames) 
      Note: possible error in 'is.element(c("BMD.xfold", ': unused argument (cnames) 
      Note: possible error in 'is.element(group, cnames)': unused argument (cnames) 
      Note: possible error in 'is.element(explev, cnames)': unused argument (cnames) 
      Note: possible error in 'is.element(c("trend"), ': unused argument (cnames) 
    See ‘/tmp/workdir/DRomics/new/DRomics.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    bmdfilter: possible error in is.element(c("BMD.zSD"), cnames): unused
      argument (cnames)
    bmdfilter: possible error in is.element(c("BMD.zSD.upper",
      "BMD.zSD.lower"), cnames): unused argument (cnames)
    bmdfilter: possible error in is.element(c("BMD.xfold"), cnames): unused
      argument (cnames)
    bmdfilter: possible error in is.element(c("BMD.xfold.upper",
      "BMD.xfold.lower"), cnames): unused argument (cnames)
    bmdplot: possible error in is.element(c("id", "BMD.zSD"), cnames):
      unused argument (cnames)
    ...
    sensitivityplot: possible error in is.element(group, cnames): unused
      argument (cnames)
    sensitivityplot: possible error in is.element(colorby, cnames): unused
      argument (cnames)
    trendplot: possible error in is.element(group, cnames): unused argument
      (cnames)
    trendplot: possible error in is.element(c("trend"), cnames): unused
      argument (cnames)
    trendplot: possible error in is.element(facetby, cnames): unused
      argument (cnames)
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘DRomics_vignette.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘DRomics_vignette.Rmd’
      ...
    6    8.90      5.47      8.19
    
    > plot(r, BMDtype = "zSD", plottype = "ecdf") + theme_bw()
    
    > bmdplotwithgradient(r$res, BMDtype = "zSD", facetby = "trend", 
    +     shapeby = "model", line.size = 1.2, scaling = TRUE)
    
      When sourcing ‘DRomics_vignette.R’:
    Error: unused argument (cnames)
    Execution halted
    
      ‘DRomics_vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        doc   2.5Mb
    ```

# drpop

<details>

* Version: 0.0.3
* GitHub: NA
* Source code: https://github.com/cran/drpop
* Date/Publication: 2021-11-05 21:10:02 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::cloud_details(, "drpop")` for more info

</details>

## Newly broken

*   checking whether package ‘drpop’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(condvar, colnames(data))': unused argument (colnames(data)) 
    See ‘/tmp/workdir/drpop/new/drpop.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    popsize_cond: possible error in is.element(condvar, colnames(data)):
      unused argument (colnames(data))
    ```

# duke

<details>

* Version: 0.0.3
* GitHub: https://github.com/aidangildea/duke
* Source code: https://github.com/cran/duke
* Date/Publication: 2023-12-15 21:50:16 UTC
* Number of recursive dependencies: 88

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
      
      [ FAIL 1 | WARN 9 | SKIP 0 | PASS 27 ]
      Error: Test failures
      Execution halted
    ```

# dymo

<details>

* Version: 1.1.0
* GitHub: NA
* Source code: https://github.com/cran/dymo
* Date/Publication: 2022-05-05 08:00:02 UTC
* Number of recursive dependencies: 147

Run `revdepcheck::cloud_details(, "dymo")` for more info

</details>

## Newly broken

*   checking whether package ‘dymo’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::is.scale’ by ‘greybox::is.scale’ when loading ‘dymo’
    See ‘/tmp/workdir/dymo/new/dymo.Rcheck/00install.out’ for details.
    ```

# dynr

<details>

* Version: 0.1.16-105
* GitHub: https://github.com/mhunter1/dynr
* Source code: https://github.com/cran/dynr
* Date/Publication: 2023-11-28 05:20:05 UTC
* Number of recursive dependencies: 148

Run `revdepcheck::cloud_details(, "dynr")` for more info

</details>

## Newly broken

*   checking whether package ‘dynr’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(id, id_toplot)': unused argument (id_toplot) 
    See ‘/tmp/workdir/dynr/new/dynr.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    autoplot.dynrTaste: possible error in is.element(id, id_toplot): unused
      argument (id_toplot)
    ```

## In both

*   checking Rd files ... WARNING
    ```
    prepare_Rd: ABI version mismatch: 
    lme4 was built with Matrix ABI version 1
    Current Matrix ABI version is 0
    Please re-install lme4 from source or restore original ‘Matrix’ package
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 10.1Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        data   6.0Mb
        doc    1.4Mb
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘linearSDE.Rmd’ using rmarkdown
    --- finished re-building ‘linearSDE.Rmd’
    
    --- re-building ‘InstallationForDevelopers.Rnw’ using Sweave
    Error: processing vignette 'InstallationForDevelopers.Rnw' failed with diagnostics:
    Running 'texi2dvi' on 'InstallationForDevelopers.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `menukeys.sty' not found.
    
    ...
                   {bm}%bm^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘NonlinearContinuousTimeModels.Rnw’
    
    SUMMARY: processing the following files failed:
      ‘InstallationForDevelopers.Rnw’ ‘InstallationForUsers.Rnw’
      ‘LinearDiscreteTimeModels.Rnw’ ‘NonlinearContinuousTimeModels.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# easysurv

<details>

* Version: 2.0.1
* GitHub: https://github.com/Maple-Health-Group/easysurv
* Source code: https://github.com/cran/easysurv
* Date/Publication: 2024-06-21 10:30:06 UTC
* Number of recursive dependencies: 155

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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘easysurv.Rmd’
      ...
             Median follow-up
    Tab+Vis          2.217659
    Tab->Vis         2.220397
    Tab              2.308008
    Vis              2.198494
    
    
      When sourcing ‘easysurv.R’:
    Error: argument "risktable_height" is missing, with no default
    Execution halted
    
      ‘easysurv.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘easysurv.Rmd’ using rmarkdown
    
    Quitting from lines 149-157 [km] (easysurv.Rmd)
    Error: processing vignette 'easysurv.Rmd' failed with diagnostics:
    argument "risktable_height" is missing, with no default
    --- failed re-building ‘easysurv.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘easysurv.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# EGM

<details>

* Version: 0.1.0
* GitHub: https://github.com/shah-in-boots/EGM
* Source code: https://github.com/cran/EGM
* Date/Publication: 2024-05-23 16:10:05 UTC
* Number of recursive dependencies: 76

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
      
      [ FAIL 1 | WARN 0 | SKIP 19 | PASS 43 ]
      Error: Test failures
      Execution halted
    ```

# entropart

<details>

* Version: 1.6-15
* GitHub: https://github.com/EricMarcon/entropart
* Source code: https://github.com/cran/entropart
* Date/Publication: 2024-08-26 19:30:09 UTC
* Number of recursive dependencies: 128

Run `revdepcheck::cloud_details(, "entropart")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘entropart-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Accumulation
    > ### Title: Diversity accumulation.
    > ### Aliases: DivAC EntAC as.AccumCurve is.AccumCurve autoplot.AccumCurve
    > ###   plot.AccumCurve
    > 
    > ### ** Examples
    > 
    ...
     11.       │ └─base::withCallingHandlers(...)
     12.       └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13.         └─l$compute_geom_2(d, theme = plot$theme)
     14.           └─ggplot2 (local) compute_geom_2(..., self = self)
     15.             └─self$geom$use_defaults(...)
     16.               └─ggplot2 (local) use_defaults(..., self = self)
     17.                 └─ggplot2:::check_aesthetics(new_params, nrow(data))
     18.                   └─cli::cli_abort(...)
     19.                     └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘entropart.Rmd’
      ...
    
    > autoplot(Abd18, Distribution = "lnorm")
    
      When sourcing ‘entropart.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `check_aesthetics()`:
    ! Aesthetics must be either length 1 or the same as the data (149).
    ✖ Fix the following mappings: `shape`, `colour`, and `size`.
    Execution halted
    
      ‘entropart.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘entropart.Rmd’ using rmarkdown
    
    Quitting from lines 53-55 [PlotN18] (entropart.Rmd)
    Error: processing vignette 'entropart.Rmd' failed with diagnostics:
    Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `check_aesthetics()`:
    ! Aesthetics must be either length 1 or the same as the data (149).
    ✖ Fix the following mappings: `shape`, `colour`, and `size`.
    --- failed re-building ‘entropart.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘entropart.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# epiCleanr

<details>

* Version: 0.2.0
* GitHub: https://github.com/truenomad/epiCleanr
* Source code: https://github.com/cran/epiCleanr
* Date/Publication: 2023-09-28 12:20:05 UTC
* Number of recursive dependencies: 129

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
     12. │     └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13. │       └─l$compute_geom_2(d, theme = plot$theme)
     14. │         └─ggplot2 (local) compute_geom_2(..., self = self)
     15. │           └─self$geom$use_defaults(...)
     16. └─base::.handleSimpleError(...)
     17.   └─rlang (local) h(simpleError(msg, call))
     18.     └─handlers[[1L]](cnd)
     19.       └─cli::cli_abort(...)
     20.         └─rlang::abort(...)
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
* Number of recursive dependencies: 91

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
* Number of recursive dependencies: 131

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

# esci

<details>

* Version: 1.0.6
* GitHub: https://github.com/rcalinjageman/esci
* Source code: https://github.com/cran/esci
* Date/Publication: 2024-12-21 16:00:02 UTC
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "esci")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘esci-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: estimate_mdiff_2x2_between
    > ### Title: Estimates for a 2x2 between-subjects design with a continuous
    > ###   outcome variable
    > ### Aliases: estimate_mdiff_2x2_between
    > 
    > ### ** Examples
    > 
    ...
    +   estimates_from_summary$interaction,
    +   effect_size = "mean"
    + )
    Warning: Using size for a discrete variable is not advised.
    Warning: Using alpha for a discrete variable is not advised.
    Error in use_defaults(..., self = self) : 
      unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, 
        NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, NULL, 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, 
    Calls: <Anonymous> ... <Anonymous> -> <Anonymous> -> compute_geom_2 -> <Anonymous>
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(esci)
      > 
      > test_check("esci")
      [1] "WARNING: ratio-scale scores cannot be negative"
      [1] "WARNING: ratio-scale scores cannot be negative"
      [1] "WARNING: ratio-scale scores cannot be negative"
    ...
       17. │             └─self$geom$use_defaults(...)
       18. └─base::.handleSimpleError(...)
       19.   └─rlang (local) h(simpleError(msg, call))
       20.     └─handlers[[1L]](cnd)
       21.       └─cli::cli_abort(...)
       22.         └─rlang::abort(...)
      
      [ FAIL 3 | WARN 0 | SKIP 52 | PASS 2889 ]
      Error: Test failures
      Execution halted
    ```

# evalITR

<details>

* Version: 1.0.0
* GitHub: https://github.com/MichaelLLi/evalITR
* Source code: https://github.com/cran/evalITR
* Date/Publication: 2023-08-25 23:10:06 UTC
* Number of recursive dependencies: 166

Run `revdepcheck::cloud_details(, "evalITR")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘cv_multiple_alg.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘cv_multiple_alg.Rmd’
      ...
        intersect, setdiff, setequal, union
    
    
    > load("../data/star.rda")
    Warning in readChar(con, 5L, useBytes = TRUE) :
      cannot open compressed file '../data/star.rda', probable reason 'No such file or directory'
    
    ...
    Execution halted
    
      ‘cv_multiple_alg.Rmd’ using ‘UTF-8’... failed
      ‘cv_single_alg.Rmd’ using ‘UTF-8’... failed
      ‘install.Rmd’ using ‘UTF-8’... OK
      ‘paper_alg1.Rmd’ using ‘UTF-8’... OK
      ‘sample_split.Rmd’ using ‘UTF-8’... failed
      ‘sample_split_caret.Rmd’ using ‘UTF-8’... failed
      ‘user_itr.Rmd’ using ‘UTF-8’... failed
      ‘user_itr_algs.Rmd’ using ‘UTF-8’... failed
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘forcats’ ‘rqPen’ ‘utils’
      All declared Imports should be used.
    ```

# eventstudyr

<details>

* Version: 1.1.3
* GitHub: https://github.com/JMSLab/eventstudyr
* Source code: https://github.com/cran/eventstudyr
* Date/Publication: 2024-03-04 15:00:02 UTC
* Number of recursive dependencies: 100

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

# expirest

<details>

* Version: 0.1.6
* GitHub: https://github.com/piusdahinden/expirest
* Source code: https://github.com/cran/expirest
* Date/Publication: 2024-03-25 16:30:02 UTC
* Number of recursive dependencies: 45

Run `revdepcheck::cloud_details(, "expirest")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(expirest)
      > 
      > test_check("expirest")
      [ FAIL 9 | WARN 0 | SKIP 0 | PASS 1122 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      ── Failure ('test-plot_expirest_wisle.R:260:3'): plot_expirest_wisle_succeeds ──
      tmp4l2[["Graph"]]$labels has length 0, not length 8.
      ── Failure ('test-plot_expirest_wisle.R:264:3'): plot_expirest_wisle_succeeds ──
      tmp4b1[["Graph"]]$labels has length 0, not length 5.
      ── Failure ('test-plot_expirest_wisle.R:269:3'): plot_expirest_wisle_succeeds ──
      tmp4b2[["Graph"]]$labels has length 0, not length 5.
      
      [ FAIL 9 | WARN 0 | SKIP 0 | PASS 1122 ]
      Error: Test failures
      Execution halted
    ```

# explainer

<details>

* Version: 1.0.2
* GitHub: https://github.com/PERSIMUNE/explainer
* Source code: https://github.com/cran/explainer
* Date/Publication: 2024-09-30 17:30:02 UTC
* Number of recursive dependencies: 186

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

# ezEDA

<details>

* Version: 0.1.1
* GitHub: https://github.com/kviswana/ezEDA
* Source code: https://github.com/cran/ezEDA
* Date/Publication: 2021-06-29 04:40:10 UTC
* Number of recursive dependencies: 79

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

# fable.prophet

<details>

* Version: 0.1.0
* GitHub: https://github.com/mitchelloharawild/fable.prophet
* Source code: https://github.com/cran/fable.prophet
* Date/Publication: 2020-08-20 09:30:03 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::cloud_details(, "fable.prophet")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘intro.Rmd’
      ...
     9 Domestic mdl    2019 Dec sample[5000] 5336974.
    10 Domestic mdl    2020 Jan sample[5000] 4886604.
    # ℹ 62 more rows
    
    > fc %>% autoplot(lax_passengers)
    
      When sourcing ‘intro.R’:
    Error: unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, 
        NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "#4D4D4DFF", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, N
    Execution halted
    
      ‘intro.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘intro.Rmd’ using rmarkdown
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# fabletools

<details>

* Version: 0.5.0
* GitHub: https://github.com/tidyverts/fabletools
* Source code: https://github.com/cran/fabletools
* Date/Publication: 2024-09-17 07:30:02 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::cloud_details(, "fabletools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘fabletools-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: autoplot.fbl_ts
    > ### Title: Plot a set of forecasts
    > ### Aliases: autoplot.fbl_ts autolayer.fbl_ts
    > 
    > ### ** Examples
    > 
    > ## Don't show: 
    ...
    > library(fable)
    > library(tsibbledata)
    > fc <- aus_production %>% model(ets = ETS(log(Beer) ~ error("M") + trend("Ad") + 
    +     season("A"))) %>% forecast(h = "3 years")
    > fc %>% autoplot(aus_production)
    Error in use_defaults(..., self = self) : 
      unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, 
        NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "#4D4D4DFF", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL,
    Calls: <Anonymous> ... <Anonymous> -> <Anonymous> -> compute_geom_2 -> <Anonymous>
    Execution halted
    ```

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
       28.                                   └─ggplot2 (local) compute_geom_2(..., self = self)
       29.                                     └─self$geom$use_defaults(...)
      ── Failure ('test-graphics.R:327:3'): autoplot_dcmp_ts() ───────────────────────
      `print(p)` produced warnings.
      ── Failure ('test-graphics.R:346:3'): autoplot_dcmp_ts() ───────────────────────
      `print(p)` produced warnings.
      
      [ FAIL 4 | WARN 5 | SKIP 1 | PASS 266 ]
      Error: Test failures
      Execution halted
    ```

# fairmodels

<details>

* Version: 1.2.1
* GitHub: https://github.com/ModelOriented/fairmodels
* Source code: https://github.com/cran/fairmodels
* Date/Publication: 2022-08-23 19:50:06 UTC
* Number of recursive dependencies: 86

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

# FCPS

<details>

* Version: 1.3.4
* GitHub: https://github.com/Mthrun/FCPS
* Source code: https://github.com/cran/FCPS
* Date/Publication: 2023-10-19 13:20:02 UTC
* Number of recursive dependencies: 269

Run `revdepcheck::cloud_details(, "FCPS")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘FCPS-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: SOTAclustering
    > ### Title: SOTA Clustering
    > ### Aliases: SOTAclustering sotaClustering
    > ### Keywords: SOTAclustering
    > 
    > ### ** Examples
    > 
    > #Does Work
    > data('Hepta')
    > out=SOTAclustering(Hepta$Data,ClusterNo=7)
    Error in is.element(i, clust) : unused argument (clust)
    Calls: SOTAclustering -> sota -> trainLeaves
    Execution halted
    ```

*   checking whether package ‘FCPS’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(i, clust)': unused argument (clust) 
    See ‘/tmp/workdir/FCPS/new/FCPS.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    trainLeaves: possible error in is.element(i, clust): unused argument
      (clust)
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.1Mb
      sub-directories of 1Mb or more:
        data   6.5Mb
        doc    3.9Mb
    ```

# fdANOVA

<details>

* Version: 0.1.2
* GitHub: NA
* Source code: https://github.com/cran/fdANOVA
* Date/Publication: 2018-08-29 19:54:26 UTC
* Number of recursive dependencies: 70

Run `revdepcheck::cloud_details(, "fdANOVA")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘fdANOVA-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: fanova.tests
    > ### Title: Tests for FANOVA Problem
    > ### Aliases: fanova.tests
    > ### Keywords: Multivariate Test
    > 
    > ### ** Examples
    > 
    ...
    +                                                      own.basis = own.basis,
    +                                                      own.cross.prod.mat = own.cross.prod.mat)))
    > 
    > # the tests based on random projections with the Gaussian white noise generated for projections
    > fanova4 <- fanova.tests(x.gait, group.label.gait, test = "TRP",
    +                         params = list(paramTRP = list(k = c(1, 2), B.TRP = 2)))
    Error in is.element(substring(formc, 1, 1), c("+", "-")) : 
      unused argument (c("+", "-"))
    Calls: fanova.tests ... pf -> anova.statistic.quick -> <Anonymous> -> orderBy
    Execution halted
    ```

# fddm

<details>

* Version: 1.0-2
* GitHub: https://github.com/rtdists/fddm
* Source code: https://github.com/cran/fddm
* Date/Publication: 2024-07-02 16:00:07 UTC
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "fddm")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘benchmark.Rmd’
      ...
    > mi <- min(bm_vec[, -seq_len(t_idx)])
    
    > ma <- max(bm_vec[, (t_idx + 1):(ncol(bm_vec) - 4)])
    
    > ggplot(mbm_vec, aes(x = factor(FuncName, levels = Names_vec), 
    +     y = time, color = factor(FuncName, levels = Names_vec), fill = factor(FuncName, .... [TRUNCATED] 
    
    ...
    
      When sourcing ‘pfddm.R’:
    Error: Not a unit object
    Execution halted
    
      ‘benchmark.Rmd’ using ‘UTF-8’... failed
      ‘example.Rmd’ using ‘UTF-8’... OK
      ‘math.Rmd’ using ‘UTF-8’... OK
      ‘pfddm.Rmd’ using ‘UTF-8’... failed
      ‘validity.Rmd’ using ‘UTF-8’... OK
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 16.6Mb
      sub-directories of 1Mb or more:
        doc    1.6Mb
        libs  14.1Mb
    ```

# feasts

<details>

* Version: 0.4.1
* GitHub: https://github.com/tidyverts/feasts
* Source code: https://github.com/cran/feasts
* Date/Publication: 2024-09-25 23:40:02 UTC
* Number of recursive dependencies: 100

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
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 100 ]
      
    ...
      ── Error ('test-graphics.R:192:3'): gg_tsdisplay() plots ───────────────────────
      Error in `p + ggplot2::labs(x = "x", y = "y", title = "title")`: non-numeric argument to binary operator
      ── Failure ('test-graphics.R:273:3'): gg_arma() plots ──────────────────────────
      p_built$plot$labels[c("x", "y")] not equivalent to list(x = "Re(1/root)", y = "Im(1/root)").
      Component "x": 1 string mismatch
      Component "y": 1 string mismatch
      
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 100 ]
      Error: Test failures
      Execution halted
    ```

# ffp

<details>

* Version: 0.2.2
* GitHub: https://github.com/Reckziegel/FFP
* Source code: https://github.com/cran/ffp
* Date/Publication: 2022-09-29 15:10:06 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "ffp")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ffp-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: scenario_density
    > ### Title: Plot Scenarios
    > ### Aliases: scenario_density scenario_histogram
    > 
    > ### ** Examples
    > 
    > x <- diff(log(EuStockMarkets))[, 1]
    > p <- exp_decay(x, 0.005)
    > 
    > scenario_density(x, p, 500)
    Error in use_defaults(..., self = self) : 
      unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, 
        NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "#4D4D4DFF", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(), NU
    Calls: <Anonymous> ... <Anonymous> -> <Anonymous> -> compute_geom_2 -> <Anonymous>
    Execution halted
    ```

# fido

<details>

* Version: 1.1.1
* GitHub: https://github.com/jsilve24/fido
* Source code: https://github.com/cran/fido
* Date/Publication: 2024-06-05 21:30:06 UTC
* Number of recursive dependencies: 133

Run `revdepcheck::cloud_details(, "fido")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘fido-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.pibblefit
    > ### Title: Plot Summaries of Posterior Distribution of pibblefit Parameters
    > ### Aliases: plot.pibblefit
    > 
    > ### ** Examples
    > 
    > sim <- pibble_sim(N=10, D=4, Q=3)
    > fit <- pibble(sim$Y, sim$X)
    > plot(fit, par="Lambda")
    Scale for colour is already present.
    Adding another scale for colour, which will replace the existing scale.
    Error in use_defaults(..., self = self) : 
      unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, 
        NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "#4D4D4DFF", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), N
    Calls: <Anonymous> ... <Anonymous> -> <Anonymous> -> compute_geom_2 -> <Anonymous>
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(fido)
      > 
      > #Sys.setenv(KMP_DUPLICATE_LIB_OK="TRUE")
      > test_check("fido")
        [1]  0.27980164 -0.69169550 -0.53205652  0.11488451 -0.42419872  2.20261388
        [7] -1.62190133 -0.90893172  0.07891428  0.75060681  0.43593605  0.26819442
    ...
       21.                           └─base::Map(...)
       22.                             └─base::mapply(FUN = f, ..., SIMPLIFY = FALSE)
       23.                               └─ggplot2 (local) `<fn>`(layer = dots[[1L]][[1L]], df = dots[[2L]][[1L]])
       24.                                 └─layer$compute_geom_2(key, single_params, theme)
       25.                                   └─ggplot2 (local) compute_geom_2(..., self = self)
       26.                                     └─self$geom$use_defaults(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 114 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘non-linear-models.Rmd’
      ...
    
    The following object is masked from ‘package:dplyr’:
    
        select
    
    
      When sourcing ‘non-linear-models.R’:
    Error: package or namespace load failed for ‘MCMCpack’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
     namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
    Execution halted
    
      ‘introduction-to-fido.Rmd’ using ‘UTF-8’... OK
      ‘mitigating-pcrbias.Rmd’ using ‘UTF-8’... OK
      ‘non-linear-models.Rmd’ using ‘UTF-8’... failed
      ‘orthus.Rmd’ using ‘UTF-8’... OK
      ‘picking_priors.Rmd’ using ‘UTF-8’... OK
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 106.6Mb
      sub-directories of 1Mb or more:
        data    4.0Mb
        libs  100.8Mb
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘introduction-to-fido.Rmd’ using rmarkdown
    --- finished re-building ‘introduction-to-fido.Rmd’
    
    --- re-building ‘mitigating-pcrbias.Rmd’ using rmarkdown
    --- finished re-building ‘mitigating-pcrbias.Rmd’
    
    --- re-building ‘non-linear-models.Rmd’ using rmarkdown
    ```

# FielDHub

<details>

* Version: 1.4.2
* GitHub: https://github.com/DidierMurilloF/FielDHub
* Source code: https://github.com/cran/FielDHub
* Date/Publication: 2024-07-26 20:20:06 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::cloud_details(, "FielDHub")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘RCBD_augmented.Rmd’
      ...
    7   7 Cassava_2022    FARGO 2025 1007   1      7      1     1     4       CH4
    8   8 Cassava_2022    FARGO 2025 1008   1      8      0     1    44       G44
    9   9 Cassava_2022    FARGO 2025 1009   1      9      0     1    23       G23
    10 10 Cassava_2022    FARGO 2025 1010   1     10      0     1   113      G113
    
    > plot(aug_RCBD)
    
    ...
    
      ‘RCBD_augmented.Rmd’ using ‘UTF-8’... failed
      ‘alpha_lattice.Rmd’ using ‘UTF-8’... failed
      ‘diagonal_arrangement.Rmd’ using ‘UTF-8’... failed
      ‘full_factorial.Rmd’ using ‘UTF-8’... failed
      ‘multi_location_prep.Rmd’ using ‘UTF-8’... failed
      ‘partially_replicated.Rmd’ using ‘UTF-8’... failed
      ‘rcbd.Rmd’ using ‘UTF-8’... failed
      ‘row_column.Rmd’ using ‘UTF-8’... failed
      ‘split_plot.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘RCBD_augmented.Rmd’ using rmarkdown
    
    Quitting from lines 210-211 [unnamed-chunk-12] (RCBD_augmented.Rmd)
    Error: processing vignette 'RCBD_augmented.Rmd' failed with diagnostics:
    unused argument (dn)
    --- failed re-building ‘RCBD_augmented.Rmd’
    
    --- re-building ‘alpha_lattice.Rmd’ using rmarkdown
    
    ...
    --- failed re-building ‘split_plot.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘RCBD_augmented.Rmd’ ‘alpha_lattice.Rmd’ ‘diagonal_arrangement.Rmd’
      ‘full_factorial.Rmd’ ‘multi_location_prep.Rmd’
      ‘partially_replicated.Rmd’ ‘rcbd.Rmd’ ‘row_column.Rmd’
      ‘split_plot.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        doc    1.8Mb
        help   1.1Mb
    ```

# figuRes2

<details>

* Version: 1.0.0
* GitHub: https://github.com/gcicc/figures2
* Source code: https://github.com/cran/figuRes2
* Date/Publication: 2022-09-09 08:02:55 UTC
* Number of recursive dependencies: 112

Run `revdepcheck::cloud_details(, "figuRes2")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘figuRes2-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: theme_grey2_nomargins
    > ### Title: figuRes2 themes
    > ### Aliases: theme_grey2_nomargins theme_grey2_default_margins
    > ###   theme_bw2_nomargins theme_bw2_default_margins theme_table_nomargins
    > 
    > ### ** Examples
    > 
    ...
        ▆
     1. ├─ggplot2::theme_set(theme_grey2_nomargins())
     2. │ └─ggplot2:::check_object(new, is.theme, "a {.cls theme} object")
     3. │   └─ggplot2 (local) check_fun(x)
     4. └─figuRes2::theme_grey2_nomargins()
     5.   └─ggplot2::theme(...)
     6.     └─lifecycle::deprecate_stop(...)
     7.       └─lifecycle:::deprecate_stop0(msg)
     8.         └─rlang::cnd_signal(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘basics.Rmd’
      ...
    Your page setup allocates: 0.1 inches to the bottom margin.
    Your page setup allocates: a rectangle, 10.8 inches wide by 8.3 inches tall for graphics/tables.
    
    > theme_set(theme_grey2_nomargins())
    
      When sourcing ‘basics.R’:
    Error: The `axis.ticks.margin` argument of `theme()` was deprecated in ggplot2
    2.0.0 and is now defunct.
    ℹ Please set `margin` property of `axis.text` instead
    Execution halted
    
      ‘basics.Rmd’ using ‘UTF-8’... failed
      ‘forest-plots.Rmd’ using ‘UTF-8’... OK
      ‘km.Rmd’ using ‘UTF-8’... OK
      ‘large-scale.Rmd’ using ‘UTF-8’... OK
    ```

# flexsurv

<details>

* Version: 2.3.2
* GitHub: https://github.com/chjackson/flexsurv
* Source code: https://github.com/cran/flexsurv
* Date/Publication: 2024-08-17 05:50:02 UTC
* Number of recursive dependencies: 150

Run `revdepcheck::cloud_details(, "flexsurv")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘test_base.R’
    Running the tests in ‘tests/test_base.R’ failed.
    Complete output:
      > test_partial <- FALSE
      > 
      > if (test_partial)
      +   options(
      +     warnPartialMatchArgs = TRUE,
      +     warnPartialMatchAttr = TRUE,
      +     warnPartialMatchDollar = TRUE
    ...
      ══ Skipped tests (1) ═══════════════════════════════════════════════════════════
      • On CRAN (1): 'test_standsurv.R:276:3'
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test_custom.R:14:1'): (code run outside of `test_that()`) ───────────
      Error in `is.element("eha", installed.packages()[, 1])`: unused argument (installed.packages()[, 1])
      
      [ FAIL 1 | WARN 0 | SKIP 1 | PASS 705 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘standsurv.Rmd’
      ...
    
    > kmsurvplot <- ggsurvplot(km)
    
    > kmsurvplot + xlab("Time from diagnosis (years)")
    Warning in eval(ei, envir) :
      Incompatible methods ("+.ggsurv", "+.gg") for "+"
    
      When sourcing ‘standsurv.R’:
    Error: non-numeric argument to binary operator
    Execution halted
    
      ‘standsurv.Rmd’ using ‘UTF-8’... failed
      ‘flexsurv.Rnw’ using ‘UTF-8’... OK
      ‘multistate.Rnw’ using ‘UTF-8’... OK
      ‘distributions.Rnw’ using ‘UTF-8’... OK
      ‘flexsurv-examples.Rnw’ using ‘UTF-8’... OK
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.4Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        doc    1.9Mb
        libs   3.4Mb
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘standsurv.Rmd’ using rmarkdown
    
    Quitting from lines 113-116 [unnamed-chunk-4] (standsurv.Rmd)
    Error: processing vignette 'standsurv.Rmd' failed with diagnostics:
    non-numeric argument to binary operator
    --- failed re-building ‘standsurv.Rmd’
    
    --- re-building ‘flexsurv.Rnw’ using knitr
    --- finished re-building ‘flexsurv.Rnw’
    ...
                                         ^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘flexsurv-examples.Rnw’
    
    SUMMARY: processing the following files failed:
      ‘standsurv.Rmd’ ‘multistate.Rnw’ ‘distributions.Rnw’
      ‘flexsurv-examples.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# flipr

<details>

* Version: 0.3.3
* GitHub: https://github.com/LMJL-Alea/flipr
* Source code: https://github.com/cran/flipr
* Date/Publication: 2023-08-23 09:00:02 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "flipr")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘alternative.Rmd’ using rmarkdown
    --- finished re-building ‘alternative.Rmd’
    
    --- re-building ‘exactness.Rmd’ using rmarkdown
    
    Quitting from lines 142-177 [unnamed-chunk-1] (exactness.Rmd)
    Error: processing vignette 'exactness.Rmd' failed with diagnostics:
    subscript out of bounds
    --- failed re-building ‘exactness.Rmd’
    
    --- re-building ‘flipr.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘exactness.Rmd’
      ...
    
    > library(flipr)
    
    > load("../R/sysdata.rda")
    Warning in readChar(con, 5L, useBytes = TRUE) :
      cannot open compressed file '../R/sysdata.rda', probable reason 'No such file or directory'
    
    ...
      cannot open compressed file '../R/sysdata.rda', probable reason 'No such file or directory'
    
      When sourcing ‘plausibility.R’:
    Error: cannot open the connection
    Execution halted
    
      ‘alternative.Rmd’ using ‘UTF-8’... OK
      ‘exactness.Rmd’ using ‘UTF-8’... failed
      ‘flipr.Rmd’ using ‘UTF-8’... failed
      ‘plausibility.Rmd’ using ‘UTF-8’... failed
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 11.2Mb
      sub-directories of 1Mb or more:
        doc    9.1Mb
        libs   1.4Mb
    ```

# forestmodel

<details>

* Version: 0.6.2
* GitHub: NA
* Source code: https://github.com/cran/forestmodel
* Date/Publication: 2020-07-19 11:50:03 UTC
* Number of recursive dependencies: 57

Run `revdepcheck::cloud_details(, "forestmodel")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘forestmodel-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: forest_rma
    > ### Title: Generate a forest plot from a meta-analysis
    > ### Aliases: forest_rma
    > 
    > ### ** Examples
    > 
    > if (require("metafor")) {
    ...
    Loading required package: metadat
    Loading required package: numDeriv
    
    Loading the 'metafor' package (version 4.6-0). For an
    introduction to the package please type: help(metafor)
    
    Error in h(simpleError(msg, call)) : 
      error in evaluating the argument 'x' in selecting a method for function 'print': unused argument (c("OR", "PETO", "D2OR", "D2ORN", "D2ORL"))
    Calls: print ... default_forest_panels -> %||% -> rma_setlab -> .handleSimpleError -> h
    Execution halted
    ```

*   checking whether package ‘forestmodel’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(measure, c("OR", ': unused argument (c("OR", "PETO", "D2OR", "D2ORN", "D2ORL")) 
      Note: possible error in 'is.element(measure, c("SMD", ': unused argument (c("SMD", "SMDH", "PBIT", "OR2D", "OR2DN", "OR2DL")) 
      Note: possible error in 'is.element(measure, c("COR", ': unused argument (c("COR", "UCOR", "RTET", "RBIS")) 
      Note: possible error in 'is.element(measure, c("SMCC", ': unused argument (c("SMCC", "SMCR", "SMCRH")) 
    See ‘/tmp/workdir/forestmodel/new/forestmodel.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    rma_setlab: possible error in is.element(measure, c("OR", "PETO",
      "D2OR", "D2ORN", "D2ORL")): unused argument (c("OR", "PETO", "D2OR",
      "D2ORN", "D2ORL"))
    rma_setlab: possible error in is.element(measure, c("SMD", "SMDH",
      "PBIT", "OR2D", "OR2DN", "OR2DL")): unused argument (c("SMD", "SMDH",
      "PBIT", "OR2D", "OR2DN", "OR2DL"))
    rma_setlab: possible error in is.element(measure, c("COR", "UCOR",
      "RTET", "RBIS")): unused argument (c("COR", "UCOR", "RTET", "RBIS"))
    rma_setlab: possible error in is.element(measure, c("SMCC", "SMCR",
      "SMCRH")): unused argument (c("SMCC", "SMCR", "SMCRH"))
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# frailtyEM

<details>

* Version: 1.0.1
* GitHub: https://github.com/tbalan/frailtyEM
* Source code: https://github.com/cran/frailtyEM
* Date/Publication: 2019-09-22 13:00:10 UTC
* Number of recursive dependencies: 78

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

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘frailtyEM_manual.Rnw’ using Sweave
    Loading required package: survival
    
    Attaching package: ‘ggplot2’
    
    The following object is masked from ‘package:base’:
    
        is.element
    
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

# gapmap

<details>

* Version: 1.0.0
* GitHub: https://github.com/evanbiederstedt/gapmap
* Source code: https://github.com/cran/gapmap
* Date/Publication: 2024-01-22 20:50:02 UTC
* Number of recursive dependencies: 55

Run `revdepcheck::cloud_details(, "gapmap")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gapmap-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gapmap
    > ### Title: Function to draw a gapped cluster heatmap
    > ### Aliases: gapmap
    > 
    > ### ** Examples
    > 
    > set.seed(1234)
    ...
    ℹ Please use the `panel.spacing` argument instead.
    Backtrace:
        ▆
     1. └─gapmap::gapmap(m = as.matrix(distxy), d_row = rev(dend), d_col = dend)
     2.   └─gapmap::gap_dendrogram(...)
     3.     └─ggplot2::theme(...)
     4.       └─lifecycle::deprecate_stop("2.2.0", "theme(panel.margin)", "theme(panel.spacing)")
     5.         └─lifecycle:::deprecate_stop0(msg)
     6.           └─rlang::cnd_signal(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘simple_example.Rmd’
      ...
    ℹ See also `vignette("ggplot2-in-packages")` for more information.
    ℹ The deprecated feature was likely used in the gapmap package.
      Please report the issue at
      <https://github.com/evanbiederstedt/gapmap/issues>.
    
      When sourcing ‘simple_example.R’:
    Error: The `panel.margin` argument of `theme()` was deprecated in ggplot2 2.2.0
    ...
      <https://github.com/evanbiederstedt/gapmap/issues>.
    
      When sourcing ‘tcga_example.R’:
    Error: The `panel.margin` argument of `theme()` was deprecated in ggplot2 2.2.0
    and is now defunct.
    ℹ Please use the `panel.spacing` argument instead.
    Execution halted
    
      ‘simple_example.Rmd’ using ‘UTF-8’... failed
      ‘tcga_example.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘simple_example.Rmd’ using rmarkdown
    
    Quitting from lines 36-38 [unnamed-chunk-3] (simple_example.Rmd)
    Error: processing vignette 'simple_example.Rmd' failed with diagnostics:
    The `panel.margin` argument of `theme()` was deprecated in ggplot2 2.2.0
    and is now defunct.
    ℹ Please use the `panel.spacing` argument instead.
    --- failed re-building ‘simple_example.Rmd’
    ...
    The `panel.margin` argument of `theme()` was deprecated in ggplot2 2.2.0
    and is now defunct.
    ℹ Please use the `panel.spacing` argument instead.
    --- failed re-building ‘tcga_example.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘simple_example.Rmd’ ‘tcga_example.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# gapminder

<details>

* Version: 1.0.0
* GitHub: https://github.com/jennybc/gapminder
* Source code: https://github.com/cran/gapminder
* Date/Publication: 2023-03-10 09:50:08 UTC
* Number of recursive dependencies: 65

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

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘gapminder.Rmd’
      ...
    
    > head(continent_colors)
       Africa  Americas      Asia    Europe   Oceania 
    "#7F3B08" "#A50026" "#40004B" "#276419" "#313695" 
    
    > knitr::include_graphics("../man/figures/gapminder-color-scheme-ggplot2.png")
    
      When sourcing ‘gapminder.R’:
    Error: Cannot find the file(s): "../man/figures/gapminder-color-scheme-ggplot2.png"
    Execution halted
    
      ‘gapminder.Rmd’ using ‘UTF-8’... failed
    ```

# GEInter

<details>

* Version: 0.3.2
* GitHub: NA
* Source code: https://github.com/cran/GEInter
* Date/Publication: 2022-05-19 06:50:19 UTC
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "GEInter")` for more info

</details>

## Newly broken

*   checking whether package ‘GEInter’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(variable[t], ': unused argument (variable[1:(t - 1)]) 
      Note: possible error in 'is.element(E_type[1:q], ': unused argument (c("ED")) 
      Note: possible error in 'is.element(fit$v_type[1:q], ': unused argument (c("ED")) 
      Note: possible error in 'is.element(b[, 1], a[1])': unused argument (a[1]) 
      Note: possible error in 'is.element(b[, 2], a[2])': unused argument (a[2]) 
    See ‘/tmp/workdir/GEInter/new/GEInter.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    RobSBoosting: possible error in is.element(variable[t], variable[1:(t -
      1)]): unused argument (variable[1:(t - 1)])
    plot.Miss.boosting: possible error in is.element(E_type[1:q], c("ED")):
      unused argument (c("ED"))
    plot.RobSBoosting: possible error in is.element(fit$v_type[1:q],
      c("ED")): unused argument (c("ED"))
    which.column: possible error in is.element(b[, 1], a[1]): unused
      argument (a[1])
    which.column: possible error in is.element(b[, 2], a[2]): unused
      argument (a[2])
    ```

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘GEInter-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Miss.boosting
    > ### Title: Robust gene-environment interaction analysis approach via sparse
    > ###   boosting, where the missingness in environmental measurements is
    > ###   effectively accommodated using multiple imputation approach
    > ### Aliases: Miss.boosting
    > 
    > ### ** Examples
    ...
    > 
    > ##continuous
    > E1[7,1]=NA
    > fit1<-Miss.boosting(G,E1,Y,im_time=1,loop_time=100,num.knots=c(2),Boundary.knots,
    + degree=c(2),v=0.1,tau=0.3,family="continuous",knots=knots,E_type=c("EC","EC","ED","ED"))
    [1] "The  1 th impute"
    Error in is.element(variable[t], variable[1:(t - 1)]) : 
      unused argument (variable[1:(t - 1)])
    Calls: Miss.boosting -> RobSBoosting
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.4Mb
      sub-directories of 1Mb or more:
        data   7.0Mb
    ```

# geoheatmap

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/geoheatmap
* Date/Publication: 2024-09-05 15:40:02 UTC
* Number of recursive dependencies: 105

Run `revdepcheck::cloud_details(, "geoheatmap")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘geoheatmap.Rmd’
      ...
    +     facet_col = "state", value_col = "teams", merge_col = "name_de", 
    +      .... [TRUNCATED] 
    
    > geoheatmap(facet_data = football_teams, grid_data = de_states_grid1, 
    +     facet_col = "state", value_col = "teams", merge_col = "name_de", 
    +      .... [TRUNCATED] 
    
      When sourcing ‘geoheatmap.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘geoheatmap.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
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
* Number of recursive dependencies: 93

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
      
      Attaching package: 'ggplot2'
      
      The following object is masked from 'package:base':
    ...
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test-sf.R:91:3'): We can make grobs from sf features ────────────────
      Error in `(x$boxlinewidth %||% defaults$linewidth[type_ind]) * 3.779528`: non-numeric argument to binary operator
      Backtrace:
          ▆
       1. └─geomtextpath:::sf_textgrob(river, as_textbox = TRUE) at test-sf.R:91:3
      
      [ FAIL 1 | WARN 0 | SKIP 4 | PASS 463 ]
      Error: Test failures
      Execution halted
    ```

# ggalign

<details>

* Version: 0.0.5
* GitHub: https://github.com/Yunuuuu/ggalign
* Source code: https://github.com/cran/ggalign
* Date/Publication: 2024-11-14 08:00:02 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::cloud_details(, "ggalign")` for more info

</details>

## Newly broken

*   checking for code/documentation mismatches ... WARNING
    ```
    Codoc mismatches from documentation object 'plot_theme':
    plot_theme
      Code: function(..., line, rect, text, title, geom, spacing, margins,
                     aspect.ratio, axis.title, axis.title.x,
                     axis.title.x.top, axis.title.x.bottom, axis.title.y,
                     axis.title.y.left, axis.title.y.right, axis.text,
                     axis.text.x, axis.text.x.top, axis.text.x.bottom,
                     axis.text.y, axis.text.y.left, axis.text.y.right,
                     axis.text.theta, axis.text.r, axis.ticks,
                     axis.ticks.x, axis.ticks.x.top, axis.ticks.x.bottom,
    ...
                     strip.text.x.top, strip.text.y, strip.text.y.left,
                     strip.text.y.right, strip.switch.pad.grid,
                     strip.switch.pad.wrap, complete = FALSE, validate =
                     TRUE)
      Argument names in code not in docs:
        geom spacing margins
      Mismatches in argument names (first 3):
        Position: 6 Code: geom Docs: aspect.ratio
        Position: 7 Code: spacing Docs: axis.title
        Position: 8 Code: margins Docs: axis.title.x
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘ComplexHeatmap’, ‘pheatmap’
    ```

# GGally

<details>

* Version: 2.2.1
* GitHub: https://github.com/ggobi/ggally
* Source code: https://github.com/cran/GGally
* Date/Publication: 2024-02-14 00:53:32 UTC
* Number of recursive dependencies: 145

Run `revdepcheck::cloud_details(, "GGally")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > if (requireNamespace("testthat", quietly = TRUE)) {
      +   library(testthat)
      +   library(GGally)
      + 
      +   test_check("GGally")
      + }
    ...
      `expected` is a character vector ('tip')
      ── Failure ('test-ggsurv.R:26:3'): multiple ────────────────────────────────────
      !is.null(a$labels$group) is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      
      [ FAIL 3 | WARN 1 | SKIP 26 | PASS 477 ]
      Error: Test failures
      Execution halted
    ```

# gganimate

<details>

* Version: 1.0.9
* GitHub: https://github.com/thomasp85/gganimate
* Source code: https://github.com/cran/gganimate
* Date/Publication: 2024-02-27 14:00:03 UTC
* Number of recursive dependencies: 96

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
      
      Attaching package: 'ggplot2'
      
      The following object is masked from 'package:base':
    ...
       3. ├─gganimate::animate(p, nframes = 2) at test-anim_save.R:14:5
       4. └─gganimate:::animate.gganim(p, nframes = 2)
       5.   └─args$renderer(frames_vars$frame_source, args$fps)
       6.     └─gganimate:::png_dim(frames[1])
       7.       └─cli::cli_abort("Provided file ({file}) does not exist")
       8.         └─rlang::abort(...)
      
      [ FAIL 1 | WARN 3 | SKIP 1 | PASS 5 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘gganimate.Rmd’
      ...
    Theme element `panel.grid.major.y` is missing
    Theme element `panel.grid.major.x` is missing
    Warning: Failed to plot frame
    Caused by error in `UseMethod()`:
    ! no applicable method for 'element_grob' applied to an object of class "NULL"
    
      When sourcing ‘gganimate.R’:
    Error: Provided file (/tmp/Rtmp2TPR8x/1a1142709f90/gganim_plot0001.png) does
    not exist
    Execution halted
    
      ‘gganimate.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘gganimate.Rmd’ using rmarkdown
    ```

# ggblanket

<details>

* Version: 12.1.0
* GitHub: https://github.com/davidhodge931/ggblanket
* Source code: https://github.com/cran/ggblanket
* Date/Publication: 2025-01-27 09:00:12 UTC
* Number of recursive dependencies: 111

Run `revdepcheck::cloud_details(, "ggblanket")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggblanket-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: annotate_axis_line
    > ### Title: Replace a axis line with an annotated segment
    > ### Aliases: annotate_axis_line
    > 
    > ### ** Examples
    > 
    > library(dplyr)
    ...
     25. │                       │ └─rlang::list2(...)
     26. │                       └─scales::alpha(coord$colour, coord$alpha)
     27. │                         └─farver::decode_colour(colour, alpha = TRUE)
     28. │                           └─farver:::decode_c(...)
     29. └─base::.handleSimpleError(`<fn>`, "Unknown colour name: ~", base::quote(NULL))
     30.   └─rlang (local) h(simpleError(msg, call))
     31.     └─handlers[[1L]](cnd)
     32.       └─cli::cli_abort(...)
     33.         └─rlang::abort(...)
    Execution halted
    ```

# ggdark

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/ggdark
* Date/Publication: 2019-01-11 17:30:06 UTC
* Number of recursive dependencies: 45

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
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      ── Error ('test_dark_mode.R:10:1'): (code run outside of `test_that()`) ────────
      Error in `match(x, table, nomatch = 0L)`: 'match' requires vector arguments
      Backtrace:
          ▆
       1. └─ggdark::dark_mode(light_theme) at test_dark_mode.R:10:1
       2.   └─geoms[["GeomPoint"]]$default_aes$colour %in% ...
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
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
* Number of recursive dependencies: 54

Run `revdepcheck::cloud_details(, "ggdemetra")` for more info

</details>

## Newly broken

*   checking whether package ‘ggdemetra’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: `aes_()` was deprecated in ggplot2 3.0.0.
    See ‘/tmp/workdir/ggdemetra/new/ggdemetra.Rcheck/00install.out’ for details.
    ```

# ggdist

<details>

* Version: 3.3.2
* GitHub: https://github.com/mjskay/ggdist
* Source code: https://github.com/cran/ggdist
* Date/Publication: 2024-03-05 05:30:23 UTC
* Number of recursive dependencies: 126

Run `revdepcheck::cloud_details(, "ggdist")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggdist-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Pr_
    > ### Title: Probability expressions in ggdist aesthetics
    > ### Aliases: Pr_ p_
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
    + )
    > 
    > # map density onto alpha of the fill
    > ggplot(df, aes(y = name, xdist = d)) +
    +   stat_slabinterval(aes(alpha = !!p_(x)))
    Error in use_defaults(..., self = self) : 
      unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, 
        NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "#4D4D4DFF", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL,
    Calls: <Anonymous> ... <Anonymous> -> <Anonymous> -> compute_geom_2 -> <Anonymous>
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
      • test.stat_sample_slabinterval/nas-with-na-rm-true.svg
      • test.subguide/dots-subguide-with-side-vertical.svg
      • test.subguide/integer-subguide-with-zero-range.svg
      • test.subguide/slab-subguide-with-inside-labels-vertical.svg
      • test.subguide/slab-subguide-with-outside-labels-vert.svg
      • test.subguide/slab-subguide-with-outside-labels.svg
      • test.subguide/slab-subguide-with-side-vertical.svg
      • test.theme_ggdist/facet-titles-on-left.svg
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘dotsinterval.Rmd’ using rmarkdown
    Warning in hook_png(..., cmd = "pngquant", post_process = function(x) { :
      cannot find pngquant; please install and put it in PATH
    Warning in hook_png(..., cmd = "pngquant", post_process = function(x) { :
      cannot find pngquant; please install and put it in PATH
    
    Quitting from lines 49-161 [dotsinterval_components] (dotsinterval.Rmd)
    Error: processing vignette 'dotsinterval.Rmd' failed with diagnostics:
    Problem while setting up geom aesthetics.
    ...
    
    --- re-building ‘freq-uncertainty-vis.Rmd’ using rmarkdown
    Warning in hook_png(..., cmd = "pngquant", post_process = function(x) { :
      cannot find pngquant; please install and put it in PATH
    Warning in hook_png(..., cmd = "pngquant", post_process = function(x) { :
      cannot find pngquant; please install and put it in PATH
    Warning in hook_png(..., cmd = "pngquant", post_process = function(x) { :
      cannot find pngquant; please install and put it in PATH
    Warning in hook_png(..., cmd = "pngquant", post_process = function(x) { :
      cannot find pngquant; please install and put it in PATH
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘dotsinterval.Rmd’
      ...
    +     xdist = dist)) + geom_hline(yintercept = 0:1, color = "gray95") + 
    +     stat_dotsin .... [TRUNCATED] 
    
      When sourcing ‘dotsinterval.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 2nd layer.
    Caused by error in `use_defaults()`:
    ...
    ℹ Error occurred in the 1st layer.
    Caused by error in `use_defaults()`:
    ! unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(7, 0, 0, 0), NULL, FALSE), list(NULL, NULL, NULL, NULL, NULL, 
        0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 7, 0, 0), NULL, FALSE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "#4D4D4DFF", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0
    Execution halted
    
      ‘dotsinterval.Rmd’ using ‘UTF-8’... failed
      ‘freq-uncertainty-vis.Rmd’ using ‘UTF-8’... failed
      ‘lineribbon.Rmd’ using ‘UTF-8’... failed
      ‘slabinterval.Rmd’ using ‘UTF-8’... failed
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        doc    1.3Mb
        help   1.5Mb
    ```

# ggedit

<details>

* Version: 0.4.1
* GitHub: https://github.com/yonicd/ggedit
* Source code: https://github.com/cran/ggedit
* Date/Publication: 2024-03-04 14:40:02 UTC
* Number of recursive dependencies: 94

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

# ggESDA

<details>

* Version: 0.2.0
* GitHub: https://github.com/kiangkiangkiang/ggESDA
* Source code: https://github.com/cran/ggESDA
* Date/Publication: 2022-08-19 08:40:10 UTC
* Number of recursive dependencies: 218

Run `revdepcheck::cloud_details(, "ggESDA")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggESDA-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggInterval_scatter
    > ### Title: scatter plot for two continuous interval data
    > ### Aliases: ggInterval_scatter
    > 
    > ### ** Examples
    > 
    > a<-rnorm(1000,0,5)
    ...
     11.       │ └─base::withCallingHandlers(...)
     12.       └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13.         └─l$compute_geom_2(d, theme = plot$theme)
     14.           └─ggplot2 (local) compute_geom_2(..., self = self)
     15.             └─self$geom$use_defaults(...)
     16.               └─ggplot2 (local) use_defaults(..., self = self)
     17.                 └─ggplot2:::check_aesthetics(new_params, nrow(data))
     18.                   └─cli::cli_abort(...)
     19.                     └─rlang::abort(...)
    Execution halted
    ```

# ggfixest

<details>

* Version: 0.2.0
* GitHub: https://github.com/grantmcdermott/ggfixest
* Source code: https://github.com/cran/ggfixest
* Date/Publication: 2025-01-22 01:00:02 UTC
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
      Error: 5 out of 103 tests failed
      In addition: There were 11 warnings (use warnings() to see them)
      Execution halted
    ```

# ggforce

<details>

* Version: 0.4.2
* GitHub: https://github.com/thomasp85/ggforce
* Source code: https://github.com/cran/ggforce
* Date/Publication: 2024-02-19 11:00:02 UTC
* Number of recursive dependencies: 69

Run `revdepcheck::cloud_details(, "ggforce")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggforce-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: facet_row
    > ### Title: One-dimensional facets
    > ### Aliases: facet_row facet_col
    > 
    > ### ** Examples
    > 
    > # Standard use
    > ggplot(mtcars) +
    +   geom_point(aes(disp, mpg)) +
    +   facet_col(~gear)
    Error in space$x : $ operator is invalid for atomic vectors
    Calls: <Anonymous> ... <Anonymous> -> draw_panels -> <Anonymous> -> init_gtable
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
* Number of recursive dependencies: 122

Run `revdepcheck::cloud_details(, "ggformula")` for more info

</details>

## Newly broken

*   checking for code/documentation mismatches ... WARNING
    ```
    Codoc mismatches from documentation object 'gf_abline':
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
* Number of recursive dependencies: 124

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

# ggh4x

<details>

* Version: 0.3.0
* GitHub: https://github.com/teunbrand/ggh4x
* Source code: https://github.com/cran/ggh4x
* Date/Publication: 2024-12-15 17:20:02 UTC
* Number of recursive dependencies: 76

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
      unused argument (list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, 
        NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "#4D4D4DFF", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, N
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
      
      Attaching package: 'ggplot2'
      
      The following object is masked from 'package:base':
    ...
       11.             └─base::Map(...)
       12.               └─base::mapply(FUN = f, ..., SIMPLIFY = FALSE)
       13.                 └─ggplot2 (local) `<fn>`(guide = dots[[1L]][[1L]], param = dots[[2L]][[1L]])
       14.                   └─guide$process_layers(param, layers, data, theme)
       15.                     └─ggplot2 (local) process_layers(..., self = self)
       16.                       └─self$get_layer_key(params, layers[include], data[include], theme)
      
      [ FAIL 1 | WARN 0 | SKIP 15 | PASS 663 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Miscellaneous.Rmd’
      ...
    +     aest .... [TRUNCATED] 
    
    > ggplot(diamonds, aes(price, carat, colour = clarity)) + 
    +     geom_point(shape = ".") + scale_colour_brewer(palette = "Dark2", 
    +     guide = "stri ..." ... [TRUNCATED] 
    
      When sourcing ‘Miscellaneous.R’:
    Error: unused argument (list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, 
        NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "#4D4D4DFF", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NUL
    Execution halted
    
      ‘Facets.Rmd’ using ‘UTF-8’... OK
      ‘Miscellaneous.Rmd’ using ‘UTF-8’... failed
      ‘Statistics.Rmd’ using ‘UTF-8’... OK
      ‘ggh4x.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Facets.Rmd’ using rmarkdown
    ```

# gghighlight

<details>

* Version: 0.4.1
* GitHub: https://github.com/yutannihilation/gghighlight
* Source code: https://github.com/cran/gghighlight
* Date/Publication: 2023-12-16 01:00:02 UTC
* Number of recursive dependencies: 85

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
      
      Attaching package: 'ggplot2'
      
      The following object is masked from 'package:base':
    ...
       15.     └─cli::cli_abort(...)
       16.       └─rlang::abort(...)
      
      [ FAIL 2 | WARN 2 | SKIP 1 | PASS 178 ]
      Deleting unused snapshots:
      • vdiffr/simple-bar-chart-with-facet.svg
      • vdiffr/simple-line-chart.svg
      • vdiffr/simple-point-chart.svg
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘gghighlight.Rmd’
      ...
    Warning in is.na(non_null_default_aes[[aes_param_name]]) :
      is.na() applied to non-(list or vector) of type 'language'
    
      When sourcing ‘gghighlight.R’:
    Error: ℹ In index: 1.
    ℹ With name: geom_point.
    Caused by error in `aes_param_name %in% names(non_null_default_aes) && is.na(non_null_default_aes[[
        aes_param_name]])`:
    ! 'length = 2' in coercion to 'logical(1)'
    Execution halted
    
      ‘gghighlight.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘gghighlight.Rmd’ using rmarkdown
    ```

# ggiraph

<details>

* Version: 0.8.12
* GitHub: https://github.com/davidgohel/ggiraph
* Source code: https://github.com/cran/ggiraph
* Date/Publication: 2025-01-08 11:10:02 UTC
* Number of recursive dependencies: 89

Run `revdepcheck::cloud_details(, "ggiraph")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggiraph-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_path_interactive
    > ### Title: Create interactive observations connections
    > ### Aliases: geom_path_interactive geom_line_interactive
    > ###   geom_step_interactive
    > 
    > ### ** Examples
    > 
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
      Attaching package: 'ggplot2'
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

# ggmap

<details>

* Version: 4.0.0
* GitHub: https://github.com/dkahle/ggmap
* Source code: https://github.com/cran/ggmap
* Date/Publication: 2023-11-19 08:10:02 UTC
* Number of recursive dependencies: 65

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

# ggmcmc

<details>

* Version: 1.5.1.1
* GitHub: https://github.com/xfim/ggmcmc
* Source code: https://github.com/cran/ggmcmc
* Date/Publication: 2021-02-10 10:50:10 UTC
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "ggmcmc")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggmcmc-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggs_geweke
    > ### Title: Dotplot of the Geweke diagnostic, the standard Z-score
    > ### Aliases: ggs_geweke
    > 
    > ### ** Examples
    > 
    > data(linear)
    > ggs_geweke(ggs(s))
    Error in grid.Call.graphics(C_polygon, x$x, x$y, index) : 
      invalid line type
    Calls: <Anonymous> ... drawDetails -> drawDetails.polygon -> grid.Call.graphics
    Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘using_ggmcmc.Rmd’
      ...
    
    > ggs_crosscorrelation(S)
    
    > ggs_Rhat(S) + xlab("R_hat")
    
    > ggs_geweke(S)
    
    ...
    +         par(mar = c(4, 4, 0.1, 0.1), cex .... [TRUNCATED] 
    
    > knit_theme$set("print")
    
      When sourcing 'v70i09.R':
    Error: argument is of length zero
    Execution halted
    
      ‘using_ggmcmc.Rmd’ using ‘UTF-8’... failed
      ‘v70i09.Rnw’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘using_ggmcmc.Rmd’ using rmarkdown
    ```

# ggmice

<details>

* Version: 0.1.0
* GitHub: https://github.com/amices/ggmice
* Source code: https://github.com/cran/ggmice
* Date/Publication: 2023-08-07 14:20:02 UTC
* Number of recursive dependencies: 122

Run `revdepcheck::cloud_details(, "ggmice")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘old_friends.Rmd’
      ...
        layout
    
    
    > p <- plot_flux(dat)
    
    > ggplotly(p)
    
      When sourcing ‘old_friends.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘ggmice.Rmd’ using ‘UTF-8’... OK
      ‘old_friends.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ggmice.Rmd’ using rmarkdown
    ```

# ggmulti

<details>

* Version: 1.0.7
* GitHub: NA
* Source code: https://github.com/cran/ggmulti
* Date/Publication: 2024-04-09 09:40:05 UTC
* Number of recursive dependencies: 125

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
      unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, 
        NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "#4D4D4DFF", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL,
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
      
      Attaching package: 'ggplot2'
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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘glyph.Rmd’
      ...
    +     Sepal.Width, colour = Species), serialaxes.data = iris, axes.layout = "radia ..." ... [TRUNCATED] 
    
      When sourcing ‘glyph.R’:
    Error: Base operators are not defined for quosures. Do you need to unquote the
    quosure?
    
    # Bad: myquosure / rhs
    ...
    > p
    
      When sourcing ‘highDim.R’:
    Error: unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(), NULL, list(
        NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "#4D4D4DFF", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 2.2), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0.5, NULL, NULL, NULL, c(0,
    Execution halted
    
      ‘glyph.Rmd’ using ‘UTF-8’... failed
      ‘highDim.Rmd’ using ‘UTF-8’... failed
      ‘histogram-density-.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘glyph.Rmd’ using rmarkdown
    ```

# ggparallel

<details>

* Version: 0.4.0
* GitHub: https://github.com/heike/ggparallel
* Source code: https://github.com/cran/ggparallel
* Date/Publication: 2024-03-09 22:00:02 UTC
* Number of recursive dependencies: 50

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
       12. │       └─ggplot2 (local) map_statistic(..., self = self)
       13. └─base::.handleSimpleError(...)
       14.   └─rlang (local) h(simpleError(msg, call))
       15.     └─handlers[[1L]](cnd)
       16.       └─cli::cli_abort(...)
       17.         └─rlang::abort(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

# ggplotlyExtra

<details>

* Version: 0.0.1
* GitHub: NA
* Source code: https://github.com/cran/ggplotlyExtra
* Date/Publication: 2019-12-02 16:20:06 UTC
* Number of recursive dependencies: 70

Run `revdepcheck::cloud_details(, "ggplotlyExtra")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggplotlyExtra-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggplotly_histogram
    > ### Title: Clean 'ggplot2' Histogram to be Converted to 'Plotly'
    > ### Aliases: ggplotly_histogram
    > 
    > ### ** Examples
    > 
    > 
    ...
    + xlab("len")
    `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    Warning in geom_bar(data = layerdata, mapping = aes(x = .data$x, y = .data$count,  :
      Ignoring unknown aesthetics: label1, label2, and label3
    > 
    > # convert `ggplot` object to `plotly` object
    > ggplotly(p, tooltip = c("Range", "count", "density"))
    Error in pm[[2]] : subscript out of bounds
    Calls: ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# ggpol

<details>

* Version: 0.0.7
* GitHub: https://github.com/erocoar/ggpol
* Source code: https://github.com/cran/ggpol
* Date/Publication: 2020-11-08 13:40:02 UTC
* Number of recursive dependencies: 54

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

# ggpubr

<details>

* Version: 0.6.0
* GitHub: https://github.com/kassambara/ggpubr
* Source code: https://github.com/cran/ggpubr
* Date/Publication: 2023-02-10 16:20:02 UTC
* Number of recursive dependencies: 91

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
      
      Attaching package: 'ggplot2'
      
      The following object is masked from 'package:base':
    ...
      [6]   6 - 10 == -4
      [7]  19 -  9 == 10
      [9]   1 -  7 == -6
      [10]  6 -  7 == -1
      [11] 13 -  6 ==  7
      ...
      
      [ FAIL 2 | WARN 5 | SKIP 0 | PASS 183 ]
      Error: Test failures
      Execution halted
    ```

# ggraph

<details>

* Version: 2.2.1
* GitHub: https://github.com/thomasp85/ggraph
* Source code: https://github.com/cran/ggraph
* Date/Publication: 2024-03-07 12:40:02 UTC
* Number of recursive dependencies: 115

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
      unused argument (list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, list(), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 
        0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, 
    Calls: <Anonymous> ... <Anonymous> -> <Anonymous> -> process_layers -> <Anonymous>
    Execution halted
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Edges.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Edges.Rmd’
      ...
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family 'Arial Narrow' not found in PostScript font database
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family 'Arial Narrow' not found in PostScript font database
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family 'Arial Narrow' not found in PostScript font database
    
    ...
      font family 'Arial' not found in PostScript font database
    
      When sourcing ‘tidygraph.R’:
    Error: invalid font type
    Execution halted
    
      ‘Edges.Rmd’ using ‘UTF-8’... failed
      ‘Layouts.Rmd’ using ‘UTF-8’... failed
      ‘Nodes.Rmd’ using ‘UTF-8’... failed
      ‘tidygraph.Rmd’ using ‘UTF-8’... failed
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.9Mb
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
* Number of recursive dependencies: 67

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
     24. │                           └─ggplot2:::sf_rescale01(...)
     25. │                             └─sf::st_normalize(x, c(x_range[1], y_range[1], x_range[2], y_range[2]))
     26. └─base::.handleSimpleError(...)
     27.   └─rlang (local) h(simpleError(msg, call))
     28.     └─handlers[[1L]](cnd)
     29.       └─cli::cli_abort(...)
     30.         └─rlang::abort(...)
    Execution halted
    ```

# ggRtsy

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/ggRtsy
* Date/Publication: 2023-09-15 19:12:05 UTC
* Number of recursive dependencies: 68

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
      
      Attaching package: 'ggplot2'
      
      The following object is masked from 'package:base':
      
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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Vignette.Rmd’
      ...
    |Antique White |(238, 223, 204) |#eedfcc |
    
    > RectangleFiller(plotExample, c("#e32636", "#9966cc", 
    +     "#f4c2c2", "#e16827"))
    
      When sourcing ‘Vignette.R’:
    Error: Can't extract rows past the end.
    ℹ Location 1 doesn't exist.
    ℹ There are only 0 rows.
    Execution halted
    
      ‘Vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Vignette.Rmd’ using rmarkdown
    
    Quitting from lines 48-49 [unnamed-chunk-2] (Vignette.Rmd)
    Error: processing vignette 'Vignette.Rmd' failed with diagnostics:
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

* Version: 0.8.5
* GitHub: https://github.com/maraab23/ggseqplot
* Source code: https://github.com/cran/ggseqplot
* Date/Publication: 2024-10-29 16:30:02 UTC
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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggseqplot.Rmd’
      ...
    Scale for fill is already present.
    Adding another scale for fill, which will replace the existing scale.
    Scale for fill is already present.
    Adding another scale for fill, which will replace the existing scale.
    
    > ggseqtrplot(actcal.seq, group = actcal$sex)
    
      When sourcing ‘ggseqplot.R’:
    Error: labsize must be a single number
    Execution halted
    
      ‘ggseqplot.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
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
* Number of recursive dependencies: 75

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
    + geom_point()
    > 
    > #sidebar - uses StatCount
    > p +
    + geom_xsidebar() +
    + geom_ysidebar()
    Error in is.element(panel_type, c("x", "y")) : 
      unused argument (c("x", "y"))
    Calls: <Anonymous> ... <Anonymous> -> draw_panel -> <Anonymous> -> render_fg
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggplot2)
      
      Attaching package: 'ggplot2'
      
      The following object is masked from 'package:base':
      
    ...
      • ops_meaningful/alpha-0-5-from-function.svg
      • side_layers/boxplot2.svg
      • vdiff_irisScatter/collapsed-histo.svg
      • vdiff_irisScatter/facetgrid-collapsed-density.svg
      • vdiff_irisScatter/facetgrid-histo.svg
      • vdiff_irisScatter/facetgrid-side-density.svg
      • vdiff_irisScatter/stacked-side-density.svg
      • vdiff_irisScatter/yside-histo.svg
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggside_aes_mapping.Rmd’
      ...
      +.gg   ggplot2
    
    > p <- ggplot(mpg, aes(displ, hwy, colour = class)) + 
    +     geom_point(size = 2) + theme_bw()
    
    > p + geom_xsidedensity() + geom_ysidedensity()
    
    ...
    Warning in max(x) : no non-missing arguments to max; returning -Inf
    Warning in min(x) : no non-missing arguments to min; returning Inf
    Warning in max(x) : no non-missing arguments to max; returning -Inf
    
      When sourcing ‘ggside_basic_usage.R’:
    Error: unused argument (c("x", "y"))
    Execution halted
    
      ‘ggside_aes_mapping.Rmd’ using ‘UTF-8’... failed
      ‘ggside_basic_usage.Rmd’ using ‘UTF-8’... failed
    ```

*   checking whether package ‘ggside’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(panel_type, ': unused argument (c("x", "y")) 
      Note: possible error in 'is.element(self$rescale, ': unused argument (suggested_var) 
    See ‘/tmp/workdir/ggside/new/ggside.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking for code/documentation mismatches ... WARNING
    ```
    Codoc mismatches from documentation object 'geom_xsideabline':
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
      Docs: function(mapping = NULL, data = NULL, stat = "identity",
                     position = "identity", ..., lineend = "butt", linejoin
                     = "round", linemitre = 10, arrow = NULL, na.rm =
                     FALSE, show.legend = NA, inherit.aes = TRUE)
      Argument names in code not in docs:
        arrow.fill
      Mismatches in argument names:
        Position: 10 Code: arrow.fill Docs: na.rm
        Position: 11 Code: na.rm Docs: show.legend
        Position: 12 Code: show.legend Docs: inherit.aes
    ```

*   checking R code for possible problems ... NOTE
    ```
    ggside_render_fg: possible error in is.element(panel_type, c("x",
      "y")): unused argument (c("x", "y"))
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ggside_aes_mapping.Rmd’ using rmarkdown
    
    Quitting from lines 43-46 [ggside_updated_aes_usage] (ggside_aes_mapping.Rmd)
    Error: processing vignette 'ggside_aes_mapping.Rmd' failed with diagnostics:
    unused argument (c("x", "y"))
    --- failed re-building ‘ggside_aes_mapping.Rmd’
    
    --- re-building ‘ggside_basic_usage.Rmd’ using rmarkdown
    ```

# ggsmc

<details>

* Version: 0.1.2.0
* GitHub: https://github.com/richardgeveritt/ggsmc
* Source code: https://github.com/cran/ggsmc
* Date/Publication: 2024-07-27 17:00:02 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::cloud_details(, "ggsmc")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Visualising.Rmd’
      ...
    20 /tmp/RtmpKyNdR5/b96ef7031e/gganim_plot0020.png
    
    > data(lv_output)
    
    > animate_reveal_time_series(lv_output, parameters = c("X", 
    +     "Y"), alpha = 0.5, ylimits = c(0, 600), duration = 10)
    
      When sourcing ‘Visualising.R’:
    Error: argument "theme" is missing, with no default
    Execution halted
    
      ‘Visualising.Rmd’ using ‘UTF-8’... failed
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 175 marked UTF-8 strings
    ```

# ggspatial

<details>

* Version: 1.1.9
* GitHub: https://github.com/paleolimbot/ggspatial
* Source code: https://github.com/cran/ggspatial
* Date/Publication: 2023-08-17 15:32:38 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::cloud_details(, "ggspatial")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggspatial-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: annotation_spatial_hline
    > ### Title: Projected horizontal and vertical lines
    > ### Aliases: annotation_spatial_hline annotation_spatial_vline
    > ###   GeomSpatialXline
    > ### Keywords: datasets
    > 
    > ### ** Examples
    ...
     25. │                             └─grid:::validGP(list(...))
     26. │                               └─grid (local) numnotnull("fontsize")
     27. │                                 └─grid (local) check.length(gparname)
     28. │                                   └─base::stop(...)
     29. └─base::.handleSimpleError(...)
     30.   └─rlang (local) h(simpleError(msg, call))
     31.     └─handlers[[1L]](cnd)
     32.       └─cli::cli_abort(...)
     33.         └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggspatial)
      > 
      > test_check("ggspatial")
      Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
      [ FAIL 1 | WARN 1 | SKIP 22 | PASS 195 ]
      
    ...
       33. │                                           └─base::stop(...)
       34. └─base::.handleSimpleError(...)
       35.   └─rlang (local) h(simpleError(msg, call))
       36.     └─handlers[[1L]](cnd)
       37.       └─cli::cli_abort(...)
       38.         └─rlang::abort(...)
      
      [ FAIL 1 | WARN 1 | SKIP 22 | PASS 195 ]
      Error: Test failures
      Execution halted
    ```

# ggstatsplot

<details>

* Version: 0.13.0
* GitHub: https://github.com/IndrajeetPatil/ggstatsplot
* Source code: https://github.com/cran/ggstatsplot
* Date/Publication: 2024-12-04 15:10:02 UTC
* Number of recursive dependencies: 176

Run `revdepcheck::cloud_details(, "ggstatsplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggstatsplot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggscatterstats
    > ### Title: Scatterplot with marginal distributions and statistical results
    > ### Aliases: ggscatterstats
    > 
    > ### ** Examples
    > 
    > set.seed(123)
    ...
    +   ggplot2::geom_rug(sides = "b")
    > 
    > # looking at the plot
    > p
    `stat_xsidebin()` using `bins = 30`. Pick better value with `binwidth`.
    `stat_ysidebin()` using `bins = 30`. Pick better value with `binwidth`.
    Error in is.element(panel_type, c("x", "y")) : 
      unused argument (c("x", "y"))
    Calls: <Anonymous> ... <Anonymous> -> draw_panel -> <Anonymous> -> render_fg
    Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggstatsplot.Rmd’
      ...
    
    > try({
    +     ggbetweenstats(iris, Species, Sepal.Length)
    + })
    
    > knitr::include_graphics("../man/figures/stats_reporting_format.png")
    
      When sourcing ‘ggstatsplot.R’:
    Error: Cannot find the file(s): "../man/figures/stats_reporting_format.png"
    Execution halted
    
      ‘additional.Rmd’ using ‘UTF-8’... OK
      ‘ggstatsplot.Rmd’ using ‘UTF-8’... failed
    ```

# ggswissmaps

<details>

* Version: 0.1.1
* GitHub: https://github.com/gibonet/ggswissmaps
* Source code: https://github.com/cran/ggswissmaps
* Date/Publication: 2016-10-29 10:48:24
* Number of recursive dependencies: 71

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
* Number of recursive dependencies: 42

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
  object 'update_guides' not found
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
# ggThemeAssist

<details>

* Version: 0.1.5
* GitHub: https://github.com/calligross/ggthemeassist
* Source code: https://github.com/cran/ggThemeAssist
* Date/Publication: 2016-08-13 16:50:55
* Number of recursive dependencies: 57

Run `revdepcheck::cloud_details(, "ggThemeAssist")` for more info

</details>

## Newly broken

*   checking whether package ‘ggThemeAssist’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ggThemeAssist/new/ggThemeAssist.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘ggThemeAssist’ ...
** package ‘ggThemeAssist’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in is.element("extrafont", installed.packages()[, 1]) : 
  unused argument (installed.packages()[, 1])
Error: unable to load R code in package ‘ggThemeAssist’
Execution halted
ERROR: lazy loading failed for package ‘ggThemeAssist’
* removing ‘/tmp/workdir/ggThemeAssist/new/ggThemeAssist.Rcheck/ggThemeAssist’


```
### CRAN

```
* installing *source* package ‘ggThemeAssist’ ...
** package ‘ggThemeAssist’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (ggThemeAssist)


```
# ggupset

<details>

* Version: 0.4.0
* GitHub: https://github.com/const-ae/ggupset
* Source code: https://github.com/cran/ggupset
* Date/Publication: 2024-06-24 10:10:04 UTC
* Number of recursive dependencies: 45

Run `revdepcheck::cloud_details(, "ggupset")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggupset-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: axis_combmatrix
    > ### Title: Convert delimited text labels into a combination matrix axis
    > ### Aliases: axis_combmatrix
    > 
    > ### ** Examples
    > 
    >   library(ggplot2)
    ...
    Datsun 710        Cyl: 4_Gears: 4
    Hornet 4 Drive    Cyl: 6_Gears: 3
    Hornet Sportabout Cyl: 8_Gears: 3
    Valiant           Cyl: 6_Gears: 3
    >   ggplot(mtcars, aes(x=combined)) +
    +     geom_bar() +
    +     axis_combmatrix(sep = "_")
    Error in as.unit(e2) : object is not coercible to a unit
    Calls: <Anonymous> ... polylineGrob -> is.unit -> unit.c -> Ops.unit -> as.unit
    Execution halted
    ```

# ggVennDiagram

<details>

* Version: 1.5.2
* GitHub: https://github.com/gaospecial/ggVennDiagram
* Source code: https://github.com/cran/ggVennDiagram
* Date/Publication: 2024-02-20 08:10:02 UTC
* Number of recursive dependencies: 97

Run `revdepcheck::cloud_details(, "ggVennDiagram")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘fully-customed.Rmd’
      ...
    [1] "b" "c" "e" "h" "k" "q" "s" "y"
    
    
    > ggVennDiagram(y, show_intersect = TRUE, set_color = "black")
    Warning in geom_text(aes(label = .data$count, text = .data$item), data = region_label) :
      Ignoring unknown aesthetics: text
    
    ...
      Ignoring unknown aesthetics: text
    
      When sourcing ‘using-ggVennDiagram.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘VennCalculator.Rmd’ using ‘UTF-8’... OK
      ‘fully-customed.Rmd’ using ‘UTF-8’... failed
      ‘using-ggVennDiagram.Rmd’ using ‘UTF-8’... failed
      ‘using-new-shapes.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
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

# Greymodels

<details>

* Version: 2.0.1
* GitHub: https://github.com/havishaJ/Greymodels
* Source code: https://github.com/cran/Greymodels
* Date/Publication: 2022-12-05 12:42:35 UTC
* Number of recursive dependencies: 89

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

# GWASinspector

<details>

* Version: 1.7.1
* GitHub: NA
* Source code: https://github.com/cran/GWASinspector
* Date/Publication: 2024-05-06 18:20:10 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::cloud_details(, "GWASinspector")` for more info

</details>

## Newly broken

*   checking whether package ‘GWASinspector’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element("hID", names(input.data))': unused argument (names(input.data)) 
      Note: possible error in 'is.element(toupper(config$supplementaryFiles$allele_ref_std_population), ': unused argument (c("COMMON", "EAS", "AMR", "AFR", "EUR", "SAS")) 
      Note: possible error in 'is.element("EFFECT", ': unused argument (original.File.Columns.upper) 
      Note: possible error in 'is.element("EFFECT", ': unused argument (renamed.File.Columns) 
      Note: possible error in 'is.element("ggplot2", ': unused argument (existing.packages) 
      Note: possible error in 'is.element("kableExtra", ': unused argument (existing.packages) 
      Note: possible error in 'is.element("rJava", existing.packages)': unused argument (existing.packages) 
      Note: possible error in 'is.element("RSQLite", ': unused argument (existing.packages) 
      Note: possible error in 'is.element("openxlsx", ': unused argument (existing.packages) 
    ...
      Note: possible error in 'is.element("highDiffEAF", ': unused argument (names(dataset)) 
      Note: possible error in 'is.element("AF", names(dataset))': unused argument (names(dataset)) 
      Note: possible error in 'is.element("REF_RSID", ': unused argument (names(dataset)) 
      Note: possible error in 'is.element("EFFECT", ': unused argument (crucial.columns) 
      Note: possible error in 'is.element(EFFECT_ALL, ': unused argument (c("A", "G", "C", "T")) 
      Note: possible error in 'is.element(OTHER_ALL, ': unused argument (c("A", "G", "C", "T")) 
    See ‘/tmp/workdir/GWASinspector/new/GWASinspector.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    add_hIDcolumn: possible error in is.element("hID", names(input.data)):
      unused argument (names(input.data))
    checkConfigFile: possible error in
      is.element(toupper(config$supplementaryFiles$allele_ref_std_population),
      c("COMMON", "EAS", "AMR", "AFR", "EUR", "SAS")): unused argument
      (c("COMMON", "EAS", "AMR", "AFR", "EUR", "SAS"))
    checkRequiredColumnNames: possible error in is.element("EFFECT",
      original.File.Columns.upper): unused argument
      (original.File.Columns.upper)
    checkRequiredColumnNames: possible error in is.element("EFFECT",
    ...
      RSQLite::dbListFields(.QC$reference.data, "variants")): unused
      argument (RSQLite::dbListFields(.QC$reference.data, "variants"))
    validate_Inspector: possible error in is.element(population.Column,
      tblFields): unused argument (tblFields)
    variable_statistics_post_matching: possible error in is.element("CHR",
      names(input.data)): unused argument (names(input.data))
    variantDiscrimination: possible error in is.element(EFFECT_ALL, c("A",
      "G", "C", "T")): unused argument (c("A", "G", "C", "T"))
    variantDiscrimination: possible error in is.element(OTHER_ALL, c("A",
      "G", "C", "T")): unused argument (c("A", "G", "C", "T"))
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘GWASinspector.Rmd’
      ...
    > knitr::opts_chunk$set(eval = FALSE, results = "hide", 
    +     collapse = TRUE, comment = "#>", results = "asis")
    
    > install.packages("GWASinspector")
    Installing package into ‘/tmp/RtmpR7P8mN/RLIBS_9831403b73’
    (as ‘lib’ is unspecified)
    
      When sourcing ‘GWASinspector.R’:
    Error: trying to use CRAN without setting a mirror
    Execution halted
    
      ‘GWASinspector.Rmd’ using ‘UTF-8’... failed
    ```

# GWlasso

<details>

* Version: 1.0.1
* GitHub: https://github.com/nibortolum/GWlasso
* Source code: https://github.com/cran/GWlasso
* Date/Publication: 2024-11-22 09:30:07 UTC
* Number of recursive dependencies: 102

Run `revdepcheck::cloud_details(, "GWlasso")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘GWlasso-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.gwlfit
    > ### Title: Plot method for gwlfit object
    > ### Aliases: plot.gwlfit
    > 
    > ### ** Examples
    > 
    > 
    ...
    Warning in min(x) : no non-missing arguments to min; returning Inf
    Warning in max(x) : no non-missing arguments to max; returning -Inf
    Warning in min(d[d > tolerance]) :
      no non-missing arguments to min; returning Inf
    Warning in min(x) : no non-missing arguments to min; returning Inf
    Warning in max(x) : no non-missing arguments to max; returning -Inf
    Error in is.element(panel_type, c("x", "y")) : 
      unused argument (c("x", "y"))
    Calls: <Anonymous> ... <Anonymous> -> draw_panel -> <Anonymous> -> render_fg
    Execution halted
    ```

# harbinger

<details>

* Version: 1.1.707
* GitHub: https://github.com/cefet-rj-dal/harbinger
* Source code: https://github.com/cran/harbinger
* Date/Publication: 2024-12-03 20:00:03 UTC
* Number of recursive dependencies: 175

Run `revdepcheck::cloud_details(, "harbinger")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘harbinger-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: hanr_arima
    > ### Title: Anomaly detector using ARIMA.
    > ### Aliases: hanr_arima
    > 
    > ### ** Examples
    > 
    > library(daltoolbox)
    ...
    > 
    > # setting up time series regression model
    > model <- hanr_arima()
    > 
    > # fitting the model
    > model <- fit(model, dataset$serie)
    Error in is.element("drift", names(obj$model$coef)) : 
      unused argument (names(obj$model$coef))
    Calls: fit -> fit.hanr_arima
    Execution halted
    ```

*   checking whether package ‘harbinger’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element("drift", names(obj$model$coef))': unused argument (names(obj$model$coef)) 
    See ‘/tmp/workdir/harbinger/new/harbinger.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    fit.hanr_arima: possible error in is.element("drift",
      names(obj$model$coef)): unused argument (names(obj$model$coef))
    fit.hcp_cf_arima: possible error in is.element("drift",
      names(obj$model$coef)): unused argument (names(obj$model$coef))
    ```

# HCmodelSets

<details>

* Version: 1.1.3
* GitHub: https://github.com/hhhelfer/HCmodelSets
* Source code: https://github.com/cran/HCmodelSets
* Date/Publication: 2023-03-15 18:00:09 UTC
* Number of recursive dependencies: 36

Run `revdepcheck::cloud_details(, "HCmodelSets")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘HCmodelSets-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: DGP
    > ### Title: Data generating process used by Battey, H. S. & Cox, D. R.
    > ###   (2018).
    > ### Aliases: DGP
    > 
    > ### ** Examples
    > 
    > ## Generates DGP
    > ## Don't show: 
    > dgp = DGP(s=5, a=3, sigStrength=1, rho=0.9, n=20, intercept=5, noise=1,
    +           var=1, d=50, DGP.seed = 2019)
    Error in is.element(type.response, c("S", "N")) : 
      unused argument (c("S", "N"))
    Calls: DGP
    Execution halted
    ```

*   checking whether package ‘HCmodelSets’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(type.response, ': unused argument (c("S", "N")) 
    See ‘/tmp/workdir/HCmodelSets/new/HCmodelSets.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    DGP: possible error in is.element(type.response, c("S", "N")): unused
      argument (c("S", "N"))
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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘getting-started.Rmd’
      ...
    +     .by = "month", .interactive = FALSE)
    Warning: Ignoring unknown labels:
    • `colour = "Legend"`
    
    > ts_alos_plt(.data = df_tbl, .date_col = Date, .value_col = Values, 
    +     .by = "month", .interactive = TRUE)
    
      When sourcing ‘getting-started.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘getting-started.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘using-tidy-fft.Rmd’
      ...
    $ value    <dbl> 112, 118, 132, 129, 121, 135, 148, 148, 136, 119, 104, 118, 1…
    
    > suppressPackageStartupMessages(library(timetk))
    
    > data_tbl %>% plot_time_series(.date_var = date_col, 
    +     .value = value)
    
      When sourcing ‘using-tidy-fft.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘getting-started.Rmd’ using ‘UTF-8’... OK
      ‘using-tidy-fft.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
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
* Number of recursive dependencies: 110

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
      
      Attaching package: 'ggplot2'
      
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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘heatmaply.Rmd’
      ...
    
    > library("heatmaply")
    
    > library("heatmaply")
    
    > heatmaply(mtcars)
    
      When sourcing ‘heatmaply.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘heatmaply.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘heatmaply.Rmd’ using rmarkdown
    
    Quitting from lines 109-111 [unnamed-chunk-5] (heatmaply.Rmd)
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

# heemod

<details>

* Version: 1.0.2
* GitHub: https://github.com/aphp/heemod
* Source code: https://github.com/cran/heemod
* Date/Publication: 2024-09-11 16:00:02 UTC
* Number of recursive dependencies: 139

Run `revdepcheck::cloud_details(, "heemod")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘e_probabilistic.Rmd’
      ...
    Scale for colour is already present.
    Adding another scale for colour, which will replace the existing scale.
    
    > bcea <- run_bcea(pm, plot = TRUE, Kmax = 10000)
    Loading required namespace: BCEA
    No reference selected. Defaulting to first intervention.
    
    ...
      ‘c_homogeneous.Rmd’ using ‘UTF-8’... OK
      ‘d_non_homogeneous.Rmd’ using ‘UTF-8’... OK
      ‘e_probabilistic.Rmd’ using ‘UTF-8’... failed
      ‘f_sensitivity.Rmd’ using ‘UTF-8’... OK
      ‘g_heterogeneity.Rmd’ using ‘UTF-8’... OK
      ‘h_tabular.Rmd’ using ‘UTF-8’... OK
      ‘i_reproduction.Rmd’ using ‘UTF-8’... OK
      ‘j_survival.Rmd’ using ‘UTF-8’... OK
      ‘j_survival_2_psa.Rmd’ using ‘UTF-8’... OK
      ‘k_calibration.Rmd’ using ‘UTF-8’... OK
    ```

# hesim

<details>

* Version: 0.5.5
* GitHub: https://github.com/hesim-dev/hesim
* Source code: https://github.com/cran/hesim
* Date/Publication: 2024-09-18 23:10:02 UTC
* Number of recursive dependencies: 106

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
      installed size is 36.8Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        data   1.5Mb
        doc    2.2Mb
        libs  30.8Mb
    ```

# hmsidwR

<details>

* Version: 1.1.2
* GitHub: https://github.com/Fgazzelloni/hmsidwR
* Source code: https://github.com/cran/hmsidwR
* Date/Publication: 2024-11-13 15:00:02 UTC
* Number of recursive dependencies: 177

Run `revdepcheck::cloud_details(, "hmsidwR")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘hmsidwR.Rmd’
      ...
    The following object is masked from ‘package:graphics’:
    
        layout
    
    
    > plotly::ggplotly(id)
    
      When sourcing ‘hmsidwR.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘hmsidwR.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘hmsidwR.Rmd’ using rmarkdown
    
    Quitting from lines 52-84 [unnamed-chunk-6] (hmsidwR.Rmd)
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
      installed size is  7.2Mb
      sub-directories of 1Mb or more:
        data   3.0Mb
        doc    4.0Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1686 marked UTF-8 strings
    ```

# HVT

<details>

* Version: 24.9.1
* GitHub: https://github.com/Mu-Sigma/HVT
* Source code: https://github.com/cran/HVT
* Date/Publication: 2024-09-11 09:50:02 UTC
* Number of recursive dependencies: 209

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
      Ignoring unknown parameters: `check_overlap`
    Scale for x is already present.
    Adding another scale for x, which will replace the existing scale.
    Scale for y is already present.
    Adding another scale for y, which will replace the existing scale.
    Warning in geom_polygon(data = boundaryCoords2, aes(x = bp.x, y = bp.y,  :
      Ignoring unknown aesthetics: text
    Error in pm[[2]] : subscript out of bounds
    Calls: scoreHVT -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

*   checking whether package ‘HVT’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(T, unlist(ijresplt))': unused argument (unlist(ijresplt)) 
    See ‘/tmp/workdir/HVT/new/HVT.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    hvq: possible error in is.element(T, unlist(ijresplt)): unused argument
      (unlist(ijresplt))
    ```

# hypsoLoop

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/hypsoLoop
* Date/Publication: 2022-02-08 09:00:02 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "hypsoLoop")` for more info

</details>

## Newly broken

*   checking whether package ‘hypsoLoop’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::set_theme’ by ‘sjPlot::set_theme’ when loading ‘hypsoLoop’
    See ‘/tmp/workdir/hypsoLoop/new/hypsoLoop.Rcheck/00install.out’ for details.
    ```

# iCARH

<details>

* Version: 2.0.2.1
* GitHub: NA
* Source code: https://github.com/cran/iCARH
* Date/Publication: 2020-08-27 07:50:07 UTC
* Number of recursive dependencies: 121

Run `revdepcheck::cloud_details(, "iCARH")` for more info

</details>

## Newly broken

*   checking whether package ‘iCARH’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(unique(paste0("cpd:", ': unused argument (igraph::get.vertex.attribute(x, "name")) 
    See ‘/tmp/workdir/iCARH/new/iCARH.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    GetDistanceMat : <anonymous>: possible error in
      is.element(unique(paste0("cpd:", unlist(metset))),
      igraph::get.vertex.attribute(x, "name")): unused argument
      (igraph::get.vertex.attribute(x, "name"))
    ```

# ICSsmoothing

<details>

* Version: 1.2.8
* GitHub: NA
* Source code: https://github.com/cran/ICSsmoothing
* Date/Publication: 2024-01-10 10:33:21 UTC
* Number of recursive dependencies: 63

Run `revdepcheck::cloud_details(, "ICSsmoothing")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ICSsmoothing)
      Loading required package: polynom
      Loading required package: ggplot2
      
      Attaching package: 'ggplot2'
      
    ...
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 13 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test_expl_spline.R:95:3'): cics_explicit works correctly. ─────────
      `is_spline` not equal to TRUE.
      1 element mismatch
      
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 13 ]
      Error: Test failures
      Execution halted
    ```

# incidental

<details>

* Version: 0.1
* GitHub: NA
* Source code: https://github.com/cran/incidental
* Date/Publication: 2020-09-16 09:50:03 UTC
* Number of recursive dependencies: 66

Run `revdepcheck::cloud_details(, "incidental")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘incidental-tutorial.Rmd’
      ...
    
    > data_subset = do.call("rbind", model_df_list)
    
    > ggplot(data_subset, aes(x = Time, y = Reported)) + 
    +     geom_point(color = "coral2", shape = 3) + geom_line(aes(x = Time, 
    +     y = Ihat), color  .... [TRUNCATED] 
    
      When sourcing ‘incidental-tutorial.R’:
    Error: `x` must be a vector, not a <data.frame/incidence_spline_model_df> object.
    Execution halted
    
      ‘incidental-tutorial.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘incidental-tutorial.Rmd’ using rmarkdown
    ```

# industRial

<details>

* Version: 0.1.0
* GitHub: https://github.com/J-Ramalho/industRial
* Source code: https://github.com/cran/industRial
* Date/Publication: 2021-06-11 09:40:02 UTC
* Number of recursive dependencies: 192

Run `revdepcheck::cloud_details(, "industRial")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘industRial-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: theme_qcc
    > ### Title: Custom theme "qcc" for the book industRial Data Science plots
    > ### Aliases: theme_qcc
    > 
    > ### ** Examples
    > 
    > library(dplyr)
    ...
    Backtrace:
        ▆
     1. └─industRial::theme_qcc()
     2.   ├─... %+replace% ...
     3.   │ └─ggplot2::is.theme(e2)
     4.   └─ggplot2::theme(...)
     5.     └─lifecycle::deprecate_stop("2.2.0", "theme(panel.margin)", "theme(panel.spacing)")
     6.       └─lifecycle:::deprecate_stop0(msg)
     7.         └─rlang::cnd_signal(...)
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

# infer

<details>

* Version: 1.0.7
* GitHub: https://github.com/tidymodels/infer
* Source code: https://github.com/cran/infer
* Date/Publication: 2024-03-25 21:50:02 UTC
* Number of recursive dependencies: 126

Run `revdepcheck::cloud_details(, "infer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘infer-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: shade_confidence_interval
    > ### Title: Add information about confidence interval
    > ### Aliases: shade_confidence_interval shade_ci
    > 
    > ### ** Examples
    > 
    > # find the point estimate---mean number of hours worked per week
    ...
     10. │           └─ggplot2:::ggplot_add.list(object, p, objectname)
     11. │             ├─ggplot2::ggplot_add(o, plot, object_name)
     12. │             └─ggplot2:::ggplot_add.Layer(o, plot, object_name)
     13. │               └─ggplot2:::new_layer_names(object, names(plot$layers))
     14. │                 └─vctrs::vec_as_names(names, repair = "check_unique")
     15. │                   └─vctrs (local) `<fn>`()
     16. │                     └─vctrs:::validate_unique(names = names, arg = arg, call = call)
     17. │                       └─vctrs:::validate_minimal_names(names, n)
     18. └─rlang::abort(message = message, call = call)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘infer.Rmd’
      ...
    # A tibble: 1 × 2
      lower_ci upper_ci
         <dbl>    <dbl>
    1     40.1     42.7
    
    > visualize(t_dist) + shade_confidence_interval(theor_ci)
    
    ...
      When sourcing ‘observed_stat_examples.R’:
    Error: Names repair functions can't return `NA` values.
    Execution halted
    
      ‘anova.Rmd’ using ‘UTF-8’... OK
      ‘chi_squared.Rmd’ using ‘UTF-8’... OK
      ‘infer.Rmd’ using ‘UTF-8’... failed
      ‘observed_stat_examples.Rmd’ using ‘UTF-8’... failed
      ‘paired.Rmd’ using ‘UTF-8’... OK
      ‘t_test.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘anova.Rmd’ using rmarkdown
    ```

# inferCSN

<details>

* Version: 1.0.8
* GitHub: https://github.com/mengxu98/inferCSN
* Source code: https://github.com/cran/inferCSN
* Date/Publication: 2024-08-24 05:30:02 UTC
* Number of recursive dependencies: 199

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
      installed size is 22.5Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        libs  19.9Mb
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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘model-injury-data-ii.Rmd’
      ...
    seasonb=2018/2019 19     17    106      84     165
    
    > ggsurvplot(fit, data = injd_sub, palette = c("#E7B800", 
    +     "#2E9FDF")) + xlab("Time [calendar days]") + ylab(expression("Survival probability  ( ..." ... [TRUNCATED] 
    Warning in eval(ei, envir) :
      Incompatible methods ("+.ggsurv", "+.gg") for "+"
    
      When sourcing ‘model-injury-data-ii.R’:
    Error: non-numeric argument to binary operator
    Execution halted
    
      ‘estimate-epi-measures.Rmd’ using ‘UTF-8’... OK
      ‘model-injury-data-i.Rmd’ using ‘UTF-8’... OK
      ‘model-injury-data-ii.Rmd’ using ‘UTF-8’... failed
      ‘prepare-injury-data.Rmd’ using ‘UTF-8’... OK
      ‘visualize-injury-data.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
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
* Number of recursive dependencies: 112

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
        5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        6. └─inTextSummaryTable::subjectProfileSummaryPlot(...)
        7.   ├─base::do.call(plyr::rbind.fill, ggplot_build(gg)$data)
        8.   └─plyr (local) `<fn>`(`<df[,12]>`, `<df[,13]>`)
        9.     └─plyr:::output_template(dfs, nrows)
       10.       └─plyr:::allocate_column(df[[var]], nrows, dfs, var)
      
      [ FAIL 59 | WARN 0 | SKIP 0 | PASS 881 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘inTextSummaryTable-aesthetics.Rmd’
      ...
    > subjectProfileSummaryPlot(data = summaryTable, xVar = "visit", 
    +     colorVar = "TRT")
    
      When sourcing ‘inTextSummaryTable-aesthetics.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 2nd layer.
    Caused by error in `check_aesthetics()`:
    ...
    ✖ Fix the following mappings: `size`.
    Execution halted
    
      ‘inTextSummaryTable-advanced.Rmd’ using ‘UTF-8’... OK
      ‘inTextSummaryTable-aesthetics.Rmd’ using ‘UTF-8’... failed
      ‘inTextSummaryTable-createTables.Rmd’ using ‘UTF-8’... OK
      ‘inTextSummaryTable-exportTables.Rmd’ using ‘UTF-8’... OK
      ‘inTextSummaryTable-introduction.Rmd’ using ‘UTF-8’... OK
      ‘inTextSummaryTable-standardTables.Rmd’ using ‘UTF-8’... OK
      ‘inTextSummaryTable-visualization.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘inTextSummaryTable-advanced.Rmd’ using rmarkdown
    --- finished re-building ‘inTextSummaryTable-advanced.Rmd’
    
    --- re-building ‘inTextSummaryTable-aesthetics.Rmd’ using rmarkdown
    
    Quitting from lines 211-224 [aesthetics-defaultsVisualization] (inTextSummaryTable-aesthetics.Rmd)
    Error: processing vignette 'inTextSummaryTable-aesthetics.Rmd' failed with diagnostics:
    Problem while setting up geom aesthetics.
    ℹ Error occurred in the 2nd layer.
    ...
    ! Aesthetics must be either length 1 or the same as the data (28).
    ✖ Fix the following mappings: `size`.
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
      installed size is 10.3Mb
      sub-directories of 1Mb or more:
        doc   9.7Mb
    ```

# inti

<details>

* Version: 0.6.6
* GitHub: https://github.com/flavjack/inti
* Source code: https://github.com/cran/inti
* Date/Publication: 2024-09-03 18:00:02 UTC
* Number of recursive dependencies: 124

Run `revdepcheck::cloud_details(, "inti")` for more info

</details>

## Newly broken

*   checking whether package ‘inti’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(variable, ': unused argument (colums_to_recode) 
    See ‘/tmp/workdir/inti/new/inti.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    metamorphosis : rename_levels: possible error in is.element(variable,
      colums_to_recode): unused argument (colums_to_recode)
    ```

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘inti-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: H2cal
    > ### Title: Broad-sense heritability in plant breeding
    > ### Aliases: H2cal
    > 
    > ### ** Examples
    > 
    > 
    ...
    +             , fixed.model = "0 + (1|bloque) + geno"
    +             , random.model = "1 + (1|bloque) + (1|geno)"
    +             , emmeans = TRUE
    +             , plot_diag = FALSE
    +             , outliers.rm = TRUE
    +             )
    Error in initializePtr() : 
      function 'cholmod_factor_ldetA' not provided by package 'Matrix'
    Calls: H2cal ... initialize -> <Anonymous> -> initializePtr -> .Call
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘heritability.Rmd’
      ...
    > library(inti)
    
    > dt <- potato
    
    > hr <- H2cal(data = dt, trait = "stemdw", gen.name = "geno", 
    +     rep.n = 5, fixed.model = "0 + (1|bloque) + geno", random.model = "1 + (1|bloque)  ..." ... [TRUNCATED] 
    
    ...
      When sourcing ‘heritability.R’:
    Error: function 'cholmod_factor_ldetA' not provided by package 'Matrix'
    Execution halted
    
      ‘apps.Rmd’ using ‘UTF-8’... OK
      ‘heritability.Rmd’ using ‘UTF-8’... failed
      ‘policy.Rmd’ using ‘UTF-8’... OK
      ‘rticles.Rmd’ using ‘UTF-8’... OK
      ‘tarpuy.Rmd’ using ‘UTF-8’... OK
      ‘yupana.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘apps.Rmd’ using rmarkdown
    --- finished re-building ‘apps.Rmd’
    
    --- re-building ‘heritability.Rmd’ using rmarkdown
    
    Quitting from lines 105-116 [unnamed-chunk-2] (heritability.Rmd)
    Error: processing vignette 'heritability.Rmd' failed with diagnostics:
    function 'cholmod_factor_ldetA' not provided by package 'Matrix'
    --- failed re-building ‘heritability.Rmd’
    ...
    --- finished re-building ‘tarpuy.Rmd’
    
    --- re-building ‘yupana.Rmd’ using rmarkdown
    --- finished re-building ‘yupana.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘heritability.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# inventorize

<details>

* Version: 1.1.1
* GitHub: NA
* Source code: https://github.com/cran/inventorize
* Date/Publication: 2022-05-31 22:20:09 UTC
* Number of recursive dependencies: 71

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
Warning in qgamma(service_level, alpha, beta) : NaNs produced
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (inventorize)


```
# IPV

<details>

* Version: 1.0.0
* GitHub: https://github.com/NilsPetras/IPV
* Source code: https://github.com/cran/IPV
* Date/Publication: 2022-09-30 15:00:02 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "IPV")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘IPV-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: nested_chart
    > ### Title: Nested Chart
    > ### Aliases: nested_chart
    > 
    > ### ** Examples
    > 
    > # as simple as that
    ...
      6.         └─ggplot2:::ggplot_add.Layer(object, p, objectname)
      7.           └─ggplot2:::new_layer_names(object, names(plot$layers))
      8.             └─vctrs::vec_as_names(names, repair = "check_unique")
      9.               └─vctrs (local) `<fn>`()
     10.                 └─vctrs:::validate_unique(names = names, arg = arg, call = call)
     11.                   └─vctrs:::stop_names_cannot_be_empty(names, call = call)
     12.                     └─vctrs:::stop_names(...)
     13.                       └─vctrs:::stop_vctrs(...)
     14.                         └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ipv-vignette.Rmd’
      ...
    Facet circle radius set to 0.211 based on the data.
    cor_spacing set to 0.193 based on the data.
    Relative scaling set to 3.78 based on the data.
    Axis tick set to 0.1 based on the data.
    dist_construct_label set to 0.5 based on the data.
    
      When sourcing ‘ipv-vignette.R’:
    Error: Names can't be empty.
    ✖ Empty name found at location 4.
    Execution halted
    
      ‘ipv-vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ipv-vignette.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.0Mb
      sub-directories of 1Mb or more:
        data   7.0Mb
    ```

# IRon

<details>

* Version: 0.1.4
* GitHub: https://github.com/nunompmoniz/IRon
* Source code: https://github.com/cran/IRon
* Date/Publication: 2023-01-20 07:20:06 UTC
* Number of recursive dependencies: 84

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
* Number of recursive dependencies: 122

Run `revdepcheck::cloud_details(, "isoorbi")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘dual_inlet.Rmd’
      ...
    |20230518_05_USGS32_vs_USGS34 |         16|     7|reference   |changeover |      NA|         10695|       11020|         65.019|       66.994|
    |20230518_05_USGS32_vs_USGS34 |         17|     7|reference   |data       |      NA|         11025|       12335|         67.025|       74.985|
    
    > orbi_plot_raw_data(df_w_blocks, isotopocules = "15N", 
    +     y = ions.incremental)
    
      When sourcing ‘dual_inlet.R’:
    Error: Names can't be empty.
    ✖ Empty name found at location 1.
    Execution halted
    
      ‘dual_inlet.Rmd’ using ‘UTF-8’... failed
      ‘flow_injection.Rmd’ using ‘UTF-8’... OK
      ‘isoxl_demo.Rmd’ using ‘UTF-8’... OK
      ‘quick_start.Rmd’ using ‘UTF-8’... OK
      ‘shot_noise.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘dual_inlet.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.2Mb
      sub-directories of 1Mb or more:
        doc       2.0Mb
        extdata   3.3Mb
    ```

# jenga

<details>

* Version: 1.3.0
* GitHub: NA
* Source code: https://github.com/cran/jenga
* Date/Publication: 2022-08-18 08:10:02 UTC
* Number of recursive dependencies: 111

Run `revdepcheck::cloud_details(, "jenga")` for more info

</details>

## Newly broken

*   checking whether package ‘jenga’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::is.scale’ by ‘greybox::is.scale’ when loading ‘jenga’
    See ‘/tmp/workdir/jenga/new/jenga.Rcheck/00install.out’ for details.
    ```

# karel

<details>

* Version: 0.1.1
* GitHub: https://github.com/mpru/karel
* Source code: https://github.com/cran/karel
* Date/Publication: 2022-03-26 21:50:02 UTC
* Number of recursive dependencies: 89

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

* Version: 1.2.0
* GitHub: https://github.com/silvaneojunior/kDGLM
* Source code: https://github.com/cran/kDGLM
* Date/Publication: 2024-05-25 09:50:03 UTC
* Number of recursive dependencies: 136

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

# Keyboard

<details>

* Version: 0.1.3
* GitHub: NA
* Source code: https://github.com/cran/Keyboard
* Date/Publication: 2022-08-11 10:10:17 UTC
* Number of recursive dependencies: 30

Run `revdepcheck::cloud_details(, "Keyboard")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Keyboard-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: select.mtd.comb.kb
    > ### Title: Maximum Tolerated Dose (MTD) Selection for Drug-combination
    > ###   Trials
    > ### Aliases: select.mtd.comb.kb
    > 
    > ### ** Examples
    > 
    ...
     0.15   0.19   0.27     NA     NA  
       NA     NA   0.50     NA     NA  
    
    NOTE: no estimate is provided for the doses at which no patient was treated.
    
    > plot_kb(sel.comb)
    Error in is.element(strpattern, c("none", names(objectPlot))) : 
      unused argument (c("none", names(objectPlot)))
    Calls: plot_kb
    Execution halted
    ```

*   checking whether package ‘Keyboard’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(strpattern, ': unused argument (c("none", names(objectPlot))) 
    See ‘/tmp/workdir/Keyboard/new/Keyboard.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    plot_kb: possible error in is.element(strpattern, c("none",
      names(objectPlot))): unused argument (c("none", names(objectPlot)))
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
      0.018   0.000   0.017 
    > # Heatmap for latent correlation matrix.
    > Heatmap_R_approx = latentcor(X = X, types = "tru", method = "approx",
    +                              showplot = TRUE)$plotR
    Error in pm[[2]] : subscript out of bounds
    Calls: latentcor ... %>% -> layout -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# lcars

<details>

* Version: 0.4.0
* GitHub: https://github.com/leonawicz/lcars
* Source code: https://github.com/cran/lcars
* Date/Publication: 2024-09-11 22:52:42 UTC
* Number of recursive dependencies: 87

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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘lcars.Rmd’
      ...
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family '0.5' not found in PostScript font database
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family '0.5' not found in PostScript font database
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family '0.5' not found in PostScript font database
    
      When sourcing ‘lcars.R’:
    Error: invalid font type
    Execution halted
    
      ‘lcars.Rmd’ using ‘UTF-8’... failed
    ```

# lemon

<details>

* Version: 0.5.0
* GitHub: https://github.com/stefanedwards/lemon
* Source code: https://github.com/cran/lemon
* Date/Publication: 2024-11-10 18:20:02 UTC
* Number of recursive dependencies: 75

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
    ...
        is.element
    
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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘capped-axes.Rmd’
      ...
    > p + coord_capped_cart(bottom = "right")
    
    > p + coord_capped_cart(bottom = "right", left = "none")
    
    > ggplot(dat1, aes(gp, y)) + geom_point(position = position_jitter(width = 0.2, 
    +     height = 0)) + coord_capped_cart(left = "none", bottom = bracke .... [TRUNCATED] 
    
    ...
      When sourcing ‘legends.R’:
    Error: Could not find panel named `panel-1-5`.
    Execution halted
    
      ‘capped-axes.Rmd’ using ‘UTF-8’... failed
      ‘facet-rep-labels.Rmd’ using ‘UTF-8’... failed
      ‘geoms.Rmd’ using ‘UTF-8’... OK
      ‘gtable_show_lemonade.Rmd’ using ‘UTF-8’... OK
      ‘legends.Rmd’ using ‘UTF-8’... failed
      ‘lemon_print.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
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
* Number of recursive dependencies: 145

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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘user_guide.Rmd’
      ...
    > yeast$`Best combinations`
      PCV_best_combination PEV_best_combination PMAD_best_combination
    1     rlr_knn, rlr_lls              vsn_lls               rlr_lls
    
    > Boxplot_data(yeast$rlr_knn_data)
    Using Majority protein IDs as id variables
    
      When sourcing ‘user_guide.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘user_guide.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘user_guide.Rmd’ using rmarkdown
    
    Quitting from lines 55-56 [unnamed-chunk-8] (user_guide.Rmd)
    Error: processing vignette 'user_guide.Rmd' failed with diagnostics:
    subscript out of bounds
    --- failed re-building ‘user_guide.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘user_guide.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# lilikoi

<details>

* Version: 2.1.1
* GitHub: NA
* Source code: https://github.com/cran/lilikoi
* Date/Publication: 2022-10-05 19:00:02 UTC
* Number of recursive dependencies: 215

Run `revdepcheck::cloud_details(, "lilikoi")` for more info

</details>

## Newly broken

*   checking whether package ‘lilikoi’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(method, c("knn"))': unused argument (c("knn")) 
      Note: possible error in 'is.element(method, c("standard", ': unused argument (c("standard", "quantile", "median", "knn")) 
    See ‘/tmp/workdir/lilikoi/new/lilikoi.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    lilikoi.preproc_knn: possible error in is.element(method, c("knn")):
      unused argument (c("knn"))
    lilikoi.preproc_norm: possible error in is.element(method,
      c("standard", "quantile", "median", "knn")): unused argument
      (c("standard", "quantile", "median", "knn"))
    ```

# LMoFit

<details>

* Version: 0.1.7
* GitHub: NA
* Source code: https://github.com/cran/LMoFit
* Date/Publication: 2024-05-14 07:33:23 UTC
* Number of recursive dependencies: 62

Run `revdepcheck::cloud_details(, "LMoFit")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘LMoFit.Rmd’
      ...
    
    > lspace_BrIII
    
      When sourcing ‘LMoFit.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `compute_geom_2()`:
    ! unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, 
        c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, 
    Execution halted
    
      ‘LMoFit.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘LMoFit.Rmd’ using rmarkdown
    
    Quitting from lines 236-237 [unnamed-chunk-15] (LMoFit.Rmd)
    Error: processing vignette 'LMoFit.Rmd' failed with diagnostics:
    Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `compute_geom_2()`:
    ! unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, 
    ...
        NULL, NULL, 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, c(0, 0, 0, 0), list(), 11, list("white", NA, NULL, NULL, TRUE), list(), 5.5, NULL, NULL, list("grey92", NULL, NULL, NULL, FALSE, TRUE), list(), list(), NULL, NULL, NULL, NULL, FALSE, list(NULL, "white", NULL, NULL, TRUE), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, c(0, 
    0, 5.5, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(5.5, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, TRUE), "topleft", NULL, c(5.5, 5.5, 5.5, 5.5), list("white", "black", 2, NULL, TRUE), NULL, NULL, "inherit", "inside", list(NULL, NULL, "grey10", 0.8, NULL, NULL, NULL, NULL, c(4.4, 4.4, 4.4, 4.4), NULL, TRUE), NULL, NULL, NULL, list(NULL, NULL, 
        NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75, list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5)))
    --- failed re-building ‘LMoFit.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘LMoFit.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.0Mb
      sub-directories of 1Mb or more:
        data   6.5Mb
    ```

# LocalCop

<details>

* Version: 0.0.2
* GitHub: https://github.com/mlysy/LocalCop
* Source code: https://github.com/cran/LocalCop
* Date/Publication: 2024-09-12 17:41:03 UTC
* Number of recursive dependencies: 91

Run `revdepcheck::cloud_details(, "LocalCop")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘LocalCop.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘LocalCop.Rmd’
      ...
    > tibble(x = x0, True = BiCopEta2Tau(family, eta = eta_fun(x0)), 
    +     Fitted = BiCopEta2Tau(fitseq$eta, family = family)) %>% pivot_longer(True:Fitt .... [TRUNCATED] 
    Warning: Removed 51 rows containing missing values or values outside the scale range
    (`geom_line()`).
    Warning: Removed 51 rows containing missing values or values outside the scale range
    (`geom_point()`).
    
      When sourcing ‘LocalCop.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘LocalCop.Rmd’ using ‘UTF-8’... failed
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 56.2Mb
      sub-directories of 1Mb or more:
        libs  55.7Mb
    ```

# lognorm

<details>

* Version: 0.1.10
* GitHub: https://github.com/bgctw/lognorm
* Source code: https://github.com/cran/lognorm
* Date/Publication: 2021-11-21 17:50:10 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::cloud_details(, "lognorm")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘lognorm.Rmd’
      ...
    > ans %>% ggplot(aes(x, density, linetype = sigmaStar, 
    +     color = sigmaStar)) + geom_area(data = ansNormal, aes(linetype = NA, 
    +     color = NA), .... [TRUNCATED] 
    Warning: A numeric `legend.position` argument in `theme()` was deprecated in ggplot2
    3.5.0.
    ℹ Please use the `legend.position.inside` argument of `theme()` instead.
    
      When sourcing ‘lognorm.R’:
    Error: invalid hex digit in 'color' or 'lty'
    Execution halted
    
      ‘lognorm.Rmd’ using ‘UTF-8’... failed
      ‘lognormalDiff.Rmd’ using ‘UTF-8’... OK
      ‘lognormalSum.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘lognorm.Rmd’ using rmarkdown
    
    Quitting from lines 51-78 [lognormalDensities] (lognorm.Rmd)
    Error: processing vignette 'lognorm.Rmd' failed with diagnostics:
    invalid hex digit in 'color' or 'lty'
    --- failed re-building ‘lognorm.Rmd’
    
    --- re-building ‘lognormalDiff.Rmd’ using rmarkdown
    ```

# lsl

<details>

* Version: 0.5.6
* GitHub: NA
* Source code: https://github.com/cran/lsl
* Date/Publication: 2017-11-08 05:30:21 UTC
* Number of recursive dependencies: 38

Run `revdepcheck::cloud_details(, "lsl")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘lsl-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: lslSEM-class
    > ### Title: A Reference Class for Learning a SEM model via penalized
    > ###   likelihood.
    > ### Aliases: lslSEM-class lslSEM
    > 
    > ### ** Examples
    > 
    ...
    > 
    > rc_sem <- lslSEM()
    > rc_sem$input(raw = lavaan::HolzingerSwineford1939)
    > rc_sem$specify(pattern = list(lambda = lambda))
    > rc_sem$learn(penalty = list(type = "l1", gamma = seq(.05, .10, .05)), variable = 7:15)
    > rc_sem$draw(type = "individual", object = "lambda")
    Error in is.element(path[, 1], theta_names_penalized) : 
      unused argument (theta_names_penalized)
    Calls: <Anonymous>
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# MAINT.Data

<details>

* Version: 2.7.1
* GitHub: NA
* Source code: https://github.com/cran/MAINT.Data
* Date/Publication: 2023-04-04 08:10:02 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::cloud_details(, "MAINT.Data")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘MAINT.Data-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ConfMat
    > ### Title: Confussion Matrices for classification results
    > ### Aliases: ConfMat
    > ### Keywords: discriminant analysis error rates
    > 
    > ### ** Examples
    > 
    ...
    > # on the temperatures by quarter in 60 Chinese meteorological stations.
    > 
    > ChinaT <- IData(ChinaTemp[1:8],VarNames=c("T1","T2","T3","T4"))
    > 
    > #Linear Discriminant Analysis
    > 
    > ChinaT.lda <- lda(ChinaT,ChinaTemp$GeoReg)
    Error in is.element(1, Config) : unused argument (Config)
    Calls: lda -> lda -> .local -> IdtNmle
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > test_check("MAINT.Data")
      Loading required package: MAINT.Data
      Loading required package: Rcpp
      
      Attaching package: 'MAINT.Data'
      
    ...
      ── Error ('test-mle.R:49:5'): mle computes correct standar errors for default Gaussian models ──
      Error in `is.element(Conf, 1:4)`: unused argument (1:4)
      Backtrace:
          ▆
       1. ├─MAINT.Data::mle(AbaloneIdt, CovCase = Cv) at test-mle.R:49:5
       2. └─MAINT.Data::mle(AbaloneIdt, CovCase = Cv)
      
      [ FAIL 10 | WARN 24 | SKIP 0 | PASS 74 ]
      Error: Test failures
      Execution halted
    ```

*   checking whether package ‘MAINT.Data’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(4, CovCase)': unused argument (CovCase) 
      Note: possible error in 'is.element(model, names(x@allres$RepresHom))': unused argument (names(x@allres$RepresHom)) 
      Note: possible error in 'is.element(Conf, 1:4)': unused argument (1:4) 
      Note: possible error in 'is.element(ModE@ModelConfig[1], ': unused argument (RestMod) 
      Note: possible error in 'is.element(nCovCases + ': unused argument (RestMod) 
      Note: possible error in 'is.element(Config, 2:5)': unused argument (2:5) 
      Note: possible error in 'is.element(Config, 1:5)': unused argument (1:5) 
      Note: possible error in 'is.element(2, Config)': unused argument (Config) 
      Note: possible error in 'is.element(1, Config)': unused argument (Config) 
    ...
      Note: possible error in 'is.element(5, Config)': unused argument (Config) 
      Note: possible error in 'is.element(4, Config)': unused argument (Config) 
      Note: possible error in 'is.element(3, Config)': unused argument (Config) 
      Note: possible error in 'is.element(x, Rewind)': unused argument (Rewind) 
      Note: possible error in 'is.element(Conf, 1:5)': unused argument (1:5) 
      Note: possible error in 'is.element(Config, c(1, ': unused argument (c(1, 4)) 
    See ‘/tmp/workdir/MAINT.Data/new/MAINT.Data.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    CovC1toRestCov: possible error in is.element(Config, 2:5): unused
      argument (2:5)
    GetCovPar: possible error in is.element(Config, 1:5): unused argument
      (1:5)
    GetSigmaPar: possible error in is.element(Config, 1:5): unused argument
      (1:5)
    IdtHetMxtNmle: possible error in is.element(2, Config): unused argument
      (Config)
    IdtNmle: possible error in is.element(1, Config): unused argument
      (Config)
    ...
    summary,IdtMclust: possible error in is.element(model,
      names(object@allres$RepresHom)): unused argument
      (names(object@allres$RepresHom))
    summary,IdtMclust: possible error in is.element(model,
      names(object@allres$RepresHet)): unused argument
      (names(object@allres$RepresHet))
    testMod,IdtE: possible error in is.element(ModE@ModelConfig[1],
      RestMod): unused argument (RestMod)
    testMod,IdtE: possible error in is.element(nCovCases + 1, RestMod):
      unused argument (RestMod)
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 19.3Mb
      sub-directories of 1Mb or more:
        R      3.5Mb
        data   6.5Mb
        libs   8.9Mb
    ```

# MarketMatching

<details>

* Version: 1.2.1
* GitHub: NA
* Source code: https://github.com/cran/MarketMatching
* Date/Publication: 2024-01-31 09:40:02 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::cloud_details(, "MarketMatching")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘MarketMatching-Vignette.Rmd’ using rmarkdown_notangle
    
    Quitting from lines 79-105 [unnamed-chunk-1] (MarketMatching-Vignette.Rmd)
    Error: processing vignette 'MarketMatching-Vignette.Rmd' failed with diagnostics:
    unused argument (c("B", "M", "K"))
    --- failed re-building ‘MarketMatching-Vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘MarketMatching-Vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# metacart

<details>

* Version: 2.0-3
* GitHub: NA
* Source code: https://github.com/cran/metacart
* Date/Publication: 2020-07-10 11:30:05 UTC
* Number of recursive dependencies: 31

Run `revdepcheck::cloud_details(, "metacart")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘metacart-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: FEmrt
    > ### Title: Fixed effect meta-tree
    > ### Aliases: FEmrt
    > 
    > ### ** Examples
    > 
    > data(dat.BCT2009)
    ...
    1) root 106 253.03570 0.2294837  
      2) T1=0 69 168.39650 0.2018800 *
      3) T1=1 37  67.44715 0.3222563  
        6) T4=0 15  20.19503 0.1906789 *
        7) T4=1 22  23.85478 0.4377661 *
    > summary(FEtree)
    Error in is.element("FEmrt", class(object)) : 
      unused argument (class(object))
    Calls: summary -> summary.FEmrt
    Execution halted
    ```

*   checking whether package ‘metacart’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element("FEmrt", class(object))': unused argument (class(object)) 
      Note: possible error in 'is.element("REmrt", class(object))': unused argument (class(object)) 
    See ‘/tmp/workdir/metacart/new/metacart.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    summary.FEmrt: possible error in is.element("FEmrt", class(object)):
      unused argument (class(object))
    summary.REmrt: possible error in is.element("REmrt", class(object)):
      unused argument (class(object))
    ```

# MetAlyzer

<details>

* Version: 1.1.0
* GitHub: https://github.com/nilsmechtel/MetAlyzer
* Source code: https://github.com/cran/MetAlyzer
* Date/Publication: 2024-12-06 14:00:02 UTC
* Number of recursive dependencies: 98

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

# metR

<details>

* Version: 0.17.0
* GitHub: https://github.com/eliocamp/metR
* Source code: https://github.com/cran/metR
* Date/Publication: 2025-01-13 19:50:02 UTC
* Number of recursive dependencies: 120

Run `revdepcheck::cloud_details(, "metR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘metR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_contour_tanaka
    > ### Title: Illuminated contours
    > ### Aliases: geom_contour_tanaka GeomContourTanaka
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    ...
     19. │                   ├─rlang::inject(self$draw_panel(data, panel_params, coord, !!!params))
     20. │                   └─self$draw_panel(...)
     21. │                     └─metR (local) draw_panel(...)
     22. │                       └─metR:::stopf(...)
     23. │                         └─base::stop(e)
     24. └─rlang (local) `<fn>`(`<smplErrr>`)
     25.   └─handlers[[1L]](cnd)
     26.     └─cli::cli_abort(...)
     27.       └─rlang::abort(...)
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
       26. │                             └─metR:::stopf(...)
       27. │                               └─base::stop(e)
       28. └─rlang (local) `<fn>`(`<smplErrr>`)
       29.   └─handlers[[1L]](cnd)
       30.     └─cli::cli_abort(...)
       31.       └─rlang::abort(...)
      
      [ FAIL 1 | WARN 1 | SKIP 19 | PASS 184 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Visualization-tools.Rmd’
      ...
    
    > ggplot(temperature[lev == 300], aes(lon, lat, z = air.z)) + 
    +     geom_contour_fill() + geom_contour_tanaka() + scale_fill_divergent()
    
      When sourcing ‘Visualization-tools.R’:
    Error: Problem while converting geom to grob.
    ℹ Error occurred in the 2nd layer.
    Caused by error:
    ! geom_path: If you are using dotted or dashed lines, colour, size and linetype must be constant over the line.
    Execution halted
    
      ‘Visualization-tools.Rmd’ using ‘UTF-8’... failed
      ‘Working-with-data.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Visualization-tools.Rmd’ using knitr
    
    Quitting from lines 241-245 [unnamed-chunk-16] (Visualization-tools.Rmd)
    Error: processing vignette 'Visualization-tools.Rmd' failed with diagnostics:
    Problem while converting geom to grob.
    ℹ Error occurred in the 2nd layer.
    Caused by error:
    ! geom_path: If you are using dotted or dashed lines, colour, size and linetype must be constant over the line.
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

* Version: 0.1.11
* GitHub: https://github.com/mfasiolo/mgcViz
* Source code: https://github.com/cran/mgcViz
* Date/Publication: 2023-10-06 10:50:02 UTC
* Number of recursive dependencies: 112

Run `revdepcheck::cloud_details(, "mgcViz")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘mgcviz.rmd’
      ...
    > v <- getViz(b)
    
    > pl <- plotSlice(x = sm(v, 1), fix = list(z = seq(-2, 
    +     2, length.out = 3), x = c(-1, 0, 1)))
    
      When sourcing ‘mgcviz.R’:
    Error: The `facets` argument of `facet_grid()` was deprecated in ggplot2 2.2.0
    and is now defunct.
    ℹ Please use the `rows` argument instead.
    Execution halted
    
      ‘mgcviz.rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘mgcviz.rmd’ using rmarkdown
    ```

# migraph

<details>

* Version: 1.4.5
* GitHub: https://github.com/stocnet/migraph
* Source code: https://github.com/cran/migraph
* Date/Publication: 2024-12-02 10:00:10 UTC
* Number of recursive dependencies: 97

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
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 46 ]
    ...
      `expected` is a character vector ('Statistic')
      ── Failure ('test-model_tests.R:73:3'): qap plot works ─────────────────────────
      qapplot$labels$x (`actual`) not identical to "Statistic" (`expected`).
      
      `actual` is NULL
      `expected` is a character vector ('Statistic')
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 46 ]
      Error: Test failures
      Execution halted
    ```

# mikropml

<details>

* Version: 1.6.1
* GitHub: https://github.com/SchlossLab/mikropml
* Source code: https://github.com/cran/mikropml
* Date/Publication: 2023-08-21 15:10:05 UTC
* Number of recursive dependencies: 129

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
* Number of recursive dependencies: 125

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
      [ FAIL 1 | WARN 11 | SKIP 0 | PASS 202 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      Backtrace:
          ▆
       1. └─miRetrieve::compare_mir_terms_scatter(df_merged, "miR-21", title = "Test_title") at test-comparemirterms.R:56:1
       2.   ├─plotly::ggplotly(plot)
       3.   └─plotly:::ggplotly.ggplot(plot)
       4.     └─plotly::gg2list(...)
      
      [ FAIL 1 | WARN 11 | SKIP 0 | PASS 202 ]
      Error: Test failures
      Execution halted
    ```

# MiscMetabar

<details>

* Version: 0.10.1
* GitHub: https://github.com/adrientaudiere/MiscMetabar
* Source code: https://github.com/cran/MiscMetabar
* Date/Publication: 2024-10-07 21:40:01 UTC
* Number of recursive dependencies: 411

Run `revdepcheck::cloud_details(, "MiscMetabar")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘MiscMetabar-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggscatt_pq
    > ### Title: Scatterplot with marginal distributions and statistical results
    > ###   against Hill diversity of phyloseq object
    > ### Aliases: ggscatt_pq
    > 
    > ### ** Examples
    > 
    ...
    Cleaning suppress 0 taxa and 0 samples.
    Taxa are now in rows.
    Joining with `by = join_by(Sample)`
    [[1]]
    `stat_xsidebin()` using `bins = 30`. Pick better value with `binwidth`.
    `stat_ysidebin()` using `bins = 30`. Pick better value with `binwidth`.
    Error in is.element(panel_type, c("x", "y")) : 
      unused argument (c("x", "y"))
    Calls: <Anonymous> ... <Anonymous> -> draw_panel -> <Anonymous> -> render_fg
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(MiscMetabar)
      Loading required package: phyloseq
      Loading required package: ggplot2
      
      Attaching package: 'ggplot2'
      
    ...
        'test_tuckey.R:26:3', 'test_targets.R:5:3', 'test_targets.R:56:3',
        'test_targets.R:84:3', 'test_targets.R:101:3', 'test_targets.R:111:3'
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test_figures_beta_div.R:72:5'): graph_test_pq works ───────────────
      `graph_test_pq(data_fungi_mini, fact = "Tree_name")` produced warnings.
      
      [ FAIL 1 | WARN 1 | SKIP 75 | PASS 83 ]
      Error: Test failures
      Execution halted
    ```

# misspi

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/misspi
* Date/Publication: 2023-10-17 09:50:02 UTC
* Number of recursive dependencies: 88

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

# mizer

<details>

* Version: 2.5.3
* GitHub: https://github.com/sizespectrum/mizer
* Source code: https://github.com/cran/mizer
* Date/Publication: 2024-10-17 07:10:09 UTC
* Number of recursive dependencies: 109

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

# MLEce

<details>

* Version: 2.1.0
* GitHub: NA
* Source code: https://github.com/cran/MLEce
* Date/Publication: 2023-09-27 14:50:06 UTC
* Number of recursive dependencies: 42

Run `revdepcheck::cloud_details(, "MLEce")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘MLEce-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: MLEce
    > ### Title: Calculating MLEces for three different distributions
    > ### Aliases: MLEce
    > 
    > ### ** Examples
    > 
    > #bivariate gamma distribution
    > data_BiGam <-  rBiGam(100, c(1,4,5))
    > res_BiGam  <- MLEce(data_BiGam, "BiGam")
    Error in is.element(distname, distlist) : unused argument (distlist)
    Calls: MLEce
    Execution halted
    ```

*   checking whether package ‘MLEce’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(distname, ': unused argument (distlist) 
    See ‘/tmp/workdir/MLEce/new/MLEce.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    MLEce: possible error in is.element(distname, distlist): unused
      argument (distlist)
    benchMLEce: possible error in is.element(distname, distlist): unused
      argument (distlist)
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
      ── Attaching packages ────────────────────────────────────── tidymodels 1.2.0 ──
      ✔ broom        1.0.7          ✔ recipes      1.1.0     
      ✔ dials        1.3.0          ✔ rsample      1.2.1     
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

# moreparty

<details>

* Version: 0.4
* GitHub: NA
* Source code: https://github.com/cran/moreparty
* Date/Publication: 2023-11-22 14:30:02 UTC
* Number of recursive dependencies: 163

Run `revdepcheck::cloud_details(, "moreparty")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Titanic_example.Rmd’
      ...
    24      Age        57 0.3187270
    25 Embarked Cherbourg 0.4603041
     [ reached 'max' / getOption("max.print") -- omitted 2 rows ]
    
    > ggForestEffects(pdep, vline = mean(pred_foret), xlab = "Probability of survival") + 
    +     xlim(c(0, 1))
    
      When sourcing ‘Titanic_example.R’:
    Error: `x` must be a vector, not a <data.frame/partial> object.
    Execution halted
    
      ‘Titanic_example.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Titanic_example.Rmd’ using rmarkdown
    ```

# mppR

<details>

* Version: 1.5.0
* GitHub: https://github.com/vincentgarin/mppR
* Source code: https://github.com/cran/mppR
* Date/Publication: 2024-02-22 17:20:02 UTC
* Number of recursive dependencies: 68

Run `revdepcheck::cloud_details(, "mppR")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘mppR_gen_vignette.Rmd’
      ...
    
    > set.seed(89341)
    
    > CV <- mpp_CV(pop.name = "USNAM", trait.name = "ULA", 
    +     mppData = mppData, Q.eff = "cr", her = 0.4, Rep = 2, k = 5, 
    +     verbose = FALSE, outp .... [TRUNCATED] 
    
      When sourcing ‘mppR_gen_vignette.R’:
    Error: unused argument (QTL.names)
    Execution halted
    
      ‘MPP_ME_QTL_detect.Rmd’ using ‘UTF-8’... OK
      ‘mppR_gen_vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking whether package ‘mppR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(mk.list, QTL.names)': unused argument (QTL.names) 
    See ‘/tmp/workdir/mppR/new/mppR.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    mpp_CV: possible error in is.element(mk.list, QTL.names): unused
      argument (QTL.names)
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘MPP_ME_QTL_detect.Rmd’ using rmarkdown
    ```

# MPTmultiverse

<details>

* Version: 0.4-2
* GitHub: https://github.com/mpt-network/MPTmultiverse
* Source code: https://github.com/cran/MPTmultiverse
* Date/Publication: 2020-06-24 09:40:11 UTC
* Number of recursive dependencies: 99

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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘introduction-bayen_kuhlmann_2011.rmd’
      ...
    ℹ Please use tidy evaluation idioms with `aes()`
    ℹ The deprecated feature was likely used in the MPTmultiverse package.
      Please report the issue at
      <https://github.com/mpt-network/MPTmultiverse/issues>.
    
      When sourcing ‘introduction-bayen_kuhlmann_2011.R’:
    Error: The `facets` argument of `facet_grid()` was deprecated in ggplot2 2.2.0
    and is now defunct.
    ℹ Please use the `rows` argument instead.
    Execution halted
    
      ‘introduction-bayen_kuhlmann_2011.rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘introduction-bayen_kuhlmann_2011.rmd’ using rmarkdown
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# mtb

<details>

* Version: 0.1.8
* GitHub: https://github.com/yh202109/mtb
* Source code: https://github.com/cran/mtb
* Date/Publication: 2022-10-20 17:22:35 UTC
* Number of recursive dependencies: 63

Run `revdepcheck::cloud_details(, "mtb")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(mtb)
      > 
      > test_check("mtb")
      
      Attaching package: 'ggplot2'
      
    ...
                 - "yend"       [6]           
                 - "xmin"       [7]           
                 - "xmax"       [8]           
                 - "ymin"       [9]           
                 - "ymax"       [10]          
      ... ...      ...          and 3 more ...
      
      [ FAIL 2 | WARN 13 | SKIP 0 | PASS 56 ]
      Error: Test failures
      Execution halted
    ```

# mudfold

<details>

* Version: 1.1.21
* GitHub: https://github.com/cran/mudfold
* Source code: https://github.com/cran/mudfold
* Date/Publication: 2022-11-24 09:30:02 UTC
* Number of recursive dependencies: 50

Run `revdepcheck::cloud_details(, "mudfold")` for more info

</details>

## Newly broken

*   checking whether package ‘mudfold’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(visitSequence, ': unused argument (names(blocks)) 
    See ‘/tmp/workdir/mudfold/new/mudfold.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    check.visitSequence: possible error in is.element(visitSequence,
      names(blocks)): unused argument (names(blocks))
    ```

# mulgar

<details>

* Version: 1.0.2
* GitHub: https://github.com/dicook/mulgar
* Source code: https://github.com/cran/mulgar
* Date/Publication: 2023-08-25 22:00:02 UTC
* Number of recursive dependencies: 43

Run `revdepcheck::cloud_details(, "mulgar")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mulgar-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggmcbic
    > ### Title: Produces an mclust summary plot with ggplot
    > ### Aliases: ggmcbic
    > 
    > ### ** Examples
    > 
    > require(mclust)
    ...
    Type 'citation("mclust")' for citing this R package in publications.
    > data(clusters)
    > clusters_BIC <- mclustBIC(clusters[,1:5], G=2:6)
    > ggmcbic(clusters_BIC)
    Warning: Removed 5 rows containing missing values or values outside the scale range
    (`geom_line()`).
    Error in grid.Call.graphics(C_segments, x$x0, x$y0, x$x1, x$y1, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.segments -> grid.Call.graphics
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.8Mb
      sub-directories of 1Mb or more:
        data   8.5Mb
    ```

# MultiTraits

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/MultiTraits
* Date/Publication: 2024-12-21 14:10:02 UTC
* Number of recursive dependencies: 138

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
      3.   ├─ggtern::ggplot_build(x)
      4.   └─ggtern:::ggplot_build.ggplot(x)
      5.     └─ggtern:::layers_add_or_remove_mask(plot)
      6.       └─ggint$plot_theme(plot)
      7.         └─ggplot2:::validate_theme(theme)
      8.           └─base::mapply(...)
      9.             └─ggplot2 (local) `<fn>`(...)
     10.               └─cli::cli_abort(...)
     11.                 └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘MultiTraits_tutorial.Rmd’
      ...
    4 490   20  16 30.293240 30.891427 38.81533  CSR
    5  51   14  31  4.068212  0.000000 95.93179    R
    6  66   27  13  5.131786 73.958953 20.90926 S/SR
    
    > CSR_plot(data = result, expand_margin = 1)
    
      When sourcing ‘MultiTraits_tutorial.R’:
    Error: The `tern.axis.ticks.length.major` theme element must be a <unit>
    object.
    Execution halted
    
      ‘MultiTraits_tutorial.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘MultiTraits_tutorial.Rmd’ using rmarkdown
    
    Quitting from lines 106-120 [unnamed-chunk-4] (MultiTraits_tutorial.Rmd)
    Error: processing vignette 'MultiTraits_tutorial.Rmd' failed with diagnostics:
    The `tern.axis.ticks.length.major` theme element must be a <unit>
    object.
    --- failed re-building ‘MultiTraits_tutorial.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘MultiTraits_tutorial.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# mvSLOUCH

<details>

* Version: 2.7.6
* GitHub: NA
* Source code: https://github.com/cran/mvSLOUCH
* Date/Publication: 2023-11-20 18:20:06 UTC
* Number of recursive dependencies: 101

Run `revdepcheck::cloud_details(, "mvSLOUCH")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘mvSLOUCH_Carnivorans.Rmd’ using rmarkdown
    trying URL 'datadryad.org/api/v2/datasets/doi%253A10.5061%252Fdryad.77tm4/download'
    downloaded 51 KB
    
    <!--
    # Copyright 2020 Jesualdo Fuentes Gonzalez, Jason Pienaar  and Krzysztof Bartoszek
    #
    # This file is part of mvSLOUCH.
    #
    ...
    Quitting from lines 147-147 [conditional_print_part2] (mvSLOUCH_Carnivorans.Rmd)
    Error: processing vignette 'mvSLOUCH_Carnivorans.Rmd' failed with diagnostics:
    unused argument (v_species_to_remove)
    --- failed re-building ‘mvSLOUCH_Carnivorans.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘mvSLOUCH_Carnivorans.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# naive

<details>

* Version: 1.2.3
* GitHub: NA
* Source code: https://github.com/cran/naive
* Date/Publication: 2023-06-20 14:30:04 UTC
* Number of recursive dependencies: 108

Run `revdepcheck::cloud_details(, "naive")` for more info

</details>

## Newly broken

*   checking whether package ‘naive’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::is.scale’ by ‘greybox::is.scale’ when loading ‘naive’
    See ‘/tmp/workdir/naive/new/naive.Rcheck/00install.out’ for details.
    ```

# ncappc

<details>

* Version: 0.3.0
* GitHub: NA
* Source code: https://github.com/cran/ncappc
* Date/Publication: 2018-08-24 20:30:03 UTC
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "ncappc")` for more info

</details>

## Newly broken

*   checking whether package ‘ncappc’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(id, obsdata[, ': unused argument (obsdata[, idNm]) 
      Note: possible error in 'is.element(id, simdata[, ': unused argument (simdata[, idNm]) 
    See ‘/tmp/workdir/ncappc/new/ncappc.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    nca.pde.deviation.outlier: possible error in is.element(id, obsdata[,
      idNm]): unused argument (obsdata[, idNm])
    nca.pde.deviation.outlier: possible error in is.element(id, simdata[,
      idNm]): unused argument (simdata[, idNm])
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘bookdown’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# neatmaps

<details>

* Version: 2.1.0
* GitHub: https://github.com/PhilBoileau/neatmaps
* Source code: https://github.com/cran/neatmaps
* Date/Publication: 2019-05-12 19:10:03 UTC
* Number of recursive dependencies: 99

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
    > df <- netsDataFrame(network_attr_df,
    +                     node_attr_df,
    +                     edge_df)
    > 
    > # run the neatmap code on df
    > neat_res <- neatmap(df, scale_df = "ecdf", max_k = 3, reps = 100, 
    +                     xlab = "vars", ylab = "nets", xlab_cex = 1, ylab_cex = 1)
    Error in pm[[2]] : subscript out of bounds
    Calls: neatmap ... %>% -> layout -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
    ```

# NetFACS

<details>

* Version: 0.5.0
* GitHub: NA
* Source code: https://github.com/cran/NetFACS
* Date/Publication: 2022-12-06 17:32:35 UTC
* Number of recursive dependencies: 100

Run `revdepcheck::cloud_details(, "NetFACS")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘NetFACS-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: network_conditional
    > ### Title: Create a network based on conditional probabilities of dyads of
    > ###   elements
    > ### Aliases: network_conditional
    > 
    > ### ** Examples
    > 
    ...
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family 'Arial Narrow' not found in PostScript font database
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family 'Arial Narrow' not found in PostScript font database
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family 'Arial Narrow' not found in PostScript font database
    Error in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  : 
      invalid font type
    Calls: <Anonymous> ... drawDetails -> drawDetails.text -> grid.Call.graphics
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘netfacs_tutorial.Rmd’
      ...
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family 'Arial Narrow' not found in PostScript font database
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family 'Arial Narrow' not found in PostScript font database
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family 'Arial Narrow' not found in PostScript font database
    
      When sourcing ‘netfacs_tutorial.R’:
    Error: invalid font type
    Execution halted
    
      ‘netfacs_tutorial.Rmd’ using ‘UTF-8’... failed
    ```

# NetworkInference

<details>

* Version: 1.2.4
* GitHub: https://github.com/desmarais-lab/NetworkInference
* Source code: https://github.com/cran/NetworkInference
* Date/Publication: 2019-02-28 05:50:06 UTC
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "NetworkInference")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘NetworkInference-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: as_cascade_long
    > ### Title: Transform long data to cascade
    > ### Aliases: as_cascade_long
    > 
    > ### ** Examples
    > 
    > 
    > df <- simulate_rnd_cascades(10, n_nodes = 20)
    > cascades <- as_cascade_long(df)
    Error in is.element(cascade_node_name, colnames(data)) : 
      unused argument (colnames(data))
    Calls: as_cascade_long ... tryCatchList -> tryCatchOne -> doTryCatch -> eval -> eval
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(NetworkInference)
      NetworkInference: Inferring latent diffusion networks.
      Version 1.2.4
      copyright (c) 2016, Fridolin Linder, Pennsylvania State University
                          Bruce Desmarais, Pennsylvania State University
      For citation information, type citation("NetworkInference").
    ...
      Error in `is.element(class(selection), c("character", "numeric", "integer", 
          "factor"))`: unused argument (c("character", "numeric", "integer", "factor"))
      Backtrace:
          ▆
       1. ├─base::plot(cascades, selection = names(cascades$cascade_nodes)[1:3]) at test_plot.R:8:5
       2. └─NetworkInference:::plot.cascade(cascades, selection = names(cascades$cascade_nodes)[1:3])
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 12 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘quickstart_vignette.Rmd’
      ...
    Type help("NetworkInference") to get started.
    
    
    > df <- simulate_rnd_cascades(50, n_node = 20)
    
    > cascades <- as_cascade_long(df)
    
    ...
    
    > policy_cascades <- as_cascade_long(policies, cascade_node_name = "statenam", 
    +     event_time = "adopt_year", cascade_id = "policy")
    
      When sourcing ‘tutorial_vignette.R’:
    Error: unused argument (colnames(data))
    Execution halted
    
      ‘quickstart_vignette.Rmd’ using ‘UTF-8’... failed
      ‘tutorial_vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking whether package ‘NetworkInference’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(cascade_node_name, ': unused argument (colnames(data)) 
      Note: possible error in 'is.element(event_time, ': unused argument (colnames(data)) 
      Note: possible error in 'is.element(cascade_id, ': unused argument (colnames(data)) 
      Note: possible error in 'is.element(class(x), ': unused argument (c("numeric", "character", "factor", "integer")) 
      Note: possible error in 'is.element(class(x), ': unused argument (c("numeric", "integer")) 
      Note: possible error in 'is.element(unique_cascade_nodes, ': unused argument (node_names) 
      Note: possible error in 'is.element(class(selection), ': unused argument (c("character", "numeric", "integer", "factor")) 
      Note: possible error in 'is.element(selection, ': unused argument (unique(pdat$cascade_id)) 
      Note: possible error in 'is.element(pdat$cascade_id, ': unused argument (selection) 
      Note: possible error in 'is.element(nodes, partial_cascade$cascade_nodes[[1]])': unused argument (partial_cascade$cascade_nodes[[1]]) 
      Note: possible error in 'is.element(partial_cascade$cascade_nodes[[1]], ': unused argument (nodes) 
    See ‘/tmp/workdir/NetworkInference/new/NetworkInference.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    as_cascade_long: possible error in is.element(cascade_node_name,
      colnames(data)): unused argument (colnames(data))
    as_cascade_long: possible error in is.element(event_time,
      colnames(data)): unused argument (colnames(data))
    as_cascade_long: possible error in is.element(cascade_id,
      colnames(data)): unused argument (colnames(data))
    assert_cascade_consistency_ : <anonymous>: possible error in
      is.element(class(x), c("numeric", "character", "factor", "integer")):
      unused argument (c("numeric", "character", "factor", "integer"))
    assert_cascade_consistency_ : <anonymous>: possible error in
    ...
    plot.cascade: possible error in is.element(selection,
      unique(pdat$cascade_id)): unused argument (unique(pdat$cascade_id))
    plot.cascade: possible error in is.element(pdat$cascade_id, selection):
      unused argument (selection)
    simulate_cascade_: possible error in is.element(nodes,
      partial_cascade$cascade_nodes[[1]]): unused argument
      (partial_cascade$cascade_nodes[[1]])
    simulate_cascades: possible error in
      is.element(partial_cascade$cascade_nodes[[1]], nodes): unused
      argument (nodes)
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘quickstart_vignette.Rmd’ using rmarkdown
    
    Quitting from lines 52-64 [unnamed-chunk-3] (quickstart_vignette.Rmd)
    Error: processing vignette 'quickstart_vignette.Rmd' failed with diagnostics:
    unused argument (colnames(data))
    --- failed re-building ‘quickstart_vignette.Rmd’
    
    --- re-building ‘tutorial_vignette.Rmd’ using rmarkdown
    ...
    Quitting from lines 99-102 [unnamed-chunk-11] (tutorial_vignette.Rmd)
    Error: processing vignette 'tutorial_vignette.Rmd' failed with diagnostics:
    unused argument (colnames(data))
    --- failed re-building ‘tutorial_vignette.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘quickstart_vignette.Rmd’ ‘tutorial_vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking C++ specification ... NOTE
    ```
      Specified C++11: please drop specification unless essential
    ```

# NHSRplotthedots

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/NHSRplotthedots
* Date/Publication: 2021-11-03 20:20:10 UTC
* Number of recursive dependencies: 87

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

# nichetools

<details>

* Version: 0.3.2
* GitHub: https://github.com/benjaminhlina/nichetools
* Source code: https://github.com/cran/nichetools
* Date/Publication: 2024-09-30 21:20:02 UTC
* Number of recursive dependencies: 118

Run `revdepcheck::cloud_details(, "nichetools")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘using-nichetools-with-the-package-SIBER.Rmd’
      ...
    +     option = "A", alpha = 0.75)
    
    > ggplot() + stat_pointinterval(data = bays_95_overlap, 
    +     aes(x = group_1, y = prop_overlap, point_fill = group_2), 
    +     interval_colour = "gre ..." ... [TRUNCATED] 
    
      When sourcing ‘using-nichetools-with-the-package-SIBER.R’:
    Error: unused argument (theme = list(list("black", 0.681818181818182, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.681818181818182, 1, TRUE), list("", "plain", "black", 15, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.681818181818182, 0.681818181818182, 1, 1, "", 5.27189705271897, 2.04545454545455, 19, TRUE), 7.5, c(7.5, 7.5, 7.5, 7.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, 
        NULL, c(3.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 3.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 3.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 3.75), NULL, TRUE), list(NULL, NULL, "#4D4D4DFF", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(3, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NU
    Execution halted
    
      ‘using-nichetools-with-the-package-SIBER.Rmd’ using ‘UTF-8’... failed
      ‘using-nichetools-with-the-package-nicheROVER.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘using-nichetools-with-the-package-SIBER.Rmd’ using rmarkdown
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
    Size of Rectangular_col: 	 99 rows x  2 columns 
    Size of Rectangular_element_max: 	 59 rows x  79 columns 
    Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
    ℹ Please use tidy evaluation idioms with `aes()`.
    ℹ See also `vignette("ggplot2-in-packages")` for more information.
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
      Warning message:
      In check_dep_version() : ABI version mismatch: 
      lme4 was built with Matrix ABI version 1
      Current Matrix ABI version is 0
      Please re-install lme4 from source or restore original 'Matrix' package
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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘NIMAA-vignette.Rmd’
      ...
    ℹ Please use tidy evaluation idioms with `aes()`.
    ℹ See also `vignette("ggplot2-in-packages")` for more information.
    ℹ The deprecated feature was likely used in the NIMAA package.
      Please report the issue at <https://github.com/jafarilab/NIMAA/issues>.
    
    Na/missing values Proportion: 	 0.2603 
    
      When sourcing ‘NIMAA-vignette.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘NIMAA-vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘NIMAA-vignette.Rmd’ using rmarkdown
    
    Quitting from lines 49-57 [plotIncMatrix function] (NIMAA-vignette.Rmd)
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

# NMF

<details>

* Version: 0.28
* GitHub: NA
* Source code: https://github.com/cran/NMF
* Date/Publication: 2024-08-22 16:20:01 UTC
* Number of recursive dependencies: 132

Run `revdepcheck::cloud_details(, "NMF")` for more info

</details>

## Newly broken

*   checking whether package ‘NMF’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/NMF/new/NMF.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘NMF-vignette.Rnw’ using knitr
    Error in citation(x) : there is no package called 'doMC'
    Converted 9 of 10 package citations to BibTeX
    Writing 11 Bibtex entries ... OK
    Results written to file 'Rpackages.bib'
    Error: processing vignette 'NMF-vignette.Rnw' failed with diagnostics:
    Running 'texi2dvi' on 'NMF-vignette.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `a4wide.sty' not found.
    ...
    l.62 \usepackage
                    {xspace}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘heatmaps.Rnw’
    
    SUMMARY: processing the following files failed:
      ‘NMF-vignette.Rnw’ ‘heatmaps.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## Installation

### Devel

```
* installing *source* package ‘NMF’ ...
** package ‘NMF’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c distance.cpp -o distance.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c divergence.cpp -o divergence.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c euclidean.cpp -o euclidean.o
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c registerDynamicSymbol.c -o registerDynamicSymbol.o
...
Creating meta registry in package 'NMF' ... OK
Creating registry 'extra_handler' in package 'NMF' ... OK
Creating registry 'extra_action' in package 'NMF' ... OK
Registering extra handler 'install.packages' [function] ... OK
Creating registry 'algorithm' in package 'NMF' ... OK
Error in is.element("...", n.update) : unused argument (n.update)
Error: unable to load R code in package ‘NMF’
Execution halted
ERROR: lazy loading failed for package ‘NMF’
* removing ‘/tmp/workdir/NMF/new/NMF.Rcheck/NMF’


```
### CRAN

```
* installing *source* package ‘NMF’ ...
** package ‘NMF’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c distance.cpp -o distance.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c divergence.cpp -o divergence.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c euclidean.cpp -o euclidean.o
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c registerDynamicSymbol.c -o registerDynamicSymbol.o
...
Registering NMF seeding method 'nndsvd' [NMFSeed] ... OK
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (NMF)


```
# nzelect

<details>

* Version: 0.4.0
* GitHub: NA
* Source code: https://github.com/cran/nzelect
* Date/Publication: 2017-10-02 20:35:23 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "nzelect")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘nzelect-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: polls
    > ### Title: New Zealand Opinion Polls
    > ### Aliases: polls
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    ...
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    Warning: Removed 159 rows containing missing values or values outside the scale range
    (`geom_line()`).
    Error in grid.Call.graphics(C_segments, x$x0, x$y0, x$x1, x$y1, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.segments -> grid.Call.graphics
    Execution halted
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

# oddsratio

<details>

* Version: 2.0.1
* GitHub: https://github.com/pat-s/oddsratio
* Source code: https://github.com/cran/oddsratio
* Date/Publication: 2020-05-24 22:00:02 UTC
* Number of recursive dependencies: 62

Run `revdepcheck::cloud_details(, "oddsratio")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘oddsratio-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: insert_or
    > ### Title: Insert odds ratios of GAM(M)s into smoothing function
    > ### Aliases: insert_or
    > 
    > ### ** Examples
    > 
    > library(oddsratio)
    ...
      5.       └─ggplot2:::ggplot_add.Layer(object, p, objectname)
      6.         └─ggplot2:::new_layer_names(object, names(plot$layers))
      7.           └─vctrs::vec_as_names(names, repair = "check_unique")
      8.             └─vctrs (local) `<fn>`()
      9.               └─vctrs:::validate_unique(names = names, arg = arg, call = call)
     10.                 └─vctrs:::stop_names_cannot_be_empty(names, call = call)
     11.                   └─vctrs:::stop_names(...)
     12.                     └─vctrs:::stop_vctrs(...)
     13.                       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘oddsratio.Rmd’
      ...
    +     pred = "x2", values = c(0.4, 0.6))
    
    > insert_or(plot, or_object2, or_yloc = 2.1, values_yloc = 2, 
    +     line_col = "green4", text_col = "black", rect_col = "green4", 
    +     rect_alpha = .... [TRUNCATED] 
    
      When sourcing ‘oddsratio.R’:
    Error: Names can't be empty.
    ✖ Empty name found at location 1.
    Execution halted
    
      ‘oddsratio.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘oddsratio.Rmd’ using rmarkdown
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘gam’
    ```

# OmicNavigator

<details>

* Version: 1.13.13
* GitHub: https://github.com/abbvie-external/OmicNavigator
* Source code: https://github.com/cran/OmicNavigator
* Date/Publication: 2023-08-25 20:40:02 UTC
* Number of recursive dependencies: 88

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
      testPlot.R....................  140 tests [0;32mOK[0m 
      testPlot.R....................  140 tests [0;32mOK[0m 
      testPlot.R....................  141 tests [0;32mOK[0m 
      testPlot.R....................  141 tests [0;32mOK[0m 
      testPlot.R....................  141 tests [0;32mOK[0m 
      testPlot.R....................  142 tests [0;32mOK[0m 
      testPlot.R....................  142 tests [0;32mOK[0m 
      testPlot.R....................  143 tests [0;32mOK[0m Error in pm[[2]] : subscript out of bounds
      Calls: suppressMessages ... plotStudy -> f -> <Anonymous> -> ggplotly.ggplot -> gg2list
      Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘OmicNavigatorAPI.Rnw’ using Sweave
    OmicNavigator R package version: 1.13.13
    The app is not installed. Install it with installApp()
    Installing study "ABC" in /tmp/RtmpIrXo7a/file22fe2698a726
    Exporting study "ABC" as an R package
    Note: No maintainer email was specified. Using the placeholder: Unknown <unknown@unknown>
    Calculating pairwise overlaps. This may take a while...
    Exported study to /tmp/RtmpIrXo7a/ONstudyABC
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

# ordbetareg

<details>

* Version: 0.7.2
* GitHub: https://github.com/saudiwin/ordbetareg_pack
* Source code: https://github.com/cran/ordbetareg
* Date/Publication: 2023-08-10 07:30:02 UTC
* Number of recursive dependencies: 177

Run `revdepcheck::cloud_details(, "ordbetareg")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ordbetareg-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pp_check_ordbeta
    > ### Title: Accurate Posterior Predictive Plots for Ordbetareg Models
    > ### Aliases: pp_check_ordbeta
    > 
    > ### ** Examples
    > 
    > 
    ...
     16. │             └─ggplot2 (local) setup_params(...)
     17. │               └─ggplot2:::make_summary_fun(...)
     18. │                 └─rlang::as_function(fun.data)
     19. │                   └─base::get(x, envir = env, mode = "function")
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
      installed size is  8.1Mb
      sub-directories of 1Mb or more:
        data   7.5Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 36 marked UTF-8 strings
    ```

# pafr

<details>

* Version: 0.0.2
* GitHub: https://github.com/dwinter/pafr
* Source code: https://github.com/cran/pafr
* Date/Publication: 2020-12-08 10:20:12 UTC
* Number of recursive dependencies: 113

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
      
      Attaching package: 'ggplot2'
      
      The following object is masked from 'package:base':
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
* Number of recursive dependencies: 105

Run `revdepcheck::cloud_details(, "pdxTrees")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘pdxTrees-vignette.Rmd’
      ...
    +     y = Pollution_Removal_value, color = Mature_Size)) + geom_point(size = 2, 
    +   .... [TRUNCATED] 
    
    > berkeley_graph + transition_states(states = Mature_Size, 
    +     transition_length = 10, state_length = 8) + enter_grow() + 
    +     exit_shrink()
    
      When sourcing ‘pdxTrees-vignette.R’:
    Error: argument "theme" is missing, with no default
    Execution halted
    
      ‘pdxTrees-vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘pdxTrees-vignette.Rmd’ using rmarkdown
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# PeakError

<details>

* Version: 2023.9.4
* GitHub: NA
* Source code: https://github.com/cran/PeakError
* Date/Publication: 2023-09-06 05:20:02 UTC
* Number of recursive dependencies: 45

Run `revdepcheck::cloud_details(, "PeakError")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘PeakError-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: PeakError
    > ### Title: PeakError
    > ### Aliases: PeakError
    > 
    > ### ** Examples
    > 
    > x <- seq(5, 85, by=5)
    ...
    ! The `panel.margin` argument of `theme()` was deprecated in ggplot2
      2.2.0 and is now defunct.
    ℹ Please use the `panel.spacing` argument instead.
    Backtrace:
        ▆
     1. └─ggplot2::theme(panel.margin = grid::unit(0, "cm"))
     2.   └─lifecycle::deprecate_stop("2.2.0", "theme(panel.margin)", "theme(panel.spacing)")
     3.     └─lifecycle:::deprecate_stop0(msg)
     4.       └─rlang::cnd_signal(...)
    Execution halted
    ```

# PeakSegJoint

<details>

* Version: 2024.12.4
* GitHub: https://github.com/tdhock/PeakSegJoint
* Source code: https://github.com/cran/PeakSegJoint
* Date/Publication: 2024-12-04 20:20:02 UTC
* Number of recursive dependencies: 49

Run `revdepcheck::cloud_details(, "PeakSegJoint")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘PeakSegJoint-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: clusterPeaks
    > ### Title: clusterPeaks
    > ### Aliases: clusterPeaks
    > 
    > ### ** Examples
    > 
    > unordered <-
    ...
    ! The `panel.margin` argument of `theme()` was deprecated in ggplot2
      2.2.0 and is now defunct.
    ℹ Please use the `panel.spacing` argument instead.
    Backtrace:
        ▆
     1. └─ggplot2::theme(panel.margin = grid::unit(0, "cm"))
     2.   └─lifecycle::deprecate_stop("2.2.0", "theme(panel.margin)", "theme(panel.spacing)")
     3.     └─lifecycle:::deprecate_stop0(msg)
     4.       └─rlang::cnd_signal(...)
    Execution halted
    ```

# PeakSegOptimal

<details>

* Version: 2024.10.1
* GitHub: https://github.com/tdhock/PeakSegOptimal
* Source code: https://github.com/cran/PeakSegOptimal
* Date/Publication: 2024-10-02 04:40:02 UTC
* Number of recursive dependencies: 48

Run `revdepcheck::cloud_details(, "PeakSegOptimal")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘PeakSegOptimal-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: PeakSegFPOP
    > ### Title: PeakSegFPOP
    > ### Aliases: PeakSegFPOP
    > 
    > ### ** Examples
    > 
    > 
    ...
    ! The `panel.margin` argument of `theme()` was deprecated in ggplot2
      2.2.0 and is now defunct.
    ℹ Please use the `panel.spacing` argument instead.
    Backtrace:
        ▆
     1. └─ggplot2::theme(panel.margin = grid::unit(0, "lines"))
     2.   └─lifecycle::deprecate_stop("2.2.0", "theme(panel.margin)", "theme(panel.spacing)")
     3.     └─lifecycle:::deprecate_stop0(msg)
     4.       └─rlang::cnd_signal(...)
    Execution halted
    ```

# personalized

<details>

* Version: 0.2.7
* GitHub: https://github.com/jaredhuling/personalized
* Source code: https://github.com/cran/personalized
* Date/Publication: 2022-06-27 20:20:03 UTC
* Number of recursive dependencies: 93

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

# Plasmidprofiler

<details>

* Version: 0.1.6
* GitHub: NA
* Source code: https://github.com/cran/Plasmidprofiler
* Date/Publication: 2017-01-06 01:10:47
* Number of recursive dependencies: 90

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
* Number of recursive dependencies: 85

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

# PlotFTIR

<details>

* Version: 1.0.0
* GitHub: https://github.com/NRCan/PlotFTIR
* Source code: https://github.com/cran/PlotFTIR
* Date/Publication: 2024-11-13 14:20:06 UTC
* Number of recursive dependencies: 112

Run `revdepcheck::cloud_details(, "PlotFTIR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘PlotFTIR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: add_wavenumber_marker
    > ### Title: Add a Marker at a Wavenumber
    > ### Aliases: add_wavenumber_marker
    > 
    > ### ** Examples
    > 
    > if (requireNamespace("ggplot2", quietly = TRUE)) {
    ...
      5.       └─ggplot2:::ggplot_add.Layer(object, p, objectname)
      6.         └─ggplot2:::new_layer_names(object, names(plot$layers))
      7.           └─vctrs::vec_as_names(names, repair = "check_unique")
      8.             └─vctrs (local) `<fn>`()
      9.               └─vctrs:::validate_unique(names = names, arg = arg, call = call)
     10.                 └─vctrs:::stop_names_cannot_be_empty(names, call = call)
     11.                   └─vctrs:::stop_names(...)
     12.                     └─vctrs:::stop_vctrs(...)
     13.                       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
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
        8.             └─vctrs (local) `<fn>`()
        9.               └─vctrs:::validate_unique(names = names, arg = arg, call = call)
       10.                 └─vctrs:::stop_names_cannot_be_empty(names, call = call)
       11.                   └─vctrs:::stop_names(...)
       12.                     └─vctrs:::stop_vctrs(...)
       13.                       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 422 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘plotting_ftir_spectra.Rmd’
      ...
    > zoom_in_on_range(plot_ftir(spectra), c(3600, 2600))
    
    > add_wavenumber_marker(plot_ftir(spectra), wavenumber = 1495, 
    +     text = "C-C Aromatic", line_aesthetics = list(linetype = "dashed"), 
    +     label .... [TRUNCATED] 
    
      When sourcing ‘plotting_ftir_spectra.R’:
    Error: Names can't be empty.
    ✖ Empty name found at location 1.
    Execution halted
    
      ‘plotting_ftir_spectra.Rmd’ using ‘UTF-8’... failed
    ```

# plotly

<details>

* Version: 4.10.4
* GitHub: https://github.com/plotly/plotly.R
* Source code: https://github.com/cran/plotly
* Date/Publication: 2024-01-13 22:40:02 UTC
* Number of recursive dependencies: 137

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
      
      Attaching package: 'ggplot2'
      
      The following object is masked from 'package:base':
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

# plotthis

<details>

* Version: 0.5.0
* GitHub: https://github.com/pwwang/plotthis
* Source code: https://github.com/cran/plotthis
* Date/Publication: 2025-01-09 06:10:02 UTC
* Number of recursive dependencies: 181

Run `revdepcheck::cloud_details(, "plotthis")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘plotthis-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: PrepareUpsetData
    > ### Title: Upset Plot
    > ### Aliases: PrepareUpsetData UpsetPlot
    > 
    > ### ** Examples
    > 
    > data <- list(
    +     A = 1:5,
    +     B = 2:6,
    +     C = 3:7,
    +     D = 4:8
    + )
    > UpsetPlot(data)
    Error in as.unit(e2) : object is not coercible to a unit
    Calls: <Anonymous> ... polylineGrob -> is.unit -> unit.c -> Ops.unit -> as.unit
    Execution halted
    ```

# pmartR

<details>

* Version: 2.4.6
* GitHub: https://github.com/pmartR/pmartR
* Source code: https://github.com/cran/pmartR
* Date/Publication: 2024-10-14 21:10:02 UTC
* Number of recursive dependencies: 147

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
      [ FAIL 1 | WARN 1 | SKIP 11 | PASS 2376 ]
      
      ══ Skipped tests (11) ══════════════════════════════════════════════════════════
    ...
      • plots/plot-spansres-color-high-color-low.svg
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
      installed size is 10.8Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        help   1.5Mb
        libs   6.7Mb
    ```

# pmxTools

<details>

* Version: 1.3
* GitHub: https://github.com/kestrel99/pmxTools
* Source code: https://github.com/cran/pmxTools
* Date/Publication: 2023-02-21 16:00:08 UTC
* Number of recursive dependencies: 84

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
       24.     └─handlers[[1L]](cnd)
       25.       └─cli::cli_abort(...)
       26.         └─rlang::abort(...)
      
      [ FAIL 1 | WARN 1 | SKIP 12 | PASS 110 ]
      Deleting unused snapshots:
      • plot/conditioned-distplot.svg
      • plot/perc.svg
      Error: Test failures
      Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘DiagrammeR’
    ```

# pogit

<details>

* Version: 1.3.0
* GitHub: NA
* Source code: https://github.com/cran/pogit
* Date/Publication: 2022-05-25 15:50:02 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "pogit")` for more info

</details>

## Newly broken

*   checking whether package ‘pogit’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(imc, c(1:5, ': unused argument (c(1:5, 10, 20, 50, 100, 200, 500)) 
    See ‘/tmp/workdir/pogit/new/pogit.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    logitBvs: possible error in is.element(imc, c(1:5, 10, 20, 50, 100,
      200, 500)): unused argument (c(1:5, 10, 20, 50, 100, 200, 500))
    negbinBvs: possible error in is.element(imc, c(1:5, 10, 20, 50, 100,
      200, 500)): unused argument (c(1:5, 10, 20, 50, 100, 200, 500))
    pogitBvs: possible error in is.element(imc, c(1:5, 10, 20, 50, 100,
      200, 500)): unused argument (c(1:5, 10, 20, 50, 100, 200, 500))
    poissonBvs: possible error in is.element(imc, c(1:5, 10, 20, 50, 100,
      200, 500)): unused argument (c(1:5, 10, 20, 50, 100, 200, 500))
    ```

# posterior

<details>

* Version: 1.6.0
* GitHub: https://github.com/stan-dev/posterior
* Source code: https://github.com/cran/posterior
* Date/Publication: 2024-07-03 23:00:02 UTC
* Number of recursive dependencies: 118

Run `revdepcheck::cloud_details(, "posterior")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘rvar.Rmd’
      ...
    
    > ggplot() + ggdist::stat_slab(aes(xdist = mixture))
    
      When sourcing ‘rvar.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `use_defaults()`:
    ! unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, 
        NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "#4D4D4DFF", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 
    Execution halted
    
      ‘pareto_diagnostics.Rmd’ using ‘UTF-8’... OK
      ‘posterior.Rmd’ using ‘UTF-8’... OK
      ‘rvar.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘pareto_diagnostics.Rmd’ using rmarkdown
    --- finished re-building ‘pareto_diagnostics.Rmd’
    
    --- re-building ‘posterior.Rmd’ using rmarkdown
    --- finished re-building ‘posterior.Rmd’
    
    --- re-building ‘rvar.Rmd’ using rmarkdown
    
    Quitting from lines 530-533 [mixture] (rvar.Rmd)
    ...
        NULL, NULL, NULL, NULL, NULL, NULL, list(NULL, NA, NULL, NULL, TRUE), NULL, 2, NULL, NULL, NULL, 1.2, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.2, NULL, list(NULL, NULL, NULL, 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0, list(), 2, list("#EBEBEBFF", NA, NULL, NULL, TRUE), list(), NULL, NULL, NULL, list("white", NULL, NULL, 
            NULL, FALSE, "white", TRUE), NULL, list(NULL, 0.5, NULL, NULL, FALSE, NULL, TRUE), NULL, NULL, NULL, NULL, FALSE, list(NULL, "white", NULL, NULL, TRUE), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(5.5, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, TRUE), "topleft", NULL, NULL, list(
            "#D9D9D9FF", NA, NULL, NULL, TRUE), NULL, NULL, "on", "inside", list(NULL, NULL, "#1A1A1AFF", 0.8, NULL, NULL, NULL, NULL, c(4.4, 4.4, 4.4, 4.4), NULL, TRUE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75))
    --- failed re-building ‘rvar.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘rvar.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# PPQplan

<details>

* Version: 1.1.0
* GitHub: https://github.com/allenzhuaz/PPQplan
* Source code: https://github.com/cran/PPQplan
* Date/Publication: 2020-10-08 04:30:06 UTC
* Number of recursive dependencies: 118

Run `revdepcheck::cloud_details(, "PPQplan")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘PPQnote.Rmd’ using rmarkdown
    --- finished re-building ‘PPQnote.Rmd’
    
    --- re-building ‘PPQplan-vignette.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘PPQplan-vignette.Rmd’
      ...
    
    > devtools::load_all()
    
      When sourcing ‘PPQplan-vignette.R’:
    Error: Could not find a root 'DESCRIPTION' file that starts with '^Package' in
    '/tmp/RtmpfcTUMn/file148e33893f24/vignettes'.
    ℹ Are you in your project directory and does your project have a 'DESCRIPTION'
      file?
    Execution halted
    
      ‘PPQnote.Rmd’ using ‘UTF-8’... OK
      ‘PPQplan-vignette.Rmd’ using ‘UTF-8’... failed
    ```

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
* Number of recursive dependencies: 103

Run `revdepcheck::cloud_details(, "ppseq")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘one_sample_expansion.Rmd’
      ...
      
    </table>
    </div>
    
    > ptest <- plot(one_sample_cal_tbl, type1_range = c(0.05, 
    +     0.1), minimum_power = 0.7, plotly = TRUE)
    
    ...
    
    > ptest <- plot(two_sample_cal_tbl, type1_range = c(0.05, 
    +     0.1), minimum_power = 0.7, plotly = TRUE)
    
      When sourcing ‘two_sample_randomized.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘one_sample_expansion.Rmd’ using ‘UTF-8’... failed
      ‘two_sample_randomized.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘one_sample_expansion.Rmd’ using rmarkdown
    
    Quitting from lines 183-188 [unnamed-chunk-13] (one_sample_expansion.Rmd)
    Error: processing vignette 'one_sample_expansion.Rmd' failed with diagnostics:
    subscript out of bounds
    --- failed re-building ‘one_sample_expansion.Rmd’
    
    --- re-building ‘two_sample_randomized.Rmd’ using rmarkdown
    ...
    Quitting from lines 179-184 [unnamed-chunk-13] (two_sample_randomized.Rmd)
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
* Number of recursive dependencies: 28

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

# pRecipe

<details>

* Version: 3.0.2
* GitHub: https://github.com/MiRoVaGo/pRecipe
* Source code: https://github.com/cran/pRecipe
* Date/Publication: 2024-12-07 15:30:02 UTC
* Number of recursive dependencies: 154

Run `revdepcheck::cloud_details(, "pRecipe")` for more info

</details>

## Newly broken

*   checking whether package ‘pRecipe’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(data_name, ': unused argument (c("all", "20cr", "chirps", "cmap", "cmorph", "cpc", "cru-ts", "em-earth", "era20c", "era5", "era5-land", "fldas", "ghcn", "gldas-clsm", "gldas-noah", "gldas-vic", "gpcc", "gpcp", "gpm-imerg", "gsmap", "jra55", "mswep", "merra2", "ncep-doe", "ncep-ncar", "persiann", "precl", "terraclimate")) 
    See ‘/tmp/workdir/pRecipe/new/pRecipe.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    dataset_check: possible error in is.element(data_name, c("all", "20cr",
      "chirps", "cmap", "cmorph", "cpc", "cru-ts", "em-earth", "era20c",
      "era5", "era5-land", "fldas", "ghcn", "gldas-clsm", "gldas-noah",
      "gldas-vic", "gpcc", "gpcp", "gpm-imerg", "gsmap", "jra55", "mswep",
      "merra2", "ncep-doe", "ncep-ncar", "persiann", "precl",
      "terraclimate")): unused argument (c("all", "20cr", "chirps", "cmap",
      "cmorph", "cpc", "cru-ts", "em-earth", "era20c", "era5", "era5-land",
      "fldas", "ghcn", "gldas-clsm", "gldas-noah", "gldas-vic", "gpcc",
      "gpcp", "gpm-imerg", "gsmap", "jra55", "mswep", "merra2", "ncep-doe",
      "ncep-ncar", "persiann", "precl", "terraclimate"))
    ```

# PReMiuM

<details>

* Version: 3.2.13
* GitHub: NA
* Source code: https://github.com/cran/PReMiuM
* Date/Publication: 2024-01-09 13:30:09 UTC
* Number of recursive dependencies: 67

Run `revdepcheck::cloud_details(, "PReMiuM")` for more info

</details>

## Newly broken

*   checking whether package ‘PReMiuM’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(names(table(data[indexOutcomeT])), ': unused argument (c("0", "1")) 
    See ‘/tmp/workdir/PReMiuM/new/PReMiuM.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    profRegr: possible error in
      is.element(names(table(data[indexOutcomeT])), c("0", "1")): unused
      argument (c("0", "1"))
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 23.1Mb
      sub-directories of 1Mb or more:
        libs  22.8Mb
    ```

# prevR

<details>

* Version: 5.0.0
* GitHub: https://github.com/larmarange/prevR
* Source code: https://github.com/cran/prevR
* Date/Publication: 2023-05-15 18:50:03 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "prevR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘prevR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: as.prevR
    > ### Title: Create an object of class prevR.
    > ### Aliases: as.prevR
    > ### Keywords: manip
    > 
    > ### ** Examples
    > 
    ...
    +   n = "n",
    +   pos = "pos",
    +   c.type = "residence",
    +   wn = "weighted.n",
    +   wpos = "weighted.pos"
    + )
    > dhs <- as.prevR(fdhs.clusters, col, fdhs.boundary)
    Error in is.element("id", names(data)) : unused argument (names(data))
    Calls: as.prevR
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘intro_prevR.Rmd’
      ...
     
    
    > col <- c(id = "cluster", x = "x", y = "y", n = "n", 
    +     pos = "pos", c.type = "residence", wn = "weighted.n", wpos = "weighted.pos")
    
    > dhs <- as.prevR(fdhs.clusters, col, fdhs.boundary)
    
      When sourcing ‘intro_prevR.R’:
    Error: unused argument (names(data))
    Execution halted
    
      ‘intro_prevR.Rmd’ using ‘UTF-8’... failed
    ```

*   checking whether package ‘prevR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element("id", names(data))': unused argument (names(data)) 
      Note: possible error in 'is.element("c.type", ': unused argument (names(clust.ind)) 
      Note: possible error in 'is.element("wcase", names(clust.ind))': unused argument (names(clust.ind)) 
    See ‘/tmp/workdir/prevR/new/prevR.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    as.prevR: possible error in is.element("id", names(data)): unused
      argument (names(data))
    import.dhs : make.clust.dbf: possible error in is.element("c.type",
      names(clust.ind)): unused argument (names(clust.ind))
    import.dhs : make.clust.dbf: possible error in is.element("wcase",
      names(clust.ind)): unused argument (names(clust.ind))
    krige,ANY-prevR: possible error in is.element(fit, "auto"): unused
      argument ("auto")
    plot,prevR-missing: possible error in is.element(type, c("position",
      "c.type", "count", "flower")): unused argument (c("position",
      "c.type", "count", "flower"))
    plot,prevR-missing: possible error in is.element("c.type",
      names(clusters)): unused argument (names(clusters))
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘intro_prevR.Rmd’ using rmarkdown
    
    Quitting from lines 40-54 [unnamed-chunk-2] (intro_prevR.Rmd)
    Error: processing vignette 'intro_prevR.Rmd' failed with diagnostics:
    unused argument (names(data))
    --- failed re-building ‘intro_prevR.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘intro_prevR.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.4Mb
      sub-directories of 1Mb or more:
        data   7.5Mb
    ```

# priorsense

<details>

* Version: 1.0.4
* GitHub: NA
* Source code: https://github.com/cran/priorsense
* Date/Publication: 2024-11-01 12:30:02 UTC
* Number of recursive dependencies: 112

Run `revdepcheck::cloud_details(, "priorsense")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘priorsense-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: powerscale_plots
    > ### Title: Diagnostic plots for power-scaling sensitivity
    > ### Aliases: powerscale_plots powerscale_plot_dens powerscale_plot_ecdf
    > ###   powerscale_plot_ecdf.powerscaled_sequence powerscale_plot_quantities
    > ###   powerscale_plot_quantities.powerscaled_sequence
    > 
    > ### ** Examples
    > 
    > ex <- example_powerscale_model()
    > 
    > powerscale_plot_dens(ex$draws)
    Error in use_defaults(..., self = self) : 
      unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, 
        NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "#4D4D4DFF", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(), NU
    Calls: <Anonymous> ... <Anonymous> -> <Anonymous> -> compute_geom_2 -> <Anonymous>
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘powerscaling.Rmd’
      ...
      <chr>    <dbl>      <dbl> <chr>              
    1 mu       0.433      0.641 prior-data conflict
    2 sigma    0.360      0.674 prior-data conflict
    
    > powerscale_plot_dens(fit, variable = "mu", facet_rows = "variable")
    
      When sourcing ‘powerscaling.R’:
    Error: unused argument (theme = list(list("black", 0.545454545454545, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.545454545454545, 1, TRUE), list("sans", "plain", "black", 12, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.545454545454545, 0.545454545454545, 1, 1, "sans", 4.21751764217518, 1.63636363636364, 19, TRUE), 6, c(6, 6, 6, 6), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, 
        NULL, c(3, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 3, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 3, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 3), NULL, TRUE), list(NULL, NULL, "#4D4D4DFF", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.4, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL,
    Execution halted
    
      ‘powerscaling.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘powerscaling.Rmd’ using rmarkdown
    
    Quitting from lines 124-125 [unnamed-chunk-7] (powerscaling.Rmd)
    Error: processing vignette 'powerscaling.Rmd' failed with diagnostics:
    unused argument (theme = list(list("black", 0.545454545454545, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.545454545454545, 1, TRUE), list("sans", "plain", "black", 12, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.545454545454545, 0.545454545454545, 1, 1, "sans", 4.21751764217518, 1.63636363636364, 19, TRUE), 6, c(6, 6, 6, 6), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, 
        NULL, c(3, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 3, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 3, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 3), NULL, TRUE), list(NULL, NULL, "#4D4D4DFF", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.4, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.4, 
    0), NULL, TRUE), NULL, list(), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 2.4), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0.5, NULL, NULL, NULL, c(0, 2.4, 0, 2.4), NULL, TRUE), list("#333333FF", 0.3, NULL, NULL, FALSE, "#333333FF", FALSE), NULL, NULL, NULL, list(), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.5, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(NULL, 0.4, NULL, NULL, FALSE, NULL, 
        FALSE), NULL, NULL, NULL, list(), NULL, NULL, NULL, NULL, list(), NULL, 2, NULL, NULL, list(), 1.2, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.2, NULL, list(NULL, NULL, NULL, 13, 0, NULL, NULL, NULL, NULL, NULL, FALSE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0, list(), 2, list(), list(), 1.5, NULL, NULL, list(), NULL, list(NULL, 0.5, NULL, NULL, FALSE, NULL, TRUE), NULL, 
        NULL, NULL, NULL, FALSE, list(), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, c(0, 0, 6, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 6, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(6, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, TRUE), "topleft", NULL, NULL, list(), NULL, NULL, "on", "outside", list(NULL, NULL, "#1A1A1AFF", 0.9, NULL, NULL, NULL, NULL, c(4.8, 4.8, 4.8, 4.8), NULL, FALSE), 
        NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 3, 3, list(), list(NULL, NULL, NULL, NULL, FALSE, NULL, TRUE), list(NULL, NULL, NULL, NULL, FALSE, NULL, TRUE), list(NULL, NULL, NULL, NULL, FALSE, NULL, TRUE), list(NULL, NULL, NULL, NULL, 0.5, 0.5, 0, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), 0.666666666666667, 0.333333333333333))
    --- failed re-building ‘powerscaling.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘powerscaling.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# probably

<details>

* Version: 1.0.3
* GitHub: https://github.com/tidymodels/probably
* Source code: https://github.com/cran/probably
* Date/Publication: 2024-02-23 03:20:02 UTC
* Number of recursive dependencies: 131

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
* Number of recursive dependencies: 117

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

# psborrow

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/psborrow
* Date/Publication: 2023-03-03 10:30:07 UTC
* Number of recursive dependencies: 107

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
      [ FAIL 10 | WARN 0 | SKIP 1 | PASS 142 ]
      
      ══ Skipped tests (1) ═══════════════════════════════════════════════════════════
    ...
      `expected` is a character vector ('ref')
      ── Failure ('test-plots.R:126:5'): Ensure output is producing a ggplot2 object with appropriate parameters ──
      p1$labels$yintercept (`actual`) not equal to "ref" (`expected`).
      
      `actual` is NULL
      `expected` is a character vector ('ref')
      
      [ FAIL 10 | WARN 0 | SKIP 1 | PASS 142 ]
      Error: Test failures
      Execution halted
    ```

# PupilPre

<details>

* Version: 0.6.2
* GitHub: NA
* Source code: https://github.com/cran/PupilPre
* Date/Publication: 2020-03-10 05:20:02 UTC
* Number of recursive dependencies: 72

Run `revdepcheck::cloud_details(, "PupilPre")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘PupilPre_Plotting.Rmd’
      ...
    
    > gridExtra::grid.arrange(ppl_plot_avg(data = dat, xlim = c(0, 
    +     2000), Column = "Pupil", Condition1 = NULL, Condition2 = NULL, 
    +     Cond1Label .... [TRUNCATED] 
    Grand average calculated using Event means.
    Grand average calculated using Event means.
    
      When sourcing ‘PupilPre_Plotting.R’:
    Error: invalid line type
    Execution halted
    
      ‘PupilPre_Basic_Preprocessing.Rmd’ using ‘UTF-8’... OK
      ‘PupilPre_Cleanup.Rmd’ using ‘UTF-8’... OK
      ‘PupilPre_Interpolation_and_Filtering.Rmd’ using ‘UTF-8’... OK
      ‘PupilPre_Message_Alignment.Rmd’ using ‘UTF-8’... OK
      ‘PupilPre_Plotting.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘PupilPre_Basic_Preprocessing.Rmd’ using rmarkdown
    ```

# qgcomp

<details>

* Version: 2.16.4
* GitHub: https://github.com/alexpkeil1/qgcomp
* Source code: https://github.com/cran/qgcomp
* Date/Publication: 2025-01-22 06:40:02 UTC
* Number of recursive dependencies: 159

Run `revdepcheck::cloud_details(, "qgcomp")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘qgcomp-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: qgcomp.partials
    > ### Title: Partial effect sizes, confidence intervals, hypothesis tests
    > ### Aliases: qgcomp.partials
    > 
    > ### ** Examples
    > 
    > set.seed(123223)
    ...
    > trainidx <- sample(1:nrow(dat), round(nrow(dat)*0.4))
    > valididx <- setdiff(1:nrow(dat),trainidx)
    > traindata = dat[trainidx,]
    > validdata = dat[valididx,]
    > splitres <- qgcomp.partials(fun="qgcomp.glm.noboot", f=y~., q=NULL, 
    +     traindata=traindata,validdata=validdata, expnms=c("x1", "x2", "x3", "x4"))
    Error in is.element(expnms, names(train.fit$pos.weights)) : 
      unused argument (names(train.fit$pos.weights))
    Calls: qgcomp.partials
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘test_asis.R’
      Running ‘test_basics.R’
      Running ‘test_bayesqgcomp.R’
      Running ‘test_boot_ints.R’
      Running ‘test_bootchooser.R’
      Running ‘test_ee.R’
      Running ‘test_factor.R’
      Running ‘test_id.R’
      Running ‘test_mice.R’
      Running ‘test_multinomial.R’
    ...
      > dim(spl$traindata) # 181 observations = 40% of total
      [1] 181  27
      > dim(spl$validdata) # 271 observations = 60% of total
      [1] 271  27
      > splitres <- qgcomp.partials(fun="qgcomp.noboot", f=y~., q=4, 
      +   traindata=spl$traindata,validdata=spl$validdata, expnms=Xnm, .fixbreaks = FALSE, .globalbreaks = TRUE)
      Error in is.element(expnms, names(train.fit$pos.weights)) : 
        unused argument (names(train.fit$pos.weights))
      Calls: qgcomp.partials
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘qgcomp-advanced-vignette.Rmd’
      ...
    > dim(validdata)
    [1] 271  26
    
    > splitres <- qgcomp.partials(fun = "qgcomp.glm.noboot", 
    +     f = y ~ ., q = 4, traindata = traindata[, c(Xnm, covars, 
    +         "y")], validdata = .... [TRUNCATED] 
    
      When sourcing ‘qgcomp-advanced-vignette.R’:
    Error: unused argument (names(train.fit$pos.weights))
    Execution halted
    
      ‘qgcomp-advanced-vignette.Rmd’ using ‘UTF-8’... failed
      ‘qgcomp-basic-vignette.Rmd’ using ‘UTF-8’... OK
    ```

*   checking whether package ‘qgcomp’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(expnms, names(train.fit$pos.weights))': unused argument (names(train.fit$pos.weights)) 
      Note: possible error in 'is.element(expnms, names(train.fit$neg.weights))': unused argument (names(train.fit$neg.weights)) 
    See ‘/tmp/workdir/qgcomp/new/qgcomp.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    qgcomp.partials: possible error in is.element(expnms,
      names(train.fit$pos.weights)): unused argument
      (names(train.fit$pos.weights))
    qgcomp.partials: possible error in is.element(expnms,
      names(train.fit$neg.weights)): unused argument
      (names(train.fit$neg.weights))
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘qgcomp-advanced-vignette.Rmd’ using knitr
    ```

# qpNCA

<details>

* Version: 1.1.6
* GitHub: NA
* Source code: https://github.com/cran/qpNCA
* Date/Publication: 2021-08-16 12:50:02 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "qpNCA")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(qpNCA)
      > 
      > test_check("qpNCA")
      [ FAIL 18 | WARN 4 | SKIP 0 | PASS 0 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
       19. │     └─mask$eval_all_mutate(quo)
       20. │       └─dplyr (local) eval()
       21. ├─base::ifelse(...)
       22. └─base::.handleSimpleError(...)
       23.   └─dplyr (local) h(simpleError(msg, call))
       24.     └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
      
      [ FAIL 18 | WARN 4 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Example-full-nca-analysis.rmd’
      ...
    Calculating Cmax/Tmax...
    
    Applying time deviation corrections and missing concentration imputations...
    
    
      When sourcing ‘Example-full-nca-analysis.R’:
    Error: ℹ In argument: `misstime = ifelse(...)`.
    ...
      When sourcing ‘Example-stepwise-nca-analysis.R’:
    Error: ℹ In argument: `misstime = ifelse(...)`.
    Caused by error in `is.element()`:
    ! unused argument (ptime)
    Execution halted
    
      ‘Parameter_Guidelines.rmd’ using ‘UTF-8’... OK
      ‘User_Guide.rmd’ using ‘UTF-8’... OK
      ‘Example-full-nca-analysis.rmd’ using ‘UTF-8’... failed
      ‘Example-stepwise-nca-analysis.rmd’ using ‘UTF-8’... failed
    ```

*   checking whether package ‘qpNCA’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(0, par$ptime)': unused argument (par$ptime) 
      Note: possible error in 'is.element(tau, par$time.tau)': unused argument (par$time.tau) 
      Note: possible error in 'is.element(teval, par$time.teval)': unused argument (par$time.teval) 
      Note: possible error in 'is.element(tstart, par$time.part)': unused argument (par$time.part) 
      Note: possible error in 'is.element(tend, par$time.part)': unused argument (par$time.part) 
      Note: possible error in 'is.element(0, ptime)': unused argument (ptime) 
      Note: possible error in 'is.element(tau, ptime)': unused argument (ptime) 
      Note: possible error in 'is.element(i, ptime)': unused argument (ptime) 
    See ‘/tmp/workdir/qpNCA/new/qpNCA.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    .calc.par: possible error in is.element(0, par$ptime): unused argument
      (par$ptime)
    .calc.par: possible error in is.element(tau, par$time.tau): unused
      argument (par$time.tau)
    .calc.par: possible error in is.element(teval, par$time.teval): unused
      argument (par$time.teval)
    .calc.par: possible error in is.element(tstart, par$time.part): unused
      argument (par$time.part)
    .calc.par: possible error in is.element(tend, par$time.part): unused
      argument (par$time.part)
    .correct.conc: possible error in is.element(0, ptime): unused argument
      (ptime)
    .correct.conc: possible error in is.element(tau, ptime): unused
      argument (ptime)
    .correct.time: possible error in is.element(i, ptime): unused argument
      (ptime)
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Parameter_Guidelines.rmd’ using rmarkdown
    --- finished re-building ‘Parameter_Guidelines.rmd’
    
    --- re-building ‘User_Guide.rmd’ using rmarkdown
    --- finished re-building ‘User_Guide.rmd’
    
    --- re-building ‘Example-full-nca-analysis.rmd’ using knitr
    
    Quitting from lines 81-114 [unnamed-chunk-4] (Example-full-nca-analysis.rmd)
    Error: processing vignette 'Example-full-nca-analysis.rmd' failed with diagnostics:
    ℹ In argument: `misstime = ifelse(...)`.
    Caused by error in `is.element()`:
    ! unused argument (ptime)
    --- failed re-building ‘Example-full-nca-analysis.rmd’
    
    --- re-building ‘Example-stepwise-nca-analysis.rmd’ using knitr
    ```

# r2dii.plot

<details>

* Version: 0.4.0
* GitHub: https://github.com/RMI-PACTA/r2dii.plot
* Source code: https://github.com/cran/r2dii.plot
* Date/Publication: 2024-02-29 16:40:02 UTC
* Number of recursive dependencies: 90

Run `revdepcheck::cloud_details(, "r2dii.plot")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(r2dii.plot)
      > 
      > test_check("r2dii.plot")
      Scale for colour is already present.
      Adding another scale for colour, which will replace the existing scale.
    ...
      `expected` is a character vector ('year')
      ── Failure ('test-plot_trajectory.R:41:3'): outputs default axis labels ────────
      p$labels$y (`actual`) not equal to "value" (`expected`).
      
      `actual` is NULL
      `expected` is a character vector ('value')
      
      [ FAIL 2 | WARN 2 | SKIP 40 | PASS 122 ]
      Error: Test failures
      Execution halted
    ```

# r4ss

<details>

* Version: 1.44.0
* GitHub: https://github.com/r4ss/r4ss
* Source code: https://github.com/cran/r4ss
* Date/Publication: 2022-05-26 18:00:02 UTC
* Number of recursive dependencies: 144

Run `revdepcheck::cloud_details(, "r4ss")` for more info

</details>

## Newly broken

*   checking whether package ‘r4ss’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(grep("MGparm", ': unused argument (grep("MGparm_dev", parvals)) 
      Note: possible error in 'is.element(grep("selparm", ': unused argument (grep("selparm_dev", parvals)) 
    See ‘/tmp/workdir/r4ss/new/r4ss.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    SS_readpar_3.24: possible error in is.element(grep("MGparm", parvals),
      grep("MGparm_dev", parvals)): unused argument (grep("MGparm_dev",
      parvals))
    SS_readpar_3.24: possible error in is.element(grep("selparm", parvals),
      grep("selparm_dev", parvals)): unused argument (grep("selparm_dev",
      parvals))
    ```

# r6qualitytools

<details>

* Version: 1.0.1
* GitHub: https://github.com/Fabianenc/r6qualitytools
* Source code: https://github.com/cran/r6qualitytools
* Date/Publication: 2024-10-03 19:30:02 UTC
* Number of recursive dependencies: 76

Run `revdepcheck::cloud_details(, "r6qualitytools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘r6qualitytools-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: FitDistr
    > ### Title: FitDistr: Maximum-likelihood Fitting of Univariate Distributions
    > ### Aliases: FitDistr
    > 
    > ### ** Examples
    > 
    > set.seed(123)
    > x = rgamma(100, shape = 5, rate = 0.1)
    > FitDistr(x, "gamma")
    Error in is.element(dots, c("upper", "lower")) : 
      unused argument (c("upper", "lower"))
    Calls: FitDistr
    Execution halted
    ```

*   checking whether package ‘r6qualitytools’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(dots, c("upper", ': unused argument (c("upper", "lower")) 
      Note: possible error in 'is.element(names(start), ': unused argument (dots) 
      Note: possible error in 'is.element(distname, ': unused argument (c("cauchy", "logistic")) 
    See ‘/tmp/workdir/r6qualitytools/new/r6qualitytools.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    FitDistr: possible error in is.element(dots, c("upper", "lower")):
      unused argument (c("upper", "lower"))
    FitDistr: possible error in is.element(names(start), dots): unused
      argument (dots)
    FitDistr: possible error in is.element(distname, c("cauchy",
      "logistic")): unused argument (c("cauchy", "logistic"))
    ```

# Radviz

<details>

* Version: 0.9.3
* GitHub: https://github.com/yannabraham/Radviz
* Source code: https://github.com/cran/Radviz
* Date/Publication: 2022-03-25 18:10:02 UTC
* Number of recursive dependencies: 64

Run `revdepcheck::cloud_details(, "Radviz")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Radviz-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Radviz
    > ### Title: Radviz Projection of Multidimensional Data
    > ### Aliases: Radviz
    > 
    > ### ** Examples
    > 
    > data(iris)
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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘multivariate_analysis.Rmd’
      ...
    ℹ Please use tidy evaluation idioms with `aes()`.
    ℹ See also `vignette("ggplot2-in-packages")` for more information.
    ℹ The deprecated feature was likely used in the Radviz package.
      Please report the issue at <https://github.com/yannabraham/Radviz/issues>.
    
    > plot(btcells.rv) + geom_point(aes(color = Treatment))
    
    ...
    [1] 15792    18
    
    > ct.rv
    
      When sourcing ‘single_cell_projections.R’:
    Error: 'language' object cannot be coerced to type 'double'
    Execution halted
    
      ‘multivariate_analysis.Rmd’ using ‘UTF-8’... failed
      ‘single_cell_projections.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘multivariate_analysis.Rmd’ using rmarkdown
    ```

# rassta

<details>

* Version: 1.0.6
* GitHub: https://github.com/bafuentes/rassta
* Source code: https://github.com/cran/rassta
* Date/Publication: 2024-08-19 06:20:13 UTC
* Number of recursive dependencies: 108

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

# rater

<details>

* Version: 1.3.1
* GitHub: https://github.com/jeffreypullin/rater
* Source code: https://github.com/cran/rater
* Date/Publication: 2023-09-11 17:40:02 UTC
* Number of recursive dependencies: 91

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
* Number of recursive dependencies: 138

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

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘plotting.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘plotting.Rmd’
      ...
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family 'Arial Narrow' not found in PostScript font database
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family 'Arial Narrow' not found in PostScript font database
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family 'Arial Narrow' not found in PostScript font database
    
    ...
    
    > include_graphics(c(here("vignettes", "images", "01-new-project.png"), 
    +     here("vignettes", "images", "02-new-directory.png"), here("vignettes",  .... [TRUNCATED] 
    
      When sourcing ‘report-writing.R’:
    Error: Cannot find the file(s): "/tmp/Rtmp4QZ5p8/file183b76f7563a/vignettes/vignettes/images/01-new-project.png"; "/tmp/Rtmp4QZ5p8/file183b76f7563a/vignettes/vignettes/images/02-new-directory.png"; "/tmp/Rtmp4QZ5p8/file183b76f7563a/vignettes/vignettes/images/03-techreport.png"; "/tmp/Rtmp4QZ5p8/file183b76f7563a/vignettes/vignettes/images/04-name-report.png"
    Execution halted
    
      ‘plotting.Rmd’ using ‘UTF-8’... failed
      ‘report-writing.Rmd’ using ‘UTF-8’... failed
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        doc       2.6Mb
        rstudio   1.6Mb
    ```

# RclusTool

<details>

* Version: 0.91.6
* GitHub: NA
* Source code: https://github.com/cran/RclusTool
* Date/Publication: 2024-02-27 19:10:02 UTC
* Number of recursive dependencies: 129

Run `revdepcheck::cloud_details(, "RclusTool")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘RclusTool-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: abdPlot
    > ### Title: Abundances barplot
    > ### Aliases: abdPlot
    > ### Keywords: internal
    > 
    > ### ** Examples
    > 
    ...
    > tf <- tempfile()
    > write.table(dat, tf, sep=",", dec=".")
    > 
    > x <- importSample(file.features=tf)
    Save directory does not exist: no RDS file saved.
    Import Features...
    Error in is.element("row.names", names(match.call())) : 
      unused argument (names(match.call()))
    Calls: importSample
    Execution halted
    ```

*   checking whether package ‘RclusTool’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(colnames(data.sample$features[["preprocessed"]]$x), ': unused argument (data.sample$config$logFeat) 
      Note: possible error in 'is.element("row.names", ': unused argument (names(match.call())) 
      Note: possible error in 'is.element(value, visu.env$slider.values)': unused argument (visu.env$slider.values) 
      Note: possible error in 'is.element("sel", names)': unused argument (names) 
      Note: possible error in 'is.element("mem", names)': unused argument (names) 
      Note: possible error in 'is.element(visu.env$profile.mode, ': unused argument (c("none", "constrained pairs")) 
      Note: possible error in 'is.element("fig", names)': unused argument (names) 
    See ‘/tmp/workdir/RclusTool/new/RclusTool.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    applyPreprocessing: possible error in
      is.element(colnames(data.sample$features[["preprocessed"]]$x),
      data.sample$config$logFeat): unused argument
      (data.sample$config$logFeat)
    importSample: possible error in is.element("row.names",
      names(match.call())): unused argument (names(match.call()))
    visualizeSampleClustering : setProfileId: possible error in
      is.element(value, visu.env$slider.values): unused argument
      (visu.env$slider.values)
    visualizeSampleClustering : setProfileId: possible error in
    ...
    visualizeSampleClustering : OnMouseClick: possible error in
      is.element(visu.env$profile.mode, c("none", "constrained pairs")):
      unused argument (c("none", "constrained pairs"))
    visualizeSampleClustering : buildPtsSearchTree: possible error in
      is.element(visu.env$profile.mode, c("none", "constrained pairs")):
      unused argument (c("none", "constrained pairs"))
    visualizeSampleClustering : updateProfilesPlot: possible error in
      is.element("fig", names): unused argument (names)
    visualizeSampleClustering : updateProfilesPlot: possible error in
      is.element("mem", names): unused argument (names)
    ```

# RDS

<details>

* Version: 0.9-10
* GitHub: NA
* Source code: https://github.com/cran/RDS
* Date/Publication: 2024-09-06 08:00:06 UTC
* Number of recursive dependencies: 109

Run `revdepcheck::cloud_details(, "RDS")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘RDS-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: RDS.HCG.estimates
    > ### Title: Homophily Configuration Graph Estimates
    > ### Aliases: RDS.HCG.estimates
    > 
    > ### ** Examples
    > 
    > 
    > data(fauxtime)
    > RDS.HCG.estimates(rds.data=fauxtime,outcome.variable='var1')
    Error in is.element("DeducerRichOutput", .packages()) : 
      unused argument (.packages())
    Calls: <Anonymous> -> print.rds.interval.estimate
    Execution halted
    ```

*   checking whether package ‘RDS’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element("DeducerRichOutput", ': unused argument (.packages()) 
    See ‘/tmp/workdir/RDS/new/RDS.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    print.rds.interval.estimate: possible error in
      is.element("DeducerRichOutput", .packages()): unused argument
      (.packages())
    ```

# regtomean

<details>

* Version: 1.2
* GitHub: NA
* Source code: https://github.com/cran/regtomean
* Date/Publication: 2024-12-17 15:00:02 UTC
* Number of recursive dependencies: 86

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

# RKorAPClient

<details>

* Version: 0.9.0
* GitHub: https://github.com/KorAP/RKorAPClient
* Source code: https://github.com/cran/RKorAPClient
* Date/Publication: 2025-01-10 18:20:08 UTC
* Number of recursive dependencies: 123

Run `revdepcheck::cloud_details(, "RKorAPClient")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library("testthat")
      > library("RKorAPClient")
      > 
      > test_check("RKorAPClient")
      <KorAPConnection> 
      apiUrl:  https://korap.ids-mannheim.de/api/v1.0/ 
      Searching "einfache" in "": 129 hits, took 
    ...
        'test-textMetadata.R:9:3', 'test-textMetadata.R:15:3'
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-misc.R:224:5'): geom_freq_by_year_ci works correctly ─────────
      gpt[["labels"]][["url"]] not equal to "webUIRequestUrl".
      target is NULL, current is character
      
      [ FAIL 1 | WARN 0 | SKIP 34 | PASS 29 ]
      Error: Test failures
      Execution halted
    ```

# rmcfs

<details>

* Version: 1.3.6
* GitHub: NA
* Source code: https://github.com/cran/rmcfs
* Date/Publication: 2024-08-19 14:40:02 UTC
* Number of recursive dependencies: 64

Run `revdepcheck::cloud_details(, "rmcfs")` for more info

</details>

## Newly broken

*   checking whether package ‘rmcfs’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(interdeps$edge_a, ': unused argument (top_nodes$attribute) 
      Note: possible error in 'is.element(interdeps$edge_b, ': unused argument (top_nodes$attribute) 
    See ‘/tmp/workdir/rmcfs/new/rmcfs.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    build.idgraph: possible error in is.element(interdeps$edge_a,
      top_nodes$attribute): unused argument (top_nodes$attribute)
    build.idgraph: possible error in is.element(interdeps$edge_b,
      top_nodes$attribute): unused argument (top_nodes$attribute)
    ```

# rnmamod

<details>

* Version: 0.4.0
* GitHub: https://github.com/LoukiaSpin/rnmamod
* Source code: https://github.com/cran/rnmamod
* Date/Publication: 2024-03-24 21:40:02 UTC
* Number of recursive dependencies: 175

Run `revdepcheck::cloud_details(, "rnmamod")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rnmamod-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: balloon_plot
    > ### Title: Enhanced balloon plot
    > ### Aliases: balloon_plot
    > 
    > ### ** Examples
    > 
    > data("pma.taylor2004")
    ...
    > interv_names <- c("placebo", "inositol")
    > 
    > # Create the enhanced balloon plot for 'inositol versus placebo'
    > balloon_plot(sens = res_sens,
    +              compar = c("inositol", "placebo"),
    +              drug_names = interv_names)
    Error in is.element(sens$measure, c("RR", "RD")) : 
      unused argument (c("RR", "RD"))
    Calls: balloon_plot
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(rnmamod)
      Loading 'rnmamod', version 0.4.0. Welcome to Network Meta-analysis with Missing Participants! ^_^
      > 
      > test_check("rnmamod")
      Compiling model graph
         Resolving undeclared variables
    ...
      ── Error ('test-run.nodesplit.no.MOD.R:43:3'): The summary log ORs and between-trial sd agree (no missing outcome data) ──
      Error in `is.element(measure, c("OR", "RR", "RD"))`: unused argument (c("OR", "RR", "RD"))
      Backtrace:
          ▆
       1. └─rnmamod::run_model(...) at test-run.nodesplit.no.MOD.R:43:3
       2.   └─rnmamod::data_preparation(data, measure)
      
      [ FAIL 4 | WARN 1 | SKIP 0 | PASS 10 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘network_description.Rmd’
      ...
    +     "BMV", "CPL", "CPL+polytar", "capasal", "TCF gel", "placebo"), 
    +     show_multi = TR .... [TRUNCATED] 
    
    > describe_network(data = nma.bottomley2011, drug_names = c("BDP", 
    +     "BMV", "CPL", "CPL+polytar", "capasal", "TCF gel", "placebo"), 
    +     measur .... [TRUNCATED] 
    
      When sourcing ‘network_description.R’:
    Error: unused argument (c("OR", "RR", "RD"))
    Execution halted
    
      ‘network_description.Rmd’ using ‘UTF-8’... failed
      ‘perform_network_metaanalysis.Rmd’ using ‘UTF-8’... OK
    ```

*   checking whether package ‘rnmamod’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(sens$measure, ': unused argument (c("RR", "RD")) 
      Note: possible error in 'is.element(compar[1], ': unused argument (drug_names) 
      Note: possible error in 'is.element(compar[2], ': unused argument (drug_names) 
      Note: possible error in 'is.element(measure, c("OR", ': unused argument (c("OR", "RR", "ROM")) 
      Note: possible error in 'is.element(measure, c("OR", ': unused argument (c("OR", "RR", "RD")) 
      Note: possible error in 'is.element(measure, c("MD", ': unused argument (c("MD", "SMD")) 
      Note: possible error in 'is.element(measure, "ROM")': unused argument ("ROM") 
      Note: possible error in 'is.element(measure, c("MD", ': unused argument (c("MD", "RD", "SMD")) 
      Note: possible error in 'is.element(length(base_risk), ': unused argument (c(1, 3)) 
    ...
      Note: possible error in 'is.element(report_results, ': unused argument (res_val) 
      Note: possible error in 'is.element(discuss_trans, ': unused argument (disc_val) 
      Note: possible error in 'is.element(proper_table, ': unused argument (c("No", "No table", "Yes")) 
      Note: possible error in 'is.element(plan_protocol, ': unused argument (c("Both", "Only direct methods")) 
      Note: possible error in 'is.element(report_results, ': unused argument (c("Both", "Only direct methods")) 
      Note: possible error in 'is.element(measure, c("OR", ': unused argument (c("OR", "MD", "SMD")) 
    See ‘/tmp/workdir/rnmamod/new/rnmamod.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    balloon_plot: possible error in is.element(sens$measure, c("RR",
      "RD")): unused argument (c("RR", "RD"))
    balloon_plot: possible error in is.element(compar[1], drug_names):
      unused argument (drug_names)
    balloon_plot: possible error in is.element(compar[2], drug_names):
      unused argument (drug_names)
    balloon_plot: possible error in is.element(measure, c("OR", "RR",
      "ROM")): unused argument (c("OR", "RR", "ROM"))
    balloon_plot: possible error in is.element(measure, c("OR", "RR",
      "RD")): unused argument (c("OR", "RR", "RD"))
    ...
    ume_plot: possible error in is.element(measure, c("OR", "ROM")): unused
      argument (c("OR", "ROM"))
    ume_plot: possible error in is.element(possible_comp$poss_comp[, 4],
      obs_comp): unused argument (obs_comp)
    unrelated_effects_plot: possible error in is.element(measure, c("OR",
      "MD", "SMD")): unused argument (c("OR", "MD", "SMD"))
    unrelated_effects_plot: possible error in is.element(measure, c("MD",
      "SMD", "ROM")): unused argument (c("MD", "SMD", "ROM"))
    unrelated_effects_plot: possible error in is.element(measure, c("OR",
      "ROM")): unused argument (c("OR", "ROM"))
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘network_description.Rmd’ using rmarkdown
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘netmeta’
    ```

# roahd

<details>

* Version: 1.4.3
* GitHub: https://github.com/astamm/roahd
* Source code: https://github.com/cran/roahd
* Date/Publication: 2021-11-04 00:10:02 UTC
* Number of recursive dependencies: 87

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

# robomit

<details>

* Version: 1.0.6
* GitHub: NA
* Source code: https://github.com/cran/robomit
* Date/Publication: 2021-06-22 06:50:02 UTC
* Number of recursive dependencies: 67

Run `revdepcheck::cloud_details(, "robomit")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘robomit-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: o_beta_boot_inf
    > ### Title: Bootstrapped mean beta* and confidence intervals
    > ### Aliases: o_beta_boot_inf
    > 
    > ### ** Examples
    > 
    > # load data, e.g. the in-build mtcars dataset
    ...
    +                 sim = 100,            # number of simulations
    +                 obs = 30,             # draws per simulation
    +                 rep = FALSE,          # bootstrapping with or without replacement
    +                 CI = c(90,95,99),     # confidence intervals
    +                 type = "lm",          # model type
    +                 useed = 123,          # seed
    +                 data = data_oster)    # dataset
    Error in is.element(90, CI) : unused argument (CI)
    Calls: o_beta_boot_inf -> tribble -> extract_frame_data_from_dots -> list2
    Execution halted
    ```

*   checking whether package ‘robomit’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(90, CI)': unused argument (CI) 
      Note: possible error in 'is.element(95, CI)': unused argument (CI) 
      Note: possible error in 'is.element(99, CI)': unused argument (CI) 
    See ‘/tmp/workdir/robomit/new/robomit.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    o_beta_boot_inf: possible error in is.element(90, CI): unused argument
      (CI)
    o_beta_boot_inf: possible error in is.element(95, CI): unused argument
      (CI)
    o_beta_boot_inf: possible error in is.element(99, CI): unused argument
      (CI)
    o_beta_boot_viz: possible error in is.element(90, CI): unused argument
      (CI)
    o_beta_boot_viz: possible error in is.element(95, CI): unused argument
      (CI)
    ...
    o_delta_boot_inf: possible error in is.element(95, CI): unused argument
      (CI)
    o_delta_boot_inf: possible error in is.element(99, CI): unused argument
      (CI)
    o_delta_boot_viz: possible error in is.element(90, CI): unused argument
      (CI)
    o_delta_boot_viz: possible error in is.element(95, CI): unused argument
      (CI)
    o_delta_boot_viz: possible error in is.element(99, CI): unused argument
      (CI)
    ```

# romic

<details>

* Version: 1.1.3
* GitHub: NA
* Source code: https://github.com/cran/romic
* Date/Publication: 2023-09-21 05:40:02 UTC
* Number of recursive dependencies: 112

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

# roptions

<details>

* Version: 1.0.3
* GitHub: NA
* Source code: https://github.com/cran/roptions
* Date/Publication: 2020-05-11 11:10:06 UTC
* Number of recursive dependencies: 70

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

# rsimsum

<details>

* Version: 0.13.0
* GitHub: https://github.com/ellessenne/rsimsum
* Source code: https://github.com/cran/rsimsum
* Date/Publication: 2024-03-03 09:40:02 UTC
* Number of recursive dependencies: 129

Run `revdepcheck::cloud_details(, "rsimsum")` for more info

</details>

## Newly broken

*   checking whether package ‘rsimsum’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘generics::is.element’ by ‘ggplot2::is.element’ when loading ‘rsimsum’
    See ‘/tmp/workdir/rsimsum/new/rsimsum.Rcheck/00install.out’ for details.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        doc    1.9Mb
        help   1.3Mb
    ```

# rSRD

<details>

* Version: 0.1.8
* GitHub: NA
* Source code: https://github.com/cran/rSRD
* Date/Publication: 2024-11-01 19:20:02 UTC
* Number of recursive dependencies: 48

Run `revdepcheck::cloud_details(, "rSRD")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rSRD-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: utilsCreateReference
    > ### Title: utilsCreateReference
    > ### Aliases: utilsCreateReference
    > 
    > ### ** Examples
    > 
    > SRDInput <- data.frame(
    ...
    + C=c(60, 59, 57, 55, 60),
    + D=c(35, 24, 44, 83, 47),
    + E=c(41, 52, 46, 50, 65))
    > proc_data <- rSRD::utilsPreprocessDF(SRDInput)
    > ref <- c("min","max","min","max","mean")
    > rSRD::utilsCreateReference(proc_data, method = "mixed", ref)
    Error in is.element(refVector[i], availableMethods) : 
      unused argument (availableMethods)
    Calls: <Anonymous>
    Execution halted
    ```

*   checking whether package ‘rSRD’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(refVector[i], ': unused argument (availableMethods) 
      Note: possible error in 'is.element(method, availableMethods)': unused argument (availableMethods) 
    See ‘/tmp/workdir/rSRD/new/rSRD.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    utilsCreateReference: possible error in is.element(refVector[i],
      availableMethods): unused argument (availableMethods)
    utilsCreateReference: possible error in is.element(method,
      availableMethods): unused argument (availableMethods)
    ```

# saeRobust

<details>

* Version: 0.5.0
* GitHub: https://github.com/wahani/saeRobust
* Source code: https://github.com/cran/saeRobust
* Date/Publication: 2024-03-15 14:50:02 UTC
* Number of recursive dependencies: 109

Run `revdepcheck::cloud_details(, "saeRobust")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘saeRobust-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: mse
    > ### Title: Compute the Mean Squared Error of an Estimator
    > ### Aliases: mse mse.fitrfh
    > 
    > ### ** Examples
    > 
    > data("grapes", package = "sae")
    ...
    > fitRFH <- rfh(
    +   grapehect ~ area + workdays - 1,
    +   data = grapes,
    +   samplingVar = "var"
    + )
    > 
    > mseRFH <- mse(fitRFH)
    Error in is.element("linear", type) : unused argument (type)
    Calls: mse -> mse.fitrfh -> predict -> predict.fitrfh
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library("saeRobust")
      Loading required package: aoos
      > 
      > if (requireNamespace("testthat", quietly = TRUE))
      +   if (requireNamespace("sae", quietly = TRUE))
      +     if (requireNamespace("saeSim", quietly = TRUE))
      +       testthat::test_check("saeRobust")
    ...
       2. └─saeRobust:::predict.fitrfh(modelFit, typ = c("reblupbc", "reblup"))
      
      [ FAIL 4 | WARN 0 | SKIP 3 | PASS 86 ]
      Error: Test failures
      In addition: Warning message:
      In check_dep_version() : ABI version mismatch: 
      lme4 was built with Matrix ABI version 1
      Current Matrix ABI version is 0
      Please re-install lme4 from source or restore original 'Matrix' package
      Execution halted
    ```

*   checking whether package ‘saeRobust’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element("linear", ': unused argument (type) 
      Note: possible error in 'is.element("reblup", ': unused argument (type) 
      Note: possible error in 'is.element("reblupbc", ': unused argument (type) 
    See ‘/tmp/workdir/saeRobust/new/saeRobust.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    predict.fitrfh: possible error in is.element("linear", type): unused
      argument (type)
    predict.fitrfh: possible error in is.element("reblup", type): unused
      argument (type)
    predict.fitrfh: possible error in is.element("reblupbc", type): unused
      argument (type)
    ```

# scUtils

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/scUtils
* Date/Publication: 2020-06-25 16:20:02 UTC
* Number of recursive dependencies: 51

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
* Number of recursive dependencies: 80

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
* Number of recursive dependencies: 124

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

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘basic-use.Rmd’
      ...
    [1] 0.8299353 0.8646037
    
    > folds <- randomFolds(data, k = 4, only_presence = TRUE, 
    +     seed = 25)
    
    > auc(cv_model)
    
      When sourcing ‘basic-use.R’:
    Error: object 'cv_model' not found
    Execution halted
    
      ‘basic-use.Rmd’ using ‘UTF-8’... failed
      ‘hyper-tuning.Rmd’ using ‘UTF-8’... OK
      ‘presence-absence.Rmd’ using ‘UTF-8’... OK
      ‘var-selection.Rmd’ using ‘UTF-8’... OK
    ```

# seAMLess

<details>

* Version: 0.1.1
* GitHub: https://github.com/eonurk/seAMLess
* Source code: https://github.com/cran/seAMLess
* Date/Publication: 2024-11-11 12:50:02 UTC
* Number of recursive dependencies: 51

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
      3.   ├─ggtern::ggplot_build(x)
      4.   └─ggtern:::ggplot_build.ggplot(x)
      5.     └─ggtern:::layers_add_or_remove_mask(plot)
      6.       └─ggint$plot_theme(plot)
      7.         └─ggplot2:::validate_theme(theme)
      8.           └─base::mapply(...)
      9.             └─ggplot2 (local) `<fn>`(...)
     10.               └─cli::cli_abort(...)
     11.                 └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘MuSiC’
    ```

# seedreg

<details>

* Version: 1.0.3
* GitHub: NA
* Source code: https://github.com/cran/seedreg
* Date/Publication: 2022-07-07 21:20:02 UTC
* Number of recursive dependencies: 127

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
     16. │             └─ggplot2 (local) setup_params(...)
     17. │               └─ggplot2:::make_summary_fun(...)
     18. │                 └─rlang::as_function(fun.data)
     19. │                   └─base::get(x, envir = env, mode = "function")
     20. └─base::.handleSimpleError(...)
     21.   └─rlang (local) h(simpleError(msg, call))
     22.     └─handlers[[1L]](cnd)
     23.       └─cli::cli_abort(...)
     24.         └─rlang::abort(...)
    Execution halted
    ```

# segen

<details>

* Version: 1.1.0
* GitHub: NA
* Source code: https://github.com/cran/segen
* Date/Publication: 2022-08-15 19:30:02 UTC
* Number of recursive dependencies: 109

Run `revdepcheck::cloud_details(, "segen")` for more info

</details>

## Newly broken

*   checking whether package ‘segen’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::is.scale’ by ‘greybox::is.scale’ when loading ‘segen’
    See ‘/tmp/workdir/segen/new/segen.Rcheck/00install.out’ for details.
    ```

# sensitivity

<details>

* Version: 1.30.1
* GitHub: NA
* Source code: https://github.com/cran/sensitivity
* Date/Publication: 2024-08-28 13:40:13 UTC
* Number of recursive dependencies: 154

Run `revdepcheck::cloud_details(, "sensitivity")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘sensitivity-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: PoincareChaosSqCoef
    > ### Title: Squared coefficients computation in generalized chaos
    > ### Aliases: PoincareChaosSqCoef
    > 
    > ### ** Examples
    > 
    > 
    ...
    > a <- 3
    > fX <- g(X, a = a)
    > 
    > out_1 <- out_2 <- PoincareOptimal(distr = list("unif", -1/2, 1/2), 
    +                                   only.values = FALSE, der = TRUE, 
    +                                   method = "quad")
    Error in is.element(distr[[1]], c("unif", "triangle", "norm", "gumbel")) : 
      unused argument (c("unif", "triangle", "norm", "gumbel"))
    Calls: PoincareOptimal
    Execution halted
    ```

*   checking whether package ‘sensitivity’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(distr[[1]], ': unused argument (c("unif", "triangle", "norm", "gumbel")) 
      Note: possible error in 'is.element(indices[[i + ': unused argument (var_null) 
      Note: possible error in 'is.element(i, var_null)': unused argument (var_null) 
      Note: possible error in 'is.element(indices[[i + ': unused argument (Z.coal) 
      Note: possible error in 'is.element(method, c("rank", ': unused argument (c("rank", "knn")) 
      Note: possible error in 'is.element(cat, catvar)': unused argument (catvar) 
    See ‘/tmp/workdir/sensitivity/new/sensitivity.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    PoincareOptimal: possible error in is.element(distr[[1]], c("unif",
      "triangle", "norm", "gumbel")): unused argument (c("unif",
      "triangle", "norm", "gumbel"))
    calc.pmvd.rec.boot: possible error in is.element(indices[[i + 1]],
      var_null): unused argument (var_null)
    calc.pmvd.rec.boot: possible error in is.element(i, var_null): unused
      argument (var_null)
    calc.pv: possible error in is.element(indices[[i + 1]], Z.coal): unused
      argument (Z.coal)
    pme_knn: possible error in is.element(method, c("rank", "knn")): unused
      argument (c("rank", "knn"))
    pmvd: possible error in is.element(indices[[i + 1]], var_null): unused
      argument (var_null)
    pmvd: possible error in is.element(i, var_null): unused argument
      (var_null)
    shapleySubsetMc : norm_vec: possible error in is.element(cat, catvar):
      unused argument (catvar)
    shapleysobol_knn: possible error in is.element(method, c("rank",
      "knn")): unused argument (c("rank", "knn"))
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.7Mb
      sub-directories of 1Mb or more:
        R      3.0Mb
        libs   5.0Mb
    ```

# sglg

<details>

* Version: 0.2.2
* GitHub: NA
* Source code: https://github.com/cran/sglg
* Date/Publication: 2022-09-04 03:50:01 UTC
* Number of recursive dependencies: 95

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
* Number of recursive dependencies: 123

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

# SHAPforxgboost

<details>

* Version: 0.1.3
* GitHub: https://github.com/liuyanguu/SHAPforxgboost
* Source code: https://github.com/cran/SHAPforxgboost
* Date/Publication: 2023-05-29 17:20:07 UTC
* Number of recursive dependencies: 116

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

# SHELF

<details>

* Version: 1.11.0
* GitHub: https://github.com/OakleyJ/SHELF
* Source code: https://github.com/cran/SHELF
* Date/Publication: 2024-09-06 15:50:02 UTC
* Number of recursive dependencies: 167

Run `revdepcheck::cloud_details(, "SHELF")` for more info

</details>

## Newly broken

*   checking whether package ‘SHELF’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(med, interval)': unused argument (interval) 
      Note: possible error in 'is.element(x.q1, x)': unused argument (x) 
      Note: possible error in 'is.element(x.q2, x)': unused argument (x) 
    See ‘/tmp/workdir/SHELF/new/SHELF.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    fitprecision: possible error in is.element(med, interval): unused
      argument (interval)
    makeSingleExpertPlot: possible error in is.element(x.q1, x): unused
      argument (x)
    makeSingleExpertPlot: possible error in is.element(x.q2, x): unused
      argument (x)
    ```

# sigminer

<details>

* Version: 2.3.1
* GitHub: https://github.com/ShixiangWang/sigminer
* Source code: https://github.com/cran/sigminer
* Date/Publication: 2024-05-11 08:50:02 UTC
* Number of recursive dependencies: 212

Run `revdepcheck::cloud_details(, "sigminer")` for more info

</details>

## Newly broken

*   checking whether package ‘sigminer’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/sigminer/new/sigminer.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is  7.9Mb
      sub-directories of 1Mb or more:
        R         1.0Mb
        data      2.0Mb
        extdata   1.0Mb
        help      1.7Mb
        libs      1.3Mb
    ```

## Installation

### Devel

```
* installing *source* package ‘sigminer’ ...
** package ‘sigminer’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c calc_cosine.cpp -o calc_cosine.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c get_intersect_size.cpp -o get_intersect_size.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o sigminer.so RcppExports.o calc_cosine.o get_intersect_size.o -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/sigminer/new/sigminer.Rcheck/00LOCK-sigminer/00new/sigminer/libs
...
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'NMF', details:
  call: is.element(models, models.wraps)
  error: unused argument (models.wraps)
Execution halted
ERROR: lazy loading failed for package ‘sigminer’
* removing ‘/tmp/workdir/sigminer/new/sigminer.Rcheck/sigminer’


```
### CRAN

```
* installing *source* package ‘sigminer’ ...
** package ‘sigminer’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c calc_cosine.cpp -o calc_cosine.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c get_intersect_size.cpp -o get_intersect_size.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o sigminer.so RcppExports.o calc_cosine.o get_intersect_size.o -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/sigminer/old/sigminer.Rcheck/00LOCK-sigminer/00new/sigminer/libs
...
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (sigminer)


```
# signatureSurvival

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/signatureSurvival
* Date/Publication: 2023-07-19 11:10:02 UTC
* Number of recursive dependencies: 103

Run `revdepcheck::cloud_details(, "signatureSurvival")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘signatureSurvival-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: MKMplot
    > ### Title: Multivariate Kaplan-Meier survival curve plot
    > ### Aliases: MKMplot
    > ### Keywords: multivariate survival analysis
    > 
    > ### ** Examples
    > 
    ...
        is.element
    
    > data(GSE50081)
    > MKMplot(data=GSE50081,mol=56,X=c("t.stage","n.stage",	"m.stage"),time="month",
    + status="status1",sml="none",quant=c("No",-0.2,0.2),plotmethod="ggsurvplot",
    + adjx = 5)
    Error in is.element(colnms, tolower(time)) : 
      unused argument (tolower(time))
    Calls: MKMplot
    Execution halted
    ```

*   checking whether package ‘signatureSurvival’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(colnms, tolower(time))': unused argument (tolower(time)) 
      Note: possible error in 'is.element(colnms, tolower(status))': unused argument (tolower(status)) 
      Note: possible error in 'is.element(Y, X)': unused argument (X) 
      Note: possible error in 'is.element(Y, X[1])': unused argument (X[1]) 
      Note: possible error in 'is.element(Y, X[2])': unused argument (X[2]) 
      Note: possible error in 'is.element(Y, X[3])': unused argument (X[3]) 
      Note: possible error in 'is.element(Y, X[4])': unused argument (X[4]) 
      Note: possible error in 'is.element(Y, X[5])': unused argument (X[5]) 
      Note: possible error in 'is.element(Y, X[6])': unused argument (X[6]) 
    ...
      Note: possible error in 'is.element(colname, c("pvalue", ': unused argument (c("pvalue", "p_value", "p-value", "p value", "pv", "PV")) 
      Note: possible error in 'is.element(colname, c("stage", ': unused argument (c("stage", "Stage")) 
      Note: possible error in 'is.element(colname, c("model", ': unused argument (c("model", "Model")) 
      Note: possible error in 'is.element(colname, c("HR", ': unused argument (c("HR", "hr", "beta", "coefficient", "hazard.rate", "Hazard.rate", "Hazard.ratio", "hazard.ratio")) 
      Note: possible error in 'is.element(colname, c("pv", ': unused argument (c("pv", "p-value", "pvalue", "p_value", "P-value", "P_value", "p.value", "P.value")) 
      Note: possible error in 'is.element(colname, c("Gene", ': unused argument (c("Gene", "gene", "gene_id", "gene_ID", "symbol", "Gene_ID")) 
    See ‘/tmp/workdir/signatureSurvival/new/signatureSurvival.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    MKMplot: possible error in is.element(colnms, tolower(time)): unused
      argument (tolower(time))
    MKMplot: possible error in is.element(colnms, tolower(status)): unused
      argument (tolower(status))
    MKMplot: possible error in is.element(Y, X): unused argument (X)
    MKMplot: possible error in is.element(Y, X[1]): unused argument (X[1])
    MKMplot: possible error in is.element(Y, X[2]): unused argument (X[2])
    MKMplot: possible error in is.element(Y, X[3]): unused argument (X[3])
    MKMplot: possible error in is.element(Y, X[4]): unused argument (X[4])
    MKMplot: possible error in is.element(Y, X[5]): unused argument (X[5])
    ...
      "hazard.ratio")): unused argument (c("HR", "hr", "beta",
      "coefficient", "hazard.rate", "Hazard.rate", "Hazard.ratio",
      "hazard.ratio"))
    weight: possible error in is.element(colname, c("pv", "p-value",
      "pvalue", "p_value", "P-value", "P_value", "p.value", "P.value")):
      unused argument (c("pv", "p-value", "pvalue", "p_value", "P-value",
      "P_value", "p.value", "P.value"))
    weight: possible error in is.element(colname, c("Gene", "gene",
      "gene_id", "gene_ID", "symbol", "Gene_ID")): unused argument
      (c("Gene", "gene", "gene_id", "gene_ID", "symbol", "Gene_ID"))
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.2Mb
      sub-directories of 1Mb or more:
        data   7.5Mb
    ```

# SimNPH

<details>

* Version: 0.5.5
* GitHub: https://github.com/SimNPH/SimNPH
* Source code: https://github.com/cran/SimNPH
* Date/Publication: 2024-03-04 10:10:02 UTC
* Number of recursive dependencies: 137

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
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 343 ]
    ...
      
      `names(actual)`:   "x"    
      `names(expected)`: "x" "y"
      
      `actual$y` is absent
      `expected$y` is a character vector ('mpg')
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 343 ]
      Error: Test failures
      Execution halted
    ```

# simRestore

<details>

* Version: 1.1.4
* GitHub: NA
* Source code: https://github.com/cran/simRestore
* Date/Publication: 2023-11-17 09:50:02 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::cloud_details(, "simRestore")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(simRestore)
      > 
      > test_check("simRestore")
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 79 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      ── Failure ('test-optimize_static.R:21:3'): simple optimization ────────────────
      vx$final_freq is not strictly less than 0.999. Difference: NaN
      ── Failure ('test-optimize_static.R:22:3'): simple optimization ────────────────
      max(vx$results$t) not equal to 2.
      1/1 mismatches
      [1] 1 - 2 == -1
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 79 ]
      Error: Test failures
      Execution halted
    ```

# singleCellHaystack

<details>

* Version: 1.0.2
* GitHub: https://github.com/alexisvdb/singleCellHaystack
* Source code: https://github.com/cran/singleCellHaystack
* Date/Publication: 2024-01-11 10:00:05 UTC
* Number of recursive dependencies: 120

Run `revdepcheck::cloud_details(, "singleCellHaystack")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘singleCellHaystack-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: show_result_haystack
    > ### Title: show_result_haystack
    > ### Aliases: show_result_haystack show_result_haystack.haystack
    > 
    > ### ** Examples
    > 
    > # using the toy example of the singleCellHaystack package
    ...
    gene_176 2.897121  -11.48481  -8.785839
    gene_234 3.293353  -11.36854  -8.669574
    gene_201 2.505329  -10.88401  -8.185042
    gene_229 3.241718  -10.53758  -7.838609
    > # 3. list a set of specified genes
    > set <- c("gene_497","gene_386", "gene_275")
    > show_result_haystack(res.haystack = res, gene = set)
    Error in is.element(rownames(result), gene) : unused argument (gene)
    Calls: show_result_haystack -> show_result_haystack.haystack -> [ -> [.data.frame
    Execution halted
    ```

*   checking whether package ‘singleCellHaystack’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(1:count.cells, ': unused argument (samp) 
      Note: possible error in 'is.element(genes, rownames(detection))': unused argument (rownames(detection)) 
      Note: possible error in 'is.element(detection.rownames, ': unused argument (genes) 
      Note: possible error in 'is.element(gene, rownames(expression))': unused argument (rownames(expression)) 
      Note: possible error in 'is.element(rownames(detection), ': unused argument (genes) 
      Note: possible error in 'is.element(rownames(result), ': unused argument (gene) 
    See ‘/tmp/workdir/singleCellHaystack/new/singleCellHaystack.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    haystack_2D: possible error in is.element(1:count.cells, samp): unused
      argument (samp)
    haystack_highD: possible error in is.element(1:count.cells, samp):
      unused argument (samp)
    hclust_haystack_highD: possible error in is.element(genes,
      rownames(detection)): unused argument (rownames(detection))
    hclust_haystack_highD: possible error in is.element(detection.rownames,
      genes): unused argument (genes)
    hclust_haystack_raw: possible error in is.element(genes,
      rownames(detection)): unused argument (rownames(detection))
    ...
    kmeans_haystack_raw: possible error in is.element(detection.rownames,
      genes): unused argument (genes)
    plot_gene_haystack_raw: possible error in is.element(gene,
      rownames(expression)): unused argument (rownames(expression))
    plot_gene_set_haystack_raw: possible error in is.element(genes,
      rownames(detection)): unused argument (rownames(detection))
    plot_gene_set_haystack_raw: possible error in
      is.element(rownames(detection), genes): unused argument (genes)
    show_result_haystack.haystack: possible error in
      is.element(rownames(result), gene): unused argument (gene)
    ```

# SNPannotator

<details>

* Version: 0.2.6.0
* GitHub: NA
* Source code: https://github.com/cran/SNPannotator
* Date/Publication: 2023-01-12 15:40:02 UTC
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "SNPannotator")` for more info

</details>

## Newly broken

*   checking whether package ‘SNPannotator’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(Linked_SNP, ': unused argument (conseq.dt$rs) 
      Note: possible error in 'is.element(rs, output$Linked_SNP)': unused argument (output$Linked_SNP) 
      Note: possible error in 'is.element(variation2, ': unused argument (LDVariantsNames) 
      Note: possible error in 'is.element(c("cadd_phred", ': unused argument (names(conseq$consequences[[1]])) 
      Note: possible error in 'is.element("description", ': unused argument (names(geneTab)) 
      Note: possible error in 'is.element(rsID, LDVariants[[i]]$synonyms)': unused argument (LDVariants[[i]]$synonyms) 
      Note: possible error in 'is.element(cadd.scores1, ': unused argument (alleles) 
    See ‘/tmp/workdir/SNPannotator/new/SNPannotator.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    annotate: possible error in is.element(Linked_SNP, conseq.dt$rs):
      unused argument (conseq.dt$rs)
    checkifOutputIncludesRS: possible error in is.element(rs,
      output$Linked_SNP): unused argument (output$Linked_SNP)
    correctSynonymIDs: possible error in is.element(variation2,
      LDVariantsNames): unused argument (LDVariantsNames)
    getConseqOnList: possible error in is.element(c("cadd_phred",
      "variant_allele"), names(conseq$consequences[[1]])): unused argument
      (names(conseq$consequences[[1]]))
    getConseqOnList.parallel: possible error in is.element(c("cadd_phred",
      "variant_allele"), names(conseq$consequences[[1]])): unused argument
      (names(conseq$consequences[[1]]))
    getVariantData: possible error in is.element("description",
      names(geneTab)): unused argument (names(geneTab))
    returnSynonym: possible error in is.element(rsID,
      LDVariants[[i]]$synonyms): unused argument (LDVariants[[i]]$synonyms)
    select.CADD.scores: possible error in is.element(cadd.scores1,
      alleles): unused argument (alleles)
    ```

# soc.ca

<details>

* Version: 0.8.0
* GitHub: https://github.com/Rsoc/soc.ca
* Source code: https://github.com/cran/soc.ca
* Date/Publication: 2021-09-02 22:50:02 UTC
* Number of recursive dependencies: 130

Run `revdepcheck::cloud_details(, "soc.ca")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘soc.ca-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: add.to.label
    > ### Title: Add values to label
    > ### Aliases: add.to.label
    > 
    > ### ** Examples
    > 
    > example(soc.ca)
    ...
    [13] "Film:\n Horror"        "Film:\n Musical"       "Film:\n Romance"      
    [16] "Film:\n SciFi"         "Art:\n Impressionism"  "Art:\n Landscape"     
    [19] "Art:\n ModernArt"      "Art:\n PerformanceArt" "Art:\n Portrait"      
    [22] "Art:\n RenaissanceArt" "Art:\n StillLife"      "Eat:\n Fish&Chips"    
    [25] "Eat:\n FrenchRest"     "Eat:\n IndianRest"     "Eat:\n ItalianRest"   
    [28] "Eat:\n Pub"            "Eat:\n SteakHouse"    
    > map.ctr(result.label)
    Error in is.element(scalebreaks, nolabel) : unused argument (nolabel)
    Calls: map.ctr -> plot.flow -> breaksandscales
    Execution halted
    ```

*   checking whether package ‘soc.ca’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(scalebreaks, ': unused argument (nolabel) 
    See ‘/tmp/workdir/soc.ca/new/soc.ca.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    breaksandscales: possible error in is.element(scalebreaks, nolabel):
      unused argument (nolabel)
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘FactoMineR’ ‘flextable’ ‘htmlTable’ ‘stringr’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 938 marked UTF-8 strings
    ```

# SOMbrero

<details>

* Version: 1.4-2
* GitHub: https://github.com/tuxette/SOMbrero
* Source code: https://github.com/cran/SOMbrero
* Date/Publication: 2024-01-25 22:10:10 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::cloud_details(, "SOMbrero")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘SOMbrero-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.somRes
    > ### Title: Plot a 'somRes' class object
    > ### Aliases: plot.somRes
    > 
    > ### ** Examples
    > 
    > # run the SOM algorithm on the numerical data of 'iris' data set
    ...
    > iris.som <- trainSOM(x.data = iris[, 1:4], nb.save = 2)
    > # plots
    > # on energy
    > plot(iris.som, what = "energy") 
    > # on observations
    > plot(iris.som, what = "obs", type = "lines")
    Error in is.element(type, authorizedtypes[[sommap$parameters$type]]) : 
      unused argument (authorizedtypes[[sommap$parameters$type]])
    Calls: plot -> plot.somRes -> plotObs
    Execution halted
    ```

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
       2. └─SOMbrero:::quality.somRes(nsom)
       3.   └─SOMbrero:::topographicError(sommap)
       4.     ├─base::mean(...)
       5.     └─base::sapply(...)
       6.       └─base::lapply(X = X, FUN = FUN, ...)
       7.         └─SOMbrero (local) FUN(X[[i]], ...)
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 56 ]
      Error: Test failures
      Execution halted
    ```

*   checking whether package ‘SOMbrero’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(type, authorizedtypes[[x$som$parameters$type]][[args$what]])': unused argument (authorizedtypes[[x$som$parameters$type]][[args$what]]) 
      Note: possible error in 'is.element(type, authorizedtypes[[sommap$parameters$type]])': unused argument (authorizedtypes[[sommap$parameters$type]]) 
      Note: possible error in 'is.element(ind.winner2[x], ': unused argument (selectNei(sommap$clustering[x], sommap$parameters$the.grid, 1, radius.type = "letremy", dist.type = "maximum")) 
    See ‘/tmp/workdir/SOMbrero/new/SOMbrero.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    plot.somSC: possible error in is.element(type,
      authorizedtypes[[x$som$parameters$type]][[args$what]]): unused
      argument (authorizedtypes[[x$som$parameters$type]][[args$what]])
    plotAdd: possible error in is.element(type,
      authorizedtypes[[sommap$parameters$type]]): unused argument
      (authorizedtypes[[sommap$parameters$type]])
    plotObs: possible error in is.element(type,
      authorizedtypes[[sommap$parameters$type]]): unused argument
      (authorizedtypes[[sommap$parameters$type]])
    plotPrototypes: possible error in is.element(type,
      authorizedtypes[[sommap$parameters$type]]): unused argument
      (authorizedtypes[[sommap$parameters$type]])
    topographicError : <anonymous>: possible error in
      is.element(ind.winner2[x], selectNei(sommap$clustering[x],
      sommap$parameters$the.grid, 1, radius.type = "letremy", dist.type =
      "maximum")): unused argument (selectNei(sommap$clustering[x],
      sommap$parameters$the.grid, 1, radius.type = "letremy", dist.type =
      "maximum"))
    ```

# spectralAnalysis

<details>

* Version: 4.3.3
* GitHub: NA
* Source code: https://github.com/cran/spectralAnalysis
* Date/Publication: 2024-01-30 08:50:02 UTC
* Number of recursive dependencies: 117

Run `revdepcheck::cloud_details(, "spectralAnalysis")` for more info

</details>

## Newly broken

*   checking whether package ‘spectralAnalysis’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/spectralAnalysis/new/spectralAnalysis.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        doc   4.6Mb
    ```

## Installation

### Devel

```
* installing *source* package ‘spectralAnalysis’ ...
** package ‘spectralAnalysis’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'NMF', details:
  call: is.element(models, models.wraps)
  error: unused argument (models.wraps)
Execution halted
ERROR: lazy loading failed for package ‘spectralAnalysis’
* removing ‘/tmp/workdir/spectralAnalysis/new/spectralAnalysis.Rcheck/spectralAnalysis’


```
### CRAN

```
* installing *source* package ‘spectralAnalysis’ ...
** package ‘spectralAnalysis’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Creating a new generic function for ‘smooth’ in package ‘spectralAnalysis’
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (spectralAnalysis)


```
# spectralR

<details>

* Version: 0.1.3
* GitHub: https://github.com/olehprylutskyi/spectralR
* Source code: https://github.com/cran/spectralR
* Date/Publication: 2023-08-24 09:20:02 UTC
* Number of recursive dependencies: 120

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
      
      test_get.pixel.data.R.........    1 tests [0;32mOK[0m [0;36m63ms[0m
      
      test_prepare.vector.data.R....    1 tests [0;32mOK[0m 
    ...
      test_prepare.vector.data.R....    8 tests [0;32mOK[0m 
      test_prepare.vector.data.R....    9 tests [0;32mOK[0m 
      test_prepare.vector.data.R....   10 tests [0;32mOK[0m 
      test_prepare.vector.data.R....   11 tests [0;32mOK[0m 
      test_prepare.vector.data.R....   12 tests [0;32mOK[0m [0;36m42ms[0m
      
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
* Number of recursive dependencies: 154

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
      
      [ FAIL 3 | WARN 4 | SKIP 0 | PASS 80 ]
      Error: Test failures
      Execution halted
    ```

# spooky

<details>

* Version: 1.4.0
* GitHub: NA
* Source code: https://github.com/cran/spooky
* Date/Publication: 2022-08-13 20:00:02 UTC
* Number of recursive dependencies: 104

Run `revdepcheck::cloud_details(, "spooky")` for more info

</details>

## Newly broken

*   checking whether package ‘spooky’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::is.scale’ by ‘greybox::is.scale’ when loading ‘spooky’
    See ‘/tmp/workdir/spooky/new/spooky.Rcheck/00install.out’ for details.
    ```

# sport

<details>

* Version: 0.2.1
* GitHub: https://github.com/gogonzo/sport
* Source code: https://github.com/cran/sport
* Date/Publication: 2024-01-08 23:50:02 UTC
* Number of recursive dependencies: 70

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
* Number of recursive dependencies: 149

Run `revdepcheck::cloud_details(, "SqueakR")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘SqueakR.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘SqueakR.Rmd’
      ...
     $ experimenters    : NULL
     $ experimental_data: list()
    
    > my_new_data <- add_timepoint_data(data_path = "../inst/extdata/Example_Mouse_Data.xlsx", 
    +     t1 = 5, t2 = 25)
    Adding call features Excel file to workspace...
    
      When sourcing ‘SqueakR.R’:
    Error: `path` does not exist: ‘../inst/extdata/Example_Mouse_Data.xlsx’
    Execution halted
    
      ‘SqueakR.Rmd’ using ‘UTF-8’... failed
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.8Mb
      sub-directories of 1Mb or more:
        doc   8.2Mb
    ```

# statgenGWAS

<details>

* Version: 1.0.10
* GitHub: https://github.com/Biometris/statgenGWAS
* Source code: https://github.com/cran/statgenGWAS
* Date/Publication: 2024-11-15 15:00:01 UTC
* Number of recursive dependencies: 72

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
        conversion failure on '← 2@3' in 'mbcsToSbcs': dot substituted for <e2>
      5: In grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
        conversion failure on '← 2@3' in 'mbcsToSbcs': dot substituted for <86>
      6: In grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
        conversion failure on '← 2@3' in 'mbcsToSbcs': dot substituted for <90>
      7: `aes_()` was deprecated in ggplot2 3.0.0.
      ℹ Please use tidy evaluation idioms with `aes()`
      ℹ The deprecated feature was likely used in the statgenGWAS package.
        Please report the issue at <https://github.com/Biometris/statgenGWAS/issues>. 
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 15.1Mb
      sub-directories of 1Mb or more:
        data   7.0Mb
        libs   7.0Mb
    ```

# statgenHTP

<details>

* Version: 1.0.7
* GitHub: https://github.com/Biometris/statgenHTP
* Source code: https://github.com/cran/statgenHTP
* Date/Publication: 2024-10-14 11:50:06 UTC
* Number of recursive dependencies: 134

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
        'length = 3' in coercion to 'logical(1)'
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
      installed size is  9.0Mb
      sub-directories of 1Mb or more:
        data   7.5Mb
    ```

# Superpower

<details>

* Version: 0.2.0
* GitHub: https://github.com/arcaldwell49/Superpower
* Source code: https://github.com/cran/Superpower
* Date/Publication: 2022-05-17 13:50:02 UTC
* Number of recursive dependencies: 119

Run `revdepcheck::cloud_details(, "Superpower")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Superpower-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ANOVA_exact
    > ### Title: Simulates an exact dataset (mu, sd, and r represent empirical,
    > ###   not population, mean and covariance matrix) from the design to
    > ###   calculate power
    > ### Aliases: ANOVA_exact ANOVA_exact2
    > 
    > ### ** Examples
    ...
    > ## with a mean pattern of 1, 0, 1, 0, conditions labeled 'condition' and
    > ## 'voice', with names for levels of "cheerful", "sad", amd "human", "robot"
    > design_result <- ANOVA_design(design = "2w*2w", n = 40, mu = c(1, 0, 1, 0),
    +       sd = 2, r = 0.8, labelnames = c("condition", "cheerful",
    +       "sad", "voice", "human", "robot"))
    > exact_result <- ANOVA_exact(design_result, alpha_level = 0.05)
    Error in is.element(correction, c("none", "GG", "HF")) : 
      unused argument (c("none", "GG", "HF"))
    Calls: ANOVA_exact
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(Superpower)
      Warning message:
      In check_dep_version() : ABI version mismatch: 
      lme4 was built with Matrix ABI version 1
      Current Matrix ABI version is 0
    ...
       1. └─Superpower::ANOVA_exact2(design_result, verbose = FALSE) at test-powerftest.R:11:3
      ── Error ('test-powerftest.R:81:3'): Test power.ftest ──────────────────────────
      Error in `is.element(correction, c("none", "GG", "HF"))`: unused argument (c("none", "GG", "HF"))
      Backtrace:
          ▆
       1. └─Superpower::ANOVA_exact2(design_result, verbose = FALSE) at test-powerftest.R:81:3
      
      [ FAIL 29 | WARN 3 | SKIP 13 | PASS 285 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘compromise_power.Rmd’
      ...
    ℹ See also `vignette("ggplot2-in-packages")` for more information.
    ℹ The deprecated feature was likely used in the Superpower package.
      Please report the issue at
      <https://github.com/arcaldwell49/Superpower/issues>.
    
    > exact2_res = ANOVA_exact2(design_result, verbose = FALSE)
    
    ...
    
      When sourcing ‘more_anova_designs.R’:
    Error: unused argument (c("univariate", "multivariate"))
    Execution halted
    
      ‘ANCOVAs.Rmd’ using ‘UTF-8’... OK
      ‘compromise_power.Rmd’ using ‘UTF-8’... failed
      ‘emmeans_power.Rmd’ using ‘UTF-8’... failed
      ‘intro_to_superpower.Rmd’ using ‘UTF-8’... failed
      ‘more_anova_designs.Rmd’ using ‘UTF-8’... failed
    ```

*   checking whether package ‘Superpower’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(emm_model, ': unused argument (c("univariate", "multivariate")) 
      Note: possible error in 'is.element(contrast_type, ': unused argument (c("pairwise", "revpairwise", "eff", "consec", "poly", "del.eff", "trt.vs.ctrl", "trt.vs.ctrl1", "trt.vs.ctrlk", "mean_chg")) 
      Note: possible error in 'is.element(correction, ': unused argument (c("none", "GG", "HF")) 
      Note: possible error in 'is.element(contrast_type, ': unused argument (c("pairwise", "revpairwise", "eff", "consec", "poly", "del.eff", "trt.vs.ctrl", "trt.vs.ctrl1", "trt.vs.ctrlk", "mean_chg", "dunnett", "tukey")) 
      Note: possible error in 'is.element(emm_p_adjust, ': unused argument (c("dunnett", "tukey", "sidak", "scheffe", "dunnettx", "mvt", "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")) 
      Note: possible error in 'is.element(p_adjust, ': unused argument (c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")) 
      Note: possible error in 'is.element(emm_p_adjust, ': unused argument (c("dunnett", "tukey", "sidak", "scheffe", "dunnettx")) 
      Note: possible error in 'is.element(return, c("main_results", ': unused argument (c("main_results", "aov", "anova", "pc_results", "pairwise", "emm_results", "emmeans", "emm", "manova_results", "manova")) 
    See ‘/tmp/workdir/Superpower/new/Superpower.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ANOVA_exact: possible error in is.element(emm_model, c("univariate",
      "multivariate")): unused argument (c("univariate", "multivariate"))
    ANOVA_exact: possible error in is.element(contrast_type, c("pairwise",
      "revpairwise", "eff", "consec", "poly", "del.eff", "trt.vs.ctrl",
      "trt.vs.ctrl1", "trt.vs.ctrlk", "mean_chg")): unused argument
      (c("pairwise", "revpairwise", "eff", "consec", "poly", "del.eff",
      "trt.vs.ctrl", "trt.vs.ctrl1", "trt.vs.ctrlk", "mean_chg"))
    ANOVA_exact: possible error in is.element(correction, c("none", "GG",
      "HF")): unused argument (c("none", "GG", "HF"))
    ANOVA_exact2: possible error in is.element(emm_model, c("univariate",
    ...
      "emm_results", "emmeans", "emm", "manova_results", "manova")): unused
      argument (c("main_results", "aov", "anova", "pc_results", "pairwise",
      "emm_results", "emmeans", "emm", "manova_results", "manova"))
    plot_power: possible error in is.element(emm_model, c("univariate",
      "multivariate")): unused argument (c("univariate", "multivariate"))
    plot_power: possible error in is.element(contrast_type, c("pairwise",
      "revpairwise", "eff", "consec", "poly", "del.eff", "trt.vs.ctrl",
      "trt.vs.ctrl1", "trt.vs.ctrlk", "mean_chg")): unused argument
      (c("pairwise", "revpairwise", "eff", "consec", "poly", "del.eff",
      "trt.vs.ctrl", "trt.vs.ctrl1", "trt.vs.ctrlk", "mean_chg"))
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ANCOVAs.Rmd’ using rmarkdown
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘gridExtra’ ‘mvtnorm’
      All declared Imports should be used.
    ```

# surveyexplorer

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/surveyexplorer
* Date/Publication: 2024-06-07 09:50:02 UTC
* Number of recursive dependencies: 86

Run `revdepcheck::cloud_details(, "surveyexplorer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘surveyexplorer-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: multi_freq
    > ### Title: Generate an UpSet plot for multiple-choice questions
    > ### Aliases: multi_freq
    > 
    > ### ** Examples
    > 
    > 
    ...
    
    > 
    > #Basic Upset plot
    > 
    > #Use `group_by` to partition the question into several groups
    >  multi_freq(berlinbears, question = dplyr::starts_with('will_eat'), group_by
    +  = gender)
    Error in as.unit(e2) : object is not coercible to a unit
    Calls: <Anonymous> ... polylineGrob -> is.unit -> unit.c -> Ops.unit -> as.unit
    Execution halted
    ```

# tabledown

<details>

* Version: 1.0.0
* GitHub: https://github.com/masiraji/tabledown
* Source code: https://github.com/cran/tabledown
* Date/Publication: 2024-05-02 13:40:03 UTC
* Number of recursive dependencies: 167

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
* Number of recursive dependencies: 167

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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘tciu-LT-kimesurface.Rmd’
      ...
    > require(ggplot2)
    
    > sample_save[[1]]
    
    > sample_save[[2]]
    
      When sourcing ‘tciu-LT-kimesurface.R’:
    ...
    
    > fmri_image(fmri_generate$fmri_data, option = "manually", 
    +     voxel_location = c(40, 22, 33), time = 4)
    
      When sourcing ‘tciu-fMRI-analytics.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘tciu-LT-kimesurface.Rmd’ using ‘UTF-8’... failed
      ‘tciu-fMRI-analytics.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘tciu-LT-kimesurface.Rmd’ using rmarkdown
    
    Quitting from lines 159-160 [unnamed-chunk-5] (tciu-LT-kimesurface.Rmd)
    Error: processing vignette 'tciu-LT-kimesurface.Rmd' failed with diagnostics:
    unused arguments (list(1, 2), list(list("black", 0.727272727272727, 1, "butt", FALSE, TRUE), list("white", "black", 0.727272727272727, 1, TRUE), list("", "plain", "black", 16, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(4, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 4, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 4, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 
        NULL, 1, -90, NULL, c(0, 0, 0, 4), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(3.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 3.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 3.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 3.2), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0.5, NULL, 
        NULL, NULL, c(0, 3.2, 0, 3.2), NULL, TRUE), list("grey20", NULL, NULL, NULL, FALSE, TRUE), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 4, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(NULL, NA, NULL, NULL, TRUE), c(8, 8, 8, 8), 16, NULL, NULL, NULL, 1.2, NULL, NULL, 8, NULL, NULL, NULL, NULL, 0.2, NULL, list(NULL, NULL, NULL, 0.8, NULL, NULL, 
        NULL, NULL, NULL, NULL, TRUE), NULL, list(NULL, "bold", "black", 14, 0, NULL, NULL, NULL, NULL, NULL, FALSE), NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, c(0, 0, 0, 0), list(), 16, list("grey92", NA, NULL, NULL, TRUE), list(), 8, NULL, NULL, list("white", NULL, NULL, NULL, FALSE, TRUE), NULL, list(NULL, 0.5, NULL, NULL, FALSE, TRUE), NULL, list(), NULL, list(), FALSE, list(NULL, "white", NULL, NULL, TRUE), list(NULL, NULL, NULL, 1.2, 0.5, 1, NULL, 
    ...
    Quitting from lines 184-185 [unnamed-chunk-5] (tciu-fMRI-analytics.Rmd)
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

# tcplfit2

<details>

* Version: 0.1.7
* GitHub: https://github.com/USEPA/CompTox-ToxCast-tcplFit2
* Source code: https://github.com/cran/tcplfit2
* Date/Publication: 2024-09-23 22:30:05 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::cloud_details(, "tcplfit2")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘tcplfit2-vignette.Rmd’
      ...
    
    > for (spid in spid.list) {
    +     temp <- mc3[is.element(mc3$spid, spid), ]
    +     conc <- 10^temp$logc
    +     resp <- temp$resp
    +     dtxsid <- temp[1, .... [TRUNCATED] 
    
      When sourcing ‘tcplfit2-vignette.R’:
    Error: unused argument (spid)
    Execution halted
    
      ‘tcplfit2-vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘tcplfit2-vignette.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        data   2.5Mb
        doc    2.4Mb
    ```

# tetragon

<details>

* Version: 1.3.0
* GitHub: NA
* Source code: https://github.com/cran/tetragon
* Date/Publication: 2022-08-13 17:30:02 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::cloud_details(, "tetragon")` for more info

</details>

## Newly broken

*   checking whether package ‘tetragon’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::is.scale’ by ‘greybox::is.scale’ when loading ‘tetragon’
    See ‘/tmp/workdir/tetragon/new/tetragon.Rcheck/00install.out’ for details.
    ```

# thematic

<details>

* Version: 0.1.6
* GitHub: https://github.com/rstudio/thematic
* Source code: https://github.com/cran/thematic
* Date/Publication: 2024-07-29 15:50:02 UTC
* Number of recursive dependencies: 105

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

# tidybayes

<details>

* Version: 3.0.7
* GitHub: https://github.com/mjskay/tidybayes
* Source code: https://github.com/cran/tidybayes
* Date/Publication: 2024-09-15 06:20:02 UTC
* Number of recursive dependencies: 194

Run `revdepcheck::cloud_details(, "tidybayes")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tidybayes-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: compare_levels
    > ### Title: Compare the value of draws of some variable from a Bayesian
    > ###   model for different levels of a factor
    > ### Aliases: compare_levels
    > ### Keywords: manip
    > 
    > ### ** Examples
    ...
     12. │     └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13. │       └─l$compute_geom_2(d, theme = plot$theme)
     14. │         └─ggplot2 (local) compute_geom_2(..., self = self)
     15. │           └─self$geom$use_defaults(...)
     16. └─base::.handleSimpleError(...)
     17.   └─rlang (local) h(simpleError(msg, call))
     18.     └─handlers[[1L]](cnd)
     19.       └─cli::cli_abort(...)
     20.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This is necessary because some tests fail otherwise; see https://github.com/hadley/testthat/issues/144
      > Sys.setenv("R_TESTS" = "")
      > 
      > library(testthat)
      > library(tidybayes)
      > 
      > test_check("tidybayes")
    ...
      • test.stat_dist_slabinterval/ccdfintervalh-using-args.svg
      • test.stat_eye/one-parameter-horizontal-eye-mode-hdi.svg
      • test.stat_eye/one-parameter-horizontal-eye.svg
      • test.stat_eye/one-parameter-horizontal-half-eye.svg
      • test.stat_eye/one-parameter-vertical-eye.svg
      • test.stat_eye/one-parameter-vertical-halfeye.svg
      • test.stat_eye/two-parameter-factor-horizontal-eye-fill.svg
      • test.stat_eye/two-parameter-factor-horizontal-half-eye.svg
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘tidy-brms.Rmd’
      ...
    +     ]) %>% median_qi(condition_mean = b_Intercept + r_condition, 
    +     .width = c(0.95, 0 .... [TRUNCATED] 
    
      When sourcing ‘tidy-brms.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `use_defaults()`:
    ...
    
      When sourcing ‘tidybayes.R’:
    Error: error in evaluating the argument 'object' in selecting a method for function 'sampling': object 'ABC_stan' not found
    Execution halted
    
      ‘tidy-brms.Rmd’ using ‘UTF-8’... failed
      ‘tidy-posterior.Rmd’ using ‘UTF-8’... failed
      ‘tidy-rstanarm.Rmd’ using ‘UTF-8’... failed
      ‘tidybayes-residuals.Rmd’ using ‘UTF-8’... failed
      ‘tidybayes.Rmd’ using ‘UTF-8’... failed
    ```

# tidycat

<details>

* Version: 0.1.2
* GitHub: https://github.com/guyabel/tidycat
* Source code: https://github.com/cran/tidycat
* Date/Publication: 2021-08-02 04:20:01 UTC
* Number of recursive dependencies: 70

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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘intro.Rmd’
      ...
    
    > library(ggforce)
    
    > ggplot(data = d0, mapping = aes(x = level, y = estimate, 
    +     colour = reference, ymin = conf.low, ymax = conf.high)) + 
    +     facet_col(facets =  .... [TRUNCATED] 
    
      When sourcing ‘intro.R’:
    Error: $ operator is invalid for atomic vectors
    Execution halted
    
      ‘intro.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
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
* Number of recursive dependencies: 133

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
* Number of recursive dependencies: 79

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
# tidyplots

<details>

* Version: 0.2.1
* GitHub: https://github.com/jbengler/tidyplots
* Source code: https://github.com/cran/tidyplots
* Date/Publication: 2025-01-19 21:50:02 UTC
* Number of recursive dependencies: 128

Run `revdepcheck::cloud_details(, "tidyplots")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tidyplots-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: add
    > ### Title: Add ggplot2 code to a tidyplot
    > ### Aliases: add
    > 
    > ### ** Examples
    > 
    > study %>%
    ...
     23.                               └─self$extract_key(...)
     24.                                 └─ggplot2 (local) extract_key(...)
     25.                                   └─scale$map(breaks)
     26.                                     └─ggplot2 (local) map(..., self = self)
     27.                                       ├─base::c(pal, na_value)
     28.                                       └─tidyplots:::c.tidycolor(pal, na_value)
     29.                                         └─tidyplots::new_color_scheme(NextMethod())
     30.                                           └─cli::cli_abort("{.arg x} must be a vector of hex colors.")
     31.                                             └─rlang::abort(...)
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
      • labels/reverse-x.svg
      • labels/reverse-y.svg
      • labels/sort-color-1.svg
      • labels/sort-color-2.svg
      • labels/sort-x-1.svg
      • labels/sort-x-2.svg
      • labels/sort-y-1.svg
      • labels/sort-y-2.svg
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘tidyplots.Rmd’
      ...
    17         D treatment  low         p07  45   male    25
    18         D treatment  low         p08  32 female    21
    19         D treatment  low         p09  37   male    22
    20         D treatment  low         p10  24 female    23
    
    > study %>% tidyplot(x = treatment, y = score)
    
      When sourcing ‘tidyplots.R’:
    Error: `x` must be a vector of hex colors.
    Execution halted
    
      ‘tidyplots.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘tidyplots.Rmd’ using rmarkdown
    
    Quitting from lines 80-82 [unnamed-chunk-5] (tidyplots.Rmd)
    Error: processing vignette 'tidyplots.Rmd' failed with diagnostics:
    `x` must be a vector of hex colors.
    --- failed re-building ‘tidyplots.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘tidyplots.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# tidytreatment

<details>

* Version: 0.3.1
* GitHub: https://github.com/bonStats/tidytreatment
* Source code: https://github.com/cran/tidytreatment
* Date/Publication: 2025-01-10 12:30:02 UTC
* Number of recursive dependencies: 109

Run `revdepcheck::cloud_details(, "tidytreatment")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘use-tidytreatment-BART.Rmd’
      ...
    +     by = ".row") %>% ggplot() + stat_halfeye(aes(x = z, y = fit)) + 
    +     facet_wrap(~c1, l .... [TRUNCATED] 
    
      When sourcing ‘use-tidytreatment-BART.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `use_defaults()`:
    ...
      When sourcing ‘use-tidytreatment-bartCause.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `use_defaults()`:
    ! unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, 
        NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "#4D4D4DFF", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 
    Execution halted
    
      ‘use-tidytreatment-BART.Rmd’ using ‘UTF-8’... failed
      ‘use-tidytreatment-bartCause.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘use-tidytreatment-BART.Rmd’ using rmarkdown
    
    Quitting from lines 163-177 [plot-tidy-bart] (use-tidytreatment-BART.Rmd)
    Error: processing vignette 'use-tidytreatment-BART.Rmd' failed with diagnostics:
    Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `use_defaults()`:
    ! unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, 
        NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "#4D4D4DFF", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, 
    ...
        NULL, NULL, NULL, NULL, NULL, NULL, list(NULL, NA, NULL, NULL, TRUE), NULL, 2, NULL, NULL, NULL, 1.2, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.2, NULL, list(NULL, NULL, NULL, 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0, list(), 2, list("white", NA, NULL, NULL, TRUE), list(NULL, "#333333FF", NULL, NULL, TRUE), NULL, NULL, 
        NULL, list("#EBEBEBFF", NULL, NULL, NULL, FALSE, "#EBEBEBFF", TRUE), NULL, list(NULL, 0.5, NULL, NULL, FALSE, NULL, TRUE), NULL, NULL, NULL, NULL, FALSE, list(NULL, "white", NULL, NULL, TRUE), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(5.5, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, 
            TRUE), "topleft", NULL, NULL, list("#D9D9D9FF", "#333333FF", NULL, NULL, TRUE), NULL, NULL, "on", "inside", list(NULL, NULL, "#1A1A1AFF", 0.8, NULL, NULL, NULL, NULL, c(4.4, 4.4, 4.4, 4.4), NULL, TRUE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75))
    --- failed re-building ‘use-tidytreatment-bartCause.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘use-tidytreatment-BART.Rmd’ ‘use-tidytreatment-bartCause.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘bartMachine’
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

# TOSTER

<details>

* Version: 0.8.3
* GitHub: NA
* Source code: https://github.com/cran/TOSTER
* Date/Publication: 2024-05-08 16:40:02 UTC
* Number of recursive dependencies: 109

Run `revdepcheck::cloud_details(, "TOSTER")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘TOSTER-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: dataTOSTone
    > ### Title: TOST One Sample T-Test
    > ### Aliases: dataTOSTone
    > 
    > ### ** Examples
    > 
    > library("TOSTER")
    ...
                      N      Mean        Median      SD           SE           
     ───────────────────────────────────────────────────────────────────────── 
       Sepal.Width    150    3.057333    3.000000    0.4358663    0.03558833   
     ───────────────────────────────────────────────────────────────────────── 
    
    Error in use_defaults(..., self = self) : 
      unused argument (theme = list(list("black", 0.727272727272727, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.727272727272727, 1, TRUE), list("", "plain", "black", 16, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.727272727272727, 0.727272727272727, 1, 1, "", 5.62335685623357, 2.18181818181818, 19, TRUE), 8, c(8, 8, 8, 8), NULL, NULL, list(NULL, NULL, "#333333", NULL, NULL, NULL, NULL, 
        NULL, c(10, 0, 0, 0), NULL, FALSE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 4, 0), NULL, TRUE), NULL, list(NULL, NULL, "#333333", NULL, NULL, NULL, 90, NULL, c(0, 10, 0, 0), NULL, FALSE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 4), NULL, TRUE), list(NULL, NULL, "#4D4D4DFF", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, "#333333", NULL, NULL, NULL, NULL, NULL, c(5, 0, 0, 0), NULL, FALSE), list(NULL, NULL, NULL,
    Calls: <Anonymous> ... <Anonymous> -> <Anonymous> -> <Anonymous> -> <Anonymous>
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(TOSTER)
      
      Attaching package: 'TOSTER'
      
      The following object is masked from 'package:testthat':
    ...
       26.                                     └─base::Map(...)
       27.                                       └─base::mapply(FUN = f, ..., SIMPLIFY = FALSE)
       28.                                         └─ggplot2 (local) `<fn>`(layer = dots[[1L]][[1L]], df = dots[[2L]][[1L]])
       29.                                           └─layer$compute_geom_2(key, single_params, theme)
       30.                                             └─ggplot2 (local) compute_geom_2(..., self = self)
       31.                                               └─self$geom$use_defaults(...)
      
      [ FAIL 8 | WARN 0 | SKIP 0 | PASS 1034 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘IntroTOSTt.Rmd’
      ...
    mean of x mean of y 
         0.75      2.33 
    
    
    > plot(res1, type = "cd")
    
      When sourcing ‘IntroTOSTt.R’:
    ...
    Error: unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(7, 0, 0, 0), NULL, FALSE), list(NULL, NULL, NULL, NULL, NULL, 
        0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 7, 0, 0), NULL, FALSE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "#4D4D4DFF", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, "bold", NULL, 11, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, FALSE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(), NULL, l
    Execution halted
    
      ‘IntroTOSTt.Rmd’ using ‘UTF-8’... failed
      ‘IntroductionToTOSTER.Rmd’ using ‘UTF-8’... OK
      ‘SMD_calcs.Rmd’ using ‘UTF-8’... OK
      ‘correlations.Rmd’ using ‘UTF-8’... OK
      ‘robustTOST.Rmd’ using ‘UTF-8’... failed
      ‘the_ftestTOSTER.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘IntroTOSTt.Rmd’ using rmarkdown
    ```

# TransProR

<details>

* Version: 0.0.6
* GitHub: https://github.com/SSSYDYSSS/TransProR
* Source code: https://github.com/cran/TransProR
* Date/Publication: 2024-12-10 21:30:09 UTC
* Number of recursive dependencies: 212

Run `revdepcheck::cloud_details(, "TransProR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘TransProR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gene_highlights
    > ### Title: Add gene highlights to a ggtree object
    > ### Aliases: gene_highlights
    > 
    > ### ** Examples
    > 
    > data("gtree", package = "TransProR")
    ...
      5. │     └─ggplot2:::ggplot_add.list(object, p, objectname)
      6. │       ├─ggplot2::ggplot_add(o, plot, object_name)
      7. │       └─ggplot2:::ggplot_add.Layer(o, plot, object_name)
      8. │         └─ggplot2:::new_layer_names(object, names(plot$layers))
      9. │           └─vctrs::vec_as_names(names, repair = "check_unique")
     10. │             └─vctrs (local) `<fn>`()
     11. │               └─vctrs:::validate_unique(names = names, arg = arg, call = call)
     12. │                 └─vctrs:::validate_minimal_names(names, n)
     13. └─rlang::abort(message = message, call = call)
    Execution halted
    ```

# TreatmentPatterns

<details>

* Version: 2.7.0
* GitHub: https://github.com/darwin-eu/TreatmentPatterns
* Source code: https://github.com/cran/TreatmentPatterns
* Date/Publication: 2024-11-27 11:50:02 UTC
* Number of recursive dependencies: 148

Run `revdepcheck::cloud_details(, "TreatmentPatterns")` for more info

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
       28. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       29. ├─output$charAgePlot
       30. ├─shiny:::`$.shinyoutput`(output, charAgePlot)
       31. │ └─.subset2(x, "impl")$getOutput(name)
       32. │   └─base::stop(v$err)
       33. └─shiny (local) `<fn>`(`<sbscOOBE>`)
      
      [ FAIL 1 | WARN 39 | SKIP 23 | PASS 160 ]
      Error: Test failures
      Execution halted
    ```

# TreatmentSelection

<details>

* Version: 2.1.1
* GitHub: NA
* Source code: https://github.com/cran/TreatmentSelection
* Date/Publication: 2017-08-11 18:55:47 UTC
* Number of recursive dependencies: 30

Run `revdepcheck::cloud_details(, "TreatmentSelection")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘TreatmentSelection-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: calibrate.trtsel
    > ### Title: assess model calibration of a trtsel object
    > ### Aliases: calibrate.trtsel
    > 
    > ### ** Examples
    > 
    > 
    ...
    > ###########################
    > trtsel.Y1 <- trtsel(event ~ Y1*trt, 
    +                    treatment.name = "trt", 
    +                    data = tsdata, 
    +                    study.design = "RCT",
    +                    default.trt = "trt all")
    Error in is.element(tmpnames, names(data)) : 
      unused argument (names(data))
    Calls: trtsel
    Execution halted
    ```

*   checking whether package ‘TreatmentSelection’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(plot.type, ': unused argument (c("calibration", "risk.t0", "risk.t1", "treatment effect", NA, "none")) 
      Note: possible error in 'is.element(plot.type, ': unused argument (c("calibration", "risk.t0", "risk.t1", "treatment effect")) 
      Note: possible error in 'is.element(plot.type, ': unused argument ("calibration") 
      Note: possible error in 'is.element(plot.type, ': unused argument ("risk.t0") 
      Note: possible error in 'is.element(plot.type, ': unused argument ("risk.t1") 
      Note: possible error in 'is.element("treatment effect", ': unused argument (plot.type) 
      Note: possible error in 'is.element(ci, c("vertical", ': unused argument (c("vertical", "horizontal")) 
      Note: possible error in 'is.element(fix.ind[i], ': unused argument (c(2, 3)) 
      Note: possible error in 'is.element(plot.type, ': unused argument (c("risk", "treatment effect", "cdf", "selection impact")) 
    ...
      Note: possible error in 'is.element(unique(trt), ': unused argument (c(0, 1)) 
      Note: possible error in 'is.element(treatment.name, ': unused argument (all.vars(formula)) 
      Note: possible error in 'is.element(all.vars(formula), ': unused argument (c(event.name, treatment.name)) 
      Note: possible error in 'is.element(marker.names, ': unused argument (c("fittedrisk.t0", "fittedrisk.t1", "trt.effect", "rec.trt", "rec.notrt")) 
      Note: possible error in 'is.element(default.trt, ': unused argument (c("trt all", "trt none")) 
      Note: possible error in 'is.element(unique(event), ': unused argument (c(0, 1)) 
    See ‘/tmp/workdir/TreatmentSelection/new/TreatmentSelection.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    calibrate.trtsel: possible error in is.element(plot.type,
      c("calibration", "risk.t0", "risk.t1", "treatment effect", NA,
      "none")): unused argument (c("calibration", "risk.t0", "risk.t1",
      "treatment effect", NA, "none"))
    calibrate.trtsel: possible error in is.element(plot.type,
      c("calibration", "risk.t0", "risk.t1", "treatment effect")): unused
      argument (c("calibration", "risk.t0", "risk.t1", "treatment effect"))
    calibrate.trtsel: possible error in is.element(plot.type,
      "calibration"): unused argument ("calibration")
    calibrate.trtsel: possible error in is.element(plot.type, "risk.t0"):
    ...
      "rec.trt", "rec.notrt"))
    trtsel: possible error in is.element(default.trt, c("trt all", "trt
      none")): unused argument (c("trt all", "trt none"))
    trtsel.boot: possible error in is.element(all.vars(formula),
      c(event.name, treatment.name)): unused argument (c(event.name,
      treatment.name))
    trtsel_measures: possible error in is.element(unique(trt), c(0, 1)):
      unused argument (c(0, 1))
    trtsel_measures: possible error in is.element(unique(event), c(0, 1)):
      unused argument (c(0, 1))
    ```

# trelliscopejs

<details>

* Version: 0.2.6
* GitHub: https://github.com/hafen/trelliscopejs
* Source code: https://github.com/cran/trelliscopejs
* Date/Publication: 2021-02-01 08:00:02 UTC
* Number of recursive dependencies: 105

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
* Number of recursive dependencies: 107

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
    ...
      3.   ├─ggtern::ggplot_build(x)
      4.   └─ggtern:::ggplot_build.ggplot(x)
      5.     └─ggtern:::layers_add_or_remove_mask(plot)
      6.       └─ggint$plot_theme(plot)
      7.         └─ggplot2:::validate_theme(theme)
      8.           └─base::mapply(...)
      9.             └─ggplot2 (local) `<fn>`(...)
     10.               └─cli::cli_abort(...)
     11.                 └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘choropleth_maps_with_tricolore.Rmd’
      ...
    
    > plot_educ <- ggplot(euro_example) + geom_sf(aes(fill = rgb, 
    +     geometry = geometry), size = 0.1) + scale_fill_identity()
    
    > plot_educ
    
      When sourcing ‘choropleth_maps_with_tricolore.R’:
    Error: The `tern.axis.ticks.length.major` theme element must be a <unit>
    object.
    Execution halted
    
      ‘choropleth_maps_with_tricolore.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘choropleth_maps_with_tricolore.Rmd’ using rmarkdown
    
    Quitting from lines 61-72 [unnamed-chunk-4] (choropleth_maps_with_tricolore.Rmd)
    Error: processing vignette 'choropleth_maps_with_tricolore.Rmd' failed with diagnostics:
    The `tern.axis.ticks.length.major` theme element must be a <unit>
    object.
    --- failed re-building ‘choropleth_maps_with_tricolore.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘choropleth_maps_with_tricolore.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

# tsnet

<details>

* Version: 0.1.0
* GitHub: https://github.com/bsiepe/tsnet
* Source code: https://github.com/cran/tsnet
* Date/Publication: 2024-02-28 11:30:02 UTC
* Number of recursive dependencies: 76

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
        unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(7, 0, 0, 0), NULL, FALSE), list(NULL, NULL, NULL, NULL, NULL, 
            0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 7, 0, 0), NULL, FALSE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "#4D4D4DFF", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, 
            NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 2.2), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0.5, NULL, NULL, NULL, c(0, 2.2, 0, 2.2), NULL, TRUE), list("#B3B3B3FF", 0.5, NULL, NULL, FALSE, "#B3B3B3FF", TRUE), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.5, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(), list("gray70", 0.5, 
            NULL, NULL, FALSE, "gray70", FALSE), NULL, NULL, list("gray70", 0.5, NULL, NULL, FALSE, "gray70", FALSE), NULL, NULL, NULL, NULL, list(NULL, NA, NULL, NULL, TRUE), NULL, 2, NULL, NULL, NULL, 1.2, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.2, NULL, list(NULL, NULL, NULL, 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0, list(), 
            2, list("white", NA, NULL, NULL, TRUE), list(), NULL, NULL, NULL, list("#DEDEDEFF", NULL, NULL, NULL, FALSE, "#DEDEDEFF", TRUE), list(), list(), NULL, NULL, NULL, NULL, FALSE, list(NULL, "white", NULL, NULL, TRUE), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(5.5, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, 
                NULL, NULL, NULL, NULL, TRUE), "topleft", NULL, NULL, list("gray90", NA, NULL, NULL, FALSE), NULL, NULL, "on", "inside", list(NULL, NULL, "black", 0.8, NULL, NULL, NULL, NULL, c(6, 6, 6, 6), NULL, FALSE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75))
      
      [ FAIL 1 | WARN 14 | SKIP 0 | PASS 107 ]
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

# UCSCXenaShiny

<details>

* Version: 2.1.0
* GitHub: https://github.com/openbiox/UCSCXenaShiny
* Source code: https://github.com/cran/UCSCXenaShiny
* Date/Publication: 2024-05-15 14:10:06 UTC
* Number of recursive dependencies: 209

Run `revdepcheck::cloud_details(, "UCSCXenaShiny")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘api.Rmd’
      ...
    ℹ The deprecated feature was likely used in the UCSCXenaShiny package.
      Please report the issue at
      <https://github.com/openbiox/UCSCXenaShiny/issues>.
    
      When sourcing ‘api.R’:
    Error: Problem while converting geom to grob.
    ℹ Error occurred in the 1st layer.
    Caused by error in `get()`:
    ! object 'interleave' not found
    Execution halted
    
      ‘api.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘api.Rmd’ using rmarkdown
    
    Quitting from lines 87-88 [unnamed-chunk-9] (api.Rmd)
    Error: processing vignette 'api.Rmd' failed with diagnostics:
    Problem while converting geom to grob.
    ℹ Error occurred in the 1st layer.
    Caused by error in `get()`:
    ! object 'interleave' not found
    --- failed re-building ‘api.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘api.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.1Mb
      sub-directories of 1Mb or more:
        data       2.0Mb
        doc        1.4Mb
        shinyapp   5.1Mb
    ```

# umiAnalyzer

<details>

* Version: 1.0.0
* GitHub: https://github.com/sfilges/umiAnalyzer
* Source code: https://github.com/cran/umiAnalyzer
* Date/Publication: 2021-11-25 08:40:02 UTC
* Number of recursive dependencies: 115

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
    > simsen <- filterUmiObject(simsen)
    > 
    > amplicon_plot <- AmpliconPlot(simsen)
    Warning: `aes_()` was deprecated in ggplot2 3.0.0.
    ℹ Please use tidy evaluation idioms with `aes()`
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
* Number of recursive dependencies: 90

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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘usmap1.Rmd’
      ...
    
    > knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
    
    > usmap::plot_usmap()
    
      When sourcing ‘usmap1.R’:
    Error: package tibble not available: install first?
    ...
    
    > usmap::plot_usmap("states", labels = TRUE)
    
      When sourcing ‘usmap3.R’:
    Error: package tibble not available: install first?
    Execution halted
    
      ‘usmap1.Rmd’ using ‘UTF-8’... failed
      ‘usmap2.Rmd’ using ‘UTF-8’... failed
      ‘usmap3.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘usmap1.Rmd’ using rmarkdown
    
    Quitting from lines 26-27 [unnamed-chunk-1] (usmap1.Rmd)
    Error: processing vignette 'usmap1.Rmd' failed with diagnostics:
    package tibble not available: install first?
    --- failed re-building ‘usmap1.Rmd’
    
    --- re-building ‘usmap2.Rmd’ using rmarkdown
    
    ...
    Quitting from lines 26-27 [unnamed-chunk-1] (usmap3.Rmd)
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

# vaccineff

<details>

* Version: 1.0.0
* GitHub: https://github.com/epiverse-trace/vaccineff
* Source code: https://github.com/cran/vaccineff
* Date/Publication: 2024-11-29 09:30:02 UTC
* Number of recursive dependencies: 77

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
* Number of recursive dependencies: 149

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

# visOmopResults

<details>

* Version: 1.0.0
* GitHub: https://github.com/darwin-eu/visOmopResults
* Source code: https://github.com/cran/visOmopResults
* Date/Publication: 2025-01-15 19:40:01 UTC
* Number of recursive dependencies: 127

Run `revdepcheck::cloud_details(, "visOmopResults")` for more info

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
      ── Failure ('test-plot.R:106:3'): Function returns a ggplot object ─────────────
      p_bar$labels$label1 == "cohort_name" is not TRUE
      
      `actual`:       
      `expected`: TRUE
      
      [ FAIL 5 | WARN 32 | SKIP 1 | PASS 414 ]
      Error: Test failures
      Execution halted
    ```

# visvow

<details>

* Version: 1.3.11
* GitHub: NA
* Source code: https://github.com/cran/visvow
* Date/Publication: 2024-01-31 16:10:02 UTC
* Number of recursive dependencies: 127

Run `revdepcheck::cloud_details(, "visvow")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(visvow)
      > 
      > test_check("visvow")
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 2288 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test.R:61:1'): (code run outside of `test_that()`) ──────────────────
      Error in `is.element(as.character(i), replyTimesN)`: unused argument (replyTimesN)
      Backtrace:
          ▆
       1. └─visvow:::vowelLong1(vowelTab, replyTimesN) at test.R:61:1
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 2288 ]
      Error: Test failures
      Execution halted
    ```

*   checking whether package ‘visvow’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(tolower(varName), ': unused argument (tolower(colnames(vowelFile()))) 
      Note: possible error in 'is.element(vowelTab()$vowel, ': unused argument (vowelExcl()) 
      Note: possible error in 'is.element(i, xi)': unused argument (xi) 
      Note: possible error in 'is.element("average", ': unused argument (input$selGeon0) 
      Note: possible error in 'is.element(vT$indexPlot, ': unused argument (input$catPlot0) 
      Note: possible error in 'is.element(vT$indexLine, ': unused argument (input$catLine0) 
      Note: possible error in 'is.element("smooth", ': unused argument (input$selGeon0) 
      Note: possible error in 'is.element("points", ': unused argument (input$selGeon0) 
      Note: possible error in 'is.element(Var[j], input$replyVar4)': unused argument (input$replyVar4) 
    ...
      Note: possible error in 'is.element("inv. X", ': unused argument (input$mdsGeon3) 
      Note: possible error in 'is.element("inv. Y", ': unused argument (input$mdsGeon3) 
      Note: possible error in 'is.element("points", ': unused argument (input$mdsGeon3) 
      Note: possible error in 'is.element("labels", ': unused argument (input$mdsGeon3) 
      Note: possible error in 'is.element(Normal[j], ': unused argument (allScalesAllowed) 
      Note: possible error in 'is.element(as.character(i), ': unused argument (replyTimesN) 
    See ‘/tmp/workdir/visvow/new/visvow.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    visvow : server : checkVar: possible error in
      is.element(tolower(varName), tolower(colnames(vowelFile()))): unused
      argument (tolower(colnames(vowelFile())))
    visvow : server: possible error in is.element(vowelTab()$vowel,
      vowelExcl()): unused argument (vowelExcl())
    visvow : server: possible error in is.element(i, xi): unused argument
      (xi)
    visvow : server: possible error in is.element("average",
      input$selGeon0): unused argument (input$selGeon0)
    visvow : server: possible error in is.element(vT$indexPlot,
    ...
    visvow : server : plotMult: possible error in is.element("inv. Y",
      input$mdsGeon3): unused argument (input$mdsGeon3)
    visvow : server : plotMult: possible error in is.element("points",
      input$mdsGeon3): unused argument (input$mdsGeon3)
    visvow : server : plotMult: possible error in is.element("labels",
      input$mdsGeon3): unused argument (input$mdsGeon3)
    visvow : server: possible error in is.element(Normal[j],
      allScalesAllowed): unused argument (allScalesAllowed)
    vowelLong1: possible error in is.element(as.character(i), replyTimesN):
      unused argument (replyTimesN)
    ```

# vivaldi

<details>

* Version: 1.0.1
* GitHub: https://github.com/GreshamLab/vivaldi
* Source code: https://github.com/cran/vivaldi
* Date/Publication: 2023-03-21 20:10:02 UTC
* Number of recursive dependencies: 101

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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘vignette.Rmd’
      ...
    |a_3_fb |        96|
    |a_3_iv |        94|
    |b_1_fb |        82|
    |b_1_iv |        91|
    
    > snv_location(DF_filt_SNVs)
    
      When sourcing ‘vignette.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
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
* Number of recursive dependencies: 132

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

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘a_Introduction.Rmd’ using rmarkdown
    
    Quitting from lines 44-58 [show points] (a_Introduction.Rmd)
    Error: processing vignette 'a_Introduction.Rmd' failed with diagnostics:
    `expand` must be a logical vector, not the number 0.05.
    --- failed re-building ‘a_Introduction.Rmd’
    
    --- re-building ‘b_RasterProcessing.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘a_Introduction.Rmd’
      ...
    +     returnclass = "sf")[1]
    
    > pointMap(occs = occurrences, ptCol = "orange", landCol = "black", 
    +     spName = "Steindachneria argentea", ptSize = 3, land = land)
    Using decimalLongitude and decimalLatitude
     as x and y coordinates, respectively.
    
    ...
    
      When sourcing ‘e_GLMWorkflow.R’:
    Error: invalid font type
    Execution halted
    
      ‘a_Introduction.Rmd’ using ‘UTF-8’... failed
      ‘b_RasterProcessing.Rmd’ using ‘UTF-8’... OK
      ‘c_DataSampling.Rmd’ using ‘UTF-8’... failed
      ‘d_Visualization.Rmd’ using ‘UTF-8’... failed
      ‘e_GLMWorkflow.Rmd’ using ‘UTF-8’... failed
    ```

# vvshiny

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/vvshiny
* Date/Publication: 2023-07-19 15:30:02 UTC
* Number of recursive dependencies: 136

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
      
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 60 ]
      Error: Test failures
      Execution halted
    ```

# VWPre

<details>

* Version: 1.2.4
* GitHub: NA
* Source code: https://github.com/cran/VWPre
* Date/Publication: 2020-11-29 17:10:02 UTC
* Number of recursive dependencies: 69

Run `revdepcheck::cloud_details(, "VWPre")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘VWPre_Plotting.Rmd’
      ...
    
    > gridExtra::grid.arrange(plot_avg(data = dat, type = "proportion", 
    +     xlim = c(0, 1000), IAColumns = c(IA_1_P = "Target", IA_2_P = "Rhyme", 
    +    .... [TRUNCATED] 
    Grand average calculated using Event means.
    Grand average calculated using Event means.
    
      When sourcing ‘VWPre_Plotting.R’:
    Error: invalid line type
    Execution halted
    
      ‘VWPre_Basic_Preprocessing.Rmd’ using ‘UTF-8’... OK
      ‘VWPre_Interest_Areas.Rmd’ using ‘UTF-8’... OK
      ‘VWPre_Message_Alignment.Rmd’ using ‘UTF-8’... OK
      ‘VWPre_Plotting.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘VWPre_Basic_Preprocessing.Rmd’ using rmarkdown
    --- finished re-building ‘VWPre_Basic_Preprocessing.Rmd’
    
    --- re-building ‘VWPre_Interest_Areas.Rmd’ using rmarkdown
    --- finished re-building ‘VWPre_Interest_Areas.Rmd’
    
    --- re-building ‘VWPre_Message_Alignment.Rmd’ using rmarkdown
    --- finished re-building ‘VWPre_Message_Alignment.Rmd’
    
    --- re-building ‘VWPre_Plotting.Rmd’ using rmarkdown
    ```

# WebAnalytics

<details>

* Version: 0.9.12
* GitHub: https://github.com/gregfrog/WebAnalytics
* Source code: https://github.com/cran/WebAnalytics
* Date/Publication: 2023-10-04 12:00:02 UTC
* Number of recursive dependencies: 67

Run `revdepcheck::cloud_details(, "WebAnalytics")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘WebAnalytics-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: configVariablesLoad
    > ### Title: Read a configuration file and store the variables
    > ### Aliases: configVariablesLoad configVariableGet configVariableIs
    > ###   configVariablesAll configVariableSet sample.config
    > ### Keywords: manip
    > 
    > ### ** Examples
    ...
    [1] "file to be copied  webanalytics.cls"
    [1] "file to be copied  sampleRfile.R"
    [1] "file to be copied  sample.config"
    [1] TRUE
    > ## End(Don't show)
    > configVariablesLoad(fileName=paste0(tempdir(),"/xx/sample.config"))
    Error in is.element("config.generateGraphForTimeOver", loadedNames) : 
      unused argument (loadedNames)
    Calls: configVariablesLoad
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘test-configVariables.R’
    Running the tests in ‘tests/test-configVariables.R’ failed.
    Complete output:
      > library(testthat)
      > library(WebAnalytics)
      Loading required package: ggplot2
      
      Attaching package: 'ggplot2'
      
      The following object is masked from 'package:base':
    ...
      
      Error:
      ! Test failed
      Backtrace:
          ▆
       1. ├─testthat::test_that(...)
       2. │ └─withr (local) `<fn>`()
       3. └─reporter$stop_if_needed()
       4.   └─rlang::abort("Test failed", call = NULL)
      Execution halted
    ```

*   checking whether package ‘WebAnalytics’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element("config.generateGraphForTimeOver", ': unused argument (loadedNames) 
      Note: possible error in 'is.element("config.generateServerSessionStats", ': unused argument (loadedNames) 
      Note: possible error in 'is.element("config.generatePercentileRankings", ': unused argument (loadedNames) 
      Note: possible error in 'is.element("config.readBaseline", ': unused argument (loadedNames) 
      Note: possible error in 'is.element("config.generateTransactionDetails", ': unused argument (loadedNames) 
      Note: possible error in 'is.element("config.generateDiagnosticPlots", ': unused argument (loadedNames) 
      Note: possible error in 'is.element("config.useragent.maximumPercentile", ': unused argument (loadedNames) 
      Note: possible error in 'is.element("config.useragent.generateFrequencies", ': unused argument (loadedNames) 
      Note: possible error in 'is.element("config.useragent.discardOther", ': unused argument (loadedNames) 
    ...
      Note: possible error in 'is.element("ApacheTimestamp", ': unused argument (columnList) 
      Note: possible error in 'is.element("MSTimestamp", ': unused argument (columnList) 
      Note: possible error in 'is.element("elapsedms", ': unused argument (columnList) 
      Note: possible error in 'is.element("elapseds", ': unused argument (columnList) 
      Note: possible error in 'is.element("elapsedus", ': unused argument (columnList) 
      Note: possible error in 'is.element("jsessionid", ': unused argument (columnList) 
    See ‘/tmp/workdir/WebAnalytics/new/WebAnalytics.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    configVariablesLoad: possible error in
      is.element("config.generateGraphForTimeOver", loadedNames): unused
      argument (loadedNames)
    configVariablesLoad: possible error in
      is.element("config.generateServerSessionStats", loadedNames): unused
      argument (loadedNames)
    configVariablesLoad: possible error in
      is.element("config.generatePercentileRankings", loadedNames): unused
      argument (loadedNames)
    configVariablesLoad: possible error in
    ...
    logFileRead: possible error in is.element("MSTimestamp", columnList):
      unused argument (columnList)
    logFileRead: possible error in is.element("elapsedms", columnList):
      unused argument (columnList)
    logFileRead: possible error in is.element("elapseds", columnList):
      unused argument (columnList)
    logFileRead: possible error in is.element("elapsedus", columnList):
      unused argument (columnList)
    logFileRead: possible error in is.element("jsessionid", columnList):
      unused argument (columnList)
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘performance.Rnw’ using Sweave
    Error: processing vignette 'performance.Rnw' failed with diagnostics:
    Running 'texi2dvi' on 'performance.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `pdfpages.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    ...
    l.7 \usepackage
                   {Sweave}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘performance.Rnw’
    
    SUMMARY: processing the following file failed:
      ‘performance.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# WhatsR

<details>

* Version: 1.0.4
* GitHub: NA
* Source code: https://github.com/cran/WhatsR
* Date/Publication: 2024-01-29 22:50:02 UTC
* Number of recursive dependencies: 137

Run `revdepcheck::cloud_details(, "WhatsR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘WhatsR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_emoji
    > ### Title: Plotting emoji distributions in 'WhatsApp' chat logs
    > ### Aliases: plot_emoji
    > 
    > ### ** Examples
    > 
    > # importing data
    ...
    > data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
    > 
    > # opening AGG graphics device from the ragg package (replace tempfile with filepath)
    > ragg::agg_png(tempfile(), width = 800, height = 600, res = 150)
    > 
    > # plotting emoji
    > plot_emoji(data,font_family="Times", exclude_sm = TRUE) #font_family = "Noto Color Emoji" on Linux
    Error in is.element(data$Sender, names) : unused argument (names)
    Calls: plot_emoji -> [ -> [.data.frame
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(WhatsR)
      > 
      > test_check("WhatsR")
      Loading required package: ragg
      [ FAIL 14 | WARN 0 | SKIP 0 | PASS 100 ]
      
    ...
      Error in `is.element(data$Sender, names)`: unused argument (names)
      Backtrace:
          ▆
       1. └─WhatsR::plot_lexical_dispersion(...) at test-WhatsR-tests.R:2051:3
       2.   ├─...[]
       3.   └─base::`[.data.frame`(...)
      
      [ FAIL 14 | WARN 0 | SKIP 0 | PASS 100 ]
      Error: Test failures
      Execution halted
    ```

*   checking whether package ‘WhatsR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'is.element(ParsedChat$Sender, ': unused argument (consentintg_ppts) 
      Note: possible error in 'is.element(data$Sender, ': unused argument (names) 
      Note: possible error in 'is.element(NewFrame$NewEmoji, ': unused argument (emoji_vec) 
      Note: possible error in 'is.element(NewFrame$NewUrls, ': unused argument (link_vec) 
      Note: possible error in 'is.element(NewFrame$NewMedia, ': unused argument (media_vec) 
      Note: possible error in 'is.element(NewFrame$NewSmilies, ': unused argument (smilie_vec) 
      Note: possible error in 'is.element(data$Sender, ': unused argument (string) 
    See ‘/tmp/workdir/WhatsR/new/WhatsR.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    parse_chat: possible error in is.element(ParsedChat$Sender,
      consentintg_ppts): unused argument (consentintg_ppts)
    plot_emoji: possible error in is.element(data$Sender, names): unused
      argument (names)
    plot_emoji: possible error in is.element(NewFrame$NewEmoji, emoji_vec):
      unused argument (emoji_vec)
    plot_lexical_dispersion: possible error in is.element(data$Sender,
      names): unused argument (names)
    plot_links: possible error in is.element(data$Sender, names): unused
      argument (names)
    ...
    plot_tokens: possible error in is.element(data$Sender, names): unused
      argument (names)
    plot_tokens_over_time: possible error in is.element(data$Sender,
      names): unused argument (names)
    plot_wordcloud: possible error in is.element(data$Sender, names):
      unused argument (names)
    summarize_tokens_per_person : SumFunc: possible error in
      is.element(data$Sender, string): unused argument (string)
    tailor_chat: possible error in is.element(data$Sender, names): unused
      argument (names)
    ```

# wilson

<details>

* Version: 2.4.2
* GitHub: https://github.com/loosolab/wilson
* Source code: https://github.com/cran/wilson
* Date/Publication: 2021-04-19 09:40:02 UTC
* Number of recursive dependencies: 205

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

# wordmap

<details>

* Version: 0.9.2
* GitHub: https://github.com/koheiw/wordmap
* Source code: https://github.com/cran/wordmap
* Date/Publication: 2025-01-07 22:10:02 UTC
* Number of recursive dependencies: 61

Run `revdepcheck::cloud_details(, "wordmap")` for more info

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
      > test_check("wordmap")
      Loading required package: wordmap
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 115 ]
      
    ...
      `print(textplot_terms(wmp1, data_dictionary_LSD2015, max_highlighted = 10))` produced warnings.
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 115 ]
      Error: Test failures
      In addition: Warning messages:
      1: In .recacheSubclasses(def@className, def, env) :
        undefined subclass "ndiMatrix" of class "replValueSp"; definition not updated
      2: In .recacheSubclasses(def@className, def, env) :
        undefined subclass "pcorMatrix" of class "replValueSp"; definition not updated
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 154 marked UTF-8 strings
    ```

# wppExplorer

<details>

* Version: 2.3-4
* GitHub: NA
* Source code: https://github.com/cran/wppExplorer
* Date/Publication: 2020-04-09 09:10:02 UTC
* Number of recursive dependencies: 99

Run `revdepcheck::cloud_details(, "wppExplorer")` for more info

</details>

## Newly broken

*   checking whether package ‘wppExplorer’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/wppExplorer/new/wppExplorer.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘wppExplorer’ ...
** package ‘wppExplorer’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error in is.element(wpp.data.env$iso3166$uncode, wpp.data.env$popM$country_code) : 
  unused argument (wpp.data.env$popM$country_code)
Error: unable to load R code in package ‘wppExplorer’
Execution halted
ERROR: lazy loading failed for package ‘wppExplorer’
* removing ‘/tmp/workdir/wppExplorer/new/wppExplorer.Rcheck/wppExplorer’


```
### CRAN

```
* installing *source* package ‘wppExplorer’ ...
** package ‘wppExplorer’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (wppExplorer)


```
# xpose

<details>

* Version: 0.4.19
* GitHub: https://github.com/UUPharmacometrics/xpose
* Source code: https://github.com/cran/xpose
* Date/Publication: 2025-01-07 20:00:02 UTC
* Number of recursive dependencies: 108

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
      
      Attaching package: 'ggplot2'
      
      The following object is masked from 'package:base':
    ...
       25.                             │   └─rlang::list2(...)
       26.                             └─ggplot2::labeller(labels)
       27.                               └─base::lapply(...)
       28.                                 └─ggplot2 (local) FUN(X[[i]], ...)
       29.                                   └─ggplot2 (local) .default(labels[label])
       30.                                     └─ggplot2 (local) x(labels)
      
      [ FAIL 2 | WARN 0 | SKIP 7 | PASS 518 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘customize_plots.Rmd’
      ...
    > dv_vs_ipred(xpdb, facets = c("SEX", "MED1"), ncol = 2, 
    +     nrow = 1, page = 1)
    Using data from $prob no.1
    Filtering data by EVID == 0
    `geom_smooth()` using formula = 'y ~ x'
    `geom_smooth()` using formula = 'y ~ x'
    
    ...
      When sourcing ‘multiple_pages.R’:
    Error: could not find function "label_variable"
    Execution halted
    
      ‘access_xpdb_data.Rmd’ using ‘UTF-8’... OK
      ‘customize_plots.Rmd’ using ‘UTF-8’... failed
      ‘import_model_outputs.Rmd’ using ‘UTF-8’... OK
      ‘introduction.Rmd’ using ‘UTF-8’... OK
      ‘multiple_pages.Rmd’ using ‘UTF-8’... failed
      ‘vpc.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
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
* Number of recursive dependencies: 108

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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘a01-the-xp_xtra-object.Rmd’
      ...
     - Compartment amounts (a)               : A1, A2
     - Not attributed (na)                   : DOSE, SS, II, TAD, CPRED
    
    > eta_vs_contcov(w_unit_labs, etavar = ETA1, quiet = TRUE)
    `geom_smooth()` using formula = 'y ~ x'
    `geom_smooth()` using formula = 'y ~ x'
    
    ...
    
    > pheno_set %>% eta_waterfall(run3, run6, quiet = TRUE)
    
      When sourcing ‘a03-useful_plots.R’:
    Error: could not find function "label_variable"
    Execution halted
    
      ‘a01-the-xp_xtra-object.Rmd’ using ‘UTF-8’... failed
      ‘a02-xpose-sets.Rmd’ using ‘UTF-8’... OK
      ‘a03-useful_plots.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘a01-the-xp_xtra-object.Rmd’ using rmarkdown
    
    Quitting from lines 95-96 [plot_cont] (a01-the-xp_xtra-object.Rmd)
    Error: processing vignette 'a01-the-xp_xtra-object.Rmd' failed with diagnostics:
    could not find function "label_variable"
    --- failed re-building ‘a01-the-xp_xtra-object.Rmd’
    
    --- re-building ‘a02-xpose-sets.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.8Mb
      sub-directories of 1Mb or more:
        data   3.5Mb
        doc    2.4Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    xplot_pairs: no visible binding for global variable ‘other_fun’
    Undefined global functions or variables:
      other_fun
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

# yamlet

<details>

* Version: 1.2.0
* GitHub: https://github.com/bergsmat/yamlet
* Source code: https://github.com/cran/yamlet
* Date/Publication: 2024-11-26 08:00:02 UTC
* Number of recursive dependencies: 102

Run `revdepcheck::cloud_details(, "yamlet")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘yamlet-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggplot_add.ggplot_isometric
    > ### Title: Add Isometry to Plot Object
    > ### Aliases: ggplot_add.ggplot_isometric
    > ### Keywords: internal
    > 
    > ### ** Examples
    > 
    ...
    
        is.element
    
    
    ismtrc> data.frame(x = 1:5, y = 3:7) %>%
    ismtrc+ ggplot(aes(x, y)) + geom_point() + isometric()
    Error in ggplot_add.ggplot_isometric(object, p, objectname) : 
      "x" %in% names(plot$labels) is not TRUE
    Calls: example ... ggplot_add -> ggplot_add.ggplot_isometric -> stopifnot
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(yamlet)
      
      Attaching package: 'yamlet'
      
      The following object is masked from 'package:stats':
      
    ...
      ══ Skipped tests (2) ═══════════════════════════════════════════════════════════
      • empty test (2): 'test-yamlet.R:1346:1', 'test-yamlet.R:1351:1'
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-yamlet.R:843:1'): ggplot.resolved is stable ──────────────────
      `print(x %>% ggplot(map) + geom_point())` did not produce any warnings.
      
      [ FAIL 1 | WARN 2 | SKIP 2 | PASS 532 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘scripted-html.Rmd’ using rmarkdown
    ```

