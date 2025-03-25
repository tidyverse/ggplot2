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

# AdverseEvents

<details>

* Version: 0.0.4
* GitHub: https://github.com/dungtsa/AdverseEvents
* Source code: https://github.com/cran/AdverseEvents
* Date/Publication: 2024-11-20 22:50:02 UTC
* Number of recursive dependencies: 176

Run `revdepcheck::cloud_details(, "AdverseEvents")` for more info

</details>

## Newly broken

*   checking whether package ‘AdverseEvents’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘AdverseEvents’
    See ‘/tmp/workdir/AdverseEvents/new/AdverseEvents.Rcheck/00install.out’ for details.
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

* Version: 0.5.0
* GitHub: https://github.com/jensharbers/agricolaeplotr
* Source code: https://github.com/cran/agricolaeplotr
* Date/Publication: 2024-01-17 16:42:04 UTC
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

# AmpliconDuo

<details>

* Version: 1.1.1
* GitHub: NA
* Source code: https://github.com/cran/AmpliconDuo
* Date/Publication: 2020-05-25 22:20:02 UTC
* Number of recursive dependencies: 29

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
    > ## depending on the size if the data sets, may take some time
    > ampliconduoset <- ampliconduo(ampliconfreqs[,1:4], sample.names = site.f[1:2])
    ..> 
    > ## plot amplicon read numbers of sample A  vs. amplicon read numbers of sample B,
    > ## indicating amplicons with significant deviations in their occurence across samples
    > plotAmpliconduo.set(ampliconduoset, nrow = 3)
    Error in discrete_scale(aesthetics, palette = NULL, na.value = na.value,  : 
      unused argument (h.start = 0)
    Calls: plotAmpliconduo.set -> scale_colour_discrete
    Execution halted
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
    
    > ### Name: plot_partialAPCeffects
    > ### Title: Partial APC plots based on an estimated GAM model
    > ### Aliases: plot_partialAPCeffects
    > 
    > ### ** Examples
    > 
    > library(APCtools)
    ...
    This is mgcv 1.8-42. For overview type 'help("mgcv-package")'.
    > 
    > data(travel)
    > model <- gam(mainTrip_distance ~ te(age, period), data = travel)
    > 
    > plot_partialAPCeffects(model, dat = travel, variable = "age")
    Error in continuous_scale(aesthetics, palette = NULL, guide = guide, na.value = na.value,  : 
      unused arguments (low = "grey90", high = "grey10")
    Calls: plot_partialAPCeffects -> scale_color_continuous
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
      Error in `continuous_scale(aesthetics, palette = NULL, guide = guide, na.value = na.value, 
          ...)`: unused arguments (low = "grey90", high = "grey10")
      Backtrace:
          ▆
       1. └─APCtools::plot_partialAPCeffects(...) at test-plots_APCeffects.R:15:3
       2.   └─ggplot2::scale_color_continuous(...)
      
      [ FAIL 1 | WARN 58 | SKIP 0 | PASS 75 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘main_functionality.Rmd’
      ...
    > plot_jointMarginalAPCeffects(model_list = model_list, 
    +     dat = travel, vlines_list = list(cohort = c(1900, 1920, 1939, 
    +         1946, 1966, 19 .... [TRUNCATED] 
    
    > plot_partialAPCeffects(model = model_pure, dat = travel, 
    +     variable = "period")
    
      When sourcing ‘main_functionality.R’:
    Error: unused arguments (low = "grey90", high = "grey10")
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

# asmbPLS

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/asmbPLS
* Date/Publication: 2023-04-17 09:50:05 UTC
* Number of recursive dependencies: 93

Run `revdepcheck::cloud_details(, "asmbPLS")` for more info

</details>

## Newly broken

*   checking whether package ‘asmbPLS’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘asmbPLS’
    See ‘/tmp/workdir/asmbPLS/new/asmbPLS.Rcheck/00install.out’ for details.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 20.5Mb
      sub-directories of 1Mb or more:
        data   2.5Mb
        libs  16.9Mb
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
    > # Automatically generate interactive plot for results produced by `stats::prcomp`
    > p <- autoplotly(prcomp(iris[c(1, 2, 3, 4)]), data = iris,
    +                 colour = 'Species', label = TRUE, label.size = 3, frame = TRUE)
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
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 1 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
          ▆
       1. ├─autoplotly::autoplotly(...) at test_all.R:26:3
       2. └─autoplotly:::autoplotly.default(...)
       3.   ├─plotly::ggplotly(...)
       4.   └─plotly:::ggplotly.ggplot(...)
       5.     └─plotly::gg2list(...)
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 1 ]
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
      cannot open file '/tmp/RtmprfZN0t/namq_10_gdp_1_Data.csv': No such file or directory
    
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

# bdl

<details>

* Version: 1.0.5
* GitHub: https://github.com/statisticspoland/R_Package_to_API_BDL
* Source code: https://github.com/cran/bdl
* Date/Publication: 2023-02-24 15:00:02 UTC
* Number of recursive dependencies: 150

Run `revdepcheck::cloud_details(, "bdl")` for more info

</details>

## Newly broken

*   checking whether package ‘bdl’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘bdl’
    See ‘/tmp/workdir/bdl/new/bdl.Rcheck/00install.out’ for details.
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
    
    > ### Name: plot_beam_clin
    > ### Title: Plot BEAM Sets
    > ### Aliases: plot_beam_clin
    > 
    > ### ** Examples
    > 
    > data(beam_stats)
    ...
     20. │                         └─plot$guides$assemble(theme)
     21. │                           └─ggplot2 (local) assemble(..., self = self)
     22. │                             └─self$package_box(grobs[[i]], position, theme + adjust)
     23. │                               └─ggplot2 (local) package_box(...)
     24. │                                 └─theme$legend.box %||% direction
     25. └─base::.handleSimpleError(...)
     26.   └─purrr (local) h(simpleError(msg, call))
     27.     └─cli::cli_abort(...)
     28.       └─rlang::abort(...)
    Execution halted
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

# BioM2

<details>

* Version: 1.1.0
* GitHub: NA
* Source code: https://github.com/cran/BioM2
* Date/Publication: 2024-09-20 12:10:21 UTC
* Number of recursive dependencies: 272

Run `revdepcheck::cloud_details(, "BioM2")` for more info

</details>

## Newly broken

*   checking whether package ‘BioM2’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘BioM2’
    See ‘/tmp/workdir/BioM2/new/BioM2.Rcheck/00install.out’ for details.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 12.8Mb
      sub-directories of 1Mb or more:
        data  12.5Mb
    ```

# BioPred

<details>

* Version: 1.0.2
* GitHub: NA
* Source code: https://github.com/cran/BioPred
* Date/Publication: 2024-11-04 08:30:13 UTC
* Number of recursive dependencies: 122

Run `revdepcheck::cloud_details(, "BioPred")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘BioPred-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: subgrp_perf
    > ### Title: Subgroup Performance Evaluation for Prognostic Cases
    > ### Aliases: subgrp_perf
    > 
    > ### ** Examples
    > 
    > # Load a sample dataset
    ...
    Risk Group = Medium    13.2 (10.23 , NA)
    Risk Group = High   11.73 (5.66 , 15.84)
    
    $fig
    Warning: Ignoring unknown labels:
    • `fill = "Risk Group"`
    • `linetype = "1"`
    Error in theme + adjust : non-numeric argument to binary operator
    Calls: <Anonymous> ... <Anonymous> -> assemble -> <Anonymous> -> package_box -> %||%
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Tutorial.Rmd’
      ...
    </tbody>
    </table>
    > res$fig
    Warning: Ignoring unknown labels:
    • `fill = "treatment_categorical , biogroup"`
    • `linetype = "1"`
    
      When sourcing ‘Tutorial.R’:
    Error: non-numeric argument to binary operator
    Execution halted
    
      ‘Tutorial.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Tutorial.Rmd’ using rmarkdown
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

# bruneimap

<details>

* Version: 0.3.1
* GitHub: https://github.com/Bruneiverse/bruneimap
* Source code: https://github.com/cran/bruneimap
* Date/Publication: 2024-12-20 10:40:09 UTC
* Number of recursive dependencies: 116

Run `revdepcheck::cloud_details(, "bruneimap")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        data   6.5Mb
    ```

# bSi

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/bSi
* Date/Publication: 2024-01-24 15:52:57 UTC
* Number of recursive dependencies: 105

Run `revdepcheck::cloud_details(, "bSi")` for more info

</details>

## Newly broken

*   checking whether package ‘bSi’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘bSi’
    See ‘/tmp/workdir/bSi/new/bSi.Rcheck/00install.out’ for details.
    ```

# calendR

<details>

* Version: 1.2
* GitHub: NA
* Source code: https://github.com/cran/calendR
* Date/Publication: 2023-10-05 17:30:02 UTC
* Number of recursive dependencies: 49

Run `revdepcheck::cloud_details(, "calendR")` for more info

</details>

## Newly broken

*   checking whether package ‘calendR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggimage::theme_transparent’ by ‘ggplot2::theme_transparent’ when loading ‘calendR’
    See ‘/tmp/workdir/calendR/new/calendR.Rcheck/00install.out’ for details.
    ```

# calendRio

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/calendRio
* Date/Publication: 2024-12-21 10:10:02 UTC
* Number of recursive dependencies: 49

Run `revdepcheck::cloud_details(, "calendRio")` for more info

</details>

## Newly broken

*   checking whether package ‘calendRio’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggimage::theme_transparent’ by ‘ggplot2::theme_transparent’ when loading ‘calendRio’
    See ‘/tmp/workdir/calendRio/new/calendRio.Rcheck/00install.out’ for details.
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

# CHOIRBM

<details>

* Version: 0.0.2
* GitHub: https://github.com/emcramer/CHOIRBM
* Source code: https://github.com/cran/CHOIRBM
* Date/Publication: 2021-02-15 16:50:11 UTC
* Number of recursive dependencies: 64

Run `revdepcheck::cloud_details(, "CHOIRBM")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘CHOIRBM-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_female_choirbm
    > ### Title: Plot the male CHOIR Body Map
    > ### Aliases: plot_female_choirbm
    > 
    > ### ** Examples
    > 
    > cbm_df <- gen_example_data()
    > plot_female_choirbm(cbm_df, "value")
    Error in switch(direction, vertical = c("left", "top"), horizontal = c("center",  : 
      EXPR must be a length 1 vector
    Calls: <Anonymous> ... <Anonymous> -> assemble -> <Anonymous> -> package_box -> %||%
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘plot-one-patient.Rmd’
      ...
    3 103 Front     1
    4 104 Front     1
    5 105 Front     0
    6 106 Front     0
    
    > plot_male_choirbm(cbm_df, "value")
    
      When sourcing ‘plot-one-patient.R’:
    Error: EXPR must be a length 1 vector
    Execution halted
    
      ‘plot-one-patient.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘plot-one-patient.Rmd’ using rmarkdown
    
    Quitting from lines 36-37 [plot1] (plot-one-patient.Rmd)
    Error: processing vignette 'plot-one-patient.Rmd' failed with diagnostics:
    EXPR must be a length 1 vector
    --- failed re-building ‘plot-one-patient.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘plot-one-patient.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# choroplethr

<details>

* Version: 3.7.3
* GitHub: NA
* Source code: https://github.com/cran/choroplethr
* Date/Publication: 2024-03-02 00:52:36 UTC
* Number of recursive dependencies: 126

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
      The following regions were missing and are being set to NA: namibia, western sahara, taiwan, antarctica, kosovo
    > 
    > # demonstrate continuous scale
    > country_choropleth(df_pop_country, "2012 World Bank Populate Estimates", num_colors=1)
    Warning in self$bind() :
      The following regions were missing and are being set to NA: namibia, western sahara, taiwan, antarctica, kosovo
    Error in continuous_scale(aesthetics, palette = NULL, guide = guide, na.value = na.value,  : 
      unused arguments (low = "#eff3ff", high = "#084594")
    Calls: country_choropleth ... <Anonymous> -> <Anonymous> -> scale_fill_continuous
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
        adding: report_dependencies199d5ce3be09/ (stored 0%)
        adding: report_dependencies199d5ce3be09/file199d49e80eed.html (deflated 8%)
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

*   checking whether package ‘ClusROC’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘ClusROC’
    See ‘/tmp/workdir/ClusROC/new/ClusROC.Rcheck/00install.out’ for details.
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
    2  2  7.480014   0.29
    
    Available components:
     [1] "num_groups" "table"      "levels"     "cluster"    "centers"    "curves"     "method"     "data"       "algorithm"  "call"      
    
    > autoplot(res, groups_by_colour = FALSE, interactive = TRUE)
    
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

# clustEff

<details>

* Version: 0.3.1
* GitHub: NA
* Source code: https://github.com/cran/clustEff
* Date/Publication: 2024-01-23 08:52:55 UTC
* Number of recursive dependencies: 128

Run `revdepcheck::cloud_details(, "clustEff")` for more info

</details>

## Newly broken

*   checking whether package ‘clustEff’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘clustEff’
    See ‘/tmp/workdir/clustEff/new/clustEff.Rcheck/00install.out’ for details.
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

# coda4microbiome

<details>

* Version: 0.2.4
* GitHub: https://github.com/malucalle/coda4microbiome
* Source code: https://github.com/cran/coda4microbiome
* Date/Publication: 2024-07-17 15:50:02 UTC
* Number of recursive dependencies: 130

Run `revdepcheck::cloud_details(, "coda4microbiome")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘coda4microbiome-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_survcurves
    > ### Title: plot_survcurves
    > ### Aliases: plot_survcurves
    > 
    > ### ** Examples
    > 
    > 
    ...
    +                  time,
    +                  status,
    +                  strata.quantile = 0.5)
    Warning: Ignoring unknown labels:
    • `linetype = "1"`
    Warning: Ignoring unknown labels:
    • `linetype = "1"`
    Error in theme + adjust : non-numeric argument to binary operator
    Calls: <Anonymous> ... <Anonymous> -> assemble -> <Anonymous> -> package_box -> %||%
    Execution halted
    ```

*   checking whether package ‘coda4microbiome’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘coda4microbiome’
    See ‘/tmp/workdir/coda4microbiome/new/coda4microbiome.Rcheck/00install.out’ for details.
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

# cogmapr

<details>

* Version: 0.9.3
* GitHub: NA
* Source code: https://github.com/cran/cogmapr
* Date/Publication: 2022-01-04 15:40:07 UTC
* Number of recursive dependencies: 83

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
    > main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
    > my.project <- ProjectCMap(main_path, project_name)
    > 
    > df.scm <- data.ggCMap(my.project, edge.filter = "4")
    > ggCMap(df.scm)
    Error in discrete_scale(aesthetics, palette = NULL, na.value = na.value,  : 
      argument 4 matches multiple formal arguments
    Calls: ggCMap -> scale_colour_discrete
    Execution halted
    ```

# CohortConstructor

<details>

* Version: 0.3.4
* GitHub: https://github.com/OHDSI/CohortConstructor
* Source code: https://github.com/cran/CohortConstructor
* Date/Publication: 2025-01-17 14:40:08 UTC
* Number of recursive dependencies: 160

Run `revdepcheck::cloud_details(, "CohortConstructor")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘a09_combine_cohorts.Rmd’
      ...
    downloaded 4.9 MB
    
    Warning in utils::download.file(url = url, destfile = file.path(pathToData,  :
      downloaded length 0 != reported length 0
    Warning in utils::download.file(url = url, destfile = file.path(pathToData,  :
      URL 'https://example-data.ohdsi.dev/GiBleed.zip': status was 'Stream error in the HTTP/2 framing layer'
    
    ...
      ‘a02_cohort_table_requirements.Rmd’ using ‘UTF-8’... OK
      ‘a03_require_demographics.Rmd’ using ‘UTF-8’... OK
      ‘a04_require_intersections.Rmd’ using ‘UTF-8’... OK
      ‘a05_update_cohort_start_end.Rmd’ using ‘UTF-8’... OK
      ‘a06_concatanate_cohorts.Rmd’ using ‘UTF-8’... OK
      ‘a07_filter_cohorts.Rmd’ using ‘UTF-8’... OK
      ‘a08_split_cohorts.Rmd’ using ‘UTF-8’... OK
      ‘a09_combine_cohorts.Rmd’ using ‘UTF-8’... failed
      ‘a10_match_cohorts.Rmd’ using ‘UTF-8’... OK
      ‘a11_benchmark.Rmd’ using ‘UTF-8’... OK
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.1Mb
      sub-directories of 1Mb or more:
        doc   9.4Mb
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

# CompAREdesign

<details>

* Version: 2.3.1
* GitHub: NA
* Source code: https://github.com/cran/CompAREdesign
* Date/Publication: 2024-02-15 13:00:02 UTC
* Number of recursive dependencies: 79

Run `revdepcheck::cloud_details(, "CompAREdesign")` for more info

</details>

## Newly broken

*   checking whether package ‘CompAREdesign’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘CompAREdesign’
    See ‘/tmp/workdir/CompAREdesign/new/CompAREdesign.Rcheck/00install.out’ for details.
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

# coursekata

<details>

* Version: 0.18.1
* GitHub: https://github.com/coursekata/coursekata-r
* Source code: https://github.com/cran/coursekata
* Date/Publication: 2024-12-12 17:10:02 UTC
* Number of recursive dependencies: 118

Run `revdepcheck::cloud_details(, "coursekata")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘coursekata-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: theme_coursekata
    > ### Title: A simple theme built on top of 'ggplot2::theme_bw'
    > ### Aliases: theme_coursekata
    > 
    > ### ** Examples
    > 
    > gf_boxplot(Thumb ~ RaceEthnic, data = Fingers, fill = ~RaceEthnic)
    Error in unit(x, default.units) : 'x' and 'units' must have length > 0
    Calls: <Anonymous> ... grobTree -> gTree -> setChildren -> gList -> linesGrob -> unit
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(coursekata)
      Loading required package: dslabs
      Loading required package: fivethirtyeight
      Some larger datasets need to be installed separately, like senators and
      house_district_forecast. To install these, we recommend you install the
      fivethirtyeightdata package by running:
    ...
      • gf_model-visual/gf-violin-cond-mod-y-on-x-pred-on-color.svg
      • gf_model-visual/gf-violin-cond-mod-y-on-x-pred-on-facet.svg
      • gf_model-visual/gf-violin-cond-mod-y-on-y-pred-on-color.svg
      • gf_model-visual/gf-violin-cond-mod-y-on-y-pred-on-facet.svg
      • gf_model-visual/gf-violin-cond-mod-y-on-y.svg
      • gf_model-visual/gf-violin-horizontal-mull-mod-y-on-x.svg
      • gf_model-visual/gf-violin-mull-mod-y-on-x-2.svg
      • gf_model-visual/gf-violin-null-mod-y-on-y.svg
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

# Coxmos

<details>

* Version: 1.1.0
* GitHub: https://github.com/BiostatOmics/Coxmos
* Source code: https://github.com/cran/Coxmos
* Date/Publication: 2025-01-18 11:30:02 UTC
* Number of recursive dependencies: 200

Run `revdepcheck::cloud_details(, "Coxmos")` for more info

</details>

## Newly broken

*   checking Rd files ... WARNING
    ```
    prepare_Rd: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘survminer’
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

# cpsvote

<details>

* Version: 0.1.0
* GitHub: https://github.com/Reed-EVIC/cpsvote
* Source code: https://github.com/cran/cpsvote
* Date/Publication: 2020-11-05 16:00:02 UTC
* Number of recursive dependencies: 90

Run `revdepcheck::cloud_details(, "cpsvote")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘voting.Rmd’
      ...
    +          .... [TRUNCATED] 
    
    > library(usmap)
    
    > cps16 %>% as_survey_design(weights = turnout_weight) %>% 
    +     mutate(state = STATE) %>% group_by(state) %>% summarize(turnout = survey_mean(hurach .... [TRUNCATED] 
    
      When sourcing ‘voting.R’:
    Error: unused arguments (low = "white", high = "blue")
    Execution halted
    
      ‘add-variables.Rmd’ using ‘UTF-8’... OK
      ‘background.Rmd’ using ‘UTF-8’... OK
      ‘basics.Rmd’ using ‘UTF-8’... OK
      ‘voting.Rmd’ using ‘UTF-8’... failed
    ```

# CRABS

<details>

* Version: 1.2.0
* GitHub: https://github.com/afmagee/CRABS
* Source code: https://github.com/cran/CRABS
* Date/Publication: 2023-10-24 10:20:07 UTC
* Number of recursive dependencies: 63

Run `revdepcheck::cloud_details(, "CRABS")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘CRABS-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: congruent.models
    > ### Title: Create a set of congruent models
    > ### Aliases: congruent.models
    > 
    > ### ** Examples
    > 
    > 
    ...
    > 
    > model_set1
    A congruent set of piecewise-linear birth-death models
    Knots: 500 
    Delta-tau: 0.1304443 
    n_models:  4 
    Error in switch(direction, vertical = c("left", "top"), horizontal = c("center",  : 
      EXPR must be a length 1 vector
    Calls: <Anonymous> ... <Anonymous> -> assemble -> <Anonymous> -> package_box -> %||%
    Execution halted
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

# csa

<details>

* Version: 0.7.1
* GitHub: https://github.com/imarkonis/csa
* Source code: https://github.com/cran/csa
* Date/Publication: 2023-10-24 13:40:11 UTC
* Number of recursive dependencies: 101

Run `revdepcheck::cloud_details(, "csa")` for more info

</details>

## Newly broken

*   checking whether package ‘csa’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘csa’
    See ‘/tmp/workdir/csa/new/csa.Rcheck/00install.out’ for details.
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

# cylcop

<details>

* Version: 0.2.0
* GitHub: https://github.com/r-lib/devtools
* Source code: https://github.com/cran/cylcop
* Date/Publication: 2022-10-29 22:00:21 UTC
* Number of recursive dependencies: 97

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

# D2MCS

<details>

* Version: 1.0.1
* GitHub: https://github.com/drordas/D2MCS
* Source code: https://github.com/cran/D2MCS
* Date/Publication: 2022-08-23 11:40:02 UTC
* Number of recursive dependencies: 179

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
       2. │ └─testthat::quasi_label(enquo(expected), expected.label, arg = "expected")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. └─strategyNoReal$plot()
       5.   └─BinaryPlot$new()$plot(binary.summary)
       6.     └─super$plot(summary)
       7.       └─ggplot2::scale_color_continuous(...)
      
      [ FAIL 6 | WARN 5 | SKIP 15 | PASS 702 ]
      Error: Test failures
      Execution halted
    ```

# dafishr

<details>

* Version: 1.0.1
* GitHub: https://github.com/CBMC-GCMP/dafishr
* Source code: https://github.com/cran/dafishr
* Date/Publication: 2024-07-22 22:10:09 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::cloud_details(, "dafishr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘dafishr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: join_mpa_data
    > ### Title: Detect fishing vessel presence within Marine Protected Areas
    > ###   polygons in Mexico
    > ### Aliases: join_mpa_data
    > 
    > ### ** Examples
    > 
    ...
    +   theme(legend.position = "")
    Warning in fortify(data, ...) :
      Arguments in `...` must be used.
    ✖ Problematic argument:
    • col = "gray90"
    ℹ Did you misspell an argument name?
    Error in switch(direction, vertical = c("left", "top"), horizontal = c("center",  : 
      EXPR must be a length 1 vector
    Calls: <Anonymous> ... <Anonymous> -> assemble -> <Anonymous> -> package_box -> %||%
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.2Mb
      sub-directories of 1Mb or more:
        data   8.0Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2020 marked UTF-8 strings
    ```

# daiquiri

<details>

* Version: 1.1.1
* GitHub: https://github.com/ropensci/daiquiri
* Source code: https://github.com/cran/daiquiri
* Date/Publication: 2023-07-18 16:50:09 UTC
* Number of recursive dependencies: 107

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
      
      Quitting from lines 467-528 [daiquiri-individual-fields] (report_htmldoc.Rmd)
      
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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘daiquiri.Rmd’
      ...
    
    > daiq_obj <- daiquiri_report(df = example_prescriptions, 
    +     field_types = fts, override_column_names = FALSE, na = c("", 
    +         "NULL"), data .... [TRUNCATED] 
    
    Quitting from lines 467-528 [daiquiri-individual-fields] (report_htmldoc.Rmd)
    
      When sourcing ‘daiquiri.R’:
    Error: `limits` must be a <numeric> vector, not a <POSIXct> object.
    Execution halted
    
      ‘daiquiri.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘daiquiri.Rmd’ using rmarkdown
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
* Number of recursive dependencies: 89

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
    4 20000    Chemo     12502.981       FALSE
    5 20000    Radio      5406.398        TRUE
    6 20000  Surgery     10462.064       FALSE
    > 
    > # plot an expected loss curve (ELC)
    > plot(exp_loss)
    Error in discrete_scale(aesthetics, palette = NULL, na.value = na.value,  : 
      argument 5 matches multiple formal arguments
    Calls: plot ... plot.exp_loss -> add_common_aes -> scale_color_discrete
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
      Backtrace:
          ▆
       1. ├─base::plot(t, basecase = list(pFailChemo = 0.37, muDieCancer = 0.09)) at test_twsa.R:44:3
       2. └─dampack:::plot.twsa(t, basecase = list(pFailChemo = 0.37, muDieCancer = 0.09))
       3.   └─dampack:::add_common_aes(...)
       4.     └─ggplot2::scale_fill_discrete(...)
      
      [ FAIL 12 | WARN 0 | SKIP 1 | PASS 122 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘dsa_generation.Rmd’
      ...
    > l_owsa_det <- run_owsa_det(params_range = my_owsa_params_range, 
    +     params_basecase = my_params_basecase, nsamp = 100, FUN = simulate_strategies, .... [TRUNCATED] 
    
    > my_owsa_NMB <- l_owsa_det$owsa_NMB
    
    > plot(my_owsa_NMB, n_x_ticks = 3)
    
    ...
    
      When sourcing ‘psa_analysis.R’:
    Error: argument 5 matches multiple formal arguments
    Execution halted
    
      ‘basic_cea.Rmd’ using ‘UTF-8’... OK
      ‘dsa_generation.Rmd’ using ‘UTF-8’... failed
      ‘psa_analysis.Rmd’ using ‘UTF-8’... failed
      ‘psa_generation.Rmd’ using ‘UTF-8’... OK
      ‘voi.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
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

# DeductiveR

<details>

* Version: 1.1.0
* GitHub: NA
* Source code: https://github.com/cran/DeductiveR
* Date/Publication: 2024-12-17 14:20:01 UTC
* Number of recursive dependencies: 99

Run `revdepcheck::cloud_details(, "DeductiveR")` for more info

</details>

## Newly broken

*   checking whether package ‘DeductiveR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘DeductiveR’
    See ‘/tmp/workdir/DeductiveR/new/DeductiveR.Rcheck/00install.out’ for details.
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
    > library(ggplot2)
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

# DEGRE

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/DEGRE
* Date/Publication: 2022-11-02 09:32:57 UTC
* Number of recursive dependencies: 95

Run `revdepcheck::cloud_details(, "DEGRE")` for more info

</details>

## Newly broken

*   checking whether package ‘DEGRE’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘DEGRE’
    See ‘/tmp/workdir/DEGRE/new/DEGRE.Rcheck/00install.out’ for details.
    ```

# Dforest

<details>

* Version: 0.4.2
* GitHub: NA
* Source code: https://github.com/cran/Dforest
* Date/Publication: 2017-11-28 22:03:57 UTC
* Number of recursive dependencies: 29

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
    >   Train_X = X[-random_seq[split_sample[[1]]],]
    >   Train_Y = Y[-random_seq[split_sample[[1]]]]
    >   Test_X = X[random_seq[split_sample[[1]]],]
    >   Test_Y = Y[random_seq[split_sample[[1]]]]
    > 
    >   Result = DF_easy(Train_X, Train_Y, Test_X, Test_Y)
    Error in continuous_scale(aesthetics, palette = NULL, guide = guide, na.value = na.value,  : 
      unused arguments (low = "blue", high = "darkblue")
    Calls: DF_easy -> DF_ConfPlot -> scale_color_continuous
    Execution halted
    ```

# did

<details>

* Version: 2.1.2
* GitHub: https://github.com/bcallaway11/did
* Source code: https://github.com/cran/did
* Date/Publication: 2022-07-20 16:00:05 UTC
* Number of recursive dependencies: 155

Run `revdepcheck::cloud_details(, "did")` for more info

</details>

## Newly broken

*   checking whether package ‘did’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘did’
    See ‘/tmp/workdir/did/new/did.Rcheck/00install.out’ for details.
    ```

## In both

*   checking running R code from vignettes ... WARNING
    ```
    Errors in running code in vignettes:
    when running code in ‘TWFE.Rmd’
      ...
    
    > knitr::opts_chunk$set(collapse = TRUE, comment = "#>", 
    +     echo = TRUE, eval = FALSE)
    
    > library(tidyverse)
    
      When sourcing ‘TWFE.R’:
    ...
    
      When sourcing ‘pre-testing.R’:
    Error: cannot open the connection
    Execution halted
    
      ‘TWFE.Rmd’ using ‘UTF-8’... failed
      ‘did-basics.Rmd’ using ‘UTF-8’... OK
      ‘extensions.Rmd’ using ‘UTF-8’... failed
      ‘multi-period-did.Rmd’ using ‘UTF-8’... OK
      ‘pre-testing.Rmd’ using ‘UTF-8’... failed
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
      
      [ FAIL 2 | WARN 4 | SKIP 0 | PASS 2766 ]
      Error: Test failures
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
      > test_check("dittoViz")
      [ FAIL 29 | WARN 0 | SKIP 0 | PASS 252 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
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
     13. │       └─l$compute_geom_2(d, theme = plot$theme)
     14. │         └─ggplot2 (local) compute_geom_2(..., self = self)
     15. │           └─self$geom$use_defaults(...)
     16. │             └─ggplot2 (local) use_defaults(..., self = self)
     17. │               └─ggplot2:::check_aesthetics(new_params, nrow(data))
     18. │                 └─vctrs::list_sizes(x)
     19. └─vctrs:::stop_scalar_type(...)
     20.   └─vctrs:::stop_vctrs(...)
     21.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘divent.Rmd’
      ...
    4 subplot_4   1.56       798
    
    > autoplot(paracou_6_abd[1, ])
    
      When sourcing ‘divent.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `list_sizes()`:
    ! `x$shape` must be a vector, not a <quosure> object.
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
    Caused by error in `list_sizes()`:
    ! `x$shape` must be a vector, not a <quosure> object.
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

# drugsens

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/drugsens
* Date/Publication: 2025-01-16 15:10:02 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "drugsens")` for more info

</details>

## Newly broken

*   checking whether package ‘drugsens’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘drugsens’
    See ‘/tmp/workdir/drugsens/new/drugsens.Rcheck/00install.out’ for details.
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

# EbayesThresh

<details>

* Version: 1.4-12
* GitHub: https://github.com/stephenslab/EbayesThresh
* Source code: https://github.com/cran/EbayesThresh
* Date/Publication: 2017-08-08 04:02:13 UTC
* Number of recursive dependencies: 66

Run `revdepcheck::cloud_details(, "EbayesThresh")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ebayesthresh.Rmd’
      ...
    +     t(error_mat.mean_mse)), nzeros = rep(nzeros/nobs, 3 * 2), 
    +     method = rep(rep( .... [TRUNCATED] 
    
    > ggplot(data = data.error_plot) + facet_grid(. ~ error_post) + 
    +     geom_line(aes(x = nzeros, y = mse, col = method), size = 1) + 
    +     scale_colo .... [TRUNCATED] 
    
      When sourcing ‘ebayesthresh.R’:
    Error: unused argument (h = c(0, 270))
    Execution halted
    
      ‘ebayesthresh.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ebayesthresh.Rmd’ using rmarkdown
    ```

# ecolRxC

<details>

* Version: 0.1.1-10
* GitHub: NA
* Source code: https://github.com/cran/ecolRxC
* Date/Publication: 2023-03-31 07:50:02 UTC
* Number of recursive dependencies: 28

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
    +                          C3 = c(78L, 7L, 28L, 42L, 28L, 84L, 49L, 85L, 260L, 100L),
    +                          C4 = c(56L, 14L, 20L, 7L, 19L, 54L, 22L, 50L, 330L, 91L),
    +                          C5 = c(14L, 3L, 6L, 2L, 3L, 14L, 8L, 8L, 45L, 7L)),
    +                          row.names = c(NA, 10L), class = "data.frame")
    > example <- ecolRxC(votes1, votes2, method = "IPF")
    > p <- plot(example, show.plot = FALSE)
    Error in continuous_scale(aesthetics, palette = NULL, guide = guide, na.value = na.value,  : 
      unused arguments (high = "#008B8B80", low = "white")
    Calls: plot -> plot.ecolRxC -> <Anonymous>
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

# eiCircles

<details>

* Version: 0.0.1-7
* GitHub: NA
* Source code: https://github.com/cran/eiCircles
* Date/Publication: 2024-03-24 19:10:02 UTC
* Number of recursive dependencies: 30

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
    +                          C3 = c(78L, 7L, 28L, 42L, 28L, 84L, 49L, 85L, 260L, 100L),
    +                          C4 = c(56L, 14L, 20L, 7L, 19L, 54L, 22L, 50L, 330L, 91L),
    +                          C5 = c(14L, 3L, 6L, 2L, 3L, 14L, 8L, 8L, 45L, 7L)),
    +                          row.names = c(NA, 10L), class = "data.frame")
    > example <- BPF(votes1, votes2)
    > p <- plot(example, show.plot = FALSE)
    Error in continuous_scale(aesthetics, palette = NULL, guide = guide, na.value = na.value,  : 
      unused arguments (high = "#A2CD5A80", low = "white")
    Calls: plot -> plot.BPF -> <Anonymous>
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
     13. │       └─l$compute_geom_2(d, theme = plot$theme)
     14. │         └─ggplot2 (local) compute_geom_2(..., self = self)
     15. │           └─self$geom$use_defaults(...)
     16. │             └─ggplot2 (local) use_defaults(..., self = self)
     17. │               └─ggplot2:::check_aesthetics(new_params, nrow(data))
     18. │                 └─vctrs::list_sizes(x)
     19. └─vctrs:::stop_scalar_type(...)
     20.   └─vctrs:::stop_vctrs(...)
     21.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘entropart.Rmd’
      ...
    > Abd18 <- as.AbdVector(N18)
    
    > autoplot(Abd18, Distribution = "lnorm")
    
      When sourcing ‘entropart.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `list_sizes()`:
    ! `x$shape` must be a vector, not a <quosure> object.
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
    Caused by error in `list_sizes()`:
    ! `x$shape` must be a vector, not a <quosure> object.
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

# episensr

<details>

* Version: 1.3.0
* GitHub: https://github.com/dhaine/episensr
* Source code: https://github.com/cran/episensr
* Date/Publication: 2023-08-30 09:20:05 UTC
* Number of recursive dependencies: 105

Run `revdepcheck::cloud_details(, "episensr")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘d_other_sens.Rmd’
      ...
    Warning: A numeric `legend.position` argument in `theme()` was deprecated in ggplot2
    3.5.0.
    ℹ Please use the `legend.position.inside` argument of `theme()` instead.
    
      When sourcing ‘d_other_sens.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 3rd layer.
    Caused by error in `list_sizes()`:
    ! `x$label` must be a vector, not an expression vector.
    Execution halted
    
      ‘b_probabilistic.Rmd’ using ‘UTF-8’... OK
      ‘c_multiple_bias.Rmd’ using ‘UTF-8’... OK
      ‘d_other_sens.Rmd’ using ‘UTF-8’... failed
      ‘episensr.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘b_probabilistic.Rmd’ using rmarkdown
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

# errors

<details>

* Version: 0.4.3
* GitHub: https://github.com/r-quantities/errors
* Source code: https://github.com/cran/errors
* Date/Publication: 2025-01-18 18:10:05 UTC
* Number of recursive dependencies: 68

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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘rjournal.Rmd’
      ...
    
    > print(p, vp = vp1)
    Warning: In '<' : boolean operators not defined for 'errors' objects, uncertainty dropped
    
      When sourcing ‘rjournal.R’:
    Error: Problem while converting geom to grob.
    ℹ Error occurred in the 2nd layer.
    Caused by error in `draw_panel()`:
    ! unused argument (height = NULL)
    Execution halted
    
      ‘rjournal.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘rjournal.Rmd’ using rmarkdown
    
    Quitting from lines 253-272 [plot] (rjournal.Rmd)
    Error: processing vignette 'rjournal.Rmd' failed with diagnostics:
    Problem while converting geom to grob.
    ℹ Error occurred in the 2nd layer.
    Caused by error in `draw_panel()`:
    ! unused argument (height = NULL)
    --- failed re-building ‘rjournal.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘rjournal.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# eRTG3D

<details>

* Version: 0.7.0
* GitHub: https://github.com/munterfi/eRTG3D
* Source code: https://github.com/cran/eRTG3D
* Date/Publication: 2022-02-25 12:10:05 UTC
* Number of recursive dependencies: 114

Run `revdepcheck::cloud_details(, "eRTG3D")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(eRTG3D)
      > 
      > test_check("eRTG3D")
      [ FAIL 1 | WARN 3 | SKIP 0 | PASS 103 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
       18.                       └─base::serialize(data, node$con, xdr = FALSE)
      
      [ FAIL 1 | WARN 3 | SKIP 0 | PASS 103 ]
      Error: Test failures
      In addition: Warning messages:
      1: In for (i in seq_len(differences)) r <- r[i1] - r[-length(r):-(length(r) -  :
        closing unused connection 7 (<-localhost:11153)
      2: In for (i in seq_len(differences)) r <- r[i1] - r[-length(r):-(length(r) -  :
        closing unused connection 6 (<-localhost:11153)
      Execution halted
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
     9 Domestic mdl    2019 Dec sample[5000] 5335490.
    10 Domestic mdl    2020 Jan sample[5000] 4889716.
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
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 312 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test_plot_density.R:14:3'): Test plot_density ─────────────────────
      plt$labels$x not equal to "probability".
      target is NULL, current is character
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 312 ]
      Error: Test failures
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

# fgeo.plot

<details>

* Version: 1.1.11
* GitHub: https://github.com/forestgeo/fgeo.plot
* Source code: https://github.com/cran/fgeo.plot
* Date/Publication: 2022-09-03 18:30:02 UTC
* Number of recursive dependencies: 99

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
    > 
    > # Species and elevation (optional) ---------------------------------------
    > 
    > # Species and elevation
    > elevation <- fgeo.x::elevation
    > autoplot_by_species(sp_elev(census, elevation))
    Error in continuous_scale(aesthetics, palette = NULL, guide = guide, na.value = na.value,  : 
      unused arguments (low = "blue", high = "red")
    Calls: autoplot_by_species ... best_elev_legend -> add_elevation_contours -> scale_colour_continuous
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
        6. ├─fgeo.plot:::plot_elev(elevation, xlim = 0)
        7. │ └─fgeo.plot:::map_pure_elev(...)
        8. │   └─... %>% ...
        9. ├─fgeo.plot:::best_elev_legend(., hide_color_legend = hide_color_legend)
       10. └─fgeo.plot:::add_elevation_contours(...)
       11.   └─ggplot2::scale_colour_continuous(low = low, high = high)
      
      [ FAIL 6 | WARN 5 | SKIP 0 | PASS 114 ]
      Error: Test failures
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
        NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "#4D4D4DFF", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, c(0, 0, 4.95, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, c(4.95, 0, 0, 0), NULL, TRUE),
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

# forestPSD

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/forestPSD
* Date/Publication: 2024-11-11 16:50:05 UTC
* Number of recursive dependencies: 47

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
    Loading required package: gridExtra
    Warning: The `<scale>` argument of `guides()` cannot be `FALSE`. Use
    "none" instead as of ggplot2 3.3.4.
    Warning: Removed 2 rows containing missing values or values outside
    the scale range (`geom_path()`).
    Warning in data("kidney") : data set ‘kidney’ not found
    Warning in emfrail(Surv(time, status) ~ age + sex + cluster(id), data = kidney,  :
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

# func2vis

<details>

* Version: 1.0-3
* GitHub: NA
* Source code: https://github.com/cran/func2vis
* Date/Publication: 2023-03-16 17:30:02 UTC
* Number of recursive dependencies: 115

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
    > data("enriched_pathways")
    > revised_pathway <- clean_pathways(df_case_vs_ctrl=t.tests.treatment.sign,
    +                                   df_pathway = enriched_pathways)
    > p <- plot_pathways(revised_pathway)
    Error in continuous_scale(aesthetics, palette = NULL, guide = guide, na.value = na.value,  : 
      unused arguments (low = "blue", high = "red")
    Calls: plot_pathways -> scale_color_continuous
    Execution halted
    ```

# FuncNN

<details>

* Version: 1.0
* GitHub: https://github.com/b-thi/FuncNN
* Source code: https://github.com/cran/FuncNN
* Date/Publication: 2020-09-15 09:40:15 UTC
* Number of recursive dependencies: 166

Run `revdepcheck::cloud_details(, "FuncNN")` for more info

</details>

## Newly broken

*   checking whether package ‘FuncNN’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘FuncNN’
    See ‘/tmp/workdir/FuncNN/new/FuncNN.Rcheck/00install.out’ for details.
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘foreach’
      All declared Imports should be used.
    ```

# GCalignR

<details>

* Version: 1.0.7
* GitHub: https://github.com/mottensmann/GCalignR
* Source code: https://github.com/cran/GCalignR
* Date/Publication: 2024-07-03 18:00:01 UTC
* Number of recursive dependencies: 83

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
    >  ## aligned gc-dataset
    >  data("aligned_peak_data")
    >  ## Default settings: The final output is plotted
    >  gc_heatmap(aligned_peak_data, algorithm_step = "aligned")
    Error in continuous_scale(aesthetics, palette = NULL, guide = guide, na.value = na.value,  : 
      unused arguments (low = "blue", high = "red")
    Calls: gc_heatmap -> scale_fill_continuous
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
      Start: 2025-01-21 13:02:50
      
    ...
      Error in `continuous_scale(aesthetics, palette = NULL, guide = guide, na.value = na.value, 
          ...)`: unused arguments (low = "blue", high = "red")
      Backtrace:
          ▆
       1. └─GCalignR::gc_heatmap(x) at test-gc_heatmap.R:6:1
       2.   └─ggplot2::scale_fill_continuous(...)
      
      [ FAIL 1 | WARN 2 | SKIP 1 | PASS 32 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘GCalignR_step_by_step.Rmd’
      ...
    0.03 
    
    
    > data("aligned_peak_data")
    
    > gc_heatmap(aligned_peak_data, threshold = 0.03)
    
      When sourcing ‘GCalignR_step_by_step.R’:
    Error: unused arguments (low = "blue", high = "red")
    Execution halted
    
      ‘GCalignR_How_does_the_Algorithm_work.Rmd’ using ‘UTF-8’... OK
      ‘GCalignR_step_by_step.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘GCalignR_How_does_the_Algorithm_work.Rmd’ using rmarkdown
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
    > internet_2015 <- subset(internet, year == 2015)
    
    > geoheatmap(facet_data = internet_2015, grid_data = europe_countries_grid1, 
    +     facet_col = "country", value_col = "users", low = "#56B1F7", 
    +    .... [TRUNCATED] 
    Data contains facets that are not in the grid. Consider checking dataset.
    
      When sourcing ‘geoheatmap.R’:
    Error: unused arguments (low = "#56B1F7", high = "#132B43")
    Execution halted
    
      ‘geoheatmap.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘geoheatmap.Rmd’ using rmarkdown
    
    Quitting from lines 76-81 [unnamed-chunk-5] (geoheatmap.Rmd)
    Error: processing vignette 'geoheatmap.Rmd' failed with diagnostics:
    unused arguments (low = "#56B1F7", high = "#132B43")
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
        geom spacing margins panel.widths panel.heights
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

*   checking examples ... ERROR
    ```
    Running examples in ‘GGally-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggpairs
    > ### Title: ggplot2 generalized pairs plot
    > ### Aliases: ggpairs
    > ### Keywords: hplot
    > 
    > ### ** Examples
    > 
    ...
    > custom_car <- ggpairs(mtcars[, c("mpg", "wt", "cyl")], upper = "blank", title = "Custom Example")
    > # ggplot example taken from example(geom_text)
    > plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg, label = rownames(mtcars)))
    > plot <- plot +
    +   ggplot2::geom_text(ggplot2::aes(colour = factor(cyl)), size = 3) +
    +   ggplot2::scale_colour_discrete(l = 40)
    Error in discrete_scale(aesthetics, palette = NULL, na.value = na.value,  : 
      argument 4 matches multiple formal arguments
    Calls: <Anonymous>
    Execution halted
    ```

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
      
      [ FAIL 3 | WARN 5 | SKIP 26 | PASS 477 ]
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
      > 
      > test_check("gganimate")
      [ FAIL 2 | WARN 3 | SKIP 1 | PASS 4 ]
      
    ...
       22.                       └─self$extract_key(...)
       23.                         └─ggplot2 (local) extract_key(...)
       24.                           └─Guide$extract_key(scale, aesthetic, ...)
       25.                             └─ggplot2 (local) extract_key(...)
       26.                               └─scale$map(breaks)
       27.                                 └─ggplot2 (local) map(..., self = self)
      
      [ FAIL 2 | WARN 3 | SKIP 1 | PASS 4 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘gganimate.Rmd’
      ...
    Theme element `panel.grid.major.x` is missing
    Theme element `panel.background` is missing
    Warning: Failed to plot frame
    Caused by error in `UseMethod()`:
    ! no applicable method for 'element_grob' applied to an object of class "NULL"
    
      When sourcing ‘gganimate.R’:
    Error: Provided file (/tmp/Rtmp3dHdpP/20956cec4984/gganim_plot0001.png) does
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

* Version: 11.1.0
* GitHub: https://github.com/davidhodge931/ggblanket
* Source code: https://github.com/cran/ggblanket
* Date/Publication: 2024-12-17 22:40:02 UTC
* Number of recursive dependencies: 111

Run `revdepcheck::cloud_details(, "ggblanket")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggblanket-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gg_boxplot
    > ### Title: Boxplot ggplot
    > ### Aliases: gg_boxplot
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
    • `shape = "NULL"`
    • `size = "NULL"`
    • `linewidth = "NULL"`
    • `linetype = "NULL"`
    • `pattern = "NULL"`
    Warning: Removed 2 rows containing non-finite outside the scale range
    (`stat_boxplot()`).
    Error in unit(y, default.units) : 'x' and 'units' must have length > 0
    Calls: <Anonymous> ... grobTree -> gTree -> setChildren -> gList -> linesGrob -> unit
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

# ggenealogy

<details>

* Version: 1.0.3
* GitHub: NA
* Source code: https://github.com/cran/ggenealogy
* Date/Publication: 2024-02-21 16:00:02 UTC
* Number of recursive dependencies: 84

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
    > data(sbGeneal)
    > ig <- dfToIG(sbGeneal)
    > varieties <- c("Bedford", "Calland", "Narow", "Pella", "Tokyo", "Young", "Zane")
    > p <- plotDegMatrix(varieties, ig, sbGeneal)
    > p + ggplot2::scale_fill_continuous(low = "white", high = "darkgreen")
    Error in continuous_scale(aesthetics, palette = NULL, guide = guide, na.value = na.value,  : 
      unused arguments (low = "white", high = "darkgreen")
    Calls: <Anonymous>
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggenealogy.Rnw’
      ...
    
    > p = plotDegMatrix(varieties, ig, sbGeneal)
    
    > p + ggplot2::scale_fill_continuous(low = "white", 
    +     high = "darkgreen") + ggplot2::theme(legend.title = ggplot2::element_text(size = 15), 
    +    .... [TRUNCATED] 
    
      When sourcing ‘ggenealogy.R’:
    Error: unused arguments (low = "white", high = "darkgreen")
    Execution halted
    
      ‘ggenealogy.Rnw’ using ‘UTF-8’... failed
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2356 marked UTF-8 strings
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ggenealogy.Rnw’ using Sweave
    Warning: Removed 1 row containing missing values or values outside
    the scale range (`geom_segment()`).
    Warning: Removed 1 row containing missing values or values outside
    the scale range (`geom_segment()`).
    Warning: Removed 1 row containing missing values or values outside
    the scale range (`geom_segment()`).
    Warning: Removed 1 row containing missing values or values outside
    ...
    Error in continuous_scale(aesthetics, palette = NULL, guide = guide, na.value = na.value,  : 
      unused arguments (low = "white", high = "darkgreen")
    
    --- failed re-building ‘ggenealogy.Rnw’
    
    SUMMARY: processing the following file failed:
      ‘ggenealogy.Rnw’
    
    Error: Vignette re-building failed.
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
    
    > ### Name: ggInterval_scaMatrix
    > ### Title: scatter plot for all variable by interval data.
    > ### Aliases: ggInterval_scaMatrix
    > 
    > ### ** Examples
    > 
    > a<-rnorm(1000,0,5)
    ...
    > b<-runif(1000,-20,-10)
    > c<-rgamma(1000,10,5)
    > d<-as.data.frame(cbind(norm=a,unif=b,gamma_10_5=c))
    > ggInterval_scaMatrix(d)
    Warning in testData(data) :
      Automatically transform a classical data to symbolic data
    Error in switch(direction, vertical = c("left", "top"), horizontal = c("center",  : 
      EXPR must be a length 1 vector
    Calls: <Anonymous> ... <Anonymous> -> assemble -> <Anonymous> -> package_box -> %||%
    Execution halted
    ```

# ggfixest

<details>

* Version: 0.1.0
* GitHub: https://github.com/grantmcdermott/ggfixest
* Source code: https://github.com/cran/ggfixest
* Date/Publication: 2023-12-14 08:00:06 UTC
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
       diff| 1774
       info| Diff plot saved to: _tinysnapshot_review/ggiplot_multi_complex_kitchen_iid.png
      Error: 16 out of 101 tests failed
      In addition: Warning message:
      `guide_axis_nested()` was deprecated in ggh4x 0.3.0.
      ℹ Please use `legendry::guide_axis_nested()` instead.
      ℹ The deprecated feature was likely used in the ggfixest package.
        Please report the issue at
        <https://github.com/grantmcdermott/ggfixest/issues>. 
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
    
    > ### Name: facet_matrix
    > ### Title: Facet by different data columns
    > ### Aliases: facet_matrix
    > 
    > ### ** Examples
    > 
    > # Standard use:
    ...
     13. │       └─l$draw_geom(d, layout)
     14. │         └─ggplot2 (local) draw_geom(..., self = self)
     15. │           └─self$geom$handle_na(data, self$computed_geom_params)
     16. │             └─ggplot2 (local) handle_na(..., self = self)
     17. └─base::.handleSimpleError(...)
     18.   └─rlang (local) h(simpleError(msg, call))
     19.     └─handlers[[1L]](cnd)
     20.       └─cli::cli_abort(...)
     21.         └─rlang::abort(...)
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
* Number of recursive dependencies: 122

Run `revdepcheck::cloud_details(, "ggformula")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggformula-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gf_boxplot
    > ### Title: Formula interface to geom_boxplot()
    > ### Aliases: gf_boxplot
    > 
    > ### ** Examples
    > 
    > gf_boxplot(age ~ substance, data = mosaicData::HELPrct)
    > gf_boxplot(age ~ substance, data = mosaicData::HELPrct, varwidth = TRUE)
    > gf_boxplot(age ~ substance, data = mosaicData::HELPrct, color = ~sex)
    Error in unit(x, default.units) : 'x' and 'units' must have length > 0
    Calls: <Anonymous> ... grobTree -> gTree -> setChildren -> gList -> linesGrob -> unit
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggformula)
      Loading required package: ggplot2
      Loading required package: scales
      Loading required package: ggridges
      
      New to ggformula?  Try the tutorials: 
    ...
      • layer-factory/gf-text1.svg
      • layer-factory/gf-text2.svg
      • layer-factory/gf-tile1.svg
      • layer-factory/proportions-within-all-dodge.svg
      • layer-factory/proportions-within-fill-dodge.svg
      • layer-factory/proportions-within-fill-facet-grid-and-group.svg
      • layer-factory/proportions-within-fill-facet-grid.svg
      • layer-factory/proportions-within-group-facet-grid.svg
      Error: Test failures
      Execution halted
    ```

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

# gggenomes

<details>

* Version: 1.0.1
* GitHub: https://github.com/thackl/gggenomes
* Source code: https://github.com/cran/gggenomes
* Date/Publication: 2024-08-30 11:40:02 UTC
* Number of recursive dependencies: 111

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
      > 
      > test_check("ggh4x")
      [ FAIL 1 | WARN 0 | SKIP 15 | PASS 663 ]
      
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

# gghalves

<details>

* Version: 0.1.4
* GitHub: https://github.com/erocoar/gghalves
* Source code: https://github.com/cran/gghalves
* Date/Publication: 2022-11-20 11:40:02 UTC
* Number of recursive dependencies: 52

Run `revdepcheck::cloud_details(, "gghalves")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gghalves-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_half_boxplot
    > ### Title: A half boxplot
    > ### Aliases: geom_half_boxplot
    > 
    > ### ** Examples
    > 
    > ggplot(iris, aes(x = Species, y = Petal.Width, fill = Species)) + 
    +   geom_half_boxplot() 
    Error in unit(x, default.units) : 'x' and 'units' must have length > 0
    Calls: <Anonymous> ... grobTree -> gTree -> setChildren -> gList -> linesGrob -> unit
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘grDevices’ ‘gtable’
      All declared Imports should be used.
    ```

# ggheatmap

<details>

* Version: 2.2
* GitHub: NA
* Source code: https://github.com/cran/ggheatmap
* Date/Publication: 2022-09-10 13:32:55 UTC
* Number of recursive dependencies: 133

Run `revdepcheck::cloud_details(, "ggheatmap")` for more info

</details>

## Newly broken

*   checking whether package ‘ggheatmap’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘ggheatmap’
    See ‘/tmp/workdir/ggheatmap/new/ggheatmap.Rcheck/00install.out’ for details.
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
      > 
      > test_check("gghighlight")
      label_key: type
      label_key: type
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
    
    > ### Name: geom_boxplot_interactive
    > ### Title: Create interactive boxplot
    > ### Aliases: geom_boxplot_interactive
    > 
    > ### ** Examples
    > 
    > # add interactive boxplot -------
    ...
     24. │                       └─ggplot2 (local) draw_panel(..., self = self)
     25. │                         └─base::lapply(...)
     26. │                           └─ggplot2 (local) FUN(X[[i]], ...)
     27. │                             └─self$draw_group(group, panel_params, coord, ...)
     28. └─base::.handleSimpleError(...)
     29.   └─rlang (local) h(simpleError(msg, call))
     30.     └─handlers[[1L]](cnd)
     31.       └─cli::cli_abort(...)
     32.         └─rlang::abort(...)
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
* Number of recursive dependencies: 110

Run `revdepcheck::cloud_details(, "ggiraphExtra")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggiraphExtra-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggBoxplot
    > ### Title: Draw boxplots of a data.frame
    > ### Aliases: ggBoxplot
    > 
    > ### ** Examples
    > 
    > require(ggplot2)
    ...
     21. │                     └─ggplot2 (local) draw_panel(..., self = self)
     22. │                       └─base::lapply(...)
     23. │                         └─ggplot2 (local) FUN(X[[i]], ...)
     24. │                           └─self$draw_group(group, panel_params, coord, ...)
     25. └─base::.handleSimpleError(...)
     26.   └─rlang (local) h(simpleError(msg, call))
     27.     └─handlers[[1L]](cnd)
     28.       └─cli::cli_abort(...)
     29.         └─rlang::abort(...)
    Execution halted
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

# ggmosaic

<details>

* Version: 0.3.3
* GitHub: https://github.com/haleyjeppson/ggmosaic
* Source code: https://github.com/cran/ggmosaic
* Date/Publication: 2021-02-23 19:50:02 UTC
* Number of recursive dependencies: 76

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
* Number of recursive dependencies: 117

Run `revdepcheck::cloud_details(, "ggparty")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggparty-graphic-partying.Rmd’
      ...
    
    > ggparty(tr_tree, terminal_space = 0.4, layout = data.frame(id = c(1, 
    +     2, 5, 7), x = c(0.35, 0.15, 0.7, 0.8), y = c(0.95, 0.6, 0.8, 
    +     0.55 .... [TRUNCATED] 
    Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ℹ Please use `linewidth` instead.
    
      When sourcing ‘ggparty-graphic-partying.R’:
    Error: unused argument (h.start = 100)
    Execution halted
    
      ‘ggparty-graphic-partying.Rmd’ using ‘UTF-8’... failed
      ‘on-the-edge.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ggparty-graphic-partying.Rmd’ using rmarkdown
    
    Quitting from lines 42-122 [unnamed-chunk-2] (ggparty-graphic-partying.Rmd)
    Error: processing vignette 'ggparty-graphic-partying.Rmd' failed with diagnostics:
    unused argument (h.start = 100)
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

# ggpp

<details>

* Version: 0.5.8-1
* GitHub: https://github.com/aphalo/ggpp
* Source code: https://github.com/cran/ggpp
* Date/Publication: 2024-07-01 07:40:02 UTC
* Number of recursive dependencies: 81

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
    +               arrow = arrow(angle = 20,
    +                             length = grid::unit(1.5, "mm"),
    +                             ends = "first",
    +                             type = "closed")) +
    +   scale_colour_discrete(l = 40) + # luminance, make colours darker
    +   expand_limits(y = 27)
    Error in discrete_scale(aesthetics, palette = NULL, na.value = na.value,  : 
      argument 4 matches multiple formal arguments
    Calls: scale_colour_discrete
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘grammar-extensions.Rmd’
      ...
    
    > my.cars <- my.cars[order(my.cars$wt), ]
    
    > ggplot(my.cars, aes(wt, mpg, label = name)) + geom_point() + 
    +     geom_text_s(aes(colour = factor(cyl)), vjust = 0.5, angle = 90, 
    +         nudge .... [TRUNCATED] 
    
      When sourcing ‘grammar-extensions.R’:
    Error: argument 4 matches multiple formal arguments
    Execution halted
    
      ‘grammar-extensions.Rmd’ using ‘UTF-8’... failed
      ‘nudge-examples.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
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
      
      Attaching package: 'ggpubr'
      
      The following object is masked from 'package:ggplot2':
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
     24. │                           └─ggplot2:::sf_rescale01(data[[geom_column(data)]], x_range, y_range)
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

# ggScatRidges

<details>

* Version: 0.1.1
* GitHub: https://github.com/matbou85/ggScatRidges
* Source code: https://github.com/cran/ggScatRidges
* Date/Publication: 2024-03-25 10:20:05 UTC
* Number of recursive dependencies: 87

Run `revdepcheck::cloud_details(, "ggScatRidges")` for more info

</details>

## Newly broken

*   checking whether package ‘ggScatRidges’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘ggScatRidges’
    See ‘/tmp/workdir/ggScatRidges/new/ggScatRidges.Rcheck/00install.out’ for details.
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
    20 /tmp/Rtmp9H70El/b9497eff43/gganim_plot0020.png
    
    > data(lv_output)
    
    > animate_reveal_time_series(lv_output, parameters = c("X", 
    +     "Y"), alpha = 0.5, ylimits = c(0, 600), duration = 10)
    
      When sourcing ‘Visualising.R’:
    Error: attempt to apply non-function
    Execution halted
    
      ‘Visualising.Rmd’ using ‘UTF-8’... failed
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 175 marked UTF-8 strings
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
# ggTimeSeries

<details>

* Version: 1.0.2
* GitHub: https://github.com/thecomeonman/ggTimeSeries
* Source code: https://github.com/cran/ggTimeSeries
* Date/Publication: 2022-01-23 16:22:42 UTC
* Number of recursive dependencies: 55

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
    + p1
    + # add new geoms
    + p1 +
    + geom_text(label = '!!!') +
    + scale_colour_continuous(low = 'red', high = 'green')
    + }
    Error in continuous_scale(aesthetics, palette = NULL, guide = guide, na.value = na.value,  : 
      unused arguments (low = "red", high = "green")
    Calls: scale_colour_continuous
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggTimeSeries.Rmd’
      ...
    +     "%m")) - 6.5)) * runif(1) * 0.75), .I]
    
    > p1 = ggplot_calendar_heatmap(dtData, "DateCol", "ValueCol")
    
    > p1 + xlab(NULL) + ylab(NULL) + scale_fill_continuous(low = "green", 
    +     high = "red") + facet_wrap(~Year, ncol = 1)
    
      When sourcing ‘ggTimeSeries.R’:
    Error: unused arguments (low = "green", high = "red")
    Execution halted
    
      ‘ggTimeSeries.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ggTimeSeries.Rmd’ using rmarkdown
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

# GimmeMyPlot

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/GimmeMyPlot
* Date/Publication: 2023-10-18 16:10:02 UTC
* Number of recursive dependencies: 117

Run `revdepcheck::cloud_details(, "GimmeMyPlot")` for more info

</details>

## Newly broken

*   checking whether package ‘GimmeMyPlot’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘GimmeMyPlot’
    See ‘/tmp/workdir/GimmeMyPlot/new/GimmeMyPlot.Rcheck/00install.out’ for details.
    ```

# greed

<details>

* Version: 0.6.1
* GitHub: https://github.com/comeetie/greed
* Source code: https://github.com/cran/greed
* Date/Publication: 2022-10-03 22:00:05 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "greed")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(greed)
      > 
      > test_check("greed")
      [ FAIL 10 | WARN 3 | SKIP 0 | PASS 291 ]
      
    ...
       15.               └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
       16.                 └─plot$guides$assemble(theme)
       17.                   └─ggplot2 (local) assemble(..., self = self)
       18.                     └─self$package_box(grobs[[i]], position, theme + adjust)
       19.                       └─ggplot2 (local) package_box(...)
       20.                         └─theme$legend.box.just %||% ...
      
      [ FAIL 10 | WARN 3 | SKIP 0 | PASS 291 ]
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
      installed size is 32.1Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        libs  28.7Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 989 marked UTF-8 strings
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

# hilldiv

<details>

* Version: 1.5.1
* GitHub: https://github.com/anttonalberdi/hilldiv
* Source code: https://github.com/cran/hilldiv
* Date/Publication: 2019-10-01 14:40:02 UTC
* Number of recursive dependencies: 147

Run `revdepcheck::cloud_details(, "hilldiv")` for more info

</details>

## Newly broken

*   checking whether package ‘hilldiv’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘hilldiv’
    See ‘/tmp/workdir/hilldiv/new/hilldiv.Rcheck/00install.out’ for details.
    ```

# hJAM

<details>

* Version: 1.0.0
* GitHub: https://github.com/lailylajiang/hJAM
* Source code: https://github.com/cran/hJAM
* Date/Publication: 2020-02-20 14:50:05 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::cloud_details(, "hJAM")` for more info

</details>

## Newly broken

*   checking whether package ‘hJAM’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘hJAM’
    See ‘/tmp/workdir/hJAM/new/hJAM.Rcheck/00install.out’ for details.
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

# IDMIR

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/IDMIR
* Date/Publication: 2023-11-09 15:30:02 UTC
* Number of recursive dependencies: 116

Run `revdepcheck::cloud_details(, "IDMIR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘IDMIR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: PlotSurvival
    > ### Title: PlotSurvival
    > ### Aliases: PlotSurvival
    > 
    > ### ** Examples
    > 
    > # Obtain the example data
    ...
    > # Run the function
    > SingleMiRNA_CRData<-SingleMiRNA_CRModel(GEP,
    + "hsa-miR-21-5p",survival,cutoff.point=NULL)
    > PlotSurvival(SingleMiRNA_CRData)
    Warning: Ignoring unknown labels:
    • `fill = "Strata"`
    • `linetype = "1"`
    Error in theme + adjust : non-numeric argument to binary operator
    Calls: <Anonymous> ... <Anonymous> -> assemble -> <Anonymous> -> package_box -> %||%
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘IDMIR.Rmd’
      ...
    +     cutoff.point = NULL, survival)
    
    > PlotSurvival(SingleMiRNA_CRData)
    Warning: Ignoring unknown labels:
    • `fill = "Strata"`
    • `linetype = "1"`
    
      When sourcing ‘IDMIR.R’:
    Error: non-numeric argument to binary operator
    Execution halted
    
      ‘IDMIR.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘IDMIR.Rmd’ using rmarkdown
    
    Quitting from lines 120-130 [unnamed-chunk-7] (IDMIR.Rmd)
    Error: processing vignette 'IDMIR.Rmd' failed with diagnostics:
    non-numeric argument to binary operator
    --- failed re-building ‘IDMIR.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘IDMIR.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ImFoR

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/ImFoR
* Date/Publication: 2023-09-21 18:50:02 UTC
* Number of recursive dependencies: 170

Run `revdepcheck::cloud_details(, "ImFoR")` for more info

</details>

## Newly broken

*   checking whether package ‘ImFoR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘ImFoR’
    See ‘/tmp/workdir/ImFoR/new/ImFoR.Rcheck/00install.out’ for details.
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

# iNEXT.4steps

<details>

* Version: 1.0.1
* GitHub: https://github.com/KaiHsiangHu/iNEXT.4steps
* Source code: https://github.com/cran/iNEXT.4steps
* Date/Publication: 2024-06-18 09:10:02 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::cloud_details(, "iNEXT.4steps")` for more info

</details>

## Newly broken

*   checking whether package ‘iNEXT.4steps’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘iNEXT.4steps’
    See ‘/tmp/workdir/iNEXT.4steps/new/iNEXT.4steps.Rcheck/00install.out’ for details.
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
      installed size is 22.6Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        libs  20.0Mb
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

# insane

<details>

* Version: 1.0.3
* GitHub: https://github.com/mcanouil/insane
* Source code: https://github.com/cran/insane
* Date/Publication: 2023-11-14 21:50:02 UTC
* Number of recursive dependencies: 133

Run `revdepcheck::cloud_details(, "insane")` for more info

</details>

## Newly broken

*   checking whether package ‘insane’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘insane’
    See ‘/tmp/workdir/insane/new/insane.Rcheck/00install.out’ for details.
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
    ...
    ! `x$size` must be a vector, not a <quosure> object.
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
      installed size is 10.3Mb
      sub-directories of 1Mb or more:
        doc   9.7Mb
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
    > forecast(fitted.data, 24,
    +   chickenPox = list(Total = rep(175, 24)), # Optional
    +   Vaccine.1.Covariate = rep(TRUE, 24),
    +   Vaccine.2.Covariate = rep(TRUE, 24)
    + )
    Scale for y is already present.
    Adding another scale for y, which will replace the existing scale.
    Error in pm[[2]] : subscript out of bounds
    Calls: forecast ... lapply -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# knfi

<details>

* Version: 1.0.1.9
* GitHub: https://github.com/SYOUNG9836/knfi
* Source code: https://github.com/cran/knfi
* Date/Publication: 2024-12-03 07:20:05 UTC
* Number of recursive dependencies: 148

Run `revdepcheck::cloud_details(, "knfi")` for more info

</details>

## Newly broken

*   checking whether package ‘knfi’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘knfi’
    See ‘/tmp/workdir/knfi/new/knfi.Rcheck/00install.out’ for details.
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 42740 marked UTF-8 strings
    ```

# lans2r

<details>

* Version: 1.2.0
* GitHub: https://github.com/KopfLab/lans2r
* Source code: https://github.com/cran/lans2r
* Date/Publication: 2023-02-19 07:20:02 UTC
* Number of recursive dependencies: 85

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
       1. ├─testthat::expect_true(is(plot_maps(data), "ggplot")) at test-plotting.R:18:3
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. ├─methods::is(plot_maps(data), "ggplot")
       5. └─lans2r::plot_maps(data)
       6.   └─ggplot2::scale_fill_continuous(low = color_scale[1], high = color_scale[2])
      
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 143 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘lans2r.Rmd’
      ...
    |analysis1 |    1|    7|           256| 0.0391172| 0.2738203|        10.014|12C      |ion_count |   702| 26.49528|   0|
    |analysis1 |    1|    8|           256| 0.0391172| 0.3129375|        10.014|12C      |ion_count |   453| 21.28380|   0|
    |analysis1 |    1|    9|           256| 0.0391172| 0.3520547|        10.014|12C      |ion_count |   319| 17.86057|   0|
    |analysis1 |    1|   10|           256| 0.0391172| 0.3911719|        10.014|12C      |ion_count |   220| 14.83240|   0|
    
    > plot_maps(maps)
    
      When sourcing ‘lans2r.R’:
    Error: unused arguments (low = "black", high = "white")
    Execution halted
    
      ‘lans2r-calculate.Rmd’ using ‘UTF-8’... OK
      ‘lans2r-hmr.Rmd’ using ‘UTF-8’... OK
      ‘lans2r.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
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
       0.02    0.00    0.02 
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

# legendry

<details>

* Version: 0.2.0
* GitHub: https://github.com/teunbrand/legendry
* Source code: https://github.com/cran/legendry
* Date/Publication: 2024-12-14 11:40:02 UTC
* Number of recursive dependencies: 70

Run `revdepcheck::cloud_details(, "legendry")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘legendry-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: key_group
    > ### Title: Group keys
    > ### Aliases: key_group key_group_split key_group_lut
    > 
    > ### ** Examples
    > 
    > # Example scale
    > values <- c("group A:value 1", "group A:value 2", "group B:value 1")
    > template <- scale_colour_discrete(limits = values)
    > 
    > # Treat the 'group X' part as groups
    > key <- key_group_split(sep = ":")
    > key(template)
    Error in self$palette(n) : attempt to apply non-function
    Calls: key ... group_from_split_label -> <Anonymous> -> extract_key -> <Anonymous> -> map
    Execution halted
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
    Error: EXPR must be a length 1 vector
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
    [1]   4.074066  12.939823  41.130281 119.281677
    
    > lspace_BrIII
    
      When sourcing ‘LMoFit.R’:
    Error: Problem while computing layer data.
    ℹ Error occurred in the 1st layer.
    Caused by error in `is.waive()`:
    ! could not find function "is.waive"
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
    Problem while computing layer data.
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

# LorMe

<details>

* Version: 1.1.0
* GitHub: NA
* Source code: https://github.com/cran/LorMe
* Date/Publication: 2024-09-13 09:00:02 UTC
* Number of recursive dependencies: 210

Run `revdepcheck::cloud_details(, "LorMe")` for more info

</details>

## Newly broken

*   checking whether package ‘LorMe’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘LorMe’
    See ‘/tmp/workdir/LorMe/new/LorMe.Rcheck/00install.out’ for details.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.5Mb
      sub-directories of 1Mb or more:
        data   7.0Mb
    ```

# lphom

<details>

* Version: 0.3.5-5
* GitHub: NA
* Source code: https://github.com/cran/lphom
* Date/Publication: 2024-03-03 10:30:02 UTC
* Number of recursive dependencies: 30

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
     Their aggregate importances, measured in percentage of the total census, are:
         %NET_ENTRIES = 0.0281%
         %NET_EXITS = 0.0262%
    If NET_ENTRIES and/or NET_EXITS are really small, less than 1% in all units, their corresponding results will not be displayed in the main output, VTM. They are anyway always included in VTM.complete. 
    *************************************************
    > p <- plot(mt.ns, show.plot = FALSE)
    Error in continuous_scale(aesthetics, palette = NULL, guide = guide, na.value = na.value,  : 
      unused arguments (high = "#CD107680", low = "white")
    Calls: plot -> plot.lphom -> pintar_con -> <Anonymous>
    Execution halted
    ```

# LTCDM

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/LTCDM
* Date/Publication: 2024-05-15 20:00:02 UTC
* Number of recursive dependencies: 99

Run `revdepcheck::cloud_details(, "LTCDM")` for more info

</details>

## Newly broken

*   checking whether package ‘LTCDM’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘LTCDM’
    See ‘/tmp/workdir/LTCDM/new/LTCDM.Rcheck/00install.out’ for details.
    ```

# mapindia

<details>

* Version: 1.0.1
* GitHub: https://github.com/shubhamdutta26/mapindia
* Source code: https://github.com/cran/mapindia
* Date/Publication: 2024-11-14 16:10:07 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::cloud_details(, "mapindia")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘getting_started.Rmd’
      ...
    
    > mh + tn
    
    > statepop2011 <- plot_india("states", data = statepop, 
    +     values = "pop_2011") + scale_fill_continuous(low = "blue", 
    +     high = "yellow", guid .... [TRUNCATED] 
    
      When sourcing ‘getting_started.R’:
    Error: unused arguments (low = "blue", high = "yellow")
    Execution halted
    
      ‘getting_started.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘getting_started.Rmd’ using rmarkdown
    ```

# mc2d

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/mc2d
* Date/Publication: 2024-06-05 17:30:07 UTC
* Number of recursive dependencies: 90

Run `revdepcheck::cloud_details(, "mc2d")` for more info

</details>

## Newly broken

*   checking whether package ‘mc2d’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘mc2d’
    See ‘/tmp/workdir/mc2d/new/mc2d.Rcheck/00install.out’ for details.
    ```

## In both

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘docmcEnglish.Rnw’ using Sweave
    Loading required package: mvtnorm
    Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘mc2d’
    
    Attaching package: ‘mc2d’
    
    The following objects are masked from ‘package:base’:
    
        pmax, pmin
    ...
    l.179   \RequirePackage{grfext}\relax
                                         ^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘mc2dLmEnglish.rnw’
    
    SUMMARY: processing the following files failed:
      ‘docmcEnglish.Rnw’ ‘mc2dLmEnglish.rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# mcStats

<details>

* Version: 0.1.2
* GitHub: NA
* Source code: https://github.com/cran/mcStats
* Date/Publication: 2020-02-26 06:50:02 UTC
* Number of recursive dependencies: 55

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

# MetaNet

<details>

* Version: 0.1.2
* GitHub: https://github.com/Asa12138/MetaNet
* Source code: https://github.com/cran/MetaNet
* Date/Publication: 2024-03-25 20:40:07 UTC
* Number of recursive dependencies: 154

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
      7.         └─ggplot2:::as_continuous_pal(elem)
      8.           └─scales::colour_ramp(x)
      9.             └─cli::cli_abort("Must provide at least one colour to create a colour ramp")
     10.               └─rlang::abort(...)
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
        9.           └─npscales$set_palettes(plot$theme)
       10.             └─ggplot2 (local) set_palettes(..., self = self)
       11.               └─ggplot2:::as_continuous_pal(elem)
       12.                 └─scales::colour_ramp(x)
       13.                   └─cli::cli_abort("Must provide at least one colour to create a colour ramp")
       14.                     └─rlang::abort(...)
      
      [ FAIL 2 | WARN 1 | SKIP 18 | PASS 184 ]
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

# MF.beta4

<details>

* Version: 1.1.1
* GitHub: https://github.com/AnneChao/MF.beta4
* Source code: https://github.com/cran/MF.beta4
* Date/Publication: 2024-12-08 06:50:02 UTC
* Number of recursive dependencies: 182

Run `revdepcheck::cloud_details(, "MF.beta4")` for more info

</details>

## Newly broken

*   checking whether package ‘MF.beta4’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘MF.beta4’
    See ‘/tmp/workdir/MF.beta4/new/MF.beta4.Rcheck/00install.out’ for details.
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Introduction.Rnw’ using Sweave
    Error: processing vignette 'Introduction.Rnw' failed with diagnostics:
    Running 'texi2dvi' on 'Introduction.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `pdfpages.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    ...
    l.4 ^^M
           
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘Introduction.Rnw’
    
    SUMMARY: processing the following file failed:
      ‘Introduction.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
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
* Number of recursive dependencies: 417

Run `revdepcheck::cloud_details(, "MiscMetabar")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(MiscMetabar)
      Loading required package: phyloseq
      Loading required package: ggplot2
      Loading required package: dada2
      Loading required package: Rcpp
      Loading required package: dplyr
    ...
        'test_tuckey.R:5:3', 'test_tuckey.R:17:3', 'test_tuckey.R:26:3',
        'test_misc.R:11:3', 'test_misc.R:30:3', 'test_misc.R:97:3'
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test_figures_beta_div.R:72:5'): graph_test_pq works ───────────────
      `graph_test_pq(data_fungi_mini, fact = "Tree_name")` produced warnings.
      
      [ FAIL 1 | WARN 0 | SKIP 75 | PASS 83 ]
      Error: Test failures
      Execution halted
    ```

# missingHE

<details>

* Version: 1.5.0
* GitHub: NA
* Source code: https://github.com/cran/missingHE
* Date/Publication: 2023-03-21 08:50:02 UTC
* Number of recursive dependencies: 152

Run `revdepcheck::cloud_details(, "missingHE")` for more info

</details>

## Newly broken

*   checking whether package ‘missingHE’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘missingHE’
    See ‘/tmp/workdir/missingHE/new/missingHE.Rcheck/00install.out’ for details.
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘mcmcr’
      All declared Imports should be used.
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

# MSPRT

<details>

* Version: 3.0
* GitHub: NA
* Source code: https://github.com/cran/MSPRT
* Date/Publication: 2020-11-13 10:20:05 UTC
* Number of recursive dependencies: 76

Run `revdepcheck::cloud_details(, "MSPRT")` for more info

</details>

## Newly broken

*   checking whether package ‘MSPRT’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘MSPRT’
    See ‘/tmp/workdir/MSPRT/new/MSPRT.Rcheck/00install.out’ for details.
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘datasets’ ‘grDevices’ ‘graphics’ ‘iterators’ ‘methods’
      All declared Imports should be used.
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
      [ FAIL 2 | WARN 13 | SKIP 0 | PASS 56 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
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

# MTLR

<details>

* Version: 0.2.1
* GitHub: https://github.com/haiderstats/MTLR
* Source code: https://github.com/cran/MTLR
* Date/Publication: 2019-06-03 21:30:03 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "MTLR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘MTLR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotcurves
    > ### Title: Graphically Visualize MTLR Survival Curves
    > ### Aliases: plotcurves
    > 
    > ### ** Examples
    > 
    > #Set up the example:
    ...
    > #Note the legend is now gone:
    > plotcurves(curves, 1:20)
    Warning: Use of `plot_data$value` is discouraged.
    ℹ Use `value` instead.
    Warning: Use of `plot_data$Index` is discouraged.
    ℹ Use `Index` instead.
    Error in switch(direction, vertical = c("left", "top"), horizontal = c("center",  : 
      EXPR must be a length 1 vector
    Calls: <Anonymous> ... <Anonymous> -> assemble -> <Anonymous> -> package_box -> %||%
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
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
    +  row.vars = "inhibitor")
    binmatnest.temperature 
                  13.21221 
    Size of Square: 	 66 rows x  66 columns 
    Size of Rectangular_row: 	 6 rows x  105 columns 
    Size of Rectangular_col: 	 99 rows x  2 columns 
    Size of Rectangular_element_max: 	 59 rows x  79 columns 
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
    
    > beatAML_incidence_matrix <- plotIncMatrix(x = beatAML_data, 
    +     index_nominal = c(2, 1), index_numeric = 3, print_skim = FALSE, 
    +     plot_weigh .... [TRUNCATED] 
    
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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘nuts.Rmd’
      ...
    
    > no_changes <- unique(c(no_changes$from_code, no_changes$to_code))
    
    > gg_2006 = ggplot() + geom_sf(data = no_2006, aes(fill = values), 
    +     color = "grey", linewidth = 0.5) + geom_sf(data = filter(no_2006, 
    +     NUT .... [TRUNCATED] 
    
      When sourcing ‘nuts.R’:
    Error: unused arguments (high = "#132B43", low = "#56B1F7")
    Execution halted
    
      ‘nuts.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
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

# OenoKPM

<details>

* Version: 2.4.1
* GitHub: NA
* Source code: https://github.com/cran/OenoKPM
* Date/Publication: 2024-04-08 19:20:10 UTC
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "OenoKPM")` for more info

</details>

## Newly broken

*   checking whether package ‘OenoKPM’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘OenoKPM’
    See ‘/tmp/workdir/OenoKPM/new/OenoKPM.Rcheck/00install.out’ for details.
    ```

# ofpetrial

<details>

* Version: 0.1.2
* GitHub: https://github.com/DIFM-Brain/ofpetrial
* Source code: https://github.com/cran/ofpetrial
* Date/Publication: 2024-12-11 23:00:02 UTC
* Number of recursive dependencies: 140

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
    Installing study "ABC" in /tmp/RtmpvCA7DM/file22cf185b60ba
    Exporting study "ABC" as an R package
    Note: No maintainer email was specified. Using the placeholder: Unknown <unknown@unknown>
    Calculating pairwise overlaps. This may take a while...
    Exported study to /tmp/RtmpvCA7DM/ONstudyABC
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

# onemap

<details>

* Version: 3.2.0
* GitHub: https://github.com/Cristianetaniguti/onemap
* Source code: https://github.com/cran/onemap
* Date/Publication: 2025-01-10 16:20:05 UTC
* Number of recursive dependencies: 174

Run `revdepcheck::cloud_details(, "onemap")` for more info

</details>

## Newly broken

*   checking whether package ‘onemap’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘onemap’
    See ‘/tmp/workdir/onemap/new/onemap.Rcheck/00install.out’ for details.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.6Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        doc    2.2Mb
        libs   4.6Mb
    ```

# OneSampleLogRankTest

<details>

* Version: 0.9.2
* GitHub: NA
* Source code: https://github.com/cran/OneSampleLogRankTest
* Date/Publication: 2024-02-03 12:30:15 UTC
* Number of recursive dependencies: 110

Run `revdepcheck::cloud_details(, "OneSampleLogRankTest")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘OneSampleLogRankTest-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotKM
    > ### Title: Plot Kaplan-Meier Curve against Population
    > ### Aliases: plotKM
    > 
    > ### ** Examples
    > 
    > # load data
    ...
    > data(dataSurv_small)
    > data(dataPop_2018_2021)
    > 
    > plotKM(dataSurv_small, dataPop_2018_2021, type = "exact")
    Warning: Ignoring unknown labels:
    • `fill = ""`
    • `linetype = "1"`
    Error in theme + adjust : non-numeric argument to binary operator
    Calls: <Anonymous> ... <Anonymous> -> assemble -> <Anonymous> -> package_box -> %||%
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘oneSampleLogRankTest.Rmd’
      ...
    
    
    > plotKM(dataSurv, dataPop_2018_2021_race_sex_eth, type = "approximate")
    Warning: Ignoring unknown labels:
    • `fill = ""`
    • `linetype = "1"`
    
      When sourcing ‘oneSampleLogRankTest.R’:
    Error: non-numeric argument to binary operator
    Execution halted
    
      ‘oneSampleLogRankTest.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘oneSampleLogRankTest.Rmd’ using rmarkdown
    
    Quitting from lines 74-77 [unnamed-chunk-3] (oneSampleLogRankTest.Rmd)
    Error: processing vignette 'oneSampleLogRankTest.Rmd' failed with diagnostics:
    non-numeric argument to binary operator
    --- failed re-building ‘oneSampleLogRankTest.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘oneSampleLogRankTest.Rmd’
    
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

# pathfindR

<details>

* Version: 2.4.1
* GitHub: https://github.com/egeulgen/pathfindR
* Source code: https://github.com/cran/pathfindR
* Date/Publication: 2024-05-04 15:30:05 UTC
* Number of recursive dependencies: 148

Run `revdepcheck::cloud_details(, "pathfindR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘pathfindR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: UpSet_plot
    > ### Title: Create UpSet Plot of Enriched Terms
    > ### Aliases: UpSet_plot
    > 
    > ### ** Examples
    > 
    > UpSet_plot(example_pathfindR_output)
    Error in identicalUnits(x) : object is not a unit
    Calls: <Anonymous> ... render_comb_axis -> gtable_col -> %||% -> unit.c -> identicalUnits
    Execution halted
    ```

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

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘comparing_results.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘intro_vignette.Rmd’
      ...
    ## Performing Active Subnetwork Search and Enrichment
    Killed
    Killed
    Killed
    Killed
    Killed
    
    ...
      When sourcing ‘visualization_vignette.R’:
    Error: object is not a unit
    Execution halted
    
      ‘comparing_results.Rmd’ using ‘UTF-8’... OK
      ‘intro_vignette.Rmd’ using ‘UTF-8’... failed
      ‘manual_execution.Rmd’ using ‘UTF-8’... failed
      ‘non_hs_analysis.Rmd’ using ‘UTF-8’... failed
      ‘obtain_data.Rmd’ using ‘UTF-8’... failed
      ‘visualization_vignette.Rmd’ using ‘UTF-8’... failed
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

# penAFT

<details>

* Version: 0.3.0
* GitHub: NA
* Source code: https://github.com/cran/penAFT
* Date/Publication: 2023-04-18 03:10:02 UTC
* Number of recursive dependencies: 31

Run `revdepcheck::cloud_details(, "penAFT")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘penAFT-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: penAFT.trace
    > ### Title: Print trace plot for the regularized Gehan estimator fit using
    > ###   'penAFT' or 'penAFT.cv'
    > ### Aliases: penAFT.trace
    > 
    > ### ** Examples
    > 
    ...
    CV through:  ### ### ###         60 % 
    CV through:  ### ### ### ###     80 % 
    CV through:  ### ### ### ### ###  100 % 
    > 
    > # -- print plot
    > penAFT.trace(fit.cv)
    Error in switch(direction, vertical = c("left", "top"), horizontal = c("center",  : 
      EXPR must be a length 1 vector
    Calls: <Anonymous> ... <Anonymous> -> assemble -> <Anonymous> -> package_box -> %||%
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

# PieGlyph

<details>

* Version: 1.0.0
* GitHub: https://github.com/rishvish/PieGlyph
* Source code: https://github.com/cran/PieGlyph
* Date/Publication: 2024-06-28 12:00:02 UTC
* Number of recursive dependencies: 90

Run `revdepcheck::cloud_details(, "PieGlyph")` for more info

</details>

## Newly broken

*   checking whether package ‘PieGlyph’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/PieGlyph/new/PieGlyph.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘time-series-example.Rmd’
      ...
    135            92    1506   720  323 16155   0.1073748        10   0 1980.167
    136           100    1360   646  310 16585   0.1116954         4   0 1980.250
    137            95    1453   765  424 18117   0.1106382         8   0 1980.333
    138           114    1522   820  403 17552   0.1118552         8   0 1980.417
    
    > knitr::include_graphics("../man/figures/scatterpie-1.png")
    
    ...
      before plotting.
    Execution halted
    
      ‘PieGlyph.Rmd’ using ‘UTF-8’... OK
      ‘interactive-pie-glyphs.Rmd’ using ‘UTF-8’... OK
      ‘multinomial-classification-example.Rmd’ using ‘UTF-8’... OK
      ‘pie-lollipop-example.Rmd’ using ‘UTF-8’... OK
      ‘spatial-example.Rmd’ using ‘UTF-8’... OK
      ‘time-series-example.Rmd’ using ‘UTF-8’... failed
      ‘unusual-situations.Rmd’ using ‘UTF-8’... failed
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

# plinkQC

<details>

* Version: 0.3.4
* GitHub: https://github.com/meyer-lab-cshl/plinkQC
* Source code: https://github.com/cran/plinkQC
* Date/Publication: 2021-07-15 15:40:02 UTC
* Number of recursive dependencies: 87

Run `revdepcheck::cloud_details(, "plinkQC")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(plinkQC)
      > 
      > test_check("plinkQC")
      [ FAIL 1 | WARN 9 | SKIP 0 | PASS 71 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
       12.                 └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
       13.                   └─plot$guides$assemble(theme)
       14.                     └─ggplot2 (local) assemble(..., self = self)
       15.                       └─self$package_box(grobs[[i]], position, theme + adjust)
       16.                         └─ggplot2 (local) package_box(...)
       17.                           └─theme$legend.box.just %||% ...
      
      [ FAIL 1 | WARN 9 | SKIP 0 | PASS 71 ]
      Error: Test failures
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

# plothelper

<details>

* Version: 0.1.9
* GitHub: https://github.com/githubwwwjjj/plothelper
* Source code: https://github.com/cran/plothelper
* Date/Publication: 2020-05-08 08:40:10 UTC
* Number of recursive dependencies: 44

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
    > pq=data.frame(cbind(p, q))
    > dat=sunshinexy(outer=pq, n=20, delete_n=5, distance=TRUE)
    > ggplot()+coord_fixed()+theme_void()+
    + 	geom_point(data=pq, aes(p, q), size=4)+
    + 	geom_line(show.legend=FALSE, data=dat, aes(x, y, group=g, color=distance), size=2)+
    + 	scale_color_continuous(low="blue", high="red")
    Error in continuous_scale(aesthetics, palette = NULL, guide = guide, na.value = na.value,  : 
      unused arguments (low = "blue", high = "red")
    Calls: scale_color_continuous
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

# PopED

<details>

* Version: 0.7.0
* GitHub: https://github.com/andrewhooker/PopED
* Source code: https://github.com/cran/PopED
* Date/Publication: 2024-10-07 19:30:02 UTC
* Number of recursive dependencies: 140

Run `revdepcheck::cloud_details(, "PopED")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        doc    1.4Mb
        test   1.1Mb
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
            NULL, FALSE, "white", TRUE), NULL, list(NULL, 0.5, NULL, NULL, FALSE, NULL, TRUE), NULL, NULL, NULL, NULL, FALSE, NULL, NULL, list(NULL, "white", NULL, NULL, TRUE), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(5.5, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, TRUE), "topleft", NULL, 
        NULL, list("#D9D9D9FF", NA, NULL, NULL, TRUE), NULL, NULL, "on", "inside", list(NULL, NULL, "#1A1A1AFF", 0.8, NULL, NULL, NULL, NULL, c(4.4, 4.4, 4.4, 4.4), NULL, TRUE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75))
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
    '/tmp/RtmpiIeOeo/file13dd4fab8118/vignettes'.
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
      9. │           └─layout$setup(data, plot$data, plot$plot_env)
     10. │             └─ggplot2 (local) setup(..., self = self)
     11. │               └─base::lapply(...)
     12. │                 └─ggplot2 (local) FUN(X[[i]], ...)
     13. │                   └─ggplot2::map_data(...)
     14. │                     └─vctrs::vec_slice(data, facet_vals$.index)
     15. └─vctrs:::stop_scalar_type(`<fn>`(`<data.frame>`), "x", `<env>`)
     16.   └─vctrs:::stop_vctrs(...)
     17.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

# predictMe

<details>

* Version: 0.1
* GitHub: https://github.com/mmiche/predictMe
* Source code: https://github.com/cran/predictMe
* Date/Publication: 2022-05-24 09:40:02 UTC
* Number of recursive dependencies: 56

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
    +                    fitted=lmRes$fitted.values)
    > # Apply function binBinary
    > x100c <- binContinuous(x=lmDf, measColumn = 1, binWidth = 20)
    > # Apply function makeDiffPlot, using columns 1 and 2 from x100c[["xTrans"]]
    > # The first of columns 1 and 2 contains the measured outcome values.
    > tp <- makeTablePlot(x100c[["xTrans"]][,1:2], measColumn = 1, plot = TRUE)
    Error in continuous_scale(aesthetics, palette = NULL, guide = guide, na.value = na.value,  : 
      unused arguments (high = "#132B43", low = "#56B1F7")
    Calls: makeTablePlot -> <Anonymous>
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘predictMe.Rmd’
      ...
    4              4         4    68.05227 63.98939   4.0628725        4          0
    5              3         3    52.96387 53.83204  -0.8681698        5          0
    6              2         2    36.07562 28.75503   7.3205928        6          0
    
    > outLs <- makeTablePlot(x100c[["xTrans"]][, 1:2], measColumn = 1, 
    +     plot = TRUE)
    
      When sourcing ‘predictMe.R’:
    Error: unused arguments (high = "#132B43", low = "#56B1F7")
    Execution halted
    
      ‘predictMe.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘predictMe.Rmd’ using rmarkdown
    
    Quitting from lines 95-99 [unnamed-chunk-4] (predictMe.Rmd)
    Error: processing vignette 'predictMe.Rmd' failed with diagnostics:
    unused arguments (high = "#132B43", low = "#56B1F7")
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
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "prevR")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘intro_prevR.Rmd’
      ...
     224228.1  523628.1 1251165.0 1669034.2 
    
    > plot(dhs, axes = TRUE)
    
    > qa <- quick.prevR(fdhs, return.results = TRUE, return.plot = TRUE, 
    +     plot.results = FALSE, progression = FALSE)
    
      When sourcing ‘intro_prevR.R’:
    Error: unused arguments (low = "grey50", high = "grey50")
    Execution halted
    
      ‘intro_prevR.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
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
    ...
        NULL, NULL, NULL, FALSE, NULL, NULL, list(), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, c(0, 0, 6, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 6, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(6, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, TRUE), "topleft", NULL, NULL, list(), NULL, NULL, "on", "outside", list(NULL, NULL, "#1A1A1AFF", 0.9, NULL, NULL, NULL, NULL, c(4.8, 4.8, 4.8, 4.8), 
            NULL, FALSE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 3, 3, list(), list(NULL, NULL, NULL, NULL, FALSE, NULL, TRUE), list(NULL, NULL, NULL, NULL, FALSE, NULL, TRUE), list(NULL, NULL, NULL, NULL, FALSE, NULL, TRUE), list(NULL, NULL, NULL, NULL, 0.5, 0.5, 0, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), 0.666666666666667, 
        0.333333333333333))
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

*   checking examples ... ERROR
    ```
    Running examples in ‘probably-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: cal_plot_breaks
    > ### Title: Probability calibration plots via binning
    > ### Aliases: cal_plot_breaks cal_plot_breaks.data.frame
    > ###   cal_plot_breaks.tune_results cal_plot_breaks.grouped_df
    > 
    > ### ** Examples
    > 
    ...
    > 
    > # The grouping can be faceted in ggplot2
    > combined %>%
    +   cal_plot_logistic(Class, .pred_good, .by = source) +
    +   facet_wrap(~source) +
    +   theme(legend.position = "")
    Error in switch(direction, vertical = c("left", "top"), horizontal = c("center",  : 
      EXPR must be a length 1 vector
    Calls: <Anonymous> ... <Anonymous> -> assemble -> <Anonymous> -> package_box -> %||%
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

# profoc

<details>

* Version: 1.3.3
* GitHub: https://github.com/BerriJ/profoc
* Source code: https://github.com/cran/profoc
* Date/Publication: 2024-09-21 22:10:02 UTC
* Number of recursive dependencies: 72

Run `revdepcheck::cloud_details(, "profoc")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘profoc.Rmd’
      ...
     9     1 D1     0.09     -1.01 
    10     1 D1     0.1      -0.922
    # ℹ 3,158 more rows
    
    > ggplot(tidy(combination$predictions), aes(x = t, y = prediction, 
    +     group = p, colour = p)) + geom_line() + scale_color_continuous(low = "#FFDD0 ..." ... [TRUNCATED] 
    
      When sourcing ‘profoc.R’:
    Error: unused arguments (low = "#FFDD00", high = "#0057B7")
    Execution halted
    
      ‘class.Rmd’ using ‘UTF-8’... OK
      ‘production.Rmd’ using ‘UTF-8’... OK
      ‘profoc.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
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
      installed size is 20.5Mb
      sub-directories of 1Mb or more:
        doc    1.4Mb
        libs  18.3Mb
    ```

# promor

<details>

* Version: 0.2.1
* GitHub: https://github.com/caranathunge/promor
* Source code: https://github.com/cran/promor
* Date/Publication: 2023-07-17 22:30:02 UTC
* Number of recursive dependencies: 164

Run `revdepcheck::cloud_details(, "promor")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘promor-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: volcano_plot
    > ### Title: Volcano plot
    > ### Aliases: volcano_plot
    > 
    > ### ** Examples
    > 
    > 
    > ## Create a volcano plot with default settings.
    > volcano_plot(ecoli_fit_df)
    Error in switch(direction, vertical = c("left", "top"), horizontal = c("center",  : 
      EXPR must be a length 1 vector
    Calls: <Anonymous> ... <Anonymous> -> assemble -> <Anonymous> -> package_box -> %||%
    Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘intro_to_promor.Rmd’
      ...
    
    > knitr::include_graphics("../man/figures/promor_ProtAnalysisFlowChart_small.png")
    
      When sourcing ‘intro_to_promor.R’:
    Error: Cannot find the file(s): "../man/figures/promor_ProtAnalysisFlowChart_small.png"
    Execution halted
    
      ‘intro_to_promor.Rmd’ using ‘UTF-8’... failed
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        data   2.5Mb
        help   1.6Mb
    ```

# protag

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/protag
* Date/Publication: 2019-08-09 10:10:02 UTC
* Number of recursive dependencies: 48

Run `revdepcheck::cloud_details(, "protag")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘protag-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: tag.search
    > ### Title: tag.search
    > ### Aliases: tag.search
    > 
    > ### ** Examples
    > 
    > search.result <- tag.search(myoglobin, delta = c(14, 28), error.Da.pair = .3)
    ...
    [1] "Found both paired peaks (mass differentiate by expected delta) and matched peaks (of the same mass)."
    
    > tag.spectra.listplot(search.result)
    
    Great plot!
    
    Error in switch(direction, vertical = c("left", "top"), horizontal = c("center",  : 
      EXPR must be a length 1 vector
    Calls: <Anonymous> ... <Anonymous> -> assemble -> <Anonymous> -> package_box -> %||%
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RColorBrewer’ ‘grDevices’
      All declared Imports should be used.
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

# psc

<details>

* Version: 1.1.0
* GitHub: https://github.com/richJJackson/psc
* Source code: https://github.com/cran/psc
* Date/Publication: 2024-12-12 21:30:02 UTC
* Number of recursive dependencies: 175

Run `revdepcheck::cloud_details(, "psc")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘psc-vignette.Rmd’
      ...
    beta    0.3524    0.1178    0.5659    0.0012    0.9988
    DIC   280.8252  273.6059  292.8416        NA        NA
    
    > plot(surv.psc)
    Warning: Ignoring unknown labels:
    • `linetype = "1"`
    
      When sourcing ‘psc-vignette.R’:
    Error: non-numeric argument to binary operator
    Execution halted
    
      ‘psc-vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘psc-vignette.Rmd’ using rmarkdown
    ```

# PSCBS

<details>

* Version: 0.67.0
* GitHub: https://github.com/HenrikBengtsson/PSCBS
* Source code: https://github.com/cran/PSCBS
* Date/Publication: 2024-02-17 19:10:02 UTC
* Number of recursive dependencies: 44

Run `revdepcheck::cloud_details(, "PSCBS")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        R         1.5Mb
        data-ex   2.6Mb
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

# qlifetable

<details>

* Version: 0.0.2-5
* GitHub: NA
* Source code: https://github.com/cran/qlifetable
* Date/Publication: 2024-06-28 07:30:07 UTC
* Number of recursive dependencies: 29

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
    > dates.b <- c("1920-05-13", "1999-04-12", "2019-01-01")
    > dates.e <- c("2002-03-23", "2009-04-12", "2019-01-01")
    > x <- quarterly_variables(dates.b, dates.e)
    > out <- time_exposed_outs(x)
    > p <- plot(out, show.plot = FALSE)
    Error in continuous_scale(aesthetics, palette = NULL, guide = guide, na.value = na.value,  : 
      unused arguments (high = "#BEBEBEFF", low = "#BEBEBE66")
    Calls: plot -> plot.qlifetable -> <Anonymous>
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
* Number of recursive dependencies: 75

Run `revdepcheck::cloud_details(, "quantities")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘introduction.Rmd’
      ...
    3.5.0.
    
    > p0 + geom_errors()
    
      When sourcing ‘introduction.R’:
    Error: Problem while converting geom to grob.
    ℹ Error occurred in the 2nd layer.
    Caused by error in `draw_panel()`:
    ! unused argument (height = NULL)
    Execution halted
    
      ‘introduction.Rmd’ using ‘UTF-8’... failed
      ‘parsing.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
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
* Number of recursive dependencies: 152

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
    > plot.grid(res, param = "mu.spline")
    Warning in fortify(data, ...) :
      Arguments in `...` must be used.
    ✖ Problematic argument:
    • col = "black"
    ℹ Did you misspell an argument name?
    Error in continuous_scale(aesthetics, palette = NULL, guide = guide, na.value = na.value,  : 
      unused arguments (low = "#00441B", high = "#E5F5E0")
    Calls: plot.grid -> <Anonymous>
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.9Mb
      sub-directories of 1Mb or more:
        R           1.5Mb
        doc         2.1Mb
        shiny_app   1.2Mb
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
    > das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
    > S <- make.S(das)
    > rv <- do.radviz(iris,S)
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
    
    > classic.S <- make.S(get.optim(classic.optim))
    
    > btcells.rv <- do.radviz(btcells.df, classic.S)
    
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

# rasterdiv

<details>

* Version: 0.3.6
* GitHub: https://github.com/mattmar/rasterdiv
* Source code: https://github.com/cran/rasterdiv
* Date/Publication: 2024-11-06 11:20:03 UTC
* Number of recursive dependencies: 102

Run `revdepcheck::cloud_details(, "rasterdiv")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘rasterdiv_04_Accumulation_Rao.Rmd’
      ...
    
    > grid.arrange(l1, l2, g1, layout_matrix = rbind(c(1, 
    +     2), c(3, 3)))
    
      When sourcing ‘rasterdiv_04_Accumulation_Rao.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 5th layer.
    Caused by error in `list_sizes()`:
    ! `x$label` must be a vector, not an expression vector.
    Execution halted
    
      ‘rasterdiv_01_Basics.Rmd’ using ‘UTF-8’... OK
      ‘rasterdiv_02_Area_based_Rao.Rmd’ using ‘UTF-8’... OK
      ‘rasterdiv_03_Advanced_multidimension_Rao.Rmd’ using ‘UTF-8’... OK
      ‘rasterdiv_04_Accumulation_Rao.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
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
    Error: Cannot find the file(s): "/tmp/RtmpJmZKYK/file14e857f46991/vignettes/vignettes/images/01-new-project.png"; "/tmp/RtmpJmZKYK/file14e857f46991/vignettes/vignettes/images/02-new-directory.png"; "/tmp/RtmpJmZKYK/file14e857f46991/vignettes/vignettes/images/03-techreport.png"; "/tmp/RtmpJmZKYK/file14e857f46991/vignettes/vignettes/images/04-name-report.png"
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

# RavenR

<details>

* Version: 2.2.2
* GitHub: https://github.com/rchlumsk/RavenR
* Source code: https://github.com/cran/RavenR
* Date/Publication: 2024-05-07 03:30:02 UTC
* Number of recursive dependencies: 133

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
    > # )
    > # metadata = metadata[metadata$start>=2000,] # subset stations with recent data
    > # metadata = metadata[1:3,] # take only the first 3 stations for brevity
    > 
    > # plot line colours by station elevation
    > rvn_met_recordplot(metadata=rvn_weathercan_metadata_sample, colorby='elev')
    Error in continuous_scale(aesthetics, palette = NULL, guide = guide, na.value = na.value,  : 
      unused arguments (high = "#56B4E9", low = "#D55E00")
    Calls: rvn_met_recordplot -> scale_color_continuous
    Execution halted
    ```

# RCTrep

<details>

* Version: 1.2.0
* GitHub: https://github.com/duolajiang/RCTrep
* Source code: https://github.com/cran/RCTrep
* Date/Publication: 2023-11-02 14:40:02 UTC
* Number of recursive dependencies: 172

Run `revdepcheck::cloud_details(, "RCTrep")` for more info

</details>

## Newly broken

*   checking whether package ‘RCTrep’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘RCTrep’
    See ‘/tmp/workdir/RCTrep/new/RCTrep.Rcheck/00install.out’ for details.
    ```

# registr

<details>

* Version: 2.1.0
* GitHub: NA
* Source code: https://github.com/cran/registr
* Date/Publication: 2022-10-02 21:40:02 UTC
* Number of recursive dependencies: 89

Run `revdepcheck::cloud_details(, "registr")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘incomplete_curves.Rmd’
      ...
    +     ggplot(dat, aes(x = index, y = value, group = id)) + geom_line(alpha = 0.2) + 
    +         xlab("t* [observed]") + ylab("Der ..." ... [TRUNCATED] 
    
    > if (have_ggplot2) {
    +     ggplot(dat, aes(x = index, y = id, col = value)) + geom_line(lwd = 2.5) + 
    +         scale_color_continuous("Derivative",  .... [TRUNCATED] 
    
      When sourcing ‘incomplete_curves.R’:
    Error: unused arguments (high = "midnightblue", low = "lightskyblue1")
    Execution halted
    
      ‘incomplete_curves.Rmd’ using ‘UTF-8’... failed
      ‘registr.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘incomplete_curves.Rmd’ using rmarkdown
    ```

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘registr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gfpca_twoStep
    > ### Title: Generalized functional principal component analysis
    > ### Aliases: gfpca_twoStep
    > 
    > ### ** Examples
    > 
    > data(growth_incomplete)
    > 
    > # estimate 2 FPCs
    > fpca_obj = gfpca_twoStep(Y = growth_incomplete, npc = 2, family = "gaussian")
    Using the first 2 FPCs which explain 73.8% of the (approximated) total variance.
    Error in initializePtr() : 
      function 'cholmod_factor_ldetA' not provided by package 'Matrix'
    Calls: gfpca_twoStep ... initialize -> <Anonymous> -> initializePtr -> .Call
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(registr)
      Warning message:
      In check_dep_version() : ABI version mismatch: 
      lme4 was built with Matrix ABI version 1
      Current Matrix ABI version is 0
      Please re-install lme4 from source or restore original 'Matrix' package
    ...
       12.         └─methods (local) `<rfMthdDf>`(...)
       13.           └─methods::new(def, ...)
       14.             ├─methods::initialize(value, ...)
       15.             └─methods::initialize(value, ...)
       16.               └─.Object$initialize(...)
       17.                 └─lme4 (local) initializePtr()
      
      [ FAIL 13 | WARN 6 | SKIP 6 | PASS 166 ]
      Error: Test failures
      Execution halted
    ```

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

# RegrCoeffsExplorer

<details>

* Version: 1.1.0
* GitHub: NA
* Source code: https://github.com/cran/RegrCoeffsExplorer
* Date/Publication: 2024-11-14 13:50:49 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::cloud_details(, "RegrCoeffsExplorer")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘BetaVisualizer.Rmd’ using rmarkdown
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

# rSAFE

<details>

* Version: 0.1.4
* GitHub: https://github.com/ModelOriented/rSAFE
* Source code: https://github.com/cran/rSAFE
* Date/Publication: 2022-08-13 13:20:02 UTC
* Number of recursive dependencies: 119

Run `revdepcheck::cloud_details(, "rSAFE")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘example_apartments.Rmd’
      ...
    
    > mp_lm2 <- model_performance(explainer_lm2)
    
    > mp_rf2 <- model_performance(explainer_rf2)
    
    > plot(mp_lm1, mp_rf1, mp_lm2, mp_rf2, geom = "boxplot")
    
      When sourcing ‘example_apartments.R’:
    Error: 'x' and 'units' must have length > 0
    Execution halted
    
      ‘example_apartments.Rmd’ using ‘UTF-8’... failed
      ‘example_hr.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘example_apartments.Rmd’ using rmarkdown
    ```

# scoringutils

<details>

* Version: 2.0.0
* GitHub: https://github.com/epiforecasts/scoringutils
* Source code: https://github.com/cran/scoringutils
* Date/Publication: 2024-10-31 20:40:02 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::cloud_details(, "scoringutils")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Deprecated-visualisations.Rmd’
      ...
    > sum_scores <- range_example %>% as_forecast_quantile() %>% 
    +     score(metrics = list(wis = wis, dispersion = dispersion_quantile)) %>% 
    +     summ .... [TRUNCATED] 
    
    > plot_interval_ranges(sum_scores, x = "model") + facet_wrap(~target_type, 
    +     scales = "free")
    
      When sourcing ‘Deprecated-visualisations.R’:
    Error: unused arguments (low = "steelblue", high = "salmon")
    Execution halted
    
      ‘Deprecated-functions.Rmd’ using ‘UTF-8’... OK
      ‘Deprecated-visualisations.Rmd’ using ‘UTF-8’... failed
      ‘scoring-rules.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Deprecated-functions.Rmd’ using rmarkdown
    --- finished re-building ‘Deprecated-functions.Rmd’
    
    --- re-building ‘Deprecated-visualisations.Rmd’ using rmarkdown
    
    Quitting from lines 422-434 [unnamed-chunk-9] (Deprecated-visualisations.Rmd)
    Error: processing vignette 'Deprecated-visualisations.Rmd' failed with diagnostics:
    unused arguments (low = "steelblue", high = "salmon")
    ...
    --- failed re-building ‘Deprecated-visualisations.Rmd’
    
    --- re-building ‘scoring-rules.Rmd’ using rmarkdown
    --- finished re-building ‘scoring-rules.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Deprecated-visualisations.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# SCOUTer

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/SCOUTer
* Date/Publication: 2020-06-30 09:30:03 UTC
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "SCOUTer")` for more info

</details>

## Newly broken

*   checking whether package ‘SCOUTer’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘SCOUTer’
    See ‘/tmp/workdir/SCOUTer/new/SCOUTer.Rcheck/00install.out’ for details.
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
    > plot_data <- shap.prep.stack.data(shap_contrib = shap_values_iris,
    +                                   n_groups = 4)
    All the features will be used.
    
    > shap.plot.force_plot(plot_data)
    Data has N = 150 | zoom in length is 50 at location 90.
    
    Error in upgradeUnit.default(x) : Not a unit object
    Calls: <Anonymous> ... is.unit -> convertUnit -> upgradeUnit -> upgradeUnit.default
    Execution halted
    ```

# shapr

<details>

* Version: 1.0.1
* GitHub: https://github.com/NorskRegnesentral/shapr
* Source code: https://github.com/cran/shapr
* Date/Publication: 2025-01-16 13:00:05 UTC
* Number of recursive dependencies: 167

Run `revdepcheck::cloud_details(, "shapr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # CRAN OMP THREAD LIMIT
      > Sys.setenv("OMP_THREAD_LIMIT" = 1)
      > 
      > library(testthat)
      > library(shapr)
      
      Attaching package: 'shapr'
    ...
          ▆
       1. ├─vdiffr::expect_doppelganger(...) at test-plot.R:272:3
       2. │ └─vdiffr (local) writer(fig, testcase, title)
       3. │   └─vdiffr:::print_plot(plot, title)
       4. └─shapr::plot_SV_several_approaches(explanation_list_named)
       5.   └─ggplot2::scale_fill_discrete(breaks = breaks, direction = direction)
      
      [ FAIL 1 | WARN 0 | SKIP 139 | PASS 38 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.9Mb
      sub-directories of 1Mb or more:
        doc    4.2Mb
        libs   6.4Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘devtools’
    ```

# simmr

<details>

* Version: 0.5.1.217
* GitHub: https://github.com/andrewcparnell/simmr
* Source code: https://github.com/cran/simmr
* Date/Publication: 2024-10-16 15:10:02 UTC
* Number of recursive dependencies: 117

Run `revdepcheck::cloud_details(, "simmr")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘simmr.Rmd’
      ...
    +     source_means = s_means, source_sds = s_sds, correction_means = c_means, 
    +  .... [TRUNCATED] 
    
    > plot(simmr_in_1D)
    Warning in geom_errorbar(mapping = mapping, data = data, stat = stat, position = position,  :
      Ignoring unknown aesthetics: height
    
      When sourcing ‘simmr.R’:
    Error: EXPR must be a length 1 vector
    Execution halted
    
      ‘advanced_plotting.Rmd’ using ‘UTF-8’... OK
      ‘quick_start.Rmd’ using ‘UTF-8’... OK
      ‘simmr.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘advanced_plotting.Rmd’ using rmarkdown
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

# sivirep

<details>

* Version: 1.0.1
* GitHub: https://github.com/epiverse-trace/sivirep
* Source code: https://github.com/cran/sivirep
* Date/Publication: 2024-12-03 23:10:02 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::cloud_details(, "sivirep")` for more info

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
      Error in `continuous_scale(aesthetics, palette = NULL, guide = guide, na.value = na.value, 
          ...)`: unused arguments (low = "#be0000", high = "#be0000")
      Backtrace:
          ▆
       1. └─sivirep::plot_map(data_agrupada = incidencia_mpio, ruta_dir = tempdir()) at test-map.R:165:3
       2.   └─ggplot2::scale_fill_continuous(...)
      
      [ FAIL 6 | WARN 0 | SKIP 0 | PASS 345 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 8 marked UTF-8 strings
    ```

# skewlmm

<details>

* Version: 1.1.2
* GitHub: https://github.com/fernandalschumacher/skewlmm
* Source code: https://github.com/cran/skewlmm
* Date/Publication: 2024-12-15 00:50:02 UTC
* Number of recursive dependencies: 81

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
    > fm1 = smn.lmm(distance ~ age+Sex, data=nlme::Orthodont,
    +               groupVar="Subject", distr="t")
    > plot(fm1)
    Error in continuous_scale(aesthetics, palette = NULL, guide = guide, na.value = na.value,  : 
      unused arguments (high = "#132B43", low = "#56B1F7")
    Calls: plot -> plot.SMN -> scale_color_continuous
    Execution halted
    ```

# smdi

<details>

* Version: 0.3.1
* GitHub: NA
* Source code: https://github.com/cran/smdi
* Date/Publication: 2024-10-04 07:10:02 UTC
* Number of recursive dependencies: 220

Run `revdepcheck::cloud_details(, "smdi")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘a_data_generation.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘a_data_generation.Rmd’
      ...
    Warning in geom_segment(aes(x = 0, y = max(y2), xend = max(x1), yend = max(y2)),  :
      All aesthetics have length 1, but the data has 2 rows.
    ℹ Please consider using `annotate()` or provide this layer with data containing
      a single row.
    Warning: Ignoring unknown labels:
    • `linetype = "1"`
    
    ...
    
      When sourcing ‘b_routine_diagnostics.R’:
    Error: Cannot find the file(s): "/tmp/Rtmpd7yyCk/file1adfbbc9cd2/vignettes/vignettes/smdi_diagnose_table.png"
    Execution halted
    
      ‘a_data_generation.Rmd’ using ‘UTF-8’... failed
      ‘b_routine_diagnostics.Rmd’ using ‘UTF-8’... failed
      ‘c_multivariate_missingness.Rmd’ using ‘UTF-8’... OK
      ‘d_narfcs_sensitivity_analysis.Rmd’ using ‘UTF-8’... OK
      ‘smdi.Rmd’ using ‘UTF-8’... OK
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
    
    > ### Name: map.ind
    > ### Title: Map the individuals of a soc.ca analysis
    > ### Aliases: map.ind
    > 
    > ### ** Examples
    > 
    > example(soc.ca)
    ...
    • `linetype = "Linetype"`
    • `size = "Size"`
    • `fill = "Fill"`
    > map  <- map.ind(result, map.title = "The contribution of the individuals with new scale",
    +  point.color = result$ctr.ind[, 1], point.shape = 18) 
    > map + scale_color_continuous(low = "white", high = "red")
    Error in continuous_scale(aesthetics, palette = NULL, guide = guide, na.value = na.value,  : 
      unused arguments (low = "white", high = "red")
    Calls: scale_color_continuous
    Execution halted
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

# SouthParkRshiny

<details>

* Version: 1.0.0
* GitHub: https://github.com/Amalan-ConStat/SouthParkRshiny
* Source code: https://github.com/cran/SouthParkRshiny
* Date/Publication: 2024-03-09 11:10:08 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::cloud_details(, "SouthParkRshiny")` for more info

</details>

## Newly broken

*   checking whether package ‘SouthParkRshiny’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘SouthParkRshiny’
    See ‘/tmp/workdir/SouthParkRshiny/new/SouthParkRshiny.Rcheck/00install.out’ for details.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.6Mb
      sub-directories of 1Mb or more:
        data   8.0Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1562 marked UTF-8 strings
    ```

# SPARRAfairness

<details>

* Version: 0.0.0.2
* GitHub: NA
* Source code: https://github.com/cran/SPARRAfairness
* Date/Publication: 2024-11-07 10:30:02 UTC
* Number of recursive dependencies: 64

Run `revdepcheck::cloud_details(, "SPARRAfairness")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘SPARRAfairness_example.Rmd’
      ...
    > obj_list = list(list(x = score_cutoffs, y = dem_par[1, 
    +     ], ci = q * dem_par[2, ]), list(x = score_cutoffs, y = dem_par[3, 
    +     ], ci = q * d .... [TRUNCATED] 
    
    > groupmetric_2panel(obj_list, labels = c("Urban", "Rural"), 
    +     col = phs_colours(c("phs-blue", "phs-magenta")), ci_col = phs_colours(c("phs-blue" .... [TRUNCATED] 
    
      When sourcing ‘SPARRAfairness_example.R’:
    Error: EXPR must be a length 1 vector
    Execution halted
    
      ‘SPARRAfairness_example.Rmd’ using ‘UTF-8’... failed
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

*   checking whether package ‘SqueakR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘SqueakR’
    See ‘/tmp/workdir/SqueakR/new/SqueakR.Rcheck/00install.out’ for details.
    ```

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
        conversion failure on '← 2@3' in 'mbcsToSbcs': dot substituted for <86>
      3: In grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
        conversion failure on '← 2@3' in 'mbcsToSbcs': dot substituted for <90>
      4: In grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
        conversion failure on '← 2@3' in 'mbcsToSbcs': dot substituted for <e2>
      5: In grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
        conversion failure on '← 2@3' in 'mbcsToSbcs': dot substituted for <86>
      6: In grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
        conversion failure on '← 2@3' in 'mbcsToSbcs': dot substituted for <90>
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
       diff| Attributes differ
      ----- FAILED[attr]: test_fitModPlots.R<205--205>
       call| expect_equal(geoms0, c("GeomPoint", "GeomLine"))
       diff| names for current but not for target
      ----- FAILED[attr]: test_fitModPlots.R<211--211>
       call| expect_equal(geoms1, c("GeomPoint"))
       diff| Attributes differ
      Error: 10 out of 651 tests failed
      In addition: There were 46 warnings (use warnings() to see them)
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

# stats19

<details>

* Version: 3.3.1
* GitHub: https://github.com/ropensci/stats19
* Source code: https://github.com/cran/stats19
* Date/Publication: 2025-01-15 08:00:02 UTC
* Number of recursive dependencies: 147

Run `revdepcheck::cloud_details(, "stats19")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘blog.Rmd’ using rmarkdown
    [WARNING] Citeproc: citation sarkar_street_2018 not found
    --- finished re-building ‘blog.Rmd’
    
    --- re-building ‘stats19-training-setup.Rmd’ using rmarkdown
    --- finished re-building ‘stats19-training-setup.Rmd’
    
    --- re-building ‘stats19-training.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... WARNING
    ```
    Errors in running code in vignettes:
    when running code in ‘blog.Rmd’
      ...
    +     cycling > 0 ~ "Cycli ..." ... [TRUNCATED] 
    
    > ggplot(crashes_types, aes(size = Total, colour = speed_limit)) + 
    +     geom_sf(show.legend = "point", alpha = 0.3) + facet_grid(vars(type), 
    +      .... [TRUNCATED] 
    
      When sourcing ‘blog.R’:
    ...
    
      When sourcing ‘stats19.R’:
    Error: unused arguments (low = "blue", high = "red")
    Execution halted
    
      ‘blog.Rmd’ using ‘UTF-8’... failed
      ‘stats19-training-setup.Rmd’ using ‘UTF-8’... OK
      ‘stats19-training.Rmd’ using ‘UTF-8’... failed
      ‘stats19-vehicles.Rmd’ using ‘UTF-8’... failed
      ‘stats19.Rmd’ using ‘UTF-8’... failed
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

# survParamSim

<details>

* Version: 0.1.6
* GitHub: https://github.com/yoshidk6/survParamSim
* Source code: https://github.com/cran/survParamSim
* Date/Publication: 2022-06-03 08:10:02 UTC
* Number of recursive dependencies: 125

Run `revdepcheck::cloud_details(, "survParamSim")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘survParamSim.Rmd’
      ...
    
    > survminer::ggsurvplot(survfit.colon)
    Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘survminer’
    Warning: Ignoring unknown labels:
    • `fill = "Strata"`
    • `linetype = "1"`
    
      When sourcing ‘survParamSim.R’:
    Error: non-numeric argument to binary operator
    Execution halted
    
      ‘survParamSim.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘survParamSim.Rmd’ using rmarkdown
    
    Quitting from lines 53-58 [plot_raw_data] (survParamSim.Rmd)
    Error: processing vignette 'survParamSim.Rmd' failed with diagnostics:
    non-numeric argument to binary operator
    --- failed re-building ‘survParamSim.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘survParamSim.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# symptomcheckR

<details>

* Version: 0.1.3
* GitHub: https://github.com/ma-kopka/symptomcheckR
* Source code: https://github.com/cran/symptomcheckR
* Date/Publication: 2024-04-16 20:40:06 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::cloud_details(, "symptomcheckR")` for more info

</details>

## Newly broken

*   checking whether package ‘symptomcheckR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘symptomcheckR’
    See ‘/tmp/workdir/symptomcheckR/new/symptomcheckR.Rcheck/00install.out’ for details.
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
    > sample_save[[1]]
    
    > sample_save[[2]]
    
      When sourcing ‘tciu-LT-kimesurface.R’:
    Error: Problem while computing layer data.
    ℹ Error occurred in the 1st layer.
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
    Problem while computing layer data.
    ℹ Error occurred in the 1st layer.
    Caused by error in `is.waive()`:
    ! could not find function "is.waive"
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

# TestGardener

<details>

* Version: 3.3.5
* GitHub: NA
* Source code: https://github.com/cran/TestGardener
* Date/Publication: 2024-09-18 17:40:02 UTC
* Number of recursive dependencies: 124

Run `revdepcheck::cloud_details(, "TestGardener")` for more info

</details>

## Newly broken

*   checking whether package ‘TestGardener’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘TestGardener’
    See ‘/tmp/workdir/TestGardener/new/TestGardener.Rcheck/00install.out’ for details.
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
        NULL, list("#EBEBEBFF", NULL, NULL, NULL, FALSE, "#EBEBEBFF", TRUE), NULL, list(NULL, 0.5, NULL, NULL, FALSE, NULL, TRUE), NULL, NULL, NULL, NULL, FALSE, NULL, NULL, list(NULL, "white", NULL, NULL, TRUE), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(5.5, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, 
            NULL, NULL, TRUE), "topleft", NULL, NULL, list("#D9D9D9FF", "#333333FF", NULL, NULL, TRUE), NULL, NULL, "on", "inside", list(NULL, NULL, "#1A1A1AFF", 0.8, NULL, NULL, NULL, NULL, c(4.4, 4.4, 4.4, 4.4), NULL, TRUE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75))
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

# track2KBA

<details>

* Version: 1.1.2
* GitHub: https://github.com/BirdLifeInternational/track2kba
* Source code: https://github.com/cran/track2KBA
* Date/Publication: 2024-07-01 10:40:07 UTC
* Number of recursive dependencies: 96

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
      ----- FAILED[xcpt]: test_mapSite.R<24--24>
       call| expect_silent(mapSite(sites2))
       diff| Execution was not silent. An error was thrown with message
       diff| 'unused arguments (high = "#132B43", low = "#56B1F7")'
      Error: 4 out of 126 tests failed
      In addition: Warning message:
      In Matching::ks.boot(WI, BW, alternative = "two.sided", nboots = iterations) :
        For publication quality p-values it is recommended that 'nboots'
       be set equal to at least 500 (preferably 1000)
      Execution halted
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
            2, list("white", NA, NULL, NULL, TRUE), list(), NULL, NULL, NULL, list("#DEDEDEFF", NULL, NULL, NULL, FALSE, "#DEDEDEFF", TRUE), list(), list(), NULL, NULL, NULL, NULL, FALSE, NULL, NULL, list(NULL, "white", NULL, NULL, TRUE), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(5.5, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 
                1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, TRUE), "topleft", NULL, NULL, list("gray90", NA, NULL, NULL, FALSE), NULL, NULL, "on", "inside", list(NULL, NULL, "black", 0.8, NULL, NULL, NULL, NULL, c(6, 6, 6, 6), NULL, FALSE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75))
      
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
    More info about dataset please run following commands:
      library(UCSCXenaTools)
      XenaGenerate(subset = XenaDatasets == "TcgaTargetGtex_rsem_gene_tpm") %>% XenaBrowse()
    
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
    > 
    > main = system.file('extdata', package = 'umiAnalyzer')
    > samples <- list.dirs(path = main, full.names = FALSE, recursive = FALSE)
    > simsen <- createUmiExperiment(experimentName = 'example',mainDir = main,sampleNames = samples)
    > simsen <- filterUmiObject(simsen)
    > 
    > amplicon_plot <- AmpliconPlot(simsen)
    Error in pm[[2]] : subscript out of bounds
    Calls: AmpliconPlot -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# umx

<details>

* Version: 4.21.0
* GitHub: https://github.com/tbates/umx
* Source code: https://github.com/cran/umx
* Date/Publication: 2024-11-15 03:30:02 UTC
* Number of recursive dependencies: 170

Run `revdepcheck::cloud_details(, "umx")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘umx-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: umxDiffMZ
    > ### Title: MZ differences method for testing evidence for causality.
    > ### Aliases: umxDiffMZ
    > 
    > ### ** Examples
    > 
    > data(twinData)
    ...
     15. │         └─l$compute_geom_2(d, theme = plot$theme)
     16. │           └─ggplot2 (local) compute_geom_2(..., self = self)
     17. │             └─self$geom$use_defaults(...)
     18. │               └─ggplot2 (local) use_defaults(..., self = self)
     19. │                 └─ggplot2:::check_aesthetics(new_params, nrow(data))
     20. │                   └─vctrs::list_sizes(x)
     21. └─vctrs:::stop_scalar_type(...)
     22.   └─vctrs:::stop_vctrs(...)
     23.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library("testthat")
      > library("umx")
      Loading required package: OpenMx
      To take full advantage of multiple cores, use:
        mxOption(key='Number of Threads', value=parallel::detectCores()) #now
        Sys.setenv(OMP_NUM_THREADS=parallel::detectCores()) #before library(OpenMx)
      For an overview type '?umx'
    ...
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test_umx_DOC_etc.r:12:2'): testing umxDiffMZ ────────────────────────
      Error in `annotate("text", x = labxy[1], y = labxy[2], label = blurb)`: Problem while setting up geom aesthetics.
      ℹ Error occurred in the 4th layer.
      Caused by error in `list_sizes()`:
      ! `x$label` must be a vector, not a call.
      
      [ FAIL 1 | WARN 8 | SKIP 25 | PASS 352 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      'DiagrammeRsvg', 'rsvg'
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.6Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        help   4.5Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘foreign’ ‘psychTools’ ‘pwr’ ‘rmarkdown’
      All declared Imports should be used.
    ```

# UniprotR

<details>

* Version: 2.4.0
* GitHub: https://github.com/Proteomicslab57357/UniprotR
* Source code: https://github.com/cran/UniprotR
* Date/Publication: 2024-03-05 15:10:02 UTC
* Number of recursive dependencies: 190

Run `revdepcheck::cloud_details(, "UniprotR")` for more info

</details>

## Newly broken

*   checking whether package ‘UniprotR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘UniprotR’
    See ‘/tmp/workdir/UniprotR/new/UniprotR.Rcheck/00install.out’ for details.
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

# VALERIE

<details>

* Version: 1.1.0
* GitHub: NA
* Source code: https://github.com/cran/VALERIE
* Date/Publication: 2020-07-10 10:20:13 UTC
* Number of recursive dependencies: 132

Run `revdepcheck::cloud_details(, "VALERIE")` for more info

</details>

## Newly broken

*   checking whether package ‘VALERIE’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘VALERIE’
    See ‘/tmp/workdir/VALERIE/new/VALERIE.Rcheck/00install.out’ for details.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.5Mb
      sub-directories of 1Mb or more:
        extdata   8.7Mb
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

# vannstats

<details>

* Version: 1.5.1.8
* GitHub: NA
* Source code: https://github.com/cran/vannstats
* Date/Publication: 2025-01-08 20:10:02 UTC
* Number of recursive dependencies: 122

Run `revdepcheck::cloud_details(, "vannstats")` for more info

</details>

## Newly broken

*   checking whether package ‘vannstats’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘vannstats’
    See ‘/tmp/workdir/vannstats/new/vannstats.Rcheck/00install.out’ for details.
    ```

# vici

<details>

* Version: 0.7.3
* GitHub: https://github.com/sistm/vici
* Source code: https://github.com/cran/vici
* Date/Publication: 2024-02-02 16:20:02 UTC
* Number of recursive dependencies: 117

Run `revdepcheck::cloud_details(, "vici")` for more info

</details>

## Newly broken

*   checking whether package ‘vici’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::theme_transparent’ by ‘ggpubr::theme_transparent’ when loading ‘vici’
    See ‘/tmp/workdir/vici/new/vici.Rcheck/00install.out’ for details.
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

# voi

<details>

* Version: 1.0.3
* GitHub: https://github.com/chjackson/voi
* Source code: https://github.com/cran/voi
* Date/Publication: 2024-09-16 11:30:02 UTC
* Number of recursive dependencies: 121

Run `revdepcheck::cloud_details(, "voi")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘plots.Rmd’
      ...
    
    > n_use <- c(250, 500, 1000)
    
    > chemo_evsi_or %>% filter(n %in% n_use) %>% ggplot(aes(x = k, 
    +     y = evsi, group = n, color = n)) + geom_line(lwd = 1.5) + 
    +     scale_colour_bi .... [TRUNCATED] 
    
      When sourcing ‘plots.R’:
    Error: unused arguments (low = "gray80", high = "gray20")
    Execution halted
    
      ‘plots.Rmd’ using ‘UTF-8’... failed
      ‘voi.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘plots.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        data   3.5Mb
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

# voteogram

<details>

* Version: 0.3.2
* GitHub: https://github.com/hrbrmstr/voteogram
* Source code: https://github.com/cran/voteogram
* Date/Publication: 2023-03-08 12:20:02 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::cloud_details(, "voteogram")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘intro_to_voteogram.Rmd’
      ...
    
    > house_carto(rep, "gt") + labs(x = NULL, y = NULL, 
    +     title = "House Vote 256 - Passes American Health Care Act,\nRepealing Obamacare") + 
    +      .... [TRUNCATED] 
    
    > senate_carto(sen) + theme_voteogram(legend = FALSE)
    
      When sourcing ‘intro_to_voteogram.R’:
    Error: EXPR must be a length 1 vector
    Execution halted
    
      ‘intro_to_voteogram.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘intro_to_voteogram.Rmd’ using rmarkdown
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

# wpa

<details>

* Version: 1.9.1
* GitHub: https://github.com/microsoft/wpa
* Source code: https://github.com/cran/wpa
* Date/Publication: 2024-06-06 13:20:02 UTC
* Number of recursive dependencies: 120

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
          ▆
       1. ├─wpa::flex_index(em_data, signals = "meetings", return = "plot") at test-flex_index.R:19:3
       2. │ └─wpa::plot_flex_index(...)
       3. │   └─plot_data_long %>% ...
       4. └─wpa::plot_hourly_pat(...)
       5.   └─ggplot2::scale_fill_continuous(...)
      
      [ FAIL 1 | WARN 250 | SKIP 0 | PASS 9 ]
      Error: Test failures
      Execution halted
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
      
      Attaching package: 'xpose'
      
      The following object is masked from 'package:stats':
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
      ── Failure ('test-xplot_boxplot.R:21:3'): xplot_boxplot ────────────────────────
      has_outliers(def_bp) is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      
      [ FAIL 1 | WARN 0 | SKIP 7 | PASS 660 ]
      Error: Test failures
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

# xray

<details>

* Version: 0.2
* GitHub: https://github.com/sicarul/xray
* Source code: https://github.com/cran/xray
* Date/Publication: 2017-12-08 05:15:59 UTC
* Number of recursive dependencies: 37

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
      task 1 failed - "argument 5 matches multiple formal arguments"
    Calls: distributions -> %do% -> <Anonymous>
    Execution halted
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
    ismtrc> library(magrittr)
    
    ismtrc> library(ggplot2)
    
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

