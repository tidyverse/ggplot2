# accSDA

<details>

* Version: 1.1.3
* GitHub: https://github.com/gumeo/accSDA
* Source code: https://github.com/cran/accSDA
* Date/Publication: 2024-03-06 18:50:02 UTC
* Number of recursive dependencies: 29

Run `revdepcheck::cloud_details(, "accSDA")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘accSDA-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ASDABarPlot
    > ### Title: barplot for ASDA objects
    > ### Aliases: ASDABarPlot
    > 
    > ### ** Examples
    > 
    >     # Generate and ASDA object with your data, e.g.
    ...
      3.     ├─base::do.call(arrangeGrob, c(list(grobs = groups[[g]]), params))
      4.     └─gridExtra (local) `<fn>`(grobs = `<list>`, layout_matrix = `<int[,1]>`)
      5.       └─base::lapply(grobs[toconv], ggplot2::ggplotGrob)
      6.         └─ggplot2 (local) FUN(X[[i]], ...)
      7.           ├─ggplot2::ggplot_gtable(ggplot_build(x))
      8.           └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
      9.             └─ggplot2::calc_element("plot.margin", theme)
     10.               └─cli::cli_abort(...)
     11.                 └─rlang::abort(...)
    Execution halted
    ```

# activAnalyzer

<details>

* Version: 2.1.1
* GitHub: https://github.com/pydemull/activAnalyzer
* Source code: https://github.com/cran/activAnalyzer
* Date/Publication: 2024-05-05 22:40:03 UTC
* Number of recursive dependencies: 152

Run `revdepcheck::cloud_details(, "activAnalyzer")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘activAnalyzer.Rmd’
      ...
    > p3 <- accum_metrics_sed$p_UBD
    
    > p4 <- accum_metrics_sed$p_gini
    
    > (p1 | p2)/(p3 | p4) + plot_layout(guides = "collect") & 
    +     theme(legend.position = "bottom")
    
      When sourcing ‘activAnalyzer.R’:
    Error: object is not a unit
    Execution halted
    
      ‘activAnalyzer.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
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

* Version: 1.4.0
* GitHub: https://github.com/mattheaphy/actxps
* Source code: https://github.com/cran/actxps
* Date/Publication: 2023-11-26 16:10:02 UTC
* Number of recursive dependencies: 131

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
    
    Quitting from lines 131-132 [plot] (actxps.Rmd)
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

# add2ggplot

<details>

* Version: 0.3.0
* GitHub: https://github.com/JiaxiangBU/add2ggplot
* Source code: https://github.com/cran/add2ggplot
* Date/Publication: 2020-02-07 11:50:02 UTC
* Number of recursive dependencies: 53

Run `revdepcheck::cloud_details(, "add2ggplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘add2ggplot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: theme_ilo
    > ### Title: One ggplot theme
    > ### Aliases: theme_ilo
    > 
    > ### ** Examples
    > 
    > datasets::mtcars %>%
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘intro.Rmd’
      ...
    
    > mtcars %>% ggplot2::ggplot(ggplot2::aes(mpg, disp)) + 
    +     ggplot2::geom_point() + theme_grey_and_red()
    
    > mtcars %>% ggplot2::ggplot(ggplot2::aes(mpg, disp)) + 
    +     ggplot2::geom_point() + theme_ilo()
    
      When sourcing ‘intro.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
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

# AeRobiology

<details>

* Version: 2.0.1
* GitHub: NA
* Source code: https://github.com/cran/AeRobiology
* Date/Publication: 2019-06-03 06:20:03 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "AeRobiology")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘my-vignette.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘my-vignette.Rmd’
      ...
    +     export.plot = FALSE, export.result = FALSE, n.types = 3, 
    +     y.start = 2011, y.end = .... [TRUNCATED] 
    
    > iplot_abundance(munich_pollen, interpolation = FALSE, 
    +     export.plot = FALSE, export.result = FALSE, n.types = 3, 
    +     y.start = 2011, y.end = .... [TRUNCATED] 
    
      When sourcing ‘my-vignette.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘my-vignette.Rmd’ using ‘UTF-8’... failed
    ```

# afex

<details>

* Version: 1.3-1
* GitHub: https://github.com/singmann/afex
* Source code: https://github.com/cran/afex
* Date/Publication: 2024-02-25 14:40:02 UTC
* Number of recursive dependencies: 226

Run `revdepcheck::cloud_details(, "afex")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘afex_plot_supported_models.Rmd’
      ...
    
    > grid::grid.draw(b34)
    
      When sourcing ‘afex_plot_supported_models.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `compute_geom_2()`:
    ...
        14, NULL, NULL, list(), 15.4, NULL, NULL, 7, NULL, NULL, NULL, NULL, 0.2, NULL, list(NULL, NULL, NULL, 0.857142857142857, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, "none", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, c(0, 0, 0, 0), list(), 14, li
    Execution halted
    
      ‘afex_analysing_accuracy_data.Rmd’ using ‘UTF-8’... OK
      ‘afex_anova_example.Rmd’ using ‘UTF-8’... OK
      ‘afex_mixed_example.Rmd’ using ‘UTF-8’... OK
      ‘afex_plot_introduction.Rmd’ using ‘UTF-8’... OK
      ‘afex_plot_supported_models.Rmd’ using ‘UTF-8’... failed
      ‘assumptions_of_ANOVAs.Rmd’ using ‘UTF-8’... OK
      ‘introduction-mixed-models.pdf.asis’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
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
    
    > ### Name: DBC
    > ### Title: Analysis: Randomized block design
    > ### Aliases: DBC
    > ### Keywords: DBC Experimental
    > 
    > ### ** Examples
    > 
    ...
     12.         │ └─base::withCallingHandlers(...)
     13.         └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     14.           └─l$compute_geom_2(d, theme = plot$theme)
     15.             └─ggplot2 (local) compute_geom_2(..., self = self)
     16.               └─self$geom$use_defaults(...)
     17.                 └─ggplot2 (local) use_defaults(..., self = self)
     18.                   └─ggplot2:::check_aesthetics(new_params, nrow(data))
     19.                     └─cli::cli_abort(...)
     20.                       └─rlang::abort(...)
    Execution halted
    ```

# allMT

<details>

* Version: 0.1.0
* GitHub: https://github.com/tmungle/allMT
* Source code: https://github.com/cran/allMT
* Date/Publication: 2023-04-20 17:32:33 UTC
* Number of recursive dependencies: 144

Run `revdepcheck::cloud_details(, "allMT")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘allMT-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: compare_cohorts
    > ### Title: Plot summarized maintenance therapy (MT) data to compare two or
    > ###   more cohorts
    > ### Aliases: compare_cohorts
    > 
    > ### ** Examples
    > 
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

# AnalysisLin

<details>

* Version: 0.1.2
* GitHub: NA
* Source code: https://github.com/cran/AnalysisLin
* Date/Publication: 2024-01-30 00:10:10 UTC
* Number of recursive dependencies: 120

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

# aplot

<details>

* Version: 0.2.2
* GitHub: https://github.com/YuLab-SMU/aplot
* Source code: https://github.com/cran/aplot
* Date/Publication: 2023-10-06 04:30:02 UTC
* Number of recursive dependencies: 53

Run `revdepcheck::cloud_details(, "aplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘aplot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: insert_left
    > ### Title: plot-insertion
    > ### Aliases: insert_left insert_right insert_top insert_bottom
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
    > ap
    > ap[2, 1] <- ap[2, 1] + theme_bw()
    > ap[2, 1] <- ap[2, 1] + 
    +             aes(color = as.factor(am)) + 
    +             scale_color_manual(values = c('steelblue', 'darkgreen'))
    > ap[1, 1] <- ap[1, 1] + theme(axis.line.x.bottom=element_line())
    > ap
    Error in identicalUnits(x) : object is not a unit
    Calls: <Anonymous> ... assemble_guides -> guides_build -> unit.c -> identicalUnits
    Execution halted
    ```

# ASRgenomics

<details>

* Version: 1.1.4
* GitHub: NA
* Source code: https://github.com/cran/ASRgenomics
* Date/Publication: 2024-01-29 21:20:02 UTC
* Number of recursive dependencies: 132

Run `revdepcheck::cloud_details(, "ASRgenomics")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ASRgenomics-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: kinship.heatmap
    > ### Title: Enhanced heatmap plot for a kinship matrix K
    > ### Aliases: kinship.heatmap
    > 
    > ### ** Examples
    > 
    > # Get G matrix.
    ...
      7.       ├─gtable::gtable_filter(...)
      8.       │ └─base::grepl(pattern, .subset2(x$layout, "name"), fixed = fixed)
      9.       │   └─base::is.factor(x)
     10.       └─ggplot2::ggplotGrob(gg.left)
     11.         ├─ggplot2::ggplot_gtable(ggplot_build(x))
     12.         └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     13.           └─ggplot2::calc_element("plot.margin", theme)
     14.             └─cli::cli_abort(...)
     15.               └─rlang::abort(...)
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
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-kinshipheat.R:11:3'): kinship heatmap works ──────────────────
      Expected `... <- NULL` to run without any errors.
      i Actually got a <rlang_error> with text:
        Theme element `plot.margin` must have class <margin/rel>.
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 263 ]
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

# auditor

<details>

* Version: 1.3.5
* GitHub: https://github.com/ModelOriented/auditor
* Source code: https://github.com/cran/auditor
* Date/Publication: 2023-10-30 15:40:07 UTC
* Number of recursive dependencies: 87

Run `revdepcheck::cloud_details(, "auditor")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘auditor-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_lift
    > ### Title: LIFT Chart
    > ### Aliases: plot_lift plotLIFT
    > 
    > ### ** Examples
    > 
    > data(titanic_imputed, package = "DALEX")
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘model_evaluation_audit.Rmd’
      ...
    > plot(eva_glm, eva_rf, type = "lift")
    Warning: The `guide` argument in `scale_*()` cannot be `FALSE`. This was deprecated in
    ggplot2 3.3.4.
    ℹ Please use "none" instead.
    ℹ The deprecated feature was likely used in the auditor package.
      Please report the issue at <https://github.com/ModelOriented/auditor/issues>.
    
      When sourcing ‘model_evaluation_audit.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘model_evaluation_audit.Rmd’ using ‘UTF-8’... failed
      ‘model_fit_audit.Rmd’ using ‘UTF-8’... OK
      ‘model_performance_audit.Rmd’ using ‘UTF-8’... OK
      ‘model_residuals_audit.Rmd’ using ‘UTF-8’... OK
      ‘observation_influence_audit.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘model_evaluation_audit.Rmd’ using knitr
    ```

# augmentedRCBD

<details>

* Version: 0.1.7
* GitHub: https://github.com/aravind-j/augmentedRCBD
* Source code: https://github.com/cran/augmentedRCBD
* Date/Publication: 2023-08-19 00:12:38 UTC
* Number of recursive dependencies: 128

Run `revdepcheck::cloud_details(, "augmentedRCBD")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘augmentedRCBD-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: augmentedRCBD.bulk
    > ### Title: Analysis of Augmented Randomised Complete Block Design for
    > ###   Multiple Traits/Characters
    > ### Aliases: augmentedRCBD.bulk
    > 
    > ### ** Examples
    > 
    ...
      2.   ├─base::withCallingHandlers(...)
      3.   └─augmentedRCBD::freqdist.augmentedRCBD(...)
      4.     ├─base::rbind(ggplotGrob(G2), ggplotGrob(G1), size = "max")
      5.     └─ggplot2::ggplotGrob(G2)
      6.       ├─ggplot2::ggplot_gtable(ggplot_build(x))
      7.       └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
      8.         └─ggplot2::calc_element("plot.margin", theme)
      9.           └─cli::cli_abort(...)
     10.             └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Data_Analysis_with_augmentedRCBD.Rmd’ using rmarkdown_notangle
    trying URL 'https://www.r-project.org/logo/Rlogo.png'
    Content type 'image/png' length 48148 bytes (47 KB)
    ==================================================
    downloaded 47 KB
    
    trying URL 'https://raw.githubusercontent.com/aravind-j/augmentedRCBD/master/vignettes/rbase.png'
    Content type 'image/png' length 57299 bytes (55 KB)
    ==================================================
    ...
    Quitting from lines 970-977 [unnamed-chunk-70] (Data_Analysis_with_augmentedRCBD.Rmd)
    Error: processing vignette 'Data_Analysis_with_augmentedRCBD.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘Data_Analysis_with_augmentedRCBD.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Data_Analysis_with_augmentedRCBD.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## Newly fixed

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Data_Analysis_with_augmentedRCBD.Rmd’ using rmarkdown_notangle
    trying URL 'https://www.r-project.org/logo/Rlogo.png'
    Content type 'image/png' length 48148 bytes (47 KB)
    ==================================================
    downloaded 47 KB
    
    trying URL 'https://raw.githubusercontent.com/aravind-j/augmentedRCBD/master/vignettes/rbase.png'
    Content type 'image/png' length 57299 bytes (55 KB)
    ==================================================
    ...
    
    Error: processing vignette 'Data_Analysis_with_augmentedRCBD.Rmd' failed with diagnostics:
    LaTeX failed to compile /tmp/workdir/augmentedRCBD/old/augmentedRCBD.Rcheck/vign_test/augmentedRCBD/vignettes/Data_Analysis_with_augmentedRCBD.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See Data_Analysis_with_augmentedRCBD.log for more info.
    --- failed re-building ‘Data_Analysis_with_augmentedRCBD.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Data_Analysis_with_augmentedRCBD.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# autoplotly

<details>

* Version: 0.1.4
* GitHub: https://github.com/terrytangyuan/autoplotly
* Source code: https://github.com/cran/autoplotly
* Date/Publication: 2021-04-18 06:50:11 UTC
* Number of recursive dependencies: 88

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

# baggr

<details>

* Version: 0.7.8
* GitHub: https://github.com/wwiecek/baggr
* Source code: https://github.com/cran/baggr
* Date/Publication: 2024-02-12 18:20:02 UTC
* Number of recursive dependencies: 104

Run `revdepcheck::cloud_details(, "baggr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘baggr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: baggr_plot
    > ### Title: Plotting method in baggr package
    > ### Aliases: baggr_plot
    > 
    > ### ** Examples
    > 
    > fit <- baggr(schools, pooling = "none")
    Automatically chose Rubin model with aggregate data based on input data.
    Setting prior for mean in each group using 10 times the max effect :
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘baggr.Rmd’
      ...
    [1] -1.866291
    
    > my_baggr_comparison <- baggr_compare(schools)
    There is no predicted effect when pooling = 'none'.
    
    > plot(my_baggr_comparison) + ggtitle("8 schools: model comparison")
    
      When sourcing ‘baggr.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘baggr.Rmd’ using ‘UTF-8’... failed
      ‘baggr_binary.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘baggr.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 195.7Mb
      sub-directories of 1Mb or more:
        libs  193.9Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# bayefdr

<details>

* Version: 0.2.1
* GitHub: https://github.com/VallejosGroup/bayefdr
* Source code: https://github.com/cran/bayefdr
* Date/Publication: 2022-10-26 19:35:06 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "bayefdr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘bayefdr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: traceplot
    > ### Title: Trace, marginal density histogram, and autocorrelation plot of
    > ###   MCMC draws.
    > ### Aliases: traceplot
    > 
    > ### ** Examples
    > 
    ...
        ▆
     1. └─bayefdr::traceplot(x)
     2.   └─ggExtra::ggMarginal(p1, type = "histogram", margins = "y")
     3.     └─ggplot2::ggplotGrob(scatP)
     4.       ├─ggplot2::ggplot_gtable(ggplot_build(x))
     5.       └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     6.         └─ggplot2::calc_element("plot.margin", theme)
     7.           └─cli::cli_abort(...)
     8.             └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(bayefdr)
      > 
      > test_check("bayefdr")
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 14 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
        6.     └─ggplot2::ggplotGrob(scatP)
        7.       ├─ggplot2::ggplot_gtable(ggplot_build(x))
        8.       └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
        9.         └─ggplot2::calc_element("plot.margin", theme)
       10.           └─cli::cli_abort(...)
       11.             └─rlang::abort(...)
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 14 ]
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
    Error: unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 14, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, FALSE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, 
        NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, li
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
      installed size is 84.5Mb
      sub-directories of 1Mb or more:
        data   1.5Mb
        libs  82.3Mb
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
* Number of recursive dependencies: 107

Run `revdepcheck::cloud_details(, "BayesianReasoning")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘PPV_NPV.Rmd’
      ...
    ℹ Please consider using `annotate()` or provide this layer with data containing
      a single row.
    Warning in ggforce::geom_mark_rect(aes(label = paste0(translated_labels$label_PPV_NPV,  :
      All aesthetics have length 1, but the data has 10201 rows.
    ℹ Please consider using `annotate()` or provide this layer with data containing
      a single row.
    
      When sourcing ‘PPV_NPV.R’:
    Error: object is not coercible to a unit
    Execution halted
    
      ‘PPV_NPV.Rmd’ using ‘UTF-8’... failed
      ‘introduction.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘PPV_NPV.Rmd’ using rmarkdown
    ```

# bayestestR

<details>

* Version: 0.13.2
* GitHub: https://github.com/easystats/bayestestR
* Source code: https://github.com/cran/bayestestR
* Date/Publication: 2024-02-12 11:40:02 UTC
* Number of recursive dependencies: 186

Run `revdepcheck::cloud_details(, "bayestestR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘bayestestR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: bayesfactor_restricted
    > ### Title: Bayes Factors (BF) for Order Restricted Models
    > ### Aliases: bayesfactor_restricted bf_restricted
    > ###   bayesfactor_restricted.stanreg bayesfactor_restricted.brmsfit
    > ###   bayesfactor_restricted.blavaan bayesfactor_restricted.emmGrid
    > ###   as.logical.bayesfactor_restricted
    > 
    ...
    + )
    > 
    > 
    > (b <- bayesfactor_restricted(posterior, hypothesis = hyps, prior = prior))
    Bayes Factor (Order-Restriction)
    
    Hypothesis    P(Prior) P(Posterior)    BF
    A > B & B > C     0.16         0.23  1.39
    A > B & A > C     0.36         0.59  1.61
    C > A             0.46         0.34 0.742
    ```

## In both

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(bayestestR)
      > 
      > test_check("bayestestR")
      Starting 2 test processes
      [ FAIL 2 | WARN 3 | SKIP 75 | PASS 186 ]
      
    ...
       14.                 └─brms:::eval2(call, envir = args, enclos = envir)
       15.                   └─base::eval(expr, envir, ...)
       16.                     └─base::eval(expr, envir, ...)
       17.                       └─rstan (local) .fun(model_code = .x1)
       18.                         └─rstan:::cxxfunctionplus(...)
       19.                           └─base::sink(type = "output")
      
      [ FAIL 2 | WARN 3 | SKIP 75 | PASS 186 ]
      Error: Test failures
      Execution halted
    ```

# bdots

<details>

* Version: 1.2.5
* GitHub: https://github.com/collinn/bdots
* Source code: https://github.com/cran/bdots
* Date/Publication: 2023-01-06 23:20:02 UTC
* Number of recursive dependencies: 53

Run `revdepcheck::cloud_details(, "bdots")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘bdots.Rmd’
      ...
    Adjusted alpha: 0.01182815 
    Significant Intervals at adjusted alpha:
         [,1] [,2]
    [1,]  556  940
    
    > plot(boot1)
    
      When sourcing ‘bdots.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘bdots.Rmd’ using ‘UTF-8’... failed
      ‘correlations.Rmd’ using ‘UTF-8’... OK
      ‘customCurves.Rmd’ using ‘UTF-8’... OK
      ‘refitCoef.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘bdots.Rmd’ using rmarkdown
    ```

# bdrc

<details>

* Version: 1.1.0
* GitHub: https://github.com/sor16/bdrc
* Source code: https://github.com/cran/bdrc
* Date/Publication: 2023-03-19 17:10:03 UTC
* Number of recursive dependencies: 75

Run `revdepcheck::cloud_details(, "bdrc")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘tournament.Rmd’
      ...
    3     1    2   plm -8.903540      4.249257 26.305595 -0.3185195  FALSE
    4     1    2  plm0 -8.873488      4.120050 25.987075         NA   TRUE
    5     2    3 gplm0  5.884914      6.692781  1.615733 24.3713418   TRUE
    6     2    3  plm0 -8.873488      4.120050 25.987075         NA  FALSE
    
    > plot(t_obj)
    
      When sourcing ‘tournament.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘background.Rmd’ using ‘UTF-8’... OK
      ‘introduction.Rmd’ using ‘UTF-8’... OK
      ‘tournament.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘background.Rmd’ using rmarkdown
    --- finished re-building ‘background.Rmd’
    
    --- re-building ‘introduction.Rmd’ using rmarkdown
    ```

# BeeBDC

<details>

* Version: 1.1.1
* GitHub: https://github.com/jbdorey/BeeBDC
* Source code: https://github.com/cran/BeeBDC
* Date/Publication: 2024-04-03 23:53:03 UTC
* Number of recursive dependencies: 219

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
        8.         └─ggplot2:::ggplot_gtable.ggplot_built(data)
        9.           └─ggplot2::calc_element("plot.margin", theme)
       10.             └─cli::cli_abort(...)
       11.               └─rlang::abort(...)
      
      [ FAIL 1 | WARN 4 | SKIP 0 | PASS 235 ]
      Error: Test failures
      Execution halted
      Warning message:
      Connection is garbage-collected, use dbDisconnect() to avoid this. 
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘BeeBDC_main.Rmd’
      ...
    
    > rm(testChecklist)
    
    > check_space <- BeeBDC::countryOutlieRs(checklist = checklistFile, 
    +     data = check_space, keepAdjacentCountry = TRUE, pointBuffer = 0.05, 
    +      .... [TRUNCATED] 
    
      When sourcing ‘BeeBDC_main.R’:
    Error: object 'checklistFile' not found
    Execution halted
    
      ‘BeeBDC_main.Rmd’ using ‘UTF-8’... failed
      ‘basic_workflow.Rmd’ using ‘UTF-8’... OK
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 107 marked UTF-8 strings
    ```

# besthr

<details>

* Version: 0.3.2
* GitHub: NA
* Source code: https://github.com/cran/besthr
* Date/Publication: 2023-04-14 08:50:08 UTC
* Number of recursive dependencies: 67

Run `revdepcheck::cloud_details(, "besthr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘besthr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.hrest
    > ### Title: plots the 'hrest' object
    > ### Aliases: plot.hrest
    > 
    > ### ** Examples
    > 
    > 
    >  d1 <- make_data()
    >  hr_est <- estimate(d1, score, group)
    >  plot(hr_est)
    Picking joint bandwidth of 0.68
    Error in as.unit(value) : object is not coercible to a unit
    Calls: <Anonymous> ... assemble_guides -> guides_build -> [<- -> [<-.unit -> as.unit
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘basic-use.Rmd’
      ...
    Confidence Intervals (0.025, 0.975)
     4.07125, 8.62625
    
    100 bootstrap resamples.
    > plot(hr_est_1)
    Picking joint bandwidth of 0.412
    
      When sourcing ‘basic-use.R’:
    Error: object is not coercible to a unit
    Execution halted
    
      ‘basic-use.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘basic-use.Rmd’ using rmarkdown
    
    Quitting from lines 34-44 [unnamed-chunk-2] (basic-use.Rmd)
    Error: processing vignette 'basic-use.Rmd' failed with diagnostics:
    object is not coercible to a unit
    --- failed re-building ‘basic-use.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘basic-use.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# BetaPASS

<details>

* Version: 1.1-2
* GitHub: NA
* Source code: https://github.com/cran/BetaPASS
* Date/Publication: 2023-10-18 21:00:08 UTC
* Number of recursive dependencies: 58

Run `revdepcheck::cloud_details(, "BetaPASS")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘BetaPASS-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: betapower
    > ### Title: Find Power with Beta distribution
    > ### Aliases: betapower
    > 
    > ### ** Examples
    > 
    > BPmat <- betapower(mu0 = 0.56, sd0 = 0.255, mu1.start = .70, mu1.end = .75, mu1.by = .05, 
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘BetaPASS.Rmd’
      ...
    |                  0.825|    0.775|          45| 0.70|
    |                  0.975|    0.975|          45| 0.75|
    |                  0.925|    0.875|          50| 0.70|
    |                  1.000|    0.975|          50| 0.75|
    
    > plot(Power.mat, link.type = "logit", by = "mu1")
    
      When sourcing ‘BetaPASS.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘BetaPASS.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘BetaPASS.Rmd’ using rmarkdown
    
    Quitting from lines 101-102 [unnamed-chunk-4] (BetaPASS.Rmd)
    Error: processing vignette 'BetaPASS.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘BetaPASS.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘BetaPASS.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# biblioverlap

<details>

* Version: 1.0.2
* GitHub: https://github.com/gavieira/biblioverlap
* Source code: https://github.com/cran/biblioverlap
* Date/Publication: 2023-11-07 19:50:02 UTC
* Number of recursive dependencies: 93

Run `revdepcheck::cloud_details(, "biblioverlap")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘biblioverlap-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_upset
    > ### Title: Plotting UpSet plot from biblioverlap results
    > ### Aliases: plot_upset
    > 
    > ### ** Examples
    > 
    > #Running document-level matching procedure
    ...
      3.     ├─base::suppressMessages(...)
      4.     │ └─base::withCallingHandlers(...)
      5.     └─UpSetR:::Make_main_bar(...)
      6.       └─ggplot2::ggplotGrob(Main_bar_plot)
      7.         ├─ggplot2::ggplot_gtable(ggplot_build(x))
      8.         └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
      9.           └─ggplot2::calc_element("plot.margin", theme)
     10.             └─cli::cli_abort(...)
     11.               └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 573 marked UTF-8 strings
    ```

# biscale

<details>

* Version: 1.0.0
* GitHub: https://github.com/chris-prener/biscale
* Source code: https://github.com/cran/biscale
* Date/Publication: 2022-05-27 08:40:09 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::cloud_details(, "biscale")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘biscale-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: bi_legend
    > ### Title: Create Object for Drawing Legend
    > ### Aliases: bi_legend
    > 
    > ### ** Examples
    > 
    > # sample 3x3 legend
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘biscale.Rmd’
      ...
    
    > knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
    
    > knitr::include_graphics("../man/figures/biscale.001.jpeg")
    
      When sourcing ‘biscale.R’:
    Error: Cannot find the file(s): "../man/figures/biscale.001.jpeg"
    ...
    > knitr::include_graphics("../man/figures/raster.jpeg")
    
      When sourcing ‘rasters.R’:
    Error: Cannot find the file(s): "../man/figures/raster.jpeg"
    Execution halted
    
      ‘biscale.Rmd’ using ‘UTF-8’... failed
      ‘bivariate_palettes.Rmd’ using ‘UTF-8’... failed
      ‘breaks.Rmd’ using ‘UTF-8’... failed
      ‘rasters.Rmd’ using ‘UTF-8’... failed
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘stats’ ‘utils’
      All declared Imports should be used.
    ```

# BlandAltmanLeh

<details>

* Version: 0.3.1
* GitHub: NA
* Source code: https://github.com/cran/BlandAltmanLeh
* Date/Publication: 2015-12-23 23:32:17
* Number of recursive dependencies: 63

Run `revdepcheck::cloud_details(, "BlandAltmanLeh")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Intro.Rmd’
      ...
    > b <- 0.02 * a + 0.3 * rnorm(150)
    
    > library(ggExtra)
    
    > print(ggMarginal(bland.altman.plot(a, b, graph.sys = "ggplot2"), 
    +     type = "histogram", size = 4))
    
      When sourcing ‘Intro.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘Intro.Rmd’... failed
    ```

# bnma

<details>

* Version: 1.6.0
* GitHub: NA
* Source code: https://github.com/cran/bnma
* Date/Publication: 2024-02-11 01:10:02 UTC
* Number of recursive dependencies: 53

Run `revdepcheck::cloud_details(, "bnma")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘bnma.Rmd’
      ...
    
    > network.forest.plot(result, label.margin = 15)
    Warning in geom_text(aes(label = "Median [95% Crl]"), y = xlim.range[2] +  :
      All aesthetics have length 1, but the data has 6 rows.
    ℹ Please consider using `annotate()` or provide this layer with data containing
      a single row.
    
      When sourcing ‘bnma.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘bnma.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘bnma.Rmd’ using rmarkdown
    
    Quitting from lines 88-99 [unnamed-chunk-8] (bnma.Rmd)
    Error: processing vignette 'bnma.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘bnma.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘bnma.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# boxly

<details>

* Version: 0.1.1
* GitHub: https://github.com/Merck/boxly
* Source code: https://github.com/cran/boxly
* Date/Publication: 2023-10-24 02:40:02 UTC
* Number of recursive dependencies: 91

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

# braidReports

<details>

* Version: 0.5.4
* GitHub: NA
* Source code: https://github.com/cran/braidReports
* Date/Publication: 2021-01-05 18:20:09 UTC
* Number of recursive dependencies: 30

Run `revdepcheck::cloud_details(, "braidReports")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘braidReports-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: makeBRAIDreport
    > ### Title: Make a BRAID Report Page
    > ### Aliases: makeBRAIDreport
    > ### Keywords: hplot
    > 
    > ### ** Examples
    > 
    ...
     22. │                       └─grid::convertUnit(short, "cm", valueOnly = TRUE)
     23. │                         ├─grid:::upgradeUnit(x)
     24. │                         └─grid:::upgradeUnit.default(x)
     25. │                           └─base::stop("Not a unit object")
     26. └─base::.handleSimpleError(`<fn>`, "Not a unit object", base::quote(upgradeUnit.default(x)))
     27.   └─rlang (local) h(simpleError(msg, call))
     28.     └─handlers[[1L]](cnd)
     29.       └─cli::cli_abort(...)
     30.         └─rlang::abort(...)
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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘exploratory-modelling.Rmd’
      ...
    +     0)
    Warning in is.na(non_null_default_aes[[aes_param_name]]) :
      is.na() applied to non-(list or vector) of type 'language'
    
      When sourcing ‘exploratory-modelling.R’:
    Error: ℹ In index: 1.
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
    Caused by error in `aes_param_name %in% names(non_null_default_aes) && is.na(non_null_default_aes[[
        aes_param_name]])`:
    ! 'length = 2' in coercion to 'logical(1)'
    --- failed re-building ‘exploratory-modelling.Rmd’
    
    --- re-building ‘finding-features.Rmd’ using rmarkdown
    ```

# calendR

<details>

* Version: 1.2
* GitHub: NA
* Source code: https://github.com/cran/calendR
* Date/Publication: 2023-10-05 17:30:02 UTC
* Number of recursive dependencies: 52

Run `revdepcheck::cloud_details(, "calendR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘calendR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: calendR
    > ### Title: Monthly and yearly calendars
    > ### Aliases: calendR
    > 
    > ### ** Examples
    > 
    > # Calendar of the current year
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

# calendRio

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/calendRio
* Date/Publication: 2022-03-10 07:50:02 UTC
* Number of recursive dependencies: 52

Run `revdepcheck::cloud_details(, "calendRio")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘calendRio-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: calendR
    > ### Title: Monthly and yearly calendars
    > ### Aliases: calendR
    > 
    > ### ** Examples
    > 
    > # Calendar of the current year
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

# capm

<details>

* Version: 0.14.0
* GitHub: NA
* Source code: https://github.com/cran/capm
* Date/Publication: 2019-10-24 16:50:05 UTC
* Number of recursive dependencies: 61

Run `revdepcheck::cloud_details(, "capm")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘capm-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: PlotPopPyramid
    > ### Title: Population PlotPopPyramid
    > ### Aliases: PlotPopPyramid
    > 
    > ### ** Examples
    > 
    > data(dogs)
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 59 marked UTF-8 strings
    ```

# cartograflow

<details>

* Version: 1.0.5
* GitHub: https://github.com/fbahoken/cartogRaflow
* Source code: https://github.com/cran/cartograflow
* Date/Publication: 2023-10-17 22:40:21 UTC
* Number of recursive dependencies: 102

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

# cheem

<details>

* Version: 0.4.0.0
* GitHub: https://github.com/nspyrison/cheem
* Source code: https://github.com/cran/cheem
* Date/Publication: 2023-11-08 21:30:02 UTC
* Number of recursive dependencies: 152

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

* Version: 0.75
* GitHub: NA
* Source code: https://github.com/cran/chillR
* Date/Publication: 2023-11-27 22:20:02 UTC
* Number of recursive dependencies: 139

Run `revdepcheck::cloud_details(, "chillR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘chillR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_scenarios
    > ### Title: Plot historic and future scenarios for climate-related metrics
    > ###   ('ggplot2' version)
    > ### Aliases: plot_scenarios
    > 
    > ### ** Examples
    > 
    ...
    >                                    
    > # Plot the climate scenarios
    > 
    > plot_scenarios(climate_scenario_list, metric = 'Chill_Portions',
    +                add_historic = TRUE, size = 2, shape = 3, color = 'blue',
    +                outlier_shape = 12, historic_color = 'skyblue',
    +                group_by = c("Year", "Scenario"))
    Error in identicalUnits(x) : object is not a unit
    Calls: <Anonymous> ... assemble_guides -> guides_build -> unit.c -> identicalUnits
    Execution halted
    ```

# chronicle

<details>

* Version: 0.3
* GitHub: NA
* Source code: https://github.com/cran/chronicle
* Date/Publication: 2021-06-25 05:00:02 UTC
* Number of recursive dependencies: 146

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

# circumplex

<details>

* Version: 0.3.10
* GitHub: https://github.com/jmgirard/circumplex
* Source code: https://github.com/cran/circumplex
* Date/Publication: 2023-08-22 07:20:05 UTC
* Number of recursive dependencies: 101

Run `revdepcheck::cloud_details(, "circumplex")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘introduction-to-ssm-analysis.Rmd’
      ...
    +     "BC", "DE", "FG", "HI", "JK", "LM", "NO"), levels = c("PA", 
    +     "BC", "DE", "FG", "HI ..." ... [TRUNCATED] 
    
    > ggplot2::ggplot(dat_r, ggplot2::aes(x = Angle, y = est)) + 
    +     ggplot2::geom_hline(yintercept = 0, size = 1.25, color = "darkgray") + 
    +     ggpl .... [TRUNCATED] 
    
      When sourcing ‘introduction-to-ssm-analysis.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘intermediate-ssm-analysis.Rmd’ using ‘UTF-8’... OK
      ‘introduction-to-ssm-analysis.Rmd’ using ‘UTF-8’... failed
      ‘using-instruments.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘intermediate-ssm-analysis.Rmd’ using rmarkdown
    ```

# cities

<details>

* Version: 0.1.3
* GitHub: NA
* Source code: https://github.com/cran/cities
* Date/Publication: 2023-08-08 07:50:10 UTC
* Number of recursive dependencies: 85

Run `revdepcheck::cloud_details(, "cities")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘cities-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_dc
    > ### Title: plot_dc
    > ### Aliases: plot_dc
    > 
    > ### ** Examples
    > 
    > total_data = 3
    ...
        ▆
     1. └─cities::plot_estimates(...)
     2.   ├─base::print(p_estimands)
     3.   └─ggplot2:::print.ggplot(p_estimands)
     4.     ├─ggplot2::ggplot_gtable(data)
     5.     └─ggplot2:::ggplot_gtable.ggplot_built(data)
     6.       └─ggplot2::calc_element("plot.margin", theme)
     7.         └─cli::cli_abort(...)
     8.           └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘CITIES_demo.Rmd’
      ...
      |                                                        
      |==================================================| 100%
    > estimates_out = plot_estimates(data_out = data_out, 
    +     total_data = total_data, timepoints = timepoints, reference_id = reference_id, 
    +     IR_ .... [TRUNCATED] 
    Warning: Using shapes for an ordinal variable is not advised
    
      When sourcing ‘CITIES_demo.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘CITIES_demo.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘CITIES_demo.Rmd’ using rmarkdown
    ```

# CleaningValidation

<details>

* Version: 1.0
* GitHub: https://github.com/ChandlerXiandeYang/CleaningValidation
* Source code: https://github.com/cran/CleaningValidation
* Date/Publication: 2024-05-17 09:10:21 UTC
* Number of recursive dependencies: 79

Run `revdepcheck::cloud_details(, "CleaningValidation")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘CleaningValidation-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: cv16_u_chart
    > ### Title: Create a u-Chart for Poisson-distributed Data
    > ### Aliases: cv16_u_chart
    > 
    > ### ** Examples
    > 
    > cv16_u_chart(data = Eq_Mic, residue_col = "Mic", cleaning_event_col = "CleaningEvent")
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

# clinDataReview

<details>

* Version: 1.5.2
* GitHub: https://github.com/openanalytics/clinDataReview
* Source code: https://github.com/cran/clinDataReview
* Date/Publication: 2024-05-17 16:30:05 UTC
* Number of recursive dependencies: 130

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
        adding: report_dependencies12051d006778/ (stored 0%)
        adding: report_dependencies12051d006778/file12056baf983f.html (deflated 8%)
    ...
      Backtrace:
          ▆
       1. └─clinDataReview::scatterplotClinData(...) at test_scatterplotClinData.R:851:3
       2.   ├─plotly::ggplotly(p = gg, width = width, height = height, tooltip = if (!is.null(hoverVars)) "text")
       3.   └─plotly:::ggplotly.ggplot(...)
       4.     └─plotly::gg2list(...)
      
      [ FAIL 25 | WARN 8 | SKIP 30 | PASS 450 ]
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
* Number of recursive dependencies: 120

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

# ClustImpute

<details>

* Version: 0.2.4
* GitHub: NA
* Source code: https://github.com/cran/ClustImpute
* Date/Publication: 2021-05-31 07:40:11 UTC
* Number of recursive dependencies: 121

Run `revdepcheck::cloud_details(, "ClustImpute")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Example_on_simulated_data.Rmd’
      ...
    > dat4plot$true_clust_fct <- factor(true_clust)
    
    > p_base <- ggplot(dat4plot, aes(x = x, y = y, color = true_clust_fct)) + 
    +     geom_point()
    
    > ggExtra::ggMarginal(p_base, groupColour = TRUE, groupFill = TRUE)
    
      When sourcing ‘Example_on_simulated_data.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘Example_on_simulated_data.Rmd’ using ‘UTF-8’... failed
      ‘description_of_algorithm.Rnw’ using ‘UTF-8’... OK
    ```

## In both

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Example_on_simulated_data.Rmd’ using rmarkdown
    
    Quitting from lines 49-53 [unnamed-chunk-3] (Example_on_simulated_data.Rmd)
    Error: processing vignette 'Example_on_simulated_data.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘Example_on_simulated_data.Rmd’
    
    --- re-building ‘description_of_algorithm.Rnw’ using Sweave
    Error: processing vignette 'description_of_algorithm.Rnw' failed with diagnostics:
    ...
    l.6 \usepackage
                   {Sweave}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘description_of_algorithm.Rnw’
    
    SUMMARY: processing the following files failed:
      ‘Example_on_simulated_data.Rmd’ ‘description_of_algorithm.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# cogmapr

<details>

* Version: 0.9.3
* GitHub: NA
* Source code: https://github.com/cran/cogmapr
* Date/Publication: 2022-01-04 15:40:07 UTC
* Number of recursive dependencies: 75

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
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
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

# CoMiRe

<details>

* Version: 0.8
* GitHub: NA
* Source code: https://github.com/cran/CoMiRe
* Date/Publication: 2023-08-23 09:10:06 UTC
* Number of recursive dependencies: 35

Run `revdepcheck::cloud_details(, "CoMiRe")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘CoMiRe-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: BMD
    > ### Title: Benchmark dose
    > ### Aliases: BMD
    > 
    > ### ** Examples
    > 
    > {
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

# CommKern

<details>

* Version: 1.0.1
* GitHub: https://github.com/aljensen89/CommKern
* Source code: https://github.com/cran/CommKern
* Date/Publication: 2022-09-23 10:20:06 UTC
* Number of recursive dependencies: 58

Run `revdepcheck::cloud_details(, "CommKern")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘CommKern-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: hms
    > ### Title: Hierarchical multimodal spinglass algorithm
    > ### Aliases: hms
    > 
    > ### ** Examples
    > 
    > 
    ...
        ▆
     1. ├─CommKern::community_plot(hms_object)
     2. └─CommKern:::community_plot.spinglass_hms(hms_object)
     3.   └─ggplot2::ggplotGrob(comm_plot)
     4.     ├─ggplot2::ggplot_gtable(ggplot_build(x))
     5.     └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     6.       └─ggplot2::calc_element("plot.margin", theme)
     7.         └─cli::cli_abort(...)
     8.           └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘CommKern.Rmd’
      ...
      .. .. ..$ : chr [1:80] "1" "2" "3" "4" ...
      ..- attr(*, "class")= chr "spinglass_net"
     $ best_hamiltonian: num -286
     - attr(*, "class")= chr "spinglass_hms"
    
    > community_plot(hms_object)
    
      When sourcing ‘CommKern.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘CommKern.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘CommKern.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.2Mb
      sub-directories of 1Mb or more:
        data   6.5Mb
    ```

# conText

<details>

* Version: 1.4.3
* GitHub: https://github.com/prodriguezsosa/ConText
* Source code: https://github.com/cran/conText
* Date/Publication: 2023-02-09 21:10:02 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::cloud_details(, "conText")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘conText-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_nns_ratio
    > ### Title: Plot output of 'get_nns_ratio()'
    > ### Aliases: plot_nns_ratio
    > ### Keywords: plot_nns_ratio
    > 
    > ### ** Examples
    > 
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘quickstart.Rmd’
      ...
    4 illegally  1.12    0.0290    1.08      1.18    0    shared
    5 laws       1.09    0.0351    1.03      1.15    0    R     
    6 legal      1.03    0.0341    0.973     1.08    0.39 R     
    
    > plot_nns_ratio(x = immig_nns_ratio, alpha = 0.01, 
    +     horizontal = TRUE)
    
      When sourcing ‘quickstart.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘quickstart.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘quickstart.Rmd’ using rmarkdown
    
    Quitting from lines 342-343 [unnamed-chunk-21] (quickstart.Rmd)
    Error: processing vignette 'quickstart.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘quickstart.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘quickstart.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        data   3.5Mb
        doc    1.5Mb
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
* Number of recursive dependencies: 117

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
* Number of recursive dependencies: 139

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

# covidcast

<details>

* Version: 0.5.2
* GitHub: https://github.com/cmu-delphi/covidcast
* Source code: https://github.com/cran/covidcast
* Date/Publication: 2023-07-12 23:40:06 UTC
* Number of recursive dependencies: 93

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
    when running code in ‘multi-signals.Rmd’
      ...
    
    > signals <- covidcast_signals(data_source = "jhu-csse", 
    +     signal = c("confirmed_7dav_incidence_prop", "deaths_7dav_incidence_prop"), 
    +     star .... [TRUNCATED] 
    
      When sourcing ‘multi-signals.R’:
    Error: Rate limit exceeded when fetching data from API anonymously. See the "API keys" section of the `covidcast_signal()` documentation for information on registering for an API key.
    ...
    Error: Rate limit exceeded when fetching data from API anonymously. See the "API keys" section of the `covidcast_signal()` documentation for information on registering for an API key.
    ℹ Message from server:
    ℹ Rate limit exceeded for anonymous queries. To remove this limit, register a free API key at https://api.delphi.cmu.edu/epidata/admin/registration_form
    Execution halted
    
      ‘correlation-utils.Rmd’ using ‘UTF-8’... OK
      ‘covidcast.Rmd’ using ‘UTF-8’... OK
      ‘external-data.Rmd’ using ‘UTF-8’... OK
      ‘multi-signals.Rmd’ using ‘UTF-8’... failed
      ‘plotting-signals.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘correlation-utils.Rmd’ using rmarkdown
    --- finished re-building ‘correlation-utils.Rmd’
    
    --- re-building ‘covidcast.Rmd’ using rmarkdown
    
    Quitting from lines 38-45 [unnamed-chunk-1] (covidcast.Rmd)
    Error: processing vignette 'covidcast.Rmd' failed with diagnostics:
    Rate limit exceeded when fetching data from API anonymously. See the "API keys" section of the `covidcast_signal()` documentation for information on registering for an API key.
    ℹ Message from server:
    ℹ Rate limit exceeded for anonymous queries. To remove this limit, register a free API key at https://api.delphi.cmu.edu/epidata/admin/registration_form
    --- failed re-building ‘covidcast.Rmd’
    
    --- re-building ‘external-data.Rmd’ using rmarkdown
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 20 marked UTF-8 strings
    ```

# cricketdata

<details>

* Version: 0.2.3
* GitHub: https://github.com/robjhyndman/cricketdata
* Source code: https://github.com/cran/cricketdata
* Date/Publication: 2023-08-29 10:30:09 UTC
* Number of recursive dependencies: 104

Run `revdepcheck::cloud_details(, "cricketdata")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘cricinfo.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘cricinfo.Rmd’
      ...
    
    > library(ggplot2)
    
    > wt20 <- readRDS("../inst/extdata/wt20.rds")
    Warning in gzfile(file, "rb") :
      cannot open compressed file '../inst/extdata/wt20.rds', probable reason 'No such file or directory'
    
    ...
    Warning in gzfile(file, "rb") :
      cannot open compressed file '../inst/extdata/wbbl_bbb.rds', probable reason 'No such file or directory'
    
      When sourcing ‘cricsheet.R’:
    Error: cannot open the connection
    Execution halted
    
      ‘cricinfo.Rmd’ using ‘UTF-8’... failed
      ‘cricketdata_R_pkg.Rmd’ using ‘UTF-8’... failed
      ‘cricsheet.Rmd’ using ‘UTF-8’... failed
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 37 marked UTF-8 strings
    ```

# crosshap

<details>

* Version: 1.4.0
* GitHub: https://github.com/jacobimarsh/crosshap
* Source code: https://github.com/cran/crosshap
* Date/Publication: 2024-03-31 15:40:02 UTC
* Number of recursive dependencies: 117

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

# crplyr

<details>

* Version: 0.4.0
* GitHub: https://github.com/Crunch-io/crplyr
* Source code: https://github.com/cran/crplyr
* Date/Publication: 2023-03-21 21:50:02 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "crplyr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(httptest)
      Loading required package: testthat
      > test_check("crplyr")
      Loading required package: crplyr
      Loading required package: crunch
      
    ...
        7.         └─ggplot2:::print.ggplot(p)
        8.           ├─ggplot2::ggplot_gtable(data)
        9.           └─ggplot2:::ggplot_gtable.ggplot_built(data)
       10.             └─ggplot2::calc_element("plot.margin", theme)
       11.               └─cli::cli_abort(...)
       12.                 └─rlang::abort(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 172 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘plotting.Rmd’
      ...
        equals, is_less_than, not
    
    
    > ds <- loadDataset("https://app.crunch.io/api/datasets/5c9336/")
    
    > autoplot(ds$CompanySize)
    
      When sourcing ‘plotting.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘plotting.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘plotting.Rmd’ using rmarkdown
    
    Quitting from lines 35-36 [basic-card-plots] (plotting.Rmd)
    Error: processing vignette 'plotting.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘plotting.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘plotting.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ctrialsgov

<details>

* Version: 0.2.5
* GitHub: NA
* Source code: https://github.com/cran/ctrialsgov
* Date/Publication: 2021-10-18 16:00:02 UTC
* Number of recursive dependencies: 100

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

* Version: 0.3.0
* GitHub: https://github.com/huizezhang-sherry/cubble
* Source code: https://github.com/cran/cubble
* Date/Publication: 2023-06-30 03:40:02 UTC
* Number of recursive dependencies: 144

Run `revdepcheck::cloud_details(, "cubble")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘cb5match.Rmd’
      ...
    > p2 <- res_tm_long %>% ggplot(aes(x = date, y = matched, 
    +     group = type, color = type)) + geom_line() + facet_wrap(vars(group)) + 
    +     scale_c .... [TRUNCATED] 
    
    > (p1 | p2) + patchwork::plot_layout(guides = "collect") + 
    +     plot_annotation(tag_levels = "a") & theme(legend.position = "bottom")
    
    ...
      When sourcing ‘cb6interactive.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘cb1class.Rmd’ using ‘UTF-8’... OK
      ‘cb2create.Rmd’ using ‘UTF-8’... OK
      ‘cb3tsibblesf.Rmd’ using ‘UTF-8’... OK
      ‘cb4glyph.Rmd’ using ‘UTF-8’... OK
      ‘cb5match.Rmd’ using ‘UTF-8’... failed
      ‘cb6interactive.Rmd’ using ‘UTF-8’... failed
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
        doc    1.3Mb
    ```

# dabestr

<details>

* Version: 2023.9.12
* GitHub: https://github.com/ACCLAB/dabestr
* Source code: https://github.com/cran/dabestr
* Date/Publication: 2023-10-13 11:50:06 UTC
* Number of recursive dependencies: 85

Run `revdepcheck::cloud_details(, "dabestr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘dabestr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: dabest_plot
    > ### Title: Producing an estimation plot
    > ### Aliases: dabest_plot
    > 
    > ### ** Examples
    > 
    > # Loading of the dataset
    ...
      7.           └─cowplot:::as_gtable.default(x)
      8.             ├─cowplot::as_grob(plot)
      9.             └─cowplot:::as_grob.ggplot(plot)
     10.               └─ggplot2::ggplotGrob(plot)
     11.                 ├─ggplot2::ggplot_gtable(ggplot_build(x))
     12.                 └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     13.                   └─ggplot2::calc_element("plot.margin", theme)
     14.                     └─cli::cli_abort(...)
     15.                       └─rlang::abort(...)
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
      • 001_plotter/proportion-sequential-mean-diff.svg
      • 001_plotter/proportion-unpaired-mean-diff-float-false.svg
      • 001_plotter/proportion-unpaired-mean-diff-float-true.svg
      • 001_plotter/proportion-unpaired-multigroup-mean-diff.svg
      • 001_plotter/two-groups-unpaired-mean-diff-colour-float-false.svg
      • 001_plotter/two-groups-unpaired-mean-diff-colour-float-true.svg
      • 001_plotter/two-groups-unpaired-mean-diff-float-false.svg
      • 001_plotter/two-groups-unpaired-mean-diff-float-true.svg
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘plot_aesthetics.Rmd’
      ...
    +     x = Group, y = Success, idx = list(c("Control 1", "Test 1", 
    +         "Test 2", "Te ..." ... [TRUNCATED] 
    
    > dabest_plot(dabest_twogroup_obj.mean_diff, float_contrast = TRUE, 
    +     swarm_x_text = 30, swarm_y_text = 1, contrast_x_text = 30, 
    +     contrast_ .... [TRUNCATED] 
    
    ...
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘datasets.Rmd’ using ‘UTF-8’... OK
      ‘plot_aesthetics.Rmd’ using ‘UTF-8’... failed
      ‘tutorial_basics.Rmd’ using ‘UTF-8’... failed
      ‘tutorial_deltadelta.Rmd’ using ‘UTF-8’... failed
      ‘tutorial_minimeta.Rmd’ using ‘UTF-8’... failed
      ‘tutorial_proportion_plots.Rmd’ using ‘UTF-8’... failed
      ‘tutorial_repeated_measures.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘datasets.Rmd’ using rmarkdown
    --- finished re-building ‘datasets.Rmd’
    
    --- re-building ‘plot_aesthetics.Rmd’ using rmarkdown
    
    Quitting from lines 81-89 [unnamed-chunk-3] (plot_aesthetics.Rmd)
    Error: processing vignette 'plot_aesthetics.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘plot_aesthetics.Rmd’
    ...
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘tutorial_repeated_measures.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘plot_aesthetics.Rmd’ ‘tutorial_basics.Rmd’ ‘tutorial_deltadelta.Rmd’
      ‘tutorial_minimeta.Rmd’ ‘tutorial_proportion_plots.Rmd’
      ‘tutorial_repeated_measures.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# DAISIEprep

<details>

* Version: 0.4.0
* GitHub: https://github.com/joshwlambert/DAISIEprep
* Source code: https://github.com/cran/DAISIEprep
* Date/Publication: 2024-04-02 11:30:06 UTC
* Number of recursive dependencies: 149

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
      [ FAIL 2 | WARN 2 | SKIP 14 | PASS 2213 ]
      
      ══ Skipped tests (14) ══════════════════════════════════════════════════════════
    ...
       23.                   └─ggplot2::ggplotGrob(plot)
       24.                     ├─ggplot2::ggplot_gtable(ggplot_build(x))
       25.                     └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
       26.                       └─ggplot2::calc_element("plot.margin", theme)
       27.                         └─cli::cli_abort(...)
       28.                           └─rlang::abort(...)
      
      [ FAIL 2 | WARN 2 | SKIP 14 | PASS 2213 ]
      Error: Test failures
      Execution halted
    ```

# dataresqc

<details>

* Version: 1.1.1
* GitHub: NA
* Source code: https://github.com/cran/dataresqc
* Date/Publication: 2023-04-02 22:00:02 UTC
* Number of recursive dependencies: 49

Run `revdepcheck::cloud_details(, "dataresqc")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘dataresqc-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_decimals
    > ### Title: Plot decimals
    > ### Aliases: plot_decimals
    > 
    > ### ** Examples
    > 
    > plot_decimals(Rosario$Tx, outfile = paste0(tempdir(),"/test.pdf"))
    ...
     1. └─dataresqc::plot_decimals(...)
     2.   └─dataresqc:::multiplot(plotlist = plots)
     3.     ├─base::print(plots[[1]])
     4.     └─ggplot2:::print.ggplot(plots[[1]])
     5.       ├─ggplot2::ggplot_gtable(data)
     6.       └─ggplot2:::ggplot_gtable.ggplot_built(data)
     7.         └─ggplot2::calc_element("plot.margin", theme)
     8.           └─cli::cli_abort(...)
     9.             └─rlang::abort(...)
    Execution halted
    ```

# ddtlcm

<details>

* Version: 0.2.1
* GitHub: https://github.com/limengbinggz/ddtlcm
* Source code: https://github.com/cran/ddtlcm
* Date/Publication: 2024-04-04 02:32:57 UTC
* Number of recursive dependencies: 150

Run `revdepcheck::cloud_details(, "ddtlcm")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ddtlcm-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.summary.ddt_lcm
    > ### Title: Plot the MAP tree and class profiles of summarized DDT-LCM
    > ###   results
    > ### Aliases: plot.summary.ddt_lcm
    > 
    > ### ** Examples
    > 
    ...
     15.                   └─cowplot:::as_gtable.default(x)
     16.                     ├─cowplot::as_grob(plot)
     17.                     └─cowplot:::as_grob.ggplot(plot)
     18.                       └─ggplot2::ggplotGrob(plot)
     19.                         ├─ggplot2::ggplot_gtable(ggplot_build(x))
     20.                         └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     21.                           └─ggplot2::calc_element("plot.margin", theme)
     22.                             └─cli::cli_abort(...)
     23.                               └─rlang::abort(...)
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
          label_size = .lab$size, label_fontfamily = .lab$family, label_fontface = .lab$face, 
          label_colour = .lab$color, label_x = .lab$label.x, label_y = .lab$label.y, 
          hjust = .lab$hjust, vjust = .lab$vjust, align = align, rel_widths = widths, 
          rel_heights = heights, legend = legend, common.legend.grob = legend.grob)`: i In index: 1.
      Caused by error in `ggplot_gtable()`:
      ! Theme element `plot.margin` must have class <margin/rel>.
      
      [ FAIL 1 | WARN 30 | SKIP 0 | PASS 62 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ddtlcm-demo.Rmd’
      ...
    > response_prob <- sim_data$response_prob
    
    > tree_with_parameter <- sim_data$tree_with_parameter
    
    > plot_tree_with_heatmap(tree_with_parameter, response_prob, 
    +     item_membership_list)
    
      When sourcing ‘ddtlcm-demo.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘ddtlcm-demo.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ddtlcm-demo.Rmd’ using rmarkdown
    
    Quitting from lines 134-139 [unnamed-chunk-5] (ddtlcm-demo.Rmd)
    Error: processing vignette 'ddtlcm-demo.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘ddtlcm-demo.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ddtlcm-demo.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.3Mb
      sub-directories of 1Mb or more:
        data   8.0Mb
    ```

# dfoliatR

<details>

* Version: 0.3.0
* GitHub: https://github.com/chguiterman/dfoliatR
* Source code: https://github.com/cran/dfoliatR
* Date/Publication: 2023-08-09 22:10:02 UTC
* Number of recursive dependencies: 109

Run `revdepcheck::cloud_details(, "dfoliatR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘dfoliatR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_outbreak
    > ### Title: Produce a stacked plot to present composited, site-level insect
    > ###   outbreak chronologies
    > ### Aliases: plot_outbreak
    > 
    > ### ** Examples
    > 
    ...
     14.                   └─cowplot:::as_gtable.default(x)
     15.                     ├─cowplot::as_grob(plot)
     16.                     └─cowplot:::as_grob.ggplot(plot)
     17.                       └─ggplot2::ggplotGrob(plot)
     18.                         ├─ggplot2::ggplot_gtable(ggplot_build(x))
     19.                         └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     20.                           └─ggplot2::calc_element("plot.margin", theme)
     21.                             └─cli::cli_abort(...)
     22.                               └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(dfoliatR)
      > 
      > test_check("dfoliatR")
      [ FAIL 1 | WARN 9 | SKIP 2 | PASS 12 ]
      
      ══ Skipped tests (2) ═══════════════════════════════════════════════════════════
    ...
          label_size = .lab$size, label_fontfamily = .lab$family, label_fontface = .lab$face, 
          label_colour = .lab$color, label_x = .lab$label.x, label_y = .lab$label.y, 
          hjust = .lab$hjust, vjust = .lab$vjust, align = align, rel_widths = widths, 
          rel_heights = heights, legend = legend, common.legend.grob = legend.grob)`: ℹ In index: 1.
      Caused by error in `ggplot_gtable()`:
      ! Theme element `plot.margin` must have class <margin/rel>.
      
      [ FAIL 1 | WARN 9 | SKIP 2 | PASS 12 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘intro-to-dfoliatR.Rmd’
      ...
    | 1683|          3|         2|       66.7|             1|           33.3|   0.3466|   -1.7847|outbreak        |
    | 1684|          3|         2|       66.7|             0|            0.0|   0.6063|   -1.0589|outbreak        |
    
    > plot_outbreak(dmj_obr)
    
      When sourcing ‘intro-to-dfoliatR.R’:
    Error: ℹ In index: 1.
    Caused by error in `ggplot_gtable()`:
    ! Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘intro-to-dfoliatR.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘intro-to-dfoliatR.Rmd’ using rmarkdown
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
     6    23  1669 aaa  
     7    22  1315 aaa  
     8    21   951 aaa  
     9    20   610 aaa  
    10    19   543 aaa  
    # ℹ 14 more rows
    
      When sourcing ‘examples.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘examples.Rmd’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘examples.Rmd’ using knitr
    ```

# disprofas

<details>

* Version: 0.1.3
* GitHub: https://github.com/piusdahinden/disprofas
* Source code: https://github.com/cran/disprofas
* Date/Publication: 2021-12-08 12:40:02 UTC
* Number of recursive dependencies: 48

Run `revdepcheck::cloud_details(, "disprofas")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘disprofas-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.plot_mztia
    > ### Title: Plot of the mztia simulation
    > ### Aliases: plot.plot_mztia
    > 
    > ### ** Examples
    > 
    > # Dissolution data of one reference batch and one test batch of n = 6
    ...
     1. ├─base::plot(gg1)
     2. └─disprofas:::plot.plot_mztia(gg1)
     3.   ├─base::plot(x$Graph, ...)
     4.   └─ggplot2:::plot.ggplot(x$Graph, ...)
     5.     ├─ggplot2::ggplot_gtable(data)
     6.     └─ggplot2:::ggplot_gtable.ggplot_built(data)
     7.       └─ggplot2::calc_element("plot.margin", theme)
     8.         └─cli::cli_abort(...)
     9.           └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(disprofas)
      > 
      > test_check("disprofas")
      [ FAIL 2 | WARN 3 | SKIP 0 | PASS 479 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
        8.   └─ggplot2:::plot.ggplot(x$Graph, ...)
        9.     ├─ggplot2::ggplot_gtable(data)
       10.     └─ggplot2:::ggplot_gtable.ggplot_built(data)
       11.       └─ggplot2::calc_element("plot.margin", theme)
       12.         └─cli::cli_abort(...)
       13.           └─rlang::abort(...)
      
      [ FAIL 2 | WARN 3 | SKIP 0 | PASS 479 ]
      Error: Test failures
      Execution halted
    ```

# distributional

<details>

* Version: 0.4.0
* GitHub: https://github.com/mitchelloharawild/distributional
* Source code: https://github.com/cran/distributional
* Date/Publication: 2024-02-07 13:30:02 UTC
* Number of recursive dependencies: 64

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
* Number of recursive dependencies: 99

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
      [ FAIL 12 | WARN 12 | SKIP 0 | PASS 307 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
       2. └─dittoViz::freqPlot(...)
       3.   └─dittoViz::yPlot(...)
       4.     └─dittoViz:::.warn_or_apply_plotly(p, plots)
       5.       ├─plotly::ggplotly(p, tooltip = "text")
       6.       └─plotly:::ggplotly.ggplot(p, tooltip = "text")
       7.         └─plotly::gg2list(...)
      
      [ FAIL 12 | WARN 12 | SKIP 0 | PASS 307 ]
      Error: Test failures
      Execution halted
    ```

# dobin

<details>

* Version: 1.0.4
* GitHub: NA
* Source code: https://github.com/cran/dobin
* Date/Publication: 2022-08-25 22:52:33 UTC
* Number of recursive dependencies: 147

Run `revdepcheck::cloud_details(, "dobin")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘dobin.Rmd’
      ...
    +     boxplotLimits = 10)
    
    > pPx <- O3plotM(pPa)
    
    > pPx$gO3x + theme(plot.margin = unit(c(0, 2, 0, 0), 
    +     "cm"))
    
      When sourcing ‘dobin.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘dobin.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘dobin.Rmd’ using rmarkdown
    ```

# dogesr

<details>

* Version: 0.5.0
* GitHub: NA
* Source code: https://github.com/cran/dogesr
* Date/Publication: 2023-08-21 11:40:05 UTC
* Number of recursive dependencies: 121

Run `revdepcheck::cloud_details(, "dogesr")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘counting-doge-families.Rmd’ using rmarkdown
    --- finished re-building ‘counting-doge-families.Rmd’
    
    --- re-building ‘doge-types.Rmd’ using rmarkdown
    
    Quitting from lines 48-51 [plot] (doge-types.Rmd)
    Error: processing vignette 'doge-types.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘doge-types.Rmd’
    
    --- re-building ‘doges-family-types.Rmd’ using rmarkdown
    --- finished re-building ‘doges-family-types.Rmd’
    
    --- re-building ‘doges-social-network.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘counting-doge-families.Rmd’
      ...
    > knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
    
    > devtools::load_all(".")
    
      When sourcing ‘counting-doge-families.R’:
    Error: Could not find a root 'DESCRIPTION' file that starts with '^Package' in
    '/tmp/Rtmpt416E4/file12ff49ee8180/vignettes'.
    ...
    ℹ Are you in your project directory and does your project have a 'DESCRIPTION'
      file?
    Execution halted
    
      ‘counting-doge-families.Rmd’ using ‘UTF-8’... failed
      ‘doge-types.Rmd’ using ‘UTF-8’... failed
      ‘doges-family-types.Rmd’ using ‘UTF-8’... failed
      ‘doges-social-network.Rmd’ using ‘UTF-8’... OK
      ‘doges-split-social-network.Rmd’ using ‘UTF-8’... OK
      ‘doges-terms.Rmd’ using ‘UTF-8’... OK
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 3 marked UTF-8 strings
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
      3.   │ └─gridExtra::arrangeGrob(...)
      4.   └─gridExtra::arrangeGrob(...)
      5.     └─base::lapply(grobs[toconv], ggplot2::ggplotGrob)
      6.       └─ggplot2 (local) FUN(X[[i]], ...)
      7.         ├─ggplot2::ggplot_gtable(ggplot_build(x))
      8.         └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
      9.           └─ggplot2::calc_element("plot.margin", theme)
     10.             └─cli::cli_abort(...)
     11.               └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

# ds4psy

<details>

* Version: 1.0.0
* GitHub: https://github.com/hneth/ds4psy
* Source code: https://github.com/cran/ds4psy
* Date/Publication: 2023-09-15 07:30:02 UTC
* Number of recursive dependencies: 55

Run `revdepcheck::cloud_details(, "ds4psy")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ds4psy-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_charmap
    > ### Title: Plot a character map as a tile plot with text labels.
    > ### Aliases: plot_charmap
    > 
    > ### ** Examples
    > 
    > # (0) Prepare: 
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

# edecob

<details>

* Version: 1.2.2
* GitHub: NA
* Source code: https://github.com/cran/edecob
* Date/Publication: 2022-11-04 12:00:02 UTC
* Number of recursive dependencies: 29

Run `revdepcheck::cloud_details(, "edecob")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘edecob-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: edecob
    > ### Title: Event DEtection using COnfidence Bounds
    > ### Aliases: edecob
    > 
    > ### ** Examples
    > 
    > library(edecob)
    ...
        ▆
     1. ├─base::plot(example_event$`Subject 1`)
     2. └─edecob:::plot.edecob(example_event$`Subject 1`)
     3.   └─ggplot2::ggplotGrob(patient_plot)
     4.     ├─ggplot2::ggplot_gtable(ggplot_build(x))
     5.     └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     6.       └─ggplot2::calc_element("plot.margin", theme)
     7.         └─cli::cli_abort(...)
     8.           └─rlang::abort(...)
    Execution halted
    ```

# entropart

<details>

* Version: 1.6-13
* GitHub: https://github.com/EricMarcon/entropart
* Source code: https://github.com/cran/entropart
* Date/Publication: 2023-09-26 14:40:02 UTC
* Number of recursive dependencies: 121

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

# envalysis

<details>

* Version: 0.7.0
* GitHub: https://github.com/zsteinmetz/envalysis
* Source code: https://github.com/cran/envalysis
* Date/Publication: 2024-03-20 15:10:02 UTC
* Number of recursive dependencies: 103

Run `revdepcheck::cloud_details(, "envalysis")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘envalysis-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: theme_publish
    > ### Title: ggplot2 theme for scientific publications
    > ### Aliases: theme_publish
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(envalysis)
      > 
      > test_check("envalysis")
      number of blank values <= 1; LOD is estimated from the calibration curve
      number of blank values <= 1; LOD is estimated from the calibration curve
      number of blank values <= 1; LOD is estimated from the calibration curve
    ...
       13.     └─ggplot2::calc_element("plot.margin", theme)
       14.       └─cli::cli_abort(...)
       15.         └─rlang::abort(...)
      
      [ FAIL 1 | WARN 0 | SKIP 3 | PASS 139 ]
      Deleting unused snapshots:
      • calibration/plot.png
      • texture/plot.png
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘calibration.Rmd’
      ...
    > dt$sum <- dt$res[, .(Content = mean(Content, na.rm = T), 
    +     CI = CI(Content, na.rm = T)), by = .(Compound, Treatment, 
    +     Day)]
    
    > ggplot(dt$sum, aes(x = Day, y = Content)) + geom_errorbar(aes(ymin = Content - 
    +     CI, ymax = Content + CI, group = Treatment), width = 1, positi .... [TRUNCATED] 
    
    ...
    
    > p + theme_publish()
    
      When sourcing ‘theme_publish.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘calibration.Rmd’ using ‘UTF-8’... failed
      ‘texture.Rmd’ using ‘UTF-8’... OK
      ‘theme_publish.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘calibration.Rmd’ using rmarkdown
    ```

# epiCleanr

<details>

* Version: 0.2.0
* GitHub: https://github.com/truenomad/epiCleanr
* Source code: https://github.com/cran/epiCleanr
* Date/Publication: 2023-09-28 12:20:05 UTC
* Number of recursive dependencies: 130

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

# EpiInvert

<details>

* Version: 0.3.1
* GitHub: https://github.com/lalvarezmat/EpiInvert
* Source code: https://github.com/cran/EpiInvert
* Date/Publication: 2022-12-14 14:40:03 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "EpiInvert")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘EpiInvert-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: EpiInvert
    > ### Title: 'EpiInvert' estimates the reproduction number Rt and a restored
    > ###   incidence curve from the original daily incidence curve and the
    > ###   serial interval distribution. EpiInvert also corrects the festive and
    > ###   weekly biases present in the registered daily incidence.
    > ### Aliases: EpiInvert
    > 
    ...
    Backtrace:
        ▆
     1. └─EpiInvert::EpiInvert_plot(res)
     2.   └─ggplot2::ggplotGrob(g1)
     3.     ├─ggplot2::ggplot_gtable(ggplot_build(x))
     4.     └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     5.       └─ggplot2::calc_element("plot.margin", theme)
     6.         └─cli::cli_abort(...)
     7.           └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.7Mb
      sub-directories of 1Mb or more:
        data   3.2Mb
        libs   4.3Mb
    ```

# esci

<details>

* Version: 1.0.2
* GitHub: https://github.com/rcalinjageman/esci
* Source code: https://github.com/cran/esci
* Date/Publication: 2024-03-21 18:10:02 UTC
* Number of recursive dependencies: 93

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
      unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, 
        NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, l
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
      Loading required package: Matrix
      Loading required package: metadat
      Loading required package: numDeriv
    ...
       17. │             └─self$geom$use_defaults(...)
       18. └─base::.handleSimpleError(...)
       19.   └─rlang (local) h(simpleError(msg, call))
       20.     └─handlers[[1L]](cnd)
       21.       └─cli::cli_abort(...)
       22.         └─rlang::abort(...)
      
      [ FAIL 14 | WARN 15 | SKIP 0 | PASS 3182 ]
      Error: Test failures
      Execution halted
    ```

# EvidenceSynthesis

<details>

* Version: 0.5.0
* GitHub: https://github.com/OHDSI/EvidenceSynthesis
* Source code: https://github.com/cran/EvidenceSynthesis
* Date/Publication: 2023-05-08 12:20:02 UTC
* Number of recursive dependencies: 117

Run `revdepcheck::cloud_details(, "EvidenceSynthesis")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘EvidenceSynthesis-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotCovariateBalances
    > ### Title: Plot covariate balances
    > ### Aliases: plotCovariateBalances
    > 
    > ### ** Examples
    > 
    > # Some example data:
    ...
      2.   └─gridExtra::grid.arrange(data_table, plot, ncol = 2)
      3.     └─gridExtra::arrangeGrob(...)
      4.       └─base::lapply(grobs[toconv], ggplot2::ggplotGrob)
      5.         └─ggplot2 (local) FUN(X[[i]], ...)
      6.           ├─ggplot2::ggplot_gtable(ggplot_build(x))
      7.           └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
      8.             └─ggplot2::calc_element("plot.margin", theme)
      9.               └─cli::cli_abort(...)
     10.                 └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > test_check("EvidenceSynthesis")
      Loading required package: EvidenceSynthesis
      Loading required package: survival
      
      |                                                                      | 0%
      |===============================================================       | 90%df = 4.0
    ...
        5.         └─ggplot2 (local) FUN(X[[i]], ...)
        6.           ├─ggplot2::ggplot_gtable(ggplot_build(x))
        7.           └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
        8.             └─ggplot2::calc_element("plot.margin", theme)
        9.               └─cli::cli_abort(...)
       10.                 └─rlang::abort(...)
      
      [ FAIL 3 | WARN 8 | SKIP 0 | PASS 57 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘NonNormalEffectSynthesis.Rmd’
      ...
    1 2.097148 1.104696 3.783668
    
    > labels <- paste("Data site", LETTERS[1:length(populations)])
    
    > plotMetaAnalysisForest(data = approximations, labels = labels, 
    +     estimate = estimate, xLabel = "Hazard Ratio", showLikelihood = TRUE)
    
    ...
    > plotMetaAnalysisForest(data = normalApproximations, 
    +     labels = paste("Site", 1:10), estimate = fixedFxNormal, xLabel = "Hazard Ratio")
    
      When sourcing ‘VideoVignette.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘BayesianBiasCorrection.Rmd’ using ‘UTF-8’... OK
      ‘NonNormalEffectSynthesis.Rmd’ using ‘UTF-8’... failed
      ‘VideoVignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘BayesianBiasCorrection.Rmd’ using rmarkdown
    
    |                                                                      | 0%
    |======                                                                | 9%
    |============                                                          | 18%
    |===================                                                   | 27%
    |=========================                                             | 36%
    |===============================                                       | 45%
    |======================================                                | 54%
    ...
    Quitting from lines 158-164 [unnamed-chunk-9] (VideoVignette.Rmd)
    Error: processing vignette 'VideoVignette.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘VideoVignette.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘NonNormalEffectSynthesis.Rmd’ ‘VideoVignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# EvolutionaryGames

<details>

* Version: 0.1.2
* GitHub: NA
* Source code: https://github.com/cran/EvolutionaryGames
* Date/Publication: 2022-08-29 00:10:02 UTC
* Number of recursive dependencies: 66

Run `revdepcheck::cloud_details(, "EvolutionaryGames")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘EvolutionaryGames-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: phaseDiagram2S
    > ### Title: Phase Diagram for two-player games with two strategies
    > ### Aliases: phaseDiagram2S
    > 
    > ### ** Examples
    > 
    > A <- matrix(c(-1, 4, 0, 2), 2, 2, byrow=TRUE)
    ...
        ▆
     1. └─EvolutionaryGames::phaseDiagram2S(...)
     2.   ├─base::print(p + vField)
     3.   └─ggplot2:::print.ggplot(p + vField)
     4.     ├─ggplot2::ggplot_gtable(data)
     5.     └─ggplot2:::ggplot_gtable.ggplot_built(data)
     6.       └─ggplot2::calc_element("plot.margin", theme)
     7.         └─cli::cli_abort(...)
     8.           └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘UsingEvolutionaryGames.Rmd’
      ...
    > library(EvolutionaryGames)
    
    > A <- matrix(c(-1, 4, 0, 2), 2, byrow = TRUE)
    
    > phaseDiagram2S(A, Replicator, strategies = c("Hawk", 
    +     "Dove"))
    
    ...
    
    > phaseDiagram2S(A, Replicator, strategies = c("Hawk", 
    +     "Dove"))
    
      When sourcing ‘UsingEvolutionaryGames_pdf.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘UsingEvolutionaryGames.Rmd’ using ‘UTF-8’... failed
      ‘UsingEvolutionaryGames_pdf.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘UsingEvolutionaryGames.Rmd’ using rmarkdown
    ```

# EvoPhylo

<details>

* Version: 0.3.2
* GitHub: https://github.com/tiago-simoes/EvoPhylo
* Source code: https://github.com/cran/EvoPhylo
* Date/Publication: 2022-11-03 17:00:02 UTC
* Number of recursive dependencies: 164

Run `revdepcheck::cloud_details(, "EvoPhylo")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘EvoPhylo-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: make_clusters
    > ### Title: Estimate and plot character partitions
    > ### Aliases: make_clusters plot.cluster_df
    > 
    > ### ** Examples
    > 
    > # See vignette("char-part") for how to use this
    ...
    > # tSNE (3 dimensions; default is 2)
    > cluster_df_tsne <- make_clusters(Dmatrix, k = 3, tsne = TRUE,
    +                                  tsne_dim = 2)
    > 
    > # Plot clusters, plots divided into 2 rows, and increasing
    > # overlap of text labels (default = 10)
    > plot(cluster_df_tsne, nrow = 2, max.overlaps = 20)
    Error in identicalUnits(x) : object is not a unit
    Calls: <Anonymous> ... assemble_guides -> guides_build -> unit.c -> identicalUnits
    Execution halted
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘char-part.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘char-part.Rmd’
      ...
    +     collapse = TRUE, dpi = 300)
    
    > devtools::load_all(".")
    
      When sourcing ‘char-part.R’:
    Error: Could not find a root 'DESCRIPTION' file that starts with '^Package' in
    '/tmp/RtmpjL0Rwa/file19d43c89cd22/vignettes'.
    ...
    ℹ Are you in your project directory and does your project have a 'DESCRIPTION'
      file?
    Execution halted
    
      ‘char-part.Rmd’ using ‘UTF-8’... failed
      ‘data_treatment.Rmd’ using ‘UTF-8’... OK
      ‘fbd-params.Rmd’ using ‘UTF-8’... failed
      ‘offset_handling.Rmd’ using ‘UTF-8’... failed
      ‘rates-selection_BEAST2.Rmd’ using ‘UTF-8’... failed
      ‘rates-selection_MrBayes.Rmd’ using ‘UTF-8’... failed
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        data      2.5Mb
        doc       1.6Mb
        extdata   2.4Mb
    ```

# evprof

<details>

* Version: 1.1.2
* GitHub: https://github.com/mcanigueral/evprof
* Source code: https://github.com/cran/evprof
* Date/Publication: 2024-03-14 14:50:05 UTC
* Number of recursive dependencies: 93

Run `revdepcheck::cloud_details(, "evprof")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘evprof-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_energy_models
    > ### Title: Compare density of estimated energy with density of real energy
    > ###   vector
    > ### Aliases: plot_energy_models
    > 
    > ### ** Examples
    > 
    ...
      7.           └─cowplot:::as_gtable.default(x)
      8.             ├─cowplot::as_grob(plot)
      9.             └─cowplot:::as_grob.ggplot(plot)
     10.               └─ggplot2::ggplotGrob(plot)
     11.                 ├─ggplot2::ggplot_gtable(ggplot_build(x))
     12.                 └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     13.                   └─ggplot2::calc_element("plot.margin", theme)
     14.                     └─cli::cli_abort(...)
     15.                       └─rlang::abort(...)
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
       10.               └─ggplot2::ggplotGrob(plot)
       11.                 ├─ggplot2::ggplot_gtable(ggplot_build(x))
       12.                 └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
       13.                   └─ggplot2::calc_element("plot.margin", theme)
       14.                     └─cli::cli_abort(...)
       15.                       └─rlang::abort(...)
      
      [ FAIL 2 | WARN 0 | SKIP 4 | PASS 48 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        data   3.5Mb
        doc    1.2Mb
    ```

# expirest

<details>

* Version: 0.1.6
* GitHub: https://github.com/piusdahinden/expirest
* Source code: https://github.com/cran/expirest
* Date/Publication: 2024-03-25 16:30:02 UTC
* Number of recursive dependencies: 46

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
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 1121 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
        9.     └─ggplot2:::plot.ggplot(x = x$Graph, ...)
       10.       ├─ggplot2::ggplot_gtable(data)
       11.       └─ggplot2:::ggplot_gtable.ggplot_built(data)
       12.         └─ggplot2::calc_element("plot.margin", theme)
       13.           └─cli::cli_abort(...)
       14.             └─rlang::abort(...)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 1121 ]
      Error: Test failures
      Execution halted
    ```

# explore

<details>

* Version: 1.3.0
* GitHub: https://github.com/rolkra/explore
* Source code: https://github.com/cran/explore
* Date/Publication: 2024-04-15 15:50:09 UTC
* Number of recursive dependencies: 97

Run `revdepcheck::cloud_details(, "explore")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘explore-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: explore_targetpct
    > ### Title: Explore variable + binary target (values 0/1)
    > ### Aliases: explore_targetpct
    > 
    > ### ** Examples
    > 
    > iris$target01 <- ifelse(iris$Species == "versicolor",1,0)
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘explore-mtcars.Rmd’
      ...
    > data %>% explain_tree(target = highmpg)
    
    > data %>% explore(wt, target = highmpg)
    
    > data %>% explore(wt, target = highmpg, split = FALSE)
    Warning: `position_dodge()` requires non-overlapping x intervals.
    
    ...
      ‘explain.Rmd’ using ‘UTF-8’... OK
      ‘explore-mtcars.Rmd’ using ‘UTF-8’... failed
      ‘explore-penguins.Rmd’ using ‘UTF-8’... OK
      ‘explore-titanic.Rmd’ using ‘UTF-8’... OK
      ‘explore.Rmd’ using ‘UTF-8’... failed
      ‘predict.Rmd’ using ‘UTF-8’... OK
      ‘report-target.Rmd’ using ‘UTF-8’... OK
      ‘report-targetpct.Rmd’ using ‘UTF-8’... failed
      ‘report.Rmd’ using ‘UTF-8’... OK
      ‘tips-tricks.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘abtest.Rmd’ using rmarkdown
    ```

# ezplot

<details>

* Version: 0.7.13
* GitHub: NA
* Source code: https://github.com/cran/ezplot
* Date/Publication: 2024-01-28 11:30:05 UTC
* Number of recursive dependencies: 109

Run `revdepcheck::cloud_details(, "ezplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ezplot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: bar_plot
    > ### Title: bar_plot
    > ### Aliases: bar_plot
    > 
    > ### ** Examples
    > 
    > library(tsibble)
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
    when running code in ‘bar_plot.Rmd’
      ...
    
    > bar_plot(ansett, "year(Week)", "Passengers", size = 16)
    
      When sourcing ‘bar_plot.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `check_aesthetics()`:
    ...
    Caused by error in `check_aesthetics()`:
    ! Aesthetics must be either length 1 or the same as the data (9).
    ✖ Fix the following mappings: `width`.
    Execution halted
    
      ‘bar_plot.Rmd’ using ‘UTF-8’... failed
      ‘basics.Rmd’ using ‘UTF-8’... failed
      ‘line_plot.Rmd’ using ‘UTF-8’... OK
      ‘overview.Rmd’ using ‘UTF-8’... failed
      ‘variable_plot.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘bar_plot.Rmd’ using rmarkdown
    
    Quitting from lines 28-29 [unnamed-chunk-2] (bar_plot.Rmd)
    Error: processing vignette 'bar_plot.Rmd' failed with diagnostics:
    Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `check_aesthetics()`:
    ! Aesthetics must be either length 1 or the same as the data (6).
    ✖ Fix the following mappings: `width`.
    --- failed re-building ‘bar_plot.Rmd’
    
    --- re-building ‘basics.Rmd’ using rmarkdown
    ```

# fable.prophet

<details>

* Version: 0.1.0
* GitHub: https://github.com/mitchelloharawild/fable.prophet
* Source code: https://github.com/cran/fable.prophet
* Date/Publication: 2020-08-20 09:30:03 UTC
* Number of recursive dependencies: 114

Run `revdepcheck::cloud_details(, "fable.prophet")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘intro.Rmd’
      ...
     9 Domestic mdl    2019 Dec sample[5000] 5335212.
    10 Domestic mdl    2020 Jan sample[5000] 4888063.
    # ℹ 62 more rows
    
    > fc %>% autoplot(lax_passengers)
    
      When sourcing ‘intro.R’:
    Error: unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, 
        NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, lis
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

* Version: 0.4.2
* GitHub: https://github.com/tidyverts/fabletools
* Source code: https://github.com/cran/fabletools
* Date/Publication: 2024-04-22 11:22:41 UTC
* Number of recursive dependencies: 106

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
      unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, 
        NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, l
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
       24.                           └─base::Map(...)
       25.                             └─base::mapply(FUN = f, ..., SIMPLIFY = FALSE)
       26.                               └─ggplot2 (local) `<fn>`(layer = dots[[1L]][[1L]], df = dots[[2L]][[1L]])
       27.                                 └─layer$compute_geom_2(key, single_params, theme)
       28.                                   └─ggplot2 (local) compute_geom_2(..., self = self)
       29.                                     └─self$geom$use_defaults(...)
      
      [ FAIL 2 | WARN 0 | SKIP 1 | PASS 269 ]
      Error: Test failures
      Execution halted
    ```

# factoextra

<details>

* Version: 1.0.7
* GitHub: https://github.com/kassambara/factoextra
* Source code: https://github.com/cran/factoextra
* Date/Publication: 2020-04-01 21:20:02 UTC
* Number of recursive dependencies: 126

Run `revdepcheck::cloud_details(, "factoextra")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘factoextra-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: eigenvalue
    > ### Title: Extract and visualize the eigenvalues/variances of dimensions
    > ### Aliases: eigenvalue get_eig get_eigenvalue fviz_eig fviz_screeplot
    > 
    > ### ** Examples
    > 
    > # Principal Component Analysis
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

# faux

<details>

* Version: 1.2.1
* GitHub: https://github.com/debruine/faux
* Source code: https://github.com/cran/faux
* Date/Publication: 2023-04-20 07:00:11 UTC
* Number of recursive dependencies: 131

Run `revdepcheck::cloud_details(, "faux")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘faux-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: beta2norm
    > ### Title: Convert beta to normal
    > ### Aliases: beta2norm
    > 
    > ### ** Examples
    > 
    > 
    ...
    Backtrace:
        ▆
     1. └─ggExtra::ggMarginal(g, type = "histogram")
     2.   └─ggplot2::ggplotGrob(scatP)
     3.     ├─ggplot2::ggplot_gtable(ggplot_build(x))
     4.     └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     5.       └─ggplot2::calc_element("plot.margin", theme)
     6.         └─cli::cli_abort(...)
     7.           └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘codebook.Rmd’ using rmarkdown
    --- finished re-building ‘codebook.Rmd’
    
    --- re-building ‘continuous.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘norta.Rmd’
      ...
    > p <- ggplot(dat, aes(uniform_var, poisson_var)) + 
    +     geom_point() + geom_smooth()
    
    > ggMarginal(p, type = "histogram")
    `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
    `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
    
    ...
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘codebook.Rmd’ using ‘UTF-8’... OK
      ‘continuous.Rmd’ using ‘UTF-8’... OK
      ‘contrasts.Rmd’ using ‘UTF-8’... OK
      ‘norta.Rmd’ using ‘UTF-8’... failed
      ‘rnorm_multi.Rmd’ using ‘UTF-8’... OK
      ‘sim_design.Rmd’ using ‘UTF-8’... OK
      ‘sim_df.Rmd’ using ‘UTF-8’... OK
    ```

# fddm

<details>

* Version: 0.5-2
* GitHub: https://github.com/rtdists/fddm
* Source code: https://github.com/cran/fddm
* Date/Publication: 2022-09-09 19:02:54 UTC
* Number of recursive dependencies: 86

Run `revdepcheck::cloud_details(, "fddm")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘benchmark.Rmd’
      ...
    > ma <- max(bm_vec[, (t_idx + 1):(ncol(bm_vec) - 4)])
    
    > ggplot(mbm_vec, aes(x = factor(FuncName, levels = Names_vec), 
    +     y = time, color = factor(FuncName, levels = Names_vec), fill = factor(FuncName, .... [TRUNCATED] 
    Warning: The dot-dot notation (`..y..`) was deprecated in ggplot2 3.4.0.
    ℹ Please use `after_stat(y)` instead.
    
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

*   checking C++ specification ... NOTE
    ```
      Specified C++11: please drop specification unless essential
    ```

# fdrci

<details>

* Version: 2.4
* GitHub: NA
* Source code: https://github.com/cran/fdrci
* Date/Publication: 2022-10-18 02:12:32 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::cloud_details(, "fdrci")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘fdrci-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: FDRplot
    > ### Title: Plot results of FDR table generated by fdrTbl()
    > ### Aliases: FDRplot
    > 
    > ### ** Examples
    > 
    > ss = 100
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

# ffp

<details>

* Version: 0.2.2
* GitHub: https://github.com/Reckziegel/FFP
* Source code: https://github.com/cran/ffp
* Date/Publication: 2022-09-29 15:10:06 UTC
* Number of recursive dependencies: 107

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
      unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, 
        NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 2.2), NULL, 
    Calls: <Anonymous> ... <Anonymous> -> <Anonymous> -> compute_geom_2 -> <Anonymous>
    Execution halted
    ```

# fido

<details>

* Version: 1.1.0
* GitHub: https://github.com/jsilve24/fido
* Source code: https://github.com/cran/fido
* Date/Publication: 2024-05-30 07:00:24 UTC
* Number of recursive dependencies: 130

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
      unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(), NULL, list(
        NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 2.2), NULL,
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

*   checking running R code from vignettes ... WARNING
    ```
    Errors in running code in vignettes:
    when running code in ‘non-linear-models.Rmd’
      ...
    
    The following object is masked from ‘package:dplyr’:
    
        select
    
    
      When sourcing ‘non-linear-models.R’:
    Error: package or namespace load failed for ‘MCMCpack’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
     there is no package called ‘quantreg’
    Execution halted
    
      ‘introduction-to-fido.Rmd’ using ‘UTF-8’... OK
      ‘mitigating-pcrbias.Rmd’ using ‘UTF-8’... OK
      ‘non-linear-models.Rmd’ using ‘UTF-8’... failed
      ‘orthus.Rmd’ using ‘UTF-8’... OK
      ‘picking_priors.Rmd’ using ‘UTF-8’... OK
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 106.2Mb
      sub-directories of 1Mb or more:
        data    4.0Mb
        libs  100.5Mb
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
    
    > ### Name: km.plot
    > ### Title: km.plot
    > ### Aliases: km.plot
    > 
    > ### ** Examples
    > 
    > {
    ...
    Backtrace:
        ▆
     1. ├─base::print(km.M[[2]])
     2. └─ggplot2:::print.ggplot(km.M[[2]])
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘basics.Rmd’
      ...
    
    > ex.bar <- ggplot(data = working.df, aes(x = group, 
    +     fill = group)) + geom_bar() + labs(x = "Group", y = "Frequency", 
    +     title = "", fill = .... [TRUNCATED] 
    
    > print(ex.bar)
    
    ...
    > km.M[[2]]
    
      When sourcing ‘km.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘basics.Rmd’ using ‘UTF-8’... failed
      ‘forest-plots.Rmd’ using ‘UTF-8’... OK
      ‘km.Rmd’ using ‘UTF-8’... failed
      ‘large-scale.Rmd’ using ‘UTF-8’... OK
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
      installed size is 11.0Mb
      sub-directories of 1Mb or more:
        doc    9.1Mb
        libs   1.2Mb
    ```

# FMM

<details>

* Version: 0.3.1
* GitHub: https://github.com/alexARC26/FMM
* Source code: https://github.com/cran/FMM
* Date/Publication: 2021-12-17 12:52:03 UTC
* Number of recursive dependencies: 68

Run `revdepcheck::cloud_details(, "FMM")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘FMMVignette.Rmd’
      ...
    +     0.25, 0, 1), "cm")) + ylim(-5, 6) + scale_color_manual(values = brewer.pal("Set1", 
    +  .... [TRUNCATED] 
    Scale for colour is already present.
    Adding another scale for colour, which will replace the existing scale.
    
    > grid.arrange(defaultrFMM2, comprFMM2, nrow = 1)
    
      When sourcing ‘FMMVignette.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘FMMVignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘FMMVignette.Rmd’ using rmarkdown
    ```

# fmriqa

<details>

* Version: 0.3.0
* GitHub: https://github.com/martin3141/fmriqa
* Source code: https://github.com/cran/fmriqa
* Date/Publication: 2018-02-19 15:59:01 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "fmriqa")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(fmriqa)
      > 
      > test_check("fmriqa")
      Reading data  : /tmp/workdir/fmriqa/new/fmriqa.Rcheck/fmriqa/extdata/qa_data.nii.gz
      
      Basic analysis parameters
    ...
        5.         └─ggplot2 (local) FUN(X[[i]], ...)
        6.           ├─ggplot2::ggplot_gtable(ggplot_build(x))
        7.           └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
        8.             └─ggplot2::calc_element("plot.margin", theme)
        9.               └─cli::cli_abort(...)
       10.                 └─rlang::abort(...)
      
      [ FAIL 2 | WARN 2 | SKIP 1 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# foreSIGHT

<details>

* Version: 1.2.0
* GitHub: https://github.com/ClimateAnalytics/foreSIGHT
* Source code: https://github.com/cran/foreSIGHT
* Date/Publication: 2023-10-19 07:00:08 UTC
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "foreSIGHT")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘foreSIGHT-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotOptions
    > ### Title: Plots the differences in performance metrics from two system
    > ###   options
    > ### Aliases: plotOptions
    > 
    > ### ** Examples
    > 
    ...
     1. └─foreSIGHT::plotOptions(...)
     2.   ├─base::print(p1)
     3.   ├─base::print(p1)
     4.   └─ggplot2:::print.ggplot(p1)
     5.     ├─ggplot2::ggplot_gtable(data)
     6.     └─ggplot2:::ggplot_gtable.ggplot_built(data)
     7.       └─ggplot2::calc_element("plot.margin", theme)
     8.         └─cli::cli_abort(...)
     9.           └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Vignette_QuickStart_simpleScal.Rmd’ using rmarkdown_notangle
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        doc    1.3Mb
        libs   1.7Mb
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

# funcharts

<details>

* Version: 1.4.1
* GitHub: https://github.com/unina-sfere/funcharts
* Source code: https://github.com/cran/funcharts
* Date/Publication: 2024-02-22 08:50:02 UTC
* Number of recursive dependencies: 123

Run `revdepcheck::cloud_details(, "funcharts")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘funcharts-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pca_mfd
    > ### Title: Multivariate functional principal components analysis
    > ### Aliases: pca_mfd
    > 
    > ### ** Examples
    > 
    > library(funcharts)
    > mfdobj <- data_sim_mfd()
    > pca_obj <- pca_mfd(mfdobj)
    > plot_pca_mfd(pca_obj)
    Error in identicalUnits(x) : object is not a unit
    Calls: <Anonymous> ... assemble_guides -> guides_build -> unit.c -> identicalUnits
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
    ! Theme element `plot.margin` must have class <margin/rel>.
    Backtrace:
        ▆
     1. └─gapmap::gapmap(m = as.matrix(distxy), d_row = rev(dend), d_col = dend)
     2.   ├─ggplot2::ggplot_gtable(ggplot2::ggplot_build(hm))
     3.   └─ggplot2:::ggplot_gtable.ggplot_built(ggplot2::ggplot_build(hm))
     4.     └─ggplot2::calc_element("plot.margin", theme)
     5.       └─cli::cli_abort(...)
     6.         └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘simple_example.Rmd’
      ...
    +     col = grey_scale)
    Warning: The `panel.margin` argument of `theme()` is deprecated as of ggplot2 2.2.0.
    ℹ Please use the `panel.spacing` argument instead.
    ℹ The deprecated feature was likely used in the gapmap package.
      Please report the issue at
      <https://github.com/evanbiederstedt/gapmap/issues>.
    
    ...
    ℹ The deprecated feature was likely used in the gapmap package.
      Please report the issue at
      <https://github.com/evanbiederstedt/gapmap/issues>.
    
      When sourcing ‘tcga_example.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
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
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘simple_example.Rmd’
    
    --- re-building ‘tcga_example.Rmd’ using rmarkdown
    ...
    Quitting from lines 43-45 [unnamed-chunk-3] (tcga_example.Rmd)
    Error: processing vignette 'tcga_example.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘tcga_example.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘simple_example.Rmd’ ‘tcga_example.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# gasper

<details>

* Version: 1.1.6
* GitHub: https://github.com/fabnavarro/gasper
* Source code: https://github.com/cran/gasper
* Date/Publication: 2024-02-28 11:10:02 UTC
* Number of recursive dependencies: 68

Run `revdepcheck::cloud_details(, "gasper")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gasper-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_signal
    > ### Title: Plot a Signal on Top of a Given Graph
    > ### Aliases: plot_signal
    > 
    > ### ** Examples
    > 
    > f <- rnorm(length(grid1$xy[,1]))
    ...
        ▆
     1. └─gasper::plot_signal(grid1, f)
     2.   ├─base::print(p2)
     3.   └─ggplot2:::print.ggplot(p2)
     4.     ├─ggplot2::ggplot_gtable(data)
     5.     └─ggplot2:::ggplot_gtable.ggplot_built(data)
     6.       └─ggplot2::calc_element("plot.margin", theme)
     7.         └─cli::cli_abort(...)
     8.           └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘gasper_vignette.rmd’
      ...
    
    > f <- rnorm(nrow(grid1$sA))
    
    > plot_graph(grid1)
    
    > plot_signal(grid1, f, size = 2)
    
      When sourcing ‘gasper_vignette.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘gasper_vignette.rmd’ using ‘UTF-8’... failed
    ```

## In both

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘gasper_vignette.rmd’ using rmarkdown
    
    Quitting from lines 173-176 [unnamed-chunk-17] (gasper_vignette.rmd)
    Error: processing vignette 'gasper_vignette.rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘gasper_vignette.rmd’
    
    SUMMARY: processing the following file failed:
      ‘gasper_vignette.rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# gaussplotR

<details>

* Version: 0.2.5
* GitHub: https://github.com/vbaliga/gaussplotR
* Source code: https://github.com/cran/gaussplotR
* Date/Publication: 2021-05-02 20:10:02 UTC
* Number of recursive dependencies: 91

Run `revdepcheck::cloud_details(, "gaussplotR")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘fit_gaussian_2D.Rmd’
      ...
    +     by = 0.1), Y_values = seq(from = -1, to = 4, by = 0.1))
    
    > gauss_data_ue <- predict_gaussian_2D(fit_object = gauss_fit_ue, 
    +     X_values = grid$X_values, Y_values = grid$Y_values, )
    
    > ggplot_gaussian_2D(gauss_data_ue)
    
      When sourcing ‘fit_gaussian_2D.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘fit_gaussian_2D.Rmd’ using ‘UTF-8’... failed
      ‘formulas-used-by-fit-gaussian-2D.Rmd’ using ‘UTF-8’... OK
      ‘troubleshooting-model-fits.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘fit_gaussian_2D.Rmd’ using rmarkdown
    ```

# gg.gap

<details>

* Version: 1.3
* GitHub: https://github.com/ChrisLou-bioinfo/gg.gap
* Source code: https://github.com/cran/gg.gap
* Date/Publication: 2019-09-30 16:10:02 UTC
* Number of recursive dependencies: 29

Run `revdepcheck::cloud_details(, "gg.gap")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gg.gap-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: add.legend
    > ### Title: Add Legend to gg.gap()
    > ### Aliases: add.legend
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# ggalignment

<details>

* Version: 1.0.1
* GitHub: NA
* Source code: https://github.com/cran/ggalignment
* Date/Publication: 2022-11-04 10:20:02 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::cloud_details(, "ggalignment")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggalignment)
      > 
      > test_check("ggalignment")
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 8 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
        6.       └─ggplot2:::print.ggplot(p)
        7.         ├─ggplot2::ggplot_gtable(data)
        8.         └─ggplot2:::ggplot_gtable.ggplot_built(data)
        9.           └─ggplot2::calc_element("plot.margin", theme)
       10.             └─cli::cli_abort(...)
       11.               └─rlang::abort(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 8 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggalignment.Rmd’
      ...
    
        intersect, setdiff, setequal, union
    
    
    > ggalignment(alignment = data.frame(img = character(), 
    +     alignment = character()), font_size = 3)
    
      When sourcing ‘ggalignment.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘ggalignment.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ggalignment.Rmd’ using rmarkdown
    
    Quitting from lines 27-33 [example-alignment-plot] (ggalignment.Rmd)
    Error: processing vignette 'ggalignment.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘ggalignment.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ggalignment.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ggalt

<details>

* Version: 0.4.0
* GitHub: https://github.com/hrbrmstr/ggalt
* Source code: https://github.com/cran/ggalt
* Date/Publication: 2017-02-15 18:16:00
* Number of recursive dependencies: 95

Run `revdepcheck::cloud_details(, "ggalt")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ggalt_examples.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggalt_examples.Rmd’
      ...
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family 'StateFace' not found in PostScript font database
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family 'StateFace' not found in PostScript font database
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family 'StateFace' not found in PostScript font database
    
      When sourcing ‘ggalt_examples.R’:
    Error: invalid font type
    Execution halted
    
      ‘ggalt_examples.Rmd’ using ‘UTF-8’... failed
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘plotly’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# gganimate

<details>

* Version: 1.0.9
* GitHub: https://github.com/thomasp85/gganimate
* Source code: https://github.com/cran/gganimate
* Date/Publication: 2024-02-27 14:00:03 UTC
* Number of recursive dependencies: 97

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
      [ FAIL 1 | WARN 3 | SKIP 1 | PASS 5 ]
      
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
    Error: Provided file (/tmp/RtmpEoDH6s/16eb8d7241d/gganim_plot0001.png) does not
    exist
    Execution halted
    
      ‘gganimate.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘gganimate.Rmd’ using rmarkdown
    ```

# ggbrace

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/ggbrace
* Date/Publication: 2024-02-20 20:30:02 UTC
* Number of recursive dependencies: 49

Run `revdepcheck::cloud_details(, "ggbrace")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggbrace-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: stat_brace
    > ### Title: create curly braces as a layer in ggplot
    > ### Aliases: stat_brace
    > 
    > ### ** Examples
    > 
    > library(ggbrace)
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

# ggbrain

<details>

* Version: 0.8.1
* GitHub: https://github.com/michaelhallquist/ggbrain
* Source code: https://github.com/cran/ggbrain
* Date/Publication: 2023-03-21 18:00:05 UTC
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "ggbrain")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggbrain_introduction.Rmd’
      ...
    
    > gg_obj <- gg_base + geom_brain(definition = "underlay", 
    +     fill_scale = scale_fill_gradient(low = "grey8", high = "grey62"), 
    +     show_legend  .... [TRUNCATED] 
    
    > gg_obj$render()
    
    ...
    
    > plot(gg_obj)
    
      When sourcing ‘ggbrain_labels.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘ggbrain_aesthetics.Rmd’ using ‘UTF-8’... OK
      ‘ggbrain_introduction.Rmd’ using ‘UTF-8’... failed
      ‘ggbrain_labels.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ggbrain_aesthetics.Rmd’ using rmarkdown
    --- finished re-building ‘ggbrain_aesthetics.Rmd’
    
    --- re-building ‘ggbrain_introduction.Rmd’ using rmarkdown
    
    Quitting from lines 238-239 [unnamed-chunk-16] (ggbrain_introduction.Rmd)
    Error: processing vignette 'ggbrain_introduction.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    ...
    Quitting from lines 47-54 [unnamed-chunk-2] (ggbrain_labels.Rmd)
    Error: processing vignette 'ggbrain_labels.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘ggbrain_labels.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘ggbrain_introduction.Rmd’ ‘ggbrain_labels.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.5Mb
      sub-directories of 1Mb or more:
        doc       3.0Mb
        extdata   1.6Mb
        libs      5.2Mb
    ```

# ggbreak

<details>

* Version: 0.1.2
* GitHub: https://github.com/YuLab-SMU/ggbreak
* Source code: https://github.com/cran/ggbreak
* Date/Publication: 2023-06-26 05:40:02 UTC
* Number of recursive dependencies: 64

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
    Error in identicalUnits(x) : object is not a unit
    Calls: <Anonymous> -> print.ggwrap
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggbreak.Rmd’
      ...
    
    > print(pg)
    
    > pg <- pg + aes(fill = group) + theme(legend.position = "bottom")
    
    > print(pg)
    
      When sourcing ‘ggbreak.R’:
    Error: object is not a unit
    Execution halted
    
      ‘ggbreak.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ggbreak.Rmd’ using rmarkdown
    ```

# ggdark

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/ggdark
* Date/Publication: 2019-01-11 17:30:06 UTC
* Number of recursive dependencies: 46

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
      unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, 
        NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, l
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
    ! unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(7, 0, 0, 0), NULL, FALSE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, 
        NULL, NULL, 1, 90, NULL, c(0, 7, 0, 0), NULL, FALSE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2
    Execution halted
    
      ‘dotsinterval.Rmd’ using ‘UTF-8’... failed
      ‘freq-uncertainty-vis.Rmd’ using ‘UTF-8’... failed
      ‘lineribbon.Rmd’ using ‘UTF-8’... failed
      ‘slabinterval.Rmd’ using ‘UTF-8’... failed
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
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
* Number of recursive dependencies: 95

Run `revdepcheck::cloud_details(, "ggedit")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggedit-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: dput.ggedit
    > ### Title: Convert ggplot object to a string call
    > ### Aliases: dput.ggedit
    > 
    > ### ** Examples
    > 
    > 
    >  pList$pointSmooth #original compiled plot
    `geom_smooth()` using formula = 'y ~ x'
    Error in compute_geom_2(..., self = self) : 
      unused arguments (list(6), list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, 
        NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL,
    Calls: <Anonymous> ... get_layer_key -> Map -> mapply -> <Anonymous> -> <Anonymous>
    Execution halted
    ```

# ggExtra

<details>

* Version: 0.10.1
* GitHub: https://github.com/daattali/ggExtra
* Source code: https://github.com/cran/ggExtra
* Date/Publication: 2023-08-21 14:40:02 UTC
* Number of recursive dependencies: 118

Run `revdepcheck::cloud_details(, "ggExtra")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggExtra.Rmd’
      ...
    
    > p1 <- ggplot(df1, aes(x, y)) + geom_point() + theme_bw()
    
    > p1
    
    > ggMarginal(p1)
    
      When sourcing ‘ggExtra.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘ggExtra.Rmd’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ggExtra.Rmd’ using rmarkdown
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘R6’ ‘scales’ ‘utils’
      All declared Imports should be used.
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
      test_nthreads.R...............    0 tests    ----- FAILED[]: test_ggiplot.R<52--52>
       call| expect_snapshot_plot(p3, label = "ggiplot_simple_ribbon")
       diff| 54503
       info| Diff plot saved to: _tinysnapshot_review/ggiplot_simple_ribbon.png
      ----- FAILED[]: test_ggiplot.R<54--54>
       call| expect_snapshot_plot(p5, label = "ggiplot_simple_mci_ribbon")
       diff| 54400
       info| Diff plot saved to: _tinysnapshot_review/ggiplot_simple_mci_ribbon.png
      Error: 2 out of 101 tests failed
      Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggiplot.Rmd’
      ...
    > iplot(list(TWFE = est_twfe_grp, `Sun & Abraham (2020)` = est_sa20_grp), 
    +     ref.line = -1, main = "Staggered treatment: Split mutli-sample")
    The degrees of freedom for the t distribution could not be deduced. Using a Normal distribution instead.
    Note that you can provide the argument `df.t` directly.
    
      When sourcing ‘ggiplot.R’:
    Error: in iplot(list(TWFE = est_twfe_grp, `Sun & Abraham (2...: 
    The 1st element of 'object' raises and error:
    Error in nb * sd : non-numeric argument to binary operator
    Execution halted
    
      ‘ggiplot.Rmd’ using ‘UTF-8’... failed
    ```

# ggflowchart

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/ggflowchart
* Date/Publication: 2023-05-11 10:10:05 UTC
* Number of recursive dependencies: 59

Run `revdepcheck::cloud_details(, "ggflowchart")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggflowchart-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggflowchart
    > ### Title: Generate a flowchart in ggplot2
    > ### Aliases: ggflowchart
    > 
    > ### ** Examples
    > 
    > data <- tibble::tibble(from = c("A", "A", "A", "B", "C", "F"), to = c("B", "C", "D", "E", "F", "G"))
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘decision-tree-example.Rmd’
      ...
    
    > node_data <- tibble::tibble(name = c("Goldilocks", 
    +     "Porridge", "Just right", "Chairs", "Just right2", "Beds", 
    +     "Just right3", "Too cold ..." ... [TRUNCATED] 
    
    > ggflowchart(goldilocks, node_data)
    
    ...
    +     "C", "F"), to = c("B", "C", "D", "E", "F", "G"))
    
    > ggflowchart(data)
    
      When sourcing ‘minimal-example.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘decision-tree-example.Rmd’ using ‘UTF-8’... failed
      ‘minimal-example.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘decision-tree-example.Rmd’ using rmarkdown
    
    Quitting from lines 64-65 [flowchart] (decision-tree-example.Rmd)
    Error: processing vignette 'decision-tree-example.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘decision-tree-example.Rmd’
    
    --- re-building ‘minimal-example.Rmd’ using rmarkdown
    ...
    Quitting from lines 31-32 [flowchart] (minimal-example.Rmd)
    Error: processing vignette 'minimal-example.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘minimal-example.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘decision-tree-example.Rmd’ ‘minimal-example.Rmd’
    
    Error: Vignette re-building failed.
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
    
    > ### Name: facet_zoom
    > ### Title: Facet data for zoom with context
    > ### Aliases: facet_zoom
    > 
    > ### ** Examples
    > 
    > # Zoom in on the versicolor species on the x-axis
    > ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
    +   geom_point() +
    +   facet_zoom(x = Species == 'versicolor')
    Error in upgradeUnit.default(x) : Not a unit object
    Calls: <Anonymous> ... is.unit -> convertUnit -> upgradeUnit -> upgradeUnit.default
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 27.7Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        help   1.2Mb
        libs  24.9Mb
    ```

# ggfortify

<details>

* Version: 0.4.17
* GitHub: https://github.com/sinhrks/ggfortify
* Source code: https://github.com/cran/ggfortify
* Date/Publication: 2024-04-17 04:30:04 UTC
* Number of recursive dependencies: 125

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
      
      [ FAIL 5 | WARN 12 | SKIP 48 | PASS 734 ]
      Error: Test failures
      Execution halted
    ```

# ggfoundry

<details>

* Version: 0.1.1
* GitHub: https://github.com/cgoo4/ggfoundry
* Source code: https://github.com/cran/ggfoundry
* Date/Publication: 2024-05-28 11:40:02 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::cloud_details(, "ggfoundry")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggfoundry.Rmd’
      ...
    
    > p2 <- p + geom_point(size = 4) + scale_shape_manual(values = c("▼", 
    +     "●", "▲")) + labs(title = "geom_point with unicodes", 
    +     subtitle = " ..." ... [TRUNCATED] 
    
    > p1 + p2 + plot_layout(guides = "collect", axes = "collect")
    
      When sourcing ‘ggfoundry.R’:
    Error: object is not coercible to a unit
    Execution halted
    
      ‘ggfoundry.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ggfoundry.Rmd’ using rmarkdown
    
    Quitting from lines 50-94 [unicodes] (ggfoundry.Rmd)
    Error: processing vignette 'ggfoundry.Rmd' failed with diagnostics:
    object is not coercible to a unit
    --- failed re-building ‘ggfoundry.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ggfoundry.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# gggap

<details>

* Version: 1.0.1
* GitHub: https://github.com/cmoralesmx/gggap
* Source code: https://github.com/cran/gggap
* Date/Publication: 2020-11-20 09:20:02 UTC
* Number of recursive dependencies: 29

Run `revdepcheck::cloud_details(, "gggap")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gggap-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gggap
    > ### Title: Define Segments in y-Axis for 'ggplot2'
    > ### Aliases: gggap
    > 
    > ### ** Examples
    > 
    > data(mtcars)
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# ggh4x

<details>

* Version: 0.2.8
* GitHub: https://github.com/teunbrand/ggh4x
* Source code: https://github.com/cran/ggh4x
* Date/Publication: 2024-01-23 21:00:02 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::cloud_details(, "ggh4x")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggh4x-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: guide_dendro
    > ### Title: Dendrogram guide
    > ### Aliases: guide_dendro
    > 
    > ### ** Examples
    > 
    > clust <- hclust(dist(USArrests), "ave")
    ...
      9.             └─ggplot2:::scale_apply(layer_data, y_vars, "map", SCALE_Y, self$panel_scales_y)
     10.               └─base::lapply(...)
     11.                 └─ggplot2 (local) FUN(X[[i]], ...)
     12.                   └─base::lapply(...)
     13.                     └─ggplot2 (local) FUN(X[[i]], ...)
     14.                       └─scales[[i]][[method]](data[[var]][scale_index[[i]]])
     15.                         └─ggplot2 (local) map(..., self = self)
     16.                           └─cli::cli_abort(...)
     17.                             └─rlang::abort(...)
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
      [ FAIL 7 | WARN 20 | SKIP 18 | PASS 740 ]
      
    ...
       14.                   └─base::lapply(...)
       15.                     └─ggplot2 (local) FUN(X[[i]], ...)
       16.                       └─scales[[i]][[method]](data[[var]][scale_index[[i]]])
       17.                         └─ggplot2 (local) map(..., self = self)
       18.                           └─cli::cli_abort(...)
       19.                             └─rlang::abort(...)
      
      [ FAIL 7 | WARN 20 | SKIP 18 | PASS 740 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Facets.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Miscellaneous.Rmd’
      ...
    
    > ggplot(diamonds, aes(price, carat, colour = clarity)) + 
    +     geom_point(shape = ".") + scale_colour_brewer(palette = "Dark2", 
    +     guide = "stri ..." ... [TRUNCATED] 
    Warning: The S3 guide system was deprecated in ggplot2 3.5.0.
    ℹ It has been replaced by a ggproto system that can be extended.
    
    ...
    ℹ Error occurred in the 1st layer.
    Caused by error in `setup_params()`:
    ! A discrete 'nbinom' distribution cannot be fitted to continuous data.
    Execution halted
    
      ‘Facets.Rmd’ using ‘UTF-8’... OK
      ‘Miscellaneous.Rmd’ using ‘UTF-8’... failed
      ‘PositionGuides.Rmd’ using ‘UTF-8’... failed
      ‘Statistics.Rmd’ using ‘UTF-8’... failed
      ‘ggh4x.Rmd’ using ‘UTF-8’... OK
    ```

# gghdx

<details>

* Version: 0.1.3
* GitHub: https://github.com/OCHA-DAP/gghdx
* Source code: https://github.com/cran/gghdx
* Date/Publication: 2024-05-14 19:50:02 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "gghdx")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gghdx-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gghdx
    > ### Title: Set HDX theme and aesthetics
    > ### Aliases: gghdx gghdx_reset
    > 
    > ### ** Examples
    > 
    > 
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘gghdx.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘gghdx.Rmd’
      ...
    
    > p
    
    > library(gghdx)
    
    > p + theme_hdx(base_family = "sans")
    
      When sourcing ‘gghdx.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘gghdx.Rmd’ using ‘UTF-8’... failed
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

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘gghighlight.Rmd’
      ...
    +     0, label_key = type)
    Warning in is.na(non_null_default_aes[[aes_param_name]]) :
      is.na() applied to non-(list or vector) of type 'language'
    
      When sourcing ‘gghighlight.R’:
    Error: ℹ In index: 1.
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

# ggHoriPlot

<details>

* Version: 1.0.1
* GitHub: https://github.com/rivasiker/ggHoriPlot
* Source code: https://github.com/cran/ggHoriPlot
* Date/Publication: 2022-10-11 16:22:33 UTC
* Number of recursive dependencies: 117

Run `revdepcheck::cloud_details(, "ggHoriPlot")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggHoriPlot.Rmd’
      ...
    > mid <- sum(range(dat_tab$y, na.rm = T))/2
    
    > b <- plotAllLayers(dat_tab, mid, cutpoints$cuts, cutpoints$color)
    
    > b/a + plot_layout(guides = "collect", heights = c(6, 
    +     1))
    
      When sourcing ‘ggHoriPlot.R’:
    Error: object is not a unit
    Execution halted
    
      ‘examples.Rmd’ using ‘UTF-8’... OK
      ‘ggHoriPlot.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘examples.Rmd’ using rmarkdown
    ```

# ggiraph

<details>

* Version: 0.8.10
* GitHub: https://github.com/davidgohel/ggiraph
* Source code: https://github.com/cran/ggiraph
* Date/Publication: 2024-05-17 12:10:02 UTC
* Number of recursive dependencies: 95

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

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘quantreg’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  9.7Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        libs   6.9Mb
    ```

# ggiraphExtra

<details>

* Version: 0.3.0
* GitHub: https://github.com/cardiomoon/ggiraphExtra
* Source code: https://github.com/cran/ggiraphExtra
* Date/Publication: 2020-10-06 07:00:02 UTC
* Number of recursive dependencies: 124

Run `revdepcheck::cloud_details(, "ggiraphExtra")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggiraphExtra-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggAncova
    > ### Title: Make an interactive plot for an ANCOVA model
    > ### Aliases: ggAncova ggAncova.default ggAncova.formula ggAncova.lm
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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘introduction.Rmd’
      ...
    
    > ggPoints(aes(x = wt, y = mpg, color = am), data = mtcars, 
    +     method = "lm", interactive = TRUE)
    
      When sourcing ‘introduction.R’:
    Error: Problem while converting geom to grob.
    ℹ Error occurred in the 3rd layer.
    Caused by error in `draw_panel()`:
    ! unused argument (arrow.fill = NULL)
    Execution halted
    
      ‘ggPredict.Rmd’ using ‘UTF-8’... OK
      ‘introduction.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ggPredict.Rmd’ using rmarkdown
    ```

# ggmap

<details>

* Version: 4.0.0
* GitHub: https://github.com/dkahle/ggmap
* Source code: https://github.com/cran/ggmap
* Date/Publication: 2023-11-19 08:10:02 UTC
* Number of recursive dependencies: 66

Run `revdepcheck::cloud_details(, "ggmap")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggmap-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: theme_nothing
    > ### Title: Make a blank ggplot2 theme.
    > ### Aliases: theme_nothing
    > 
    > ### ** Examples
    > 
    > 
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
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
    
    Error in use_defaults(..., self = self) : 
      unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, 
        NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, l
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
      ── Error ('test_stat.R:18:3'): test stat ───────────────────────────────────────
      Error in `stat_hist_(prop = 0.5)`: Problem while setting up geom aesthetics.
      ℹ Error occurred in the 1st layer.
      Caused by error in `check_aesthetics()`:
      ! Aesthetics must be either length 1 or the same as the data (83).
      ✖ Fix the following mappings: `width`.
      
      [ FAIL 5 | WARN 1 | SKIP 0 | PASS 21 ]
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
    Error: unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), 
        list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 2.2), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0.5, NULL, NULL, NULL, c(0, 2.2, 0, 2.2), NULL, TRUE), list("grey20", NULL, NULL, NULL, FALSE, "grey20",
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
* Number of recursive dependencies: 51

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
       12.                     └─self$get_layer_key(params, layers[include], data[include], theme)
       13.                       └─ggplot2 (local) get_layer_key(...)
       14.                         └─base::Map(...)
       15.                           └─base::mapply(FUN = f, ..., SIMPLIFY = FALSE)
       16.                             └─ggplot2 (local) `<fn>`(layer = dots[[1L]][[1L]], df = dots[[2L]][[1L]])
       17.                               └─layer$compute_geom_2(key, single_params, theme)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

# ggpicrust2

<details>

* Version: 1.7.3
* GitHub: https://github.com/cafferychen777/ggpicrust2
* Source code: https://github.com/cran/ggpicrust2
* Date/Publication: 2023-11-08 16:10:02 UTC
* Number of recursive dependencies: 238

Run `revdepcheck::cloud_details(, "ggpicrust2")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggpicrust2-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pathway_pca
    > ### Title: Perform Principal Component Analysis (PCA) on functional pathway
    > ###   abundance data and create visualizations of the PCA results.
    > ### Aliases: pathway_pca
    > 
    > ### ** Examples
    > 
    ...
    > 
    > # Create example metadata
    > # Please ensure the sample IDs in the metadata have the column name "sample_name"
    > metadata_example <- data.frame(sample_name = colnames(kegg_abundance_example),
    +                                group = factor(rep(c("Control", "Treatment"), each = 5)))
    > 
    > pca_plot <- pathway_pca(kegg_abundance_example, metadata_example, "group")
    Error in identicalUnits(x) : object is not a unit
    Calls: pathway_pca ... assemble_guides -> guides_build -> unit.c -> identicalUnits
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        R      2.1Mb
        data   2.0Mb
    ```

# ggpie

<details>

* Version: 0.2.5
* GitHub: https://github.com/showteeth/ggpie
* Source code: https://github.com/cran/ggpie
* Date/Publication: 2022-11-16 07:40:06 UTC
* Number of recursive dependencies: 59

Run `revdepcheck::cloud_details(, "ggpie")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggpie-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggdonut
    > ### Title: Create donut plot.
    > ### Aliases: ggdonut
    > 
    > ### ** Examples
    > 
    > library(ggpie)
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggpie.Rmd’
      ...
     $ x      : num [1:53940] 3.95 3.89 4.05 4.2 4.34 3.94 3.95 4.07 3.87 4 ...
     $ y      : num [1:53940] 3.98 3.84 4.07 4.23 4.35 3.96 3.98 4.11 3.78 4.05 ...
     $ z      : num [1:53940] 2.43 2.31 2.31 2.63 2.75 2.48 2.47 2.53 2.49 2.39 ...
    
    > ggpie(data = diamonds, group_key = "cut", count_type = "full", 
    +     label_type = "none")
    
      When sourcing ‘ggpie.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘ggpie.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ggpie.Rmd’ using rmarkdown
    
    Quitting from lines 73-75 [pie_basic_no_label] (ggpie.Rmd)
    Error: processing vignette 'ggpie.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘ggpie.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ggpie.Rmd’
    
    Error: Vignette re-building failed.
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

# ggprism

<details>

* Version: 1.0.5
* GitHub: https://github.com/csdaw/ggprism
* Source code: https://github.com/cran/ggprism
* Date/Publication: 2024-03-21 10:50:02 UTC
* Number of recursive dependencies: 105

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
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

# ggpubr

<details>

* Version: 0.6.0
* GitHub: https://github.com/kassambara/ggpubr
* Source code: https://github.com/cran/ggpubr
* Date/Publication: 2023-02-10 16:20:02 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::cloud_details(, "ggpubr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggpubr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggpie
    > ### Title: Pie chart
    > ### Aliases: ggpie
    > 
    > ### ** Examples
    > 
    > 
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

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
      [ FAIL 2 | WARN 5 | SKIP 0 | PASS 183 ]
      
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
      unused argument (list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, list(), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, 
        NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 2.2), NULL, T
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
      installed size is  9.0Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        doc    3.9Mb
        libs   2.9Mb
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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘introduction.Rmd’
      ...
    > resid_interact(penguin_model, plots = c("resid", "qq"))
    Warning: The following aesthetics were dropped during statistical transformation: label.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    
      When sourcing ‘introduction.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘introduction.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘introduction.Rmd’ using rmarkdown
    ```

# ggseqplot

<details>

* Version: 0.8.4
* GitHub: https://github.com/maraab23/ggseqplot
* Source code: https://github.com/cran/ggseqplot
* Date/Publication: 2024-05-17 21:40:03 UTC
* Number of recursive dependencies: 139

Run `revdepcheck::cloud_details(, "ggseqplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggseqplot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggseqmsplot
    > ### Title: Modal State Sequence Plot
    > ### Aliases: ggseqmsplot
    > 
    > ### ** Examples
    > 
    > # Use example data from TraMineR: actcal data set
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

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggseqplot)
      Loading required package: TraMineR
      
      TraMineR stable version 2.2-10 (Built: 2024-05-22)
      Website: http://traminer.unige.ch
      Please type 'citation("TraMineR")' for citation information.
    ...
      Backtrace:
          ▆
       1. ├─testthat::expect_s3_class(ggseqtrplot(biofam.seq), "ggplot") at test-ggseqtrplot.R:35:3
       2. │ └─testthat::quasi_label(enquo(object), arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. └─ggseqplot::ggseqtrplot(biofam.seq)
      
      [ FAIL 1 | WARN 1036 | SKIP 0 | PASS 131 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggseqplot.Rmd’
      ...
    > p1 + p2 + plot_layout(guides = "collect") & scale_fill_manual(values = canva_palettes$`Fun and tropical`[1:4]) & 
    +     theme_ipsum(base_family = "" .... [TRUNCATED] 
    Scale for fill is already present.
    Adding another scale for fill, which will replace the existing scale.
    Scale for fill is already present.
    Adding another scale for fill, which will replace the existing scale.
    
      When sourcing ‘ggseqplot.R’:
    Error: object is not coercible to a unit
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
* Number of recursive dependencies: 76

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
    Codoc mismatches from documentation object 'geom_xsidebar':
    geom_xsidebar
      Code: function(mapping = NULL, data = NULL, stat = "count", position
                     = "stack", ..., just = 0.5, na.rm = FALSE, orientation
                     = "x", show.legend = NA, inherit.aes = TRUE)
      Docs: function(mapping = NULL, data = NULL, stat = "count", position
                     = "stack", ..., just = 0.5, width = NULL, na.rm =
                     FALSE, orientation = "x", show.legend = NA,
                     inherit.aes = TRUE)
      Argument names in docs not in code:
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

# ggstatsplot

<details>

* Version: 0.12.3
* GitHub: https://github.com/IndrajeetPatil/ggstatsplot
* Source code: https://github.com/cran/ggstatsplot
* Date/Publication: 2024-04-06 17:42:59 UTC
* Number of recursive dependencies: 168

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
      > if (getRversion() < "4.4.0") {
      +   library(testthat)
      +   suppressPackageStartupMessages(library(ggstatsplot))
      + 
      +   test_check("ggstatsplot")
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

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggstatsplot.Rmd’
      ...
        journal = {{Journal of Open Source Software}},
      }
    
    > ggbetweenstats(iris, Species, Sepal.Length)
    
    > knitr::include_graphics("../man/figures/stats_reporting_format.png")
    
      When sourcing ‘ggstatsplot.R’:
    Error: Cannot find the file(s): "../man/figures/stats_reporting_format.png"
    Execution halted
    
      ‘additional.Rmd’ using ‘UTF-8’... OK
      ‘ggstatsplot.Rmd’ using ‘UTF-8’... failed
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

*   checking examples ... ERROR
    ```
    Running examples in ‘ggtern-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: annotate
    > ### Title: Create an annotation layer (ggtern version).
    > ### Aliases: annotate
    > 
    > ### ** Examples
    > 
    > ggtern() + 
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
    Package which this enhances but not available for checking: ‘sp’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘chemometrics’
    ```

# ggthemes

<details>

* Version: 5.1.0
* GitHub: https://github.com/jrnold/ggthemes
* Source code: https://github.com/cran/ggthemes
* Date/Publication: 2024-02-10 00:30:02 UTC
* Number of recursive dependencies: 101

Run `revdepcheck::cloud_details(, "ggthemes")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggthemes-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: theme_economist
    > ### Title: ggplot color theme based on the Economist
    > ### Aliases: theme_economist theme_economist_white
    > 
    > ### ** Examples
    > 
    > library("ggplot2")
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘quantreg’
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 138 marked UTF-8 strings
    ```

# ggupset

<details>

* Version: 0.3.0
* GitHub: https://github.com/const-ae/ggupset
* Source code: https://github.com/cran/ggupset
* Date/Publication: 2020-05-05 10:40:03 UTC
* Number of recursive dependencies: 46

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
* Number of recursive dependencies: 98

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

# graphPAF

<details>

* Version: 2.0.0
* GitHub: https://github.com/johnfergusonNUIG/graphPAF
* Source code: https://github.com/cran/graphPAF
* Date/Publication: 2023-12-21 00:50:06 UTC
* Number of recursive dependencies: 50

Run `revdepcheck::cloud_details(, "graphPAF")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘graphPAF-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.rf.data.frame
    > ### Title: Create a fan_plot of a rf.data.frame object
    > ### Aliases: plot.rf.data.frame
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

# greatR

<details>

* Version: 2.0.0
* GitHub: https://github.com/ruthkr/greatR
* Source code: https://github.com/cran/greatR
* Date/Publication: 2024-04-09 22:40:07 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::cloud_details(, "greatR")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘process-results.Rmd’
      ...
    
    > reg_summary$non_registered_genes
    [1] "BRAA02G018970.3C"
    
    > plot(reg_summary, type = "registered", scatterplot_size = c(4, 
    +     3.5))
    
      When sourcing ‘process-results.R’:
    Error: object is not a unit
    Execution halted
    
      ‘data-requirement.Rmd’ using ‘UTF-8’... OK
      ‘process-results.Rmd’ using ‘UTF-8’... failed
      ‘register-data-manually.Rmd’ using ‘UTF-8’... OK
      ‘register-data.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘data-requirement.Rmd’ using rmarkdown
    --- finished re-building ‘data-requirement.Rmd’
    
    --- re-building ‘process-results.Rmd’ using rmarkdown
    
    Quitting from lines 76-81 [plot-summary-results] (process-results.Rmd)
    Error: processing vignette 'process-results.Rmd' failed with diagnostics:
    object is not a unit
    ...
    --- finished re-building ‘register-data-manually.Rmd’
    
    --- re-building ‘register-data.Rmd’ using rmarkdown
    --- finished re-building ‘register-data.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘process-results.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# Greymodels

<details>

* Version: 2.0.1
* GitHub: https://github.com/havishaJ/Greymodels
* Source code: https://github.com/cran/Greymodels
* Date/Publication: 2022-12-05 12:42:35 UTC
* Number of recursive dependencies: 91

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

# groupdata2

<details>

* Version: 2.0.3
* GitHub: https://github.com/ludvigolsen/groupdata2
* Source code: https://github.com/cran/groupdata2
* Date/Publication: 2023-06-18 12:30:02 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "groupdata2")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘description_of_groupdata2.Rmd’
      ...
    
    > greedy_plot <- ggplot(greedy_data, aes(x, freq, color = Size))
    
    > greedy_plot + geom_point() + labs(x = "group", y = "group Size", 
    +     title = "Greedy Distribution of Elements in groups", color = "Size") + 
    +    .... [TRUNCATED] 
    
      When sourcing ‘description_of_groupdata2.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘automatic_groups_with_groupdata2.Rmd’ using ‘UTF-8’... OK
      ‘cross-validation_with_groupdata2.Rmd’ using ‘UTF-8’... OK
      ‘description_of_groupdata2.Rmd’ using ‘UTF-8’... failed
      ‘introduction_to_groupdata2.Rmd’ using ‘UTF-8’... OK
      ‘time_series_with_groupdata2.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘automatic_groups_with_groupdata2.Rmd’ using rmarkdown
    --- finished re-building ‘automatic_groups_with_groupdata2.Rmd’
    
    --- re-building ‘cross-validation_with_groupdata2.Rmd’ using rmarkdown
    Loading required namespace: broom
    --- finished re-building ‘cross-validation_with_groupdata2.Rmd’
    
    --- re-building ‘description_of_groupdata2.Rmd’ using rmarkdown
    ```

# GSD

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/GSD
* Date/Publication: 2024-02-05 20:40:13 UTC
* Number of recursive dependencies: 32

Run `revdepcheck::cloud_details(, "GSD")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘GSD-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gfdecomp
    > ### Title: Graph Fourier Decomposition
    > ### Aliases: gfdecomp
    > ### Keywords: nonparametric
    > 
    > ### ** Examples
    > 
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

# gtExtras

<details>

* Version: 0.5.0
* GitHub: https://github.com/jthomasmock/gtExtras
* Source code: https://github.com/cran/gtExtras
* Date/Publication: 2023-09-15 22:32:06 UTC
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
       18.                       └─ggplot2:::print.ggplot(x)
       19.                         ├─ggplot2::ggplot_gtable(data)
       20.                         └─ggplot2:::ggplot_gtable.ggplot_built(data)
       21.                           └─ggplot2::calc_element("plot.margin", theme)
       22.                             └─cli::cli_abort(...)
       23.                               └─rlang::abort(...)
      
      [ FAIL 1 | WARN 14 | SKIP 23 | PASS 112 ]
      Error: Test failures
      Execution halted
    ```

# HaploCatcher

<details>

* Version: 1.0.4
* GitHub: NA
* Source code: https://github.com/cran/HaploCatcher
* Date/Publication: 2023-04-21 23:32:39 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::cloud_details(, "HaploCatcher")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘An_Intro_to_HaploCatcher.Rmd’
      ...
    > set.seed(NULL)
    
    > results1 <- auto_locus(geno_mat = geno_mat, gene_file = gene_comp, 
    +     gene_name = "sst1_solid_stem", marker_info = marker_info, 
    +     chromosom .... [TRUNCATED] 
    Loading required package: lattice
    
      When sourcing ‘An_Intro_to_HaploCatcher.R’:
    Error: object is not a unit
    Execution halted
    
      ‘An_Intro_to_HaploCatcher.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘An_Intro_to_HaploCatcher.Rmd’ using rmarkdown
    
    Quitting from lines 242-253 [example_models_1] (An_Intro_to_HaploCatcher.Rmd)
    Error: processing vignette 'An_Intro_to_HaploCatcher.Rmd' failed with diagnostics:
    object is not a unit
    --- failed re-building ‘An_Intro_to_HaploCatcher.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘An_Intro_to_HaploCatcher.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# hdnom

<details>

* Version: 6.0.3
* GitHub: https://github.com/nanxstats/hdnom
* Source code: https://github.com/cran/hdnom
* Date/Publication: 2024-03-03 03:20:02 UTC
* Number of recursive dependencies: 66

Run `revdepcheck::cloud_details(, "hdnom")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘hdnom-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: calibrate
    > ### Title: Calibrate high-dimensional Cox models
    > ### Aliases: calibrate
    > 
    > ### ** Examples
    > 
    > data("smart")
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘hdnom.Rmd’
      ...
    Mean     0.6841580 0.6935303
    Min      0.6770945 0.6800316
    0.25 Qt. 0.6821133 0.6924729
    Median   0.6831368 0.6956285
    0.75 Qt. 0.6864527 0.6966638
    Max      0.6939574 0.6997908
    
      When sourcing ‘hdnom.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘hdnom.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘hdnom.Rmd’ using rmarkdown
    ```

# healthyR

<details>

* Version: 0.2.1
* GitHub: https://github.com/spsanderson/healthyR
* Source code: https://github.com/cran/healthyR
* Date/Publication: 2023-04-06 22:20:03 UTC
* Number of recursive dependencies: 158

Run `revdepcheck::cloud_details(, "healthyR")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘getting-started.Rmd’
      ...
    
    > ts_alos_plt(.data = df_tbl, .date_col = Date, .value_col = Values, 
    +     .by = "month", .interactive = FALSE)
    
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

# healthyR.ai

<details>

* Version: 0.0.13
* GitHub: https://github.com/spsanderson/healthyR.ai
* Source code: https://github.com/cran/healthyR.ai
* Date/Publication: 2023-04-03 00:20:02 UTC
* Number of recursive dependencies: 229

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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘getting-started.Rmd’
      ...
    > pca_list <- pca_your_recipe(.recipe_object = rec_obj, 
    +     .data = data_tbl, .threshold = 0.8, .top_n = 5)
    Warning: !  The following columns have zero variance so scaling cannot be used:
      date_col_day, date_col_mday, date_col_mweek, and date_col_mday7.
    ℹ Consider using ?step_zv (`?recipes::step_zv()`) to remove those columns
      before normalizing.
    
      When sourcing ‘getting-started.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘auto-kmeans.Rmd’ using ‘UTF-8’... OK
      ‘getting-started.Rmd’ using ‘UTF-8’... failed
      ‘kmeans-umap.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘auto-kmeans.Rmd’ using rmarkdown
    --- finished re-building ‘auto-kmeans.Rmd’
    
    --- re-building ‘getting-started.Rmd’ using rmarkdown
    
    Quitting from lines 107-113 [pca_your_rec] (getting-started.Rmd)
    Error: processing vignette 'getting-started.Rmd' failed with diagnostics:
    subscript out of bounds
    --- failed re-building ‘getting-started.Rmd’
    
    --- re-building ‘kmeans-umap.Rmd’ using rmarkdown
    ```

# healthyR.ts

<details>

* Version: 0.3.0
* GitHub: https://github.com/spsanderson/healthyR.ts
* Source code: https://github.com/cran/healthyR.ts
* Date/Publication: 2023-11-15 06:00:05 UTC
* Number of recursive dependencies: 222

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
        doc   5.2Mb
    ```

# heatmaply

<details>

* Version: 1.5.0
* GitHub: https://github.com/talgalili/heatmaply
* Source code: https://github.com/cran/heatmaply
* Date/Publication: 2023-10-06 20:50:02 UTC
* Number of recursive dependencies: 111

Run `revdepcheck::cloud_details(, "heatmaply")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘heatmaply-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggheatmap
    > ### Title: ggplot heatmap equivalent to heatmaply
    > ### Aliases: ggheatmap
    > 
    > ### ** Examples
    > 
    > ggheatmap(mtcars)
    ...
      2.   └─heatmaply:::arrange_plots(...)
      3.     └─egg::ggarrange(...)
      4.       └─base::lapply(plots, ggplot2::ggplotGrob)
      5.         └─ggplot2 (local) FUN(X[[i]], ...)
      6.           ├─ggplot2::ggplot_gtable(ggplot_build(x))
      7.           └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
      8.             └─ggplot2::calc_element("plot.margin", theme)
      9.               └─cli::cli_abort(...)
     10.                 └─rlang::abort(...)
    Execution halted
    ```

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
      
      [ FAIL 59 | WARN 0 | SKIP 0 | PASS 185 ]
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

# hermiter

<details>

* Version: 2.3.1
* GitHub: https://github.com/MikeJaredS/hermiter
* Source code: https://github.com/cran/hermiter
* Date/Publication: 2024-03-06 23:50:02 UTC
* Number of recursive dependencies: 79

Run `revdepcheck::cloud_details(, "hermiter")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘hermiter.Rmd’
      ...
    > p2 <- ggplot(df_pdf_cdf) + geom_tile(aes(X, Y, fill = pdf_est)) + 
    +     scale_fill_continuous_sequential(palette = "Oslo", breaks = seq(0, 
    +       .... [TRUNCATED] 
    
    > p1 + ggtitle("Actual PDF") + theme(legend.title = element_blank()) + 
    +     p2 + ggtitle("Estimated PDF") + theme(legend.title = element_blank()) +  .... [TRUNCATED] 
    
      When sourcing ‘hermiter.R’:
    Error: object is not a unit
    Execution halted
    
      ‘hermiter.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘hermiter.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        R      2.6Mb
        doc    1.9Mb
        libs   1.8Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# heumilkr

<details>

* Version: 0.2.0
* GitHub: https://github.com/lschneiderbauer/heumilkr
* Source code: https://github.com/cran/heumilkr
* Date/Publication: 2024-04-01 13:50:06 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "heumilkr")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘clarke_wright_performance.Rmd’
      ...
    +     "F", "tai"), group_desc = c("Augerat A, 1995", "Augerat B, 1995", 
    +     "Christofides and ..." ... [TRUNCATED] 
    
    > ggMarginal(ggplot(merge(result, description, by = "group"), 
    +     aes(x = n_site, y = clarke_wright_perf_xi, color = group_desc)) + 
    +     geom_poi .... [TRUNCATED] 
    
      When sourcing ‘clarke_wright_performance.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘clarke_wright_performance.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘clarke_wright_performance.Rmd’ using rmarkdown
    
    Quitting from lines 69-97 [perf_scale_based_graph] (clarke_wright_performance.Rmd)
    Error: processing vignette 'clarke_wright_performance.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘clarke_wright_performance.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘clarke_wright_performance.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# heuristicsmineR

<details>

* Version: 0.3.0
* GitHub: https://github.com/bupaverse/heuristicsmineR
* Source code: https://github.com/cran/heuristicsmineR
* Date/Publication: 2023-04-04 13:20:06 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "heuristicsmineR")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        libs   3.1Mb
    ```

# HistDAWass

<details>

* Version: 1.0.8
* GitHub: NA
* Source code: https://github.com/cran/HistDAWass
* Date/Publication: 2024-01-24 17:42:31 UTC
* Number of recursive dependencies: 111

Run `revdepcheck::cloud_details(, "HistDAWass")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘HistDAWass-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot-HTS
    > ### Title: Method plot for a histogram time series
    > ### Aliases: plot-HTS plot,HTS-method
    > 
    > ### ** Examples
    > 
    > plot(subsetHTS(RetHTS, from = 1, to = 10)) # plots RetHTS dataset
    ...
      4.     └─HistDAWass:::plot.HTS.1v(x, type = type, border = border, maxno.perplot = maxno.perplot)
      5.       └─HistDAWass:::multiplot(listofP)
      6.         ├─base::print(plots[[1]])
      7.         └─ggplot2:::print.ggplot(plots[[1]])
      8.           ├─ggplot2::ggplot_gtable(data)
      9.           └─ggplot2:::ggplot_gtable.ggplot_built(data)
     10.             └─ggplot2::calc_element("plot.margin", theme)
     11.               └─cli::cli_abort(...)
     12.                 └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.3Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        data   2.0Mb
        libs   5.6Mb
    ```

# huito

<details>

* Version: 0.2.4
* GitHub: https://github.com/flavjack/huito
* Source code: https://github.com/cran/huito
* Date/Publication: 2023-10-25 16:30:02 UTC
* Number of recursive dependencies: 137

Run `revdepcheck::cloud_details(, "huito")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘huito-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: include_shape
    > ### Title: Shape layer
    > ### Aliases: include_shape
    > 
    > ### ** Examples
    > 
    > 
    ...
      5.   └─cowplot::draw_plot(...)
      6.     ├─cowplot::as_grob(plot)
      7.     └─cowplot:::as_grob.ggplot(plot)
      8.       └─ggplot2::ggplotGrob(plot)
      9.         ├─ggplot2::ggplot_gtable(ggplot_build(x))
     10.         └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     11.           └─ggplot2::calc_element("plot.margin", theme)
     12.             └─cli::cli_abort(...)
     13.               └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘germinar.Rmd’ using rmarkdown
    
    Quitting from lines 67-69 [unnamed-chunk-2] (germinar.Rmd)
    Error: processing vignette 'germinar.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘germinar.Rmd’
    
    --- re-building ‘huito.Rmd’ using rmarkdown
    --- finished re-building ‘huito.Rmd’
    ...
    Quitting from lines 68-70 [unnamed-chunk-2] (stickers.Rmd)
    Error: processing vignette 'stickers.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘stickers.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘germinar.Rmd’ ‘labels.Rmd’ ‘stickers.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘germinar.Rmd’
      ...
    > huito_fonts(font)
    
    > label <- label_layout(size = c(5.08, 5.08), border_width = 0, 
    +     background = "#b1d842") %>% include_image(value = "https://germinar.inkaverse.c ..." ... [TRUNCATED] 
    
    > label %>% label_print(mode = "preview")
    
    ...
    > label %>% label_print(mode = "preview")
    
      When sourcing ‘stickers.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘germinar.Rmd’ using ‘UTF-8’... failed
      ‘huito.Rmd’ using ‘UTF-8’... failed
      ‘labels.Rmd’ using ‘UTF-8’... failed
      ‘stickers.Rmd’ using ‘UTF-8’... failed
    ```

# hurricaneexposure

<details>

* Version: 0.1.1
* GitHub: https://github.com/geanders/hurricaneexposure
* Source code: https://github.com/cran/hurricaneexposure
* Date/Publication: 2020-02-13 14:30:02 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::cloud_details(, "hurricaneexposure")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘hurricaneexposure-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: default_map
    > ### Title: Create a default map with eastern US states
    > ### Aliases: default_map
    > 
    > ### ** Examples
    > 
    > default_map()
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘hurricaneexposure.Rmd’
      ...
    
    > map_event_exposure(storm = "Floyd-1999", event_type = "flood")
    
    > map_event_exposure(storm = "Ivan-2004", event_type = "tornado")
    
    > map_tracks(storms = "Floyd-1999")
    
      When sourcing ‘hurricaneexposure.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘hurricaneexposure.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘hurricaneexposure.Rmd’ using rmarkdown
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘mapproj’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# HVT

<details>

* Version: 24.5.2
* GitHub: https://github.com/Mu-Sigma/HVT
* Source code: https://github.com/cran/HVT
* Date/Publication: 2024-05-15 08:50:21 UTC
* Number of recursive dependencies: 200

Run `revdepcheck::cloud_details(, "HVT")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘HVT-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: getTransitionProbability
    > ### Title: Creating Transition Probabilities list
    > ### Aliases: getTransitionProbability
    > ### Keywords: Transition_or_Prediction
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

# hydraulics

<details>

* Version: 0.7.0
* GitHub: https://github.com/EdM44/hydraulics
* Source code: https://github.com/cran/hydraulics
* Date/Publication: 2024-03-06 13:10:08 UTC
* Number of recursive dependencies: 70

Run `revdepcheck::cloud_details(, "hydraulics")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘hydraulics-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: moody
    > ### Title: Creates a Moody diagram with optional manually added points
    > ### Aliases: moody
    > 
    > ### ** Examples
    > 
    > 
    ...
    Backtrace:
        ▆
     1. └─hydraulics::moody()
     2.   └─ggplot2::ggplotGrob(p4)
     3.     ├─ggplot2::ggplot_gtable(ggplot_build(x))
     4.     └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     5.       └─ggplot2::calc_element("plot.margin", theme)
     6.         └─cli::cli_abort(...)
     7.           └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘hydraulics_vignette.Rmd’
      ...
    Mean Roughness, ks = 0.000434 m
    > Re_values <- unlist((as.data.frame(t(ans)))$Re)
    
    > f_values <- unlist((as.data.frame(t(ans)))$f)
    
    > moody(Re = Re_values, f = f_values)
    
      When sourcing ‘hydraulics_vignette.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘hydraulics_vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘hydraulics_vignette.Rmd’ using rmarkdown
    ```

# hyperSpec

<details>

* Version: 0.100.2
* GitHub: https://github.com/r-hyperspec/hyperSpec
* Source code: https://github.com/cran/hyperSpec
* Date/Publication: 2024-05-01 16:02:11 UTC
* Number of recursive dependencies: 141

Run `revdepcheck::cloud_details(, "hyperSpec")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘hyperSpec-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: qplotmixmap
    > ### Title: qplotmap with colour mixing for multivariate overlay
    > ### Aliases: qplotmixmap
    > 
    > ### ** Examples
    > 
    > chondro <- chondro - spc.fit.poly.below (chondro)
    ...
      2.   └─hyperSpec::legendright(p, l)
      3.     ├─base::print(l, viewport(layout.pos.col = 2), newpage = FALSE)
      4.     ├─base::print(l, viewport(layout.pos.col = 2), newpage = FALSE)
      5.     └─ggplot2:::print.ggplot(l, viewport(layout.pos.col = 2), newpage = FALSE)
      6.       ├─ggplot2::ggplot_gtable(data)
      7.       └─ggplot2:::ggplot_gtable.ggplot_built(data)
      8.         └─ggplot2::calc_element("plot.margin", theme)
      9.           └─cli::cli_abort(...)
     10.             └─rlang::abort(...)
    Execution halted
    ```

# hypsoLoop

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/hypsoLoop
* Date/Publication: 2022-02-08 09:00:02 UTC
* Number of recursive dependencies: 97

Run `revdepcheck::cloud_details(, "hypsoLoop")` for more info

</details>

## Newly broken

*   checking whether package ‘hypsoLoop’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::set_theme’ by ‘sjPlot::set_theme’ when loading ‘hypsoLoop’
    See ‘/tmp/workdir/hypsoLoop/new/hypsoLoop.Rcheck/00install.out’ for details.
    ```

# ICvectorfields

<details>

* Version: 0.1.2
* GitHub: https://github.com/goodsman/ICvectorfields
* Source code: https://github.com/cran/ICvectorfields
* Date/Publication: 2022-02-26 22:30:02 UTC
* Number of recursive dependencies: 93

Run `revdepcheck::cloud_details(, "ICvectorfields")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Using_ICvectorfields.Rmd’
      ...
    
    > SimVF
    Warning: The `scale_name` argument of `continuous_scale()` is deprecated as of ggplot2
    3.5.0.
    Warning: The S3 guide system was deprecated in ggplot2 3.5.0.
    ℹ It has been replaced by a ggproto system that can be extended.
    
      When sourcing ‘Using_ICvectorfields.R’:
    Error: argument "theme" is missing, with no default
    Execution halted
    
      ‘Using_ICvectorfields.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Using_ICvectorfields.Rmd’ using rmarkdown
    ```

# idiogramFISH

<details>

* Version: 2.0.13
* GitHub: NA
* Source code: https://github.com/cran/idiogramFISH
* Date/Publication: 2023-08-22 16:50:02 UTC
* Number of recursive dependencies: 170

Run `revdepcheck::cloud_details(, "idiogramFISH")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        R     1.5Mb
        doc   2.0Mb
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘index.Rmd’
      ...
    > if (requireNamespace("RCurl", quietly = TRUE)) {
    +     v <- sub("Version: ", "", readLines("../DESCRIPTION")[3])
    +     pkg <- "idiogramFISH"
    +     l .... [TRUNCATED] 
    Warning in file(con, "r") :
      cannot open file '../DESCRIPTION': No such file or directory
    
      When sourcing ‘index.R’:
    Error: cannot open the connection
    Execution halted
    
      ‘AVignette.Rmd’ using ‘UTF-8’... OK
      ‘index.Rmd’ using ‘UTF-8’... failed
    ```

# idopNetwork

<details>

* Version: 0.1.2
* GitHub: https://github.com/cxzdsa2332/idopNetwork
* Source code: https://github.com/cran/idopNetwork
* Date/Publication: 2023-04-18 06:50:02 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::cloud_details(, "idopNetwork")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘idopNetwork_vignette.Rmd’
      ...
    
    > df = data_cleaning(gut_microbe)
    
    > result1 = test_result$d1_power_fitting
    
    > power_equation_plot(result1)
    
      When sourcing ‘idopNetwork_vignette.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘idopNetwork_vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘idopNetwork_vignette.Rmd’ using rmarkdown
    
    Quitting from lines 86-87 [unnamed-chunk-9] (idopNetwork_vignette.Rmd)
    Error: processing vignette 'idopNetwork_vignette.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘idopNetwork_vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘idopNetwork_vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# iglu

<details>

* Version: 4.0.0
* GitHub: https://github.com/irinagain/iglu
* Source code: https://github.com/cran/iglu
* Date/Publication: 2024-02-23 17:50:02 UTC
* Number of recursive dependencies: 124

Run `revdepcheck::cloud_details(, "iglu")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘iglu-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: agp
    > ### Title: Display Ambulatory Glucose Profile (AGP) statistics for selected
    > ###   subject
    > ### Aliases: agp
    > 
    > ### ** Examples
    > 
    ...
      4.     └─base::lapply(x$plots, plot_table, guides = guides)
      5.       ├─patchwork (local) FUN(X[[i]], ...)
      6.       └─patchwork:::plot_table.ggplot(X[[i]], ...)
      7.         └─ggplot2::ggplotGrob(x)
      8.           ├─ggplot2::ggplot_gtable(ggplot_build(x))
      9.           └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     10.             └─ggplot2::calc_element("plot.margin", theme)
     11.               └─cli::cli_abort(...)
     12.                 └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘AGP_and_Episodes.Rmd’
      ...
    
    > knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
    
    > library(iglu)
    
    > agp(example_data_1_subject)
    
      When sourcing ‘AGP_and_Episodes.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘AGP_and_Episodes.Rmd’ using ‘UTF-8’... failed
      ‘MAGE.Rmd’ using ‘UTF-8’... OK
      ‘iglu.Rmd’ using ‘UTF-8’... OK
      ‘lasagna_plots.Rmd’ using ‘UTF-8’... OK
      ‘metrics_list.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘AGP_and_Episodes.Rmd’ using rmarkdown
    
    Quitting from lines 24-25 [unnamed-chunk-1] (AGP_and_Episodes.Rmd)
    Error: processing vignette 'AGP_and_Episodes.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘AGP_and_Episodes.Rmd’
    
    --- re-building ‘MAGE.Rmd’ using rmarkdown
    ```

# igoR

<details>

* Version: 0.2.0
* GitHub: https://github.com/dieghernan/igoR
* Source code: https://github.com/cran/igoR
* Date/Publication: 2024-02-05 15:30:02 UTC
* Number of recursive dependencies: 67

Run `revdepcheck::cloud_details(, "igoR")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘igoR.Rmd’
      ...
    +     mutate(variable = factor(variable, levels = c("Total IGOs", 
    +         "Numb ..." ... [TRUNCATED] 
    
    > ggplot(all_by_year, aes(x = year, y = value)) + geom_line(color = "black", 
    +     aes(linetype = variable)) + scale_x_continuous(limits = c(1800, 
    + .... [TRUNCATED] 
    
      When sourcing ‘igoR.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘igoR.Rmd’ using ‘UTF-8’... failed
      ‘mapping.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘igoR.Rmd’ using rmarkdown
    
    Quitting from lines 123-150 [Fig1] (igoR.Rmd)
    Error: processing vignette 'igoR.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘igoR.Rmd’
    
    --- re-building ‘mapping.Rmd’ using rmarkdown
    --- finished re-building ‘mapping.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘igoR.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 160 marked UTF-8 strings
    ```

# immunarch

<details>

* Version: 0.9.1
* GitHub: https://github.com/immunomind/immunarch
* Source code: https://github.com/cran/immunarch
* Date/Publication: 2024-03-18 19:10:06 UTC
* Number of recursive dependencies: 194

Run `revdepcheck::cloud_details(, "immunarch")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘immunarch-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pubRepStatistics
    > ### Title: Statistics of number of public clonotypes for each possible
    > ###   combinations of repertoires
    > ### Aliases: pubRepStatistics
    > 
    > ### ** Examples
    > 
    ...
      5.     ├─base::suppressMessages(...)
      6.     │ └─base::withCallingHandlers(...)
      7.     └─UpSetR:::Make_main_bar(...)
      8.       └─ggplot2::ggplotGrob(Main_bar_plot)
      9.         ├─ggplot2::ggplot_gtable(ggplot_build(x))
     10.         └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     11.           └─ggplot2::calc_element("plot.margin", theme)
     12.             └─cli::cli_abort(...)
     13.               └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.5Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        data   5.5Mb
        doc    1.6Mb
    ```

# immuneSIM

<details>

* Version: 0.8.7
* GitHub: https://github.com/GreiffLab/immuneSIM
* Source code: https://github.com/cran/immuneSIM
* Date/Publication: 2019-09-27 10:30:06 UTC
* Number of recursive dependencies: 66

Run `revdepcheck::cloud_details(, "immuneSIM")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘immuneSIM-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_repertoire_A_vs_B
    > ### Title: Comparative plots of main repertoire features of two input
    > ###   repertoires (length distribution, amino acid frequency, VDJ usage,
    > ###   kmer occurrence)
    > ### Aliases: plot_repertoire_A_vs_B
    > 
    > ### ** Examples
    ...
        ▆
     1. └─immuneSIM::plot_repertoire_A_vs_B(...)
     2.   ├─base::print(plots_aa_freq_list_imgt[[1]], vp = vplayout(1, 1))
     3.   └─ggplot2:::print.ggplot(...)
     4.     ├─ggplot2::ggplot_gtable(data)
     5.     └─ggplot2:::ggplot_gtable.ggplot_built(data)
     6.       └─ggplot2::calc_element("plot.margin", theme)
     7.         └─cli::cli_abort(...)
     8.           └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.2Mb
      sub-directories of 1Mb or more:
        R   8.1Mb
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# iNEXT.4steps

<details>

* Version: 1.0.0
* GitHub: https://github.com/KaiHsiangHu/iNEXT.4steps
* Source code: https://github.com/cran/iNEXT.4steps
* Date/Publication: 2024-04-10 20:00:05 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "iNEXT.4steps")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘iNEXT.4steps-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggCompleteness
    > ### Title: ggplot for depicting sample completeness profiles
    > ### Aliases: ggCompleteness
    > 
    > ### ** Examples
    > 
    > 
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(iNEXT.4steps)
      > 
      > test_check("iNEXT.4steps")
      [ FAIL 2 | WARN 5 | SKIP 0 | PASS 10 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
       3.     └─ggpubr:::.get_legend(p, position = position)
       4.       ├─ggplot2::ggplot_gtable(ggplot_build(p))
       5.       └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(p))
       6.         └─ggplot2::calc_element("plot.margin", theme)
       7.           └─cli::cli_abort(...)
       8.             └─rlang::abort(...)
      
      [ FAIL 2 | WARN 5 | SKIP 0 | PASS 10 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Vignette-iNEXT.4steps-April10.Rmd’
      ...
    
    
    > data(Data_spider)
    
    > Four_Steps_out1 <- iNEXT4steps(data = Data_spider, 
    +     datatype = "abundance")
    
      When sourcing ‘Vignette-iNEXT.4steps-April10.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘Vignette-iNEXT.4steps-April10.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Vignette-iNEXT.4steps-April10.Rmd’ using rmarkdown
    
    Quitting from lines 209-213 [unnamed-chunk-8] (Vignette-iNEXT.4steps-April10.Rmd)
    Error: processing vignette 'Vignette-iNEXT.4steps-April10.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘Vignette-iNEXT.4steps-April10.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Vignette-iNEXT.4steps-April10.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# iNEXT.beta3D

<details>

* Version: 1.0.2
* GitHub: https://github.com/AnneChao/iNEXT.beta3D
* Source code: https://github.com/cran/iNEXT.beta3D
* Date/Publication: 2024-04-17 19:40:11 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "iNEXT.beta3D")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘iNEXT.beta3D-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggiNEXTbeta3D
    > ### Title: ggplot2 extension for the iNEXTbeta3D object
    > ### Aliases: ggiNEXTbeta3D
    > 
    > ### ** Examples
    > 
    > 
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
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

# insurancerating

<details>

* Version: 0.7.4
* GitHub: https://github.com/mharinga/insurancerating
* Source code: https://github.com/cran/insurancerating
* Date/Publication: 2024-05-20 11:30:03 UTC
* Number of recursive dependencies: 133

Run `revdepcheck::cloud_details(, "insurancerating")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘insurancerating-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: autoplot.univariate
    > ### Title: Automatically create a ggplot for objects obtained from
    > ###   univariate()
    > ### Aliases: autoplot.univariate
    > 
    > ### ** Examples
    > 
    ...
    > xzip <- univariate(MTPL, x = bm, severity = amount, nclaims = nclaims,
    + exposure = exposure, by = zip)
    > autoplot(xzip, show_plots = 1:2)
    Warning: Removed 16 rows containing missing values or values outside the scale range
    (`geom_point()`).
    Warning: Removed 5 rows containing missing values or values outside the scale range
    (`geom_line()`).
    Error in identicalUnits(x) : object is not a unit
    Calls: <Anonymous> ... assemble_guides -> guides_build -> unit.c -> identicalUnits
    Execution halted
    ```

# inTextSummaryTable

<details>

* Version: 3.3.2
* GitHub: https://github.com/openanalytics/inTextSummaryTable
* Source code: https://github.com/cran/inTextSummaryTable
* Date/Publication: 2024-03-09 16:20:02 UTC
* Number of recursive dependencies: 120

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
      [ FAIL 59 | WARN 1 | SKIP 0 | PASS 881 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
        5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        6. └─inTextSummaryTable::subjectProfileSummaryPlot(...)
        7.   ├─base::do.call(plyr::rbind.fill, ggplot_build(gg)$data)
        8.   └─plyr (local) `<fn>`(`<df[,12]>`, `<df[,13]>`)
        9.     └─plyr:::output_template(dfs, nrows)
       10.       └─plyr:::allocate_column(df[[var]], nrows, dfs, var)
      
      [ FAIL 59 | WARN 1 | SKIP 0 | PASS 881 ]
      Error: Test failures
      Execution halted
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

*   checking installed package size ... NOTE
    ```
      installed size is 10.5Mb
      sub-directories of 1Mb or more:
        doc   9.9Mb
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
# jskm

<details>

* Version: 0.5.3
* GitHub: https://github.com/jinseob2kim/jstable
* Source code: https://github.com/cran/jskm
* Date/Publication: 2024-01-26 06:20:08 UTC
* Number of recursive dependencies: 103

Run `revdepcheck::cloud_details(, "jskm")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘jskm-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: jskm
    > ### Title: Creates a Kaplan-Meier plot for survfit object.
    > ### Aliases: jskm
    > 
    > ### ** Examples
    > 
    > library(survival)
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(jskm)
      > 
      > test_check("jskm")
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 2 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
          label_size = .lab$size, label_fontfamily = .lab$family, label_fontface = .lab$face, 
          label_colour = .lab$color, label_x = .lab$label.x, label_y = .lab$label.y, 
          hjust = .lab$hjust, vjust = .lab$vjust, align = align, rel_widths = widths, 
          rel_heights = heights, legend = legend, common.legend.grob = legend.grob)`: ℹ In index: 1.
      Caused by error in `ggplot_gtable()`:
      ! Theme element `plot.margin` must have class <margin/rel>.
      
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 2 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘jskm.Rmd’
      ...
    > data(colon)
    Warning in data(colon) : data set ‘colon’ not found
    
    > fit <- survfit(Surv(time, status) ~ rx, data = colon)
    
    > jskm(fit)
    
      When sourcing ‘jskm.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘jskm.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘jskm.Rmd’ using rmarkdown
    
    Quitting from lines 35-47 [unnamed-chunk-1] (jskm.Rmd)
    Error: processing vignette 'jskm.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘jskm.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘jskm.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# KaradaColor

<details>

* Version: 0.1.5
* GitHub: https://github.com/KaradaGood/KaradaColor
* Source code: https://github.com/cran/KaradaColor
* Date/Publication: 2023-04-21 08:02:37 UTC
* Number of recursive dependencies: 40

Run `revdepcheck::cloud_details(, "KaradaColor")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘KaradaColor-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: kg_get_color
    > ### Title: Get color palette data
    > ### Aliases: kg_get_color kg_get_palette
    > 
    > ### ** Examples
    > 
    > library("scales")
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

# karel

<details>

* Version: 0.1.1
* GitHub: https://github.com/mpru/karel
* Source code: https://github.com/cran/karel
* Date/Publication: 2022-03-26 21:50:02 UTC
* Number of recursive dependencies: 90

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

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘fitting.Rmd’
      ...
    > outcome <- Multinom(p = c("p.1", "p.2"), data = chickenPox[, 
    +     c(2, 3, 5)])
    
    > fitted.model <- fit_model(structure * 2, chickenPox = outcome)
    
    > forecast(fitted.model, t = 24, plot = "base")
    
      When sourcing ‘fitting.R’:
    Error: Error: Missing extra argument: Vaccine.1.Covariate
    Execution halted
    
      ‘example1.Rmd’ using ‘UTF-8’... OK
      ‘fitting.Rmd’ using ‘UTF-8’... failed
      ‘intro.Rmd’ using ‘UTF-8’... OK
      ‘outcomes.Rmd’ using ‘UTF-8’... OK
      ‘structures.Rmd’ using ‘UTF-8’... OK
    ```

# labsimplex

<details>

* Version: 0.1.2
* GitHub: NA
* Source code: https://github.com/cran/labsimplex
* Date/Publication: 2020-06-03 16:10:06 UTC
* Number of recursive dependencies: 68

Run `revdepcheck::cloud_details(, "labsimplex")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘labsimplex-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: addSimplex2Surface
    > ### Title: Adds the simplex movements to a response surface contour
    > ### Aliases: addSimplex2Surface
    > 
    > ### ** Examples
    > 
    > simplex <- exampleOptimization(surface = exampleSurfaceR2,
    ...
    Backtrace:
        ▆
     1. ├─base::print(p)
     2. └─ggplot2:::print.ggplot(p)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘labsimplex.Rmd’
      ...
    +     0.6, 0, 0)), phi = 30, theta = 30, ltheta = -120, expand = 0.6, 
    +     xlab = "Te ..." ... [TRUNCATED] 
    
    > (cont.surf <- cntr(surface = exampleSurfaceR2, length = 200))
    Warning: Removed 796 rows containing missing values or values outside the scale range
    (`geom_tile()`).
    
      When sourcing ‘labsimplex.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘labsimplex.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘labsimplex.Rmd’ using rmarkdown
    
    Quitting from lines 66-69 [surfaces1] (labsimplex.Rmd)
    Error: processing vignette 'labsimplex.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘labsimplex.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘labsimplex.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# landscapemetrics

<details>

* Version: 2.1.2
* GitHub: https://github.com/r-spatialecology/landscapemetrics
* Source code: https://github.com/cran/landscapemetrics
* Date/Publication: 2024-05-02 12:52:46 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "landscapemetrics")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘landscapemetrics-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: show_cores
    > ### Title: Show core area
    > ### Aliases: show_cores
    > 
    > ### ** Examples
    > 
    > landscape <- terra::rast(landscapemetrics::landscape)
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.9Mb
      sub-directories of 1Mb or more:
        libs   6.3Mb
    ```

# landscapetools

<details>

* Version: 0.5.0
* GitHub: https://github.com/ropensci/landscapetools
* Source code: https://github.com/cran/landscapetools
* Date/Publication: 2019-02-25 22:40:03 UTC
* Number of recursive dependencies: 75

Run `revdepcheck::cloud_details(, "landscapetools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘landscapetools-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: util_merge
    > ### Title: util_merge
    > ### Aliases: util_merge util_merge.RasterLayer
    > 
    > ### ** Examples
    > 
    > x <- util_merge(gradient_landscape, random_landscape)
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘overview.Rmd’
      ...
    
    > library(landscapetools)
    
    > show_landscape(gradient_landscape)
    Loading required package: raster
    Loading required package: sp
    
      When sourcing ‘overview.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘overview.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘overview.Rmd’ using rmarkdown
    
    Quitting from lines 31-46 [unnamed-chunk-1] (overview.Rmd)
    Error: processing vignette 'overview.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘overview.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘overview.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# latentcor

<details>

* Version: 2.0.1
* GitHub: NA
* Source code: https://github.com/cran/latentcor
* Date/Publication: 2022-09-05 20:50:02 UTC
* Number of recursive dependencies: 143

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
      0.020   0.000   0.021 
    > # Heatmap for latent correlation matrix.
    > Heatmap_R_approx = latentcor(X = X, types = "tru", method = "approx",
    +                              showplot = TRUE)$plotR
    Error in pm[[2]] : subscript out of bounds
    Calls: latentcor ... %>% -> layout -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# latte

<details>

* Version: 0.2.1
* GitHub: https://github.com/dkahle/latte
* Source code: https://github.com/cran/latte
* Date/Publication: 2019-03-25 10:50:03 UTC
* Number of recursive dependencies: 86

Run `revdepcheck::cloud_details(, "latte")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘latte-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot-matrix
    > ### Title: Plot a matrix
    > ### Aliases: plot-matrix plot_matrix
    > 
    > ### ** Examples
    > 
    > 
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# lemon

<details>

* Version: 0.4.9
* GitHub: https://github.com/stefanedwards/lemon
* Source code: https://github.com/cran/lemon
* Date/Publication: 2024-02-08 08:00:08 UTC
* Number of recursive dependencies: 76

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
       17.                           ├─grid::unit.c(unit(1, "npc"), unit(1, "npc") - tick.length)
       18.                           └─grid:::Ops.unit(unit(1, "npc"), tick.length)
       19.                             └─grid:::as.unit(e2)
      
      [ FAIL 1 | WARN 0 | SKIP 3 | PASS 138 ]
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
    Error: object is not coercible to a unit
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

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/lfproQC
* Date/Publication: 2024-05-23 16:10:02 UTC
* Number of recursive dependencies: 138

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
    Warning: Removed 266 rows containing non-finite outside the scale range
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
    1              knn_rlr            lls_loess               lls_rlr
    
    > Boxplot_data(yeast$knn_rlr_data)
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
    
    Quitting from lines 53-54 [unnamed-chunk-8] (user_guide.Rmd)
    Error: processing vignette 'user_guide.Rmd' failed with diagnostics:
    subscript out of bounds
    --- failed re-building ‘user_guide.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘user_guide.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.2Mb
      sub-directories of 1Mb or more:
        doc   5.9Mb
    ```

# LLSR

<details>

* Version: 0.0.3.1
* GitHub: https://github.com/diegofcoelho/LLSR
* Source code: https://github.com/cran/LLSR
* Date/Publication: 2021-02-17 18:20:02 UTC
* Number of recursive dependencies: 62

Run `revdepcheck::cloud_details(, "LLSR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘LLSR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: AQSys.plot
    > ### Title: Dataset and Fitted Function plot
    > ### Aliases: AQSys.plot
    > 
    > ### ** Examples
    > 
    > #Populating variable dataSET with binodal data
    ...
        ▆
     1. └─LLSR::AQSys.plot(dataSET)
     2.   ├─base::print(plot_image)
     3.   └─ggplot2:::print.ggplot(plot_image)
     4.     ├─ggplot2::ggplot_gtable(data)
     5.     └─ggplot2:::ggplot_gtable.ggplot_built(data)
     6.       └─ggplot2::calc_element("plot.margin", theme)
     7.         └─cli::cli_abort(...)
     8.           └─rlang::abort(...)
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
        NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5)))
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

# lomb

<details>

* Version: 2.5.0
* GitHub: NA
* Source code: https://github.com/cran/lomb
* Date/Publication: 2024-03-26 15:10:05 UTC
* Number of recursive dependencies: 86

Run `revdepcheck::cloud_details(, "lomb")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘lomb-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: getpeaks
    > ### Title: Retrieve periodogram peaks
    > ### Aliases: getpeaks
    > ### Keywords: ts
    > 
    > ### ** Examples
    > 
    ...
      2.   ├─base::plot(sp.out, ...)
      3.   └─lomb::plot.lsp(sp.out, ...)
      4.     ├─base::print(p)
      5.     └─ggplot2:::print.ggplot(p)
      6.       ├─ggplot2::ggplot_gtable(data)
      7.       └─ggplot2:::ggplot_gtable.ggplot_built(data)
      8.         └─ggplot2::calc_element("plot.margin", theme)
      9.           └─cli::cli_abort(...)
     10.             └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.0Mb
      sub-directories of 1Mb or more:
        data   6.5Mb
    ```

# LongDat

<details>

* Version: 1.1.2
* GitHub: https://github.com/CCY-dev/LongDat
* Source code: https://github.com/cran/LongDat
* Date/Publication: 2023-07-17 05:40:02 UTC
* Number of recursive dependencies: 144

Run `revdepcheck::cloud_details(, "LongDat")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘LongDat_cont_tutorial.Rmd’
      ...
    </table></div>
    > test_plot <- cuneiform_plot(result_table = test_cont[[1]], 
    +     title_size = 15)
    [1] "Finished plotting successfully!"
    
    > test_plot
    
    ...
    [1] "Finished plotting successfully!"
    
    > test_plot
    
      When sourcing ‘LongDat_disc_tutorial.R’:
    Error: object is not coercible to a unit
    Execution halted
    
      ‘LongDat_cont_tutorial.Rmd’ using ‘UTF-8’... failed
      ‘LongDat_disc_tutorial.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘LongDat_cont_tutorial.Rmd’ using rmarkdown
    Warning in eng_r(options) :
      Failed to tidy R code in chunk 'unnamed-chunk-3'. Reason:
    Error : The formatR package is required by the chunk option tidy = TRUE but not installed; tidy = TRUE will be ignored.
    
    Warning in eng_r(options) :
      Failed to tidy R code in chunk 'unnamed-chunk-4'. Reason:
    Error : The formatR package is required by the chunk option tidy = TRUE but not installed; tidy = TRUE will be ignored.
    
    ...
    Quitting from lines 181-182 [unnamed-chunk-11] (LongDat_disc_tutorial.Rmd)
    Error: processing vignette 'LongDat_disc_tutorial.Rmd' failed with diagnostics:
    object is not coercible to a unit
    --- failed re-building ‘LongDat_disc_tutorial.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘LongDat_cont_tutorial.Rmd’ ‘LongDat_disc_tutorial.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# longitudinalcascade

<details>

* Version: 0.3.2.6
* GitHub: NA
* Source code: https://github.com/cran/longitudinalcascade
* Date/Publication: 2023-05-02 20:50:02 UTC
* Number of recursive dependencies: 40

Run `revdepcheck::cloud_details(, "longitudinalcascade")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘longitudinalcascade-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: longitudinalcascade
    > ### Title: Longitudinal cascade statistics and charts
    > ### Aliases: longitudinalcascade
    > ### Keywords: cascade longitudinal survival
    > 
    > ### ** Examples
    > 
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

# longmixr

<details>

* Version: 1.0.0
* GitHub: https://github.com/cellmapslab/longmixr
* Source code: https://github.com/cran/longmixr
* Date/Publication: 2022-01-13 20:32:42 UTC
* Number of recursive dependencies: 135

Run `revdepcheck::cloud_details(, "longmixr")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘analysis_workflow.Rmd’
      ...
    
    > fviz_screeplot(quest_A_dim, main = "Questionnaire A")
    
      When sourcing ‘analysis_workflow.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `check_aesthetics()`:
    ! Aesthetics must be either length 1 or the same as the data (5).
    ✖ Fix the following mappings: `width`.
    Execution halted
    
      ‘analysis_workflow.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘analysis_workflow.Rmd’ using rmarkdown
    ```

# manhplot

<details>

* Version: 1.1
* GitHub: https://github.com/cgrace1978/manhplot
* Source code: https://github.com/cran/manhplot
* Date/Publication: 2019-11-25 16:40:03 UTC
* Number of recursive dependencies: 56

Run `revdepcheck::cloud_details(, "manhplot")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(manhplot)
      > 
      > test_check("manhplot")
      [ FAIL 2 | WARN 3 | SKIP 0 | PASS 0 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
       1. └─manhplot::manhplusplot(...) at testmanhplusplot.R:17:3
       2.   ├─ggplot2::ggplot_gtable(ggplot_build(final.table.plot))
       3.   └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(final.table.plot))
       4.     └─ggplot2::calc_element("plot.margin", theme)
       5.       └─cli::cli_abort(...)
       6.         └─rlang::abort(...)
      
      [ FAIL 2 | WARN 3 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

# mau

<details>

* Version: 0.1.2
* GitHub: https://github.com/pedroguarderas/mau
* Source code: https://github.com/cran/mau
* Date/Publication: 2018-01-17 05:35:14 UTC
* Number of recursive dependencies: 57

Run `revdepcheck::cloud_details(, "mau")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mau-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Spider.Plot
    > ### Title: Spider plot
    > ### Aliases: Spider.Plot
    > 
    > ### ** Examples
    > 
    > # Preparing data
    ...
    Backtrace:
        ▆
     1. ├─base::plot(p)
     2. └─ggplot2:::plot.ggplot(p)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

# MBNMAdose

<details>

* Version: 0.4.3
* GitHub: NA
* Source code: https://github.com/cran/MBNMAdose
* Date/Publication: 2024-04-18 12:42:47 UTC
* Number of recursive dependencies: 118

Run `revdepcheck::cloud_details(, "MBNMAdose")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘outputs-4.Rmd’
      ...
    
    > plot(trip.emax)
    
      When sourcing ‘outputs-4.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `use_defaults()`:
    ...
    Execution halted
    
      ‘consistencychecking-3.Rmd’ using ‘UTF-8’... OK
      ‘dataexploration-1.Rmd’ using ‘UTF-8’... OK
      ‘mbnmadose-overview.Rmd’ using ‘UTF-8’... OK
      ‘metaregression-6.Rmd’ using ‘UTF-8’... OK
      ‘nma_in_mbnmadose.Rmd’ using ‘UTF-8’... OK
      ‘outputs-4.Rmd’ using ‘UTF-8’... failed
      ‘predictions-5.Rmd’ using ‘UTF-8’... OK
      ‘runmbnmadose-2.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘consistencychecking-3.Rmd’ using rmarkdown
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6 marked Latin-1 strings
    ```

# MBNMAtime

<details>

* Version: 0.2.4
* GitHub: NA
* Source code: https://github.com/cran/MBNMAtime
* Date/Publication: 2023-10-14 15:20:02 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "MBNMAtime")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘consistencychecking-3.Rmd’ using rmarkdown
    
    Quitting from lines 141-146 [unnamed-chunk-8] (consistencychecking-3.Rmd)
    Error: processing vignette 'consistencychecking-3.Rmd' failed with diagnostics:
    unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, 
        NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, 
        NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 2.2), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0.5, NULL, NULL, NULL, c(0, 2.2, 0, 2.2), NULL, TRUE), list("grey20", NULL, NULL, NULL, FALSE, "grey20", TRUE), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.5, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list("transparent", NA, NULL, NULL, FALSE), NULL, 
        2, NULL, NULL, list("transparent", NA, NULL, NULL, FALSE), 1.2, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.2, NULL, list(NULL, NULL, NULL, 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0, list(), 2, list(), list(NULL, "grey20", NULL, NULL, TRUE), NULL, NULL, NULL, list("grey92", NULL, NULL, NULL, FALSE, "grey92", TRUE), list("grey95", 
            NULL, NULL, NULL, FALSE, "grey95", FALSE), list("grey95", 0.5, NULL, NULL, FALSE, "grey95", FALSE), NULL, NULL, NULL, NULL, FALSE, list("white", NA, NULL, NULL, FALSE), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(5.5, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, TRUE), "topleft", 
        NULL, NULL, list("lightsteelblue1", "black", NULL, NULL, FALSE), NULL, NULL, "inherit", "inside", list(NULL, NULL, "black", 0.8, NULL, NULL, NULL, NULL, c(4.4, 4.4, 4.4, 4.4), NULL, FALSE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75))
    --- failed re-building ‘consistencychecking-3.Rmd’
    
    --- re-building ‘dataexploration-1.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘consistencychecking-3.Rmd’
      ...
    |-> direct         |        |  0.228| -0.213|  0.684|
    |-> indirect       |        | -0.515| -0.891| -0.137|
    |                  |        |       |       |       |
    
    > plot(nodesplit, plot.type = "forest")
    
      When sourcing ‘consistencychecking-3.R’:
    ...
    ! unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, 
        NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0,
    Execution halted
    
      ‘consistencychecking-3.Rmd’ using ‘UTF-8’... failed
      ‘dataexploration-1.Rmd’ using ‘UTF-8’... failed
      ‘mbnmatime-overview.Rmd’ using ‘UTF-8’... OK
      ‘outputs-4.Rmd’ using ‘UTF-8’... failed
      ‘predictions-5.Rmd’ using ‘UTF-8’... OK
      ‘runmbnmatime-2.Rmd’ using ‘UTF-8’... OK
    ```

# metaforest

<details>

* Version: 0.1.4
* GitHub: NA
* Source code: https://github.com/cran/metaforest
* Date/Publication: 2024-01-26 09:40:05 UTC
* Number of recursive dependencies: 124

Run `revdepcheck::cloud_details(, "metaforest")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > rm(list=ls())
      > library(testthat)
      > library(caret)
      Loading required package: ggplot2
      Loading required package: lattice
      > library(metaforest)
      Loading required package: metafor
    ...
        6.     └─ggplot2::ggplotGrob(plots[[x]] + theme(axis.title.y = element_blank()))
        7.       ├─ggplot2::ggplot_gtable(ggplot_build(x))
        8.       └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
        9.         └─ggplot2::calc_element("plot.margin", theme)
       10.           └─cli::cli_abort(...)
       11.             └─rlang::abort(...)
      
      [ FAIL 1 | WARN 3 | SKIP 0 | PASS 18 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Introduction_to_metaforest.Rmd’
      ...
    
    > set.seed(62)
    
    > check_conv <- readRDS("C:/Git_Repositories/S4_meta-analysis/check_conv.RData")
    Warning in gzfile(file, "rb") :
      cannot open compressed file 'C:/Git_Repositories/S4_meta-analysis/check_conv.RData', probable reason 'No such file or directory'
    
      When sourcing ‘Introduction_to_metaforest.R’:
    Error: cannot open the connection
    Execution halted
    
      ‘Introduction_to_metaforest.Rmd’ using ‘UTF-8’... failed
    ```

# metan

<details>

* Version: 1.18.0
* GitHub: https://github.com/TiagoOlivoto/metan
* Source code: https://github.com/cran/metan
* Date/Publication: 2023-03-05 22:00:15 UTC
* Number of recursive dependencies: 116

Run `revdepcheck::cloud_details(, "metan")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘metan-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: network_plot
    > ### Title: Network plot of a correlation matrix
    > ### Aliases: network_plot
    > 
    > ### ** Examples
    > 
    > cor <- corr_coef(iris)
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

# metaplot

<details>

* Version: 0.8.4
* GitHub: NA
* Source code: https://github.com/cran/metaplot
* Date/Publication: 2024-02-18 05:30:10 UTC
* Number of recursive dependencies: 40

Run `revdepcheck::cloud_details(, "metaplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘metaplot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: boxplot.data.frame
    > ### Title: Boxplot Method for Data Frame
    > ### Aliases: boxplot.data.frame
    > 
    > ### ** Examples
    > 
    > library(dplyr)
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

# metR

<details>

* Version: 0.15.0
* GitHub: https://github.com/eliocamp/metR
* Source code: https://github.com/cran/metR
* Date/Publication: 2024-02-09 00:40:02 UTC
* Number of recursive dependencies: 120

Run `revdepcheck::cloud_details(, "metR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘metR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: GeostrophicWind
    > ### Title: Calculate geostrophic winds
    > ### Aliases: GeostrophicWind
    > 
    > ### ** Examples
    > 
    > data(geopotential)
    ...
    > ggplot(geopotential[date == date[1]], aes(lon, lat)) +
    +     geom_contour(aes(z = gh)) +
    +     geom_vector(aes(dx = u, dy = v), skip = 2) +
    +     scale_mag()
    Warning: The S3 guide system was deprecated in ggplot2 3.5.0.
    ℹ It has been replaced by a ggproto system that can be extended.
    Error in (function (layer, df)  : 
      argument "theme" is missing, with no default
    Calls: <Anonymous> ... use_defaults -> eval_from_theme -> %||% -> calc_element
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Visualization-tools.Rmd’
      ...
    
    > (g <- ggplot(temperature[lev == 500], aes(lon, lat)) + 
    +     geom_contour_fill(aes(z = air.z)) + geom_vector(aes(dx = t.dx, 
    +     dy = t.dy), skip .... [TRUNCATED] 
    Warning: The S3 guide system was deprecated in ggplot2 3.5.0.
    ℹ It has been replaced by a ggproto system that can be extended.
    
    ...
    +     dy = gh.dlat), s .... [TRUNCATED] 
    Warning: The S3 guide system was deprecated in ggplot2 3.5.0.
    ℹ It has been replaced by a ggproto system that can be extended.
    
      When sourcing ‘Working-with-data.R’:
    Error: argument "theme" is missing, with no default
    Execution halted
    
      ‘Visualization-tools.Rmd’ using ‘UTF-8’... failed
      ‘Working-with-data.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Visualization-tools.Rmd’ using knitr
    
    Quitting from lines 284-293 [unnamed-chunk-19] (Visualization-tools.Rmd)
    Error: processing vignette 'Visualization-tools.Rmd' failed with diagnostics:
    argument "theme" is missing, with no default
    --- failed re-building ‘Visualization-tools.Rmd’
    
    --- re-building ‘Working-with-data.Rmd’ using knitr
    ...
    Quitting from lines 199-210 [unnamed-chunk-13] (Working-with-data.Rmd)
    Error: processing vignette 'Working-with-data.Rmd' failed with diagnostics:
    argument "theme" is missing, with no default
    --- failed re-building ‘Working-with-data.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘Visualization-tools.Rmd’ ‘Working-with-data.Rmd’
    
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

# miceFast

<details>

* Version: 0.8.2
* GitHub: https://github.com/Polkas/miceFast
* Source code: https://github.com/cran/miceFast
* Date/Publication: 2022-11-17 21:10:02 UTC
* Number of recursive dependencies: 112

Run `revdepcheck::cloud_details(, "miceFast")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘miceFast-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: upset_NA
    > ### Title: upset plot for NA values
    > ### Aliases: upset_NA
    > 
    > ### ** Examples
    > 
    > library(miceFast)
    ...
      4.     ├─base::suppressMessages(...)
      5.     │ └─base::withCallingHandlers(...)
      6.     └─UpSetR:::Make_main_bar(...)
      7.       └─ggplot2::ggplotGrob(Main_bar_plot)
      8.         ├─ggplot2::ggplot_gtable(ggplot_build(x))
      9.         └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     10.           └─ggplot2::calc_element("plot.margin", theme)
     11.             └─cli::cli_abort(...)
     12.               └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(miceFast)
      > library(data.table)
      > library(dplyr)
      
      Attaching package: 'dplyr'
      
    ...
       15.       └─ggplot2::ggplotGrob(Main_bar_plot)
       16.         ├─ggplot2::ggplot_gtable(ggplot_build(x))
       17.         └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
       18.           └─ggplot2::calc_element("plot.margin", theme)
       19.             └─cli::cli_abort(...)
       20.               └─rlang::abort(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 103 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘miceFast-intro.Rmd’
      ...
    
    > set.seed(123456)
    
    > data(air_miss)
    
    > upset_NA(air_miss, 6)
    
      When sourcing ‘miceFast-intro.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘miceFast-intro.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘miceFast-intro.Rmd’ using rmarkdown
    
    Quitting from lines 84-85 [unnamed-chunk-6] (miceFast-intro.Rmd)
    Error: processing vignette 'miceFast-intro.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘miceFast-intro.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘miceFast-intro.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking C++ specification ... NOTE
    ```
      Specified C++11: please drop specification unless essential
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 12.1Mb
      sub-directories of 1Mb or more:
        libs  10.9Mb
    ```

# MicrobiomeStat

<details>

* Version: 1.2
* GitHub: NA
* Source code: https://github.com/cran/MicrobiomeStat
* Date/Publication: 2024-04-01 22:30:02 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::cloud_details(, "MicrobiomeStat")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘MicrobiomeStat-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: linda
    > ### Title: Linear (Lin) Model for Differential Abundance (DA) Analysis of
    > ###   High-dimensional Compositional Data
    > ### Aliases: linda
    > 
    > ### ** Examples
    > 
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

# micromap

<details>

* Version: 1.9.8
* GitHub: https://github.com/USEPA/micromap
* Source code: https://github.com/cran/micromap
* Date/Publication: 2024-02-06 14:00:02 UTC
* Number of recursive dependencies: 45

Run `revdepcheck::cloud_details(, "micromap")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘micromap-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: lmgroupedplot
    > ### Title: Linked Micromaps
    > ### Aliases: lmgroupedplot lmplot mmgroupedplot mmplot
    > ###   mmplot.SpatialPolygonsDataFrame mmplot.sf mmplot.default
    > ### Keywords: hplot
    > 
    > ### ** Examples
    ...
      6.       ├─base::suppressWarnings(...)
      7.       │ └─base::withCallingHandlers(...)
      8.       ├─base::print(plobject[[p]], vp = subplot(1, p * 2))
      9.       └─ggplot2:::print.ggplot(plobject[[p]], vp = subplot(1, p * 2))
     10.         ├─ggplot2::ggplot_gtable(data)
     11.         └─ggplot2:::ggplot_gtable.ggplot_built(data)
     12.           └─ggplot2::calc_element("plot.margin", theme)
     13.             └─cli::cli_abort(...)
     14.               └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Introduction_Guide.Rnw’
      ...
    6 AK      1    1       4      17    0         1    0
    
    > mmplot(stat.data = edPov, map.data = statePolys, panel.types = c("labels", 
    +     "dot", "dot", "map"), panel.data = list("state", "pov", "ed", 
    +   .... [TRUNCATED] 
    
      When sourcing ‘Introduction_Guide.R’:
    Error: Theme element `plot.margin` must have class
    <margin/rel>.
    Execution halted
    
      ‘Introduction_Guide.Rnw’ using ‘UTF-8’... failed
    ```

## In both

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Introduction_Guide.Rnw’ using Sweave
    Loading required package: RColorBrewer
    Loading required package: sp
    Loading required package: sf
    Linking to GEOS 3.10.2, GDAL 3.4.1, PROJ 8.2.1; sf_use_s2()
    is TRUE
    
    Error: processing vignette 'Introduction_Guide.Rnw' failed with diagnostics:
    ...
      Theme element `plot.margin` must have class
    <margin/rel>.
    
    --- failed re-building ‘Introduction_Guide.Rnw’
    
    SUMMARY: processing the following file failed:
      ‘Introduction_Guide.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# MiMIR

<details>

* Version: 1.5
* GitHub: NA
* Source code: https://github.com/cran/MiMIR
* Date/Publication: 2024-02-01 08:50:02 UTC
* Number of recursive dependencies: 188

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

# MIMSunit

<details>

* Version: 0.11.2
* GitHub: https://github.com/mhealthgroup/MIMSunit
* Source code: https://github.com/cran/MIMSunit
* Date/Publication: 2022-06-21 11:00:09 UTC
* Number of recursive dependencies: 114

Run `revdepcheck::cloud_details(, "MIMSunit")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘MIMSunit-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: bandlimited_interp
    > ### Title: Apply a bandlimited interpolation filter to the signal to change
    > ###   the sampling rate
    > ### Aliases: bandlimited_interp
    > 
    > ### ** Examples
    > 
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

# miRetrieve

<details>

* Version: 1.3.4
* GitHub: NA
* Source code: https://github.com/cran/miRetrieve
* Date/Publication: 2021-09-18 17:30:02 UTC
* Number of recursive dependencies: 126

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

* Version: 2.5.1
* GitHub: https://github.com/sizespectrum/mizer
* Source code: https://github.com/cran/mizer
* Date/Publication: 2024-03-08 23:10:02 UTC
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
      > library(testthat)
      > library(mizer)
      > 
      > test_check("mizer")
      [ FAIL 10 | WARN 0 | SKIP 5 | PASS 1251 ]
      
    ...
      • plots/plot-spectra.svg
      • plots/plot-yield-by-gear.svg
      • plots/plot-yield.svg
      • plots/plotfishing-mortality.svg
      • plots/plotfmort-truncated.svg
      • plots/plotpredation-mortality.svg
      • plots/plotpredmort-truncated.new.svg
      • plots/plotpredmort-truncated.svg
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        doc    1.5Mb
        help   1.8Mb
    ```

# mlr3spatiotempcv

<details>

* Version: 2.3.1
* GitHub: https://github.com/mlr-org/mlr3spatiotempcv
* Source code: https://github.com/cran/mlr3spatiotempcv
* Date/Publication: 2024-04-17 12:10:05 UTC
* Number of recursive dependencies: 168

Run `revdepcheck::cloud_details(, "mlr3spatiotempcv")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mlr3spatiotempcv-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: autoplot.ResamplingCustomCV
    > ### Title: Visualization Functions for Non-Spatial CV Methods.
    > ### Aliases: autoplot.ResamplingCustomCV plot.ResamplingCustomCV
    > 
    > ### ** Examples
    > 
    > if (mlr3misc::require_namespaces(c("sf", "patchwork"), quietly = TRUE)) {
    ...
    + 
    +   autoplot(resampling, task) +
    +     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
    +   autoplot(resampling, task, fold_id = 1)
    +   autoplot(resampling, task, fold_id = c(1, 2)) *
    +     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
    + }
    Error in identicalUnits(x) : object is not a unit
    Calls: <Anonymous> ... assemble_guides -> guides_build -> unit.c -> identicalUnits
    Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘spatiotemp-viz.Rmd’
      ...
    
    > knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
    
    > knitr::include_graphics("../man/figures/sptcv_cstf_multiplot.png")
    
      When sourcing ‘spatiotemp-viz.R’:
    Error: Cannot find the file(s): "../man/figures/sptcv_cstf_multiplot.png"
    Execution halted
    
      ‘mlr3spatiotempcv.Rmd’ using ‘UTF-8’... OK
      ‘spatiotemp-viz.Rmd’ using ‘UTF-8’... failed
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.9Mb
      sub-directories of 1Mb or more:
        data   3.5Mb
    ```

# mlr3viz

<details>

* Version: 0.8.0
* GitHub: https://github.com/mlr-org/mlr3viz
* Source code: https://github.com/cran/mlr3viz
* Date/Publication: 2024-03-05 12:50:03 UTC
* Number of recursive dependencies: 140

Run `revdepcheck::cloud_details(, "mlr3viz")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mlr3viz-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: autoplot.OptimInstanceSingleCrit
    > ### Title: Plots for Optimization Instances
    > ### Aliases: autoplot.OptimInstanceSingleCrit
    > 
    > ### ** Examples
    > 
    > if (requireNamespace("mlr3") && requireNamespace("bbotk") && requireNamespace("patchwork")) {
    ...
    INFO  [11:58:58.325] [bbotk]   5.884797  2.2371095 -32.51896
    INFO  [11:58:58.325] [bbotk]  -7.841127 -0.8872557 -91.31148
    INFO  [11:58:58.334] [bbotk] Finished optimizing after 20 evaluation(s)
    INFO  [11:58:58.335] [bbotk] Result:
    INFO  [11:58:58.338] [bbotk]        x1        x2  x_domain        y
    INFO  [11:58:58.338] [bbotk]     <num>     <num>    <list>    <num>
    INFO  [11:58:58.338] [bbotk]  2.582281 -2.940254 <list[2]> 9.657379
    Error in identicalUnits(x) : object is not a unit
    Calls: print ... assemble_guides -> guides_build -> unit.c -> identicalUnits
    Execution halted
    ```

# modeltime.resample

<details>

* Version: 0.2.3
* GitHub: https://github.com/business-science/modeltime.resample
* Source code: https://github.com/cran/modeltime.resample
* Date/Publication: 2023-04-12 15:50:02 UTC
* Number of recursive dependencies: 229

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
      ✔ broom        1.0.6          ✔ recipes      1.0.10    
      ✔ dials        1.2.1          ✔ rsample      1.2.1     
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

# mosaic

<details>

* Version: 1.9.1
* GitHub: https://github.com/ProjectMOSAIC/mosaic
* Source code: https://github.com/cran/mosaic
* Date/Publication: 2024-02-23 14:30:06 UTC
* Number of recursive dependencies: 131

Run `revdepcheck::cloud_details(, "mosaic")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mosaic-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: mUSMap
    > ### Title: Make a US map with 'ggplot2'
    > ### Aliases: mUSMap
    > 
    > ### ** Examples
    > 
    > USArrests2 <- USArrests |> tibble::rownames_to_column("state")
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(mosaic)
      Registered S3 method overwritten by 'mosaic':
        method                           from   
        fortify.SpatialPolygonsDataFrame ggplot2
      
      The 'mosaic' package masks several functions from core packages in order to add 
    ...
      • plotModel/plotmodel2.svg
      • plotModel/plotmodel3.svg
      • plotPoints/plotpoints2.svg
      • plotPoints/plotpoints3.svg
      • rfun/rfun2.svg
      • statTally/stattally2.svg
      • statTally/stattally3.svg
      • xpnorm/xpnorm2.svg
      Error: Test failures
      Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘MinimalRgg.Rnw’
      ...
    > set.seed(123)
    
    > knitr::opts_chunk$set(dev = "pdf", eval = FALSE, tidy = FALSE, 
    +     fig.align = "center", fig.show = "hold", message = FALSE)
    
    > apropos()
    
      When sourcing ‘MinimalRgg.R’:
    Error: argument "what" is missing, with no default
    Execution halted
    
      ‘Resampling.Rmd’ using ‘UTF-8’... OK
      ‘mosaic-resources.Rmd’ using ‘UTF-8’... OK
      ‘MinimalRgg.Rnw’ using ‘UTF-8’... failed
    ```

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘manipulate’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        R     5.0Mb
        doc   1.2Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘cubature’
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Resampling.Rmd’ using rmarkdown
    ```

# motifr

<details>

* Version: 1.0.0
* GitHub: https://github.com/marioangst/motifr
* Source code: https://github.com/cran/motifr
* Date/Publication: 2020-12-10 15:40:02 UTC
* Number of recursive dependencies: 121

Run `revdepcheck::cloud_details(, "motifr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘motifr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: directed_dummy_net
    > ### Title: Two-level directed network dummy example
    > ### Aliases: directed_dummy_net
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

# mpwR

<details>

* Version: 0.1.5
* GitHub: NA
* Source code: https://github.com/cran/mpwR
* Date/Publication: 2023-11-13 23:33:26 UTC
* Number of recursive dependencies: 112

Run `revdepcheck::cloud_details(, "mpwR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mpwR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_Upset
    > ### Title: Upset Plot
    > ### Aliases: plot_Upset
    > 
    > ### ** Examples
    > 
    > # Load libraries
    ...
      3.     ├─base::suppressMessages(...)
      4.     │ └─base::withCallingHandlers(...)
      5.     └─UpSetR:::Make_main_bar(...)
      6.       └─ggplot2::ggplotGrob(Main_bar_plot)
      7.         ├─ggplot2::ggplot_gtable(ggplot_build(x))
      8.         └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
      9.           └─ggplot2::calc_element("plot.margin", theme)
     10.             └─cli::cli_abort(...)
     11.               └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(mpwR)
      > 
      > test_check("mpwR")
      For DIA-NN no quantitative LFQ data on peptide-level.
      For PD no quantitative LFQ data on peptide-level.
      For DIA-NN no quantitative LFQ data on peptide-level.
    ...
        6.       └─ggplot2::ggplotGrob(Main_bar_plot)
        7.         ├─ggplot2::ggplot_gtable(ggplot_build(x))
        8.         └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
        9.           └─ggplot2::calc_element("plot.margin", theme)
       10.             └─cli::cli_abort(...)
       11.               └─rlang::abort(...)
      
      [ FAIL 1 | WARN 123 | SKIP 0 | PASS 658 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Workflow.Rmd’
      ...
    > plot_CV_density(input_list = CV_LFQ_PG, cv_col = "PG_quant")
    
    > Upset_prepared <- get_Upset_list(input_list = files, 
    +     level = "ProteinGroup.IDs")
    
    > plot_Upset(input_list = Upset_prepared, label = "ProteinGroup.IDs")
    
      When sourcing ‘Workflow.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘Import.Rmd’ using ‘UTF-8’... OK
      ‘Output_Explanations.Rmd’ using ‘UTF-8’... OK
      ‘Requirements.Rmd’ using ‘UTF-8’... OK
      ‘Workflow.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Import.Rmd’ using rmarkdown
    --- finished re-building ‘Import.Rmd’
    
    --- re-building ‘Output_Explanations.Rmd’ using rmarkdown
    --- finished re-building ‘Output_Explanations.Rmd’
    
    --- re-building ‘Requirements.Rmd’ using rmarkdown
    --- finished re-building ‘Requirements.Rmd’
    
    --- re-building ‘Workflow.Rmd’ using rmarkdown
    ```

# mrfDepth

<details>

* Version: 1.0.17
* GitHub: NA
* Source code: https://github.com/cran/mrfDepth
* Date/Publication: 2024-05-24 21:20:02 UTC
* Number of recursive dependencies: 44

Run `revdepcheck::cloud_details(, "mrfDepth")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mrfDepth-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: bagplot
    > ### Title: Draws a bagplot, a bivariate boxplot
    > ### Aliases: bagplot
    > ### Keywords: Graphical
    > 
    > ### ** Examples
    > 
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 20.2Mb
      sub-directories of 1Mb or more:
        data   1.6Mb
        libs  18.1Mb
    ```

# musclesyneRgies

<details>

* Version: 1.2.5
* GitHub: https://github.com/alesantuz/musclesyneRgies
* Source code: https://github.com/cran/musclesyneRgies
* Date/Publication: 2022-07-19 17:10:02 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "musclesyneRgies")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘musclesyneRgies-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_classified_syns
    > ### Title: Plot muscle synergies
    > ### Aliases: plot_classified_syns
    > 
    > ### ** Examples
    > 
    > # Load some data
    ...
      3.   │ └─base::withCallingHandlers(...)
      4.   └─gridExtra::arrangeGrob(...)
      5.     └─base::lapply(grobs[toconv], ggplot2::ggplotGrob)
      6.       └─ggplot2 (local) FUN(X[[i]], ...)
      7.         ├─ggplot2::ggplot_gtable(ggplot_build(x))
      8.         └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
      9.           └─ggplot2::calc_element("plot.margin", theme)
     10.             └─cli::cli_abort(...)
     11.               └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(musclesyneRgies)
      > 
      > test_check("musclesyneRgies")
      [ FAIL 1 | WARN 13 | SKIP 0 | PASS 45 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
       4.       └─ggplot2 (local) FUN(X[[i]], ...)
       5.         ├─ggplot2::ggplot_gtable(ggplot_build(x))
       6.         └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
       7.           └─ggplot2::calc_element("plot.margin", theme)
       8.             └─cli::cli_abort(...)
       9.               └─rlang::abort(...)
      
      [ FAIL 1 | WARN 13 | SKIP 0 | PASS 45 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘plots.Rmd’
      ...
    > library(musclesyneRgies)
    
    > data("RAW_DATA")
    
    > pp <- plot_rawEMG(RAW_DATA[[1]], trial = names(RAW_DATA)[1], 
    +     row_number = 4, col_number = 4, line_col = "tomato3")
    
      When sourcing ‘plots.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘analysis.Rmd’ using ‘UTF-8’... OK
      ‘plots.Rmd’ using ‘UTF-8’... failed
      ‘pro_tips.Rmd’ using ‘UTF-8’... OK
      ‘workflow.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘analysis.Rmd’ using rmarkdown
    ```

# naniar

<details>

* Version: 1.1.0
* GitHub: https://github.com/njtierney/naniar
* Source code: https://github.com/cran/naniar
* Date/Publication: 2024-03-05 10:10:02 UTC
* Number of recursive dependencies: 173

Run `revdepcheck::cloud_details(, "naniar")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘naniar-visualisation.Rmd’
      ...
    
    > library(naniar)
    
    > vis_miss(airquality)
    
    > gg_miss_upset(airquality)
    
      When sourcing ‘naniar-visualisation.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘exploring-imputed-values.Rmd’ using ‘UTF-8’... OK
      ‘getting-started-w-naniar.Rmd’ using ‘UTF-8’... OK
      ‘naniar-visualisation.Rmd’ using ‘UTF-8’... failed
      ‘replace-with-na.Rmd’ using ‘UTF-8’... OK
      ‘special-missing-values.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘exploring-imputed-values.Rmd’ using rmarkdown
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
* Number of recursive dependencies: 101

Run `revdepcheck::cloud_details(, "NetFACS")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘NetFACS-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: multiple_network_plot
    > ### Title: Plots networks for multiple conditions
    > ### Aliases: multiple_network_plot multiple.network.plot
    > 
    > ### ** Examples
    > 
    > data(emotions_set)
    ...
      4.     └─base::lapply(x$plots, plot_table, guides = guides)
      5.       ├─patchwork (local) FUN(X[[i]], ...)
      6.       └─patchwork:::plot_table.ggplot(X[[i]], ...)
      7.         └─ggplot2::ggplotGrob(x)
      8.           ├─ggplot2::ggplot_gtable(ggplot_build(x))
      9.           └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     10.             └─ggplot2::calc_element("plot.margin", theme)
     11.               └─cli::cli_abort(...)
     12.                 └─rlang::abort(...)
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

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘netfacs_tutorial.Rmd’ using rmarkdown
    ```

# NHSRplotthedots

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/NHSRplotthedots
* Date/Publication: 2021-11-03 20:20:10 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "NHSRplotthedots")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘NHSRplotthedots-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ptd_spc
    > ### Title: SPC Plotting Function
    > ### Aliases: ptd_spc
    > 
    > ### ** Examples
    > 
    > library(NHSRdatasets)
    ...
     1. ├─base (local) `<fn>`(x)
     2. └─NHSRplotthedots:::print.ptd_spc_df(x)
     3.   ├─base::print(p)
     4.   └─ggplot2:::print.ggplot(p)
     5.     ├─ggplot2::ggplot_gtable(data)
     6.     └─ggplot2:::ggplot_gtable.ggplot_built(data)
     7.       └─ggplot2::calc_element("plot.margin", theme)
     8.         └─cli::cli_abort(...)
     9.           └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘deviations.Rmd’
      ...
    
    > spc_data <- ptd_spc(df, value_field = data, date_field = date)
    
    > spc_data %>% plot() + labs(caption = paste("UPL = ", 
    +     round(spc_data$upl[1], 2), ", Mean = ", round(spc_data$mean[1], 
    +         2), ", LPL =  ..." ... [TRUNCATED] 
    
    ...
    > ptd_spc(stable_set, value_field = breaches, date_field = period, 
    +     improvement_direction = "decrease")
    
      When sourcing ‘intro.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘deviations.Rmd’ using ‘UTF-8’... failed
      ‘intro.Rmd’ using ‘UTF-8’... failed
      ‘number-of-points-required.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘deviations.Rmd’ using rmarkdown
    
    Quitting from lines 60-74 [unnamed-chunk-1] (deviations.Rmd)
    Error: processing vignette 'deviations.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘deviations.Rmd’
    
    --- re-building ‘intro.Rmd’ using rmarkdown
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘NHSRdatasets’ ‘grid’ ‘utils’
      All declared Imports should be used.
    ```

# nima

<details>

* Version: 0.6.2
* GitHub: https://github.com/nhejazi/nima
* Source code: https://github.com/cran/nima
* Date/Publication: 2020-03-06 06:10:03 UTC
* Number of recursive dependencies: 65

Run `revdepcheck::cloud_details(, "nima")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘nima-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: theme_jetblack
    > ### Title: A jet black theme with inverted colors
    > ### Aliases: theme_jetblack
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# NIMAA

<details>

* Version: 0.2.1
* GitHub: https://github.com/jafarilab/NIMAA
* Source code: https://github.com/cran/NIMAA
* Date/Publication: 2022-04-11 14:12:45 UTC
* Number of recursive dependencies: 172

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
      > 
      > test_check("NIMAA")
      binmatnest.temperature 
                    13.21249 
      Size of Square: 	 66 rows x  66 columns 
    ...
       1. └─NIMAA::extractSubMatrix(...) at test-extract-nonmissing-submatrix.R:5:3
       2.   └─NIMAA:::plotSubmatrix(...)
       3.     ├─base::print(plotly::ggplotly(p))
       4.     ├─plotly::ggplotly(p)
       5.     └─plotly:::ggplotly.ggplot(p)
       6.       └─plotly::gg2list(...)
      
      [ FAIL 1 | WARN 4 | SKIP 0 | PASS 7 ]
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

# nparACT

<details>

* Version: 0.8
* GitHub: NA
* Source code: https://github.com/cran/nparACT
* Date/Publication: 2017-12-20 14:25:17 UTC
* Number of recursive dependencies: 31

Run `revdepcheck::cloud_details(, "nparACT")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘nparACT-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: nparACT-package
    > ### Title: Non-Parametric Measures of Actigraphy Data
    > ### Aliases: nparACT-package nparACT
    > ### Keywords: package
    > 
    > ### ** Examples
    > 
    ...
     1. └─nparACT::nparACT_base("sleepstudy", SR = 4/60)
     2.   └─nparACT_auxfunctions2$nparACT_plot_hourly(data, data_hrs, SR)
     3.     ├─base::print(p)
     4.     └─ggplot2:::print.ggplot(p)
     5.       ├─ggplot2::ggplot_gtable(data)
     6.       └─ggplot2:::ggplot_gtable.ggplot_built(data)
     7.         └─ggplot2::calc_element("plot.margin", theme)
     8.           └─cli::cli_abort(...)
     9.             └─rlang::abort(...)
    Execution halted
    ```

# nullabor

<details>

* Version: 0.3.9
* GitHub: https://github.com/dicook/nullabor
* Source code: https://github.com/cran/nullabor
* Date/Publication: 2020-02-25 21:50:02 UTC
* Number of recursive dependencies: 79

Run `revdepcheck::cloud_details(, "nullabor")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘nullabor-examples.Rmd’
      ...
    +     data = dframe) + scale_colour_manual(values = c("red", "blue"), 
    +     guide = "n ..." ... [TRUNCATED] 
    Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ℹ Please use `linewidth` instead.
    Warning: Removed 20 rows containing missing values or values outside the scale range
    (`geom_rect()`).
    
      When sourcing ‘nullabor-examples.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘distances.Rmd’ using ‘UTF-8’... OK
      ‘nullabor-examples.Rmd’ using ‘UTF-8’... failed
      ‘nullabor.Rmd’ using ‘UTF-8’... OK
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘forecast’ ‘rlang’ ‘tsibble’ ‘viridis’
      All declared Imports should be used.
    ```

# OBIC

<details>

* Version: 3.0.2
* GitHub: https://github.com/AgroCares/Open-Bodem-Index-Calculator
* Source code: https://github.com/cran/OBIC
* Date/Publication: 2024-03-05 12:40:08 UTC
* Number of recursive dependencies: 75

Run `revdepcheck::cloud_details(, "OBIC")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘obic_workability.Rmd’
      ...
    > gg2 <- ggplot(data = dt, aes(x = field, fill = field)) + 
    +     geom_col(aes(y = I_P_WO)) + theme_bw() + theme(axis.text = element_text(size = 10, 
     .... [TRUNCATED] 
    
    > (gg | gg2) + plot_layout(guides = "collect") + plot_annotation(caption = "Baseline workability scores.", 
    +     theme = theme(plot.caption = element .... [TRUNCATED] 
    
      When sourcing ‘obic_workability.R’:
    Error: object is not a unit
    Execution halted
    
      ‘description-of-the-columns.Rmd’ using ‘UTF-8’... OK
      ‘obic_introduction.Rmd’ using ‘UTF-8’... OK
      ‘obic_score_aggregation.Rmd’ using ‘UTF-8’... OK
      ‘obic_water_functions.Rmd’ using ‘UTF-8’... OK
      ‘obic_workability.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘description-of-the-columns.Rmd’ using rmarkdown
    --- finished re-building ‘description-of-the-columns.Rmd’
    
    --- re-building ‘obic_introduction.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        data   4.0Mb
        doc    1.4Mb
    ```

# OddsPlotty

<details>

* Version: 1.0.2
* GitHub: https://github.com/StatsGary/OddsPlotty
* Source code: https://github.com/cran/OddsPlotty
* Date/Publication: 2021-11-13 14:40:02 UTC
* Number of recursive dependencies: 146

Run `revdepcheck::cloud_details(, "OddsPlotty")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘introduction.Rmd’
      ...
    > plot <- plotty$odds_plot
    
    > plot <- plot + ggthemes::theme_economist() + theme(legend.position = "NULL")
    
    > plot + geom_text(label = round(plotty$odds_plot$data$OR, 
    +     digits = 2), hjust = 0.1, vjust = 1, color = "navy")
    
      When sourcing ‘introduction.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘introduction.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘introduction.Rmd’ using rmarkdown
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘caret’ ‘e1071’ ‘ggthemes’ ‘mlbench’ ‘rmarkdown’ ‘tidymodels’
      All declared Imports should be used.
    ```

# ofpetrial

<details>

* Version: 0.1.1
* GitHub: https://github.com/DIFM-Brain/ofpetrial
* Source code: https://github.com/cran/ofpetrial
* Date/Publication: 2024-05-15 08:50:03 UTC
* Number of recursive dependencies: 145

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
     27. ├─dplyr::bind_rows(.)
     28. │ └─rlang::list2(...)
     29. └─ggExtra::ggMarginal(., type = "histogram")
     30.   └─ggplot2::ggplotGrob(scatP)
     31.     ├─ggplot2::ggplot_gtable(ggplot_build(x))
     32.     └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     33.       └─ggplot2::calc_element("plot.margin", theme)
     34.         └─cli::cli_abort(...)
     35.           └─rlang::abort(...)
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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘OmicNavigatorAPI.Rnw’
      ...
        "test_02": 0.07
      }
    ] 
    
    > resultsUpset <- getResultsUpset(study = "ABC", modelID = "model_01", 
    +     sigValue = 0.5, operator = "<", column = "p_val")
    
      When sourcing ‘OmicNavigatorAPI.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘OmicNavigatorAPI.Rnw’ using ‘UTF-8’... failed
      ‘OmicNavigatorUsersGuide.Rnw’ using ‘UTF-8’... OK
    ```

## In both

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘OmicNavigatorAPI.Rnw’ using Sweave
    OmicNavigator R package version: 1.13.13
    The app is not installed. Install it with installApp()
    Installing study "ABC" in /tmp/RtmpZpDw4T/file231e3ed1b448
    Exporting study "ABC" as an R package
    Note: No maintainer email was specified. Using the placeholder: Unknown <unknown@unknown>
    Calculating pairwise overlaps. This may take a while...
    Exported study to /tmp/RtmpZpDw4T/ONstudyABC
    Success!
    ...
                       write 
    l.14 
         
    --- failed re-building ‘OmicNavigatorUsersGuide.Rnw’
    
    SUMMARY: processing the following files failed:
      ‘OmicNavigatorAPI.Rnw’ ‘OmicNavigatorUsersGuide.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# oncomsm

<details>

* Version: 0.1.4
* GitHub: https://github.com/Boehringer-Ingelheim/oncomsm
* Source code: https://github.com/cran/oncomsm
* Date/Publication: 2023-04-17 07:00:02 UTC
* Number of recursive dependencies: 126

Run `revdepcheck::cloud_details(, "oncomsm")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(dplyr)
      
      Attaching package: 'dplyr'
      
      The following objects are masked from 'package:stats':
      
          filter, lag
    ...
       10.               └─grid::unit.c(legend.box.margin[4], widths, legend.box.margin[2])
       11.                 └─grid:::identicalUnits(x)
      
      [ FAIL 1 | WARN 0 | SKIP 2 | PASS 59 ]
      Deleting unused snapshots:
      • plots/plot-mstate-srp-model-2.svg
      • plots/plot-mstate-srp-model-3.svg
      • plots/plot-srp-model-2.svg
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘avoiding-bias.Rmd’
      ...
    
    > mdl <- create_srpmodel(A = define_srp_prior(median_t_q05 = c(1, 
    +     4, 12), median_t_q95 = c(6, 8, 36), shape_q05 = c(0.99, 0.99, 
    +     0.99), s .... [TRUNCATED] 
    
    > plot(mdl, confidence = 0.9)
    
    ...
    
    > plot(mdl, parameter_sample = smpl_prior, confidence = 0.75)
    
      When sourcing ‘oncomsm.R’:
    Error: object is not a unit
    Execution halted
    
      ‘avoiding-bias.Rmd’ using ‘UTF-8’... failed
      ‘oncomsm.Rmd’ using ‘UTF-8’... failed
      ‘prior-choice.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘avoiding-bias.Rmd’ using rmarkdown
    
    Quitting from lines 35-46 [unnamed-chunk-2] (avoiding-bias.Rmd)
    Error: processing vignette 'avoiding-bias.Rmd' failed with diagnostics:
    object is not a unit
    --- failed re-building ‘avoiding-bias.Rmd’
    
    --- re-building ‘oncomsm.Rmd’ using rmarkdown
    
    Quitting from lines 211-215 [plotting-the-prior] (oncomsm.Rmd)
    Error: processing vignette 'oncomsm.Rmd' failed with diagnostics:
    object is not a unit
    --- failed re-building ‘oncomsm.Rmd’
    
    --- re-building ‘prior-choice.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 59.1Mb
      sub-directories of 1Mb or more:
        doc    1.1Mb
        libs  56.9Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# ontophylo

<details>

* Version: 1.1.3
* GitHub: https://github.com/diegosasso/ontophylo
* Source code: https://github.com/cran/ontophylo
* Date/Publication: 2024-01-10 10:33:17 UTC
* Number of recursive dependencies: 101

Run `revdepcheck::cloud_details(, "ontophylo")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ontophylo-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: edgeplot
    > ### Title: Plot edge profiles and contMap
    > ### Aliases: edgeplot
    > 
    > ### ** Examples
    > 
    > data("hym_tree", "hym_kde")
    ...
      2. │ └─base::withCallingHandlers(...)
      3. └─ontophylo::edgeplot(map_stat, prof_stat)
      4.   ├─base::print(plot_edgeprof, vp = vp)
      5.   └─ggplot2:::print.ggplot(plot_edgeprof, vp = vp)
      6.     ├─ggplot2::ggplot_gtable(data)
      7.     └─ggplot2:::ggplot_gtable.ggplot_built(data)
      8.       └─ggplot2::calc_element("plot.margin", theme)
      9.         └─cli::cli_abort(...)
     10.           └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.3Mb
      sub-directories of 1Mb or more:
        data   7.0Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 42 marked UTF-8 strings
    ```

# OpenLand

<details>

* Version: 1.0.3
* GitHub: https://github.com/reginalexavier/OpenLand
* Source code: https://github.com/cran/OpenLand
* Date/Publication: 2024-05-03 13:40:02 UTC
* Number of recursive dependencies: 119

Run `revdepcheck::cloud_details(, "OpenLand")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(OpenLand)
      > 
      > test_check("OpenLand")
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 103 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
        9.           └─ggplot2 (local) FUN(X[[i]], ...)
       10.             ├─ggplot2::ggplot_gtable(ggplot_build(x))
       11.             └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
       12.               └─ggplot2::calc_element("plot.margin", theme)
       13.                 └─cli::cli_abort(...)
       14.                   └─rlang::abort(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 103 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘openland_vignette.Rmd’
      ...
    
    
    
    > plot(testSL$interval_lvl, labels = c(leftlabel = "Interval Change Area (%)", 
    +     rightlabel = "Annual Change Area (%)"), marginplot = c(-8, 
    +    .... [TRUNCATED] 
    
      When sourcing ‘openland_vignette.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘openland_vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘openland_vignette.Rmd’ using rmarkdown
    trying URL 'https://zenodo.org/record/3685230/files/SaoLourencoBasin.rda?download=1'
    Content type 'application/octet-stream' length 5309066 bytes (5.1 MB)
    ==================================================
    downloaded 5.1 MB
    
    
    Quitting from lines 184-191 [unnamed-chunk-10] (openland_vignette.Rmd)
    Error: processing vignette 'openland_vignette.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘openland_vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘openland_vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ordbetareg

<details>

* Version: 0.7.2
* GitHub: https://github.com/saudiwin/ordbetareg_pack
* Source code: https://github.com/cran/ordbetareg
* Date/Publication: 2023-08-10 07:30:02 UTC
* Number of recursive dependencies: 182

Run `revdepcheck::cloud_details(, "ordbetareg")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘package_introduction.Rmd’
      ...
    +     theme_minimal() + theme(panel.grid = element_blank()) + scale_x_continuous(brea .... [TRUNCATED] 
    
    > plots <- pp_check_ordbeta(ord_fit_mean, ndraws = 100, 
    +     outcome_label = "Thermometer Rating", new_theme = ggthemes::theme_economist())
    
    > plots$discrete
    
      When sourcing ‘package_introduction.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘package_introduction.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘package_introduction.Rmd’ using rmarkdown
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

# otsad

<details>

* Version: 0.2.0
* GitHub: https://github.com/alaineiturria/otsad
* Source code: https://github.com/cran/otsad
* Date/Publication: 2019-09-06 09:50:02 UTC
* Number of recursive dependencies: 109

Run `revdepcheck::cloud_details(, "otsad")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘otsad-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: CpKnnCad
    > ### Title: Classic processing KNN based Conformal Anomaly Detector
    > ###   (KNN-CAD)
    > ### Aliases: CpKnnCad
    > 
    > ### ** Examples
    > 
    ...
    +   ncm.type = "ICAD",
    +   reducefp = TRUE
    + )
    > 
    > ## Plot results
    > res <- cbind(df, result)
    > PlotDetections(res, title = "KNN-CAD ANOMALY DETECTOR")
    Error in pm[[2]] : subscript out of bounds
    Calls: PlotDetections -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘otsad.Rnw’ using knitr
    Error: processing vignette 'otsad.Rnw' failed with diagnostics:
    Running 'texi2dvi' on 'otsad.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `colortbl.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    ...
    l.12 \makeatletter
                      ^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘otsad.Rnw’
    
    SUMMARY: processing the following file failed:
      ‘otsad.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# OutliersO3

<details>

* Version: 0.6.3
* GitHub: NA
* Source code: https://github.com/cran/OutliersO3
* Date/Publication: 2020-04-25 00:10:02 UTC
* Number of recursive dependencies: 145

Run `revdepcheck::cloud_details(, "OutliersO3")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘DrawingO3plots.Rmd’
      ...
    > O3s <- O3prep(data, method = "HDo", tols = 0.05, boxplotLimits = 6)
    
    > O3s1 <- O3plotT(O3s, caseNames = Election2005$Name)
    
    > O3s1$gO3 + theme(plot.margin = unit(c(0, 2, 0, 0), 
    +     "cm"))
    
    ...
    +     1, 0, 0), "cm")), O3r1$gpcp, ncol = 1, heights = c(2, 1))
    
      When sourcing ‘MultTolLevels.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘DrawingO3plots.Rmd’ using ‘UTF-8’... failed
      ‘MultTolLevels.Rmd’ using ‘UTF-8’... failed
      ‘PCPsO3.Rmd’ using ‘UTF-8’... OK
      ‘xtraO3methods.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘DrawingO3plots.Rmd’ using rmarkdown
    
    Quitting from lines 25-32 [unnamed-chunk-1] (DrawingO3plots.Rmd)
    Error: processing vignette 'DrawingO3plots.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘DrawingO3plots.Rmd’
    
    --- re-building ‘MultTolLevels.Rmd’ using rmarkdown
    ```

# palettes

<details>

* Version: 0.2.0
* GitHub: https://github.com/mccarthy-m-g/palettes
* Source code: https://github.com/cran/palettes
* Date/Publication: 2024-02-05 11:50:02 UTC
* Number of recursive dependencies: 110

Run `revdepcheck::cloud_details(, "palettes")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘biscale.Rmd’
      ...
    +     "2-2")
    
    > names(unnamed_colour_vector)
    [1] "1-1" "2-1" "1-2" "2-2"
    
    > bi_pal(named_colour_vector, dim = 2)
    
    ...
      When sourcing ‘biscale.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘biscale.Rmd’ using ‘UTF-8’... failed
      ‘compatibility.Rmd’ using ‘UTF-8’... OK
      ‘creating-packages.Rmd’ using ‘UTF-8’... OK
      ‘ggplot2.Rmd’ using ‘UTF-8’... OK
      ‘gt.Rmd’ using ‘UTF-8’... OK
      ‘palettes.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘biscale.Rmd’ using rmarkdown
    
    Quitting from lines 66-67 [unnamed-chunk-4] (biscale.Rmd)
    Error: processing vignette 'biscale.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘biscale.Rmd’
    
    --- re-building ‘compatibility.Rmd’ using rmarkdown
    --- finished re-building ‘compatibility.Rmd’
    
    --- re-building ‘creating-packages.Rmd’ using rmarkdown
    --- finished re-building ‘creating-packages.Rmd’
    
    --- re-building ‘ggplot2.Rmd’ using rmarkdown
    ```

# ParBayesianOptimization

<details>

* Version: 1.2.6
* GitHub: https://github.com/AnotherSamWilson/ParBayesianOptimization
* Source code: https://github.com/cran/ParBayesianOptimization
* Date/Publication: 2022-10-18 14:47:54 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::cloud_details(, "ParBayesianOptimization")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ParBayesianOptimization-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.bayesOpt
    > ### Title: Plot a 'bayesOpt' object
    > ### Aliases: plot.bayesOpt
    > 
    > ### ** Examples
    > 
    > scoringFunction <- function(x) {
    ...
      3. └─ParBayesianOptimization:::plot.bayesOpt(Results)
      4.   └─ggpubr::ggarrange(...)
      5.     └─ggpubr::get_legend(plots)
      6.       └─ggpubr:::.get_legend(p[[i]], position = position)
      7.         ├─ggplot2::ggplot_gtable(ggplot_build(p))
      8.         └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(p))
      9.           └─ggplot2::calc_element("plot.margin", theme)
     10.             └─cli::cli_abort(...)
     11.               └─rlang::abort(...)
    Execution halted
    ```

# patchwork

<details>

* Version: 1.2.0
* GitHub: https://github.com/thomasp85/patchwork
* Source code: https://github.com/cran/patchwork
* Date/Publication: 2024-01-08 14:40:02 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "patchwork")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘patchwork-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: free
    > ### Title: Free a plot from alignment
    > ### Aliases: free
    > 
    > ### ** Examples
    > 
    > # Sometimes you have a plot that defies good composition alginment, e.g. due
    ...
    > p1 / p2
    > 
    > # We can fix this be using free
    > free(p1) / p2
    > 
    > # We can still collect guides like before
    > free(p1) / p2 + plot_layout(guides = "collect")
    Error in identicalUnits(x) : object is not a unit
    Calls: <Anonymous> ... assemble_guides -> guides_build -> unit.c -> identicalUnits
    Execution halted
    ```

# pathfindR

<details>

* Version: 2.4.1
* GitHub: https://github.com/egeulgen/pathfindR
* Source code: https://github.com/cran/pathfindR
* Date/Publication: 2024-05-04 15:30:05 UTC
* Number of recursive dependencies: 150

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
    ...
      9.             └─ggplot2:::scale_apply(layer_data, x_vars, "map", SCALE_X, self$panel_scales_x)
     10.               └─base::lapply(...)
     11.                 └─ggplot2 (local) FUN(X[[i]], ...)
     12.                   └─base::lapply(...)
     13.                     └─ggplot2 (local) FUN(X[[i]], ...)
     14.                       └─scales[[i]][[method]](data[[var]][scale_index[[i]]])
     15.                         └─ggplot2 (local) map(..., self = self)
     16.                           └─cli::cli_abort(...)
     17.                             └─rlang::abort(...)
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
    
    
    
    > output_df <- run_pathfindR(example_pathfindR_input, 
    +     pin_name_path = "/path/to/myPIN.sif")
    
      When sourcing ‘intro_vignette.R’:
    ...
      When sourcing ‘visualization_vignette.R’:
    Error: The `palette` function must return at least 21 values.
    Execution halted
    
      ‘comparing_results.Rmd’ using ‘UTF-8’... OK
      ‘intro_vignette.Rmd’ using ‘UTF-8’... failed
      ‘manual_execution.Rmd’ using ‘UTF-8’... failed
      ‘non_hs_analysis.Rmd’ using ‘UTF-8’... failed
      ‘obtain_data.Rmd’ using ‘UTF-8’... failed
      ‘visualization_vignette.Rmd’ using ‘UTF-8’... failed
    ```

# pdSpecEst

<details>

* Version: 1.2.4
* GitHub: https://github.com/JorisChau/pdSpecEst
* Source code: https://github.com/cran/pdSpecEst
* Date/Publication: 2020-01-08 09:10:07 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "pdSpecEst")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘wavelet_est_clust.Rmd’
      ...
    Warning: Use of `longdata$Var1` is discouraged.
    ℹ Use `Var1` instead.
    Warning: Use of `longdata$Var2` is discouraged.
    ℹ Use `Var2` instead.
    Warning: Use of `longdata$value` is discouraged.
    ℹ Use `value` instead.
    
      When sourcing ‘wavelet_est_clust.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘depth_ranktests.Rmd’ using ‘UTF-8’... OK
      ‘wavelet_est_clust.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘depth_ranktests.Rmd’ using rmarkdown
    
    warning: logmat_sympd(): imaginary components on diagonal are non-zero
    
    warning: logmat_sympd(): given matrix is not hermitian
    --- finished re-building ‘depth_ranktests.Rmd’
    
    --- re-building ‘wavelet_est_clust.Rmd’ using rmarkdown
    ```

## In both

*   checking C++ specification ... NOTE
    ```
      Specified C++11: please drop specification unless essential
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  9.1Mb
      sub-directories of 1Mb or more:
        libs   8.0Mb
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
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

# personalized

<details>

* Version: 0.2.7
* GitHub: https://github.com/jaredhuling/personalized
* Source code: https://github.com/cran/personalized
* Date/Publication: 2022-06-27 20:20:03 UTC
* Number of recursive dependencies: 94

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

# PGRdup

<details>

* Version: 0.2.3.9
* GitHub: https://github.com/aravind-j/PGRdup
* Source code: https://github.com/cran/PGRdup
* Date/Publication: 2023-08-31 22:10:16 UTC
* Number of recursive dependencies: 69

Run `revdepcheck::cloud_details(, "PGRdup")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Introduction.Rmd’ using rmarkdown_notangle
    
    Quitting from lines 1195-1203 [unnamed-chunk-59] (Introduction.Rmd)
    Error: processing vignette 'Introduction.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘Introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## Newly fixed

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Introduction.Rmd’ using rmarkdown_notangle
    tlmgr: package repository https://mirrors.rit.edu/CTAN/systems/texlive/tlnet (verified)
    [1/1, ??:??/??:??] install: colortbl [4k]
    running mktexlsr ...
    done running mktexlsr.
    tlmgr: package log updated: /opt/TinyTeX/texmf-var/web2c/tlmgr.log
    tlmgr: command log updated: /opt/TinyTeX/texmf-var/web2c/tlmgr-commands.log
    
    tlmgr: Remote database (revision 71410 of the texlive-scripts package)
    ...
    
    Error: processing vignette 'Introduction.Rmd' failed with diagnostics:
    LaTeX failed to compile /tmp/workdir/PGRdup/old/PGRdup.Rcheck/vign_test/PGRdup/vignettes/Introduction.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See Introduction.log for more info.
    --- failed re-building ‘Introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Introduction.Rmd’
    
    Error: Vignette re-building failed.
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
* Number of recursive dependencies: 86

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
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 49 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      Backtrace:
          ▆
       1. └─plotDK::plotDK(...) at test-plotDK.R:32:5
       2.   ├─plotly::ggplotly(p, tooltip = c("text", "fill"))
       3.   └─plotly:::ggplotly.ggplot(p, tooltip = c("text", "fill"))
       4.     └─plotly::gg2list(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 49 ]
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

# plotly

<details>

* Version: 4.10.4
* GitHub: https://github.com/plotly/plotly.R
* Source code: https://github.com/cran/plotly
* Date/Publication: 2024-01-13 22:40:02 UTC
* Number of recursive dependencies: 147

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

# pmartR

<details>

* Version: 2.4.5
* GitHub: https://github.com/pmartR/pmartR
* Source code: https://github.com/cran/pmartR
* Date/Publication: 2024-05-21 15:50:02 UTC
* Number of recursive dependencies: 149

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
      [ FAIL 1 | WARN 1 | SKIP 11 | PASS 2375 ]
      
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
      installed size is 10.4Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        help   1.5Mb
        libs   6.3Mb
    ```

# pmxTools

<details>

* Version: 1.3
* GitHub: https://github.com/kestrel99/pmxTools
* Source code: https://github.com/cran/pmxTools
* Date/Publication: 2023-02-21 16:00:08 UTC
* Number of recursive dependencies: 85

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

# politeness

<details>

* Version: 0.9.3
* GitHub: NA
* Source code: https://github.com/cran/politeness
* Date/Publication: 2023-11-12 13:13:26 UTC
* Number of recursive dependencies: 93

Run `revdepcheck::cloud_details(, "politeness")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘politeness-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: politenessPlot
    > ### Title: Politeness plot
    > ### Aliases: politenessPlot
    > 
    > ### ** Examples
    > 
    > 
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘politeness.Rmd’
      ...
    28      0                  0        0        0            0
    29      0                  1        0        0            0
    30      0                  1        0        1            0
    
    > politeness::politenessPlot(df_politeness, split = phone_offers$condition, 
    +     split_levels = c("Tough", "Warm"), split_name = "Condition")
    
      When sourcing ‘politeness.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘politeness.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘politeness.Rmd’ using rmarkdown
    
    Quitting from lines 119-123 [unnamed-chunk-8] (politeness.Rmd)
    Error: processing vignette 'politeness.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘politeness.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘politeness.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 52 marked UTF-8 strings
    ```

# posterior

<details>

* Version: 1.5.0
* GitHub: https://github.com/stan-dev/posterior
* Source code: https://github.com/cran/posterior
* Date/Publication: 2023-10-31 08:30:02 UTC
* Number of recursive dependencies: 120

Run `revdepcheck::cloud_details(, "posterior")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘posterior.Rmd’ using rmarkdown
    --- finished re-building ‘posterior.Rmd’
    
    --- re-building ‘rvar.Rmd’ using rmarkdown
    
    Quitting from lines 526-529 [mixture] (rvar.Rmd)
    Error: processing vignette 'rvar.Rmd' failed with diagnostics:
    Problem while setting up geom aesthetics.
    ...
        NULL, NULL, 1.2, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.2, NULL, list(NULL, NULL, NULL, 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0, list(), 2, list("grey92", NA, NULL, NULL, TRUE), list(), NULL, NULL, NULL, list("white", NULL, NULL, NULL, FALSE, "white", TRUE), NULL, list(NULL, 0.5, NULL, NULL, FALSE, NULL, TRUE), NULL, 
        NULL, NULL, NULL, FALSE, list(NULL, "white", NULL, NULL, TRUE), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(5.5, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, TRUE), "topleft", NULL, NULL, list("grey85", NA, NULL, NULL, TRUE), NULL, NULL, "inherit", "inside", list(NULL, NULL, "grey10", 
            0.8, NULL, NULL, NULL, NULL, c(4.4, 4.4, 4.4, 4.4), NULL, TRUE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75))
    --- failed re-building ‘rvar.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘rvar.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘rvar.Rmd’
      ...
    > y
    rvar<4000>[3] mean ± sd:
    [1] 3.00 ± 1.00  2.02 ± 0.99  0.96 ± 0.99 
    
    > X + y
    
      When sourcing ‘rvar.R’:
    Error: Cannot broadcast array of shape [4000,3,1] to array of shape [4000,4,3]:
    All dimensions must be 1 or equal.
    Execution halted
    
      ‘posterior.Rmd’ using ‘UTF-8’... OK
      ‘rvar.Rmd’ using ‘UTF-8’... failed
    ```

# PPQplan

<details>

* Version: 1.1.0
* GitHub: https://github.com/allenzhuaz/PPQplan
* Source code: https://github.com/cran/PPQplan
* Date/Publication: 2020-10-08 04:30:06 UTC
* Number of recursive dependencies: 119

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
    '/tmp/RtmpklfDYr/file150247a66b97/vignettes'.
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

* Version: 0.2.4
* GitHub: https://github.com/zabore/ppseq
* Source code: https://github.com/cran/ppseq
* Date/Publication: 2024-04-04 18:20:02 UTC
* Number of recursive dependencies: 106

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

# PPtreeregViz

<details>

* Version: 2.0.5
* GitHub: https://github.com/sunsmiling/PPtreeregViz
* Source code: https://github.com/cran/PPtreeregViz
* Date/Publication: 2022-12-23 19:20:05 UTC
* Number of recursive dependencies: 125

Run `revdepcheck::cloud_details(, "PPtreeregViz")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘PPtreeregViz-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: PPregNodeViz
    > ### Title: Node visualization
    > ### Aliases: PPregNodeViz
    > ### Keywords: tree
    > 
    > ### ** Examples
    > 
    ...
        ▆
     1. └─PPtreeregViz::PPregNodeViz(Model, node.id = 1)
     2.   └─ggExtra::ggMarginal(...)
     3.     └─ggplot2::ggplotGrob(scatP)
     4.       ├─ggplot2::ggplot_gtable(ggplot_build(x))
     5.       └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     6.         └─ggplot2::calc_element("plot.margin", theme)
     7.           └─cli::cli_abort(...)
     8.             └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘PPtreeregViz.Rmd’
      ...
    
    > plot(Tree.Imp)
    
    > plot(Tree.Imp, marginal = TRUE, num_var = 5)
    
    > PPregNodeViz(Model, node.id = 1)
    
      When sourcing ‘PPtreeregViz.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘PPtreeregViz.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘PPtreeregViz.Rmd’ using rmarkdown
    ```

## In both

*   checking C++ specification ... NOTE
    ```
      Specified C++11: please drop specification unless essential
    ```

# precrec

<details>

* Version: 0.14.4
* GitHub: https://github.com/evalclass/precrec
* Source code: https://github.com/cran/precrec
* Date/Publication: 2023-10-11 22:10:02 UTC
* Number of recursive dependencies: 71

Run `revdepcheck::cloud_details(, "precrec")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘introduction.Rmd’
      ...
    > msmdat3 <- mmdata(samps2[["scores"]], samps2[["labels"]], 
    +     modnames = samps2[["modnames"]])
    
    > mscurves <- evalmod(msmdat3)
    
    > autoplot(mscurves)
    
      When sourcing ‘introduction.R’:
    Error: object is not a unit
    Execution halted
    
      ‘introduction.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘introduction.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        libs   4.2Mb
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
    > plot(dhs, axes = TRUE)
    
    > qa <- quick.prevR(fdhs, return.results = TRUE, return.plot = TRUE, 
    +     plot.results = FALSE, progression = FALSE)
    
    > qa$plot
    
      When sourcing ‘intro_prevR.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
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

# primerTree

<details>

* Version: 1.0.6
* GitHub: NA
* Source code: https://github.com/cran/primerTree
* Date/Publication: 2022-04-05 14:30:02 UTC
* Number of recursive dependencies: 53

Run `revdepcheck::cloud_details(, "primerTree")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘primerTree-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.primerTree
    > ### Title: plot function for a primerTree object, calls plot_tree_ranks
    > ### Aliases: plot.primerTree
    > 
    > ### ** Examples
    > 
    > library(gridExtra)
    ...
      4.     ├─base::do.call(arrangeGrob, plots)
      5.     └─gridExtra (local) `<fn>`(...)
      6.       └─base::lapply(grobs[toconv], ggplot2::ggplotGrob)
      7.         └─ggplot2 (local) FUN(X[[i]], ...)
      8.           ├─ggplot2::ggplot_gtable(ggplot_build(x))
      9.           └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     10.             └─ggplot2::calc_element("plot.margin", theme)
     11.               └─cli::cli_abort(...)
     12.                 └─rlang::abort(...)
    Execution halted
    ```

# processmapR

<details>

* Version: 0.5.3
* GitHub: https://github.com/bupaverse/processmapr
* Source code: https://github.com/cran/processmapR
* Date/Publication: 2023-04-06 12:50:02 UTC
* Number of recursive dependencies: 118

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
      
      [ FAIL 6 | WARN 0 | SKIP 10 | PASS 107 ]
      Error: Test failures
      Execution halted
    ```

# PTXQC

<details>

* Version: 1.1.1
* GitHub: https://github.com/cbielow/PTXQC
* Source code: https://github.com/cran/PTXQC
* Date/Publication: 2024-03-11 19:50:02 UTC
* Number of recursive dependencies: 97

Run `revdepcheck::cloud_details(, "PTXQC")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(PTXQC)
      Loading package PTXQC (version 1.1.1)
      > 
      > ##
      > ## calls all code in PTXQC/tests/testthat/test*.R
      > ##
    ...
        8.           └─ggplot2::ggplotGrob(Main_bar_plot)
        9.             ├─ggplot2::ggplot_gtable(ggplot_build(x))
       10.             └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
       11.               └─ggplot2::calc_element("plot.margin", theme)
       12.                 └─cli::cli_abort(...)
       13.                   └─rlang::abort(...)
      
      [ FAIL 1 | WARN 20 | SKIP 0 | PASS 131 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.5Mb
      sub-directories of 1Mb or more:
        R          1.5Mb
        doc        4.0Mb
        examples   2.6Mb
    ```

# qacBase

<details>

* Version: 1.0.3
* GitHub: https://github.com/rkabacoff/qacBase
* Source code: https://github.com/cran/qacBase
* Date/Publication: 2022-02-09 22:20:02 UTC
* Number of recursive dependencies: 99

Run `revdepcheck::cloud_details(, "qacBase")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘qacBase-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: scatter
    > ### Title: Scatterplot
    > ### Aliases: scatter
    > 
    > ### ** Examples
    > 
    > scatter(cars74, hp, mpg)
    ...
        ▆
     1. └─qacBase::scatter(...)
     2.   └─ggExtra::ggMarginal(p, size = 8, type = margin, fill = margin_color)
     3.     └─ggplot2::ggplotGrob(scatP)
     4.       ├─ggplot2::ggplot_gtable(ggplot_build(x))
     5.       └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     6.         └─ggplot2::calc_element("plot.margin", theme)
     7.           └─cli::cli_abort(...)
     8.             └─rlang::abort(...)
    Execution halted
    ```

# qgcomp

<details>

* Version: 2.15.2
* GitHub: https://github.com/alexpkeil1/qgcomp
* Source code: https://github.com/cran/qgcomp
* Date/Publication: 2023-08-10 09:10:06 UTC
* Number of recursive dependencies: 157

Run `revdepcheck::cloud_details(, "qgcomp")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘qgcomp-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.qgcompfit
    > ### Title: Default plotting method for a qgcompfit object
    > ### Aliases: plot.qgcompfit plot.qgcompmultfit
    > 
    > ### ** Examples
    > 
    > set.seed(12)
    ...
      3.   └─qgcomp:::.plot_noboot_base(x, nms, theme_butterfly_r, theme_butterfly_l)
      4.     └─gridExtra::arrangeGrob(...)
      5.       └─base::lapply(grobs[toconv], ggplot2::ggplotGrob)
      6.         └─ggplot2 (local) FUN(X[[i]], ...)
      7.           ├─ggplot2::ggplot_gtable(ggplot_build(x))
      8.           └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
      9.             └─ggplot2::calc_element("plot.margin", theme)
     10.               └─cli::cli_abort(...)
     11.                 └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘qgcomp-vignette.Rmd’
      ...
    
                 Estimate Std. Error Lower CI Upper CI t value  Pr(>|t|)
    (Intercept) -0.348084   0.108037 -0.55983 -0.13634 -3.2219 0.0013688
    psi1         0.256969   0.071459  0.11691  0.39703  3.5960 0.0003601
    
    > plot(qc.fit3)
    
      When sourcing ‘qgcomp-vignette.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘qgcomp-vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘qgcomp-vignette.Rmd’ using knitr
    
    Quitting from lines 234-242 [adjusting for covariates a] (qgcomp-vignette.Rmd)
    Error: processing vignette 'qgcomp-vignette.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘qgcomp-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘qgcomp-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# qgcompint

<details>

* Version: 0.7.0
* GitHub: https://github.com/alexpkeil1/qgcomp
* Source code: https://github.com/cran/qgcompint
* Date/Publication: 2022-03-22 16:00:02 UTC
* Number of recursive dependencies: 132

Run `revdepcheck::cloud_details(, "qgcompint")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘qgcompint-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.qgcompemmfit
    > ### Title: Default plotting method for a qgcompfit object
    > ### Aliases: plot.qgcompemmfit
    > 
    > ### ** Examples
    > 
    > set.seed(50)
    ...
      5.     └─qgcomp:::.plot_noboot_base(x, nms, theme_butterfly_r, theme_butterfly_l)
      6.       └─gridExtra::arrangeGrob(...)
      7.         └─base::lapply(grobs[toconv], ggplot2::ggplotGrob)
      8.           └─ggplot2 (local) FUN(X[[i]], ...)
      9.             ├─ggplot2::ggplot_gtable(ggplot_build(x))
     10.             └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     11.               └─ggplot2::calc_element("plot.margin", theme)
     12.                 └─cli::cli_abort(...)
     13.                   └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘qgcompint-vignette.Rmd’
      ...
    1         1
    2         1
    3         1
    4         1
    
    > plot(qfit1, emmval = 0)
    
      When sourcing ‘qgcompint-vignette.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘qgcompint-vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘qgcompint-vignette.Rmd’ using knitr
    
    Quitting from lines 119-121 [first_step_plot] (qgcompint-vignette.Rmd)
    Error: processing vignette 'qgcompint-vignette.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘qgcompint-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘qgcompint-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# qpNCA

<details>

* Version: 1.1.6
* GitHub: NA
* Source code: https://github.com/cran/qpNCA
* Date/Publication: 2021-08-16 12:50:02 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::cloud_details(, "qpNCA")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Example-full-nca-analysis.rmd’
      ...
    
    Performing Thalf estimation...
    
    Creating regression plots in standard output...
    
    [[1]]
    
    ...
    [[1]]
    
      When sourcing ‘Example-stepwise-nca-analysis.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘Parameter_Guidelines.rmd’ using ‘UTF-8’... OK
      ‘User_Guide.rmd’ using ‘UTF-8’... OK
      ‘Example-full-nca-analysis.rmd’ using ‘UTF-8’... failed
      ‘Example-stepwise-nca-analysis.rmd’ using ‘UTF-8’... failed
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
    ...
    Quitting from lines 121-135 [unnamed-chunk-6] (Example-stepwise-nca-analysis.rmd)
    Error: processing vignette 'Example-stepwise-nca-analysis.rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘Example-stepwise-nca-analysis.rmd’
    
    SUMMARY: processing the following files failed:
      ‘Example-full-nca-analysis.rmd’ ‘Example-stepwise-nca-analysis.rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# QurvE

<details>

* Version: 1.1.1
* GitHub: https://github.com/NicWir/QurvE
* Source code: https://github.com/cran/QurvE
* Date/Publication: 2024-01-26 12:40:14 UTC
* Number of recursive dependencies: 145

Run `revdepcheck::cloud_details(, "QurvE")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘QurvE-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: flFitSpline
    > ### Title: Perform a smooth spline fit on fluorescence data
    > ### Aliases: flFitSpline
    > 
    > ### ** Examples
    > 
    > # load example dataset
    ...
     17.                   └─cowplot:::as_gtable.default(x)
     18.                     ├─cowplot::as_grob(plot)
     19.                     └─cowplot:::as_grob.ggplot(plot)
     20.                       └─ggplot2::ggplotGrob(plot)
     21.                         ├─ggplot2::ggplot_gtable(ggplot_build(x))
     22.                         └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     23.                           └─ggplot2::calc_element("plot.margin", theme)
     24.                             └─cli::cli_abort(...)
     25.                               └─rlang::abort(...)
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
* Number of recursive dependencies: 91

Run `revdepcheck::cloud_details(, "r2dii.plot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘r2dii.plot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_emission_intensity
    > ### Title: Create an emission intensity plot
    > ### Aliases: plot_emission_intensity
    > 
    > ### ** Examples
    > 
    > # plot with `qplot_emission_intensity()` parameters
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

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
       10.         └─ggplot2:::print.ggplot(x)
       11.           ├─ggplot2::ggplot_gtable(data)
       12.           └─ggplot2:::ggplot_gtable.ggplot_built(data)
       13.             └─ggplot2::calc_element("plot.margin", theme)
       14.               └─cli::cli_abort(...)
       15.                 └─rlang::abort(...)
      
      [ FAIL 1 | WARN 2 | SKIP 39 | PASS 124 ]
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

# rainette

<details>

* Version: 0.3.1.1
* GitHub: https://github.com/juba/rainette
* Source code: https://github.com/cran/rainette
* Date/Publication: 2023-03-28 16:50:02 UTC
* Number of recursive dependencies: 117

Run `revdepcheck::cloud_details(, "rainette")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(rainette)
      
      Attaching package: 'rainette'
      
      The following object is masked from 'package:stats':
      
    ...
      • plots/base-rainette2-plot-measure-frequency.svg
      • plots/base-rainette2-plot-measure-lr.svg
      • plots/base-rainette2-plot-with-complete-groups.svg
      • plots/base-rainette2-plot-with-free-scales.svg
      • plots/base-rainette2-plot-with-k-5.svg
      • plots/base-rainette2-plot-with-k-and-without-negative.svg
      • plots/base-rainette2-plot-with-k-n-terms-and-font-size.svg
      • plots/base-rainette2-plot.svg
      Error: Test failures
      Execution halted
    ```

# rassta

<details>

* Version: 1.0.5
* GitHub: https://github.com/bafuentes/rassta
* Source code: https://github.com/cran/rassta
* Date/Publication: 2022-08-30 22:30:02 UTC
* Number of recursive dependencies: 120

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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘signature.Rmd’
      ...
    > clim.var <- rast(vardir)
    
    > clim.cu <- rast(paste(d, "/climate.tif", sep = ""))
    
    > clim.difun <- select_functions(cu.rast = clim.cu, 
    +     var.rast = clim.var, mode = "auto")
    
    ...
      When sourcing ‘signature.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘classunits.Rmd’ using ‘UTF-8’... OK
      ‘modeling.Rmd’ using ‘UTF-8’... OK
      ‘sampling.Rmd’ using ‘UTF-8’... OK
      ‘signature.Rmd’ using ‘UTF-8’... failed
      ‘similarity.Rmd’ using ‘UTF-8’... OK
      ‘stratunits.Rmd’ using ‘UTF-8’... OK
    ```

# RAT

<details>

* Version: 0.3.1
* GitHub: NA
* Source code: https://github.com/cran/RAT
* Date/Publication: 2022-08-24 07:00:23 UTC
* Number of recursive dependencies: 32

Run `revdepcheck::cloud_details(, "RAT")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘RAT-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: i.map
    > ### Title: Map of international collaboration.
    > ### Aliases: i.map
    > 
    > ### ** Examples
    > 
    > data(biblio)
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

# Rcan

<details>

* Version: 1.3.82
* GitHub: https://github.com/timat35/Rcan
* Source code: https://github.com/cran/Rcan
* Date/Publication: 2020-05-19 11:40:07 UTC
* Number of recursive dependencies: 47

Run `revdepcheck::cloud_details(, "Rcan")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Rcan-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: csu_trendCohortPeriod
    > ### Title: csu_trendCohortPeriod
    > ### Aliases: csu_trendCohortPeriod
    > 
    > ### ** Examples
    > 
    > 
    ...
    ! Theme element `plot.margin` must have class <margin/rel>.
    Backtrace:
        ▆
     1. └─Rcan::csu_trendCohortPeriod(...)
     2.   ├─ggplot2::ggplot_gtable(gb_plot)
     3.   └─ggplot2:::ggplot_gtable.ggplot_built(gb_plot)
     4.     └─ggplot2::calc_element("plot.margin", theme)
     5.       └─cli::cli_abort(...)
     6.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 26334 marked UTF-8 strings
    ```

# redist

<details>

* Version: 4.2.0
* GitHub: https://github.com/alarm-redist/redist
* Source code: https://github.com/cran/redist
* Date/Publication: 2024-01-13 13:20:02 UTC
* Number of recursive dependencies: 132

Run `revdepcheck::cloud_details(, "redist")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘redist.Rmd’
      ...
    # ℹ 991 more rows
    
    > library(patchwork)
    
    > hist(plan_sum, max_dev) + hist(iowa_plans, comp) + 
    +     plot_layout(guides = "collect")
    
      When sourcing ‘redist.R’:
    Error: object is not a unit
    Execution halted
    
      ‘common_args.Rmd’ using ‘UTF-8’... OK
      ‘flip.Rmd’ using ‘UTF-8’... OK
      ‘map-preproc.Rmd’ using ‘UTF-8’... OK
      ‘redist.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘common_args.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 27.4Mb
      sub-directories of 1Mb or more:
        data   1.2Mb
        libs  23.4Mb
    ```

# Relectoral

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/Relectoral
* Date/Publication: 2020-06-14 14:20:02 UTC
* Number of recursive dependencies: 79

Run `revdepcheck::cloud_details(, "Relectoral")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Relectoral-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: mapa
    > ### Title: Graphs. Representation on maps. Choropleth map
    > ### Aliases: mapa
    > 
    > ### ** Examples
    > 
    > 
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Volatilidad.Rmd’
      ...
    Loading required package: readxl
    
    > require("readxl")
    
    > dat1 <- read_xlsx("../inst/data_raw/volatilidad/abril_19.xlsx", 
    +     col_names = TRUE)
    
      When sourcing ‘Volatilidad.R’:
    Error: `path` does not exist: ‘../inst/data_raw/volatilidad/abril_19.xlsx’
    Execution halted
    
      ‘Volatilidad.Rmd’ using ‘UTF-8’... failed
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rmarkdown’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# reliabilitydiag

<details>

* Version: 0.2.1
* GitHub: https://github.com/aijordan/reliabilitydiag
* Source code: https://github.com/cran/reliabilitydiag
* Date/Publication: 2022-06-29 00:20:06 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::cloud_details(, "reliabilitydiag")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘reliabilitydiag-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.reliabilitydiag
    > ### Title: Plotting reliability diagram objects
    > ### Aliases: plot.reliabilitydiag autoplot.reliabilitydiag
    > ###   autolayer.reliabilitydiag
    > 
    > ### ** Examples
    > 
    ...
      2. └─reliabilitydiag:::autoplot.reliabilitydiag(r["EMOS"], type = "discrimination")
      3.   ├─base::do.call(...)
      4.   └─ggExtra (local) `<fn>`(...)
      5.     └─ggplot2::ggplotGrob(scatP)
      6.       ├─ggplot2::ggplot_gtable(ggplot_build(x))
      7.       └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
      8.         └─ggplot2::calc_element("plot.margin", theme)
      9.           └─cli::cli_abort(...)
     10.             └─rlang::abort(...)
    Execution halted
    ```

# relliptical

<details>

* Version: 1.3.0
* GitHub: NA
* Source code: https://github.com/cran/relliptical
* Date/Publication: 2024-02-07 12:50:02 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::cloud_details(, "relliptical")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘relliptical-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: rtelliptical
    > ### Title: Sampling Random Numbers from Truncated Multivariate Elliptical
    > ###   Distributions
    > ### Aliases: rtelliptical
    > 
    > ### ** Examples
    > 
    ...
    Backtrace:
        ▆
     1. └─ggExtra::ggMarginal(f1, type = "histogram", fill = "grey")
     2.   └─ggplot2::ggplotGrob(scatP)
     3.     ├─ggplot2::ggplot_gtable(ggplot_build(x))
     4.     └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     5.       └─ggplot2::calc_element("plot.margin", theme)
     6.         └─cli::cli_abort(...)
     7.           └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        libs   6.4Mb
    ```

# Repliscope

<details>

* Version: 1.1.1
* GitHub: NA
* Source code: https://github.com/cran/Repliscope
* Date/Publication: 2022-09-13 07:20:02 UTC
* Number of recursive dependencies: 62

Run `revdepcheck::cloud_details(, "Repliscope")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Repliscope-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotBed
    > ### Title: A function to boxplot 'score' column of a BED dataframe, per
    > ###   unique chromosome name in the 'chrom' column. The resulting plot also
    > ###   highlights outliers based on the inter quartile range (IQR). The
    > ###   genome wide median is plotted as a pink line through the boxplots.
    > ### Aliases: plotBed
    > ### Keywords: BED bioinformatics boxplot genomics
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

# reportRmd

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/reportRmd
* Date/Publication: 2023-11-16 17:00:03 UTC
* Number of recursive dependencies: 108

Run `revdepcheck::cloud_details(, "reportRmd")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘reportRmd-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggkmcif
    > ### Title: Plot KM and CIF curves with ggplot
    > ### Aliases: ggkmcif
    > 
    > ### ** Examples
    > 
    > data("pembrolizumab")
    ...
    Backtrace:
        ▆
     1. └─reportRmd::ggkmcif(...)
     2.   └─ggplot2::ggplotGrob(data.table)
     3.     ├─ggplot2::ggplot_gtable(ggplot_build(x))
     4.     └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     5.       └─ggplot2::calc_element("plot.margin", theme)
     6.         └─cli::cli_abort(...)
     7.           └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘reportRmd.Rmd’
      ...
    > ctDNA <- clear_labels(ctDNA)
    
    > plotuv(data = pembrolizumab, response = "orr", covs = c("age", 
    +     "cohort", "pdl1", "change_ctdna_group"))
    Boxplots not shown for categories with fewer than 20 observations.
    Boxplots not shown for categories with fewer than 20 observations.
    
      When sourcing ‘reportRmd.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘reportRmd.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘reportRmd.Rmd’ using rmarkdown
    
    Quitting from lines 380-383 [unnamed-chunk-30] (reportRmd.Rmd)
    Error: processing vignette 'reportRmd.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘reportRmd.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘reportRmd.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# reReg

<details>

* Version: 1.4.6
* GitHub: https://github.com/stc04003/reReg
* Source code: https://github.com/cran/reReg
* Date/Publication: 2023-09-20 08:00:02 UTC
* Number of recursive dependencies: 63

Run `revdepcheck::cloud_details(, "reReg")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘reReg-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.Recur
    > ### Title: Produce Event Plot or Mean Cumulative Function Plot
    > ### Aliases: plot.Recur
    > ### Keywords: Plots
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

# reservr

<details>

* Version: 0.0.2
* GitHub: https://github.com/AshesITR/reservr
* Source code: https://github.com/cran/reservr
* Date/Publication: 2023-10-18 20:50:05 UTC
* Number of recursive dependencies: 142

Run `revdepcheck::cloud_details(, "reservr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘reservr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: dist_bdegp
    > ### Title: Construct a BDEGP-Family
    > ### Aliases: dist_bdegp
    > 
    > ### ** Examples
    > 
    > dist <- dist_bdegp(n = 1, m = 2, u = 10, epsilon = 3)
    ...
    +   theoretical = dist,
    +   empirical = dist_empirical(x),
    +   .x = seq(0, 20, length.out = 101),
    +   with_params = list(theoretical = params)
    + )
    Warning: Removed 9 rows containing missing values or values outside the scale range
    (`geom_line()`).
    Error in as.unit(value) : object is not coercible to a unit
    Calls: <Anonymous> ... assemble_guides -> guides_build -> [<- -> [<-.unit -> as.unit
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘distributions.Rmd’
      ...
    
    > attr(trunc_fit$logLik, "nobs")
    [1] 62
    
    > plot_distributions(true = norm, fit1 = norm, fit2 = norm2, 
    +     fit3 = dist_normal(3), .x = seq(-2, 7, 0.01), with_params = list(true = list(mean  .... [TRUNCATED] 
    
      When sourcing ‘distributions.R’:
    Error: object is not a unit
    Execution halted
    
      ‘distributions.Rmd’ using ‘UTF-8’... failed
      ‘tensorflow.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘distributions.Rmd’ using rmarkdown
    
    Quitting from lines 170-227 [unnamed-chunk-10] (distributions.Rmd)
    Error: processing vignette 'distributions.Rmd' failed with diagnostics:
    object is not a unit
    --- failed re-building ‘distributions.Rmd’
    
    --- re-building ‘tensorflow.Rmd’ using rmarkdown
    --- finished re-building ‘tensorflow.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘distributions.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 16.5Mb
      sub-directories of 1Mb or more:
        R      3.5Mb
        libs  12.4Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# restriktor

<details>

* Version: 0.5-60
* GitHub: NA
* Source code: https://github.com/cran/restriktor
* Date/Publication: 2024-05-24 11:00:03 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::cloud_details(, "restriktor")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘restriktor-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: evSyn
    > ### Title: GORIC(A) Evidence synthesis
    > ### Aliases: evSyn evsyn evSyn_est.list evSyn_ICweights.list
    > ###   evSyn_ICvalues.list evSyn_LL.list print.evSyn print.summary.evSyn
    > ###   summary.evSyn plot.evSyn
    > 
    > ### ** Examples
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
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
        6.       └─ggplot2:::print.ggplot(x)
        7.         ├─ggplot2::ggplot_gtable(data)
        8.         └─ggplot2:::ggplot_gtable.ggplot_built(data)
        9.           └─ggplot2::calc_element("plot.margin", theme)
       10.             └─cli::cli_abort(...)
       11.               └─rlang::abort(...)
      
      [ FAIL 1 | WARN 44 | SKIP 0 | PASS 138 ]
      Error: Test failures
      Execution halted
    ```

# rimu

<details>

* Version: 0.6
* GitHub: NA
* Source code: https://github.com/cran/rimu
* Date/Publication: 2022-10-06 04:50:02 UTC
* Number of recursive dependencies: 53

Run `revdepcheck::cloud_details(, "rimu")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rimu-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.mr
    > ### Title: Plot multiple-response objects
    > ### Aliases: plot.mr image.mr
    > 
    > ### ** Examples
    > 
    > data(rstudiosurvey)
    ...
      4.     ├─base::suppressMessages(...)
      5.     │ └─base::withCallingHandlers(...)
      6.     └─UpSetR:::Make_main_bar(...)
      7.       └─ggplot2::ggplotGrob(Main_bar_plot)
      8.         ├─ggplot2::ggplot_gtable(ggplot_build(x))
      9.         └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     10.           └─ggplot2::calc_element("plot.margin", theme)
     11.             └─cli::cli_abort(...)
     12.               └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘backyard-birds.Rmd’ using rmarkdown
    
    Quitting from lines 63-64 [unnamed-chunk-6] (backyard-birds.Rmd)
    Error: processing vignette 'backyard-birds.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘backyard-birds.Rmd’
    
    --- re-building ‘ethnicity.Rmd’ using rmarkdown
    ...
    --- failed re-building ‘ethnicity.Rmd’
    
    --- re-building ‘internals.Rmd’ using rmarkdown
    --- finished re-building ‘internals.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘backyard-birds.Rmd’ ‘ethnicity.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘backyard-birds.Rmd’
      ...
     Aythya collaris Xanthocephalus xanthocephalus Gracula religiosa
                1090                            80                 1
     Icterus parisorum Coccyzus erythropthalmus
                     8                        1
    
    > plot(bird_presence, nsets = 12)
    
    ...
    
    > plot(ethnicity, nsets = 6)
    
      When sourcing ‘ethnicity.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘backyard-birds.Rmd’ using ‘UTF-8’... failed
      ‘ethnicity.Rmd’ using ‘UTF-8’... failed
      ‘internals.Rmd’ using ‘UTF-8’... OK
    ```

# rKOMICS

<details>

* Version: 1.3
* GitHub: NA
* Source code: https://github.com/cran/rKOMICS
* Date/Publication: 2023-06-29 22:40:03 UTC
* Number of recursive dependencies: 137

Run `revdepcheck::cloud_details(, "rKOMICS")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rKOMICS-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: msc.pca
    > ### Title: Prinicple Component Analysis based on MSC
    > ### Aliases: msc.pca
    > 
    > ### ** Examples
    > 
    > data(matrices)
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

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 24.8Mb
      sub-directories of 1Mb or more:
        extdata  24.0Mb
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘example.Rnw’ using Sweave
    Loading required package: viridisLite
    Warning: Removed 95 rows containing non-finite outside the scale range
    (`stat_boxplot()`).
    Warning: Removed 89 rows containing non-finite outside the scale range
    (`stat_boxplot()`).
    Warning: Removed 149 rows containing non-finite outside the scale range
    (`stat_boxplot()`).
    Warning: Removed 286 rows containing non-finite outside the scale range
    ...
    l.5 \usepackage
                   {xcolor}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘example.Rnw’
    
    SUMMARY: processing the following file failed:
      ‘example.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# rmcorr

<details>

* Version: 0.6.0
* GitHub: https://github.com/lmarusich/rmcorr
* Source code: https://github.com/cran/rmcorr
* Date/Publication: 2023-08-09 16:40:10 UTC
* Number of recursive dependencies: 140

Run `revdepcheck::cloud_details(, "rmcorr")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘CI_fix.Rmd’ using rmarkdown
    --- finished re-building ‘CI_fix.Rmd’
    
    --- re-building ‘FAQ_and_limitations.Rmd’ using rmarkdown
    --- finished re-building ‘FAQ_and_limitations.Rmd’
    
    --- re-building ‘New_rmcorr_paper_analyses_figures.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘estimates_w_NaN.Rmd’
      ...
    
    > library(ggExtra)
    
    > load(file = "../man/data/ghosh_synth.rda")
    Warning in readChar(con, 5L, useBytes = TRUE) :
      cannot open compressed file '../man/data/ghosh_synth.rda', probable reason 'No such file or directory'
    
    ...
    Execution halted
    
      ‘CI_fix.Rmd’ using ‘UTF-8’... OK
      ‘FAQ_and_limitations.Rmd’ using ‘UTF-8’... OK
      ‘New_rmcorr_paper_analyses_figures.Rmd’ using ‘UTF-8’... OK
      ‘compcor.Rmd’ using ‘UTF-8’... OK
      ‘estimates_w_NaN.Rmd’ using ‘UTF-8’... failed
      ‘model_diag.Rmd’ using ‘UTF-8’... OK
      ‘repro_bootstrapping.Rmd’ using ‘UTF-8’... OK
      ‘rmcorr_mat.Rmd’ using ‘UTF-8’... OK
    ```

# RNAseqQC

<details>

* Version: 0.1.4
* GitHub: NA
* Source code: https://github.com/cran/RNAseqQC
* Date/Publication: 2022-06-15 09:50:06 UTC
* Number of recursive dependencies: 176

Run `revdepcheck::cloud_details(, "RNAseqQC")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘introduction.Rmd’
      ...
    +     show_plot = F)$plot + theme(legend.position = "bottom")
    
    > plot_loadings(pca_res, PC = 2, color_by = "gc_content")
    
    > plot_pca_scatters(vsd, n_PCs = 5, color_by = "treatment", 
    +     shape_by = "mutation")
    
      When sourcing 'introduction.R':
    Error: object is not coercible to a unit
    Execution halted
    
      ‘data.Rmd’ using ‘UTF-8’... OK
      ‘introduction.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘data.Rmd’ using rmarkdown
    --- finished re-building ‘data.Rmd’
    
    --- re-building ‘introduction.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.9Mb
      sub-directories of 1Mb or more:
        data   7.5Mb
        doc    2.2Mb
    ```

# roahd

<details>

* Version: 1.4.3
* GitHub: https://github.com/astamm/roahd
* Source code: https://github.com/cran/roahd
* Date/Publication: 2021-11-04 00:10:02 UTC
* Number of recursive dependencies: 88

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

# robustbase

<details>

* Version: 0.99-2
* GitHub: NA
* Source code: https://github.com/cran/robustbase
* Date/Publication: 2024-01-27 16:30:02 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::cloud_details(, "robustbase")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘lmrob_simulation.Rnw’
      ...
    +     d.x_psi(x, "lqq"), d.x_psi(x, "hampel"))
    
    > print(ggplot(tmp, aes(x, value, color = psi)) + geom_line(lwd = 1.25) + 
    +     ylab(quote(psi(x))) + scale_color_discrete(name = quote(psi ~ 
    +      .... [TRUNCATED] 
    
      When sourcing ‘lmrob_simulation.R’:
    Error: Theme element `plot.margin` must have class
    <margin/rel>.
    Execution halted
    
      ‘fastMcd-kmini.Rnw’ using ‘UTF-8’... OK
      ‘lmrob_simulation.Rnw’ using ‘UTF-8’... failed
      ‘psi_functions.Rnw’ using ‘UTF-8’... OK
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      'robustX', 'matrixStats', 'quantreg', 'Hmisc'
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘matrixStats’, ‘robustX’, ‘quantreg’, ‘Hmisc’
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘fastMcd-kmini.Rnw’ using Sweave
    Loading required package: robustbase
    Error: processing vignette 'fastMcd-kmini.Rnw' failed with diagnostics:
    Running 'texi2dvi' on 'fastMcd-kmini.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `mathtools.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    ...
    l.179   \RequirePackage{grfext}\relax
                                         ^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘psi_functions.Rnw’
    
    SUMMARY: processing the following files failed:
      ‘fastMcd-kmini.Rnw’ ‘lmrob_simulation.Rnw’ ‘psi_functions.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# romic

<details>

* Version: 1.1.3
* GitHub: NA
* Source code: https://github.com/cran/romic
* Date/Publication: 2023-09-21 05:40:02 UTC
* Number of recursive dependencies: 113

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

# rotations

<details>

* Version: 1.6.5
* GitHub: https://github.com/stanfill/rotationsC
* Source code: https://github.com/cran/rotations
* Date/Publication: 2023-12-08 00:10:02 UTC
* Number of recursive dependencies: 79

Run `revdepcheck::cloud_details(, "rotations")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rotations-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot
    > ### Title: Visualizing random rotations
    > ### Aliases: plot plot.SO3 plot.Q4
    > 
    > ### ** Examples
    > 
    > r <- rvmises(200, kappa = 1.0)
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘rotations-intro.Rnw’
      ...
    
    > region(x = Rs, method = "direct", type = "bootstrap", 
    +     estimator = "median", alp = 0.05, m = 300)
    [1] 0.238
    
    > plot(x = Rs, center = mean(Rs), show_estimates = "all")
    
      When sourcing ‘rotations-intro.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘rotations-intro.Rnw’ using ‘UTF-8’... failed
    ```

## In both

*   checking C++ specification ... NOTE
    ```
      Specified C++11: please drop specification unless essential
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 19.1Mb
      sub-directories of 1Mb or more:
        R      3.5Mb
        data   7.0Mb
        libs   8.0Mb
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘rotations-intro.Rnw’ using knitr
    
    Quitting from lines 324-336 [ex7] (rotations-intro.Rnw)
    Error: processing vignette 'rotations-intro.Rnw' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘rotations-intro.Rnw’
    
    SUMMARY: processing the following file failed:
      ‘rotations-intro.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# rreg

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/rreg
* Date/Publication: 2018-03-22 14:11:31 UTC
* Number of recursive dependencies: 50

Run `revdepcheck::cloud_details(, "rreg")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rreg-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: regbar
    > ### Title: Barplot with explicit data comparison
    > ### Aliases: regbar
    > 
    > ### ** Examples
    > 
    > # basic usage
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

# rSDI

<details>

* Version: 0.2.1
* GitHub: https://github.com/ehengirmen/rSDI
* Source code: https://github.com/cran/rSDI
* Date/Publication: 2024-05-30 07:40:02 UTC
* Number of recursive dependencies: 93

Run `revdepcheck::cloud_details(, "rSDI")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘rSDI.Rmd’
      ...
    |A  |  0|  3|
    |B  |  4|  0|
    |C  |  0|  0|
    |D  |  4|  3|
    
    > p
    
      When sourcing ‘rSDI.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘rSDI.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘rSDI.Rmd’ using rmarkdown
    
    Quitting from lines 82-83 [unnamed-chunk-5] (rSDI.Rmd)
    Error: processing vignette 'rSDI.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘rSDI.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘rSDI.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# SangerTools

<details>

* Version: 1.0.2
* GitHub: NA
* Source code: https://github.com/cran/SangerTools
* Date/Publication: 2022-02-20 13:10:02 UTC
* Number of recursive dependencies: 104

Run `revdepcheck::cloud_details(, "SangerTools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘SangerTools-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: categorical_col_chart
    > ### Title: Plot Counts of Categorical Variables
    > ### Aliases: categorical_col_chart
    > 
    > ### ** Examples
    > 
    > library(SangerTools)
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘SangerTools_Vignette.Rmd’
      ...
    </table>
    > diabetes_df <- health_data %>% dplyr::filter(Diabetes == 
    +     1)
    
    > SangerTools::categorical_col_chart(df = diabetes_df, 
    +     grouping_var = Ethnicity) + scale_fill_sanger() + labs(title = "Diabetic Patients by Eth ..." ... [TRUNCATED] 
    
      When sourcing ‘SangerTools_Vignette.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘SangerTools_Vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘SangerTools_Vignette.Rmd’ using rmarkdown
    
    Quitting from lines 119-140 [categorical_column_chart] (SangerTools_Vignette.Rmd)
    Error: processing vignette 'SangerTools_Vignette.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘SangerTools_Vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘SangerTools_Vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# santaR

<details>

* Version: 1.2.4
* GitHub: https://github.com/adwolfer/santaR
* Source code: https://github.com/cran/santaR
* Date/Publication: 2024-03-07 00:30:02 UTC
* Number of recursive dependencies: 93

Run `revdepcheck::cloud_details(, "santaR")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘advanced-command-line-functions.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘getting-started.Rmd’
      ...
    
    > knitr::include_graphics("../man/figures/santaR-approach.jpg")
    
      When sourcing ‘getting-started.R’:
    Error: Cannot find the file(s): "../man/figures/santaR-approach.jpg"
    Execution halted
    when running code in ‘selecting-optimal-df.Rmd’
    ...
    Execution halted
    
      ‘advanced-command-line-functions.Rmd’ using ‘UTF-8’... OK
      ‘automated-command-line.Rmd’ using ‘UTF-8’... OK
      ‘getting-started.Rmd’ using ‘UTF-8’... failed
      ‘plotting-options.Rmd’ using ‘UTF-8’... OK
      ‘prepare-input-data.Rmd’ using ‘UTF-8’... OK
      ‘selecting-optimal-df.Rmd’ using ‘UTF-8’... failed
      ‘theoretical-background.Rmd’ using ‘UTF-8’... failed
      ‘santaR-GUI.pdf.asis’ using ‘UTF-8’... OK
    ```

# scoringutils

<details>

* Version: 1.2.2
* GitHub: https://github.com/epiforecasts/scoringutils
* Source code: https://github.com/cran/scoringutils
* Date/Publication: 2023-11-29 15:50:10 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::cloud_details(, "scoringutils")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘scoringutils-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_predictions
    > ### Title: Plot Predictions vs True Values
    > ### Aliases: plot_predictions
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
    +     by = c("target_type", "location"),
    +     range = c(0, 50, 90, 95)
    +   ) +
    +   facet_wrap(~ location + target_type, scales = "free_y") +
    +   aes(fill = model, color = model)
    Error in use_defaults(..., self = self) : 
      unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, 
        NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, l
    Calls: <Anonymous> ... <Anonymous> -> <Anonymous> -> compute_geom_2 -> <Anonymous>
    Execution halted
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘metric-details.Rmd’ using rmarkdown
    --- finished re-building ‘metric-details.Rmd’
    
    --- re-building ‘scoring-forecasts-directly.Rmd’ using rmarkdown
    --- finished re-building ‘scoring-forecasts-directly.Rmd’
    
    --- re-building ‘scoringutils.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘scoringutils.Rmd’
      ...
    The following messages were produced when checking inputs:
    1.  144 values for `prediction` are NA in the data provided and the corresponding rows were removed. This may indicate a problem if unexpected.
    
    > example_quantile %>% make_NA(what = "truth", target_end_date >= 
    +     "2021-07-15", target_end_date < "2021-05-22") %>% make_NA(what = "forecast",  .... [TRUNCATED] 
    
      When sourcing ‘scoringutils.R’:
    Error: unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, 
        NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, lis
    Execution halted
    
      ‘metric-details.Rmd’ using ‘UTF-8’... OK
      ‘scoring-forecasts-directly.Rmd’ using ‘UTF-8’... OK
      ‘scoringutils.Rmd’ using ‘UTF-8’... failed
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

# SDLfilter

<details>

* Version: 2.3.3
* GitHub: https://github.com/TakahiroShimada/SDLfilter
* Source code: https://github.com/cran/SDLfilter
* Date/Publication: 2023-11-10 00:00:11 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "SDLfilter")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘SDLfilter-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ddfilter
    > ### Title: Filter locations using a data driven filter
    > ### Aliases: ddfilter
    > 
    > ### ** Examples
    > 
    > #### Load data sets
    ...
      2.   ├─base::do.call(arrangeGrob, c(list(grobs = groups[[g]]), params))
      3.   └─gridExtra (local) `<fn>`(grobs = `<list>`, top = "page 1 of 1", layout_matrix = `<int[,2]>`)
      4.     └─base::lapply(grobs[toconv], ggplot2::ggplotGrob)
      5.       └─ggplot2 (local) FUN(X[[i]], ...)
      6.         ├─ggplot2::ggplot_gtable(ggplot_build(x))
      7.         └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
      8.           └─ggplot2::calc_element("plot.margin", theme)
      9.             └─cli::cli_abort(...)
     10.               └─rlang::abort(...)
    Execution halted
    ```

# see

<details>

* Version: 0.8.4
* GitHub: https://github.com/easystats/see
* Source code: https://github.com/cran/see
* Date/Publication: 2024-04-29 04:40:03 UTC
* Number of recursive dependencies: 233

Run `revdepcheck::cloud_details(, "see")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘see-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_binomdensity
    > ### Title: Add dot-densities for binary 'y' variables
    > ### Aliases: geom_binomdensity
    > 
    > ### ** Examples
    > 
    > ## Don't show: 
    ...
     14. │         └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     15. │           └─l$compute_geom_2(d, theme = plot$theme)
     16. │             └─ggplot2 (local) compute_geom_2(..., self = self)
     17. │               └─self$geom$use_defaults(...)
     18. └─base::.handleSimpleError(...)
     19.   └─rlang (local) h(simpleError(msg, call))
     20.     └─handlers[[1L]](cnd)
     21.       └─cli::cli_abort(...)
     22.         └─rlang::abort(...)
    Execution halted
    ```

# sentimentr

<details>

* Version: 2.9.0
* GitHub: https://github.com/trinker/sentimentr
* Source code: https://github.com/cran/sentimentr
* Date/Publication: 2021-10-12 08:30:02 UTC
* Number of recursive dependencies: 66

Run `revdepcheck::cloud_details(, "sentimentr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘sentimentr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: sentiment
    > ### Title: Polarity Score (Sentiment Analysis)
    > ### Aliases: sentiment
    > 
    > ### ** Examples
    > 
    > mytext <- c(
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        data   5.0Mb
    ```

# sentometrics

<details>

* Version: 1.0.0
* GitHub: https://github.com/SentometricsResearch/sentometrics
* Source code: https://github.com/cran/sentometrics
* Date/Publication: 2021-08-18 07:50:02 UTC
* Number of recursive dependencies: 127

Run `revdepcheck::cloud_details(, "sentometrics")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘sentometrics-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.sento_measures
    > ### Title: Plot sentiment measures
    > ### Aliases: plot.sento_measures
    > 
    > ### ** Examples
    > 
    > # construct a sento_measures object to start with
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking C++ specification ... NOTE
    ```
      Specified C++11: please drop specification unless essential
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  9.7Mb
      sub-directories of 1Mb or more:
        data   2.3Mb
        libs   6.2Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4436 marked UTF-8 strings
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# sglg

<details>

* Version: 0.2.2
* GitHub: NA
* Source code: https://github.com/cran/sglg
* Date/Publication: 2022-09-04 03:50:01 UTC
* Number of recursive dependencies: 96

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

# SHAPforxgboost

<details>

* Version: 0.1.3
* GitHub: https://github.com/liuyanguu/SHAPforxgboost
* Source code: https://github.com/cran/SHAPforxgboost
* Date/Publication: 2023-05-29 17:20:07 UTC
* Number of recursive dependencies: 120

Run `revdepcheck::cloud_details(, "SHAPforxgboost")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘SHAPforxgboost-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: scatter.plot.diagonal
    > ### Title: Make customized scatter plot with diagonal line and R2 printed.
    > ### Aliases: scatter.plot.diagonal
    > 
    > ### ** Examples
    > 
    > scatter.plot.diagonal(data = iris, x = "Sepal.Length", y = "Petal.Length")
    ...
        ▆
     1. └─SHAPforxgboost::scatter.plot.diagonal(...)
     2.   └─ggExtra::ggMarginal(...)
     3.     └─ggplot2::ggplotGrob(scatP)
     4.       ├─ggplot2::ggplot_gtable(ggplot_build(x))
     5.       └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     6.         └─ggplot2::calc_element("plot.margin", theme)
     7.           └─cli::cli_abort(...)
     8.             └─rlang::abort(...)
    Execution halted
    ```

# shazam

<details>

* Version: 1.2.0
* GitHub: NA
* Source code: https://github.com/cran/shazam
* Date/Publication: 2023-10-02 18:50:02 UTC
* Number of recursive dependencies: 127

Run `revdepcheck::cloud_details(, "shazam")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘shazam-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotMutability
    > ### Title: Plot mutability probabilities
    > ### Aliases: plotMutability
    > 
    > ### ** Examples
    > 
    > # Plot one nucleotide in circular style
    ...
      3.   └─alakazam (local) `<fn>`(C = `<gg>`, ncol = 1L)
      4.     ├─base::plot(p[[1]])
      5.     ├─base::plot(p[[1]])
      6.     └─ggplot2:::plot.ggplot(p[[1]])
      7.       ├─ggplot2::ggplot_gtable(data)
      8.       └─ggplot2:::ggplot_gtable.ggplot_built(data)
      9.         └─ggplot2::calc_element("plot.margin", theme)
     10.           └─cli::cli_abort(...)
     11.             └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Targeting-Vignette.Rmd’
      ...
    +     sequenceColumn = "clonal_sequence", germlineColumn = "clonal_germline", 
    +     vCallColu .... [TRUNCATED] 
    Warning in createMutabilityMatrix(db, sub_mat, model = model, sequenceColumn = sequenceColumn,  :
      Insufficient number of mutations to infer some 5-mers. Filled with 0. 
    
    > plotMutability(model, nucleotides = "A", style = "hedgehog")
    
      When sourcing ‘Targeting-Vignette.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘Baseline-Vignette.Rmd’ using ‘UTF-8’... OK
      ‘DistToNearest-Vignette.Rmd’ using ‘UTF-8’... OK
      ‘Mutation-Vignette.Rmd’ using ‘UTF-8’... OK
      ‘Shmulate-Vignette.Rmd’ using ‘UTF-8’... OK
      ‘Targeting-Vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Baseline-Vignette.Rmd’ using rmarkdown
    --- finished re-building ‘Baseline-Vignette.Rmd’
    
    --- re-building ‘DistToNearest-Vignette.Rmd’ using rmarkdown
    --- finished re-building ‘DistToNearest-Vignette.Rmd’
    
    --- re-building ‘Mutation-Vignette.Rmd’ using rmarkdown
    --- finished re-building ‘Mutation-Vignette.Rmd’
    ...
    Quitting from lines 167-170 [unnamed-chunk-8] (Targeting-Vignette.Rmd)
    Error: processing vignette 'Targeting-Vignette.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘Targeting-Vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Targeting-Vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# simulariatools

<details>

* Version: 2.5.1
* GitHub: https://github.com/Simularia/simulariatools
* Source code: https://github.com/cran/simulariatools
* Date/Publication: 2023-11-08 14:10:02 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "simulariatools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘simulariatools-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotAvgTemp
    > ### Title: Plot average temperature
    > ### Aliases: plotAvgTemp
    > 
    > ### ** Examples
    > 
    > # Plot histogram with monthly averages together with maxima and minima 
    ...
     1. └─simulariatools::plotAvgTemp(stMeteo)
     2.   └─simulariatools (local) mmplot(v, data_table)
     3.     ├─base::print(b, vp = subplot(2, 1))
     4.     └─ggplot2:::print.ggplot(b, vp = subplot(2, 1))
     5.       ├─ggplot2::ggplot_gtable(data)
     6.       └─ggplot2:::ggplot_gtable.ggplot_built(data)
     7.         └─ggplot2::calc_element("plot.margin", theme)
     8.           └─cli::cli_abort(...)
     9.             └─rlang::abort(...)
    Execution halted
    ```

# sjPlot

<details>

* Version: 2.8.16
* GitHub: https://github.com/strengejacke/sjPlot
* Source code: https://github.com/cran/sjPlot
* Date/Publication: 2024-05-13 17:50:02 UTC
* Number of recursive dependencies: 187

Run `revdepcheck::cloud_details(, "sjPlot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘sjPlot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_frq
    > ### Title: Plot frequencies of variables
    > ### Aliases: plot_frq
    > 
    > ### ** Examples
    > 
    > library(sjlabelled)
    ...
      4.   └─gridExtra (local) `<fn>`(`<gg>`, `<gg>`, `<gg>`, `<gg>`, nrow = 2, ncol = 2)
      5.     └─gridExtra::arrangeGrob(...)
      6.       └─base::lapply(grobs[toconv], ggplot2::ggplotGrob)
      7.         └─ggplot2 (local) FUN(X[[i]], ...)
      8.           ├─ggplot2::ggplot_gtable(ggplot_build(x))
      9.           └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     10.             └─ggplot2::calc_element("plot.margin", theme)
     11.               └─cli::cli_abort(...)
     12.                 └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘custplot.Rmd’
      ...
    
    
    > data(efc)
    
    > set_theme(geom.outline.color = "antiquewhite4", geom.outline.size = 1, 
    +     geom.label.size = 2, geom.label.color = "grey50", title.color = "red", .... [TRUNCATED] 
    
    ...
      ‘plot_interactions.Rmd’ using ‘UTF-8’... OK
      ‘plot_likert_scales.Rmd’ using ‘UTF-8’... OK
      ‘plot_marginal_effects.Rmd’ using ‘UTF-8’... OK
      ‘plot_model_estimates.Rmd’ using ‘UTF-8’... OK
      ‘sjtitemanalysis.Rmd’ using ‘UTF-8’... OK
      ‘tab_bayes.Rmd’ using ‘UTF-8’... OK
      ‘tab_mixed.Rmd’ using ‘UTF-8’... OK
      ‘tab_model_estimates.Rmd’ using ‘UTF-8’... OK
      ‘tab_model_robust.Rmd’ using ‘UTF-8’... OK
      ‘table_css.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘blackwhitefigures.Rmd’ using rmarkdown
    ```

# SleepCycles

<details>

* Version: 1.1.4
* GitHub: NA
* Source code: https://github.com/cran/SleepCycles
* Date/Publication: 2021-09-27 13:50:10 UTC
* Number of recursive dependencies: 56

Run `revdepcheck::cloud_details(, "SleepCycles")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘SleepCycles-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: SleepCycles
    > ### Title: Sleep Cycle Detection
    > ### Aliases: SleepCycles
    > 
    > ### ** Examples
    > 
    > data(sleepstages)
    ...
      4.       ├─grid::grid.draw(plot)
      5.       └─ggplot2:::grid.draw.ggplot(plot)
      6.         ├─base::print(x)
      7.         └─ggplot2:::print.ggplot(x)
      8.           ├─ggplot2::ggplot_gtable(data)
      9.           └─ggplot2:::ggplot_gtable.ggplot_built(data)
     10.             └─ggplot2::calc_element("plot.margin", theme)
     11.               └─cli::cli_abort(...)
     12.                 └─rlang::abort(...)
    Execution halted
    ```

# smallsets

<details>

* Version: 2.0.0
* GitHub: https://github.com/lydialucchesi/smallsets
* Source code: https://github.com/cran/smallsets
* Date/Publication: 2023-12-05 00:00:02 UTC
* Number of recursive dependencies: 107

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
    Error in as.unit(value) : object is not coercible to a unit
    Calls: <Anonymous> ... assemble_guides -> guides_build -> [<- -> [<-.unit -> as.unit
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘smallsets.Rmd’
      ...
    > library(smallsets)
    
    > set.seed(145)
    
    > Smallset_Timeline(data = s_data, code = system.file("s_data_preprocess.R", 
    +     package = "smallsets"))
    
      When sourcing ‘smallsets.R’:
    Error: object is not coercible to a unit
    Execution halted
    
      ‘smallsets.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘smallsets.Rmd’ using rmarkdown
    
    Quitting from lines 36-42 [timeline1] (smallsets.Rmd)
    Error: processing vignette 'smallsets.Rmd' failed with diagnostics:
    object is not coercible to a unit
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

# smdi

<details>

* Version: 0.2.2
* GitHub: NA
* Source code: https://github.com/cran/smdi
* Date/Publication: 2023-07-17 14:20:02 UTC
* Number of recursive dependencies: 188

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
    > usethis::use_data(smdi_data_complete, overwrite = TRUE)
    Warning in path_file(base_path) :
      restarting interrupted promise evaluation
    
      When sourcing ‘a_data_generation.R’:
    Error: Failed to evaluate glue component {ui_value(project_name())}
    Caused by error:
    ...
    
      When sourcing ‘c_multivariate_missingness.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘a_data_generation.Rmd’ using ‘UTF-8’... failed
      ‘b_routine_diagnostics.Rmd’ using ‘UTF-8’... failed
      ‘c_multivariate_missingness.Rmd’ using ‘UTF-8’... failed
      ‘d_narfcs_sensitivity_analysis.Rmd’ using ‘UTF-8’... OK
      ‘smdi.Rmd’ using ‘UTF-8’... OK
    ```

# soc.ca

<details>

* Version: 0.8.0
* GitHub: https://github.com/Rsoc/soc.ca
* Source code: https://github.com/cran/soc.ca
* Date/Publication: 2021-09-02 22:50:02 UTC
* Number of recursive dependencies: 140

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
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
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

# spbal

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/spbal
* Date/Publication: 2024-05-17 16:00:02 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::cloud_details(, "spbal")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘spbal.Rmd’
      ...
      st_point_on_surface may not give correct results for longitude/latitude data
    Warning in st_point_on_surface.sfc(sf::st_zm(x)) :
      st_point_on_surface may not give correct results for longitude/latitude data
    
      When sourcing ‘spbal.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `$<-.data.frame`:
    ! replacement has 1 row, data has 0
    Execution halted
    
      ‘spbal.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘spbal.Rmd’ using rmarkdown
    
    Quitting from lines 159-187 [BASex1c] (spbal.Rmd)
    Error: processing vignette 'spbal.Rmd' failed with diagnostics:
    Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `$<-.data.frame`:
    ! replacement has 1 row, data has 0
    --- failed re-building ‘spbal.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘spbal.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# speccurvieR

<details>

* Version: 0.3.0
* GitHub: https://github.com/zaynesember/speccurvieR
* Source code: https://github.com/cran/speccurvieR
* Date/Publication: 2024-01-24 19:40:02 UTC
* Number of recursive dependencies: 46

Run `revdepcheck::cloud_details(, "speccurvieR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘speccurvieR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotCurve
    > ### Title: Plots a specification curve.
    > ### Aliases: plotCurve
    > 
    > ### ** Examples
    > 
    > plotCurve(sca_data = sca(y="Salnty", x="T_degC", c("ChlorA", "O2Sat"),
    ...
     1. └─speccurvieR::plotCurve(...)
     2.   ├─grid::grid.draw(rbind(ggplotGrob(sc1), ggplotGrob(sc2)))
     3.   ├─base::rbind(ggplotGrob(sc1), ggplotGrob(sc2))
     4.   └─ggplot2::ggplotGrob(sc1)
     5.     ├─ggplot2::ggplot_gtable(ggplot_build(x))
     6.     └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     7.       └─ggplot2::calc_element("plot.margin", theme)
     8.         └─cli::cli_abort(...)
     9.           └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

# spinifex

<details>

* Version: 0.3.7.0
* GitHub: https://github.com/nspyrison/spinifex
* Source code: https://github.com/cran/spinifex
* Date/Publication: 2024-01-29 14:40:02 UTC
* Number of recursive dependencies: 163

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
      spinifex --- version 0.3.7.0
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

# spotoroo

<details>

* Version: 0.1.4
* GitHub: https://github.com/TengMCing/spotoroo
* Source code: https://github.com/cran/spotoroo
* Date/Publication: 2023-08-21 05:50:02 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::cloud_details(, "spotoroo")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(spotoroo)
      > 
      > test_check("spotoroo")
      
      -------------------------------- SPOTOROO 0.1.4 --------------------------------
      
    ...
      i Actually got a <rlang_error> with text:
        Theme element `plot.margin` must have class <margin/rel>.
      ── Failure ('test-plot_spotoroo.R:64:3'): plot_spotoroo() works ────────────────
      Expected `plot_spotoroo(result, type = "timeline")` to run without any errors.
      i Actually got a <rlang_error> with text:
        Theme element `plot.margin` must have class <margin/rel>.
      
      [ FAIL 2 | WARN 5 | SKIP 0 | PASS 65 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Clustering-hot-spots.Rmd’
      ...
    
    ────────────────────────────────────────────────────────────────────────────────
    
    > plot_spotoroo(result, type = "def")
    
    > plot_spotoroo(result, type = "timeline")
    
      When sourcing ‘Clustering-hot-spots.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘Clustering-hot-spots.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Clustering-hot-spots.Rmd’ using rmarkdown
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

# SqueakR

<details>

* Version: 1.3.0
* GitHub: https://github.com/osimon81/SqueakR
* Source code: https://github.com/cran/SqueakR
* Date/Publication: 2022-06-28 09:20:04 UTC
* Number of recursive dependencies: 141

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

# stabm

<details>

* Version: 1.2.2
* GitHub: https://github.com/bommert/stabm
* Source code: https://github.com/cran/stabm
* Date/Publication: 2023-04-04 13:20:02 UTC
* Number of recursive dependencies: 71

Run `revdepcheck::cloud_details(, "stabm")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘stabm-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotFeatures
    > ### Title: Plot Selected Features
    > ### Aliases: plotFeatures
    > 
    > ### ** Examples
    > 
    > feats = list(1:3, 1:4, 1:5)
    ...
      5.       └─cowplot:::as_gtable.default(plot)
      6.         ├─cowplot::as_grob(plot)
      7.         └─cowplot:::as_grob.ggplot(plot)
      8.           └─ggplot2::ggplotGrob(plot)
      9.             ├─ggplot2::ggplot_gtable(ggplot_build(x))
     10.             └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     11.               └─ggplot2::calc_element("plot.margin", theme)
     12.                 └─cli::cli_abort(...)
     13.                   └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(stabm)
      > 
      > test_check("stabm")
      [ FAIL 10 | WARN 3 | SKIP 0 | PASS 290 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
       10.           └─ggplot2::ggplotGrob(plot)
       11.             ├─ggplot2::ggplot_gtable(ggplot_build(x))
       12.             └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
       13.               └─ggplot2::calc_element("plot.margin", theme)
       14.                 └─cli::cli_abort(...)
       15.                   └─rlang::abort(...)
      
      [ FAIL 10 | WARN 3 | SKIP 0 | PASS 290 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘stabm.Rmd’
      ...
    [1] 0.4353893
    
    > plotFeatures(feats)
    Loading required namespace: ggplot2
    Loading required namespace: cowplot
    Loading required namespace: ggdendro
    
      When sourcing ‘stabm.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘stabm.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘stabm.Rmd’ using rmarkdown
    
    Quitting from lines 65-66 [unnamed-chunk-5] (stabm.Rmd)
    Error: processing vignette 'stabm.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘stabm.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘stabm.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# starvz

<details>

* Version: 0.8.0
* GitHub: https://github.com/schnorr/starvz
* Source code: https://github.com/cran/starvz
* Date/Publication: 2024-02-23 23:50:02 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "starvz")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘starvz-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: panel_gpubandwidth
    > ### Title: Create a line chart panel with GPU bandwidth
    > ### Aliases: panel_gpubandwidth
    > 
    > ### ** Examples
    > 
    > panel_gpubandwidth(data = starvz_sample_lu)
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

# statgenMPP

<details>

* Version: 1.0.2
* GitHub: NA
* Source code: https://github.com/cran/statgenMPP
* Date/Publication: 2022-12-02 22:00:02 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::cloud_details(, "statgenMPP")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘tinytest.R’
    Running the tests in ‘tests/tinytest.R’ failed.
    Complete output:
      > 
      > if ( requireNamespace("tinytest", quietly=TRUE) ){
      +   tinytest::test_package("statgenMPP")
      + }
      Loading required package: statgenGWAS
      
      test_calcIBDmpp.R.............    0 tests    
    ...
        7.             ├─base::plot(ABC_MQM, plotType = "QTLProfileExt")
        8.             └─statgenMPP:::plot.QTLMPP(ABC_MQM, plotType = "QTLProfileExt")
        9.               └─ggplot2::ggplotGrob(p1)
       10.                 ├─ggplot2::ggplot_gtable(ggplot_build(x))
       11.                 └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
       12.                   └─ggplot2::calc_element("plot.margin", theme)
       13.                     └─cli::cli_abort(...)
       14.                       └─rlang::abort(...)
      There were 50 or more warnings (use warnings() to see the first 50)
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘QTLMapping_in_MultiParentPopulations.Rmd’
      ...
    
    > plot(ABCMQM, plotType = "QTLProfile")
    
    > plot(ABCMQM, plotType = "parEffs")
    
    > plot(ABCMQM, plotType = "QTLProfileExt")
    
      When sourcing ‘QTLMapping_in_MultiParentPopulations.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘QTLMapping_in_MultiParentPopulations.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘QTLMapping_in_MultiParentPopulations.Rmd’ using rmarkdown
    ```

# statVisual

<details>

* Version: 1.2.1
* GitHub: NA
* Source code: https://github.com/cran/statVisual
* Date/Publication: 2020-02-20 19:30:02 UTC
* Number of recursive dependencies: 193

Run `revdepcheck::cloud_details(, "statVisual")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘statVisual-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: PCA_score
    > ### Title: Scatter Plot of 2 Specified Principal Components
    > ### Aliases: PCA_score
    > ### Keywords: method
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
    when running code in ‘statVisual.Rmd’
      ...
    
    > factoextra::fviz_eig(pca.obj, addlabels = TRUE)
    
      When sourcing ‘statVisual.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `check_aesthetics()`:
    ! Aesthetics must be either length 1 or the same as the data (6).
    ✖ Fix the following mappings: `width`.
    Execution halted
    
      ‘statVisual.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘statVisual.Rmd’ using rmarkdown
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘gbm’ ‘ggfortify’ ‘tibble’ ‘tidyverse’
      All declared Imports should be used.
    ```

# superheat

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/superheat
* Date/Publication: 2017-02-04 23:35:29
* Number of recursive dependencies: 69

Run `revdepcheck::cloud_details(, "superheat")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘superheat-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: superheat
    > ### Title: Generate supervised heatmaps.
    > ### Aliases: superheat
    > 
    > ### ** Examples
    > 
    > # plot a heatmap of the numerical iris variables
    ...
      6.     ├─gtable::gtable_filter(...)
      7.     │ └─base::grepl(pattern, .subset2(x$layout, "name"), fixed = fixed)
      8.     │   └─base::is.factor(x)
      9.     └─ggplot2::ggplotGrob(gg.right)
     10.       ├─ggplot2::ggplot_gtable(ggplot_build(x))
     11.       └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     12.         └─ggplot2::calc_element("plot.margin", theme)
     13.           └─cli::cli_abort(...)
     14.             └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(superheat)
      > 
      > test_check("superheat")
      [ FAIL 58 | WARN 256 | SKIP 0 | PASS 0 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
        9.     └─ggplot2::ggplotGrob(gg.top)
       10.       ├─ggplot2::ggplot_gtable(ggplot_build(x))
       11.       └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
       12.         └─ggplot2::calc_element("plot.margin", theme)
       13.           └─cli::cli_abort(...)
       14.             └─rlang::abort(...)
      
      [ FAIL 58 | WARN 256 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# surveyexplorer

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/surveyexplorer
* Date/Publication: 2023-12-21 16:40:02 UTC
* Number of recursive dependencies: 87

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
      9.             └─ggplot2:::scale_apply(layer_data, x_vars, "map", SCALE_X, self$panel_scales_x)
     10.               └─base::lapply(...)
     11.                 └─ggplot2 (local) FUN(X[[i]], ...)
     12.                   └─base::lapply(...)
     13.                     └─ggplot2 (local) FUN(X[[i]], ...)
     14.                       └─scales[[i]][[method]](data[[var]][scale_index[[i]]])
     15.                         └─ggplot2 (local) map(..., self = self)
     16.                           └─cli::cli_abort(...)
     17.                             └─rlang::abort(...)
    Execution halted
    ```

# survivalAnalysis

<details>

* Version: 0.3.0
* GitHub: NA
* Source code: https://github.com/cran/survivalAnalysis
* Date/Publication: 2022-02-11 14:00:02 UTC
* Number of recursive dependencies: 159

Run `revdepcheck::cloud_details(, "survivalAnalysis")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘survivalAnalysis-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: forest_plot
    > ### Title: Forest plots for survival analysis.
    > ### Aliases: forest_plot forest_plot.df
    > 
    > ### ** Examples
    > 
    > library(magrittr)
    ...
     10.           └─cowplot:::as_gtable.default(x)
     11.             ├─cowplot::as_grob(plot)
     12.             └─cowplot:::as_grob.ggplot(plot)
     13.               └─ggplot2::ggplotGrob(plot)
     14.                 ├─ggplot2::ggplot_gtable(ggplot_build(x))
     15.                 └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     16.                   └─ggplot2::calc_element("plot.margin", theme)
     17.                     └─cli::cli_abort(...)
     18.                       └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘multivariate.Rmd’ using rmarkdown
    
    Quitting from lines 88-89 [unnamed-chunk-6] (multivariate.Rmd)
    Error: processing vignette 'multivariate.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘multivariate.Rmd’
    
    --- re-building ‘univariate.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘multivariate.Rmd’
      ...
    Warning in strwidth(., family = ggtheme$text$family, units = "in") :
      conversion failure on '(0.98–1.00)' in 'mbcsToSbcs': dot substituted for <e2>
    Warning in strwidth(., family = ggtheme$text$family, units = "in") :
      conversion failure on '(0.98–1.00)' in 'mbcsToSbcs': dot substituted for <80>
    Warning in strwidth(., family = ggtheme$text$family, units = "in") :
      conversion failure on '(0.98–1.00)' in 'mbcsToSbcs': dot substituted for <93>
    
    ...
      font family 'Arial' not found in PostScript font database
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family 'Arial' not found in PostScript font database
    
      When sourcing ‘univariate.R’:
    Error: invalid font type
    Execution halted
    
      ‘multivariate.Rmd’ using ‘UTF-8’... failed
      ‘univariate.Rmd’ using ‘UTF-8’... failed
    ```

# Sysrecon

<details>

* Version: 0.1.3
* GitHub: NA
* Source code: https://github.com/cran/Sysrecon
* Date/Publication: 2023-02-20 08:50:02 UTC
* Number of recursive dependencies: 61

Run `revdepcheck::cloud_details(, "Sysrecon")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Sysrecon-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Sysrecon
    > ### Title: Sysrecon
    > ### Aliases: Sysrecon
    > 
    > ### ** Examples
    > 
    > 
    ...
      no non-missing arguments to min; returning Inf
    Warning in min(freq[grepl(i, allwords, ignore.case = T)]) :
      no non-missing arguments to min; returning Inf
    Warning in min(freq[grepl(i, allwords, ignore.case = T)]) :
      no non-missing arguments to min; returning Inf
    Warning in min(freq[grepl(i, allwords, ignore.case = T)]) :
      no non-missing arguments to min; returning Inf
    Error in as.unit(value) : object is not coercible to a unit
    Calls: Sysrecon ... assemble_guides -> guides_build -> [<- -> [<-.unit -> as.unit
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 38 marked UTF-8 strings
    ```

# tabledown

<details>

* Version: 1.0.0
* GitHub: https://github.com/masiraji/tabledown
* Source code: https://github.com/cran/tabledown
* Date/Publication: 2024-05-02 13:40:03 UTC
* Number of recursive dependencies: 144

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

# tabr

<details>

* Version: 0.4.9
* GitHub: https://github.com/leonawicz/tabr
* Source code: https://github.com/cran/tabr
* Date/Publication: 2023-09-21 16:50:02 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "tabr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tabr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_fretboard
    > ### Title: Chord and fretboard diagram plots
    > ### Aliases: plot_fretboard plot_chord
    > 
    > ### ** Examples
    > 
    > # General patterns: scale shifting exercise
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

# TcGSA

<details>

* Version: 0.12.10
* GitHub: https://github.com/sistm/TcGSA
* Source code: https://github.com/cran/TcGSA
* Date/Publication: 2022-02-28 21:40:02 UTC
* Number of recursive dependencies: 122

Run `revdepcheck::cloud_details(, "TcGSA")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘TcGSA_userguide.Rmd’
      ...
    Optimally clustering...
    
    DONE
    
    Scale for y is already present.
    Adding another scale for y, which will replace the existing scale.
    
      When sourcing ‘TcGSA_userguide.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘TcGSA_userguide.Rmd’ using ‘UTF-8’... failed
    ```

# TCIU

<details>

* Version: 1.2.6
* GitHub: https://github.com/SOCR/TCIU
* Source code: https://github.com/cran/TCIU
* Date/Publication: 2024-05-17 23:40:21 UTC
* Number of recursive dependencies: 172

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

# thematic

<details>

* Version: 0.1.5
* GitHub: https://github.com/rstudio/thematic
* Source code: https://github.com/cran/thematic
* Date/Publication: 2024-02-14 00:20:03 UTC
* Number of recursive dependencies: 106

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

* Version: 3.0.6
* GitHub: https://github.com/mjskay/tidybayes
* Source code: https://github.com/cran/tidybayes
* Date/Publication: 2023-08-12 23:30:02 UTC
* Number of recursive dependencies: 192

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
      • test.geom_interval/grouped-intervals-h-stat.svg
      • test.geom_pointinterval/grouped-pointintervals-h-stat.svg
      • test.stat_dist_slabinterval/ccdfintervalh-using-args.svg
      • test.stat_eye/one-parameter-horizontal-eye-mode-hdi.svg
      • test.stat_eye/one-parameter-horizontal-half-eye.svg
      • test.stat_eye/one-parameter-vertical-eye.svg
      • test.stat_eye/one-parameter-vertical-halfeye.svg
      • test.stat_eye/two-parameter-factor-horizontal-eye-fill.svg
      Error: Test failures
      Execution halted
    ```

## In both

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

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘dotwhisker’
    ```

# tidyCDISC

<details>

* Version: 0.2.1
* GitHub: https://github.com/Biogen-Inc/tidyCDISC
* Source code: https://github.com/cran/tidyCDISC
* Date/Publication: 2023-03-16 14:20:02 UTC
* Number of recursive dependencies: 140

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
* Number of recursive dependencies: 74

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
# tidysdm

<details>

* Version: 0.9.4
* GitHub: https://github.com/EvolEcolGroup/tidysdm
* Source code: https://github.com/cran/tidysdm
* Date/Publication: 2024-03-05 20:30:02 UTC
* Number of recursive dependencies: 167

Run `revdepcheck::cloud_details(, "tidysdm")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘a0_tidysdm_overview.Rmd’
      ...
    > climate_vars <- names(climate_present)
    
    > lacerta_thin <- lacerta_thin %>% bind_cols(terra::extract(climate_present, 
    +     lacerta_thin, ID = FALSE))
    
    > lacerta_thin %>% plot_pres_vs_bg(class)
    
      When sourcing ‘a0_tidysdm_overview.R’:
    Error: object is not a unit
    Execution halted
    
      ‘a0_tidysdm_overview.Rmd’ using ‘UTF-8’... failed
      ‘a1_palaeodata_application.Rmd’ using ‘UTF-8’... OK
      ‘a2_tidymodels_additions.Rmd’ using ‘UTF-8’... OK
      ‘a3_troubleshooting.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘a0_tidysdm_overview.Rmd’ using rmarkdown
    ```

# tidytreatment

<details>

* Version: 0.2.2
* GitHub: https://github.com/bonStats/tidytreatment
* Source code: https://github.com/cran/tidytreatment
* Date/Publication: 2022-02-21 09:00:07 UTC
* Number of recursive dependencies: 98

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
    ! unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, 
        NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0,
    Execution halted
    
      ‘use-tidytreatment-BART.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘use-tidytreatment-BART.Rmd’ using rmarkdown
    
    Quitting from lines 163-177 [plot-tidy-bart] (use-tidytreatment-BART.Rmd)
    Error: processing vignette 'use-tidytreatment-BART.Rmd' failed with diagnostics:
    Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `use_defaults()`:
    ! unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, 
    ...
        NULL, NULL, 1.2, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.2, NULL, list(NULL, NULL, NULL, 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0, list(), 2, list("white", NA, NULL, NULL, TRUE), list(NULL, "grey20", NULL, NULL, TRUE), NULL, NULL, NULL, list("grey92", NULL, NULL, NULL, FALSE, "grey92", TRUE), NULL, list(NULL, 0.5, NULL, 
            NULL, FALSE, NULL, TRUE), NULL, NULL, NULL, NULL, FALSE, list(NULL, "white", NULL, NULL, TRUE), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(5.5, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, TRUE), "topleft", NULL, NULL, list("grey85", "grey20", NULL, NULL, TRUE), NULL, NULL, "inherit", 
        "inside", list(NULL, NULL, "grey10", 0.8, NULL, NULL, NULL, NULL, c(4.4, 4.4, 4.4, 4.4), NULL, TRUE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75))
    --- failed re-building ‘use-tidytreatment-BART.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘use-tidytreatment-BART.Rmd’
    
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
* Number of recursive dependencies: 226

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

* Version: 2.4.1
* GitHub: https://github.com/xjsun1221/tinyarray
* Source code: https://github.com/cran/tinyarray
* Date/Publication: 2024-06-04 09:45:15 UTC
* Number of recursive dependencies: 240

Run `revdepcheck::cloud_details(, "tinyarray")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tinyarray-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: exp_surv
    > ### Title: exp_surv
    > ### Aliases: exp_surv
    > 
    > ### ** Examples
    > 
    > tmp = exp_surv(exprSet_hub1,meta1)
    > patchwork::wrap_plots(tmp)+patchwork::plot_layout(guides = "collect")
    Error in identicalUnits(x) : object is not a unit
    Calls: <Anonymous> ... assemble_guides -> guides_build -> unit.c -> identicalUnits
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

# tmap

<details>

* Version: 3.3-4
* GitHub: https://github.com/r-tmap/tmap
* Source code: https://github.com/cran/tmap
* Date/Publication: 2023-09-12 21:20:02 UTC
* Number of recursive dependencies: 145

Run `revdepcheck::cloud_details(, "tmap")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tmap-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: tm_symbols
    > ### Title: Draw symbols
    > ### Aliases: tm_symbols tm_squares tm_bubbles tm_dots tm_markers
    > 
    > ### ** Examples
    > 
    > data(World, metro)
    ...
        ▆
     1. └─base::lapply(...)
     2.   └─global FUN(X[[i]], ...)
     3.     └─ggplot2::ggplotGrob(...)
     4.       ├─ggplot2::ggplot_gtable(ggplot_build(x))
     5.       └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     6.         └─ggplot2::calc_element("plot.margin", theme)
     7.           └─cli::cli_abort(...)
     8.             └─rlang::abort(...)
    Execution halted
    ```

# TOmicsVis

<details>

* Version: 2.0.0
* GitHub: https://github.com/benben-miao/TOmicsVis
* Source code: https://github.com/cran/TOmicsVis
* Date/Publication: 2023-08-28 18:30:02 UTC
* Number of recursive dependencies: 264

Run `revdepcheck::cloud_details(, "TOmicsVis")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘TOmicsVis-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: upsetr_plot
    > ### Title: UpSet plot for stat common and unique gene among multiple sets.
    > ### Aliases: upsetr_plot
    > 
    > ### ** Examples
    > 
    > # 1. Library TOmicsVis package
    ...
      3.     ├─base::suppressMessages(...)
      4.     │ └─base::withCallingHandlers(...)
      5.     └─UpSetR:::Make_main_bar(...)
      6.       └─ggplot2::ggplotGrob(Main_bar_plot)
      7.         ├─ggplot2::ggplot_gtable(ggplot_build(x))
      8.         └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
      9.           └─ggplot2::calc_element("plot.margin", theme)
     10.             └─cli::cli_abort(...)
     11.               └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Tutorials.Rmd’
      ...
    5 transcript_8832 transcript_3069 transcript_10224  transcript_9881
    6   transcript_74 transcript_9809  transcript_3151  transcript_8836
    
    > upsetr_plot(data = degs_lists, sets_num = 4, keep_order = FALSE, 
    +     order_by = "freq", decrease = TRUE, mainbar_color = "#006600", 
    +     number .... [TRUNCATED] 
    
      When sourcing ‘Tutorials.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘Tutorials.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Tutorials.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.3Mb
      sub-directories of 1Mb or more:
        data          1.5Mb
        data-tables   1.5Mb
        doc           1.9Mb
        help          1.2Mb
    ```

# tornado

<details>

* Version: 0.1.3
* GitHub: https://github.com/bertcarnell/tornado
* Source code: https://github.com/cran/tornado
* Date/Publication: 2024-01-21 17:30:02 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::cloud_details(, "tornado")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tornado-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.tornado_plot
    > ### Title: Plot a Tornado Plot object
    > ### Aliases: plot.tornado_plot
    > 
    > ### ** Examples
    > 
    > gtest <- lm(mpg ~ cyl*wt*hp, data = mtcars)
    ...
     13.         │ └─base::withCallingHandlers(...)
     14.         └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     15.           └─l$compute_geom_2(d, theme = plot$theme)
     16.             └─ggplot2 (local) compute_geom_2(..., self = self)
     17.               └─self$geom$use_defaults(...)
     18.                 └─ggplot2 (local) use_defaults(..., self = self)
     19.                   └─ggplot2:::check_aesthetics(new_params, nrow(data))
     20.                     └─cli::cli_abort(...)
     21.                       └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > if (require(testthat))
      + {
      +   library(tornado)
      + 
      +   test_check("tornado")
      + }
      Loading required package: testthat
    ...
                  ...))
      })(position = "identity", stat = "identity", width = NULL)`: Problem while setting up geom aesthetics.
      ℹ Error occurred in the 1st layer.
      Caused by error in `check_aesthetics()`:
      ! Aesthetics must be either length 1 or the same as the data (20).
      ✖ Fix the following mappings: `width`.
      
      [ FAIL 14 | WARN 0 | SKIP 0 | PASS 101 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘tornadoVignette.Rmd’
      ...
    +      .... [TRUNCATED] 
    Loading required package: lattice
    
      When sourcing ‘tornadoVignette.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `check_aesthetics()`:
    ! Aesthetics must be either length 1 or the same as the data (20).
    ✖ Fix the following mappings: `width`.
    Execution halted
    
      ‘tornadoVignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘tornadoVignette.Rmd’ using rmarkdown
    ```

# TOSTER

<details>

* Version: 0.8.3
* GitHub: NA
* Source code: https://github.com/cran/TOSTER
* Date/Publication: 2024-05-08 16:40:02 UTC
* Number of recursive dependencies: 102

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
      unused argument (theme = list(list("black", 0.727272727272727, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.727272727272727, 1, TRUE), list("", "plain", "black", 16, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.727272727272727, 1.45454545454545, "", 5.62335685623357, 2.18181818181818, 19, TRUE), 8, c(8, 8, 8, 8), NULL, NULL, list(NULL, NULL, "#333333", NULL, NULL, NULL, NULL, NULL, c(10, 0, 0, 0), NULL, FALSE), list(NULL, NULL, NULL, NULL, NULL, 0, 
        NULL, NULL, c(0, 0, 4, 0), NULL, TRUE), NULL, list(NULL, NULL, "#333333", NULL, NULL, NULL, 90, NULL, c(0, 10, 0, 0), NULL, FALSE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 4), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, "#333333", NULL, NULL, NULL, NULL, NULL, c(5, 0, 0, 0), NULL, FALSE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 3.2, 0), NULL, TRUE), NULL, list(), NULL
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
    Error: unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(7, 0, 0, 0), NULL, FALSE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, 
        NULL, NULL, 1, 90, NULL, c(0, 7, 0, 0), NULL, FALSE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, "bold", NULL, 11, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, FALSE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 2.2), NULL, TRUE)
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

# toxEval

<details>

* Version: 1.3.2
* GitHub: https://github.com/DOI-USGS/toxEval
* Source code: https://github.com/cran/toxEval
* Date/Publication: 2024-02-08 07:30:02 UTC
* Number of recursive dependencies: 127

Run `revdepcheck::cloud_details(, "toxEval")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘toxEval-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_tox_stacks
    > ### Title: Plot stacked bar charts
    > ### Aliases: plot_tox_stacks
    > 
    > ### ** Examples
    > 
    > # This is the example workflow:
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.1Mb
      sub-directories of 1Mb or more:
        R   7.2Mb
    ```

# TreatmentPatterns

<details>

* Version: 2.6.7
* GitHub: https://github.com/darwin-eu/TreatmentPatterns
* Source code: https://github.com/cran/TreatmentPatterns
* Date/Publication: 2024-05-24 08:30:32 UTC
* Number of recursive dependencies: 142

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
       22. ├─testthat::expect_s3_class(output$charAgePlot$html, "html") at test-CharacterizationPlots.R:47:9
       23. │ └─testthat::quasi_label(enquo(object), arg = "object")
       24. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       25. ├─output$charAgePlot
       26. └─shiny:::`$.shinyoutput`(output, charAgePlot)
       27.   └─.subset2(x, "impl")$getOutput(name)
      
      [ FAIL 1 | WARN 0 | SKIP 21 | PASS 134 ]
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
    
    > ### Name: plot.trtsel
    > ### Title: plot risk curves, treatment effect curves or cdf of risk for a
    > ###   trtsel object.
    > ### Aliases: plot.trtsel plot
    > 
    > ### ** Examples
    > 
    ...
     1. ├─base::plot(...)
     2. └─TreatmentSelection:::plot.trtsel(...)
     3.   └─TreatmentSelection (local) tmp.plotfun(...)
     4.     └─ggplot2::ggplotGrob((p))
     5.       ├─ggplot2::ggplot_gtable(ggplot_build(x))
     6.       └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     7.         └─ggplot2::calc_element("plot.margin", theme)
     8.           └─cli::cli_abort(...)
     9.             └─rlang::abort(...)
    Execution halted
    ```

# TreeDep

<details>

* Version: 0.1.3
* GitHub: NA
* Source code: https://github.com/cran/TreeDep
* Date/Publication: 2018-12-02 17:50:03 UTC
* Number of recursive dependencies: 32

Run `revdepcheck::cloud_details(, "TreeDep")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘TreeDep-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: TreeDep_plot
    > ### Title: TreeDep_plot - Generates a plot for selected variables and
    > ###   dates.
    > ### Aliases: TreeDep_plot
    > 
    > ### ** Examples
    > 
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

# TreeDist

<details>

* Version: 2.7.0
* GitHub: https://github.com/ms609/TreeDist
* Source code: https://github.com/cran/TreeDist
* Date/Publication: 2023-10-25 22:10:02 UTC
* Number of recursive dependencies: 230

Run `revdepcheck::cloud_details(, "TreeDist")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘treespace.Rmd’
      ...
    [1] "Epoch: 3 finished. 212 datapoints changed bestmatch"
    [1] "Epoch: 4 started"
    [1] "Epoch: 4 finished. 203 datapoints changed bestmatch"
    [1] "Epoch: 5 started"
    [1] "Epoch: 5 finished. 165 datapoints changed bestmatch"
    [1] "---- Esom Training Finished ----"
    
    ...
    
      ‘Generalized-RF.Rmd’ using ‘UTF-8’... OK
      ‘Robinson-Foulds.Rmd’ using ‘UTF-8’... OK
      ‘Using-TreeDist.Rmd’ using ‘UTF-8’... OK
      ‘compare-treesets.Rmd’ using ‘UTF-8’... OK
      ‘different-leaves.Rmd’ using ‘UTF-8’... OK
      ‘information.Rmd’ using ‘UTF-8’... OK
      ‘landscapes.Rmd’ using ‘UTF-8’... OK
      ‘treespace.Rmd’ using ‘UTF-8’... failed
      ‘using-distances.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Generalized-RF.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.2Mb
      sub-directories of 1Mb or more:
        doc    5.0Mb
        libs   3.6Mb
    ```

# treeheatr

<details>

* Version: 0.2.1
* GitHub: https://github.com/trang1618/treeheatr
* Source code: https://github.com/cran/treeheatr
* Date/Publication: 2020-11-19 21:00:03 UTC
* Number of recursive dependencies: 97

Run `revdepcheck::cloud_details(, "treeheatr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘treeheatr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: draw_heat
    > ### Title: Draws the heatmap.
    > ### Aliases: draw_heat
    > 
    > ### ** Examples
    > 
    > x <- compute_tree(penguins, target_lab = 'species')
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘explore.Rmd’
      ...
      Please report the issue at <https://github.com/trang1618/treeheatr/issues>.
    Warning: The `guide` argument in `scale_*()` cannot be `FALSE`. This was deprecated in
    ggplot2 3.3.4.
    ℹ Please use "none" instead.
    ℹ The deprecated feature was likely used in the treeheatr package.
      Please report the issue at <https://github.com/trang1618/treeheatr/issues>.
    
      When sourcing ‘explore.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘explore.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘explore.Rmd’ using rmarkdown
    
    Quitting from lines 33-36 [unnamed-chunk-2] (explore.Rmd)
    Error: processing vignette 'explore.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘explore.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘explore.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# trelliscopejs

<details>

* Version: 0.2.6
* GitHub: https://github.com/hafen/trelliscopejs
* Source code: https://github.com/cran/trelliscopejs
* Date/Publication: 2021-02-01 08:00:02 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "trelliscopejs")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘trelliscopejs-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: cog
    > ### Title: Cast Column as a Cognostic
    > ### Aliases: cog
    > 
    > ### ** Examples
    > 
    > library(dplyr)
    ...
      8.         ├─base::tryCatch(...)
      9.         │ └─base (local) tryCatchList(expr, classes, parentenv, handlers)
     10.         ├─base::print(p)
     11.         └─ggplot2:::print.ggplot(p)
     12.           ├─ggplot2::ggplot_gtable(data)
     13.           └─ggplot2:::ggplot_gtable.ggplot_built(data)
     14.             └─ggplot2::calc_element("plot.margin", theme)
     15.               └─cli::cli_abort(...)
     16.                 └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(trelliscopejs)
      > 
      > test_check("trelliscopejs")
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
       12.         └─ggplot2:::print.ggplot(p)
       13.           ├─ggplot2::ggplot_gtable(data)
       14.           └─ggplot2:::ggplot_gtable.ggplot_built(data)
       15.             └─ggplot2::calc_element("plot.margin", theme)
       16.               └─cli::cli_abort(...)
       17.                 └─rlang::abort(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

# tricolore

<details>

* Version: 1.2.4
* GitHub: https://github.com/jschoeley/tricolore
* Source code: https://github.com/cran/tricolore
* Date/Publication: 2024-05-15 15:00:02 UTC
* Number of recursive dependencies: 108

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
* Number of recursive dependencies: 77

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
        unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(7, 0, 0, 0), NULL, FALSE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, 
            NULL, NULL, 1, 90, NULL, c(0, 7, 0, 0), NULL, FALSE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, 
            NULL, NULL, NULL, c(0, 0, 0, 2.2), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0.5, NULL, NULL, NULL, c(0, 2.2, 0, 2.2), NULL, TRUE), list("grey70", 0.5, NULL, NULL, FALSE, "grey70", TRUE), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.5, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(), list("gray70", 0.5, NULL, NULL, FALSE, "gray70", FALSE), NULL, NULL, list("gray70", 0.5, NULL, NULL, FALSE, 
            "gray70", FALSE), NULL, NULL, NULL, NULL, list(NULL, NA, NULL, NULL, TRUE), NULL, 2, NULL, NULL, NULL, 1.2, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.2, NULL, list(NULL, NULL, NULL, 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0, list(), 2, list("white", NA, NULL, NULL, TRUE), list(), NULL, NULL, NULL, list("grey87", NULL, NULL, 
            NULL, FALSE, "grey87", TRUE), list(), list(), NULL, NULL, NULL, NULL, FALSE, list(NULL, "white", NULL, NULL, TRUE), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(5.5, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, TRUE), "topleft", NULL, NULL, list("gray90", NA, NULL, NULL, FALSE), NULL, 
            NULL, "inherit", "inside", list(NULL, NULL, "black", 0.8, NULL, NULL, NULL, NULL, c(6, 6, 6, 6), NULL, FALSE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75))
      
      [ FAIL 1 | WARN 14 | SKIP 0 | PASS 108 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 163.0Mb
      sub-directories of 1Mb or more:
        libs  162.0Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# umiAnalyzer

<details>

* Version: 1.0.0
* GitHub: https://github.com/sfilges/umiAnalyzer
* Source code: https://github.com/cran/umiAnalyzer
* Date/Publication: 2021-11-25 08:40:02 UTC
* Number of recursive dependencies: 116

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

# UnalR

<details>

* Version: 1.0.0
* GitHub: https://github.com/estadisticaun/UnalR
* Source code: https://github.com/cran/UnalR
* Date/Publication: 2024-05-25 17:20:05 UTC
* Number of recursive dependencies: 168

Run `revdepcheck::cloud_details(, "UnalR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘UnalR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Plot.Barras
    > ### Title: Cree un gráfico de barras que muestre la información de forma
    > ###   horizontal o vertical, para variables nominales u ordinales con dos
    > ###   diferentes paquetes
    > ### Aliases: Plot.Barras
    > 
    > ### ** Examples
    ...
     1. └─(if (getRversion() >= "3.4") withAutoprint else force)(...)
     2.   └─base::source(...)
     3.     ├─base::print(yy$value)
     4.     └─ggplot2:::print.ggplot(yy$value)
     5.       ├─ggplot2::ggplot_gtable(data)
     6.       └─ggplot2:::ggplot_gtable.ggplot_built(data)
     7.         └─ggplot2::calc_element("plot.margin", theme)
     8.           └─cli::cli_abort(...)
     9.             └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.0Mb
      sub-directories of 1Mb or more:
        R      2.3Mb
        data   2.0Mb
        help   2.6Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 312859 marked UTF-8 strings
    ```

# UpSetR

<details>

* Version: 1.4.0
* GitHub: https://github.com/hms-dbmi/UpSetR
* Source code: https://github.com/cran/UpSetR
* Date/Publication: 2019-05-22 23:30:03 UTC
* Number of recursive dependencies: 36

Run `revdepcheck::cloud_details(, "UpSetR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘UpSetR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: upset
    > ### Title: UpSetR Plot
    > ### Aliases: upset
    > 
    > ### ** Examples
    > 
    > movies <- read.csv( system.file("extdata", "movies.csv", package = "UpSetR"), header=TRUE, sep=";" )
    ...
      2.   ├─base::suppressMessages(...)
      3.   │ └─base::withCallingHandlers(...)
      4.   └─UpSetR:::Make_main_bar(...)
      5.     └─ggplot2::ggplotGrob(Main_bar_plot)
      6.       ├─ggplot2::ggplot_gtable(ggplot_build(x))
      7.       └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
      8.         └─ggplot2::calc_element("plot.margin", theme)
      9.           └─cli::cli_abort(...)
     10.             └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘attribute.plots.Rmd’
      ...
    
    > movies <- read.csv(system.file("extdata", "movies.csv", 
    +     package = "UpSetR"), header = T, sep = ";")
    
    > upset(movies, main.bar.color = "black", queries = list(list(query = intersects, 
    +     params = list("Drama"), active = T)), attribute.plots = list( .... [TRUNCATED] 
    
    ...
    +         assign = 20 .... [TRUNCATED] 
    
      When sourcing ‘set.metadata.plots.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘attribute.plots.Rmd’ using ‘UTF-8’... failed
      ‘basic.usage.Rmd’ using ‘UTF-8’... failed
      ‘queries.Rmd’ using ‘UTF-8’... failed
      ‘set.metadata.plots.Rmd’ using ‘UTF-8’... failed
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.1Mb
      sub-directories of 1Mb or more:
        doc   7.6Mb
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# vDiveR

<details>

* Version: 1.2.1
* GitHub: NA
* Source code: https://github.com/cran/vDiveR
* Date/Publication: 2024-01-09 20:20:02 UTC
* Number of recursive dependencies: 131

Run `revdepcheck::cloud_details(, "vDiveR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘vDiveR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_conservationLevel
    > ### Title: Conservation Levels Distribution Plot
    > ### Aliases: plot_conservationLevel
    > 
    > ### ** Examples
    > 
    > plot_conservationLevel(proteins_1host, conservation_label = 1,alpha=0.8, base_size = 15)
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DT’ ‘maps’ ‘readr’
      All declared Imports should be used.
    ```

# VDSM

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/VDSM
* Date/Publication: 2021-04-16 09:00:02 UTC
* Number of recursive dependencies: 57

Run `revdepcheck::cloud_details(, "VDSM")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘VDSM-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Gplot
    > ### Title: Gplot.
    > ### Aliases: Gplot
    > 
    > ### ** Examples
    > 
    > data(exampleX)
    ...
        ▆
     1. └─VDSM::Gplot(X, f, p)
     2.   ├─base::suppressWarnings(ggplot_gtable(ggplot_build(p1.common.y)))
     3.   │ └─base::withCallingHandlers(...)
     4.   ├─ggplot2::ggplot_gtable(ggplot_build(p1.common.y))
     5.   └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(p1.common.y))
     6.     └─ggplot2::calc_element("plot.margin", theme)
     7.       └─cli::cli_abort(...)
     8.         └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(VDSM)
      > 
      > test_check("VDSM")
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 0 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
       3.   │ └─base::withCallingHandlers(...)
       4.   ├─ggplot2::ggplot_gtable(ggplot_build(p1.common.y))
       5.   └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(p1.common.y))
       6.     └─ggplot2::calc_element("plot.margin", theme)
       7.       └─cli::cli_abort(...)
       8.         └─rlang::abort(...)
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

# virtualPollen

<details>

* Version: 1.0.1
* GitHub: https://github.com/BlasBenito/virtualPollen
* Source code: https://github.com/cran/virtualPollen
* Date/Publication: 2022-02-13 13:00:02 UTC
* Number of recursive dependencies: 122

Run `revdepcheck::cloud_details(, "virtualPollen")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘virtualPollen-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: simulateDriverS
    > ### Title: Generates drivers for 'simulatePopulation'.
    > ### Aliases: simulateDriverS
    > 
    > ### ** Examples
    > 
    > 
    ...
      8.           └─cowplot:::as_gtable.default(x)
      9.             ├─cowplot::as_grob(plot)
     10.             └─cowplot:::as_grob.ggplot(plot)
     11.               └─ggplot2::ggplotGrob(plot)
     12.                 ├─ggplot2::ggplot_gtable(ggplot_build(x))
     13.                 └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     14.                   └─ggplot2::calc_element("plot.margin", theme)
     15.                     └─cli::cli_abort(...)
     16.                       └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘using_virtualPollen.Rmd’
      ...
    
    > p7 <- ggplot(data = acfToDf(moves.100, 200, 50), aes(x = lag, 
    +     y = acf)) + geom_hline(aes(yintercept = 0)) + geom_hline(aes(yintercept = ci.ma .... [TRUNCATED] 
    
    > plot_grid(p4, p5, p6, p7, labels = c("a", "b", "c", 
    +     "d"), align = "v", nrow = 2)
    
      When sourcing ‘using_virtualPollen.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘using_virtualPollen.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘using_virtualPollen.Rmd’ using rmarkdown
    ```

# viscomp

<details>

* Version: 1.0.0
* GitHub: https://github.com/georgiosseitidis/viscomp
* Source code: https://github.com/cran/viscomp
* Date/Publication: 2023-01-16 09:50:02 UTC
* Number of recursive dependencies: 149

Run `revdepcheck::cloud_details(, "viscomp")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘viscomp-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: loccos
    > ### Title: Leaving One Component Combination Out Scatter plot
    > ### Aliases: loccos
    > 
    > ### ** Examples
    > 
    > data(nmaMACE)
    ...
        ▆
     1. └─viscomp::loccos(model = nmaMACE, combination = c("B"))
     2.   └─ggExtra::ggMarginal(p, type = "histogram", fill = histogram.color)
     3.     └─ggplot2::ggplotGrob(scatP)
     4.       ├─ggplot2::ggplot_gtable(ggplot_build(x))
     5.       └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     6.         └─ggplot2::calc_element("plot.margin", theme)
     7.           └─cli::cli_abort(...)
     8.             └─rlang::abort(...)
    Execution halted
    ```

# visR

<details>

* Version: 0.4.1
* GitHub: https://github.com/openpharma/visR
* Source code: https://github.com/cran/visR
* Date/Publication: 2024-03-15 21:50:02 UTC
* Number of recursive dependencies: 148

Run `revdepcheck::cloud_details(, "visR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘visR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: add_risktable
    > ### Title: Add risk tables to visR plots through an S3 method
    > ### Aliases: add_risktable add_risktable.ggsurvfit
    > ###   add_risktable.ggtidycuminc
    > 
    > ### ** Examples
    > 
    ...
      4. │ └─gglist %>% align_plots()
      5. └─visR::align_plots(.)
      6.   └─base::lapply(pltlist, ggplot2::ggplotGrob)
      7.     └─ggplot2 (local) FUN(X[[i]], ...)
      8.       ├─ggplot2::ggplot_gtable(ggplot_build(x))
      9.       └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     10.         └─ggplot2::calc_element("plot.margin", theme)
     11.           └─cli::cli_abort(...)
     12.             └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(visR)
      > library(vdiffr)
      > library(survival)
      > 
      > test_check("visR")
    ...
       10.         └─ggplot2::calc_element("plot.margin", theme)
       11.           └─cli::cli_abort(...)
       12.             └─rlang::abort(...)
      
      [ FAIL 14 | WARN 28 | SKIP 24 | PASS 991 ]
      Error: Test failures
      In addition: Warning message:
      In .Internal(delayedAssign(x, substitute(value), eval.env, assign.env)) :
        closing unused connection 4 (https://raw.githubusercontent.com/vntkumar8/covid-survival/main/data/final.csv)
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘CDISC_ADaM.Rmd’
      ...
    </table>
    </div>
    
    > visr(survfit_object) %>% visR::add_CI() %>% visR::add_risktable()
    Warning: `visr.survfit()` was deprecated in visR 0.4.0.
    ℹ Please use `ggsurvfit::ggsurvfit()` instead.
    
    ...
    +     size = 2) %>% visR::add_risktable()
    
      When sourcing ‘Time_to_event_analysis.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘CDISC_ADaM.Rmd’ using ‘UTF-8’... failed
      ‘Consort_flow_diagram.Rmd’ using ‘UTF-8’... OK
      ‘Styling_KM_plots.Rmd’ using ‘UTF-8’... OK
      ‘Time_to_event_analysis.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘CDISC_ADaM.Rmd’ using rmarkdown
    
    Quitting from lines 86-90 [km_plot_1] (CDISC_ADaM.Rmd)
    Error: processing vignette 'CDISC_ADaM.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘CDISC_ADaM.Rmd’
    
    --- re-building ‘Consort_flow_diagram.Rmd’ using rmarkdown
    ```

# vivainsights

<details>

* Version: 0.5.2
* GitHub: https://github.com/microsoft/vivainsights
* Source code: https://github.com/cran/vivainsights
* Date/Publication: 2024-03-14 17:40:02 UTC
* Number of recursive dependencies: 114

Run `revdepcheck::cloud_details(, "vivainsights")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘vivainsights-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: tm_freq
    > ### Title: Perform a Word or Ngram Frequency Analysis and return a Circular
    > ###   Bar Plot
    > ### Aliases: tm_freq
    > 
    > ### ** Examples
    > 
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

# vivaldi

<details>

* Version: 1.0.1
* GitHub: https://github.com/GreshamLab/vivaldi
* Source code: https://github.com/cran/vivaldi
* Date/Publication: 2023-03-21 20:10:02 UTC
* Number of recursive dependencies: 102

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
      
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 29 ]
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

# vvshiny

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/vvshiny
* Date/Publication: 2023-07-19 15:30:02 UTC
* Number of recursive dependencies: 131

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

# WASP

<details>

* Version: 1.4.3
* GitHub: https://github.com/zejiang-unsw/WASP
* Source code: https://github.com/cran/WASP
* Date/Publication: 2022-08-22 07:50:24 UTC
* Number of recursive dependencies: 153

Run `revdepcheck::cloud_details(, "WASP")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘WASP-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: fig.dwt.vt
    > ### Title: Plot function: Variance structure before and after variance
    > ###   transformation
    > ### Aliases: fig.dwt.vt
    > 
    > ### ** Examples
    > 
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rlang’
      All declared Imports should be used.
    ```

# Wats

<details>

* Version: 1.0.1
* GitHub: https://github.com/OuhscBbmc/Wats
* Source code: https://github.com/cran/Wats
* Date/Publication: 2023-03-10 22:50:05 UTC
* Number of recursive dependencies: 122

Run `revdepcheck::cloud_details(, "Wats")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘mbr-figures.Rmd’
      ...
    > grid::grid.newpage()
    
    > grid::pushViewport(grid::viewport(layout = grid::grid.layout(3, 
    +     1)))
    
    > print(top_panel, vp = vp_layout(1, 1))
    
      When sourcing ‘mbr-figures.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘mbr-figures.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘mbr-figures.Rmd’ using rmarkdown
    ```

# whomds

<details>

* Version: 1.1.1
* GitHub: https://github.com/lindsayevanslee/whomds
* Source code: https://github.com/cran/whomds
* Date/Publication: 2023-09-08 04:30:02 UTC
* Number of recursive dependencies: 123

Run `revdepcheck::cloud_details(, "whomds")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘whomds-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: fig_density
    > ### Title: Plot a density of a score
    > ### Aliases: fig_density
    > 
    > ### ** Examples
    > 
    > fig_density(df_adults, score = "disability_score", cutoffs = c(19.1, 34.4, 49.6), 
    ...
    Backtrace:
        ▆
     1. ├─base (local) `<fn>`(x)
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_gtable(data)
     4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     5.     └─ggplot2::calc_element("plot.margin", theme)
     6.       └─cli::cli_abort(...)
     7.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking running R code from vignettes ... WARNING
    ```
    Errors in running code in vignettes:
    when running code in ‘c2_getting_started_EN.Rmd’
      ...
    +     out.width = "80%", fig.align = "center", collapse = TRUE, 
    +     comment = "#> ..." ... [TRUNCATED] 
    
    > install.packages("whomds")
    Installing package into ‘/tmp/workdir/whomds/new/whomds.Rcheck’
    (as ‘lib’ is unspecified)
    
    ...
      ‘c2_getting_started_EN.Rmd’ using ‘UTF-8’... failed
      ‘c2_getting_started_ES.Rmd’ using ‘UTF-8’... failed
      ‘c3_rasch_adults_EN.Rmd’ using ‘UTF-8’... failed
      ‘c3_rasch_adults_ES.Rmd’ using ‘UTF-8’... failed
      ‘c4_rasch_children_EN.Rmd’ using ‘UTF-8’... failed
      ‘c4_rasch_children_ES.Rmd’ using ‘UTF-8’... failed
      ‘c5_best_practices_EN.Rmd’ using ‘UTF-8’... OK
      ‘c5_best_practices_ES.Rmd’ using ‘UTF-8’... OK
      ‘c6_after_rasch_EN.Rmd’ using ‘UTF-8’... failed
      ‘c6_after_rasch_ES.Rmd’ using ‘UTF-8’... failed
    ```

# wilson

<details>

* Version: 2.4.2
* GitHub: https://github.com/loosolab/wilson
* Source code: https://github.com/cran/wilson
* Date/Publication: 2021-04-19 09:40:02 UTC
* Number of recursive dependencies: 199

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

# WVPlots

<details>

* Version: 1.3.8
* GitHub: https://github.com/WinVector/WVPlots
* Source code: https://github.com/cran/WVPlots
* Date/Publication: 2024-04-22 20:40:07 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::cloud_details(, "WVPlots")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘WVPlots-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ScatterHist
    > ### Title: Plot a scatter plot with marginals.
    > ### Aliases: ScatterHist
    > 
    > ### ** Examples
    > 
    > 
    ...
      2.   └─gridExtra::grid.arrange(...)
      3.     └─gridExtra::arrangeGrob(...)
      4.       └─base::lapply(grobs[toconv], ggplot2::ggplotGrob)
      5.         └─ggplot2 (local) FUN(X[[i]], ...)
      6.           ├─ggplot2::ggplot_gtable(ggplot_build(x))
      7.           └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
      8.             └─ggplot2::calc_element("plot.margin", theme)
      9.               └─cli::cli_abort(...)
     10.                 └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘tinytest.R’
    Running the tests in ‘tests/tinytest.R’ failed.
    Complete output:
      > 
      > if (requireNamespace("tinytest", quietly=TRUE) ) {
      +   if (requireNamespace('data.table', quietly = TRUE)) {
      +     # don't multi-thread during CRAN checks
      +     data.table::setDTthreads(1)
      +   }
      +   tinytest::test_package("WVPlots")
    ...
       10.                   └─gridExtra::grid.arrange(...)
       11.                     └─gridExtra::arrangeGrob(...)
       12.                       └─base::lapply(grobs[toconv], ggplot2::ggplotGrob)
       13.                         └─ggplot2 (local) FUN(X[[i]], ...)
       14.                           ├─ggplot2::ggplot_gtable(ggplot_build(x))
       15.                           └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
       16.                             └─ggplot2::calc_element("plot.margin", theme)
       17.                               └─cli::cli_abort(...)
       18.                                 └─rlang::abort(...)
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘WVPlots_concept.Rmd’
      ...
    > frm$absY <- abs(frm$y)
    
    > frm$posY = frm$y > 0
    
    > WVPlots::ScatterHist(frm, "x", "y", smoothmethod = "lm", 
    +     title = "Example Linear Fit")
    
    ...
    > frm$posY = frm$y > 0
    
    > WVPlots::ScatterHist(frm, "x", "y", title = "Example Fit")
    
      When sourcing ‘WVPlots_examples.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘WVPlots_concept.Rmd’ using ‘UTF-8’... failed
      ‘WVPlots_examples.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘WVPlots_concept.Rmd’ using rmarkdown
    ```

# xaringanthemer

<details>

* Version: 0.4.2
* GitHub: https://github.com/gadenbuie/xaringanthemer
* Source code: https://github.com/cran/xaringanthemer
* Date/Publication: 2022-08-20 18:40:02 UTC
* Number of recursive dependencies: 75

Run `revdepcheck::cloud_details(, "xaringanthemer")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(xaringanthemer)
      > 
      > test_check("xaringanthemer")
      [ FAIL 1 | WARN 18 | SKIP 1 | PASS 308 ]
      
      ══ Skipped tests (1) ═══════════════════════════════════════════════════════════
    ...
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-ggplot2.R:267:3'): theme_xaringan_restore_defaults() restores defaults ──
      res$after_restore$line_colour (`actual`) not equal to res$original$colour (`expected`).
      
      `actual` is a character vector ('#0088ff')
      `expected` is an S3 object of class <quosure/formula>, a call
      
      [ FAIL 1 | WARN 18 | SKIP 1 | PASS 308 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘xaringanthemer.Rmd’
      ...
    Warning in file(con, "r") :
      cannot open file './../man/fragments/_quick-intro.Rmd': No such file or directory
    
    Quitting from lines 43-43 [unnamed-chunk-2] (xaringanthemer.Rmd)
    
      When tangling ‘xaringanthemer.Rmd’:
    Error: cannot open the connection
    Execution halted
    
      ‘ggplot2-themes.Rmd’ using ‘UTF-8’... OK
      ‘template-variables.Rmd’ using ‘UTF-8’... OK
      ‘xaringanthemer.Rmd’ using ‘UTF-8’... failed
    ```

# xpose

<details>

* Version: 0.4.18
* GitHub: https://github.com/UUPharmacometrics/xpose
* Source code: https://github.com/cran/xpose
* Date/Publication: 2024-02-01 16:20:02 UTC
* Number of recursive dependencies: 109

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
    ...
     1. ├─base (local) `<fn>`(x)
     2. ├─xpose:::print.xpose_plot(x)
     3. │ └─x %>% paginate(page_2_draw, page_tot) %>% print.ggplot(...)
     4. └─ggplot2:::print.ggplot(., ...)
     5.   ├─ggplot2::ggplot_gtable(data)
     6.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
     7.     └─ggplot2::calc_element("plot.margin", theme)
     8.       └─cli::cli_abort(...)
     9.         └─rlang::abort(...)
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
        8. └─ggplot2:::print.ggplot(., ...)
        9.   ├─ggplot2::ggplot_gtable(data)
       10.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
       11.     └─ggplot2::calc_element("plot.margin", theme)
       12.       └─cli::cli_abort(...)
       13.         └─rlang::abort(...)
      
      [ FAIL 4 | WARN 0 | SKIP 8 | PASS 510 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘customize_plots.Rmd’
      ...
    Using data from $prob no.1
    Filtering data by EVID == 0
    Using data from $prob no.1
    Filtering data by EVID == 0
    Using data from $prob no.1
    Filtering data by EVID == 0
    
    ...
      When sourcing ‘vpc.R’:
    Error: Theme element `plot.margin` must have class <margin/rel>.
    Execution halted
    
      ‘access_xpdb_data.Rmd’ using ‘UTF-8’... OK
      ‘customize_plots.Rmd’ using ‘UTF-8’... failed
      ‘import_model_outputs.Rmd’ using ‘UTF-8’... OK
      ‘introduction.Rmd’ using ‘UTF-8’... failed
      ‘multiple_pages.Rmd’ using ‘UTF-8’... failed
      ‘vpc.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘access_xpdb_data.Rmd’ using rmarkdown
    --- finished re-building ‘access_xpdb_data.Rmd’
    
    --- re-building ‘customize_plots.Rmd’ using rmarkdown
    
    Quitting from lines 36-42 [demo type scatter] (customize_plots.Rmd)
    Error: processing vignette 'customize_plots.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘customize_plots.Rmd’
    ...
    Error: processing vignette 'vpc.Rmd' failed with diagnostics:
    Theme element `plot.margin` must have class <margin/rel>.
    --- failed re-building ‘vpc.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘customize_plots.Rmd’ ‘introduction.Rmd’ ‘multiple_pages.Rmd’
      ‘vpc.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

