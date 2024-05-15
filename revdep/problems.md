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
    
    Quitting from lines  at lines 131-132 [plot] (actxps.Rmd)
    Error: processing vignette 'actxps.Rmd' failed with diagnostics:
    Internal error: adjust_color() expects an input of length 1
    --- failed re-building ‘actxps.Rmd’
    ...
    Quitting from lines  at lines 205-211 [trx-plot] (transactions.Rmd)
    Error: processing vignette 'transactions.Rmd' failed with diagnostics:
    Internal error: adjust_color() expects an input of length 1
    --- failed re-building ‘transactions.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘actxps.Rmd’ ‘misc.Rmd’ ‘transactions.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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
    Error: argument "theme" is missing, with no default
    Execution halted
    
      ‘my-vignette.Rmd’ using ‘UTF-8’... failed
    ```

# afex

<details>

* Version: 1.3-1
* GitHub: https://github.com/singmann/afex
* Source code: https://github.com/cran/afex
* Date/Publication: 2024-02-25 14:40:02 UTC
* Number of recursive dependencies: 227

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

# agricolaeplotr

<details>

* Version: 0.5.0
* GitHub: https://github.com/jensharbers/agricolaeplotr
* Source code: https://github.com/cran/agricolaeplotr
* Date/Publication: 2024-01-17 16:42:04 UTC
* Number of recursive dependencies: 144

Run `revdepcheck::cloud_details(, "agricolaeplotr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘agricolaeplotr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: sample_locations
    > ### Title: Sample Locations
    > ### Aliases: sample_locations
    > 
    > ### ** Examples
    > 
    > library(agricolaeplotr)
    ...
     16.                         └─ggplot2 (local) FUN(X[[i]], ...)
     17.                           └─base::lapply(...)
     18.                             └─ggplot2 (local) FUN(X[[i]], ...)
     19.                               └─g$draw_key(data, g$params, key_size)
     20.                                 └─ggplot2 (local) draw_key(...)
     21.                                   └─ggplot2::draw_key_polygon(data, params, size)
     22.                                     └─rlang:::Summary.quosure(from_theme(thin), 1.524, na.rm = FALSE)
     23.                                       └─rlang:::abort_quosure_op("Summary", .Generic)
     24.                                         └─rlang::abort(...)
    Execution halted
    ```

# ammistability

<details>

* Version: 0.1.4
* GitHub: https://github.com/ajaygpb/ammistability
* Source code: https://github.com/cran/ammistability
* Date/Publication: 2023-05-24 07:40:08 UTC
* Number of recursive dependencies: 70

Run `revdepcheck::cloud_details(, "ammistability")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Introduction.Rmd’ using rmarkdown_notangle
    ! Undefined control sequence.
    l.108 \NewDocumentCommand
                             \citeproctext{}{} 
    
    Error: processing vignette 'Introduction.Rmd' failed with diagnostics:
    LaTeX failed to compile /tmp/workdir/ammistability/new/ammistability.Rcheck/vign_test/ammistability/vignettes/Introduction.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See Introduction.log for more info.
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
    Trying to upgrade TinyTeX automatically now...
    If reinstallation fails, try install_tinytex() again. Then install the following packages:
    
    tinytex::tlmgr_install(c("amscls", "amsfonts", "amsmath", "atbegshi", "atveryend", "auxhook", "babel", "bibtex", "bigintcalc", "bitset", "booktabs", "cm", "ctablestack", "dehyph", "dvipdfmx", "dvips", "ec", "epstopdf-pkg", "etex", "etexcmds", "etoolbox", "euenc", "everyshi", "fancyvrb", "filehook", "firstaid", "float", "fontspec", "framed", "geometry", "gettitlestring", "glyphlist", "graphics", "graphics-cfg", "graphics-def", "helvetic", "hycolor", "hyperref", "hyph-utf8", "hyphen-base", "iftex", "inconsolata", "infwarerr", "intcalc", "knuth-lib", "kpathsea", "kvdefinekeys", "kvoptions", "kvsetkeys", "l3backend", "l3kernel", "l3packages", "latex", "latex-amsmath-dev", "latex-bin", "latex-fonts", "latex-tools-dev", "latexconfig", "latexmk", "letltxmacro", "lm", "lm-math", "ltxcmds", "lua-alt-getopt", "lua-uni-algos", "luahbtex", "lualatex-math", "lualibs", "luaotfload", "luatex", "luatexbase", "mdwtools", "metafont", "mfware", "modes", "natbib", "pdfescape", "pdftex", "pdftexcmds", "plain", "psnfss", "refcount", "rerunfilecheck", "scheme-infraonly", "selnolig", "stringenc", "symbol", "tex", "tex-ini-files", "texlive-scripts", "texlive.infra", "times", "tipa", "tools", "unicode-data", "unicode-math", "uniquecounter", "url", "xcolor", "xetex", "xetexconfig", "xkeyval", "xunicode", "zapfding"))
    
    The directory /opt/TinyTeX/texmf-local is not empty. It will be backed up to /tmp/RtmpCfJ2Ma/filed896b38a178 and restored later.
    
    tlmgr: no auxiliary texmf trees defined, so nothing removed
    ...
    
    Error: processing vignette 'Introduction.Rmd' failed with diagnostics:
    LaTeX failed to compile /tmp/workdir/ammistability/old/ammistability.Rcheck/vign_test/ammistability/vignettes/Introduction.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See Introduction.log for more info.
    --- failed re-building ‘Introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Introduction.Rmd’
    
    Error: Vignette re-building failed.
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
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: bar_plot ... use_defaults -> eval_from_theme -> %||% -> calc_element
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
    > 
    > animate <- anim_animate(plot)
    You can now pass it to gganimate::animate().
                       The recommended setting is nframes = 89
    > 
    > plotly::ggplotly(animate)
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: <Anonymous> ... use_defaults -> eval_from_theme -> %||% -> calc_element
    Execution halted
    ```

# aopdata

<details>

* Version: 1.0.3
* GitHub: https://github.com/ipeaGIT/aopdata
* Source code: https://github.com/cran/aopdata
* Date/Publication: 2023-08-31 07:20:02 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "aopdata")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘landuse_maps.Rmd’
      ...
    +     direction = 1 .... [TRUNCATED] 
    
      When sourcing ‘landuse_maps.R’:
    Error: Summary operations are not defined for quosures. Do you need to unquote
    the quosure?
    
    # Bad: min(myquosure)
    ...
    # Good: min(!!myquosure)
    Execution halted
    
      ‘access_inequality.Rmd’ using ‘UTF-8’... OK
      ‘access_maps.Rmd’ using ‘UTF-8’... OK
      ‘data_dic_en.Rmd’ using ‘UTF-8’... OK
      ‘data_dic_pt.Rmd’ using ‘UTF-8’... OK
      ‘intro_to_aopdata.Rmd’ using ‘UTF-8’... OK
      ‘landuse_maps.Rmd’ using ‘UTF-8’... failed
      ‘population_maps.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘access_inequality.Rmd’ using rmarkdown
    ```

# ARPALData

<details>

* Version: 1.5.2
* GitHub: NA
* Source code: https://github.com/cran/ARPALData
* Date/Publication: 2024-03-17 00:00:05 UTC
* Number of recursive dependencies: 141

Run `revdepcheck::cloud_details(, "ARPALData")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ARPALData-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: get_ARPA_Lombardia_zoning
    > ### Title: Download ARPA Lombardia zoning geometries
    > ### Aliases: get_ARPA_Lombardia_zoning
    > 
    > ### ** Examples
    > 
    > zones <- get_ARPA_Lombardia_zoning(plot_map = TRUE)
    ...
     16.                         └─ggplot2 (local) FUN(X[[i]], ...)
     17.                           └─base::lapply(...)
     18.                             └─ggplot2 (local) FUN(X[[i]], ...)
     19.                               └─g$draw_key(data, g$params, key_size)
     20.                                 └─ggplot2 (local) draw_key(...)
     21.                                   └─ggplot2::draw_key_polygon(data, params, size)
     22.                                     └─rlang:::Summary.quosure(from_theme(thin), 1.524, na.rm = FALSE)
     23.                                       └─rlang:::abort_quosure_op("Summary", .Generic)
     24.                                         └─rlang::abort(...)
    Execution halted
    ```

# asmbPLS

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/asmbPLS
* Date/Publication: 2023-04-17 09:50:05 UTC
* Number of recursive dependencies: 100

Run `revdepcheck::cloud_details(, "asmbPLS")` for more info

</details>

## Newly broken

*   checking whether package ‘asmbPLS’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘asmbPLS’
    See ‘/tmp/workdir/asmbPLS/new/asmbPLS.Rcheck/00install.out’ for details.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 37.6Mb
      sub-directories of 1Mb or more:
        data   2.1Mb
        libs  34.4Mb
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
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: autoplotly ... use_defaults -> eval_from_theme -> %||% -> calc_element
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
       10.               └─ggplot2 (local) compute_geom_2(..., self = self)
       11.                 └─self$geom$use_defaults(data, self$aes_params, modifiers, theme = theme)
       12.                   └─ggplot2 (local) use_defaults(..., self = self)
       13.                     └─ggplot2:::eval_from_theme(default_aes, theme)
       14.                       ├─calc_element("geom", theme) %||% .default_geom_element
       15.                       └─ggplot2::calc_element("geom", theme)
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 1 ]
      Error: Test failures
      Execution halted
    ```

# BayesGrowth

<details>

* Version: 1.0.0
* GitHub: https://github.com/jonathansmart/BayesGrowth
* Source code: https://github.com/cran/BayesGrowth
* Date/Publication: 2023-11-21 18:10:08 UTC
* Number of recursive dependencies: 110

Run `revdepcheck::cloud_details(, "BayesGrowth")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘MCMC-example.Rmd’
      ...
    2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 2.2
    ), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0.5, NULL, NULL, NULL, c(0, 2.2, 0, 2.2), NULL, TRUE), list("grey20", NULL, NULL, NULL, FALSE, TRUE), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 2.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(NULL, NA, NULL, NULL, TRUE), c(5.5, 5.5, 5.5, 5.5), 11, NULL, NULL, NULL, 1.2, NULL, NULL, 5.5, 
        NULL, NULL, NULL, NULL, 0.2, NULL, list(NULL, NULL, NULL, 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, c(0, 0, 0, 0), list(), 11, list("white", NA, NULL, NULL, TRUE), list(NA, "grey20", NULL, NULL, TRUE), 5.5, NULL, NULL, list("grey92", NULL, NULL, NULL, FALSE, TRUE), NULL, list(NULL, 0.5, NULL, NULL, FALSE, TRUE), NULL, NULL, 
        NULL, NULL, FALSE, list(NULL, "white", NULL, NULL, TRUE), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(5.5, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, TRUE), "topleft", NULL, c(5.5, 5.5, 5.5, 5.5), list("grey85", "grey20", NULL, NULL, TRUE), NULL, NULL, "inherit", "inside", list(NULL, 
            NULL, "grey10", 0.8, NULL, NULL, NULL, NULL, c(4.4, 4.4, 4.4, 4.4), NULL, TRUE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75))
    
      When sourcing ‘MCMC-example.R’:
    Error: unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 14, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, FALSE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 
    2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL,
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
      installed size is 109.3Mb
      sub-directories of 1Mb or more:
        libs  107.7Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# bdl

<details>

* Version: 1.0.5
* GitHub: https://github.com/statisticspoland/R_Package_to_API_BDL
* Source code: https://github.com/cran/bdl
* Date/Publication: 2023-02-24 15:00:02 UTC
* Number of recursive dependencies: 144

Run `revdepcheck::cloud_details(, "bdl")` for more info

</details>

## Newly broken

*   checking whether package ‘bdl’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘bdl’
    See ‘/tmp/workdir/bdl/new/bdl.Rcheck/00install.out’ for details.
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

*   checking examples ... ERROR
    ```
    Running examples in ‘BeeBDC-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: summaryMaps
    > ### Title: Create country-level summary maps of species and occurrence
    > ###   numbers
    > ### Aliases: summaryMaps
    > 
    > ### ** Examples
    > 
    ...
     23.                                     └─ggplot2 (local) FUN(X[[i]], ...)
     24.                                       └─base::lapply(...)
     25.                                         └─ggplot2 (local) FUN(X[[i]], ...)
     26.                                           └─g$draw_key(data, g$params, key_size)
     27.                                             └─ggplot2 (local) draw_key(...)
     28.                                               └─ggplot2::draw_key_polygon(data, params, size)
     29.                                                 └─rlang:::Summary.quosure(from_theme(thin), 1.524, na.rm = FALSE)
     30.                                                   └─rlang:::abort_quosure_op("Summary", .Generic)
     31.                                                     └─rlang::abort(...)
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
       28.                                               └─ggplot2::draw_key_polygon(data, params, size)
       29.                                                 └─rlang:::Summary.quosure(from_theme(thin), 1.524, na.rm = FALSE)
       30.                                                   └─rlang:::abort_quosure_op("Summary", .Generic)
       31.                                                     └─rlang::abort(...)
      
      [ FAIL 1 | WARN 4 | SKIP 0 | PASS 241 ]
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
    
    ...
    Error: Summary operations are not defined for quosures. Do you need to unquote
    the quosure?
    
    # Bad: min(myquosure)
    
    # Good: min(!!myquosure)
    Execution halted
    
      ‘BeeBDC_main.Rmd’ using ‘UTF-8’... failed
      ‘basic_workflow.Rmd’ using ‘UTF-8’... failed
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 107 marked UTF-8 strings
    ```

# blockCV

<details>

* Version: 3.1-3
* GitHub: https://github.com/rvalavi/blockCV
* Source code: https://github.com/cran/blockCV
* Date/Publication: 2023-06-04 13:20:02 UTC
* Number of recursive dependencies: 139

Run `revdepcheck::cloud_details(, "blockCV")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘tutorial_1.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘tutorial_1.Rmd’
      ...
    > cv_plot(cv = scv, x = pa_data)
    
      When sourcing ‘tutorial_1.R’:
    Error: Base operators are not defined for quosures. Do you need to unquote the
    quosure?
    
    # Bad: myquosure * rhs
    ...
    Error: Base operators are not defined for quosures. Do you need to unquote the
    quosure?
    
    # Bad: myquosure * rhs
    
    # Good: !!myquosure * rhs
    Execution halted
    
      ‘tutorial_1.Rmd’ using ‘UTF-8’... failed
      ‘tutorial_2.Rmd’ using ‘UTF-8’... failed
    ```

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'cv_spatial.Rd':
      ‘[biomod2]{BIOMOD_cv}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        doc       1.9Mb
        extdata   1.9Mb
        libs      1.4Mb
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
       26.               └─ggplot2 (local) compute_geom_2(..., self = self)
       27.                 └─self$geom$use_defaults(data, self$aes_params, modifiers, theme = theme)
       28.                   └─ggplot2 (local) use_defaults(..., self = self)
       29.                     └─ggplot2:::eval_from_theme(default_aes, theme)
       30.                       ├─calc_element("geom", theme) %||% .default_geom_element
       31.                       └─ggplot2::calc_element("geom", theme)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 25 ]
      Error: Test failures
      Execution halted
    ```

# bSi

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/bSi
* Date/Publication: 2024-01-24 15:52:57 UTC
* Number of recursive dependencies: 99

Run `revdepcheck::cloud_details(, "bSi")` for more info

</details>

## Newly broken

*   checking whether package ‘bSi’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘bSi’
    See ‘/tmp/workdir/bSi/new/bSi.Rcheck/00install.out’ for details.
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
    Warning: Use of `x$linkcum` is discouraged.
    ℹ Use `linkcum` instead.
    Warning: Use of `x$flowcum` is discouraged.
    ℹ Use `flowcum` instead.
    Warning: Use of `x$flowcum` is discouraged.
    ℹ Use `flowcum` instead.
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: flowgini ... use_defaults -> eval_from_theme -> %||% -> calc_element
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
    + cohort_offset = cohort_offset, sr_first_pos = sr_first_pos,
    + missing_prob = missing_prob, cohort_fixed = cohort_fixed, accrual_type = accrual_type,
    + accrual_param = accrual_param, hist_lag = hist_lag, analysis_times = analysis_times,
    + time_trend = time_trend, cohorts_start = cohorts_start, cohorts_sim = cohorts_sim,
    + iter = 2, coresnum = 1, save = FALSE, ret_list = TRUE, plot_ocs = TRUE
    + )
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: trial_ocs ... use_defaults -> eval_from_theme -> %||% -> calc_element
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
       23.             └─ggplot2 (local) compute_geom_2(..., self = self)
       24.               └─self$geom$use_defaults(data, self$aes_params, modifiers, theme = theme)
       25.                 └─ggplot2 (local) use_defaults(..., self = self)
       26.                   └─ggplot2:::eval_from_theme(default_aes, theme)
       27.                     ├─calc_element("geom", theme) %||% .default_geom_element
       28.                     └─ggplot2::calc_element("geom", theme)
      
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
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: make_barplot ... use_defaults -> eval_from_theme -> %||% -> calc_element
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘chronicle.Rmd’
      ...
    +     filename = "quick_demo", title = "A quick chronicle demo", 
    +     author =  .... [TRUNCATED] 
    
    Quitting from lines  at lines 34-46 [unnamed-chunk-3] (quick_demo.Rmd)
    
      When sourcing ‘chronicle.R’:
    Error: ℹ In index: 1.
    Caused by error in `compute_geom_2()`:
    ! argument "theme" is missing, with no default
    Execution halted
    
      ‘chronicle.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘chronicle.Rmd’ using rmarkdown
    
    Quitting from lines  at lines 34-46 [unnamed-chunk-3] (quick_demo.Rmd)
    Error: processing vignette 'chronicle.Rmd' failed with diagnostics:
    ℹ In index: 1.
    Caused by error in `compute_geom_2()`:
    ! argument "theme" is missing, with no default
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

* Version: 1.5.1
* GitHub: https://github.com/openanalytics/clinDataReview
* Source code: https://github.com/cran/clinDataReview
* Date/Publication: 2024-04-24 20:10:03 UTC
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
    + 	xVar = "ADY",
    + 	yVar = "LBSTRESN",
    + 	aesPointVar = list(color = "TRTP", fill = "TRTP"),
    + 	aesLineVar = list(group = "USUBJID", color = "TRTP"),
    + 	labelVars = labelVars
    + )
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: scatterplotClinData ... use_defaults -> eval_from_theme -> %||% -> calc_element
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
        adding: report_dependencies1dcd775cc9bd/ (stored 0%)
        adding: report_dependencies1dcd775cc9bd/file1dcd27c26d11.html (deflated 8%)
    ...
        9.               └─ggplot2 (local) compute_geom_2(..., self = self)
       10.                 └─self$geom$use_defaults(data, self$aes_params, modifiers, theme = theme)
       11.                   └─ggplot2 (local) use_defaults(..., self = self)
       12.                     └─ggplot2:::eval_from_theme(default_aes, theme)
       13.                       ├─calc_element("geom", theme) %||% .default_geom_element
       14.                       └─ggplot2::calc_element("geom", theme)
      
      [ FAIL 23 | WARN 8 | SKIP 30 | PASS 450 ]
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
    
    Quitting from lines  at lines 167-208 [timeProfiles] (clinDataReview-dataVisualization.Rmd)
    Error: processing vignette 'clinDataReview-dataVisualization.Rmd' failed with diagnostics:
    argument "theme" is missing, with no default
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
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        doc   4.3Mb
    ```

# clinUtils

<details>

* Version: 0.1.5
* GitHub: https://github.com/openanalytics/clinUtils
* Source code: https://github.com/cran/clinUtils
* Date/Publication: 2024-04-23 20:50:31 UTC
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
    Error: argument "theme" is missing, with no default
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
    <script type="application/json" data-for="htmlwidget-83ab763989048a4e00a9">{"x":{"data":[{"orientation":"v","width":0.45000000000000018,"base":0,"x":[2.7749999999999999],"y":[1],"text":"count:  1<br />ACTARM: Xanomeline Low Dose<br />LBNRIND: ABNORMAL","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(248,118,109,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"name":"ABNORMAL","legendgroup":"ABNORMAL","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.89999999999999991,0.90000000000000013,0.45000000000000018],"base":[0,0,0],"x":[1,2,3.2250000000000001],"y":[5,10,6],"text":["count:  5<br />ACTARM: Placebo<br />LBNRIND: NORMAL","count: 10<br />ACTARM: Xanomeline High Dose<br />LBNRIND: NORMAL","count:  6<br />ACTARM: Xanomeline Low Dose<br />LBNRIND: NORMAL"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(0,191,196,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"name":"NORMAL","legendgroup":"NORMAL","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.89999999999999991,0.90000000000000013,0.90000000000000036],"base":[0,0,0],"x":[1,2,3],"y":[5,10,7],"text":["count:  5<br />ACTARM: Placebo<br />LBNRIND: NORMAL","count: 10<br />ACTARM: Xanomeline High Dose<br />LBNRIND: NORMAL","count:  7<br />ACTARM: Xanomeline Low Dose<br />LBNRIND: NORMAL"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(0,191,196,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"name":"NORMAL","legendgroup":"NORMAL","showlegend":false,"xaxis":"x2","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":34.995433789954348,"r":7.3059360730593621,"b":87.284551874724187,"l":48.949771689497723},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"xaxis":{"domain":[0,0.48912807131985214],"automargin":true,"type":"linear","autorange":false,"range":[0.40000000000000002,3.6000000000000001],"tickmode":"array","ticktext":["Placebo","Xanomeline High Dose","Xanomeline Low Dose"],"tickvals":[1,2,3],"categoryorder":"array","categoryarray":["Placebo","Xanomeline High Dose","Xanomeline Low Dose"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.6529680365296811,"tickwidth":0.66417600664176002,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":45,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"y","title":"","hoverformat":".2f"},"annotations":[{"text":"ACTARM","x":0.5,"y":0,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"top","annotationType":"axis","yshift":-71.942086121299511},{"text":"count","x":0,"y":0.5,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"xref":"paper","yref":"paper","textangle":-90,"xanchor":"right","yanchor":"center","annotationType":"axis","xshift":-33.607305936073068},{"text":"Ketones","x":0.24456403565992607,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.68949771689498},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"pH","x":0.75543596434007387,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.68949771689498},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"}],"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-0.5,10.5],"tickmode":"array","ticktext":["0.0","2.5","5.0","7.5","10.0"],"tickvals":[0,2.5,5,7.5,10],"categoryorder":"array","categoryarray":["0.0","2.5","5.0","7.5","10.0"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.6529680365296811,"tickwidth":0.66417600664176002,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"x","title":"","hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":0.48912807131985214,"y0":0,"y1":1},{"type":"rect","fillcolor":"rgba(217,217,217,1)","line":{"color":"transparent","width":0.66417600664176002,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":0.48912807131985214,"y0":0,"y1":23.37899543378996,"yanchor":1,"ysizemode":"pixel"},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0.51087192868014786,"x1":1,"y0":0,"y1":1},{"type":"rect","fillcolor":"rgba(217,217,217,1)","line":{"color":"transparent","width":0.66417600664176002,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.51087192868014786,"x1":1,"y0":0,"y1":23.37899543378996,"yanchor":1,"ysizemode":"pixel"}],"xaxis2":{"type":"linear","autorange":false,"range":[0.40000000000000002,3.6000000000000001],"tickmode":"array","ticktext":["Placebo","Xanomeline High Dose","Xanomeline Low Dose"],"tickvals":[1,2,3],"categoryorder":"array","categoryarray":["Placebo","Xanomeline High Dose","Xanomeline Low Dose"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.6529680365296811,"tickwidth":0.66417600664176002,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":45,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0.51087192868014786,1],"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"y","title":"","hoverformat":".2f"},"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.8897637795275593,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.68949771689498},"title":{"text":"LBNRIND","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"164d49f3d8b9":{"x":{},"fill":{},"type":"bar"}},"cur_data":"164d49f3d8b9","visdat":{"164d49f3d8b9":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
    
    > knitPrintListPlots(plotsList = listPlotsInteractiveLB, 
    +     generalLabel = "lab-hist-interactive", type = "plotly", titles = simpleCap(tolower(nam .... [TRUNCATED] 
    
    Quitting from lines  at lines 2-4 [lab-hist-interactive1]
    
      When sourcing ‘clinUtils-vignette.R’:
    Error: there is no package called 'webshot'
    Execution halted
    
      ‘clinUtils-vignette.Rmd’ using ‘UTF-8’... failed
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.9Mb
      sub-directories of 1Mb or more:
        doc   6.5Mb
    ```

# ClusROC

<details>

* Version: 1.0.2
* GitHub: https://github.com/toduckhanh/ClusROC
* Source code: https://github.com/cran/ClusROC
* Date/Publication: 2022-11-17 15:00:02 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::cloud_details(, "ClusROC")` for more info

</details>

## Newly broken

*   checking whether package ‘ClusROC’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘ClusROC’
    See ‘/tmp/workdir/ClusROC/new/ClusROC.Rcheck/00install.out’ for details.
    ```

# clustEff

<details>

* Version: 0.3.1
* GitHub: NA
* Source code: https://github.com/cran/clustEff
* Date/Publication: 2024-01-23 08:52:55 UTC
* Number of recursive dependencies: 136

Run `revdepcheck::cloud_details(, "clustEff")` for more info

</details>

## Newly broken

*   checking whether package ‘clustEff’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘clustEff’
    See ‘/tmp/workdir/clustEff/new/clustEff.Rcheck/00install.out’ for details.
    ```

# coda4microbiome

<details>

* Version: 0.2.3
* GitHub: https://github.com/malucalle/coda4microbiome
* Source code: https://github.com/cran/coda4microbiome
* Date/Publication: 2024-02-21 08:30:06 UTC
* Number of recursive dependencies: 136

Run `revdepcheck::cloud_details(, "coda4microbiome")` for more info

</details>

## Newly broken

*   checking whether package ‘coda4microbiome’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘coda4microbiome’
    See ‘/tmp/workdir/coda4microbiome/new/coda4microbiome.Rcheck/00install.out’ for details.
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
    + sr_drugs_pos = sr_drugs_pos, target_rr = target_rr, sharing_type = sharing_type,
    + safety_prob = safety_prob, Bayes_Sup = Bayes_Sup, prob_rr_transform = prob_rr_transform,
    + cohort_offset = cohort_offset, Bayes_Fut = Bayes_Fut, sr_first_pos = sr_first_pos
    + )
    > 
    > plot_trial(res_list, unit = "n")
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: plot_trial ... use_defaults -> eval_from_theme -> %||% -> calc_element
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
    Error: argument "theme" is missing, with no default
    Execution halted
    
      ‘my-vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘my-vignette.Rmd’ using rmarkdown
    
    Quitting from lines  at lines 1043-1073 [unnamed-chunk-20] (my-vignette.Rmd)
    Error: processing vignette 'my-vignette.Rmd' failed with diagnostics:
    argument "theme" is missing, with no default
    --- failed re-building ‘my-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘my-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# CompAREdesign

<details>

* Version: 2.3.1
* GitHub: NA
* Source code: https://github.com/cran/CompAREdesign
* Date/Publication: 2024-02-15 13:00:02 UTC
* Number of recursive dependencies: 90

Run `revdepcheck::cloud_details(, "CompAREdesign")` for more info

</details>

## Newly broken

*   checking whether package ‘CompAREdesign’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘CompAREdesign’
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
    + )
    Warning encountered during diversity analysis:you have empty rows: their dissimilarities may be
                     meaningless in method “bray”
    > #To run grouped bar plot function
    > plot_group_bar <- group_bar_plots(core_1$final_otu_table_bef_filter,
    + core_1$final_otu_aft_filter, 10)
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: group_bar_plots ... use_defaults -> eval_from_theme -> %||% -> calc_element
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
       10.               └─ggplot2 (local) compute_geom_2(..., self = self)
       11.                 └─self$geom$use_defaults(data, self$aes_params, modifiers, theme = theme)
       12.                   └─ggplot2 (local) use_defaults(..., self = self)
       13.                     └─ggplot2:::eval_from_theme(default_aes, theme)
       14.                       ├─calc_element("geom", theme) %||% .default_geom_element
       15.                       └─ggplot2::calc_element("geom", theme)
      
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
* Number of recursive dependencies: 140

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
    ℹ Please consider using `annotate()` or provide this layer with data containing
      a single row.
    Warning in geom_text(data = solar_system, aes(x = 0, y = 0, label = sun),  :
      All aesthetics have length 1, but the data has 250 rows.
    ℹ Please consider using `annotate()` or provide this layer with data containing
      a single row.
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: animSolar ... use_defaults -> eval_from_theme -> %||% -> calc_element
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
    Error: argument "theme" is missing, with no default
    Execution halted
    
      ‘corrViz.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘corrViz.Rmd’ using rmarkdown
    
    Quitting from lines  at lines 76-81 [heatmap] (corrViz.Rmd)
    Error: processing vignette 'corrViz.Rmd' failed with diagnostics:
    argument "theme" is missing, with no default
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
    
    Quitting from lines  at lines 38-45 [unnamed-chunk-1] (covidcast.Rmd)
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

# Coxmos

<details>

* Version: 1.0.2
* GitHub: https://github.com/BiostatOmics/Coxmos
* Source code: https://github.com/cran/Coxmos
* Date/Publication: 2024-03-25 20:32:38 UTC
* Number of recursive dependencies: 204

Run `revdepcheck::cloud_details(, "Coxmos")` for more info

</details>

## Newly broken

*   checking Rd files ... WARNING
    ```
    prepare_Rd: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘survminer’
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Coxmos-pipeline.Rmd’
      ...
    Warning in data("X_proteomic") : data set ‘X_proteomic’ not found
    
    > data("Y_proteomic")
    Warning in data("Y_proteomic") : data set ‘Y_proteomic’ not found
    
    > X <- X_proteomic
    
      When sourcing ‘Coxmos-pipeline.R’:
    Error: object 'X_proteomic' not found
    Execution halted
    
      ‘Coxmos-MO-pipeline.Rmd’ using ‘UTF-8’... OK
      ‘Coxmos-pipeline.Rmd’ using ‘UTF-8’... failed
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        data   2.1Mb
        doc    2.9Mb
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
     15. │           └─self$geom$use_defaults(data, self$aes_params, modifiers, theme = theme)
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
* Number of recursive dependencies: 95

Run `revdepcheck::cloud_details(, "csa")` for more info

</details>

## Newly broken

*   checking whether package ‘csa’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘csa’
    See ‘/tmp/workdir/csa/new/csa.Rcheck/00install.out’ for details.
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
       10.               └─ggplot2 (local) compute_geom_2(..., self = self)
       11.                 └─self$geom$use_defaults(data, self$aes_params, modifiers, theme = theme)
       12.                   └─ggplot2 (local) use_defaults(..., self = self)
       13.                     └─ggplot2:::eval_from_theme(default_aes, theme)
       14.                       ├─calc_element("geom", theme) %||% .default_geom_element
       15.                       └─ggplot2::calc_element("geom", theme)
      
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
    when running code in ‘cb6interactive.Rmd’
      ...
    +     y .... [TRUNCATED] 
    Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ℹ Please use `linewidth` instead.
    
    > ts_interactive <- ggplotly(ts_static, width = 600, 
    +     height = 300) %>% highlight(on = "plotly_selected", opacityDim = 0.012)
    
    ...
      When sourcing ‘cb6interactive.R’:
    Error: argument "theme" is missing, with no default
    Execution halted
    
      ‘cb1class.Rmd’ using ‘UTF-8’... OK
      ‘cb2create.Rmd’ using ‘UTF-8’... OK
      ‘cb3tsibblesf.Rmd’ using ‘UTF-8’... OK
      ‘cb4glyph.Rmd’ using ‘UTF-8’... OK
      ‘cb5match.Rmd’ using ‘UTF-8’... OK
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
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        data   3.0Mb
        doc    1.3Mb
    ```

# dafishr

<details>

* Version: 1.0.0
* GitHub: https://github.com/CBMC-GCMP/dafishr
* Source code: https://github.com/cran/dafishr
* Date/Publication: 2022-12-06 13:10:02 UTC
* Number of recursive dependencies: 114

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
     17.                           └─ggplot2 (local) FUN(X[[i]], ...)
     18.                             └─g$draw_key(data, g$params, key_size)
     19.                               └─ggplot2 (local) draw_key(...)
     20.                                 └─ggplot2::draw_key_point(data, params, size)
     21.                                   ├─grid::pointsGrob(...)
     22.                                   │ └─grid::grob(...)
     23.                                   └─ggplot2::ggpar(...)
     24.                                     └─rlang:::Ops.quosure(pointsize, .pt)
     25.                                       └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.9Mb
      sub-directories of 1Mb or more:
        data   7.6Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2020 marked UTF-8 strings
    ```

# damAOI

<details>

* Version: 0.0
* GitHub: NA
* Source code: https://github.com/cran/damAOI
* Date/Publication: 2024-02-07 18:00:02 UTC
* Number of recursive dependencies: 101

Run `revdepcheck::cloud_details(, "damAOI")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘damAOI.Rmd’
      ...
    +     ggplot2::aes(fill = as.factor(area)), alpha = 0.3) + ggplot2::geom_sf(data = bufferandcli .... [TRUNCATED] 
    
      When sourcing ‘damAOI.R’:
    Error: Summary operations are not defined for quosures. Do you need to unquote
    the quosure?
    
    # Bad: min(myquosure)
    
    # Good: min(!!myquosure)
    Execution halted
    
      ‘damAOI.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘damAOI.Rmd’ using rmarkdown
    ```

# deeptime

<details>

* Version: 1.1.1
* GitHub: https://github.com/willgearty/deeptime
* Source code: https://github.com/cran/deeptime
* Date/Publication: 2024-03-08 17:10:10 UTC
* Number of recursive dependencies: 181

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
      6. │     └─ggplot2 (local) setup(..., self = self)
      7. │       └─self$facet$compute_layout(data, self$facet_params)
      8. │         └─ggplot2 (local) compute_layout(..., self = self)
      9. │           └─ggplot2:::wrap_layout(id, dims, params$dir)
     10. │             └─ggplot2:::data_frame0(...)
     11. │               └─vctrs::data_frame(..., .name_repair = "minimal")
     12. └─vctrs:::stop_recycle_incompatible_size(...)
     13.   └─vctrs:::stop_vctrs(...)
     14.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
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
      • gggeo_scale/gggeo-scale-top-new.svg
      • gggeo_scale/gggeo-scale-top-old.svg
      • points_range/geom-points-range-aes-new.svg
      • points_range/geom-points-range-aes-old.svg
      • points_range/geom-points-range-bg-new.svg
      • points_range/geom-points-range-bg-old.svg
      • points_range/geom-points-range-h-new.svg
      • points_range/geom-points-range-h-old.svg
      Error: Test failures
      Execution halted
    ```

# DEGRE

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/DEGRE
* Date/Publication: 2022-11-02 09:32:57 UTC
* Number of recursive dependencies: 89

Run `revdepcheck::cloud_details(, "DEGRE")` for more info

</details>

## Newly broken

*   checking whether package ‘DEGRE’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘DEGRE’
    See ‘/tmp/workdir/DEGRE/new/DEGRE.Rcheck/00install.out’ for details.
    ```

# densityarea

<details>

* Version: 0.1.0
* GitHub: https://github.com/JoFrhwld/densityarea
* Source code: https://github.com/cran/densityarea
* Date/Publication: 2023-10-02 10:20:06 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "densityarea")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘densityarea.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘sf-operations.Rmd’
      ...
    > vowel_intersections <- relocate(mutate(vowel_intersections, 
    +     groups = map_chr(origins, .f = new_label, labels = vowel_polygons$plt_vclass)), 
     .... [TRUNCATED] 
    
      When sourcing ‘sf-operations.R’:
    Error: ℹ In argument: `groups = map_chr(origins, .f = new_label, labels =
      vowel_polygons$plt_vclass)`.
    Caused by error:
    ! object 'new_label' not found
    Execution halted
    
      ‘densityarea.Rmd’ using ‘UTF-8’... OK
      ‘sf-operations.Rmd’ using ‘UTF-8’... failed
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2534 marked UTF-8 strings
    ```

# did

<details>

* Version: 2.1.2
* GitHub: https://github.com/bcallaway11/did
* Source code: https://github.com/cran/did
* Date/Publication: 2022-07-20 16:00:05 UTC
* Number of recursive dependencies: 125

Run `revdepcheck::cloud_details(, "did")` for more info

</details>

## Newly broken

*   checking whether package ‘did’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘did’
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
     15. │           └─self$geom$use_defaults(data, self$aes_params, modifiers, theme = theme)
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
    16     4        D     8                          32 0.2500000
    > # through hovering the cursor over the relevant parts of the plot
    > if (requireNamespace("plotly", quietly = TRUE)) {
    +     barPlot(example_df, "clustering", group.by = "groups",
    +         do.hover = TRUE)
    +     }
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: barPlot ... use_defaults -> eval_from_theme -> %||% -> calc_element
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
       12.                   └─ggplot2 (local) compute_geom_2(..., self = self)
       13.                     └─self$geom$use_defaults(data, self$aes_params, modifiers, theme = theme)
       14.                       └─ggplot2 (local) use_defaults(..., self = self)
       15.                         └─ggplot2:::eval_from_theme(default_aes, theme)
       16.                           ├─calc_element("geom", theme) %||% .default_geom_element
       17.                           └─ggplot2::calc_element("geom", theme)
      
      [ FAIL 12 | WARN 12 | SKIP 0 | PASS 307 ]
      Error: Test failures
      Execution halted
    ```

# dots

<details>

* Version: 0.0.2
* GitHub: https://github.com/christopherkenny/dots
* Source code: https://github.com/cran/dots
* Date/Publication: 2022-07-15 08:40:07 UTC
* Number of recursive dependencies: 99

Run `revdepcheck::cloud_details(, "dots")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘dots-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: dots
    > ### Title: Make dot density plots
    > ### Aliases: dots
    > 
    > ### ** Examples
    > 
    > data('suffolk')
    ...
     17.                           └─ggplot2 (local) FUN(X[[i]], ...)
     18.                             └─g$draw_key(data, g$params, key_size)
     19.                               └─ggplot2 (local) draw_key(...)
     20.                                 └─ggplot2::draw_key_point(data, params, size)
     21.                                   ├─grid::pointsGrob(...)
     22.                                   │ └─grid::grob(...)
     23.                                   └─ggplot2::ggpar(...)
     24.                                     └─rlang:::Ops.quosure(pointsize, .pt)
     25.                                       └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘making_dot_density_maps.Rmd’
      ...
    > dots::dots(shp = suffolk, cols = vap_hisp)
    
      When sourcing ‘making_dot_density_maps.R’:
    Error: Base operators are not defined for quosures. Do you need to unquote the
    quosure?
    
    # Bad: myquosure * rhs
    
    # Good: !!myquosure * rhs
    Execution halted
    
      ‘making_dot_density_maps.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘making_dot_density_maps.Rmd’ using rmarkdown
    
    Quitting from lines  at lines 50-51 [unnamed-chunk-3] (making_dot_density_maps.Rmd)
    Error: processing vignette 'making_dot_density_maps.Rmd' failed with diagnostics:
    Base operators are not defined for quosures. Do you need to unquote the
    quosure?
    
    # Bad: myquosure * rhs
    
    # Good: !!myquosure * rhs
    --- failed re-building ‘making_dot_density_maps.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘making_dot_density_maps.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

# eks

<details>

* Version: 1.0.5
* GitHub: NA
* Source code: https://github.com/cran/eks
* Date/Publication: 2024-05-01 23:24:46 UTC
* Number of recursive dependencies: 89

Run `revdepcheck::cloud_details(, "eks")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘eks-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: tidyst_kms
    > ### Title: Tidy and geospatial kernel mean shift clustering
    > ### Aliases: tidy_kms st_kms
    > ### Keywords: smooth
    > 
    > ### ** Examples
    > 
    ...
     17.                           └─ggplot2 (local) FUN(X[[i]], ...)
     18.                             └─g$draw_key(data, g$params, key_size)
     19.                               └─ggplot2 (local) draw_key(...)
     20.                                 └─ggplot2::draw_key_point(data, params, size)
     21.                                   ├─grid::pointsGrob(...)
     22.                                   │ └─grid::grob(...)
     23.                                   └─ggplot2::ggpar(...)
     24.                                     └─rlang:::Ops.quosure(pointsize, .pt)
     25.                                       └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘tidysf_kde.Rmd’
      ...
    +     scale_fill_discrete_sequential(h1 = 275) + coord_sf(xlim = .... [TRUNCATED] 
    
      When sourcing ‘tidysf_kde.R’:
    Error: Summary operations are not defined for quosures. Do you need to unquote
    the quosure?
    
    # Bad: min(myquosure)
    
    # Good: min(!!myquosure)
    Execution halted
    
      ‘tidysf_kde.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘tidysf_kde.Rmd’ using rmarkdown
    ```

# entropart

<details>

* Version: 1.6-13
* GitHub: https://github.com/EricMarcon/entropart
* Source code: https://github.com/cran/entropart
* Date/Publication: 2023-09-26 14:40:02 UTC
* Number of recursive dependencies: 122

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
     15.             └─self$geom$use_defaults(data, self$aes_params, modifiers, theme = theme)
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
    
    Quitting from lines  at lines 53-55 [PlotN18] (entropart.Rmd)
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
     15. │           └─self$geom$use_defaults(data, self$aes_params, modifiers, theme = theme)
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

# epiR

<details>

* Version: 2.0.74
* GitHub: NA
* Source code: https://github.com/cran/epiR
* Date/Publication: 2024-04-27 12:30:02 UTC
* Number of recursive dependencies: 125

Run `revdepcheck::cloud_details(, "epiR")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘epiR_descriptive.Rmd’
      ...
    +     fill = "tra ..." ... [TRUNCATED] 
    
      When sourcing ‘epiR_descriptive.R’:
    Error: Base operators are not defined for quosures. Do you need to unquote the
    quosure?
    
    # Bad: myquosure * rhs
    
    # Good: !!myquosure * rhs
    Execution halted
    
      ‘epiR_descriptive.Rmd’... failed
      ‘epiR_measures_of_association.Rmd’... OK
      ‘epiR_sample_size.Rmd’... OK
      ‘epiR_surveillance.Rmd’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘epiR_descriptive.Rmd’ using rmarkdown
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
    2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 2.2
    ), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0.5, NULL, NULL, NULL, c(0, 2.2, 0, 2.2), NULL, TRUE), list("grey20", NULL, NULL, NULL, FALSE, TRUE), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 2.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list("black", 1, NULL, NULL, FALSE, TRUE), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(NULL, NA, NULL, NULL, TRUE), c(5.5, 5.5, 5.5, 5.5), 11, NULL, 
        NULL, NULL, 1.2, NULL, NULL, 5.5, NULL, NULL, NULL, NULL, 0.2, NULL, list(NULL, NULL, NULL, 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, c(0, 0, 0, 0), list(), 11, list("white", NA, NULL, NULL, TRUE), list(), 5.5, NULL, NULL, list("grey92", NULL, NULL, NULL, FALSE, TRUE), list(), list(), NULL, NULL, NULL, NULL, FALSE, list(NULL, 
            "white", NULL, NULL, TRUE), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(5.5, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, TRUE), "topleft", NULL, c(5.5, 5.5, 5.5, 5.5), list("white", "black", 2, NULL, TRUE), NULL, NULL, "inherit", "inside", list(NULL, NULL, "grey10", 0.8, NULL, 
            NULL, NULL, NULL, c(4.4, 4.4, 4.4, 4.4), NULL, TRUE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75))
    Error in use_defaults(..., self = self) : 
      unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 
    2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL
    Calls: <Anonymous> ... .handleSimpleError -> h -> <Anonymous> -> <Anonymous>
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
       17. │             └─self$geom$use_defaults(data, self$aes_params, modifiers, theme = theme)
       18. └─base::.handleSimpleError(...)
       19.   └─rlang (local) h(simpleError(msg, call))
       20.     └─handlers[[1L]](cnd)
       21.       └─cli::cli_abort(...)
       22.         └─rlang::abort(...)
      
      [ FAIL 14 | WARN 15 | SKIP 0 | PASS 3182 ]
      Error: Test failures
      Execution halted
    ```

# evalITR

<details>

* Version: 1.0.0
* GitHub: https://github.com/MichaelLLi/evalITR
* Source code: https://github.com/cran/evalITR
* Date/Publication: 2023-08-25 23:10:06 UTC
* Number of recursive dependencies: 168

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

# explainer

<details>

* Version: 1.0.1
* GitHub: https://github.com/PERSIMUNE/explainer
* Source code: https://github.com/cran/explainer
* Date/Publication: 2024-04-18 09:00:02 UTC
* Number of recursive dependencies: 193

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
    > myplot <- eDecisionCurve(
    +   task = maintask,
    +   trained_model = mylrn,
    +   splits = splits,
    +   seed = seed
    + )
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: eDecisionCurve ... use_defaults -> eval_from_theme -> %||% -> calc_element
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ggpmisc’
      All declared Imports should be used.
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
    2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 2.2
    ), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0.5, NULL, NULL, NULL, c(0, 2.2, 0, 2.2), NULL, TRUE), list("grey20", NULL, NULL, NULL, FALSE, TRUE), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 2.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(NULL, NA, NULL, NULL, TRUE), c(5.5, 5.5, 5.5, 5.5), 11, NULL, NULL, NULL, 1.2, NULL, NULL, 5.5, 
        NULL, NULL, NULL, NULL, 0.2, NULL, list(NULL, NULL, NULL, 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, c(0, 0, 0, 0), list(), 11, list("grey92", NA, NULL, NULL, TRUE), list(), 5.5, NULL, NULL, list("white", NULL, NULL, NULL, FALSE, TRUE), NULL, list(NULL, 0.5, NULL, NULL, FALSE, TRUE), NULL, NULL, NULL, NULL, FALSE, list(NULL, 
            "white", NULL, NULL, TRUE), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(5.5, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, TRUE), "topleft", NULL, c(5.5, 5.5, 5.5, 5.5), list("grey85", NA, NULL, NULL, TRUE), NULL, NULL, "inherit", "inside", list(NULL, NULL, "grey10", 0.8, NULL, 
            NULL, NULL, NULL, c(4.4, 4.4, 4.4, 4.4), NULL, TRUE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75))
    
      When sourcing ‘intro.R’:
    Error: unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 
    2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, 
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
    2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 2.2
    ), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0.5, NULL, NULL, NULL, c(0, 2.2, 0, 2.2), NULL, TRUE), list("grey20", NULL, NULL, NULL, FALSE, TRUE), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 2.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(NULL, NA, NULL, NULL, TRUE), c(5.5, 5.5, 5.5, 5.5), 11, NULL, NULL, NULL, 1.2, NULL, NULL, 5.5, 
        NULL, NULL, NULL, NULL, 0.2, NULL, list(NULL, NULL, NULL, 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, c(0, 0, 0, 0), list(), 11, list("grey92", NA, NULL, NULL, TRUE), list(), 5.5, NULL, NULL, list("white", NULL, NULL, NULL, FALSE, TRUE), NULL, list(NULL, 0.5, NULL, NULL, FALSE, TRUE), NULL, NULL, NULL, NULL, FALSE, list(NULL, 
            "white", NULL, NULL, TRUE), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(5.5, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, TRUE), "topleft", NULL, c(5.5, 5.5, 5.5, 5.5), list("grey85", NA, NULL, NULL, TRUE), NULL, NULL, "inherit", "inside", list(NULL, NULL, "grey10", 0.8, NULL, 
            NULL, NULL, NULL, c(4.4, 4.4, 4.4, 4.4), NULL, TRUE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75))
    Error in use_defaults(..., self = self) : 
      unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 
    2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL
    Calls: <Anonymous> ... .handleSimpleError -> h -> <Anonymous> -> <Anonymous>
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
       32. │                               │ └─base::withCallingHandlers(...)
       33. │                               └─layer$geom$use_defaults(...)
       34. └─base::.handleSimpleError(...)
       35.   └─rlang (local) h(simpleError(msg, call))
       36.     └─handlers[[1L]](cnd)
       37.       └─layer$geom$use_defaults(...)
      
      [ FAIL 2 | WARN 0 | SKIP 1 | PASS 269 ]
      Error: Test failures
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
    ...
    2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 2.2), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0.5, NULL, NULL, NULL, 
        c(0, 2.2, 0, 2.2), NULL, TRUE), list("grey20", NULL, NULL, NULL, FALSE, TRUE), NULL, NULL, NULL, list(), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 2.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(NULL, NA, NULL, NULL, TRUE), c(5.5, 5.5, 5.5, 5.5), 11, NULL, NULL, NULL, 1.2, NULL, NULL, 5.5, NULL, NULL, NULL, NULL, 0.2, NULL, list(NULL, NULL, NULL, 0.8, NULL, 
        NULL, NULL, NULL, NULL, NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, c(0, 0, 0, 0), list(), 11, list("grey92", NA, NULL, NULL, TRUE), list(), 5.5, NULL, NULL, list("white", NULL, NULL, NULL, FALSE, TRUE), NULL, list(NULL, 0.5, NULL, NULL, FALSE, TRUE), NULL, NULL, NULL, NULL, FALSE, list(NULL, "white", NULL, NULL, TRUE), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, 
        NULL, c(0, 0, 5.5, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(5.5, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, TRUE), "topleft", NULL, c(5.5, 5.5, 5.5, 5.5), list("grey85", NA, NULL, NULL, TRUE), NULL, NULL, "inherit", "inside", list(NULL, NULL, "grey10", 0.8, NULL, NULL, NULL, NULL, c(4.4, 4.4, 4.4, 4.4), NULL, TRUE), NULL, NULL, NULL, 
        list(NULL, NULL, NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75))
    Error in use_defaults(..., self = self) : 
      unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 
    2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 2.2), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL,
    Calls: <Anonymous> ... .handleSimpleError -> h -> <Anonymous> -> <Anonymous>
    Execution halted
    ```

# fido

<details>

* Version: 1.0.4
* GitHub: https://github.com/jsilve24/fido
* Source code: https://github.com/cran/fido
* Date/Publication: 2023-03-24 12:00:10 UTC
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
    ...
    ! unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, 
        NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 2.2), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0.5, NULL, NULL, NULL, 
        c(0, 2.2, 0, 2.2), NULL, TRUE), list(), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 2.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(), c(5.5, 5.5, 5.5, 5.5), 11, NULL, NULL, list(), 1.2, NULL, NULL, 5.5, NULL, NULL, NULL, NULL, 0.2, NULL, list(NULL, NULL, NULL, 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, list(NULL, NULL, NULL, 
        NULL, 0, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, c(0, 0, 0, 0), list(), 11, list(), list(), 5.5, NULL, NULL, list("grey92", NULL, NULL, NULL, FALSE, TRUE), NULL, list(NULL, 0.5, NULL, NULL, FALSE, TRUE), NULL, NULL, NULL, NULL, FALSE, list(), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), list(
        NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(5.5, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, TRUE), "topleft", NULL, c(5.5, 5.5, 5.5, 5.5), list(), NULL, NULL, "inherit", "inside", list(NULL, NULL, "grey10", 0.8, NULL, NULL, NULL, NULL, c(4.4, 4.4, 4.4, 4.4), NULL, TRUE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75))
    Error in use_defaults(..., self = self) : 
      unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, 
        NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 2.2), NULL, TRUE), NULL, list(NULL, NULL, NULL, 
    Calls: <Anonymous> ... .handleSimpleError -> h -> <Anonymous> -> <Anonymous>
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 116.1Mb
      sub-directories of 1Mb or more:
        libs  114.1Mb
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
    
    Quitting from lines  at lines 142-177 [unnamed-chunk-1] (exactness.Rmd)
    Error: processing vignette 'exactness.Rmd' failed with diagnostics:
    argument "theme" is missing, with no default
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
      installed size is 11.6Mb
      sub-directories of 1Mb or more:
        doc    9.1Mb
        libs   1.6Mb
    ```

# fmesher

<details>

* Version: 0.1.5
* GitHub: https://github.com/inlabru-org/fmesher
* Source code: https://github.com/cran/fmesher
* Date/Publication: 2023-12-20 21:50:08 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::cloud_details(, "fmesher")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘fmesher-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: fm_int
    > ### Title: Multi-domain integration
    > ### Aliases: fm_int fm_int.list fm_int.numeric fm_int.character
    > ###   fm_int.factor fm_int.SpatRaster fm_int.fm_lattice_2d
    > ###   fm_int.fm_mesh_1d fm_int.fm_mesh_2d fm_int.inla.mesh.lattice
    > ###   fm_int.inla.mesh.1d fm_int.inla.mesh
    > 
    ...
    +     geom_sf(data = fm_as_sfc(fmexample$mesh, multi = TRUE), alpha = 0.5) +
    +     geom_sf(data = fmexample$boundary_sf[[1]], fill = "red", alpha = 0.5) +
    +     geom_sf(data = ips, aes(size = weight)) +
    +     scale_size_area()
    + }
    Warning: Using `as.character()` on a quosure is deprecated as of rlang 0.3.0. Please use
    `as_label()` or `as_name()` instead.
    This warning is displayed once every 8 hours.
    Error: Unknown colour name: ~
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 16.7Mb
      sub-directories of 1Mb or more:
        libs  14.1Mb
    ```

# forestecology

<details>

* Version: 0.2.0
* GitHub: https://github.com/rudeboybert/forestecology
* Source code: https://github.com/cran/forestecology
* Date/Publication: 2021-10-02 13:30:05 UTC
* Number of recursive dependencies: 102

Run `revdepcheck::cloud_details(, "forestecology")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘forestecology-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: add_buffer_variable
    > ### Title: Identify trees in the buffer region
    > ### Aliases: add_buffer_variable
    > 
    > ### ** Examples
    > 
    > library(tibble)
    ...
     17.                           └─ggplot2 (local) FUN(X[[i]], ...)
     18.                             └─g$draw_key(data, g$params, key_size)
     19.                               └─ggplot2 (local) draw_key(...)
     20.                                 └─ggplot2::draw_key_point(data, params, size)
     21.                                   ├─grid::pointsGrob(...)
     22.                                   │ └─grid::grob(...)
     23.                                   └─ggplot2::ggpar(...)
     24.                                     └─rlang:::Ops.quosure(pointsize, .pt)
     25.                                       └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘forestecology.Rmd’
      ...
    
    > ggplot() + geom_sf(data = census_1_ex %>% sf::st_as_sf(coords = c("gx", 
    +     "gy")), aes(col = sp, size = dbh))
    Warning: Using `as.character()` on a quosure is deprecated as of rlang 0.3.0. Please use
    `as_label()` or `as_name()` instead.
    This warning is displayed once every 8 hours.
    
      When sourcing ‘forestecology.R’:
    Error: Unknown colour name: ~
    Execution halted
    
      ‘forestecology.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘forestecology.Rmd’ using rmarkdown
    
    Quitting from lines  at lines 64-69 [unnamed-chunk-3] (forestecology.Rmd)
    Error: processing vignette 'forestecology.Rmd' failed with diagnostics:
    Unknown colour name: ~
    --- failed re-building ‘forestecology.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘forestecology.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘blockCV’ ‘patchwork’
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
    
    The following object is masked from ‘package:graphics’:
    
        layout
    
    > ggplotly(pl2)
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: ggplotly ... use_defaults -> eval_from_theme -> %||% -> calc_element
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

# FuncNN

<details>

* Version: 1.0
* GitHub: https://github.com/b-thi/FuncNN
* Source code: https://github.com/cran/FuncNN
* Date/Publication: 2020-09-15 09:40:15 UTC
* Number of recursive dependencies: 170

Run `revdepcheck::cloud_details(, "FuncNN")` for more info

</details>

## Newly broken

*   checking whether package ‘FuncNN’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘FuncNN’
    See ‘/tmp/workdir/FuncNN/new/FuncNN.Rcheck/00install.out’ for details.
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘foreach’
      All declared Imports should be used.
    ```

# geomander

<details>

* Version: 2.3.0
* GitHub: https://github.com/christopherkenny/geomander
* Source code: https://github.com/cran/geomander
* Date/Publication: 2024-02-15 21:20:02 UTC
* Number of recursive dependencies: 124

Run `revdepcheck::cloud_details(, "geomander")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘geomander-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geo_plot_group
    > ### Title: Create Plots of Shapes by Group with Connected Components
    > ###   Colored
    > ### Aliases: geo_plot_group
    > 
    > ### ** Examples
    > 
    ...
     15.                       └─ggplot2 (local) FUN(X[[i]], ...)
     16.                         └─base::lapply(...)
     17.                           └─ggplot2 (local) FUN(X[[i]], ...)
     18.                             └─g$draw_key(data, g$params, key_size)
     19.                               └─ggplot2 (local) draw_key(...)
     20.                                 └─ggplot2::draw_key_polygon(data, params, size)
     21.                                   └─rlang:::Summary.quosure(from_theme(thin), 1.524, na.rm = FALSE)
     22.                                     └─rlang:::abort_quosure_op("Summary", .Generic)
     23.                                       └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Redistricting_School_Districts.Rmd’
      ...
    +     fill = NA, lwd = 1.5)
    
      When sourcing ‘Redistricting_School_Districts.R’:
    Error: Summary operations are not defined for quosures. Do you need to unquote
    the quosure?
    
    # Bad: min(myquosure)
    
    # Good: min(!!myquosure)
    Execution halted
    
      ‘Merging_Election_Data.Rmd’ using ‘UTF-8’... OK
      ‘Redistricting_School_Districts.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Merging_Election_Data.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.3Mb
      sub-directories of 1Mb or more:
        data   3.3Mb
        libs   4.1Mb
    ```

# geomtextpath

<details>

* Version: 0.1.3
* GitHub: https://github.com/AllanCameron/geomtextpath
* Source code: https://github.com/cran/geomtextpath
* Date/Publication: 2024-03-12 16:30:03 UTC
* Number of recursive dependencies: 94

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
     20. │                   └─self$draw_panel(data, panel_params, coord, na.rm = FALSE, legend = "polygon")
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
      [ FAIL 1 | WARN 0 | SKIP 3 | PASS 465 ]
      
    ...
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test-sf.R:91:3'): We can make grobs from sf features ────────────────
      Error in `(x$boxlinewidth %||% defaults$linewidth[type_ind]) * 3.779528`: non-numeric argument to binary operator
      Backtrace:
          ▆
       1. └─geomtextpath:::sf_textgrob(river, as_textbox = TRUE) at test-sf.R:91:3
      
      [ FAIL 1 | WARN 0 | SKIP 3 | PASS 465 ]
      Error: Test failures
      Execution halted
    ```

# germinationmetrics

<details>

* Version: 0.1.8
* GitHub: https://github.com/aravind-j/germinationmetrics
* Source code: https://github.com/cran/germinationmetrics
* Date/Publication: 2023-08-18 18:02:32 UTC
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "germinationmetrics")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Introduction.Rmd’ using rmarkdown_notangle
    ! Undefined control sequence.
    l.108 \NewDocumentCommand
                             \citeproctext{}{} 
    
    Error: processing vignette 'Introduction.Rmd' failed with diagnostics:
    LaTeX failed to compile /tmp/workdir/germinationmetrics/new/germinationmetrics.Rcheck/vign_test/germinationmetrics/vignettes/Introduction.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See Introduction.log for more info.
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
    Trying to upgrade TinyTeX automatically now...
    If reinstallation fails, try install_tinytex() again. Then install the following packages:
    
    tinytex::tlmgr_install(c("amscls", "amsfonts", "amsmath", "atbegshi", "atveryend", "auxhook", "babel", "bibtex", "bigintcalc", "bitset", "booktabs", "cm", "ctablestack", "dehyph", "dvipdfmx", "dvips", "ec", "epstopdf-pkg", "etex", "etexcmds", "etoolbox", "euenc", "everyshi", "fancyvrb", "filehook", "firstaid", "float", "fontspec", "framed", "geometry", "gettitlestring", "glyphlist", "graphics", "graphics-cfg", "graphics-def", "helvetic", "hycolor", "hyperref", "hyph-utf8", "hyphen-base", "iftex", "inconsolata", "infwarerr", "intcalc", "knuth-lib", "kpathsea", "kvdefinekeys", "kvoptions", "kvsetkeys", "l3backend", "l3kernel", "l3packages", "latex", "latex-amsmath-dev", "latex-bin", "latex-fonts", "latex-tools-dev", "latexconfig", "latexmk", "letltxmacro", "lm", "lm-math", "ltxcmds", "lua-alt-getopt", "lua-uni-algos", "luahbtex", "lualatex-math", "lualibs", "luaotfload", "luatex", "luatexbase", "mdwtools", "metafont", "mfware", "modes", "natbib", "pdfescape", "pdftex", "pdftexcmds", "plain", "psnfss", "refcount", "rerunfilecheck", "scheme-infraonly", "selnolig", "stringenc", "symbol", "tex", "tex-ini-files", "texlive-scripts", "texlive.infra", "times", "tipa", "tools", "unicode-data", "unicode-math", "uniquecounter", "url", "xcolor", "xetex", "xetexconfig", "xkeyval", "xunicode", "zapfding"))
    
    The directory /opt/TinyTeX/texmf-local is not empty. It will be backed up to /tmp/RtmpSUduR6/filefb36377d31c and restored later.
    
    tlmgr: no auxiliary texmf trees defined, so nothing removed
    ...
    
    Error: processing vignette 'Introduction.Rmd' failed with diagnostics:
    LaTeX failed to compile /tmp/workdir/germinationmetrics/old/germinationmetrics.Rcheck/vign_test/germinationmetrics/vignettes/Introduction.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See Introduction.log for more info.
    --- failed re-building ‘Introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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
      [ FAIL 6 | WARN 0 | SKIP 1 | PASS 0 ]
      
    ...
       26. │                 └─ggplot2::calc_element("geom", theme)
       27. └─base::.handleSimpleError(...)
       28.   └─rlang (local) h(simpleError(msg, call))
       29.     └─handlers[[1L]](cnd)
       30.       └─cli::cli_abort(...)
       31.         └─rlang::abort(...)
      
      [ FAIL 6 | WARN 0 | SKIP 1 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘gganimate.Rmd’
      ...
    +     state_length = 1)
    
    > anim
    
      When sourcing ‘gganimate.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `compute_geom_2()`:
    ! argument "theme" is missing, with no default
    Execution halted
    
      ‘gganimate.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘gganimate.Rmd’ using rmarkdown
    ```

# ggautomap

<details>

* Version: 0.3.2
* GitHub: https://github.com/cidm-ph/ggautomap
* Source code: https://github.com/cran/ggautomap
* Date/Publication: 2023-05-24 09:00:02 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::cloud_details(, "ggautomap")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggautomap-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_centroids
    > ### Title: Geographic centroid of locations
    > ### Aliases: geom_centroids
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
    > cartographer::nc_type_example_2 |>
    +   head(n = 100) |>
    +   ggplot(aes(location = county)) +
    +   geom_boundaries(feature_type = "sf.nc") +
    +   geom_centroids(aes(colour = type), position = position_circle_repel_sf(scale = 6), size = 0.5) +
    +   coord_automap(feature_type = "sf.nc")
    Error in valid.pch(x$pch) : 
      'language' object cannot be coerced to type 'integer'
    Calls: <Anonymous> ... validGrob.grob -> validDetails -> validDetails.points -> valid.pch
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggautomap.Rmd’
      ...
    5 2768     Blacktown Western Sydney      2021 A    
    6 2766     Blacktown Western Sydney      2021 B    
    
    > covid_cases_nsw %>% ggplot(aes(location = lga)) + 
    +     geom_boundaries(feature_type = "nswgeo.lga") + geom_geoscatter(aes(colour = type), 
    +     s .... [TRUNCATED] 
    
      When sourcing ‘ggautomap.R’:
    Error: 'language' object cannot be coerced to type 'integer'
    Execution halted
    
      ‘ggautomap.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ggautomap.Rmd’ using rmarkdown
    
    Quitting from lines  at lines 47-54 [scatter] (ggautomap.Rmd)
    Error: processing vignette 'ggautomap.Rmd' failed with diagnostics:
    'language' object cannot be coerced to type 'integer'
    --- failed re-building ‘ggautomap.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ggautomap.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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
* Number of recursive dependencies: 127

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
    2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 2.2
    ), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0.5, NULL, NULL, NULL, c(0, 2.2, 0, 2.2), NULL, TRUE), list("grey20", NULL, NULL, NULL, FALSE, TRUE), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 2.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(NULL, NA, NULL, NULL, TRUE), c(5.5, 5.5, 5.5, 5.5), 11, NULL, NULL, NULL, 1.2, NULL, NULL, 5.5, 
        NULL, NULL, NULL, NULL, 0.2, NULL, list(NULL, NULL, NULL, 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, c(0, 0, 0, 0), list(), 11, list("grey92", NA, NULL, NULL, TRUE), list(), 5.5, NULL, NULL, list("white", NULL, NULL, NULL, FALSE, TRUE), NULL, list(NULL, 0.5, NULL, NULL, FALSE, TRUE), NULL, NULL, NULL, NULL, FALSE, list(NULL, 
            "white", NULL, NULL, TRUE), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(5.5, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, TRUE), "topleft", NULL, c(5.5, 5.5, 5.5, 5.5), list("grey85", NA, NULL, NULL, TRUE), NULL, NULL, "inherit", "inside", list(NULL, NULL, "grey10", 0.8, NULL, 
            NULL, NULL, NULL, c(4.4, 4.4, 4.4, 4.4), NULL, TRUE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75))
    Error in use_defaults(..., self = self) : 
      unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 
    2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL
    Calls: <Anonymous> ... .handleSimpleError -> h -> <Anonymous> -> <Anonymous>
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
    
    Quitting from lines  at lines 49-161 [dotsinterval_components] (dotsinterval.Rmd)
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
    ! unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(7, 0, 0, 0), NULL, FALSE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 7, 
    0, 0), NULL, FALSE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL,
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
        doc    1.3Mb
        help   2.0Mb
        libs   1.0Mb
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
    ...
     10. │     │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
     11. │     │ └─base::withCallingHandlers(...)
     12. │     └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13. │       └─l$compute_geom_2(d, theme = plot$theme)
     14. └─base::.handleSimpleError(...)
     15.   └─rlang (local) h(simpleError(msg, call))
     16.     └─handlers[[1L]](cnd)
     17.       └─cli::cli_abort(...)
     18.         └─rlang::abort(...)
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
      test_nthreads.R...............    0 tests    ----- FAILED[]: test_ggiplot.R<52--52>
       call| expect_snapshot_plot(p3, label = "ggiplot_simple_ribbon")
       diff| 84719
       info| Diff plot saved to: _tinysnapshot_review/ggiplot_simple_ribbon.png
      ----- FAILED[]: test_ggiplot.R<54--54>
       call| expect_snapshot_plot(p5, label = "ggiplot_simple_mci_ribbon")
       diff| 84507
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
    
    > ### Name: facet_nested_wrap
    > ### Title: Ribbon of panels with nested strips.
    > ### Aliases: facet_nested_wrap
    > 
    > ### ** Examples
    > 
    > # A standard plot
    ...
      6. │     └─ggplot2 (local) setup(..., self = self)
      7. │       └─self$facet$compute_layout(data, self$facet_params)
      8. │         └─ggplot2 (local) compute_layout(..., self = self)
      9. │           └─ggplot2:::wrap_layout(id, dims, params$dir)
     10. │             └─ggplot2:::data_frame0(...)
     11. │               └─vctrs::data_frame(..., .name_repair = "minimal")
     12. └─vctrs:::stop_recycle_incompatible_size(...)
     13.   └─vctrs:::stop_vctrs(...)
     14.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
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
      [ FAIL 9 | WARN 1 | SKIP 18 | PASS 719 ]
      
    ...
       11. │           └─ggplot2:::wrap_layout(id, dims, params$dir)
       12. │             └─ggplot2:::data_frame0(...)
       13. │               └─vctrs::data_frame(..., .name_repair = "minimal")
       14. └─vctrs:::stop_recycle_incompatible_size(...)
       15.   └─vctrs:::stop_vctrs(...)
       16.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
      
      [ FAIL 9 | WARN 1 | SKIP 18 | PASS 719 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Facets.Rmd’ using rmarkdown
    
    Quitting from lines  at lines 33-39 [wrap_mimick] (Facets.Rmd)
    Error: processing vignette 'Facets.Rmd' failed with diagnostics:
    Can't recycle `ROW` (size 0) to size 7.
    --- failed re-building ‘Facets.Rmd’
    
    --- re-building ‘Miscellaneous.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Facets.Rmd’
      ...
    Loading required package: ggplot2
    
    > p <- ggplot(mpg, aes(displ, hwy, colour = as.factor(cyl))) + 
    +     geom_point() + labs(x = "Engine displacement", y = "Highway miles per gallon") + .... [TRUNCATED] 
    
    > p + facet_wrap2(vars(class))
    
    ...
    ℹ Error occurred in the 1st layer.
    Caused by error in `setup_params()`:
    ! A discrete 'nbinom' distribution cannot be fitted to continuous data.
    Execution halted
    
      ‘Facets.Rmd’ using ‘UTF-8’... failed
      ‘Miscellaneous.Rmd’ using ‘UTF-8’... failed
      ‘PositionGuides.Rmd’ using ‘UTF-8’... OK
      ‘Statistics.Rmd’ using ‘UTF-8’... failed
      ‘ggh4x.Rmd’ using ‘UTF-8’... OK
    ```

# ggheatmap

<details>

* Version: 2.2
* GitHub: NA
* Source code: https://github.com/cran/ggheatmap
* Date/Publication: 2022-09-10 13:32:55 UTC
* Number of recursive dependencies: 127

Run `revdepcheck::cloud_details(, "ggheatmap")` for more info

</details>

## Newly broken

*   checking whether package ‘ggheatmap’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘ggheatmap’
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

# ggiraph

<details>

* Version: 0.8.9
* GitHub: https://github.com/davidgohel/ggiraph
* Source code: https://github.com/cran/ggiraph
* Date/Publication: 2024-02-24 16:20:13 UTC
* Number of recursive dependencies: 95

Run `revdepcheck::cloud_details(, "ggiraph")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggiraph-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_sf_interactive
    > ### Title: Create interactive sf objects
    > ### Aliases: geom_sf_interactive geom_sf_label_interactive
    > ###   geom_sf_text_interactive
    > 
    > ### ** Examples
    > 
    ...
    +   x <- girafe( ggobj = gg)
    +   if( interactive() ) print(x)
    + }
    Warning in CPL_crs_from_input(x) :
      GDAL Message 1: +init=epsg:XXXX syntax is deprecated. It might return a CRS with a non-EPSG compliant axis order.
    Warning: Using `as.character()` on a quosure is deprecated as of rlang 0.3.0. Please use
    `as_label()` or `as_name()` instead.
    This warning is displayed once every 8 hours.
    Error: Unknown colour name: ~
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
      test-utils.R..................    7 tests [0;31m1 fails[0m 
      test-utils.R..................    8 tests [0;31m1 fails[0m 
      test-utils.R..................   11 tests [0;31m1 fails[0m Error in as.data.frame.default(x[[i]], optional = TRUE) : 
        cannot coerce class 'c("quosure", "formula")' to a data.frame
      Calls: <Anonymous> ... <Anonymous> -> as.data.frame -> as.data.frame.default
      In addition: Warning message:
      'ggiraph' is deprecated.
      Use 'girafe' instead.
      See help("Deprecated") 
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.9Mb
      sub-directories of 1Mb or more:
        libs   9.5Mb
    ```

# ggmice

<details>

* Version: 0.1.0
* GitHub: https://github.com/amices/ggmice
* Source code: https://github.com/cran/ggmice
* Date/Publication: 2023-08-07 14:20:02 UTC
* Number of recursive dependencies: 121

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
    Error: argument "theme" is missing, with no default
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
* Number of recursive dependencies: 126

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
    2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 2.2
    ), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0.5, NULL, NULL, NULL, c(0, 2.2, 0, 2.2), NULL, TRUE), list("grey20", NULL, NULL, NULL, FALSE, TRUE), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 2.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(NULL, NA, NULL, NULL, TRUE), c(5.5, 5.5, 5.5, 5.5), 11, NULL, NULL, NULL, 1.2, NULL, NULL, 5.5, 
        NULL, NULL, NULL, NULL, 0.2, NULL, list(NULL, NULL, NULL, 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, c(0, 0, 0, 0), list(), 11, list("grey92", NA, NULL, NULL, TRUE), list(), 5.5, NULL, NULL, list("white", NULL, NULL, NULL, FALSE, TRUE), NULL, list(NULL, 0.5, NULL, NULL, FALSE, TRUE), NULL, NULL, NULL, NULL, FALSE, list(NULL, 
            "white", NULL, NULL, TRUE), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(5.5, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, TRUE), "topleft", NULL, c(5.5, 5.5, 5.5, 5.5), list("grey85", NA, NULL, NULL, TRUE), NULL, NULL, "inherit", "inside", list(NULL, NULL, "grey10", 0.8, NULL, 
            NULL, NULL, NULL, c(4.4, 4.4, 4.4, 4.4), NULL, TRUE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75))
    Error in use_defaults(..., self = self) : 
      unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 
    2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL
    Calls: <Anonymous> ... .handleSimpleError -> h -> <Anonymous> -> <Anonymous>
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
       24. │                             │ └─base::withCallingHandlers(...)
       25. │                             └─layer$geom$use_defaults(...)
       26. └─base::.handleSimpleError(...)
       27.   └─rlang (local) h(simpleError(msg, call))
       28.     └─handlers[[1L]](cnd)
       29.       └─layer$geom$use_defaults(...)
      
      [ FAIL 4 | WARN 3 | SKIP 0 | PASS 30 ]
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
        0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, TRUE), "topleft", NULL, c(5, 12, 5, 12), list("grey85", NA, NULL, NULL, TRUE), NULL, NULL, "inherit", "inside", list(NULL, NULL, "grey10", 0.8, NULL, NULL, NULL, NULL, c(4.4, 4.4, 4.4, 4.4), NULL, TRUE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75))
    
      When sourcing ‘highDim.R’:
    Error: unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), NULL, NULL, list(), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, 
        NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 2.2), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0.5, NULL, NULL, NULL, c(0, 2.2, 0, 2.2), NULL, TRUE), list("grey20", NULL, NULL, NULL, FALSE, TRUE), NULL, NULL, NULL, list(), NULL, NULL, N
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
       11. │     └─l$compute_geom_2(d, theme = plot$theme)
       12. └─base::.handleSimpleError(...)
       13.   └─rlang (local) h(simpleError(msg, call))
       14.     └─handlers[[1L]](cnd)
       15.       └─cli::cli_abort(...)
       16.         └─rlang::abort(...)
      
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
    `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    Warning in geom_bar(data = layerdata, mapping = aes(x = .data$x, y = .data$count,  :
      Ignoring unknown aesthetics: label1, label2, and label3
    > 
    > # convert `ggplot` object to `plotly` object
    > ggplotly(p, tooltip = c("Range", "count", "density"))
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: ggplotly ... use_defaults -> eval_from_theme -> %||% -> calc_element
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
      unused argument (list(list("black", 0.5, 1, "butt", FALSE, TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), NULL, list(), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 
    0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 2.2), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 
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
      installed size is  9.9Mb
      sub-directories of 1Mb or more:
        doc    3.9Mb
        libs   4.4Mb
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
    
    > ### Name: scale_fill_dra
    > ### Title: Dave's Redistricting App classic scale for 'ggplot2'
    > ### Aliases: scale_fill_dra scale_color_dra scale_colour_dra
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
     15.                       └─ggplot2 (local) FUN(X[[i]], ...)
     16.                         └─base::lapply(...)
     17.                           └─ggplot2 (local) FUN(X[[i]], ...)
     18.                             └─g$draw_key(data, g$params, key_size)
     19.                               └─ggplot2 (local) draw_key(...)
     20.                                 └─ggplot2::draw_key_polygon(data, params, size)
     21.                                   └─rlang:::Summary.quosure(from_theme(thin), 1.524, na.rm = FALSE)
     22.                                     └─rlang:::abort_quosure_op("Summary", .Generic)
     23.                                       └─rlang::abort(...)
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
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: resid_interact ... use_defaults -> eval_from_theme -> %||% -> calc_element
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
    Error: argument "theme" is missing, with no default
    Execution halted
    
      ‘introduction.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘introduction.Rmd’ using rmarkdown
    ```

# ggScatRidges

<details>

* Version: 0.1.1
* GitHub: https://github.com/matbou85/ggScatRidges
* Source code: https://github.com/cran/ggScatRidges
* Date/Publication: 2024-03-25 10:20:05 UTC
* Number of recursive dependencies: 117

Run `revdepcheck::cloud_details(, "ggScatRidges")` for more info

</details>

## Newly broken

*   checking whether package ‘ggScatRidges’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘ggScatRidges’
    See ‘/tmp/workdir/ggScatRidges/new/ggScatRidges.Rcheck/00install.out’ for details.
    ```

# ggseqplot

<details>

* Version: 0.8.3
* GitHub: https://github.com/maraab23/ggseqplot
* Source code: https://github.com/cran/ggseqplot
* Date/Publication: 2023-09-22 21:30:02 UTC
* Number of recursive dependencies: 130

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
      
      TraMineR stable version 2.2-9 (Built: 2024-01-09)
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
* Number of recursive dependencies: 76

Run `revdepcheck::cloud_details(, "ggside")` for more info

</details>

## Newly broken

*   checking for code/documentation mismatches ... WARNING
    ```
    Codoc mismatches from documentation object 'geom_xsideboxplot':
    geom_xsideboxplot
      Code: function(mapping = NULL, data = NULL, stat = "boxplot",
                     position = "dodge2", ..., outliers = TRUE,
                     outlier.colour = NULL, outlier.color = NULL,
                     outlier.fill = NULL, outlier.shape = NULL,
                     outlier.size = NULL, outlier.stroke = 0.5,
                     outlier.alpha = NULL, notch = FALSE, notchwidth = 0.5,
                     staplewidth = 0, varwidth = FALSE, na.rm = FALSE,
                     orientation = "x", show.legend = NA, inherit.aes =
    ...
                     position = "dodge2", ..., outliers = TRUE,
                     outlier.colour = NULL, outlier.color = NULL,
                     outlier.fill = NULL, outlier.shape = 19, outlier.size
                     = 1.5, outlier.stroke = 0.5, outlier.alpha = NULL,
                     notch = FALSE, notchwidth = 0.5, staplewidth = 0,
                     varwidth = FALSE, na.rm = FALSE, orientation = "y",
                     show.legend = NA, inherit.aes = TRUE)
      Mismatches in argument default values:
        Name: 'outlier.shape' Code: NULL Docs: 19
        Name: 'outlier.size' Code: NULL Docs: 1.5
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
     16. │             └─ggplot2 (local) use_defaults(..., self = self)
     17. │               └─ggplot2:::eval_from_theme(default_aes, theme)
     18. │                 ├─calc_element("geom", theme) %||% .default_geom_element
     19. │                 └─ggplot2::calc_element("geom", theme)
     20. └─base::.handleSimpleError(...)
     21.   └─rlang (local) h(simpleError(msg, call))
     22.     └─handlers[[1L]](cnd)
     23.       └─cli::cli_abort(...)
     24.         └─rlang::abort(...)
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
    Error: argument "theme" is missing, with no default
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
      installed size is 11.0Mb
      sub-directories of 1Mb or more:
        doc    9.5Mb
        help   1.1Mb
    ```

# GIFT

<details>

* Version: 1.3.2
* GitHub: https://github.com/BioGeoMacro/GIFT
* Source code: https://github.com/cran/GIFT
* Date/Publication: 2024-02-27 10:50:02 UTC
* Number of recursive dependencies: 119

Run `revdepcheck::cloud_details(, "GIFT")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘GIFT.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘GIFT.Rmd’
      ...
    +         library("knitr")
    +          .... [TRUNCATED] 
    
    > options(tinytex.verbose = TRUE)
    
    > knitr::include_graphics("../man/figures/biodiv_gottingen_logo.png")
    
    ...
    
    > knitr::include_graphics("../man/figures/biodiv_gottingen_logo.png")
    
      When sourcing ‘GIFT_advanced_users.R’:
    Error: Cannot find the file(s): "../man/figures/biodiv_gottingen_logo.png"
    Execution halted
    
      ‘GIFT.Rmd’ using ‘UTF-8’... failed
      ‘GIFT_API.Rmd’ using ‘UTF-8’... failed
      ‘GIFT_advanced_users.Rmd’ using ‘UTF-8’... failed
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.2Mb
      sub-directories of 1Mb or more:
        doc    3.3Mb
        help   2.6Mb
    ```

# GimmeMyPlot

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/GimmeMyPlot
* Date/Publication: 2023-10-18 16:10:02 UTC
* Number of recursive dependencies: 111

Run `revdepcheck::cloud_details(, "GimmeMyPlot")` for more info

</details>

## Newly broken

*   checking whether package ‘GimmeMyPlot’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘GimmeMyPlot’
    See ‘/tmp/workdir/GimmeMyPlot/new/GimmeMyPlot.Rcheck/00install.out’ for details.
    ```

# gprofiler2

<details>

* Version: 0.2.3
* GitHub: NA
* Source code: https://github.com/cran/gprofiler2
* Date/Publication: 2024-02-23 21:50:02 UTC
* Number of recursive dependencies: 74

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
    >  gostplot(gostres)
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: gostplot ... use_defaults -> eval_from_theme -> %||% -> calc_element
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘gprofiler2.Rmd’
      ...
      effective_domain_size source_order    parents
    1                 20212         1236 GO:0005003
    2                 20212         1234 GO:0004714
    3                 21031        12892 GO:0007169
    
    > gostplot(gostres, capped = TRUE, interactive = TRUE)
    
      When sourcing ‘gprofiler2.R’:
    Error: argument "theme" is missing, with no default
    Execution halted
    
      ‘gprofiler2.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘gprofiler2.Rmd’ using rmarkdown
    
    Quitting from lines  at lines 246-247 [unnamed-chunk-14] (gprofiler2.Rmd)
    Error: processing vignette 'gprofiler2.Rmd' failed with diagnostics:
    argument "theme" is missing, with no default
    --- failed re-building ‘gprofiler2.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘gprofiler2.Rmd’
    
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
    +     geom_line(data = xy1, aes(x = x, y = y,color = "Raw Data")) +
    +     geom_line(data = xy2, aes(x = x, y = y,color = "Fitted&Forecasts")) +
    +     geom_line(data = set3, aes(x = CI, y = y,color = "LowerBound"), linetype=2) +
    +     geom_line(data = set4, aes(x = CI, y = y,color = "UpperBound"), linetype=2) +
    +     scale_color_manual(name = "Label",values = colors)
    >   r <- ggplotly(p)
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: ggplotly ... use_defaults -> eval_from_theme -> %||% -> calc_element
    Execution halted
    ```

# h3jsr

<details>

* Version: 1.3.1
* GitHub: NA
* Source code: https://github.com/cran/h3jsr
* Date/Publication: 2023-01-21 09:20:10 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "h3jsr")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘intro-to-h3jsr.Rmd’
      ...
    +     scale_fill_ .... [TRUNCATED] 
    
      When sourcing ‘intro-to-h3jsr.R’:
    Error: Summary operations are not defined for quosures. Do you need to unquote
    the quosure?
    
    # Bad: min(myquosure)
    
    # Good: min(!!myquosure)
    Execution halted
    
      ‘intro-to-h3jsr.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘intro-to-h3jsr.Rmd’ using rmarkdown
    
    Quitting from lines  at lines 79-89 [c4] (intro-to-h3jsr.Rmd)
    Error: processing vignette 'intro-to-h3jsr.Rmd' failed with diagnostics:
    Summary operations are not defined for quosures. Do you need to unquote
    the quosure?
    
    # Bad: min(myquosure)
    
    # Good: min(!!myquosure)
    --- failed re-building ‘intro-to-h3jsr.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘intro-to-h3jsr.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# healthyR

<details>

* Version: 0.2.1
* GitHub: https://github.com/spsanderson/healthyR
* Source code: https://github.com/cran/healthyR
* Date/Publication: 2023-04-06 22:20:03 UTC
* Number of recursive dependencies: 157

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
    Error: argument "theme" is missing, with no default
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
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        data   2.4Mb
        doc    3.7Mb
    ```

# healthyR.ai

<details>

* Version: 0.0.13
* GitHub: https://github.com/spsanderson/healthyR.ai
* Source code: https://github.com/cran/healthyR.ai
* Date/Publication: 2023-04-03 00:20:02 UTC
* Number of recursive dependencies: 228

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
    > 
    > output_list <- pca_your_recipe(rec_obj, .data = data_tbl)
    Warning: !  The following columns have zero variance so scaling cannot be used:
      date_col_day, date_col_mday, date_col_mweek, and date_col_mday7.
    ℹ Consider using ?step_zv (`?recipes::step_zv()`) to remove those columns
      before normalizing.
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: pca_your_recipe ... use_defaults -> eval_from_theme -> %||% -> calc_element
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
    Error: argument "theme" is missing, with no default
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
    
    Quitting from lines  at lines 107-113 [pca_your_rec] (getting-started.Rmd)
    Error: processing vignette 'getting-started.Rmd' failed with diagnostics:
    argument "theme" is missing, with no default
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
    +   .data = data_tbl,
    +   .value_col = value,
    +   .date_col = date_col,
    +   .harmonics = 3,
    +   .frequency = 12
    + )
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: tidy_fft ... use_defaults -> eval_from_theme -> %||% -> calc_element
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
    Error: argument "theme" is missing, with no default
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
      installed size is  6.5Mb
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
       14.             └─ggplot2 (local) compute_geom_2(..., self = self)
       15.               └─self$geom$use_defaults(data, self$aes_params, modifiers, theme = theme)
       16.                 └─ggplot2 (local) use_defaults(..., self = self)
       17.                   └─ggplot2:::eval_from_theme(default_aes, theme)
       18.                     ├─calc_element("geom", theme) %||% .default_geom_element
       19.                     └─ggplot2::calc_element("geom", theme)
      
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
    Error: argument "theme" is missing, with no default
    Execution halted
    
      ‘heatmaply.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘heatmaply.Rmd’ using rmarkdown
    
    Quitting from lines  at lines 109-111 [unnamed-chunk-5] (heatmaply.Rmd)
    Error: processing vignette 'heatmaply.Rmd' failed with diagnostics:
    argument "theme" is missing, with no default
    --- failed re-building ‘heatmaply.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘heatmaply.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        doc   5.1Mb
    ```

# hilldiv

<details>

* Version: 1.5.1
* GitHub: https://github.com/anttonalberdi/hilldiv
* Source code: https://github.com/cran/hilldiv
* Date/Publication: 2019-10-01 14:40:02 UTC
* Number of recursive dependencies: 153

Run `revdepcheck::cloud_details(, "hilldiv")` for more info

</details>

## Newly broken

*   checking whether package ‘hilldiv’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘hilldiv’
    See ‘/tmp/workdir/hilldiv/new/hilldiv.Rcheck/00install.out’ for details.
    ```

# hJAM

<details>

* Version: 1.0.0
* GitHub: https://github.com/lailylajiang/hJAM
* Source code: https://github.com/cran/hJAM
* Date/Publication: 2020-02-20 14:50:05 UTC
* Number of recursive dependencies: 101

Run `revdepcheck::cloud_details(, "hJAM")` for more info

</details>

## Newly broken

*   checking whether package ‘hJAM’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘hJAM’
    See ‘/tmp/workdir/hJAM/new/hJAM.Rcheck/00install.out’ for details.
    ```

# HVT

<details>

* Version: 23.11.1
* GitHub: https://github.com/Mu-Sigma/HVT
* Source code: https://github.com/cran/HVT
* Date/Publication: 2023-11-19 15:20:12 UTC
* Number of recursive dependencies: 200

Run `revdepcheck::cloud_details(, "HVT")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘HVT-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: diagPlot
    > ### Title: Diagnosis Plot
    > ### Aliases: diagPlot
    > ### Keywords: hplot internal
    > 
    > ### ** Examples
    > 
    ...
    Scale for x is already present.
    Adding another scale for x, which will replace the existing scale.
    Scale for y is already present.
    Adding another scale for y, which will replace the existing scale.
    Warning in geom_polygon(data = boundaryCoords2, aes(x = bp.x, y = bp.y,  :
      Ignoring unknown aesthetics: text
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: HVT ... use_defaults -> eval_from_theme -> %||% -> calc_element
    Execution halted
    ```

# HYPEtools

<details>

* Version: 1.6.1
* GitHub: https://github.com/rcapell/HYPEtools
* Source code: https://github.com/cran/HYPEtools
* Date/Publication: 2024-01-12 17:20:02 UTC
* Number of recursive dependencies: 164

Run `revdepcheck::cloud_details(, "HYPEtools")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘plot_map_statistics.Rmd’
      ...
    
    > stat.nm.plot <- "NSE"
    
    > PlotMapPoints(x = stats.cout[, c(1, stat.col.plot)], 
    +     sites = map.Qobs, sites.subid.column = 3, bg = map.subid)
    Joining "SUBID" from GIS Data (sites) To "SUBID" from subass (x)
    
    ...
      When sourcing ‘plot_map_statistics.R’:
    Error: 'language' object cannot be coerced to type 'integer'
    Execution halted
    
      ‘analyze_hype_ts.Rmd’ using ‘UTF-8’... OK
      ‘basin_characteristics.Rmd’ using ‘UTF-8’... OK
      ‘basin_network.Rmd’ using ‘UTF-8’... OK
      ‘import_files.Rmd’ using ‘UTF-8’... OK
      ‘modify_par.Rmd’ using ‘UTF-8’... OK
      ‘plot_map_statistics.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘analyze_hype_ts.Rmd’ using rmarkdown
    ```

# ImFoR

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/ImFoR
* Date/Publication: 2023-09-21 18:50:02 UTC
* Number of recursive dependencies: 173

Run `revdepcheck::cloud_details(, "ImFoR")` for more info

</details>

## Newly broken

*   checking whether package ‘ImFoR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘ImFoR’
    See ‘/tmp/workdir/ImFoR/new/ImFoR.Rcheck/00install.out’ for details.
    ```

# iNEXT.4steps

<details>

* Version: 1.0.0
* GitHub: https://github.com/KaiHsiangHu/iNEXT.4steps
* Source code: https://github.com/cran/iNEXT.4steps
* Date/Publication: 2024-04-10 20:00:05 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::cloud_details(, "iNEXT.4steps")` for more info

</details>

## Newly broken

*   checking whether package ‘iNEXT.4steps’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘iNEXT.4steps’
    See ‘/tmp/workdir/iNEXT.4steps/new/iNEXT.4steps.Rcheck/00install.out’ for details.
    ```

# insane

<details>

* Version: 1.0.3
* GitHub: https://github.com/mcanouil/insane
* Source code: https://github.com/cran/insane
* Date/Publication: 2023-11-14 21:50:02 UTC
* Number of recursive dependencies: 127

Run `revdepcheck::cloud_details(, "insane")` for more info

</details>

## Newly broken

*   checking whether package ‘insane’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘insane’
    See ‘/tmp/workdir/insane/new/insane.Rcheck/00install.out’ for details.
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
    
    Quitting from lines  at lines 211-224 [aesthetics-defaultsVisualization] (inTextSummaryTable-aesthetics.Rmd)
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
      installed size is 10.9Mb
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
Error in compute_geom_2(..., self = self) : 
  argument "theme" is missing, with no default
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
# itsdm

<details>

* Version: 0.2.1
* GitHub: https://github.com/LLeiSong/itsdm
* Source code: https://github.com/cran/itsdm
* Date/Publication: 2023-06-11 00:00:02 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::cloud_details(, "itsdm")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘itsdm-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: suspicious_env_outliers
    > ### Title: Function to detect suspicious outliers based on environmental
    > ###   variables.
    > ### Aliases: suspicious_env_outliers
    > 
    > ### ** Examples
    > 
    ...
    row [51] - suspicious column: [bio12] - suspicious value: [380.00]
    	distribution: 97.143% >= 777.00 - [mean: 1058.20] - [sd: 190.90] - [norm. obs: 102]
    	given:
    		[bio1] > [24.01] (value: 24.41)
    
    
    Error in valid.pch(x$pch) : 
      'language' object cannot be coerced to type 'integer'
    Calls: suspicious_env_outliers ... validGrob.grob -> validDetails -> validDetails.points -> valid.pch
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
     20. │                 └─ggplot2 (local) use_defaults(..., self = self)
     21. │                   └─ggplot2:::eval_from_theme(default_aes, theme)
     22. │                     ├─calc_element("geom", theme) %||% .default_geom_element
     23. │                     └─ggplot2::calc_element("geom", theme)
     24. └─base::.handleSimpleError(...)
     25.   └─rlang (local) h(simpleError(msg, call))
     26.     └─handlers[[1L]](cnd)
     27.       └─cli::cli_abort(...)
     28.         └─rlang::abort(...)
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
       24. │                       └─ggplot2::calc_element("geom", theme)
       25. └─base::.handleSimpleError(...)
       26.   └─rlang (local) h(simpleError(msg, call))
       27.     └─handlers[[1L]](cnd)
       28.       └─cli::cli_abort(...)
       29.         └─rlang::abort(...)
      
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
    > proc.time() - start_time
       user  system elapsed 
      0.036   0.000   0.036 
    > # Heatmap for latent correlation matrix.
    > Heatmap_R_approx = latentcor(X = X, types = "tru", method = "approx",
    +                              showplot = TRUE)$plotR
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: latentcor ... use_defaults -> eval_from_theme -> %||% -> calc_element
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.3Mb
      sub-directories of 1Mb or more:
        R   6.9Mb
    ```

# mapSpain

<details>

* Version: 0.9.0
* GitHub: https://github.com/rOpenSpain/mapSpain
* Source code: https://github.com/cran/mapSpain
* Date/Publication: 2024-01-23 20:50:02 UTC
* Number of recursive dependencies: 101

Run `revdepcheck::cloud_details(, "mapSpain")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mapSpain-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: esp_munic.sf
    > ### Title: All Municipalities 'POLYGON' object of Spain (2019)
    > ### Aliases: esp_munic.sf
    > 
    > ### ** Examples
    > 
    > data("esp_munic.sf")
    ...
     15.                       └─ggplot2 (local) FUN(X[[i]], ...)
     16.                         └─base::lapply(...)
     17.                           └─ggplot2 (local) FUN(X[[i]], ...)
     18.                             └─g$draw_key(data, g$params, key_size)
     19.                               └─ggplot2 (local) draw_key(...)
     20.                                 └─ggplot2::draw_key_polygon(data, params, size)
     21.                                   └─rlang:::Summary.quosure(from_theme(thin), 1.524, na.rm = FALSE)
     22.                                     └─rlang:::abort_quosure_op("Summary", .Generic)
     23.                                       └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(mapSpain)
      > 
      > test_check("mapSpain")
      Starting 2 test processes
      [ FAIL 1 | WARN 0 | SKIP 29 | PASS 158 ]
      
    ...
        'test-esp_move_can.R:42:3', 'test-esp_make_provider.R:8:3',
        'test-esp_make_provider.R:31:3'
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-esp_get_nuts.R:50:3'): Test local NUTS ───────────────────────
      `esp_get_nuts(resolution = "20")` produced warnings.
      
      [ FAIL 1 | WARN 0 | SKIP 29 | PASS 158 ]
      Error: Test failures
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.1Mb
      sub-directories of 1Mb or more:
        data   7.0Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 13138 marked UTF-8 strings
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
    
    Quitting from lines  at lines 141-146 [unnamed-chunk-8] (consistencychecking-3.Rmd)
    Error: processing vignette 'consistencychecking-3.Rmd' failed with diagnostics:
    unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 
    2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 2.2
    ), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0.5, NULL, NULL, NULL, c(0, 2.2, 0, 2.2), NULL, TRUE), list("grey20", NULL, NULL, NULL, FALSE, TRUE), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 2.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list("transparent", NA, NULL, NULL, FALSE), c(5.5, 5.5, 5.5, 5.5), 11, NULL, NULL, list("transparent", 
        NA, NULL, NULL, FALSE), 1.2, NULL, NULL, 5.5, NULL, NULL, NULL, NULL, 0.2, NULL, list(NULL, NULL, NULL, 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, c(0, 0, 0, 0), list(), 11, list(), list(NA, "grey20", NULL, NULL, TRUE), 5.5, NULL, NULL, list("grey92", NULL, NULL, NULL, FALSE, TRUE), list("grey95", NULL, NULL, NULL, FALSE, FALSE), 
        list("grey95", 0.5, NULL, NULL, FALSE, FALSE), NULL, NULL, NULL, NULL, FALSE, list("white", NA, NULL, NULL, FALSE), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(5.5, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, TRUE), "topleft", NULL, c(5.5, 5.5, 5.5, 5.5), list("lightsteelblue1", "black", 
            NULL, NULL, FALSE), NULL, NULL, "inherit", "inside", list(NULL, NULL, "black", 0.8, NULL, NULL, NULL, NULL, c(4.4, 4.4, 4.4, 4.4), NULL, FALSE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75))
    --- failed re-building ‘consistencychecking-3.Rmd’
    
    --- re-building ‘dataexploration-1.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘consistencychecking-3.Rmd’
      ...
    2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 2.2
    ), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0.5, NULL, NULL, NULL, c(0, 2.2, 0, 2.2), NULL, TRUE), list("grey20", NULL, NULL, NULL, FALSE, TRUE), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 2.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list("transparent", NA, NULL, NULL, FALSE), c(5.5, 5.5, 5.5, 5.5), 11, NULL, NULL, list("transparent", 
        NA, NULL, NULL, FALSE), 1.2, NULL, NULL, 5.5, NULL, NULL, NULL, NULL, 0.2, NULL, list(NULL, NULL, NULL, 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, c(0, 0, 0, 0), list(), 11, list(), list(NA, "grey20", NULL, NULL, TRUE), 5.5, NULL, NULL, list("grey92", NULL, NULL, NULL, FALSE, TRUE), list("grey95", NULL, NULL, NULL, FALSE, FALSE), 
        list("grey95", 0.5, NULL, NULL, FALSE, FALSE), NULL, NULL, NULL, NULL, FALSE, list("white", NA, NULL, NULL, FALSE), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(5.5, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, TRUE), "topleft", NULL, c(5.5, 5.5, 5.5, 5.5), list("lightsteelblue1", "black", 
            NULL, NULL, FALSE), NULL, NULL, "inherit", "inside", list(NULL, NULL, "black", 0.8, NULL, NULL, NULL, NULL, c(4.4, 4.4, 4.4, 4.4), NULL, FALSE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75))
    
      When sourcing ‘consistencychecking-3.R’:
    ...
    ! unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 
    2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, N
    Execution halted
    
      ‘consistencychecking-3.Rmd’ using ‘UTF-8’... failed
      ‘dataexploration-1.Rmd’ using ‘UTF-8’... failed
      ‘mbnmatime-overview.Rmd’ using ‘UTF-8’... OK
      ‘outputs-4.Rmd’ using ‘UTF-8’... failed
      ‘predictions-5.Rmd’ using ‘UTF-8’... OK
      ‘runmbnmatime-2.Rmd’ using ‘UTF-8’... OK
    ```

# mc2d

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/mc2d
* Date/Publication: 2023-07-17 16:00:02 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::cloud_details(, "mc2d")` for more info

</details>

## Newly broken

*   checking whether package ‘mc2d’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘mc2d’
    See ‘/tmp/workdir/mc2d/new/mc2d.Rcheck/00install.out’ for details.
    ```

## In both

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘docmcEnglish.Rnw’ using Sweave
    Loading required package: mvtnorm
    Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘mc2d’
    
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

# MetaIntegrator

<details>

* Version: 2.1.3
* GitHub: NA
* Source code: https://github.com/cran/MetaIntegrator
* Date/Publication: 2020-02-26 13:00:11 UTC
* Number of recursive dependencies: 178

Run `revdepcheck::cloud_details(, "MetaIntegrator")` for more info

</details>

## Newly broken

*   checking whether package ‘MetaIntegrator’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘MetaIntegrator’
    See ‘/tmp/workdir/MetaIntegrator/new/MetaIntegrator.Rcheck/00install.out’ for details.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        data   3.9Mb
        doc    2.1Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘BiocManager’ ‘DT’ ‘GEOmetadb’ ‘RMySQL’ ‘RSQLite’ ‘gplots’ ‘pheatmap’
      ‘readr’
      All declared Imports should be used.
    ```

# MF.beta4

<details>

* Version: 1.0.3
* GitHub: https://github.com/AnneChao/MF.beta4
* Source code: https://github.com/cran/MF.beta4
* Date/Publication: 2024-04-16 16:30:02 UTC
* Number of recursive dependencies: 173

Run `revdepcheck::cloud_details(, "MF.beta4")` for more info

</details>

## Newly broken

*   checking whether package ‘MF.beta4’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘MF.beta4’
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
    | Pruning samples on5SD:
      56 metabolites x  500 samples 
    | Performing scaling ...  DONE!
    | Imputation ...  DONE!
    > p_avail<-colnames(b_p)[c(1:5)]
    > LOBOV_accuracies(sur$surrogates, b_p, p_avail, MiMIR::acc_LOBOV)
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: LOBOV_accuracies ... use_defaults -> eval_from_theme -> %||% -> calc_element
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
        9.               └─ggplot2 (local) compute_geom_2(..., self = self)
       10.                 └─self$geom$use_defaults(data, self$aes_params, modifiers, theme = theme)
       11.                   └─ggplot2 (local) use_defaults(..., self = self)
       12.                     └─ggplot2:::eval_from_theme(default_aes, theme)
       13.                       ├─calc_element("geom", theme) %||% .default_geom_element
       14.                       └─ggplot2::calc_element("geom", theme)
      
      [ FAIL 1 | WARN 11 | SKIP 0 | PASS 202 ]
      Error: Test failures
      Execution halted
    ```

# missingHE

<details>

* Version: 1.5.0
* GitHub: NA
* Source code: https://github.com/cran/missingHE
* Date/Publication: 2023-03-21 08:50:02 UTC
* Number of recursive dependencies: 151

Run `revdepcheck::cloud_details(, "missingHE")` for more info

</details>

## Newly broken

*   checking whether package ‘missingHE’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘missingHE’
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
    > er.eval <- evaliq(x.true[na.idx], x.est[na.idx])
    `geom_smooth()` using formula = 'y ~ x'
    > 
    > # Interactive plot
    > er.eval <- evaliq(x.true[na.idx], x.est[na.idx], interactive = TRUE)
    `geom_smooth()` using formula = 'y ~ x'
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: evaliq ... use_defaults -> eval_from_theme -> %||% -> calc_element
    Execution halted
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
     22.                                   └─ggplot2 (local) FUN(X[[i]], ...)
     23.                                     └─g$draw_key(data, g$params, key_size)
     24.                                       └─ggplot2 (local) draw_key(...)
     25.                                         └─ggplot2::draw_key_point(data, params, size)
     26.                                           ├─grid::pointsGrob(...)
     27.                                           │ └─grid::grob(...)
     28.                                           └─ggplot2::ggpar(...)
     29.                                             └─rlang:::Ops.quosure(pointsize, .pt)
     30.                                               └─rlang::abort(...)
    Execution halted
    ```

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
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        data   3.4Mb
        help   1.2Mb
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
      ✔ broom        1.0.5          ✔ recipes      1.0.10    
      ✔ dials        1.2.1          ✔ rsample      1.2.1     
    ...
       10.               └─ggplot2 (local) compute_geom_2(..., self = self)
       11.                 └─self$geom$use_defaults(data, self$aes_params, modifiers, theme = theme)
       12.                   └─ggplot2 (local) use_defaults(..., self = self)
       13.                     └─ggplot2:::eval_from_theme(default_aes, theme)
       14.                       ├─calc_element("geom", theme) %||% .default_geom_element
       15.                       └─ggplot2::calc_element("geom", theme)
      
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

# MSPRT

<details>

* Version: 3.0
* GitHub: NA
* Source code: https://github.com/cran/MSPRT
* Date/Publication: 2020-11-13 10:20:05 UTC
* Number of recursive dependencies: 87

Run `revdepcheck::cloud_details(, "MSPRT")` for more info

</details>

## Newly broken

*   checking whether package ‘MSPRT’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘MSPRT’
    See ‘/tmp/workdir/MSPRT/new/MSPRT.Rcheck/00install.out’ for details.
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘datasets’ ‘grDevices’ ‘graphics’ ‘iterators’ ‘methods’
      All declared Imports should be used.
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
    +                     node_attr_df,
    +                     edge_df)
    > 
    > # run the neatmap code on df
    > neat_res <- neatmap(df, scale_df = "ecdf", max_k = 3, reps = 100, 
    +                     xlab = "vars", ylab = "nets", xlab_cex = 1, ylab_cex = 1)
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: neatmap ... use_defaults -> eval_from_theme -> %||% -> calc_element
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

# NIMAA

<details>

* Version: 0.2.1
* GitHub: https://github.com/jafarilab/NIMAA
* Source code: https://github.com/cran/NIMAA
* Date/Publication: 2022-04-11 14:12:45 UTC
* Number of recursive dependencies: 173

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
    binmatnest.temperature 
                  13.21221 
    Size of Square: 	 66 rows x  66 columns 
    Size of Rectangular_row: 	 6 rows x  105 columns 
    Size of Rectangular_col: 	 99 rows x  2 columns 
    Size of Rectangular_element_max: 	 59 rows x  79 columns 
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: extractSubMatrix ... use_defaults -> eval_from_theme -> %||% -> calc_element
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
                    13.21246 
      Size of Square: 	 66 rows x  66 columns 
    ...
       11.                 └─ggplot2 (local) compute_geom_2(..., self = self)
       12.                   └─self$geom$use_defaults(data, self$aes_params, modifiers, theme = theme)
       13.                     └─ggplot2 (local) use_defaults(..., self = self)
       14.                       └─ggplot2:::eval_from_theme(default_aes, theme)
       15.                         ├─calc_element("geom", theme) %||% .default_geom_element
       16.                         └─ggplot2::calc_element("geom", theme)
      
      [ FAIL 1 | WARN 4 | SKIP 0 | PASS 7 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘NIMAA-vignette.Rmd’
      ...
    +     shape = c("Square", "Rectangular_element_max"), row.vars = "patient_id", 
    +    .... [TRUNCATED] 
    binmatnest.temperature 
                  20.12109 
    Size of Square: 	 96 rows x  96 columns 
    Size of Rectangular_element_max: 	 87 rows x  140 columns 
    
      When sourcing ‘NIMAA-vignette.R’:
    Error: argument "theme" is missing, with no default
    Execution halted
    
      ‘NIMAA-vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘NIMAA-vignette.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        data   1.0Mb
        doc    4.0Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 24 marked UTF-8 strings
    ```

# nswgeo

<details>

* Version: 0.4.0
* GitHub: https://github.com/cidm-ph/nswgeo
* Source code: https://github.com/cran/nswgeo
* Date/Publication: 2024-01-29 13:40:05 UTC
* Number of recursive dependencies: 61

Run `revdepcheck::cloud_details(, "nswgeo")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘nswgeo-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: australia
    > ### Title: Geospatial data of the Australian state and territory
    > ###   administrative boundaries.
    > ### Aliases: australia states
    > ### Keywords: datasets
    > 
    > ### ** Examples
    ...
     15.                       └─ggplot2 (local) FUN(X[[i]], ...)
     16.                         └─base::lapply(...)
     17.                           └─ggplot2 (local) FUN(X[[i]], ...)
     18.                             └─g$draw_key(data, g$params, key_size)
     19.                               └─ggplot2 (local) draw_key(...)
     20.                                 └─ggplot2::draw_key_polygon(data, params, size)
     21.                                   └─rlang:::Summary.quosure(from_theme(thin), 1.524, na.rm = FALSE)
     22.                                     └─rlang:::abort_quosure_op("Summary", .Generic)
     23.                                       └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# OenoKPM

<details>

* Version: 2.4.1
* GitHub: NA
* Source code: https://github.com/cran/OenoKPM
* Date/Publication: 2024-04-08 19:20:10 UTC
* Number of recursive dependencies: 85

Run `revdepcheck::cloud_details(, "OenoKPM")` for more info

</details>

## Newly broken

*   checking whether package ‘OenoKPM’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘OenoKPM’
    See ‘/tmp/workdir/OenoKPM/new/OenoKPM.Rcheck/00install.out’ for details.
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
      testPlot.R....................  141 tests [0;32mOK[0m 
      testPlot.R....................  141 tests [0;32mOK[0m 
      testPlot.R....................  141 tests [0;32mOK[0m 
      testPlot.R....................  142 tests [0;32mOK[0m 
      testPlot.R....................  142 tests [0;32mOK[0m 
      testPlot.R....................  143 tests [0;32mOK[0m Error in compute_geom_2(..., self = self) : 
        argument "theme" is missing, with no default
      Calls: suppressMessages ... use_defaults -> eval_from_theme -> %||% -> calc_element
      Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘OmicNavigatorAPI.Rnw’ using Sweave
    OmicNavigator R package version: 1.13.13
    The app is not installed. Install it with installApp()
    Installing study "ABC" in /tmp/Rtmpc8E08Z/file279050a9efa7
    Exporting study "ABC" as an R package
    Note: No maintainer email was specified. Using the placeholder: Unknown <unknown@unknown>
    Calculating pairwise overlaps. This may take a while...
    Exported study to /tmp/Rtmpc8E08Z/ONstudyABC
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
    +   reducefp = TRUE
    + )
    > 
    > ## Plot results
    > res <- cbind(df, result)
    > PlotDetections(res, title = "KNN-CAD ANOMALY DETECTOR")
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: PlotDetections ... use_defaults -> eval_from_theme -> %||% -> calc_element
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
    l.270 \long
               \def\@secondoffive#1#2#3#4#5{#2}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘otsad.Rnw’
    
    SUMMARY: processing the following file failed:
      ‘otsad.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# pdxTrees

<details>

* Version: 0.4.0
* GitHub: https://github.com/mcconvil/pdxTrees
* Source code: https://github.com/cran/pdxTrees
* Date/Publication: 2020-08-17 14:00:02 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "pdxTrees")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘pdxTrees-vignette.Rmd’
      ...
    > berkeley_graph + transition_states(states = Mature_Size, 
    +     transition_length = 10, state_length = 8) + enter_grow() + 
    +     exit_shrink()
    Warning: Failed to apply `after_scale()` modifications to legend
    Caused by error in `build()`:
    ! argument "theme" is missing, with no default
    
      When sourcing ‘pdxTrees-vignette.R’:
    Error: promise already under evaluation: recursive default argument reference or earlier problems?
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
       14.               └─ggplot2 (local) compute_geom_2(..., self = self)
       15.                 └─self$geom$use_defaults(data, self$aes_params, modifiers, theme = theme)
       16.                   └─ggplot2 (local) use_defaults(..., self = self)
       17.                     └─ggplot2:::eval_from_theme(default_aes, theme)
       18.                       ├─calc_element("geom", theme) %||% .default_geom_element
       19.                       └─ggplot2::calc_element("geom", theme)
      
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
    ! Undefined control sequence.
    l.108 \NewDocumentCommand
                             \citeproctext{}{} 
    
    Error: processing vignette 'Introduction.Rmd' failed with diagnostics:
    LaTeX failed to compile /tmp/workdir/PGRdup/new/PGRdup.Rcheck/vign_test/PGRdup/vignettes/Introduction.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See Introduction.log for more info.
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
    Trying to upgrade TinyTeX automatically now...
    If reinstallation fails, try install_tinytex() again. Then install the following packages:
    
    tinytex::tlmgr_install(c("amscls", "amsfonts", "amsmath", "atbegshi", "atveryend", "auxhook", "babel", "bibtex", "bigintcalc", "bitset", "booktabs", "cm", "ctablestack", "dehyph", "dvipdfmx", "dvips", "ec", "epstopdf-pkg", "etex", "etexcmds", "etoolbox", "euenc", "everyshi", "fancyvrb", "filehook", "firstaid", "float", "fontspec", "framed", "geometry", "gettitlestring", "glyphlist", "graphics", "graphics-cfg", "graphics-def", "helvetic", "hycolor", "hyperref", "hyph-utf8", "hyphen-base", "iftex", "inconsolata", "infwarerr", "intcalc", "knuth-lib", "kpathsea", "kvdefinekeys", "kvoptions", "kvsetkeys", "l3backend", "l3kernel", "l3packages", "latex", "latex-amsmath-dev", "latex-bin", "latex-fonts", "latex-tools-dev", "latexconfig", "latexmk", "letltxmacro", "lm", "lm-math", "ltxcmds", "lua-alt-getopt", "lua-uni-algos", "luahbtex", "lualatex-math", "lualibs", "luaotfload", "luatex", "luatexbase", "mdwtools", "metafont", "mfware", "modes", "natbib", "pdfescape", "pdftex", "pdftexcmds", "plain", "psnfss", "refcount", "rerunfilecheck", "scheme-infraonly", "selnolig", "stringenc", "symbol", "tex", "tex-ini-files", "texlive-scripts", "texlive.infra", "times", "tipa", "tools", "unicode-data", "unicode-math", "uniquecounter", "url", "xcolor", "xetex", "xetexconfig", "xkeyval", "xunicode", "zapfding"))
    
    The directory /opt/TinyTeX/texmf-local is not empty. It will be backed up to /tmp/RtmpQwt2mx/filed245f15abf1 and restored later.
    
    tlmgr: no auxiliary texmf trees defined, so nothing removed
    ...
    
    Error: processing vignette 'Introduction.Rmd' failed with diagnostics:
    LaTeX failed to compile /tmp/workdir/PGRdup/old/PGRdup.Rcheck/vign_test/PGRdup/vignettes/Introduction.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See Introduction.log for more info.
    --- failed re-building ‘Introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# plantTracker

<details>

* Version: 1.1.0
* GitHub: https://github.com/aestears/plantTracker
* Source code: https://github.com/cran/plantTracker
* Date/Publication: 2023-05-05 18:20:02 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::cloud_details(, "plantTracker")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Suggested_plantTracker_Workflow.Rmd’
      ...
      a single row.
    
      When sourcing ‘Suggested_plantTracker_Workflow.R’:
    Error: Summary operations are not defined for quosures. Do you need to unquote
    the quosure?
    
    # Bad: min(myquosure)
    ...
    Error: Summary operations are not defined for quosures. Do you need to unquote
    the quosure?
    
    # Bad: min(myquosure)
    
    # Good: min(!!myquosure)
    Execution halted
    
      ‘Suggested_plantTracker_Workflow.Rmd’ using ‘UTF-8’... failed
      ‘Using_the_plantTracker_trackSpp_function.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
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
    ...
    
    # Good: min(!!myquosure)
    --- failed re-building ‘Using_the_plantTracker_trackSpp_function.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘Suggested_plantTracker_Workflow.Rmd’
      ‘Using_the_plantTracker_trackSpp_function.Rmd’
    
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
    Warning: Vectorized input to `element_text()` is not officially supported.
    ℹ Results may be unexpected or may change in future versions of ggplot2.
    Warning in geom_tile(aes(x = Plasmid, y = Sample, label = AMR_gene, fill = Inc_group,  :
      Ignoring unknown aesthetics: label and text
    Warning: Use of `report$Sureness` is discouraged.
    ℹ Use `Sureness` instead.
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: main ... use_defaults -> eval_from_theme -> %||% -> calc_element
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
        9.               └─ggplot2 (local) compute_geom_2(..., self = self)
       10.                 └─self$geom$use_defaults(data, self$aes_params, modifiers, theme = theme)
       11.                   └─ggplot2 (local) use_defaults(..., self = self)
       12.                     └─ggplot2:::eval_from_theme(default_aes, theme)
       13.                       ├─calc_element("geom", theme) %||% .default_geom_element
       14.                       └─ggplot2::calc_element("geom", theme)
      
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
* Number of recursive dependencies: 148

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
    + style(p, marker.line = list(width = 2.5), marker.size = 10)
    + ## Don't show: 
    + }) # examplesIf
    > (p <- ggplotly(qplot(data = mtcars, wt, mpg, geom = c("point", "smooth"))))
    Warning: `qplot()` was deprecated in ggplot2 3.4.0.
    `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: <Anonymous> ... use_defaults -> eval_from_theme -> %||% -> calc_element
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
      installed size is  7.9Mb
      sub-directories of 1Mb or more:
        R             1.5Mb
        htmlwidgets   4.0Mb
    ```

# pmartR

<details>

* Version: 2.4.4
* GitHub: https://github.com/pmartR/pmartR
* Source code: https://github.com/cran/pmartR
* Date/Publication: 2024-02-27 21:20:02 UTC
* Number of recursive dependencies: 150

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
      [ FAIL 1 | WARN 0 | SKIP 6 | PASS 2575 ]
      
      ══ Skipped tests (6) ═══════════════════════════════════════════════════════════
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
      installed size is 14.3Mb
      sub-directories of 1Mb or more:
        libs  11.4Mb
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

# PointedSDMs

<details>

* Version: 1.3.2
* GitHub: https://github.com/PhilipMostert/PointedSDMs
* Source code: https://github.com/cran/PointedSDMs
* Date/Publication: 2024-02-02 09:50:02 UTC
* Number of recursive dependencies: 143

Run `revdepcheck::cloud_details(, "PointedSDMs")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘PointedSDMs-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: dataSDM
    > ### Title: R6 class for creating a 'dataSDM' object.
    > ### Aliases: dataSDM
    > 
    > ### ** Examples
    > 
    > 
    ...
     17.                           └─ggplot2 (local) FUN(X[[i]], ...)
     18.                             └─g$draw_key(data, g$params, key_size)
     19.                               └─ggplot2 (local) draw_key(...)
     20.                                 └─ggplot2::draw_key_point(data, params, size)
     21.                                   ├─grid::pointsGrob(...)
     22.                                   │ └─grid::grob(...)
     23.                                   └─ggplot2::ggpar(...)
     24.                                     └─rlang:::Ops.quosure(pointsize, .pt)
     25.                                       └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking running R code from vignettes ... WARNING
    ```
    Errors in running code in vignettes:
    when running code in ‘Marked_Point_Process.Rmd’
      ...
    
    > knitr::opts_chunk$set(collapse = TRUE, comment = "#>", 
    +     eval = FALSE, warning = FALSE, message = FALSE)
    
    > library(spatstat)
    
      When sourcing ‘Marked_Point_Process.R’:
    ...
    +     resolution = "high")
    
      When sourcing ‘Spatiotemporal_example.R’:
    Error: there is no package called ‘USAboundaries’
    Execution halted
    
      ‘Marked_Point_Process.Rmd’ using ‘UTF-8’... failed
      ‘Setophaga.Rmd’ using ‘UTF-8’... failed
      ‘Solitary_tinamou.Rmd’ using ‘UTF-8’... failed
      ‘Spatiotemporal_example.Rmd’ using ‘UTF-8’... failed
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
    
    Quitting from lines  at lines 526-529 [mixture] (rvar.Rmd)
    Error: processing vignette 'rvar.Rmd' failed with diagnostics:
    Problem while setting up geom aesthetics.
    ...
        NULL, NULL, NULL, NULL, 0.2, NULL, list(NULL, NULL, NULL, 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, c(0, 0, 0, 0), list(), 11, list("grey92", NA, NULL, NULL, TRUE), list(), 5.5, NULL, NULL, list("white", NULL, NULL, NULL, FALSE, TRUE), NULL, list(NULL, 0.5, NULL, NULL, FALSE, TRUE), NULL, NULL, NULL, NULL, FALSE, list(NULL, 
            "white", NULL, NULL, TRUE), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(5.5, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, TRUE), "topleft", NULL, c(5.5, 5.5, 5.5, 5.5), list("grey85", NA, NULL, NULL, TRUE), NULL, NULL, "inherit", "inside", list(NULL, NULL, "grey10", 0.8, NULL, 
            NULL, NULL, NULL, c(4.4, 4.4, 4.4, 4.4), NULL, TRUE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75))
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
    '/tmp/Rtmpv5CY0G/file171e30ae70cd/vignettes'.
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
    Error: argument "theme" is missing, with no default
    Execution halted
    
      ‘one_sample_expansion.Rmd’ using ‘UTF-8’... failed
      ‘two_sample_randomized.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘one_sample_expansion.Rmd’ using rmarkdown
    
    Quitting from lines  at lines 183-188 [unnamed-chunk-13] (one_sample_expansion.Rmd)
    Error: processing vignette 'one_sample_expansion.Rmd' failed with diagnostics:
    argument "theme" is missing, with no default
    --- failed re-building ‘one_sample_expansion.Rmd’
    
    --- re-building ‘two_sample_randomized.Rmd’ using rmarkdown
    ...
    Quitting from lines  at lines 179-184 [unnamed-chunk-13] (two_sample_randomized.Rmd)
    Error: processing vignette 'two_sample_randomized.Rmd' failed with diagnostics:
    argument "theme" is missing, with no default
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
       20.                     └─ggplot2 (local) use_defaults(..., self = self)
       21.                       └─ggplot2:::eval_from_theme(default_aes, theme)
       22.                         ├─calc_element("geom", theme) %||% .default_geom_element
       23.                         └─ggplot2::calc_element("geom", theme)
      ── Failure ('test_trace_explorer.R:240:3'): test trace_explorer on eventlog with param `plotly` ──
      `chart` inherits from 'gg'/'ggplot' not 'plotly'.
      
      [ FAIL 6 | WARN 0 | SKIP 10 | PASS 107 ]
      Error: Test failures
      Execution halted
    ```

# QuadratiK

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/QuadratiK
* Date/Publication: 2024-02-23 18:30:05 UTC
* Number of recursive dependencies: 131

Run `revdepcheck::cloud_details(, "QuadratiK")` for more info

</details>

## Newly broken

*   checking whether package ‘QuadratiK’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘QuadratiK’
    See ‘/tmp/workdir/QuadratiK/new/QuadratiK.Rcheck/00install.out’ for details.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 15.9Mb
      sub-directories of 1Mb or more:
        libs  15.2Mb
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

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.9Mb
      sub-directories of 1Mb or more:
        libs   4.7Mb
    ```

# rangeMapper

<details>

* Version: 2.0.3
* GitHub: https://github.com/mpio-be/rangeMapper
* Source code: https://github.com/cran/rangeMapper
* Date/Publication: 2022-10-03 22:20:02 UTC
* Number of recursive dependencies: 112

Run `revdepcheck::cloud_details(, "rangeMapper")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Case_studies_Valcu_et_al_2012.Rmd’
      ...
    +     geom_sf(data = bmr, aes(fill = value), size = 0.05) + scale_fill_gradientn(co .... [TRUNCATED] 
    
      When sourcing ‘Case_studies_Valcu_et_al_2012.R’:
    Error: Summary operations are not defined for quosures. Do you need to unquote
    the quosure?
    
    # Bad: min(myquosure)
    
    # Good: min(!!myquosure)
    Execution halted
    
      ‘Case_studies_Valcu_et_al_2012.Rmd’ using ‘UTF-8’... failed
      ‘rangeMapper.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Case_studies_Valcu_et_al_2012.Rmd’ using rmarkdown
    ```

# rassta

<details>

* Version: 1.0.5
* GitHub: https://github.com/bafuentes/rassta
* Source code: https://github.com/cran/rassta
* Date/Publication: 2022-08-30 22:30:02 UTC
* Number of recursive dependencies: 121

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
    > # Single-layer SpatRaster of topographic classification units
    > ## 5 classification units
    > tcf <- list.files(path = p, pattern = "topography.tif", full.names = TRUE)
    > tcu <- terra::rast(tcf)
    > # Automatic selection of distribution functions
    > tdif <- select_functions(cu.rast = tcu, var.rast = tvars, fun = mean)
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: select_functions ... use_defaults -> eval_from_theme -> %||% -> calc_element
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
      test_select_functions.R.......    0 tests    Error in compute_geom_2(..., self = self) : 
        argument "theme" is missing, with no default
      Calls: <Anonymous> ... use_defaults -> eval_from_theme -> %||% -> calc_element
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
    Error: argument "theme" is missing, with no default
    Execution halted
    
      ‘classunits.Rmd’ using ‘UTF-8’... OK
      ‘modeling.Rmd’ using ‘UTF-8’... OK
      ‘sampling.Rmd’ using ‘UTF-8’... OK
      ‘signature.Rmd’ using ‘UTF-8’... failed
      ‘similarity.Rmd’ using ‘UTF-8’... OK
      ‘stratunits.Rmd’ using ‘UTF-8’... OK
    ```

# RCTrep

<details>

* Version: 1.2.0
* GitHub: https://github.com/duolajiang/RCTrep
* Source code: https://github.com/cran/RCTrep
* Date/Publication: 2023-11-02 14:40:02 UTC
* Number of recursive dependencies: 166

Run `revdepcheck::cloud_details(, "RCTrep")` for more info

</details>

## Newly broken

*   checking whether package ‘RCTrep’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘RCTrep’
    See ‘/tmp/workdir/RCTrep/new/RCTrep.Rcheck/00install.out’ for details.
    ```

# redistmetrics

<details>

* Version: 1.0.7
* GitHub: https://github.com/alarm-redist/redistmetrics
* Source code: https://github.com/cran/redistmetrics
* Date/Publication: 2023-12-12 19:30:02 UTC
* Number of recursive dependencies: 85

Run `revdepcheck::cloud_details(, "redistmetrics")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘compactness.Rmd’
      ...
    +     labs(fill = "none")
    
      When sourcing ‘compactness.R’:
    Error: Summary operations are not defined for quosures. Do you need to unquote
    the quosure?
    
    # Bad: min(myquosure)
    ...
    
    # Good: min(!!myquosure)
    Execution halted
    
      ‘compactness.Rmd’ using ‘UTF-8’... failed
      ‘distances.Rmd’ using ‘UTF-8’... OK
      ‘other.Rmd’ using ‘UTF-8’... OK
      ‘party.Rmd’ using ‘UTF-8’... OK
      ‘redistmetrics.Rmd’ using ‘UTF-8’... OK
      ‘splits.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘compactness.Rmd’ using rmarkdown
    
    Quitting from lines  at lines 29-35 [unnamed-chunk-2] (compactness.Rmd)
    Error: processing vignette 'compactness.Rmd' failed with diagnostics:
    Summary operations are not defined for quosures. Do you need to unquote
    the quosure?
    
    # Bad: min(myquosure)
    
    ...
    # Good: min(!!myquosure)
    --- failed re-building ‘compactness.Rmd’
    
    --- re-building ‘distances.Rmd’ using rmarkdown
    --- finished re-building ‘distances.Rmd’
    
    --- re-building ‘other.Rmd’ using rmarkdown
    --- finished re-building ‘other.Rmd’
    
    --- re-building ‘party.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 13.7Mb
      sub-directories of 1Mb or more:
        libs  12.6Mb
    ```

# ref.ICAR

<details>

* Version: 2.0.1
* GitHub: NA
* Source code: https://github.com/cran/ref.ICAR
* Date/Publication: 2023-08-22 08:50:02 UTC
* Number of recursive dependencies: 103

Run `revdepcheck::cloud_details(, "ref.ICAR")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ref-icar-vignette.Rmd’
      ...
    +     scale_fill_brewer(palette = "OrRd") + labs(title = "Plot of observed \n verbal SAT s ..." ... [TRUNCATED] 
    
      When sourcing ‘ref-icar-vignette.R’:
    Error: Summary operations are not defined for quosures. Do you need to unquote
    the quosure?
    
    # Bad: min(myquosure)
    
    # Good: min(!!myquosure)
    Execution halted
    
      ‘ref-icar-vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ref-icar-vignette.Rmd’ using rmarkdown
    Warning in eng_r(options) :
      Failed to tidy R code in chunk 'unnamed-chunk-1'. Reason:
    Error : The formatR package is required by the chunk option tidy = TRUE but not installed; tidy = TRUE will be ignored.
    
    Warning in eng_r(options) :
      Failed to tidy R code in chunk 'unnamed-chunk-2'. Reason:
    Error : The formatR package is required by the chunk option tidy = TRUE but not installed; tidy = TRUE will be ignored.
    
    ...
    # Bad: min(myquosure)
    
    # Good: min(!!myquosure)
    --- failed re-building ‘ref-icar-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ref-icar-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# remap

<details>

* Version: 0.3.1
* GitHub: https://github.com/jadonwagstaff/remap
* Source code: https://github.com/cran/remap
* Date/Publication: 2023-06-14 20:50:02 UTC
* Number of recursive dependencies: 64

Run `revdepcheck::cloud_details(, "remap")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Introduction_to_remap.Rmd’
      ...
    +     ggti .... [TRUNCATED] 
    
      When sourcing ‘Introduction_to_remap.R’:
    Error: Summary operations are not defined for quosures. Do you need to unquote
    the quosure?
    
    # Bad: min(myquosure)
    
    # Good: min(!!myquosure)
    Execution halted
    
      ‘Introduction_to_remap.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Introduction_to_remap.Rmd’ using rmarkdown
    
    Quitting from lines  at lines 43-54 [initial_map] (Introduction_to_remap.Rmd)
    Error: processing vignette 'Introduction_to_remap.Rmd' failed with diagnostics:
    Summary operations are not defined for quosures. Do you need to unquote
    the quosure?
    
    # Bad: min(myquosure)
    
    # Good: min(!!myquosure)
    --- failed re-building ‘Introduction_to_remap.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Introduction_to_remap.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# rKIN

<details>

* Version: 1.0.2
* GitHub: https://github.com/salbeke/rKIN
* Source code: https://github.com/cran/rKIN
* Date/Publication: 2023-10-02 22:20:02 UTC
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "rKIN")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rKIN-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: estEllipse
    > ### Title: Estimate Bivariate Normal Ellipse Isotope Niche
    > ### Aliases: estEllipse
    > 
    > ### ** Examples
    > 
    > library(rKIN)
    ...
     15.                       └─ggplot2 (local) FUN(X[[i]], ...)
     16.                         └─base::lapply(...)
     17.                           └─ggplot2 (local) FUN(X[[i]], ...)
     18.                             └─g$draw_key(data, g$params, key_size)
     19.                               └─ggplot2 (local) draw_key(...)
     20.                                 └─ggplot2::draw_key_polygon(data, params, size)
     21.                                   └─rlang:::Summary.quosure(from_theme(thin), 1.524, na.rm = FALSE)
     22.                                     └─rlang:::abort_quosure_op("Summary", .Generic)
     23.                                       └─rlang::abort(...)
    Execution halted
    ```

# rLFT

<details>

* Version: 1.0.1
* GitHub: NA
* Source code: https://github.com/cran/rLFT
* Date/Publication: 2021-09-24 04:10:02 UTC
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "rLFT")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘rLFT_Introduction.Rmd’
      ...
    old-style crs object detected; please recreate object with a recent sf::st_crs()
    
      When sourcing ‘rLFT_Introduction.R’:
    Error: Summary operations are not defined for quosures. Do you need to unquote
    the quosure?
    
    # Bad: min(myquosure)
    
    # Good: min(!!myquosure)
    Execution halted
    
      ‘rLFT_Introduction.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘rLFT_Introduction.Rmd’ using rmarkdown
    
    Quitting from lines  at lines 56-66 [unnamed-chunk-2] (rLFT_Introduction.Rmd)
    Error: processing vignette 'rLFT_Introduction.Rmd' failed with diagnostics:
    Summary operations are not defined for quosures. Do you need to unquote
    the quosure?
    
    # Bad: min(myquosure)
    
    # Good: min(!!myquosure)
    --- failed re-building ‘rLFT_Introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘rLFT_Introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        help   1.8Mb
        libs   4.0Mb
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
    +   centerline = sin(2 * pi * grid),
    +   Cov = Cov
    + )
    > names <- paste0("id_", 1:nrow(Data[[1]]))
    > DG <- depthgram(Data, marginal_outliers = TRUE, ids = names)
    > plot(DG)
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: plot ... use_defaults -> eval_from_theme -> %||% -> calc_element
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.2Mb
      sub-directories of 1Mb or more:
        data   4.8Mb
        doc    1.7Mb
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
    36         5.7
    37         5.7
    38         5.7
    39         5.7
    40         5.7
    41         5.7
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: box.spread ... use_defaults -> eval_from_theme -> %||% -> calc_element
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
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
    2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 2.2
    ), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0.5, NULL, NULL, NULL, c(0, 2.2, 0, 2.2), NULL, TRUE), list("grey80", NULL, NULL, NULL, FALSE, FALSE), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 2.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list("grey80", NULL, NULL, NULL, FALSE, FALSE), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(), c(5.5, 5.5, 5.5, 5.5), 11, NULL, NULL, list(), 1.2, 
        NULL, NULL, 5.5, NULL, NULL, NULL, NULL, 0.2, NULL, list(NULL, NULL, NULL, 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, "bottom", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, c(0, 0, 0, 0), list(), 11, list(), list(), 5.5, NULL, NULL, list("grey92", NULL, NULL, NULL, FALSE, TRUE), list(), list(), NULL, NULL, NULL, NULL, FALSE, list(), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, 
            c(0, 0, 5.5, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(5.5, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, TRUE), "topleft", NULL, c(5.5, 5.5, 5.5, 5.5), list(), NULL, NULL, "inherit", "inside", list(NULL, NULL, "grey10", 0.8, NULL, NULL, NULL, NULL, c(4.4, 4.4, 4.4, 4.4), NULL, TRUE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 
            NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75))
    Error in use_defaults(..., self = self) : 
      unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 
    2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL
    Calls: <Anonymous> ... .handleSimpleError -> h -> <Anonymous> -> <Anonymous>
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
    2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 2.2
    ), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0.5, NULL, NULL, NULL, c(0, 2.2, 0, 2.2), NULL, TRUE), list("grey80", NULL, NULL, NULL, FALSE, FALSE), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 2.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list("grey80", NULL, NULL, NULL, FALSE, FALSE), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(), c(5.5, 5.5, 5.5, 5.5), 11, NULL, NULL, list(), 1.2, 
        NULL, NULL, 5.5, NULL, NULL, NULL, NULL, 0.2, NULL, list(NULL, NULL, NULL, 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, "bottom", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, c(0, 0, 0, 0), list(), 11, list(), list(), 5.5, NULL, NULL, list("grey92", NULL, NULL, NULL, FALSE, TRUE), list(), list(), NULL, NULL, NULL, NULL, FALSE, list(), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, 
            c(0, 0, 5.5, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(5.5, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, TRUE), "topleft", NULL, c(5.5, 5.5, 5.5, 5.5), list(), NULL, NULL, "inherit", "inside", list(NULL, NULL, "grey10", 0.8, NULL, NULL, NULL, NULL, c(4.4, 4.4, 4.4, 4.4), NULL, TRUE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 
            NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75))
    
      When sourcing ‘scoringutils.R’:
    Error: unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 
    2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, 
    Execution halted
    
      ‘metric-details.Rmd’ using ‘UTF-8’... OK
      ‘scoring-forecasts-directly.Rmd’ using ‘UTF-8’... OK
      ‘scoringutils.Rmd’ using ‘UTF-8’... failed
    ```

# SCOUTer

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/SCOUTer
* Date/Publication: 2020-06-30 09:30:03 UTC
* Number of recursive dependencies: 99

Run `revdepcheck::cloud_details(, "SCOUTer")` for more info

</details>

## Newly broken

*   checking whether package ‘SCOUTer’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘SCOUTer’
    See ‘/tmp/workdir/SCOUTer/new/SCOUTer.Rcheck/00install.out’ for details.
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
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: graphly ... use_defaults -> eval_from_theme -> %||% -> calc_element
    Execution halted
    ```

# see

<details>

* Version: 0.8.4
* GitHub: https://github.com/easystats/see
* Source code: https://github.com/cran/see
* Date/Publication: 2024-04-29 04:40:03 UTC
* Number of recursive dependencies: 234

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
     17. │               └─self$geom$use_defaults(data, self$aes_params, modifiers, theme = theme)
     18. └─base::.handleSimpleError(...)
     19.   └─rlang (local) h(simpleError(msg, call))
     20.     └─handlers[[1L]](cnd)
     21.       └─cli::cli_abort(...)
     22.         └─rlang::abort(...)
    Execution halted
    ```

# sfnetworks

<details>

* Version: 0.6.4
* GitHub: https://github.com/luukvdmeer/sfnetworks
* Source code: https://github.com/cran/sfnetworks
* Date/Publication: 2024-04-09 22:40:02 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "sfnetworks")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘sfn01_structure.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘sfn01_structure.Rmd’
      ...
    > plot(other_net, cex = 2, lwd = 2, main = "Straight lines")
    
    > st_geometry(edges) = st_sfc(c(l2, l3, l1), crs = 4326)
    
    > net = sfnetwork(nodes, edges)
    Checking if spatial network structure is valid...
    
    ...
    Error: [1m[22m[36mℹ[39m In argument: `azimuth = edge_azimuth()`.
    [1mCaused by error in `st_geod_azimuth()`:[22m
    [33m![39m st_is_longlat(x) is not TRUE
    Execution halted
    
      ‘sfn01_structure.Rmd’ using ‘UTF-8’... failed
      ‘sfn02_preprocess_clean.Rmd’ using ‘UTF-8’... OK
      ‘sfn03_join_filter.Rmd’ using ‘UTF-8’... OK
      ‘sfn04_routing.Rmd’ using ‘UTF-8’... OK
      ‘sfn05_morphers.Rmd’ using ‘UTF-8’... failed
    ```

# sftrack

<details>

* Version: 0.5.4
* GitHub: https://github.com/mablab/sftrack
* Source code: https://github.com/cran/sftrack
* Date/Publication: 2023-03-16 12:20:02 UTC
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "sftrack")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘sftrack-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_sftrack
    > ### Title: Function to plot sftrack objects in ggplot
    > ### Aliases: geom_sftrack geom_sftrack.sftrack geom_sftrack.sftraj
    > 
    > ### ** Examples
    > 
    > #'
    ...
     17.                           └─ggplot2 (local) FUN(X[[i]], ...)
     18.                             └─g$draw_key(data, g$params, key_size)
     19.                               └─ggplot2 (local) draw_key(...)
     20.                                 └─ggplot2::draw_key_path(data, params, size)
     21.                                   ├─grid::segmentsGrob(...)
     22.                                   │ └─grid::grob(...)
     23.                                   └─ggplot2::ggpar(...)
     24.                                     └─rlang:::Ops.quosure(args$lwd, .pt)
     25.                                       └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘sftrack5_spatial.Rmd’
      ...
    > ggplot() + geom_sftrack(data = my_sftraj)
    
      When sourcing ‘sftrack5_spatial.R’:
    Error: Base operators are not defined for quosures. Do you need to unquote the
    quosure?
    
    # Bad: myquosure * rhs
    
    # Good: !!myquosure * rhs
    Execution halted
    
      ‘sftrack1_overview.Rmd’ using ‘UTF-8’... OK
      ‘sftrack2_reading.Rmd’ using ‘UTF-8’... OK
      ‘sftrack3_workingwith.Rmd’ using ‘UTF-8’... OK
      ‘sftrack4_groups.Rmd’ using ‘UTF-8’... OK
      ‘sftrack5_spatial.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘sftrack1_overview.Rmd’ using rmarkdown
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
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: deviance_residuals ... use_defaults -> eval_from_theme -> %||% -> calc_element
    Execution halted
    ```

# sievePH

<details>

* Version: 1.0.4
* GitHub: https://github.com/mjuraska/sievePH
* Source code: https://github.com/cran/sievePH
* Date/Publication: 2023-02-03 18:40:02 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "sievePH")` for more info

</details>

## Newly broken

*   checking whether package ‘sievePH’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘sievePH’
    See ‘/tmp/workdir/sievePH/new/sievePH.Rcheck/00install.out’ for details.
    ```

# SouthParkRshiny

<details>

* Version: 1.0.0
* GitHub: https://github.com/Amalan-ConStat/SouthParkRshiny
* Source code: https://github.com/cran/SouthParkRshiny
* Date/Publication: 2024-03-09 11:10:08 UTC
* Number of recursive dependencies: 118

Run `revdepcheck::cloud_details(, "SouthParkRshiny")` for more info

</details>

## Newly broken

*   checking whether package ‘SouthParkRshiny’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘SouthParkRshiny’
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

# spatialrisk

<details>

* Version: 0.7.1
* GitHub: https://github.com/mharinga/spatialrisk
* Source code: https://github.com/cran/spatialrisk
* Date/Publication: 2024-02-21 12:50:02 UTC
* Number of recursive dependencies: 129

Run `revdepcheck::cloud_details(, "spatialrisk")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘spatialrisk-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: choropleth_ggplot2
    > ### Title: Map object of class sf using ggplot2
    > ### Aliases: choropleth_ggplot2
    > 
    > ### ** Examples
    > 
    > test <- points_to_polygon(nl_postcode2, insurance, sum(amount, na.rm = TRUE))
    ...
     15.                       └─ggplot2 (local) FUN(X[[i]], ...)
     16.                         └─base::lapply(...)
     17.                           └─ggplot2 (local) FUN(X[[i]], ...)
     18.                             └─g$draw_key(data, g$params, key_size)
     19.                               └─ggplot2 (local) draw_key(...)
     20.                                 └─ggplot2::draw_key_polygon(data, params, size)
     21.                                   └─rlang:::Summary.quosure(from_theme(thin), 1.524, na.rm = FALSE)
     22.                                     └─rlang:::abort_quosure_op("Summary", .Generic)
     23.                                       └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.0Mb
      sub-directories of 1Mb or more:
        data   6.5Mb
        help   1.7Mb
        libs   2.7Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 283 marked UTF-8 strings
    ```

# spatialsample

<details>

* Version: 0.5.1
* GitHub: https://github.com/tidymodels/spatialsample
* Source code: https://github.com/cran/spatialsample
* Date/Publication: 2023-11-08 00:20:02 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::cloud_details(, "spatialsample")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘spatialsample-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: autoplot.spatial_rset
    > ### Title: Create a ggplot for spatial resamples.
    > ### Aliases: autoplot.spatial_rset autoplot.spatial_block_cv
    > 
    > ### ** Examples
    > 
    > 
    ...
     15.                       └─ggplot2 (local) FUN(X[[i]], ...)
     16.                         └─base::lapply(...)
     17.                           └─ggplot2 (local) FUN(X[[i]], ...)
     18.                             └─g$draw_key(data, g$params, key_size)
     19.                               └─ggplot2 (local) draw_key(...)
     20.                                 └─ggplot2::draw_key_polygon(data, params, size)
     21.                                   └─rlang:::Summary.quosure(from_theme(thin), 1.524, na.rm = FALSE)
     22.                                     └─rlang:::abort_quosure_op("Summary", .Generic)
     23.                                       └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(spatialsample)
      > 
      > sf::sf_extSoftVersion()
                GEOS           GDAL         proj.4 GDAL_with_GEOS     USE_PROJ_H 
             "3.8.0"        "3.0.4"        "6.3.1"         "true"         "true" 
                PROJ 
    ...
      • autoplot/buffered-rset-plot.svg
      • autoplot/buffered-vfold-plot.svg
      • autoplot/buffered-vfold-split.svg
      • autoplot/cluster-split-plots.svg
      • autoplot/repeated-block-cv.svg
      • autoplot/repeated-llo.svg
      • autoplot/repeated-vfold.svg
      • autoplot/snake-flips-rows-the-right-way.svg
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘spatialsample.Rmd’
      ...
    > autoplot(cluster_folds)
    
      When sourcing ‘spatialsample.R’:
    Error: Base operators are not defined for quosures. Do you need to unquote the
    quosure?
    
    # Bad: myquosure * rhs
    
    # Good: !!myquosure * rhs
    Execution halted
    
      ‘spatialsample.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘spatialsample.Rmd’ using rmarkdown
    
    Quitting from lines  at lines 56-62 [unnamed-chunk-6] (spatialsample.Rmd)
    Error: processing vignette 'spatialsample.Rmd' failed with diagnostics:
    Base operators are not defined for quosures. Do you need to unquote the
    quosure?
    
    # Bad: myquosure * rhs
    
    # Good: !!myquosure * rhs
    --- failed re-building ‘spatialsample.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘spatialsample.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# spinifex

<details>

* Version: 0.3.7.0
* GitHub: https://github.com/nspyrison/spinifex
* Source code: https://github.com/cran/spinifex
* Date/Publication: 2024-01-29 14:40:02 UTC
* Number of recursive dependencies: 164

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
       12.                 └─ggplot2 (local) compute_geom_2(..., self = self)
       13.                   └─self$geom$use_defaults(data, self$aes_params, modifiers, theme = theme)
       14.                     └─ggplot2 (local) use_defaults(..., self = self)
       15.                       └─ggplot2:::eval_from_theme(default_aes, theme)
       16.                         ├─calc_element("geom", theme) %||% .default_geom_element
       17.                         └─ggplot2::calc_element("geom", theme)
      
      [ FAIL 3 | WARN 4 | SKIP 0 | PASS 78 ]
      Error: Test failures
      Execution halted
    ```

# spmodel

<details>

* Version: 0.6.0
* GitHub: https://github.com/USEPA/spmodel
* Source code: https://github.com/cran/spmodel
* Date/Publication: 2024-04-16 23:40:02 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::cloud_details(, "spmodel")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘introduction.Rmd’
      ...
    8  186.3500     L     0        0 POINT (279050.9 1517324)
    9  362.3125     L     0        0 POINT (346145.9 1512479)
    10 430.5000     L     0        0 POINT (321354.6 1509966)
    
    > ggplot(moose, aes(color = presence)) + scale_color_viridis_d(option = "H") + 
    +     geom_sf(size = 2)
    
      When sourcing ‘introduction.R’:
    Error: 'language' object cannot be coerced to type 'integer'
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
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        R      1.9Mb
        data   1.7Mb
        doc    1.5Mb
    ```

# SqueakR

<details>

* Version: 1.3.0
* GitHub: https://github.com/osimon81/SqueakR
* Source code: https://github.com/cran/SqueakR
* Date/Publication: 2022-06-28 09:20:04 UTC
* Number of recursive dependencies: 142

Run `revdepcheck::cloud_details(, "SqueakR")` for more info

</details>

## Newly broken

*   checking whether package ‘SqueakR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘SqueakR’
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

# stats19

<details>

* Version: 3.0.3
* GitHub: https://github.com/ropensci/stats19
* Source code: https://github.com/cran/stats19
* Date/Publication: 2024-02-09 00:30:07 UTC
* Number of recursive dependencies: 161

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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘blog.Rmd’
      ...
    Try running dl_stats19(), change arguments or try later.FALSE
    Reading in: 
    
    
      When sourcing ‘blog.R’:
    Error: `file` is not one of the supported inputs:
    • A filepath or character vector of filepaths
    ...
    
      When sourcing ‘stats19.R’:
    Error: Unknown colour name: ~
    Execution halted
    
      ‘blog.Rmd’ using ‘UTF-8’... failed
      ‘stats19-training-setup.Rmd’ using ‘UTF-8’... OK
      ‘stats19-training.Rmd’ using ‘UTF-8’... failed
      ‘stats19-vehicles.Rmd’ using ‘UTF-8’... failed
      ‘stats19.Rmd’ using ‘UTF-8’... failed
    ```

# streamDepletr

<details>

* Version: 0.2.0
* GitHub: https://github.com/FoundrySpatial/streamDepletr
* Source code: https://github.com/cran/streamDepletr
* Date/Publication: 2023-07-19 21:30:02 UTC
* Number of recursive dependencies: 70

Run `revdepcheck::cloud_details(, "streamDepletr")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘intro-to-streamDepletr.Rmd’
      ...
    +   .... [TRUNCATED] 
    
      When sourcing ‘intro-to-streamDepletr.R’:
    Error: Base operators are not defined for quosures. Do you need to unquote the
    quosure?
    
    # Bad: myquosure * rhs
    
    # Good: !!myquosure * rhs
    Execution halted
    
      ‘intro-to-streamDepletr.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘intro-to-streamDepletr.Rmd’ using rmarkdown
    ```

# survminer

<details>

* Version: 0.4.9
* GitHub: https://github.com/kassambara/survminer
* Source code: https://github.com/cran/survminer
* Date/Publication: 2021-03-09 09:50:03 UTC
* Number of recursive dependencies: 130

Run `revdepcheck::cloud_details(, "survminer")` for more info

</details>

## Newly broken

*   checking whether package ‘survminer’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘survminer’
    See ‘/tmp/workdir/survminer/new/survminer.Rcheck/00install.out’ for details.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        doc   5.5Mb
    ```

# symptomcheckR

<details>

* Version: 0.1.3
* GitHub: https://github.com/ma-kopka/symptomcheckR
* Source code: https://github.com/cran/symptomcheckR
* Date/Publication: 2024-04-16 20:40:06 UTC
* Number of recursive dependencies: 101

Run `revdepcheck::cloud_details(, "symptomcheckR")` for more info

</details>

## Newly broken

*   checking whether package ‘symptomcheckR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘symptomcheckR’
    See ‘/tmp/workdir/symptomcheckR/new/symptomcheckR.Rcheck/00install.out’ for details.
    ```

# tabledown

<details>

* Version: 1.0.0
* GitHub: https://github.com/masiraji/tabledown
* Source code: https://github.com/cran/tabledown
* Date/Publication: 2024-05-02 13:40:03 UTC
* Number of recursive dependencies: 145

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
    Iteration: 18, Log-Lik: -5351.363, Max-Change: 0.00054
    Iteration: 19, Log-Lik: -5351.363, Max-Change: 0.00012
    Iteration: 20, Log-Lik: -5351.363, Max-Change: 0.00035
    Iteration: 21, Log-Lik: -5351.363, Max-Change: 0.00010
    > 
    > plot <- ggreliability_plotly(data, model)
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: ggreliability_plotly ... use_defaults -> eval_from_theme -> %||% -> calc_element
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 551 marked UTF-8 strings
    ```

# tcgaViz

<details>

* Version: 1.0.2
* GitHub: NA
* Source code: https://github.com/cran/tcgaViz
* Date/Publication: 2023-04-04 15:40:02 UTC
* Number of recursive dependencies: 139

Run `revdepcheck::cloud_details(, "tcgaViz")` for more info

</details>

## Newly broken

*   checking whether package ‘tcgaViz’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘tcgaViz’
    See ‘/tmp/workdir/tcgaViz/new/tcgaViz.Rcheck/00install.out’ for details.
    ```

# TCIU

<details>

* Version: 1.2.5
* GitHub: https://github.com/SOCR/TCIU
* Source code: https://github.com/cran/TCIU
* Date/Publication: 2024-03-08 17:00:05 UTC
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
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: fmri_image ... use_defaults -> eval_from_theme -> %||% -> calc_element
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘tciu-LT-kimesurface.Rmd’
      ...
    
    > sample_save[[2]]
    
      When sourcing ‘tciu-LT-kimesurface.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `compute_geom_2()`:
    ...
    
    > fmri_image(fmri_generate$fmri_data, option = "manually", 
    +     voxel_location = c(40, 22, 33), time = 4)
    
      When sourcing ‘tciu-fMRI-analytics.R’:
    Error: argument "theme" is missing, with no default
    Execution halted
    
      ‘tciu-LT-kimesurface.Rmd’ using ‘UTF-8’... failed
      ‘tciu-fMRI-analytics.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘tciu-LT-kimesurface.Rmd’ using rmarkdown
    
    Quitting from lines  at lines 159-160 [unnamed-chunk-5] (tciu-LT-kimesurface.Rmd)
    Error: processing vignette 'tciu-LT-kimesurface.Rmd' failed with diagnostics:
    Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `compute_geom_2()`:
    ! unused argument (theme = list(list("black", 0.727272727272727, 1, "butt", FALSE, TRUE), list("white", "black", 0.727272727272727, 1, TRUE), list("", "plain", "black", 16, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(4, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 4, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 4, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 
        NULL, 1, -90, NULL, c(0, 0, 0, 4), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(3.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 3.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 3.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 3.2), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0.5, NULL, 
    ...
    Quitting from lines  at lines 184-185 [unnamed-chunk-5] (tciu-fMRI-analytics.Rmd)
    Error: processing vignette 'tciu-fMRI-analytics.Rmd' failed with diagnostics:
    argument "theme" is missing, with no default
    --- failed re-building ‘tciu-fMRI-analytics.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘tciu-LT-kimesurface.Rmd’ ‘tciu-fMRI-analytics.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 14.7Mb
      sub-directories of 1Mb or more:
        data   1.8Mb
        doc   12.3Mb
    ```

# TestGardener

<details>

* Version: 3.3.3
* GitHub: NA
* Source code: https://github.com/cran/TestGardener
* Date/Publication: 2024-03-20 13:50:02 UTC
* Number of recursive dependencies: 131

Run `revdepcheck::cloud_details(, "TestGardener")` for more info

</details>

## Newly broken

*   checking whether package ‘TestGardener’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘TestGardener’
    See ‘/tmp/workdir/TestGardener/new/TestGardener.Rcheck/00install.out’ for details.
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
* Number of recursive dependencies: 193

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
     15. │           └─self$geom$use_defaults(data, self$aes_params, modifiers, theme = theme)
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
* Number of recursive dependencies: 141

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
       16. ├─plotly::config(...)
       17. │ └─plotly:::modify_list(p$x$config, args)
       18. │   ├─utils::modifyList(x %||% list(), y %||% list(), ...)
       19. │   │ └─base::stopifnot(is.list(x), is.list(val))
       20. │   └─x %||% list()
       21. └─plotly::layout(...)
      
      [ FAIL 1 | WARN 1 | SKIP 15 | PASS 91 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        data   1.6Mb
        doc    1.8Mb
    ```

# tidydr

<details>

* Version: 0.0.5
* GitHub: https://github.com/YuLab-SMU/tidydr
* Source code: https://github.com/cran/tidydr
* Date/Publication: 2023-03-08 09:20:02 UTC
* Number of recursive dependencies: 71

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

*   checking examples ... ERROR
    ```
    Running examples in ‘tidysdm-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: autoplot.spatial_initial_split
    > ### Title: Create a ggplot for a spatial initial rsplit.
    > ### Aliases: autoplot.spatial_initial_split
    > 
    > ### ** Examples
    > 
    > 
    ...
     15.                       └─ggplot2 (local) FUN(X[[i]], ...)
     16.                         └─base::lapply(...)
     17.                           └─ggplot2 (local) FUN(X[[i]], ...)
     18.                             └─g$draw_key(data, g$params, key_size)
     19.                               └─ggplot2 (local) draw_key(...)
     20.                                 └─ggplot2::draw_key_polygon(data, params, size)
     21.                                   └─rlang:::Summary.quosure(from_theme(thin), 1.524, na.rm = FALSE)
     22.                                     └─rlang:::abort_quosure_op("Summary", .Generic)
     23.                                       └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘a0_tidysdm_overview.Rmd’
      ...
    +     geom_sf(data = lacerta_thin, aes(col = class))
    
      When sourcing ‘a0_tidysdm_overview.R’:
    Error: Base operators are not defined for quosures. Do you need to unquote the
    quosure?
    
    # Bad: myquosure * rhs
    ...
    
    # Bad: myquosure * rhs
    
    # Good: !!myquosure * rhs
    Execution halted
    
      ‘a0_tidysdm_overview.Rmd’ using ‘UTF-8’... failed
      ‘a1_palaeodata_application.Rmd’ using ‘UTF-8’... failed
      ‘a2_tidymodels_additions.Rmd’ using ‘UTF-8’... failed
      ‘a3_troubleshooting.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘a0_tidysdm_overview.Rmd’ using rmarkdown
    ```

# tidyterra

<details>

* Version: 0.6.0
* GitHub: https://github.com/dieghernan/tidyterra
* Source code: https://github.com/cran/tidyterra
* Date/Publication: 2024-04-22 23:50:02 UTC
* Number of recursive dependencies: 102

Run `revdepcheck::cloud_details(, "tidyterra")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tidyterra-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: filter-joins.SpatVector
    > ### Title: Filtering joins for 'SpatVector' objects
    > ### Aliases: filter-joins.SpatVector semi_join.SpatVector
    > ###   anti_join.SpatVector
    > 
    > ### ** Examples
    > 
    ...
     15.                       └─ggplot2 (local) FUN(X[[i]], ...)
     16.                         └─base::lapply(...)
     17.                           └─ggplot2 (local) FUN(X[[i]], ...)
     18.                             └─g$draw_key(data, g$params, key_size)
     19.                               └─ggplot2 (local) draw_key(...)
     20.                                 └─ggplot2::draw_key_polygon(data, params, size)
     21.                                   └─rlang:::Summary.quosure(from_theme(thin), 1.524, na.rm = FALSE)
     22.                                     └─rlang:::abort_quosure_op("Summary", .Generic)
     23.                                       └─rlang::abort(...)
    Execution halted
    ```

# tidytransit

<details>

* Version: 1.6.1
* GitHub: https://github.com/r-transit/tidytransit
* Source code: https://github.com/cran/tidytransit
* Date/Publication: 2023-12-07 13:40:02 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "tidytransit")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘frequency.Rmd’
      ...
    +     labs(color = "H ..." ... [TRUNCATED] 
    
      When sourcing ‘frequency.R’:
    Error: Base operators are not defined for quosures. Do you need to unquote the
    quosure?
    
    # Bad: myquosure * rhs
    
    # Good: !!myquosure * rhs
    Execution halted
    
      ‘frequency.Rmd’ using ‘UTF-8’... failed
      ‘introduction.Rmd’ using ‘UTF-8’... OK
      ‘servicepatterns.Rmd’ using ‘UTF-8’... OK
      ‘timetable.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘frequency.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        doc       2.0Mb
        extdata   4.5Mb
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
    ! unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 
    2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, N
    Execution halted
    
      ‘use-tidytreatment-BART.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘use-tidytreatment-BART.Rmd’ using rmarkdown
    
    Quitting from lines  at lines 163-177 [plot-tidy-bart] (use-tidytreatment-BART.Rmd)
    Error: processing vignette 'use-tidytreatment-BART.Rmd' failed with diagnostics:
    Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `use_defaults()`:
    ! unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 
    ...
        NULL, NULL, NULL, NULL, 0.2, NULL, list(NULL, NULL, NULL, 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, c(0, 0, 0, 0), list(), 11, list("white", NA, NULL, NULL, TRUE), list(NA, "grey20", NULL, NULL, TRUE), 5.5, NULL, NULL, list("grey92", NULL, NULL, NULL, FALSE, TRUE), NULL, list(NULL, 0.5, NULL, NULL, FALSE, TRUE), NULL, NULL, 
        NULL, NULL, FALSE, list(NULL, "white", NULL, NULL, TRUE), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(5.5, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, TRUE), "topleft", NULL, c(5.5, 5.5, 5.5, 5.5), list("grey85", "grey20", NULL, NULL, TRUE), NULL, NULL, "inherit", "inside", list(NULL, 
            NULL, "grey10", 0.8, NULL, NULL, NULL, NULL, c(4.4, 4.4, 4.4, 4.4), NULL, TRUE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75))
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

# tilemaps

<details>

* Version: 0.2.0
* GitHub: https://github.com/kaerosen/tilemaps
* Source code: https://github.com/cran/tilemaps
* Date/Publication: 2020-07-10 04:20:02 UTC
* Number of recursive dependencies: 72

Run `revdepcheck::cloud_details(, "tilemaps")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘tilemaps.Rmd’
      ...
    +     fu .... [TRUNCATED] 
    
      When sourcing ‘tilemaps.R’:
    Error: Summary operations are not defined for quosures. Do you need to unquote
    the quosure?
    
    # Bad: min(myquosure)
    
    # Good: min(!!myquosure)
    Execution halted
    
      ‘tilemaps.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘tilemaps.Rmd’ using rmarkdown
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lwgeom’
      All declared Imports should be used.
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
       17.                 └─ggplot2 (local) compute_geom_2(..., self = self)
       18.                   └─self$geom$use_defaults(data, self$aes_params, modifiers, theme = theme)
       19.                     └─ggplot2 (local) use_defaults(..., self = self)
       20.                       └─ggplot2:::eval_from_theme(default_aes, theme)
       21.                         ├─calc_element("geom", theme) %||% .default_geom_element
       22.                         └─ggplot2::calc_element("geom", theme)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 406 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2750 marked UTF-8 strings
    ```

# tongfen

<details>

* Version: 0.3.5
* GitHub: https://github.com/mountainMath/tongfen
* Source code: https://github.com/cran/tongfen
* Date/Publication: 2022-04-28 18:50:02 UTC
* Number of recursive dependencies: 101

Run `revdepcheck::cloud_details(, "tongfen")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘polling_districts.Rmd’ using rmarkdown
    
    Quitting from lines  at lines 44-79 [unnamed-chunk-3] (polling_districts.Rmd)
    Error: processing vignette 'polling_districts.Rmd' failed with diagnostics:
    Summary operations are not defined for quosures. Do you need to unquote
    the quosure?
    
    # Bad: min(myquosure)
    
    ...
    --- finished re-building ‘tongfen_ca.Rmd’
    
    --- re-building ‘tongfen_us.Rmd’ using rmarkdown
    --- finished re-building ‘tongfen_us.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘polling_districts.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘polling_districts.Rmd’
      ...
    +     size = 0.2, color = "black") + facet_wrap("Year") + scale_fill_manual(values = party_colou .... [TRUNCATED] 
    
      When sourcing ‘polling_districts.R’:
    Error: Summary operations are not defined for quosures. Do you need to unquote
    the quosure?
    
    # Bad: min(myquosure)
    ...
    # Bad: min(myquosure)
    
    # Good: min(!!myquosure)
    Execution halted
    
      ‘polling_districts.Rmd’ using ‘UTF-8’... failed
      ‘tongfen-ca-estimate.Rmd’ using ‘UTF-8’... failed
      ‘tongfen.Rmd’ using ‘UTF-8’... failed
      ‘tongfen_ca.Rmd’ using ‘UTF-8’... failed
      ‘tongfen_us.Rmd’ using ‘UTF-8’... failed
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 160 marked UTF-8 strings
    ```

# TOSTER

<details>

* Version: 0.8.2
* GitHub: NA
* Source code: https://github.com/cran/TOSTER
* Date/Publication: 2024-04-16 16:40:02 UTC
* Number of recursive dependencies: 103

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
      unused argument (theme = list(list("black", 0.727272727272727, 1, "butt", FALSE, TRUE), list("white", "black", 0.727272727272727, 1, TRUE), list("", "plain", "black", 16, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.727272727272727, 1.45454545454545, "", 5.62335685623357, 2.18181818181818, 19, TRUE), NULL, NULL, list(NULL, NULL, "#333333", NULL, NULL, NULL, NULL, NULL, c(10, 0, 0, 0), NULL, FALSE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 4, 0), 
        NULL, TRUE), NULL, list(NULL, NULL, "#333333", NULL, NULL, NULL, 90, NULL, c(0, 10, 0, 0), NULL, FALSE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 4), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, "#333333", NULL, NULL, NULL, NULL, NULL, c(5, 0, 0, 0), NULL, FALSE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 3.2, 0), NULL, TRUE), NULL, list(), NULL, list(NULL, NULL, NULL, NU
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
       34. │                                         │ └─base::withCallingHandlers(...)
       35. │                                         └─layer$geom$use_defaults(...)
       36. └─base::.handleSimpleError(...)
       37.   └─rlang (local) h(simpleError(msg, call))
       38.     └─handlers[[1L]](cnd)
       39.       └─layer$geom$use_defaults(...)
      
      [ FAIL 8 | WARN 0 | SKIP 0 | PASS 1029 ]
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
    Error: unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(7, 0, 0, 0), NULL, FALSE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 7, 
    0, 0), NULL, FALSE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, "bold", NULL, 11, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, FALSE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 2.2), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0.5,
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

# TreatmentPatterns

<details>

* Version: 2.6.6
* GitHub: https://github.com/darwin-eu-dev/TreatmentPatterns
* Source code: https://github.com/cran/TreatmentPatterns
* Date/Publication: 2024-04-16 15:10:06 UTC
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
      
      [ FAIL 1 | WARN 0 | SKIP 31 | PASS 95 ]
      Error: Test failures
      Execution halted
    ```

# trelliscopejs

<details>

* Version: 0.2.6
* GitHub: https://github.com/hafen/trelliscopejs
* Source code: https://github.com/cran/trelliscopejs
* Date/Publication: 2021-02-01 08:00:02 UTC
* Number of recursive dependencies: 107

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
       14.                 └─ggplot2 (local) compute_geom_2(..., self = self)
       15.                   └─self$geom$use_defaults(data, self$aes_params, modifiers, theme = theme)
       16.                     └─ggplot2 (local) use_defaults(..., self = self)
       17.                       └─ggplot2:::eval_from_theme(default_aes, theme)
       18.                         ├─calc_element("geom", theme) %||% .default_geom_element
       19.                         └─ggplot2::calc_element("geom", theme)
      
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
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
        unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, list("black", "white", "#3366FF", 0.5, 1, "", 3.86605783866058, 1.5, 19, TRUE), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(7, 0, 0, 0), NULL, FALSE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 7, 
        0, 0), NULL, FALSE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 2.2), 
            NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0.5, NULL, NULL, NULL, c(0, 2.2, 0, 2.2), NULL, TRUE), list("grey70", 0.5, NULL, NULL, FALSE, TRUE), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 2.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(), list("gray70", 0.5, NULL, NULL, FALSE, FALSE), NULL, NULL, list("gray70", 0.5, NULL, NULL, FALSE, FALSE), NULL, NULL, NULL, NULL, list(NULL, NA, NULL, 
            NULL, TRUE), c(5.5, 5.5, 5.5, 5.5), 11, NULL, NULL, NULL, 1.2, NULL, NULL, 5.5, NULL, NULL, NULL, NULL, 0.2, NULL, list(NULL, NULL, NULL, 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, c(0, 0, 0, 0), list(), 11, list("white", NA, NULL, NULL, TRUE), list(), 5.5, NULL, NULL, list("grey87", NULL, NULL, NULL, FALSE, TRUE), list(), list(), 
            NULL, NULL, NULL, NULL, FALSE, list(NULL, "white", NULL, NULL, TRUE), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(5.5, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, TRUE), "topleft", NULL, c(5.5, 5.5, 5.5, 5.5), list("gray90", NA, NULL, NULL, FALSE), NULL, NULL, "inherit", "inside", 
            list(NULL, NULL, "black", 0.8, NULL, NULL, NULL, NULL, c(6, 6, 6, 6), NULL, FALSE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75))
      
      [ FAIL 1 | WARN 15 | SKIP 0 | PASS 108 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 206.1Mb
      sub-directories of 1Mb or more:
        libs  204.6Mb
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
    > main = system.file('extdata', package = 'umiAnalyzer')
    > samples <- list.dirs(path = main, full.names = FALSE, recursive = FALSE)
    > simsen <- createUmiExperiment(experimentName = 'example',mainDir = main,sampleNames = samples)
    > simsen <- filterUmiObject(simsen)
    > 
    > amplicon_plot <- AmpliconPlot(simsen)
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: AmpliconPlot ... use_defaults -> eval_from_theme -> %||% -> calc_element
    Execution halted
    ```

# UniprotR

<details>

* Version: 2.4.0
* GitHub: https://github.com/Proteomicslab57357/UniprotR
* Source code: https://github.com/cran/UniprotR
* Date/Publication: 2024-03-05 15:10:02 UTC
* Number of recursive dependencies: 192

Run `revdepcheck::cloud_details(, "UniprotR")` for more info

</details>

## Newly broken

*   checking whether package ‘UniprotR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘UniprotR’
    See ‘/tmp/workdir/UniprotR/new/UniprotR.Rcheck/00install.out’ for details.
    ```

# VALERIE

<details>

* Version: 1.1.0
* GitHub: NA
* Source code: https://github.com/cran/VALERIE
* Date/Publication: 2020-07-10 10:20:13 UTC
* Number of recursive dependencies: 133

Run `revdepcheck::cloud_details(, "VALERIE")` for more info

</details>

## Newly broken

*   checking whether package ‘VALERIE’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘VALERIE’
    See ‘/tmp/workdir/VALERIE/new/VALERIE.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘VALERIE.Rmd’ using rmarkdown
    Trying to upgrade TinyTeX automatically now...
    If reinstallation fails, try install_tinytex() again. Then install the following packages:
    
    tinytex::tlmgr_install(c("amscls", "amsfonts", "amsmath", "atbegshi", "atveryend", "auxhook", "babel", "bibtex", "bigintcalc", "bitset", "booktabs", "cm", "ctablestack", "dehyph", "dvipdfmx", "dvips", "ec", "epstopdf-pkg", "etex", "etexcmds", "etoolbox", "euenc", "everyshi", "fancyvrb", "filehook", "firstaid", "float", "fontspec", "framed", "geometry", "gettitlestring", "glyphlist", "graphics", "graphics-cfg", "graphics-def", "helvetic", "hycolor", "hyperref", "hyph-utf8", "hyphen-base", "iftex", "inconsolata", "infwarerr", "intcalc", "knuth-lib", "kpathsea", "kvdefinekeys", "kvoptions", "kvsetkeys", "l3backend", "l3kernel", "l3packages", "latex", "latex-amsmath-dev", "latex-bin", "latex-fonts", "latex-tools-dev", "latexconfig", "latexmk", "letltxmacro", "lm", "lm-math", "ltxcmds", "lua-alt-getopt", "lua-uni-algos", "luahbtex", "lualatex-math", "lualibs", "luaotfload", "luatex", "luatexbase", "mdwtools", "metafont", "mfware", "modes", "natbib", "pdfescape", "pdftex", "pdftexcmds", "plain", "psnfss", "refcount", "rerunfilecheck", "scheme-infraonly", "selnolig", "stringenc", "symbol", "tex", "tex-ini-files", "texlive-scripts", "texlive.infra", "times", "tipa", "tools", "unicode-data", "unicode-math", "uniquecounter", "url", "xcolor", "xetex", "xetexconfig", "xkeyval", "xunicode", "zapfding"))
    
    The directory /opt/TinyTeX/texmf-local is not empty. It will be backed up to /tmp/RtmpbQSeNG/file20363012f5cc and restored later.
    
    tlmgr: no auxiliary texmf trees defined, so nothing removed
    ...
    
    Error: processing vignette 'VALERIE.Rmd' failed with diagnostics:
    LaTeX failed to compile /tmp/workdir/VALERIE/old/VALERIE.Rcheck/vign_test/VALERIE/vignettes/VALERIE.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See VALERIE.log for more info.
    --- failed re-building ‘VALERIE.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘VALERIE.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.6Mb
      sub-directories of 1Mb or more:
        extdata   8.7Mb
    ```

# VancouvR

<details>

* Version: 0.1.8
* GitHub: https://github.com/mountainMath/VancouvR
* Source code: https://github.com/cran/VancouvR
* Date/Publication: 2024-04-18 16:12:35 UTC
* Number of recursive dependencies: 91

Run `revdepcheck::cloud_details(, "VancouvR")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Demo.Rmd’
      ...
    +     labs(title .... [TRUNCATED] 
    
      When sourcing ‘Demo.R’:
    Error: Summary operations are not defined for quosures. Do you need to unquote
    the quosure?
    
    # Bad: min(myquosure)
    
    # Good: min(!!myquosure)
    Execution halted
    
      ‘Demo.Rmd’ using ‘UTF-8’... failed
      ‘Isolines.Rmd’ using ‘UTF-8’... OK
    ```

# vannstats

<details>

* Version: 1.3.4.14
* GitHub: NA
* Source code: https://github.com/cran/vannstats
* Date/Publication: 2023-04-15 04:30:02 UTC
* Number of recursive dependencies: 101

Run `revdepcheck::cloud_details(, "vannstats")` for more info

</details>

## Newly broken

*   checking whether package ‘vannstats’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘vannstats’
    See ‘/tmp/workdir/vannstats/new/vannstats.Rcheck/00install.out’ for details.
    ```

# vici

<details>

* Version: 0.7.3
* GitHub: https://github.com/sistm/vici
* Source code: https://github.com/cran/vici
* Date/Publication: 2024-02-02 16:20:02 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::cloud_details(, "vici")` for more info

</details>

## Newly broken

*   checking whether package ‘vici’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::ggpar’ by ‘ggpubr::ggpar’ when loading ‘vici’
    See ‘/tmp/workdir/vici/new/vici.Rcheck/00install.out’ for details.
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
    7      m2   PB1 266     G     A    minor     0.022     0.978
    8      m2   PB2 199     A     G    minor     0.043     0.957
    9      m2   PB2  88     G     A    major     0.055     0.945
    10     m2   PB2 180     C     T    minor     0.011     0.989
    > 
    > snv_location(df)
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: snv_location ... use_defaults -> eval_from_theme -> %||% -> calc_element
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
      i Actually got a <simpleError> with text:
        argument "theme" is missing, with no default
      
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
    Error: argument "theme" is missing, with no default
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
* Number of recursive dependencies: 132

Run `revdepcheck::cloud_details(, "vvshiny")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘vvshiny-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggplotly_with_legend
    > ### Title: Make ggplotly and add legend with color as title
    > ### Aliases: ggplotly_with_legend
    > 
    > ### ** Examples
    > 
    > df <- data.frame(x_var = rnorm(100),
    ...
    > ggplot_instellingen <- ggplot2::geom_point()
    > scale_y <- ggplot2::scale_y_continuous()
    > plot <- basic_plot(df, "x_var", "y_var", "color_var", xlab_setting,
    +                    ylab_setting, ggplot_instellingen, "none", scale_y)
    > mapping_table <- list(color_var = "user friendly name var")
    > plotly_object <- ggplotly_with_legend(plot, "color_var", mapping_table)
    Error in compute_geom_2(..., self = self) : 
      argument "theme" is missing, with no default
    Calls: ggplotly_with_legend ... use_defaults -> eval_from_theme -> %||% -> calc_element
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
       11. │               └─self$geom$use_defaults(data, self$aes_params, modifiers, theme = theme)
       12. │                 └─ggplot2 (local) use_defaults(..., self = self)
       13. │                   └─ggplot2:::eval_from_theme(default_aes, theme)
       14. │                     ├─calc_element("geom", theme) %||% .default_geom_element
       15. │                     └─ggplot2::calc_element("geom", theme)
       16. └─plotly::layout(...)
      
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 60 ]
      Error: Test failures
      Execution halted
    ```

# waywiser

<details>

* Version: 0.5.1
* GitHub: https://github.com/ropensci/waywiser
* Source code: https://github.com/cran/waywiser
* Date/Publication: 2023-10-31 15:50:02 UTC
* Number of recursive dependencies: 172

Run `revdepcheck::cloud_details(, "waywiser")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘residual-autocorrelation.Rmd’
      ...
    +     weights)) %>% sf::st_ .... [TRUNCATED] 
    
      When sourcing ‘residual-autocorrelation.R’:
    Error: Summary operations are not defined for quosures. Do you need to unquote
    the quosure?
    
    # Bad: min(myquosure)
    
    # Good: min(!!myquosure)
    Execution halted
    
      ‘multi-scale-assessment.Rmd’ using ‘UTF-8’... OK
      ‘residual-autocorrelation.Rmd’ using ‘UTF-8’... failed
      ‘waywiser.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘multi-scale-assessment.Rmd’ using rmarkdown
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# wildlifeDI

<details>

* Version: 1.0.0
* GitHub: https://github.com/jedalong/wildlifeDI
* Source code: https://github.com/cran/wildlifeDI
* Date/Publication: 2024-03-22 19:30:02 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::cloud_details(, "wildlifeDI")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘wildlifeDI-vignette-contact_analysis.Rmd’
      ...
      GDAL Error 1: PROJ: proj_as_wkt: DatumEnsemble can only be exported to WKT2:2019
    
      When sourcing ‘wildlifeDI-vignette-contact_analysis.R’:
    Error: Base operators are not defined for quosures. Do you need to unquote the
    quosure?
    
    # Bad: myquosure * rhs
    
    # Good: !!myquosure * rhs
    Execution halted
    
      ‘wildlifeDI-vignette-contact_analysis.Rmd’ using ‘UTF-8’... failed
      ‘wildlifeDI-vignette.rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘wildlifeDI-vignette-contact_analysis.Rmd’ using rmarkdown
    
    Quitting from lines  at lines 53-55 [unnamed-chunk-3] (wildlifeDI-vignette-contact_analysis.Rmd)
    Error: processing vignette 'wildlifeDI-vignette-contact_analysis.Rmd' failed with diagnostics:
    Base operators are not defined for quosures. Do you need to unquote the
    quosure?
    
    # Bad: myquosure * rhs
    
    # Good: !!myquosure * rhs
    --- failed re-building ‘wildlifeDI-vignette-contact_analysis.Rmd’
    
    --- re-building ‘wildlifeDI-vignette.rmd’ using rmarkdown
    ```

# wilson

<details>

* Version: 2.4.2
* GitHub: https://github.com/loosolab/wilson
* Source code: https://github.com/cran/wilson
* Date/Publication: 2021-04-19 09:40:02 UTC
* Number of recursive dependencies: 200

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
        9.               └─ggplot2 (local) compute_geom_2(..., self = self)
       10.                 └─self$geom$use_defaults(data, self$aes_params, modifiers, theme = theme)
       11.                   └─ggplot2 (local) use_defaults(..., self = self)
       12.                     └─ggplot2:::eval_from_theme(default_aes, theme)
       13.                       ├─calc_element("geom", theme) %||% .default_geom_element
       14.                       └─ggplot2::calc_element("geom", theme)
      
      [ FAIL 3 | WARN 11 | SKIP 1 | PASS 74 ]
      Error: Test failures
      Execution halted
    ```

# WorldMapR

<details>

* Version: 0.1.1
* GitHub: https://github.com/Luigi-Annic/WorldMapR
* Source code: https://github.com/cran/WorldMapR
* Date/Publication: 2024-04-22 19:30:07 UTC
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "WorldMapR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘WorldMapR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: worldplotCat
    > ### Title: worldplotCat
    > ### Aliases: worldplotCat
    > 
    > ### ** Examples
    > 
    > data(testdata1b)
    ...
     16.                         └─ggplot2 (local) FUN(X[[i]], ...)
     17.                           └─base::lapply(...)
     18.                             └─ggplot2 (local) FUN(X[[i]], ...)
     19.                               └─g$draw_key(data, g$params, key_size)
     20.                                 └─ggplot2 (local) draw_key(...)
     21.                                   └─ggplot2::draw_key_polygon(data, params, size)
     22.                                     └─rlang:::Summary.quosure(from_theme(thin), 1.27, na.rm = FALSE)
     23.                                       └─rlang:::abort_quosure_op("Summary", .Generic)
     24.                                         └─rlang::abort(...)
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
       19.                               └─g$draw_key(data, g$params, key_size)
       20.                                 └─ggplot2 (local) draw_key(...)
       21.                                   └─ggplot2::draw_key_polygon(data, params, size)
       22.                                     └─rlang:::Summary.quosure(from_theme(thin), 1.27, na.rm = FALSE)
       23.                                       └─rlang:::abort_quosure_op("Summary", .Generic)
       24.                                         └─rlang::abort(...)
      
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4 marked Latin-1 strings
      Note: found 5 marked UTF-8 strings
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
    
    Quitting from lines  at lines 43-43 [unnamed-chunk-2] (./../man/fragments/_quick-intro.Rmd)
    
      When tangling ‘xaringanthemer.Rmd’:
    Error: cannot open the connection
    Execution halted
    
      ‘ggplot2-themes.Rmd’ using ‘UTF-8’... OK
      ‘template-variables.Rmd’ using ‘UTF-8’... OK
      ‘xaringanthemer.Rmd’ using ‘UTF-8’... failed
    ```

