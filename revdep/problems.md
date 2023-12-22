# AcademicThemes

<details>

* Version: 0.0.1
* GitHub: https://github.com/hwarden162/AcademicThemes
* Source code: https://github.com/cran/AcademicThemes
* Date/Publication: 2023-03-27 12:40:02 UTC
* Number of recursive dependencies: 119

Run `revdepcheck::cloud_details(, "AcademicThemes")` for more info

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
      `environment(actual$super)$env$call`:   `scale_fill_academic_d(palette_name)`
      `environment(expected$super)$env$call`: `eval(code, test_env)`               
      
      environment(actual$super)$members$call vs environment(expected$super)$members$call
      - `scale_fill_academic_d(palette_name)`
      + `eval(code, test_env)`
      
      [ FAIL 124 | WARN 124 | SKIP 0 | PASS 133 ]
      Error: Test failures
      Execution halted
    ```

# accrualPlot

<details>

* Version: 1.0.7
* GitHub: https://github.com/CTU-Bern/accrualPlot
* Source code: https://github.com/cran/accrualPlot
* Date/Publication: 2022-08-16 10:10:05 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::cloud_details(, "accrualPlot")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘accrualPlot.Rmd’ using rmarkdown
    
    Quitting from lines 104-120 [unnamed-chunk-9] (accrualPlot.Rmd)
    Error: processing vignette 'accrualPlot.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘accrualPlot.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘accrualPlot.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# activAnalyzer

<details>

* Version: 2.0.1
* GitHub: https://github.com/pydemull/activAnalyzer
* Source code: https://github.com/cran/activAnalyzer
* Date/Publication: 2023-02-13 09:50:06 UTC
* Number of recursive dependencies: 152

Run `revdepcheck::cloud_details(, "activAnalyzer")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘activAnalyzer.Rmd’ using rmarkdown
    
    Quitting from lines 185-203 [unnamed-chunk-17] (activAnalyzer.Rmd)
    Error: processing vignette 'activAnalyzer.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘activAnalyzer.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘activAnalyzer.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        doc       1.0Mb
        extdata   2.0Mb
    ```

# afex

<details>

* Version: 1.3-0
* GitHub: https://github.com/singmann/afex
* Source code: https://github.com/cran/afex
* Date/Publication: 2023-04-17 22:00:02 UTC
* Number of recursive dependencies: 226

Run `revdepcheck::cloud_details(, "afex")` for more info

</details>

## Newly broken

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

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘rstanarm’
    ```

# AgroReg

<details>

* Version: 1.2.9
* GitHub: NA
* Source code: https://github.com/cran/AgroReg
* Date/Publication: 2023-01-18 12:10:17 UTC
* Number of recursive dependencies: 117

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
    Error in `adjust_scale()`:
    ! Cannot add <ggproto> objects together.
    ℹ Did you forget to add this object to a <ggplot> object?
    Backtrace:
        ▆
     1. └─AgroReg::adjust_scale(...)
     2.   └─ggplot2:::`+.gg`(plots, scale_y_continuous(breaks = scale.y, limits = limits.y))
     3.     └─cli::cli_abort(...)
     4.       └─rlang::abort(...)
    Execution halted
    ```

# ale

<details>

* Version: 0.2.0
* GitHub: https://github.com/Tripartio/ale
* Source code: https://github.com/cran/ale
* Date/Publication: 2023-10-19 21:30:05 UTC
* Number of recursive dependencies: 90

Run `revdepcheck::cloud_details(, "ale")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ale-ALEPlot.Rmd’ using rmarkdown
    
    Quitting from lines 261-286 [ale one-way link] (ale-ALEPlot.Rmd)
    Error: processing vignette 'ale-ALEPlot.Rmd' failed with diagnostics:
    Problem while computing aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `scales_add_defaults()`:
    ! could not find function "scales_add_defaults"
    --- failed re-building ‘ale-ALEPlot.Rmd’
    ...
    --- finished re-building ‘ale-statistics.Rmd’
    
    --- re-building ‘ale-x-datatypes.Rmd’ using rmarkdown
    --- finished re-building ‘ale-x-datatypes.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ale-ALEPlot.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# animbook

<details>

* Version: 1.0.0
* GitHub: https://github.com/KrisanatA/animbook
* Source code: https://github.com/cran/animbook
* Date/Publication: 2023-12-05 17:50:07 UTC
* Number of recursive dependencies: 77

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
    Error in train(..., self = self) : 
      unused argument (list("x", "y", "id", "group", "as.factor(id)", "label"))
    Calls: <Anonymous> ... ggplotly.ggplot -> gg2list -> get_gdefs_ggproto -> <Anonymous>
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
    +     theme_void()
    > ap <- p %>% 
    +     insert_top(p2, height=.3) %>% 
    +     insert_right(p3, width=.1)
    > 
    > ap
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

# aplotExtra

<details>

* Version: 0.0.2
* GitHub: https://github.com/YuLab-SMU/aplotExtra
* Source code: https://github.com/cran/aplotExtra
* Date/Publication: 2023-08-25 17:20:02 UTC
* Number of recursive dependencies: 66

Run `revdepcheck::cloud_details(, "aplotExtra")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘aplotExtra-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: upset_plot
    > ### Title: upsetplot2
    > ### Aliases: upset_plot
    > 
    > ### ** Examples
    > 
    >  list = list(A = sample(LETTERS, 20),
    +              B = sample(LETTERS, 22),
    +              C = sample(LETTERS, 14),
    +              D = sample(LETTERS, 30, replace = TRUE))
    >  upset_plot(list)
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

# appeears

<details>

* Version: 1.1
* GitHub: https://github.com/bluegreen-labs/appeears
* Source code: https://github.com/cran/appeears
* Date/Publication: 2023-09-15 15:02:06 UTC
* Number of recursive dependencies: 122

Run `revdepcheck::cloud_details(, "appeears")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘appeears_vignette.Rmd’ using rmarkdown
    
    Quitting from lines 398-457 [unnamed-chunk-14] (appeears_vignette.Rmd)
    Error: processing vignette 'appeears_vignette.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘appeears_vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘appeears_vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ASRgenomics

<details>

* Version: 1.1.3
* GitHub: NA
* Source code: https://github.com/cran/ASRgenomics
* Date/Publication: 2022-11-28 12:30:10 UTC
* Number of recursive dependencies: 131

Run `revdepcheck::cloud_details(, "ASRgenomics")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  8.9Mb
      sub-directories of 1Mb or more:
        data   8.4Mb
    ```

# assignPOP

<details>

* Version: 1.2.4
* GitHub: https://github.com/alexkychen/assignPOP
* Source code: https://github.com/cran/assignPOP
* Date/Publication: 2021-10-27 19:30:02 UTC
* Number of recursive dependencies: 142

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
      Actual value: "List of 11\\n \$ data       :'data\.frame':\\t72 obs\. of  6 variables:\\n  \.\.\$ Ind\.ID    : Factor w/ 24 levels "A10","A12","AA9",\.\.: 3 1 2 5 6 4 7 8 11 9 \.\.\.\\n  \.\.\$ origin\.pop: Factor w/ 3 levels "pop\.1","pop\.2",\.\.: 1 1 1 2 2 2 3 3 1 1 \.\.\.\\n  \.\.\$ pred\.pop  : Factor w/ 3 levels "pop\.1","pop\.3",\.\.: 1 2 2 1 1 1 1 1 2 2 \.\.\.\\n  \.\.\$ fold_n    : chr \[1:72\] "fold_1" "fold_1" "fold_1" "fold_1" \.\.\.\\n  \.\.\$ variable  : Factor w/ 3 levels "pop\.1","pop\.2",\.\.: 1 1 1 1 1 1 1 1 1 1 \.\.\.\\n  \.\.\$ value     : num \[1:72\] 0\.4 0\.326 0\.26 0\.383 0\.44 \.\.\.\\n \$ layers     :List of 1\\n  \.\.\$ :Classes 'LayerInstance', 'Layer', 'ggproto', 'gg' <ggproto object: Class LayerInstance, Layer, gg>\\n    aes_params: list\\n    compute_aesthetics: function\\n    compute_geom_1: function\\n    compute_geom_2: function\\n    compute_position: function\\n    compute_statistic: function\\n    computed_geom_params: NULL\\n    computed_mapping: NULL\\n    computed_stat_params: NULL\\n    constructor: call\\n    data: waiver\\n    draw_geom: function\\n    finish_statistics: function\\n    geom: <ggproto object: Class GeomBar, GeomRect, Geom, gg>\\n        aesthetics: function\\n        default_aes: uneval\\n        draw_group: function\\n        draw_key: function\\n        draw_layer: function\\n        draw_panel: function\\n        extra_params: just na\.rm orientation\\n        handle_na: function\\n        non_missing_aes: xmin xmax ymin ymax\\n        optional_aes: \\n        parameters: function\\n        rename_size: TRUE\\n        required_aes: x y\\n        setup_data: function\\n        setup_params: function\\n        use_defaults: function\\n        super:  <ggproto object: Class GeomRect, Geom, gg>\\n    geom_params: list\\n    inherit\.aes: TRUE\\n    layer_data: function\\n    map_statistic: function\\n    mapping: NULL\\n    position: <ggproto object: Class PositionStack, Position, gg>\\n        compute_layer: function\\n        compute_panel: function\\n        fill: FALSE\\n        required_aes: \\n        reverse: FALSE\\n        setup_data: function\\n        setup_params: function\\n        type: NULL\\n        vjust: 1\\n        super:  <ggproto object: Class Position, gg>\\n    print: function\\n    setup_layer: function\\n    show\.legend: NA\\n    stat: <ggproto object: Class StatIdentity, Stat, gg>\\n        aesthetics: function\\n        compute_group: function\\n        compute_layer: function\\n        compute_panel: function\\n        default_aes: uneval\\n        dropped_aes: \\n        extra_params: na\.rm\\n        finish_layer: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        required_aes: \\n        retransform: TRUE\\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Stat, gg>\\n    stat_params: list\\n    super:  <ggproto object: Class Layer, gg> \\n \$ scales     :Classes 'ScalesList', 'ggproto', 'gg' <ggproto object: Class ScalesList, gg>\\n    add: function\\n    add_defaults: function\\n    add_missing: function\\n    backtransform_df: function\\n    clone: function\\n    find: function\\n    get_scales: function\\n    has_scale: function\\n    input: function\\n    map_df: function\\n    n: function\\n    non_position_scales: function\\n    scales: list\\n    train_df: function\\n    transform_df: function\\n    super:  <ggproto object: Class ScalesList, gg> \\n \$ guides     :Classes 'Guides', 'ggproto', 'gg' <ggproto object: Class Guides, gg>\\n    add: function\\n    assemble: function\\n    build: function\\n    draw: function\\n    get_custom: function\\n    get_guide: function\\n    get_params: function\\n    get_position: function\\n    guides: list\\n    merge: function\\n    missing: <ggproto object: Class GuideNone, Guide, gg>\\n        arrange_layout: function\\n        assemble_drawing: function\\n        available_aes: any\\n        build_decor: function\\n        build_labels: function\\n        build_ticks: function\\n        build_title: function\\n        draw: function\\n        draw_early_exit: function\\n        elements: list\\n        extract_decor: function\\n        extract_key: function\\n        extract_params: function\\n        get_layer_key: function\\n        hashables: list\\n        measure_grobs: function\\n        merge: function\\n        override_elements: function\\n        params: list\\n        process_layers: function\\n        setup_elements: function\\n        setup_params: function\\n        train: function\\n        transform: function\\n        super:  <ggproto object: Class GuideNone, Guide, gg>\\n    package_box: function\\n    print: function\\n    process_layers: function\\n    setup: function\\n    subset_guides: function\\n    train: function\\n    update_params: function\\n    super:  <ggproto object: Class Guides, gg> \\n \$ mapping    :List of 3\\n  \.\.\$ x   : language ~Ind\.ID\\n  \.\. \.\.- attr\(\*, "\.Environment"\)=<environment: 0x56384f29c298> \\n  \.\.\$ y   : language ~value\\n  \.\. \.\.- attr\(\*, "\.Environment"\)=<environment: 0x56384f29c298> \\n  \.\.\$ fill: language ~variable\\n  \.\. \.\.- attr\(\*, "\.Environment"\)=<environment: 0x56384f29c298> \\n  \.\.- attr\(\*, "class"\)= chr "uneval"\\n \$ theme      :List of 124\\n  \.\.\$ line                            :List of 6\\n  \.\. \.\.\$ colour       : chr "black"\\n  \.\. \.\.\$ linewidth    : num 0\.5\\n  \.\. \.\.\$ linetype     : num 1\\n  \.\. \.\.\$ lineend      : chr "butt"\\n  \.\. \.\.\$ arrow        : logi FALSE\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_line" "element"\\n  \.\.\$ rect                            :List of 5\\n  \.\. \.\.\$ fill         : chr "white"\\n  \.\. \.\.\$ colour       : chr "black"\\n  \.\. \.\.\$ linewidth    : num 0\.5\\n  \.\. \.\.\$ linetype     : num 1\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_rect" "element"\\n  \.\.\$ text                            :List of 11\\n  \.\. \.\.\$ family       : chr ""\\n  \.\. \.\.\$ face         : chr "plain"\\n  \.\. \.\.\$ colour       : chr "black"\\n  \.\. \.\.\$ size         : num 11\\n  \.\. \.\.\$ hjust        : num 0\.5\\n  \.\. \.\.\$ vjust        : num 0\.5\\n  \.\. \.\.\$ angle        : num 0\\n  \.\. \.\.\$ lineheight   : num 0\.9\\n  \.\. \.\.\$ margin       : 'margin' num \[1:4\] 0points 0points 0points 0points\\n  \.\. \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\. \.\.\$ debug        : logi FALSE\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ title                           : NULL\\n  \.\.\$ aspect\.ratio                    : NULL\\n  \.\.\$ axis\.title                      : NULL\\n  \.\.\$ axis\.title\.x                    : list\(\)\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_blank" "element"\\n  \.\.\$ axis\.title\.x\.top                :List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : NULL\\n  \.\. \.\.\$ size         : NULL\\n  \.\. \.\.\$ hjust        : NULL\\n  \.\. \.\.\$ vjust        : num 0\\n  \.\. \.\.\$ angle        : NULL\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : 'margin' num \[1:4\] 0points 0points 2\.75points 0points\\n  \.\. \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ axis\.title\.x\.bottom             : NULL\\n  \.\.\$ axis\.title\.y                    :List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : NULL\\n  \.\. \.\.\$ size         : NULL\\n  \.\. \.\.\$ hjust        : NULL\\n  \.\. \.\.\$ vjust        : num 1\\n  \.\. \.\.\$ angle        : num 90\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : 'margin' num \[1:4\] 0points 2\.75points 0points 0points\\n  \.\. \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ axis\.title\.y\.left               : NULL\\n  \.\.\$ axis\.title\.y\.right              :List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : NULL\\n  \.\. \.\.\$ size         : NULL\\n  \.\. \.\.\$ hjust        : NULL\\n  \.\. \.\.\$ vjust        : num 0\\n  \.\. \.\.\$ angle        : num -90\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : 'margin' num \[1:4\] 0points 0points 0points 2\.75points\\n  \.\. \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ axis\.text                       :List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : chr "grey30"\\n  \.\. \.\.\$ size         : 'rel' num 0\.8\\n  \.\. \.\.\$ hjust        : NULL\\n  \.\. \.\.\$ vjust        : NULL\\n  \.\. \.\.\$ angle        : NULL\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : NULL\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ axis\.text\.x                     :List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : NULL\\n  \.\. \.\.\$ size         : NULL\\n  \.\. \.\.\$ hjust        : NULL\\n  \.\. \.\.\$ vjust        : num 1\\n  \.\. \.\.\$ angle        : num 90\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : 'margin' num \[1:4\] 2\.2points 0points 0points 0points\\n  \.\. \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi FALSE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ axis\.text\.x\.top                 :List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : NULL\\n  \.\. \.\.\$ size         : NULL\\n  \.\. \.\.\$ hjust        : NULL\\n  \.\. \.\.\$ vjust        : num 0\\n  \.\. \.\.\$ angle        : NULL\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : 'margin' num \[1:4\] 0points 0points 2\.2points 0points\\n  \.\. \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ axis\.text\.x\.bottom              : NULL\\n  \.\.\$ axis\.text\.y                     :List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : NULL\\n  \.\. \.\.\$ size         : NULL\\n  \.\. \.\.\$ hjust        : num 1\\n  \.\. \.\.\$ vjust        : NULL\\n  \.\. \.\.\$ angle        : NULL\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : 'margin' num \[1:4\] 0points 2\.2points 0points 0points\\n  \.\. \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ axis\.text\.y\.left                : NULL\\n  \.\.\$ axis\.text\.y\.right               :List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : NULL\\n  \.\. \.\.\$ size         : NULL\\n  \.\. \.\.\$ hjust        : num 0\\n  \.\. \.\.\$ vjust        : NULL\\n  \.\. \.\.\$ angle        : NULL\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : 'margin' num \[1:4\] 0points 0points 0points 2\.2points\\n  \.\. \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ axis\.ticks                      :List of 6\\n  \.\. \.\.\$ colour       : chr "grey20"\\n  \.\. \.\.\$ linewidth    : NULL\\n  \.\. \.\.\$ linetype     : NULL\\n  \.\. \.\.\$ lineend      : NULL\\n  \.\. \.\.\$ arrow        : logi FALSE\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_line" "element"\\n  \.\.\$ axis\.ticks\.x                    : NULL\\n  \.\.\$ axis\.ticks\.x\.top                : NULL\\n  \.\.\$ axis\.ticks\.x\.bottom             : NULL\\n  \.\.\$ axis\.ticks\.y                    : NULL\\n  \.\.\$ axis\.ticks\.y\.left               : NULL\\n  \.\.\$ axis\.ticks\.y\.right              : NULL\\n  \.\.\$ axis\.minor\.ticks\.x\.top          : NULL\\n  \.\.\$ axis\.minor\.ticks\.x\.bottom       : NULL\\n  \.\.\$ axis\.minor\.ticks\.y\.left         : NULL\\n  \.\.\$ axis\.minor\.ticks\.y\.right        : NULL\\n  \.\.\$ axis\.ticks\.length               : 'simpleUnit' num 2\.75points\\n  \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\.\$ axis\.ticks\.length\.x             : NULL\\n  \.\.\$ axis\.ticks\.length\.x\.top         : NULL\\n  \.\.\$ axis\.ticks\.length\.x\.bottom      : NULL\\n  \.\.\$ axis\.ticks\.length\.y             : NULL\\n  \.\.\$ axis\.ticks\.length\.y\.left        : NULL\\n  \.\.\$ axis\.ticks\.length\.y\.right       : NULL\\n  \.\.\$ axis\.minor\.ticks\.length         : 'rel' num 0\.75\\n  \.\.\$ axis\.minor\.ticks\.length\.x       : NULL\\n  \.\.\$ axis\.minor\.ticks\.length\.x\.top   : NULL\\n  \.\.\$ axis\.minor\.ticks\.length\.x\.bottom: NULL\\n  \.\.\$ axis\.minor\.ticks\.length\.y       : NULL\\n  \.\.\$ axis\.minor\.ticks\.length\.y\.left  : NULL\\n  \.\.\$ axis\.minor\.ticks\.length\.y\.right : NULL\\n  \.\.\$ axis\.line                       : list\(\)\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_blank" "element"\\n  \.\.\$ axis\.line\.x                     : NULL\\n  \.\.\$ axis\.line\.x\.top                 : NULL\\n  \.\.\$ axis\.line\.x\.bottom              : NULL\\n  \.\.\$ axis\.line\.y                     : NULL\\n  \.\.\$ axis\.line\.y\.left                : NULL\\n  \.\.\$ axis\.line\.y\.right               : NULL\\n  \.\.\$ legend\.background               :List of 5\\n  \.\. \.\.\$ fill         : NULL\\n  \.\. \.\.\$ colour       : logi NA\\n  \.\. \.\.\$ linewidth    : NULL\\n  \.\. \.\.\$ linetype     : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_rect" "element"\\n  \.\.\$ legend\.margin                   : 'margin' num \[1:4\] 5\.5points 5\.5points 5\.5points 5\.5points\\n  \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\.\$ legend\.spacing                  : 'simpleUnit' num 11points\\n  \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\.\$ legend\.spacing\.x                : NULL\\n  \.\.\$ legend\.spacing\.y                : NULL\\n  \.\.\$ legend\.key                      : NULL\\n  \.\.\$ legend\.key\.size                 : 'simpleUnit' num 1\.2lines\\n  \.\. \.\.- attr\(\*, "unit"\)= int 3\\n  \.\.\$ legend\.key\.height               : NULL\\n  \.\.\$ legend\.key\.width                : NULL\\n  \.\.\$ legend\.key\.spacing              : 'simpleUnit' num 5\.5points\\n  \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\.\$ legend\.key\.spacing\.x            : NULL\\n  \.\.\$ legend\.key\.spacing\.y            : NULL\\n  \.\.\$ legend\.frame                    : NULL\\n  \.\.\$ legend\.ticks                    : NULL\\n  \.\.\$ legend\.ticks\.length             : NULL\\n  \.\.\$ legend\.axis\.line                : NULL\\n  \.\.\$ legend\.text                     :List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : NULL\\n  \.\. \.\.\$ size         : 'rel' num 0\.8\\n  \.\. \.\.\$ hjust        : NULL\\n  \.\. \.\.\$ vjust        : NULL\\n  \.\. \.\.\$ angle        : NULL\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : NULL\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ legend\.text\.position            : NULL\\n  \.\.\$ legend\.title                    :List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : NULL\\n  \.\. \.\.\$ size         : NULL\\n  \.\. \.\.\$ hjust        : num 0\\n  \.\. \.\.\$ vjust        : NULL\\n  \.\. \.\.\$ angle        : NULL\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : NULL\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ legend\.title\.position           : NULL\\n  \.\.\$ legend\.position                 : chr "right"\\n  \.\.\$ legend\.position\.inside          : NULL\\n  \.\.\$ legend\.direction                : NULL\\n  \.\.\$ legend\.byrow                    : NULL\\n  \.\.\$ legend\.justification            : chr "center"\\n  \.\.\$ legend\.justification\.top        : NULL\\n  \.\.\$ legend\.justification\.bottom     : NULL\\n  \.\.\$ legend\.justification\.left       : NULL\\n  \.\.\$ legend\.justification\.right      : NULL\\n  \.\.\$ legend\.justification\.inside     : NULL\\n  \.\.\$ legend\.location                 : NULL\\n  \.\.\$ legend\.box                      : NULL\\n  \.\.\$ legend\.box\.just                 : NULL\\n  \.\.\$ legend\.box\.margin               : 'margin' num \[1:4\] 0cm 0cm 0cm 0cm\\n  \.\. \.\.- attr\(\*, "unit"\)= int 1\\n  \.\.\$ legend\.box\.background           : list\(\)\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_blank" "element"\\n  \.\.\$ legend\.box\.spacing              : 'simpleUnit' num 11points\\n  \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\.\$ panel\.background                :List of 5\\n  \.\. \.\.\$ fill         : chr "white"\\n  \.\. \.\.\$ colour       : logi NA\\n  \.\. \.\.\$ linewidth    : NULL\\n  \.\. \.\.\$ linetype     : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_rect" "element"\\n  \.\.\$ panel\.border                    :List of 5\\n  \.\. \.\.\$ fill         : logi NA\\n  \.\. \.\.\$ colour       : chr "grey20"\\n  \.\. \.\.\$ linewidth    : NULL\\n  \.\. \.\.\$ linetype     : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_rect" "element"\\n  \.\.\$ panel\.spacing                   : 'simpleUnit' num 5\.5points\\n  \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\.\$ panel\.spacing\.x                 : NULL\\n  \.\.\$ panel\.spacing\.y                 : NULL\\n  \.\.\$ panel\.grid                      :List of 6\\n  \.\. \.\.\$ colour       : chr "grey92"\\n  \.\. \.\.\$ linewidth    : NULL\\n  \.\. \.\.\$ linetype     : NULL\\n  \.\. \.\.\$ lineend      : NULL\\n  \.\. \.\.\$ arrow        : logi FALSE\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_line" "element"\\n  \.\.\$ panel\.grid\.major                : list\(\)\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_blank" "element"\\n  \.\.\$ panel\.grid\.minor                : list\(\)\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_blank" "element"\\n  \.\.\$ panel\.grid\.major\.x              : NULL\\n  \.\.\$ panel\.grid\.major\.y              : NULL\\n  \.\.\$ panel\.grid\.minor\.x              : NULL\\n  \.\.\$ panel\.grid\.minor\.y              : NULL\\n  \.\. \[list output truncated\]\\n  \.\.- attr\(\*, "class"\)= chr \[1:2\] "theme" "gg"\\n  \.\.- attr\(\*, "complete"\)= logi TRUE\\n  \.\.- attr\(\*, "validate"\)= logi TRUE\\n \$ coordinates:Classes 'CoordCartesian', 'Coord', 'ggproto', 'gg' <ggproto object: Class CoordCartesian, Coord, gg>\\n    aspect: function\\n    backtransform_range: function\\n    clip: on\\n    default: FALSE\\n    distance: function\\n    expand: TRUE\\n    is_free: function\\n    is_linear: function\\n    labels: function\\n    limits: list\\n    modify_scales: function\\n    range: function\\n    render_axis_h: function\\n    render_axis_v: function\\n    render_bg: function\\n    render_fg: function\\n    setup_data: function\\n    setup_layout: function\\n    setup_panel_guides: function\\n    setup_panel_params: function\\n    setup_params: function\\n    train_panel_guides: function\\n    transform: function\\n    super:  <ggproto object: Class CoordCartesian, Coord, gg> \\n \$ facet      :Classes 'FacetGrid', 'Facet', 'ggproto', 'gg' <ggproto object: Class FacetGrid, Facet, gg>\\n    compute_layout: function\\n    draw_back: function\\n    draw_front: function\\n    draw_labels: function\\n    draw_panels: function\\n    finish_data: function\\n    init_scales: function\\n    map_data: function\\n    params: list\\n    setup_data: function\\n    setup_params: function\\n    shrink: TRUE\\n    train_scales: function\\n    vars: function\\n    super:  <ggproto object: Class FacetGrid, Facet, gg> \\n \$ plot_env   :<environment: 0x56384f29c298> \\n \$ layout     :Classes 'Layout', 'ggproto', 'gg' <ggproto object: Class Layout, gg>\\n    coord: NULL\\n    coord_params: list\\n    facet: NULL\\n    facet_params: list\\n    finish_data: function\\n    get_scales: function\\n    layout: NULL\\n    map_position: function\\n    panel_params: NULL\\n    panel_scales_x: NULL\\n    panel_scales_y: NULL\\n    render: function\\n    render_labels: function\\n    reset_scales: function\\n    resolve_label: function\\n    setup: function\\n    setup_panel_guides: function\\n    setup_panel_params: function\\n    train_position: function\\n    super:  <ggproto object: Class Layout, gg> \\n \$ labels     :List of 4\\n  \.\.\$ title: chr "K = 3  "\\n  \.\.\$ y    : chr "Probability"\\n  \.\.\$ x    : chr "Ind\.ID"\\n  \.\.\$ fill : chr "variable"\\n - attr\(\*, "class"\)= chr \[1:2\] "gg" "ggplot""
      Backtrace:
          ▆
       1. └─testthat::expect_output(str(plot), "List of 10") at test_membership.R:5:3
       2.   └─testthat::expect_match(...)
       3.     └─testthat:::expect_match_(...)
      
      [ FAIL 3 | WARN 1 | SKIP 0 | PASS 39 ]
      Error: Test failures
      Execution halted
    ```

# augmentedRCBD

<details>

* Version: 0.1.7
* GitHub: https://github.com/aravind-j/augmentedRCBD
* Source code: https://github.com/cran/augmentedRCBD
* Date/Publication: 2023-08-19 00:12:38 UTC
* Number of recursive dependencies: 129

Run `revdepcheck::cloud_details(, "augmentedRCBD")` for more info

</details>

## Newly broken

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
    
    Error: processing vignette 'Data_Analysis_with_augmentedRCBD.Rmd' failed with diagnostics:
    LaTeX failed to compile /tmp/workdir/augmentedRCBD/new/augmentedRCBD.Rcheck/vign_test/augmentedRCBD/vignettes/Data_Analysis_with_augmentedRCBD.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See Data_Analysis_with_augmentedRCBD.log for more info.
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

# autohrf

<details>

* Version: 1.1.2
* GitHub: https://github.com/demsarjure/autohrf
* Source code: https://github.com/cran/autohrf
* Date/Publication: 2023-02-15 11:50:05 UTC
* Number of recursive dependencies: 62

Run `revdepcheck::cloud_details(, "autohrf")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # SPDX-FileCopyrightText: 2022 Jure Demšar, Nina Purg, Grega Repovš
      > #
      > # SPDX-License-Identifier: GPL-3.0-or-later
      > 
      > library(testthat)
      > library(autohrf)
      > 
    ...
      1/1 mismatches
      [1] 11 - 9 == 2
      ── Failure ('test_modelling.R:111:3'): plot_best_models ────────────────────────
      length(plot) not equal to 9.
      1/1 mismatches
      [1] 11 - 9 == 2
      
      [ FAIL 4 | WARN 2 | SKIP 0 | PASS 16 ]
      Error: Test failures
      Execution halted
    ```

# autoplotly

<details>

* Version: 0.1.4
* GitHub: https://github.com/terrytangyuan/autoplotly
* Source code: https://github.com/cran/autoplotly
* Date/Publication: 2021-04-18 06:50:11 UTC
* Number of recursive dependencies: 89

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
    Error in train(..., self = self) : 
      unused argument (list("PC2 (5.31%)", "PC1 (92.46%)", "Species", "rownames", "Species"))
    Calls: autoplotly ... ggplotly.ggplot -> gg2list -> get_gdefs_ggproto -> <Anonymous>
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
       2. └─autoplotly:::autoplotly.default(...)
       3.   ├─plotly::ggplotly(...)
       4.   └─plotly:::ggplotly.ggplot(...)
       5.     └─plotly::gg2list(...)
       6.       └─plotly:::get_gdefs_ggproto(npscales$scales, theme, plot, layers)
       7.         └─guides$train(scales, theme$legend.direction, plot$labels)
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 1 ]
      Error: Test failures
      Execution halted
    ```

# autoReg

<details>

* Version: 0.3.3
* GitHub: https://github.com/cardiomoon/autoReg
* Source code: https://github.com/cran/autoReg
* Date/Publication: 2023-11-14 05:53:27 UTC
* Number of recursive dependencies: 233

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
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Automatic_Regression_Modeling.Rmd’ using rmarkdown
    
    Quitting from lines 142-143 [unnamed-chunk-15] (Automatic_Regression_Modeling.Rmd)
    Error: processing vignette 'Automatic_Regression_Modeling.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘Automatic_Regression_Modeling.Rmd’
    
    --- re-building ‘Bootstrap_Prediction.Rmd’ using rmarkdown
    
    ...
    Error: processing vignette 'Survival.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘Survival.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘Automatic_Regression_Modeling.Rmd’ ‘Bootstrap_Prediction.Rmd’
      ‘Getting_started.Rmd’ ‘Survival.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# BayesianReasoning

<details>

* Version: 0.4.2
* GitHub: https://github.com/gorkang/BayesianReasoning
* Source code: https://github.com/cran/BayesianReasoning
* Date/Publication: 2023-11-14 11:33:20 UTC
* Number of recursive dependencies: 108

Run `revdepcheck::cloud_details(, "BayesianReasoning")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘PPV_NPV.Rmd’ using rmarkdown
    
    Quitting from lines 111-114 [unnamed-chunk-4] (PPV_NPV.Rmd)
    Error: processing vignette 'PPV_NPV.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘PPV_NPV.Rmd’
    
    --- re-building ‘introduction.Rmd’ using rmarkdown
    --- finished re-building ‘introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘PPV_NPV.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# bayesplot

<details>

* Version: 1.10.0
* GitHub: https://github.com/stan-dev/bayesplot
* Source code: https://github.com/cran/bayesplot
* Date/Publication: 2022-11-16 22:00:08 UTC
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
      This is bayesplot version 1.10.0
      - Online documentation and vignettes at mc-stan.org/bayesplot
      - bayesplot theme set to bayesplot::theme_default()
         * Does _not_ affect other ggplot2 plots
         * See ?bayesplot_theme_set for details on theme setting
    ...
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-convenience-functions.R:113:3'): legend_move returns correct theme object ──
      `pos` not equivalent to list(legend.position = c(0.25, 0.5)).
      Length mismatch: comparison on first 1 components
      Component "legend.position": target is character, current is numeric
      
      [ FAIL 1 | WARN 3 | SKIP 104 | PASS 901 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘rstanarm’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.6Mb
      sub-directories of 1Mb or more:
        R     3.9Mb
        doc   3.9Mb
    ```

# bayestestR

<details>

* Version: 0.13.1
* GitHub: https://github.com/easystats/bayestestR
* Source code: https://github.com/cran/bayestestR
* Date/Publication: 2023-04-07 15:20:02 UTC
* Number of recursive dependencies: 187

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
    A > B & B > C     0.17         0.25  1.52
    A > B & A > C     0.33         0.62  1.91
    C > A             0.52         0.31 0.600
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
      [ FAIL 1 | WARN 0 | SKIP 72 | PASS 94 ]
      
    ...
       24.                       └─methods::new(...)
       25.                         ├─methods::initialize(value, ...)
       26.                         └─Matrix (local) initialize(value, ...)
       27.                           ├─methods::callNextMethod()
       28.                           └─methods (local) .nextMethod(.Object = .Object, ... = ...)
       29.                             └─methods::validObject(.Object)
      
      [ FAIL 1 | WARN 0 | SKIP 72 | PASS 94 ]
      Error: Test failures
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘rstanarm’
    ```

# BCClong

<details>

* Version: 1.0.1
* GitHub: NA
* Source code: https://github.com/cran/BCClong
* Date/Publication: 2023-08-08 10:10:08 UTC
* Number of recursive dependencies: 142

Run `revdepcheck::cloud_details(, "BCClong")` for more info

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
      `expected`: 10
      ── Failure ('test-Trajplot.R:15:3'): trajplot works ────────────────────────────
      length(p2) (`actual`) not equal to 10 (`expected`).
      
        `actual`: 11
      `expected`: 10
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 29 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 18.0Mb
      sub-directories of 1Mb or more:
        doc       1.2Mb
        extdata   3.3Mb
        libs     13.4Mb
    ```

# BeeBDC

<details>

* Version: 1.0.3
* GitHub: https://github.com/jbdorey/BeeBDC
* Source code: https://github.com/cran/BeeBDC
* Date/Publication: 2023-12-20 15:50:33 UTC
* Number of recursive dependencies: 220

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
      `expected`:  9
      ── Failure ('test-summaryMaps.R:30:3'): summaryMaps list size ──────────────────
      length(testMap) (`actual`) not equal to 9 (`expected`).
      
        `actual`: 11
      `expected`:  9
      
      [ FAIL 3 | WARN 1 | SKIP 0 | PASS 231 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 107 marked UTF-8 strings
    ```

# benchr

<details>

* Version: 0.2.5
* GitHub: NA
* Source code: https://github.com/cran/benchr
* Date/Publication: 2020-03-07 06:30:03 UTC
* Number of recursive dependencies: 31

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
       diff| Modes: character, NULL
       diff| Lengths: 1, 0
       diff| target is character, current is NULL
      ----- FAILED[data]: test-plot.R<69--69>
       call| expect_equal(pp2p$plot$scales$scales[[2]]$trans$name, "identity")
       diff| Modes: character, NULL
       diff| Lengths: 1, 0
       diff| target is character, current is NULL
      Error: 7 out of 176 tests failed
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

# besthr

<details>

* Version: 0.3.2
* GitHub: NA
* Source code: https://github.com/cran/besthr
* Date/Publication: 2023-04-14 08:50:08 UTC
* Number of recursive dependencies: 68

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
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘basic-use.Rmd’ using rmarkdown
    
    Quitting from lines 34-44 [unnamed-chunk-2] (basic-use.Rmd)
    Error: processing vignette 'basic-use.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘basic-use.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘basic-use.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# bioassays

<details>

* Version: 1.0.1
* GitHub: NA
* Source code: https://github.com/cran/bioassays
* Date/Publication: 2020-10-09 20:10:02 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::cloud_details(, "bioassays")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(bioassays)
      > 
      > test_check("bioassays")
      F1
      F2
      F3
    ...
       1. ├─testthat::expect_identical(eg4$guides[[1]], NULL) at test_heatmap.R:43:3
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. ├─eg4$guides[[1]]
       5. └─ggplot2:::`[[.ggproto`(eg4$guides, 1)
       6.   └─ggplot2:::fetch_ggproto(x, name)
      
      [ FAIL 4 | WARN 5 | SKIP 0 | PASS 48 ]
      Error: Test failures
      Execution halted
    ```

# BOSO

<details>

* Version: 1.0.3
* GitHub: NA
* Source code: https://github.com/cran/BOSO
* Date/Publication: 2021-07-01 07:40:11 UTC
* Number of recursive dependencies: 156

Run `revdepcheck::cloud_details(, "BOSO")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘BOSO.Rmd’ using rmarkdown
    
    Quitting from lines 475-492 [plot_comparison_BOSOvsREST] (BOSO.Rmd)
    Error: processing vignette 'BOSO.Rmd' failed with diagnostics:
    The `legend.pos` theme element is not defined in the element hierarchy.
    --- failed re-building ‘BOSO.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘BOSO.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: 'cplexAPI', 'bestsubset'
    ```

# boxly

<details>

* Version: 0.1.1
* GitHub: https://github.com/Merck/boxly
* Source code: https://github.com/cran/boxly
* Date/Publication: 2023-10-24 02:40:02 UTC
* Number of recursive dependencies: 92

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
       18.   │   └─plotly:::is.plotly(p)
       19.   ├─plotly::ggplotly(p, tooltip = "text", dynamicTicks = TRUE)
       20.   └─plotly:::ggplotly.ggplot(p, tooltip = "text", dynamicTicks = TRUE)
       21.     └─plotly::gg2list(...)
       22.       └─plotly:::get_gdefs_ggproto(npscales$scales, theme, plot, layers)
       23.         └─guides$train(scales, theme$legend.direction, plot$labels)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 25 ]
      Error: Test failures
      Execution halted
    ```

# breakDown

<details>

* Version: 0.2.1
* GitHub: https://github.com/pbiecek/breakDown
* Source code: https://github.com/cran/breakDown
* Date/Publication: 2021-01-20 12:30:06 UTC
* Number of recursive dependencies: 119

Run `revdepcheck::cloud_details(, "breakDown")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(breakDown)
      > 
      > test_check("breakDown")
                                   contribution
      (Intercept)                         0.240
      - number_project = 2               -0.051
    ...
      ── Failure ('test_plot.R:38:3'): Output format ─────────────────────────────────
      plot(broken_rf_classif) has length 11, not length 9.
      ── Failure ('test_plot.R:39:3'): Output format ─────────────────────────────────
      plot(broken_lm_regr) has length 11, not length 9.
      ── Failure ('test_plot.R:40:3'): Output format ─────────────────────────────────
      plot(broken_glm_classif) has length 11, not length 9.
      
      [ FAIL 3 | WARN 12 | SKIP 0 | PASS 30 ]
      Error: Test failures
      Execution halted
    ```

# breathtestcore

<details>

* Version: 0.8.6
* GitHub: https://github.com/dmenne/breathtestcore
* Source code: https://github.com/cran/breathtestcore
* Date/Publication: 2023-02-13 14:00:07 UTC
* Number of recursive dependencies: 126

Run `revdepcheck::cloud_details(, "breathtestcore")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Complete output:
      > library(testthat)
      > 
      > options(Ncpus = parallelly::availableCores(omit = 1))
      > test_check("breathtestcore")
      Loading required package: breathtestcore
      Starting 1 test process
      [ FAIL 3 | WARN 11 | SKIP 4 | PASS 356 ]
    ...
      `expected`:  9
      ── Failure ('test_plot_breathtestfit.R:81:3'): Plot multiple groups data only (no fit) ──
      length(p) (`actual`) not equal to 9 (`expected`).
      
        `actual`: 11
      `expected`:  9
      
      [ FAIL 3 | WARN 11 | SKIP 4 | PASS 356 ]
      Error: Test failures
      Execution halted
    ```

# camcorder

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/camcorder
* Date/Publication: 2022-10-03 07:40:05 UTC
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "camcorder")` for more info

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
       16.                 ├─base::unlist(guide_loc == panel_loc)
       17.                 └─base::Ops.data.frame(guide_loc, panel_loc)
      
      [ FAIL 1 | WARN 0 | SKIP 3 | PASS 8 ]
      Deleting unused snapshots:
      • recording/camcorder_preview_ggplot2.png
      • recording/camcorder_preview_patchwork.png
      • recording/camcorder_preview_polaroid.png
      Error: Test failures
      Execution halted
    ```

# canadianmaps

<details>

* Version: 1.3.0
* GitHub: https://github.com/joellecayen/canadianmaps
* Source code: https://github.com/cran/canadianmaps
* Date/Publication: 2023-07-10 22:30:20 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "canadianmaps")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(canadianmaps)
      > 
      > test_check("canadianmaps")
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 4 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      `expected` is a character vector ('manual')
      ── Failure ('test-functions.R:23:3'): check-functions: scale_color_map() returns a ggplot manual fill object ──
      output$scale_name (`actual`) not equal to "manual" (`expected`).
      
      `actual` is NULL
      `expected` is a character vector ('manual')
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 4 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘sf’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2992 marked UTF-8 strings
    ```

# cats

<details>

* Version: 1.0.2
* GitHub: NA
* Source code: https://github.com/cran/cats
* Date/Publication: 2022-03-11 10:20:07 UTC
* Number of recursive dependencies: 84

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
    Error in train(..., self = self) : 
      unused argument (list("Simulation", "Prob", "Error_Rate"))
    Calls: trial_ocs ... ggplotly.ggplot -> gg2list -> get_gdefs_ggproto -> <Anonymous>
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘epitools’ ‘forcats’ ‘purrr’
      All declared Imports should be used.
    ```

# CEDA

<details>

* Version: 1.1.0
* GitHub: NA
* Source code: https://github.com/cran/CEDA
* Date/Publication: 2022-08-11 13:50:12 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "CEDA")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Userguide.Rmd’ using rmarkdown
    
    Quitting from lines 190-191 [fig3] (Userguide.Rmd)
    Error: processing vignette 'Userguide.Rmd' failed with diagnostics:
    The `legend.text.align` theme element is not defined in the element
    hierarchy.
    --- failed re-building ‘Userguide.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Userguide.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# cheem

<details>

* Version: 0.4.0.0
* GitHub: https://github.com/nspyrison/cheem
* Source code: https://github.com/cran/cheem
* Date/Publication: 2023-11-08 21:30:02 UTC
* Number of recursive dependencies: 153

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
       15. │   └─x %||% list()
       16. ├─plotly::ggplotly(...)
       17. └─plotly:::ggplotly.ggplot(...)
       18.   └─plotly::gg2list(...)
       19.     └─plotly:::get_gdefs_ggproto(npscales$scales, theme, plot, layers)
       20.       └─guides$train(scales, theme$legend.direction, plot$labels)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 10 ]
      Error: Test failures
      Execution halted
    ```

# chem16S

<details>

* Version: 1.0.0
* GitHub: https://github.com/jedick/chem16S
* Source code: https://github.com/cran/chem16S
* Date/Publication: 2023-07-17 17:10:02 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::cloud_details(, "chem16S")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘metrics.Rmd’ using rmarkdown
    --- finished re-building ‘metrics.Rmd’
    
    --- re-building ‘phyloseq.Rmd’ using rmarkdown
    --- finished re-building ‘phyloseq.Rmd’
    
    --- re-building ‘plotting.Rmd’ using rmarkdown
    
    ...
    Quitting from lines 169-174 [plot_metrics2.Humboldt_redox_OM] (plotting.Rmd)
    Error: processing vignette 'plotting.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘plotting.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘plotting.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ChemoSpec

<details>

* Version: 6.1.9
* GitHub: https://github.com/bryanhanson/ChemoSpec
* Source code: https://github.com/cran/ChemoSpec
* Date/Publication: 2023-06-07 11:00:02 UTC
* Number of recursive dependencies: 167

Run `revdepcheck::cloud_details(, "ChemoSpec")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ChemoSpec-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: sgfSpectra
    > ### Title: Apply Savitzky-Golay filters to a Spectra object
    > ### Aliases: sgfSpectra
    > ### Keywords: multivariate utilities
    > 
    > ### ** Examples
    > 
    ...
    +   p3
    + }
    Coordinate system already present. Adding new coordinate system, which will
    replace the existing one.
    Coordinate system already present. Adding new coordinate system, which will
    replace the existing one.
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ChemoSpec.Rmd’ using rmarkdown
    
    Quitting from lines 411-415 [classPCA] (ChemoSpec.Rmd)
    Error: processing vignette 'ChemoSpec.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘ChemoSpec.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ChemoSpec.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## Newly fixed

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ChemoSpec.Rmd’ using rmarkdown
    
    tlmgr: Remote database (rev 69178) seems to be older than local (rev 69180 of texlive-scripts); please use different mirror or  wait a day or so.
    Warning in system2("tlmgr", args, ...) :
      running command ''tlmgr' search --file --global '/grffile.sty'' had status 1
    ! LaTeX Error: File `grffile.sty' not found.
    
    ! Emergency stop.
    ...
    
    Error: processing vignette 'ChemoSpec.Rmd' failed with diagnostics:
    LaTeX failed to compile /tmp/workdir/ChemoSpec/old/ChemoSpec.Rcheck/vign_test/ChemoSpec/vignettes/ChemoSpec.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See ChemoSpec.log for more info.
    --- failed re-building ‘ChemoSpec.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ChemoSpec.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ChemoSpecUtils

<details>

* Version: 1.0.3
* GitHub: https://github.com/bryanhanson/ChemoSpecUtils
* Source code: https://github.com/cran/ChemoSpecUtils
* Date/Publication: 2023-05-30 09:40:02 UTC
* Number of recursive dependencies: 100

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
    The ChemoSpec graphics option is set to 'ggplot2'
    To change it, do
    	options(ChemoSpecGraphics = 'option'),
    	where 'option' is one of 'base' or 'ggplot2' or'plotly'.
    Coordinate system already present. Adding new coordinate system, which will
    replace the existing one.
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘mvoutlier’
    ```

# chessboard

<details>

* Version: 0.1
* GitHub: https://github.com/frbcesab/chessboard
* Source code: https://github.com/cran/chessboard
* Date/Publication: 2023-10-14 08:00:02 UTC
* Number of recursive dependencies: 85

Run `revdepcheck::cloud_details(, "chessboard")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘chess-pieces.Rmd’ using rmarkdown
    
    Quitting from lines 111-130 [cb-pawn] (chess-pieces.Rmd)
    Error: processing vignette 'chess-pieces.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘chess-pieces.Rmd’
    
    --- re-building ‘chessboard.Rmd’ using rmarkdown
    ...
    --- failed re-building ‘chessboard.Rmd’
    
    --- re-building ‘visualization-tools.Rmd’ using rmarkdown
    --- finished re-building ‘visualization-tools.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘chess-pieces.Rmd’ ‘chessboard.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# chillR

<details>

* Version: 0.75
* GitHub: NA
* Source code: https://github.com/cran/chillR
* Date/Publication: 2023-11-27 22:20:02 UTC
* Number of recursive dependencies: 140

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
    > # Plot the climate scenarios
    > 
    > plot_scenarios(climate_scenario_list, metric = 'Chill_Portions',
    +                add_historic = TRUE, size = 2, shape = 3, color = 'blue',
    +                outlier_shape = 12, historic_color = 'skyblue',
    +                group_by = c("Year", "Scenario"))
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
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
    Error in train(..., self = self) : 
      unused argument (list("Species", "Sepal.Length", "Species"))
    Calls: make_barplot ... ggplotly.ggplot -> gg2list -> get_gdefs_ggproto -> <Anonymous>
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘chronicle.Rmd’ using rmarkdown
    
    Quitting from lines 50-57 [unnamed-chunk-3] (quick_demo.Rmd)
    Error: processing vignette 'chronicle.Rmd' failed with diagnostics:
    ℹ In index: 1.
    Caused by error in `train()`:
    ! unused argument (list("Petal.Width", "Petal.Length", "Species"))
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

* Version: 1.5.0
* GitHub: https://github.com/openanalytics/clinDataReview
* Source code: https://github.com/cran/clinDataReview
* Date/Publication: 2023-12-11 08:30:02 UTC
* Number of recursive dependencies: 131

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
    Error in train(..., self = self) : 
      unused argument (list("Numeric Result/Finding in Standard Units vs Analysis Relative Day ", "Analysis Relative Day", "Numeric Result/Finding in Standard Units", "Planned Treatment", "Planned Treatment", "Unique Subject Identifier", "hover"))
    Calls: scatterplotClinData ... ggplotly.ggplot -> gg2list -> get_gdefs_ggproto -> <Anonymous>
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
        adding: report_dependenciesfa6694426b4/ (stored 0%)
        adding: report_dependenciesfa6694426b4/filefa65f9888c5.html (deflated 8%)
    ...
       4.     └─plotly::gg2list(...)
       5.       └─plotly:::get_gdefs_ggproto(npscales$scales, theme, plot, layers)
       6.         └─guides$train(scales, theme$legend.direction, plot$labels)
      ── Failure ('test_staticScatterplotClinData.R:89:3'): An axis transformation can be specified as a non character in the scatterplot ──
      ggplot2::layer_scales(gg)$x$trans not equal to `xTrans`.
      target is NULL, current is transform
      
      [ FAIL 11 | WARN 8 | SKIP 29 | PASS 474 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘clinDataReview-dataPreprocessing.Rmd’ using rmarkdown
    --- finished re-building ‘clinDataReview-dataPreprocessing.Rmd’
    
    --- re-building ‘clinDataReview-dataVisualization.Rmd’ using rmarkdown
    
    Quitting from lines 167-208 [timeProfiles] (clinDataReview-dataVisualization.Rmd)
    Error: processing vignette 'clinDataReview-dataVisualization.Rmd' failed with diagnostics:
    unused argument (list("Actual value of Alanine Aminotransferase (U/L)", "Analysis Relative Day", "Numeric Result/Finding in Standard Units", "Planned Treatment", "Planned Treatment", "Unique Subject Identifier", "hover"))
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
      installed size is  5.9Mb
      sub-directories of 1Mb or more:
        doc   4.3Mb
    ```

# clinUtils

<details>

* Version: 0.1.4
* GitHub: https://github.com/openanalytics/clinUtils
* Source code: https://github.com/cran/clinUtils
* Date/Publication: 2023-01-06 11:50:28 UTC
* Number of recursive dependencies: 121

Run `revdepcheck::cloud_details(, "clinUtils")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘clinUtils-vignette.Rmd’ using rmarkdown
    
    Quitting from lines 442-448 [figure-interactive-creation] (clinUtils-vignette.Rmd)
    Error: processing vignette 'clinUtils-vignette.Rmd' failed with diagnostics:
    unused argument (list("ACTARM", "LBNRIND", "count", "weight"))
    --- failed re-building ‘clinUtils-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘clinUtils-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.7Mb
      sub-directories of 1Mb or more:
        doc   5.4Mb
    ```

# clugenr

<details>

* Version: 1.0.2
* GitHub: https://github.com/clugen/clugenr
* Source code: https://github.com/cran/clugenr
* Date/Publication: 2023-12-15 23:40:02 UTC
* Number of recursive dependencies: 125

Run `revdepcheck::cloud_details(, "clugenr")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘dev.Rmd’ using rmarkdown
    --- finished re-building ‘dev.Rmd’
    
    --- re-building ‘examples2d.Rmd’ using rmarkdown
    
    Quitting from lines 53-56 [unnamed-chunk-4] (examples2d.Rmd)
    Error: processing vignette 'examples2d.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘examples2d.Rmd’
    ...
    --- failed re-building ‘examplesnd.Rmd’
    
    --- re-building ‘theory.Rmd’ using rmarkdown
    --- finished re-building ‘theory.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘examples2d.Rmd’ ‘examplesmrg.Rmd’ ‘examplesnd.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ClustAssess

<details>

* Version: 0.3.0
* GitHub: https://github.com/Core-Bioinformatics/ClustAssess
* Source code: https://github.com/cran/ClustAssess
* Date/Publication: 2022-01-26 16:52:46 UTC
* Number of recursive dependencies: 165

Run `revdepcheck::cloud_details(, "ClustAssess")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ClustAssess-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: marker_overlap
    > ### Title: Cell-Wise Marker Gene Overlap
    > ### Aliases: marker_overlap
    > 
    > ### ** Examples
    > 
    > suppressWarnings({
    ...
    Attaching package: ‘SeuratObject’
    
    The following object is masked from ‘package:base’:
    
        intersect
    
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ClustAssess.Rmd’ using rmarkdown
    
    Quitting from lines 22-27 [setup] (ClustAssess.Rmd)
    Error: processing vignette 'ClustAssess.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘ClustAssess.Rmd’
    
    --- re-building ‘comparing-soft-and-hierarchical.Rmd’ using rmarkdown
    --- finished re-building ‘comparing-soft-and-hierarchical.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ClustAssess.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# cobalt

<details>

* Version: 4.5.2
* GitHub: https://github.com/ngreifer/cobalt
* Source code: https://github.com/cran/cobalt
* Date/Publication: 2023-11-20 22:10:02 UTC
* Number of recursive dependencies: 149

Run `revdepcheck::cloud_details(, "cobalt")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘cobalt-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: balance-statistics
    > ### Title: Balance Statistics in 'bal.tab' and 'love.plot'
    > ### Aliases: balance-statistics
    > 
    > ### ** Examples
    > 
    > data(lalonde)
    ...
    Warning: No shared levels found between `names(values)` of the manual scale and the
    data's fill values.
    Warning: No shared levels found between `names(values)` of the manual scale and the
    data's fill values.
    Warning: No shared levels found between `names(values)` of the manual scale and the
    data's fill values.
    Error in legg$grobs[[which(legg$layout$name == "guide-box")]] : 
      attempt to select less than one element in get1index
    Calls: love.plot -> eval.parent -> eval -> eval -> love.plot
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘cobalt.Rmd’ using rmarkdown
    
    Quitting from lines 347-366 [unnamed-chunk-17] (cobalt.Rmd)
    Error: processing vignette 'cobalt.Rmd' failed with diagnostics:
    attempt to select less than one element in get1index
    --- failed re-building ‘cobalt.Rmd’
    
    --- re-building ‘longitudinal-treat.Rmd’ using rmarkdown
    --- finished re-building ‘longitudinal-treat.Rmd’
    ...
    --- finished re-building ‘other-packages.Rmd’
    
    --- re-building ‘segmented-data.Rmd’ using rmarkdown
    --- finished re-building ‘segmented-data.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘cobalt.Rmd’ ‘love.plot.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# CohortPlat

<details>

* Version: 1.0.5
* GitHub: NA
* Source code: https://github.com/cran/CohortPlat
* Date/Publication: 2022-02-14 09:30:02 UTC
* Number of recursive dependencies: 83

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
    > plot_trial(res_list, unit = "n")
    Warning: The `guide` argument in `scale_*()` cannot be `FALSE`. This was deprecated in
    ggplot2 3.3.4.
    ℹ Please use "none" instead.
    ℹ The deprecated feature was likely used in the base package.
      Please report the issue to the authors.
    Error in train(..., self = self) : 
      unused argument (list("Overview of Study", "", "N", "Decision_Int", "Decision_Fin", "Cohort", "End_N", "Final_Suc", "Int_Suc"))
    Calls: plot_trial ... ggplotly.ggplot -> gg2list -> get_gdefs_ggproto -> <Anonymous>
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘my-vignette.Rmd’ using rmarkdown
    
    Quitting from lines 1043-1073 [unnamed-chunk-20] (my-vignette.Rmd)
    Error: processing vignette 'my-vignette.Rmd' failed with diagnostics:
    unused argument (list("Simulation", "Prob", "Error_Rate"))
    --- failed re-building ‘my-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘my-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# constructive

<details>

* Version: 0.2.0
* GitHub: https://github.com/cynkra/constructive
* Source code: https://github.com/cran/constructive
* Date/Publication: 2023-11-13 17:33:24 UTC
* Number of recursive dependencies: 115

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
      [ FAIL 3 | WARN 0 | SKIP 58 | PASS 10 ]
      
      ══ Skipped tests (58) ══════════════════════════════════════════════════════════
    ...
       20. │               └─utils::getFromNamespace(fun, pkg)
       21. │                 └─base::get(x, envir = ns, inherits = FALSE)
       22. └─base::.handleSimpleError(...)
       23.   └─rlang (local) h(simpleError(msg, call))
       24.     └─handlers[[1L]](cnd)
       25.       └─rlang::abort(...)
      
      [ FAIL 3 | WARN 0 | SKIP 58 | PASS 10 ]
      Error: Test failures
      Execution halted
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
    
    > ### Name: corrBarplot
    > ### Title: corrBarplot
    > ### Aliases: corrBarplot
    > 
    > ### ** Examples
    > 
    > cm <- cor(mtcars)
    > 
    > corrBarplot(mat = cm,
    +            interactive = TRUE)
    Error in train(..., self = self) : 
      unused argument (list("Correlation", "", "correlation", "pair"))
    Calls: corrBarplot ... ggplotly.ggplot -> gg2list -> get_gdefs_ggproto -> <Anonymous>
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘corrViz.Rmd’ using rmarkdown
    
    Quitting from lines 76-81 [heatmap] (corrViz.Rmd)
    Error: processing vignette 'corrViz.Rmd' failed with diagnostics:
    unused argument (list("", "", "correlation"))
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

# coveffectsplot

<details>

* Version: 1.0.4
* GitHub: https://github.com/smouksassi/coveffectsplot
* Source code: https://github.com/cran/coveffectsplot
* Date/Publication: 2023-09-18 12:40:16 UTC
* Number of recursive dependencies: 149

Run `revdepcheck::cloud_details(, "coveffectsplot")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Exposure_Response_Example.Rmd’ using rmarkdown
    --- finished re-building ‘Exposure_Response_Example.Rmd’
    
    --- re-building ‘PKPD_Example.Rmd’ using rmarkdown
    --- finished re-building ‘PKPD_Example.Rmd’
    
    --- re-building ‘PK_Example.Rmd’ using rmarkdown
    
    Quitting from lines 206-273 [computenca] (PK_Example.Rmd)
    ...
    --- finished re-building ‘app.Rmd’
    
    --- re-building ‘introduction_to_coveffectsplot.Rmd’ using rmarkdown
    --- finished re-building ‘introduction_to_coveffectsplot.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘PK_Example.Rmd’ ‘PK_Example_full.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# Covid19Wastewater

<details>

* Version: 1.0.1
* GitHub: https://github.com/UW-Madison-DSI/Covid19Wastewater
* Source code: https://github.com/cran/Covid19Wastewater
* Date/Publication: 2023-08-24 20:10:02 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::cloud_details(, "Covid19Wastewater")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Covid19Wastewater-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: VariantPlot
    > ### Title: Shows each variant in proportion to the others in 2 week time
    > ###   periods
    > ### Aliases: VariantPlot
    > 
    > ### ** Examples
    > 
    > data(Covariants_data, package = "Covid19Wastewater")
    > VariantPlot(Covariants_data)
    Error in train(..., self = self) : 
      unused argument (list("Covariant Percent", "Week", "variable"))
    Calls: VariantPlot ... ggplotly.ggplot -> gg2list -> get_gdefs_ggproto -> <Anonymous>
    Execution halted
    ```

## Newly fixed

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Aux_info_data.Rmd’ using rmarkdown
    
    tlmgr: Remote database (rev 69178) seems to be older than local (rev 69180 of texlive-scripts); please use different mirror or  wait a day or so.
    Warning in system2("tlmgr", args, ...) :
      running command ''tlmgr' search --file --global '/grffile.sty'' had status 1
    ! LaTeX Error: File `grffile.sty' not found.
    
    ! Emergency stop.
    <read *> 
    ...
    --- finished re-building ‘time_series_offset.Rmd’
    
    --- re-building ‘variant_data.Rmd’ using rmarkdown
    --- finished re-building ‘variant_data.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Aux_info_data.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# cowplot

<details>

* Version: 1.1.2
* GitHub: https://github.com/wilkelab/cowplot
* Source code: https://github.com/cran/cowplot
* Date/Publication: 2023-12-15 07:40:02 UTC
* Number of recursive dependencies: 101

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
      [ FAIL 10 | WARN 4 | SKIP 7 | PASS 45 ]
      
      ══ Skipped tests (7) ═══════════════════════════════════════════════════════════
    ...
      `expected` is NULL
      ── Failure ('test_plot_components.R:14:3'): plot components ────────────────────
      all(component_names %in% plot_component_names(p)) is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      
      [ FAIL 10 | WARN 4 | SKIP 7 | PASS 45 ]
      Error: Test failures
      Execution halted
    ```

# CRABS

<details>

* Version: 1.2.0
* GitHub: https://github.com/afmagee/CRABS
* Source code: https://github.com/cran/CRABS
* Date/Publication: 2023-10-24 10:20:07 UTC
* Number of recursive dependencies: 64

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
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

# cricketdata

<details>

* Version: 0.2.3
* GitHub: https://github.com/robjhyndman/cricketdata
* Source code: https://github.com/cran/cricketdata
* Date/Publication: 2023-08-29 10:30:09 UTC
* Number of recursive dependencies: 105

Run `revdepcheck::cloud_details(, "cricketdata")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘cricinfo.Rmd’ using rmarkdown
    --- finished re-building ‘cricinfo.Rmd’
    
    --- re-building ‘cricketdata_R_pkg.Rmd’ using rmarkdown
    --- finished re-building ‘cricketdata_R_pkg.Rmd’
    
    --- re-building ‘cricsheet.Rmd’ using rmarkdown
    
    ...
    Quitting from lines 244-368 [unnamed-chunk-7] (cricsheet.Rmd)
    Error: processing vignette 'cricsheet.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘cricsheet.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘cricsheet.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 37 marked UTF-8 strings
    ```

# crosshap

<details>

* Version: 1.2.2
* GitHub: NA
* Source code: https://github.com/cran/crosshap
* Date/Publication: 2023-05-02 07:50:08 UTC
* Number of recursive dependencies: 118

Run `revdepcheck::cloud_details(, "crosshap")` for more info

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
        8.           └─base::lapply(x$plots, plot_table, guides = guides)
        9.             ├─patchwork (local) FUN(X[[i]], ...)
       10.             └─patchwork:::plot_table.ggplot(X[[i]], ...)
       11.               └─patchwork:::add_guides(gt, guides == "collect")
       12.                 ├─base::unlist(guide_loc == panel_loc)
       13.                 └─base::Ops.data.frame(guide_loc, panel_loc)
      
      [ FAIL 4 | WARN 0 | SKIP 2 | PASS 5 ]
      Error: Test failures
      Execution halted
    ```

# ctrialsgov

<details>

* Version: 0.2.5
* GitHub: NA
* Source code: https://github.com/cran/ctrialsgov
* Date/Publication: 2021-10-18 16:00:02 UTC
* Number of recursive dependencies: 101

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
       2. └─ctrialsgov:::ctgov_to_plotly.ctgov_bar_plot(p)
       3.   ├─plotly::ggplotly(p, tooltip = "text")
       4.   └─plotly:::ggplotly.ggplot(p, tooltip = "text")
       5.     └─plotly::gg2list(...)
       6.       └─plotly:::get_gdefs_ggproto(npscales$scales, theme, plot, layers)
       7.         └─guides$train(scales, theme$legend.direction, plot$labels)
      
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
* Number of recursive dependencies: 146

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
    
    ...
    Quitting from lines 123-125 [unnamed-chunk-7] (cb6interactive.Rmd)
    Error: processing vignette 'cb6interactive.Rmd' failed with diagnostics:
    unused argument (list("Month", "Temperature", "id", "temp_diff_var", "temp_diff_var", "tmin", "tmax"))
    --- failed re-building ‘cb6interactive.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘cb5match.Rmd’ ‘cb6interactive.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        data   3.0Mb
        doc    1.3Mb
    ```

# CytoSimplex

<details>

* Version: 0.1.1
* GitHub: https://github.com/welch-lab/CytoSimplex
* Source code: https://github.com/cran/CytoSimplex
* Date/Publication: 2023-12-15 09:30:06 UTC
* Number of recursive dependencies: 178

Run `revdepcheck::cloud_details(, "CytoSimplex")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘CytoSimplex.Rmd’ using rmarkdown
    
    Quitting from lines 86-92 [splitCluster] (CytoSimplex.Rmd)
    Error: processing vignette 'CytoSimplex.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘CytoSimplex.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘CytoSimplex.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.6Mb
      sub-directories of 1Mb or more:
        data   3.4Mb
        doc    1.1Mb
        libs   6.2Mb
    ```

# daiquiri

<details>

* Version: 1.1.1
* GitHub: https://github.com/ropensci/daiquiri
* Source code: https://github.com/cran/daiquiri
* Date/Publication: 2023-07-18 16:50:09 UTC
* Number of recursive dependencies: 111

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
      
      Quitting from lines 352-384 [daiquiri-overview-nonconformant] (report_htmldoc.Rmd)
      
    ...
       56.                                             └─base::seq.default(limits[1], limits[2], length.out = nbin)
      
      [ FAIL 4 | WARN 0 | SKIP 7 | PASS 479 ]
      Deleting unused snapshots:
      • aggregate_data/test_[ALL_FIELDS_COMBINED].csv
      • aggregate_data/test_[DUPLICATES].csv
      • aggregate_data/test_col1.csv
      • aggregate_data/test_col2.csv
      Error: Test failures
      Execution halted
    ```

# dartR

<details>

* Version: 2.9.7
* GitHub: NA
* Source code: https://github.com/cran/dartR
* Date/Publication: 2023-06-07 10:40:02 UTC
* Number of recursive dependencies: 279

Run `revdepcheck::cloud_details(, "dartR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘dartR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gl.dist.ind
    > ### Title: Calculates a distance matrix for individuals defined in a
    > ###   genlight object
    > ### Aliases: gl.dist.ind
    > 
    > ### ** Examples
    > 
    ...
    > D <- gl.dist.ind(testset.gl[1:20,], method='euclidean',scale=TRUE)
    Starting gl.dist.ind 
      Processing genlight object with SNP data
      Warning: data include loci that are scored NA across all individuals.
      Consider filtering using gl <- gl.filter.allna(gl)
      Calculating scaled Euclidean Distances between individuals
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: gl.dist.ind
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.6Mb
      sub-directories of 1Mb or more:
        R      3.5Mb
        data   1.7Mb
        help   1.6Mb
    ```

# dartR.base

<details>

* Version: 0.65
* GitHub: NA
* Source code: https://github.com/cran/dartR.base
* Date/Publication: 2023-11-17 20:00:08 UTC
* Number of recursive dependencies: 271

Run `revdepcheck::cloud_details(, "dartR.base")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘dartR.base-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gl.dist.ind
    > ### Title: Calculates a distance matrix for individuals defined in a
    > ###   genlight object
    > ### Aliases: gl.dist.ind
    > 
    > ### ** Examples
    > 
    ...
    > D <- gl.dist.ind(testset.gl[1:20,], method='euclidean',scale=TRUE)
    Starting gl.dist.ind 
      Processing genlight object with SNP data
      Warning: data include loci that are scored NA across all individuals.
      Consider filtering using gl <- gl.filter.allna(gl)
      Calculating scaled Euclidean Distances between individuals
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: gl.dist.ind
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        R      1.9Mb
        data   1.7Mb
        help   2.6Mb
    ```

# dartR.captive

<details>

* Version: 0.75
* GitHub: NA
* Source code: https://github.com/cran/dartR.captive
* Date/Publication: 2023-11-27 17:10:09 UTC
* Number of recursive dependencies: 127

Run `revdepcheck::cloud_details(, "dartR.captive")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘dartR.captive-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gl.filter.parent.offspring
    > ### Title: Filters putative parent offspring within a population
    > ### Aliases: gl.filter.parent.offspring
    > 
    > ### ** Examples
    > 
    > out <- gl.filter.parent.offspring(testset.gl[1:10, 1:50])
    ...
    Starting gl.select.colors 
      Warning: Number of required colors not specified, set to 9
      Library: RColorBrewer
      Palette: brewer.pal
      Showing and returning 2 of 9 colors for library RColorBrewer : palette Blues 
    Completed: gl.select.colors 
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: gl.filter.parent.offspring ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

# dartR.popgen

<details>

* Version: 0.32
* GitHub: NA
* Source code: https://github.com/cran/dartR.popgen
* Date/Publication: 2023-11-21 18:10:02 UTC
* Number of recursive dependencies: 150

Run `revdepcheck::cloud_details(, "dartR.popgen")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘dartR.popgen-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gl.ld.distance
    > ### Title: Plots linkage disequilibrium against distance by population
    > ###   disequilibrium patterns
    > ### Aliases: gl.ld.distance
    > 
    > ### ** Examples
    > 
    ...
      Warning: data include loci that are scored NA across all individuals.
      Consider filtering using gl <- gl.filter.allna(gl)
      Warning: Data may include monomorphic loci in call rate 
                        calculations for filtering
      Recalculating Call Rate
      Removing loci based on Call Rate, threshold = 1 
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: gl.filter.callrate ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

# dataquieR

<details>

* Version: 2.0.1
* GitHub: NA
* Source code: https://github.com/cran/dataquieR
* Date/Publication: 2023-07-19 16:20:10 UTC
* Number of recursive dependencies: 161

Run `revdepcheck::cloud_details(, "dataquieR")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘DQ-report-example.Rmd’ using rmarkdown
    
    Quitting from lines 226-227 [unnamed-chunk-21] (DQ-report-example.Rmd)
    Error: processing vignette 'DQ-report-example.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘DQ-report-example.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘DQ-report-example.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # options("testthat.default_reporter" = testthat::RStudioReporter)
      > library(dataquieR)
      > testthat::test_check("dataquieR")
      All cached dataframes have been purged.
      All cached dataframes have been purged.
      All cached dataframes have been purged.
      All cached dataframes have been purged.
    ...
      • print/im-empty-repsumtab.svg
      • print/im-ex1-repsumtab.svg
      • print/im-ex2-repsumtab.svg
      • pro_applicability_matrix/appmatrix-plot-for-segment-v10000-ok.svg
      • pro_applicability_matrix/appmatrix-plot-ok.svg
      • util_heatmap_1th/util-heatmap-1th-1.svg
      • util_heatmap_1th/util-heatmap-1th-2.svg
      • util_heatmap_1th/util-heatmap-1th-3.svg
      Error: Test failures
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        R   3.8Mb
    ```

# decisionSupport

<details>

* Version: 1.113
* GitHub: NA
* Source code: https://github.com/cran/decisionSupport
* Date/Publication: 2023-10-05 23:20:02 UTC
* Number of recursive dependencies: 175

Run `revdepcheck::cloud_details(, "decisionSupport")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘decisionSupport-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: compound_figure
    > ### Title: Compound figure for decision support
    > ### Aliases: compound_figure
    > ### Keywords: Monte-Carlo NPV decision-analysis decisionSupport
    > ###   net-present-value risk uncertainty
    > 
    > ### ** Examples
    ...
    + decision_var_name = "Decision",
    + cashflow_var_name = "cashflow",
    + model_runs = 1e2, 
    + distribution_method = 'smooth_simple_overlay')
    [1] "Processing 1 output variables. This can take some time."
    [1] "Output variable 1 (Decision) completed."
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘example_decision_function.Rmd’ using rmarkdown
    
    Quitting from lines 316-318 [compound_figure] (example_decision_function.Rmd)
    Error: processing vignette 'example_decision_function.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘example_decision_function.Rmd’
    
    --- re-building ‘wildfire_example.Rmd’ using rmarkdown
    --- finished re-building ‘wildfire_example.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘example_decision_function.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# deeptime

<details>

* Version: 1.0.1
* GitHub: https://github.com/willgearty/deeptime
* Source code: https://github.com/cran/deeptime
* Date/Publication: 2023-02-16 16:40:02 UTC
* Number of recursive dependencies: 156

Run `revdepcheck::cloud_details(, "deeptime")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘deeptime-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: coord_geo
    > ### Title: Transformed coordinate system with geological timescale
    > ### Aliases: coord_geo CoordGeo
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    ...
      9.             └─coord$render_axis_h(range, theme)
     10.               └─deeptime (local) render_axis_h(..., self = self)
     11.                 └─deeptime:::render_geo_scale(self, panel_params, theme, "bottom")
     12.                   └─base::mapply(...)
     13.                     └─deeptime (local) `<fn>`(...)
     14.                       └─ggplot2::coord_trans(x = self$trans$x, xlim = lims, ylim = c(0, 1), expand = FALSE)
     15.                         └─ggplot2:::check_coord_limits(xlim)
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
      > library(deeptime)
      > 
      > test_check("deeptime")
      Scale for y is already present.
      Adding another scale for y, which will replace the existing scale.
      [ FAIL 10 | WARN 3 | SKIP 12 | PASS 32 ]
    ...
      • ggarrange2/ggarrange2-with-layout-new.svg
      • ggarrange2/ggarrange2-with-layout-old.svg
      • gggeo_scale/gggeo-scale-left-and-right-new.svg
      • gggeo_scale/gggeo-scale-left-and-right-old.svg
      • gggeo_scale/gggeo-scale-new.svg
      • gggeo_scale/gggeo-scale-old.svg
      • gggeo_scale/gggeo-scale-top-new.svg
      • gggeo_scale/gggeo-scale-top-old.svg
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘coord.Rmd’ using rmarkdown
    
    Quitting from lines 28-32 [unnamed-chunk-3] (coord.Rmd)
    Error: processing vignette 'coord.Rmd' failed with diagnostics:
    cannot allocate vector of size 7978.1 Gb
    --- failed re-building ‘coord.Rmd’
    
    --- re-building ‘coord_geo.Rmd’ using rmarkdown
    
    ...
    Error: processing vignette 'traits.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘traits.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘coord.Rmd’ ‘coord_geo.Rmd’ ‘ggarrange2.Rmd’ ‘phylogenies.Rmd’
      ‘traits.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# directlabels

<details>

* Version: 2023.8.25
* GitHub: https://github.com/tdhock/directlabels
* Source code: https://github.com/cran/directlabels
* Date/Publication: 2023-09-01 08:30:06 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::cloud_details(, "directlabels")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘ggplot.R’
    Running the tests in ‘tests/ggplot.R’ failed.
    Complete output:
      > if(require(ggplot2)){
      +   library(directlabels)
      +   data(mpg,package="ggplot2")
      +   plots <-
      +     list(qplot=qplot(hwy,cty,data=mpg,colour=class),
      +          ggplot=ggplot(mpg,aes(hwy,cty,colour=class))+geom_point(),
      +          aes2=ggplot(,aes(hwy,cty))+geom_point(aes(colour=class),data=mpg),
    ...
      The following object is masked from 'package:directlabels':
      
          gapply
      
      Error in y[setdiff(names(y), names(x))] : 
        object of type 'environment' is not subsettable
      Calls: legends2hide -> defaults
      In addition: Warning message:
      `qplot()` was deprecated in ggplot2 3.4.0. 
      Execution halted
    ```

# distributional

<details>

* Version: 0.3.2
* GitHub: https://github.com/mitchelloharawild/distributional
* Source code: https://github.com/cran/distributional
* Date/Publication: 2023-03-22 14:40:02 UTC
* Number of recursive dependencies: 66

Run `revdepcheck::cloud_details(, "distributional")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘distributional-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_hilo_linerange
    > ### Title: Line ranges for hilo intervals
    > ### Aliases: geom_hilo_linerange
    > 
    > ### ** Examples
    > 
    > dist <- dist_normal(1:3, 1:3)
    ...
      7.         └─ggplot2 (local) train_df(..., self = self)
      8.           └─base::lapply(self$scales, function(scale) scale$train_df(df = df))
      9.             └─ggplot2 (local) FUN(X[[i]], ...)
     10.               └─scale$train_df(df = df)
     11.                 └─ggplot2 (local) train_df(..., self = self)
     12.                   └─self$train(df[[aesthetic]])
     13.                     └─ggplot2 (local) train(..., self = self)
     14.                       └─cli::cli_abort(...)
     15.                         └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(distributional)
      > 
      > test_check("distributional")
      [ FAIL 2 | WARN 2 | SKIP 0 | PASS 498 ]
      
      == Failed tests ================================================================
    ...
       18.               \-scale$train_df(df = df)
       19.                 \-ggplot2 (local) train_df(..., self = self)
       20.                   \-self$train(df[[aesthetic]])
       21.                     \-ggplot2 (local) train(..., self = self)
       22.                       \-cli::cli_abort(...)
       23.                         \-rlang::abort(...)
      
      [ FAIL 2 | WARN 2 | SKIP 0 | PASS 498 ]
      Error: Test failures
      Execution halted
    ```

# docxtools

<details>

* Version: 0.3.0
* GitHub: https://github.com/graphdr/docxtools
* Source code: https://github.com/cran/docxtools
* Date/Publication: 2022-11-12 00:40:02 UTC
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "docxtools")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(docxtools)
      > 
      > test_check("docxtools")
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 22 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test_put.R:21:3'): put_axes() attributes match expectations ───────
      p$layers[[1]]$geom$non_missing_aes not identical to c("linetype", "linewidth", "shape").
      Lengths differ: 2 is not 3
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 22 ]
      Error: Test failures
      Execution halted
    ```

# dowser

<details>

* Version: 2.1.0
* GitHub: NA
* Source code: https://github.com/cran/dowser
* Date/Publication: 2023-12-22 05:40:02 UTC
* Number of recursive dependencies: 145

Run `revdepcheck::cloud_details(, "dowser")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Building-Trees-Vignette.Rmd’ using rmarkdown
    ! kpathsea: Running mktexfmt pdflatex.fmt
    
    ! /usr/bin/mktexfmt: kpsewhich -var-value=TEXMFROOT failed, aborting early.
    
    ! BEGIN failed--compilation aborted at /usr/bin/mktexfmt line 28.
    
    Error: processing vignette 'Building-Trees-Vignette.Rmd' failed with diagnostics:
    LaTeX failed to compile /tmp/workdir/dowser/new/dowser.Rcheck/vign_test/dowser/vignettes/Building-Trees-Vignette.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See Building-Trees-Vignette.log for more info.
    ...
    --- failed re-building ‘Sequences-Vignette.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘Building-Trees-Vignette.Rmd’ ‘Discrete-Trait-Vignette.Rmd’
      ‘Germlines-Vignette.Rmd’ ‘Measurable-Evolution.Rmd’
      ‘Plotting-Trees-Vignette.Rmd’ ‘Quickstart-Vignette.Rmd’
      ‘Resolve-Light-Chains-Vignette.Rmd’ ‘Sequences-Vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## Newly fixed

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Building-Trees-Vignette.Rmd’ using rmarkdown
    Could not determine directory of tlmgr executable, maybe shared library woes?
    Check for error messages above at /usr/local/bin/tlmgr line 75.
    BEGIN failed--compilation aborted at /usr/local/bin/tlmgr line 84.
    Warning in system2("tlmgr", args, ...) :
      running command ''tlmgr' search --file --global '/grffile.sty'' had status 1
    ! LaTeX Error: File `grffile.sty' not found.
    
    ! Emergency stop.
    ...
    --- failed re-building ‘Sequences-Vignette.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘Building-Trees-Vignette.Rmd’ ‘Discrete-Trait-Vignette.Rmd’
      ‘Germlines-Vignette.Rmd’ ‘Measurable-Evolution.Rmd’
      ‘Plotting-Trees-Vignette.Rmd’ ‘Quickstart-Vignette.Rmd’
      ‘Resolve-Light-Chains-Vignette.Rmd’ ‘Sequences-Vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# DR.SC

<details>

* Version: 3.3
* GitHub: https://github.com/feiyoung/DR.SC
* Source code: https://github.com/cran/DR.SC
* Date/Publication: 2023-08-09 19:00:05 UTC
* Number of recursive dependencies: 151

Run `revdepcheck::cloud_details(, "DR.SC")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘DR.SC-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: drscPlot
    > ### Title: tNSE or UMAP plot visualization
    > ### Aliases: drscPlot
    > ### Keywords: tSNE UMAP
    > 
    > ### ** Examples
    > 
    ...
    Fit DR-SC model...
    
    Using accurate PCA to obtain initial values
    Finish DR-SC model fitting
    
    > drscPlot(seu1)
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 21.3Mb
      sub-directories of 1Mb or more:
        data   1.6Mb
        libs  19.3Mb
    ```

# dragon

<details>

* Version: 1.2.1
* GitHub: https://github.com/sjspielman/dragon
* Source code: https://github.com/cran/dragon
* Date/Publication: 2022-04-08 08:42:33 UTC
* Number of recursive dependencies: 131

Run `revdepcheck::cloud_details(, "dragon")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(dragon)
      > 
      > test_check("dragon")
      NULL
      NULL
      NULL
    ...
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test_build-linear-models.R:102:3'): fct_run_linear_models::plot_linear_model_cluster() ──
      `box_legend` is not NULL
      
      `actual` is an S3 object of class <zeroGrob/grob/gDesc>, a list
      `expected` is NULL
      
      [ FAIL 1 | WARN 25 | SKIP 3 | PASS 199 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking package subdirectories ... NOTE
    ```
    Problems with news in ‘NEWS.md’:
    No news entries found.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘htmltools’
      All declared Imports should be used.
    ```

# dynamAedes

<details>

* Version: 2.1.2
* GitHub: https://github.com/mattmar/dynamAedes
* Source code: https://github.com/cran/dynamAedes
* Date/Publication: 2023-03-30 14:10:05 UTC
* Number of recursive dependencies: 148

Run `revdepcheck::cloud_details(, "dynamAedes")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘dynamAedes_local.Rmd’ using rmarkdown
    starting worker pid=4229 on localhost:11343 at 13:14:41.702
    Loading required package: dynamAedes
    loaded dynamAedes and set parent environment
    
      |                                                                            
      |%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%| 100%
    Quitting from lines 341-376 [unnamed-chunk-28] (dynamAedes_local.Rmd)
    Error: processing vignette 'dynamAedes_local.Rmd' failed with diagnostics:
    ...
    Error: processing vignette 'dynamAedes_regional.Rmd' failed with diagnostics:
    The `legend.pos` theme element is not defined in the element hierarchy.
    --- failed re-building ‘dynamAedes_regional.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘dynamAedes_local.Rmd’ ‘dynamAedes_punctual.Rmd’
      ‘dynamAedes_regional.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘rgdal’
    ```

# dynamite

<details>

* Version: 1.4.5
* GitHub: https://github.com/ropensci/dynamite
* Source code: https://github.com/cran/dynamite
* Date/Publication: 2023-08-23 15:50:20 UTC
* Number of recursive dependencies: 103

Run `revdepcheck::cloud_details(, "dynamite")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘dynamite-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.dynamitefit
    > ### Title: Traceplots and Density Plots for a 'dynamitefit' Object
    > ### Aliases: plot.dynamitefit
    > 
    > ### ** Examples
    > 
    > data.table::setDTthreads(1) # For CRAN
    > plot(gaussian_example_fit, type = "beta")
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘dynamite.Rmd’ using rmarkdown
    
    Quitting from lines 500-501 [parameterposteriorplot] (dynamite.Rmd)
    Error: processing vignette 'dynamite.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘dynamite.Rmd’
    
    --- re-building ‘dynamite_priors.Rmd’ using rmarkdown
    ...
    --- finished re-building ‘dynamite_priors.Rmd’
    
    --- re-building ‘dynamite_simulation.Rmd’ using rmarkdown
    --- finished re-building ‘dynamite_simulation.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘dynamite.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 22.1Mb
      sub-directories of 1Mb or more:
        R      5.0Mb
        data  16.2Mb
    ```

# EcoDiet

<details>

* Version: 2.0.0
* GitHub: https://github.com/pyhernvann/EcoDiet
* Source code: https://github.com/cran/EcoDiet
* Date/Publication: 2023-01-06 23:50:02 UTC
* Number of recursive dependencies: 136

Run `revdepcheck::cloud_details(, "EcoDiet")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘convergence_problems.Rmd’ using rmarkdown
    --- finished re-building ‘convergence_problems.Rmd’
    
    --- re-building ‘introduction_EcoDiet.Rmd’ using rmarkdown
    
    Quitting from lines 373-374 [unnamed-chunk-35] (introduction_EcoDiet.Rmd)
    Error: processing vignette 'introduction_EcoDiet.Rmd' failed with diagnostics:
    The `legend.byrow` theme element must be a <logical> object.
    ...
    --- failed re-building ‘introduction_EcoDiet.Rmd’
    
    --- re-building ‘realistic_example.Rmd’ using rmarkdown
    --- finished re-building ‘realistic_example.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘introduction_EcoDiet.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# eHDPrep

<details>

* Version: 1.3.3
* GitHub: https://github.com/overton-group/eHDPrep
* Source code: https://github.com/cran/eHDPrep
* Date/Publication: 2023-06-05 18:20:12 UTC
* Number of recursive dependencies: 122

Run `revdepcheck::cloud_details(, "eHDPrep")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Introduction_to_eHDPrep.Rmd’ using rmarkdown
    
    Quitting from lines 880-891 [unnamed-chunk-56] (Introduction_to_eHDPrep.Rmd)
    Error: processing vignette 'Introduction_to_eHDPrep.Rmd' failed with diagnostics:
    non-numeric argument to binary operator
    --- failed re-building ‘Introduction_to_eHDPrep.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Introduction_to_eHDPrep.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## Newly fixed

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Introduction_to_eHDPrep.Rmd’ using rmarkdown
    tlmgr: package repository https://ctan.math.utah.edu/ctan/tex-archive/systems/texlive/tlnet (verified)
    [1/1, ??:??/??:??] install: grffile [4k]
    running mktexlsr ...
    done running mktexlsr.
    tlmgr: package log updated: /opt/TinyTeX/texmf-var/web2c/tlmgr.log
    tlmgr: command log updated: /opt/TinyTeX/texmf-var/web2c/tlmgr-commands.log
    
    tlmgr: Remote database (rev 69178) seems to be older than local (rev 69180 of texlive-scripts); please use different mirror or  wait a day or so.
    ...
    
    Error: processing vignette 'Introduction_to_eHDPrep.Rmd' failed with diagnostics:
    LaTeX failed to compile /tmp/workdir/eHDPrep/old/eHDPrep.Rcheck/vign_test/eHDPrep/vignettes/Introduction_to_eHDPrep.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See Introduction_to_eHDPrep.log for more info.
    --- failed re-building ‘Introduction_to_eHDPrep.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Introduction_to_eHDPrep.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 7 marked UTF-8 strings
    ```

# episensr

<details>

* Version: 1.3.0
* GitHub: https://github.com/dhaine/episensr
* Source code: https://github.com/cran/episensr
* Date/Publication: 2023-08-30 09:20:05 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::cloud_details(, "episensr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘episensr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.mbias
    > ### Title: Plot DAGs before and after conditioning on collider (M bias)
    > ### Aliases: plot.mbias
    > 
    > ### ** Examples
    > 
    > plot(mbias(or = c(2, 5.4, 2.5, 1.5, 1),
    ...
     11. │     │ └─base::withCallingHandlers(...)
     12. │     └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13. │       └─l$compute_aesthetics(d, plot)
     14. │         └─ggplot2 (local) compute_aesthetics(..., self = self)
     15. └─base::.handleSimpleError(...)
     16.   └─rlang (local) h(simpleError(msg, call))
     17.     └─handlers[[1L]](cnd)
     18.       └─cli::cli_abort(...)
     19.         └─rlang::abort(...)
    Execution halted
    ```

# epos

<details>

* Version: 1.0
* GitHub: NA
* Source code: https://github.com/cran/epos
* Date/Publication: 2021-02-20 01:10:14 UTC
* Number of recursive dependencies: 98

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
      [ FAIL 1 | WARN 12 | SKIP 0 | PASS 13 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      [1] 11 - 9 == 2
      Backtrace:
          ▆
       1. └─testthat::expect_that(length(tanimotobaseline), equals(9)) at test_createTanimotoBaseline.R:39:3
       2.   └─testthat (local) condition(object)
       3.     └─testthat::expect_equal(x, expected, ..., expected.label = label)
      
      [ FAIL 1 | WARN 12 | SKIP 0 | PASS 13 ]
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

# EvoPhylo

<details>

* Version: 0.3.2
* GitHub: https://github.com/tiago-simoes/EvoPhylo
* Source code: https://github.com/cran/EvoPhylo
* Date/Publication: 2022-11-03 17:00:02 UTC
* Number of recursive dependencies: 166

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
    > cluster_df_tsne <- make_clusters(Dmatrix, k = 3, tsne = TRUE,
    +                                  tsne_dim = 2)
    > 
    > # Plot clusters, plots divided into 2 rows, and increasing
    > # overlap of text labels (default = 10)
    > plot(cluster_df_tsne, nrow = 2, max.overlaps = 20)
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘char-part.Rmd’ using rmarkdown
    Warning: ggrepel: 4 unlabeled data points (too many overlaps). Consider increasing max.overlaps
    
    Quitting from lines 99-103 [unnamed-chunk-8] (char-part.Rmd)
    Error: processing vignette 'char-part.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘char-part.Rmd’
    
    --- re-building ‘data_treatment.Rmd’ using rmarkdown
    ...
    Error: processing vignette 'rates-selection_MrBayes.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘rates-selection_MrBayes.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘char-part.Rmd’ ‘fbd-params.Rmd’ ‘rates-selection_BEAST2.Rmd’
      ‘rates-selection_MrBayes.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.9Mb
      sub-directories of 1Mb or more:
        data      2.5Mb
        doc       1.6Mb
        extdata   2.4Mb
    ```

# explainer

<details>

* Version: 1.0.0
* GitHub: https://github.com/PERSIMUNE/explainer
* Source code: https://github.com/cran/explainer
* Date/Publication: 2023-12-15 12:40:06 UTC
* Number of recursive dependencies: 194

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
    Error in train(..., self = self) : 
      unused argument (list("Decision curve analysis", "number of patients in the test set N=226", "probability threshold", "net benefit", "variable", "xintercept"))
    Calls: eDecisionCurve ... ggplotly.ggplot -> gg2list -> get_gdefs_ggproto -> <Anonymous>
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
* Number of recursive dependencies: 109

Run `revdepcheck::cloud_details(, "fable.prophet")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘intro.Rmd’ using rmarkdown
    
    Quitting from lines 147-149 [forecast-plot] (intro.Rmd)
    Error: processing vignette 'intro.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘intro.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘intro.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# fabletools

<details>

* Version: 0.3.4
* GitHub: https://github.com/tidyverts/fabletools
* Source code: https://github.com/cran/fabletools
* Date/Publication: 2023-10-11 22:40:02 UTC
* Number of recursive dependencies: 104

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
    + }) # examplesIf
    > library(fable)
    > library(tsibbledata)
    > fc <- aus_production %>% model(ets = ETS(log(Beer) ~ error("M") + trend("Ad") + 
    +     season("A"))) %>% forecast(h = "3 years")
    > fc %>% autoplot(aus_production)
    Error in if (guide$reverse) key <- key[nrow(key):1, ] : 
      argument is of length zero
    Calls: <Anonymous> ... <Anonymous> -> train -> guide_train -> guide_train.level_guide
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
      
      [ FAIL 2 | WARN 6 | SKIP 1 | PASS 269 ]
      Error: Test failures
      In addition: Warning messages:
      1: `flatten()` is deprecated as of rlang 1.1.0.
      ℹ Please use `purrr::list_flatten()` or `purrr::list_c()`.
      This warning is displayed once every 8 hours. 
      2: `squash()` is deprecated as of rlang 1.1.0.
      This warning is displayed once every 8 hours. 
      Execution halted
    ```

# fairmodels

<details>

* Version: 1.2.1
* GitHub: https://github.com/ModelOriented/fairmodels
* Source code: https://github.com/cran/fairmodels
* Date/Publication: 2022-08-23 19:50:06 UTC
* Number of recursive dependencies: 90

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
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Advanced_tutorial.Rmd’ using rmarkdown
    
    Quitting from lines 286-314 [unnamed-chunk-18] (Advanced_tutorial.Rmd)
    Error: processing vignette 'Advanced_tutorial.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘Advanced_tutorial.Rmd’
    
    --- re-building ‘Basic_tutorial.Rmd’ using rmarkdown
    ...
    Quitting from lines 255-257 [unnamed-chunk-19] (Basic_tutorial.Rmd)
    Error: processing vignette 'Basic_tutorial.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘Basic_tutorial.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘Advanced_tutorial.Rmd’ ‘Basic_tutorial.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# fdaPOIFD

<details>

* Version: 1.0.3
* GitHub: https://github.com/aefdz/fdaPOIFD
* Source code: https://github.com/cran/fdaPOIFD
* Date/Publication: 2022-05-16 16:10:05 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::cloud_details(, "fdaPOIFD")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘fdaPOIFD-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: exampleData
    > ### Title: exampleData
    > ### Aliases: exampleData
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    > data(exampleData)
    > plotPOFD(exampleData$PoFDintervals)
    Warning: Removed 3014 rows containing missing values or values outside the scale range
    (`geom_line()`).
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘FastGP’ ‘MASS’ ‘fdapace’
      All declared Imports should be used.
    ```

# feasts

<details>

* Version: 0.3.1
* GitHub: https://github.com/tidyverts/feasts
* Source code: https://github.com/cran/feasts
* Date/Publication: 2023-03-22 14:20:10 UTC
* Number of recursive dependencies: 99

Run `revdepcheck::cloud_details(, "feasts")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘feasts-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gg_season
    > ### Title: Seasonal plot
    > ### Aliases: gg_season
    > 
    > ### ** Examples
    > 
    > library(tsibble)
    ...
    > tsibbledata::aus_retail %>%
    +   filter(
    +     State == "Victoria",
    +     Industry == "Cafes, restaurants and catering services"
    +   ) %>%
    +   gg_season(Turnover)
    Error in ggplot2::scale_x_date()$trans$breaks(as.Date(limit), n = len) : 
      attempt to apply non-function
    Calls: <Anonymous> ... get_breaks -> <Anonymous> -> get_breaks -> <Anonymous> -> breaks
    Execution halted
    ```

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
      [ FAIL 4 | WARN 1 | SKIP 0 | PASS 90 ]
      
    ...
       12.                     └─scale$get_breaks(continuous_scale_sorted)
       13.                       └─tsibble (local) get_breaks(..., self = self)
       14.                         └─ggplot2::ggproto_parent(ggplot2::ScaleContinuous, self)$get_breaks(limits)
       15.                           └─ggplot2 (local) get_breaks(..., self = self)
       16.                             └─self$breaks(limits)
       17.                               └─feasts (local) breaks(...)
      
      [ FAIL 4 | WARN 1 | SKIP 0 | PASS 90 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘feasts.Rmd’ using rmarkdown
    
    Quitting from lines 49-51 [season-plot] (feasts.Rmd)
    Error: processing vignette 'feasts.Rmd' failed with diagnostics:
    attempt to apply non-function
    --- failed re-building ‘feasts.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘feasts.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ferrn

<details>

* Version: 0.0.2
* GitHub: https://github.com/huizezhang-sherry/ferrn
* Source code: https://github.com/cran/ferrn
* Date/Publication: 2022-08-06 13:50:02 UTC
* Number of recursive dependencies: 105

Run `revdepcheck::cloud_details(, "ferrn")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ferrn-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: explore_trace_search
    > ### Title: Plot the count in each iteration
    > ### Aliases: explore_trace_search
    > 
    > ### ** Examples
    > 
    > # Summary plots for search points in two algorithms
    ...
    map tries to color
    > p1 / p2
    Warning: Removed 5 rows containing missing values or values outside the scale range
    (`geom_label_repel()`).
    Warning: Removed 1 row containing missing values or values outside the scale range
    (`geom_label_repel()`).
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

# figpatch

<details>

* Version: 0.2
* GitHub: https://github.com/BradyAJohnston/figpatch
* Source code: https://github.com/cran/figpatch
* Date/Publication: 2022-05-03 07:00:24 UTC
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "figpatch")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘figpatch-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: fig_scale
    > ### Title: Scales the Dimensions of Multiple Figs
    > ### Aliases: fig_scale
    > 
    > ### ** Examples
    > 
    > library(figpatch)
    ...
    +   )
    > 
    > # without scaling
    > fl %>%
    +   lapply(fig) %>%
    +   fig_wrap(ncol = 2)
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

# flipr

<details>

* Version: 0.3.3
* GitHub: https://github.com/LMJL-Alea/flipr
* Source code: https://github.com/cran/flipr
* Date/Publication: 2023-08-23 09:00:02 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::cloud_details(, "flipr")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘alternative.Rmd’ using rmarkdown
    --- finished re-building ‘alternative.Rmd’
    
    --- re-building ‘exactness.Rmd’ using rmarkdown
    
    Quitting from lines 142-177 [unnamed-chunk-1] (exactness.Rmd)
    Error: processing vignette 'exactness.Rmd' failed with diagnostics:
    unused argument (list("Significance level", "Probability of wrongly rejecting H0", "mf", "intercept", "slope"))
    ...
    --- finished re-building ‘flipr.Rmd’
    
    --- re-building ‘plausibility.Rmd’ using rmarkdown
    --- finished re-building ‘plausibility.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘exactness.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.6Mb
      sub-directories of 1Mb or more:
        doc    9.1Mb
        libs   1.6Mb
    ```

# FLORAL

<details>

* Version: 0.2.0
* GitHub: https://github.com/vdblab/FLORAL
* Source code: https://github.com/cran/FLORAL
* Date/Publication: 2023-07-05 23:43:05 UTC
* Number of recursive dependencies: 137

Run `revdepcheck::cloud_details(, "FLORAL")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Using-FLORAL-for-Microbiome-Analysis.Rmd’ using rmarkdown
    0%   10   20   30   40   50   60   70   80   90   100%
    [----|----|----|----|----|----|----|----|----|----|
    **************************************************|
    0%   10   20   30   40   50   60   70   80   90   100%
    [----|----|----|----|----|----|----|----|----|----|
    **************************************************|
    0%   10   20   30   40   50   60   70   80   90   100%
    [----|----|----|----|----|----|----|----|----|----|
    ...
    Quitting from lines 78-79 [plots] (Using-FLORAL-for-Microbiome-Analysis.Rmd)
    Error: processing vignette 'Using-FLORAL-for-Microbiome-Analysis.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘Using-FLORAL-for-Microbiome-Analysis.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Using-FLORAL-for-Microbiome-Analysis.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.8Mb
      sub-directories of 1Mb or more:
        libs   8.7Mb
    ```

# fmeffects

<details>

* Version: 0.1.1
* GitHub: https://github.com/holgstr/fmeffects
* Source code: https://github.com/cran/fmeffects
* Date/Publication: 2023-09-26 15:10:02 UTC
* Number of recursive dependencies: 158

Run `revdepcheck::cloud_details(, "fmeffects")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘fmeffects-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ForwardMarginalEffect
    > ### Title: R6 Class representing a forward marginal effect (FME)
    > ### Aliases: ForwardMarginalEffect
    > 
    > ### ** Examples
    > 
    > 
    ...
    Warning in ggplot2::geom_segment(ggplot2::aes(x = (0.5 * min(x1) + 0.5 *  :
      All aesthetics have length 1, but the data has 699 rows.
    ℹ Did you mean to use `annotate()`?
    Warning in ggplot2::geom_segment(ggplot2::aes(y = (0.5 * min(x2) + 0.5 *  :
      All aesthetics have length 1, but the data has 699 rows.
    ℹ Did you mean to use `annotate()`?
    Error in as.vector(x, "character") : 
      cannot coerce type 'environment' to vector of type 'character'
    Calls: <Anonymous> ... validDetails.text -> as.character -> as.character.default
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘fme_theory.Rmd’ using rmarkdown
    --- finished re-building ‘fme_theory.Rmd’
    
    --- re-building ‘fmeffects.Rmd’ using rmarkdown
    
    Quitting from lines 92-100 [unnamed-chunk-7] (fmeffects.Rmd)
    Error: processing vignette 'fmeffects.Rmd' failed with diagnostics:
    cannot coerce type 'environment' to vector of type 'character'
    --- failed re-building ‘fmeffects.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘fmeffects.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# foqat

<details>

* Version: 2.0.8.2
* GitHub: https://github.com/tianshu129/foqat
* Source code: https://github.com/cran/foqat
* Date/Publication: 2023-09-30 06:10:02 UTC
* Number of recursive dependencies: 76

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
    
    ...
    --- failed re-building ‘Plot_Functions.Rmd’
    
    --- re-building ‘Trace_Gas_Chemistry.Rmd’ using rmarkdown
    --- finished re-building ‘Trace_Gas_Chemistry.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Plot_Functions.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# forestecology

<details>

* Version: 0.2.0
* GitHub: https://github.com/rudeboybert/forestecology
* Source code: https://github.com/cran/forestecology
* Date/Publication: 2021-10-02 13:30:05 UTC
* Number of recursive dependencies: 103

Run `revdepcheck::cloud_details(, "forestecology")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘forestecology.Rmd’ using rmarkdown
    
    Quitting from lines 177-185 [unnamed-chunk-13] (forestecology.Rmd)
    Error: processing vignette 'forestecology.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
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

# funcharts

<details>

* Version: 1.3.2
* GitHub: https://github.com/unina-sfere/funcharts
* Source code: https://github.com/cran/funcharts
* Date/Publication: 2023-09-05 08:40:02 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::cloud_details(, "funcharts")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘funcharts-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: cbind_mfd
    > ### Title: Bind variables of two Multivariate Functional Data Objects
    > ### Aliases: cbind_mfd
    > 
    > ### ** Examples
    > 
    > library(funcharts)
    > mfdobj1 <- data_sim_mfd(nvar = 3)
    > mfdobj2 <- data_sim_mfd(nvar = 2)
    > dimnames(mfdobj2$coefs)[[3]] <- mfdobj2$fdnames[[3]] <- c("var10", "var11")
    > 
    > plot_mfd(mfdobj1)
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(funcharts)
      > 
      > test_check("funcharts")
      [ FAIL 2 | WARN 20 | SKIP 0 | PASS 97 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
        7.       └─base::lapply(x$plots, plot_table, guides = guides)
        8.         ├─patchwork (local) FUN(X[[i]], ...)
        9.         └─patchwork:::plot_table.ggplot(X[[i]], ...)
       10.           └─patchwork:::add_guides(gt, guides == "collect")
       11.             ├─base::unlist(guide_loc == panel_loc)
       12.             └─base::Ops.data.frame(guide_loc, panel_loc)
      
      [ FAIL 2 | WARN 20 | SKIP 0 | PASS 97 ]
      Error: Test failures
      Execution halted
    ```

# funkyheatmap

<details>

* Version: 0.5.0
* GitHub: https://github.com/funkyheatmap/funkyheatmap
* Source code: https://github.com/cran/funkyheatmap
* Date/Publication: 2023-09-23 06:10:02 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "funkyheatmap")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘funkyheatmap-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: funky_heatmap
    > ### Title: Generate a funky heatmaps for benchmarks
    > ### Aliases: funky_heatmap
    > 
    > ### ** Examples
    > 
    > library(tibble, warn.conflicts = FALSE)
    ...
    ℹ No legends were provided, trying to automatically infer legends.
    ℹ Some palettes were not used in the column info, adding legends for them.
    ℹ Legend 1 did not contain a geom, inferring from the column info.
    ℹ Legend 1 did not contain labels, inferring from the geom.
    ℹ Legend 1 did not contain size, inferring from the labels.
    ℹ Legend 1 did not contain color, inferring from the palette.
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
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
        7.         └─base::lapply(x$plots, plot_table, guides = guides)
        8.           ├─patchwork (local) FUN(X[[i]], ...)
        9.           └─patchwork:::plot_table.ggplot(X[[i]], ...)
       10.             └─patchwork:::add_guides(gt, guides == "collect")
       11.               ├─base::unlist(guide_loc == panel_loc)
       12.               └─base::Ops.data.frame(guide_loc, panel_loc)
      
      [ FAIL 1 | WARN 3 | SKIP 0 | PASS 37 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘mtcars.Rmd’ using rmarkdown
    
    Quitting from lines 39-40 [unnamed-chunk-2] (mtcars.Rmd)
    Error: processing vignette 'mtcars.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘mtcars.Rmd’
    
    --- re-building ‘scIB.Rmd’ using rmarkdown
    ...
    Quitting from lines 242-254 [summary-figure] (scIB.Rmd)
    Error: processing vignette 'scIB.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘scIB.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘mtcars.Rmd’ ‘scIB.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 36 marked UTF-8 strings
    ```

# funtimes

<details>

* Version: 9.1
* GitHub: NA
* Source code: https://github.com/cran/funtimes
* Date/Publication: 2023-03-21 23:40:02 UTC
* Number of recursive dependencies: 191

Run `revdepcheck::cloud_details(, "funtimes")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘beales.Rmd’ using rmarkdown
    --- finished re-building ‘beales.Rmd’
    
    --- re-building ‘trendtests.Rmd’ using rmarkdown
    
    Quitting from lines 53-64 [unnamed-chunk-3] (trendtests.Rmd)
    Error: processing vignette 'trendtests.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    ...
    --- failed re-building ‘trendtests.Rmd’
    
    --- re-building ‘tsclusters.Rmd’ using rmarkdown
    --- finished re-building ‘tsclusters.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘trendtests.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# genekitr

<details>

* Version: 1.2.5
* GitHub: https://github.com/GangLiLab/genekitr
* Source code: https://github.com/cran/genekitr
* Date/Publication: 2023-09-07 08:50:09 UTC
* Number of recursive dependencies: 211

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
    > library(ggplot2)
    ...
      7.         └─ggplot2::ggplotGrob(x)
      8.           ├─ggplot2::ggplot_gtable(ggplot_build(x))
      9.           └─ggplot2:::ggplot_gtable.ggplot_built(ggplot_build(x))
     10.             └─ggplot2:::plot_theme(plot)
     11.               └─ggplot2:::validate_theme(theme)
     12.                 └─base::mapply(...)
     13.                   └─ggplot2 (local) `<fn>`(...)
     14.                     └─cli::cli_abort(...)
     15.                       └─rlang::abort(...)
    Execution halted
    ```

# geomtextpath

<details>

* Version: 0.1.1
* GitHub: https://github.com/AllanCameron/geomtextpath
* Source code: https://github.com/cran/geomtextpath
* Date/Publication: 2022-08-30 17:00:05 UTC
* Number of recursive dependencies: 95

Run `revdepcheck::cloud_details(, "geomtextpath")` for more info

</details>

## Newly broken

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
      [ FAIL 1 | WARN 7 | SKIP 3 | PASS 460 ]
      
    ...
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-coord_curvedpolar.R:76:3'): wrapping first and last labels works as expected ──
      make_label(axis_labels$textpath$label[[9]]$glyph) (`actual`) not identical to expression(paste(1, "/", 10)) (`expected`).
      
      `actual[[1]]`:   `paste(1L, "/", 10L)`
      `expected[[1]]`: `paste(1, "/", 10)`  
      
      [ FAIL 1 | WARN 7 | SKIP 3 | PASS 460 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘aesthetics.Rmd’ using rmarkdown
    
    Quitting from lines 85-90 [hjust_explain2] (aesthetics.Rmd)
    Error: processing vignette 'aesthetics.Rmd' failed with diagnostics:
    non-numeric argument to binary operator
    --- failed re-building ‘aesthetics.Rmd’
    
    --- re-building ‘curved_polar.Rmd’ using rmarkdown
    ...
    --- finished re-building ‘curved_polar.Rmd’
    
    --- re-building ‘geomtextpath.Rmd’ using rmarkdown
    --- finished re-building ‘geomtextpath.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘aesthetics.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# GET

<details>

* Version: 0.5
* GitHub: NA
* Source code: https://github.com/cran/GET
* Date/Publication: 2023-09-29 15:32:44 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::cloud_details(, "GET")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘GET-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: GDPtax
    > ### Title: GDP per capita with country groups and profit tax
    > ### Aliases: GDPtax
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    ...
    > for(i in 1:4)
    +   assign(paste0("p", i), plot(subset(GDPtax$GDP, GDPtax$Group == i)) +
    +     ggplot2::labs(title=paste("Group ", i, sep=""), y="GDP"))
    > p4
    > if(require("patchwork", quietly=TRUE))
    +   p1 + p2 + p3 + p4
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘FDRenvelopes.tex.rsp’ using rsp
    Error: processing vignette 'FDRenvelopes.tex.rsp' failed with diagnostics:
    Running 'texi2dvi' on 'FDRenvelopes.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `thumbpdf.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ...
            
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘pointpatterns.tex.rsp’
    
    SUMMARY: processing the following files failed:
      ‘FDRenvelopes.tex.rsp’ ‘GET.tex.rsp’ ‘HotSpots.tex.rsp’
      ‘pointpatterns.tex.rsp’
    
    Error: Vignette re-building failed.
    Execution halted
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
    
    > ### Name: gg.gap
    > ### Title: Define Segments in y-Axis for 'ggplot2'
    > ### Aliases: gg.gap
    > 
    > ### ** Examples
    > 
    > data(mtcars)
    ...
    +     ggtitle("Number of Cars by Gear") +
    +     xlab("Gears")+
    +     scale_y_continuous(trans = 'reverse')
    > #single segments and missing tick_width
    > gg.gap(plot=p,
    +        segments=c(10,5),
    +        ylim=c(15,0))
    Error in gg.gap(plot = p, segments = c(10, 5), ylim = c(15, 0)) : 
      ylim: c(15,0) is wrong. It should be c(0,15)
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# GGally

<details>

* Version: 2.2.0
* GitHub: https://github.com/ggobi/ggally
* Source code: https://github.com/cran/GGally
* Date/Publication: 2023-11-22 08:30:12 UTC
* Number of recursive dependencies: 146

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
      `actual`:   FALSE
      `expected`: TRUE 
      Backtrace:
          ▆
       1. └─GGally (local) expect_legend("right", right) at test-gglegend.R:23:3
       2.   └─testthat::expect_true(inherits(plotLegend, "gTree")) at test-gglegend.R:18:5
      
      [ FAIL 3 | WARN 1 | SKIP 25 | PASS 479 ]
      Error: Test failures
      Execution halted
    ```

# ggblanket

<details>

* Version: 5.2.0
* GitHub: https://github.com/davidhodge931/ggblanket
* Source code: https://github.com/cran/ggblanket
* Date/Publication: 2023-11-10 01:20:05 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::cloud_details(, "ggblanket")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ggblanket.Rmd’ using rmarkdown
    
    Quitting from lines 87-101 [unnamed-chunk-4] (ggblanket.Rmd)
    Error: processing vignette 'ggblanket.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘ggblanket.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ggblanket.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ggbrain

<details>

* Version: 0.8.1
* GitHub: https://github.com/michaelhallquist/ggbrain
* Source code: https://github.com/cran/ggbrain
* Date/Publication: 2023-03-21 18:00:05 UTC
* Number of recursive dependencies: 75

Run `revdepcheck::cloud_details(, "ggbrain")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ggbrain_aesthetics.Rmd’ using rmarkdown
    --- finished re-building ‘ggbrain_aesthetics.Rmd’
    
    --- re-building ‘ggbrain_introduction.Rmd’ using rmarkdown
    
    Quitting from lines 238-239 [unnamed-chunk-16] (ggbrain_introduction.Rmd)
    Error: processing vignette 'ggbrain_introduction.Rmd' failed with diagnostics:
    comparison (==) is possible only for atomic and list types
    ...
    Quitting from lines 47-54 [unnamed-chunk-2] (ggbrain_labels.Rmd)
    Error: processing vignette 'ggbrain_labels.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘ggbrain_labels.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘ggbrain_introduction.Rmd’ ‘ggbrain_labels.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 18.3Mb
      sub-directories of 1Mb or more:
        doc       3.0Mb
        extdata   1.6Mb
        libs     13.0Mb
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
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> -> print.ggwrap
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ggbreak.Rmd’ using rmarkdown
    
    Quitting from lines 68-84 [unnamed-chunk-2] (ggbreak.Rmd)
    Error: processing vignette 'ggbreak.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘ggbreak.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ggbreak.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ggcharts

<details>

* Version: 0.2.1
* GitHub: https://github.com/thomas-neitmann/ggcharts
* Source code: https://github.com/cran/ggcharts
* Date/Publication: 2020-05-20 00:40:02 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "ggcharts")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggcharts-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pyramid_chart
    > ### Title: Pyramid Chart
    > ### Aliases: pyramid_chart
    > 
    > ### ** Examples
    > 
    > data(popch)
    ...
    > pyramid_chart(popch, age, pop, sex)
    Warning: `expand_scale()` was deprecated in ggplot2 3.3.0.
    ℹ Please use `expansion()` instead.
    ℹ The deprecated feature was likely used in the ggcharts package.
      Please report the issue at
      <https://github.com/thomas-neitmann/ggcharts/issues>.
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

# ggdag

<details>

* Version: 0.2.10
* GitHub: https://github.com/r-causal/ggdag
* Source code: https://github.com/cran/ggdag
* Date/Publication: 2023-05-28 23:30:02 UTC
* Number of recursive dependencies: 103

Run `revdepcheck::cloud_details(, "ggdag")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggdag-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Covariate Adjustment Sets
    > ### Title: Covariate Adjustment Sets
    > ### Aliases: 'Covariate Adjustment Sets' dag_adjustment_sets
    > ###   ggdag_adjustment_set
    > 
    > ### ** Examples
    > 
    ...
     11. │     │ └─base::withCallingHandlers(...)
     12. │     └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13. │       └─l$compute_aesthetics(d, plot)
     14. │         └─ggplot2 (local) compute_aesthetics(..., self = self)
     15. └─base::.handleSimpleError(...)
     16.   └─rlang (local) h(simpleError(msg, call))
     17.     └─handlers[[1L]](cnd)
     18.       └─cli::cli_abort(...)
     19.         └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggdag)
      
      Attaching package: 'ggdag'
      
      The following object is masked from 'package:stats':
    ...
      • quick_plots/ggdag-collider-triangle-is-triangle-too.svg
      • quick_plots/ggdag-confounder-triangle-is-triangle.svg
      • relations/ggdag-ancestors-identifies-v-w1-and-z1.svg
      • relations/ggdag-descendants-identifies-y-x-and-z1.svg
      • relations/ggdag-parents-identifies-z2-x-w1-and-w2.svg
      • themes/theme-dag-gray-grid.svg
      • themes/theme-dag-gray.svg
      • themes/theme-dag-grid.svg
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘bias-structures.Rmd’ using rmarkdown
    
    Quitting from lines 44-46 [unnamed-chunk-1] (bias-structures.Rmd)
    Error: processing vignette 'bias-structures.Rmd' failed with diagnostics:
    Problem while computing aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `plot$scales$add_defaults()`:
    ! attempt to apply non-function
    --- failed re-building ‘bias-structures.Rmd’
    ...
    ℹ Error occurred in the 1st layer.
    Caused by error in `plot$scales$add_defaults()`:
    ! attempt to apply non-function
    --- failed re-building ‘intro-to-ggdag.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘bias-structures.Rmd’ ‘intro-to-dags.Rmd’ ‘intro-to-ggdag.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ggdist

<details>

* Version: 3.3.1
* GitHub: https://github.com/mjskay/ggdist
* Source code: https://github.com/cran/ggdist
* Date/Publication: 2023-11-27 06:00:02 UTC
* Number of recursive dependencies: 133

Run `revdepcheck::cloud_details(, "ggdist")` for more info

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
      • test.stat_sample_slabinterval/fill-type-gradient-with-two-groups-h.svg
      • test.stat_sample_slabinterval/gradientintervalh-with-two-groups.svg
      • test.stat_sample_slabinterval/halfeyeh-log-scale-tri-no-trim.svg
      • test.stat_sample_slabinterval/histinterval-with-outlines-bw-bars.svg
      • test.stat_sample_slabinterval/histintervalh-log-scale-transform.svg
      • test.stat_sample_slabinterval/histintervalh-with-outline.svg
      • test.stat_sample_slabinterval/nas-with-na-rm-true.svg
      • test.theme_ggdist/facet-titles-on-left.svg
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘dotsinterval.Rmd’ using rmarkdown
    
    Quitting from lines 347-367 [layout_top] (dotsinterval.Rmd)
    Error: processing vignette 'dotsinterval.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘dotsinterval.Rmd’
    
    --- re-building ‘freq-uncertainty-vis.Rmd’ using rmarkdown
    --- finished re-building ‘freq-uncertainty-vis.Rmd’
    ...
    Quitting from lines 46-173 [slabinterval_family] (slabinterval.Rmd)
    Error: processing vignette 'slabinterval.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘slabinterval.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘dotsinterval.Rmd’ ‘slabinterval.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        doc    2.3Mb
        libs   1.0Mb
    ```

# ggDoE

<details>

* Version: 0.7.9
* GitHub: https://github.com/toledo60/ggDoE
* Source code: https://github.com/cran/ggDoE
* Date/Publication: 2023-07-06 20:20:02 UTC
* Number of recursive dependencies: 95

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
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

# ggeasy

<details>

* Version: 0.1.4
* GitHub: https://github.com/jonocarroll/ggeasy
* Source code: https://github.com/cran/ggeasy
* Date/Publication: 2023-03-12 10:00:23 UTC
* Number of recursive dependencies: 95

Run `revdepcheck::cloud_details(, "ggeasy")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘shortcuts.Rmd’ using rmarkdown
    
    Quitting from lines 38-52 [unnamed-chunk-4] (shortcuts.Rmd)
    Error: processing vignette 'shortcuts.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘shortcuts.Rmd’
    
    --- re-building ‘tests_and_coverage.Rmd’ using rmarkdown
    --- finished re-building ‘tests_and_coverage.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘shortcuts.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ggedit

<details>

* Version: 0.3.1
* GitHub: https://github.com/yonicd/ggedit
* Source code: https://github.com/cran/ggedit
* Date/Publication: 2020-06-02 11:50:06 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "ggedit")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggedit-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: cloneFacet
    > ### Title: Clone ggplot facet object
    > ### Aliases: cloneFacet
    > 
    > ### ** Examples
    > 
    >  obj=ggplot2::facet_grid(a+b~c+d,scales = 'free',as.table = FALSE,switch = 'x',shrink = FALSE)
    > 
    >  cloneFacet(obj)
    Error in if (use.names && nt[i] == nc[i]) dQuote(nt[i]) else i : 
      missing value where TRUE/FALSE needed
    Calls: cloneFacet ... lapply -> FUN -> all.equal -> all.equal.list -> paste0
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# ggfittext

<details>

* Version: 0.10.1
* GitHub: https://github.com/wilkox/ggfittext
* Source code: https://github.com/cran/ggfittext
* Date/Publication: 2023-09-05 11:50:14 UTC
* Number of recursive dependencies: 79

Run `revdepcheck::cloud_details(, "ggfittext")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggfittext)
      > 
      > test_check("ggfittext")
      [ FAIL 3 | WARN 0 | SKIP 14 | PASS 4 ]
      
    ...
      • richtext/basic-rich-text-with-grow-and-reflow.svg
      • richtext/basic-rich-text-with-grow.svg
      • richtext/basic-rich-text-with-reflow.svg
      • richtext/complex-rich-text-with-grow-and-reflow.svg
      • richtext/complex-rich-text-with-reflow-only.svg
      • richtext/rich-bar-plot-with-grow-and-reflow.svg
      • richtext/rich-bar-plot-with-grow.svg
      • richtext/rich-bar-plot-with-reflow.svg
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘introduction-to-ggfittext.Rmd’ using rmarkdown
    
    Quitting from lines 140-146 [unnamed-chunk-10] (introduction-to-ggfittext.Rmd)
    Error: processing vignette 'introduction-to-ggfittext.Rmd' failed with diagnostics:
    Problem while converting geom to grob.
    ℹ Error occurred in the 2nd layer.
    Caused by error in `x == -Inf`:
    ! comparison (==) is possible only for atomic and list types
    --- failed re-building ‘introduction-to-ggfittext.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘introduction-to-ggfittext.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ggforce

<details>

* Version: 0.4.1
* GitHub: https://github.com/thomasp85/ggforce
* Source code: https://github.com/cran/ggforce
* Date/Publication: 2022-10-04 09:50:02 UTC
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
    > ggplot(mpg) +
    +   geom_point(aes(x = .panel_x, y = .panel_y)) +
    +   facet_matrix(vars(displ, cty, hwy))
    Error in !params$axis_labels$x : invalid argument type
    Calls: <Anonymous> ... <Anonymous> -> draw_panels -> <Anonymous> -> draw_panels
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 29.0Mb
      sub-directories of 1Mb or more:
        help   1.4Mb
        libs  26.6Mb
    ```

# ggfun

<details>

* Version: 0.1.3
* GitHub: NA
* Source code: https://github.com/cran/ggfun
* Date/Publication: 2023-09-15 10:12:08 UTC
* Number of recursive dependencies: 63

Run `revdepcheck::cloud_details(, "ggfun")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggfun-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: keybox
    > ### Title: keybox
    > ### Aliases: keybox
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    > p <- ggplot(mtcars, aes(mpg, disp, color=factor(cyl), size=cyl)) + geom_point()
    > keybox(p, 'roundrect', gp = gpar(col = '#808080', lty = "dashed"))
    Error in g$grob[[i]] : no such index at level 2
    Calls: keybox
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
    > 
    > #single segments and missing tick_width
    > gggap(
    +   plot = p,
    +   segments = c(10, 5),
    +   ylim = c(15, 0))
    Error in desired_transform_valid(trans, ascending_ylim, ylim) : 
      ylim: c(15,0) is wrong. It should be c(0,15)
    Calls: gggap -> desired_transform_valid
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# gghdr

<details>

* Version: 0.2.0
* GitHub: https://github.com/Sayani07/gghdr
* Source code: https://github.com/cran/gghdr
* Date/Publication: 2022-10-27 15:15:19 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::cloud_details(, "gghdr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gghdr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_hdr_boxplot
    > ### Title: Box plot for the highest density region
    > ### Aliases: geom_hdr_boxplot
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
      7.         └─ggplot2 (local) train_df(..., self = self)
      8.           └─base::lapply(self$scales, function(scale) scale$train_df(df = df))
      9.             └─ggplot2 (local) FUN(X[[i]], ...)
     10.               └─scale$train_df(df = df)
     11.                 └─ggplot2 (local) train_df(..., self = self)
     12.                   └─self$train(df[[aesthetic]])
     13.                     └─ggplot2 (local) train(..., self = self)
     14.                       └─cli::cli_abort(...)
     15.                         └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(gghdr)
      > library(ggplot2)
      > library(vdiffr)
      > 
      > test_check("gghdr")
    ...
       14.                     └─scale$train_df(df = df)
       15.                       └─ggplot2 (local) train_df(..., self = self)
       16.                         └─self$train(df[[aesthetic]])
       17.                           └─ggplot2 (local) train(..., self = self)
       18.                             └─cli::cli_abort(...)
       19.                               └─rlang::abort(...)
      
      [ FAIL 3 | WARN 5 | SKIP 1 | PASS 8 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘gghdr.Rmd’ using rmarkdown
    
    Quitting from lines 93-98 [setup] (gghdr.Rmd)
    Error: processing vignette 'gghdr.Rmd' failed with diagnostics:
    Continuous values supplied to discrete scale.
    ℹ Example values: c(0.99, 0.95, 0.5, 0.99, 0.95, 0.5)
    --- failed re-building ‘gghdr.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘gghdr.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# gghdx

<details>

* Version: 0.1.1
* GitHub: https://github.com/OCHA-DAP/gghdx
* Source code: https://github.com/cran/gghdx
* Date/Publication: 2023-08-18 18:22:33 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::cloud_details(, "gghdx")` for more info

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
      
      `environment(actual$super)$env$call`:   `scale_color_gradient2_hdx()` 
      `environment(expected$super)$env$call`: `scale_colour_gradient2_hdx()`
      
      `environment(actual$super)$members$call`:   `scale_color_gradient2_hdx()` 
      `environment(expected$super)$members$call`: `scale_colour_gradient2_hdx()`
      
      [ FAIL 9 | WARN 18 | SKIP 0 | PASS 58 ]
      Error: Test failures
      Execution halted
    ```

# ggHoriPlot

<details>

* Version: 1.0.1
* GitHub: https://github.com/rivasiker/ggHoriPlot
* Source code: https://github.com/cran/ggHoriPlot
* Date/Publication: 2022-10-11 16:22:33 UTC
* Number of recursive dependencies: 118

Run `revdepcheck::cloud_details(, "ggHoriPlot")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘examples.Rmd’ using rmarkdown
    --- finished re-building ‘examples.Rmd’
    
    --- re-building ‘ggHoriPlot.Rmd’ using rmarkdown
    
    Quitting from lines 124-149 [midpoint_3] (ggHoriPlot.Rmd)
    Error: processing vignette 'ggHoriPlot.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘ggHoriPlot.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ggHoriPlot.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ggiraph

<details>

* Version: 0.8.8
* GitHub: https://github.com/davidgohel/ggiraph
* Source code: https://github.com/cran/ggiraph
* Date/Publication: 2023-12-09 15:50:02 UTC
* Number of recursive dependencies: 98

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
      
      test-geom_label_interactive.R.    0 tests    
      test-geom_label_interactive.R.    0 tests    
      test-geom_label_interactive.R.    0 tests    
      test-geom_label_interactive.R.    0 tests    
      test-geom_label_interactive.R.    0 tests    
      test-geom_label_interactive.R.    8 tests [0;32mOK[0m Error in sort.int(x, na.last = na.last, decreasing = decreasing, ...) : 
        'x' must be atomic
      Calls: <Anonymous> ... FUN -> eval -> eval -> sort -> sort.default -> sort.int
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.9Mb
      sub-directories of 1Mb or more:
        libs   9.5Mb
    ```

# ggiraphExtra

<details>

* Version: 0.3.0
* GitHub: https://github.com/cardiomoon/ggiraphExtra
* Source code: https://github.com/cran/ggiraphExtra
* Date/Publication: 2020-10-06 07:00:02 UTC
* Number of recursive dependencies: 125

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

# gglm

<details>

* Version: 1.0.2
* GitHub: https://github.com/graysonwhite/gglm
* Source code: https://github.com/cran/gglm
* Date/Publication: 2023-03-21 18:00:02 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::cloud_details(, "gglm")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gglm-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gglm
    > ### Title: gglm
    > ### Aliases: gglm
    > 
    > ### ** Examples
    > 
    > data(mtcars)
    > m1 <- lm(mpg ~ cyl + disp + hp, data = mtcars)
    > gglm(m1)
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
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

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ggmice.Rmd’ using rmarkdown
    
    Quitting from lines 184-190 [facet] (ggmice.Rmd)
    Error: processing vignette 'ggmice.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘ggmice.Rmd’
    
    --- re-building ‘old_friends.Rmd’ using rmarkdown
    ...
    Quitting from lines 195-206 [bwplots] (old_friends.Rmd)
    Error: processing vignette 'old_friends.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘old_friends.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘ggmice.Rmd’ ‘old_friends.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ggmosaic

<details>

* Version: 0.3.3
* GitHub: https://github.com/haleyjeppson/ggmosaic
* Source code: https://github.com/cran/ggmosaic
* Date/Publication: 2021-02-23 19:50:02 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::cloud_details(, "ggmosaic")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ggmosaic.Rmd’ using rmarkdown
    
    Quitting from lines 45-84 [variety] (ggmosaic.Rmd)
    Error: processing vignette 'ggmosaic.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘ggmosaic.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ggmosaic.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    ':::' call which should be '::': ‘scales:::censor’
      See the note in ?`:::` about the use of this operator.
    Unexported object imported by a ':::' call: ‘productplots:::bound’
      See the note in ?`:::` about the use of this operator.
    ```

# ggnewscale

<details>

* Version: 0.4.9
* GitHub: https://github.com/eliocamp/ggnewscale
* Source code: https://github.com/cran/ggnewscale
* Date/Publication: 2023-05-25 07:30:02 UTC
* Number of recursive dependencies: 63

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
      [ FAIL 6 | WARN 0 | SKIP 0 | PASS 1 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      [ FAIL 6 | WARN 0 | SKIP 0 | PASS 1 ]
      Deleting unused snapshots:
      • newscale/guides-outisde-of-scales.svg
      • newscale/guides.svg
      • newscale/guides2.svg
      • newscale/implicit-mapping.svg
      • newscale/many-layers.svg
      • newscale/respects-override-aes-2.svg
      Error: Test failures
      Execution halted
    ```

# ggnuplot

<details>

* Version: 0.1.0
* GitHub: https://github.com/hriebl/ggnuplot
* Source code: https://github.com/cran/ggnuplot
* Date/Publication: 2020-06-04 13:50:06 UTC
* Number of recursive dependencies: 28

Run `revdepcheck::cloud_details(, "ggnuplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggnuplot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: scale_color_gnuplot
    > ### Title: The gnuplot color palette for discrete data, ported to ggplot2
    > ### Aliases: scale_color_gnuplot scale_colour_gnuplot scale_fill_gnuplot
    > ###   gnupalette gnucolors
    > 
    > ### ** Examples
    > 
    ...
    > 
    > ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
    +   geom_point() +
    +   scale_color_gnuplot() +
    +   scale_x_gnuplot() +
    +   scale_y_gnuplot() +
    +   theme_gnuplot()
    Error in get(x, envir = env, mode = "function") : invalid first argument
    Calls: scale_x_gnuplot ... gnuaxis -> dup_axis -> sec_axis -> as_function -> get
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# ggpicrust2

<details>

* Version: 1.7.3
* GitHub: https://github.com/cafferychen777/ggpicrust2
* Source code: https://github.com/cran/ggpicrust2
* Date/Publication: 2023-11-08 16:10:02 UTC
* Number of recursive dependencies: 249

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
    > # Create example metadata
    > # Please ensure the sample IDs in the metadata have the column name "sample_name"
    > metadata_example <- data.frame(sample_name = colnames(kegg_abundance_example),
    +                                group = factor(rep(c("Control", "Treatment"), each = 5)))
    > 
    > pca_plot <- pathway_pca(kegg_abundance_example, metadata_example, "group")
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: pathway_pca ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        R      2.0Mb
        data   1.6Mb
    ```

# ggpp

<details>

* Version: 0.5.5
* GitHub: https://github.com/aphalo/ggpp
* Source code: https://github.com/cran/ggpp
* Date/Publication: 2023-11-08 00:50:02 UTC
* Number of recursive dependencies: 83

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
    > p +
    +   geom_label_s(aes(colour = factor(cyl)),
    +               nudge_x = 0.3,
    +               arrow = arrow(angle = 20,
    +                             length = grid::unit(1/3, "lines"))) +
    +   scale_colour_discrete(l = 40) + # luminance, make colours darker
    +   expand_limits(x = 7)
    Error in 1 - vjust[is_case] : non-numeric argument to binary operator
    Calls: <Anonymous> ... lapply -> FUN -> <Anonymous> -> draw_key -> rotate_just
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggpp)
      Loading required package: ggplot2
      Registered S3 methods overwritten by 'ggpp':
        method                  from   
        heightDetails.titleGrob ggplot2
        widthDetails.titleGrob  ggplot2
    ...
      * stat_fmt_tb/stat-fmt-tb-2.svg
      * stat_fmt_tb/stat-fmt-tb-3.svg
      * stat_fmt_tb/stat-fmt-tb-4.svg
      * stat_panel_counts/stat-group-counts-x.svg
      * stat_panel_counts/stat-group-counts-xy-color.svg
      * stat_panel_counts/stat-group-counts-y.svg
      * stat_panel_counts/stat-panel-counts-x.svg
      * stat_panel_counts/stat-panel-counts-y.svg
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘grammar-extensions.Rmd’ using rmarkdown
    
    Quitting from lines 807-819 [unnamed-chunk-22] (grammar-extensions.Rmd)
    Error: processing vignette 'grammar-extensions.Rmd' failed with diagnostics:
    non-numeric argument to binary operator
    --- failed re-building ‘grammar-extensions.Rmd’
    
    --- re-building ‘nudge-examples.Rmd’ using rmarkdown
    --- finished re-building ‘nudge-examples.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘grammar-extensions.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ggprism

<details>

* Version: 1.0.4
* GitHub: https://github.com/csdaw/ggprism
* Source code: https://github.com/cran/ggprism
* Date/Publication: 2022-11-04 15:20:05 UTC
* Number of recursive dependencies: 106

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
      2. └─ggplot2:::print.ggplot(x)
      3.   ├─ggplot2::ggplot_gtable(data)
      4.   └─ggplot2:::ggplot_gtable.ggplot_built(data)
      5.     └─ggplot2:::plot_theme(plot)
      6.       └─ggplot2:::validate_theme(theme)
      7.         └─base::mapply(...)
      8.           └─ggplot2 (local) `<fn>`(...)
      9.             └─cli::cli_abort(...)
     10.               └─rlang::abort(...)
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
      test-annotation_ticks.R.......   21 tests [0;32mOK[0m [0;34m1.1s[0m
      
      test-guide_prism_bracket.R....    0 tests    
      test-guide_prism_bracket.R....    0 tests    
      test-guide_prism_bracket.R....    0 tests    
      test-guide_prism_bracket.R....    0 tests    
      test-guide_prism_bracket.R....    0 tests    
      test-guide_prism_bracket.R....    1 tests [0;32mOK[0m Error in if (msg != "") { : the condition has length > 1
      Calls: <Anonymous> ... lapply -> FUN -> eval -> eval -> expect_silent -> fun
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘axes.Rmd’ using rmarkdown
    
    Quitting from lines 38-48 [unnamed-chunk-2] (axes.Rmd)
    Error: processing vignette 'axes.Rmd' failed with diagnostics:
    <Guide> classes have been rewritten as <ggproto> classes.
    The old S3 guide methods have been superseded.
    --- failed re-building ‘axes.Rmd’
    
    --- re-building ‘colours.Rmd’ using rmarkdown
    ...
    Error: processing vignette 'themes.Rmd' failed with diagnostics:
    The `legend.text.align` theme element is not defined in the element
    hierarchy.
    --- failed re-building ‘themes.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘axes.Rmd’ ‘colours.Rmd’ ‘ggprism.Rmd’ ‘pvalues.Rmd’ ‘themes.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ggragged

<details>

* Version: 0.1.0
* GitHub: https://github.com/mikmart/ggragged
* Source code: https://github.com/cran/ggragged
* Date/Publication: 2023-04-20 16:42:32 UTC
* Number of recursive dependencies: 66

Run `revdepcheck::cloud_details(, "ggragged")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggragged-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: facet_ragged
    > ### Title: Lay out panels in a ragged grid
    > ### Aliases: facet_ragged facet_ragged_rows facet_ragged_cols
    > 
    > ### ** Examples
    > 
    > p <- ggplot(Indometh, aes(time, conc)) + geom_line()
    ...
    > # Panels for each subject, with cohorts on separate rows
    > p + facet_ragged_rows(
    +   vars(Cohort = 1 + Subject %in% 3:6),
    +   vars(Subject = as.character(Subject)),
    +   labeller = label_both
    + )
    Error in if (params$axis_labels$x) layout$SCALE_X else seq(n) : 
      argument is of length zero
    Calls: <Anonymous> ... <Anonymous> -> draw_panels -> <Anonymous> -> draw_panels
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
       12.                 └─ggragged (local) draw_panels(...)
       13.                   └─FacetWrap$draw_panels(...)
       14.                     └─ggplot2 (local) draw_panels(..., self = self)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 12 ]
      Deleting unused snapshots:
      • facet_ragged_cols/x-axes-on-both-sides-switched.svg
      • facet_ragged_rows/y-axes-on-both-sides-switched.svg
      Error: Test failures
      Execution halted
    ```

# ggraph

<details>

* Version: 2.1.0
* GitHub: https://github.com/thomasp85/ggraph
* Source code: https://github.com/cran/ggraph
* Date/Publication: 2022-10-09 20:33:19 UTC
* Number of recursive dependencies: 100

Run `revdepcheck::cloud_details(, "ggraph")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggraph-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_edge_density
    > ### Title: Show edges as a density map
    > ### Aliases: geom_edge_density
    > 
    > ### ** Examples
    > 
    > require(tidygraph)
    ...
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    Warning: Raster pixels are placed at uneven horizontal intervals and will be shifted
    ℹ Consider using `geom_tile()` instead.
    Warning: Raster pixels are placed at uneven horizontal intervals and will be shifted
    ℹ Consider using `geom_tile()` instead.
    Error: Unknown colour name: TRUE
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Edges.Rmd’ using rmarkdown
    
    Quitting from lines 142-145 [unnamed-chunk-7] (Edges.Rmd)
    Error: processing vignette 'Edges.Rmd' failed with diagnostics:
    Unknown colour name: TRUE
    --- failed re-building ‘Edges.Rmd’
    
    --- re-building ‘Layouts.Rmd’ using rmarkdown
    ...
    --- finished re-building ‘Nodes.Rmd’
    
    --- re-building ‘tidygraph.Rmd’ using rmarkdown
    --- finished re-building ‘tidygraph.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Edges.Rmd’
    
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
      installed size is 11.0Mb
      sub-directories of 1Mb or more:
        doc    3.5Mb
        libs   5.9Mb
    ```

# ggridges

<details>

* Version: 0.5.5
* GitHub: https://github.com/wilkelab/ggridges
* Source code: https://github.com/cran/ggridges
* Date/Publication: 2023-12-15 05:30:02 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::cloud_details(, "ggridges")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggridges)
      > 
      > test_check("ggridges")
      Picking joint bandwidth of 0.181
      [ FAIL 1 | WARN 1 | SKIP 5 | PASS 57 ]
      
    ...
        'test_theme_ridges.R:25:3'
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test_scale_cyclical.R:26:3'): basic tests ─────────────────────────
      "guide-box" %in% ggplotGrob(p)$layout$name not equal to TRUE.
      1 element mismatch
      
      [ FAIL 1 | WARN 1 | SKIP 5 | PASS 57 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘gallery.Rmd’ using rmarkdown
    --- finished re-building ‘gallery.Rmd’
    
    --- re-building ‘introduction.Rmd’ using rmarkdown
    
    Quitting from lines 35-41 [unnamed-chunk-2] (introduction.Rmd)
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6242 marked UTF-8 strings
    ```

# ggseqplot

<details>

* Version: 0.8.3
* GitHub: https://github.com/maraab23/ggseqplot
* Source code: https://github.com/cran/ggseqplot
* Date/Publication: 2023-09-22 21:30:02 UTC
* Number of recursive dependencies: 131

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
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ggseqplot.Rmd’ using rmarkdown
    
    Quitting from lines 305-325 [unnamed-chunk-2] (ggseqplot.Rmd)
    Error: processing vignette 'ggseqplot.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘ggseqplot.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ggseqplot.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ggside

<details>

* Version: 0.2.3
* GitHub: https://github.com/jtlandis/ggside
* Source code: https://github.com/cran/ggside
* Date/Publication: 2023-12-10 06:00:06 UTC
* Number of recursive dependencies: 77

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
    > p <-ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species, fill = Species)) +
    + geom_point()
    > 
    > #sidebar - uses StatCount
    > p +
    + geom_xsidebar() +
    + geom_ysidebar()
    Error in guide$position : object of type 'closure' is not subsettable
    Calls: <Anonymous> ... panel_guides_grob -> %||% -> guide_for_position -> vapply -> FUN
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(vdiffr)
      > library(ggplot2)
      > library(ggside)
      Registered S3 method overwritten by 'ggside':
        method from   
        +.gg   ggplot2
    ...
      • non_aes_mapping_legend/non-aes-color-blue.svg
      • non_aes_mapping_legend/non-aes-xcolor-red.svg
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
      ...
    --- re-building ‘ggside_aes_mapping.Rmd’ using rmarkdown
    
    Quitting from lines 78-79 [ggside_legacy_example] (ggside_aes_mapping.Rmd)
    Error: processing vignette 'ggside_aes_mapping.Rmd' failed with diagnostics:
    object of type 'closure' is not subsettable
    --- failed re-building ‘ggside_aes_mapping.Rmd’
    
    --- re-building ‘ggside_basic_usage.Rmd’ using rmarkdown
    ...
    Quitting from lines 73-77 [ggside_summarise_diamond_base] (ggside_basic_usage.Rmd)
    Error: processing vignette 'ggside_basic_usage.Rmd' failed with diagnostics:
    object of type 'closure' is not subsettable
    --- failed re-building ‘ggside_basic_usage.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘ggside_aes_mapping.Rmd’ ‘ggside_basic_usage.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ggstance

<details>

* Version: 0.3.6
* GitHub: https://github.com/lionel-/ggstance
* Source code: https://github.com/cran/ggstance
* Date/Publication: 2022-11-16 00:20:02 UTC
* Number of recursive dependencies: 93

Run `revdepcheck::cloud_details(, "ggstance")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > .libPaths()
      [1] "/tmp/workdir/ggstance/new/ggstance.Rcheck"
      [2] "/tmp/workdir/ggstance/new"                
      [3] "/opt/R/4.3.1/lib/R/site-library"          
      [4] "/opt/R/4.3.1/lib/R/library"               
      > library("testthat")
      > library("ggstance")
    ...
      • geoms/geom-boxploth-facet-wrap-with-fill.svg
      • geoms/geom-boxploth-with-fill.svg
      • geoms/geom-histogramh-position-nudge-with-fill.svg
      • geoms/geom-histogramh-position-stack-with-fill.svg
      • geoms/geom-pointrangeh-facet-wrap.svg
      • geoms/geom-pointrangeh-position-dodgev.svg
      • geoms/geom-violinh-draw-quantiles.svg
      • geoms/geom-violinh-facet-wrap.svg
      Error: Test failures
      Execution halted
    ```

# ggstats

<details>

* Version: 0.5.1
* GitHub: https://github.com/larmarange/ggstats
* Source code: https://github.com/cran/ggstats
* Date/Publication: 2023-11-21 08:10:02 UTC
* Number of recursive dependencies: 118

Run `revdepcheck::cloud_details(, "ggstats")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggstats-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggcoef_model
    > ### Title: Plot model coefficients
    > ### Aliases: ggcoef_model ggcoef_table ggcoef_compare ggcoef_multinom
    > ###   ggcoef_multicomponents ggcoef_plot
    > 
    > ### ** Examples
    > 
    > mod <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
    > ggcoef_model(mod)
    > 
    > ggcoef_table(mod)
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ggcoef_model.Rmd’ using rmarkdown
    
    Quitting from lines 223-225 [unnamed-chunk-18] (ggcoef_model.Rmd)
    Error: processing vignette 'ggcoef_model.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘ggcoef_model.Rmd’
    
    --- re-building ‘gglikert.Rmd’ using rmarkdown
    ...
    --- finished re-building ‘stat_prop.Rmd’
    
    --- re-building ‘stat_weighted_mean.Rmd’ using rmarkdown
    --- finished re-building ‘stat_weighted_mean.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ggcoef_model.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ggstatsplot

<details>

* Version: 0.12.1
* GitHub: https://github.com/IndrajeetPatil/ggstatsplot
* Source code: https://github.com/cran/ggstatsplot
* Date/Publication: 2023-09-20 22:10:03 UTC
* Number of recursive dependencies: 166

Run `revdepcheck::cloud_details(, "ggstatsplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggstatsplot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: combine_plots
    > ### Title: Combining and arranging multiple plots in a grid
    > ### Aliases: combine_plots
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
    +     theme = theme(
    +       plot.subtitle = element_text(size = 20),
    +       plot.title = element_text(size = 30)
    +     )
    +   )
    + )
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

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
      • pairwise_ggsignif/within-non-parametric-all.svg
      • pairwise_ggsignif/within-non-parametric-only-non-significant.svg
      • pairwise_ggsignif/within-non-parametric-only-significant.svg
      • pairwise_ggsignif/within-parametric-all.svg
      • pairwise_ggsignif/within-parametric-only-significant.svg
      • pairwise_ggsignif/within-robust-all.svg
      • pairwise_ggsignif/within-robust-only-non-significant.svg
      • pairwise_ggsignif/within-robust-only-significant.svg
      Error: Test failures
      Execution halted
    ```

# ggsurvfit

<details>

* Version: 1.0.0
* GitHub: https://github.com/pharmaverse/ggsurvfit
* Source code: https://github.com/cran/ggsurvfit
* Date/Publication: 2023-10-31 01:00:02 UTC
* Number of recursive dependencies: 116

Run `revdepcheck::cloud_details(, "ggsurvfit")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘themes.Rmd’ using rmarkdown
    
    Quitting from lines 26-41 [unnamed-chunk-2] (themes.Rmd)
    Error: processing vignette 'themes.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘themes.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘themes.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ggtern

<details>

* Version: 3.4.2
* GitHub: NA
* Source code: https://github.com/cran/ggtern
* Date/Publication: 2023-06-06 11:10:02 UTC
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
  object 'build_guides' not found
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
# ggthemes

<details>

* Version: 5.0.0
* GitHub: https://github.com/jrnold/ggthemes
* Source code: https://github.com/cran/ggthemes
* Date/Publication: 2023-11-21 10:50:02 UTC
* Number of recursive dependencies: 103

Run `revdepcheck::cloud_details(, "ggthemes")` for more info

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
      
      `environment(actual$super)$env$call`:   `scale_colour_stata()`
      `environment(expected$super)$env$call`: `scale_color_stata()` 
      
      `environment(actual$super)$members$call`:   `scale_colour_stata()`
      `environment(expected$super)$members$call`: `scale_color_stata()` 
      
      [ FAIL 8 | WARN 71 | SKIP 3 | PASS 264 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 138 marked UTF-8 strings
    ```

# ggVennDiagram

<details>

* Version: 1.4.9
* GitHub: https://github.com/gaospecial/ggVennDiagram
* Source code: https://github.com/cran/ggVennDiagram
* Date/Publication: 2023-12-22 03:40:03 UTC
* Number of recursive dependencies: 99

Run `revdepcheck::cloud_details(, "ggVennDiagram")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggVennDiagram-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_shapes
    > ### Title: plot all shapes provided by internal dataset
    > ### Aliases: plot_shapes
    > 
    > ### ** Examples
    > 
    > plot_shapes()
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘fully-customed.Rmd’ using rmarkdown
    --- finished re-building ‘fully-customed.Rmd’
    
    --- re-building ‘using-ggVennDiagram.Rmd’ using rmarkdown
    
    Quitting from lines 114-120 [unnamed-chunk-8] (using-ggVennDiagram.Rmd)
    Error: processing vignette 'using-ggVennDiagram.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘using-ggVennDiagram.Rmd’
    ...
    --- failed re-building ‘using-new-shapes.Rmd’
    
    --- re-building ‘venn-plot-with-more-than-four-sets.Rmd’ using rmarkdown
    --- finished re-building ‘venn-plot-with-more-than-four-sets.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘using-ggVennDiagram.Rmd’ ‘using-new-shapes.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.2Mb
      sub-directories of 1Mb or more:
        doc   6.0Mb
    ```

# ghibli

<details>

* Version: 0.3.3
* GitHub: https://github.com/ewenme/ghibli
* Source code: https://github.com/cran/ghibli
* Date/Publication: 2022-08-26 13:52:03 UTC
* Number of recursive dependencies: 72

Run `revdepcheck::cloud_details(, "ghibli")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ghibli)
      > 
      > test_check("ghibli")
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 8 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-scales.R:40:3'): scale_colour_ghibli_d fails as expected ─────
      `base_color_plot + scale_colour_ghibli_d()` did not throw an error.
      ── Failure ('test-scales.R:67:3'): scale_fill_ghibli_d fails as expected ───────
      `base_fill_plot + scale_fill_ghibli_d()` did not throw an error.
      
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 8 ]
      Error: Test failures
      Execution halted
    ```

# glancedata

<details>

* Version: 1.0.1
* GitHub: NA
* Source code: https://github.com/cran/glancedata
* Date/Publication: 2019-11-22 23:10:05 UTC
* Number of recursive dependencies: 109

Run `revdepcheck::cloud_details(, "glancedata")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(glancedata)
      > 
      > test_check("glancedata")
      [ FAIL 5 | WARN 4 | SKIP 0 | PASS 13 ]
      
    ...
      `expected`: TRUE 
      ── Failure ('test-plot_numerical_vars.R:37:3'): qqplot is a list of length 9 ───
      mymode == "list" & mylength == 9 is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      
      [ FAIL 5 | WARN 4 | SKIP 0 | PASS 13 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# gMCPLite

<details>

* Version: 0.1.4
* GitHub: https://github.com/Merck/gMCPLite
* Source code: https://github.com/cran/gMCPLite
* Date/Publication: 2023-11-08 05:10:02 UTC
* Number of recursive dependencies: 112

Run `revdepcheck::cloud_details(, "gMCPLite")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gMCPLite-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: hGraph
    > ### Title: Create multiplicity graphs using ggplot2
    > ### Aliases: hGraph
    > 
    > ### ** Examples
    > 
    > # Use Cairo PDF device for better Unicode character support
    ...
    > # Adjust box width
    > # add legend in middle of plot
    > hGraph(3,x=sqrt(0:2),y=c(1,3,1.5),size=6,halfWid=.3,halfHgt=.3, trhw=0.6,
    +        palette=cbPalette[2:4], fill = c(1, 2, 2),
    +        legend.position = c(.6,.5), legend.name = "Legend:", labels = c("Group 1", "Group 2"),
    +        nameHypotheses=c("H1:\n Long name","H2:\n Longer name","H3:\n Longest name"))
    Error in as.vector(x, "character") : 
      cannot coerce type 'environment' to vector of type 'character'
    Calls: <Anonymous> ... validDetails.text -> as.character -> as.character.default
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘GraphicalMultiplicity.Rmd’ using rmarkdown
    --- finished re-building ‘GraphicalMultiplicity.Rmd’
    
    --- re-building ‘hGraph.Rmd’ using rmarkdown
    
    Quitting from lines 162-170 [unnamed-chunk-7] (hGraph.Rmd)
    Error: processing vignette 'hGraph.Rmd' failed with diagnostics:
    cannot coerce type 'environment' to vector of type 'character'
    --- failed re-building ‘hGraph.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘hGraph.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# gosset

<details>

* Version: 1.0
* GitHub: https://github.com/agrdatasci/gosset
* Source code: https://github.com/cran/gosset
* Date/Publication: 2023-04-21 17:02:32 UTC
* Number of recursive dependencies: 159

Run `revdepcheck::cloud_details(, "gosset")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Overview.Rmd’ using rmarkdown_notangle
    
    Quitting from lines 188-189 [node_info2] (Overview.Rmd)
    Error: processing vignette 'Overview.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘Overview.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Overview.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# grafify

<details>

* Version: 4.0
* GitHub: https://github.com/ashenoy-cmbi/grafify
* Source code: https://github.com/cran/grafify
* Date/Publication: 2023-10-07 11:10:02 UTC
* Number of recursive dependencies: 107

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
      ── Error ('test-scale_colour_grafify.R:20:3'): Check colour and fill scales ────
      Error in `expect_match(db1$scales$scales[[1]]$scale_name, "muted")`: is.character(act$val) is not TRUE
      Backtrace:
          ▆
       1. └─testthat::expect_match(db1$scales$scales[[1]]$scale_name, "muted") at test-scale_colour_grafify.R:20:3
       2.   └─base::stopifnot(is.character(act$val))
      
      [ FAIL 21 | WARN 22 | SKIP 0 | PASS 225 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        help   5.4Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘gratia’
    ```

# gratia

<details>

* Version: 0.8.1
* GitHub: https://github.com/gavinsimpson/gratia
* Source code: https://github.com/cran/gratia
* Date/Publication: 2023-02-02 16:50:10 UTC
* Number of recursive dependencies: 117

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
    > ## simulate some data...
    > dat <- data_sim("eg1", n = 400, dist = "normal", scale = 2, seed = 2)
    > mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)
    > ## run some basic model checks
    > appraise(mod, point_col = "steelblue", point_alpha = 0.4)
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘custom-plotting.Rmd’ using rmarkdown
    
    Quitting from lines 51-53 [draw-eg1-1] (custom-plotting.Rmd)
    Error: processing vignette 'custom-plotting.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘custom-plotting.Rmd’
    
    --- re-building ‘data-slices.Rmd’ using rmarkdown
    
    ...
    Quitting from lines 51-52 [draw-gam] (gratia.Rmd)
    Error: processing vignette 'gratia.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘gratia.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘custom-plotting.Rmd’ ‘data-slices.Rmd’ ‘gratia.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking tests ... ERROR
    ```
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Complete output:
      > ## Test `gratia` using the `testthat` package
      > 
      > ## Setup
      > library("testthat")
      > 
      > ## Runs the tests in tests/testthat
      > test_check("gratia")
    ...
       18. │                           └─methods (local) `<rfMthdDf>`(...)
       19. │                             └─methods::new(def, ...)
       20. │                               ├─methods::initialize(value, ...)
       21. │                               └─methods::initialize(value, ...)
       22. │                                 └─.Object$initialize(...)
       23. │                                   └─lme4 (local) initializePtr()
       24. └─base::.handleSimpleError(...)
       25.   └─testthat (local) h(simpleError(msg, call))
       26.     └─rlang::abort(...)
      Execution halted
    ```

# Greymodels

<details>

* Version: 2.0.1
* GitHub: https://github.com/havishaJ/Greymodels
* Source code: https://github.com/cran/Greymodels
* Date/Publication: 2022-12-05 12:42:35 UTC
* Number of recursive dependencies: 92

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
    Error in train(..., self = self) : 
      unused argument (list("Number of observation", "Data Forecast & Prediction", "EPGM (1, 1) model", "colour"))
    Calls: ggplotly ... ggplotly.ggplot -> gg2list -> get_gdefs_ggproto -> <Anonymous>
    Execution halted
    ```

# gsDesign

<details>

* Version: 3.6.0
* GitHub: https://github.com/keaven/gsDesign
* Source code: https://github.com/cran/gsDesign
* Date/Publication: 2023-11-12 05:43:19 UTC
* Number of recursive dependencies: 105

Run `revdepcheck::cloud_details(, "gsDesign")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘gsDesign_independent_code.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(gsDesign)
      > 
      > test_check("gsDesign")
      Linear spending function with none = [ FAIL 1 | WARN 19 | SKIP 112 | PASS 1439 ]
      
    ...
       24.                                   ├─grid:::validGrob(g)
       25.                                   └─grid:::validGrob.grob(g)
       26.                                     ├─grid::validDetails(x)
       27.                                     └─grid:::validDetails.text(x)
       28.                                       ├─base::as.character(x$label)
       29.                                       └─base::as.character.default(x$label)
      
      [ FAIL 1 | WARN 19 | SKIP 112 | PASS 1439 ]
      Error: Test failures
      Execution halted
    ```

# GseaVis

<details>

* Version: 0.0.5
* GitHub: https://github.com/junjunlab/GseaVis
* Source code: https://github.com/cran/GseaVis
* Date/Publication: 2022-12-20 19:40:07 UTC
* Number of recursive dependencies: 103

Run `revdepcheck::cloud_details(, "GseaVis")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘GseaVis-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gseaNb
    > ### Title: gseaNb
    > ### Aliases: gseaNb
    > 
    > ### ** Examples
    > 
    > # load data
    ...
    > gseaRes <- readRDS(test_data)
    > 
    > # all plot
    > gseaNb(object = gseaRes,
    +       geneSetID = 'GOBP_NUCLEOSIDE_DIPHOSPHATE_METABOLIC_PROCESS',
    +       subPlot = 2)
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
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

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘An_Intro_to_HaploCatcher.Rmd’ using rmarkdown
    
    Quitting from lines 242-253 [example_models_1] (An_Intro_to_HaploCatcher.Rmd)
    Error: processing vignette 'An_Intro_to_HaploCatcher.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘An_Intro_to_HaploCatcher.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘An_Intro_to_HaploCatcher.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# harmony

<details>

* Version: 1.2.0
* GitHub: NA
* Source code: https://github.com/cran/harmony
* Date/Publication: 2023-11-29 08:30:04 UTC
* Number of recursive dependencies: 214

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
      Please specify either 'title' or 'pagetitle' in the metadata,
      e.g. by using --metadata pagetitle="..." on the command line.
      Falling back to 'quickstart.knit'
    --- finished re-building ‘quickstart.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘Seurat.Rmd’ ‘detailedWalkthrough.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 22.5Mb
      sub-directories of 1Mb or more:
        data   6.8Mb
        doc    2.6Mb
        libs  12.8Mb
    ```

# healthyR.ts

<details>

* Version: 0.3.0
* GitHub: https://github.com/spsanderson/healthyR.ts
* Source code: https://github.com/cran/healthyR.ts
* Date/Publication: 2023-11-15 06:00:05 UTC
* Number of recursive dependencies: 217

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
    Error in train(..., self = self) : 
      unused argument (list("Time", "Measurement", "Harmonic 3 Plot", "harmonic"))
    Calls: tidy_fft ... ggplotly.ggplot -> gg2list -> get_gdefs_ggproto -> <Anonymous>
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘getting-started.Rmd’ using rmarkdown
    --- finished re-building ‘getting-started.Rmd’
    
    --- re-building ‘using-tidy-fft.Rmd’ using rmarkdown
    
    Quitting from lines 109-117 [run_func] (using-tidy-fft.Rmd)
    Error: processing vignette 'using-tidy-fft.Rmd' failed with diagnostics:
    unused argument (list("Time", "Measurement", "Harmonic 8 Plot", "harmonic"))
    --- failed re-building ‘using-tidy-fft.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘using-tidy-fft.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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
* Number of recursive dependencies: 112

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
        6. ├─heatmaply:::predict_colors(ggplotly(g), plot_method = "ggplot")
        7. ├─plotly::ggplotly(g)
        8. └─plotly:::ggplotly.ggplot(g)
        9.   └─plotly::gg2list(...)
       10.     └─plotly:::get_gdefs_ggproto(npscales$scales, theme, plot, layers)
       11.       └─guides$train(scales, theme$legend.direction, plot$labels)
      
      [ FAIL 58 | WARN 0 | SKIP 0 | PASS 193 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘heatmaply.Rmd’ using rmarkdown
    
    Quitting from lines 109-111 [unnamed-chunk-5] (heatmaply.Rmd)
    Error: processing vignette 'heatmaply.Rmd' failed with diagnostics:
    unused argument (list(NULL, "column", "row", "text"))
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

# hermiter

<details>

* Version: 2.3.0
* GitHub: https://github.com/MikeJaredS/hermiter
* Source code: https://github.com/cran/hermiter
* Date/Publication: 2023-05-08 16:20:03 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "hermiter")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘hermiter.Rmd’ using rmarkdown
    
    Quitting from lines 396-409 [unnamed-chunk-24] (hermiter.Rmd)
    Error: processing vignette 'hermiter.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘hermiter.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘hermiter.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        R      2.3Mb
        doc    1.6Mb
        libs   2.5Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# hidecan

<details>

* Version: 1.1.0
* GitHub: https://github.com/PlantandFoodResearch/hidecan
* Source code: https://github.com/cran/hidecan
* Date/Publication: 2023-02-10 09:40:02 UTC
* Number of recursive dependencies: 91

Run `revdepcheck::cloud_details(, "hidecan")` for more info

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
       11.         ├─ggplot2::ggplot_add(object, p, objectname)
       12.         └─ggnewscale:::ggplot_add.new_aes(object, p, objectname)
       13.           └─ggnewscale:::bump_aes_scales(plot$scales$scales, new_aes = object)
       14.             └─base::lapply(scales, bump_aes_scale, new_aes = new_aes)
       15.               └─ggnewscale (local) FUN(X[[i]], ...)
       16.                 └─ggnewscale:::isTRUE(scale$guide == "none")
      
      [ FAIL 4 | WARN 0 | SKIP 1 | PASS 89 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘hidecan-step-by-step.Rmd’ using rmarkdown
    
    Quitting from lines 168-174 [create-hidecan-plot] (hidecan-step-by-step.Rmd)
    Error: processing vignette 'hidecan-step-by-step.Rmd' failed with diagnostics:
    comparison (==) is possible only for atomic and list types
    --- failed re-building ‘hidecan-step-by-step.Rmd’
    
    --- re-building ‘hidecan.Rmd’ using rmarkdown
    ...
    Quitting from lines 97-105 [hidecan-plot] (hidecan.Rmd)
    Error: processing vignette 'hidecan.Rmd' failed with diagnostics:
    comparison (==) is possible only for atomic and list types
    --- failed re-building ‘hidecan.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘hidecan-step-by-step.Rmd’ ‘hidecan.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# hmer

<details>

* Version: 1.5.6
* GitHub: https://github.com/andy-iskauskas/hmer
* Source code: https://github.com/cran/hmer
* Date/Publication: 2023-08-30 17:10:06 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::cloud_details(, "hmer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘hmer-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: validation_pairs
    > ### Title: Validation Set Diagnostics and Implausibility
    > ### Aliases: validation_pairs
    > 
    > ### ** Examples
    > 
    >  validation_pairs(SIREmulators$ems, SIRSample$validation, SIREmulators$targets)
    Error in pmg$grobs[[legend_layout$grob_pos]] <- legend_obj : 
      attempt to select more than one element in integerOneIndex
    Calls: <Anonymous> -> print.ggmatrix -> ggmatrix_gtable
    Execution halted
    ```

# HVT

<details>

* Version: 23.11.1
* GitHub: https://github.com/Mu-Sigma/HVT
* Source code: https://github.com/cran/HVT
* Date/Publication: 2023-11-19 15:20:12 UTC
* Number of recursive dependencies: 182

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
    Error in train(..., self = self) : 
      unused argument (list("Hierarchical Voronoi Tessellation for Level 1", "Level", "x", "y", "factor(depth)", "interaction(depth, cluster, child)", "n", "hoverText"))
    Calls: HVT ... ggplotly.ggplot -> gg2list -> get_gdefs_ggproto -> <Anonymous>
    Execution halted
    ```

# idopNetwork

<details>

* Version: 0.1.2
* GitHub: https://github.com/cxzdsa2332/idopNetwork
* Source code: https://github.com/cran/idopNetwork
* Date/Publication: 2023-04-18 06:50:02 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::cloud_details(, "idopNetwork")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘idopNetwork_vignette.Rmd’ using rmarkdown
    
    Quitting from lines 287-288 [unnamed-chunk-32] (idopNetwork_vignette.Rmd)
    Error: processing vignette 'idopNetwork_vignette.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘idopNetwork_vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘idopNetwork_vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# iglu

<details>

* Version: 3.5.0
* GitHub: NA
* Source code: https://github.com/cran/iglu
* Date/Publication: 2023-10-20 15:20:02 UTC
* Number of recursive dependencies: 125

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
    > data(example_data_1_subject)
    > agp(example_data_1_subject, daily = FALSE)
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘AGP_and_Episodes.Rmd’ using rmarkdown
    
    Quitting from lines 24-25 [unnamed-chunk-1] (AGP_and_Episodes.Rmd)
    Error: processing vignette 'AGP_and_Episodes.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘AGP_and_Episodes.Rmd’
    
    --- re-building ‘MAGE.Rmd’ using rmarkdown
    
    ...
    --- finished re-building ‘lasagna_plots.Rmd’
    
    --- re-building ‘metrics_list.Rmd’ using rmarkdown
    --- finished re-building ‘metrics_list.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘AGP_and_Episodes.Rmd’ ‘MAGE.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# iml

<details>

* Version: 0.11.1
* GitHub: https://github.com/christophM/iml
* Source code: https://github.com/cran/iml
* Date/Publication: 2022-09-08 09:52:58 UTC
* Number of recursive dependencies: 179

Run `revdepcheck::cloud_details(, "iml")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘iml-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: FeatureEffects
    > ### Title: Effect of a feature on predictions
    > ### Aliases: FeatureEffects
    > 
    > ### ** Examples
    > 
    > # We train a random forest on the Boston dataset:
    ...
    > rf <- rpart(medv ~ ., data = Boston)
    > mod <- Predictor$new(rf, data = Boston)
    > 
    > # Compute the accumulated local effects for all features
    > eff <- FeatureEffects$new(mod)
    > eff$plot()
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘intro.Rmd’ using rmarkdown
    
    Quitting from lines 127-129 [unnamed-chunk-10] (intro.Rmd)
    Error: processing vignette 'intro.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘intro.Rmd’
    
    --- re-building ‘parallel.Rmd’ using rmarkdown
    --- finished re-building ‘parallel.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘intro.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# incidence

<details>

* Version: 1.7.3
* GitHub: https://github.com/reconhub/incidence
* Source code: https://github.com/cran/incidence
* Date/Publication: 2020-11-04 16:30:07 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "incidence")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘incidence-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: fit
    > ### Title: Fit exponential models to incidence data
    > ### Aliases: fit fit_optim_split print.incidence_fit
    > ###   print.incidence_fit_list
    > 
    > ### ** Examples
    > 
    ...
     19.                               └─ggplot2 (local) extract_key(...)
     20.                                 └─Guide$extract_key(scale, aesthetic, ...)
     21.                                   └─ggplot2 (local) extract_key(...)
     22.                                     └─scale$get_labels(breaks)
     23.                                       └─ggplot2 (local) get_labels(..., self = self)
     24.                                         └─self$scale$get_labels(breaks)
     25.                                           └─ggplot2 (local) get_labels(..., self = self)
     26.                                             └─cli::cli_abort(...)
     27.                                               └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘conversions.Rmd’ using rmarkdown
    --- finished re-building ‘conversions.Rmd’
    
    --- re-building ‘customize_plot.Rmd’ using rmarkdown
    
    Quitting from lines 78-81 [default] (customize_plot.Rmd)
    Error: processing vignette 'customize_plot.Rmd' failed with diagnostics:
    `breaks` and `labels` have different lengths.
    --- failed re-building ‘customize_plot.Rmd’
    ...
    Quitting from lines 210-213 [i7outcome] (overview.Rmd)
    Error: processing vignette 'overview.Rmd' failed with diagnostics:
    `breaks` and `labels` have different lengths.
    --- failed re-building ‘overview.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘customize_plot.Rmd’ ‘overview.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# infer

<details>

* Version: 1.0.5
* GitHub: https://github.com/tidymodels/infer
* Source code: https://github.com/cran/infer
* Date/Publication: 2023-09-06 02:20:02 UTC
* Number of recursive dependencies: 127

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
      • visualize/viz-assume-z-p-val-left.svg
      • visualize/viz-assume-z-p-val-right.svg
      • visualize/viz-assume-z.svg
      • visualize/viz-fit-conf-int.svg
      • visualize/viz-fit-no-h0.svg
      • visualize/viz-fit-p-val-both.svg
      • visualize/viz-fit-p-val-left.svg
      • visualize/viz-fit-p-val-right.svg
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘anova.Rmd’ using rmarkdown
    Response: age (numeric)
    Explanatory: partyid (factor)
    Null Hypothesis: independence
    # A tibble: 1 × 1
       stat
      <dbl>
    1  2.48
    Response: age (numeric)
    ...
      p_value
        <dbl>
    1     0.3
    --- finished re-building ‘t_test.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘infer.Rmd’ ‘observed_stat_examples.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# inferCSN

<details>

* Version: 0.99.8
* GitHub: https://github.com/mengxu98/inferCSN
* Source code: https://github.com/cran/inferCSN
* Date/Publication: 2023-12-04 05:00:02 UTC
* Number of recursive dependencies: 100

Run `revdepcheck::cloud_details(, "inferCSN")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘inferCSN-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: auc.calculate
    > ### Title: AUC value calculate
    > ### Aliases: auc.calculate
    > 
    > ### ** Examples
    > 
    > library(inferCSN)
    > data("exampleMatrix")
    > data("exampleGroundTruth")
    > weightDT <- inferCSN(exampleMatrix)
    > auc <- auc.calculate(weightDT, exampleGroundTruth, plot = TRUE)
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: auc.calculate ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 35.6Mb
      sub-directories of 1Mb or more:
        libs  34.6Mb
    ```

# insurancerating

<details>

* Version: 0.7.2
* GitHub: https://github.com/mharinga/insurancerating
* Source code: https://github.com/cran/insurancerating
* Date/Publication: 2022-12-20 15:30:02 UTC
* Number of recursive dependencies: 134

Run `revdepcheck::cloud_details(, "insurancerating")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘insurancerating-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: autoplot.riskfactor
    > ### Title: Automatically create a ggplot for objects obtained from
    > ###   rating_factors()
    > ### Aliases: autoplot.riskfactor
    > 
    > ### ** Examples
    > 
    ...
    > mod2 <- glm(nclaims ~ area, offset = log(exposure), family = poisson(),
    +  data = df)
    > 
    > x <- rating_factors(mod1, mod2, model_data = df, exposure = exposure)
    Significance levels: *** p < 0.001; ** p < 0.01;
        * p < 0.05; . p < 0.1> autoplot(x)
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

# interpretCI

<details>

* Version: 0.1.1
* GitHub: https://github.com/cardiomoon/interpretCI
* Source code: https://github.com/cran/interpretCI
* Date/Publication: 2022-01-28 08:50:02 UTC
* Number of recursive dependencies: 114

Run `revdepcheck::cloud_details(, "interpretCI")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘interpretCI-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.meanCI
    > ### Title: S3 method for an object of class "meanCI"
    > ### Aliases: plot.meanCI
    > 
    > ### ** Examples
    > 
    > meanCI(mtcars,mpg) %>% plot()
    > meanCI(mtcars,am,mpg) %>% plot()
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Confidence_interval_for_a_mean.Rmd’ using rmarkdown
    --- finished re-building ‘Confidence_interval_for_a_mean.Rmd’
    
    --- re-building ‘Confidence_interval_for_a_proportion.Rmd’ using rmarkdown
    --- finished re-building ‘Confidence_interval_for_a_proportion.Rmd’
    
    --- re-building ‘Confidence_interval_for_paired_mean_difference.Rmd’ using rmarkdown
    
    Quitting from lines 160-161 [unnamed-chunk-12] (Confidence_interval_for_paired_mean_difference.Rmd)
    ...
    --- failed re-building ‘Package_interpretCI.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘Confidence_interval_for_paired_mean_difference.Rmd’
      ‘Confidence_interval_for_unpaired_mean_difference.Rmd’
      ‘Hypothesis_test_Paired_Mean_Diff.Rmd’
      ‘Hypothesis_test_Unpaired_Mean_diff.Rmd’ ‘Package_interpretCI.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘moonBook’
      All declared Imports should be used.
    ```

# inTextSummaryTable

<details>

* Version: 3.3.1
* GitHub: https://github.com/openanalytics/inTextSummaryTable
* Source code: https://github.com/cran/inTextSummaryTable
* Date/Publication: 2023-09-12 10:20:02 UTC
* Number of recursive dependencies: 121

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
      [ FAIL 2 | WARN 167 | SKIP 0 | PASS 991 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test_subjectProfileSummaryPlot-general.R:25:2'): The plot is correctly facetted based on a variable ──
      `... <- NULL` produced warnings.
      ── Failure ('test_subjectProfileSummaryPlot-table.R:356:2'): The size of the points (in the legend) is correctly set ──
      gg$guides$colour$override.aes$size not equal to `pointSize`.
      target is NULL, current is numeric
      
      [ FAIL 2 | WARN 167 | SKIP 0 | PASS 991 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.9Mb
      sub-directories of 1Mb or more:
        doc   9.9Mb
    ```

# intradayModel

<details>

* Version: 0.0.1
* GitHub: https://github.com/convexfi/intradayModel
* Source code: https://github.com/cran/intradayModel
* Date/Publication: 2023-05-22 08:30:02 UTC
* Number of recursive dependencies: 122

Run `revdepcheck::cloud_details(, "intradayModel")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘intradayModel-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: generate_plots
    > ### Title: Plot Analysis and Forecast Result
    > ### Aliases: generate_plots
    > 
    > ### ** Examples
    > 
    > library(intradayModel)
    ...
    > analysis_result <- decompose_volume(purpose = "analysis", model_fit, volume_aapl_training)
    > forecast_result <- forecast_volume(model_fit, volume_aapl_testing)
    > 
    > # plot the analysis and forecast result
    > generate_plots(analysis_result)
    $components
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘intradayModel.Rmd’ using rmarkdown
    Warning in hook_png(before, options, envir, "pngquant", function(x) { :
      cannot find pngquant; please install and put it in PATH
    Warning in hook_png(before, options, envir, "pngquant", function(x) { :
      cannot find pngquant; please install and put it in PATH
    Warning in hook_png(before, options, envir, "pngquant", function(x) { :
      cannot find pngquant; please install and put it in PATH
    
    ...
    Quitting from lines 82-87 [unnamed-chunk-5] (intradayModel.Rmd)
    Error: processing vignette 'intradayModel.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘intradayModel.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘intradayModel.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# inventorize

<details>

* Version: 1.1.1
* GitHub: NA
* Source code: https://github.com/cran/inventorize
* Date/Publication: 2022-05-31 22:20:09 UTC
* Number of recursive dependencies: 72

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
Error in train(..., self = self) : 
  unused argument (list("Max  Policy Dynamic", "period", "demand", "colour"))
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
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (inventorize)


```
# iNZightRegression

<details>

* Version: 1.3.3
* GitHub: https://github.com/iNZightVIT/iNZightRegression
* Source code: https://github.com/cran/iNZightRegression
* Date/Publication: 2023-01-26 23:00:02 UTC
* Number of recursive dependencies: 153

Run `revdepcheck::cloud_details(, "iNZightRegression")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘iNZightRegression-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: inzplot
    > ### Title: inzplot method
    > ### Aliases: inzplot inzplot.glm inzplot.lm
    > 
    > ### ** Examples
    > 
    > iris_fit <- lm(Sepal.Width ~ Sepal.Length, data = iris)
    > inzplot(iris_fit)
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: inzplot -> inzplot.lm
    Execution halted
    ```

# iNZightTS

<details>

* Version: 1.5.9
* GitHub: https://github.com/iNZightVIT/iNZightTS
* Source code: https://github.com/cran/iNZightTS
* Date/Publication: 2022-01-31 21:50:02 UTC
* Number of recursive dependencies: 76

Run `revdepcheck::cloud_details(, "iNZightTS")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘iNZightTS-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: decompose
    > ### Title: Decompose a time series object
    > ### Aliases: decompose plot.inzdecomp
    > 
    > ### ** Examples
    > 
    > t <- iNZightTS(visitorsQ)
    > decomp.ts <- decompose(t, data.name = "Visitors")
    > plot(decomp.ts)
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: plot ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(iNZightTS)
      
      Attaching package: 'iNZightTS'
      
      The following object is masked from 'package:stats':
      
    ...
        6.       └─base::lapply(x$plots, plot_table, guides = guides)
        7.         ├─patchwork (local) FUN(X[[i]], ...)
        8.         └─patchwork:::plot_table.ggplot(X[[i]], ...)
        9.           └─patchwork:::add_guides(gt, guides == "collect")
       10.             ├─base::unlist(guide_loc == panel_loc)
       11.             └─base::Ops.data.frame(guide_loc, panel_loc)
      
      [ FAIL 6 | WARN 2 | SKIP 1 | PASS 63 ]
      Error: Test failures
      Execution halted
    ```

# irt

<details>

* Version: 0.2.7
* GitHub: https://github.com/egonulates/irt
* Source code: https://github.com/cran/irt
* Date/Publication: 2022-11-09 21:50:10 UTC
* Number of recursive dependencies: 55

Run `revdepcheck::cloud_details(, "irt")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘irt-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_empirical_icc2
    > ### Title: Plot Empirical Item Characteristic Curve
    > ### Aliases: plot_empirical_icc2
    > 
    > ### ** Examples
    > 
    > ip <- generate_ip(model = c("3PL", "GRM"), n = 20)
    ...
    Error in `plot_empirical_icc2()`:
    ! Cannot add <ggproto> objects together.
    ℹ Did you forget to add this object to a <ggplot> object?
    Backtrace:
        ▆
     1. └─irt::plot_empirical_icc2(resp = resp, item = "Item_5")
     2.   └─ggplot2:::`+.gg`(...)
     3.     └─cli::cli_abort(...)
     4.       └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking C++ specification ... NOTE
    ```
      Specified C++11: please drop specification unless essential
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 27.7Mb
      sub-directories of 1Mb or more:
        libs  25.9Mb
    ```

*   checking Rd \usage sections ... NOTE
    ```
    S3 methods shown with full name in documentation object 'print.Item':
      ‘print.Item’
    
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

# itsdm

<details>

* Version: 0.2.1
* GitHub: https://github.com/LLeiSong/itsdm
* Source code: https://github.com/cran/itsdm
* Date/Publication: 2023-06-11 00:00:02 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::cloud_details(, "itsdm")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘itsdm-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: convert_to_pa
    > ### Title: Convert predicted suitability to presence-absence map.
    > ### Aliases: convert_to_pa
    > 
    > ### ** Examples
    > 
    > # Using a pseudo presence-only occurrence dataset of
    ...
    > pa_thred <- convert_to_pa(mod$prediction,
    +                           method = 'threshold', beta = 0.5, visualize = FALSE)
    > pa_thred
    Threshold conversion
    species prevalence = 0.489942528735632
    > plot(pa_thred)
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

# ivDiag

<details>

* Version: 1.0.6
* GitHub: NA
* Source code: https://github.com/cran/ivDiag
* Date/Publication: 2023-09-17 06:00:02 UTC
* Number of recursive dependencies: 90

Run `revdepcheck::cloud_details(, "ivDiag")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ivDiag-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ltz
    > ### Title: Local-to-Zero Test
    > ### Aliases: ltz
    > 
    > ### ** Examples
    > 
    > data(ivDiag)
    ...
    Scale for x is already present.
    Adding another scale for x, which will replace the existing scale.
    Scale for x is already present.
    Adding another scale for x, which will replace the existing scale.
    Scale for x is already present.
    Adding another scale for x, which will replace the existing scale.
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

# lares

<details>

* Version: 5.2.4
* GitHub: https://github.com/laresbernardo/lares
* Source code: https://github.com/cran/lares
* Date/Publication: 2023-12-07 18:10:02 UTC
* Number of recursive dependencies: 99

Run `revdepcheck::cloud_details(, "lares")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘lares-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: clusterVisualK
    > ### Title: Visualize K-Means Clusters for Several K
    > ### Aliases: clusterVisualK
    > 
    > ### ** Examples
    > 
    > Sys.unsetenv("LARES_FONT") # Temporal
    ...
    > df <- subset(iris, select = c(-Species))
    > df <- df[sample(nrow(df)), ]
    > 
    > # Calculate and plot
    > result <- clusterVisualK(df, ks = 2:4)
    > plot(result$plot)
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: plot ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

# latentcor

<details>

* Version: 2.0.1
* GitHub: NA
* Source code: https://github.com/cran/latentcor
* Date/Publication: 2022-09-05 20:50:02 UTC
* Number of recursive dependencies: 144

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
      0.018   0.000   0.018 
    > # Heatmap for latent correlation matrix.
    > Heatmap_R_approx = latentcor(X = X, types = "tru", method = "approx",
    +                              showplot = TRUE)$plotR
    Error in train(..., self = self) : 
      unused argument (list(NULL, "Vertical axis:", "Horizontal axis:", "text", "yintercept", "xintercept"))
    Calls: latentcor ... ggplotly.ggplot -> gg2list -> get_gdefs_ggproto -> <Anonymous>
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.3Mb
      sub-directories of 1Mb or more:
        R   6.9Mb
    ```

# ldsr

<details>

* Version: 0.0.2
* GitHub: https://github.com/ntthung/ldsr
* Source code: https://github.com/cran/ldsr
* Date/Publication: 2020-05-04 14:40:09 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "ldsr")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ldsr.Rmd’ using rmarkdown
    
    Quitting from lines 86-105 [unnamed-chunk-6] (ldsr.Rmd)
    Error: processing vignette 'ldsr.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘ldsr.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ldsr.Rmd’
    
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
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        libs   5.9Mb
    ```

# lemon

<details>

* Version: 0.4.7
* GitHub: https://github.com/stefanedwards/lemon
* Source code: https://github.com/cran/lemon
* Date/Publication: 2023-11-07 12:00:02 UTC
* Number of recursive dependencies: 77

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
     13. │               └─ggplot2 (local) bottom(panel_params$guides, position = "bottom", theme = theme)
     14. │                 └─lemon (local) bottom(...)
     15. │                   └─lemon:::panel_guides_grob(guides, position, theme)
     16. │                     └─pair$guide$draw(theme, pair$params)
     17. │                       └─ggplot2 (local) draw(..., self = self)
     18. │                         └─self$setup_params(params)
     19. │                           └─ggplot2 (local) setup_params(...)
     20. │                             └─rlang::arg_match0(params$position, .trbl)
     21. └─rlang::abort(message = message, call = call)
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
       25. │                                         └─ggplot2 (local) setup_params(...)
       26. │                                           └─rlang::arg_match0(params$position, .trbl)
       27. └─rlang::abort(message = message, call = call)
      
      [ FAIL 3 | WARN 0 | SKIP 1 | PASS 138 ]
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
    
    Quitting from lines 61-62 [unnamed-chunk-1] (capped-axes.Rmd)
    Error: processing vignette 'capped-axes.Rmd' failed with diagnostics:
    `params$position` must be a string or character vector.
    --- failed re-building ‘capped-axes.Rmd’
    
    --- re-building ‘facet-rep-labels.Rmd’ using rmarkdown
    
    ...
    
        ```{r %s}
    '
    --- finished re-building ‘lemon_print.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘capped-axes.Rmd’ ‘facet-rep-labels.Rmd’ ‘legends.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# LMD

<details>

* Version: 1.0.0
* GitHub: https://github.com/shubhra-opensource/LMD
* Source code: https://github.com/cran/LMD
* Date/Publication: 2022-09-20 09:56:07 UTC
* Number of recursive dependencies: 95

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
    > x=1:100
    > y = (2 / 3 )* sin(x * 30) + (2 / 3) * sin(x * 17.5) + (4 / 5) *cos(x * 2)
    > plot_lmd(lmd(y))
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Getting_Started_with_LMD.Rmd’ using rmarkdown
    
    Quitting from lines 350-397 [unnamed-chunk-8] (Getting_Started_with_LMD.Rmd)
    Error: processing vignette 'Getting_Started_with_LMD.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘Getting_Started_with_LMD.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Getting_Started_with_LMD.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# lmls

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/lmls
* Date/Publication: 2022-01-18 08:32:47 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "lmls")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘lmls.Rmd’ using rmarkdown
    
    Quitting from lines 291-305 [abdom-mcmc-3] (lmls.Rmd)
    Error: processing vignette 'lmls.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘lmls.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘lmls.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## Newly fixed

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘lmls.Rmd’ using rmarkdown
    
    tlmgr: Remote database (rev 69178) seems to be older than local (rev 69180 of texlive-scripts); please use different mirror or  wait a day or so.
    Warning in system2("tlmgr", args, ...) :
      running command ''tlmgr' search --file --global '/grffile.sty'' had status 1
    ! LaTeX Error: File `grffile.sty' not found.
    
    ! Emergency stop.
    ...
    
    Error: processing vignette 'lmls.Rmd' failed with diagnostics:
    LaTeX failed to compile /tmp/workdir/lmls/old/lmls.Rcheck/vign_test/lmls/vignettes/lmls.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See lmls.log for more info.
    --- failed re-building ‘lmls.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘lmls.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# LMoFit

<details>

* Version: 0.1.6
* GitHub: NA
* Source code: https://github.com/cran/LMoFit
* Date/Publication: 2020-11-26 11:10:02 UTC
* Number of recursive dependencies: 65

Run `revdepcheck::cloud_details(, "LMoFit")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘LMoFit.Rmd’ using rmarkdown
    
    Quitting from lines 236-237 [unnamed-chunk-15] (LMoFit.Rmd)
    Error: processing vignette 'LMoFit.Rmd' failed with diagnostics:
    Problem while computing aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `scales_add_defaults()`:
    ! could not find function "scales_add_defaults"
    --- failed re-building ‘LMoFit.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘LMoFit.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# LongDat

<details>

* Version: 1.1.2
* GitHub: https://github.com/CCY-dev/LongDat
* Source code: https://github.com/cran/LongDat
* Date/Publication: 2023-07-17 05:40:02 UTC
* Number of recursive dependencies: 152

Run `revdepcheck::cloud_details(, "LongDat")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
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
    '==' only defined for equally-sized data frames
    --- failed re-building ‘LongDat_disc_tutorial.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘LongDat_cont_tutorial.Rmd’ ‘LongDat_disc_tutorial.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# LSTS

<details>

* Version: 2.1
* GitHub: https://github.com/pachadotdev/LSTS
* Source code: https://github.com/cran/LSTS
* Date/Publication: 2021-07-29 16:00:02 UTC
* Number of recursive dependencies: 49

Run `revdepcheck::cloud_details(, "LSTS")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘LSTS-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ts.diag
    > ### Title: Diagnostic Plots for Time Series fits
    > ### Aliases: ts.diag
    > 
    > ### ** Examples
    > 
    > ts.diag(malleco)
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Rdpack’
      All declared Imports should be used.
    ```

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) LS.kalman.Rd:65: Escaped LaTeX specials: \&
    checkRd: (-1) spectral.density.Rd:55: Escaped LaTeX specials: \&
    ```

# manydata

<details>

* Version: 0.8.3
* GitHub: https://github.com/globalgov/manydata
* Source code: https://github.com/cran/manydata
* Date/Publication: 2023-06-15 11:30:03 UTC
* Number of recursive dependencies: 134

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
      > 
      > test_check("manydata")
      There were 5 matched observations by manyID variable across datasets in database.There were 5 matched observations by manyID variable across datasets in database.There were 5 matched observations by manyID variable across datasets in database.There were 5 matched observations by manyID variable across datasets in database.There were 5 matched observations by manyID variable across datasets in database.There were 5 matched observations by manyID variable across datasets in database.There were 5 matched observations by manyID variable across datasets in database.There were 5 matched observations by manyID variable across datasets in database.There were 5 matched observations by manyID variable across datasets in database.There were 5 matched observations by manyID variable across datasets in database.There were 5 matched observations by manyID variable across datasets in database.There were 5 matched observations by manyID variable across datasets in database.There were 5 matched observations by manyID variable across datasets in database.There were 5 matched observations by manyID variable across datasets in database.There were 5 matched observations by manyID variable across datasets in database.There were 5 matched observations by manyID variable across datasets in database.There were 5 matched observations by manyID variable across datasets in database.There were 5 matched observations by manyID variable across datasets in database.There were 5 matched observations by manyID variable across datasets in database.There were 5 matched observations by manyID variable across datasets in database.There were 116 matched observations by ID variable across datasets in database.There were 116 matched observations by ID variable across datasets in database.There were 116 matched observations by ID variable across datasets in database.There were 116 matched observations by ID variable across datasets in database.[ FAIL 3 | WARN 4 | SKIP 0 | PASS 111 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      `db` has length 11, not length 9.
      ── Failure ('test_db.R:6:3'): dbplot() returns the correct output format ───────
      Names of `db` ('data', 'layers', 'scales', 'guides', 'mapping', 'theme', 'coordinates', 'facet', 'plot_env', 'layout', 'labels') don't match 'data', 'layers', 'scales', 'mapping', 'theme', 'coordinates', 'facet', 'plot_env', 'labels'
      ── Failure ('test_releases.R:8:3'): Plotting function visualises historical
                milestones/releases of a repository ──
      `testplot` has length 11, not length 9.
      
      [ FAIL 3 | WARN 4 | SKIP 0 | PASS 111 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 3 marked UTF-8 strings
    ```

# manynet

<details>

* Version: 0.3.0
* GitHub: https://github.com/snlab-ch/manynet
* Source code: https://github.com/cran/manynet
* Date/Publication: 2023-12-15 19:10:02 UTC
* Number of recursive dependencies: 140

Run `revdepcheck::cloud_details(, "manynet")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(manynet)
      Registered S3 method overwritten by 'manynet':
        method          from     
        print.tbl_graph tidygraph
      > 
      > test_check("manynet")
    ...
      ── Failure ('test-map_theme.R:61:3'): scales graph correctly ───────────────────
      test_uzh[["scales"]][["scales"]][[1]][["call"]][["scale_name"]] not equal to "UZH".
      target is NULL, current is character
      ── Failure ('test-map_theme.R:62:3'): scales graph correctly ───────────────────
      test_rug[["scales"]][["scales"]][[1]][["call"]][["scale_name"]] not equal to "RUG".
      target is NULL, current is character
      
      [ FAIL 6 | WARN 1 | SKIP 14 | PASS 324 ]
      Error: Test failures
      Execution halted
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

# mappoly

<details>

* Version: 0.3.3
* GitHub: https://github.com/mmollina/MAPpoly
* Source code: https://github.com/cran/mappoly
* Date/Publication: 2023-01-05 20:10:02 UTC
* Number of recursive dependencies: 195

Run `revdepcheck::cloud_details(, "mappoly")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > if(requireNamespace("testthat", quietly = TRUE)){
      +   library(testthat)
      +   library(mappoly)
      +   test_check("mappoly")
      + }
      =====================================================
      MAPpoly Package [Version 0.3.32023-01-05 20:10:02 UTC]
    ...
        5. └─mappoly:::plot.mappoly.homoprob(hom.t1)
        6.   ├─plotly::ggplotly(p)
        7.   └─plotly:::ggplotly.ggplot(p)
        8.     └─plotly::gg2list(...)
        9.       └─plotly:::get_gdefs_ggproto(npscales$scales, theme, plot, layers)
       10.         └─guides$train(scales, theme$legend.direction, plot$labels)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 122 ]
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
      installed size is 22.5Mb
      sub-directories of 1Mb or more:
        R      3.1Mb
        data   4.3Mb
        libs  13.0Mb
    ```

# marqLevAlg

<details>

* Version: 2.0.8
* GitHub: https://github.com/VivianePhilipps/marqLevAlgParallel
* Source code: https://github.com/cran/marqLevAlg
* Date/Publication: 2023-03-22 14:00:05 UTC
* Number of recursive dependencies: 61

Run `revdepcheck::cloud_details(, "marqLevAlg")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘mla.Rmd’ using rmarkdown
    
    Quitting from lines 367-460 [speedup] (mla.Rmd)
    Error: processing vignette 'mla.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘mla.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘mla.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# MASSExtra

<details>

* Version: 1.2.2
* GitHub: NA
* Source code: https://github.com/cran/MASSExtra
* Date/Publication: 2023-02-16 07:40:02 UTC
* Number of recursive dependencies: 58

Run `revdepcheck::cloud_details(, "MASSExtra")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘rationale.Rmd’ using rmarkdown
    
    Quitting from lines 118-124 [unnamed-chunk-1] (rationale.Rmd)
    Error: processing vignette 'rationale.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘rationale.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘rationale.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# mcp

<details>

* Version: 0.3.3
* GitHub: https://github.com/lindeloev/mcp
* Source code: https://github.com/cran/mcp
* Date/Publication: 2023-03-22 09:10:02 UTC
* Number of recursive dependencies: 101

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
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

# MedLEA

<details>

* Version: 1.0.2
* GitHub: NA
* Source code: https://github.com/cran/MedLEA
* Date/Publication: 2023-03-13 11:30:08 UTC
* Number of recursive dependencies: 67

Run `revdepcheck::cloud_details(, "MedLEA")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘MedLEA.Rmd’ using rmarkdown
    
    Quitting from lines 67-75 [example5] (MedLEA.Rmd)
    Error: processing vignette 'MedLEA.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘MedLEA.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘MedLEA.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# messi

<details>

* Version: 0.1.1
* GitHub: https://github.com/umich-cphds/messi
* Source code: https://github.com/cran/messi
* Date/Publication: 2023-07-13 14:00:04 UTC
* Number of recursive dependencies: 33

Run `revdepcheck::cloud_details(, "messi")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘messi-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: messi
    > ### Title: Implementation of Mediation with External Summary Statistics
    > ###   Information (MESSI) from Boss et al. (2023).
    > ### Aliases: messi
    > 
    > ### ** Examples
    > 
    ...
    47    0.204
    48    0.198
    49    0.115
    50    0.096
    
    $plot_alpha_a
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

# metaconfoundr

<details>

* Version: 0.1.2
* GitHub: https://github.com/malcolmbarrett/metaconfoundr
* Source code: https://github.com/cran/metaconfoundr
* Date/Publication: 2023-01-17 19:00:09 UTC
* Number of recursive dependencies: 121

Run `revdepcheck::cloud_details(, "metaconfoundr")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘intro-to-metaconfoundr.Rmd’ using rmarkdown
    
    Quitting from lines 185-193 [unnamed-chunk-13] (intro-to-metaconfoundr.Rmd)
    Error: processing vignette 'intro-to-metaconfoundr.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘intro-to-metaconfoundr.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘intro-to-metaconfoundr.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# metR

<details>

* Version: 0.14.1
* GitHub: https://github.com/eliocamp/metR
* Source code: https://github.com/cran/metR
* Date/Publication: 2023-10-30 20:50:03 UTC
* Number of recursive dependencies: 118

Run `revdepcheck::cloud_details(, "metR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘metR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: as.discretised_scale
    > ### Title: Create discretised versions of continuous scales
    > ### Aliases: as.discretised_scale scale_fill_discretised
    > ###   scale_fill_divergent_discretised discretised_scale ScaleDiscretised
    > ### Keywords: datasets
    > 
    > ### ** Examples
    ...
     21. │                     └─ggplot2 (local) transform_df(..., self = self)
     22. │                       └─base::lapply(df[aesthetics], self$transform)
     23. │                         └─ggplot2 (local) FUN(X[[i]], ...)
     24. │                           └─metR (local) transform(..., self = self)
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
      > library(testthat)
      > library(metR)
      > # library(vdiffr)
      > 
      > on_cran <- !isTRUE(as.logical(Sys.getenv("NOT_CRAN", "false")))
      > if (on_cran) data.table::setDTthreads(2)
      > 
    ...
      • vis-streamline/streamline-ywrapped.svg
      • vis-text-contour/labels-text.svg
      • vis-text-contour/minsize.svg
      • vis-text-contour/placement-fraction.svg
      • vis-text-contour/placement-minmax-horizontal.svg
      • vis-text-contour/placement-minmax-vertical.svg
      • vis-text-contour/placement-n.svg
      • vis-text-contour/text-contour-norotate.svg
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Visualization-tools.Rmd’ using rmarkdown
    
    Quitting from lines 291-295 [unnamed-chunk-20] (Visualization-tools.Rmd)
    Error: processing vignette 'Visualization-tools.Rmd' failed with diagnostics:
    attempt to apply non-function
    --- failed re-building ‘Visualization-tools.Rmd’
    
    --- re-building ‘Working-with-data.Rmd’ using rmarkdown
    --- finished re-building ‘Working-with-data.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Visualization-tools.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        data   1.8Mb
        doc    1.6Mb
    ```

# mFD

<details>

* Version: 1.0.6
* GitHub: https://github.com/CmlMagneville/mFD
* Source code: https://github.com/cran/mFD
* Date/Publication: 2023-11-02 09:30:02 UTC
* Number of recursive dependencies: 150

Run `revdepcheck::cloud_details(, "mFD")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mFD-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: alpha.fd.fe.plot
    > ### Title: Illustrate Functional Diversity indices based on Functional
    > ###   Entities
    > ### Aliases: alpha.fd.fe.plot
    > 
    > ### ** Examples
    > 
    ...
    +   size_line_fred    = 1.5,
    +   size_arrow_fvuln  = 1,
    +   check_input       = TRUE) 
    Warning in ggplot2::geom_segment(ggplot2::aes(x = fe_vuln_k[1], y = 0.5,  :
      All aesthetics have length 1, but the data has 8 rows.
    ℹ Did you mean to use `annotate()`?
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Compute_and_interpret_quality_of_functional_spaces.Rmd’ using rmarkdown
    
    Quitting from lines 273-277 [unnamed-chunk-9] (Compute_and_interpret_quality_of_functional_spaces.Rmd)
    Error: processing vignette 'Compute_and_interpret_quality_of_functional_spaces.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘Compute_and_interpret_quality_of_functional_spaces.Rmd’
    
    --- re-building ‘Compute_functional_hill_indices.Rmd’ using rmarkdown
    --- finished re-building ‘Compute_functional_hill_indices.Rmd’
    ...
    '==' only defined for equally-sized data frames
    --- failed re-building ‘mFD_general_workflow.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘Compute_and_interpret_quality_of_functional_spaces.Rmd’
      ‘Customised_plots.Rmd’ ‘How_to_deal_with_Functional_Entities.Rmd’
      ‘mFD_general_workflow.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# mfp2

<details>

* Version: 1.0.0
* GitHub: https://github.com/EdwinKipruto/mfp2
* Source code: https://github.com/cran/mfp2
* Date/Publication: 2023-11-14 18:00:03 UTC
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "mfp2")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘mfp2.Rmd’ using rmarkdown
    
    Quitting from lines 273-274 [fig1] (mfp2.Rmd)
    Error: processing vignette 'mfp2.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘mfp2.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘mfp2.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# MiMIR

<details>

* Version: 1.4
* GitHub: NA
* Source code: https://github.com/cran/MiMIR
* Date/Publication: 2022-05-23 11:30:02 UTC
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
    Error in train(..., self = self) : 
      unused argument (list("Clinical variables", "AUC", "AUC of the current dataset compared to the LOBOV", "Uploaded"))
    Calls: LOBOV_accuracies ... ggplotly.ggplot -> gg2list -> get_gdefs_ggproto -> <Anonymous>
    Execution halted
    ```

# miRetrieve

<details>

* Version: 1.3.4
* GitHub: NA
* Source code: https://github.com/cran/miRetrieve
* Date/Publication: 2021-09-18 17:30:02 UTC
* Number of recursive dependencies: 130

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
       1. └─miRetrieve::compare_mir_terms_scatter(df_merged, "miR-21", title = "Test_title") at test-comparemirterms.R:56:1
       2.   ├─plotly::ggplotly(plot)
       3.   └─plotly:::ggplotly.ggplot(plot)
       4.     └─plotly::gg2list(...)
       5.       └─plotly:::get_gdefs_ggproto(npscales$scales, theme, plot, layers)
       6.         └─guides$train(scales, theme$legend.direction, plot$labels)
      
      [ FAIL 1 | WARN 11 | SKIP 0 | PASS 202 ]
      Error: Test failures
      Execution halted
    ```

# MixOptim

<details>

* Version: 0.1.2
* GitHub: NA
* Source code: https://github.com/cran/MixOptim
* Date/Publication: 2020-07-01 09:40:11 UTC
* Number of recursive dependencies: 54

Run `revdepcheck::cloud_details(, "MixOptim")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘basic.Rmd’ using rmarkdown
    
    Quitting from lines 29-40 [unnamed-chunk-2] (basic.Rmd)
    Error: processing vignette 'basic.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘basic.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘basic.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rlang’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# mizer

<details>

* Version: 2.5.0
* GitHub: https://github.com/sizespectrum/mizer
* Source code: https://github.com/cran/mizer
* Date/Publication: 2023-12-08 13:00:02 UTC
* Number of recursive dependencies: 110

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
      • plots/plot-single-growth-curve.svg
      • plots/plot-spectra.svg
      • plots/plot-yield-by-gear.svg
      • plots/plot-yield.svg
      • plots/plotfishing-mortality.svg
      • plots/plotfmort-truncated.svg
      • plots/plotpredation-mortality.svg
      • plots/plotpredmort-truncated.svg
      Error: Test failures
      Execution halted
    ```

# mlr3fairness

<details>

* Version: 0.3.2
* GitHub: https://github.com/mlr-org/mlr3fairness
* Source code: https://github.com/cran/mlr3fairness
* Date/Publication: 2023-05-04 23:00:02 UTC
* Number of recursive dependencies: 130

Run `revdepcheck::cloud_details(, "mlr3fairness")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘debiasing-vignette.Rmd’ using rmarkdown
    --- finished re-building ‘debiasing-vignette.Rmd’
    
    --- re-building ‘measures-vignette.Rmd’ using rmarkdown
    --- finished re-building ‘measures-vignette.Rmd’
    
    --- re-building ‘reports-vignette.Rmd’ using rmarkdown
    
    ...
    --- failed re-building ‘reports-vignette.Rmd’
    
    --- re-building ‘visualization-vignette.Rmd’ using rmarkdown
    --- finished re-building ‘visualization-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘reports-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# mlr3spatiotempcv

<details>

* Version: 2.2.0
* GitHub: https://github.com/mlr-org/mlr3spatiotempcv
* Source code: https://github.com/cran/mlr3spatiotempcv
* Date/Publication: 2023-10-24 09:40:02 UTC
* Number of recursive dependencies: 167

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
    +   autoplot(resampling, task) +
    +     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
    +   autoplot(resampling, task, fold_id = 1)
    +   autoplot(resampling, task, fold_id = c(1, 2)) *
    +     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
    + }
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘mlr3spatiotempcv.Rmd’ using rmarkdown
    pandoc-citeproc: reference linnenbrink2023 not found
    --- finished re-building ‘mlr3spatiotempcv.Rmd’
    
    --- re-building ‘spatiotemp-viz.Rmd’ using rmarkdown
    
    Quitting from lines 91-149 [unnamed-chunk-4] (spatiotemp-viz.Rmd)
    Error: processing vignette 'spatiotemp-viz.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘spatiotemp-viz.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘spatiotemp-viz.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        data   3.4Mb
        help   1.2Mb
    ```

# mlr3viz

<details>

* Version: 0.7.0
* GitHub: https://github.com/mlr-org/mlr3viz
* Source code: https://github.com/cran/mlr3viz
* Date/Publication: 2023-12-21 10:00:02 UTC
* Number of recursive dependencies: 141

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
    INFO  [13:06:19.934] [bbotk]   5.884797  2.2371095 -32.51896
    INFO  [13:06:19.934] [bbotk]  -7.841127 -0.8872557 -91.31148
    INFO  [13:06:19.945] [bbotk] Finished optimizing after 20 evaluation(s)
    INFO  [13:06:19.947] [bbotk] Result:
    INFO  [13:06:19.949] [bbotk]        x1        x2  x_domain        y
    INFO  [13:06:19.949] [bbotk]  2.582281 -2.940254 <list[2]> 9.657379
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: print ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

# modeltime.resample

<details>

* Version: 0.2.3
* GitHub: https://github.com/business-science/modeltime.resample
* Source code: https://github.com/cran/modeltime.resample
* Date/Publication: 2023-04-12 15:50:02 UTC
* Number of recursive dependencies: 224

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
      ── Attaching packages ────────────────────────────────────── tidymodels 1.1.1 ──
      ✔ broom        1.0.5     ✔ recipes      1.0.9
      ✔ dials        1.2.0     ✔ rsample      1.2.0
    ...
       2. └─modeltime.resample::plot_modeltime_resamples(., .interactive = TRUE)
       3.   ├─plotly::ggplotly(g)
       4.   └─plotly:::ggplotly.ggplot(g)
       5.     └─plotly::gg2list(...)
       6.       └─plotly:::get_gdefs_ggproto(npscales$scales, theme, plot, layers)
       7.         └─guides$train(scales, theme$legend.direction, plot$labels)
      
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

# moderndive

<details>

* Version: 0.5.5
* GitHub: https://github.com/moderndive/moderndive
* Source code: https://github.com/cran/moderndive
* Date/Publication: 2022-12-01 22:20:02 UTC
* Number of recursive dependencies: 108

Run `revdepcheck::cloud_details(, "moderndive")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘moderndive.Rmd’ using rmarkdown
    
    Quitting from lines 410-421 [interaction-and-parallel-slopes-model-1] (moderndive.Rmd)
    Error: processing vignette 'moderndive.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘moderndive.Rmd’
    
    --- re-building ‘paper.Rmd’ using rmarkdown
    --- finished re-building ‘paper.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘moderndive.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.9Mb
      sub-directories of 1Mb or more:
        data   4.1Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 5238 marked UTF-8 strings
    ```

# mrgsim.sa

<details>

* Version: 0.2.0
* GitHub: https://github.com/kylebaron/mrgsim.sa
* Source code: https://github.com/cran/mrgsim.sa
* Date/Publication: 2023-12-08 13:50:02 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::cloud_details(, "mrgsim.sa")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘mrgsim.sa.Rmd’ using rmarkdown
    
    Quitting from lines 170-171 [unnamed-chunk-15] (mrgsim.sa.Rmd)
    Error: processing vignette 'mrgsim.sa.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘mrgsim.sa.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘mrgsim.sa.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# msmtools

<details>

* Version: 2.0.1
* GitHub: https://github.com/contefranz/msmtools
* Source code: https://github.com/cran/msmtools
* Date/Publication: 2021-04-12 23:20:05 UTC
* Number of recursive dependencies: 93

Run `revdepcheck::cloud_details(, "msmtools")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘msmtools.Rmd’ using rmarkdown_notangle
    
    Quitting from lines 475-476 [plot_M] (msmtools.Rmd)
    Error: processing vignette 'msmtools.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘msmtools.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘msmtools.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# mvGPS

<details>

* Version: 1.2.2
* GitHub: https://github.com/williazo/mvGPS
* Source code: https://github.com/cran/mvGPS
* Date/Publication: 2021-12-07 08:20:15 UTC
* Number of recursive dependencies: 118

Run `revdepcheck::cloud_details(, "mvGPS")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘mvGPS-intro.Rmd’ using rmarkdown
    
    Quitting from lines 33-52 [dag_draw] (mvGPS-intro.Rmd)
    Error: processing vignette 'mvGPS-intro.Rmd' failed with diagnostics:
    Problem while computing aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `plot$scales$add_defaults()`:
    ! attempt to apply non-function
    --- failed re-building ‘mvGPS-intro.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘mvGPS-intro.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Rdpack’
      All declared Imports should be used.
    ```

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) bal.Rd:156: Escaped LaTeX specials: \&
    ```

# mvinfluence

<details>

* Version: 0.9.0
* GitHub: https://github.com/friendly/mvinfluence
* Source code: https://github.com/cran/mvinfluence
* Date/Publication: 2022-09-20 17:10:02 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::cloud_details(, "mvinfluence")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘uni-vs-multi.Rmd’ using rmarkdown
    
    Quitting from lines 171-190 [dfbetas-plot] (uni-vs-multi.Rmd)
    Error: processing vignette 'uni-vs-multi.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘uni-vs-multi.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘uni-vs-multi.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# NAIR

<details>

* Version: 1.0.2
* GitHub: https://github.com/mlizhangx/Network-Analysis-for-Repertoire-Sequencing-
* Source code: https://github.com/cran/NAIR
* Date/Publication: 2023-09-27 10:20:08 UTC
* Number of recursive dependencies: 86

Run `revdepcheck::cloud_details(, "NAIR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(NAIR)
      Welcome to NAIR: Network Analysis of Immune Repertoire.
      Get started using `vignette("NAIR")`, or by visiting
      https://mlizhangx.github.io/Network-Analysis-for-Repertoire-Sequencing-/
      > 
      > test_check("NAIR")
    ...
      `expected` is a character vector ('UMIs')
      ── Failure ('test_functions.R:1422:3'): plots legends behave correctly ─────────
      sc_net$plots$SampleID$guides$size$name (`actual`) not equal to "legend" (`expected`).
      
      `actual` is NULL
      `expected` is a character vector ('legend')
      
      [ FAIL 48 | WARN 0 | SKIP 0 | PASS 1172 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.1Mb
      sub-directories of 1Mb or more:
        libs   6.6Mb
    ```

# neatmaps

<details>

* Version: 2.1.0
* GitHub: https://github.com/PhilBoileau/neatmaps
* Source code: https://github.com/cran/neatmaps
* Date/Publication: 2019-05-12 19:10:03 UTC
* Number of recursive dependencies: 100

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
    Error in train(..., self = self) : 
      unused argument (list(NULL, "column", "row", "text"))
    Calls: neatmap ... ggplotly.ggplot -> gg2list -> get_gdefs_ggproto -> <Anonymous>
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
    ```

# nestedmodels

<details>

* Version: 1.1.0
* GitHub: https://github.com/ashbythorpe/nestedmodels
* Source code: https://github.com/cran/nestedmodels
* Date/Publication: 2023-09-30 11:00:02 UTC
* Number of recursive dependencies: 137

Run `revdepcheck::cloud_details(, "nestedmodels")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘nestedmodels-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: autoplot.nested_model_fit
    > ### Title: Create a set of ggplots for a nested model object
    > ### Aliases: autoplot.nested_model_fit
    > 
    > ### ** Examples
    > 
    > ## Don't show: 
    ...
    > model <- linear_reg(penalty = 1) %>% set_engine("glmnet") %>% nested()
    > fit <- fit(model, z ~ x + y + a + b, nested_data)
    > plots <- autoplot(fit)
    > plots[[1]]
    > library(patchwork)
    > reduce(plots, `+`)
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

# NetFACS

<details>

* Version: 0.5.0
* GitHub: NA
* Source code: https://github.com/cran/NetFACS
* Date/Publication: 2022-12-06 17:32:35 UTC
* Number of recursive dependencies: 102

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
    +   random.level = NULL,
    +   combination.size = 2
    + )
    > 
    > emo.nets <- multiple_netfacs_network(emo.faces, min.count = 5)
    > multiple_network_plot(emo.nets)
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘netfacs_tutorial.Rmd’ using rmarkdown
    
    Quitting from lines 361-362 [multi.plot] (netfacs_tutorial.Rmd)
    Error: processing vignette 'netfacs_tutorial.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘netfacs_tutorial.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘netfacs_tutorial.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# NetworkExtinction

<details>

* Version: 1.0.3
* GitHub: NA
* Source code: https://github.com/cran/NetworkExtinction
* Date/Publication: 2023-03-31 11:40:02 UTC
* Number of recursive dependencies: 101

Run `revdepcheck::cloud_details(, "NetworkExtinction")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘NetworkExtinction.Rmd’ using rmarkdown
    
    Quitting from lines 202-205 [unnamed-chunk-10] (NetworkExtinction.Rmd)
    Error: processing vignette 'NetworkExtinction.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘NetworkExtinction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘NetworkExtinction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# OBIC

<details>

* Version: 3.0.1
* GitHub: https://github.com/AgroCares/Open-Bodem-Index-Calculator
* Source code: https://github.com/cran/OBIC
* Date/Publication: 2023-12-12 13:10:06 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::cloud_details(, "OBIC")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘description-of-the-columns.Rmd’ using rmarkdown
    --- finished re-building ‘description-of-the-columns.Rmd’
    
    --- re-building ‘obic_introduction.Rmd’ using rmarkdown
    
    Quitting from lines 243-292 [unnamed-chunk-8] (obic_introduction.Rmd)
    Error: processing vignette 'obic_introduction.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘obic_introduction.Rmd’
    ...
    Error: processing vignette 'obic_workability.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘obic_workability.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘obic_introduction.Rmd’ ‘obic_score_aggregation.Rmd’
      ‘obic_workability.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

# oncomsm

<details>

* Version: 0.1.4
* GitHub: https://github.com/Boehringer-Ingelheim/oncomsm
* Source code: https://github.com/cran/oncomsm
* Date/Publication: 2023-04-17 07:00:02 UTC
* Number of recursive dependencies: 125

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
       12.                 ├─base::unlist(guide_loc == panel_loc)
       13.                 └─base::Ops.data.frame(guide_loc, panel_loc)
      
      [ FAIL 1 | WARN 0 | SKIP 2 | PASS 59 ]
      Deleting unused snapshots:
      • plots/plot-mstate-srp-model-2.svg
      • plots/plot-mstate-srp-model-3.svg
      • plots/plot-srp-model-2.svg
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘avoiding-bias.Rmd’ using rmarkdown
    
    Quitting from lines 35-46 [unnamed-chunk-2] (avoiding-bias.Rmd)
    Error: processing vignette 'avoiding-bias.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘avoiding-bias.Rmd’
    
    --- re-building ‘oncomsm.Rmd’ using rmarkdown
    ...
    --- failed re-building ‘oncomsm.Rmd’
    
    --- re-building ‘prior-choice.Rmd’ using rmarkdown
    --- finished re-building ‘prior-choice.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘avoiding-bias.Rmd’ ‘oncomsm.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 65.2Mb
      sub-directories of 1Mb or more:
        doc    1.1Mb
        libs  62.8Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# OpenLand

<details>

* Version: 1.0.2
* GitHub: https://github.com/reginalexavier/OpenLand
* Source code: https://github.com/cran/OpenLand
* Date/Publication: 2021-11-02 07:20:02 UTC
* Number of recursive dependencies: 121

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
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 110 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      Actual value: "List of 11\\n \$ data       : tibble \[200 × 5\] \(S3: tbl_df/tbl/data\.frame\)\\n  \.\.\$ Period    : chr \[1:200\] "2000-2001" "2000-2001" "2000-2001" "2000-2001" \.\.\.\\n  \.\.\$ area_gross: num \[1:200\] 0\.000388 0\.000379 0\.00038 0\.000411 0\.000403 0\.000416 0\.000368 0\.000445 0\.000387 0\.000399 \.\.\.\\n  \.\.\$ From      : Factor w/ 5 levels "GUP","OZS","PSN",\.\.: 3 3 3 3 4 4 4 4 2 2 \.\.\.\\n  \.\.\$ To        : Factor w/ 5 levels "GUP","OZS","PSN",\.\.: 4 2 1 5 3 2 1 5 3 4 \.\.\.\\n  \.\.\$ changes   : chr \[1:200\] "Gain" "Gain" "Gain" "Gain" \.\.\.\\n \$ layers     :List of 4\\n  \.\.\$ :Classes 'LayerInstance', 'Layer', 'ggproto', 'gg' <ggproto object: Class LayerInstance, Layer, gg>\\n    aes_params: list\\n    compute_aesthetics: function\\n    compute_geom_1: function\\n    compute_geom_2: function\\n    compute_position: function\\n    compute_statistic: function\\n    computed_geom_params: NULL\\n    computed_mapping: NULL\\n    computed_stat_params: NULL\\n    constructor: call\\n    data: waiver\\n    draw_geom: function\\n    finish_statistics: function\\n    geom: <ggproto object: Class GeomBar, GeomRect, Geom, gg>\\n        aesthetics: function\\n        default_aes: uneval\\n        draw_group: function\\n        draw_key: function\\n        draw_layer: function\\n        draw_panel: function\\n        extra_params: just na\.rm orientation\\n        handle_na: function\\n        non_missing_aes: xmin xmax ymin ymax\\n        optional_aes: \\n        parameters: function\\n        rename_size: TRUE\\n        required_aes: x y\\n        setup_data: function\\n        setup_params: function\\n        use_defaults: function\\n        super:  <ggproto object: Class GeomRect, Geom, gg>\\n    geom_params: list\\n    inherit\.aes: TRUE\\n    layer_data: function\\n    map_statistic: function\\n    mapping: uneval\\n    position: <ggproto object: Class PositionStack, Position, gg>\\n        compute_layer: function\\n        compute_panel: function\\n        fill: FALSE\\n        required_aes: \\n        reverse: FALSE\\n        setup_data: function\\n        setup_params: function\\n        type: NULL\\n        vjust: 1\\n        super:  <ggproto object: Class Position, gg>\\n    print: function\\n    setup_layer: function\\n    show\.legend: NA\\n    stat: <ggproto object: Class StatIdentity, Stat, gg>\\n        aesthetics: function\\n        compute_group: function\\n        compute_layer: function\\n        compute_panel: function\\n        default_aes: uneval\\n        dropped_aes: \\n        extra_params: na\.rm\\n        finish_layer: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        required_aes: \\n        retransform: TRUE\\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Stat, gg>\\n    stat_params: list\\n    super:  <ggproto object: Class Layer, gg> \\n  \.\.\$ :Classes 'LayerInstance', 'Layer', 'ggproto', 'gg' <ggproto object: Class LayerInstance, Layer, gg>\\n    aes_params: list\\n    compute_aesthetics: function\\n    compute_geom_1: function\\n    compute_geom_2: function\\n    compute_position: function\\n    compute_statistic: function\\n    computed_geom_params: NULL\\n    computed_mapping: NULL\\n    computed_stat_params: NULL\\n    constructor: call\\n    data: tbl_df, tbl, data\.frame\\n    draw_geom: function\\n    finish_statistics: function\\n    geom: <ggproto object: Class GeomBar, GeomRect, Geom, gg>\\n        aesthetics: function\\n        default_aes: uneval\\n        draw_group: function\\n        draw_key: function\\n        draw_layer: function\\n        draw_panel: function\\n        extra_params: just na\.rm orientation\\n        handle_na: function\\n        non_missing_aes: xmin xmax ymin ymax\\n        optional_aes: \\n        parameters: function\\n        rename_size: TRUE\\n        required_aes: x y\\n        setup_data: function\\n        setup_params: function\\n        use_defaults: function\\n        super:  <ggproto object: Class GeomRect, Geom, gg>\\n    geom_params: list\\n    inherit\.aes: FALSE\\n    layer_data: function\\n    map_statistic: function\\n    mapping: uneval\\n    position: <ggproto object: Class PositionStack, Position, gg>\\n        compute_layer: function\\n        compute_panel: function\\n        fill: FALSE\\n        required_aes: \\n        reverse: FALSE\\n        setup_data: function\\n        setup_params: function\\n        type: NULL\\n        vjust: 1\\n        super:  <ggproto object: Class Position, gg>\\n    print: function\\n    setup_layer: function\\n    show\.legend: NA\\n    stat: <ggproto object: Class StatIdentity, Stat, gg>\\n        aesthetics: function\\n        compute_group: function\\n        compute_layer: function\\n        compute_panel: function\\n        default_aes: uneval\\n        dropped_aes: \\n        extra_params: na\.rm\\n        finish_layer: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        required_aes: \\n        retransform: TRUE\\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Stat, gg>\\n    stat_params: list\\n    super:  <ggproto object: Class Layer, gg> \\n  \.\.\$ :Classes 'LayerInstance', 'Layer', 'ggproto', 'gg' <ggproto object: Class LayerInstance, Layer, gg>\\n    aes_params: list\\n    compute_aesthetics: function\\n    compute_geom_1: function\\n    compute_geom_2: function\\n    compute_position: function\\n    compute_statistic: function\\n    computed_geom_params: NULL\\n    computed_mapping: NULL\\n    computed_stat_params: NULL\\n    constructor: call\\n    data: tbl_df, tbl, data\.frame\\n    draw_geom: function\\n    finish_statistics: function\\n    geom: <ggproto object: Class GeomSegment, Geom, gg>\\n        aesthetics: function\\n        default_aes: uneval\\n        draw_group: function\\n        draw_key: function\\n        draw_layer: function\\n        draw_panel: function\\n        extra_params: na\.rm\\n        handle_na: function\\n        non_missing_aes: linetype linewidth\\n        optional_aes: \\n        parameters: function\\n        rename_size: TRUE\\n        required_aes: x y xend\|yend\\n        setup_data: function\\n        setup_params: function\\n        use_defaults: function\\n        super:  <ggproto object: Class Geom, gg>\\n    geom_params: list\\n    inherit\.aes: TRUE\\n    layer_data: function\\n    map_statistic: function\\n    mapping: uneval\\n    position: <ggproto object: Class PositionIdentity, Position, gg>\\n        compute_layer: function\\n        compute_panel: function\\n        required_aes: \\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Position, gg>\\n    print: function\\n    setup_layer: function\\n    show\.legend: NA\\n    stat: <ggproto object: Class StatIdentity, Stat, gg>\\n        aesthetics: function\\n        compute_group: function\\n        compute_layer: function\\n        compute_panel: function\\n        default_aes: uneval\\n        dropped_aes: \\n        extra_params: na\.rm\\n        finish_layer: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        required_aes: \\n        retransform: TRUE\\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Stat, gg>\\n    stat_params: list\\n    super:  <ggproto object: Class Layer, gg> \\n  \.\.\$ :Classes 'LayerInstance', 'Layer', 'ggproto', 'gg' <ggproto object: Class LayerInstance, Layer, gg>\\n    aes_params: list\\n    compute_aesthetics: function\\n    compute_geom_1: function\\n    compute_geom_2: function\\n    compute_position: function\\n    compute_statistic: function\\n    computed_geom_params: NULL\\n    computed_mapping: NULL\\n    computed_stat_params: NULL\\n    constructor: call\\n    data: data\.frame\\n    draw_geom: function\\n    finish_statistics: function\\n    geom: <ggproto object: Class GeomHline, Geom, gg>\\n        aesthetics: function\\n        check_constant_aes: FALSE\\n        default_aes: uneval\\n        draw_group: function\\n        draw_key: function\\n        draw_layer: function\\n        draw_panel: function\\n        extra_params: na\.rm\\n        handle_na: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        rename_size: TRUE\\n        required_aes: yintercept\\n        setup_data: function\\n        setup_params: function\\n        use_defaults: function\\n        super:  <ggproto object: Class Geom, gg>\\n    geom_params: list\\n    inherit\.aes: FALSE\\n    layer_data: function\\n    map_statistic: function\\n    mapping: uneval\\n    position: <ggproto object: Class PositionIdentity, Position, gg>\\n        compute_layer: function\\n        compute_panel: function\\n        required_aes: \\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Position, gg>\\n    print: function\\n    setup_layer: function\\n    show\.legend: FALSE\\n    stat: <ggproto object: Class StatIdentity, Stat, gg>\\n        aesthetics: function\\n        compute_group: function\\n        compute_layer: function\\n        compute_panel: function\\n        default_aes: uneval\\n        dropped_aes: \\n        extra_params: na\.rm\\n        finish_layer: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        required_aes: \\n        retransform: TRUE\\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Stat, gg>\\n    stat_params: list\\n    super:  <ggproto object: Class Layer, gg> \\n \$ scales     :Classes 'ScalesList', 'ggproto', 'gg' <ggproto object: Class ScalesList, gg>\\n    add: function\\n    add_defaults: function\\n    add_missing: function\\n    backtransform_df: function\\n    clone: function\\n    find: function\\n    get_scales: function\\n    has_scale: function\\n    input: function\\n    map_df: function\\n    n: function\\n    non_position_scales: function\\n    scales: list\\n    train_df: function\\n    transform_df: function\\n    super:  <ggproto object: Class ScalesList, gg> \\n \$ guides     :Classes 'Guides', 'ggproto', 'gg' <ggproto object: Class Guides, gg>\\n    add: function\\n    assemble: function\\n    build: function\\n    draw: function\\n    get_custom: function\\n    get_guide: function\\n    get_params: function\\n    get_position: function\\n    guides: NULL\\n    merge: function\\n    missing: <ggproto object: Class GuideNone, Guide, gg>\\n        arrange_layout: function\\n        assemble_drawing: function\\n        available_aes: any\\n        build_decor: function\\n        build_labels: function\\n        build_ticks: function\\n        build_title: function\\n        draw: function\\n        draw_early_exit: function\\n        elements: list\\n        extract_decor: function\\n        extract_key: function\\n        extract_params: function\\n        get_layer_key: function\\n        hashables: list\\n        measure_grobs: function\\n        merge: function\\n        override_elements: function\\n        params: list\\n        process_layers: function\\n        setup_elements: function\\n        setup_params: function\\n        train: function\\n        transform: function\\n        super:  <ggproto object: Class GuideNone, Guide, gg>\\n    package_box: function\\n    print: function\\n    process_layers: function\\n    setup: function\\n    subset_guides: function\\n    train: function\\n    update_params: function\\n    super:  <ggproto object: Class Guides, gg> \\n \$ mapping    :List of 2\\n  \.\.\$ x: language ~To\\n  \.\. \.\.- attr\(\*, "\.Environment"\)=<environment: 0x55aa8e68aea0> \\n  \.\.\$ y: language ~area_gross\\n  \.\. \.\.- attr\(\*, "\.Environment"\)=<environment: 0x55aa8e68aea0> \\n  \.\.- attr\(\*, "class"\)= chr "uneval"\\n \$ theme      :List of 1\\n  \.\.\$ plot\.title:List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : NULL\\n  \.\. \.\.\$ size         : NULL\\n  \.\. \.\.\$ hjust        : num 0\.5\\n  \.\. \.\.\$ vjust        : NULL\\n  \.\. \.\.\$ angle        : NULL\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : NULL\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi FALSE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.- attr\(\*, "complete"\)= logi FALSE\\n  \.\.- attr\(\*, "validate"\)= logi TRUE\\n \$ coordinates:Classes 'CoordCartesian', 'Coord', 'ggproto', 'gg' <ggproto object: Class CoordCartesian, Coord, gg>\\n    aspect: function\\n    backtransform_range: function\\n    clip: on\\n    default: TRUE\\n    distance: function\\n    expand: TRUE\\n    is_free: function\\n    is_linear: function\\n    labels: function\\n    limits: list\\n    modify_scales: function\\n    range: function\\n    render_axis_h: function\\n    render_axis_v: function\\n    render_bg: function\\n    render_fg: function\\n    setup_data: function\\n    setup_layout: function\\n    setup_panel_guides: function\\n    setup_panel_params: function\\n    setup_params: function\\n    train_panel_guides: function\\n    transform: function\\n    super:  <ggproto object: Class CoordCartesian, Coord, gg> \\n \$ facet      :Classes 'FacetNull', 'Facet', 'ggproto', 'gg' <ggproto object: Class FacetNull, Facet, gg>\\n    compute_layout: function\\n    draw_back: function\\n    draw_front: function\\n    draw_labels: function\\n    draw_panels: function\\n    finish_data: function\\n    init_scales: function\\n    map_data: function\\n    params: list\\n    setup_data: function\\n    setup_params: function\\n    shrink: TRUE\\n    train_scales: function\\n    vars: function\\n    super:  <ggproto object: Class FacetNull, Facet, gg> \\n \$ plot_env   :<environment: 0x55aa8e68aea0> \\n \$ layout     :Classes 'Layout', 'ggproto', 'gg' <ggproto object: Class Layout, gg>\\n    coord: NULL\\n    coord_params: list\\n    facet: NULL\\n    facet_params: list\\n    finish_data: function\\n    get_scales: function\\n    layout: NULL\\n    map_position: function\\n    panel_params: NULL\\n    panel_scales_x: NULL\\n    panel_scales_y: NULL\\n    render: function\\n    render_labels: function\\n    reset_scales: function\\n    resolve_label: function\\n    setup: function\\n    setup_panel_guides: function\\n    setup_panel_params: function\\n    train_position: function\\n    super:  <ggproto object: Class Layout, gg> \\n \$ labels     :List of 7\\n  \.\.\$ title     : NULL\\n  \.\.\$ y         : chr "Area \(Km2\)"\\n  \.\.\$ x         : chr "LUC category"\\n  \.\.\$ fill      : chr "Changes"\\n  \.\.\$ xend      : chr "as\.numeric\(To\) \+ 0\.3"\\n  \.\.\$ yend      : chr "area"\\n  \.\.\$ yintercept: chr "yintercept"\\n - attr\(\*, "class"\)= chr \[1:2\] "gg" "ggplot""
      Backtrace:
          ▆
       1. └─testthat::expect_output(...) at test_plots.R:59:3
       2.   └─testthat::expect_match(...)
       3.     └─testthat:::expect_match_(...)
      
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 110 ]
      Error: Test failures
      Execution halted
    ```

# ordr

<details>

* Version: 0.1.1
* GitHub: https://github.com/corybrunson/ordr
* Source code: https://github.com/cran/ordr
* Date/Publication: 2022-10-20 20:52:35 UTC
* Number of recursive dependencies: 102

Run `revdepcheck::cloud_details(, "ordr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ordr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: methods-cancor
    > ### Title: Functionality for canonical correlations
    > ### Aliases: methods-cancor as_tbl_ord.cancor_ord recover_rows.cancor_ord
    > ###   recover_cols.cancor_ord recover_inertia.cancor_ord
    > ###   recover_coord.cancor_ord recover_conference.cancor_ord
    > ###   recover_supp_rows.cancor_ord recover_supp_cols.cancor_ord
    > ###   recover_aug_rows.cancor_ord recover_aug_cols.cancor_ord
    ...
    +   geom_unit_circle() +
    +   geom_rows_vector(arrow = NULL, elements = "structure") +
    +   geom_cols_vector(arrow = NULL, elements = "structure", linetype = "dashed") +
    +   geom_rows_text(elements = "structure", hjust = "outward") +
    +   geom_cols_text(elements = "structure", hjust = "outward") +
    +   scale_color_brewer(limits = c("rows", "cols"), type = "qual") +
    +   expand_limits(x = c(-1, 1), y = c(-1, 1))
    Error in 1 - hjust[is_case] : non-numeric argument to binary operator
    Calls: <Anonymous> ... lapply -> FUN -> <Anonymous> -> draw_key -> rotate_just
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4 marked UTF-8 strings
    ```

# pagoo

<details>

* Version: 0.3.17
* GitHub: https://github.com/iferres/pagoo
* Source code: https://github.com/cran/pagoo
* Date/Publication: 2022-11-18 18:50:02 UTC
* Number of recursive dependencies: 148

Run `revdepcheck::cloud_details(, "pagoo")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Input.Rmd’ using rmarkdown
    --- finished re-building ‘Input.Rmd’
    
    --- re-building ‘Methods_Plots.Rmd’ using rmarkdown
    
    Quitting from lines 82-93 [unnamed-chunk-6] (Methods_Plots.Rmd)
    Error: processing vignette 'Methods_Plots.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘Methods_Plots.Rmd’
    ...
    --- finished re-building ‘Subseting.Rmd’
    
    --- re-building ‘pagoo.Rmd’ using rmarkdown
    --- finished re-building ‘pagoo.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘Methods_Plots.Rmd’ ‘Recipes.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        extdata   4.1Mb
    ```

# paletteer

<details>

* Version: 1.5.0
* GitHub: https://github.com/EmilHvitfeldt/paletteer
* Source code: https://github.com/cran/paletteer
* Date/Publication: 2022-10-19 10:02:40 UTC
* Number of recursive dependencies: 91

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
      • vdiffr_palette_check/tvthemes.svg
      • vdiffr_palette_check/unikn.svg
      • vdiffr_palette_check/vapeplot.svg
      • vdiffr_palette_check/vaporwave.svg
      • vdiffr_palette_check/viridis.svg
      • vdiffr_palette_check/werpals.svg
      • vdiffr_palette_check/wesanderson.svg
      • vdiffr_palette_check/yarrr.svg
      Error: Test failures
      Execution halted
    ```

# palettes

<details>

* Version: 0.1.1
* GitHub: https://github.com/mccarthy-m-g/palettes
* Source code: https://github.com/cran/palettes
* Date/Publication: 2023-01-12 22:50:02 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::cloud_details(, "palettes")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘biscale.Rmd’ using rmarkdown
    
    Quitting from lines 105-138 [unnamed-chunk-7] (biscale.Rmd)
    Error: processing vignette 'biscale.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘biscale.Rmd’
    
    --- re-building ‘compatibility.Rmd’ using rmarkdown
    --- finished re-building ‘compatibility.Rmd’
    ...
    --- finished re-building ‘gt.Rmd’
    
    --- re-building ‘palettes.Rmd’ using rmarkdown
    --- finished re-building ‘palettes.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘biscale.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# patchwork

<details>

* Version: 1.1.3
* GitHub: https://github.com/thomasp85/patchwork
* Source code: https://github.com/cran/patchwork
* Date/Publication: 2023-08-14 13:10:05 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::cloud_details(, "patchwork")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘patchwork-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: area
    > ### Title: Specify a plotting area in a layout
    > ### Aliases: area
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
    > 
    > # Show the layout to make sure it looks as it should
    > plot(layout)
    > 
    > # Apply it to a patchwork
    > p1 + p2 + p3 + plot_layout(design = layout)
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
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
      [ FAIL 7 | WARN 0 | SKIP 0 | PASS 0 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
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

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘patchwork.Rmd’ using rmarkdown
    
    Quitting from lines 64-65 [unnamed-chunk-3] (patchwork.Rmd)
    Error: processing vignette 'patchwork.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘patchwork.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘patchwork.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# pathfindR

<details>

* Version: 2.3.0
* GitHub: https://github.com/egeulgen/pathfindR
* Source code: https://github.com/cran/pathfindR
* Date/Publication: 2023-10-08 20:30:02 UTC
* Number of recursive dependencies: 144

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
     16. │             └─ggplot2 (local) setup_data(...)
     17. │               ├─data$width %||% params$width %||% resolution(data$x, FALSE)
     18. │               └─ggplot2::resolution(data$x, FALSE)
     19. │                 └─ggplot2:::unique0(as.numeric(x))
     20. └─base::.handleSimpleError(...)
     21.   └─rlang (local) h(simpleError(msg, call))
     22.     └─handlers[[1L]](cnd)
     23.       └─cli::cli_abort(...)
     24.         └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘comparing_results.Rmd’ using rmarkdown
    Warning: ggrepel: 9 unlabeled data points (too many overlaps). Consider increasing max.overlaps
    --- finished re-building ‘comparing_results.Rmd’
    
    --- re-building ‘intro_vignette.Rmd’ using rmarkdown
    --- finished re-building ‘intro_vignette.Rmd’
    
    --- re-building ‘manual_execution.Rmd’ using rmarkdown
    --- finished re-building ‘manual_execution.Rmd’
    ...
    ℹ Error occurred in the 2nd layer.
    Caused by error in `unique0()`:
    ! 'list' object cannot be coerced to type 'double'
    --- failed re-building ‘visualization_vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘visualization_vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# performance

<details>

* Version: 0.10.8
* GitHub: https://github.com/easystats/performance
* Source code: https://github.com/cran/performance
* Date/Publication: 2023-10-30 08:40:02 UTC
* Number of recursive dependencies: 287

Run `revdepcheck::cloud_details(, "performance")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘performance-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: check_distribution
    > ### Title: Classify the distribution of a model-family using machine
    > ###   learning
    > ### Aliases: check_distribution
    > 
    > ### ** Examples
    > 
    ...
    + plot(check_distribution(model))
    + ## Don't show: 
    + }) # examplesIf
    Loading required package: see
    Loading required package: patchwork
    > plot(check_distribution(model))
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

## In both

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(performance)
      > 
      > test_check("performance")
      Starting 2 test processes
      [ FAIL 1 | WARN 0 | SKIP 33 | PASS 316 ]
      
    ...
       25.                             └─methods::new(...)
       26.                               ├─methods::initialize(value, ...)
       27.                               └─Matrix (local) initialize(value, ...)
       28.                                 ├─methods::callNextMethod()
       29.                                 └─methods (local) .nextMethod(.Object = .Object, ... = ...)
       30.                                   └─methods::validObject(.Object)
      
      [ FAIL 1 | WARN 0 | SKIP 33 | PASS 316 ]
      Error: Test failures
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘rstanarm’
    ```

# personalized

<details>

* Version: 0.2.7
* GitHub: https://github.com/jaredhuling/personalized
* Source code: https://github.com/cran/personalized
* Date/Publication: 2022-06-27 20:20:03 UTC
* Number of recursive dependencies: 95

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
        6.   │ └─plotly:::dots2plots(...)
        7.   ├─plotly::ggplotly(p.primary, tooltip = paste0("tooltip", 1:4))
        8.   └─plotly:::ggplotly.ggplot(...)
        9.     └─plotly::gg2list(...)
       10.       └─plotly:::get_gdefs_ggproto(npscales$scales, theme, plot, layers)
       11.         └─guides$train(scales, theme$legend.direction, plot$labels)
      
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
* Number of recursive dependencies: 91

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
    Error in train(..., self = self) : 
      unused argument (list("Plasmid", "Sample", "AMR_gene", "Inc_group", "paste(\"Sureness: \", round(report$Sureness, 2))"))
    Calls: main ... ggplotly.ggplot -> gg2list -> get_gdefs_ggproto -> <Anonymous>
    Execution halted
    ```

# platetools

<details>

* Version: 0.1.5
* GitHub: https://github.com/swarchal/platetools
* Source code: https://github.com/cran/platetools
* Date/Publication: 2021-06-03 12:00:02 UTC
* Number of recursive dependencies: 48

Run `revdepcheck::cloud_details(, "platetools")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(platetools)
      > 
      > test_check("platetools")
      [ FAIL 2 | WARN 1 | SKIP 4 | PASS 187 ]
      
      ══ Skipped tests (4) ═══════════════════════════════════════════════════════════
    ...
      length(out96) not equal to 9L.
      1/1 mismatches
      [1] 11 - 9 == 2
      ── Failure ('test-plot_wrapper.R:34:5'): returns expected ggplot object ────────
      names(out96) not equal to c(...).
      Lengths differ: 11 is not 9
      
      [ FAIL 2 | WARN 1 | SKIP 4 | PASS 187 ]
      Error: Test failures
      Execution halted
    ```

# plot3logit

<details>

* Version: 3.1.4
* GitHub: https://github.com/f-santi/plot3logit
* Source code: https://github.com/cran/plot3logit
* Date/Publication: 2023-12-10 11:30:05 UTC
* Number of recursive dependencies: 111

Run `revdepcheck::cloud_details(, "plot3logit")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘plot3logit-overview.Rmd’ using rmarkdown
    
    Quitting from lines 231-232 [unnamed-chunk-10] (plot3logit-overview.Rmd)
    Error: processing vignette 'plot3logit-overview.Rmd' failed with diagnostics:
    attempt to apply non-function
    --- failed re-building ‘plot3logit-overview.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘plot3logit-overview.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# plotDK

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/plotDK
* Date/Publication: 2021-10-01 08:00:02 UTC
* Number of recursive dependencies: 87

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
       1. └─plotDK::plotDK(...) at test-plotDK.R:32:5
       2.   ├─plotly::ggplotly(p, tooltip = c("text", "fill"))
       3.   └─plotly:::ggplotly.ggplot(p, tooltip = c("text", "fill"))
       4.     └─plotly::gg2list(...)
       5.       └─plotly:::get_gdefs_ggproto(npscales$scales, theme, plot, layers)
       6.         └─guides$train(scales, theme$legend.direction, plot$labels)
      
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

* Version: 4.10.3
* GitHub: https://github.com/plotly/plotly.R
* Source code: https://github.com/cran/plotly
* Date/Publication: 2023-10-21 22:50:09 UTC
* Number of recursive dependencies: 148

Run `revdepcheck::cloud_details(, "plotly")` for more info

</details>

## Newly broken

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
      • cookbook-scatterplots/jitter.svg
      • cookbook-scatterplots/overlap.svg
      • cookbook-scatterplots/scale-color-hue.svg
      • cookbook-scatterplots/shape-manual.svg
      • cookbook-scatterplots/shape.svg
      • ggplot-contour/contour.svg
      • ggplot-heatmap/heatmap.svg
      • ggplot-path/path-colors2.svg
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

# plotmm

<details>

* Version: 0.1.1
* GitHub: https://github.com/pdwaggoner/plotmm
* Source code: https://github.com/cran/plotmm
* Date/Publication: 2022-11-09 21:50:02 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "plotmm")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Getting-Started.Rmd’ using rmarkdown
    
    Quitting from lines 89-101 [unnamed-chunk-4] (Getting-Started.Rmd)
    Error: processing vignette 'Getting-Started.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘Getting-Started.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Getting-Started.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# plutor

<details>

* Version: 0.1.0
* GitHub: https://github.com/william-swl/plutor
* Source code: https://github.com/cran/plutor
* Date/Publication: 2023-10-27 08:00:02 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::cloud_details(, "plutor")` for more info

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
      Backtrace:
          ▆
       1. ├─extract_compare(p) %>% ... at test-geom_compare.R:175:3
       2. └─dplyr::mutate(., label = c("lab1", "lab2", "lab3"))
      
      [ FAIL 1 | WARN 40 | SKIP 26 | PASS 0 ]
      Deleting unused snapshots:
      • geom_compare/geom-compare-manual-comparisons-table.svg
      Error: Test failures
      Execution halted
    ```

# pmartR

<details>

* Version: 2.4.2
* GitHub: https://github.com/pmartR/pmartR
* Source code: https://github.com/cran/pmartR
* Date/Publication: 2023-12-12 10:50:02 UTC
* Number of recursive dependencies: 150

Run `revdepcheck::cloud_details(, "pmartR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘pmartR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.dataRes
    > ### Title: Plot dataRes object
    > ### Aliases: plot.dataRes
    > 
    > ### ** Examples
    > 
    > ## Don't show: 
    ...
    + ## Don't show: 
    + }) # examplesIf
    > library(pmartRdata)
    > mylipid <- edata_transform(omicsData = lipid_pos_object, data_scale = "log2")
    > result <- edata_summary(omicsData = mylipid, by = "molecule", groupvar = "Virus")
    > plot(result)
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(pmartR)
      > 
      > test_check("pmartR")
      [ FAIL 1 | WARN 0 | SKIP 5 | PASS 2535 ]
      
      ══ Skipped tests (5) ═══════════════════════════════════════════════════════════
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
* Number of recursive dependencies: 86

Run `revdepcheck::cloud_details(, "pmxTools")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘pk-curves.Rmd’ using rmarkdown
    
    Quitting from lines 63-102 [1cptdemo] (pk-curves.Rmd)
    Error: processing vignette 'pk-curves.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘pk-curves.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘pk-curves.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘DiagrammeR’
    ```

# portvine

<details>

* Version: 1.0.2
* GitHub: https://github.com/EmanuelSommer/portvine
* Source code: https://github.com/cran/portvine
* Date/Publication: 2023-01-06 09:40:04 UTC
* Number of recursive dependencies: 132

Run `revdepcheck::cloud_details(, "portvine")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘get_started.Rmd’ using rmarkdown
    
    Quitting from lines 292-294 [unnamed-chunk-18] (get_started.Rmd)
    Error: processing vignette 'get_started.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘get_started.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘get_started.Rmd’
    
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
      installed size is 54.5Mb
      sub-directories of 1Mb or more:
        libs  53.5Mb
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

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘PPQnote.Rmd’ using rmarkdown
    --- finished re-building ‘PPQnote.Rmd’
    
    --- re-building ‘PPQplan-vignette.Rmd’ using rmarkdown
    
    Quitting from lines 130-131 [unnamed-chunk-10] (PPQplan-vignette.Rmd)
    Error: processing vignette 'PPQplan-vignette.Rmd' failed with diagnostics:
    unused argument (list("Heatmap for Sterile Concentration Assay\nLSL = 95%LC, USL = 105%LC, k = 2.373", "Mean Value", "Standard Deviation", "Passing\nProbability \n", "Pass.Prob", "level"))
    --- failed re-building ‘PPQplan-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘PPQplan-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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

* Version: 0.2.2
* GitHub: https://github.com/zabore/ppseq
* Source code: https://github.com/cran/ppseq
* Date/Publication: 2023-04-17 18:20:03 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::cloud_details(, "ppseq")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ppseq-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.calibrate_thresholds
    > ### Title: Plot method for 'calibrate_thresholds' objects
    > ### Aliases: plot.calibrate_thresholds
    > 
    > ### ** Examples
    > 
    > 
    ...
    +   S = 10,
    +   nsim = 10
    +   )
    Joining with `by = join_by(sim_num, pp_threshold, ppp_threshold)`
    > 
    > plot(cal_tbl1, type1_range = c(0.01, 0.2), minimum_power = 0.7)
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Comparing ‘spelling.Rout’ to ‘spelling.Rout.save’ ...6c6
    < NULL
    ---
    > All Done!
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ppseq)
    ...
       11.               └─patchwork:::add_guides(gt, guides == "collect")
       12.                 ├─base::unlist(guide_loc == panel_loc)
       13.                 └─base::Ops.data.frame(guide_loc, panel_loc)
      
      [ FAIL 2 | WARN 0 | SKIP 18 | PASS 60 ]
      Deleting unused snapshots:
      • plot/plot-one-sample-cal-tbl-plotly.svg
      • plot/plot-one-sample-decision-rules-plotly.svg
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘one_sample_expansion.Rmd’ using rmarkdown
    
    Quitting from lines 183-188 [unnamed-chunk-13] (one_sample_expansion.Rmd)
    Error: processing vignette 'one_sample_expansion.Rmd' failed with diagnostics:
    unused argument (list("Type I error", "Power", "Distance to optimal accuracy", "Design", "Average N under the null", "Average N under the alternative"))
    --- failed re-building ‘one_sample_expansion.Rmd’
    
    --- re-building ‘two_sample_randomized.Rmd’ using rmarkdown
    ...
    Quitting from lines 179-184 [unnamed-chunk-13] (two_sample_randomized.Rmd)
    Error: processing vignette 'two_sample_randomized.Rmd' failed with diagnostics:
    unused argument (list("Type I error", "Power", "Distance to optimal accuracy", "Design", "Average N under the null", "Average N under the alternative"))
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
        doc  10.4Mb
    ```

# prcbench

<details>

* Version: 1.1.8
* GitHub: https://github.com/evalclass/prcbench
* Source code: https://github.com/cran/prcbench
* Date/Publication: 2023-03-12 16:10:09 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::cloud_details(, "prcbench")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘prcbench-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: autoplot
    > ### Title: Plot the result of Precision-Recall curve evaluation
    > ### Aliases: autoplot autoplot.evalcurve
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
    > 
    > ## Plot evaluation results on test datasets r1, r2, and r3
    > testset <- create_testset("curve", c("c1", "c2", "c3"))
    > toolset <- create_toolset(set_names = "crv5")
    > eres1 <- run_evalcurve(testset, toolset)
    > autoplot(eres1)
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘introduction.Rmd’ using rmarkdown
    
    Quitting from lines 181-194 [unnamed-chunk-7] (introduction.Rmd)
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# PRECAST

<details>

* Version: 1.6.3
* GitHub: https://github.com/feiyoung/PRECAST
* Source code: https://github.com/cran/PRECAST
* Date/Publication: 2023-11-06 05:50:08 UTC
* Number of recursive dependencies: 225

Run `revdepcheck::cloud_details(, "PRECAST")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘PRECAST-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: SpaPlot
    > ### Title: Spatial heatmap
    > ### Aliases: SpaPlot
    > 
    > ### ** Examples
    > 
    > 
    ...
    Using only PRECAST results to obtain the batch corrected gene expressions since species is unknown or the genelist in PRECASTObj has less than 5 overlapp with the housekeeping genes of given species.
    Start integration...
    2023-12-22 13:24:15.683225 : ***** Data integration finished!, 0.001 mins elapsed.
    Put the data into a new Seurat object...
    2023-12-22 13:24:15.795851 : ***** New Seurat object is generated!, 0.002 mins elapsed.
    >   SpaPlot(seuInt)
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 24.6Mb
      sub-directories of 1Mb or more:
        data   3.2Mb
        libs  20.6Mb
    ```

# precrec

<details>

* Version: 0.14.4
* GitHub: https://github.com/evalclass/precrec
* Source code: https://github.com/cran/precrec
* Date/Publication: 2023-10-11 22:10:02 UTC
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "precrec")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘introduction.Rmd’ using rmarkdown
    
    Quitting from lines 62-70 [unnamed-chunk-3] (introduction.Rmd)
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.0Mb
      sub-directories of 1Mb or more:
        data   1.5Mb
        libs   6.5Mb
    ```

# processmapR

<details>

* Version: 0.5.3
* GitHub: https://github.com/bupaverse/processmapr
* Source code: https://github.com/cran/processmapR
* Date/Publication: 2023-04-06 12:50:02 UTC
* Number of recursive dependencies: 122

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
       12.     └─plotly:::ggplotly.ggplot(p)
       13.       └─plotly::gg2list(...)
       14.         └─plotly:::get_gdefs_ggproto(npscales$scales, theme, plot, layers)
       15.           └─guides$train(scales, theme$legend.direction, plot$labels)
      ── Failure ('test_trace_explorer.R:240:3'): test trace_explorer on eventlog with param `plotly` ──
      `chart` inherits from 'gg'/'ggplot' not 'plotly'.
      
      [ FAIL 6 | WARN 0 | SKIP 10 | PASS 107 ]
      Error: Test failures
      Execution halted
    ```

# PTXQC

<details>

* Version: 1.1.0
* GitHub: https://github.com/cbielow/PTXQC
* Source code: https://github.com/cran/PTXQC
* Date/Publication: 2023-12-15 15:40:03 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "PTXQC")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘PTXQC-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_RatiosPG
    > ### Title: Plot ratios of labeled data (e.g. SILAC) from proteinGroups.txt
    > ### Aliases: plot_RatiosPG
    > 
    > ### ** Examples
    > 
    > 
    ...
    >  y2 = dnorm(x2, mean = -1)
    >  data = data.frame( x = c(x1,x2),
    +                     y = c(y1,y2), 
    +                     col = c(rep("ok", length(x1)), rep("shifted", length(x2))), 
    +                     ltype = c(rep("solid", length(x1)), rep("dotted", length(x2))))
    >  plot_RatiosPG(data, range(data$x), "Ratio plot", "red", "group")
    Error in as.vector(x, "character") : 
      cannot coerce type 'environment' to vector of type 'character'
    Calls: <Anonymous> ... validDetails.text -> as.character -> as.character.default
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(PTXQC)
      Loading package PTXQC (version 1.1.0)
      > 
      > ##
      > ## calls all code in PTXQC/tests/testthat/test*.R
      > ##
    ...
       44.                               ├─grid:::validGrob(g)
       45.                               └─grid:::validGrob.grob(g)
       46.                                 ├─grid::validDetails(x)
       47.                                 └─grid:::validDetails.text(x)
       48.                                   ├─base::as.character(x$label)
       49.                                   └─base::as.character.default(x$label)
      
      [ FAIL 1 | WARN 23 | SKIP 0 | PASS 131 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.9Mb
      sub-directories of 1Mb or more:
        doc        4.0Mb
        examples   2.6Mb
    ```

# qacBase

<details>

* Version: 1.0.3
* GitHub: https://github.com/rkabacoff/qacBase
* Source code: https://github.com/cran/qacBase
* Date/Publication: 2022-02-09 22:20:02 UTC
* Number of recursive dependencies: 111

Run `revdepcheck::cloud_details(, "qacBase")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘qacBase-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: univariate_plot
    > ### Title: Univariate plot
    > ### Aliases: univariate_plot
    > 
    > ### ** Examples
    > 
    > univariate_plot(mtcars, mpg)
    Warning: The dot-dot notation (`..density..`) was deprecated in ggplot2 3.4.0.
    ℹ Please use `after_stat(density)` instead.
    ℹ The deprecated feature was likely used in the qacBase package.
      Please report the issue at <https://github.com/rkabacoff/qacBase/issues>.
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

# rabhit

<details>

* Version: 0.2.5
* GitHub: NA
* Source code: https://github.com/cran/rabhit
* Date/Publication: 2023-02-06 13:32:31 UTC
* Number of recursive dependencies: 135

Run `revdepcheck::cloud_details(, "rabhit")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘RAbHIT-vignette.Rmd’ using rmarkdown
    
    Quitting from lines 199-201 [unnamed-chunk-8] (RAbHIT-vignette.Rmd)
    Error: processing vignette 'RAbHIT-vignette.Rmd' failed with diagnostics:
    subscript out of bounds
    --- failed re-building ‘RAbHIT-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘RAbHIT-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# Racmacs

<details>

* Version: 1.2.9
* GitHub: https://github.com/acorg/Racmacs
* Source code: https://github.com/cran/Racmacs
* Date/Publication: 2023-11-30 11:40:02 UTC
* Number of recursive dependencies: 133

Run `revdepcheck::cloud_details(, "Racmacs")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(Racmacs)
      > 
      > test_check("Racmacs")
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 1434 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
        6. └─Racmacs::plotly_map_table_distance(map)
        7.   ├─plotly::ggplotly(gp, tooltip = "text")
        8.   └─plotly:::ggplotly.ggplot(gp, tooltip = "text")
        9.     └─plotly::gg2list(...)
       10.       └─plotly:::get_gdefs_ggproto(npscales$scales, theme, plot, layers)
       11.         └─guides$train(scales, theme$legend.direction, plot$labels)
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 1434 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 69.7Mb
      sub-directories of 1Mb or more:
        doc           5.3Mb
        htmlwidgets   1.8Mb
        libs         60.0Mb
    ```

# radiant.basics

<details>

* Version: 1.6.0
* GitHub: https://github.com/radiant-rstats/radiant.basics
* Source code: https://github.com/cran/radiant.basics
* Date/Publication: 2023-09-07 02:30:09 UTC
* Number of recursive dependencies: 146

Run `revdepcheck::cloud_details(, "radiant.basics")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘radiant.basics-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.clt
    > ### Title: Plot method for the Central Limit Theorem simulation
    > ### Aliases: plot.clt
    > 
    > ### ** Examples
    > 
    > clt("Uniform", 100, 100, unif_min = 10, unif_max = 20) %>% plot()
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

# radiant.data

<details>

* Version: 1.6.3
* GitHub: https://github.com/radiant-rstats/radiant.data
* Source code: https://github.com/cran/radiant.data
* Date/Publication: 2023-12-17 07:50:02 UTC
* Number of recursive dependencies: 149

Run `revdepcheck::cloud_details(, "radiant.data")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘radiant.data-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: visualize
    > ### Title: Visualize data using ggplot2 <https://ggplot2.tidyverse.org/>
    > ### Aliases: visualize
    > 
    > ### ** Examples
    > 
    > visualize(diamonds, "price:cut", type = "dist", fillcol = "red")
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: visualize ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

# radiant.model

<details>

* Version: 1.6.3
* GitHub: https://github.com/radiant-rstats/radiant.model
* Source code: https://github.com/cran/radiant.model
* Date/Publication: 2023-10-16 06:00:03 UTC
* Number of recursive dependencies: 173

Run `revdepcheck::cloud_details(, "radiant.model")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘radiant.model-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.crtree
    > ### Title: Plot method for the crtree function
    > ### Aliases: plot.crtree
    > 
    > ### ** Examples
    > 
    > result <- crtree(titanic, "survived", c("pclass", "sex"), lev = "Yes")
    > plot(result)
    > result <- crtree(diamonds, "price", c("carat", "clarity", "cut"))
    > plot(result, plots = "prune")
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: plot ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

# radiant.multivariate

<details>

* Version: 1.6.1
* GitHub: https://github.com/radiant-rstats/radiant.multivariate
* Source code: https://github.com/cran/radiant.multivariate
* Date/Publication: 2023-09-23 05:10:02 UTC
* Number of recursive dependencies: 178

Run `revdepcheck::cloud_details(, "radiant.multivariate")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘radiant.multivariate-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.conjoint
    > ### Title: Plot method for the conjoint function
    > ### Aliases: plot.conjoint
    > 
    > ### ** Examples
    > 
    > result <- conjoint(mp3, rvar = "Rating", evar = "Memory:Shape")
    > plot(result, scale_plot = TRUE)
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: plot ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

# RARfreq

<details>

* Version: 0.1.4
* GitHub: NA
* Source code: https://github.com/cran/RARfreq
* Date/Publication: 2023-04-04 15:00:06 UTC
* Number of recursive dependencies: 70

Run `revdepcheck::cloud_details(, "RARfreq")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘RARfreq-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: SEU_power_comparison_Power_vs_Trt
    > ### Title: Comparison of Powers for Treatment Effects under Different SEU
    > ###   Randomization Methods (Binary Responses)
    > ### Aliases: SEU_power_comparison_Power_vs_Trt
    > 
    > ### ** Examples
    > 
    ...
    5   0.3 One-sided proportion test   0.4
    6   0.4 One-sided proportion test   0.2
    7   0.5 One-sided proportion test   0.2
    8   0.6 One-sided proportion test   0.6
    
    $Plot
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
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
    Error in train(..., self = self) : 
      unused argument (list("CU", "variable", "value", ".ID"))
    Calls: select_functions ... ggplotly.ggplot -> gg2list -> get_gdefs_ggproto -> <Anonymous>
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
      test_select_functions.R.......    0 tests    Error in train(..., self = self) : 
        unused argument (list("CU", "variable", "value", ".ID"))
      Calls: <Anonymous> ... ggplotly.ggplot -> gg2list -> get_gdefs_ggproto -> <Anonymous>
      Execution halted
    ```

# rcssci

<details>

* Version: 0.4.0
* GitHub: https://github.com/popnie/RCSsci
* Source code: https://github.com/cran/rcssci
* Date/Publication: 2023-02-15 21:20:02 UTC
* Number of recursive dependencies: 138

Run `revdepcheck::cloud_details(, "rcssci")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rcssci-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: rcs_cox.lshap
    > ### Title: rcs_cox.lshap
    > ### Aliases: rcs_cox.lshap
    > 
    > ### ** Examples
    > 
    > library(rcssci)
    > rcs_cox.lshap(data=sbpdata, y = "status",x = "sbp",time = "time",
    + prob=0.1,filepath=tempdir())
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: rcs_cox.lshap ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

# redist

<details>

* Version: 4.1.1
* GitHub: https://github.com/alarm-redist/redist
* Source code: https://github.com/cran/redist
* Date/Publication: 2023-04-03 10:50:02 UTC
* Number of recursive dependencies: 141

Run `revdepcheck::cloud_details(, "redist")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘redist-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: compare_plans
    > ### Title: Make a comparison between two sets of plans
    > ### Aliases: compare_plans
    > 
    > ### ** Examples
    > 
    > data(iowa)
    > iowa_map <- redist_map(iowa, ndists = 4, pop_tol = 0.05)
    > plans1 <- redist_smc(iowa_map, 100, silent = TRUE)
    > plans2 <- redist_mergesplit(iowa_map, 200, warmup = 100, silent = TRUE)
    > compare_plans(plans1, plans2, shp = iowa_map)
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘common_args.Rmd’ using rmarkdown
    --- finished re-building ‘common_args.Rmd’
    
    --- re-building ‘map-preproc.Rmd’ using rmarkdown
    trying URL 'https://github.com/alarm-redist/redist-data/raw/main/data/king_county.rds'
    Content type 'application/octet-stream' length 200100 bytes (195 KB)
    ==================================================
    downloaded 195 KB
    
    ...
    Quitting from lines 209-210 [iowa-adj] (redist.Rmd)
    Error: processing vignette 'redist.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘redist.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘map-preproc.Rmd’ ‘mcmc.Rmd’ ‘redist.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 52.7Mb
      sub-directories of 1Mb or more:
        data   1.4Mb
        libs  48.5Mb
    ```

# regressinator

<details>

* Version: 0.1.2
* GitHub: https://github.com/capnrefsmmat/regressinator
* Source code: https://github.com/cran/regressinator
* Date/Publication: 2023-08-11 13:40:02 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::cloud_details(, "regressinator")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘linear-regression-diagnostics.Rmd’ using rmarkdown
    --- finished re-building ‘linear-regression-diagnostics.Rmd’
    
    --- re-building ‘logistic-regression-diagnostics.Rmd’ using rmarkdown
    --- finished re-building ‘logistic-regression-diagnostics.Rmd’
    
    --- re-building ‘other-glm-diagnostics.Rmd’ using rmarkdown
    
    ...
    --- failed re-building ‘other-glm-diagnostics.Rmd’
    
    --- re-building ‘regressinator.Rmd’ using rmarkdown
    --- finished re-building ‘regressinator.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘other-glm-diagnostics.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# rempsyc

<details>

* Version: 0.1.7
* GitHub: https://github.com/rempsyc/rempsyc
* Source code: https://github.com/cran/rempsyc
* Date/Publication: 2023-10-09 22:30:02 UTC
* Number of recursive dependencies: 174

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
    +   grid = FALSE,
    +   shapiro = TRUE
    + )
    + ## Don't show: 
    + }) # examplesIf
    > nice_normality(data = iris, variable = "Sepal.Length", group = "Species")
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
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
      [ FAIL 1 | WARN 0 | SKIP 20 | PASS 45 ]
    ...
        7.         └─base::lapply(x$plots, plot_table, guides = guides)
        8.           ├─patchwork (local) FUN(X[[i]], ...)
        9.           └─patchwork:::plot_table.ggplot(X[[i]], ...)
       10.             └─patchwork:::add_guides(gt, guides == "collect")
       11.               ├─base::unlist(guide_loc == panel_loc)
       12.               └─base::Ops.data.frame(guide_loc, panel_loc)
      
      [ FAIL 1 | WARN 0 | SKIP 20 | PASS 45 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘assumptions.Rmd’ using rmarkdown
    
    Quitting from lines 55-57 [unnamed-chunk-3] (assumptions.Rmd)
    Error: processing vignette 'assumptions.Rmd' failed with diagnostics:
    The viewport is too small to show this set of plots. You may try one of
      the following steps to resolve this problem.
      
    - To fix this issue, please make the window larger.
      
    ...
    --- finished re-building ‘table.Rmd’
    
    --- re-building ‘violin.Rmd’ using rmarkdown
    --- finished re-building ‘violin.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘assumptions.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# reservr

<details>

* Version: 0.0.2
* GitHub: https://github.com/AshesITR/reservr
* Source code: https://github.com/cran/reservr
* Date/Publication: 2023-10-18 20:50:05 UTC
* Number of recursive dependencies: 143

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
    > plot_distributions(
    +   theoretical = dist,
    +   empirical = dist_empirical(x),
    +   .x = seq(0, 20, length.out = 101),
    +   with_params = list(theoretical = params)
    + )
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘distributions.Rmd’ using rmarkdown
    
    Quitting from lines 170-227 [unnamed-chunk-10] (distributions.Rmd)
    Error: processing vignette 'distributions.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
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
      installed size is 24.4Mb
      sub-directories of 1Mb or more:
        R      1.9Mb
        libs  21.9Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# RevGadgets

<details>

* Version: 1.2.1
* GitHub: https://github.com/revbayes/RevGadgets
* Source code: https://github.com/cran/RevGadgets
* Date/Publication: 2023-11-29 20:30:02 UTC
* Number of recursive dependencies: 130

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
       17.                   └─base::mapply(...)
       18.                     └─deeptime (local) `<fn>`(...)
       19.                       └─ggplot2::coord_trans(x = self$trans$x, xlim = lims, ylim = c(0, 1), expand = FALSE)
       20.                         └─ggplot2:::check_coord_limits(xlim)
       21.                           └─cli::cli_abort(...)
       22.                             └─rlang::abort(...)
      
      [ FAIL 1 | WARN 44 | SKIP 0 | PASS 139 ]
      Error: Test failures
      Execution halted
    ```

# rfishdraw

<details>

* Version: 0.1.0
* GitHub: https://github.com/Otoliths/rfishdraw
* Source code: https://github.com/cran/rfishdraw
* Date/Publication: 2021-09-08 09:30:01 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::cloud_details(, "rfishdraw")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘rfishdraw-vegnette.Rmd’ using rmarkdown
    sh: 1: node: not found
    
    Quitting from lines 154-160 [unnamed-chunk-6] (rfishdraw-vegnette.Rmd)
    Error: processing vignette 'rfishdraw-vegnette.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘rfishdraw-vegnette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘rfishdraw-vegnette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# rhierbaps

<details>

* Version: 1.1.4
* GitHub: https://github.com/gtonkinhill/rhierbaps
* Source code: https://github.com/cran/rhierbaps
* Date/Publication: 2022-11-18 14:50:07 UTC
* Number of recursive dependencies: 104

Run `revdepcheck::cloud_details(, "rhierbaps")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘introduction.Rmd’ using rmarkdown
    Read 1 item
    
    Quitting from lines 126-127 [unnamed-chunk-10] (introduction.Rmd)
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# RNAseqQC

<details>

* Version: 0.1.4
* GitHub: NA
* Source code: https://github.com/cran/RNAseqQC
* Date/Publication: 2022-06-15 09:50:06 UTC
* Number of recursive dependencies: 177

Run `revdepcheck::cloud_details(, "RNAseqQC")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘data.Rmd’ using rmarkdown
    --- finished re-building ‘data.Rmd’
    
    --- re-building ‘introduction.Rmd’ using rmarkdown
    
    Quitting from lines 166-167 [unnamed-chunk-18] (introduction.Rmd)
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.8Mb
      sub-directories of 1Mb or more:
        data   7.4Mb
        doc    2.2Mb
    ```

# robber

<details>

* Version: 0.2.3
* GitHub: https://github.com/Chabert-Liddell/robber
* Source code: https://github.com/cran/robber
* Date/Publication: 2023-02-28 08:32:35 UTC
* Number of recursive dependencies: 145

Run `revdepcheck::cloud_details(, "robber")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘topological-analysis.Rmd’ using rmarkdown
    
    Quitting from lines 432-470 [print_topo] (topological-analysis.Rmd)
    Error: processing vignette 'topological-analysis.Rmd' failed with diagnostics:
    cannot coerce type 'environment' to vector of type 'character'
    --- failed re-building ‘topological-analysis.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘topological-analysis.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# rock

<details>

* Version: 0.6.7
* GitHub: NA
* Source code: https://github.com/cran/rock
* Date/Publication: 2022-12-13 12:30:02 UTC
* Number of recursive dependencies: 141

Run `revdepcheck::cloud_details(, "rock")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘introduction_to_rock.Rmd’ using rmarkdown
    ! kpathsea: Running mktexfmt pdflatex.fmt
    
    ! /usr/bin/mktexfmt: kpsewhich -var-value=TEXMFROOT failed, aborting early.
    
    ! BEGIN failed--compilation aborted at /usr/bin/mktexfmt line 28.
    
    Error: processing vignette 'introduction_to_rock.Rmd' failed with diagnostics:
    LaTeX failed to compile /tmp/workdir/rock/new/rock.Rcheck/vign_test/rock/vignettes/introduction_to_rock.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See introduction_to_rock.log for more info.
    --- failed re-building ‘introduction_to_rock.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘introduction_to_rock.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## Newly fixed

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘introduction_to_rock.Rmd’ using rmarkdown
    Could not determine directory of tlmgr executable, maybe shared library woes?
    Check for error messages above at /usr/local/bin/tlmgr line 75.
    BEGIN failed--compilation aborted at /usr/local/bin/tlmgr line 84.
    Warning in system2("tlmgr", args, ...) :
      running command ''tlmgr' search --file --global '/grffile.sty'' had status 1
    ! LaTeX Error: File `grffile.sty' not found.
    
    ...
    
    Error: processing vignette 'introduction_to_rock.Rmd' failed with diagnostics:
    LaTeX failed to compile /tmp/workdir/rock/old/rock.Rcheck/vign_test/rock/vignettes/introduction_to_rock.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See introduction_to_rock.log for more info.
    --- failed re-building ‘introduction_to_rock.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘introduction_to_rock.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# romic

<details>

* Version: 1.1.3
* GitHub: NA
* Source code: https://github.com/cran/romic
* Date/Publication: 2023-09-21 05:40:02 UTC
* Number of recursive dependencies: 114

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
        5. │ ├─plotly::ggplotly(heatmap_plot)
        6. │ └─plotly:::ggplotly.ggplot(heatmap_plot)
        7. │   └─plotly::gg2list(...)
        8. │     └─plotly:::get_gdefs_ggproto(npscales$scales, theme, plot, layers)
        9. │       └─guides$train(scales, theme$legend.direction, plot$labels)
       10. └─plotly::layout(., margin = 0)
      
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
* Number of recursive dependencies: 71

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
    Error in train(..., self = self) : 
      unused argument (list("stock price at expiration", "profit/loss", "Option contract", "Box Spread Plot"))
    Calls: box.spread ... ggplotly.ggplot -> gg2list -> get_gdefs_ggproto -> <Anonymous>
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# rTPC

<details>

* Version: 1.0.4
* GitHub: https://github.com/padpadpadpad/rTPC
* Source code: https://github.com/cran/rTPC
* Date/Publication: 2023-08-17 06:40:06 UTC
* Number of recursive dependencies: 139

Run `revdepcheck::cloud_details(, "rTPC")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘bootstrapping_many_curves.Rmd’ using rmarkdown
    --- finished re-building ‘bootstrapping_many_curves.Rmd’
    
    --- re-building ‘bootstrapping_models.Rmd’ using rmarkdown
    
    Quitting from lines 135-173 [plot_boots] (bootstrapping_models.Rmd)
    Error: processing vignette 'bootstrapping_models.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘bootstrapping_models.Rmd’
    ...
    Quitting from lines 123-173 [bootstrap1] (weighted_bootstrapping.Rmd)
    Error: processing vignette 'weighted_bootstrapping.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘weighted_bootstrapping.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘bootstrapping_models.Rmd’ ‘weighted_bootstrapping.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# santaR

<details>

* Version: 1.2.3
* GitHub: https://github.com/adwolfer/santaR
* Source code: https://github.com/cran/santaR
* Date/Publication: 2022-05-23 23:20:06 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::cloud_details(, "santaR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(santaR)
      
      This is santaR version 1.2.3 
      
      > 
      > test_check("santaR")
    ...
      1/1 mismatches
      [1] 11 - 9 == 2
      ── Failure ('test_dfSearch-plot_nbTP_histogram.R:69:3'): change dfCuttOff ──────
      length(result_nbTPHisto) not equal to 9.
      1/1 mismatches
      [1] 11 - 9 == 2
      
      [ FAIL 8 | WARN 2 | SKIP 0 | PASS 681 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DT’ ‘gridExtra’
      All declared Imports should be used.
    ```

# scCustomize

<details>

* Version: 2.0.1
* GitHub: https://github.com/samuel-marsh/scCustomize
* Source code: https://github.com/cran/scCustomize
* Date/Publication: 2023-11-17 19:00:03 UTC
* Number of recursive dependencies: 267

Run `revdepcheck::cloud_details(, "scCustomize")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘scCustomize-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Cell_Highlight_Plot
    > ### Title: Meta Highlight Plot
    > ### Aliases: Cell_Highlight_Plot
    > 
    > ### ** Examples
    > 
    > library(Seurat)
    ...
    > 
    > # Format as named list
    > cells <- list("MS4A1" = MS4A1,
    +               "GZMB" = GZMB)
    > 
    > Cell_Highlight_Plot(seurat_object = pbmc_small, cells_highlight = cells)
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

# scdhlm

<details>

* Version: 0.7.2
* GitHub: https://github.com/jepusto/scdhlm
* Source code: https://github.com/cran/scdhlm
* Date/Publication: 2023-03-12 10:30:02 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::cloud_details(, "scdhlm")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(scdhlm)
      Loading required package: nlme
      > 
      > test_check("scdhlm")
      [ FAIL 3 | WARN 0 | SKIP 11 | PASS 261 ]
      
    ...
       17.                       ├─base::all.equal(...)
       18.                       └─base::all.equal.default(...)
       19.                         └─base::all.equal.list(target, current, ...)
       20.                           ├─base::all.equal(...)
       21.                           └─base::all.equal.environment(...)
       22.                             └─base::as.list.environment(target, all.names = all.names, sorted = TRUE)
      
      [ FAIL 3 | WARN 0 | SKIP 11 | PASS 261 ]
      Error: Test failures
      Execution halted
    ```

# scRNAstat

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/scRNAstat
* Date/Publication: 2021-09-22 08:10:02 UTC
* Number of recursive dependencies: 156

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
    Scale for y is already present.
    Adding another scale for y, which will replace the existing scale.
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: basic_filter ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

## In both

*   checking whether package ‘scRNAstat’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: namespace ‘DBI’ is not available and has been replaced
    See ‘/tmp/workdir/scRNAstat/new/scRNAstat.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.7Mb
      sub-directories of 1Mb or more:
        data   7.6Mb
    ```

# SCVA

<details>

* Version: 1.3.1
* GitHub: NA
* Source code: https://github.com/cran/SCVA
* Date/Publication: 2020-01-09 22:50:10 UTC
* Number of recursive dependencies: 81

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
    Error in train(..., self = self) : 
      unused argument (list("Scores", "Measurement Times", "lvl", "lvl", "lvl", "xintercept"))
    Calls: graphly ... ggplotly.ggplot -> gg2list -> get_gdefs_ggproto -> <Anonymous>
    Execution halted
    ```

# see

<details>

* Version: 0.8.1
* GitHub: https://github.com/easystats/see
* Source code: https://github.com/cran/see
* Date/Publication: 2023-11-03 12:20:02 UTC
* Number of recursive dependencies: 221

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
     13. │         │ └─base::withCallingHandlers(...)
     14. │         └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     15. │           └─l$compute_aesthetics(d, plot)
     16. │             └─ggplot2 (local) compute_aesthetics(..., self = self)
     17. └─base::.handleSimpleError(...)
     18.   └─rlang (local) h(simpleError(msg, call))
     19.     └─handlers[[1L]](cnd)
     20.       └─cli::cli_abort(...)
     21.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘rstanarm’
    ```

# sfdep

<details>

* Version: 0.2.3
* GitHub: https://github.com/josiahparry/sfdep
* Source code: https://github.com/cran/sfdep
* Date/Publication: 2023-01-11 06:30:02 UTC
* Number of recursive dependencies: 101

Run `revdepcheck::cloud_details(, "sfdep")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘basics-of-sfdep.Rmd’ using rmarkdown
    
    Quitting from lines 79-115 [unnamed-chunk-3] (basics-of-sfdep.Rmd)
    Error: processing vignette 'basics-of-sfdep.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘basics-of-sfdep.Rmd’
    
    --- re-building ‘conditional-permutation.Rmd’ using rmarkdown
    ...
    --- finished re-building ‘spacetime-s3.Rmd’
    
    --- re-building ‘spdep-and-pysal.Rmd’ using rmarkdown
    --- finished re-building ‘spdep-and-pysal.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘basics-of-sfdep.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# shadowtext

<details>

* Version: 0.1.2
* GitHub: https://github.com/GuangchuangYu/shadowtext
* Source code: https://github.com/cran/shadowtext
* Date/Publication: 2022-04-22 09:10:02 UTC
* Number of recursive dependencies: 53

Run `revdepcheck::cloud_details(, "shadowtext")` for more info

</details>

## Newly broken

*   checking whether package ‘shadowtext’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/shadowtext/new/shadowtext.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘shadowtext’ ...
** package ‘shadowtext’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in get(x, envir = ns, inherits = FALSE) : 
  object 'add_margins' not found
Error: unable to load R code in package ‘shadowtext’
Execution halted
ERROR: lazy loading failed for package ‘shadowtext’
* removing ‘/tmp/workdir/shadowtext/new/shadowtext.Rcheck/shadowtext’


```
### CRAN

```
* installing *source* package ‘shadowtext’ ...
** package ‘shadowtext’ successfully unpacked and MD5 sums checked
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
* DONE (shadowtext)


```
# shapviz

<details>

* Version: 0.9.2
* GitHub: https://github.com/ModelOriented/shapviz
* Source code: https://github.com/cran/shapviz
* Date/Publication: 2023-10-14 17:00:02 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::cloud_details(, "shapviz")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘basic_use.Rmd’ using rmarkdown
    
    Quitting from lines 167-172 [unnamed-chunk-9] (basic_use.Rmd)
    Error: processing vignette 'basic_use.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘basic_use.Rmd’
    
    --- re-building ‘geographic.Rmd’ using rmarkdown
    
    ...
    Quitting from lines 43-70 [unnamed-chunk-2] (multiple_output.Rmd)
    Error: processing vignette 'multiple_output.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘multiple_output.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘basic_use.Rmd’ ‘geographic.Rmd’ ‘multiple_output.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      'fastshap', 'h2o', 'lightgbm'
    ```

# shinipsum

<details>

* Version: 0.1.0
* GitHub: https://github.com/Thinkr-open/shinipsum
* Source code: https://github.com/cran/shinipsum
* Date/Publication: 2020-04-30 18:40:03 UTC
* Number of recursive dependencies: 100

Run `revdepcheck::cloud_details(, "shinipsum")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(shinipsum)
      > 
      > test_check("shinipsum")
      [ FAIL 100 | WARN 1 | SKIP 0 | PASS 2300 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      `a` has length 11, not length 9.
      Backtrace:
          ▆
       1. └─base::lapply(...) at test-ggplot.R:4:3
       2.   └─shinipsum (local) FUN(X[[i]], ...)
       3.     └─testthat::expect_length(a, 9) at test-ggplot.R:9:7
      
      [ FAIL 100 | WARN 1 | SKIP 0 | PASS 2300 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

# shiny

<details>

* Version: 1.8.0
* GitHub: https://github.com/rstudio/shiny
* Source code: https://github.com/cran/shiny
* Date/Publication: 2023-11-17 23:40:02 UTC
* Number of recursive dependencies: 92

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
      [ FAIL 2 | WARN 0 | SKIP 14 | PASS 1575 ]
      
      ══ Skipped tests (14) ══════════════════════════════════════════════════════════
    ...
      `expected$top`:  10
      ── Failure ('test-plot-coordmap.R:360:3'): ggplot coordmap with various scales and coords ──
      sortList(m$panels[[1]]$log) (`actual`) not equal to sortList(list(x = 10, y = 2)) (`expected`).
      
      `actual$x` is NULL
      `expected$x` is a double vector (10)
      
      [ FAIL 2 | WARN 0 | SKIP 14 | PASS 1575 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 12.9Mb
      sub-directories of 1Mb or more:
        R      2.0Mb
        help   1.2Mb
        www    9.3Mb
    ```

# simmr

<details>

* Version: 0.5.1.216
* GitHub: https://github.com/andrewcparnell/simmr
* Source code: https://github.com/cran/simmr
* Date/Publication: 2023-10-27 14:20:02 UTC
* Number of recursive dependencies: 129

Run `revdepcheck::cloud_details(, "simmr")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘advanced_plotting.Rmd’ using rmarkdown
    
    Quitting from lines 187-213 [unnamed-chunk-13] (advanced_plotting.Rmd)
    Error: processing vignette 'advanced_plotting.Rmd' failed with diagnostics:
    comparison (==) is possible only for atomic and list types
    --- failed re-building ‘advanced_plotting.Rmd’
    
    --- re-building ‘quick_start.Rmd’ using rmarkdown
    ...
    --- finished re-building ‘quick_start.Rmd’
    
    --- re-building ‘simmr.Rmd’ using rmarkdown
    --- finished re-building ‘simmr.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘advanced_plotting.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.9Mb
      sub-directories of 1Mb or more:
        doc    1.6Mb
        libs   5.9Mb
    ```

# simplevis

<details>

* Version: 7.1.0
* GitHub: https://github.com/StatisticsNZ/simplevis
* Source code: https://github.com/cran/simplevis
* Date/Publication: 2023-05-09 04:00:02 UTC
* Number of recursive dependencies: 130

Run `revdepcheck::cloud_details(, "simplevis")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘simplevis-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: add_tooltip
    > ### Title: Add a tooltip column
    > ### Aliases: add_tooltip
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
    > 
    > p
    > 
    > if (requireNamespace("plotly", quietly = TRUE)) {
    +   plotly::ggplotly(p, tooltip = "text")
    + }
    Error in train(..., self = self) : 
      unused argument (list("Sepal.Width", "Sepal.Length", "Species", "tooltip"))
    Calls: <Anonymous> ... ggplotly.ggplot -> gg2list -> get_gdefs_ggproto -> <Anonymous>
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: 'rgdal', 'rgeos'
    ```

# SimplyAgree

<details>

* Version: 0.1.2
* GitHub: NA
* Source code: https://github.com/cran/SimplyAgree
* Date/Publication: 2022-12-15 00:50:02 UTC
* Number of recursive dependencies: 108

Run `revdepcheck::cloud_details(, "SimplyAgree")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Deming.Rmd’ using rmarkdown
    
    Quitting from lines 72-73 [unnamed-chunk-4] (Deming.Rmd)
    Error: processing vignette 'Deming.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘Deming.Rmd’
    
    --- re-building ‘agreement_analysis.Rmd’ using rmarkdown
    
    ...
    --- finished re-building ‘reanalysis.Rmd’
    
    --- re-building ‘reliability_analysis.Rmd’ using rmarkdown
    --- finished re-building ‘reliability_analysis.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘Deming.Rmd’ ‘agreement_analysis.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# singleCellHaystack

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/singleCellHaystack
* Date/Publication: 2022-12-20 10:00:02 UTC
* Number of recursive dependencies: 118

Run `revdepcheck::cloud_details(, "singleCellHaystack")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘a01_toy_example.Rmd’ using rmarkdown
    
      |                                                                            
      |                                                                      |   0%
      |                                                                            
      |=                                                                     |   2%
      |                                                                            
      |===                                                                   |   4%
      |                                                                            
    ...
    Quitting from lines 113-118 [unnamed-chunk-6] (a01_toy_example.Rmd)
    Error: processing vignette 'a01_toy_example.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘a01_toy_example.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘a01_toy_example.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# smallsets

<details>

* Version: 2.0.0
* GitHub: https://github.com/lydialucchesi/smallsets
* Source code: https://github.com/cran/smallsets
* Date/Publication: 2023-12-05 00:00:02 UTC
* Number of recursive dependencies: 108

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
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘smallsets.Rmd’ using rmarkdown
    
    Quitting from lines 36-42 [timeline1] (smallsets.Rmd)
    Error: processing vignette 'smallsets.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
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

# spinifex

<details>

* Version: 0.3.6
* GitHub: https://github.com/nspyrison/spinifex
* Source code: https://github.com/cran/spinifex
* Date/Publication: 2022-03-31 08:30:02 UTC
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
      spinifex --- version 0.3.6
      Please share bugs, suggestions, and feature requests at:
    ...
      1/1 mismatches
      [1] 11 - 9 == 2
      ── Failure ('test-zDepricated_3_visualize.r:102:3'): view_manip_space: gganimate class and length ──
      length(ret_heavy) not equal to 9L.
      1/1 mismatches
      [1] 11 - 9 == 2
      
      [ FAIL 10 | WARN 11 | SKIP 0 | PASS 154 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) PimaIndiansDiabetes_long.Rd:14: Escaped LaTeX specials: \^
    checkRd: (-1) PimaIndiansDiabetes_wide.Rd:16: Escaped LaTeX specials: \^
    ```

# spotoroo

<details>

* Version: 0.1.4
* GitHub: https://github.com/TengMCing/spotoroo
* Source code: https://github.com/cran/spotoroo
* Date/Publication: 2023-08-21 05:50:02 UTC
* Number of recursive dependencies: 110

Run `revdepcheck::cloud_details(, "spotoroo")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Clustering-hot-spots.Rmd’ using rmarkdown
    
    Quitting from lines 383-386 [unnamed-chunk-23] (Clustering-hot-spots.Rmd)
    Error: processing vignette 'Clustering-hot-spots.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘Clustering-hot-spots.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Clustering-hot-spots.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

# spqdep

<details>

* Version: 0.1.2
* GitHub: NA
* Source code: https://github.com/cran/spqdep
* Date/Publication: 2022-03-28 16:20:02 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::cloud_details(, "spqdep")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘spq_userguide.Rmd’ using rmarkdown
    
    Quitting from lines 377-378 [unnamed-chunk-24] (spq_userguide.Rmd)
    Error: processing vignette 'spq_userguide.Rmd' failed with diagnostics:
    Invalid index: field name 'x_start' not found
    --- failed re-building ‘spq_userguide.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘spq_userguide.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘lwgeom’ ‘rgeoda’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4 marked UTF-8 strings
    ```

# stars

<details>

* Version: 0.6-4
* GitHub: https://github.com/r-spatial/stars
* Source code: https://github.com/cran/stars
* Date/Publication: 2023-09-11 11:50:02 UTC
* Number of recursive dependencies: 163

Run `revdepcheck::cloud_details(, "stars")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘stars1.Rmd’ using rmarkdown
    --- finished re-building ‘stars1.Rmd’
    
    --- re-building ‘stars2.Rmd’ using rmarkdown
    --- finished re-building ‘stars2.Rmd’
    
    --- re-building ‘stars3.Rmd’ using rmarkdown
    Killed
    --- re-building ‘stars4.Rmd’ using rmarkdown
    ...
    --- finished re-building ‘stars7.Rmd’
    
    --- re-building ‘stars8.Rmd’ using rmarkdown
    --- finished re-building ‘stars8.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘stars3.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        doc   2.4Mb
        nc    1.7Mb
    ```

# suddengains

<details>

* Version: 0.7.2
* GitHub: https://github.com/milanwiedemann/suddengains
* Source code: https://github.com/cran/suddengains
* Date/Publication: 2023-02-28 08:30:02 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::cloud_details(, "suddengains")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘suddengains-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_sg_intervals
    > ### Title: Plot summary of available data per time point and analysed
    > ###   session to session intervals
    > ### Aliases: plot_sg_intervals
    > 
    > ### ** Examples
    > 
    ...
    > plot_sg_intervals(data = sgdata,
    +                   id_var_name = "id",
    +                   sg_var_list = c("bdi_s1", "bdi_s2", "bdi_s3",
    +                                   "bdi_s4", "bdi_s5", "bdi_s6",
    +                                   "bdi_s7", "bdi_s8", "bdi_s9",
    +                                   "bdi_s10", "bdi_s11", "bdi_s12"))
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘shinygains.Rmd’ using rmarkdown
    --- finished re-building ‘shinygains.Rmd’
    
    --- re-building ‘suddengains-tutorial.Rmd’ using rmarkdown
    
    Quitting from lines 793-798 [unnamed-chunk-32] (suddengains-tutorial.Rmd)
    Error: processing vignette 'suddengains-tutorial.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘suddengains-tutorial.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘suddengains-tutorial.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘cli’
      All declared Imports should be used.
    ```

# superb

<details>

* Version: 0.95.7
* GitHub: https://github.com/dcousin3/superb
* Source code: https://github.com/cran/superb
* Date/Publication: 2023-01-22 20:50:02 UTC
* Number of recursive dependencies: 120

Run `revdepcheck::cloud_details(, "superb")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > test_check("superb")
      Loading required package: superb
      ==> THIS IS A TEST OF runDebug <==
      [ FAIL 5 | WARN 0 | SKIP 0 | PASS 167 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      Actual value: "List of 11\\n \$ data       :'data\.frame':\\t6 obs\. of  5 variables:\\n  \.\.\$ dose      : num \[1:6\] 0\.5 0\.5 1 1 2 2\\n  \.\.\$ supp      : chr \[1:6\] "OJ" "VC" "OJ" "VC" \.\.\.\\n  \.\.\$ center    : num \[1:6\] 13 8 22 17 26 26\\n  \.\.\$ lowerwidth: num \[1:6\] -3 -2 -3 -2 -2 -4\\n  \.\.\$ upperwidth: num \[1:6\] 3 2 3 2 2 4\\n \$ layers     :List of 4\\n  \.\.\$ :Classes 'LayerInstance', 'Layer', 'ggproto', 'gg' <ggproto object: Class LayerInstance, Layer, gg>\\n    aes_params: list\\n    compute_aesthetics: function\\n    compute_geom_1: function\\n    compute_geom_2: function\\n    compute_position: function\\n    compute_statistic: function\\n    computed_geom_params: NULL\\n    computed_mapping: NULL\\n    computed_stat_params: NULL\\n    constructor: call\\n    data: data\.frame\\n    draw_geom: function\\n    finish_statistics: function\\n    geom: <ggproto object: Class GeomViolin, Geom, gg>\\n        aesthetics: function\\n        default_aes: uneval\\n        draw_group: function\\n        draw_key: function\\n        draw_layer: function\\n        draw_panel: function\\n        extra_params: na\.rm orientation lineend linejoin linemitre\\n        handle_na: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        rename_size: TRUE\\n        required_aes: x y\\n        setup_data: function\\n        setup_params: function\\n        use_defaults: function\\n        super:  <ggproto object: Class Geom, gg>\\n    geom_params: list\\n    inherit\.aes: TRUE\\n    layer_data: function\\n    map_statistic: function\\n    mapping: uneval\\n    position: <ggproto object: Class PositionDodge, Position, gg>\\n        compute_layer: function\\n        compute_panel: function\\n        preserve: total\\n        required_aes: \\n        setup_data: function\\n        setup_params: function\\n        width: 0\.75\\n        super:  <ggproto object: Class PositionDodge, Position, gg>\\n    print: function\\n    setup_layer: function\\n    show\.legend: NA\\n    stat: <ggproto object: Class StatYdensity, Stat, gg>\\n        aesthetics: function\\n        compute_group: function\\n        compute_layer: function\\n        compute_panel: function\\n        default_aes: uneval\\n        dropped_aes: weight\\n        extra_params: na\.rm orientation\\n        finish_layer: function\\n        non_missing_aes: weight\\n        optional_aes: \\n        parameters: function\\n        required_aes: x y\\n        retransform: TRUE\\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Stat, gg>\\n    stat_params: list\\n    super:  <ggproto object: Class Layer, gg> \\n  \.\.\$ :Classes 'LayerInstance', 'Layer', 'ggproto', 'gg' <ggproto object: Class LayerInstance, Layer, gg>\\n    aes_params: list\\n    compute_aesthetics: function\\n    compute_geom_1: function\\n    compute_geom_2: function\\n    compute_position: function\\n    compute_statistic: function\\n    computed_geom_params: NULL\\n    computed_mapping: NULL\\n    computed_stat_params: NULL\\n    constructor: call\\n    data: data\.frame\\n    draw_geom: function\\n    finish_statistics: function\\n    geom: <ggproto object: Class GeomPoint, Geom, gg>\\n        aesthetics: function\\n        default_aes: uneval\\n        draw_group: function\\n        draw_key: function\\n        draw_layer: function\\n        draw_panel: function\\n        extra_params: na\.rm\\n        handle_na: function\\n        non_missing_aes: size shape colour\\n        optional_aes: \\n        parameters: function\\n        rename_size: FALSE\\n        required_aes: x y\\n        setup_data: function\\n        setup_params: function\\n        use_defaults: function\\n        super:  <ggproto object: Class Geom, gg>\\n    geom_params: list\\n    inherit\.aes: TRUE\\n    layer_data: function\\n    map_statistic: function\\n    mapping: uneval\\n    position: <ggproto object: Class PositionJitterdodge, Position, gg>\\n        compute_layer: function\\n        compute_panel: function\\n        dodge\.width: 0\.75\\n        jitter\.height: 0\\n        jitter\.width: 0\.1\\n        required_aes: x y\\n        seed: 723969313\\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class PositionJitterdodge, Position, gg>\\n    print: function\\n    setup_layer: function\\n    show\.legend: NA\\n    stat: <ggproto object: Class StatIdentity, Stat, gg>\\n        aesthetics: function\\n        compute_group: function\\n        compute_layer: function\\n        compute_panel: function\\n        default_aes: uneval\\n        dropped_aes: \\n        extra_params: na\.rm\\n        finish_layer: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        required_aes: \\n        retransform: TRUE\\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Stat, gg>\\n    stat_params: list\\n    super:  <ggproto object: Class Layer, gg> \\n  \.\.\$ :Classes 'LayerInstance', 'Layer', 'ggproto', 'gg' <ggproto object: Class LayerInstance, Layer, gg>\\n    aes_params: list\\n    compute_aesthetics: function\\n    compute_geom_1: function\\n    compute_geom_2: function\\n    compute_position: function\\n    compute_statistic: function\\n    computed_geom_params: NULL\\n    computed_mapping: NULL\\n    computed_stat_params: NULL\\n    constructor: call\\n    data: waiver\\n    draw_geom: function\\n    finish_statistics: function\\n    geom: <ggproto object: Class GeomPoint, Geom, gg>\\n        aesthetics: function\\n        default_aes: uneval\\n        draw_group: function\\n        draw_key: function\\n        draw_layer: function\\n        draw_panel: function\\n        extra_params: na\.rm\\n        handle_na: function\\n        non_missing_aes: size shape colour\\n        optional_aes: \\n        parameters: function\\n        rename_size: FALSE\\n        required_aes: x y\\n        setup_data: function\\n        setup_params: function\\n        use_defaults: function\\n        super:  <ggproto object: Class Geom, gg>\\n    geom_params: list\\n    inherit\.aes: TRUE\\n    layer_data: function\\n    map_statistic: function\\n    mapping: uneval\\n    position: <ggproto object: Class PositionDodge, Position, gg>\\n        compute_layer: function\\n        compute_panel: function\\n        preserve: total\\n        required_aes: \\n        setup_data: function\\n        setup_params: function\\n        width: 0\.75\\n        super:  <ggproto object: Class PositionDodge, Position, gg>\\n    print: function\\n    setup_layer: function\\n    show\.legend: NA\\n    stat: <ggproto object: Class StatIdentity, Stat, gg>\\n        aesthetics: function\\n        compute_group: function\\n        compute_layer: function\\n        compute_panel: function\\n        default_aes: uneval\\n        dropped_aes: \\n        extra_params: na\.rm\\n        finish_layer: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        required_aes: \\n        retransform: TRUE\\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Stat, gg>\\n    stat_params: list\\n    super:  <ggproto object: Class Layer, gg> \\n  \.\.\$ :Classes 'LayerInstance', 'Layer', 'ggproto', 'gg' <ggproto object: Class LayerInstance, Layer, gg>\\n    aes_params: list\\n    compute_aesthetics: function\\n    compute_geom_1: function\\n    compute_geom_2: function\\n    compute_position: function\\n    compute_statistic: function\\n    computed_geom_params: NULL\\n    computed_mapping: NULL\\n    computed_stat_params: NULL\\n    constructor: call\\n    data: waiver\\n    draw_geom: function\\n    finish_statistics: function\\n    geom: <ggproto object: Class GeomsuperbErrorbar, Geom, gg>\\n        aesthetics: function\\n        default_aes: uneval\\n        draw_group: function\\n        draw_key: function\\n        draw_layer: function\\n        draw_panel: function\\n        extra_params: na\.rm orientation direction tipformat tipgap pointing\\n        handle_na: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        rename_size: FALSE\\n        required_aes: x\|y ymin\|xmin ymax\|xmax\\n        setup_data: function\\n        setup_params: function\\n        use_defaults: function\\n        super:  <ggproto object: Class Geom, gg>\\n    geom_params: list\\n    inherit\.aes: TRUE\\n    layer_data: function\\n    map_statistic: function\\n    mapping: uneval\\n    position: <ggproto object: Class PositionDodge, Position, gg>\\n        compute_layer: function\\n        compute_panel: function\\n        preserve: total\\n        required_aes: \\n        setup_data: function\\n        setup_params: function\\n        width: 0\.75\\n        super:  <ggproto object: Class PositionDodge, Position, gg>\\n    print: function\\n    setup_layer: function\\n    show\.legend: NA\\n    stat: <ggproto object: Class StatIdentity, Stat, gg>\\n        aesthetics: function\\n        compute_group: function\\n        compute_layer: function\\n        compute_panel: function\\n        default_aes: uneval\\n        dropped_aes: \\n        extra_params: na\.rm\\n        finish_layer: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        required_aes: \\n        retransform: TRUE\\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Stat, gg>\\n    stat_params: list\\n    super:  <ggproto object: Class Layer, gg> \\n \$ scales     :Classes 'ScalesList', 'ggproto', 'gg' <ggproto object: Class ScalesList, gg>\\n    add: function\\n    add_defaults: function\\n    add_missing: function\\n    backtransform_df: function\\n    clone: function\\n    find: function\\n    get_scales: function\\n    has_scale: function\\n    input: function\\n    map_df: function\\n    n: function\\n    non_position_scales: function\\n    scales: list\\n    train_df: function\\n    transform_df: function\\n    super:  <ggproto object: Class ScalesList, gg> \\n \$ guides     :Classes 'Guides', 'ggproto', 'gg' <ggproto object: Class Guides, gg>\\n    add: function\\n    assemble: function\\n    build: function\\n    draw: function\\n    get_custom: function\\n    get_guide: function\\n    get_params: function\\n    get_position: function\\n    guides: NULL\\n    merge: function\\n    missing: <ggproto object: Class GuideNone, Guide, gg>\\n        arrange_layout: function\\n        assemble_drawing: function\\n        available_aes: any\\n        build_decor: function\\n        build_labels: function\\n        build_ticks: function\\n        build_title: function\\n        draw: function\\n        draw_early_exit: function\\n        elements: list\\n        extract_decor: function\\n        extract_key: function\\n        extract_params: function\\n        get_layer_key: function\\n        hashables: list\\n        measure_grobs: function\\n        merge: function\\n        override_elements: function\\n        params: list\\n        process_layers: function\\n        setup_elements: function\\n        setup_params: function\\n        train: function\\n        transform: function\\n        super:  <ggproto object: Class GuideNone, Guide, gg>\\n    package_box: function\\n    print: function\\n    process_layers: function\\n    setup: function\\n    subset_guides: function\\n    train: function\\n    update_params: function\\n    super:  <ggproto object: Class Guides, gg> \\n \$ mapping    :List of 2\\n  \.\.\$ x     : language ~dose\\n  \.\. \.\.- attr\(\*, "\.Environment"\)=<environment: 0x55fb05104358> \\n  \.\.\$ colour: language ~supp\\n  \.\. \.\.- attr\(\*, "\.Environment"\)=<environment: 0x55fb05104358> \\n  \.\.- attr\(\*, "class"\)= chr "uneval"\\n \$ theme      : list\(\)\\n \$ coordinates:Classes 'CoordCartesian', 'Coord', 'ggproto', 'gg' <ggproto object: Class CoordCartesian, Coord, gg>\\n    aspect: function\\n    backtransform_range: function\\n    clip: on\\n    default: TRUE\\n    distance: function\\n    expand: TRUE\\n    is_free: function\\n    is_linear: function\\n    labels: function\\n    limits: list\\n    modify_scales: function\\n    range: function\\n    render_axis_h: function\\n    render_axis_v: function\\n    render_bg: function\\n    render_fg: function\\n    setup_data: function\\n    setup_layout: function\\n    setup_panel_guides: function\\n    setup_panel_params: function\\n    setup_params: function\\n    train_panel_guides: function\\n    transform: function\\n    super:  <ggproto object: Class CoordCartesian, Coord, gg> \\n \$ facet      :Classes 'FacetGrid', 'Facet', 'ggproto', 'gg' <ggproto object: Class FacetGrid, Facet, gg>\\n    compute_layout: function\\n    draw_back: function\\n    draw_front: function\\n    draw_labels: function\\n    draw_panels: function\\n    finish_data: function\\n    init_scales: function\\n    map_data: function\\n    params: list\\n    setup_data: function\\n    setup_params: function\\n    shrink: TRUE\\n    train_scales: function\\n    vars: function\\n    super:  <ggproto object: Class FacetGrid, Facet, gg> \\n \$ plot_env   :<environment: 0x55fb05104358> \\n \$ layout     :Classes 'Layout', 'ggproto', 'gg' <ggproto object: Class Layout, gg>\\n    coord: NULL\\n    coord_params: list\\n    facet: NULL\\n    facet_params: list\\n    finish_data: function\\n    get_scales: function\\n    layout: NULL\\n    map_position: function\\n    panel_params: NULL\\n    panel_scales_x: NULL\\n    panel_scales_y: NULL\\n    render: function\\n    render_labels: function\\n    reset_scales: function\\n    resolve_label: function\\n    setup: function\\n    setup_panel_guides: function\\n    setup_panel_params: function\\n    train_position: function\\n    super:  <ggproto object: Class Layout, gg> \\n \$ labels     :List of 7\\n  \.\.\$ x     : chr "dose"\\n  \.\.\$ colour: chr "supp"\\n  \.\.\$ y     : chr "center"\\n  \.\.\$ fill  : chr "supp"\\n  \.\.\$ group : chr "supp"\\n  \.\.\$ ymin  : chr "center \+ lowerwidth"\\n  \.\.\$ ymax  : chr "center \+ upperwidth"\\n - attr\(\*, "class"\)= chr \[1:2\] "gg" "ggplot""
      Backtrace:
          ▆
       1. └─testthat::expect_output(str(p5), "List of 9") at test_subsidiaryFunctions.R:109:5
       2.   └─testthat::expect_match(...)
       3.     └─testthat:::expect_match_(...)
      
      [ FAIL 5 | WARN 0 | SKIP 0 | PASS 167 ]
      Error: Test failures
      Execution halted
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
    Warning in min(freq[grepl(i, allwords, ignore.case = T)]) :
      no non-missing arguments to min; returning Inf
    Warning in min(freq[grepl(i, allwords, ignore.case = T)]) :
      no non-missing arguments to min; returning Inf
    Warning in min(freq[grepl(i, allwords, ignore.case = T)]) :
      no non-missing arguments to min; returning Inf
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: Sysrecon ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 38 marked UTF-8 strings
    ```

# TCIU

<details>

* Version: 1.2.4
* GitHub: https://github.com/SOCR/TCIU
* Source code: https://github.com/cran/TCIU
* Date/Publication: 2023-10-06 16:20:11 UTC
* Number of recursive dependencies: 172

Run `revdepcheck::cloud_details(, "TCIU")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘tciu-LT-kimesurface.Rmd’ using rmarkdown
    
    Quitting from lines 159-160 [unnamed-chunk-5] (tciu-LT-kimesurface.Rmd)
    Error: processing vignette 'tciu-LT-kimesurface.Rmd' failed with diagnostics:
    Problem while computing aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `scales_add_defaults()`:
    ! could not find function "scales_add_defaults"
    ...
    --- failed re-building ‘tciu-LT-kimesurface.Rmd’
    
    --- re-building ‘tciu-fMRI-analytics.Rmd’ using rmarkdown
    --- finished re-building ‘tciu-fMRI-analytics.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘tciu-LT-kimesurface.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 14.6Mb
      sub-directories of 1Mb or more:
        data   1.8Mb
        doc   12.3Mb
    ```

# tern

<details>

* Version: 0.9.3
* GitHub: https://github.com/insightsengineering/tern
* Source code: https://github.com/cran/tern
* Date/Publication: 2023-12-08 16:20:03 UTC
* Number of recursive dependencies: 108

Run `revdepcheck::cloud_details(, "tern")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tern-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: h_g_ipp
    > ### Title: Helper Function To Create Simple Line Plot over Time
    > ### Aliases: h_g_ipp
    > 
    > ### ** Examples
    > 
    > library(dplyr)
    ...
    +   yvar = "AVAL",
    +   xlab = "Visit",
    +   id_var = "USUBJID",
    +   ylab = "SGOT/ALT (U/L)",
    +   add_baseline_hline = TRUE
    + )
    > p
    Error in 1 - vjust[is_case] : non-numeric argument to binary operator
    Calls: <Anonymous> ... lapply -> FUN -> <Anonymous> -> draw_key -> rotate_just
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.6Mb
      sub-directories of 1Mb or more:
        doc    4.5Mb
        help   2.8Mb
    ```

# text

<details>

* Version: 1.0
* GitHub: https://github.com/OscarKjell/text
* Source code: https://github.com/cran/text
* Date/Publication: 2023-08-09 16:40:05 UTC
* Number of recursive dependencies: 155

Run `revdepcheck::cloud_details(, "text")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘text-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: textPCAPlot
    > ### Title: Plot words according to 2-D plot from 2 PCA components.
    > ### Aliases: textPCAPlot
    > 
    > ### ** Examples
    > 
    > # The test-data included in the package is called: DP_projections_HILS_SWLS_100
    ...
      4.   │   └─ggplot2::ggproto(...)
      5.   │     └─rlang::list2(...)
      6.   └─ggplot2::guide_legend(...)
      7.     └─ggplot2::new_guide(...)
      8.       └─ggplot2:::validate_theme(params$theme, call = caller_env())
      9.         └─base::mapply(...)
     10.           └─ggplot2 (local) `<fn>`(...)
     11.             └─cli::cli_abort(...)
     12.               └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 3 marked UTF-8 strings
    ```

# thematic

<details>

* Version: 0.1.4
* GitHub: https://github.com/rstudio/thematic
* Source code: https://github.com/cran/thematic
* Date/Publication: 2023-11-04 04:00:02 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "thematic")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(thematic)
      > 
      > test_check("thematic")
      Error : thematic doesn't (yet) support the 'devSVG_vdiffr' graphics device. Please report this error to https://github.com/rstudio/thematic/issues/new
      Error : thematic doesn't (yet) support the 'devSVG_vdiffr' graphics device. Please report this error to https://github.com/rstudio/thematic/issues/new
      Error : thematic doesn't (yet) support the 'devSVG_vdiffr' graphics device. Please report this error to https://github.com/rstudio/thematic/issues/new
    ...
        7.         └─ggplot2:::print.ggplot(p)
        8.           ├─ggplot2::ggplot_build(x)
        9.           └─thematic (local) ggplot_build.ggplot(x)
       10.             └─thematic:::resolve_theme_inheritance(p$theme)
       11.               └─thematic:::theme_relationships()
       12.                 └─base::vapply(...)
      
      [ FAIL 8 | WARN 1 | SKIP 8 | PASS 27 ]
      Error: Test failures
      Execution halted
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
        8. ├─plotly::config(...)
        9. │ └─plotly:::modify_list(p$x$config, args)
       10. │   ├─utils::modifyList(x %||% list(), y %||% list(), ...)
       11. │   │ └─base::stopifnot(is.list(x), is.list(val))
       12. │   └─x %||% list()
       13. └─plotly::layout(...)
      
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

# TidyDensity

<details>

* Version: 1.2.6
* GitHub: https://github.com/spsanderson/TidyDensity
* Source code: https://github.com/cran/TidyDensity
* Date/Publication: 2023-10-30 14:30:06 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "TidyDensity")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘TidyDensity-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: tidy_four_autoplot
    > ### Title: Automatic Plot of Density Data
    > ### Aliases: tidy_four_autoplot
    > 
    > ### ** Examples
    > 
    > tidy_normal(.num_sims = 5) %>%
    +   tidy_four_autoplot()
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

# tidyHeatmap

<details>

* Version: 1.8.1
* GitHub: https://github.com/stemangiola/tidyHeatmap
* Source code: https://github.com/cran/tidyHeatmap
* Date/Publication: 2022-05-20 07:20:03 UTC
* Number of recursive dependencies: 112

Run `revdepcheck::cloud_details(, "tidyHeatmap")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘introduction.Rmd’ using rmarkdown
    
    Quitting from lines 435-454 [unnamed-chunk-30] (./../man/fragments/intro.Rmd)
    
    Quitting from lines 435-454 [unnamed-chunk-1] (./../man/fragments/intro.Rmd)
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# tidypaleo

<details>

* Version: 0.1.3
* GitHub: https://github.com/paleolimbot/tidypaleo
* Source code: https://github.com/cran/tidypaleo
* Date/Publication: 2023-01-18 08:20:03 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "tidypaleo")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘age_depth.Rmd’ using rmarkdown
    --- finished re-building ‘age_depth.Rmd’
    
    --- re-building ‘nested_analysis.Rmd’ using rmarkdown
    --- finished re-building ‘nested_analysis.Rmd’
    
    --- re-building ‘strat_diagrams.Rmd’ using rmarkdown
    
    ...
    Quitting from lines 223-243 [unnamed-chunk-17] (strat_diagrams.Rmd)
    Error: processing vignette 'strat_diagrams.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘strat_diagrams.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘strat_diagrams.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# tidyterra

<details>

* Version: 0.5.1
* GitHub: https://github.com/dieghernan/tidyterra
* Source code: https://github.com/cran/tidyterra
* Date/Publication: 2023-12-15 12:00:02 UTC
* Number of recursive dependencies: 93

Run `revdepcheck::cloud_details(, "tidyterra")` for more info

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
      ── Failure ('test-fortify-Spat.R:75:3'): Fortify SpatRasters ───────────────────
      `build_terra` (`actual`) not identical to `build_point` (`expected`).
      
      `environment(environment(actual$layout$super)$env$layout$super)$env$...` is an internal dots object
      `environment(environment(expected$layout$super)$env$layout$super)$env$...` is absent
      
      [ FAIL 1 | WARN 55 | SKIP 133 | PASS 1147 ]
      Error: Test failures
      Execution halted
    ```

# timetk

<details>

* Version: 2.9.0
* GitHub: https://github.com/business-science/timetk
* Source code: https://github.com/cran/timetk
* Date/Publication: 2023-10-31 22:30:02 UTC
* Number of recursive dependencies: 221

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
        9.   └─timetk:::plot_time_series.data.frame(...)
       10.     ├─plotly::ggplotly(g, dynamicTicks = TRUE)
       11.     └─plotly:::ggplotly.ggplot(g, dynamicTicks = TRUE)
       12.       └─plotly::gg2list(...)
       13.         └─plotly:::get_gdefs_ggproto(npscales$scales, theme, plot, layers)
       14.           └─guides$train(scales, theme$legend.direction, plot$labels)
      
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

* Version: 2.3.1
* GitHub: https://github.com/xjsun1221/tinyarray
* Source code: https://github.com/cran/tinyarray
* Date/Publication: 2023-08-18 08:20:02 UTC
* Number of recursive dependencies: 234

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
    > k = box_surv(log2(exp_hub1+1),exprSet_hub1,meta1);k[[1]]
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

# tlars

<details>

* Version: 0.0.1
* GitHub: https://github.com/jasinmachkour/tlars
* Source code: https://github.com/cran/tlars
* Date/Publication: 2022-07-15 08:40:02 UTC
* Number of recursive dependencies: 69

Run `revdepcheck::cloud_details(, "tlars")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘tlars_variable_selection.Rmd’ using rmarkdown
    
    Quitting from lines 273-320 [FDR_and_TPR] (tlars_variable_selection.Rmd)
    Error: processing vignette 'tlars_variable_selection.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘tlars_variable_selection.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘tlars_variable_selection.Rmd’
    
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
      installed size is 14.0Mb
      sub-directories of 1Mb or more:
        doc    2.3Mb
        libs  11.2Mb
    ```

# tmt

<details>

* Version: 0.3.1-2
* GitHub: https://github.com/jansteinfeld/tmt
* Source code: https://github.com/cran/tmt
* Date/Publication: 2022-05-17 09:10:02 UTC
* Number of recursive dependencies: 92

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
      - tmt 0.3.1-2 (2022-05-16)
      > 
      > test_check("tmt")
      The following items are specified in the dataset, but not in the submitted mstdesign: ii1 
      The following items are specified in the mstdesign, but not in the dataset: i1 
    ...
      [1] 11 - 9 == 2
      Backtrace:
          ▆
       1. └─testthat::expect_that(length(p), equals(9)) at test-tmt_gmc.R:34:5
       2.   └─testthat (local) condition(object)
       3.     └─testthat::expect_equal(x, expected, ..., expected.label = label)
      
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 346 ]
      Error: Test failures
      Execution halted
    ```

# tornado

<details>

* Version: 0.1.2
* GitHub: https://github.com/bertcarnell/tornado
* Source code: https://github.com/cran/tornado
* Date/Publication: 2023-02-12 18:00:02 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::cloud_details(, "tornado")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tornado-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: importance.glm
    > ### Title: GLM variable importance plot
    > ### Aliases: importance.glm
    > 
    > ### ** Examples
    > 
    > gtest <- glm(mpg ~ cyl*wt*hp + gear + carb, data=mtcars, family=gaussian)
    > gtestreduced <- glm(mpg ~ 1, data=mtcars, family=gaussian)
    > imp <- importance(gtest, gtestreduced)
    > plot(imp)
    Error in ggp2_grob$grobs[[which(ggp2_grob$layout$name == "guide-box")]] : 
      attempt to select less than one element in get1index
    Calls: plot -> plot.importance_plot
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(tornado)
      > 
      > test_check("tornado")
      Loading required namespace: glmnet
      Loading required package: ggplot2
      Loading required package: lattice
    ...
      ── Error ('test-plot_importance_plot.R:8:3'): plotting works for base packages ──
      Error in `ggp2_grob$grobs[[which(ggp2_grob$layout$name == "guide-box")]]`: attempt to select less than one element in get1index
      Backtrace:
          ▆
       1. ├─base::plot(imp) at test-plot_importance_plot.R:8:3
       2. └─tornado:::plot.importance_plot(imp)
      
      [ FAIL 5 | WARN 0 | SKIP 0 | PASS 106 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘tornadoVignette.Rmd’ using rmarkdown
    
    Quitting from lines 266-270 [imp.lm] (tornadoVignette.Rmd)
    Error: processing vignette 'tornadoVignette.Rmd' failed with diagnostics:
    attempt to select less than one element in get1index
    --- failed re-building ‘tornadoVignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘tornadoVignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# treefit

<details>

* Version: 1.0.2
* GitHub: https://github.com/hayamizu-lab/treefit-r
* Source code: https://github.com/cran/treefit
* Date/Publication: 2022-01-18 07:50:02 UTC
* Number of recursive dependencies: 160

Run `revdepcheck::cloud_details(, "treefit")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘treefit.Rmd’ using rmarkdown
    
    Quitting from lines 118-119 [plot-tree-like-data-estimation-result] (treefit.Rmd)
    Error: processing vignette 'treefit.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘treefit.Rmd’
    
    --- re-building ‘working-with-seurat.Rmd’ using rmarkdown
    ...
    ==================================================
    downloaded 6.0 MB
    
    --- finished re-building ‘working-with-seurat.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘treefit.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# treeheatr

<details>

* Version: 0.2.1
* GitHub: https://github.com/trang1618/treeheatr
* Source code: https://github.com/cran/treeheatr
* Date/Publication: 2020-11-19 21:00:03 UTC
* Number of recursive dependencies: 98

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
    > draw_heat(x$dat, x$fit)
    Error in scale$guide == "none" : 
      comparison (==) is possible only for atomic and list types
    Calls: draw_heat ... ggplot_add.new_aes -> bump_aes_scales -> lapply -> FUN -> isTRUE
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘explore.Rmd’ using rmarkdown
    
    Quitting from lines 57-74 [unnamed-chunk-4] (explore.Rmd)
    Error: processing vignette 'explore.Rmd' failed with diagnostics:
    comparison (==) is possible only for atomic and list types
    --- failed re-building ‘explore.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘explore.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# trending

<details>

* Version: 0.1.0
* GitHub: https://github.com/reconverse/trending
* Source code: https://github.com/cran/trending
* Date/Publication: 2023-04-03 19:00:02 UTC
* Number of recursive dependencies: 143

Run `revdepcheck::cloud_details(, "trending")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Introduction.Rmd’ using rmarkdown
    --- finished re-building ‘Introduction.Rmd’
    
    --- re-building ‘prediction_intervals.Rmd’ using rmarkdown
    
    Quitting from lines 62-109 [poisson] (prediction_intervals.Rmd)
    Error: processing vignette 'prediction_intervals.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘prediction_intervals.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘prediction_intervals.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# TRexSelector

<details>

* Version: 0.0.1
* GitHub: https://github.com/jasinmachkour/trex
* Source code: https://github.com/cran/TRexSelector
* Date/Publication: 2022-08-17 06:50:06 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "TRexSelector")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘TRexSelector_usage_and_simulations.Rmd’ using rmarkdown
    
    Quitting from lines 216-251 [FDR_and_TPR] (TRexSelector_usage_and_simulations.Rmd)
    Error: processing vignette 'TRexSelector_usage_and_simulations.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘TRexSelector_usage_and_simulations.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘TRexSelector_usage_and_simulations.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# triplot

<details>

* Version: 1.3.0
* GitHub: https://github.com/ModelOriented/triplot
* Source code: https://github.com/cran/triplot
* Date/Publication: 2020-07-13 17:00:03 UTC
* Number of recursive dependencies: 77

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
    +                                 data = apartments_num[,-1],
    +                                 y = apartments_num[, 1],
    +                                 verbose = FALSE)
    > apartments_tri <- calculate_triplot(x = explainer_apartments,
    +  new_observation = apartments_num_new_observation[-1])
    > plot(apartments_tri)
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
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
        9.     └─base::lapply(x$plots, plot_table, guides = guides)
       10.       ├─patchwork (local) FUN(X[[i]], ...)
       11.       └─patchwork:::plot_table.ggplot(X[[i]], ...)
       12.         └─patchwork:::add_guides(gt, guides == "collect")
       13.           ├─base::unlist(guide_loc == panel_loc)
       14.           └─base::Ops.data.frame(guide_loc, panel_loc)
      
      [ FAIL 4 | WARN 1 | SKIP 0 | PASS 80 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# tvthemes

<details>

* Version: 1.3.2
* GitHub: https://github.com/Ryo-N7/tvthemes
* Source code: https://github.com/cran/tvthemes
* Date/Publication: 2022-11-17 18:20:02 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::cloud_details(, "tvthemes")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(tvthemes)
      > library(ggplot2)
      > 
      > test_check("tvthemes")
      [ FAIL 64 | WARN 3 | SKIP 1 | PASS 768 ]
      
    ...
      scale_color_avatar(palette = "AirNomads") not equal to scale_colour_avatar(palette = "AirNomads").
      Component "call": target, current do not match when deparsed
      Backtrace:
          ▆
       1. └─tvthemes (local) expect_eqNe(scale_color_avatar(palette = "AirNomads"), scale_colour_avatar(palette = "AirNomads")) at test-thelastairbender.R:254:3
       2.   └─testthat::expect_equal(..., check.environment = FALSE) at test-thelastairbender.R:3:16
      
      [ FAIL 64 | WARN 3 | SKIP 1 | PASS 768 ]
      Error: Test failures
      Execution halted
    ```

# ufs

<details>

* Version: 0.5.10
* GitHub: NA
* Source code: https://github.com/cran/ufs
* Date/Publication: 2023-06-09 16:30:03 UTC
* Number of recursive dependencies: 161

Run `revdepcheck::cloud_details(, "ufs")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ufs-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: duoComparisonDiamondPlot
    > ### Title: meansComparisonDiamondPlot and duoComparisonDiamondPlot
    > ### Aliases: duoComparisonDiamondPlot meansComparisonDiamondPlot
    > ### Keywords: hplot
    > 
    > ### ** Examples
    > 
    ...
    +                            items='weight',
    +                            compareBy='feed',
    +                            xbreaks=c(100,200,300,400),
    +                            showData=FALSE);
    > duoComparisonDiamondPlot(mtcars,
    +                          items=c('disp', 'hp'),
    +                          compareBy='vs',
    +                          xbreaks=c(100,200, 300, 400));
    Error: Cannot create zero-length unit vector ("unit" subsetting)
    Execution halted
    ```

# umiAnalyzer

<details>

* Version: 1.0.0
* GitHub: https://github.com/sfilges/umiAnalyzer
* Source code: https://github.com/cran/umiAnalyzer
* Date/Publication: 2021-11-25 08:40:02 UTC
* Number of recursive dependencies: 117

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
    Error in train(..., self = self) : 
      unused argument (list("Assay", "Variant Allele Frequency (%)", "Variants", "yintercept"))
    Calls: AmpliconPlot ... ggplotly.ggplot -> gg2list -> get_gdefs_ggproto -> <Anonymous>
    Execution halted
    ```

# updog

<details>

* Version: 2.1.5
* GitHub: https://github.com/dcgerard/updog
* Source code: https://github.com/cran/updog
* Date/Publication: 2023-11-29 15:50:02 UTC
* Number of recursive dependencies: 151

Run `revdepcheck::cloud_details(, "updog")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘updog-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: flexdog
    > ### Title: Flexible genotyping for polyploids from next-generation
    > ###   sequencing data.
    > ### Aliases: flexdog
    > 
    > ### ** Examples
    > 
    ...
             Fit: 5 of 5 
    Initial Bias: 2.718282 
    Log-Likelihood: -15.44141 
    Keeping old fit.
    
    Done!
    > plot(fout)
    Error in upgradeUnit.default(x) : Not a unit object
    Calls: <Anonymous> ... lapply -> FUN -> upgradeUnit -> upgradeUnit.default
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘multidog.Rmd’ using rmarkdown
    
    Quitting from lines 88-89 [unnamed-chunk-8] (multidog.Rmd)
    Error: processing vignette 'multidog.Rmd' failed with diagnostics:
    Not a unit object
    --- failed re-building ‘multidog.Rmd’
    
    --- re-building ‘oracle_calculations.Rmd’ using rmarkdown
    ...
    --- finished re-building ‘simulate_ngs.Rmd’
    
    --- re-building ‘smells_like_updog.Rmd’ using rmarkdown
    --- finished re-building ‘smells_like_updog.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘multidog.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 13.5Mb
      sub-directories of 1Mb or more:
        libs  12.5Mb
    ```

# viridis

<details>

* Version: 0.6.4
* GitHub: https://github.com/sjmgarnier/viridis
* Source code: https://github.com/cran/viridis
* Date/Publication: 2023-07-22 12:50:02 UTC
* Number of recursive dependencies: 86

Run `revdepcheck::cloud_details(, "viridis")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(viridis)
      Loading required package: viridisLite
      > library(ggplot2)
      > library(MASS)
      > library(gridExtra)
      > 
    ...
      ── Failure ('test-viridis.R:5:3'): scales work correctly ───────────────────────
      fill_scale$scale_name not equal to "gradientn".
      target is NULL, current is character
      ── Failure ('test-viridis.R:11:3'): scales work correctly ──────────────────────
      color_scale$scale_name not equal to "gradientn".
      target is NULL, current is character
      
      [ FAIL 2 | WARN 1 | SKIP 1 | PASS 3 ]
      Error: Test failures
      Execution halted
    ```

# visR

<details>

* Version: 0.4.0
* GitHub: https://github.com/openpharma/visR
* Source code: https://github.com/cran/visR
* Date/Publication: 2023-11-20 18:20:02 UTC
* Number of recursive dependencies: 155

Run `revdepcheck::cloud_details(, "visR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘visR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: add_highlight
    > ### Title: Highlight a specific strata
    > ### Aliases: add_highlight add_highlight.ggsurvfit
    > 
    > ### ** Examples
    > 
    > 
    ...
    +   visR::estimate_KM(strata = "SEX") %>%
    +   visR::visr() %>%
    +   visR::add_CI(alpha = 0.4) %>%
    +   visR::add_highlight(strata = "M", bg_alpha = 0.2)
    Error in FUN(X[[i]], ...) : 
      The strata you specified has not been found in the provided plot.
      Available strata: NULL, NULL
      Please adjust and rerun.
    Calls: %>% ... add_highlight.ggsurvfit -> <Anonymous> -> lapply -> FUN
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
      
      `actual`:   TRUE 
      `expected`: FALSE
      
      [ FAIL 12 | WARN 28 | SKIP 27 | PASS 988 ]
      Error: Test failures
      In addition: Warning message:
      In .Internal(islistfactor(x, recursive)) :
        closing unused connection 4 (https://raw.githubusercontent.com/vntkumar8/covid-survival/main/data/final.csv)
      Execution halted
    ```

# visxhclust

<details>

* Version: 1.1.0
* GitHub: https://github.com/rhenkin/visxhclust
* Source code: https://github.com/cran/visxhclust
* Date/Publication: 2023-03-17 12:10:02 UTC
* Number of recursive dependencies: 137

Run `revdepcheck::cloud_details(, "visxhclust")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘visxhclust-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_annotation_dist
    > ### Title: Plot distribution of annotation data across clusters
    > ### Aliases: plot_annotation_dist
    > 
    > ### ** Examples
    > 
    > dmat <- compute_dmat(iris, "euclidean", TRUE, c("Petal.Length", "Sepal.Length"))
    > clusters <- compute_clusters(dmat, "complete")
    > cluster_labels <- cut_clusters(clusters, 2)
    > plot_annotation_dist(iris["Species"], cluster_labels)
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

# vivaldi

<details>

* Version: 1.0.1
* GitHub: https://github.com/GreshamLab/vivaldi
* Source code: https://github.com/cran/vivaldi
* Date/Publication: 2023-03-21 20:10:02 UTC
* Number of recursive dependencies: 106

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
    Error in train(..., self = self) : 
      unused argument (list("Nucleotide Position", "minorfreq", "minor"))
    Calls: snv_location ... ggplotly.ggplot -> gg2list -> get_gdefs_ggproto -> <Anonymous>
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
        unused argument (list("Nucleotide Position", "minorfreq", "minor"))
      
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 29 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘vignette.Rmd’ using rmarkdown
    
    Quitting from lines 286-287 [unnamed-chunk-18] (vignette.Rmd)
    Error: processing vignette 'vignette.Rmd' failed with diagnostics:
    unused argument (list("Nucleotide Position", "minorfreq", "minor"))
    --- failed re-building ‘vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        doc       5.4Mb
        extdata   1.1Mb
    ```

# vivid

<details>

* Version: 0.2.8
* GitHub: NA
* Source code: https://github.com/cran/vivid
* Date/Publication: 2023-07-10 22:20:02 UTC
* Number of recursive dependencies: 220

Run `revdepcheck::cloud_details(, "vivid")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘vivid-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pdpPairs
    > ### Title: pdpPairs
    > ### Aliases: pdpPairs
    > 
    > ### ** Examples
    > 
    > # Load in the data:
    > aq <- na.omit(airquality)
    > f <- lm(Ozone ~ ., data = aq)
    > pdpPairs(aq, f, "Ozone")
    Generating ice/pdp fits... waiting...
    Finished ice/pdp
    Error in pmg$grobs[[legend_layout$grob_pos]] <- legend_obj : 
      attempt to select more than one element in integerOneIndex
    Calls: pdpPairs ... withCallingHandlers -> print -> print.ggmatrix -> ggmatrix_gtable
    Execution halted
    ```

# vroom

<details>

* Version: 1.6.5
* GitHub: https://github.com/tidyverse/vroom
* Source code: https://github.com/cran/vroom
* Date/Publication: 2023-12-05 23:50:02 UTC
* Number of recursive dependencies: 97

Run `revdepcheck::cloud_details(, "vroom")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘benchmarks.Rmd’ using rmarkdown
    
    Quitting from lines 214-219 [unnamed-chunk-2] (benchmarks.Rmd)
    Error: processing vignette 'benchmarks.Rmd' failed with diagnostics:
    '==' only defined for equally-sized data frames
    --- failed re-building ‘benchmarks.Rmd’
    
    --- re-building ‘vroom.Rmd’ using rmarkdown
    --- finished re-building ‘vroom.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘benchmarks.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 27.5Mb
      sub-directories of 1Mb or more:
        libs  25.9Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘readr’
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
       3. │ ├─plotly::ggplotly(plot)
       4. │ └─plotly:::ggplotly.ggplot(plot)
       5. │   └─plotly::gg2list(...)
       6. │     └─plotly:::get_gdefs_ggproto(npscales$scales, theme, plot, layers)
       7. │       └─guides$train(scales, theme$legend.direction, plot$labels)
       8. └─plotly::layout(...)
      
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 60 ]
      Error: Test failures
      Execution halted
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
       1. └─wilson::create_geneview(...) at test-interactive-plots.R:21:3
       2.   ├─plotly::ggplotly(...)
       3.   └─plotly:::ggplotly.ggplot(...)
       4.     └─plotly::gg2list(...)
       5.       └─plotly:::get_gdefs_ggproto(npscales$scales, theme, plot, layers)
       6.         └─guides$train(scales, theme$legend.direction, plot$labels)
      
      [ FAIL 3 | WARN 11 | SKIP 1 | PASS 74 ]
      Error: Test failures
      Execution halted
    ```

# wordpredictor

<details>

* Version: 0.0.3
* GitHub: https://github.com/pakjiddat/word-predictor
* Source code: https://github.com/cran/wordpredictor
* Date/Publication: 2022-01-04 14:30:02 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::cloud_details(, "wordpredictor")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘wordpredictor-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ModelEvaluator
    > ### Title: Evaluates performance of n-gram models
    > ### Aliases: ModelEvaluator
    > 
    > ### ** Examples
    > 
    > 
    ...
    > me <- ModelEvaluator$new(ve = ve)
    > # The performance evaluation is performed
    > me$compare_performance(opts = list(
    +     "save_to" = NULL,
    +     "dir" = ed
    + ))
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(wordpredictor)
      > 
      > test_check("wordpredictor")
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 119 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
        8.           └─base::lapply(x$plots, plot_table, guides = guides)
        9.             ├─patchwork (local) FUN(X[[i]], ...)
       10.             └─patchwork:::plot_table.ggplot(X[[i]], ...)
       11.               └─patchwork:::add_guides(gt, guides == "collect")
       12.                 ├─base::unlist(guide_loc == panel_loc)
       13.                 └─base::Ops.data.frame(guide_loc, panel_loc)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 119 ]
      Error: Test failures
      Execution halted
    ```

