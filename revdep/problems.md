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

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘afex_analysing_accuracy_data.Rmd’ using rmarkdown
    --- finished re-building ‘afex_analysing_accuracy_data.Rmd’
    
    --- re-building ‘afex_anova_example.Rmd’ using rmarkdown
    --- finished re-building ‘afex_anova_example.Rmd’
    
    --- re-building ‘afex_mixed_example.Rmd’ using rmarkdown
    --- finished re-building ‘afex_mixed_example.Rmd’
    
    ...
    --- finished re-building ‘assumptions_of_ANOVAs.Rmd’
    
    --- re-building ‘introduction-mixed-models.pdf.asis’ using asis
    --- finished re-building ‘introduction-mixed-models.pdf.asis’
    
    SUMMARY: processing the following file failed:
      ‘afex_plot_supported_models.Rmd’
    
    Error: Vignette re-building failed.
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

# APCI

<details>

* Version: 1.0.7
* GitHub: NA
* Source code: https://github.com/cran/APCI
* Date/Publication: 2024-01-30 06:40:03 UTC
* Number of recursive dependencies: 87

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
       -0.341586100   0.108521398   0.196915587  -0.384744431   0.213074939 
          acc8:pcc3     acc9:pcc3     acc1:pcc4     acc2:pcc4     acc3:pcc4 
       -0.127365980   0.127168930  -0.469822600   0.040191453   0.330041906 
          acc4:pcc4     acc5:pcc4     acc6:pcc4     acc7:pcc4     acc8:pcc4 
        0.312063653   0.268107970  -0.184595190  -0.048022414  -0.334166947 
          acc9:pcc4     acc1:pcc5     acc2:pcc5     acc3:pcc5     acc4:pcc5 
        0.061742631   0.027680958   0.288167814  -0.061750085  -0.167463426 
          acc5:pcc5     acc6:pcc5     acc7:pcc5     acc8:pcc5     acc9:pcc5 
       -0.558510769  -0.300765544   0.317141130   0.728091684  -0.318307169 
      Killed
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
      Actual value: "List of 11\\n \$ data       :'data\.frame':\\t72 obs\. of  6 variables:\\n  \.\.\$ Ind\.ID    : Factor w/ 24 levels "A10","A12","AA9",\.\.: 3 1 2 5 6 4 7 8 11 9 \.\.\.\\n  \.\.\$ origin\.pop: Factor w/ 3 levels "pop\.1","pop\.2",\.\.: 1 1 1 2 2 2 3 3 1 1 \.\.\.\\n  \.\.\$ pred\.pop  : Factor w/ 3 levels "pop\.1","pop\.3",\.\.: 1 2 2 1 1 1 1 1 2 2 \.\.\.\\n  \.\.\$ fold_n    : chr \[1:72\] "fold_1" "fold_1" "fold_1" "fold_1" \.\.\.\\n  \.\.\$ variable  : Factor w/ 3 levels "pop\.1","pop\.2",\.\.: 1 1 1 1 1 1 1 1 1 1 \.\.\.\\n  \.\.\$ value     : num \[1:72\] 0\.4 0\.326 0\.26 0\.383 0\.44 \.\.\.\\n \$ layers     :List of 1\\n  \.\.\$ :Classes 'LayerInstance', 'Layer', 'ggproto', 'gg' <ggproto object: Class LayerInstance, Layer, gg>\\n    aes_params: list\\n    compute_aesthetics: function\\n    compute_geom_1: function\\n    compute_geom_2: function\\n    compute_position: function\\n    compute_statistic: function\\n    computed_geom_params: NULL\\n    computed_mapping: NULL\\n    computed_stat_params: NULL\\n    constructor: call\\n    data: waiver\\n    draw_geom: function\\n    finish_statistics: function\\n    geom: <ggproto object: Class GeomBar, GeomRect, Geom, gg>\\n        aesthetics: function\\n        default_aes: uneval\\n        draw_group: function\\n        draw_key: function\\n        draw_layer: function\\n        draw_panel: function\\n        extra_params: just na\.rm orientation\\n        handle_na: function\\n        non_missing_aes: xmin xmax ymin ymax\\n        optional_aes: \\n        parameters: function\\n        rename_size: TRUE\\n        required_aes: x y\\n        setup_data: function\\n        setup_params: function\\n        use_defaults: function\\n        super:  <ggproto object: Class GeomRect, Geom, gg>\\n    geom_params: list\\n    inherit\.aes: TRUE\\n    layer_data: function\\n    map_statistic: function\\n    mapping: NULL\\n    position: <ggproto object: Class PositionStack, Position, gg>\\n        compute_layer: function\\n        compute_panel: function\\n        fill: FALSE\\n        required_aes: \\n        reverse: FALSE\\n        setup_data: function\\n        setup_params: function\\n        type: NULL\\n        vjust: 1\\n        super:  <ggproto object: Class Position, gg>\\n    print: function\\n    setup_layer: function\\n    show\.legend: NA\\n    stat: <ggproto object: Class StatIdentity, Stat, gg>\\n        aesthetics: function\\n        compute_group: function\\n        compute_layer: function\\n        compute_panel: function\\n        default_aes: uneval\\n        dropped_aes: \\n        extra_params: na\.rm\\n        finish_layer: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        required_aes: \\n        retransform: TRUE\\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Stat, gg>\\n    stat_params: list\\n    super:  <ggproto object: Class Layer, gg> \\n \$ scales     :Classes 'ScalesList', 'ggproto', 'gg' <ggproto object: Class ScalesList, gg>\\n    add: function\\n    add_defaults: function\\n    add_missing: function\\n    backtransform_df: function\\n    clone: function\\n    find: function\\n    get_scales: function\\n    has_scale: function\\n    input: function\\n    map_df: function\\n    n: function\\n    non_position_scales: function\\n    scales: list\\n    train_df: function\\n    transform_df: function\\n    super:  <ggproto object: Class ScalesList, gg> \\n \$ guides     :Classes 'Guides', 'ggproto', 'gg' <ggproto object: Class Guides, gg>\\n    add: function\\n    assemble: function\\n    build: function\\n    draw: function\\n    get_custom: function\\n    get_guide: function\\n    get_params: function\\n    get_position: function\\n    guides: list\\n    merge: function\\n    missing: <ggproto object: Class GuideNone, Guide, gg>\\n        add_title: function\\n        arrange_layout: function\\n        assemble_drawing: function\\n        available_aes: any\\n        build_decor: function\\n        build_labels: function\\n        build_ticks: function\\n        build_title: function\\n        draw: function\\n        draw_early_exit: function\\n        elements: list\\n        extract_decor: function\\n        extract_key: function\\n        extract_params: function\\n        get_layer_key: function\\n        hashables: list\\n        measure_grobs: function\\n        merge: function\\n        override_elements: function\\n        params: list\\n        process_layers: function\\n        setup_elements: function\\n        setup_params: function\\n        train: function\\n        transform: function\\n        super:  <ggproto object: Class GuideNone, Guide, gg>\\n    package_box: function\\n    print: function\\n    process_layers: function\\n    setup: function\\n    subset_guides: function\\n    train: function\\n    update_params: function\\n    super:  <ggproto object: Class Guides, gg> \\n \$ mapping    :List of 3\\n  \.\.\$ x   : language ~Ind\.ID\\n  \.\. \.\.- attr\(\*, "\.Environment"\)=<environment: 0x5562edf4d5b0> \\n  \.\.\$ y   : language ~value\\n  \.\. \.\.- attr\(\*, "\.Environment"\)=<environment: 0x5562edf4d5b0> \\n  \.\.\$ fill: language ~variable\\n  \.\. \.\.- attr\(\*, "\.Environment"\)=<environment: 0x5562edf4d5b0> \\n  \.\.- attr\(\*, "class"\)= chr "uneval"\\n \$ theme      :List of 124\\n  \.\.\$ line                            :List of 6\\n  \.\. \.\.\$ colour       : chr "black"\\n  \.\. \.\.\$ linewidth    : num 0\.5\\n  \.\. \.\.\$ linetype     : num 1\\n  \.\. \.\.\$ lineend      : chr "butt"\\n  \.\. \.\.\$ arrow        : logi FALSE\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_line" "element"\\n  \.\.\$ rect                            :List of 5\\n  \.\. \.\.\$ fill         : chr "white"\\n  \.\. \.\.\$ colour       : chr "black"\\n  \.\. \.\.\$ linewidth    : num 0\.5\\n  \.\. \.\.\$ linetype     : num 1\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_rect" "element"\\n  \.\.\$ text                            :List of 11\\n  \.\. \.\.\$ family       : chr ""\\n  \.\. \.\.\$ face         : chr "plain"\\n  \.\. \.\.\$ colour       : chr "black"\\n  \.\. \.\.\$ size         : num 11\\n  \.\. \.\.\$ hjust        : num 0\.5\\n  \.\. \.\.\$ vjust        : num 0\.5\\n  \.\. \.\.\$ angle        : num 0\\n  \.\. \.\.\$ lineheight   : num 0\.9\\n  \.\. \.\.\$ margin       : 'margin' num \[1:4\] 0points 0points 0points 0points\\n  \.\. \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\. \.\.\$ debug        : logi FALSE\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ title                           : NULL\\n  \.\.\$ aspect\.ratio                    : NULL\\n  \.\.\$ axis\.title                      : NULL\\n  \.\.\$ axis\.title\.x                    : list\(\)\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_blank" "element"\\n  \.\.\$ axis\.title\.x\.top                :List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : NULL\\n  \.\. \.\.\$ size         : NULL\\n  \.\. \.\.\$ hjust        : NULL\\n  \.\. \.\.\$ vjust        : num 0\\n  \.\. \.\.\$ angle        : NULL\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : 'margin' num \[1:4\] 0points 0points 2\.75points 0points\\n  \.\. \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ axis\.title\.x\.bottom             : NULL\\n  \.\.\$ axis\.title\.y                    :List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : NULL\\n  \.\. \.\.\$ size         : NULL\\n  \.\. \.\.\$ hjust        : NULL\\n  \.\. \.\.\$ vjust        : num 1\\n  \.\. \.\.\$ angle        : num 90\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : 'margin' num \[1:4\] 0points 2\.75points 0points 0points\\n  \.\. \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ axis\.title\.y\.left               : NULL\\n  \.\.\$ axis\.title\.y\.right              :List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : NULL\\n  \.\. \.\.\$ size         : NULL\\n  \.\. \.\.\$ hjust        : NULL\\n  \.\. \.\.\$ vjust        : num 1\\n  \.\. \.\.\$ angle        : num -90\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : 'margin' num \[1:4\] 0points 0points 0points 2\.75points\\n  \.\. \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ axis\.text                       :List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : chr "grey30"\\n  \.\. \.\.\$ size         : 'rel' num 0\.8\\n  \.\. \.\.\$ hjust        : NULL\\n  \.\. \.\.\$ vjust        : NULL\\n  \.\. \.\.\$ angle        : NULL\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : NULL\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ axis\.text\.x                     :List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : NULL\\n  \.\. \.\.\$ size         : NULL\\n  \.\. \.\.\$ hjust        : NULL\\n  \.\. \.\.\$ vjust        : num 1\\n  \.\. \.\.\$ angle        : num 90\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : 'margin' num \[1:4\] 2\.2points 0points 0points 0points\\n  \.\. \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi FALSE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ axis\.text\.x\.top                 :List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : NULL\\n  \.\. \.\.\$ size         : NULL\\n  \.\. \.\.\$ hjust        : NULL\\n  \.\. \.\.\$ vjust        : num 0\\n  \.\. \.\.\$ angle        : NULL\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : 'margin' num \[1:4\] 0points 0points 2\.2points 0points\\n  \.\. \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ axis\.text\.x\.bottom              : NULL\\n  \.\.\$ axis\.text\.y                     :List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : NULL\\n  \.\. \.\.\$ size         : NULL\\n  \.\. \.\.\$ hjust        : num 1\\n  \.\. \.\.\$ vjust        : NULL\\n  \.\. \.\.\$ angle        : NULL\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : 'margin' num \[1:4\] 0points 2\.2points 0points 0points\\n  \.\. \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ axis\.text\.y\.left                : NULL\\n  \.\.\$ axis\.text\.y\.right               :List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : NULL\\n  \.\. \.\.\$ size         : NULL\\n  \.\. \.\.\$ hjust        : num 0\\n  \.\. \.\.\$ vjust        : NULL\\n  \.\. \.\.\$ angle        : NULL\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : 'margin' num \[1:4\] 0points 0points 0points 2\.2points\\n  \.\. \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ axis\.ticks                      :List of 6\\n  \.\. \.\.\$ colour       : chr "grey20"\\n  \.\. \.\.\$ linewidth    : NULL\\n  \.\. \.\.\$ linetype     : NULL\\n  \.\. \.\.\$ lineend      : NULL\\n  \.\. \.\.\$ arrow        : logi FALSE\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_line" "element"\\n  \.\.\$ axis\.ticks\.x                    : NULL\\n  \.\.\$ axis\.ticks\.x\.top                : NULL\\n  \.\.\$ axis\.ticks\.x\.bottom             : NULL\\n  \.\.\$ axis\.ticks\.y                    : NULL\\n  \.\.\$ axis\.ticks\.y\.left               : NULL\\n  \.\.\$ axis\.ticks\.y\.right              : NULL\\n  \.\.\$ axis\.minor\.ticks\.x\.top          : NULL\\n  \.\.\$ axis\.minor\.ticks\.x\.bottom       : NULL\\n  \.\.\$ axis\.minor\.ticks\.y\.left         : NULL\\n  \.\.\$ axis\.minor\.ticks\.y\.right        : NULL\\n  \.\.\$ axis\.ticks\.length               : 'simpleUnit' num 2\.75points\\n  \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\.\$ axis\.ticks\.length\.x             : NULL\\n  \.\.\$ axis\.ticks\.length\.x\.top         : NULL\\n  \.\.\$ axis\.ticks\.length\.x\.bottom      : NULL\\n  \.\.\$ axis\.ticks\.length\.y             : NULL\\n  \.\.\$ axis\.ticks\.length\.y\.left        : NULL\\n  \.\.\$ axis\.ticks\.length\.y\.right       : NULL\\n  \.\.\$ axis\.minor\.ticks\.length         : 'rel' num 0\.75\\n  \.\.\$ axis\.minor\.ticks\.length\.x       : NULL\\n  \.\.\$ axis\.minor\.ticks\.length\.x\.top   : NULL\\n  \.\.\$ axis\.minor\.ticks\.length\.x\.bottom: NULL\\n  \.\.\$ axis\.minor\.ticks\.length\.y       : NULL\\n  \.\.\$ axis\.minor\.ticks\.length\.y\.left  : NULL\\n  \.\.\$ axis\.minor\.ticks\.length\.y\.right : NULL\\n  \.\.\$ axis\.line                       : list\(\)\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_blank" "element"\\n  \.\.\$ axis\.line\.x                     : NULL\\n  \.\.\$ axis\.line\.x\.top                 : NULL\\n  \.\.\$ axis\.line\.x\.bottom              : NULL\\n  \.\.\$ axis\.line\.y                     : NULL\\n  \.\.\$ axis\.line\.y\.left                : NULL\\n  \.\.\$ axis\.line\.y\.right               : NULL\\n  \.\.\$ legend\.background               :List of 5\\n  \.\. \.\.\$ fill         : NULL\\n  \.\. \.\.\$ colour       : logi NA\\n  \.\. \.\.\$ linewidth    : NULL\\n  \.\. \.\.\$ linetype     : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_rect" "element"\\n  \.\.\$ legend\.margin                   : 'margin' num \[1:4\] 5\.5points 5\.5points 5\.5points 5\.5points\\n  \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\.\$ legend\.spacing                  : 'simpleUnit' num 11points\\n  \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\.\$ legend\.spacing\.x                : NULL\\n  \.\.\$ legend\.spacing\.y                : NULL\\n  \.\.\$ legend\.key                      : NULL\\n  \.\.\$ legend\.key\.size                 : 'simpleUnit' num 1\.2lines\\n  \.\. \.\.- attr\(\*, "unit"\)= int 3\\n  \.\.\$ legend\.key\.height               : NULL\\n  \.\.\$ legend\.key\.width                : NULL\\n  \.\.\$ legend\.key\.spacing              : 'simpleUnit' num 5\.5points\\n  \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\.\$ legend\.key\.spacing\.x            : NULL\\n  \.\.\$ legend\.key\.spacing\.y            : NULL\\n  \.\.\$ legend\.frame                    : NULL\\n  \.\.\$ legend\.ticks                    : NULL\\n  \.\.\$ legend\.ticks\.length             : 'rel' num 0\.2\\n  \.\.\$ legend\.axis\.line                : NULL\\n  \.\.\$ legend\.text                     :List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : NULL\\n  \.\. \.\.\$ size         : 'rel' num 0\.8\\n  \.\. \.\.\$ hjust        : NULL\\n  \.\. \.\.\$ vjust        : NULL\\n  \.\. \.\.\$ angle        : NULL\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : NULL\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ legend\.text\.position            : NULL\\n  \.\.\$ legend\.title                    :List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : NULL\\n  \.\. \.\.\$ size         : NULL\\n  \.\. \.\.\$ hjust        : num 0\\n  \.\. \.\.\$ vjust        : NULL\\n  \.\. \.\.\$ angle        : NULL\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : NULL\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ legend\.title\.position           : NULL\\n  \.\.\$ legend\.position                 : chr "right"\\n  \.\.\$ legend\.position\.inside          : NULL\\n  \.\.\$ legend\.direction                : NULL\\n  \.\.\$ legend\.byrow                    : NULL\\n  \.\.\$ legend\.justification            : chr "center"\\n  \.\.\$ legend\.justification\.top        : NULL\\n  \.\.\$ legend\.justification\.bottom     : NULL\\n  \.\.\$ legend\.justification\.left       : NULL\\n  \.\.\$ legend\.justification\.right      : NULL\\n  \.\.\$ legend\.justification\.inside     : NULL\\n  \.\.\$ legend\.location                 : NULL\\n  \.\.\$ legend\.box                      : NULL\\n  \.\.\$ legend\.box\.just                 : NULL\\n  \.\.\$ legend\.box\.margin               : 'margin' num \[1:4\] 0cm 0cm 0cm 0cm\\n  \.\. \.\.- attr\(\*, "unit"\)= int 1\\n  \.\.\$ legend\.box\.background           : list\(\)\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_blank" "element"\\n  \.\.\$ legend\.box\.spacing              : 'simpleUnit' num 11points\\n  \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\.\$ panel\.background                :List of 5\\n  \.\. \.\.\$ fill         : chr "white"\\n  \.\. \.\.\$ colour       : logi NA\\n  \.\. \.\.\$ linewidth    : NULL\\n  \.\. \.\.\$ linetype     : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_rect" "element"\\n  \.\.\$ panel\.border                    :List of 5\\n  \.\. \.\.\$ fill         : logi NA\\n  \.\. \.\.\$ colour       : chr "grey20"\\n  \.\. \.\.\$ linewidth    : NULL\\n  \.\. \.\.\$ linetype     : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_rect" "element"\\n  \.\.\$ panel\.spacing                   : 'simpleUnit' num 5\.5points\\n  \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\.\$ panel\.spacing\.x                 : NULL\\n  \.\.\$ panel\.spacing\.y                 : NULL\\n  \.\.\$ panel\.grid                      :List of 6\\n  \.\. \.\.\$ colour       : chr "grey92"\\n  \.\. \.\.\$ linewidth    : NULL\\n  \.\. \.\.\$ linetype     : NULL\\n  \.\. \.\.\$ lineend      : NULL\\n  \.\. \.\.\$ arrow        : logi FALSE\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_line" "element"\\n  \.\.\$ panel\.grid\.major                : list\(\)\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_blank" "element"\\n  \.\.\$ panel\.grid\.minor                : list\(\)\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_blank" "element"\\n  \.\.\$ panel\.grid\.major\.x              : NULL\\n  \.\.\$ panel\.grid\.major\.y              : NULL\\n  \.\.\$ panel\.grid\.minor\.x              : NULL\\n  \.\.\$ panel\.grid\.minor\.y              : NULL\\n  \.\. \[list output truncated\]\\n  \.\.- attr\(\*, "class"\)= chr \[1:2\] "theme" "gg"\\n  \.\.- attr\(\*, "complete"\)= logi TRUE\\n  \.\.- attr\(\*, "validate"\)= logi TRUE\\n \$ coordinates:Classes 'CoordCartesian', 'Coord', 'ggproto', 'gg' <ggproto object: Class CoordCartesian, Coord, gg>\\n    aspect: function\\n    backtransform_range: function\\n    clip: on\\n    default: FALSE\\n    distance: function\\n    expand: TRUE\\n    is_free: function\\n    is_linear: function\\n    labels: function\\n    limits: list\\n    modify_scales: function\\n    range: function\\n    render_axis_h: function\\n    render_axis_v: function\\n    render_bg: function\\n    render_fg: function\\n    setup_data: function\\n    setup_layout: function\\n    setup_panel_guides: function\\n    setup_panel_params: function\\n    setup_params: function\\n    train_panel_guides: function\\n    transform: function\\n    super:  <ggproto object: Class CoordCartesian, Coord, gg> \\n \$ facet      :Classes 'FacetGrid', 'Facet', 'ggproto', 'gg' <ggproto object: Class FacetGrid, Facet, gg>\\n    compute_layout: function\\n    draw_back: function\\n    draw_front: function\\n    draw_labels: function\\n    draw_panels: function\\n    finish_data: function\\n    init_scales: function\\n    map_data: function\\n    params: list\\n    setup_data: function\\n    setup_params: function\\n    shrink: TRUE\\n    train_scales: function\\n    vars: function\\n    super:  <ggproto object: Class FacetGrid, Facet, gg> \\n \$ plot_env   :<environment: 0x5562edf4d5b0> \\n \$ layout     :Classes 'Layout', 'ggproto', 'gg' <ggproto object: Class Layout, gg>\\n    coord: NULL\\n    coord_params: list\\n    facet: NULL\\n    facet_params: list\\n    finish_data: function\\n    get_scales: function\\n    layout: NULL\\n    map_position: function\\n    panel_params: NULL\\n    panel_scales_x: NULL\\n    panel_scales_y: NULL\\n    render: function\\n    render_labels: function\\n    reset_scales: function\\n    resolve_label: function\\n    setup: function\\n    setup_panel_guides: function\\n    setup_panel_params: function\\n    train_position: function\\n    super:  <ggproto object: Class Layout, gg> \\n \$ labels     :List of 4\\n  \.\.\$ title: chr "K = 3  "\\n  \.\.\$ y    : chr "Probability"\\n  \.\.\$ x    : chr "Ind\.ID"\\n  \.\.\$ fill : chr "variable"\\n - attr\(\*, "class"\)= chr \[1:2\] "gg" "ggplot""
      Backtrace:
          ▆
       1. └─testthat::expect_output(str(plot), "List of 10") at test_membership.R:5:3
       2.   └─testthat::expect_match(...)
       3.     └─testthat:::expect_match_(...)
      
      [ FAIL 3 | WARN 1 | SKIP 0 | PASS 39 ]
      Error: Test failures
      Execution halted
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
      
      [ FAIL 3 | WARN 3 | SKIP 0 | PASS 231 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 107 marked UTF-8 strings
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
* Number of recursive dependencies: 153

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

# cobalt

<details>

* Version: 4.5.3
* GitHub: https://github.com/ngreifer/cobalt
* Source code: https://github.com/cran/cobalt
* Date/Publication: 2024-01-10 03:23:07 UTC
* Number of recursive dependencies: 176

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

# constructive

<details>

* Version: 0.2.0
* GitHub: https://github.com/cynkra/constructive
* Source code: https://github.com/cran/constructive
* Date/Publication: 2023-11-13 17:33:24 UTC
* Number of recursive dependencies: 112

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

# cylcop

<details>

* Version: 0.2.0
* GitHub: https://github.com/r-lib/devtools
* Source code: https://github.com/cran/cylcop
* Date/Publication: 2022-10-29 22:00:21 UTC
* Number of recursive dependencies: 98

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
     21. │             └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     22. │               └─l$compute_geom_2(d)
     23. │                 └─ggplot2 (local) compute_geom_2(..., self = self)
     24. │                   └─self$geom$use_defaults(data, self$aes_params, modifiers)
     25. │                     └─ggplot2 (local) use_defaults(..., self = self)
     26. │                       └─vctrs::vec_cbind(data[setdiff(names(data), aes_params)], !!!params[aes_params])
     27. └─vctrs:::stop_scalar_type(`<fn>`(`<expression>`), "label", `<env>`)
     28.   └─vctrs:::stop_vctrs(...)
     29.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
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

# ddpcr

<details>

* Version: 1.15.2
* GitHub: https://github.com/daattali/ddpcr
* Source code: https://github.com/cran/ddpcr
* Date/Publication: 2023-08-20 22:32:32 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::cloud_details(, "ddpcr")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        sample_data   3.0Mb
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘shinydisconnect’
      All declared Imports should be used.
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
    Warning: The `trans` argument of `continuous_scale()` is deprecated as of ggplot2 3.5.0.
    ℹ Please use the `transform` argument instead.
    Warning: The `scale_name` argument of `continuous_scale()` is deprecated as of ggplot2
    3.5.0.
    Warning: The S3 guide system was deprecated in ggplot2 3.5.0.
    ℹ It has been replaced by a ggproto system that can be extended.
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
      > library(distributional)
      > 
      > test_check("distributional")
      [ FAIL 2 | WARN 3 | SKIP 0 | PASS 498 ]
      
      == Failed tests ================================================================
    ...
       18.               \-base::mapply(FUN = f, ..., SIMPLIFY = FALSE)
       19.                 \-ggplot2 (local) `<fn>`(...)
       20.                   \-guide$train(param, scale, aes, title = labels[[aes]])
       21.                     \-ggplot2 (local) train(..., self = self)
       22.                       +-ggplot2::guide_train(params, scale, aesthetic)
       23.                       \-distributional:::guide_train.level_guide(params, scale, aesthetic)
      
      [ FAIL 2 | WARN 3 | SKIP 0 | PASS 498 ]
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

# dynamAedes

<details>

* Version: 2.2.8
* GitHub: https://github.com/mattmar/dynamAedes
* Source code: https://github.com/cran/dynamAedes
* Date/Publication: 2024-01-08 23:00:03 UTC
* Number of recursive dependencies: 127

Run `revdepcheck::cloud_details(, "dynamAedes")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘dynamAedes_01_punctual.Rmd’ using rmarkdown
    starting worker pid=3991 on localhost:11769 at 17:08:19.406
    Loading required package: dynamAedes
    loaded dynamAedes and set parent environment
    
      |                                                                            
      |%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%| 100%
    Quitting from lines 182-199 [unnamed-chunk-14] (dynamAedes_01_punctual.Rmd)
    Error: processing vignette 'dynamAedes_01_punctual.Rmd' failed with diagnostics:
    ...
    
    --- re-building ‘dynamAedes_05_spreader.Rmd’ using rmarkdown
    --- finished re-building ‘dynamAedes_05_spreader.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘dynamAedes_01_punctual.Rmd’ ‘dynamAedes_02_local.Rmd’
      ‘dynamAedes_03_regional.Rmd’ ‘dynamAedes_04_uncompModel.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘b_probabilistic.Rmd’ using rmarkdown
    --- finished re-building ‘b_probabilistic.Rmd’
    
    --- re-building ‘c_multiple_bias.Rmd’ using rmarkdown
    --- finished re-building ‘c_multiple_bias.Rmd’
    
    --- re-building ‘d_other_sens.Rmd’ using rmarkdown
    
    Quitting from lines 136-168 [conf_ext_apply] (d_other_sens.Rmd)
    ...
      collapsing to unique 'x' values
    Warning in regularize.values(x, y, ties, missing(ties), na.rm = na.rm) :
      collapsing to unique 'x' values
    --- finished re-building ‘episensr.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘d_other_sens.Rmd’
    
    Error: Vignette re-building failed.
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
      
      [ FAIL 2 | WARN 5 | SKIP 1 | PASS 269 ]
      Error: Test failures
      In addition: Warning messages:
      1: `flatten()` is deprecated as of rlang 1.1.0.
      ℹ Please use `purrr::list_flatten()` or `purrr::list_c()`.
      This warning is displayed once every 8 hours. 
      2: `squash()` is deprecated as of rlang 1.1.0.
      This warning is displayed once every 8 hours. 
      Execution halted
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
     14.                       ├─rlang::inject(self$extract_key(scale, !!!params))
     15.                       └─self$extract_key(...)
     16.                         └─ggplot2 (local) extract_key(...)
     17.                           └─Guide$extract_key(scale, aesthetic, ...)
     18.                             └─ggplot2 (local) extract_key(...)
     19.                               └─scale$get_labels(breaks)
     20.                                 └─ggplot2 (local) get_labels(..., self = self)
     21.                                   └─cli::cli_abort(...)
     22.                                     └─rlang::abort(...)
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
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 98 ]
      
    ...
       15.                         └─Guide$extract_key(scale, aesthetic, ...)
       16.                           └─ggplot2 (local) extract_key(...)
       17.                             └─scale$get_labels(breaks)
       18.                               └─ggplot2 (local) get_labels(..., self = self)
       19.                                 └─cli::cli_abort(...)
       20.                                   └─rlang::abort(...)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 98 ]
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
    `breaks` and `labels` have different lengths.
    --- failed re-building ‘feasts.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘feasts.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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
      
      [ FAIL 3 | WARN 2 | SKIP 25 | PASS 479 ]
      Error: Test failures
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
    --- failed re-building ‘ggbrain_introduction.Rmd’
    
    --- re-building ‘ggbrain_labels.Rmd’ using rmarkdown
    --- finished re-building ‘ggbrain_labels.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ggbrain_introduction.Rmd’
    
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

# gghdr

<details>

* Version: 0.2.0
* GitHub: https://github.com/Sayani07/gghdr
* Source code: https://github.com/cran/gghdr
* Date/Publication: 2022-10-27 15:15:19 UTC
* Number of recursive dependencies: 92

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
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    Warning: The `scale_name` argument of `discrete_scale()` is deprecated as of ggplot2
    3.5.0.
    Warning: The S3 guide system was deprecated in ggplot2 3.5.0.
    ℹ It has been replaced by a ggproto system that can be extended.
    Error in if (guide$reverse) { : argument is of length zero
    Calls: <Anonymous> ... <Anonymous> -> train -> guide_train -> guide_train.prob_guide
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
       14.                     └─base::mapply(FUN = f, ..., SIMPLIFY = FALSE)
       15.                       └─ggplot2 (local) `<fn>`(...)
       16.                         └─guide$train(param, scale, aes, title = labels[[aes]])
       17.                           └─ggplot2 (local) train(..., self = self)
       18.                             ├─ggplot2::guide_train(params, scale, aesthetic)
       19.                             └─gghdr:::guide_train.prob_guide(params, scale, aesthetic)
      
      [ FAIL 3 | WARN 6 | SKIP 1 | PASS 8 ]
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
    argument is of length zero
    --- failed re-building ‘gghdr.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘gghdr.Rmd’
    
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

# ggparallel

<details>

* Version: 0.3.0
* GitHub: https://github.com/heike/ggparallel
* Source code: https://github.com/cran/ggparallel
* Date/Publication: 2024-01-29 18:50:06 UTC
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
       12. │       └─ggplot2 (local) compute_aesthetics(..., self = self)
       13. └─base::.handleSimpleError(...)
       14.   └─rlang (local) h(simpleError(msg, call))
       15.     └─handlers[[1L]](cnd)
       16.       └─cli::cli_abort(...)
       17.         └─rlang::abort(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

# ggpp

<details>

* Version: 0.5.6
* GitHub: https://github.com/aphalo/ggpp
* Source code: https://github.com/cran/ggpp
* Date/Publication: 2024-01-09 07:00:02 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::cloud_details(, "ggpp")` for more info

</details>

## Newly broken

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
      test-annotation_ticks.R.......   21 tests [0;32mOK[0m [0;34m0.8s[0m
      
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

# ggstatsplot

<details>

* Version: 0.12.2
* GitHub: https://github.com/IndrajeetPatil/ggstatsplot
* Source code: https://github.com/cran/ggstatsplot
* Date/Publication: 2024-01-14 14:30:02 UTC
* Number of recursive dependencies: 166

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

# gsDesign

<details>

* Version: 3.6.0
* GitHub: https://github.com/keaven/gsDesign
* Source code: https://github.com/cran/gsDesign
* Date/Publication: 2023-11-12 05:43:19 UTC
* Number of recursive dependencies: 102

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

# injurytools

<details>

* Version: 1.0.3
* GitHub: https://github.com/lzumeta/injurytools
* Source code: https://github.com/cran/injurytools
* Date/Publication: 2023-11-14 17:20:05 UTC
* Number of recursive dependencies: 152

Run `revdepcheck::cloud_details(, "injurytools")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘estimate-epi-measures.Rmd’ using rmarkdown
    --- finished re-building ‘estimate-epi-measures.Rmd’
    
    --- re-building ‘model-injury-data-i.Rmd’ using rmarkdown
    --- finished re-building ‘model-injury-data-i.Rmd’
    
    --- re-building ‘model-injury-data-ii.Rmd’ using rmarkdown
    
    Quitting from lines 266-298 [unnamed-chunk-11] (model-injury-data-ii.Rmd)
    ...
    --- finished re-building ‘prepare-injury-data.Rmd’
    
    --- re-building ‘visualize-injury-data.Rmd’ using rmarkdown
    --- finished re-building ‘visualize-injury-data.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘model-injury-data-ii.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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
    
    > ### Name: geom_arrow
    > ### Title: Arrows
    > ### Aliases: geom_arrow geom_vector GeomArrow StatArrow
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    ...
      9.             └─base::Map(...)
     10.               └─base::mapply(FUN = f, ..., SIMPLIFY = FALSE)
     11.                 └─ggplot2 (local) `<fn>`(guide = dots[[1L]][[1L]], param = dots[[2L]][[1L]])
     12.                   └─guide$process_layers(param, layers, data)
     13.                     └─ggplot2 (local) process_layers(..., self = self)
     14.                       ├─ggplot2::guide_geom(params, layers, default_mapping = NULL)
     15.                       └─ggplot2:::guide_geom.default(params, layers, default_mapping = NULL)
     16.                         └─cli::cli_abort(...)
     17.                           └─rlang::abort(...)
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
    <Guide> classes have been rewritten as <ggproto> classes.
    The old S3 guide methods have been superseded.
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

# NAIR

<details>

* Version: 1.0.3
* GitHub: https://github.com/mlizhangx/Network-Analysis-for-Repertoire-Sequencing-
* Source code: https://github.com/cran/NAIR
* Date/Publication: 2024-01-09 17:00:02 UTC
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
      ── Failure ('test_functions.R:1419:3'): plots legends behave correctly ─────────
      sc_net$plots$SampleID$guides$size$name (`actual`) not equal to "legend" (`expected`).
      
      `actual` is NULL
      `expected` is a character vector ('legend')
      
      [ FAIL 48 | WARN 0 | SKIP 0 | PASS 1171 ]
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
      Actual value: "List of 11\\n \$ data       : tibble \[200 × 5\] \(S3: tbl_df/tbl/data\.frame\)\\n  \.\.\$ Period    : chr \[1:200\] "2000-2001" "2000-2001" "2000-2001" "2000-2001" \.\.\.\\n  \.\.\$ area_gross: num \[1:200\] 0\.000388 0\.000379 0\.00038 0\.000411 0\.000403 0\.000416 0\.000368 0\.000445 0\.000387 0\.000399 \.\.\.\\n  \.\.\$ From      : Factor w/ 5 levels "GUP","OZS","PSN",\.\.: 3 3 3 3 4 4 4 4 2 2 \.\.\.\\n  \.\.\$ To        : Factor w/ 5 levels "GUP","OZS","PSN",\.\.: 4 2 1 5 3 2 1 5 3 4 \.\.\.\\n  \.\.\$ changes   : chr \[1:200\] "Gain" "Gain" "Gain" "Gain" \.\.\.\\n \$ layers     :List of 4\\n  \.\.\$ :Classes 'LayerInstance', 'Layer', 'ggproto', 'gg' <ggproto object: Class LayerInstance, Layer, gg>\\n    aes_params: list\\n    compute_aesthetics: function\\n    compute_geom_1: function\\n    compute_geom_2: function\\n    compute_position: function\\n    compute_statistic: function\\n    computed_geom_params: NULL\\n    computed_mapping: NULL\\n    computed_stat_params: NULL\\n    constructor: call\\n    data: waiver\\n    draw_geom: function\\n    finish_statistics: function\\n    geom: <ggproto object: Class GeomBar, GeomRect, Geom, gg>\\n        aesthetics: function\\n        default_aes: uneval\\n        draw_group: function\\n        draw_key: function\\n        draw_layer: function\\n        draw_panel: function\\n        extra_params: just na\.rm orientation\\n        handle_na: function\\n        non_missing_aes: xmin xmax ymin ymax\\n        optional_aes: \\n        parameters: function\\n        rename_size: TRUE\\n        required_aes: x y\\n        setup_data: function\\n        setup_params: function\\n        use_defaults: function\\n        super:  <ggproto object: Class GeomRect, Geom, gg>\\n    geom_params: list\\n    inherit\.aes: TRUE\\n    layer_data: function\\n    map_statistic: function\\n    mapping: uneval\\n    position: <ggproto object: Class PositionStack, Position, gg>\\n        compute_layer: function\\n        compute_panel: function\\n        fill: FALSE\\n        required_aes: \\n        reverse: FALSE\\n        setup_data: function\\n        setup_params: function\\n        type: NULL\\n        vjust: 1\\n        super:  <ggproto object: Class Position, gg>\\n    print: function\\n    setup_layer: function\\n    show\.legend: NA\\n    stat: <ggproto object: Class StatIdentity, Stat, gg>\\n        aesthetics: function\\n        compute_group: function\\n        compute_layer: function\\n        compute_panel: function\\n        default_aes: uneval\\n        dropped_aes: \\n        extra_params: na\.rm\\n        finish_layer: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        required_aes: \\n        retransform: TRUE\\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Stat, gg>\\n    stat_params: list\\n    super:  <ggproto object: Class Layer, gg> \\n  \.\.\$ :Classes 'LayerInstance', 'Layer', 'ggproto', 'gg' <ggproto object: Class LayerInstance, Layer, gg>\\n    aes_params: list\\n    compute_aesthetics: function\\n    compute_geom_1: function\\n    compute_geom_2: function\\n    compute_position: function\\n    compute_statistic: function\\n    computed_geom_params: NULL\\n    computed_mapping: NULL\\n    computed_stat_params: NULL\\n    constructor: call\\n    data: tbl_df, tbl, data\.frame\\n    draw_geom: function\\n    finish_statistics: function\\n    geom: <ggproto object: Class GeomBar, GeomRect, Geom, gg>\\n        aesthetics: function\\n        default_aes: uneval\\n        draw_group: function\\n        draw_key: function\\n        draw_layer: function\\n        draw_panel: function\\n        extra_params: just na\.rm orientation\\n        handle_na: function\\n        non_missing_aes: xmin xmax ymin ymax\\n        optional_aes: \\n        parameters: function\\n        rename_size: TRUE\\n        required_aes: x y\\n        setup_data: function\\n        setup_params: function\\n        use_defaults: function\\n        super:  <ggproto object: Class GeomRect, Geom, gg>\\n    geom_params: list\\n    inherit\.aes: FALSE\\n    layer_data: function\\n    map_statistic: function\\n    mapping: uneval\\n    position: <ggproto object: Class PositionStack, Position, gg>\\n        compute_layer: function\\n        compute_panel: function\\n        fill: FALSE\\n        required_aes: \\n        reverse: FALSE\\n        setup_data: function\\n        setup_params: function\\n        type: NULL\\n        vjust: 1\\n        super:  <ggproto object: Class Position, gg>\\n    print: function\\n    setup_layer: function\\n    show\.legend: NA\\n    stat: <ggproto object: Class StatIdentity, Stat, gg>\\n        aesthetics: function\\n        compute_group: function\\n        compute_layer: function\\n        compute_panel: function\\n        default_aes: uneval\\n        dropped_aes: \\n        extra_params: na\.rm\\n        finish_layer: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        required_aes: \\n        retransform: TRUE\\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Stat, gg>\\n    stat_params: list\\n    super:  <ggproto object: Class Layer, gg> \\n  \.\.\$ :Classes 'LayerInstance', 'Layer', 'ggproto', 'gg' <ggproto object: Class LayerInstance, Layer, gg>\\n    aes_params: list\\n    compute_aesthetics: function\\n    compute_geom_1: function\\n    compute_geom_2: function\\n    compute_position: function\\n    compute_statistic: function\\n    computed_geom_params: NULL\\n    computed_mapping: NULL\\n    computed_stat_params: NULL\\n    constructor: call\\n    data: tbl_df, tbl, data\.frame\\n    draw_geom: function\\n    finish_statistics: function\\n    geom: <ggproto object: Class GeomSegment, Geom, gg>\\n        aesthetics: function\\n        default_aes: uneval\\n        draw_group: function\\n        draw_key: function\\n        draw_layer: function\\n        draw_panel: function\\n        extra_params: na\.rm\\n        handle_na: function\\n        non_missing_aes: linetype linewidth\\n        optional_aes: \\n        parameters: function\\n        rename_size: TRUE\\n        required_aes: x y xend\|yend\\n        setup_data: function\\n        setup_params: function\\n        use_defaults: function\\n        super:  <ggproto object: Class Geom, gg>\\n    geom_params: list\\n    inherit\.aes: TRUE\\n    layer_data: function\\n    map_statistic: function\\n    mapping: uneval\\n    position: <ggproto object: Class PositionIdentity, Position, gg>\\n        compute_layer: function\\n        compute_panel: function\\n        required_aes: \\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Position, gg>\\n    print: function\\n    setup_layer: function\\n    show\.legend: NA\\n    stat: <ggproto object: Class StatIdentity, Stat, gg>\\n        aesthetics: function\\n        compute_group: function\\n        compute_layer: function\\n        compute_panel: function\\n        default_aes: uneval\\n        dropped_aes: \\n        extra_params: na\.rm\\n        finish_layer: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        required_aes: \\n        retransform: TRUE\\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Stat, gg>\\n    stat_params: list\\n    super:  <ggproto object: Class Layer, gg> \\n  \.\.\$ :Classes 'LayerInstance', 'Layer', 'ggproto', 'gg' <ggproto object: Class LayerInstance, Layer, gg>\\n    aes_params: list\\n    compute_aesthetics: function\\n    compute_geom_1: function\\n    compute_geom_2: function\\n    compute_position: function\\n    compute_statistic: function\\n    computed_geom_params: NULL\\n    computed_mapping: NULL\\n    computed_stat_params: NULL\\n    constructor: call\\n    data: data\.frame\\n    draw_geom: function\\n    finish_statistics: function\\n    geom: <ggproto object: Class GeomHline, Geom, gg>\\n        aesthetics: function\\n        check_constant_aes: FALSE\\n        default_aes: uneval\\n        draw_group: function\\n        draw_key: function\\n        draw_layer: function\\n        draw_panel: function\\n        extra_params: na\.rm\\n        handle_na: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        rename_size: TRUE\\n        required_aes: yintercept\\n        setup_data: function\\n        setup_params: function\\n        use_defaults: function\\n        super:  <ggproto object: Class Geom, gg>\\n    geom_params: list\\n    inherit\.aes: FALSE\\n    layer_data: function\\n    map_statistic: function\\n    mapping: uneval\\n    position: <ggproto object: Class PositionIdentity, Position, gg>\\n        compute_layer: function\\n        compute_panel: function\\n        required_aes: \\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Position, gg>\\n    print: function\\n    setup_layer: function\\n    show\.legend: FALSE\\n    stat: <ggproto object: Class StatIdentity, Stat, gg>\\n        aesthetics: function\\n        compute_group: function\\n        compute_layer: function\\n        compute_panel: function\\n        default_aes: uneval\\n        dropped_aes: \\n        extra_params: na\.rm\\n        finish_layer: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        required_aes: \\n        retransform: TRUE\\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Stat, gg>\\n    stat_params: list\\n    super:  <ggproto object: Class Layer, gg> \\n \$ scales     :Classes 'ScalesList', 'ggproto', 'gg' <ggproto object: Class ScalesList, gg>\\n    add: function\\n    add_defaults: function\\n    add_missing: function\\n    backtransform_df: function\\n    clone: function\\n    find: function\\n    get_scales: function\\n    has_scale: function\\n    input: function\\n    map_df: function\\n    n: function\\n    non_position_scales: function\\n    scales: list\\n    train_df: function\\n    transform_df: function\\n    super:  <ggproto object: Class ScalesList, gg> \\n \$ guides     :Classes 'Guides', 'ggproto', 'gg' <ggproto object: Class Guides, gg>\\n    add: function\\n    assemble: function\\n    build: function\\n    draw: function\\n    get_custom: function\\n    get_guide: function\\n    get_params: function\\n    get_position: function\\n    guides: NULL\\n    merge: function\\n    missing: <ggproto object: Class GuideNone, Guide, gg>\\n        add_title: function\\n        arrange_layout: function\\n        assemble_drawing: function\\n        available_aes: any\\n        build_decor: function\\n        build_labels: function\\n        build_ticks: function\\n        build_title: function\\n        draw: function\\n        draw_early_exit: function\\n        elements: list\\n        extract_decor: function\\n        extract_key: function\\n        extract_params: function\\n        get_layer_key: function\\n        hashables: list\\n        measure_grobs: function\\n        merge: function\\n        override_elements: function\\n        params: list\\n        process_layers: function\\n        setup_elements: function\\n        setup_params: function\\n        train: function\\n        transform: function\\n        super:  <ggproto object: Class GuideNone, Guide, gg>\\n    package_box: function\\n    print: function\\n    process_layers: function\\n    setup: function\\n    subset_guides: function\\n    train: function\\n    update_params: function\\n    super:  <ggproto object: Class Guides, gg> \\n \$ mapping    :List of 2\\n  \.\.\$ x: language ~To\\n  \.\. \.\.- attr\(\*, "\.Environment"\)=<environment: 0x556687e8b5d0> \\n  \.\.\$ y: language ~area_gross\\n  \.\. \.\.- attr\(\*, "\.Environment"\)=<environment: 0x556687e8b5d0> \\n  \.\.- attr\(\*, "class"\)= chr "uneval"\\n \$ theme      :List of 1\\n  \.\.\$ plot\.title:List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : NULL\\n  \.\. \.\.\$ size         : NULL\\n  \.\. \.\.\$ hjust        : num 0\.5\\n  \.\. \.\.\$ vjust        : NULL\\n  \.\. \.\.\$ angle        : NULL\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : NULL\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi FALSE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.- attr\(\*, "complete"\)= logi FALSE\\n  \.\.- attr\(\*, "validate"\)= logi TRUE\\n \$ coordinates:Classes 'CoordCartesian', 'Coord', 'ggproto', 'gg' <ggproto object: Class CoordCartesian, Coord, gg>\\n    aspect: function\\n    backtransform_range: function\\n    clip: on\\n    default: TRUE\\n    distance: function\\n    expand: TRUE\\n    is_free: function\\n    is_linear: function\\n    labels: function\\n    limits: list\\n    modify_scales: function\\n    range: function\\n    render_axis_h: function\\n    render_axis_v: function\\n    render_bg: function\\n    render_fg: function\\n    setup_data: function\\n    setup_layout: function\\n    setup_panel_guides: function\\n    setup_panel_params: function\\n    setup_params: function\\n    train_panel_guides: function\\n    transform: function\\n    super:  <ggproto object: Class CoordCartesian, Coord, gg> \\n \$ facet      :Classes 'FacetNull', 'Facet', 'ggproto', 'gg' <ggproto object: Class FacetNull, Facet, gg>\\n    compute_layout: function\\n    draw_back: function\\n    draw_front: function\\n    draw_labels: function\\n    draw_panels: function\\n    finish_data: function\\n    init_scales: function\\n    map_data: function\\n    params: list\\n    setup_data: function\\n    setup_params: function\\n    shrink: TRUE\\n    train_scales: function\\n    vars: function\\n    super:  <ggproto object: Class FacetNull, Facet, gg> \\n \$ plot_env   :<environment: 0x556687e8b5d0> \\n \$ layout     :Classes 'Layout', 'ggproto', 'gg' <ggproto object: Class Layout, gg>\\n    coord: NULL\\n    coord_params: list\\n    facet: NULL\\n    facet_params: list\\n    finish_data: function\\n    get_scales: function\\n    layout: NULL\\n    map_position: function\\n    panel_params: NULL\\n    panel_scales_x: NULL\\n    panel_scales_y: NULL\\n    render: function\\n    render_labels: function\\n    reset_scales: function\\n    resolve_label: function\\n    setup: function\\n    setup_panel_guides: function\\n    setup_panel_params: function\\n    train_position: function\\n    super:  <ggproto object: Class Layout, gg> \\n \$ labels     :List of 7\\n  \.\.\$ title     : NULL\\n  \.\.\$ y         : chr "Area \(Km2\)"\\n  \.\.\$ x         : chr "LUC category"\\n  \.\.\$ fill      : chr "Changes"\\n  \.\.\$ xend      : chr "as\.numeric\(To\) \+ 0\.3"\\n  \.\.\$ yend      : chr "area"\\n  \.\.\$ yintercept: chr "yintercept"\\n - attr\(\*, "class"\)= chr \[1:2\] "gg" "ggplot""
      Backtrace:
          ▆
       1. └─testthat::expect_output(...) at test_plots.R:59:3
       2.   └─testthat::expect_match(...)
       3.     └─testthat:::expect_match_(...)
      
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 110 ]
      Error: Test failures
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
    could not find function "guides_train"
    --- failed re-building ‘plot3logit-overview.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘plot3logit-overview.Rmd’
    
    Error: Vignette re-building failed.
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

# Repliscope

<details>

* Version: 1.1.1
* GitHub: NA
* Source code: https://github.com/cran/Repliscope
* Date/Publication: 2022-09-13 07:20:02 UTC
* Number of recursive dependencies: 65

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
     28. │                                   └─self$scale$rescale(x, self$limits, self$continuous_range)
     29. │                                     └─ggplot2 (local) rescale(..., self = self)
     30. │                                       └─self$rescaler(x, from = range)
     31. │                                         └─scales (local) rescaler(...)
     32. └─base::.handleSimpleError(...)
     33.   └─rlang (local) h(simpleError(msg, call))
     34.     └─handlers[[1L]](cnd)
     35.       └─cli::cli_abort(...)
     36.         └─rlang::abort(...)
    Execution halted
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

# saros

<details>

* Version: 1.0.1
* GitHub: https://github.com/NIFU-NO/saros
* Source code: https://github.com/cran/saros
* Date/Publication: 2024-01-26 11:30:02 UTC
* Number of recursive dependencies: 141

Run `revdepcheck::cloud_details(, "saros")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘saros-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: embed_cat_prop_plot
    > ### Title: Embed Interactive Categorical Plot
    > ### Aliases: embed_cat_prop_plot
    > 
    > ### ** Examples
    > 
    > embed_cat_prop_plot(data = ex_survey, dep = b_1:b_3)
    ...
     16.                       │ ├─base::tryCatch(...)
     17.                       │ │ └─base (local) tryCatchList(expr, classes, parentenv, handlers)
     18.                       │ │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
     19.                       │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
     20.                       │ └─base::withCallingHandlers(...)
     21.                       ├─ggplot2::merge_element(t2[[item]], t1[[item]])
     22.                       └─ggplot2:::merge_element.element(t2[[item]], t1[[item]])
     23.                         └─cli::cli_abort("Only elements of the same class can be merged.")
     24.                           └─rlang::abort(...)
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
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 184 ]
    ...
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test-draft_report.R:4:3'): draft_report ─────────────────────────────
      Error in `setup_elements(...)`: Can't merge the `text` theme element.
      Caused by error in `merge_element()`:
      ! Only elements of the same class can be merged.
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 184 ]
      Error: Test failures
      Execution halted
    ```

# scCustomize

<details>

* Version: 2.0.1
* GitHub: https://github.com/samuel-marsh/scCustomize
* Source code: https://github.com/cran/scCustomize
* Date/Publication: 2023-11-17 19:00:03 UTC
* Number of recursive dependencies: 265

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

# spqdep

<details>

* Version: 0.1.3.2
* GitHub: NA
* Source code: https://github.com/cran/spqdep
* Date/Publication: 2024-02-05 12:30:02 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::cloud_details(, "spqdep")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘spq_userguide.Rmd’ using rmarkdown
    
    Quitting from lines 500-501 [unnamed-chunk-24] (spq_userguide.Rmd)
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
    Namespace in Imports field not imported from: ‘lwgeom’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4 marked UTF-8 strings
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
      Actual value: "List of 11\\n \$ data       :'data\.frame':\\t6 obs\. of  5 variables:\\n  \.\.\$ dose      : num \[1:6\] 0\.5 0\.5 1 1 2 2\\n  \.\.\$ supp      : chr \[1:6\] "OJ" "VC" "OJ" "VC" \.\.\.\\n  \.\.\$ center    : num \[1:6\] 13 8 22 17 26 26\\n  \.\.\$ lowerwidth: num \[1:6\] -3 -2 -3 -2 -2 -4\\n  \.\.\$ upperwidth: num \[1:6\] 3 2 3 2 2 4\\n \$ layers     :List of 4\\n  \.\.\$ :Classes 'LayerInstance', 'Layer', 'ggproto', 'gg' <ggproto object: Class LayerInstance, Layer, gg>\\n    aes_params: list\\n    compute_aesthetics: function\\n    compute_geom_1: function\\n    compute_geom_2: function\\n    compute_position: function\\n    compute_statistic: function\\n    computed_geom_params: NULL\\n    computed_mapping: NULL\\n    computed_stat_params: NULL\\n    constructor: call\\n    data: data\.frame\\n    draw_geom: function\\n    finish_statistics: function\\n    geom: <ggproto object: Class GeomViolin, Geom, gg>\\n        aesthetics: function\\n        default_aes: uneval\\n        draw_group: function\\n        draw_key: function\\n        draw_layer: function\\n        draw_panel: function\\n        extra_params: na\.rm orientation lineend linejoin linemitre\\n        handle_na: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        rename_size: TRUE\\n        required_aes: x y\\n        setup_data: function\\n        setup_params: function\\n        use_defaults: function\\n        super:  <ggproto object: Class Geom, gg>\\n    geom_params: list\\n    inherit\.aes: TRUE\\n    layer_data: function\\n    map_statistic: function\\n    mapping: uneval\\n    position: <ggproto object: Class PositionDodge, Position, gg>\\n        compute_layer: function\\n        compute_panel: function\\n        preserve: total\\n        required_aes: \\n        setup_data: function\\n        setup_params: function\\n        width: 0\.75\\n        super:  <ggproto object: Class PositionDodge, Position, gg>\\n    print: function\\n    setup_layer: function\\n    show\.legend: NA\\n    stat: <ggproto object: Class StatYdensity, Stat, gg>\\n        aesthetics: function\\n        compute_group: function\\n        compute_layer: function\\n        compute_panel: function\\n        default_aes: uneval\\n        dropped_aes: weight\\n        extra_params: na\.rm orientation\\n        finish_layer: function\\n        non_missing_aes: weight\\n        optional_aes: \\n        parameters: function\\n        required_aes: x y\\n        retransform: TRUE\\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Stat, gg>\\n    stat_params: list\\n    super:  <ggproto object: Class Layer, gg> \\n  \.\.\$ :Classes 'LayerInstance', 'Layer', 'ggproto', 'gg' <ggproto object: Class LayerInstance, Layer, gg>\\n    aes_params: list\\n    compute_aesthetics: function\\n    compute_geom_1: function\\n    compute_geom_2: function\\n    compute_position: function\\n    compute_statistic: function\\n    computed_geom_params: NULL\\n    computed_mapping: NULL\\n    computed_stat_params: NULL\\n    constructor: call\\n    data: data\.frame\\n    draw_geom: function\\n    finish_statistics: function\\n    geom: <ggproto object: Class GeomPoint, Geom, gg>\\n        aesthetics: function\\n        default_aes: uneval\\n        draw_group: function\\n        draw_key: function\\n        draw_layer: function\\n        draw_panel: function\\n        extra_params: na\.rm\\n        handle_na: function\\n        non_missing_aes: size shape colour\\n        optional_aes: \\n        parameters: function\\n        rename_size: FALSE\\n        required_aes: x y\\n        setup_data: function\\n        setup_params: function\\n        use_defaults: function\\n        super:  <ggproto object: Class Geom, gg>\\n    geom_params: list\\n    inherit\.aes: TRUE\\n    layer_data: function\\n    map_statistic: function\\n    mapping: uneval\\n    position: <ggproto object: Class PositionJitterdodge, Position, gg>\\n        compute_layer: function\\n        compute_panel: function\\n        dodge\.width: 0\.75\\n        jitter\.height: 0\\n        jitter\.width: 0\.1\\n        required_aes: x y\\n        seed: 208773918\\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class PositionJitterdodge, Position, gg>\\n    print: function\\n    setup_layer: function\\n    show\.legend: NA\\n    stat: <ggproto object: Class StatIdentity, Stat, gg>\\n        aesthetics: function\\n        compute_group: function\\n        compute_layer: function\\n        compute_panel: function\\n        default_aes: uneval\\n        dropped_aes: \\n        extra_params: na\.rm\\n        finish_layer: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        required_aes: \\n        retransform: TRUE\\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Stat, gg>\\n    stat_params: list\\n    super:  <ggproto object: Class Layer, gg> \\n  \.\.\$ :Classes 'LayerInstance', 'Layer', 'ggproto', 'gg' <ggproto object: Class LayerInstance, Layer, gg>\\n    aes_params: list\\n    compute_aesthetics: function\\n    compute_geom_1: function\\n    compute_geom_2: function\\n    compute_position: function\\n    compute_statistic: function\\n    computed_geom_params: NULL\\n    computed_mapping: NULL\\n    computed_stat_params: NULL\\n    constructor: call\\n    data: waiver\\n    draw_geom: function\\n    finish_statistics: function\\n    geom: <ggproto object: Class GeomPoint, Geom, gg>\\n        aesthetics: function\\n        default_aes: uneval\\n        draw_group: function\\n        draw_key: function\\n        draw_layer: function\\n        draw_panel: function\\n        extra_params: na\.rm\\n        handle_na: function\\n        non_missing_aes: size shape colour\\n        optional_aes: \\n        parameters: function\\n        rename_size: FALSE\\n        required_aes: x y\\n        setup_data: function\\n        setup_params: function\\n        use_defaults: function\\n        super:  <ggproto object: Class Geom, gg>\\n    geom_params: list\\n    inherit\.aes: TRUE\\n    layer_data: function\\n    map_statistic: function\\n    mapping: uneval\\n    position: <ggproto object: Class PositionDodge, Position, gg>\\n        compute_layer: function\\n        compute_panel: function\\n        preserve: total\\n        required_aes: \\n        setup_data: function\\n        setup_params: function\\n        width: 0\.75\\n        super:  <ggproto object: Class PositionDodge, Position, gg>\\n    print: function\\n    setup_layer: function\\n    show\.legend: NA\\n    stat: <ggproto object: Class StatIdentity, Stat, gg>\\n        aesthetics: function\\n        compute_group: function\\n        compute_layer: function\\n        compute_panel: function\\n        default_aes: uneval\\n        dropped_aes: \\n        extra_params: na\.rm\\n        finish_layer: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        required_aes: \\n        retransform: TRUE\\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Stat, gg>\\n    stat_params: list\\n    super:  <ggproto object: Class Layer, gg> \\n  \.\.\$ :Classes 'LayerInstance', 'Layer', 'ggproto', 'gg' <ggproto object: Class LayerInstance, Layer, gg>\\n    aes_params: list\\n    compute_aesthetics: function\\n    compute_geom_1: function\\n    compute_geom_2: function\\n    compute_position: function\\n    compute_statistic: function\\n    computed_geom_params: NULL\\n    computed_mapping: NULL\\n    computed_stat_params: NULL\\n    constructor: call\\n    data: waiver\\n    draw_geom: function\\n    finish_statistics: function\\n    geom: <ggproto object: Class GeomsuperbErrorbar, Geom, gg>\\n        aesthetics: function\\n        default_aes: uneval\\n        draw_group: function\\n        draw_key: function\\n        draw_layer: function\\n        draw_panel: function\\n        extra_params: na\.rm orientation direction tipformat tipgap pointing\\n        handle_na: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        rename_size: FALSE\\n        required_aes: x\|y ymin\|xmin ymax\|xmax\\n        setup_data: function\\n        setup_params: function\\n        use_defaults: function\\n        super:  <ggproto object: Class Geom, gg>\\n    geom_params: list\\n    inherit\.aes: TRUE\\n    layer_data: function\\n    map_statistic: function\\n    mapping: uneval\\n    position: <ggproto object: Class PositionDodge, Position, gg>\\n        compute_layer: function\\n        compute_panel: function\\n        preserve: total\\n        required_aes: \\n        setup_data: function\\n        setup_params: function\\n        width: 0\.75\\n        super:  <ggproto object: Class PositionDodge, Position, gg>\\n    print: function\\n    setup_layer: function\\n    show\.legend: NA\\n    stat: <ggproto object: Class StatIdentity, Stat, gg>\\n        aesthetics: function\\n        compute_group: function\\n        compute_layer: function\\n        compute_panel: function\\n        default_aes: uneval\\n        dropped_aes: \\n        extra_params: na\.rm\\n        finish_layer: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        required_aes: \\n        retransform: TRUE\\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Stat, gg>\\n    stat_params: list\\n    super:  <ggproto object: Class Layer, gg> \\n \$ scales     :Classes 'ScalesList', 'ggproto', 'gg' <ggproto object: Class ScalesList, gg>\\n    add: function\\n    add_defaults: function\\n    add_missing: function\\n    backtransform_df: function\\n    clone: function\\n    find: function\\n    get_scales: function\\n    has_scale: function\\n    input: function\\n    map_df: function\\n    n: function\\n    non_position_scales: function\\n    scales: list\\n    train_df: function\\n    transform_df: function\\n    super:  <ggproto object: Class ScalesList, gg> \\n \$ guides     :Classes 'Guides', 'ggproto', 'gg' <ggproto object: Class Guides, gg>\\n    add: function\\n    assemble: function\\n    build: function\\n    draw: function\\n    get_custom: function\\n    get_guide: function\\n    get_params: function\\n    get_position: function\\n    guides: NULL\\n    merge: function\\n    missing: <ggproto object: Class GuideNone, Guide, gg>\\n        add_title: function\\n        arrange_layout: function\\n        assemble_drawing: function\\n        available_aes: any\\n        build_decor: function\\n        build_labels: function\\n        build_ticks: function\\n        build_title: function\\n        draw: function\\n        draw_early_exit: function\\n        elements: list\\n        extract_decor: function\\n        extract_key: function\\n        extract_params: function\\n        get_layer_key: function\\n        hashables: list\\n        measure_grobs: function\\n        merge: function\\n        override_elements: function\\n        params: list\\n        process_layers: function\\n        setup_elements: function\\n        setup_params: function\\n        train: function\\n        transform: function\\n        super:  <ggproto object: Class GuideNone, Guide, gg>\\n    package_box: function\\n    print: function\\n    process_layers: function\\n    setup: function\\n    subset_guides: function\\n    train: function\\n    update_params: function\\n    super:  <ggproto object: Class Guides, gg> \\n \$ mapping    :List of 2\\n  \.\.\$ x     : language ~dose\\n  \.\. \.\.- attr\(\*, "\.Environment"\)=<environment: 0x5591d9010f60> \\n  \.\.\$ colour: language ~supp\\n  \.\. \.\.- attr\(\*, "\.Environment"\)=<environment: 0x5591d9010f60> \\n  \.\.- attr\(\*, "class"\)= chr "uneval"\\n \$ theme      : list\(\)\\n \$ coordinates:Classes 'CoordCartesian', 'Coord', 'ggproto', 'gg' <ggproto object: Class CoordCartesian, Coord, gg>\\n    aspect: function\\n    backtransform_range: function\\n    clip: on\\n    default: TRUE\\n    distance: function\\n    expand: TRUE\\n    is_free: function\\n    is_linear: function\\n    labels: function\\n    limits: list\\n    modify_scales: function\\n    range: function\\n    render_axis_h: function\\n    render_axis_v: function\\n    render_bg: function\\n    render_fg: function\\n    setup_data: function\\n    setup_layout: function\\n    setup_panel_guides: function\\n    setup_panel_params: function\\n    setup_params: function\\n    train_panel_guides: function\\n    transform: function\\n    super:  <ggproto object: Class CoordCartesian, Coord, gg> \\n \$ facet      :Classes 'FacetGrid', 'Facet', 'ggproto', 'gg' <ggproto object: Class FacetGrid, Facet, gg>\\n    compute_layout: function\\n    draw_back: function\\n    draw_front: function\\n    draw_labels: function\\n    draw_panels: function\\n    finish_data: function\\n    init_scales: function\\n    map_data: function\\n    params: list\\n    setup_data: function\\n    setup_params: function\\n    shrink: TRUE\\n    train_scales: function\\n    vars: function\\n    super:  <ggproto object: Class FacetGrid, Facet, gg> \\n \$ plot_env   :<environment: 0x5591d9010f60> \\n \$ layout     :Classes 'Layout', 'ggproto', 'gg' <ggproto object: Class Layout, gg>\\n    coord: NULL\\n    coord_params: list\\n    facet: NULL\\n    facet_params: list\\n    finish_data: function\\n    get_scales: function\\n    layout: NULL\\n    map_position: function\\n    panel_params: NULL\\n    panel_scales_x: NULL\\n    panel_scales_y: NULL\\n    render: function\\n    render_labels: function\\n    reset_scales: function\\n    resolve_label: function\\n    setup: function\\n    setup_panel_guides: function\\n    setup_panel_params: function\\n    train_position: function\\n    super:  <ggproto object: Class Layout, gg> \\n \$ labels     :List of 7\\n  \.\.\$ x     : chr "dose"\\n  \.\.\$ colour: chr "supp"\\n  \.\.\$ y     : chr "center"\\n  \.\.\$ fill  : chr "supp"\\n  \.\.\$ group : chr "supp"\\n  \.\.\$ ymin  : chr "center \+ lowerwidth"\\n  \.\.\$ ymax  : chr "center \+ upperwidth"\\n - attr\(\*, "class"\)= chr \[1:2\] "gg" "ggplot""
      Backtrace:
          ▆
       1. └─testthat::expect_output(str(p5), "List of 9") at test_subsidiaryFunctions.R:109:5
       2.   └─testthat::expect_match(...)
       3.     └─testthat:::expect_match_(...)
      
      [ FAIL 5 | WARN 0 | SKIP 0 | PASS 167 ]
      Error: Test failures
      Execution halted
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
* Number of recursive dependencies: 97

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

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘httpgd’
    ```

# tmt

<details>

* Version: 0.3.1-2
* GitHub: https://github.com/jansteinfeld/tmt
* Source code: https://github.com/cran/tmt
* Date/Publication: 2022-05-17 09:10:02 UTC
* Number of recursive dependencies: 95

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
* Number of recursive dependencies: 154

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

# umx

<details>

* Version: 4.19.0
* GitHub: https://github.com/tbates/umx
* Source code: https://github.com/cran/umx
* Date/Publication: 2023-11-16 22:50:06 UTC
* Number of recursive dependencies: 176

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
     14. │       └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     15. │         └─l$compute_geom_2(d)
     16. │           └─ggplot2 (local) compute_geom_2(..., self = self)
     17. │             └─self$geom$use_defaults(data, self$aes_params, modifiers)
     18. │               └─ggplot2 (local) use_defaults(..., self = self)
     19. │                 └─vctrs::vec_cbind(data[setdiff(names(data), aes_params)], !!!params[aes_params])
     20. └─vctrs:::stop_scalar_type(...)
     21.   └─vctrs:::stop_vctrs(...)
     22.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
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
      Caused by error in `vec_cbind()`:
      ! `label` must be a vector, not a call.
      
      [ FAIL 1 | WARN 4 | SKIP 26 | PASS 342 ]
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
      installed size is  7.5Mb
      sub-directories of 1Mb or more:
        R      1.9Mb
        help   5.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘foreign’ ‘psychTools’ ‘pwr’ ‘rmarkdown’
      All declared Imports should be used.
    ```

# visR

<details>

* Version: 0.4.0
* GitHub: https://github.com/openpharma/visR
* Source code: https://github.com/cran/visR
* Date/Publication: 2023-11-20 18:20:02 UTC
* Number of recursive dependencies: 148

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
      In for (i in seq_along(quosures)) { :
        closing unused connection 4 (https://raw.githubusercontent.com/vntkumar8/covid-survival/main/data/final.csv)
      Execution halted
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

