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
    
    > ### Name: coord_axes_inside
    > ### Title: Cartesian coordinates with interior axes
    > ### Aliases: coord_axes_inside
    > 
    > ### ** Examples
    > 
    > # A standard plot
    ...
    > p + coord_axes_inside()
    Theme element `panel.background` is missing
    Theme element `panel.grid.minor.y` is missing
    Theme element `panel.grid.minor.x` is missing
    Theme element `panel.grid.major.y` is missing
    Theme element `panel.grid.major.x` is missing
    Error in UseMethod("element_grob") : 
      no applicable method for 'element_grob' applied to an object of class "NULL"
    Calls: <Anonymous> ... lapply -> FUN -> draw_axis_labels -> exec -> <Anonymous>
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
      Theme element `panel.background` is missing
      Theme element `panel.grid.minor.y` is missing
    ...
       16.                             └─ggplot2 (local) build_labels(...)
       17.                               └─base::lapply(...)
       18.                                 └─ggplot2 (local) FUN(X[[i]], ...)
       19.                                   └─ggplot2:::draw_axis_labels(...)
       20.                                     ├─rlang::exec(...)
       21.                                     └─ggplot2 (local) `<fn>`(...)
      
      [ FAIL 1 | WARN 0 | SKIP 18 | PASS 750 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Facets.Rmd’ using rmarkdown
    --- finished re-building ‘Facets.Rmd’
    
    --- re-building ‘Miscellaneous.Rmd’ using rmarkdown
    
    Quitting from lines 199-204 [unnamed-chunk-13] (Miscellaneous.Rmd)
    Error: processing vignette 'Miscellaneous.Rmd' failed with diagnostics:
    no applicable method for 'element_grob' applied to an object of class "NULL"
    ...
    --- finished re-building ‘Statistics.Rmd’
    
    --- re-building ‘ggh4x.Rmd’ using rmarkdown
    --- finished re-building ‘ggh4x.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Miscellaneous.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# MplusAutomation

<details>

* Version: 1.1.1
* GitHub: https://github.com/michaelhallquist/MplusAutomation
* Source code: https://github.com/cran/MplusAutomation
* Date/Publication: 2024-01-30 23:40:02 UTC
* Number of recursive dependencies: 89

Run `revdepcheck::cloud_details(, "MplusAutomation")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        R      3.1Mb
        data   1.0Mb
    ```

# PlasmaMutationDetector

<details>

* Version: 1.7.2
* GitHub: NA
* Source code: https://github.com/cran/PlasmaMutationDetector
* Date/Publication: 2018-06-11 07:43:09 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::cloud_details(, "PlasmaMutationDetector")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        extdata   4.0Mb
    ```

# Superpower

<details>

* Version: 0.2.0
* GitHub: https://github.com/arcaldwell49/Superpower
* Source code: https://github.com/cran/Superpower
* Date/Publication: 2022-05-17 13:50:02 UTC
* Number of recursive dependencies: 112

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
      [ FAIL 2 | WARN 18 | SKIP 13 | PASS 397 ]
    ...
      1/1 mismatches
      [1] -0.00304 - 0 == -0.00304
      ── Failure ('test_sim_cor.R:38:3'): simulated correlations fit expected values ──
      `res5` not equal to 0.
      1/1 mismatches
      [1] -0.00322 - 0 == -0.00322
      
      [ FAIL 2 | WARN 18 | SKIP 13 | PASS 397 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘gridExtra’ ‘mvtnorm’
      All declared Imports should be used.
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
      
      `actual`:   "#0088ff"
      `expected`: "black"  
      
      [ FAIL 1 | WARN 18 | SKIP 1 | PASS 308 ]
      Error: Test failures
      Execution halted
    ```

