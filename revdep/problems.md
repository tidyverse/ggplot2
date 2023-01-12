# APCI

<details>

* Version: 1.0.6
* GitHub: NA
* Source code: https://github.com/cran/APCI
* Date/Publication: 2022-11-11 08:00:02 UTC
* Number of recursive dependencies: 90

Run `cloud_details(, "APCI")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘1_tests.R’
    Running the tests in ‘tests/1_tests.R’ failed.
    Last 13 lines of output:
          acc2:pcc2     acc3:pcc2     acc4:pcc2     acc5:pcc2     acc6:pcc2 
       -0.234254001   0.033033426  -0.215776733  -0.010520346   0.530194874 
          acc7:pcc2     acc8:pcc2     acc9:pcc2     acc1:pcc3     acc2:pcc3 
        0.137240404  -0.349418798   0.186182498   0.048227819   0.035207950 
          acc3:pcc3     acc4:pcc3     acc5:pcc3     acc6:pcc3     acc7:pcc3 
       -0.358192884   0.056638396   0.187380340  -0.353918254   0.218324300 
          acc8:pcc3     acc9:pcc3     acc1:pcc4     acc2:pcc4     acc3:pcc4 
       -0.086038986   0.123032319  -0.449982209  -0.066031345   0.319471451 
          acc4:pcc4     acc5:pcc4     acc6:pcc4     acc7:pcc4     acc8:pcc4 
        0.230667559   0.289486370  -0.137530674  -0.088833405  -0.249926829 
          acc9:pcc4     acc1:pcc5     acc2:pcc5     acc3:pcc5     acc4:pcc5 
        0.133943911   0.069193881   0.389964017  -0.157461980  -0.079205503 
          acc5:pcc5     acc6:pcc5     acc7:pcc5     acc8:pcc5     acc9:pcc5 
       -0.522669705  -0.392322607   0.322495821   0.594365830  -0.343459108 
      Killed
    ```

# BNPdensity

<details>

* Version: 2021.5.4
* GitHub: NA
* Source code: https://github.com/cran/BNPdensity
* Date/Publication: 2021-05-13 10:52:11 UTC
* Number of recursive dependencies: 105

Run `cloud_details(, "BNPdensity")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘BNPdensity-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: GOFplots_noncensored
    > ### Title: Plot Goodness of fits graphical checks for non censored data
    > ### Aliases: GOFplots_noncensored
    > 
    > ### ** Examples
    > 
    > 
    ...
     15.               │ │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
     16.               │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
     17.               │ └─base::withCallingHandlers(...)
     18.               └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     19.                 └─l$compute_geom_1(d)
     20.                   └─ggplot2 (local) compute_geom_1(..., self = self)
     21.                     └─ggplot2:::check_required_aesthetics(...)
     22.                       └─cli::cli_abort(message, call = call)
     23.                         └─rlang::abort(...)
    Execution halted
    ```

# gamlss.ggplots

<details>

* Version: 2.0-1
* GitHub: NA
* Source code: https://github.com/cran/gamlss.ggplots
* Date/Publication: 2022-11-12 16:50:02 UTC
* Number of recursive dependencies: 44

Run `cloud_details(, "gamlss.ggplots")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gamlss.ggplots-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: resid_dtop
    > ### Title: Detrended Transformed Owen's Plot and ECDF for the residuals
    > ### Aliases: resid_dtop resid_ecdf y_ecdf
    > ### Keywords: regression
    > 
    > ### ** Examples
    > 
    ...
      9.       │ │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
     10.       │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
     11.       │ └─base::withCallingHandlers(...)
     12.       └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13.         └─l$compute_geom_1(d)
     14.           └─ggplot2 (local) compute_geom_1(..., self = self)
     15.             └─ggplot2:::check_required_aesthetics(...)
     16.               └─cli::cli_abort(message, call = call)
     17.                 └─rlang::abort(...)
    Execution halted
    ```

# Hapi

<details>

* Version: 0.0.3
* GitHub: NA
* Source code: https://github.com/cran/Hapi
* Date/Publication: 2018-07-28 15:10:07 UTC
* Number of recursive dependencies: 53

Run `cloud_details(, "Hapi")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Hapi-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: hapiCVResolution
    > ### Title: Histogram of crossover resolution
    > ### Aliases: hapiCVResolution
    > 
    > ### ** Examples
    > 
    > data(crossover)
    ...
     13. │       └─l$map_statistic(d, plot)
     14. │         └─ggplot2 (local) map_statistic(..., self = self)
     15. │           └─base::lapply(new, eval_tidy, mask, env)
     16. │             └─rlang (local) FUN(X[[i]], ...)
     17. └─base::.handleSimpleError(...)
     18.   └─rlang (local) h(simpleError(msg, call))
     19.     └─handlers[[1L]](cnd)
     20.       └─cli::cli_abort(...)
     21.         └─rlang::abort(...)
    Execution halted
    ```

# listdown

<details>

* Version: 0.5.4
* GitHub: https://github.com/kaneplusplus/listdown
* Source code: https://github.com/cran/listdown
* Date/Publication: 2022-11-09 14:50:02 UTC
* Number of recursive dependencies: 74

Run `cloud_details(, "listdown")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.r’
    Running the tests in ‘tests/testthat.r’ failed.
    Last 13 lines of output:
      `ldb` not equal to read_reference("listdown-page-bundle.rds").
      Component "cc": Component "Sepal.Length": Component "Sepal.Width": Component "layers": Component 1: Component 4: Component 6: Component 8: Component 5: target, current do not match when deparsed
      Component "cc": Component "Sepal.Length": Component "Sepal.Width": Component "layers": Component 1: Component 12: Component 5: Component 21: Component 1: Component 2: Component 1: Component 6: Component 5: Component 8: Component 10: Component 8: Component 20: target, current do not match when deparsed
      Component "cc": Component "Sepal.Length": Component "Sepal.Width": Component "coordinates": Component "super": Component "super": Component "super": Component "super": Component "train_panel_guides": target, current do not match when deparsed
      Component "cc": Component "Sepal.Length": Component "Sepal.Width": Component "plot_env": Component "cc": Component "Sepal.Length": Component "Sepal.Width": Component "layers": Component 1: Component 4: Component 6: Component 8: Component 5: target, current do not match when deparsed
      Component "cc": Component "Sepal.Length": Component "Sepal.Width": Component "plot_env": Component "cc": Component "Sepal.Length": Component "Sepal.Width": Component "coordinates": Component "super": Component "super": Component "super": Component "super": Component "train_panel_guides": target, current do not match when deparsed
      Component "cc": Component "Sepal.Length": Component "Petal.Length": Component "layers": Component 1: Component 4: Component 6: Component 8: Component 5: target, current do not match when deparsed
      Component "cc": Component "Sepal.Length": Component "Petal.Length": Component "layers": Component 1: Component 12: Component 5: Component 21: Component 1: Component 2: Component 1: Component 6: Component 5: Component 8: Component 10: Component 8: Component 20: target, current do not match when deparsed
      Component "cc": Component "Sepal.Length": Component "Petal.Length": Component "coordinates": Component "super": Component "super": Component "super": Component "super": Component "train_panel_guides": target, current do not match when deparsed
      Component "cc": Component "Sepal.Length": Component "Petal.Length": Component "plot_env": Component "cc": Component "Sepal.Length": Component "Sepal.Width": Component "layers": Component 1: Component 4: Component 6: Component 8: Component 5: target, current do not match when deparsed
      ...
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 36 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘workflowr’
    ```

# openalexR

<details>

* Version: 1.0.0
* GitHub: https://github.com/massimoaria/openalexR
* Source code: https://github.com/cran/openalexR
* Date/Publication: 2022-10-06 10:40:02 UTC
* Number of recursive dependencies: 78

Run `cloud_details(, "openalexR")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘A_Brief_Introduction_to_openalexR.Rmd’ using rmarkdown
    Quitting from lines 207-213 (A_Brief_Introduction_to_openalexR.Rmd) 
    Error: processing vignette 'A_Brief_Introduction_to_openalexR.Rmd' failed with diagnostics:
    $ operator is invalid for atomic vectors
    --- failed re-building ‘A_Brief_Introduction_to_openalexR.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘A_Brief_Introduction_to_openalexR.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# RMixtComp

<details>

* Version: 4.1.3
* GitHub: https://github.com/modal-inria/MixtComp
* Source code: https://github.com/cran/RMixtComp
* Date/Publication: 2021-03-29 15:12:12 UTC
* Number of recursive dependencies: 106

Run `cloud_details(, "RMixtComp")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ClusVis.Rmd’ using rmarkdown
    --- finished re-building ‘ClusVis.Rmd’
    
    --- re-building ‘MixtComp.Rmd’ using rmarkdown
    Quitting from lines 138-142 (MixtComp.Rmd) 
    Error: processing vignette 'MixtComp.Rmd' failed with diagnostics:
    dim(X) must have a positive length
    --- failed re-building ‘MixtComp.Rmd’
    ...
    --- finished re-building ‘dataFormat.Rmd’
    
    --- re-building ‘mixtCompObject.Rmd’ using rmarkdown
    --- finished re-building ‘mixtCompObject.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘MixtComp.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# rxode2

<details>

* Version: 2.0.11
* GitHub: https://github.com/nlmixr2/rxode2
* Source code: https://github.com/cran/rxode2
* Date/Publication: 2022-11-01 21:45:04 UTC
* Number of recursive dependencies: 195

Run `cloud_details(, "rxode2")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rxode2-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: rxSuppressMsg
    > ### Title: Respect suppress messages
    > ### Aliases: rxSuppressMsg
    > 
    > ### ** Examples
    > 
    > 
    ...
    rxode2 model syntax error:
    ================================================================================
    
    rxode2 syntax error after 'bytesOR'
    :001: d/dt(matt)=/3
                     ^
    ================================================================================
    Error in nchar(sm[1L], type = "w") : invalid multibyte string, element 1
    Calls: try ... tryCatch -> tryCatchList -> tryCatchOne -> <Anonymous>
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘symengine’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 35.7Mb
      sub-directories of 1Mb or more:
        libs  33.2Mb
    ```

# xpose

<details>

* Version: 0.4.14
* GitHub: https://github.com/UUPharmacometrics/xpose
* Source code: https://github.com/cran/xpose
* Date/Publication: 2022-11-07 22:30:02 UTC
* Number of recursive dependencies: 108

Run `cloud_details(, "xpose")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
           ▆
        1. ├─testthat::expect_error(...) at test-xpose_save.R:33:2
        2. │ └─testthat:::quasi_capture(...)
        3. │   ├─testthat (local) .capture(...)
        4. │   │ └─base::withCallingHandlers(...)
        5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        6. └─xpose::xpose_save(plot = plot, file = paths_1)
        7.   └─ggplot2::ggsave(...)
        8.     └─ggplot2:::plot_dev(device, filename, dpi = dpi)
        9.       └─cli::cli_abort("Unknown graphics device {.val {device}}", call = call)
       10.         └─rlang::abort(...)
      
      [ FAIL 1 | WARN 3 | SKIP 7 | PASS 521 ]
      Error: Test failures
      Execution halted
    ```

