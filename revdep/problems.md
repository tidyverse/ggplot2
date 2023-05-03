# afex

<details>

* Version: 1.2-1
* GitHub: https://github.com/singmann/afex
* Source code: https://github.com/cran/afex
* Date/Publication: 2023-01-09 08:40:11 UTC
* Number of recursive dependencies: 224

Run `cloud_details(, "afex")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘afex_analysing_accuracy_data.Rmd’ using rmarkdown
    Quitting from lines 326-331 (afex_analysing_accuracy_data.Rmd) 
    Error: processing vignette 'afex_analysing_accuracy_data.Rmd' failed with diagnostics:
    Problem while computing position.
    ℹ Error occurred in the 1st layer.
    Caused by error in `ggplot2::remove_missing()`:
    ! `na.rm` must be `TRUE` or `FALSE`, not an empty logical vector.
    --- failed re-building ‘afex_analysing_accuracy_data.Rmd’
    
    ...
    --- finished re-building ‘assumptions_of_ANOVAs.Rmd’
    
    --- re-building ‘introduction-mixed-models.pdf.asis’ using asis
    --- finished re-building ‘introduction-mixed-models.pdf.asis’
    
    SUMMARY: processing the following file failed:
      ‘afex_analysing_accuracy_data.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# dalmatian

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/dalmatian
* Date/Publication: 2021-11-22 19:40:02 UTC
* Number of recursive dependencies: 92

Run `cloud_details(, "dalmatian")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘beta-binomial-1.Rmd’ using rmarkdown
    Quitting from lines 142-147 (beta-binomial-1.Rmd) 
    Error: processing vignette 'beta-binomial-1.Rmd' failed with diagnostics:
    could not find function "discrete_range"
    --- failed re-building ‘beta-binomial-1.Rmd’
    
    --- re-building ‘gamma-1.Rmd’ using rmarkdown
    Quitting from lines 144-149 (gamma-1.Rmd) 
    Error: processing vignette 'gamma-1.Rmd' failed with diagnostics:
    ...
    --- re-building ‘weights-1-simulate.Rmd’ using rmarkdown
    --- finished re-building ‘weights-1-simulate.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘beta-binomial-1.Rmd’ ‘gamma-1.Rmd’ ‘negative-binomial-1.Rmd’
      ‘pied-flycatchers-1.Rmd’ ‘pied-flycatchers-2.Rmd’
      ‘pied-flycatchers-3.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# DriveML

<details>

* Version: 0.1.5
* GitHub: https://github.com/daya6489/DriveML
* Source code: https://github.com/cran/DriveML
* Date/Publication: 2022-12-02 11:20:02 UTC
* Number of recursive dependencies: 119

Run `cloud_details(, "DriveML")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘DriveML.Rmd’ using rmarkdown
    Quitting from lines 341-343 (DriveML.Rmd) 
    Error: processing vignette 'DriveML.Rmd' failed with diagnostics:
    could not find function "discrete_range"
    --- failed re-building ‘DriveML.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘DriveML.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 8 marked UTF-8 strings
    ```

# EcoEnsemble

<details>

* Version: 1.0.2
* GitHub: NA
* Source code: https://github.com/cran/EcoEnsemble
* Date/Publication: 2023-03-17 15:00:05 UTC
* Number of recursive dependencies: 87

Run `cloud_details(, "EcoEnsemble")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘EcoEnsemble.Rmd’ using rmarkdown
    Quitting from lines 722-723 (EcoEnsemble.Rmd) 
    Error: processing vignette 'EcoEnsemble.Rmd' failed with diagnostics:
    could not find function "discrete_range"
    --- failed re-building ‘EcoEnsemble.Rmd’
    
    --- re-building ‘ExploringPriors.Rmd’ using rmarkdown
    --- finished re-building ‘ExploringPriors.Rmd’
    
    --- re-building ‘SyntheticData.Rmd’ using rmarkdown
    --- finished re-building ‘SyntheticData.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘EcoEnsemble.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 195.8Mb
      sub-directories of 1Mb or more:
        libs  194.8Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RcppParallel’ ‘rstantools’
      All declared Imports should be used.
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# ggh4x

<details>

* Version: 0.2.3
* GitHub: https://github.com/teunbrand/ggh4x
* Source code: https://github.com/cran/ggh4x
* Date/Publication: 2022-11-09 08:40:24 UTC
* Number of recursive dependencies: 79

Run `cloud_details(, "ggh4x")` for more info

</details>

## Newly broken

*   checking whether package ‘ggh4x’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ggh4x/new/ggh4x.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ggh4x’ ...
** package ‘ggh4x’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in get(x, envir = ns, inherits = FALSE) : 
  object 'continuous_range' not found
Error: unable to load R code in package ‘ggh4x’
Execution halted
ERROR: lazy loading failed for package ‘ggh4x’
* removing ‘/tmp/workdir/ggh4x/new/ggh4x.Rcheck/ggh4x’


```
### CRAN

```
* installing *source* package ‘ggh4x’ ...
** package ‘ggh4x’ successfully unpacked and MD5 sums checked
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
* DONE (ggh4x)


```
# ggtern

<details>

* Version: 3.4.1
* GitHub: NA
* Source code: https://github.com/cran/ggtern
* Date/Publication: 2022-12-06 03:10:02 UTC
* Number of recursive dependencies: 42

Run `cloud_details(, "ggtern")` for more info

</details>

## Newly broken

*   checking Rd files ... WARNING
    ```
    prepare_Rd: the condition has length > 1 and only the first element will be used
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

# siland

<details>

* Version: 2.0.5
* GitHub: NA
* Source code: https://github.com/cran/siland
* Date/Publication: 2021-01-27 20:10:03 UTC
* Number of recursive dependencies: 108

Run `cloud_details(, "siland")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘siland.Rmd’ using rmarkdown
    Quitting from lines 155-156 (siland.Rmd) 
    Error: processing vignette 'siland.Rmd' failed with diagnostics:
    could not find function "discrete_range"
    --- failed re-building ‘siland.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘siland.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# TCIU

<details>

* Version: 1.2.2
* GitHub: https://github.com/SOCR/TCIU
* Source code: https://github.com/cran/TCIU
* Date/Publication: 2023-02-27 20:02:30 UTC
* Number of recursive dependencies: 171

Run `cloud_details(, "TCIU")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘tciu-LT-kimesurface.Rmd’ using rmarkdown
    Quitting from lines 159-160 (tciu-LT-kimesurface.Rmd) 
    Error: processing vignette 'tciu-LT-kimesurface.Rmd' failed with diagnostics:
    could not find function "discrete_range"
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
      installed size is 12.9Mb
      sub-directories of 1Mb or more:
        doc  11.8Mb
    ```

# xpose

<details>

* Version: 0.4.15
* GitHub: https://github.com/UUPharmacometrics/xpose
* Source code: https://github.com/cran/xpose
* Date/Publication: 2023-02-25 15:30:02 UTC
* Number of recursive dependencies: 109

Run `cloud_details(, "xpose")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
           ▆
        1. ├─testthat::expect_error(...) at test-xpose_save.R:27:2
        2. │ └─testthat:::quasi_capture(...)
        3. │   ├─testthat (local) .capture(...)
        4. │   │ └─base::withCallingHandlers(...)
        5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        6. └─xpose::xpose_save(plot = plot, file = paths_1[3])
        7.   └─ggplot2::ggsave(...)
        8.     └─ggplot2:::plot_dev(device, filename, dpi = dpi)
        9.       └─cli::cli_abort(...)
       10.         └─rlang::abort(...)
      
      [ FAIL 1 | WARN 0 | SKIP 7 | PASS 520 ]
      Error: Test failures
      Execution halted
    ```

