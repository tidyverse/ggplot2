# CNAIM

<details>

* Version: 2.1.4
* GitHub: https://github.com/Utiligize/CNAIM
* Source code: https://github.com/cran/CNAIM
* Date/Publication: 2022-08-31 08:40:22 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::cloud_details(, "CNAIM")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        R      1.6Mb
        data   1.1Mb
        help   1.6Mb
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

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘plotting-signals.Rmd’
      ...
    
    > cprop <- covidcast_signal(data_source = "jhu-csse", 
    +     signal = "confirmed_cumulative_prop", start_day = "2020-07-01", 
    +     end_day = "2020-07 ..." ... [TRUNCATED] 
    
      When sourcing ‘plotting-signals.R’:
    Error: Rate limit exceeded when fetching data from API anonymously. See the "API keys" section of the `covidcast_signal()` documentation for information on registering for an API key.
    ℹ Message from server:
    ℹ Rate limit exceeded for anonymous queries. To remove this limit, register a free API key at https://api.delphi.cmu.edu/epidata/admin/registration_form
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
    
    Quitting from covidcast.Rmd:37-45 [unnamed-chunk-1]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
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

# embryogrowth

<details>

* Version: 9.5
* GitHub: NA
* Source code: https://github.com/cran/embryogrowth
* Date/Publication: 2024-08-23 07:20:02 UTC
* Number of recursive dependencies: 105

Run `revdepcheck::cloud_details(, "embryogrowth")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  7.8Mb
      sub-directories of 1Mb or more:
        data   7.0Mb
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 7267 marked UTF-8 strings
    ```

# infer

<details>

* Version: 1.0.7
* GitHub: https://github.com/tidymodels/infer
* Source code: https://github.com/cran/infer
* Date/Publication: 2024-03-25 21:50:02 UTC
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

# SimNPH

<details>

* Version: 0.5.6
* GitHub: https://github.com/SimNPH/SimNPH
* Source code: https://github.com/cran/SimNPH
* Date/Publication: 2025-02-20 12:20:10 UTC
* Number of recursive dependencies: 133

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
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 413 ]
    ...
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-report_plots.R:13:5'): if labs_from_labels works ─────────────
      ggplot2::get_labs(gg)[c("x", "y")] (`actual`) not equal to list(x = "weight", y = "mpg") (`expected`).
      
      `actual$x`:   "wt"    
      `expected$x`: "weight"
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 413 ]
      Error: Test failures
      Execution halted
    ```

