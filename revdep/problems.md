# ggdibbler

<details>

* Version: 0.6.1
* GitHub: https://github.com/harriet-mason/ggdibbler
* Source code: https://github.com/cran/ggdibbler
* Date/Publication: 2025-12-06 13:00:02 UTC
* Number of recursive dependencies: 160

Run `revdepcheck::cloud_details(, "ggdibbler")` for more info

</details>

## Newly broken

*   checking for code/documentation mismatches ... WARNING
    ```
    Codoc mismatches from Rd file 'geom_boxplot_sample.Rd':
    stat_boxplot_sample
      Code: function(mapping = NULL, data = NULL, geom = "boxplot",
                     position = "identity", ..., times = 10, orientation =
                     NA, seed = NULL, coef = 1.5, quantile.type = 7, na.rm
                     = FALSE, show.legend = NA, inherit.aes = TRUE)
      Docs: function(mapping = NULL, data = NULL, geom = "boxplot",
                     position = "identity", ..., times = 10, orientation =
                     NA, seed = NULL, coef = 1.5, na.rm = FALSE,
                     show.legend = NA, inherit.aes = TRUE)
      Argument names in code not in docs:
        quantile.type
      Mismatches in argument names:
        Position: 10 Code: quantile.type Docs: na.rm
        Position: 11 Code: na.rm Docs: show.legend
        Position: 12 Code: show.legend Docs: inherit.aes
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.5Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        data   3.0Mb
        doc    1.9Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 124 marked UTF-8 strings
    ```

# simRestore

<details>

* Version: 1.1.5
* GitHub: NA
* Source code: https://github.com/cran/simRestore
* Date/Publication: 2025-10-27 13:10:02 UTC
* Number of recursive dependencies: 78

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
      Saving _problems/test-optimize_static-21.R
      Saving _problems/test-optimize_static-22.R
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 79 ]
    ...
      ── Failure ('test-optimize_static.R:22:3'): simple optimization ────────────────
      Expected `length(vx$results$t)` to equal 5.
      Differences:
      1/1 mismatches
      [1] 4 - 5 == -1
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 79 ]
      Error:
      ! Test failures.
      Execution halted
    ```

