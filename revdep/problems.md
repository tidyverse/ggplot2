# bfw

<details>

* Version: 0.4.2
* GitHub: https://github.com/oeysan/bfw
* Source code: https://github.com/cran/bfw
* Date/Publication: 2022-02-22 14:20:02 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "bfw")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in Rd file 'PlotMean.Rd':
      ‘[ggplot2]{ggplot2-ggproto}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# crayons

<details>

* Version: 0.0.4
* GitHub: https://github.com/christopherkenny/crayons
* Source code: https://github.com/cran/crayons
* Date/Publication: 2025-09-02 07:30:14 UTC
* Number of recursive dependencies: 20

Run `revdepcheck::cloud_details(, "crayons")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in Rd file 'scale_crayons.Rd':
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::Scale}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# cycleTrendR

<details>

* Version: 0.3.0
* GitHub: https://github.com/PietroPiu-labstats/cycleTrendR
* Source code: https://github.com/cran/cycleTrendR
* Date/Publication: 2026-01-26 09:50:08 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::cloud_details(, "cycleTrendR")` for more info

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
      Error in `stats::stl(tsdata, s.window = "periodic", robust = stlrobust)`: series is not periodic or has less than two periods
      Backtrace:
          ▆
       1. └─cycleTrendR::adaptive_cycle_trend_analysis(...) at test-dates_type.R:5:3
       2.   └─stats::stl(tsdata, s.window = "periodic", robust = stlrobust)
      
      [ FAIL 1 | WARN 4 | SKIP 0 | PASS 6 ]
      Error:
      ! Test failures.
      Execution halted
    ```

# eks

<details>

* Version: 1.1.2
* GitHub: NA
* Source code: https://github.com/cran/eks
* Date/Publication: 2025-12-08 09:50:10 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "eks")` for more info

</details>

## Newly broken

*   checking whether package ‘eks’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/eks/new/eks.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘eks’ ...
** package ‘eks’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error in ggplot2::ggproto("StatContourKs", ggplot2::StatContour, dropped_aes = "weight",  : 
  Members of a <ggproto> object cannot have duplicate names
("dropped_aes").
Error: unable to load R code in package ‘eks’
Execution halted
ERROR: lazy loading failed for package ‘eks’
* removing ‘/tmp/workdir/eks/new/eks.Rcheck/eks’


```
### CRAN

```
* installing *source* package ‘eks’ ...
** package ‘eks’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (eks)


```
# flexoki

<details>

* Version: 0.0.2
* GitHub: https://github.com/christopherkenny/flexoki
* Source code: https://github.com/cran/flexoki
* Date/Publication: 2025-09-03 16:20:12 UTC
* Number of recursive dependencies: 20

Run `revdepcheck::cloud_details(, "flexoki")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in Rd file 'scale_flexoki_b.Rd':
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::Scale}’
    
    Missing link or links in Rd file 'scale_flexoki_c.Rd':
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::Scale}’
    
    Missing link or links in Rd file 'scale_flexoki_d.Rd':
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::Scale}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# geomtextpath

<details>

* Version: 0.2.0
* GitHub: https://github.com/AllanCameron/geomtextpath
* Source code: https://github.com/cran/geomtextpath
* Date/Publication: 2025-07-21 12:10:02 UTC
* Number of recursive dependencies: 84

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
      Saving _problems/test-textpathgrob-81.R
      [ FAIL 1 | WARN 139 | SKIP 5 | PASS 464 ]
    ...
      Expected `class(res)` to equal `c("zeroGrob", "grob", "gDesc")`.
      Differences:
      `actual`:   "zeroGrob" "null" "grob" "gDesc"
      `expected`: "zeroGrob"        "grob" "gDesc"
      
      
      [ FAIL 1 | WARN 139 | SKIP 5 | PASS 464 ]
      Error:
      ! Test failures.
      Execution halted
    ```

# ggalluvial

<details>

* Version: 0.12.5
* GitHub: https://github.com/corybrunson/ggalluvial
* Source code: https://github.com/cran/ggalluvial
* Date/Publication: 2023-02-22 09:50:02 UTC
* Number of recursive dependencies: 89

Run `revdepcheck::cloud_details(, "ggalluvial")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in Rd file 'ggalluvial-ggproto.Rd':
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::ggplot2-ggproto}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# GGally

<details>

* Version: 2.4.0
* GitHub: https://github.com/ggobi/ggally
* Source code: https://github.com/cran/GGally
* Date/Publication: 2025-08-23 07:00:02 UTC
* Number of recursive dependencies: 138

Run `revdepcheck::cloud_details(, "GGally")` for more info

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
      Expected `ggsurv(...)` to run silently.
      Actual noise: warnings.
      ── Failure ('test-ggsurv.R:69:3'): stops ───────────────────────────────────────
      Expected `ggsurv(sf.kid, CI = TRUE, lty.est = 1:4)` to run silently.
      Actual noise: warnings.
      
      [ FAIL 2 | WARN 5 | SKIP 31 | PASS 515 ]
      Error:
      ! Test failures.
      Execution halted
    ```

# ggarchery

<details>

* Version: 0.4.4
* GitHub: https://github.com/mdhall272/ggarchery
* Source code: https://github.com/cran/ggarchery
* Date/Publication: 2025-07-24 13:10:08 UTC
* Number of recursive dependencies: 29

Run `revdepcheck::cloud_details(, "ggarchery")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in Rd file 'ggarchery-ggproto.Rd':
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::ggplot2-ggproto}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# ggChernoff

<details>

* Version: 0.3.0
* GitHub: https://github.com/Selbosh/ggChernoff
* Source code: https://github.com/cran/ggChernoff
* Date/Publication: 2022-11-17 13:10:01 UTC
* Number of recursive dependencies: 17

Run `revdepcheck::cloud_details(, "ggChernoff")` for more info

</details>

## Newly broken

*   checking whether package ‘ggChernoff’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ggChernoff/new/ggChernoff.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ggChernoff’ ...
** package ‘ggChernoff’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error in ggproto("GeomChernoff", ggplot2::Geom, required_aes = c("x",  : 
  Members of a <ggproto> object cannot have duplicate names ("draw_key").
Error: unable to load R code in package ‘ggChernoff’
Execution halted
ERROR: lazy loading failed for package ‘ggChernoff’
* removing ‘/tmp/workdir/ggChernoff/new/ggChernoff.Rcheck/ggChernoff’


```
### CRAN

```
* installing *source* package ‘ggChernoff’ ...
** package ‘ggChernoff’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (ggChernoff)


```
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
    Codoc mismatches from Rd file 'geom_linerange_sample.Rd':
    geom_linerange_sample
      Code: function(mapping = NULL, data = NULL, stat = "identity_sample",
                     position = "identity", ..., times = 10, orientation =
                     NA, seed = NULL, lineend = "butt", arrow = NULL,
                     arrow.fill = NULL, na.rm = FALSE, show.legend = NA,
                     inherit.aes = TRUE)
      Docs: function(mapping = NULL, data = NULL, stat = "identity_sample",
                     position = "identity", ..., times = 10, orientation =
                     NA, seed = NULL, lineend = "butt", na.rm = FALSE,
    ...
                     position = "identity", ..., times = 10, seed = NULL,
                     curvature = 0.5, angle = 90, ncp = 5, arrow = NULL,
                     arrow.fill = NULL, lineend = "butt", na.rm = FALSE,
                     show.legend = NA, inherit.aes = TRUE)
      Argument names in code not in docs:
        shape
      Mismatches in argument names (first 3):
        Position: 11 Code: shape Docs: arrow
        Position: 12 Code: arrow Docs: arrow.fill
        Position: 13 Code: arrow.fill Docs: lineend
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

# ggdist

<details>

* Version: 3.3.3
* GitHub: https://github.com/mjskay/ggdist
* Source code: https://github.com/cran/ggdist
* Date/Publication: 2025-04-23 00:20:02 UTC
* Number of recursive dependencies: 126

Run `revdepcheck::cloud_details(, "ggdist")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in Rd file 'geom_blur_dots.Rd':
      ‘[ggplot2:ggplot2-ggproto]{Position}’
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::Scale}’
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::Geom}’
    
    Missing link or links in Rd file 'geom_dots.Rd':
      ‘[ggplot2:ggplot2-ggproto]{Position}’
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::Scale}’
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::Geom}’
    
    ...
      ‘[ggplot2:ggplot2-ggproto]{Geom}’
      ‘[ggplot2:ggplot2-ggproto]{Position}’
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::Scale}’
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::Stat}’
    
    Missing link or links in Rd file 'sub-geometry-scales.Rd':
      ‘[ggplot2:ggplot2-ggproto]{Guide}’
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::Scale}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        doc    1.3Mb
        help   1.5Mb
    ```

# ggfields

<details>

* Version: 0.0.7
* GitHub: https://github.com/pepijn-devries/ggfields
* Source code: https://github.com/cran/ggfields
* Date/Publication: 2025-06-19 23:00:02 UTC
* Number of recursive dependencies: 103

Run `revdepcheck::cloud_details(, "ggfields")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in Rd file 'scale.Rd':
      ‘[ggplot2:ggplot2-ggproto]{Scale}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# ggformula

<details>

* Version: 1.0.1
* GitHub: https://github.com/ProjectMOSAIC/ggformula
* Source code: https://github.com/cran/ggformula
* Date/Publication: 2026-01-17 06:10:38 UTC
* Number of recursive dependencies: 133

Run `revdepcheck::cloud_details(, "ggformula")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggformula-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gf_hex_interactive
    > ### Title: Interactive hex plots
    > ### Aliases: gf_hex_interactive
    > 
    > ### ** Examples
    > 
    > gf_hex_interactive(mpg ~ wt, data = mtcars,
    ...
     34. │                                       ├─vctrs::vec_rep_each(data[c("x", "y", "radius")], times = 6L)
     35. │                                       ├─data[c("x", "y", "radius")]
     36. │                                       └─base::`[.data.frame`(data, c("x", "y", "radius"))
     37. │                                         └─base::stop("undefined columns selected")
     38. └─base::.handleSimpleError(...)
     39.   └─rlang (local) h(simpleError(msg, call))
     40.     └─handlers[[1L]](cnd)
     41.       └─cli::cli_abort(...)
     42.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.3Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        doc    2.6Mb
        help   1.6Mb
    ```

# ggfortify

<details>

* Version: 0.4.19
* GitHub: https://github.com/sinhrks/ggfortify
* Source code: https://github.com/cran/ggfortify
* Date/Publication: 2025-07-27 05:20:02 UTC
* Number of recursive dependencies: 121

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
       10.         │   ├─base::paste(chr(...), collapse = "\n")
       11.         │   └─rlang::chr(...)
       12.         │     └─rlang::dots_values(...)
       13.         └─lifecycle:::lifecycle_message(...)
       14.           └─lifecycle:::spec(what, env, signaller = signaller)
      
      [ FAIL 2 | WARN 10 | SKIP 48 | PASS 715 ]
      Error:
      ! Test failures.
      Execution halted
    ```

# gggda

<details>

* Version: 0.1.1
* GitHub: https://github.com/corybrunson/gggda
* Source code: https://github.com/cran/gggda
* Date/Publication: 2025-07-19 23:50:02 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::cloud_details(, "gggda")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in Rd file 'gggda-ggproto.Rd':
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::ggplot2-ggproto}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# gghexsize

<details>

* Version: 0.1.0
* GitHub: https://github.com/hrryt/gghexsize
* Source code: https://github.com/cran/gghexsize
* Date/Publication: 2025-05-13 09:00:02 UTC
* Number of recursive dependencies: 19

Run `revdepcheck::cloud_details(, "gghexsize")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in Rd file 'scale_size_tile.Rd':
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::Scale}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# ggiraph

<details>

* Version: 0.9.3
* GitHub: https://github.com/davidgohel/ggiraph
* Source code: https://github.com/cran/ggiraph
* Date/Publication: 2026-01-19 09:40:02 UTC
* Number of recursive dependencies: 87

Run `revdepcheck::cloud_details(, "ggiraph")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggiraph-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_crossbar_interactive
    > ### Title: Create interactive vertical intervals: lines, crossbars &
    > ###   errorbars
    > ### Aliases: geom_crossbar_interactive geom_errorbar_interactive
    > ###   geom_linerange_interactive geom_pointrange_interactive
    > 
    > ### ** Examples
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

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.8Mb
      sub-directories of 1Mb or more:
        R          1.5Mb
        examples   1.8Mb
        libs       5.2Mb
    ```

# ggpattern

<details>

* Version: 1.2.1
* GitHub: https://github.com/trevorld/ggpattern
* Source code: https://github.com/cran/ggpattern
* Date/Publication: 2025-08-27 05:10:08 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "ggpattern")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in Rd file 'geom-docs.Rd':
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::Geom}’
    
    Missing link or links in Rd file 'ggpattern-ggproto.Rd':
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::Geom}’
    
    Missing link or links in Rd file 'scale_continuous.Rd':
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::Scale}’
    
    Missing link or links in Rd file 'scale_discrete.Rd':
    ...
    Missing link or links in Rd file 'scale_pattern_manual.Rd':
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::Scale}’
    
    Missing link or links in Rd file 'scale_pattern_shape.Rd':
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::Scale}’
    
    Missing link or links in Rd file 'scale_pattern_size_continuous.Rd':
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::Scale}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# ggpmisc

<details>

* Version: 0.6.3
* GitHub: https://github.com/aphalo/ggpmisc
* Source code: https://github.com/cran/ggpmisc
* Date/Publication: 2025-11-29 17:10:02 UTC
* Number of recursive dependencies: 131

Run `revdepcheck::cloud_details(, "ggpmisc")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in Rd file 'ggpmisc-ggproto.Rd':
      ‘[ggplot2]{ggplot2-ggproto}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# ggpp

<details>

* Version: 0.6.0
* GitHub: https://github.com/aphalo/ggpp
* Source code: https://github.com/cran/ggpp
* Date/Publication: 2026-01-18 17:40:02 UTC
* Number of recursive dependencies: 76

Run `revdepcheck::cloud_details(, "ggpp")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in Rd file 'ggpp-ggproto.Rd':
      ‘[ggplot2]{ggplot2-ggproto}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# ggraph

<details>

* Version: 2.2.2
* GitHub: https://github.com/thomasp85/ggraph
* Source code: https://github.com/cran/ggraph
* Date/Publication: 2025-08-24 12:20:02 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::cloud_details(, "ggraph")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggraph-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_axis_hive
    > ### Title: Draw rectangular bars and labels on hive axes
    > ### Aliases: geom_axis_hive
    > 
    > ### ** Examples
    > 
    > # Plot the flare import graph as a hive plot
    ...
     19. │                   ├─rlang::inject(self$draw_panel(data, panel_params, coord, !!!params))
     20. │                   └─self$draw_panel(data, panel_params, coord, label = TRUE, axis = TRUE)
     21. │                     └─ggraph (local) draw_panel(...)
     22. │                       └─data %>% group_by(.data$axis) %>% ...
     23. ├─dplyr::summarise(...)
     24. ├─dplyr::group_by(., .data$axis)
     25. └─dplyr:::group_by.data.frame(., .data$axis)
     26.   └─dplyr::group_by_prepare(.data, ..., .add = .add, error_call = current_env())
     27.     └─rlang::abort(bullets, call = error_call)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Edges.Rmd’ using rmarkdown
    
    Quitting from Edges.Rmd:257-262 [unnamed-chunk-16]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `geom_axis_hive()`:
    ! Problem while converting geom to grob.
    ℹ Error occurred in the 2nd layer.
    Caused by error in `group_by()`:
    ...
    --- finished re-building ‘Nodes.Rmd’
    
    --- re-building ‘tidygraph.Rmd’ using rmarkdown
    --- finished re-building ‘tidygraph.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘Edges.Rmd’ ‘Layouts.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.1Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        doc    4.0Mb
        help   1.7Mb
        libs   2.8Mb
    ```

# ggsankeyfier

<details>

* Version: 0.1.8
* GitHub: https://github.com/pepijn-devries/ggsankeyfier
* Source code: https://github.com/cran/ggsankeyfier
* Date/Publication: 2024-04-08 13:30:07 UTC
* Number of recursive dependencies: 71

Run `revdepcheck::cloud_details(, "ggsankeyfier")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in Rd file 'position_sankey.Rd':
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::Position}’
    
    Missing link or links in Rd file 'scale_waist.Rd':
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::Scale}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# ggspectra

<details>

* Version: 0.3.17
* GitHub: https://github.com/aphalo/ggspectra
* Source code: https://github.com/cran/ggspectra
* Date/Publication: 2025-09-24 18:20:09 UTC
* Number of recursive dependencies: 64

Run `revdepcheck::cloud_details(, "ggspectra")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in Rd file 'gg2spectra-ggproto.Rd':
      ‘[ggplot2]{ggplot2-ggproto}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# ggtricks

<details>

* Version: 0.1.0
* GitHub: https://github.com/AbdoulMa/ggtricks
* Source code: https://github.com/cran/ggtricks
* Date/Publication: 2023-05-10 16:00:05 UTC
* Number of recursive dependencies: 60

Run `revdepcheck::cloud_details(, "ggtricks")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in Rd file 'GeomDonut.Rd':
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::Geom}’
    
    Missing link or links in Rd file 'GeomDonutSlice.Rd':
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::Geom}’
    
    Missing link or links in Rd file 'GeomPie.Rd':
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::Geom}’
    
    Missing link or links in Rd file 'GeomSeriesCircles.Rd':
    ...
    Missing link or links in Rd file 'StatSeriesCircles.Rd':
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::Geom}’
    
    Missing link or links in Rd file 'StatSeriesText.Rd':
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::Geom}’
    
    Missing link or links in Rd file 'StatSlice.Rd':
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::Geom}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# ggvfields

<details>

* Version: 1.0.0
* GitHub: https://github.com/dusty-turner/ggvfields
* Source code: https://github.com/cran/ggvfields
* Date/Publication: 2025-03-15 17:10:05 UTC
* Number of recursive dependencies: 60

Run `revdepcheck::cloud_details(, "ggvfields")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in Rd file 'geom_stream.Rd':
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::GeomPath}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# lemon

<details>

* Version: 0.5.2
* GitHub: https://github.com/stefanedwards/lemon
* Source code: https://github.com/cran/lemon
* Date/Publication: 2025-09-04 11:50:02 UTC
* Number of recursive dependencies: 70

Run `revdepcheck::cloud_details(, "lemon")` for more info

</details>

## Newly broken

*   checking whether package ‘lemon’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/lemon/new/lemon.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘lemon’ ...
** package ‘lemon’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in ggplot2::ggproto("GeomSideRange", `_inherit` = ggplot2::Geom,  : 
  Members of a <ggproto> object cannot have duplicate names
("default_aes").
Error: unable to load R code in package ‘lemon’
Execution halted
ERROR: lazy loading failed for package ‘lemon’
* removing ‘/tmp/workdir/lemon/new/lemon.Rcheck/lemon’


```
### CRAN

```
* installing *source* package ‘lemon’ ...
** package ‘lemon’ successfully unpacked and MD5 sums checked
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
* DONE (lemon)


```
# NHSRplotthedots

<details>

* Version: 0.2.2
* GitHub: https://github.com/nhs-r-community/NHSRplotthedots
* Source code: https://github.com/cran/NHSRplotthedots
* Date/Publication: 2025-09-29 11:00:02 UTC
* Number of recursive dependencies: 111

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
      Saving _problems/test-ptd_create_ggplot-85.R
      [ FAIL 1 | WARN 0 | SKIP 4 | PASS 555 ]
    ...
      Expected `inherits(pl, "ggplot2::labels")` to be TRUE.
      Differences:
      `actual`:   FALSE
      `expected`: TRUE 
      
      
      [ FAIL 1 | WARN 0 | SKIP 4 | PASS 555 ]
      Error:
      ! Test failures.
      Execution halted
    ```

# NMF

<details>

* Version: 0.28
* GitHub: NA
* Source code: https://github.com/cran/NMF
* Date/Publication: 2024-08-22 16:20:01 UTC
* Number of recursive dependencies: 129

Run `revdepcheck::cloud_details(, "NMF")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        R      2.0Mb
        doc    1.0Mb
        help   1.5Mb
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘NMF-vignette.Rnw’ using knitr
    Error in citation(x) : there is no package called 'doMC'
    Converted 9 of 10 package citations to BibTeX
    Writing 11 Bibtex entries ... OK
    Results written to file 'Rpackages.bib'
    Error: processing vignette 'NMF-vignette.Rnw' failed with diagnostics:
    Running 'texi2dvi' on 'NMF-vignette.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `a4wide.sty' not found.
    ...
    l.62 \usepackage
                    {xspace}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘heatmaps.Rnw’
    
    SUMMARY: processing the following files failed:
      ‘NMF-vignette.Rnw’ ‘heatmaps.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ordr

<details>

* Version: 0.2.0
* GitHub: https://github.com/corybrunson/ordr
* Source code: https://github.com/cran/ordr
* Date/Publication: 2025-07-10 21:40:07 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::cloud_details(, "ordr")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in Rd file 'ordr-ggproto.Rd':
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::ggplot2-ggproto}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4 marked UTF-8 strings
    ```

# rticulate

<details>

* Version: 2.2.0
* GitHub: https://github.com/stefanocoretta/rticulate
* Source code: https://github.com/cran/rticulate
* Date/Publication: 2025-09-07 23:30:09 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::cloud_details(, "rticulate")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘filter-signals.Rmd’ using rmarkdown
    --- finished re-building ‘filter-signals.Rmd’
    
    --- re-building ‘kinematics.Rmd’ using rmarkdown
    --- finished re-building ‘kinematics.Rmd’
    
    --- re-building ‘overview.Rmd’ using rmarkdown
    --- finished re-building ‘overview.Rmd’
    
    ...
    <text>:1:4: unexpected numeric constant
    1: 4.0.0
           ^
    --- failed re-building ‘transform-coord.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘transform-coord.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# simaerep

<details>

* Version: 1.0.0
* GitHub: https://github.com/openpharma/simaerep
* Source code: https://github.com/cran/simaerep
* Date/Publication: 2025-10-28 11:40:02 UTC
* Number of recursive dependencies: 140

Run `revdepcheck::cloud_details(, "simaerep")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(simaerep)
      Loading required package: ggplot2
      > 
      > test_check("simaerep")
      Saving _problems/test_plot-24.R
    ...
      
      [ FAIL 3 | WARN 3 | SKIP 9 | PASS 139 ]
      Deleting unused snapshots: 'validation/study-025-or.svg',
      'validation/study-025-ur.svg', 'validation/study-050-or.svg',
      'validation/study-050-ur.svg', 'validation/study-075-or.svg',
      'validation/study-075-ur.svg', 'validation/study-100-or.svg', and
      'validation/study-100-ur.svg'
      Error:
      ! Test failures.
      Execution halted
    ```

# statgenGxE

<details>

* Version: 1.0.11
* GitHub: https://github.com/Biometris/statgenGxE
* Source code: https://github.com/cran/statgenGxE
* Date/Publication: 2025-11-12 09:20:02 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "statgenGxE")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(statgenGxE)
      > 
      > test_check("statgenGxE")
      Saving _problems/test-plots-449.R
      [ FAIL 1 | WARN 3 | SKIP 17 | PASS 491 ]
      
    ...
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-plots.R:449:3'): option colorGenoBy in megaEnv plot functions correctly ──
      Expected `gbRight$layout[["name"]]` to equal `c("guides", "legend.box.background")`.
      Differences:
      target is NULL, current is character
      
      [ FAIL 1 | WARN 3 | SKIP 17 | PASS 491 ]
      Error:
      ! Test failures.
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘asreml’
    ```

# unusualprofile

<details>

* Version: 0.1.4
* GitHub: https://github.com/wjschne/unusualprofile
* Source code: https://github.com/cran/unusualprofile
* Date/Publication: 2024-02-14 23:20:03 UTC
* Number of recursive dependencies: 97

Run `revdepcheck::cloud_details(, "unusualprofile")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(unusualprofile)
      > 
      > test_check("unusualprofile")
      Saving _problems/test-main-53.R
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 41 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-main.R:44:3'): Example works ─────────────────────────────────
      Expected `{ ... }` to run silently.
      Actual noise: warnings.
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 41 ]
      Error:
      ! Test failures.
      Execution halted
    ```

# wacolors

<details>

* Version: 0.3.1
* GitHub: https://github.com/CoryMcCartan/wacolors
* Source code: https://github.com/cran/wacolors
* Date/Publication: 2022-03-01 15:50:02 UTC
* Number of recursive dependencies: 56

Run `revdepcheck::cloud_details(, "wacolors")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in Rd file 'scale_wa.Rd':
      ‘[ggplot2:ggplot2-ggproto]{ggplot2::Scale}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

