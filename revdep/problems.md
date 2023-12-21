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
    A325-3  0.05499911 -0.11523891  0.93226790  0.2228111  0.06341159
    A325-4  0.32361642 -0.09323900  0.22281111  0.9342903  0.32783116
    A325-5  0.45700947 -0.04589968  0.06341159  0.3278312  0.87470189
    > 
    > # Plot a subset of the individuals.
    > kinship.heatmap(K = G[1:10, 1:10], dendrogram = TRUE, row.label = TRUE, col.label = TRUE)
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Calls: kinship.heatmap ... do.call -> <Anonymous> -> <Anonymous> -> startsWith
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
      i Actually got a <simpleError> with text:
        cannot coerce type 'closure' to vector of type 'character'
      ── Failure ('test-kinshipheat.R:21:3'): kinship heatmap works ──────────────────
      Expected `... <- NULL` to run without any errors.
      i Actually got a <simpleError> with text:
        cannot coerce type 'closure' to vector of type 'character'
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 263 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.9Mb
      sub-directories of 1Mb or more:
        data   8.4Mb
    ```

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

# BAS

<details>

* Version: 1.7.1
* GitHub: https://github.com/merliseclyde/BAS
* Source code: https://github.com/cran/BAS
* Date/Publication: 2023-12-06 10:40:02 UTC
* Number of recursive dependencies: 111

Run `revdepcheck::cloud_details(, "BAS")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘BAS-vignette.Rmd’ using rmarkdown
    
    Quitting from lines 267-273 [unnamed-chunk-6] (BAS-vignette.Rmd)
    Error: processing vignette 'BAS-vignette.Rmd' failed with diagnostics:
    cannot coerce type 'closure' to vector of type 'character'
    --- failed re-building ‘BAS-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘BAS-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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
      installed size is 14.9Mb
      sub-directories of 1Mb or more:
        doc       1.2Mb
        extdata   3.3Mb
        libs     10.3Mb
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

*   checking re-building of vignette outputs ... WARNING
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

# BasketballAnalyzeR

<details>

* Version: 0.5.0
* GitHub: https://github.com/sndmrc/BasketballAnalyzeR
* Source code: https://github.com/cran/BasketballAnalyzeR
* Date/Publication: 2020-06-26 09:00:11 UTC
* Number of recursive dependencies: 86

Run `revdepcheck::cloud_details(, "BasketballAnalyzeR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘BasketballAnalyzeR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.variability
    > ### Title: Plots a variability diagram from a 'variability' object
    > ### Aliases: plot.variability
    > 
    > ### ** Examples
    > 
    > Pbox.BC <- subset(Pbox, Team=="Oklahoma City Thunder" & MIN >= 500,
    ...
      3.   ├─ggplot2::guides(...)
      4.   │ └─rlang::list2(...)
      5.   └─ggplot2::guide_legend(title = leg.title, nrow = leg.nrow, title.position = leg.title.pos)
      6.     └─ggplot2::new_guide(...)
      7.       └─ggplot2:::validate_theme(params$theme)
      8.         └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
      9.           └─ggplot2 (local) `<fn>`(dots[[1L]][[1L]], dots[[2L]][[1L]], element_tree = `<named list>`)
     10.             └─cli::cli_abort(...)
     11.               └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.9Mb
      sub-directories of 1Mb or more:
        data   6.4Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘circlize’ ‘hexbin’ ‘scales’ ‘sna’
      All declared Imports should be used.
    ```

# BayesianFactorZoo

<details>

* Version: 0.0.0.2
* GitHub: NA
* Source code: https://github.com/cran/BayesianFactorZoo
* Date/Publication: 2023-11-14 12:43:44 UTC
* Number of recursive dependencies: 75

Run `revdepcheck::cloud_details(, "BayesianFactorZoo")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘BayesianFactorZoo-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: BayesianFM
    > ### Title: Bayesian Fama-MacBeth
    > ### Aliases: BayesianFM
    > 
    > ### ** Examples
    > 
    > 
    ...
     1. ├─ggplot2::guides(...)
     2. │ └─rlang::list2(...)
     3. └─ggplot2::guide_legend(...)
     4.   └─ggplot2::new_guide(...)
     5.     └─ggplot2:::validate_theme(params$theme)
     6.       └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
     7.         └─ggplot2 (local) `<fn>`(dots[[1L]][[1L]], dots[[2L]][[1L]], element_tree = `<named list>`)
     8.           └─cli::cli_abort(...)
     9.             └─rlang::abort(...)
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

*   checking re-building of vignette outputs ... WARNING
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

# CARBayesST

<details>

* Version: 4.0
* GitHub: https://github.com/duncanplee/CARBayesST
* Source code: https://github.com/cran/CARBayesST
* Date/Publication: 2023-10-30 16:40:02 UTC
* Number of recursive dependencies: 119

Run `revdepcheck::cloud_details(, "CARBayesST")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘CARBayesST.Rnw’
      ...
    6 S02000265 2007       24  55.04868 14.08884 1.25 1.760 0.4359777 -0.83016413
    
    > library(GGally)
    Loading required package: ggplot2
    
    > ggpairs(pollutionhealthdata, columns = c(9, 5:7))
    
      When sourcing ‘CARBayesST.R’:
    Error: cannot coerce type 'closure' to vector of type 'character'
    Execution halted
    
      ‘CARBayesST.Rnw’... failed
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 12.9Mb
      sub-directories of 1Mb or more:
        R      1.9Mb
        libs   9.7Mb
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘CARBayesST.Rnw’ using Sweave
    Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1; sf_use_s2() is TRUE
    
    Attaching package: ‘dplyr’
    
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    ...
    Loading required package: ggplot2
    Error: processing vignette 'CARBayesST.Rnw' failed with diagnostics:
    cannot coerce type 'closure' to vector of type 'character'
    --- failed re-building ‘CARBayesST.Rnw’
    
    SUMMARY: processing the following file failed:
      ‘CARBayesST.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
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

*   checking re-building of vignette outputs ... WARNING
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

# CINNA

<details>

* Version: 1.2.2
* GitHub: NA
* Source code: https://github.com/cran/CINNA
* Date/Publication: 2023-08-08 16:40:02 UTC
* Number of recursive dependencies: 149

Run `revdepcheck::cloud_details(, "CINNA")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘CINNA.Rmd’ using rmarkdown
    
    Quitting from lines 319-322 [unnamed-chunk-19] (CINNA.Rmd)
    Error: processing vignette 'CINNA.Rmd' failed with diagnostics:
    cannot coerce type 'closure' to vector of type 'character'
    --- failed re-building ‘CINNA.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘CINNA.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘circlize’ ‘utils’
      All declared Imports should be used.
    ```

# COMIX

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/COMIX
* Date/Publication: 2022-11-23 16:20:02 UTC
* Number of recursive dependencies: 54

Run `revdepcheck::cloud_details(, "COMIX")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘COMIX-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotGewekeParams
    > ### Title: This function creates plots for the Geweke diagnostic and
    > ###   results of test of stationarity for the parameters of the model.
    > ### Aliases: plotGewekeParams
    > 
    > ### ** Examples
    > 
    ...
     21. └─vctrs (local) `<fn>`()
     22.   └─vctrs::vec_default_ptype2(...)
     23.     ├─base::withRestarts(...)
     24.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     25.     │   └─base (local) doWithOneRestart(return(expr), restart)
     26.     └─vctrs::stop_incompatible_type(...)
     27.       └─vctrs:::stop_incompatible(...)
     28.         └─vctrs:::stop_vctrs(...)
     29.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 26.6Mb
      sub-directories of 1Mb or more:
        doc    1.8Mb
        libs  24.5Mb
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
        ▆
     1. ├─base (local) `<fn>`(x)
     2. ├─CRABS:::print.CRABSset(x)
     3. │ └─CRABS:::plot.CRABSset(x)
     4. │   └─ggplot2::theme(...)
     5. │     └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
     6. │       ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
     7. │       └─rlang::list2(..., ... = NULL)
     8. └─rlang::abort(message = message)
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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
      installed size is  9.8Mb
      sub-directories of 1Mb or more:
        data   3.4Mb
        doc    1.1Mb
        libs   4.4Mb
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

# DSAIDE

<details>

* Version: 0.9.6
* GitHub: https://github.com/ahgroup/DSAIDE
* Source code: https://github.com/cran/DSAIDE
* Date/Publication: 2023-08-23 21:20:16 UTC
* Number of recursive dependencies: 128

Run `revdepcheck::cloud_details(, "DSAIDE")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(DSAIDE)
      Loading required package: shiny
      Welcome to the DSAIDE package. Type dsaidemenu() to get started.
      > 
      > test_check("DSAIDE")
      [ FAIL 4 | WARN 1 | SKIP 2 | PASS 38 ]
    ...
        8.     └─ggplot2::new_guide(...)
        9.       └─ggplot2:::validate_theme(params$theme)
       10.         └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
       11.           └─ggplot2 (local) `<fn>`(dots[[1L]][[1L]], dots[[2L]][[1L]], element_tree = `<named list>`)
       12.             └─cli::cli_abort(...)
       13.               └─rlang::abort(...)
      
      [ FAIL 4 | WARN 1 | SKIP 2 | PASS 38 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        appinformation   4.1Mb
    ```

# DSAIRM

<details>

* Version: 0.9.6
* GitHub: https://github.com/ahgroup/DSAIRM
* Source code: https://github.com/cran/DSAIRM
* Date/Publication: 2023-08-23 18:50:05 UTC
* Number of recursive dependencies: 129

Run `revdepcheck::cloud_details(, "DSAIRM")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(DSAIRM)
      Loading required package: shiny
      Welcome to the DSAIRM package. Type dsairmmenu() to get started.
      > 
      > test_check("DSAIRM")
      [ FAIL 5 | WARN 1 | SKIP 1 | PASS 34 ]
    ...
        5.     └─ggplot2::new_guide(...)
        6.       └─ggplot2:::validate_theme(params$theme)
        7.         └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
        8.           └─ggplot2 (local) `<fn>`(dots[[1L]][[1L]], dots[[2L]][[1L]], element_tree = `<named list>`)
        9.             └─cli::cli_abort(...)
       10.               └─rlang::abort(...)
      
      [ FAIL 5 | WARN 1 | SKIP 1 | PASS 34 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        appinformation   3.6Mb
    ```

# DSjobtracker

<details>

* Version: 2.0.0
* GitHub: https://github.com/thiyangt/DSjobtracker
* Source code: https://github.com/cran/DSjobtracker
* Date/Publication: 2023-12-09 07:40:02 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::cloud_details(, "DSjobtracker")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘DSjobtracker.Rmd’ using rmarkdown
    
    Quitting from lines 163-197 [unnamed-chunk-10] (DSjobtracker.Rmd)
    Error: processing vignette 'DSjobtracker.Rmd' failed with diagnostics:
    The `legend.title` theme element must be a <element_text> object.
    --- failed re-building ‘DSjobtracker.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘DSjobtracker.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 386 marked UTF-8 strings
    ```

# EGAnet

<details>

* Version: 2.0.3
* GitHub: https://github.com/hfgolino/EGAnet
* Source code: https://github.com/cran/EGAnet
* Date/Publication: 2023-11-17 17:30:05 UTC
* Number of recursive dependencies: 190

Run `revdepcheck::cloud_details(, "EGAnet")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘EGAnet-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: compare.EGA.plots
    > ### Title: Visually Compare Two or More 'EGAnet' plots
    > ### Aliases: compare.EGA.plots
    > 
    > ### ** Examples
    > 
    > # Obtain WMT-2 data
    ...
    > # Draw random samples of 300 cases
    > sample1 <- wmt[sample(1:nrow(wmt), 300),]
    > sample2 <- wmt[sample(1:nrow(wmt), 300),]
    > 
    > # Estimate EGAs
    > ega1 <- EGA(sample1)
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Calls: EGA ... basic_plot_setup -> silent_call -> capture.output -> withVisible
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        data   3.5Mb
    ```

# EIX

<details>

* Version: 1.2.0
* GitHub: https://github.com/ModelOriented/EIX
* Source code: https://github.com/cran/EIX
* Date/Publication: 2021-03-23 08:10:02 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "EIX")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘EIX-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: importance
    > ### Title: Importance of variables and interactions in the model
    > ### Aliases: importance
    > 
    > ### ** Examples
    > 
    > library("EIX")
    ...
    Backtrace:
        ▆
     1. ├─base::plot(imp, top = 10)
     2. ├─EIX:::plot.importance(imp, top = 10)
     3. │ └─ggplot2::theme(...)
     4. │   └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
     5. │     ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
     6. │     └─rlang::list2(..., ... = NULL)
     7. └─rlang::abort(message = message)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘EIX.Rmd’ using rmarkdown
    Warning: ggrepel: 2 unlabeled data points (too many overlaps). Consider increasing max.overlaps
    
    Quitting from lines 144-148 [unnamed-chunk-10] (EIX.Rmd)
    Error: processing vignette 'EIX.Rmd' failed with diagnostics:
    Argument 1 can't be empty.
    --- failed re-building ‘EIX.Rmd’
    
    ...
    Quitting from lines 73-74 [unnamed-chunk-7] (titanic_data.Rmd)
    Error: processing vignette 'titanic_data.Rmd' failed with diagnostics:
    Argument 1 can't be empty.
    --- failed re-building ‘titanic_data.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘EIX.Rmd’ ‘titanic_data.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# EMMIXmfa

<details>

* Version: 2.0.11
* GitHub: https://github.com/suren-rathnayake/EMMIXmfa
* Source code: https://github.com/cran/EMMIXmfa
* Date/Publication: 2019-12-16 22:50:02 UTC
* Number of recursive dependencies: 59

Run `revdepcheck::cloud_details(, "EMMIXmfa")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘EMMIXmfa-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: factor_scores
    > ### Title: Computes Factor Scores
    > ### Aliases: factor_scores factor_scores.mcfa factor_scores.mctfa
    > ###   plot.emmix
    > ### Keywords: cluster multivariate models
    > 
    > ### ** Examples
    ...
    > Y <- as.matrix(Y)
    > clust <- predict(model, Y)
    > 
    > fa_scores <- factor_scores(model, Y)
    > # Visualizing new data in factor space
    > plot_factors(fa_scores, type = "Umean", clust = clust)
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Calls: plot_factors ... do.call -> <Anonymous> -> scale_y_continuous -> startsWith
    Execution halted
    ```

# EVI

<details>

* Version: 0.2.0-0
* GitHub: NA
* Source code: https://github.com/cran/EVI
* Date/Publication: 2023-12-05 09:20:02 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::cloud_details(, "EVI")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘EVI-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: evi.graphs
    > ### Title: This function produces plots of the time series data with the
    > ###   EVI predictions.
    > ### Aliases: evi.graphs
    > 
    > ### ** Examples
    > 
    ...
     22. └─vctrs (local) `<fn>`()
     23.   └─vctrs::vec_default_ptype2(...)
     24.     ├─base::withRestarts(...)
     25.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     26.     │   └─base (local) doWithOneRestart(return(expr), restart)
     27.     └─vctrs::stop_incompatible_type(...)
     28.       └─vctrs:::stop_incompatible(...)
     29.         └─vctrs:::stop_vctrs(...)
     30.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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
      installed size is  7.7Mb
      sub-directories of 1Mb or more:
        libs   6.7Mb
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
    ! LaTeX Error: File `ae.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ...
                        [T1]{fontenc}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘pointpatterns.tex.rsp’
    
    SUMMARY: processing the following files failed:
      ‘FDRenvelopes.tex.rsp’ ‘GET.tex.rsp’ ‘HotSpots.tex.rsp’
      ‘pointpatterns.tex.rsp’
    
    Error: Vignette re-building failed.
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

*   checking examples ... ERROR
    ```
    Running examples in ‘GGally-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: +.gg
    > ### Title: Modify a 'ggmatrix' object by adding an 'ggplot2' object to all
    > ###   plots
    > ### Aliases: +.gg add_to_ggmatrix
    > 
    > ### ** Examples
    > 
    ...
    > p_ <- GGally::print_if_interactive
    > data(tips)
    > 
    > pm <- ggpairs(tips[, 2:4], ggplot2::aes(color = sex))
    > ## change to black and white theme
    > pm + ggplot2::theme_bw()
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Calls: <Anonymous> ... do.call -> <Anonymous> -> scale_y_continuous -> startsWith
    Execution halted
    ```

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
       17.           └─GGally::getPlot(pm, i, j)
       18.             └─GGally (local) fn(pm$data, plotObj$mapping)
       19.               ├─base::do.call(original_fn, allParams)
       20.               └─GGally (local) `<fn>`(data = `<df[,5]>`, mapping = `<uneval>`)
       21.                 └─ggplot2::scale_y_continuous()
       22.                   └─base::startsWith(as.character(call[[1]]), "scale_")
      
      [ FAIL 12 | WARN 1 | SKIP 22 | PASS 466 ]
      Error: Test failures
      Execution halted
    ```

# GOplot

<details>

* Version: 1.0.2
* GitHub: https://github.com/wencke/wencke.github.io
* Source code: https://github.com/cran/GOplot
* Date/Publication: 2016-03-30 20:35:02
* Number of recursive dependencies: 54

Run `revdepcheck::cloud_details(, "GOplot")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘GOplot_vignette.Rmd’ using rmarkdown
    
    Quitting from lines 72-74 [GOBar] (GOplot_vignette.Rmd)
    Error: processing vignette 'GOplot_vignette.Rmd' failed with diagnostics:
    The `legend.title` theme element must be a <element_text> object.
    --- failed re-building ‘GOplot_vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘GOplot_vignette.Rmd’
    
    Error: Vignette re-building failed.
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

*   checking re-building of vignette outputs ... WARNING
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

# ICSClust

<details>

* Version: 0.1.0
* GitHub: https://github.com/AuroreAA/ICSClust
* Source code: https://github.com/cran/ICSClust
* Date/Publication: 2023-09-21 13:20:02 UTC
* Number of recursive dependencies: 137

Run `revdepcheck::cloud_details(, "ICSClust")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ICSClust-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ICSClust
    > ### Title: Tandem clustering with ICS
    > ### Aliases: ICSClust
    > 
    > ### ** Examples
    > 
    > X <- iris[,1:4]
    ...
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Calls: <Anonymous> ... do.call -> <Anonymous> -> scale_y_continuous -> startsWith
    Execution halted
    ```

# ICtest

<details>

* Version: 0.3-5
* GitHub: NA
* Source code: https://github.com/cran/ICtest
* Date/Publication: 2022-05-18 07:30:29 UTC
* Number of recursive dependencies: 97

Run `revdepcheck::cloud_details(, "ICtest")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ICtest-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggplot.ictest
    > ### Title: Scatterplot Matrix for a ictest Object using ggplot2
    > ### Aliases: ggplot.ictest
    > ### Keywords: hplot
    > 
    > ### ** Examples
    > 
    ...
    > # The aesthetics variables
    > mapvar <- data.frame(iris[, 5])
    > colnames(mapvar) <- "species"
    > 
    > TestCov <- PCAasymp(X, k = 2)
    > ggplot(TestCov)
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Calls: <Anonymous> ... do.call -> <Anonymous> -> scale_y_continuous -> startsWith
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ICA.Rmd’ using rmarkdown
    --- finished re-building ‘ICA.Rmd’
    
    --- re-building ‘PCA.Rmd’ using rmarkdown
    
    Quitting from lines 122-127 [unnamed-chunk-4] (PCA.Rmd)
    Error: processing vignette 'PCA.Rmd' failed with diagnostics:
    cannot coerce type 'closure' to vector of type 'character'
    ...
    --- failed re-building ‘PCA.Rmd’
    
    --- re-building ‘SIR.Rmd’ using rmarkdown
    --- finished re-building ‘SIR.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘PCA.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        doc    2.5Mb
        libs   3.6Mb
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

# Lahman

<details>

* Version: 11.0-0
* GitHub: https://github.com/cdalzell/Lahman
* Source code: https://github.com/cran/Lahman
* Date/Publication: 2023-05-04 08:40:02 UTC
* Number of recursive dependencies: 97

Run `revdepcheck::cloud_details(, "Lahman")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Lahman-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Batting
    > ### Title: Batting table
    > ### Aliases: Batting
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    ...
     21. └─vctrs (local) `<fn>`()
     22.   └─vctrs::vec_default_ptype2(...)
     23.     ├─base::withRestarts(...)
     24.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     25.     │   └─base (local) doWithOneRestart(return(expr), restart)
     26.     └─vctrs::stop_incompatible_type(...)
     27.       └─vctrs:::stop_incompatible(...)
     28.         └─vctrs:::stop_vctrs(...)
     29.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.3Mb
      sub-directories of 1Mb or more:
        data   8.4Mb
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

# MOSS

<details>

* Version: 0.2.2
* GitHub: https://github.com/agugonrey/MOSS
* Source code: https://github.com/cran/MOSS
* Date/Publication: 2022-03-25 15:50:05 UTC
* Number of recursive dependencies: 184

Run `revdepcheck::cloud_details(, "MOSS")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(MOSS)
      
       _____________________________________________________________________
      |MOSS: Multi-Omic integration via Sparse Singular value decomposition.|
       _____________________________________________________________________
    ...
        6.       └─ggplot2::new_guide(...)
        7.         └─ggplot2:::validate_theme(params$theme)
        8.           └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
        9.             └─ggplot2 (local) `<fn>`(dots[[1L]][[1L]], dots[[2L]][[1L]], element_tree = `<named list>`)
       10.               └─cli::cli_abort(...)
       11.                 └─rlang::abort(...)
      
      [ FAIL 2 | WARN 7 | SKIP 0 | PASS 18 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘MOSS_working_example.Rmd’ using rmarkdown
    
    kpathsea: Running mktexfmt pdftex.fmt
    /usr/bin/mktexfmt: kpsewhich -var-value=TEXMFROOT failed, aborting early.
    BEGIN failed--compilation aborted at /usr/bin/mktexfmt line 28.
    !!! Error: pdfTeX run failed with value 1!
    
    kpathsea: Running mktexfmt pdftex.fmt
    /usr/bin/mktexfmt: kpsewhich -var-value=TEXMFROOT failed, aborting early.
    ...
    Quitting from lines 175-183 [unnamed-chunk-8] (MOSS_working_example.Rmd)
    Error: processing vignette 'MOSS_working_example.Rmd' failed with diagnostics:
    The `legend.title` theme element must be a <element_text> object.
    --- failed re-building ‘MOSS_working_example.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘MOSS_working_example.Rmd’
    
    Error: Vignette re-building failed.
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

# MultIS

<details>

* Version: 0.6.2
* GitHub: NA
* Source code: https://github.com/cran/MultIS
* Date/Publication: 2021-08-06 11:10:02 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::cloud_details(, "MultIS")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘QuickStart.Rmd’ using knitr
    
    Quitting from lines 213-229 [unnamed-chunk-6] (QuickStart.Rmd)
    Error: processing vignette 'QuickStart.Rmd' failed with diagnostics:
    Can't combine <character> and <logical>.
    --- failed re-building ‘QuickStart.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘QuickStart.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘poweRlaw’ ‘rmutil’
      All declared Imports should be used.
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
      `expected` is a character vector ('legend')
      
      [ FAIL 48 | WARN 0 | SKIP 0 | PASS 1172 ]
      Error: Test failures
      In addition: Warning messages:
      1: In for (i in seq_len(n)) { :
        closing unused connection 5 (/tmp/RtmpR4LTf6/c.rds)
      2: In for (i in seq_len(n)) { :
        closing unused connection 4 (/tmp/RtmpR4LTf6/b.rds)
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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
      Actual value: "List of 11\\n \$ data       : tibble \[200 × 5\] \(S3: tbl_df/tbl/data\.frame\)\\n  \.\.\$ Period    : chr \[1:200\] "2000-2001" "2000-2001" "2000-2001" "2000-2001" \.\.\.\\n  \.\.\$ area_gross: num \[1:200\] 0\.000388 0\.000379 0\.00038 0\.000411 0\.000403 0\.000416 0\.000368 0\.000445 0\.000387 0\.000399 \.\.\.\\n  \.\.\$ From      : Factor w/ 5 levels "GUP","OZS","PSN",\.\.: 3 3 3 3 4 4 4 4 2 2 \.\.\.\\n  \.\.\$ To        : Factor w/ 5 levels "GUP","OZS","PSN",\.\.: 4 2 1 5 3 2 1 5 3 4 \.\.\.\\n  \.\.\$ changes   : chr \[1:200\] "Gain" "Gain" "Gain" "Gain" \.\.\.\\n \$ layers     :List of 4\\n  \.\.\$ :Classes 'LayerInstance', 'Layer', 'ggproto', 'gg' <ggproto object: Class LayerInstance, Layer, gg>\\n    aes_params: list\\n    compute_aesthetics: function\\n    compute_geom_1: function\\n    compute_geom_2: function\\n    compute_position: function\\n    compute_statistic: function\\n    computed_geom_params: NULL\\n    computed_mapping: NULL\\n    computed_stat_params: NULL\\n    constructor: call\\n    data: waiver\\n    draw_geom: function\\n    finish_statistics: function\\n    geom: <ggproto object: Class GeomBar, GeomRect, Geom, gg>\\n        aesthetics: function\\n        default_aes: uneval\\n        draw_group: function\\n        draw_key: function\\n        draw_layer: function\\n        draw_panel: function\\n        extra_params: just na\.rm orientation\\n        handle_na: function\\n        non_missing_aes: xmin xmax ymin ymax\\n        optional_aes: \\n        parameters: function\\n        rename_size: TRUE\\n        required_aes: x y\\n        setup_data: function\\n        setup_params: function\\n        use_defaults: function\\n        super:  <ggproto object: Class GeomRect, Geom, gg>\\n    geom_params: list\\n    inherit\.aes: TRUE\\n    layer_data: function\\n    map_statistic: function\\n    mapping: uneval\\n    position: <ggproto object: Class PositionStack, Position, gg>\\n        compute_layer: function\\n        compute_panel: function\\n        fill: FALSE\\n        required_aes: \\n        reverse: FALSE\\n        setup_data: function\\n        setup_params: function\\n        type: NULL\\n        vjust: 1\\n        super:  <ggproto object: Class Position, gg>\\n    print: function\\n    setup_layer: function\\n    show\.legend: NA\\n    stat: <ggproto object: Class StatIdentity, Stat, gg>\\n        aesthetics: function\\n        compute_group: function\\n        compute_layer: function\\n        compute_panel: function\\n        default_aes: uneval\\n        dropped_aes: \\n        extra_params: na\.rm\\n        finish_layer: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        required_aes: \\n        retransform: TRUE\\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Stat, gg>\\n    stat_params: list\\n    super:  <ggproto object: Class Layer, gg> \\n  \.\.\$ :Classes 'LayerInstance', 'Layer', 'ggproto', 'gg' <ggproto object: Class LayerInstance, Layer, gg>\\n    aes_params: list\\n    compute_aesthetics: function\\n    compute_geom_1: function\\n    compute_geom_2: function\\n    compute_position: function\\n    compute_statistic: function\\n    computed_geom_params: NULL\\n    computed_mapping: NULL\\n    computed_stat_params: NULL\\n    constructor: call\\n    data: tbl_df, tbl, data\.frame\\n    draw_geom: function\\n    finish_statistics: function\\n    geom: <ggproto object: Class GeomBar, GeomRect, Geom, gg>\\n        aesthetics: function\\n        default_aes: uneval\\n        draw_group: function\\n        draw_key: function\\n        draw_layer: function\\n        draw_panel: function\\n        extra_params: just na\.rm orientation\\n        handle_na: function\\n        non_missing_aes: xmin xmax ymin ymax\\n        optional_aes: \\n        parameters: function\\n        rename_size: TRUE\\n        required_aes: x y\\n        setup_data: function\\n        setup_params: function\\n        use_defaults: function\\n        super:  <ggproto object: Class GeomRect, Geom, gg>\\n    geom_params: list\\n    inherit\.aes: FALSE\\n    layer_data: function\\n    map_statistic: function\\n    mapping: uneval\\n    position: <ggproto object: Class PositionStack, Position, gg>\\n        compute_layer: function\\n        compute_panel: function\\n        fill: FALSE\\n        required_aes: \\n        reverse: FALSE\\n        setup_data: function\\n        setup_params: function\\n        type: NULL\\n        vjust: 1\\n        super:  <ggproto object: Class Position, gg>\\n    print: function\\n    setup_layer: function\\n    show\.legend: NA\\n    stat: <ggproto object: Class StatIdentity, Stat, gg>\\n        aesthetics: function\\n        compute_group: function\\n        compute_layer: function\\n        compute_panel: function\\n        default_aes: uneval\\n        dropped_aes: \\n        extra_params: na\.rm\\n        finish_layer: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        required_aes: \\n        retransform: TRUE\\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Stat, gg>\\n    stat_params: list\\n    super:  <ggproto object: Class Layer, gg> \\n  \.\.\$ :Classes 'LayerInstance', 'Layer', 'ggproto', 'gg' <ggproto object: Class LayerInstance, Layer, gg>\\n    aes_params: list\\n    compute_aesthetics: function\\n    compute_geom_1: function\\n    compute_geom_2: function\\n    compute_position: function\\n    compute_statistic: function\\n    computed_geom_params: NULL\\n    computed_mapping: NULL\\n    computed_stat_params: NULL\\n    constructor: call\\n    data: tbl_df, tbl, data\.frame\\n    draw_geom: function\\n    finish_statistics: function\\n    geom: <ggproto object: Class GeomSegment, Geom, gg>\\n        aesthetics: function\\n        default_aes: uneval\\n        draw_group: function\\n        draw_key: function\\n        draw_layer: function\\n        draw_panel: function\\n        extra_params: na\.rm\\n        handle_na: function\\n        non_missing_aes: linetype linewidth\\n        optional_aes: \\n        parameters: function\\n        rename_size: TRUE\\n        required_aes: x y xend\|yend\\n        setup_data: function\\n        setup_params: function\\n        use_defaults: function\\n        super:  <ggproto object: Class Geom, gg>\\n    geom_params: list\\n    inherit\.aes: TRUE\\n    layer_data: function\\n    map_statistic: function\\n    mapping: uneval\\n    position: <ggproto object: Class PositionIdentity, Position, gg>\\n        compute_layer: function\\n        compute_panel: function\\n        required_aes: \\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Position, gg>\\n    print: function\\n    setup_layer: function\\n    show\.legend: NA\\n    stat: <ggproto object: Class StatIdentity, Stat, gg>\\n        aesthetics: function\\n        compute_group: function\\n        compute_layer: function\\n        compute_panel: function\\n        default_aes: uneval\\n        dropped_aes: \\n        extra_params: na\.rm\\n        finish_layer: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        required_aes: \\n        retransform: TRUE\\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Stat, gg>\\n    stat_params: list\\n    super:  <ggproto object: Class Layer, gg> \\n  \.\.\$ :Classes 'LayerInstance', 'Layer', 'ggproto', 'gg' <ggproto object: Class LayerInstance, Layer, gg>\\n    aes_params: list\\n    compute_aesthetics: function\\n    compute_geom_1: function\\n    compute_geom_2: function\\n    compute_position: function\\n    compute_statistic: function\\n    computed_geom_params: NULL\\n    computed_mapping: NULL\\n    computed_stat_params: NULL\\n    constructor: call\\n    data: data\.frame\\n    draw_geom: function\\n    finish_statistics: function\\n    geom: <ggproto object: Class GeomHline, Geom, gg>\\n        aesthetics: function\\n        check_constant_aes: FALSE\\n        default_aes: uneval\\n        draw_group: function\\n        draw_key: function\\n        draw_layer: function\\n        draw_panel: function\\n        extra_params: na\.rm\\n        handle_na: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        rename_size: TRUE\\n        required_aes: yintercept\\n        setup_data: function\\n        setup_params: function\\n        use_defaults: function\\n        super:  <ggproto object: Class Geom, gg>\\n    geom_params: list\\n    inherit\.aes: FALSE\\n    layer_data: function\\n    map_statistic: function\\n    mapping: uneval\\n    position: <ggproto object: Class PositionIdentity, Position, gg>\\n        compute_layer: function\\n        compute_panel: function\\n        required_aes: \\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Position, gg>\\n    print: function\\n    setup_layer: function\\n    show\.legend: FALSE\\n    stat: <ggproto object: Class StatIdentity, Stat, gg>\\n        aesthetics: function\\n        compute_group: function\\n        compute_layer: function\\n        compute_panel: function\\n        default_aes: uneval\\n        dropped_aes: \\n        extra_params: na\.rm\\n        finish_layer: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        required_aes: \\n        retransform: TRUE\\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Stat, gg>\\n    stat_params: list\\n    super:  <ggproto object: Class Layer, gg> \\n \$ scales     :Classes 'ScalesList', 'ggproto', 'gg' <ggproto object: Class ScalesList, gg>\\n    add: function\\n    add_defaults: function\\n    add_missing: function\\n    backtransform_df: function\\n    clone: function\\n    find: function\\n    get_scales: function\\n    has_scale: function\\n    input: function\\n    map_df: function\\n    n: function\\n    non_position_scales: function\\n    scales: list\\n    train_df: function\\n    transform_df: function\\n    super:  <ggproto object: Class ScalesList, gg> \\n \$ guides     :Classes 'Guides', 'ggproto', 'gg' <ggproto object: Class Guides, gg>\\n    add: function\\n    assemble: function\\n    build: function\\n    draw: function\\n    get_custom: function\\n    get_guide: function\\n    get_params: function\\n    get_position: function\\n    guides: NULL\\n    merge: function\\n    missing: <ggproto object: Class GuideNone, Guide, gg>\\n        arrange_layout: function\\n        assemble_drawing: function\\n        available_aes: any\\n        build_decor: function\\n        build_labels: function\\n        build_ticks: function\\n        build_title: function\\n        draw: function\\n        draw_early_exit: function\\n        elements: list\\n        extract_decor: function\\n        extract_key: function\\n        extract_params: function\\n        get_layer_key: function\\n        hashables: list\\n        measure_grobs: function\\n        merge: function\\n        override_elements: function\\n        params: list\\n        process_layers: function\\n        setup_elements: function\\n        setup_params: function\\n        train: function\\n        transform: function\\n        super:  <ggproto object: Class GuideNone, Guide, gg>\\n    package_box: function\\n    print: function\\n    process_layers: function\\n    setup: function\\n    subset_guides: function\\n    train: function\\n    update_params: function\\n    super:  <ggproto object: Class Guides, gg> \\n \$ mapping    :List of 2\\n  \.\.\$ x: language ~To\\n  \.\. \.\.- attr\(\*, "\.Environment"\)=<environment: 0x55988d2380e8> \\n  \.\.\$ y: language ~area_gross\\n  \.\. \.\.- attr\(\*, "\.Environment"\)=<environment: 0x55988d2380e8> \\n  \.\.- attr\(\*, "class"\)= chr "uneval"\\n \$ theme      :List of 1\\n  \.\.\$ plot\.title:List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : NULL\\n  \.\. \.\.\$ size         : NULL\\n  \.\. \.\.\$ hjust        : num 0\.5\\n  \.\. \.\.\$ vjust        : NULL\\n  \.\. \.\.\$ angle        : NULL\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : NULL\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi FALSE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.- attr\(\*, "complete"\)= logi FALSE\\n  \.\.- attr\(\*, "validate"\)= logi TRUE\\n \$ coordinates:Classes 'CoordCartesian', 'Coord', 'ggproto', 'gg' <ggproto object: Class CoordCartesian, Coord, gg>\\n    aspect: function\\n    backtransform_range: function\\n    clip: on\\n    default: TRUE\\n    distance: function\\n    expand: TRUE\\n    is_free: function\\n    is_linear: function\\n    labels: function\\n    limits: list\\n    modify_scales: function\\n    range: function\\n    render_axis_h: function\\n    render_axis_v: function\\n    render_bg: function\\n    render_fg: function\\n    setup_data: function\\n    setup_layout: function\\n    setup_panel_guides: function\\n    setup_panel_params: function\\n    setup_params: function\\n    train_panel_guides: function\\n    transform: function\\n    super:  <ggproto object: Class CoordCartesian, Coord, gg> \\n \$ facet      :Classes 'FacetNull', 'Facet', 'ggproto', 'gg' <ggproto object: Class FacetNull, Facet, gg>\\n    compute_layout: function\\n    draw_back: function\\n    draw_front: function\\n    draw_labels: function\\n    draw_panels: function\\n    finish_data: function\\n    init_scales: function\\n    map_data: function\\n    params: list\\n    setup_data: function\\n    setup_params: function\\n    shrink: TRUE\\n    train_scales: function\\n    vars: function\\n    super:  <ggproto object: Class FacetNull, Facet, gg> \\n \$ plot_env   :<environment: 0x55988d2380e8> \\n \$ layout     :Classes 'Layout', 'ggproto', 'gg' <ggproto object: Class Layout, gg>\\n    coord: NULL\\n    coord_params: list\\n    facet: NULL\\n    facet_params: list\\n    finish_data: function\\n    get_scales: function\\n    layout: NULL\\n    map_position: function\\n    panel_params: NULL\\n    panel_scales_x: NULL\\n    panel_scales_y: NULL\\n    render: function\\n    render_labels: function\\n    reset_scales: function\\n    resolve_label: function\\n    setup: function\\n    setup_panel_guides: function\\n    setup_panel_params: function\\n    train_position: function\\n    super:  <ggproto object: Class Layout, gg> \\n \$ labels     :List of 7\\n  \.\.\$ title     : NULL\\n  \.\.\$ y         : chr "Area \(Km2\)"\\n  \.\.\$ x         : chr "LUC category"\\n  \.\.\$ fill      : chr "Changes"\\n  \.\.\$ xend      : chr "as\.numeric\(To\) \+ 0\.3"\\n  \.\.\$ yend      : chr "area"\\n  \.\.\$ yintercept: chr "yintercept"\\n - attr\(\*, "class"\)= chr \[1:2\] "gg" "ggplot""
      Backtrace:
          ▆
       1. └─testthat::expect_output(...) at test_plots.R:59:3
       2.   └─testthat::expect_match(...)
       3.     └─testthat:::expect_match_(...)
      
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 110 ]
      Error: Test failures
      Execution halted
    ```

# PAMscapes

<details>

* Version: 0.5.3
* GitHub: NA
* Source code: https://github.com/cran/PAMscapes
* Date/Publication: 2023-10-04 16:20:05 UTC
* Number of recursive dependencies: 109

Run `revdepcheck::cloud_details(, "PAMscapes")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘PAMscapes-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: markNA
    > ### Title: Mark NA Values by Time and Frequency
    > ### Aliases: markNA
    > 
    > ### ** Examples
    > 
    > manta <- checkSoundscapeInput(system.file('extdata/MANTAExampleSmall1.csv', package='PAMscapes'))
    ...
      2.   ├─ggplot2::guides(...)
      3.   │ └─rlang::list2(...)
      4.   └─ggplot2::guide_colorbar(...)
      5.     └─ggplot2::new_guide(...)
      6.       └─ggplot2:::validate_theme(params$theme)
      7.         └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
      8.           └─ggplot2 (local) `<fn>`(dots[[1L]][[2L]], dots[[2L]][[2L]], element_tree = `<named list>`)
      9.             └─cli::cli_abort(...)
     10.               └─rlang::abort(...)
    Execution halted
    ```

# PAsso

<details>

* Version: 0.1.10
* GitHub: https://github.com/XiaoruiZhu/PAsso
* Source code: https://github.com/cran/PAsso
* Date/Publication: 2021-06-18 09:20:08 UTC
* Number of recursive dependencies: 180

Run `revdepcheck::cloud_details(, "PAsso")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘PAsso-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot
    > ### Title: A matrix of partial regression plots between responses after
    > ###   adjustments
    > ### Aliases: plot plot.PAsso
    > 
    > ### ** Examples
    > 
    ...
    > 
    > PAsso_2v <- PAsso(responses = c("PreVote.num", "PID"),
    +                  adjustments = c("income.num", "age", "edu.year"),
    +                  data = ANES2016)
    > 
    > plot(PAsso_2v)
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Calls: <Anonymous> ... do.call -> <Anonymous> -> scale_y_continuous -> startsWith
    Execution halted
    ```

# PHENTHAUproc

<details>

* Version: 0.9.9
* GitHub: NA
* Source code: https://github.com/cran/PHENTHAUproc
* Date/Publication: 2023-12-06 16:10:04 UTC
* Number of recursive dependencies: 91

Run `revdepcheck::cloud_details(, "PHENTHAUproc")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘PHENTHAUproc.Rmd’ using rmarkdown
    
    Quitting from lines 181-208 [unnamed-chunk-11] (PHENTHAUproc.Rmd)
    Error: processing vignette 'PHENTHAUproc.Rmd' failed with diagnostics:
    The `legend.title` theme element must be a <element_text> object.
    --- failed re-building ‘PHENTHAUproc.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘PHENTHAUproc.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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

*   checking re-building of vignette outputs ... WARNING
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

# PPforest

<details>

* Version: 0.1.3
* GitHub: https://github.com/natydasilva/PPforest
* Source code: https://github.com/cran/PPforest
* Date/Publication: 2022-09-09 23:32:55 UTC
* Number of recursive dependencies: 104

Run `revdepcheck::cloud_details(, "PPforest")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘PPforest-vignette.Rmd’ using rmarkdown
    
    Quitting from lines 118-129 [descri] (PPforest-vignette.Rmd)
    Error: processing vignette 'PPforest-vignette.Rmd' failed with diagnostics:
    cannot coerce type 'closure' to vector of type 'character'
    --- failed re-building ‘PPforest-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘PPforest-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.4Mb
      sub-directories of 1Mb or more:
        libs   7.1Mb
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
    2023-12-19 10:44:04 : ***** Data integration finished!, 0 mins elapsed.
    Put the data into a new Seurat object...
    2023-12-19 10:44:04 : ***** New Seurat object is generated!, 0.003 mins elapsed.
    >   SpaPlot(seuInt)
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: <Anonymous> ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 19.3Mb
      sub-directories of 1Mb or more:
        data   3.2Mb
        libs  15.3Mb
    ```

# PSAboot

<details>

* Version: 1.3.8
* GitHub: https://github.com/jbryer/PSAboot
* Source code: https://github.com/cran/PSAboot
* Date/Publication: 2023-10-23 19:20:02 UTC
* Number of recursive dependencies: 117

Run `revdepcheck::cloud_details(, "PSAboot")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘MatchingOrder.Rmd’ using rmarkdown
    --- finished re-building ‘MatchingOrder.Rmd’
    
    --- re-building ‘PSAboot.Rmd’ using rmarkdown
    
    Quitting from lines 154-155 [lalonde.plot] (PSAboot.Rmd)
    Error: processing vignette 'PSAboot.Rmd' failed with diagnostics:
    Can't combine <character> and <logical>.
    --- failed re-building ‘PSAboot.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘PSAboot.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        doc   5.5Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4 marked UTF-8 strings
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

# ParBayesianOptimization

<details>

* Version: 1.2.6
* GitHub: https://github.com/AnotherSamWilson/ParBayesianOptimization
* Source code: https://github.com/cran/ParBayesianOptimization
* Date/Publication: 2022-10-18 14:47:54 UTC
* Number of recursive dependencies: 108

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
      4.   ├─ggplot2::guides(...)
      5.   │ └─rlang::list2(...)
      6.   └─ggplot2::guide_legend(...)
      7.     └─ggplot2::new_guide(...)
      8.       └─ggplot2:::validate_theme(params$theme)
      9.         └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
     10.           └─ggplot2 (local) `<fn>`(dots[[1L]][[1L]], dots[[2L]][[1L]], element_tree = `<named list>`)
     11.             └─cli::cli_abort(...)
     12.               └─rlang::abort(...)
    Execution halted
    ```

# PeakSegOptimal

<details>

* Version: 2018.05.25
* GitHub: NA
* Source code: https://github.com/cran/PeakSegOptimal
* Date/Publication: 2018-05-25 15:50:28 UTC
* Number of recursive dependencies: 49

Run `revdepcheck::cloud_details(, "PeakSegOptimal")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘PeakSegOptimal-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: PeakSegPDPAchrom
    > ### Title: PeakSegPDPAchrom
    > ### Aliases: PeakSegPDPAchrom
    > 
    > ### ** Examples
    > 
    > 
    ...
     21. └─vctrs (local) `<fn>`()
     22.   └─vctrs::vec_default_ptype2(...)
     23.     ├─base::withRestarts(...)
     24.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     25.     │   └─base (local) doWithOneRestart(return(expr), restart)
     26.     └─vctrs::stop_incompatible_type(...)
     27.       └─vctrs:::stop_incompatible(...)
     28.         └─vctrs:::stop_vctrs(...)
     29.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

# PhenotypeSimulator

<details>

* Version: 0.3.4
* GitHub: https://github.com/HannahVMeyer/PhenotypeSimulator
* Source code: https://github.com/cran/PhenotypeSimulator
* Date/Publication: 2021-07-16 13:30:02 UTC
* Number of recursive dependencies: 99

Run `revdepcheck::cloud_details(, "PhenotypeSimulator")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘PhenotypeSimulator.Rmd’ using rmarkdown
    --- finished re-building ‘PhenotypeSimulator.Rmd’
    
    --- re-building ‘Simulation-and-LinearModel.Rmd’ using rmarkdown
    
    Quitting from lines 283-345 [heatmaps] (Simulation-and-LinearModel.Rmd)
    Error: processing vignette 'Simulation-and-LinearModel.Rmd' failed with diagnostics:
    The `legend.title` theme element must be a <element_text> object.
    ...
    --- finished re-building ‘SimulationBasedOnExampleData.Rmd’
    
    --- re-building ‘sample-scripts-external-genotype-simulation.Rmd’ using rmarkdown
    --- finished re-building ‘sample-scripts-external-genotype-simulation.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Simulation-and-LinearModel.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        doc       1.5Mb
        extdata   3.1Mb
        libs      1.0Mb
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

# QurvE

<details>

* Version: 1.1
* GitHub: https://github.com/NicWir/QurvE
* Source code: https://github.com/cran/QurvE
* Date/Publication: 2023-07-04 13:33:02 UTC
* Number of recursive dependencies: 154

Run `revdepcheck::cloud_details(, "QurvE")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘QurvE-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.grid
    > ### Title: Plot a matrix of growth curve panels
    > ### Aliases: plot.grid
    > 
    > ### ** Examples
    > 
    > # Create random growth data set
    ...
      2.   ├─ggplot2::guides(...)
      3.   │ └─rlang::list2(...)
      4.   └─ggplot2::guide_colourbar(...)
      5.     └─ggplot2::new_guide(...)
      6.       └─ggplot2:::validate_theme(params$theme)
      7.         └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
      8.           └─ggplot2 (local) `<fn>`(dots[[1L]][[5L]], dots[[2L]][[5L]], element_tree = `<named list>`)
      9.             └─cli::cli_abort(...)
     10.               └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.4Mb
      sub-directories of 1Mb or more:
        doc         3.7Mb
        shiny_app   1.4Mb
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
      installed size is 53.6Mb
      sub-directories of 1Mb or more:
        doc           5.3Mb
        htmlwidgets   1.8Mb
        libs         43.9Mb
    ```

# RavenR

<details>

* Version: 2.2.0
* GitHub: https://github.com/rchlumsk/RavenR
* Source code: https://github.com/cran/RavenR
* Date/Publication: 2022-10-28 21:02:50 UTC
* Number of recursive dependencies: 141

Run `revdepcheck::cloud_details(, "RavenR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘RavenR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: rvn_rvi_process_ggplot
    > ### Title: Plot Raven hydrologic process network
    > ### Aliases: rvn_rvi_process_ggplot
    > 
    > ### ** Examples
    > 
    > 
    ...
     21. └─vctrs (local) `<fn>`()
     22.   └─vctrs::vec_default_ptype2(...)
     23.     ├─base::withRestarts(...)
     24.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     25.     │   └─base (local) doWithOneRestart(return(expr), restart)
     26.     └─vctrs::stop_incompatible_type(...)
     27.       └─vctrs:::stop_incompatible(...)
     28.         └─vctrs:::stop_vctrs(...)
     29.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Introduction_to_RavenR.Rmd’ using rmarkdown
    
    Quitting from lines 213-217 [RVI connection plot example] (Introduction_to_RavenR.Rmd)
    Error: processing vignette 'Introduction_to_RavenR.Rmd' failed with diagnostics:
    Can't combine <character> and <logical>.
    --- failed re-building ‘Introduction_to_RavenR.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Introduction_to_RavenR.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        doc       1.0Mb
        extdata   1.0Mb
        libs      1.9Mb
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

# SHELF

<details>

* Version: 1.9.0
* GitHub: https://github.com/OakleyJ/SHELF
* Source code: https://github.com/cran/SHELF
* Date/Publication: 2023-06-07 16:00:02 UTC
* Number of recursive dependencies: 118

Run `revdepcheck::cloud_details(, "SHELF")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Dirichlet-elicitation.Rmd’ using rmarkdown
    --- finished re-building ‘Dirichlet-elicitation.Rmd’
    
    --- re-building ‘Multivariate-normal-copula.Rmd’ using rmarkdown
    
    Quitting from lines 180-188 [unnamed-chunk-12] (Multivariate-normal-copula.Rmd)
    Error: processing vignette 'Multivariate-normal-copula.Rmd' failed with diagnostics:
    cannot coerce type 'closure' to vector of type 'character'
    ...
    --- failed re-building ‘Multivariate-normal-copula.Rmd’
    
    --- re-building ‘SHELF-overview.Rmd’ using rmarkdown
    --- finished re-building ‘SHELF-overview.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Multivariate-normal-copula.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# SRTsim

<details>

* Version: 0.99.6
* GitHub: NA
* Source code: https://github.com/cran/SRTsim
* Date/Publication: 2023-01-13 15:20:02 UTC
* Number of recursive dependencies: 164

Run `revdepcheck::cloud_details(, "SRTsim")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘SRTsim-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: srtsim_count_affine
    > ### Title: Generate Data with Estimated Parameters For A New Designed
    > ###   Pattern
    > ### Aliases: srtsim_count_affine
    > 
    > ### ** Examples
    > 
    ...
      2.   ├─ggplot2::guides(...)
      3.   │ └─rlang::list2(...)
      4.   └─ggplot2::guide_colourbar(...)
      5.     └─ggplot2::new_guide(...)
      6.       └─ggplot2:::validate_theme(params$theme)
      7.         └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
      8.           └─ggplot2 (local) `<fn>`(dots[[1L]][[2L]], dots[[2L]][[2L]], element_tree = `<named list>`)
      9.             └─cli::cli_abort(...)
     10.               └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘SRTsim.Rmd’ using rmarkdown
    The magick package is required to crop "/tmp/workdir/SRTsim/new/SRTsim.Rcheck/vign_test/SRTsim/vignettes/SRTsim_files/figure-html/tissue simulation metrics comparison-1.png" but not available.
    
    Quitting from lines 144-146 [pattern comparison] (SRTsim.Rmd)
    Error: processing vignette 'SRTsim.Rmd' failed with diagnostics:
    The `legend.title` theme element must be a <element_text> object.
    --- failed re-building ‘SRTsim.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘SRTsim.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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

*   checking re-building of vignette outputs ... WARNING
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

# SoupX

<details>

* Version: 1.6.2
* GitHub: https://github.com/constantAmateur/SoupX
* Source code: https://github.com/cran/SoupX
* Date/Publication: 2022-11-01 14:00:03 UTC
* Number of recursive dependencies: 197

Run `revdepcheck::cloud_details(, "SoupX")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘pbmcTutorial.Rmd’ using rmarkdown
    
    Quitting from lines 195-197 [sanity_check] (pbmcTutorial.Rmd)
    Error: processing vignette 'pbmcTutorial.Rmd' failed with diagnostics:
    Can't combine <character> and <logical>.
    --- failed re-building ‘pbmcTutorial.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘pbmcTutorial.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        data   4.1Mb
        doc    1.0Mb
    ```

# SpatPCA

<details>

* Version: 1.3.5
* GitHub: https://github.com/egpivo/SpatPCA
* Source code: https://github.com/cran/SpatPCA
* Date/Publication: 2023-11-13 09:33:19 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "SpatPCA")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘demo-one-dim-location.Rmd’ using rmarkdown
    
    warning: inv_sympd(): given matrix is not symmetric
    
    warning: inv_sympd(): given matrix is not symmetric
    
    warning: inv_sympd(): given matrix is not symmetric
    
    warning: inv_sympd(): given matrix is not symmetric
    ...
    Quitting from lines 30-47 [unnamed-chunk-2] (demo-two-dim-location.Rmd)
    Error: processing vignette 'demo-two-dim-location.Rmd' failed with diagnostics:
    The `legend.text` theme element must be a <element_text> object.
    --- failed re-building ‘demo-two-dim-location.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘demo-two-dim-location.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.7Mb
      sub-directories of 1Mb or more:
        doc    1.8Mb
        libs   5.8Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# SynthETIC

<details>

* Version: 1.0.5
* GitHub: https://github.com/agi-lab/SynthETIC
* Source code: https://github.com/cran/SynthETIC
* Date/Publication: 2023-09-03 13:50:05 UTC
* Number of recursive dependencies: 57

Run `revdepcheck::cloud_details(, "SynthETIC")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘SynthETIC-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.claims
    > ### Title: Plot of Cumulative Claims Payments (Incurred Pattern)
    > ### Aliases: plot.claims
    > 
    > ### ** Examples
    > 
    > plot(test_claims_object)
    ...
      3.   ├─ggplot2::guides(...)
      4.   │ └─rlang::list2(...)
      5.   └─ggplot2::guide_legend(...)
      6.     └─ggplot2::new_guide(...)
      7.       └─ggplot2:::validate_theme(params$theme)
      8.         └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
      9.           └─ggplot2 (local) `<fn>`(dots[[1L]][[1L]], dots[[2L]][[1L]], element_tree = `<named list>`)
     10.             └─cli::cli_abort(...)
     11.               └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘SynthETIC-demo.Rmd’ using rmarkdown
    
    Quitting from lines 1029-1032 [unnamed-chunk-45] (SynthETIC-demo.Rmd)
    Error: processing vignette 'SynthETIC-demo.Rmd' failed with diagnostics:
    The `legend.title` theme element must be a <element_text> object.
    --- failed re-building ‘SynthETIC-demo.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘SynthETIC-demo.Rmd’
    
    Error: Vignette re-building failed.
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

*   checking re-building of vignette outputs ... WARNING
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

# TOSTER

<details>

* Version: 0.8.0
* GitHub: NA
* Source code: https://github.com/cran/TOSTER
* Date/Publication: 2023-09-14 20:40:05 UTC
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
     15. │               └─jmvcore (local) fun()
     16. │                 ├─base::do.call(...)
     17. │                 └─TOSTER (local) `<fn>`(`<Image>`, theme = `<named list>`, ggtheme = `<list>`)
     18. │                   └─TOSTER:::plot_tost_jam(TOSTres, ggtheme = ggtheme)
     19. │                     └─ggplot2::theme(...)
     20. │                       └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
     21. │                         ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
     22. │                         └─rlang::list2(..., ... = NULL)
     23. └─rlang::abort(message = message)
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

# add2ggplot

<details>

* Version: 0.3.0
* GitHub: https://github.com/JiaxiangBU/add2ggplot
* Source code: https://github.com/cran/add2ggplot
* Date/Publication: 2020-02-07 11:50:02 UTC
* Number of recursive dependencies: 56

Run `revdepcheck::cloud_details(, "add2ggplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘add2ggplot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: theme_white
    > ### Title: A ggplot theme with a white background.
    > ### Aliases: theme_white
    > 
    > ### ** Examples
    > 
    > datasets::mtcars %>%
    ...
    ! Argument 1 can't be empty.
    Backtrace:
        ▆
     1. ├─add2ggplot::theme_white()
     2. │ └─ggplot2::theme(...)
     3. │   └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
     4. │     ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
     5. │     └─rlang::list2(..., ... = NULL)
     6. └─rlang::abort(message = message)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘intro.Rmd’ using rmarkdown
    
    Quitting from lines 61-65 [unnamed-chunk-8] (intro.Rmd)
    Error: processing vignette 'intro.Rmd' failed with diagnostics:
    Argument 1 can't be empty.
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

# agriutilities

<details>

* Version: 1.1.0
* GitHub: https://github.com/AparicioJohan/agriutilities
* Source code: https://github.com/cran/agriutilities
* Date/Publication: 2023-03-19 08:50:16 UTC
* Number of recursive dependencies: 117

Run `revdepcheck::cloud_details(, "agriutilities")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘agriutilities-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: covcor_heat
    > ### Title: Correlation Covariance Heatmap
    > ### Aliases: covcor_heat
    > 
    > ### ** Examples
    > 
    > library(agriutilities)
    ...
      2.   ├─ggplot2::guides(...)
      3.   │ └─rlang::list2(...)
      4.   └─ggplot2::guide_colorbar(...)
      5.     └─ggplot2::new_guide(...)
      6.       └─ggplot2:::validate_theme(params$theme)
      7.         └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
      8.           └─ggplot2 (local) `<fn>`(dots[[1L]][[3L]], dots[[2L]][[3L]], element_tree = `<named list>`)
      9.             └─cli::cli_abort(...)
     10.               └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘asreml’
    ```

# airGR

<details>

* Version: 1.7.6
* GitHub: NA
* Source code: https://github.com/cran/airGR
* Date/Publication: 2023-10-26 07:30:05 UTC
* Number of recursive dependencies: 110

Run `revdepcheck::cloud_details(, "airGR")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘V01_get_started.Rmd’ using rmarkdown
    --- finished re-building ‘V01_get_started.Rmd’
    
    --- re-building ‘V02.1_param_optim.Rmd’ using rmarkdown
    --- finished re-building ‘V02.1_param_optim.Rmd’
    
    --- re-building ‘V02.2_param_mcmc.Rmd’ using rmarkdown
    
    Quitting from lines 168-169 [unnamed-chunk-12] (V02.2_param_mcmc.Rmd)
    ...
    --- finished re-building ‘V04_cemaneige_hysteresis.Rmd’
    
    --- re-building ‘V05_sd_model.Rmd’ using rmarkdown
    --- finished re-building ‘V05_sd_model.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘V02.2_param_mcmc.Rmd’
    
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

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘ggtree’
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

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘ggtree’
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

*   checking re-building of vignette outputs ... WARNING
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
      Actual value: "List of 11\\n \$ data       :'data\.frame':\\t72 obs\. of  6 variables:\\n  \.\.\$ Ind\.ID    : Factor w/ 24 levels "A10","A12","AA9",\.\.: 3 1 2 5 6 4 7 8 11 9 \.\.\.\\n  \.\.\$ origin\.pop: Factor w/ 3 levels "pop\.1","pop\.2",\.\.: 1 1 1 2 2 2 3 3 1 1 \.\.\.\\n  \.\.\$ pred\.pop  : Factor w/ 3 levels "pop\.1","pop\.3",\.\.: 1 2 2 1 1 1 1 1 2 2 \.\.\.\\n  \.\.\$ fold_n    : chr \[1:72\] "fold_1" "fold_1" "fold_1" "fold_1" \.\.\.\\n  \.\.\$ variable  : Factor w/ 3 levels "pop\.1","pop\.2",\.\.: 1 1 1 1 1 1 1 1 1 1 \.\.\.\\n  \.\.\$ value     : num \[1:72\] 0\.4 0\.326 0\.26 0\.383 0\.44 \.\.\.\\n \$ layers     :List of 1\\n  \.\.\$ :Classes 'LayerInstance', 'Layer', 'ggproto', 'gg' <ggproto object: Class LayerInstance, Layer, gg>\\n    aes_params: list\\n    compute_aesthetics: function\\n    compute_geom_1: function\\n    compute_geom_2: function\\n    compute_position: function\\n    compute_statistic: function\\n    computed_geom_params: NULL\\n    computed_mapping: NULL\\n    computed_stat_params: NULL\\n    constructor: call\\n    data: waiver\\n    draw_geom: function\\n    finish_statistics: function\\n    geom: <ggproto object: Class GeomBar, GeomRect, Geom, gg>\\n        aesthetics: function\\n        default_aes: uneval\\n        draw_group: function\\n        draw_key: function\\n        draw_layer: function\\n        draw_panel: function\\n        extra_params: just na\.rm orientation\\n        handle_na: function\\n        non_missing_aes: xmin xmax ymin ymax\\n        optional_aes: \\n        parameters: function\\n        rename_size: TRUE\\n        required_aes: x y\\n        setup_data: function\\n        setup_params: function\\n        use_defaults: function\\n        super:  <ggproto object: Class GeomRect, Geom, gg>\\n    geom_params: list\\n    inherit\.aes: TRUE\\n    layer_data: function\\n    map_statistic: function\\n    mapping: NULL\\n    position: <ggproto object: Class PositionStack, Position, gg>\\n        compute_layer: function\\n        compute_panel: function\\n        fill: FALSE\\n        required_aes: \\n        reverse: FALSE\\n        setup_data: function\\n        setup_params: function\\n        type: NULL\\n        vjust: 1\\n        super:  <ggproto object: Class Position, gg>\\n    print: function\\n    setup_layer: function\\n    show\.legend: NA\\n    stat: <ggproto object: Class StatIdentity, Stat, gg>\\n        aesthetics: function\\n        compute_group: function\\n        compute_layer: function\\n        compute_panel: function\\n        default_aes: uneval\\n        dropped_aes: \\n        extra_params: na\.rm\\n        finish_layer: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        required_aes: \\n        retransform: TRUE\\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Stat, gg>\\n    stat_params: list\\n    super:  <ggproto object: Class Layer, gg> \\n \$ scales     :Classes 'ScalesList', 'ggproto', 'gg' <ggproto object: Class ScalesList, gg>\\n    add: function\\n    add_defaults: function\\n    add_missing: function\\n    backtransform_df: function\\n    clone: function\\n    find: function\\n    get_scales: function\\n    has_scale: function\\n    input: function\\n    map_df: function\\n    n: function\\n    non_position_scales: function\\n    scales: list\\n    train_df: function\\n    transform_df: function\\n    super:  <ggproto object: Class ScalesList, gg> \\n \$ guides     :Classes 'Guides', 'ggproto', 'gg' <ggproto object: Class Guides, gg>\\n    add: function\\n    assemble: function\\n    build: function\\n    draw: function\\n    get_custom: function\\n    get_guide: function\\n    get_params: function\\n    get_position: function\\n    guides: list\\n    merge: function\\n    missing: <ggproto object: Class GuideNone, Guide, gg>\\n        arrange_layout: function\\n        assemble_drawing: function\\n        available_aes: any\\n        build_decor: function\\n        build_labels: function\\n        build_ticks: function\\n        build_title: function\\n        draw: function\\n        draw_early_exit: function\\n        elements: list\\n        extract_decor: function\\n        extract_key: function\\n        extract_params: function\\n        get_layer_key: function\\n        hashables: list\\n        measure_grobs: function\\n        merge: function\\n        override_elements: function\\n        params: list\\n        process_layers: function\\n        setup_elements: function\\n        setup_params: function\\n        train: function\\n        transform: function\\n        super:  <ggproto object: Class GuideNone, Guide, gg>\\n    package_box: function\\n    print: function\\n    process_layers: function\\n    setup: function\\n    subset_guides: function\\n    train: function\\n    update_params: function\\n    super:  <ggproto object: Class Guides, gg> \\n \$ mapping    :List of 3\\n  \.\.\$ x   : language ~Ind\.ID\\n  \.\. \.\.- attr\(\*, "\.Environment"\)=<environment: 0x5615003983c8> \\n  \.\.\$ y   : language ~value\\n  \.\. \.\.- attr\(\*, "\.Environment"\)=<environment: 0x5615003983c8> \\n  \.\.\$ fill: language ~variable\\n  \.\. \.\.- attr\(\*, "\.Environment"\)=<environment: 0x5615003983c8> \\n  \.\.- attr\(\*, "class"\)= chr "uneval"\\n \$ theme      :List of 124\\n  \.\.\$ line                            :List of 6\\n  \.\. \.\.\$ colour       : chr "black"\\n  \.\. \.\.\$ linewidth    : num 0\.5\\n  \.\. \.\.\$ linetype     : num 1\\n  \.\. \.\.\$ lineend      : chr "butt"\\n  \.\. \.\.\$ arrow        : logi FALSE\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_line" "element"\\n  \.\.\$ rect                            :List of 5\\n  \.\. \.\.\$ fill         : chr "white"\\n  \.\. \.\.\$ colour       : chr "black"\\n  \.\. \.\.\$ linewidth    : num 0\.5\\n  \.\. \.\.\$ linetype     : num 1\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_rect" "element"\\n  \.\.\$ text                            :List of 11\\n  \.\. \.\.\$ family       : chr ""\\n  \.\. \.\.\$ face         : chr "plain"\\n  \.\. \.\.\$ colour       : chr "black"\\n  \.\. \.\.\$ size         : num 11\\n  \.\. \.\.\$ hjust        : num 0\.5\\n  \.\. \.\.\$ vjust        : num 0\.5\\n  \.\. \.\.\$ angle        : num 0\\n  \.\. \.\.\$ lineheight   : num 0\.9\\n  \.\. \.\.\$ margin       : 'margin' num \[1:4\] 0points 0points 0points 0points\\n  \.\. \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\. \.\.\$ debug        : logi FALSE\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ title                           : NULL\\n  \.\.\$ aspect\.ratio                    : NULL\\n  \.\.\$ axis\.title                      : NULL\\n  \.\.\$ axis\.title\.x                    : list\(\)\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_blank" "element"\\n  \.\.\$ axis\.title\.x\.top                :List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : NULL\\n  \.\. \.\.\$ size         : NULL\\n  \.\. \.\.\$ hjust        : NULL\\n  \.\. \.\.\$ vjust        : num 0\\n  \.\. \.\.\$ angle        : NULL\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : 'margin' num \[1:4\] 0points 0points 2\.75points 0points\\n  \.\. \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ axis\.title\.x\.bottom             : NULL\\n  \.\.\$ axis\.title\.y                    :List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : NULL\\n  \.\. \.\.\$ size         : NULL\\n  \.\. \.\.\$ hjust        : NULL\\n  \.\. \.\.\$ vjust        : num 1\\n  \.\. \.\.\$ angle        : num 90\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : 'margin' num \[1:4\] 0points 2\.75points 0points 0points\\n  \.\. \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ axis\.title\.y\.left               : NULL\\n  \.\.\$ axis\.title\.y\.right              :List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : NULL\\n  \.\. \.\.\$ size         : NULL\\n  \.\. \.\.\$ hjust        : NULL\\n  \.\. \.\.\$ vjust        : num 0\\n  \.\. \.\.\$ angle        : num -90\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : 'margin' num \[1:4\] 0points 0points 0points 2\.75points\\n  \.\. \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ axis\.text                       :List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : chr "grey30"\\n  \.\. \.\.\$ size         : 'rel' num 0\.8\\n  \.\. \.\.\$ hjust        : NULL\\n  \.\. \.\.\$ vjust        : NULL\\n  \.\. \.\.\$ angle        : NULL\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : NULL\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ axis\.text\.x                     :List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : NULL\\n  \.\. \.\.\$ size         : NULL\\n  \.\. \.\.\$ hjust        : NULL\\n  \.\. \.\.\$ vjust        : num 1\\n  \.\. \.\.\$ angle        : num 90\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : 'margin' num \[1:4\] 2\.2points 0points 0points 0points\\n  \.\. \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi FALSE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ axis\.text\.x\.top                 :List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : NULL\\n  \.\. \.\.\$ size         : NULL\\n  \.\. \.\.\$ hjust        : NULL\\n  \.\. \.\.\$ vjust        : num 0\\n  \.\. \.\.\$ angle        : NULL\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : 'margin' num \[1:4\] 0points 0points 2\.2points 0points\\n  \.\. \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ axis\.text\.x\.bottom              : NULL\\n  \.\.\$ axis\.text\.y                     :List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : NULL\\n  \.\. \.\.\$ size         : NULL\\n  \.\. \.\.\$ hjust        : num 1\\n  \.\. \.\.\$ vjust        : NULL\\n  \.\. \.\.\$ angle        : NULL\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : 'margin' num \[1:4\] 0points 2\.2points 0points 0points\\n  \.\. \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ axis\.text\.y\.left                : NULL\\n  \.\.\$ axis\.text\.y\.right               :List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : NULL\\n  \.\. \.\.\$ size         : NULL\\n  \.\. \.\.\$ hjust        : num 0\\n  \.\. \.\.\$ vjust        : NULL\\n  \.\. \.\.\$ angle        : NULL\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : 'margin' num \[1:4\] 0points 0points 0points 2\.2points\\n  \.\. \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ axis\.ticks                      :List of 6\\n  \.\. \.\.\$ colour       : chr "grey20"\\n  \.\. \.\.\$ linewidth    : NULL\\n  \.\. \.\.\$ linetype     : NULL\\n  \.\. \.\.\$ lineend      : NULL\\n  \.\. \.\.\$ arrow        : logi FALSE\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_line" "element"\\n  \.\.\$ axis\.ticks\.x                    : NULL\\n  \.\.\$ axis\.ticks\.x\.top                : NULL\\n  \.\.\$ axis\.ticks\.x\.bottom             : NULL\\n  \.\.\$ axis\.ticks\.y                    : NULL\\n  \.\.\$ axis\.ticks\.y\.left               : NULL\\n  \.\.\$ axis\.ticks\.y\.right              : NULL\\n  \.\.\$ axis\.minor\.ticks\.x\.top          : NULL\\n  \.\.\$ axis\.minor\.ticks\.x\.bottom       : NULL\\n  \.\.\$ axis\.minor\.ticks\.y\.left         : NULL\\n  \.\.\$ axis\.minor\.ticks\.y\.right        : NULL\\n  \.\.\$ axis\.ticks\.length               : 'simpleUnit' num 2\.75points\\n  \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\.\$ axis\.ticks\.length\.x             : NULL\\n  \.\.\$ axis\.ticks\.length\.x\.top         : NULL\\n  \.\.\$ axis\.ticks\.length\.x\.bottom      : NULL\\n  \.\.\$ axis\.ticks\.length\.y             : NULL\\n  \.\.\$ axis\.ticks\.length\.y\.left        : NULL\\n  \.\.\$ axis\.ticks\.length\.y\.right       : NULL\\n  \.\.\$ axis\.minor\.ticks\.length         : 'rel' num 0\.75\\n  \.\.\$ axis\.minor\.ticks\.length\.x       : NULL\\n  \.\.\$ axis\.minor\.ticks\.length\.x\.top   : NULL\\n  \.\.\$ axis\.minor\.ticks\.length\.x\.bottom: NULL\\n  \.\.\$ axis\.minor\.ticks\.length\.y       : NULL\\n  \.\.\$ axis\.minor\.ticks\.length\.y\.left  : NULL\\n  \.\.\$ axis\.minor\.ticks\.length\.y\.right : NULL\\n  \.\.\$ axis\.line                       : list\(\)\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_blank" "element"\\n  \.\.\$ axis\.line\.x                     : NULL\\n  \.\.\$ axis\.line\.x\.top                 : NULL\\n  \.\.\$ axis\.line\.x\.bottom              : NULL\\n  \.\.\$ axis\.line\.y                     : NULL\\n  \.\.\$ axis\.line\.y\.left                : NULL\\n  \.\.\$ axis\.line\.y\.right               : NULL\\n  \.\.\$ legend\.background               :List of 5\\n  \.\. \.\.\$ fill         : NULL\\n  \.\. \.\.\$ colour       : logi NA\\n  \.\. \.\.\$ linewidth    : NULL\\n  \.\. \.\.\$ linetype     : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_rect" "element"\\n  \.\.\$ legend\.margin                   : 'margin' num \[1:4\] 5\.5points 5\.5points 5\.5points 5\.5points\\n  \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\.\$ legend\.spacing                  : 'simpleUnit' num 11points\\n  \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\.\$ legend\.spacing\.x                : NULL\\n  \.\.\$ legend\.spacing\.y                : NULL\\n  \.\.\$ legend\.key                      : NULL\\n  \.\.\$ legend\.key\.size                 : 'simpleUnit' num 1\.2lines\\n  \.\. \.\.- attr\(\*, "unit"\)= int 3\\n  \.\.\$ legend\.key\.height               : NULL\\n  \.\.\$ legend\.key\.width                : NULL\\n  \.\.\$ legend\.key\.spacing              : 'simpleUnit' num 5\.5points\\n  \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\.\$ legend\.key\.spacing\.x            : NULL\\n  \.\.\$ legend\.key\.spacing\.y            : NULL\\n  \.\.\$ legend\.frame                    : NULL\\n  \.\.\$ legend\.ticks                    : NULL\\n  \.\.\$ legend\.ticks\.length             : NULL\\n  \.\.\$ legend\.axis\.line                : NULL\\n  \.\.\$ legend\.text                     :List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : NULL\\n  \.\. \.\.\$ size         : 'rel' num 0\.8\\n  \.\. \.\.\$ hjust        : NULL\\n  \.\. \.\.\$ vjust        : NULL\\n  \.\. \.\.\$ angle        : NULL\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : NULL\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ legend\.text\.position            : NULL\\n  \.\.\$ legend\.title                    :List of 11\\n  \.\. \.\.\$ family       : NULL\\n  \.\. \.\.\$ face         : NULL\\n  \.\. \.\.\$ colour       : NULL\\n  \.\. \.\.\$ size         : NULL\\n  \.\. \.\.\$ hjust        : num 0\\n  \.\. \.\.\$ vjust        : NULL\\n  \.\. \.\.\$ angle        : NULL\\n  \.\. \.\.\$ lineheight   : NULL\\n  \.\. \.\.\$ margin       : NULL\\n  \.\. \.\.\$ debug        : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_text" "element"\\n  \.\.\$ legend\.title\.position           : NULL\\n  \.\.\$ legend\.position                 : chr "right"\\n  \.\.\$ legend\.position\.inside          : NULL\\n  \.\.\$ legend\.direction                : NULL\\n  \.\.\$ legend\.byrow                    : NULL\\n  \.\.\$ legend\.justification            : chr "center"\\n  \.\.\$ legend\.justification\.top        : NULL\\n  \.\.\$ legend\.justification\.bottom     : NULL\\n  \.\.\$ legend\.justification\.left       : NULL\\n  \.\.\$ legend\.justification\.right      : NULL\\n  \.\.\$ legend\.justification\.inside     : NULL\\n  \.\.\$ legend\.location                 : NULL\\n  \.\.\$ legend\.box                      : NULL\\n  \.\.\$ legend\.box\.just                 : NULL\\n  \.\.\$ legend\.box\.margin               : 'margin' num \[1:4\] 0cm 0cm 0cm 0cm\\n  \.\. \.\.- attr\(\*, "unit"\)= int 1\\n  \.\.\$ legend\.box\.background           : list\(\)\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_blank" "element"\\n  \.\.\$ legend\.box\.spacing              : 'simpleUnit' num 11points\\n  \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\.\$ panel\.background                :List of 5\\n  \.\. \.\.\$ fill         : chr "white"\\n  \.\. \.\.\$ colour       : logi NA\\n  \.\. \.\.\$ linewidth    : NULL\\n  \.\. \.\.\$ linetype     : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_rect" "element"\\n  \.\.\$ panel\.border                    :List of 5\\n  \.\. \.\.\$ fill         : logi NA\\n  \.\. \.\.\$ colour       : chr "grey20"\\n  \.\. \.\.\$ linewidth    : NULL\\n  \.\. \.\.\$ linetype     : NULL\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_rect" "element"\\n  \.\.\$ panel\.spacing                   : 'simpleUnit' num 5\.5points\\n  \.\. \.\.- attr\(\*, "unit"\)= int 8\\n  \.\.\$ panel\.spacing\.x                 : NULL\\n  \.\.\$ panel\.spacing\.y                 : NULL\\n  \.\.\$ panel\.grid                      :List of 6\\n  \.\. \.\.\$ colour       : chr "grey92"\\n  \.\. \.\.\$ linewidth    : NULL\\n  \.\. \.\.\$ linetype     : NULL\\n  \.\. \.\.\$ lineend      : NULL\\n  \.\. \.\.\$ arrow        : logi FALSE\\n  \.\. \.\.\$ inherit\.blank: logi TRUE\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_line" "element"\\n  \.\.\$ panel\.grid\.major                : list\(\)\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_blank" "element"\\n  \.\.\$ panel\.grid\.minor                : list\(\)\\n  \.\. \.\.- attr\(\*, "class"\)= chr \[1:2\] "element_blank" "element"\\n  \.\.\$ panel\.grid\.major\.x              : NULL\\n  \.\.\$ panel\.grid\.major\.y              : NULL\\n  \.\.\$ panel\.grid\.minor\.x              : NULL\\n  \.\.\$ panel\.grid\.minor\.y              : NULL\\n  \.\. \[list output truncated\]\\n  \.\.- attr\(\*, "class"\)= chr \[1:2\] "theme" "gg"\\n  \.\.- attr\(\*, "complete"\)= logi TRUE\\n  \.\.- attr\(\*, "validate"\)= logi TRUE\\n \$ coordinates:Classes 'CoordCartesian', 'Coord', 'ggproto', 'gg' <ggproto object: Class CoordCartesian, Coord, gg>\\n    aspect: function\\n    backtransform_range: function\\n    clip: on\\n    default: FALSE\\n    distance: function\\n    expand: TRUE\\n    is_free: function\\n    is_linear: function\\n    labels: function\\n    limits: list\\n    modify_scales: function\\n    range: function\\n    render_axis_h: function\\n    render_axis_v: function\\n    render_bg: function\\n    render_fg: function\\n    setup_data: function\\n    setup_layout: function\\n    setup_panel_guides: function\\n    setup_panel_params: function\\n    setup_params: function\\n    train_panel_guides: function\\n    transform: function\\n    super:  <ggproto object: Class CoordCartesian, Coord, gg> \\n \$ facet      :Classes 'FacetGrid', 'Facet', 'ggproto', 'gg' <ggproto object: Class FacetGrid, Facet, gg>\\n    compute_layout: function\\n    draw_back: function\\n    draw_front: function\\n    draw_labels: function\\n    draw_panels: function\\n    finish_data: function\\n    init_scales: function\\n    map_data: function\\n    params: list\\n    setup_data: function\\n    setup_params: function\\n    shrink: TRUE\\n    train_scales: function\\n    vars: function\\n    super:  <ggproto object: Class FacetGrid, Facet, gg> \\n \$ plot_env   :<environment: 0x5615003983c8> \\n \$ layout     :Classes 'Layout', 'ggproto', 'gg' <ggproto object: Class Layout, gg>\\n    coord: NULL\\n    coord_params: list\\n    facet: NULL\\n    facet_params: list\\n    finish_data: function\\n    get_scales: function\\n    layout: NULL\\n    map_position: function\\n    panel_params: NULL\\n    panel_scales_x: NULL\\n    panel_scales_y: NULL\\n    render: function\\n    render_labels: function\\n    reset_scales: function\\n    resolve_label: function\\n    setup: function\\n    setup_panel_guides: function\\n    setup_panel_params: function\\n    train_position: function\\n    super:  <ggproto object: Class Layout, gg> \\n \$ labels     :List of 4\\n  \.\.\$ title: chr "K = 3  "\\n  \.\.\$ y    : chr "Probability"\\n  \.\.\$ x    : chr "Ind\.ID"\\n  \.\.\$ fill : chr "variable"\\n - attr\(\*, "class"\)= chr \[1:2\] "gg" "ggplot""
      Backtrace:
          ▆
       1. └─testthat::expect_output(str(plot), "List of 10") at test_membership.R:5:3
       2.   └─testthat::expect_match(...)
       3.     └─testthat:::expect_match_(...)
      
      [ FAIL 3 | WARN 1 | SKIP 0 | PASS 39 ]
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

*   checking re-building of vignette outputs ... WARNING
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

# basket

<details>

* Version: 0.10.11
* GitHub: https://github.com/kaneplusplus/basket
* Source code: https://github.com/cran/basket
* Date/Publication: 2021-10-16 23:30:22 UTC
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "basket")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘using-the-basket-package.rmd’ using rmarkdown
    
    Quitting from lines 97-100 [unnamed-chunk-5] (using-the-basket-package.rmd)
    Error: processing vignette 'using-the-basket-package.rmd' failed with diagnostics:
    The `legend.title` theme element must be a <element_text> object.
    --- failed re-building ‘using-the-basket-package.rmd’
    
    SUMMARY: processing the following file failed:
      ‘using-the-basket-package.rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# bayesAB

<details>

* Version: 1.1.3
* GitHub: https://github.com/FrankPortman/bayesAB
* Source code: https://github.com/cran/bayesAB
* Date/Publication: 2021-06-25 00:50:02 UTC
* Number of recursive dependencies: 75

Run `revdepcheck::cloud_details(, "bayesAB")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘bayesAB-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: bayesTest
    > ### Title: Fit a Bayesian model to A/B test data.
    > ### Aliases: bayesTest
    > 
    > ### ** Examples
    > 
    > A_binom <- rbinom(100, 1, .5)
    ...
     24. └─vctrs (local) `<fn>`()
     25.   └─vctrs::vec_default_ptype2(...)
     26.     ├─base::withRestarts(...)
     27.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     28.     │   └─base (local) doWithOneRestart(return(expr), restart)
     29.     └─vctrs::stop_incompatible_type(...)
     30.       └─vctrs:::stop_incompatible(...)
     31.         └─vctrs:::stop_vctrs(...)
     32.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(bayesAB)
      > 
      > test_check("bayesAB")
      [ FAIL 1 | WARN 3 | SKIP 0 | PASS 125 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
       35.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
       36.     │   └─base (local) doWithOneRestart(return(expr), restart)
       37.     └─vctrs::stop_incompatible_type(...)
       38.       └─vctrs:::stop_incompatible(...)
       39.         └─vctrs:::stop_vctrs(...)
       40.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
      
      [ FAIL 1 | WARN 3 | SKIP 0 | PASS 125 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘introduction.Rmd’ using rmarkdown
    
    Quitting from lines 64-67 [unnamed-chunk-4] (introduction.Rmd)
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    Can't combine <character> and <logical>.
    --- failed re-building ‘introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘introduction.Rmd’
    
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
      [ FAIL 1 | WARN 0 | SKIP 73 | PASS 84 ]
      
    ...
       24.                       └─methods::new(...)
       25.                         ├─methods::initialize(value, ...)
       26.                         └─Matrix (local) initialize(value, ...)
       27.                           ├─methods::callNextMethod()
       28.                           └─methods (local) .nextMethod(.Object = .Object, ... = ...)
       29.                             └─methods::validObject(.Object)
      
      [ FAIL 1 | WARN 0 | SKIP 73 | PASS 84 ]
      Error: Test failures
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: 'bayesQR', 'rstanarm'
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

*   checking re-building of vignette outputs ... WARNING
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

# bigtime

<details>

* Version: 0.2.3
* GitHub: https://github.com/ineswilms/bigtime
* Source code: https://github.com/cran/bigtime
* Date/Publication: 2023-08-21 20:30:05 UTC
* Number of recursive dependencies: 40

Run `revdepcheck::cloud_details(, "bigtime")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘bigtime-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: diagnostics_plot
    > ### Title: Creates a Diagnostic Plot
    > ### Aliases: diagnostics_plot
    > 
    > ### ** Examples
    > 
    > # VAR example
    ...
        ▆
     1. ├─bigtime::diagnostics_plot(mod, variable = 1)
     2. ├─bigtime:::diagnostics_plot.bigtime.VAR(mod, variable = 1)
     3. │ └─bigtime:::.diagnostics_plot(Y, fit, res, s, variable, dates)
     4. │   └─ggplot2::theme(...)
     5. │     └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
     6. │       ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
     7. │       └─rlang::list2(..., ... = NULL)
     8. └─rlang::abort(message = message)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.3Mb
      sub-directories of 1Mb or more:
        libs   7.0Mb
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
      - satisfaction_level = 0.45        -0.052
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

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘camcorder_record.Rmd’ using rmarkdown
    
    Quitting from lines 48-128 [style-diamonds-plot] (camcorder_record.Rmd)
    Error: processing vignette 'camcorder_record.Rmd' failed with diagnostics:
    The `legend.title` theme element must be a <element_text> object.
    --- failed re-building ‘camcorder_record.Rmd’
    
    --- re-building ‘camcorder_view.Rmd’ using rmarkdown
    ...
    --- finished re-building ‘camcorder_view.Rmd’
    
    --- re-building ‘pdf_fonts.Rmd’ using rmarkdown
    --- finished re-building ‘pdf_fonts.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘camcorder_record.Rmd’
    
    Error: Vignette re-building failed.
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

# cellpypes

<details>

* Version: 0.1.3
* GitHub: https://github.com/FelixTheStudent/cellpypes
* Source code: https://github.com/cran/cellpypes
* Date/Publication: 2022-05-19 07:00:08 UTC
* Number of recursive dependencies: 184

Run `revdepcheck::cloud_details(, "cellpypes")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘cellpypes-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_last
    > ### Title: Plot the last modified rule or class
    > ### Aliases: plot_last
    > 
    > ### ** Examples
    > 
    > plot_last(rule(simulated_umis, "T", "CD3E",">", 1))
    ...
     32. └─vctrs (local) `<fn>`()
     33.   └─vctrs::vec_default_ptype2(...)
     34.     ├─base::withRestarts(...)
     35.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     36.     │   └─base (local) doWithOneRestart(return(expr), restart)
     37.     └─vctrs::stop_incompatible_type(...)
     38.       └─vctrs:::stop_incompatible(...)
     39.         └─vctrs:::stop_vctrs(...)
     40.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(cellpypes)
      > 
      > test_check("cellpypes")
      converting counts to integer mode
      [ FAIL 1 | WARN 5 | SKIP 0 | PASS 54 ]
      
    ...
       42.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
       43.     │   └─base (local) doWithOneRestart(return(expr), restart)
       44.     └─vctrs::stop_incompatible_type(...)
       45.       └─vctrs:::stop_incompatible(...)
       46.         └─vctrs:::stop_vctrs(...)
       47.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
      
      [ FAIL 1 | WARN 5 | SKIP 0 | PASS 54 ]
      Error: Test failures
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

*   checking examples ... ERROR
    ```
    Running examples in ‘cheem-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: radial_cheem_tour
    > ### Title: Cheem tour; 1D manual tour on the selected attribution
    > ### Aliases: radial_cheem_tour
    > 
    > ### ** Examples
    > 
    > library(cheem)
    ...
    ! Argument 1 can't be empty.
    Backtrace:
        ▆
     1. ├─cheem::radial_cheem_tour(ames_rf_chm, basis = bas, manip_var = mv)
     2. │ └─ggplot2::theme(...)
     3. │   └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
     4. │     ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
     5. │     └─rlang::list2(..., ... = NULL)
     6. └─rlang::abort(message = message)
    Execution halted
    ```

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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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
      2.   ├─ggplot2::guides(...)
      3.   │ └─rlang::list2(...)
      4.   └─ggplot2::guide_legend(title.position = "top", title.hjust = 0.5)
      5.     └─ggplot2::new_guide(...)
      6.       └─ggplot2:::validate_theme(params$theme)
      7.         └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
      8.           └─ggplot2 (local) `<fn>`(dots[[1L]][[1L]], dots[[2L]][[1L]], element_tree = `<named list>`)
      9.             └─cli::cli_abort(...)
     10.               └─rlang::abort(...)
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

*   checking re-building of vignette outputs ... WARNING
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
        adding: report_dependencies152f391518a9/ (stored 0%)
        adding: report_dependencies152f391518a9/file152f49f53bcd.html (deflated 8%)
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

# corona

<details>

* Version: 0.3.0
* GitHub: NA
* Source code: https://github.com/cran/corona
* Date/Publication: 2020-09-23 09:30:03 UTC
* Number of recursive dependencies: 42

Run `revdepcheck::cloud_details(, "corona")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘corona-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: corona_vienna
    > ### Title: Plot Semmelweis' original data from Vienna.
    > ### Aliases: corona_vienna
    > ### Keywords: Semmelweis Vienna corona
    > 
    > ### ** Examples
    > 
    ...
     23. └─vctrs (local) `<fn>`()
     24.   └─vctrs::vec_default_ptype2(...)
     25.     ├─base::withRestarts(...)
     26.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     27.     │   └─base (local) doWithOneRestart(return(expr), restart)
     28.     └─vctrs::stop_incompatible_type(...)
     29.       └─vctrs:::stop_incompatible(...)
     30.         └─vctrs:::stop_vctrs(...)
     31.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

# covidcast

<details>

* Version: 0.5.2
* GitHub: https://github.com/cmu-delphi/covidcast
* Source code: https://github.com/cran/covidcast
* Date/Publication: 2023-07-12 23:40:06 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::cloud_details(, "covidcast")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘correlation-utils.Rmd’ using rmarkdown
    --- finished re-building ‘correlation-utils.Rmd’
    
    --- re-building ‘covidcast.Rmd’ using rmarkdown
    --- finished re-building ‘covidcast.Rmd’
    
    --- re-building ‘external-data.Rmd’ using rmarkdown
    --- finished re-building ‘external-data.Rmd’
    ...
    Quitting from lines 66-76 [unnamed-chunk-5] (plotting-signals.Rmd)
    Error: processing vignette 'plotting-signals.Rmd' failed with diagnostics:
    The `legend.text` theme element must be a <element_text> object.
    --- failed re-building ‘plotting-signals.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘plotting-signals.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 20 marked UTF-8 strings
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

# creditmodel

<details>

* Version: 1.3.1
* GitHub: NA
* Source code: https://github.com/cran/creditmodel
* Date/Publication: 2022-01-07 11:32:41 UTC
* Number of recursive dependencies: 72

Run `revdepcheck::cloud_details(, "creditmodel")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘creditmodel-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: partial_dependence_plot
    > ### Title: partial_dependence_plot
    > ### Aliases: partial_dependence_plot get_partial_dependence_plots
    > 
    > ### ** Examples
    > 
    > sub = cv_split(UCICreditCard, k = 30)[[1]]
    ...
    > #plot partial dependency of all variables
    > pd_list = get_partial_dependence_plots(model = lr_model, x_list = x_list[1:2],
    +  x_train = dat_train, save_data = FALSE,plot_show = TRUE)
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Error in gList(...) : only 'grobs' allowed in "gList"
    Calls: get_partial_dependence_plots ... <Anonymous> -> grobTree -> gTree -> setChildren -> gList
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        data   5.1Mb
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking examples ... ERROR
    ```
    Running examples in ‘crosshap-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: build_mid_dotplot
    > ### Title: Middle MG/hap dot plot
    > ### Aliases: build_mid_dotplot
    > 
    > ### ** Examples
    > 
    > build_mid_dotplot(HapObject, epsilon = 0.6, hide_labels = FALSE)
    ...
    ! Argument 1 can't be empty.
    Backtrace:
        ▆
     1. ├─crosshap::build_mid_dotplot(HapObject, epsilon = 0.6, hide_labels = FALSE)
     2. │ └─ggplot2::theme(...)
     3. │   └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
     4. │     ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
     5. │     └─rlang::list2(..., ... = NULL)
     6. └─rlang::abort(message = message)
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
       7. └─rlang::abort(message = message)
      
      [ FAIL 4 | WARN 0 | SKIP 2 | PASS 5 ]
      Deleting unused snapshots:
      • hapviz/haplotype-viz-alt2.svg
      • hapviz/haplotype-viz-isolatewt.svg
      • hapviz/haplotype-viz-nolabs.svg
      • hapviz/haplotype-viz2.svg
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

*   checking re-building of vignette outputs ... WARNING
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
    
    > ### Name: plot_joint_circ
    > ### Title: Circular Scatterplot of Turn Angles and Step Lengths
    > ### Aliases: plot_joint_circ
    > 
    > ### ** Examples
    > 
    > set.seed(123)
    ...
    ! Argument 1 can't be empty.
    Backtrace:
        ▆
     1. ├─cylcop::plot_joint_circ(traj)
     2. │ └─ggplot2::theme(...)
     3. │   └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
     4. │     ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
     5. │     └─rlang::list2(..., ... = NULL)
     6. └─rlang::abort(message = message)
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
      
      Quitting from lines 276-308 [daiquiri-overview-presence] (report_htmldoc.Rmd)
      
    ...
       31. └─rlang::abort(message = message)
      
      [ FAIL 8 | WARN 4 | SKIP 7 | PASS 466 ]
      Deleting unused snapshots:
      • aggregate_data/test_[ALL_FIELDS_COMBINED].csv
      • aggregate_data/test_[DUPLICATES].csv
      • aggregate_data/test_col1.csv
      • aggregate_data/test_col2.csv
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘daiquiri.Rmd’ using rmarkdown
    
    Quitting from lines 276-308 [unnamed-chunk-4] (report_htmldoc.Rmd)
    Error: processing vignette 'daiquiri.Rmd' failed with diagnostics:
    Argument 1 can't be empty.
    --- failed re-building ‘daiquiri.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘daiquiri.Rmd’
    
    Error: Vignette re-building failed.
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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
      [ FAIL 8 | WARN 3 | SKIP 12 | PASS 30 ]
      
      ══ Skipped tests (12) ══════════════════════════════════════════════════════════
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

## In both

*   checking re-building of vignette outputs ... WARNING
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

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘ggtree’
    ```

# did2s

<details>

* Version: 1.0.2
* GitHub: NA
* Source code: https://github.com/cran/did2s
* Date/Publication: 2023-04-07 15:50:02 UTC
* Number of recursive dependencies: 130

Run `revdepcheck::cloud_details(, "did2s")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(did2s)
      Loading required package: fixest
      did2s (v1.0.2). For more information on the methodology, visit <https://www.kylebutts.github.io/did2s>
      
      To cite did2s in publications use:
      
    ...
       12.       └─ggplot2::new_guide(...)
       13.         └─ggplot2:::validate_theme(params$theme)
       14.           └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
       15.             └─ggplot2 (local) `<fn>`(dots[[1L]][[1L]], dots[[2L]][[1L]], element_tree = `<named list>`)
       16.               └─cli::cli_abort(...)
       17.                 └─rlang::abort(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 11 ]
      Error: Test failures
      Execution halted
    ```

# diffudist

<details>

* Version: 1.0.1
* GitHub: https://github.com/gbertagnolli/diffudist
* Source code: https://github.com/cran/diffudist
* Date/Publication: 2023-02-27 19:42:40 UTC
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "diffudist")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘diffudist-package.Rmd’ using rmarkdown
    
    Quitting from lines 186-196 [avg-ddms] (diffudist-package.Rmd)
    Error: processing vignette 'diffudist-package.Rmd' failed with diagnostics:
    Argument 1 can't be empty.
    --- failed re-building ‘diffudist-package.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘diffudist-package.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        libs   4.1Mb
    ```

# digiRhythm

<details>

* Version: 1.1
* GitHub: NA
* Source code: https://github.com/cran/digiRhythm
* Date/Publication: 2022-11-15 22:20:02 UTC
* Number of recursive dependencies: 100

Run `revdepcheck::cloud_details(, "digiRhythm")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘digiRhythm-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: diurnality
    > ### Title: Computes the diurnality index based on an activity dataframe
    > ### Aliases: diurnality
    > 
    > ### ** Examples
    > 
    > data("df516b_2", package = "digiRhythm")
    ...
    ! Argument 1 can't be empty.
    Backtrace:
        ▆
     1. ├─digiRhythm::diurnality(data, activity)
     2. │ └─ggplot2::theme(...)
     3. │   └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
     4. │     ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
     5. │     └─rlang::list2(..., ... = NULL)
     6. └─rlang::abort(message = message)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Actgram_diurnality_avg_activity.Rmd’ using rmarkdown
    
    Quitting from lines 76-79 [diurnality] (Actgram_diurnality_avg_activity.Rmd)
    Error: processing vignette 'Actgram_diurnality_avg_activity.Rmd' failed with diagnostics:
    Argument 1 can't be empty.
    --- failed re-building ‘Actgram_diurnality_avg_activity.Rmd’
    
    --- re-building ‘DFC_and_HP_and_changing_plots.Rmd’ using rmarkdown
    --- finished re-building ‘DFC_and_HP_and_changing_plots.Rmd’
    ...
    --- finished re-building ‘Loading_and_preprocessing_data_in_DigiRhythm.Rmd’
    
    --- re-building ‘Visualisation.Rmd’ using rmarkdown
    --- finished re-building ‘Visualisation.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Actgram_diurnality_avg_activity.Rmd’
    
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
    > dots_and_violin(
    +   fabaceae_clade_n_df, "clade", "label_count", "parsed_n", 2,
    +   30, "Chromosome haploid number", desiredorder1, 1, .85, 4,
    +   "ownwork",
    +   violin = FALSE
    + )
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Calls: dots_and_violin ... mapply -> <Anonymous> -> scale_y_continuous -> startsWith
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

# dotwhisker

<details>

* Version: 0.7.4
* GitHub: https://github.com/fsolt/dotwhisker
* Source code: https://github.com/cran/dotwhisker
* Date/Publication: 2021-09-02 14:50:35 UTC
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "dotwhisker")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘dotwhisker-vignette.Rmd’ using rmarkdown
    
    Quitting from lines 350-398 [distribution] (dotwhisker-vignette.Rmd)
    Error: processing vignette 'dotwhisker-vignette.Rmd' failed with diagnostics:
    cannot coerce type 'closure' to vector of type 'character'
    --- failed re-building ‘dotwhisker-vignette.Rmd’
    
    --- re-building ‘kl2007_examples.Rmd’ using rmarkdown
    --- finished re-building ‘kl2007_examples.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘dotwhisker-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Unknown package ‘broomExtra’ in Rd xrefs
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
        6.       └─ggplot2::new_guide(...)
        7.         └─ggplot2:::validate_theme(params$theme)
        8.           └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
        9.             └─ggplot2 (local) `<fn>`(dots[[1L]][[4L]], dots[[2L]][[4L]], element_tree = `<named list>`)
       10.               └─cli::cli_abort(...)
       11.                 └─rlang::abort(...)
      
      [ FAIL 4 | WARN 21 | SKIP 3 | PASS 185 ]
      Error: Test failures
      Execution halted
    ```

## In both

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

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘dynamAedes_local.Rmd’ using rmarkdown
    starting worker pid=6507 on localhost:11243 at 10:12:31.272
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

*   checking re-building of vignette outputs ... WARNING
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

# ecochange

<details>

* Version: 2.9.3.1
* GitHub: NA
* Source code: https://github.com/cran/ecochange
* Date/Publication: 2023-03-02 20:10:02 UTC
* Number of recursive dependencies: 93

Run `revdepcheck::cloud_details(, "ecochange")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ecochange-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: EBVstats
    > ### Title: EBV Stats
    > ### Aliases: EBVstats
    > 
    > ### ** Examples
    > 
    > ## RasterBrick of structural Essential Biodiversity Variables
    ...
    ! Argument 1 can't be empty.
    Backtrace:
        ▆
     1. ├─ecochange::plot.EBVstats(...)
     2. │ └─ggplot2::theme(...)
     3. │   └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
     4. │     ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
     5. │     └─rlang::list2(..., ... = NULL)
     6. └─rlang::abort(message = message)
    Execution halted
    ```

# ecostats

<details>

* Version: 1.1.11
* GitHub: NA
* Source code: https://github.com/cran/ecostats
* Date/Publication: 2022-08-24 06:50:02 UTC
* Number of recursive dependencies: 223

Run `revdepcheck::cloud_details(, "ecostats")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ecostats-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Myrtaceae
    > ### Title: Species richness of _Myrtaceae_ plants
    > ### Aliases: Myrtaceae
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    ...
     1. ├─ggplot2::guides(...)
     2. │ └─rlang::list2(...)
     3. └─ggplot2::guide_colorbar(...)
     4.   └─ggplot2::new_guide(...)
     5.     └─ggplot2:::validate_theme(params$theme)
     6.       └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
     7.         └─ggplot2 (local) `<fn>`(dots[[1L]][[3L]], dots[[2L]][[3L]], element_tree = `<named list>`)
     8.           └─cli::cli_abort(...)
     9.             └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        doc    3.5Mb
        help   1.3Mb
    ```

# eiCompare

<details>

* Version: 3.0.4
* GitHub: https://github.com/RPVote/eiCompare
* Source code: https://github.com/cran/eiCompare
* Date/Publication: 2023-08-31 13:30:02 UTC
* Number of recursive dependencies: 147

Run `revdepcheck::cloud_details(, "eiCompare")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘bisg.Rmd’ using rmarkdown
    --- finished re-building ‘bisg.Rmd’
    
    --- re-building ‘ei.Rmd’ using rmarkdown
    --- finished re-building ‘ei.Rmd’
    
    --- re-building ‘parallel_processing.Rmd’ using rmarkdown
    --- finished re-building ‘parallel_processing.Rmd’
    ...
    Quitting from lines 58-79 [cvap_map] (performance_analysis.Rmd)
    Error: processing vignette 'performance_analysis.Rmd' failed with diagnostics:
    The `legend.title` theme element must be a <element_text> object.
    --- failed re-building ‘performance_analysis.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘performance_analysis.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# elfgen

<details>

* Version: 2.3.3
* GitHub: https://github.com/HARPgroup/elfgen
* Source code: https://github.com/cran/elfgen
* Date/Publication: 2022-08-23 09:10:02 UTC
* Number of recursive dependencies: 126

Run `revdepcheck::cloud_details(, "elfgen")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(elfgen)
      > 
      > test_check("elfgen")
      [ FAIL 4 | WARN 0 | SKIP 0 | PASS 14 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
        8.     └─ggplot2::new_guide(...)
        9.       └─ggplot2:::validate_theme(params$theme)
       10.         └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
       11.           └─ggplot2 (local) `<fn>`(dots[[1L]][[1L]], dots[[2L]][[1L]], element_tree = `<named list>`)
       12.             └─cli::cli_abort(...)
       13.               └─rlang::abort(...)
      
      [ FAIL 4 | WARN 0 | SKIP 0 | PASS 14 ]
      Error: Test failures
      Execution halted
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
    
    > ### Name: consistency_check
    > ### Title: Consistency Check Function
    > ### Aliases: consistency_check
    > 
    > ### ** Examples
    > 
    > # check the consistency between malaria tests and cases
    ...
    ! Argument 1 can't be empty.
    Backtrace:
        ▆
     1. ├─epiCleanr::consistency_check(...)
     2. │ └─ggplot2::theme(...)
     3. │   └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
     4. │     ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
     5. │     └─rlang::list2(..., ... = NULL)
     6. └─rlang::abort(message = message)
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

# exuber

<details>

* Version: 1.0.2
* GitHub: https://github.com/kvasilopoulos/exuber
* Source code: https://github.com/cran/exuber
* Date/Publication: 2023-03-22 23:10:02 UTC
* Number of recursive dependencies: 102

Run `revdepcheck::cloud_details(, "exuber")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘exuber-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: sim_blan
    > ### Title: Simulation of a Blanchard (1979) bubble process
    > ### Aliases: sim_blan
    > 
    > ### ** Examples
    > 
    > sim_blan(n = 100, seed = 123) %>%
    ...
     1. ├─sim_blan(n = 100, seed = 123) %>% autoplot()
     2. ├─ggplot2::autoplot(.)
     3. ├─exuber:::autoplot.sim(.)
     4. │ └─exuber::theme_exuber()
     5. │   └─ggplot2::theme(...)
     6. │     └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
     7. │       ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
     8. │       └─rlang::list2(..., ... = NULL)
     9. └─rlang::abort(message = message)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(exuber)
      > 
      > test_check("exuber")
      [ FAIL 18 | WARN 0 | SKIP 4 | PASS 211 ]
      
    ...
        8. │ └─exuber::theme_exuber()
        9. │   └─ggplot2::theme(...)
       10. │     └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
       11. │       ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
       12. │       └─rlang::list2(..., ... = NULL)
       13. └─rlang::abort(message = message)
      
      [ FAIL 18 | WARN 0 | SKIP 4 | PASS 211 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘exuber.Rmd’ using rmarkdown
    
    Quitting from lines 73-74 [plot-radf] (exuber.Rmd)
    Error: processing vignette 'exuber.Rmd' failed with diagnostics:
    Argument 1 can't be empty.
    --- failed re-building ‘exuber.Rmd’
    
    --- re-building ‘plotting.Rmd’ using rmarkdown
    
    ...
    ℹ With name: sim_psy1.
    Caused by error in `list2()`:
    ! Argument 1 can't be empty.
    --- failed re-building ‘simulation.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘exuber.Rmd’ ‘plotting.Rmd’ ‘simulation.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        libs   4.4Mb
    ```

# ezEDA

<details>

* Version: 0.1.1
* GitHub: https://github.com/kviswana/ezEDA
* Source code: https://github.com/cran/ezEDA
* Date/Publication: 2021-06-29 04:40:10 UTC
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "ezEDA")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ezEDA-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: multi_measures_relationship
    > ### Title: Plot the relationship between many measures
    > ### Aliases: multi_measures_relationship
    > 
    > ### ** Examples
    > 
    > multi_measures_relationship(ggplot2::mpg, hwy, displ)
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Calls: <Anonymous> ... do.call -> <Anonymous> -> scale_y_continuous -> startsWith
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ezEDA.Rmd’ using rmarkdown
    
    Quitting from lines 182-185 [multi_measures_relationship] (ezEDA.Rmd)
    Error: processing vignette 'ezEDA.Rmd' failed with diagnostics:
    cannot coerce type 'closure' to vector of type 'character'
    --- failed re-building ‘ezEDA.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ezEDA.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(fairmodels)
      > 
      > 
      > test_check("fairmodels")
      Welcome to DALEX (version: 2.4.3).
      Find examples and detailed introduction at: http://ema.drwhy.ai/
    ...
       1. ├─fairmodels::plot_density(fobject) at test_plot_density.R:11:3
       2. │ └─ggplot2::theme(...)
       3. │   └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
       4. │     ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
       5. │     └─rlang::list2(..., ... = NULL)
       6. └─rlang::abort(message = message)
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 309 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
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
    Quitting from lines 107-108 [unnamed-chunk-8] (Basic_tutorial.Rmd)
    Error: processing vignette 'Basic_tutorial.Rmd' failed with diagnostics:
    Argument 1 can't be empty.
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

*   checking re-building of vignette outputs ... WARNING
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

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘scales’
      All declared Imports should be used.
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
    
    > ### Name: explore_trace_interp
    > ### Title: Plot the trace the search progression
    > ### Aliases: explore_trace_interp
    > 
    > ### ** Examples
    > 
    > # Compare the trace of interpolated points in two algorithms
    ...
      2. ├─ferrn::explore_trace_interp(., interp_size = 2)
      3. │ └─ferrn::theme_fern()
      4. │   ├─ggplot2::theme_bw() %+replace% ...
      5. │   │ └─ggplot2::is.theme(e2)
      6. │   └─ggplot2::theme(...)
      7. │     └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
      8. │       ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
      9. │       └─rlang::list2(..., ... = NULL)
     10. └─rlang::abort(message = message)
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

# finalfit

<details>

* Version: 1.0.7
* GitHub: https://github.com/ewenharrison/finalfit
* Source code: https://github.com/cran/finalfit
* Date/Publication: 2023-11-16 17:40:02 UTC
* Number of recursive dependencies: 172

Run `revdepcheck::cloud_details(, "finalfit")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘finalfit-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: boot_predict
    > ### Title: Bootstrap simulation for model prediction
    > ### Aliases: boot_predict
    > 
    > ### ** Examples
    > 
    > library(finalfit)
    ...
     21. └─vctrs (local) `<fn>`()
     22.   └─vctrs::vec_default_ptype2(...)
     23.     ├─base::withRestarts(...)
     24.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     25.     │   └─base (local) doWithOneRestart(return(expr), restart)
     26.     └─vctrs::stop_incompatible_type(...)
     27.       └─vctrs:::stop_incompatible(...)
     28.         └─vctrs:::stop_vctrs(...)
     29.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(finalfit)
      > 
      > test_check("finalfit")
      [ FAIL 4 | WARN 16 | SKIP 0 | PASS 127 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. └─finalfit::ff_plot(colon_s, "Surv(time, status)", "age.factor")
       5.   ├─base::do.call(hr_plot, args)
       6.   └─finalfit (local) `<fn>`(`<df[,32]>`, "Surv(time, status)", "age.factor")
       7.     └─ggplot2::scale_x_continuous(trans = "log10", breaks = breaks)
       8.       └─base::startsWith(as.character(call[[1]]), "scale_")
      
      [ FAIL 4 | WARN 16 | SKIP 0 | PASS 127 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘all_plots_examples.Rmd’ using rmarkdown
    
    Quitting from lines 337-343 [unnamed-chunk-22] (all_plots_examples.Rmd)
    Error: processing vignette 'all_plots_examples.Rmd' failed with diagnostics:
    cannot coerce type 'closure' to vector of type 'character'
    --- failed re-building ‘all_plots_examples.Rmd’
    
    --- re-building ‘all_tables_examples.Rmd’ using rmarkdown
    --- finished re-building ‘all_tables_examples.Rmd’
    ...
    Quitting from lines 153-158 [unnamed-chunk-8] (survival.Rmd)
    Error: processing vignette 'survival.Rmd' failed with diagnostics:
    cannot coerce type 'closure' to vector of type 'character'
    --- failed re-building ‘survival.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘all_plots_examples.Rmd’ ‘survival.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        doc   4.9Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyselect’
      All declared Imports should be used.
    ```

# finalsize

<details>

* Version: 0.2.0
* GitHub: https://github.com/epiverse-trace/finalsize
* Source code: https://github.com/cran/finalsize
* Date/Publication: 2023-05-09 23:10:02 UTC
* Number of recursive dependencies: 116

Run `revdepcheck::cloud_details(, "finalsize")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘compare_sir_model.Rmd’ using rmarkdown
    --- finished re-building ‘compare_sir_model.Rmd’
    
    --- re-building ‘finalsize.Rmd’ using rmarkdown
    --- finished re-building ‘finalsize.Rmd’
    
    --- re-building ‘susceptibility_matrices.Rmd’ using rmarkdown
    --- finished re-building ‘susceptibility_matrices.Rmd’
    
    ...
    Quitting from lines 351-432 [unnamed-chunk-16] (varying_susceptibility.Rmd)
    Error: processing vignette 'varying_susceptibility.Rmd' failed with diagnostics:
    The `legend.title` theme element must be a <element_text> object.
    --- failed re-building ‘varying_susceptibility.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘varying_susceptibility.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.1Mb
      sub-directories of 1Mb or more:
        libs   9.6Mb
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

*   checking re-building of vignette outputs ... WARNING
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
      installed size is 11.4Mb
      sub-directories of 1Mb or more:
        doc    9.1Mb
        libs   1.5Mb
    ```

# fmeffects

<details>

* Version: 0.1.1
* GitHub: https://github.com/holgstr/fmeffects
* Source code: https://github.com/cran/fmeffects
* Date/Publication: 2023-09-26 15:10:02 UTC
* Number of recursive dependencies: 157

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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

# foreSIGHT

<details>

* Version: 1.2.0
* GitHub: https://github.com/ClimateAnalytics/foreSIGHT
* Source code: https://github.com/cran/foreSIGHT
* Date/Publication: 2023-10-19 07:00:08 UTC
* Number of recursive dependencies: 95

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
      5.     │   └─ggplot2::ggproto(...)
      6.     │     └─rlang::list2(...)
      7.     └─ggplot2::guide_colorbar(...)
      8.       └─ggplot2::new_guide(...)
      9.         └─ggplot2:::validate_theme(params$theme)
     10.           └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
     11.             └─ggplot2 (local) `<fn>`(dots[[1L]][[3L]], dots[[2L]][[3L]], element_tree = `<named list>`)
     12.               └─cli::cli_abort(...)
     13.                 └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Vignette_QuickStart_simpleScal.Rmd’ using rmarkdown_notangle
    
    Quitting from lines 332-338 [plotP] (Vignette_QuickStart_simpleScal.Rmd)
    Error: processing vignette 'Vignette_QuickStart_simpleScal.Rmd' failed with diagnostics:
    The `legend.title` theme element must be a <element_text> object.
    --- failed re-building ‘Vignette_QuickStart_simpleScal.Rmd’
    
    --- re-building ‘Vignette_Tutorial.Rmd’ using rmarkdown_notangle
    ...
    Quitting from lines 1505-1513 [useCaseD2] (Vignette_Tutorial.Rmd)
    Error: processing vignette 'Vignette_Tutorial.Rmd' failed with diagnostics:
    The `legend.title` theme element must be a <element_text> object.
    --- failed re-building ‘Vignette_Tutorial.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘Vignette_QuickStart_simpleScal.Rmd’ ‘Vignette_Tutorial.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.6Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        doc    1.3Mb
        libs   2.0Mb
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

# gMOIP

<details>

* Version: 1.5.0
* GitHub: https://github.com/relund/gMOIP
* Source code: https://github.com/cran/gMOIP
* Date/Publication: 2023-05-26 11:20:02 UTC
* Number of recursive dependencies: 109

Run `revdepcheck::cloud_details(, "gMOIP")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘bi-objective_2x.Rmd’ using rmarkdown
    Warning: ggrepel: 70 unlabeled data points (too many overlaps). Consider increasing max.overlaps
    Warning: ggrepel: 70 unlabeled data points (too many overlaps). Consider increasing max.overlaps
    
    Quitting from lines 118-126 [2DMILPMax] (bi-objective_2x.Rmd)
    Error: processing vignette 'bi-objective_2x.Rmd' failed with diagnostics:
    Can't combine <character> and <logical>.
    --- failed re-building ‘bi-objective_2x.Rmd’
    
    ...
    --- finished re-building ‘polytope_3d_ex1.Rmd’
    
    --- re-building ‘tri-objective.Rmd’ using rmarkdown
    --- finished re-building ‘tri-objective.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘bi-objective_2x.Rmd’ ‘bi-objective_3x_ex1.Rmd’ ‘intro.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# gapfill

<details>

* Version: 0.9.6-1
* GitHub: https://github.com/florafauna/gapfill
* Source code: https://github.com/cran/gapfill
* Date/Publication: 2021-02-12 10:10:05 UTC
* Number of recursive dependencies: 71

Run `revdepcheck::cloud_details(, "gapfill")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gapfill-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Image
    > ### Title: Image Panels
    > ### Aliases: Image
    > 
    > ### ** Examples
    > 
    > library("abind")
    ...
      2.   ├─ggplot2::guides(...)
      3.   │ └─rlang::list2(...)
      4.   └─ggplot2::guide_colorbar(...)
      5.     └─ggplot2::new_guide(...)
      6.       └─ggplot2:::validate_theme(params$theme)
      7.         └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
      8.           └─ggplot2 (local) `<fn>`(dots[[1L]][[3L]], dots[[2L]][[3L]], element_tree = `<named list>`)
      9.             └─cli::cli_abort(...)
     10.               └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘run-all.R’
    Running the tests in ‘tests/run-all.R’ failed.
    Complete output:
      > library(testthat)
      > test_check('gapfill')
      Loading required package: gapfill
      Loading required package: ggplot2
      --> See ?Gapfill and https://doi.org/10.1109/TGRS.2017.2785240 <--
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 867 ]
      
    ...
        8.     └─ggplot2::new_guide(...)
        9.       └─ggplot2:::validate_theme(params$theme)
       10.         └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
       11.           └─ggplot2 (local) `<fn>`(dots[[1L]][[3L]], dots[[2L]][[3L]], element_tree = `<named list>`)
       12.             └─cli::cli_abort(...)
       13.               └─rlang::abort(...)
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 867 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      'raster', 'doParallel', 'doMPI'
    ```

# geneHapR

<details>

* Version: 1.1.9
* GitHub: NA
* Source code: https://github.com/cran/geneHapR
* Date/Publication: 2023-04-09 18:00:02 UTC
* Number of recursive dependencies: 163

Run `revdepcheck::cloud_details(, "geneHapR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘geneHapR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotHapTable
    > ### Title: plotHapTable
    > ### Aliases: plotHapTable
    > 
    > ### ** Examples
    > 
    > data("geneHapR_test")
    ...
      2.   ├─ggplot2::guides(...)
      3.   │ └─rlang::list2(...)
      4.   └─ggplot2::guide_colorbar(title.position = "top", title.hjust = 0.5)
      5.     └─ggplot2::new_guide(...)
      6.       └─ggplot2:::validate_theme(params$theme)
      7.         └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
      8.           └─ggplot2 (local) `<fn>`(dots[[1L]][[1L]], dots[[2L]][[1L]], element_tree = `<named list>`)
      9.             └─cli::cli_abort(...)
     10.               └─rlang::abort(...)
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
        7.     └─ggplot2::new_guide(...)
        8.       └─ggplot2:::validate_theme(params$theme)
        9.         └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
       10.           └─ggplot2 (local) `<fn>`(dots[[1L]][[1L]], dots[[2L]][[1L]], element_tree = `<named list>`)
       11.             └─cli::cli_abort(...)
       12.               └─rlang::abort(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘maptools’
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

# ggVennDiagram

<details>

* Version: 1.2.3
* GitHub: https://github.com/gaospecial/ggVennDiagram
* Source code: https://github.com/cran/ggVennDiagram
* Date/Publication: 2023-08-14 11:20:13 UTC
* Number of recursive dependencies: 108

Run `revdepcheck::cloud_details(, "ggVennDiagram")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘fully-customed.Rmd’ using rmarkdown
    
    Quitting from lines 102-126 [unnamed-chunk-6] (fully-customed.Rmd)
    Error: processing vignette 'fully-customed.Rmd' failed with diagnostics:
    unused argument (list("count", "name", "text", "x", "y"))
    --- failed re-building ‘fully-customed.Rmd’
    
    --- re-building ‘using-ggVennDiagram.Rmd’ using rmarkdown
    
    ...
    --- finished re-building ‘using-new-shapes.Rmd’
    
    --- re-building ‘venn-plot-with-more-than-four-sets.Rmd’ using rmarkdown
    --- finished re-building ‘venn-plot-with-more-than-four-sets.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘fully-customed.Rmd’ ‘using-ggVennDiagram.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.3Mb
      sub-directories of 1Mb or more:
        doc   8.3Mb
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

*   checking examples ... ERROR
    ```
    Running examples in ‘ggblanket-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: dark_mode
    > ### Title: Dark theme for a ggplot
    > ### Aliases: dark_mode
    > 
    > ### ** Examples
    > 
    > library(palmerpenguins)
    ...
      2.   ├─ggplot2::guides(...)
      3.   │ └─rlang::list2(...)
      4.   └─ggplot2::guide_legend(...)
      5.     └─ggplot2::new_guide(...)
      6.       └─ggplot2:::validate_theme(params$theme)
      7.         └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
      8.           └─ggplot2 (local) `<fn>`(dots[[1L]][[1L]], dots[[2L]][[1L]], element_tree = `<named list>`)
      9.             └─cli::cli_abort(...)
     10.               └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ggbrain_aesthetics.Rmd’ using rmarkdown
    --- finished re-building ‘ggbrain_aesthetics.Rmd’
    
    --- re-building ‘ggbrain_introduction.Rmd’ using rmarkdown
    
    Quitting from lines 238-239 [unnamed-chunk-16] (ggbrain_introduction.Rmd)
    Error: processing vignette 'ggbrain_introduction.Rmd' failed with diagnostics:
    comparison (1) is possible only for atomic and list types
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
      installed size is 11.7Mb
      sub-directories of 1Mb or more:
        doc       3.0Mb
        extdata   1.6Mb
        libs      6.5Mb
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        doc   2.3Mb
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

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggeasy)
      > 
      > test_check("ggeasy")
      [ FAIL 1 | WARN 0 | SKIP 4 | PASS 503 ]
      
      ══ Skipped tests (4) ═══════════════════════════════════════════════════════════
    ...
          ▆
       1. ├─ggplot2::theme(axis.ticks.x = ggplot2::element_blank(), ) at test-remove-axis.R:49:3
       2. │ └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
       3. │   ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
       4. │   └─rlang::list2(..., ... = NULL)
       5. └─rlang::abort(message = message)
      
      [ FAIL 1 | WARN 0 | SKIP 4 | PASS 503 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘introduction-to-ggfittext.Rmd’ using rmarkdown
    
    Quitting from lines 140-146 [unnamed-chunk-10] (introduction-to-ggfittext.Rmd)
    Error: processing vignette 'introduction-to-ggfittext.Rmd' failed with diagnostics:
    Problem while converting geom to grob.
    ℹ Error occurred in the 2nd layer.
    Caused by error in `x == -Inf`:
    ! comparison (1) is possible only for atomic and list types
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
      installed size is 28.8Mb
      sub-directories of 1Mb or more:
        help   1.4Mb
        libs  26.4Mb
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

# ggh4x

<details>

* Version: 0.2.6
* GitHub: https://github.com/teunbrand/ggh4x
* Source code: https://github.com/cran/ggh4x
* Date/Publication: 2023-08-30 19:10:06 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "ggh4x")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggh4x-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: facetted_pos_scales
    > ### Title: Set individual scales in facets
    > ### Aliases: facetted_pos_scales
    > 
    > ### ** Examples
    > 
    > plot <- ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
    ...
    > # Reversing the y-axis in the second panel. When providing a list of scales,
    > # NULL indicates to use the default, global scale
    > plot +
    +   facetted_pos_scales(
    +     y = list(NULL, scale_y_continuous(trans = "reverse"))
    +   )
    Error in if (scale$trans$name %in% c("date", "time", "hms")) { : 
      argument is of length zero
    Calls: <Anonymous> ... <Anonymous> -> finish_data -> lapply -> FUN -> should_transform
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
      
      Attaching package: 'ggh4x'
      
      The following object is masked from 'package:ggplot2':
    ...
      `expected[[1]]`: 7 7 7
      
        `actual[[2]]`: 8 13 18
      `expected[[2]]`: 6 11 16
      
      [ FAIL 39 | WARN 88 | SKIP 20 | PASS 690 ]
      Deleting unused snapshots:
      • facet_manual/removable-whitespace.svg
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Facets.Rmd’ using rmarkdown
    
    Quitting from lines 347-358 [position_scales_list] (Facets.Rmd)
    Error: processing vignette 'Facets.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘Facets.Rmd’
    
    --- re-building ‘Miscellaneous.Rmd’ using rmarkdown
    
    ...
    Error: processing vignette 'ggh4x.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘ggh4x.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘Facets.Rmd’ ‘Miscellaneous.Rmd’ ‘PositionGuides.Rmd’
      ‘Statistics.Rmd’ ‘ggh4x.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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

*   checking re-building of vignette outputs ... WARNING
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
      installed size is 11.3Mb
      sub-directories of 1Mb or more:
        libs   8.8Mb
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

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘nlme’
      All declared Imports should be used.
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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
    Namespaces in Imports field not imported from:
      ‘productplots’ ‘scales’
      All declared Imports should be used.
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

# ggpath

<details>

* Version: 1.0.1
* GitHub: https://github.com/mrcaseb/ggpath
* Source code: https://github.com/cran/ggpath
* Date/Publication: 2023-01-29 22:40:05 UTC
* Number of recursive dependencies: 69

Run `revdepcheck::cloud_details(, "ggpath")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggpath-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: element_path
    > ### Title: Theme Element for Image Grobs
    > ### Aliases: element_path
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
    Error in `list2()`:
    ! Argument 1 can't be empty.
    Backtrace:
        ▆
     1. ├─ggplot2::theme(...)
     2. │ └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
     3. │   ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
     4. │   └─rlang::list2(..., ... = NULL)
     5. └─rlang::abort(message = message)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggpath)
      > 
      > test_check("ggpath")
      [ FAIL 1 | WARN 0 | SKIP 3 | PASS 3 ]
      
      ══ Skipped tests (3) ═══════════════════════════════════════════════════════════
    ...
      • geom_from_path/p1.svg
      • geom_lines/p1.svg
      • geom_lines/p2.svg
      • geom_lines/p3.svg
      • geom_lines/p4.svg
      • geom_lines/p5.svg
      • theme-elements/p1.svg
      • theme-elements/p2.svg
      Error: Test failures
      Execution halted
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
      7.         └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
      8.           └─ggplot2 (local) `<fn>`(dots[[1L]][[51L]], dots[[2L]][[51L]], element_tree = `<named list>`)
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
       22.                                   └─guide$train(guide_param, scale)
       23.                                     └─ggplot2 (local) train(..., self = self)
       24.                                       ├─ggplot2::guide_train(params, scale, aesthetic)
       25.                                       └─ggplot2:::guide_train.default(params, scale, aesthetic)
       26.                                         └─cli::cli_abort(...)
       27.                                           └─rlang::abort(...)
      Warning message:
      In if (msg != "") { :
        the condition has length > 1 and only the first element will be used
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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
    
    > ### Name: ggseqfplot
    > ### Title: Sequence Frequency Plot
    > ### Aliases: ggseqfplot
    > 
    > ### ** Examples
    > 
    > # Use example data from TraMineR: actcal data set
    ...
    +            ylabs = "share") +
    +   scale_x_discrete(breaks = 1:12,
    +                    labels = month.abb,
    +                    expand = expansion(add = c(0.2, 0)))
    Scale for x is already present.
    Adding another scale for x, which will replace the existing scale.
    Error in if (scale$trans$name %in% c("date", "time", "hms")) { : 
      argument is of length zero
    Calls: <Anonymous> ... <Anonymous> -> finish_data -> lapply -> FUN -> should_transform
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
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
    > 
    > p <-ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species, fill = Species)) +
    + geom_point()
    > 
    > #sidebar - uses StatCount
    > p +
    + geom_xsidebar() +
    + geom_ysidebar()
    Error: object of type 'closure' is not subsettable
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

*   checking re-building of vignette outputs ... WARNING
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
      [3] "/opt/R/4.1.1/lib/R/site-library"          
      [4] "/opt/R/4.1.1/lib/R/library"               
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking examples ... ERROR
    ```
    Running examples in ‘ggthemes-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: scale_colour_excel_new
    > ### Title: Excel (current versions) color scales
    > ### Aliases: scale_colour_excel_new scale_color_excel_new
    > ###   scale_fill_excel_new
    > 
    > ### ** Examples
    > 
    ...
    ! Argument 1 can't be empty.
    Backtrace:
        ▆
     1. ├─ggthemes::theme_excel_new()
     2. │ └─ggplot2::theme(...)
     3. │   └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
     4. │     ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
     5. │     └─rlang::list2(..., ... = NULL)
     6. └─rlang::abort(message = message)
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
    
    > ### Name: scale_x_mergelist
    > ### Title: Merge list columns into character vectors
    > ### Aliases: scale_x_mergelist
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
     19. │                 └─ggplot2 (local) FUN(X[[i]], ...)
     20. │                   └─base::lapply(df[aesthetics], scale$transformation$inverse)
     21. │                     └─base::match.fun(FUN)
     22. │                       └─base::stop(...)
     23. └─base::.handleSimpleError(...)
     24.   └─rlang (local) h(simpleError(msg, call))
     25.     └─handlers[[1L]](cnd)
     26.       └─cli::cli_abort(...)
     27.         └─rlang::abort(...)
    Execution halted
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

*   checking examples ... ERROR
    ```
    Running examples in ‘glancedata-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_numerical_vars
    > ### Title: Plot Continuous Variables
    > ### Aliases: plot_numerical_vars
    > 
    > ### ** Examples
    > 
    > library(glancedata)
    > 
    > plot_numerical_vars(iris, "pairwise")
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Calls: <Anonymous> ... do.call -> <Anonymous> -> scale_y_continuous -> startsWith
    Execution halted
    ```

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

# greatR

<details>

* Version: 1.0.0
* GitHub: https://github.com/ruthkr/greatR
* Source code: https://github.com/cran/greatR
* Date/Publication: 2023-07-19 13:50:02 UTC
* Number of recursive dependencies: 72

Run `revdepcheck::cloud_details(, "greatR")` for more info

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
        7.         └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
        8.           └─ggplot2 (local) `<fn>`(dots[[1L]][[1L]], dots[[2L]][[1L]], element_tree = `<named list>`)
        9.             └─cli::cli_abort(...)
       10.               └─rlang::abort(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 89 ]
      
      No one is perfect!
      Error: Failures detected.
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘data-requirement.Rmd’ using rmarkdown
    --- finished re-building ‘data-requirement.Rmd’
    
    --- re-building ‘register-data-manually.Rmd’ using rmarkdown
    --- finished re-building ‘register-data-manually.Rmd’
    
    --- re-building ‘register-data.Rmd’ using rmarkdown
    --- finished re-building ‘register-data.Rmd’
    ...
    Quitting from lines 101-106 [plot-dist-original] (visualise-results.Rmd)
    Error: processing vignette 'visualise-results.Rmd' failed with diagnostics:
    The `legend.text` theme element must be a <element_text> object.
    --- failed re-building ‘visualise-results.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘visualise-results.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# greed

<details>

* Version: 0.6.1
* GitHub: https://github.com/comeetie/greed
* Source code: https://github.com/cran/greed
* Date/Publication: 2022-10-03 22:00:05 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "greed")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(greed)
      > 
      > test_check("greed")
      [ FAIL 9 | WARN 3 | SKIP 0 | PASS 292 ]
      
    ...
       15.             └─ggplot2::new_guide(...)
       16.               └─ggplot2:::validate_theme(params$theme)
       17.                 └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
       18.                   └─ggplot2 (local) `<fn>`(dots[[1L]][[2L]], dots[[2L]][[2L]], element_tree = `<named list>`)
       19.                     └─cli::cli_abort(...)
       20.                       └─rlang::abort(...)
      
      [ FAIL 9 | WARN 3 | SKIP 0 | PASS 292 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 45.5Mb
      sub-directories of 1Mb or more:
        data   1.4Mb
        libs  42.3Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 989 marked UTF-8 strings
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

# gtExtras

<details>

* Version: 0.5.0
* GitHub: https://github.com/jthomasmock/gtExtras
* Source code: https://github.com/cran/gtExtras
* Date/Publication: 2023-09-15 22:32:06 UTC
* Number of recursive dependencies: 106

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
       16.           └─gt:::text_transform_at_location.cells_body(...)
       17.             └─gtExtras (local) fn(body[[col]][stub_df$rownum_i %in% loc$rows])
       18.               └─base::mapply(plot_fn_spark, trim, list_data_in, SIMPLIFY = FALSE)
       19.                 └─gtExtras (local) `<fn>`(dots[[1L]][[1L]], dots[[2L]][[1L]])
       20.                   └─ggplot2::scale_x_continuous(expand = expansion(mult = 0.05))
       21.                     └─base::startsWith(as.character(call[[1]]), "scale_")
      
      [ FAIL 3 | WARN 14 | SKIP 23 | PASS 105 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        help   4.7Mb
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

*   checking re-building of vignette outputs ... WARNING
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
      installed size is 19.8Mb
      sub-directories of 1Mb or more:
        data   6.8Mb
        doc    2.6Mb
        libs  10.2Mb
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

*   checking re-building of vignette outputs ... WARNING
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
      3. │ └─heatmaply:::heatmaply.default(...)
      4. │   ├─heatmaply::heatmaply(...)
      5. │   └─heatmaply:::heatmaply.heatmapr(...)
      6. │     └─heatmaply:::ggplot_heatmap(...)
      7. │       └─ggplot2::theme(...)
      8. │         └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
      9. │           ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
     10. │           └─rlang::list2(..., ... = NULL)
     11. └─rlang::abort(message = message)
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
       1. ├─heatmaply:::ggplot_heatmap(as.matrix(iris_plot)) at test_plots.R:72:3
       2. │ └─ggplot2::theme(...)
       3. │   └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
       4. │     ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
       5. │     └─rlang::list2(..., ... = NULL)
       6. └─rlang::abort(message = message)
      
      [ FAIL 57 | WARN 0 | SKIP 0 | PASS 169 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘heatmaply.Rmd’ using rmarkdown
    
    Quitting from lines 109-111 [unnamed-chunk-5] (heatmaply.Rmd)
    Error: processing vignette 'heatmaply.Rmd' failed with diagnostics:
    Argument 1 can't be empty.
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

# heatwaveR

<details>

* Version: 0.4.6
* GitHub: https://github.com/robwschlegel/heatwaveR
* Source code: https://github.com/cran/heatwaveR
* Date/Publication: 2021-10-27 14:50:02 UTC
* Number of recursive dependencies: 129

Run `revdepcheck::cloud_details(, "heatwaveR")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘MHW_to_nc.Rmd’ using rmarkdown
    --- finished re-building ‘MHW_to_nc.Rmd’
    
    --- re-building ‘OISST_preparation.Rmd’ using rmarkdown
    --- finished re-building ‘OISST_preparation.Rmd’
    
    --- re-building ‘complex_clims.Rmd’ using rmarkdown
    
    Quitting from lines 82-98 [visuals] (complex_clims.Rmd)
    ...
    --- finished re-building ‘exceedance.Rmd’
    
    --- re-building ‘gridded_event_detection.Rmd’ using rmarkdown
    --- finished re-building ‘gridded_event_detection.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘complex_clims.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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

*   checking re-building of vignette outputs ... WARNING
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
      installed size is  6.6Mb
      sub-directories of 1Mb or more:
        R      2.3Mb
        doc    1.6Mb
        libs   2.4Mb
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
       12.       └─ggplot2::new_guide(...)
       13.         └─ggplot2:::validate_theme(params$theme)
       14.           └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
       15.             └─ggplot2 (local) `<fn>`(dots[[1L]][[1L]], dots[[2L]][[1L]], element_tree = `<named list>`)
       16.               └─cli::cli_abort(...)
       17.                 └─rlang::abort(...)
      
      [ FAIL 6 | WARN 0 | SKIP 1 | PASS 74 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘hidecan-step-by-step.Rmd’ using rmarkdown
    
    Quitting from lines 168-174 [create-hidecan-plot] (hidecan-step-by-step.Rmd)
    Error: processing vignette 'hidecan-step-by-step.Rmd' failed with diagnostics:
    The `legend.title` theme element must be a <element_text> object.
    --- failed re-building ‘hidecan-step-by-step.Rmd’
    
    --- re-building ‘hidecan.Rmd’ using rmarkdown
    ...
    Quitting from lines 97-105 [hidecan-plot] (hidecan.Rmd)
    Error: processing vignette 'hidecan.Rmd' failed with diagnostics:
    The `legend.title` theme element must be a <element_text> object.
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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
* Number of recursive dependencies: 178

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

*   checking re-building of vignette outputs ... WARNING
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

# imputeTS

<details>

* Version: 3.3
* GitHub: https://github.com/SteffenMoritz/imputeTS
* Source code: https://github.com/cran/imputeTS
* Date/Publication: 2022-09-09 06:52:55 UTC
* Number of recursive dependencies: 112

Run `revdepcheck::cloud_details(, "imputeTS")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘imputeTS-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggplot_na_distribution2
    > ### Title: Stacked Barplot to Visualize Missing Values per Interval
    > ### Aliases: ggplot_na_distribution2
    > 
    > ### ** Examples
    > 
    > # Example 1: Visualize the missing values in tsNH4 time series as percentages
    ...
    ! Argument 1 can't be empty.
    Backtrace:
        ▆
     1. ├─imputeTS::ggplot_na_distribution2(tsNH4)
     2. │ └─ggplot2::theme(...)
     3. │   └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
     4. │     ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
     5. │     └─rlang::list2(..., ... = NULL)
     6. └─rlang::abort(message = message)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > test_check("imputeTS")
      Loading required package: imputeTS
      [ FAIL 2 | WARN 18 | SKIP 52 | PASS 427 ]
      
      ══ Skipped tests (52) ══════════════════════════════════════════════════════════
      • On CRAN (52): 'test-ggplot_na_distribution.R:59:3',
    ...
       4. ├─imputeTS::ggplot_na_gapsize(tsAirgap, legend = F)
       5. │ └─ggplot2::theme(legend.position = "none", )
       6. │   └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
       7. │     ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
       8. │     └─rlang::list2(..., ... = NULL)
       9. └─rlang::abort(message = message)
      
      [ FAIL 2 | WARN 18 | SKIP 52 | PASS 427 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Cheat_Sheet_imputeTS.pdf.asis’ using asis
    --- finished re-building ‘Cheat_Sheet_imputeTS.pdf.asis’
    
    --- re-building ‘imputeTS-Time-Series-Missing-Value-Imputation-in-R.ltx’ using tex
    Error: processing vignette 'imputeTS-Time-Series-Missing-Value-Imputation-in-R.ltx' failed with diagnostics:
    Running 'texi2dvi' on 'imputeTS-Time-Series-Missing-Value-Imputation-in-R.ltx' failed.
    LaTeX errors:
    ! LaTeX Error: File `etex.sty' not found.
    
    ...
    Error: processing vignette 'gallery_visualizations.Rmd' failed with diagnostics:
    Argument 1 can't be empty.
    --- failed re-building ‘gallery_visualizations.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘imputeTS-Time-Series-Missing-Value-Imputation-in-R.ltx’
      ‘gallery_visualizations.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        data   1.6Mb
        doc    1.1Mb
        libs   2.2Mb
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
      [ FAIL 7 | WARN 129 | SKIP 0 | PASS 978 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
       11.               └─base::startsWith(as.character(call[[1]]), "scale_")
      ── Failure ('test_subjectProfileSummaryPlot-general.R:25:2'): The plot is correctly facetted based on a variable ──
      `... <- NULL` produced warnings.
      ── Failure ('test_subjectProfileSummaryPlot-table.R:356:2'): The size of the points (in the legend) is correctly set ──
      gg$guides$colour$override.aes$size not equal to `pointSize`.
      target is NULL, current is numeric
      
      [ FAIL 7 | WARN 129 | SKIP 0 | PASS 978 ]
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

*   checking re-building of vignette outputs ... WARNING
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

# industRial

<details>

* Version: 0.1.0
* GitHub: https://github.com/J-Ramalho/industRial
* Source code: https://github.com/cran/industRial
* Date/Publication: 2021-06-11 09:40:02 UTC
* Number of recursive dependencies: 187

Run `revdepcheck::cloud_details(, "industRial")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘industRial-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: theme_qcc
    > ### Title: Custom theme "qcc" for the book industRial Data Science plots
    > ### Aliases: theme_qcc
    > 
    > ### ** Examples
    > 
    > library(dplyr)
    ...
        ▆
     1. ├─industRial::theme_qcc()
     2. │ ├─... %+replace% ...
     3. │ │ └─ggplot2::is.theme(e2)
     4. │ └─ggplot2::theme(...)
     5. │   └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
     6. │     ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
     7. │     └─rlang::list2(..., ... = NULL)
     8. └─rlang::abort(message = message)
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
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

*   checking re-building of vignette outputs ... WARNING
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
    1   0.248
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
      installed size is 28.4Mb
      sub-directories of 1Mb or more:
        libs  27.4Mb
    ```

# injurytools

<details>

* Version: 1.0.3
* GitHub: https://github.com/lzumeta/injurytools
* Source code: https://github.com/cran/injurytools
* Date/Publication: 2023-11-14 17:20:05 UTC
* Number of recursive dependencies: 155

Run `revdepcheck::cloud_details(, "injurytools")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘estimate-epi-measures.Rmd’ using rmarkdown
    --- finished re-building ‘estimate-epi-measures.Rmd’
    
    --- re-building ‘model-injury-data-i.Rmd’ using rmarkdown
    --- finished re-building ‘model-injury-data-i.Rmd’
    
    --- re-building ‘model-injury-data-ii.Rmd’ using rmarkdown
    
    ...
    --- finished re-building ‘prepare-injury-data.Rmd’
    
    --- re-building ‘visualize-injury-data.Rmd’ using rmarkdown
    --- finished re-building ‘visualize-injury-data.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘model-injury-data-ii.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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

# interactions

<details>

* Version: 1.1.5
* GitHub: https://github.com/jacob-long/interactions
* Source code: https://github.com/cran/interactions
* Date/Publication: 2021-07-02 07:00:04 UTC
* Number of recursive dependencies: 100

Run `revdepcheck::cloud_details(, "interactions")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘interactions-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: johnson_neyman
    > ### Title: Calculate Johnson-Neyman intervals for 2-way interactions
    > ### Aliases: johnson_neyman
    > 
    > ### ** Examples
    > 
    > # Using a fitted lm model
    ...
    > fit <- lm(Income ~ HSGrad + Murder*Illiteracy,
    +   data = states)
    > johnson_neyman(model = fit, pred = Murder,
    +   modx = Illiteracy)
    Error in `list2()`:
    ! Argument 1 can't be empty.
    Backtrace:
     1. interactions::johnson_neyman(model = fit, pred = Murder, modx = Illiteracy)
     7. rlang::abort(message = message)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘categorical.Rmd’ using rmarkdown
    --- finished re-building ‘categorical.Rmd’
    
    --- re-building ‘interactions.Rmd’ using rmarkdown
    
    Quitting from lines 326-327 [unnamed-chunk-23] (interactions.Rmd)
    Error: processing vignette 'interactions.Rmd' failed with diagnostics:
    Argument 1 can't be empty.
    --- failed re-building ‘interactions.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘interactions.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(vdiffr)
      > library(interactions)
      > 
      > test_check("interactions")
      `geom_smooth()` using formula = 'y ~ x'
      Failed with error:  'there is no package called 'broom.mixed''
    ...
      • interact_plot/plmlabelsc2.svg
      • interact_plot/plmlabelscpred.svg
      • interact_plot/plmlabelscs.svg
      • interact_plot/plmlinearchnp.svg
      • interact_plot/plmm.svg
      • interact_plot/plmruglb.svg
      • interact_plot/prsacont.svg
      • interact_plot/psvy1.svg
      Error: Test failures
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      'brms', 'rstanarm'
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘quantreg’, ‘brms’, ‘effects’, ‘Hmisc’, ‘rockchalk’, ‘pequod’
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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
# ipsRdbs

<details>

* Version: 0.2.6
* GitHub: https://github.com/sujit-sahu/ipsRdbs
* Source code: https://github.com/cran/ipsRdbs
* Date/Publication: 2023-07-05 13:40:02 UTC
* Number of recursive dependencies: 104

Run `revdepcheck::cloud_details(, "ipsRdbs")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ipsRdbs-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: cheese
    > ### Title: Testing of cheese data set
    > ### Aliases: cheese
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    ...
     Median : 5.329  
     Mean   : 5.942  
     3rd Qu.: 7.575  
     Max.   :10.199  
    >  pairs(cheese)
    >  GGally::ggpairs(data=cheese)
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Calls: <Anonymous> ... do.call -> <Anonymous> -> scale_y_continuous -> startsWith
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

*   checking installed package size ... NOTE
    ```
      installed size is 27.7Mb
      sub-directories of 1Mb or more:
        libs  25.9Mb
    ```

# isotracer

<details>

* Version: 1.1.5
* GitHub: NA
* Source code: https://github.com/cran/isotracer
* Date/Publication: 2023-09-21 21:10:06 UTC
* Number of recursive dependencies: 158

Run `revdepcheck::cloud_details(, "isotracer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘isotracer-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggtopo
    > ### Title: Plot a topology
    > ### Aliases: ggtopo
    > 
    > ### ** Examples
    > 
    > if (requireNamespace("ggraph")) {
    ...
     23. └─vctrs (local) `<fn>`()
     24.   └─vctrs::vec_default_ptype2(...)
     25.     ├─base::withRestarts(...)
     26.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     27.     │   └─base (local) doWithOneRestart(return(expr), restart)
     28.     └─vctrs::stop_incompatible_type(...)
     29.       └─vctrs:::stop_incompatible(...)
     30.         └─vctrs:::stop_vctrs(...)
     31.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(isotracer)
      To automatically run isotracer in parallel on a multicore CPU, you can call:
        options(mc.cores = parallel::detectCores())
      
      
      Attaching package: 'isotracer'
    ...
       40.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
       41.     │   └─base (local) doWithOneRestart(return(expr), restart)
       42.     └─vctrs::stop_incompatible_type(...)
       43.       └─vctrs:::stop_incompatible(...)
       44.         └─vctrs:::stop_vctrs(...)
       45.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 290 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 101.7Mb
      sub-directories of 1Mb or more:
        data   2.1Mb
        doc    2.3Mb
        libs  95.8Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rstantools’
      All declared Imports should be used.
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# ivDiag

<details>

* Version: 1.0.6
* GitHub: NA
* Source code: https://github.com/cran/ivDiag
* Date/Publication: 2023-09-17 06:00:02 UTC
* Number of recursive dependencies: 89

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

# junctions

<details>

* Version: 2.0.3
* GitHub: https://github.com/thijsjanzen/junctions
* Source code: https://github.com/cran/junctions
* Date/Publication: 2022-02-24 12:30:02 UTC
* Number of recursive dependencies: 75

Run `revdepcheck::cloud_details(, "junctions")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(junctions)
      Loading required package: RcppParallel
      > 
      > test_check("junctions")
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 124 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-unphased.R:176:3'): unphased, junctions ──────────────────────
      `obs_j` not equal to `exp_j`.
      1/1 mismatches
      [1] 8.3 - 9.99 == -1.69
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 124 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        libs   5.3Mb
    ```

# khroma

<details>

* Version: 1.11.0
* GitHub: https://github.com/tesselle/khroma
* Source code: https://github.com/cran/khroma
* Date/Publication: 2023-08-21 13:20:12 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::cloud_details(, "khroma")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘khroma-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: scale_logical_discrete
    > ### Title: Binary Discrete Color Schemes for 'ggplot2' and 'ggraph'
    > ### Aliases: scale_logical_discrete scale_colour_logical
    > ###   scale_color_logical scale_fill_logical
    > 
    > ### ** Examples
    > 
    ...
     21. └─vctrs (local) `<fn>`()
     22.   └─vctrs::vec_default_ptype2(...)
     23.     ├─base::withRestarts(...)
     24.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     25.     │   └─base (local) doWithOneRestart(return(expr), restart)
     26.     └─vctrs::stop_incompatible_type(...)
     27.       └─vctrs:::stop_incompatible(...)
     28.         └─vctrs:::stop_vctrs(...)
     29.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        doc   3.3Mb
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
      3. │ └─heatmaply:::heatmaply.default(...)
      4. │   ├─heatmaply::heatmaply(...)
      5. │   └─heatmaply:::heatmaply.heatmapr(...)
      6. │     └─heatmaply:::ggplot_heatmap(...)
      7. │       └─ggplot2::theme(...)
      8. │         └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
      9. │           ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
     10. │           └─rlang::list2(..., ... = NULL)
     11. └─rlang::abort(message = message)
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

# lpirfs

<details>

* Version: 0.2.3
* GitHub: https://github.com/adaemmerp/lpirfs
* Source code: https://github.com/cran/lpirfs
* Date/Publication: 2023-07-06 07:50:03 UTC
* Number of recursive dependencies: 124

Run `revdepcheck::cloud_details(, "lpirfs")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘lpirfs_vignette.Rmd’ using rmarkdown
    
    Quitting from lines 285-443 [unnamed-chunk-7] (lpirfs_vignette.Rmd)
    Error: processing vignette 'lpirfs_vignette.Rmd' failed with diagnostics:
    The `legend.title` theme element must be a <element_text> object.
    --- failed re-building ‘lpirfs_vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘lpirfs_vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.6Mb
      sub-directories of 1Mb or more:
        libs   8.9Mb
    ```

# lsl

<details>

* Version: 0.5.6
* GitHub: NA
* Source code: https://github.com/cran/lsl
* Date/Publication: 2017-11-08 05:30:21 UTC
* Number of recursive dependencies: 38

Run `revdepcheck::cloud_details(, "lsl")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘lsl-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: lslSEM-class
    > ### Title: A Reference Class for Learning a SEM model via penalized
    > ###   likelihood.
    > ### Aliases: lslSEM-class lslSEM
    > 
    > ### ** Examples
    > 
    ...
     22. └─vctrs (local) `<fn>`()
     23.   └─vctrs::vec_default_ptype2(...)
     24.     ├─base::withRestarts(...)
     25.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     26.     │   └─base (local) doWithOneRestart(return(expr), restart)
     27.     └─vctrs::stop_incompatible_type(...)
     28.       └─vctrs:::stop_incompatible(...)
     29.         └─vctrs:::stop_vctrs(...)
     30.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

# mcmcabn

<details>

* Version: 0.6
* GitHub: NA
* Source code: https://github.com/cran/mcmcabn
* Date/Publication: 2023-09-28 12:20:02 UTC
* Number of recursive dependencies: 129

Run `revdepcheck::cloud_details(, "mcmcabn")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘mcmcabn.Rmd’ using rmarkdown
    
    Quitting from lines 77-95 [unnamed-chunk-3] (mcmcabn.Rmd)
    Error: processing vignette 'mcmcabn.Rmd' failed with diagnostics:
    Problem while computing aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `plot$scales$add_defaults()`:
    ! attempt to apply non-function
    --- failed re-building ‘mcmcabn.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘mcmcabn.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > if (require("testthat", character.only = TRUE)) {
      +   test_check("mcmcabn")
      + }
      Loading required package: testthat
      Loading required package: mcmcabn
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
      
    ...
      <packageNotFoundError/error/condition>
      Error in `find.package(package, lib.loc, verbose = verbose)`: there is no package called 'bnlearn'
      Backtrace:
          ▆
       1. └─utils::data(asia, package = "bnlearn") at test.R:12:1
       2.   └─base::find.package(package, lib.loc, verbose = verbose)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘bnlearn’
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

# mdthemes

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/mdthemes
* Date/Publication: 2020-06-14 14:40:17 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "mdthemes")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mdthemes-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: md_theme_base
    > ### Title: ggthemes Markdown Themes
    > ### Aliases: md_theme_base md_theme_calc md_theme_clean md_theme_economist
    > ###   md_theme_economist_white md_theme_excel md_theme_excel_new
    > ###   md_theme_few md_theme_fivethirtyeight md_theme_foundation
    > ###   md_theme_gdocs md_theme_hc md_theme_igray md_theme_map_gg
    > ###   md_theme_pander md_theme_par md_theme_solarized md_theme_solarized_2
    ...
        ▆
     1. ├─mdthemes::md_theme_excel_new()
     2. │ ├─mdthemes::as_md_theme(ggthemes::theme_excel_new(...))
     3. │ └─ggthemes::theme_excel_new(...)
     4. │   └─ggplot2::theme(...)
     5. │     └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
     6. │       ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
     7. │       └─rlang::list2(..., ... = NULL)
     8. └─rlang::abort(message = message)
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘intro-to-metaconfoundr.Rmd’ using rmarkdown
    
    Quitting from lines 90-93 [unnamed-chunk-8] (intro-to-metaconfoundr.Rmd)
    Error: processing vignette 'intro-to-metaconfoundr.Rmd' failed with diagnostics:
    Argument 1 can't be empty.
    --- failed re-building ‘intro-to-metaconfoundr.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘intro-to-metaconfoundr.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# metaplot

<details>

* Version: 0.8.3
* GitHub: NA
* Source code: https://github.com/cran/metaplot
* Date/Publication: 2019-04-25 22:52:09 UTC
* Number of recursive dependencies: 40

Run `revdepcheck::cloud_details(, "metaplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘metaplot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: categorical_data_frame
    > ### Title: Categorical Function for Data Frame
    > ### Aliases: categorical_data_frame
    > 
    > ### ** Examples
    > 
    > 
    ...
    
    > library(csv)
    > x <- as.csv(system.file(package = 'metaplot', 'extdata/theoph.csv'))
    > x %<>% pack
    > x %>% metaplot(site)
    > x %>% metaplot(site, gg = T)
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Calls: %>% ... do.call -> <Anonymous> -> scale_x_continuous -> startsWith
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# metaviz

<details>

* Version: 0.3.1
* GitHub: https://github.com/Mkossmeier/metaviz
* Source code: https://github.com/cran/metaviz
* Date/Publication: 2020-04-09 09:10:08 UTC
* Number of recursive dependencies: 136

Run `revdepcheck::cloud_details(, "metaviz")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘metaviz-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: viz_forest
    > ### Title: Forest plot variants for meta-analyses
    > ### Aliases: viz_forest
    > 
    > ### ** Examples
    > 
    > library(metaviz)
    > # Plotting the mozart data using a classic forest plot
    > viz_forest(x = mozart[, c("d", "se")],
    + study_labels = mozart[, "study_name"], xlab = "Cohen d")
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Calls: viz_forest ... do.call -> <Anonymous> -> scale_y_continuous -> startsWith
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘funnelinf.Rmd’ using rmarkdown
    --- finished re-building ‘funnelinf.Rmd’
    
    --- re-building ‘metaviz.Rmd’ using rmarkdown
    
    Quitting from lines 38-40 [unnamed-chunk-3] (metaviz.Rmd)
    Error: processing vignette 'metaviz.Rmd' failed with diagnostics:
    cannot coerce type 'closure' to vector of type 'character'
    --- failed re-building ‘metaviz.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘metaviz.Rmd’
    
    Error: Vignette re-building failed.
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
* Number of recursive dependencies: 129

Run `revdepcheck::cloud_details(, "mlr3fairness")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
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
* Number of recursive dependencies: 166

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

*   checking re-building of vignette outputs ... WARNING
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

* Version: 0.6.2
* GitHub: https://github.com/mlr-org/mlr3viz
* Source code: https://github.com/cran/mlr3viz
* Date/Publication: 2023-11-23 13:10:02 UTC
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
    INFO  [10:09:42.956] [bbotk]   5.884797  2.2371095 -32.51896
    INFO  [10:09:42.956] [bbotk]  -7.841127 -0.8872557 -91.31148
    INFO  [10:09:42.967] [bbotk] Finished optimizing after 20 evaluation(s)
    INFO  [10:09:42.968] [bbotk] Result:
    INFO  [10:09:42.970] [bbotk]        x1        x2  x_domain        y
    INFO  [10:09:42.970] [bbotk]  2.582281 -2.940254 <list[2]> 9.657379
    Error in Ops.data.frame(guide_loc, panel_loc) : 
      ‘==’ only defined for equally-sized data frames
    Calls: print ... plot_table.ggplot -> add_guides -> unlist -> Ops.data.frame
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > if (requireNamespace("testthat", quietly = TRUE)) {
      +   library("testthat")
      +   library("mlr3viz")
      +   test_check("mlr3viz")
      + }
      Starting 2 test processes
      [ FAIL 1 | WARN 0 | SKIP 21 | PASS 82 ]
    ...
      • TuningInstanceSingleCrit/tisc-surface-grid-50.svg
      • TuningInstanceSingleCrit/tisc-surface-regr-lm.svg
      • TuningInstanceSingleCrit/tisc-surface.svg
      • plot_learner_prediction/learner-prediction-1d-se.svg
      • plot_learner_prediction/learner-prediction-binary-prob.svg
      • plot_learner_prediction/learner-prediction-binary-response.svg
      • plot_learner_prediction/learner-prediction-categorical.svg
      • plot_learner_prediction/learner-prediction-prob.svg
      Error: Test failures
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

# mshap

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/mshap
* Date/Publication: 2021-06-17 08:40:02 UTC
* Number of recursive dependencies: 127

Run `revdepcheck::cloud_details(, "mshap")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(mshap)
      > 
      > test_check("mshap")
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 19 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
        5.     └─ggplot2::new_guide(...)
        6.       └─ggplot2:::validate_theme(params$theme)
        7.         └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
        8.           └─ggplot2 (local) `<fn>`(dots[[1L]][[3L]], dots[[2L]][[3L]], element_tree = `<named list>`)
        9.             └─cli::cli_abort(...)
       10.               └─rlang::abort(...)
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 19 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘mSHAP.Rmd’ using rmarkdown
    --- finished re-building ‘mSHAP.Rmd’
    
    --- re-building ‘mshap_plots.Rmd’ using rmarkdown
    
    Quitting from lines 65-69 [unnamed-chunk-3] (mshap_plots.Rmd)
    Error: processing vignette 'mshap_plots.Rmd' failed with diagnostics:
    The `legend.title` theme element must be a <element_text> object.
    --- failed re-building ‘mshap_plots.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘mshap_plots.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyselect’
      All declared Imports should be used.
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

# mvMAPIT

<details>

* Version: 2.0.3
* GitHub: https://github.com/lcrawlab/mvMAPIT
* Source code: https://github.com/cran/mvMAPIT
* Date/Publication: 2023-09-26 07:40:02 UTC
* Number of recursive dependencies: 123

Run `revdepcheck::cloud_details(, "mvMAPIT")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘mvMAPIT.Rmd’ using rmarkdown
    --- finished re-building ‘mvMAPIT.Rmd’
    
    --- re-building ‘study-compare-p-value-combine-methods.Rmd’ using rmarkdown
    
    Quitting from lines 83-92 [plot] (study-compare-p-value-combine-methods.Rmd)
    Error: processing vignette 'study-compare-p-value-combine-methods.Rmd' failed with diagnostics:
    cannot coerce type 'closure' to vector of type 'character'
    --- failed re-building ‘study-compare-p-value-combine-methods.Rmd’
    ...
    --- finished re-building ‘tutorial-docker-mvmapit.Rmd’
    
    --- re-building ‘tutorial-simulations.Rmd’ using rmarkdown
    --- finished re-building ‘tutorial-simulations.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘study-compare-p-value-combine-methods.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 24.8Mb
      sub-directories of 1Mb or more:
        libs  23.3Mb
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

*   checking re-building of vignette outputs ... WARNING
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

# naniar

<details>

* Version: 1.0.0
* GitHub: https://github.com/njtierney/naniar
* Source code: https://github.com/cran/naniar
* Date/Publication: 2023-02-02 09:50:02 UTC
* Number of recursive dependencies: 174

Run `revdepcheck::cloud_details(, "naniar")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘exploring-imputed-values.Rmd’ using rmarkdown
    --- finished re-building ‘exploring-imputed-values.Rmd’
    
    --- re-building ‘getting-started-w-naniar.Rmd’ using rmarkdown
    
    Quitting from lines 303-312 [simpute-visible] (getting-started-w-naniar.Rmd)
    Error: processing vignette 'getting-started-w-naniar.Rmd' failed with diagnostics:
    Can't combine <character> and <shade>.
    ...
    --- finished re-building ‘replace-with-na.Rmd’
    
    --- re-building ‘special-missing-values.Rmd’ using rmarkdown
    --- finished re-building ‘special-missing-values.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘getting-started-w-naniar.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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
      3. │ └─heatmaply:::heatmaply.default(...)
      4. │   ├─heatmaply::heatmaply(...)
      5. │   └─heatmaply:::heatmaply.heatmapr(...)
      6. │     └─heatmaply:::ggplot_heatmap(...)
      7. │       └─ggplot2::theme(...)
      8. │         └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
      9. │           ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
     10. │           └─rlang::list2(..., ... = NULL)
     11. └─rlang::abort(message = message)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
    ```

# nestedcv

<details>

* Version: 0.7.3
* GitHub: https://github.com/myles-lewis/nestedcv
* Source code: https://github.com/cran/nestedcv
* Date/Publication: 2023-12-04 11:40:02 UTC
* Number of recursive dependencies: 175

Run `revdepcheck::cloud_details(, "nestedcv")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘nestedcv-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pred_nestcv_glmnet
    > ### Title: Prediction wrappers to use fastshap with nestedcv
    > ### Aliases: pred_nestcv_glmnet pred_nestcv_glmnet_class1
    > ###   pred_nestcv_glmnet_class2 pred_nestcv_glmnet_class3 pred_train
    > ###   pred_train_class1 pred_train_class2 pred_train_class3
    > ###   pred_SuperLearner
    > 
    ...
      4.   │   └─ggplot2::ggproto(...)
      5.   │     └─rlang::list2(...)
      6.   └─ggplot2::guide_colorbar(barwidth = 0.5, barheight = 8, title.position = "left")
      7.     └─ggplot2::new_guide(...)
      8.       └─ggplot2:::validate_theme(params$theme)
      9.         └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
     10.           └─ggplot2 (local) `<fn>`(dots[[1L]][[3L]], dots[[2L]][[3L]], element_tree = `<named list>`)
     11.             └─cli::cli_abort(...)
     12.               └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘nestedcv.Rmd’ using rmarkdown
    --- finished re-building ‘nestedcv.Rmd’
    
    --- re-building ‘nestedcv_hsstan.Rmd’ using rmarkdown
    --- finished re-building ‘nestedcv_hsstan.Rmd’
    
    --- re-building ‘nestedcv_shap.Rmd’ using rmarkdown
    
    ...
    Quitting from lines 154-156 [unnamed-chunk-6] (nestedcv_shap.Rmd)
    Error: processing vignette 'nestedcv_shap.Rmd' failed with diagnostics:
    The `legend.title` theme element must be a <element_text> object.
    --- failed re-building ‘nestedcv_shap.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘nestedcv_shap.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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

# nett

<details>

* Version: 1.0.0
* GitHub: https://github.com/aaamini/nett
* Source code: https://github.com/cran/nett
* Date/Publication: 2022-11-09 10:50:05 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "nett")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(nett)
      > 
      > test_check("nett")
      time = 1.478[ FAIL 1 | WARN 1 | SKIP 0 | PASS 3 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
       1. ├─nett::plot_roc(out$roc) at test-roc.R:21:1
       2. │ └─ggplot2::theme(...)
       3. │   └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
       4. │     ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
       5. │     └─rlang::list2(..., ... = NULL)
       6. └─rlang::abort(message = message)
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 3 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Community_Detection.Rmd’ using rmarkdown
    --- finished re-building ‘Community_Detection.Rmd’
    
    --- re-building ‘Visualization.Rmd’ using rmarkdown
    --- finished re-building ‘Visualization.Rmd’
    
    --- re-building ‘explore-comm.Rmd’ using rmarkdown
    --- finished re-building ‘explore-comm.Rmd’
    ...
    Quitting from lines 191-194 [unnamed-chunk-9] (hard_dcsbm_testing.Rmd)
    Error: processing vignette 'hard_dcsbm_testing.Rmd' failed with diagnostics:
    Argument 1 can't be empty.
    --- failed re-building ‘hard_dcsbm_testing.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘hard_dcsbm_testing.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# novelqualcodes

<details>

* Version: 0.13.0
* GitHub: https://github.com/DesiQuintans/novelqualcodes
* Source code: https://github.com/cran/novelqualcodes
* Date/Publication: 2023-09-11 09:40:06 UTC
* Number of recursive dependencies: 75

Run `revdepcheck::cloud_details(, "novelqualcodes")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘novelqualcodes-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_novelty
    > ### Title: Plot novelty of interviews over time
    > ### Aliases: plot_novelty
    > 
    > ### ** Examples
    > 
    > # Field notes and coding matrices included with the package
    ...
     21. └─vctrs (local) `<fn>`()
     22.   └─vctrs::vec_default_ptype2(...)
     23.     ├─base::withRestarts(...)
     24.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     25.     │   └─base (local) doWithOneRestart(return(expr), restart)
     26.     └─vctrs::stop_incompatible_type(...)
     27.       └─vctrs:::stop_incompatible(...)
     28.         └─vctrs:::stop_vctrs(...)
     29.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘guided-tour.Rmd’ using rmarkdown
    
    Quitting from lines 291-292 [unnamed-chunk-8] (guided-tour.Rmd)
    Error: processing vignette 'guided-tour.Rmd' failed with diagnostics:
    Can't combine <character> and <logical>.
    --- failed re-building ‘guided-tour.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘guided-tour.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# nphRCT

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/nphRCT
* Date/Publication: 2022-09-01 13:40:02 UTC
* Number of recursive dependencies: 117

Run `revdepcheck::cloud_details(, "nphRCT")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘explanation.Rmd’ using rmarkdown
    
    Quitting from lines 44-73 [unnamed-chunk-1] (explanation.Rmd)
    Error: processing vignette 'explanation.Rmd' failed with diagnostics:
    cannot coerce type 'closure' to vector of type 'character'
    --- failed re-building ‘explanation.Rmd’
    
    --- re-building ‘weighted_log_rank_tests.Rmd’ using rmarkdown
    --- finished re-building ‘weighted_log_rank_tests.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘explanation.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# nzelect

<details>

* Version: 0.4.0
* GitHub: NA
* Source code: https://github.com/cran/nzelect
* Date/Publication: 2017-10-02 20:35:23 UTC
* Number of recursive dependencies: 95

Run `revdepcheck::cloud_details(, "nzelect")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘README.Rmd’ using rmarkdown
    
    Quitting from lines 64-82 [unnamed-chunk-3] (README.Rmd)
    Error: processing vignette 'README.Rmd' failed with diagnostics:
    cannot coerce type 'closure' to vector of type 'character'
    --- failed re-building ‘README.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘README.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.9Mb
      sub-directories of 1Mb or more:
        data   5.6Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6409 marked UTF-8 strings
    ```

# ogrdbstats

<details>

* Version: 0.5.0
* GitHub: https://github.com/airr-community/ogrdbstats
* Source code: https://github.com/cran/ogrdbstats
* Date/Publication: 2023-03-09 08:50:05 UTC
* Number of recursive dependencies: 129

Run `revdepcheck::cloud_details(, "ogrdbstats")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ogrdbstats-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: make_novel_base_grobs
    > ### Title: Create plots showing base usage at selected locations in
    > ###   sequences based on novel alleles
    > ### Aliases: make_novel_base_grobs
    > 
    > ### ** Examples
    > 
    ...
    > base_grobs = make_novel_base_grobs(
    +                  example_rep$inferred_seqs,
    +                  example_rep$input_sequences,
    +                  'V',
    +                  FALSE
    +              )
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Calls: make_novel_base_grobs ... mapply -> <Anonymous> -> scale_y_continuous -> startsWith
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Using_ogrdbstats.Rmd’ using rmarkdown
    
    Quitting from lines 278-296 [unnamed-chunk-1] (Using_ogrdbstats.Rmd)
    Error: processing vignette 'Using_ogrdbstats.Rmd' failed with diagnostics:
    cannot coerce type 'closure' to vector of type 'character'
    --- failed re-building ‘Using_ogrdbstats.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Using_ogrdbstats.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# omu

<details>

* Version: 1.1.1
* GitHub: https://github.com/connor-reid-tiffany/Omu
* Source code: https://github.com/cran/omu
* Date/Publication: 2023-10-16 22:30:02 UTC
* Number of recursive dependencies: 139

Run `revdepcheck::cloud_details(, "omu")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘omu-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_volcano
    > ### Title: Create a volcano plot
    > ### Aliases: plot_volcano
    > 
    > ### ** Examples
    > 
    > c57_nos2KO_mouse_countDF <- assign_hierarchy(c57_nos2KO_mouse_countDF, TRUE, "KEGG")
    ...
     21. └─vctrs (local) `<fn>`()
     22.   └─vctrs::vec_default_ptype2(...)
     23.     ├─base::withRestarts(...)
     24.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     25.     │   └─base (local) doWithOneRestart(return(expr), restart)
     26.     └─vctrs::stop_incompatible_type(...)
     27.       └─vctrs:::stop_incompatible(...)
     28.         └─vctrs:::stop_vctrs(...)
     29.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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
* Number of recursive dependencies: 286

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

## In both

*   checking whether package ‘personalized’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'simplify2array(msg, except = 0L)': unused argument (except = 0) 
    See ‘/tmp/workdir/personalized/new/personalized.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    xgb_cv_personalized: possible error in simplify2array(msg, except =
      0L): unused argument (except = 0)
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ggh4x’
      All declared Imports should be used.
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
      installed size is 11.2Mb
      sub-directories of 1Mb or more:
        libs   8.2Mb
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking installed package size ... NOTE
    ```
      installed size is 54.5Mb
      sub-directories of 1Mb or more:
        libs  53.5Mb
    ```

# pould

<details>

* Version: 1.0.1
* GitHub: NA
* Source code: https://github.com/cran/pould
* Date/Publication: 2020-10-16 13:50:03 UTC
* Number of recursive dependencies: 105

Run `revdepcheck::cloud_details(, "pould")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘pould-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: LD.heat.map
    > ### Title: Generates heat-maps for four linkage disequilibrium (LD) values
    > ###   (D', Wn, WLoc1/Loc2 and WLoc2/Loc1) generated for all pairs of phased
    > ###   and unphased two-locus haplotypes by LDWrap().
    > ### Aliases: LD.heat.map
    > ### Keywords: disequilibrium heat heat-map heatmap linkage map
    > 
    ...
      2.   ├─ggplot2::guides(...)
      3.   │ └─rlang::list2(...)
      4.   └─ggplot2::guide_colorbar(...)
      5.     └─ggplot2::new_guide(...)
      6.       └─ggplot2:::validate_theme(params$theme)
      7.         └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
      8.           └─ggplot2 (local) `<fn>`(dots[[1L]][[3L]], dots[[2L]][[3L]], element_tree = `<named list>`)
      9.             └─cli::cli_abort(...)
     10.               └─rlang::abort(...)
    Execution halted
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

# precintcon

<details>

* Version: 2.3.0
* GitHub: https://github.com/lucasvenez/precintcon
* Source code: https://github.com/cran/precintcon
* Date/Publication: 2016-07-17 13:49:19
* Number of recursive dependencies: 28

Run `revdepcheck::cloud_details(, "precintcon")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘precintcon-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pplot.lorenz
    > ### Title: Plot Lorenz's curve
    > ### Aliases: pplot.ci pplot.lorenz precintcon.plot.lorenz
    > ### Keywords: curve lorenz's precipitation
    > 
    > ### ** Examples
    > 
    ...
    > # Loading the daily precipitation serie.
    > data(daily)
    > 
    > ##
    > # Performing the a set of statistical analysis
    > pplot.ci(daily, interval = 1)
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Calls: pplot.ci ... mapply -> <Anonymous> -> scale_x_continuous -> startsWith
    Execution halted
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

*   checking re-building of vignette outputs ... WARNING
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
      installed size is  9.9Mb
      sub-directories of 1Mb or more:
        data   1.5Mb
        libs   6.4Mb
    ```

# predictNMB

<details>

* Version: 0.2.1
* GitHub: https://github.com/ropensci/predictNMB
* Source code: https://github.com/cran/predictNMB
* Date/Publication: 2023-06-03 07:40:02 UTC
* Number of recursive dependencies: 117

Run `revdepcheck::cloud_details(, "predictNMB")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > #' @srrstats {G5.2, G5.2a, G5.2b, G5.3, G5.4, G5.4a, G5.5, G5.6, G5.6a, G5.6b,
      > #' G5.8, G5.8a, G5.8b, G5.8c, G5.8d, G5.9, G5.9b, G5.11}
      > #' Covered by package tests.
      > #' @srrstats {EA6.0, EA6.0a, EA6.0b, EA6.0c, EA6.0d}
      > #' All return values are tested. Classes and expectations for graphics are
      > #' tested in in test-plot.R.
    ...
      • ce_plot/ce-plot-predictnmbsim-obj-filled-points.svg
      • ce_plot/ce-plot-predictnmbsim-obj-method-shapes.svg
      • ce_plot/ce-plot-predictnmbsim-obj-methods-reorder.svg
      • ce_plot/ce-plot-predictnmbsim-obj-no-wtp-line.svg
      • ce_plot/ce-plot-predictnmbsim-obj-none.svg
      • ce_plot/ce-plot-predictnmbsim-obj-solid-wtp-linetype.svg
      • ce_plot/ce-plot-predictnmbsim-obj-triangles.svg
      • ce_plot/ce-plot-predictnmbsim-obj-twodash-wtp-linetype.svg
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘creating-nmb-functions.Rmd’ using rmarkdown
    
    Quitting from lines 321-322 [unnamed-chunk-18] (creating-nmb-functions.Rmd)
    Error: processing vignette 'creating-nmb-functions.Rmd' failed with diagnostics:
    Argument 1 can't be empty.
    --- failed re-building ‘creating-nmb-functions.Rmd’
    
    --- re-building ‘detailed-example.Rmd’ using rmarkdown
    
    ...
    Error: processing vignette 'summarising-results-with-predictNMB.Rmd' failed with diagnostics:
    Argument 1 can't be empty.
    --- failed re-building ‘summarising-results-with-predictNMB.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘creating-nmb-functions.Rmd’ ‘detailed-example.Rmd’ ‘predictNMB.Rmd’
      ‘summarising-results-with-predictNMB.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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

# profiplots

<details>

* Version: 0.2.3
* GitHub: NA
* Source code: https://github.com/cran/profiplots
* Date/Publication: 2023-11-16 11:20:07 UTC
* Number of recursive dependencies: 121

Run `revdepcheck::cloud_details(, "profiplots")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘how-to-use.Rmd’ using rmarkdown
    Warning in doTryCatch(return(expr), name, parentenv, handler) :
      "lwd.ticks" is not a graphical parameter
    Warning in doTryCatch(return(expr), name, parentenv, handler) :
      "lwd.ticks" is not a graphical parameter
    Warning in doTryCatch(return(expr), name, parentenv, handler) :
      "lwd.ticks" is not a graphical parameter
    Warning in doTryCatch(return(expr), name, parentenv, handler) :
    ...
    Quitting from lines 1054-1074 [unnamed-chunk-42] (plots-gallery.Rmd)
    Error: processing vignette 'plots-gallery.Rmd' failed with diagnostics:
    Can't combine <character> and <logical>.
    --- failed re-building ‘plots-gallery.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘plots-gallery.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# promor

<details>

* Version: 0.2.1
* GitHub: https://github.com/caranathunge/promor
* Source code: https://github.com/cran/promor
* Date/Publication: 2023-07-17 22:30:02 UTC
* Number of recursive dependencies: 157

Run `revdepcheck::cloud_details(, "promor")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘promor-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: impute_plot
    > ### Title: Visualize the impact of imputation
    > ### Aliases: impute_plot
    > 
    > ### ** Examples
    > 
    > 
    ...
    ! Argument 1 can't be empty.
    Backtrace:
        ▆
     1. ├─promor::impute_plot(raw_df, imp_df, global = FALSE)
     2. │ └─ggplot2::theme(...)
     3. │   └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
     4. │     ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
     5. │     └─rlang::list2(..., ... = NULL)
     6. └─rlang::abort(message = message)
    Execution halted
    ```

# psyntur

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/psyntur
* Date/Publication: 2021-09-15 09:20:05 UTC
* Number of recursive dependencies: 117

Run `revdepcheck::cloud_details(, "psyntur")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘psyntur-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pairs_plot
    > ### Title: A pairs plot
    > ### Aliases: pairs_plot
    > 
    > ### ** Examples
    > 
    > # A simple pairs plot
    ...
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Calls: <Anonymous> ... do.call -> <Anonymous> -> scale_y_continuous -> startsWith
    Execution halted
    ```

# ptable

<details>

* Version: 1.0.0
* GitHub: https://github.com/sdcTools/ptable
* Source code: https://github.com/cran/ptable
* Date/Publication: 2023-03-01 20:10:09 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::cloud_details(, "ptable")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ptable-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot
    > ### Title: Plot the results of the perturbation table generator
    > ### Aliases: plot
    > ### Keywords: plot
    > 
    > ### ** Examples
    > 
    ...
      5.       ├─ggplot2::guides(...)
      6.       │ └─rlang::list2(...)
      7.       └─ggplot2::guide_legend(...)
      8.         └─ggplot2::new_guide(...)
      9.           └─ggplot2:::validate_theme(params$theme)
     10.             └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
     11.               └─ggplot2 (local) `<fn>`(dots[[1L]][[1L]], dots[[2L]][[1L]], element_tree = `<named list>`)
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
      > library(ptable)
      
      Attaching package: 'ptable'
      
      The following object is masked from 'package:graphics':
      
    ...
      i Actually got a <rlang_error> with text:
        The `legend.title` theme element must be a <element_text> object.
      ── Failure ('test-functions.R:45:3'): Plot functions are ok ────────────────────
      Expected `plot(obj1, type = "p", file = "test.pdf")` to run without any errors.
      i Actually got a <rlang_error> with text:
        The `legend.title` theme element must be a <element_text> object.
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 76 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘introduction.Rmd’ using rmarkdown
    
    Quitting from lines 322-323 [unnamed-chunk-24] (introduction.Rmd)
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    The `legend.title` theme element must be a <element_text> object.
    --- failed re-building ‘introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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

# qad

<details>

* Version: 1.0.4
* GitHub: https://github.com/griefl/qad
* Source code: https://github.com/cran/qad
* Date/Publication: 2022-12-14 16:50:02 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "qad")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘qad-vignette.Rmd’ using rmarkdown
    
    Quitting from lines 128-227 [unnamed-chunk-3] (qad-vignette.Rmd)
    Error: processing vignette 'qad-vignette.Rmd' failed with diagnostics:
    The `legend.title` theme element must be a <element_text> object.
    --- failed re-building ‘qad-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘qad-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# qicharts

<details>

* Version: 0.5.8
* GitHub: NA
* Source code: https://github.com/cran/qicharts
* Date/Publication: 2021-04-20 12:20:03 UTC
* Number of recursive dependencies: 59

Run `revdepcheck::cloud_details(, "qicharts")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘qicharts-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: summary.tcc
    > ### Title: Summarise Trellis Control Charts
    > ### Aliases: summary.tcc
    > 
    > ### ** Examples
    > 
    > # Build data frame for example
    ...
     21. └─vctrs (local) `<fn>`()
     22.   └─vctrs::vec_default_ptype2(...)
     23.     ├─base::withRestarts(...)
     24.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     25.     │   └─base (local) doWithOneRestart(return(expr), restart)
     26.     └─vctrs::stop_incompatible_type(...)
     27.       └─vctrs:::stop_incompatible(...)
     28.         └─vctrs:::stop_vctrs(...)
     29.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

# qicharts2

<details>

* Version: 0.7.4
* GitHub: https://github.com/anhoej/qicharts2
* Source code: https://github.com/cran/qicharts2
* Date/Publication: 2023-08-21 08:30:10 UTC
* Number of recursive dependencies: 72

Run `revdepcheck::cloud_details(, "qicharts2")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘qicharts2-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: qic
    > ### Title: Statistical process control charts.
    > ### Aliases: qic
    > 
    > ### ** Examples
    > 
    > # Lock random number generator to make reproducible results.
    ...
     21. └─vctrs (local) `<fn>`()
     22.   └─vctrs::vec_default_ptype2(...)
     23.     ├─base::withRestarts(...)
     24.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     25.     │   └─base (local) doWithOneRestart(return(expr), restart)
     26.     └─vctrs::stop_incompatible_type(...)
     27.       └─vctrs:::stop_incompatible(...)
     28.         └─vctrs:::stop_vctrs(...)
     29.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘qicharts2.Rmd’ using rmarkdown
    
    Quitting from lines 34-40 [unnamed-chunk-2] (qicharts2.Rmd)
    Error: processing vignette 'qicharts2.Rmd' failed with diagnostics:
    Can't combine <character> and <logical>.
    --- failed re-building ‘qicharts2.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘qicharts2.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# qualvar

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/qualvar
* Date/Publication: 2018-01-03 00:12:05 UTC
* Number of recursive dependencies: 86

Run `revdepcheck::cloud_details(, "qualvar")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘wilcox1973.Rmd’ using rmarkdown
    
    Quitting from lines 111-120 [correlation] (wilcox1973.Rmd)
    Error: processing vignette 'wilcox1973.Rmd' failed with diagnostics:
    cannot coerce type 'closure' to vector of type 'character'
    --- failed re-building ‘wilcox1973.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘wilcox1973.Rmd’
    
    Error: Vignette re-building failed.
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

*   checking re-building of vignette outputs ... WARNING
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
    > ### Title: Visualize data using ggplot2 <URL:
    > ###   https://ggplot2.tidyverse.org/>
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

# randomForestExplainer

<details>

* Version: 0.10.1
* GitHub: https://github.com/ModelOriented/randomForestExplainer
* Source code: https://github.com/cran/randomForestExplainer
* Date/Publication: 2020-07-11 20:30:02 UTC
* Number of recursive dependencies: 105

Run `revdepcheck::cloud_details(, "randomForestExplainer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘randomForestExplainer-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_importance_ggpairs
    > ### Title: Plot importance measures with ggpairs
    > ### Aliases: plot_importance_ggpairs
    > 
    > ### ** Examples
    > 
    > forest <- randomForest::randomForest(Species ~ ., data = iris, localImp = TRUE, ntree = 200)
    > frame <- measure_importance(forest, measures = c("mean_min_depth", "times_a_root"))
    > plot_importance_ggpairs(frame, measures = c("mean_min_depth", "times_a_root"))
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Calls: <Anonymous> ... do.call -> <Anonymous> -> scale_y_continuous -> startsWith
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(randomForestExplainer)
      > 
      > test_check("randomForestExplainer")
      [ FAIL 12 | WARN 70 | SKIP 0 | PASS 49 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
       18.       └─GGally::getPlot(pm, i, j)
       19.         └─GGally (local) fn(pm$data, plotObj$mapping)
       20.           ├─base::do.call(original_fn, allParams)
       21.           └─GGally (local) `<fn>`(data = `<df[,5]>`, mapping = `<uneval>`)
       22.             └─ggplot2::scale_y_continuous()
       23.               └─base::startsWith(as.character(call[[1]]), "scale_")
      
      [ FAIL 12 | WARN 70 | SKIP 0 | PASS 49 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘randomForestExplainer.Rmd’ using rmarkdown
    
    Quitting from lines 149-151 [unnamed-chunk-11] (randomForestExplainer.Rmd)
    Error: processing vignette 'randomForestExplainer.Rmd' failed with diagnostics:
    cannot coerce type 'closure' to vector of type 'character'
    --- failed re-building ‘randomForestExplainer.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘randomForestExplainer.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
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

# rciplot

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/rciplot
* Date/Publication: 2023-03-15 09:10:02 UTC
* Number of recursive dependencies: 31

Run `revdepcheck::cloud_details(, "rciplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rciplot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: rciplot
    > ### Title: rciplot
    > ### Aliases: rciplot
    > 
    > ### ** Examples
    > 
    > # Using example data from `sample_data.rda` to recreate Figure 2 of
    ...
    ! Argument 1 can't be empty.
    Backtrace:
        ▆
     1. ├─rciplot::rciplot(...)
     2. │ └─ggplot2::theme(legend.position = "right", )
     3. │   └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
     4. │     ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
     5. │     └─rlang::list2(..., ... = NULL)
     6. └─rlang::abort(message = message)
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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
      installed size is 19.8Mb
      sub-directories of 1Mb or more:
        R      1.9Mb
        libs  17.3Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
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

*   checking re-building of vignette outputs ... WARNING
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

# ridgetorus

<details>

* Version: 1.0.2
* GitHub: https://github.com/egarpor/ridgetorus
* Source code: https://github.com/cran/ridgetorus
* Date/Publication: 2023-08-27 22:40:02 UTC
* Number of recursive dependencies: 142

Run `revdepcheck::cloud_details(, "ridgetorus")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ridgetorus-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: torus_pairs
    > ### Title: Toroidal pairs plot
    > ### Aliases: torus_pairs
    > 
    > ### ** Examples
    > 
    > # Generate data
    ...
    +                    sigma = diag(0.1, nrow = 2))
    + ))
    > col <- rainbow(3)[rep(1:3, each = n)]
    > 
    > # Torus pairs
    > torus_pairs(x, col_data = col)
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Calls: <Anonymous> ... do.call -> <Anonymous> -> <Anonymous> -> startsWith
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        libs   5.7Mb
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

*   checking re-building of vignette outputs ... WARNING
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

# robvis

<details>

* Version: 0.3.0
* GitHub: https://github.com/mcguinlu/robvis
* Source code: https://github.com/cran/robvis
* Date/Publication: 2019-11-22 15:00:02 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "robvis")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Introduction_to_robvis.Rmd’ using rmarkdown
    
    Quitting from lines 95-96 [unnamed-chunk-4] (Introduction_to_robvis.Rmd)
    Error: processing vignette 'Introduction_to_robvis.Rmd' failed with diagnostics:
    The `legend.title` theme element must be a <element_text> object.
    --- failed re-building ‘Introduction_to_robvis.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Introduction_to_robvis.Rmd’
    
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

# rpf

<details>

* Version: 1.0.14
* GitHub: https://github.com/jpritikin/rpf
* Source code: https://github.com/cran/rpf
* Date/Publication: 2023-08-21 23:20:02 UTC
* Number of recursive dependencies: 75

Run `revdepcheck::cloud_details(, "rpf")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘customitem.Rmd’ using knitr
    --- finished re-building ‘customitem.Rmd’
    
    --- re-building ‘diagnostics.Rmd’ using knitr
    
    Quitting from lines 49-78 [unnamed-chunk-4] (diagnostics.Rmd)
    Error: processing vignette 'diagnostics.Rmd' failed with diagnostics:
    The `legend.text` theme element must be a <element_text> object.
    ...
    --- failed re-building ‘diagnostics.Rmd’
    
    --- re-building ‘flexmirt-plots.Rmd’ using knitr
    --- finished re-building ‘flexmirt-plots.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘diagnostics.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 36.4Mb
      sub-directories of 1Mb or more:
        libs  35.3Mb
    ```

# rrr

<details>

* Version: 1.0.0
* GitHub: https://github.com/chrisaddy/rrr
* Source code: https://github.com/cran/rrr
* Date/Publication: 2016-12-09 15:15:55
* Number of recursive dependencies: 104

Run `revdepcheck::cloud_details(, "rrr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rrr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: residuals
    > ### Title: Reduced-Rank Regression Residuals
    > ### Aliases: residuals
    > 
    > ### ** Examples
    > 
    > data(tobacco)
    ...
     7     -0.0419           2.08              -0.339
     8     -0.145            2.75              -0.190
     9     -0.0979           1.53              -0.588
    10     -0.355            2.61              -0.333
    # ℹ 15 more rows
    > residuals(tobacco_x, tobacco_y, rank = 1)
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Calls: <Anonymous> ... do.call -> <Anonymous> -> scale_y_continuous -> startsWith
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘rrr.Rmd’ using rmarkdown
    
    Quitting from lines 249-250 [unnamed-chunk-17] (rrr.Rmd)
    Error: processing vignette 'rrr.Rmd' failed with diagnostics:
    cannot coerce type 'closure' to vector of type 'character'
    --- failed re-building ‘rrr.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘rrr.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Rcpp’
      All declared Imports should be used.
    ```

# sageR

<details>

* Version: 0.6.1
* GitHub: https://github.com/fbertran/sageR
* Source code: https://github.com/cran/sageR
* Date/Publication: 2023-03-23 18:40:02 UTC
* Number of recursive dependencies: 249

Run `revdepcheck::cloud_details(, "sageR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘sageR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: air_pollution
    > ### Title: Air pollution data
    > ### Aliases: air_pollution
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    ...
     $ LPOP   : num  5.86 5.27 5.45 5.79 5.41 ...
     $ l_pm2  : num  4.75 3.06 2.76 7.21 2.9 ...
     $ l_pmax : num  5.41 4.82 6.11 5.53 5.39 ...
    > library(ggplot2)
    > library(GGally)
    > GGally::ggpairs(air_pollution[,2:4],)
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Calls: <Anonymous> ... do.call -> <Anonymous> -> scale_y_continuous -> startsWith
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘DescTools’
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 120 marked UTF-8 strings
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
      [ FAIL 3 | WARN 2 | SKIP 11 | PASS 261 ]
      
    ...
       17.                       ├─base::all.equal(...)
       18.                       └─base::all.equal.default(...)
       19.                         └─base::all.equal.list(target, current, ...)
       20.                           ├─base::all.equal(...)
       21.                           └─base::all.equal.environment(...)
       22.                             └─base::as.list.environment(target, all.names = all.names, sorted = TRUE)
      
      [ FAIL 3 | WARN 2 | SKIP 11 | PASS 261 ]
      Error: Test failures
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

*   checking re-building of vignette outputs ... WARNING
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
        8.       └─ggplot2::new_guide(...)
        9.         └─ggplot2:::validate_theme(params$theme)
       10.           └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
       11.             └─ggplot2 (local) `<fn>`(dots[[1L]][[3L]], dots[[2L]][[3L]], element_tree = `<named list>`)
       12.               └─cli::cli_abort(...)
       13.                 └─rlang::abort(...)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 91 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘basic_use.Rmd’ using rmarkdown
    
    Quitting from lines 141-149 [unnamed-chunk-7] (basic_use.Rmd)
    Error: processing vignette 'basic_use.Rmd' failed with diagnostics:
    The `legend.title` theme element must be a <element_text> object.
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

# signatureSurvival

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/signatureSurvival
* Date/Publication: 2023-07-19 11:10:02 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::cloud_details(, "signatureSurvival")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘signatureSurvival-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: MKMplot
    > ### Title: Multivariate Kaplan-Meier survival curve plot
    > ### Aliases: MKMplot
    > ### Keywords: multivariate survival analysis
    > 
    > ### ** Examples
    > 
    ...
    > require(ggplot2)
    Loading required package: ggplot2
    > data(GSE50081)
    > MKMplot(data=GSE50081,mol=56,X=c("t.stage","n.stage",	"m.stage"),time="month",
    + status="status1",sml="none",quant=c("No",-0.2,0.2),plotmethod="ggsurvplot",
    + adjx = 5)
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Calls: MKMplot ... do.call -> <Anonymous> -> <Anonymous> -> startsWith
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.2Mb
      sub-directories of 1Mb or more:
        data   7.3Mb
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

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘advanced_plotting.Rmd’ using rmarkdown
    
    Quitting from lines 187-213 [unnamed-chunk-13] (advanced_plotting.Rmd)
    Error: processing vignette 'advanced_plotting.Rmd' failed with diagnostics:
    comparison (1) is possible only for atomic and list types
    --- failed re-building ‘advanced_plotting.Rmd’
    
    --- re-building ‘quick_start.Rmd’ using rmarkdown
    ...
    Quitting from lines 227-228 [unnamed-chunk-15] (simmr.Rmd)
    Error: processing vignette 'simmr.Rmd' failed with diagnostics:
    cannot coerce type 'closure' to vector of type 'character'
    --- failed re-building ‘simmr.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘advanced_plotting.Rmd’ ‘simmr.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        doc    1.6Mb
        libs   3.8Mb
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
    
    > ### Name: plotStabilityClass
    > ### Title: Plot stability class
    > ### Aliases: plotStabilityClass
    > 
    > ### ** Examples
    > 
    > data(stMeteo)
    ...
      2.   ├─ggplot2::guides(...)
      3.   │ └─rlang::list2(...)
      4.   └─ggplot2::guide_legend(...)
      5.     └─ggplot2::new_guide(...)
      6.       └─ggplot2:::validate_theme(params$theme)
      7.         └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
      8.           └─ggplot2 (local) `<fn>`(dots[[1L]][[1L]], dots[[2L]][[1L]], element_tree = `<named list>`)
      9.             └─cli::cli_abort(...)
     10.               └─rlang::abort(...)
    Execution halted
    ```

# sjPlot

<details>

* Version: 2.8.15
* GitHub: https://github.com/strengejacke/sjPlot
* Source code: https://github.com/cran/sjPlot
* Date/Publication: 2023-08-17 15:30:02 UTC
* Number of recursive dependencies: 188

Run `revdepcheck::cloud_details(, "sjPlot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘sjPlot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: sjplot
    > ### Title: Wrapper to create plots and tables within a pipe-workflow
    > ### Aliases: sjplot sjtab
    > 
    > ### ** Examples
    > 
    > library(dplyr)
    ...
    > # Grouped frequencies
    > efc %>% sjplot(e42dep, c172code, fun = "grpfrq")
    > 
    > # Grouped frequencies, as box plots
    > efc %>% sjplot(e17age, c172code, fun = "grpfrq",
    +                type = "box", geom.colors = "Set1")
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Calls: %>% ... do.call -> <Anonymous> -> scale_y_continuous -> startsWith
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘rstanarm’
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
       14.         └─ggplot2::new_guide(...)
       15.           └─ggplot2:::validate_theme(params$theme)
       16.             └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
       17.               └─ggplot2 (local) `<fn>`(dots[[1L]][[1L]], dots[[2L]][[1L]], element_tree = `<named list>`)
       18.                 └─cli::cli_abort(...)
       19.                   └─rlang::abort(...)
      
      [ FAIL 3 | WARN 44 | SKIP 0 | PASS 28 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

# statVisual

<details>

* Version: 1.2.1
* GitHub: NA
* Source code: https://github.com/cran/statVisual
* Date/Publication: 2020-02-20 19:30:02 UTC
* Number of recursive dependencies: 194

Run `revdepcheck::cloud_details(, "statVisual")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘statVisual.Rmd’ using rmarkdown
    
    Quitting from lines 1171-1213 [unnamed-chunk-56] (statVisual.Rmd)
    Error: processing vignette 'statVisual.Rmd' failed with diagnostics:
    cannot coerce type 'closure' to vector of type 'character'
    --- failed re-building ‘statVisual.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘statVisual.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘gbm’ ‘ggfortify’ ‘tibble’ ‘tidyverse’
      All declared Imports should be used.
    ```

# statgenGWAS

<details>

* Version: 1.0.9
* GitHub: https://github.com/Biometris/statgenGWAS
* Source code: https://github.com/cran/statgenGWAS
* Date/Publication: 2022-10-13 15:30:43 UTC
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "statgenGWAS")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘tinytest.R’
    Running the tests in ‘tests/tinytest.R’ failed.
    Complete output:
      > 
      > if ( requireNamespace("tinytest", quietly=TRUE) ){
      +   tinytest::test_package("statgenGWAS")
      + }
      
      test_GWAS.R...................    0 tests    
      test_GWAS.R...................    0 tests    
    ...
        conversion failure on '← 2@3' in 'mbcsToSbcs': dot substituted for <86>
      3: In grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
        conversion failure on '← 2@3' in 'mbcsToSbcs': dot substituted for <90>
      4: In grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
        conversion failure on '← 2@3' in 'mbcsToSbcs': dot substituted for <e2>
      5: In grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
        conversion failure on '← 2@3' in 'mbcsToSbcs': dot substituted for <86>
      6: In grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
        conversion failure on '← 2@3' in 'mbcsToSbcs': dot substituted for <90>
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘GWAS.Rmd’ using rmarkdown
    
    Quitting from lines 282-284 [manhattanStg] (GWAS.Rmd)
    Error: processing vignette 'GWAS.Rmd' failed with diagnostics:
    cannot coerce type 'closure' to vector of type 'character'
    --- failed re-building ‘GWAS.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘GWAS.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 13.6Mb
      sub-directories of 1Mb or more:
        data   3.1Mb
        libs   9.4Mb
    ```

# statgenMPP

<details>

* Version: 1.0.2
* GitHub: NA
* Source code: https://github.com/cran/statgenMPP
* Date/Publication: 2022-12-02 22:00:02 UTC
* Number of recursive dependencies: 80

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
      test_createQTLmpp.R...........    0 tests    
      test_createQTLmpp.R...........    0 tests    
      test_createQTLmpp.R...........    0 tests    
      test_createQTLmpp.R...........    0 tests    
      test_createQTLmpp.R...........    0 tests    
      test_createQTLmpp.R...........    1 tests [0;32mOK[0m 
      test_createQTLmpp.R...........    2 tests [0;32mOK[0m Error in as.character(call[[1]]) : 
        cannot coerce type 'closure' to vector of type 'character'
      Calls: <Anonymous> ... do.call -> <Anonymous> -> <Anonymous> -> startsWith
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘QTLMapping_in_MultiParentPopulations.Rmd’ using rmarkdown
    
    Quitting from lines 163-165 [QPABCSQM] (QTLMapping_in_MultiParentPopulations.Rmd)
    Error: processing vignette 'QTLMapping_in_MultiParentPopulations.Rmd' failed with diagnostics:
    cannot coerce type 'closure' to vector of type 'character'
    --- failed re-building ‘QTLMapping_in_MultiParentPopulations.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘QTLMapping_in_MultiParentPopulations.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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

*   checking re-building of vignette outputs ... WARNING
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
      Actual value: "List of 11\\n \$ data       :'data\.frame':\\t6 obs\. of  5 variables:\\n  \.\.\$ dose      : num \[1:6\] 0\.5 0\.5 1 1 2 2\\n  \.\.\$ supp      : chr \[1:6\] "OJ" "VC" "OJ" "VC" \.\.\.\\n  \.\.\$ center    : num \[1:6\] 13 8 22 17 26 26\\n  \.\.\$ lowerwidth: num \[1:6\] -3 -2 -3 -2 -2 -4\\n  \.\.\$ upperwidth: num \[1:6\] 3 2 3 2 2 4\\n \$ layers     :List of 4\\n  \.\.\$ :Classes 'LayerInstance', 'Layer', 'ggproto', 'gg' <ggproto object: Class LayerInstance, Layer, gg>\\n    aes_params: list\\n    compute_aesthetics: function\\n    compute_geom_1: function\\n    compute_geom_2: function\\n    compute_position: function\\n    compute_statistic: function\\n    computed_geom_params: NULL\\n    computed_mapping: NULL\\n    computed_stat_params: NULL\\n    constructor: call\\n    data: data\.frame\\n    draw_geom: function\\n    finish_statistics: function\\n    geom: <ggproto object: Class GeomViolin, Geom, gg>\\n        aesthetics: function\\n        default_aes: uneval\\n        draw_group: function\\n        draw_key: function\\n        draw_layer: function\\n        draw_panel: function\\n        extra_params: na\.rm orientation lineend linejoin linemitre\\n        handle_na: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        rename_size: TRUE\\n        required_aes: x y\\n        setup_data: function\\n        setup_params: function\\n        use_defaults: function\\n        super:  <ggproto object: Class Geom, gg>\\n    geom_params: list\\n    inherit\.aes: TRUE\\n    layer_data: function\\n    map_statistic: function\\n    mapping: uneval\\n    position: <ggproto object: Class PositionDodge, Position, gg>\\n        compute_layer: function\\n        compute_panel: function\\n        preserve: total\\n        required_aes: \\n        setup_data: function\\n        setup_params: function\\n        width: 0\.75\\n        super:  <ggproto object: Class PositionDodge, Position, gg>\\n    print: function\\n    setup_layer: function\\n    show\.legend: NA\\n    stat: <ggproto object: Class StatYdensity, Stat, gg>\\n        aesthetics: function\\n        compute_group: function\\n        compute_layer: function\\n        compute_panel: function\\n        default_aes: uneval\\n        dropped_aes: weight\\n        extra_params: na\.rm orientation\\n        finish_layer: function\\n        non_missing_aes: weight\\n        optional_aes: \\n        parameters: function\\n        required_aes: x y\\n        retransform: TRUE\\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Stat, gg>\\n    stat_params: list\\n    super:  <ggproto object: Class Layer, gg> \\n  \.\.\$ :Classes 'LayerInstance', 'Layer', 'ggproto', 'gg' <ggproto object: Class LayerInstance, Layer, gg>\\n    aes_params: list\\n    compute_aesthetics: function\\n    compute_geom_1: function\\n    compute_geom_2: function\\n    compute_position: function\\n    compute_statistic: function\\n    computed_geom_params: NULL\\n    computed_mapping: NULL\\n    computed_stat_params: NULL\\n    constructor: call\\n    data: data\.frame\\n    draw_geom: function\\n    finish_statistics: function\\n    geom: <ggproto object: Class GeomPoint, Geom, gg>\\n        aesthetics: function\\n        default_aes: uneval\\n        draw_group: function\\n        draw_key: function\\n        draw_layer: function\\n        draw_panel: function\\n        extra_params: na\.rm\\n        handle_na: function\\n        non_missing_aes: size shape colour\\n        optional_aes: \\n        parameters: function\\n        rename_size: FALSE\\n        required_aes: x y\\n        setup_data: function\\n        setup_params: function\\n        use_defaults: function\\n        super:  <ggproto object: Class Geom, gg>\\n    geom_params: list\\n    inherit\.aes: TRUE\\n    layer_data: function\\n    map_statistic: function\\n    mapping: uneval\\n    position: <ggproto object: Class PositionJitterdodge, Position, gg>\\n        compute_layer: function\\n        compute_panel: function\\n        dodge\.width: 0\.75\\n        jitter\.height: 0\\n        jitter\.width: 0\.1\\n        required_aes: x y\\n        seed: 31782059\\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class PositionJitterdodge, Position, gg>\\n    print: function\\n    setup_layer: function\\n    show\.legend: NA\\n    stat: <ggproto object: Class StatIdentity, Stat, gg>\\n        aesthetics: function\\n        compute_group: function\\n        compute_layer: function\\n        compute_panel: function\\n        default_aes: uneval\\n        dropped_aes: \\n        extra_params: na\.rm\\n        finish_layer: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        required_aes: \\n        retransform: TRUE\\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Stat, gg>\\n    stat_params: list\\n    super:  <ggproto object: Class Layer, gg> \\n  \.\.\$ :Classes 'LayerInstance', 'Layer', 'ggproto', 'gg' <ggproto object: Class LayerInstance, Layer, gg>\\n    aes_params: list\\n    compute_aesthetics: function\\n    compute_geom_1: function\\n    compute_geom_2: function\\n    compute_position: function\\n    compute_statistic: function\\n    computed_geom_params: NULL\\n    computed_mapping: NULL\\n    computed_stat_params: NULL\\n    constructor: call\\n    data: waiver\\n    draw_geom: function\\n    finish_statistics: function\\n    geom: <ggproto object: Class GeomPoint, Geom, gg>\\n        aesthetics: function\\n        default_aes: uneval\\n        draw_group: function\\n        draw_key: function\\n        draw_layer: function\\n        draw_panel: function\\n        extra_params: na\.rm\\n        handle_na: function\\n        non_missing_aes: size shape colour\\n        optional_aes: \\n        parameters: function\\n        rename_size: FALSE\\n        required_aes: x y\\n        setup_data: function\\n        setup_params: function\\n        use_defaults: function\\n        super:  <ggproto object: Class Geom, gg>\\n    geom_params: list\\n    inherit\.aes: TRUE\\n    layer_data: function\\n    map_statistic: function\\n    mapping: uneval\\n    position: <ggproto object: Class PositionDodge, Position, gg>\\n        compute_layer: function\\n        compute_panel: function\\n        preserve: total\\n        required_aes: \\n        setup_data: function\\n        setup_params: function\\n        width: 0\.75\\n        super:  <ggproto object: Class PositionDodge, Position, gg>\\n    print: function\\n    setup_layer: function\\n    show\.legend: NA\\n    stat: <ggproto object: Class StatIdentity, Stat, gg>\\n        aesthetics: function\\n        compute_group: function\\n        compute_layer: function\\n        compute_panel: function\\n        default_aes: uneval\\n        dropped_aes: \\n        extra_params: na\.rm\\n        finish_layer: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        required_aes: \\n        retransform: TRUE\\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Stat, gg>\\n    stat_params: list\\n    super:  <ggproto object: Class Layer, gg> \\n  \.\.\$ :Classes 'LayerInstance', 'Layer', 'ggproto', 'gg' <ggproto object: Class LayerInstance, Layer, gg>\\n    aes_params: list\\n    compute_aesthetics: function\\n    compute_geom_1: function\\n    compute_geom_2: function\\n    compute_position: function\\n    compute_statistic: function\\n    computed_geom_params: NULL\\n    computed_mapping: NULL\\n    computed_stat_params: NULL\\n    constructor: call\\n    data: waiver\\n    draw_geom: function\\n    finish_statistics: function\\n    geom: <ggproto object: Class GeomsuperbErrorbar, Geom, gg>\\n        aesthetics: function\\n        default_aes: uneval\\n        draw_group: function\\n        draw_key: function\\n        draw_layer: function\\n        draw_panel: function\\n        extra_params: na\.rm orientation direction tipformat tipgap pointing\\n        handle_na: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        rename_size: FALSE\\n        required_aes: x\|y ymin\|xmin ymax\|xmax\\n        setup_data: function\\n        setup_params: function\\n        use_defaults: function\\n        super:  <ggproto object: Class Geom, gg>\\n    geom_params: list\\n    inherit\.aes: TRUE\\n    layer_data: function\\n    map_statistic: function\\n    mapping: uneval\\n    position: <ggproto object: Class PositionDodge, Position, gg>\\n        compute_layer: function\\n        compute_panel: function\\n        preserve: total\\n        required_aes: \\n        setup_data: function\\n        setup_params: function\\n        width: 0\.75\\n        super:  <ggproto object: Class PositionDodge, Position, gg>\\n    print: function\\n    setup_layer: function\\n    show\.legend: NA\\n    stat: <ggproto object: Class StatIdentity, Stat, gg>\\n        aesthetics: function\\n        compute_group: function\\n        compute_layer: function\\n        compute_panel: function\\n        default_aes: uneval\\n        dropped_aes: \\n        extra_params: na\.rm\\n        finish_layer: function\\n        non_missing_aes: \\n        optional_aes: \\n        parameters: function\\n        required_aes: \\n        retransform: TRUE\\n        setup_data: function\\n        setup_params: function\\n        super:  <ggproto object: Class Stat, gg>\\n    stat_params: list\\n    super:  <ggproto object: Class Layer, gg> \\n \$ scales     :Classes 'ScalesList', 'ggproto', 'gg' <ggproto object: Class ScalesList, gg>\\n    add: function\\n    add_defaults: function\\n    add_missing: function\\n    backtransform_df: function\\n    clone: function\\n    find: function\\n    get_scales: function\\n    has_scale: function\\n    input: function\\n    map_df: function\\n    n: function\\n    non_position_scales: function\\n    scales: list\\n    train_df: function\\n    transform_df: function\\n    super:  <ggproto object: Class ScalesList, gg> \\n \$ guides     :Classes 'Guides', 'ggproto', 'gg' <ggproto object: Class Guides, gg>\\n    add: function\\n    assemble: function\\n    build: function\\n    draw: function\\n    get_custom: function\\n    get_guide: function\\n    get_params: function\\n    get_position: function\\n    guides: NULL\\n    merge: function\\n    missing: <ggproto object: Class GuideNone, Guide, gg>\\n        arrange_layout: function\\n        assemble_drawing: function\\n        available_aes: any\\n        build_decor: function\\n        build_labels: function\\n        build_ticks: function\\n        build_title: function\\n        draw: function\\n        draw_early_exit: function\\n        elements: list\\n        extract_decor: function\\n        extract_key: function\\n        extract_params: function\\n        get_layer_key: function\\n        hashables: list\\n        measure_grobs: function\\n        merge: function\\n        override_elements: function\\n        params: list\\n        process_layers: function\\n        setup_elements: function\\n        setup_params: function\\n        train: function\\n        transform: function\\n        super:  <ggproto object: Class GuideNone, Guide, gg>\\n    package_box: function\\n    print: function\\n    process_layers: function\\n    setup: function\\n    subset_guides: function\\n    train: function\\n    update_params: function\\n    super:  <ggproto object: Class Guides, gg> \\n \$ mapping    :List of 2\\n  \.\.\$ x     : language ~dose\\n  \.\. \.\.- attr\(\*, "\.Environment"\)=<environment: 0x55dd4f0f5040> \\n  \.\.\$ colour: language ~supp\\n  \.\. \.\.- attr\(\*, "\.Environment"\)=<environment: 0x55dd4f0f5040> \\n  \.\.- attr\(\*, "class"\)= chr "uneval"\\n \$ theme      : list\(\)\\n \$ coordinates:Classes 'CoordCartesian', 'Coord', 'ggproto', 'gg' <ggproto object: Class CoordCartesian, Coord, gg>\\n    aspect: function\\n    backtransform_range: function\\n    clip: on\\n    default: TRUE\\n    distance: function\\n    expand: TRUE\\n    is_free: function\\n    is_linear: function\\n    labels: function\\n    limits: list\\n    modify_scales: function\\n    range: function\\n    render_axis_h: function\\n    render_axis_v: function\\n    render_bg: function\\n    render_fg: function\\n    setup_data: function\\n    setup_layout: function\\n    setup_panel_guides: function\\n    setup_panel_params: function\\n    setup_params: function\\n    train_panel_guides: function\\n    transform: function\\n    super:  <ggproto object: Class CoordCartesian, Coord, gg> \\n \$ facet      :Classes 'FacetGrid', 'Facet', 'ggproto', 'gg' <ggproto object: Class FacetGrid, Facet, gg>\\n    compute_layout: function\\n    draw_back: function\\n    draw_front: function\\n    draw_labels: function\\n    draw_panels: function\\n    finish_data: function\\n    init_scales: function\\n    map_data: function\\n    params: list\\n    setup_data: function\\n    setup_params: function\\n    shrink: TRUE\\n    train_scales: function\\n    vars: function\\n    super:  <ggproto object: Class FacetGrid, Facet, gg> \\n \$ plot_env   :<environment: 0x55dd4f0f5040> \\n \$ layout     :Classes 'Layout', 'ggproto', 'gg' <ggproto object: Class Layout, gg>\\n    coord: NULL\\n    coord_params: list\\n    facet: NULL\\n    facet_params: list\\n    finish_data: function\\n    get_scales: function\\n    layout: NULL\\n    map_position: function\\n    panel_params: NULL\\n    panel_scales_x: NULL\\n    panel_scales_y: NULL\\n    render: function\\n    render_labels: function\\n    reset_scales: function\\n    resolve_label: function\\n    setup: function\\n    setup_panel_guides: function\\n    setup_panel_params: function\\n    train_position: function\\n    super:  <ggproto object: Class Layout, gg> \\n \$ labels     :List of 7\\n  \.\.\$ x     : chr "dose"\\n  \.\.\$ colour: chr "supp"\\n  \.\.\$ y     : chr "center"\\n  \.\.\$ fill  : chr "supp"\\n  \.\.\$ group : chr "supp"\\n  \.\.\$ ymin  : chr "center \+ lowerwidth"\\n  \.\.\$ ymax  : chr "center \+ upperwidth"\\n - attr\(\*, "class"\)= chr \[1:2\] "gg" "ggplot""
      Backtrace:
          ▆
       1. └─testthat::expect_output(str(p5), "List of 9") at test_subsidiaryFunctions.R:109:5
       2.   └─testthat::expect_match(...)
       3.     └─testthat:::expect_match_(...)
      
      [ FAIL 5 | WARN 0 | SKIP 0 | PASS 167 ]
      Error: Test failures
      Execution halted
    ```

# superheat

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/superheat
* Date/Publication: 2017-02-04 23:35:29
* Number of recursive dependencies: 72

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
    > # cluster by species and plot Sepal.Length on the right
    > # save the superheat object to access the membership vectors
    > sh <- superheat(X = iris[,-c(1, 5)],
    +                 yr = iris[,1],
    +                 yr.axis.name = "Sepal.Length",
    +                 membership.rows = iris$Species)
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Calls: superheat ... do.call -> <Anonymous> -> <Anonymous> -> startsWith
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
      [ FAIL 58 | WARN 18 | SKIP 0 | PASS 0 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
          ▆
       1. └─superheat::superheat(X, row.title = "rowname", yt = 1:3, bottom.label.size = 0.3) at test-layout-examples.R:235:3
       2.   ├─base::do.call(generate_heat, heat.arg.list)
       3.   └─superheat (local) `<fn>`(...)
       4.     └─ggplot2::scale_y_continuous(name = "", expand = c(0, 0))
       5.       └─base::startsWith(as.character(call[[1]]), "scale_")
      
      [ FAIL 58 | WARN 18 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# survivalAnalysis

<details>

* Version: 0.3.0
* GitHub: NA
* Source code: https://github.com/cran/survivalAnalysis
* Date/Publication: 2022-02-11 14:00:02 UTC
* Number of recursive dependencies: 160

Run `revdepcheck::cloud_details(, "survivalAnalysis")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘survivalAnalysis-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: kaplan_meier_plot
    > ### Title: Kaplan Meier plots from survival results.
    > ### Aliases: kaplan_meier_plot
    > 
    > ### ** Examples
    > 
    > library(magrittr)
    ...
    > survival::aml %>%
    +   analyse_survival(vars(time, status), x) %>%
    +   kaplan_meier_plot(break.time.by="breakByMonth",
    +                     xlab=".OS.months",
    +                     risk.table=TRUE,
    +                     ggtheme=ggplot2::theme_bw(10))
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Calls: %>% ... do.call -> <Anonymous> -> <Anonymous> -> startsWith
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘multivariate.Rmd’ using rmarkdown
    --- finished re-building ‘multivariate.Rmd’
    
    --- re-building ‘univariate.Rmd’ using rmarkdown
    
    Quitting from lines 118-126 [unnamed-chunk-9] (univariate.Rmd)
    Error: processing vignette 'univariate.Rmd' failed with diagnostics:
    cannot coerce type 'closure' to vector of type 'character'
    --- failed re-building ‘univariate.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘univariate.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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

*   checking examples ... ERROR
    ```
    Running examples in ‘survminer-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: BRCAOV.survInfo
    > ### Title: Breast and Ovarian Cancers Survival Information
    > ### Aliases: BRCAOV.survInfo
    > 
    > ### ** Examples
    > 
    > data(BRCAOV.survInfo)
    > library(survival)
    > fit <- survfit(Surv(times, patient.vital_status) ~ admin.disease_code,
    +            data = BRCAOV.survInfo)
    > ggsurvplot(fit, data = BRCAOV.survInfo, risk.table = TRUE)
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Calls: ggsurvplot ... do.call -> <Anonymous> -> <Anonymous> -> startsWith
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(survminer)
      Loading required package: ggplot2
      Loading required package: ggpubr
      > 
      > test_check("survminer")
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 0 ]
    ...
       1. └─survminer::ggrisktable(fit, data = lung, color = "strata") at test-ggsurvtable.R:9:3
       2.   └─survminer::ggsurvtable(fit, data, survtable = "risk.table", ...)
       3.     ├─base::do.call(.plot_survtable, opts)
       4.     └─survminer (local) `<fn>`(...)
       5.       └─ggplot2::scale_x_continuous(...)
       6.         └─base::startsWith(as.character(call[[1]]), "scale_")
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Informative_Survival_Plots.Rmd’ using rmarkdown
    
    Quitting from lines 66-72 [unnamed-chunk-4] (Informative_Survival_Plots.Rmd)
    Error: processing vignette 'Informative_Survival_Plots.Rmd' failed with diagnostics:
    cannot coerce type 'closure' to vector of type 'character'
    --- failed re-building ‘Informative_Survival_Plots.Rmd’
    
    --- re-building ‘Playing_with_fonts_and_texts.Rmd’ using rmarkdown
    
    ...
    --- finished re-building ‘Specifiying_weights_in_log-rank_comparisons.Rmd’
    
    --- re-building ‘ggforest-show-interactions-hazard-ratio.Rmd’ using rmarkdown
    --- finished re-building ‘ggforest-show-interactions-hazard-ratio.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘Informative_Survival_Plots.Rmd’ ‘Playing_with_fonts_and_texts.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        doc   5.5Mb
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
      8.       └─ggplot2:::validate_theme(params$theme)
      9.         └─base::mapply(validate_element, theme, names(theme), MoreArgs = list(element_tree = tree))
     10.           └─ggplot2 (local) `<fn>`(dots[[1L]][[1L]], dots[[2L]][[1L]], element_tree = `<named list>`)
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

*   checking re-building of vignette outputs ... WARNING
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

# tidyLPA

<details>

* Version: 1.1.0
* GitHub: https://github.com/data-edu/tidyLPA
* Source code: https://github.com/cran/tidyLPA
* Date/Publication: 2021-11-17 11:40:02 UTC
* Number of recursive dependencies: 102

Run `revdepcheck::cloud_details(, "tidyLPA")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(tidyLPA)
      You can use the function citation('tidyLPA') to create a citation for the use of {tidyLPA}.
      Mplus is not installed. Use only package = 'mclust' when calling estimate_profiles().
      > options("test_mplus" = FALSE)
      > if(file.exists("test_local.R")) source("test_local.R")
      > # To run all mplus tests, run the code below once to create a file that is not
    ...
       4.   ├─tidyLPA::plot_density(variables = "Petal.Length", x = `<df[,5]>`)
       5.   └─tidyLPA:::plot_density.default(variables = "Petal.Length", x = `<df[,5]>`)
       6.     ├─base::do.call(.plot_density_fun, Args)
       7.     └─tidyLPA (local) `<fn>`(variables = "Petal.Length", plot_df = `<df[,5]>`)
       8.       └─ggplot2::scale_x_continuous(expand = c(0, 0))
       9.         └─base::startsWith(as.character(call[[1]]), "scale_")
      
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 10 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘mix’
      All declared Imports should be used.
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

*   checking re-building of vignette outputs ... WARNING
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

# tidyseurat

<details>

* Version: 0.7.9
* GitHub: https://github.com/stemangiola/tidyseurat
* Source code: https://github.com/cran/tidyseurat
* Date/Publication: 2023-11-28 03:20:02 UTC
* Number of recursive dependencies: 206

Run `revdepcheck::cloud_details(, "tidyseurat")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘figures_article.Rmd’ using knitr
    --- finished re-building ‘figures_article.Rmd’
    
    --- re-building ‘introduction.Rmd’ using knitr
    
    Quitting from lines 172-177 [pc_plot] (./../man/fragments/intro.Rmd)
    
    Quitting from lines 172-177 [unnamed-chunk-2] (./../man/fragments/intro.Rmd)
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    cannot coerce type 'closure' to vector of type 'character'
    --- failed re-building ‘introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking S3 generic/method consistency ... WARNING
    ```
    Warning: declared S3 method 'filter.Seurat' not found
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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

*   checking re-building of vignette outputs ... WARNING
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
      comparison (1) is possible only for atomic and list types
    Calls: draw_heat ... ggplot_add.new_aes -> bump_aes_scales -> lapply -> FUN -> isTRUE
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘explore.Rmd’ using rmarkdown
    
    Quitting from lines 57-74 [unnamed-chunk-4] (explore.Rmd)
    Error: processing vignette 'explore.Rmd' failed with diagnostics:
    comparison (1) is possible only for atomic and list types
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

*   checking re-building of vignette outputs ... WARNING
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

# tsfeatures

<details>

* Version: 1.1.1
* GitHub: https://github.com/robjhyndman/tsfeatures
* Source code: https://github.com/cran/tsfeatures
* Date/Publication: 2023-08-28 14:00:02 UTC
* Number of recursive dependencies: 114

Run `revdepcheck::cloud_details(, "tsfeatures")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘tsfeatures.Rmd’ using rmarkdown
    trying URL 'https://github.com/robjhyndman/tsfeatures/raw/master/extra-data/yahoo.rda'
    Content type 'application/octet-stream' length 7897660 bytes (7.5 MB)
    ==================================================
    downloaded 7.5 MB
    
    trying URL 'https://github.com/robjhyndman/tsfeatures/raw/master/extra-data/hwl.rda'
    Content type 'application/octet-stream' length 185319 bytes (180 KB)
    ==================================================
    ...
    Quitting from lines 426-440 [ijf2017graphs] (tsfeatures.Rmd)
    Error: processing vignette 'tsfeatures.Rmd' failed with diagnostics:
    cannot coerce type 'closure' to vector of type 'character'
    --- failed re-building ‘tsfeatures.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘tsfeatures.Rmd’
    
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

*   checking re-building of vignette outputs ... WARNING
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
      installed size is  9.0Mb
      sub-directories of 1Mb or more:
        libs   8.0Mb
    ```

# ushr

<details>

* Version: 0.2.3
* GitHub: https://github.com/SineadMorris/ushr
* Source code: https://github.com/cran/ushr
* Date/Publication: 2020-04-21 18:20:03 UTC
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "ushr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ushr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_pairs
    > ### Title: Plot pairwise parameter distributions
    > ### Aliases: plot_pairs
    > 
    > ### ** Examples
    > 
    > 
    ...
    Warning in sqrt(diag(fisher_info)) : NaNs produced
    Warning in sqrt(diag(fisher_info)) : NaNs produced
    Warning in sqrt(diag(fisher_info)) : NaNs produced
    Warning in sqrt(diag(fisher_info)) : NaNs produced
    > 
    > plot_pairs(model_output)
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Calls: <Anonymous> ... do.call -> <Anonymous> -> scale_y_continuous -> startsWith
    Execution halted
    ```

# usincometaxes

<details>

* Version: 0.7.0
* GitHub: https://github.com/shanejorr/usincometaxes
* Source code: https://github.com/cran/usincometaxes
* Date/Publication: 2023-08-19 21:20:02 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "usincometaxes")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘send-data-to-taxsim.Rmd’ using rmarkdown
    --- finished re-building ‘send-data-to-taxsim.Rmd’
    
    --- re-building ‘taxsim-input.Rmd’ using rmarkdown
    --- finished re-building ‘taxsim-input.Rmd’
    
    --- re-building ‘taxsim-output.Rmd’ using rmarkdown
    --- finished re-building ‘taxsim-output.Rmd’
    ...
    --- failed re-building ‘using-usincometaxes.Rmd’
    
    --- re-building ‘wasm.Rmd’ using rmarkdown
    --- finished re-building ‘wasm.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘using-usincometaxes.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# vDiveR

<details>

* Version: 1.2.0
* GitHub: NA
* Source code: https://github.com/cran/vDiveR
* Date/Publication: 2023-09-12 05:10:02 UTC
* Number of recursive dependencies: 132

Run `revdepcheck::cloud_details(, "vDiveR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘vDiveR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_entropy
    > ### Title: Entropy plot
    > ### Aliases: plot_entropy
    > 
    > ### ** Examples
    > 
    > plot_entropy(proteins_1host)
    Error in as.character(call[[1]]) : 
      cannot coerce type 'closure' to vector of type 'character'
    Calls: plot_entropy ... mapply -> <Anonymous> -> scale_x_continuous -> startsWith
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DT’ ‘maps’ ‘readr’
      All declared Imports should be used.
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

*   checking re-building of vignette outputs ... WARNING
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
* Number of recursive dependencies: 219

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

*   checking re-building of vignette outputs ... WARNING
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
      installed size is 25.8Mb
      sub-directories of 1Mb or more:
        libs  24.2Mb
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
      
      [ FAIL 3 | WARN 7 | SKIP 1 | PASS 74 ]
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

# wql

<details>

* Version: 1.0.0
* GitHub: https://github.com/jsta/wql
* Source code: https://github.com/cran/wql
* Date/Publication: 2022-08-10 16:20:02 UTC
* Number of recursive dependencies: 56

Run `revdepcheck::cloud_details(, "wql")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘wql-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: seasonTrend
    > ### Title: Determine seasonal trends
    > ### Aliases: seasonTrend
    > ### Keywords: Graphics ts
    > 
    > ### ** Examples
    > 
    ...
     21. └─vctrs (local) `<fn>`()
     22.   └─vctrs::vec_default_ptype2(...)
     23.     ├─base::withRestarts(...)
     24.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     25.     │   └─base (local) doWithOneRestart(return(expr), restart)
     26.     └─vctrs::stop_incompatible_type(...)
     27.       └─vctrs:::stop_incompatible(...)
     28.         └─vctrs:::stop_vctrs(...)
     29.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘wql-package.Rmd’ using rmarkdown
    
    Quitting from lines 216-218 [unnamed-chunk-22] (wql-package.Rmd)
    Error: processing vignette 'wql-package.Rmd' failed with diagnostics:
    Can't combine <character> and <logical>.
    --- failed re-building ‘wql-package.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘wql-package.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# wqtrends

<details>

* Version: 1.4.1
* GitHub: NA
* Source code: https://github.com/cran/wqtrends
* Date/Publication: 2023-08-17 15:20:02 UTC
* Number of recursive dependencies: 91

Run `revdepcheck::cloud_details(, "wqtrends")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘wqtrends-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: show_sumtrndseason
    > ### Title: Plot seasonal rates of change based on average estimates for
    > ###   multiple window widths
    > ### Aliases: show_sumtrndseason
    > 
    > ### ** Examples
    > 
    ...
    ! Argument 1 can't be empty.
    Backtrace:
        ▆
     1. ├─wqtrends::show_sumtrndseason(...)
     2. │ └─ggplot2::theme(legend.position = "top", )
     3. │   └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
     4. │     ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
     5. │     └─rlang::list2(..., ... = NULL)
     6. └─rlang::abort(message = message)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(wqtrends)
      > 
      > test_check("wqtrends")
      reducing knots for cont_year spline from 1000 
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 50 ]
      
    ...
       1. ├─wqtrends::show_sumtrndseason(...) at test-show_sumtrndseason.R:3:3
       2. │ └─ggplot2::theme(legend.position = "top", )
       3. │   └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
       4. │     ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
       5. │     └─rlang::list2(..., ... = NULL)
       6. └─rlang::abort(message = message)
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 50 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘introduction.Rmd’ using rmarkdown
    
    Quitting from lines 174-175 [unnamed-chunk-19] (introduction.Rmd)
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    Argument 1 can't be empty.
    --- failed re-building ‘introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘baytrends’
    ```

# xadmix

<details>

* Version: 1.0.0
* GitHub: https://github.com/SpaceCowboy-71/xadmix
* Source code: https://github.com/cran/xadmix
* Date/Publication: 2022-07-08 15:20:02 UTC
* Number of recursive dependencies: 61

Run `revdepcheck::cloud_details(, "xadmix")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘xadmix-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: admix_barplot
    > ### Title: Admixture Data Stacked Barplot
    > ### Aliases: admix_barplot
    > 
    > ### ** Examples
    > 
    > # load simulated admixture data
    ...
    ! Argument 1 can't be empty.
    Backtrace:
        ▆
     1. ├─xadmix::admix_barplot(xadmixture, K = 4:ncol(xadmixture), names = FALSE)
     2. │ └─ggplot2::theme(axis.text.x = element_blank(), )
     3. │   └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
     4. │     ├─ggplot2:::modify_list(vals, list2(..., ... = NULL))
     5. │     └─rlang::list2(..., ... = NULL)
     6. └─rlang::abort(message = message)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘xadmix-manual.Rmd’ using rmarkdown
    
    Quitting from lines 96-102 [unnamed-chunk-5] (xadmix-manual.Rmd)
    Error: processing vignette 'xadmix-manual.Rmd' failed with diagnostics:
    Argument 1 can't be empty.
    --- failed re-building ‘xadmix-manual.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘xadmix-manual.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# yamlet

<details>

* Version: 0.10.33
* GitHub: https://github.com/bergsmat/yamlet
* Source code: https://github.com/cran/yamlet
* Date/Publication: 2023-10-06 04:40:02 UTC
* Number of recursive dependencies: 95

Run `revdepcheck::cloud_details(, "yamlet")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘yamlet-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggplot.decorated
    > ### Title: Create a New ggplot for a Decorated Data Frame
    > ### Aliases: ggplot.decorated
    > 
    > ### ** Examples
    > 
    > file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
    ...
     25. └─vctrs (local) `<fn>`()
     26.   └─vctrs::vec_default_ptype2(...)
     27.     ├─base::withRestarts(...)
     28.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
     29.     │   └─base (local) doWithOneRestart(return(expr), restart)
     30.     └─vctrs::stop_incompatible_type(...)
     31.       └─vctrs:::stop_incompatible(...)
     32.         └─vctrs:::stop_vctrs(...)
     33.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(yamlet)
      
      Attaching package: 'yamlet'
      
      The following object is masked from 'package:stats':
      
    ...
       33.     │ └─base (local) withOneRestart(expr, restarts[[1L]])
       34.     │   └─base (local) doWithOneRestart(return(expr), restart)
       35.     └─vctrs::stop_incompatible_type(...)
       36.       └─vctrs:::stop_incompatible(...)
       37.         └─vctrs:::stop_vctrs(...)
       38.           └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
      
      [ FAIL 1 | WARN 1 | SKIP 2 | PASS 509 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘yamlet-introduction.Rmd’ using rmarkdown
    
    Quitting from lines 321-333 [unnamed-chunk-9] (yamlet-introduction.Rmd)
    Error: processing vignette 'yamlet-introduction.Rmd' failed with diagnostics:
    Can't combine <character> and <classified>.
    --- failed re-building ‘yamlet-introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘yamlet-introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

