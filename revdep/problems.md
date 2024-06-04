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
      installed size is 19.7Mb
      sub-directories of 1Mb or more:
        data   2.1Mb
        libs  16.5Mb
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

# bdscale

<details>

* Version: 2.0.0
* GitHub: https://github.com/dvmlls/bdscale
* Source code: https://github.com/cran/bdscale
* Date/Publication: 2016-03-17 13:27:37
* Number of recursive dependencies: 63

Run `revdepcheck::cloud_details(, "bdscale")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘bdscale.Rmd’
      ...
    > options <- as.Date(c("2014-08-15", "2014-09-19"))
    
    > plot + geom_vline(xintercept = as.numeric(options), 
    +     size = 2, alpha = 0.25) + ggtitle("calendar dates, option expiry")
    Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ℹ Please use `linewidth` instead.
    
      When sourcing ‘bdscale.R’:
    Error: `transform_date()` works with objects of class <Date> only
    Execution halted
    
      ‘bdscale.Rmd’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘bdscale.Rmd’ using rmarkdown
    ```

# biclustermd

<details>

* Version: 0.2.3
* GitHub: https://github.com/jreisner/biclustermd
* Source code: https://github.com/cran/biclustermd
* Date/Publication: 2021-06-17 15:10:06 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::cloud_details(, "biclustermd")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(biclustermd)
      Loading required package: ggplot2
      Loading required package: tidyr
      
      Attaching package: 'tidyr'
      
    ...
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 67 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-autoplot_biclustermd.R:6:3'): autoplot_biclustermd() correctly plots cluster lines ──
      ap$data[[3]]$xintercept[-1] not equal to cumsum(colSums(sbc$P)) + 0.5.
      Classes differ: 'mapped_discrete'/'numeric' is not 'numeric'
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 67 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘nycflights13’
      All declared Imports should be used.
    ```

# brolgar

<details>

* Version: 1.0.1
* GitHub: https://github.com/njtierney/brolgar
* Source code: https://github.com/cran/brolgar
* Date/Publication: 2024-05-10 14:50:34 UTC
* Number of recursive dependencies: 101

Run `revdepcheck::cloud_details(, "brolgar")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘brolgar-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: facet_sample
    > ### Title: Facet data into groups to facilitate exploration
    > ### Aliases: facet_sample
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    > ggplot(heights,
    + aes(x = year,
    +     y = height_cm,
    +     group = country)) +
    +   geom_line() +
    +   facet_sample()
    Error in if (params$as.table) { : argument is of length zero
    Calls: <Anonymous> ... <Anonymous> -> setup -> <Anonymous> -> compute_layout
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘getting-started.Rmd’
      ...
    > set.seed(2019 - 7 - 23 - 1936)
    
    > library(ggplot2)
    
    > ggplot(wages, aes(x = xp, y = ln_wages, group = id)) + 
    +     geom_line() + facet_strata()
    
    ...
    Error: argument is of length zero
    Execution halted
    
      ‘exploratory-modelling.Rmd’ using ‘UTF-8’... OK
      ‘finding-features.Rmd’ using ‘UTF-8’... OK
      ‘getting-started.Rmd’ using ‘UTF-8’... failed
      ‘id-interesting-obs.Rmd’ using ‘UTF-8’... OK
      ‘longitudinal-data-structures.Rmd’ using ‘UTF-8’... OK
      ‘mixed-effects-models.Rmd’ using ‘UTF-8’... failed
      ‘visualisation-gallery.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘exploratory-modelling.Rmd’ using rmarkdown
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

# CausalImpact

<details>

* Version: 1.3.0
* GitHub: NA
* Source code: https://github.com/cran/CausalImpact
* Date/Publication: 2022-11-09 08:40:40 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::cloud_details(, "CausalImpact")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘CausalImpact_import_test.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # Copyright 2014-2022 Google Inc. All rights reserved.
      > #
      > # Licensed under the Apache License, Version 2.0 (the "License");
      > # you may not use this file except in compliance with the License.
      > # You may obtain a copy of the License at
      > #
    ...
       20.                   └─base::lapply(df[aesthetics], self$transform)
       21.                     └─ggplot2 (local) FUN(X[[i]], ...)
       22.                       └─ggplot2 (local) transform(..., self = self)
       23.                         └─transformation$transform(x)
       24.                           └─cli::cli_abort("{.fun transform_date} works with objects of class {.cls Date} only")
       25.                             └─rlang::abort(...)
      
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 739 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘CausalImpact.Rmd’
      ...
    
    > impact <- CausalImpact(data, pre.period, post.period)
    
    > q <- plot(impact) + theme_bw(base_size = 11)
    
    > suppressWarnings(plot(q))
    
      When sourcing ‘CausalImpact.R’:
    Error: `transform_date()` works with objects of class <Date> only
    Execution halted
    
      ‘CausalImpact.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘CausalImpact.Rmd’ using rmarkdown
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

# CLVTools

<details>

* Version: 0.10.0
* GitHub: https://github.com/bachmannpatrick/CLVTools
* Source code: https://github.com/cran/CLVTools
* Date/Publication: 2023-10-23 20:10:02 UTC
* Number of recursive dependencies: 85

Run `revdepcheck::cloud_details(, "CLVTools")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘CLVTools.Rmd’
      ...
    248:                39.95483      2.336902
    249:                34.28958     38.969738
    250:                47.35500      7.284870
    
    > plot(clv.apparel)
    Plotting from 2005-01-03 until 2006-07-16.
    
      When sourcing ‘CLVTools.R’:
    Error: `transform_date()` works with objects of class <Date> only
    Execution halted
    
      ‘CLVTools.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘CLVTools.Rmd’ using rmarkdown
    
    Quitting from lines 270-272 [plot-actual] (CLVTools.Rmd)
    Error: processing vignette 'CLVTools.Rmd' failed with diagnostics:
    `transform_date()` works with objects of class <Date> only
    --- failed re-building ‘CLVTools.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘CLVTools.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 17.4Mb
      sub-directories of 1Mb or more:
        libs  15.5Mb
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
    when running code in ‘plotting-signals.Rmd’
      ...
    > plot(inum, choro_col = colors, choro_params = list(breaks = breaks), 
    +     title = "New COVID cases (7-day trailing average) on 2020-07-14")
    
      When sourcing ‘plotting-signals.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 5th layer.
    Caused by error in `check_aesthetics()`:
    ! Aesthetics must be either length 1 or the same as the data (3078).
    ✖ Fix the following mappings: `fill`.
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

# EpiCurve

<details>

* Version: 2.4-2
* GitHub: https://github.com/IamKDO/EpiCurve
* Source code: https://github.com/cran/EpiCurve
* Date/Publication: 2021-07-14 16:20:05 UTC
* Number of recursive dependencies: 56

Run `revdepcheck::cloud_details(, "EpiCurve")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘EpiCurve.Rmd’
      ...
    |2016-11-06 |  6.60| 116|
    |2016-11-07 |  7.68| 141|
    |2016-11-07 | 10.08| 126|
    
    > EpiCurve(DF, date = "UTS", period = "day", colors = "#9900ef", 
    +     xlabel = sprintf("From %s to %s", min(DF$UTS), max(DF$UTS)))
    
      When sourcing ‘EpiCurve.R’:
    Error: `transform_date()` works with objects of class <Date> only
    Execution halted
    
      ‘EpiCurve.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘EpiCurve.Rmd’ using rmarkdown
    
    Quitting from lines 103-106 [unnamed-chunk-3] (EpiCurve.Rmd)
    Error: processing vignette 'EpiCurve.Rmd' failed with diagnostics:
    `transform_date()` works with objects of class <Date> only
    --- failed re-building ‘EpiCurve.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘EpiCurve.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
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
    +     geom_histogram(binwidth = 7, colour = "gray", fill = "dark blue", 
    +         li .... [TRUNCATED] 
    
    > ggplot(data = dat.df, aes(x = as.Date(odate))) + theme_bw() + 
    +     geom_histogram(binwidth = 7, colour = "gray", fill = "dark blue", 
    +         li .... [TRUNCATED] 
    
      When sourcing ‘epiR_descriptive.R’:
    Error: `transform_date()` works with objects of class <Date> only
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

# geostan

<details>

* Version: 0.6.1
* GitHub: https://github.com/ConnorDonegan/geostan
* Source code: https://github.com/cran/geostan
* Date/Publication: 2024-05-10 22:23:01 UTC
* Number of recursive dependencies: 108

Run `revdepcheck::cloud_details(, "geostan")` for more info

</details>

## Newly broken

*   checking whether package ‘geostan’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/geostan/new/geostan.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is 129.8Mb
      sub-directories of 1Mb or more:
        libs  127.6Mb
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

## Installation

### Devel

```
* installing *source* package ‘geostan’ ...
** package ‘geostan’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++17


g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -I"../inst/include" -I"/opt/R/4.3.1/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -DUSE_STANC3 -D_HAS_AUTO_PTR_ETC=0 -I'/opt/R/4.3.1/lib/R/site-library/BH/include' -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppParallel/include' -I'/opt/R/4.3.1/lib/R/site-library/rstan/include' -I'/opt/R/4.3.1/lib/R/site-library/StanHeaders/include' -I/usr/local/include    -I'/opt/R/4.3.1/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from /opt/R/4.3.1/lib/R/site-library/RcppEigen/include/Eigen/Core:205,
...
/opt/R/4.3.1/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:654:74: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
  654 |   return internal::first_aligned<int(unpacket_traits<DefaultPacketType>::alignment),Derived>(m);
      |                                                                          ^~~~~~~~~
stanExports_foundation.cc:32:1: fatal error: error writing to /tmp/cccy6wQ6.s: Cannot allocate memory
   32 | }
      | ^
compilation terminated.
make: *** [/opt/R/4.3.1/lib/R/etc/Makeconf:198: stanExports_foundation.o] Error 1
ERROR: compilation failed for package ‘geostan’
* removing ‘/tmp/workdir/geostan/new/geostan.Rcheck/geostan’


```
### CRAN

```
* installing *source* package ‘geostan’ ...
** package ‘geostan’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++17


g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -I"../inst/include" -I"/opt/R/4.3.1/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -DUSE_STANC3 -D_HAS_AUTO_PTR_ETC=0 -I'/opt/R/4.3.1/lib/R/site-library/BH/include' -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppParallel/include' -I'/opt/R/4.3.1/lib/R/site-library/rstan/include' -I'/opt/R/4.3.1/lib/R/site-library/StanHeaders/include' -I/usr/local/include    -I'/opt/R/4.3.1/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from /opt/R/4.3.1/lib/R/site-library/RcppEigen/include/Eigen/Core:205,
...
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (geostan)


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
    
    > ### Name: print.ggedit
    > ### Title: Print ggedit objects
    > ### Aliases: print.ggedit
    > 
    > ### ** Examples
    > 
    > p <- as.gglist(pList[1:2])
    ...
      8. │       └─ggplot2 (local) setup(..., self = self)
      9. │         └─self$facet$compute_layout(data, self$facet_params)
     10. │           └─ggplot2 (local) compute_layout(..., self = self)
     11. │             └─ggplot2:::wrap_layout(id, dims, params$dir)
     12. │               └─ggplot2:::data_frame0(...)
     13. │                 └─vctrs::data_frame(..., .name_repair = "minimal")
     14. └─vctrs:::stop_recycle_incompatible_size(...)
     15.   └─vctrs:::stop_vctrs(...)
     16.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
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
      ----- FAILED[]: test_ggcoefplot.R<66--66>
       call| expect_snapshot_plot(p_did_iid_summ, label = "ggcoefplot_did_iid")
       diff| 7580
       info| Diff plot saved to: _tinysnapshot_review/ggcoefplot_did_iid.png
      ----- FAILED[]: test_ggcoefplot.R<67--67>
       call| expect_snapshot_plot(p_did_iid, label = "ggcoefplot_did_iid")
       diff| 7580
       info| Diff plot saved to: _tinysnapshot_review/ggcoefplot_did_iid.png
      Error: 12 out of 101 tests failed
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

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘basics.Rmd’ using knitr
    
    Attaching package: 'zoo'
    
    The following objects are masked from 'package:base':
    
        as.Date, as.Date.numeric
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
      [ FAIL 8 | WARN 0 | SKIP 18 | PASS 723 ]
      
    ...
       11. │           └─ggplot2:::wrap_layout(id, dims, params$dir)
       12. │             └─ggplot2:::data_frame0(...)
       13. │               └─vctrs::data_frame(..., .name_repair = "minimal")
       14. └─vctrs:::stop_recycle_incompatible_size(...)
       15.   └─vctrs:::stop_vctrs(...)
       16.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
      
      [ FAIL 8 | WARN 0 | SKIP 18 | PASS 723 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Facets.Rmd’ using rmarkdown
    
    Quitting from lines 33-39 [wrap_mimick] (Facets.Rmd)
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
      ‘Miscellaneous.Rmd’ using ‘UTF-8’... OK
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

# iglu

<details>

* Version: 4.0.0
* GitHub: https://github.com/irinagain/iglu
* Source code: https://github.com/cran/iglu
* Date/Publication: 2024-02-23 17:50:02 UTC
* Number of recursive dependencies: 125

Run `revdepcheck::cloud_details(, "iglu")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘iglu-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: mage_ma_single
    > ### Title: Calculates Mean Amplitude of Glycemic Excursions (see "mage")
    > ### Aliases: mage_ma_single
    > 
    > ### ** Examples
    > 
    > data(example_data_1_subject)
    ...
     12.                 └─ggplot2 (local) transform_df(..., self = self)
     13.                   └─base::lapply(df[aesthetics], self$transform)
     14.                     └─ggplot2 (local) FUN(X[[i]], ...)
     15.                       └─ggplot2 (local) transform(..., self = self)
     16.                         └─ggproto_parent(ScaleContinuous, self)$transform(x)
     17.                           └─ggplot2 (local) transform(..., self = self)
     18.                             └─transformation$transform(x)
     19.                               └─cli::cli_abort("{.fun transform_time} works with objects of class {.cls POSIXct} only")
     20.                                 └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘MAGE.Rmd’
      ...
    
    > fig1data <- example_data_1_subject[1:200, ]
    
    > mage(fig1data, plot = TRUE, show_ma = TRUE, title = "Glucose Trace - Subject 1")
    
      When sourcing ‘MAGE.R’:
    Error: ℹ In index: 1.
    Caused by error in `transformation$transform()`:
    ! `transform_time()` works with objects of class <POSIXct> only
    Execution halted
    
      ‘AGP_and_Episodes.Rmd’ using ‘UTF-8’... OK
      ‘MAGE.Rmd’ using ‘UTF-8’... failed
      ‘iglu.Rmd’ using ‘UTF-8’... OK
      ‘lasagna_plots.Rmd’ using ‘UTF-8’... OK
      ‘metrics_list.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘AGP_and_Episodes.Rmd’ using rmarkdown
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

# MarketMatching

<details>

* Version: 1.2.1
* GitHub: NA
* Source code: https://github.com/cran/MarketMatching
* Date/Publication: 2024-01-31 09:40:02 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::cloud_details(, "MarketMatching")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘MarketMatching-Vignette.Rmd’ using rmarkdown_notangle
    
    Quitting from lines 114-115 [unnamed-chunk-3] (MarketMatching-Vignette.Rmd)
    Error: processing vignette 'MarketMatching-Vignette.Rmd' failed with diagnostics:
    `transform_date()` works with objects of class <Date> only
    --- failed re-building ‘MarketMatching-Vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘MarketMatching-Vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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

# MIMSunit

<details>

* Version: 0.11.2
* GitHub: https://github.com/mhealthgroup/MIMSunit
* Source code: https://github.com/cran/MIMSunit
* Date/Publication: 2022-06-21 11:00:09 UTC
* Number of recursive dependencies: 114

Run `revdepcheck::cloud_details(, "MIMSunit")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘MIMSunit-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: illustrate_extrapolation
    > ### Title: Plot illustrations about extrapolation in illustration style.
    > ### Aliases: illustrate_extrapolation
    > 
    > ### ** Examples
    > 
    >   # Use the maxed-out data for the conceptual diagram
    ...
     12.                 └─ggplot2 (local) transform_df(..., self = self)
     13.                   └─base::lapply(df[aesthetics], self$transform)
     14.                     └─ggplot2 (local) FUN(X[[i]], ...)
     15.                       └─ggplot2 (local) transform(..., self = self)
     16.                         └─ggproto_parent(ScaleContinuous, self)$transform(x)
     17.                           └─ggplot2 (local) transform(..., self = self)
     18.                             └─transformation$transform(x)
     19.                               └─cli::cli_abort("{.fun transform_time} works with objects of class {.cls POSIXct} only")
     20.                                 └─rlang::abort(...)
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

*   checking examples ... ERROR
    ```
    Running examples in ‘nzelect-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: polls
    > ### Title: New Zealand Opinion Polls
    > ### Aliases: polls
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    ...
     10.             └─ggplot2 (local) FUN(X[[i]], ...)
     11.               └─scale$transform_df(df = df)
     12.                 └─ggplot2 (local) transform_df(..., self = self)
     13.                   └─base::lapply(df[aesthetics], self$transform)
     14.                     └─ggplot2 (local) FUN(X[[i]], ...)
     15.                       └─ggplot2 (local) transform(..., self = self)
     16.                         └─transformation$transform(x)
     17.                           └─cli::cli_abort("{.fun transform_date} works with objects of class {.cls Date} only")
     18.                             └─rlang::abort(...)
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

# posologyr

<details>

* Version: 1.2.4
* GitHub: https://github.com/levenc/posologyr
* Source code: https://github.com/cran/posologyr
* Date/Publication: 2024-02-09 11:50:02 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "posologyr")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘a_priori_dosing.Rmd’
      ...
    16: source(output, echo = TRUE)
    17: doTryCatch(return(expr), name, parentenv, handler)
    18: tryCatchOne(expr, names, parentenv, handlers[[1L]])
    19: tryCatchList(expr, classes, parentenv, handlers)
    20: tryCatch({    source(output, echo = TRUE)}, error = function(e) {    cat("\n  When sourcing ", sQuote(output), ":\n", sep = "")    stop(conditionMessage(e), call. = FALSE, domain = NA)})
    21: tools:::.run_one_vignette("a_priori_dosing.Rmd", "/tmp/workdir/posologyr/new/posologyr.Rcheck/00_pkg_src/posologyr/vignettes",     encoding = "UTF-8", pkgdir = "/tmp/workdir/posologyr/new/posologyr.Rcheck/00_pkg_src/posologyr")
    An irrecoverable exception occurred. R is aborting now ...
    ...
    Segmentation fault
    
    ... incomplete output.  Crash?
    
      ‘a_posteriori_dosing.Rmd’ using ‘UTF-8’... OK
      ‘a_priori_dosing.Rmd’ using ‘UTF-8’... failed to complete the test
      ‘auc_based_dosing.Rmd’ using ‘UTF-8’... OK
      ‘multiple_endpoints.Rmd’ using ‘UTF-8’... OK
      ‘patient_data_input.Rmd’ using ‘UTF-8’... OK
      ‘posologyr_user_defined_models.Rmd’ using ‘UTF-8’... OK
    ```

# qicharts

<details>

* Version: 0.5.8
* GitHub: NA
* Source code: https://github.com/cran/qicharts
* Date/Publication: 2021-04-20 12:20:03 UTC
* Number of recursive dependencies: 56

Run `revdepcheck::cloud_details(, "qicharts")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘qicharts-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: tcc
    > ### Title: Trellis Control Charts
    > ### Aliases: tcc
    > 
    > ### ** Examples
    > 
    > # Run chart of 24 random normal variables
    ...
     10.             └─ggplot2 (local) FUN(X[[i]], ...)
     11.               └─scale$transform_df(df = df)
     12.                 └─ggplot2 (local) transform_df(..., self = self)
     13.                   └─base::lapply(df[aesthetics], self$transform)
     14.                     └─ggplot2 (local) FUN(X[[i]], ...)
     15.                       └─ggplot2 (local) transform(..., self = self)
     16.                         └─transformation$transform(x)
     17.                           └─cli::cli_abort("{.fun transform_date} works with objects of class {.cls Date} only")
     18.                             └─rlang::abort(...)
    Execution halted
    ```

# qicharts2

<details>

* Version: 0.7.5
* GitHub: https://github.com/anhoej/qicharts2
* Source code: https://github.com/cran/qicharts2
* Date/Publication: 2024-05-09 06:20:03 UTC
* Number of recursive dependencies: 71

Run `revdepcheck::cloud_details(, "qicharts2")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘qicharts2.Rmd’
      ...
    > qic(month, n, notes = notes, data = cdi, decimals = 0, 
    +     title = "Hospital acquired Clostridium difficile infections", 
    +     ylab = "Count", x .... [TRUNCATED] 
    
    > qic(month, n, data = cdi, decimals = 0, freeze = 24, 
    +     part.labels = c("Baseline", "Intervention"), title = "Hospital acquired Clostridium diff ..." ... [TRUNCATED] 
    
      When sourcing ‘qicharts2.R’:
    Error: `transform_time()` works with objects of class <POSIXct> only
    Execution halted
    
      ‘qicharts2.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘qicharts2.Rmd’ using rmarkdown
    ```

# QuadratiK

<details>

* Version: 1.1.0
* GitHub: https://github.com/giovsaraceno/QuadratiK-package
* Source code: https://github.com/cran/QuadratiK
* Date/Publication: 2024-05-14 13:50:06 UTC
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
      installed size is 14.6Mb
      sub-directories of 1Mb or more:
        libs  12.9Mb
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

# scdtb

<details>

* Version: 0.1.0
* GitHub: https://github.com/mightymetrika/scdtb
* Source code: https://github.com/cran/scdtb
* Date/Publication: 2024-04-30 08:50:02 UTC
* Number of recursive dependencies: 97

Run `revdepcheck::cloud_details(, "scdtb")` for more info

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
      Error in `nlme::gls(model = mod_form, data = .df, method = "REML", correlation = nlme::corAR1(form = re_form), 
          ...)`: false convergence (8)
      Backtrace:
          ▆
       1. └─scdtb::mixed_model_analysis(...) at test-mixed_model_analysis.R:114:3
       2.   └─nlme::gls(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 45 ]
      Error: Test failures
      Execution halted
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
# tis

<details>

* Version: 1.39
* GitHub: NA
* Source code: https://github.com/cran/tis
* Date/Publication: 2021-09-28 19:50:02 UTC
* Number of recursive dependencies: 15

Run `revdepcheck::cloud_details(, "tis")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tis-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: fortify.tis
    > ### Title: Fortify a tis object
    > ### Aliases: fortify.tis
    > 
    > ### ** Examples
    > 
    >     if(require("ggplot2") && require("reshape")) {
    ...
     10.             └─ggplot2 (local) FUN(X[[i]], ...)
     11.               └─scale$transform_df(df = df)
     12.                 └─ggplot2 (local) transform_df(..., self = self)
     13.                   └─base::lapply(df[aesthetics], self$transform)
     14.                     └─ggplot2 (local) FUN(X[[i]], ...)
     15.                       └─ggplot2 (local) transform(..., self = self)
     16.                         └─transformation$transform(x)
     17.                           └─cli::cli_abort("{.fun transform_date} works with objects of class {.cls Date} only")
     18.                             └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘zoo’
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

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.6Mb
      sub-directories of 1Mb or more:
        extdata   8.7Mb
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

# Wats

<details>

* Version: 1.0.1
* GitHub: https://github.com/OuhscBbmc/Wats
* Source code: https://github.com/cran/Wats
* Date/Publication: 2023-03-10 22:50:05 UTC
* Number of recursive dependencies: 122

Run `revdepcheck::cloud_details(, "Wats")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Wats-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: cartesian_periodic
    > ### Title: Linear Plot with Periodic Elements
    > ### Aliases: cartesian_periodic
    > ### Keywords: Cartesian
    > 
    > ### ** Examples
    > 
    ...
     10.             └─ggplot2 (local) FUN(X[[i]], ...)
     11.               └─scale$transform_df(df = df)
     12.                 └─ggplot2 (local) transform_df(..., self = self)
     13.                   └─base::lapply(df[aesthetics], self$transform)
     14.                     └─ggplot2 (local) FUN(X[[i]], ...)
     15.                       └─ggplot2 (local) transform(..., self = self)
     16.                         └─transformation$transform(x)
     17.                           └─cli::cli_abort("{.fun transform_date} works with objects of class {.cls Date} only")
     18.                             └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘mbr-figures.Rmd’
      ...
    +     dv_name = "birth_rate", center_function = stats::median, 
    +     spread_function = h_sprea .... [TRUNCATED] 
    
    > cartesian_rolling(ds_linear = portfolio_cartesian$ds_linear, 
    +     x_name = "date", y_name = "birth_rate", stage_id_name = "stage_id", 
    +     chang .... [TRUNCATED] 
    
      When sourcing ‘mbr-figures.R’:
    Error: `transform_date()` works with objects of class <Date> only
    Execution halted
    
      ‘mbr-figures.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘mbr-figures.Rmd’ using rmarkdown
    
    Quitting from lines 135-165 [fig-2-individual-basic] (mbr-figures.Rmd)
    Error: processing vignette 'mbr-figures.Rmd' failed with diagnostics:
    `transform_date()` works with objects of class <Date> only
    --- failed re-building ‘mbr-figures.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘mbr-figures.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘xaringanthemer.Rmd’
      ...
    Warning in file(con, "r") :
      cannot open file './../man/fragments/_quick-intro.Rmd': No such file or directory
    
    Quitting from lines 43-43 [unnamed-chunk-2] (xaringanthemer.Rmd)
    
      When tangling ‘xaringanthemer.Rmd’:
    Error: cannot open the connection
    Execution halted
    
      ‘ggplot2-themes.Rmd’ using ‘UTF-8’... OK
      ‘template-variables.Rmd’ using ‘UTF-8’... OK
      ‘xaringanthemer.Rmd’ using ‘UTF-8’... failed
    ```

