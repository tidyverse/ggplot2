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

