This is a small patch release at the request of CRAN. It includes changes to
adapt to the new defaults of `all_equal()`, and updates the test setup so that
vdiffr is only used if available. It further moves ggplot2 to MIT license after
having gotten consent from all contributers.

Since there are no changes in executable code there is no breaking changes and 
problems with reverse dependencies is not expected.

## Test environments
* local R installation, R 4.0.3
* ubuntu 16.04 (on travis-ci), R 4.0.3
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 note
