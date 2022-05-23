This is a small release as requested by CRAN to remove a call to the deprecated
`default.stringsAsFactors()` in base. This patch contains no user facing changes
and no breaking changes.

## R CMD check results

0 errors | 0 warnings | 0 note

## revdepcheck results

We checked 3967 reverse dependencies, comparing R CMD check results across CRAN 
and dev versions of this package.

 * We saw 2 new problems
 * We failed to check 6 packages

Issues with CRAN packages are summarised below. Looking into the 2 problems, 
none of them are related to ggplot2.

### New problems
(This reports the first line of each new failure)

* eurlex
  checking re-building of vignette outputs ... WARNING

* gggrid
  checking re-building of vignette outputs ... WARNING

### Failed to check

* CausalImpact (NA)
* ctsem        (NA)
* loon.ggplot  (NA)
* SSVS         (NA)
* valse        (NA)
* vivid        (NA)
