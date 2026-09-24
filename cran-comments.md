This is a patch release that resolves a failing test and supports some minimal
bug fixes and a small uninvasive feature. We detected a single failure
(ggdibbler) where a package needs to redocument. They have been notified. A
second failure (simRestore) seems unrelated to ggplot2

## revdepcheck results

We checked 6300 reverse dependencies (6276 from CRAN + 24 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 2 new problems
 * We failed to check 160 packages

Issues with CRAN packages are summarised below.

### New problems
(This reports the first line of each new failure)

* ggdibbler
  checking for code/documentation mismatches ... WARNING

* simRestore
  checking tests ... ERROR
