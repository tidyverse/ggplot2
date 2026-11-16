This is a patch release that provides compatibility with an anticipated S7 release.
We found 8 package with reverse dependency issues, out of which 5 are seemingly unrelated to our changes but 3 are related.
We've sent out pull requests to the affected 3 packages (constructive, NHSRplotthedots, xpose.extras).

## revdepcheck results

We checked 7188 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 8 new problems
 * We failed to check 16 packages

Issues with CRAN packages are summarised below.

### New problems
(This reports the first line of each new failure)

* constructive
  checking tests ... ERROR

* irelink
  checking tests ... ERROR

* NHSRplotthedots
  checking tests ... ERROR

* nycOpenData
  checking re-building of vignette outputs ... ERROR

* PGRdup
  checking re-building of vignette outputs ... ERROR

* PSAboot
  checking re-building of vignette outputs ... ERROR

* wjake
  checking examples ... ERROR

* xpose.xtras
  checking tests ... ERROR