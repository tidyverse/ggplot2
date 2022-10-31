This is a minor release focusing on internal changes.

We have tested reverse dependencies and contacted failing packages 4 weeks ago,
notifying them of the failure and where in their code they need to look.

At the time of submission 44 packages are still failing.

## revdepcheck results

We checked 4238 reverse dependencies (4224 from CRAN + 14 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 46 new problems
 * We failed to check 22 packages

Issues with CRAN packages are summarised below.

### New problems
(This reports the first line of each new failure)

* afex
  checking re-building of vignette outputs ... WARNING

* antaresViz
  checking tests ... ERROR

* bayesplot
  checking tests ... ERROR

* bigsnpr
  checking tests ... ERROR

* chronochrt
  checking examples ... ERROR
  checking tests ... ERROR
  checking re-building of vignette outputs ... WARNING

* coveffectsplot
  checking re-building of vignette outputs ... WARNING

* dataquieR
  checking tests ... ERROR

* deeptime
  checking dependencies in R code ... WARNING

* docxtools
  checking tests ... ERROR

* DriveML
  checking re-building of vignette outputs ... WARNING

* evaluate
  checking tests ... ERROR

* fdistr
  checking tests ... ERROR

* ggbeeswarm
  checking examples ... ERROR

* ggconf
  checking examples ... ERROR
  checking tests ... ERROR

* ggdag
  checking examples ... ERROR
  checking tests ... ERROR
  checking dependencies in R code ... WARNING
  checking re-building of vignette outputs ... WARNING

* ggetho
  checking dependencies in R code ... WARNING

* ggfun
  checking dependencies in R code ... WARNING

* gghalves
  checking examples ... ERROR
  checking re-building of vignette outputs ... WARNING

* ggip
  checking tests ... ERROR

* ggiraph
  checking examples ... ERROR
  checking tests ... ERROR

* ggiraphExtra
  checking examples ... ERROR
  checking re-building of vignette outputs ... WARNING

* gglgbtq
  checking dependencies in R code ... WARNING

* ggmap
  checking dependencies in R code ... WARNING

* ggmulti
  checking examples ... ERROR
  checking dependencies in R code ... WARNING

* ggpubr
  checking examples ... ERROR

* ggstance
  checking dependencies in R code ... WARNING

* ggstar
  checking dependencies in R code ... WARNING

* iheiddown
  checking tests ... ERROR

* inTextSummaryTable
  checking tests ... ERROR

* jjAnno
  checking examples ... ERROR

* lemon
  checking examples ... ERROR
  checking tests ... ERROR
  checking re-building of vignette outputs ... WARNING

* lingtypology
  checking examples ... ERROR

* listdown
  checking tests ... ERROR

* openalexR
  checking re-building of vignette outputs ... WARNING

* pathviewr
  checking tests ... ERROR

* plotly
  checking tests ... ERROR

* plotmm
  checking examples ... ERROR

* qqboxplot
  checking examples ... ERROR
  checking re-building of vignette outputs ... WARNING

* RJafroc
  checking tests ... ERROR

* robustlmm
  checking running R code from vignettes ... WARNING

* scdhlm
  checking tests ... ERROR

* schtools
  checking tests ... ERROR

* tcpl
  checking re-building of vignette outputs ... WARNING

* tvthemes
  checking tests ... ERROR

* usmap
  checking tests ... ERROR

* xpose
  checking tests ... ERROR

### Failed to check

* CausalImpact    (NA)
* ctsem           (NA)
* expss           (NA)
* ggPMX           (NA)
* ggprism         (NA)
* ggshadow        (NA)
* ggtern          (NA)
* loon.ggplot     (NA)
* nlmixr2         (NA)
* nlmixr2extra    (NA)
* nlmixr2plot     (NA)
* OpenMx          (NA)
* Platypus        (NA)
* RcppCensSpatial (NA)
* rPBK            (NA)
* simpr           (NA)
* Sofi            (NA)
* SSVS            (NA)
* tidySEM         (NA)
* valse           (NA)
* vivid           (NA)
* xpose.nlmixr2   (NA)
