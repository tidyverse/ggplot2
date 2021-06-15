This is mainly a patch release, though we have folded in a couple of new 
features. The release contains a couple of internal breaking changes which can
affect packages that inspects the internals of ggplot objects. All failing 
reverse dependencies have been notified 3 weeks ago with information about what
needs to be fixed and how. Most of these have already published a fix or is in 
the process of doing so.

## R CMD check results

0 errors | 0 warnings | 0 note

## revdepcheck results

We checked 3149 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 11 new problems
 * We failed to check 39 packages

Issues with CRAN packages are summarised below.

### New problems
(This reports the first line of each new failure)

* bayesAB
  checking tests ... ERROR

* BayesianReasoning
  checking tests ... ERROR

* cvms
  checking tests ... ERROR

* ezEDA
  checking tests ... ERROR

* ggseg
  checking examples ... ERROR
  checking tests ... ERROR

* HRM
  checking dependencies in R code ... NOTE

* plotly
  checking tests ... ERROR

* ratPASTA
  checking tests ... ERROR

* rBiasCorrection
  checking tests ... ERROR

* tricolore
  checking examples ... ERROR

* xpose
  checking tests ... ERROR

### Failed to check

* ActivePathways   (NA)
* apc              (NA)
* apisensr         (NA)
* backShift        (NA)
* bayesdfa         (NA)
* bayesGAM         (NA)
* bayesZIB         (NA)
* bmgarch          (NA)
* CausalImpact     (NA)
* CB2              (NA)
* cbar             (NA)
* dfpk             (NA)
* diceR            (NA)
* GenHMM1d         (NA)
* ggmsa            (NA)
* ggtern           (NA)
* glmmfields       (NA)
* MarketMatching   (NA)
* mcmcabn          (NA)
* metagam          (NA)
* mlr3pipelines    (NA)
* OncoBayes2       (NA)
* osmplotr         (NA)
* pcalg            (NA)
* penaltyLearning  (NA)
* phylopath        (NA)
* rabhit           (NA)
* raw              (NA)
* rstap            (NA)
* scoper           (NA)
* spectralAnalysis (NA)
* StroupGLMM       (NA)
* SynthETIC        (NA)
* tigger           (NA)
* trackr           (NA)
* valse            (NA)
* vivid            (NA)
* wrswoR           (NA)
* zenplots         (NA)
