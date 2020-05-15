This is a major release that contains a few breaking changes in the internal
structure of the ggplot objects. This has resulted in breakage in some of the 
reverse dependenciees. All have been given a 1 month heads up to update their 
code.

## Test environments
* local R installation, R 3.6.0
* ubuntu 16.04 (on travis-ci), R 3.6.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 note

## revdepcheck results

We checked 3157 reverse dependencies (2685 from CRAN + 472 from BioConductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 30 new problems
 * We failed to check 79 packages

Issues with CRAN packages are summarised below.

### New problems
(This reports the first line of each new failure)

* apyramid
  checking tests ...

* autocogs
  checking examples ... ERROR
  checking tests ...

* bayesdfa
  checking installed package size ... NOTE
  checking for GNU extensions in Makefiles ... NOTE

* benchr
  checking tests ...

* biclustermd
  checking tests ...

* dabestr
  checking examples ... ERROR
  checking tests ...

* DeLorean
  checking installed package size ... NOTE
  checking dependencies in R code ... NOTE
  checking for GNU extensions in Makefiles ... NOTE

* describedata
  checking examples ... ERROR

* ezplot
  checking examples ... ERROR
  checking tests ...

* GGally
  checking examples ... ERROR
  checking tests ...

* ggdag
  checking examples ... ERROR

* ggeasy
  checking tests ...

* ggfortify
  checking tests ...

* ggpol
  checking examples ... ERROR

* ggspectra
  checking examples ... ERROR

* ggstance
  checking tests ...

* gWQS
  checking running R code from vignettes ...

* helda
  checking tests ...

* interactions
  checking tests ...

* lemon
  checking tests ...

* mcStats
  checking tests ...

* plot3logit
  checking examples ... ERROR

* PSCBS
  checking tests ...

* RGraphics
  checking whether package ‘RGraphics’ can be installed ... WARNING
  checking R code for possible problems ... NOTE

* riskRegression
  checking tests ...

* Seurat
  checking examples ... ERROR

* spartan
  checking tests ...

* survsup
  checking examples ... ERROR

* tricolore
  checking examples ... ERROR
  checking tests ...

* xpose
  checking tests ...

### Failed to check

* aslib                    (NA)
* av                       (NA)
* BACA                     (NA)
* BACCT                    (NA)
* bamdit                   (NA)
* BayesPostEst             (NA)
* BayesRS                  (NA)
* BNSP                     (NA)
* BPEC                     (NA)
* bsam                     (NA)
* BTSPAS                   (NA)
* CollapsABEL              (NA)
* crmPack                  (NA)
* Crossover                (NA)
* ctsem                    (NA)
* Deducer                  (NA)
* DistributionOptimization (NA)
* DiversityOccupancy       (NA)
* dynr                     (NA)
* evoper                   (NA)
* ewoc                     (NA)
* fingerPro                (NA)
* fsdaR                    (NA)
* G2Sd                     (NA)
* ggdmc                    (NA)
* ggtern                   (NA)
* GUIgems                  (NA)
* hbbr                     (NA)
* imageData                (NA)
* InSilicoVA               (NA)
* jarbes                   (NA)
* JointAI                  (NA)
* L0Learn                  (NA)
* lilikoi                  (NA)
* llama                    (NA)
* LLSR                     (NA)
* matchingMarkets          (NA)
* mbgraphic                (NA)
* mcmcabn                  (NA)
* MergeGUI                 (NA)
* metaMix                  (NA)
* mfbvar                   (NA)
* MissingDataGUI           (NA)
* mleap                    (NA)
* morse                    (NA)
* mwaved                   (NA)
* nlmixr                   (NA)
* OpenStreetMap            (NA)
* openVA                   (NA)
* petro.One                (NA)
* phase1PRMD               (NA)
* phase1RMD                (NA)
* PortfolioEffectHFT       (NA)
* qdap                     (NA)
* RclusTool                (NA)
* RcmdrPlugin.FuzzyClust   (NA)
* Rdrools                  (NA)
* rmcfs                    (NA)
* rpanel                   (NA)
* rrd                      (NA)
* rrepast                  (NA)
* RSCAT                    (NA)
* rstanarm                 (NA)
* rsvg                     (NA)
* RtutoR                   (NA)
* RxODE                    (NA)
* SeqFeatR                 (NA)
* sf                       (NA)
* simmr                    (NA)
* smartR                   (NA)
* spcosa                   (NA)
* stpp                     (NA)
* TeachingDemos            (NA)
* trialr                   (NA)
* vortexR                  (NA)
* WaveSampling             (NA)
* XLConnect                (NA)
* zenplots                 (NA)
* zooaRchGUI               (NA)
