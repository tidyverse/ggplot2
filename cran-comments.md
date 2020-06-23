This is a patch release that fixes regressions introduced in 3.3.0. It has no 
user facing changes, but includes some internal changes that affects a few 
reverse dependencies (mostly in expectations in their unit tests). All 
problematic reverse dependencies were notified well in advance, and most have 
sent fixes to CRAN.

## Test environments
* local R installation, R 4.0.1
* ubuntu 16.04 (on travis-ci), R 4.0.1
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 note

## revdepcheck results

We checked 2752 reverse dependencies (2744 from CRAN + 8 from BioConductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 13 new problems
 * We failed to check 95 packages

Issues with CRAN packages are summarised below.

### New problems
(This reports the first line of each new failure)

* drugCombo
  checking whether package ‘drugCombo’ can be installed ... WARNING

* frontiles
  checking whether package ‘frontiles’ can be installed ... WARNING

* GENEAsphere
  checking whether package ‘GENEAsphere’ can be installed ... WARNING

* ggdistribute
  checking examples ... ERROR

* gMOIP
  checking whether package ‘gMOIP’ can be installed ... WARNING

* helda
  checking tests ... ERROR

* lemon
  checking examples ... ERROR

* metagen
  checking examples ... ERROR

* NeatMap
  checking whether package ‘NeatMap’ can be installed ... WARNING

* PPQplan
  checking whether package ‘PPQplan’ can be installed ... WARNING

* predict3d
  checking whether package ‘predict3d’ can be installed ... WARNING

* ratPASTA
  checking tests ... ERROR

* vmsbase
  checking S3 generic/method consistency ... WARNING
  checking replacement functions ... WARNING
  checking for missing documentation entries ... WARNING
  checking for code/documentation mismatches ... WARNING
  checking dependencies in R code ... NOTE
  checking foreign function calls ... NOTE
  checking R code for possible problems ... NOTE
  checking Rd \usage sections ... NOTE

### Failed to check

* AID                  (NA)
* ALA4R                (NA)
* av                   (NA)
* backShift            (NA)
* BGGM                 (NA)
* bootnet              (NA)
* BPEC                 (NA)
* breathteststan       (NA)
* cate                 (NA)
* CausalImpact         (NA)
* CB2                  (NA)
* cbar                 (NA)
* csp                  (NA)
* decisionSupport      (NA)
* dendroTools          (NA)
* dfpk                 (NA)
* diceR                (NA)
* dimRed               (NA)
* EffectLiteR          (NA)
* EGAnet               (NA)
* EstimateGroupNetwork (NA)
* EvaluateCore         (NA)
* ezCutoffs            (NA)
* fingertipscharts     (NA)
* ForecastComb         (NA)
* fSRM                 (NA)
* gastempt             (NA)
* GeomComb             (NA)
* GGEBiplots           (NA)
* ggmsa                (NA)
* gscaLCA              (NA)
* HierDpart            (NA)
* hilldiv              (NA)
* iarm                 (NA)
* idiogramFISH         (NA)
* JWileymisc           (NA)
* likert               (NA)
* lsl                  (NA)
* MAINT.Data           (NA)
* MarketMatching       (NA)
* mcvis                (NA)
* mrbayes              (NA)
* multilevelPSA        (NA)
* multilevelTools      (NA)
* MultisiteMediation   (NA)
* mvdalab              (NA)
* NetworkChange        (NA)
* networktools         (NA)
* neuropsychology      (NA)
* nLTT                 (NA)
* NMF                  (NA)
* OncoBayes2           (NA)
* osmplotr             (NA)
* OutlierDetection     (NA)
* pcalg                (NA)
* PCMBase              (NA)
* penaltyLearning      (NA)
* PhyInformR           (NA)
* phylopath            (NA)
* pmc                  (NA)
* pompom               (NA)
* processR             (NA)
* profileR             (NA)
* prophet              (NA)
* pscore               (NA)
* psychonetrics        (NA)
* qgraph               (NA)
* quokar               (NA)
* r4lineups            (NA)
* radiant.basics       (NA)
* radiant.data         (NA)
* radiant.model        (NA)
* radiant.multivariate (NA)
* RAM                  (NA)
* RBesT                (NA)
* rhierbaps            (NA)
* rrd                  (NA)
* rstanarm             (NA)
* sdmvspecies          (NA)
* sensiPhy             (NA)
* ShinyItemAnalysis    (NA)
* ShortForm            (NA)
* SimCorrMix           (NA)
* SimDesign            (NA)
* SimMultiCorrData     (NA)
* spectralAnalysis     (NA)
* StroupGLMM           (NA)
* trackdf              (NA)
* trackr               (NA)
* treespace            (NA)
* TriMatch             (NA)
* userfriendlyscience  (NA)
* vcfR                 (NA)
* webr                 (NA)
* wrswoR               (NA)
