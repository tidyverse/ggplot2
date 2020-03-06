# aslib

<details>

* Version: 0.1
* Source code: https://github.com/cran/aslib
* URL: https://github.com/coseal/aslib-r/
* BugReports: https://github.com/coseal/aslib-r/issues
* Date/Publication: 2016-11-25 08:42:53
* Number of recursive dependencies: 79

Run `revdep_details(,"aslib")` for more info

</details>

## In both

*   checking whether package ‘aslib’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/aslib/new/aslib.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘aslib’ ...
** package ‘aslib’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/aslib/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/aslib/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/aslib/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘aslib’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/aslib/new/aslib.Rcheck/aslib’

```
### CRAN

```
* installing *source* package ‘aslib’ ...
** package ‘aslib’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/aslib/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/aslib/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/aslib/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘aslib’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/aslib/old/aslib.Rcheck/aslib’

```
# av

<details>

* Version: 0.5.0
* Source code: https://github.com/cran/av
* URL: https://docs.ropensci.org/av (website), https://github.com/ropensci/av (devel)
* BugReports: https://github.com/ropensci/av/issues
* Date/Publication: 2020-01-29 09:20:02 UTC
* Number of recursive dependencies: 51

Run `revdep_details(,"av")` for more info

</details>

## In both

*   checking whether package ‘av’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/av/new/av.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘av’ ...
** package ‘av’ successfully unpacked and MD5 sums checked
** using staged installation
Found pkg-config cflags and libs!
Using PKG_CFLAGS=-I/usr/local/Cellar/ffmpeg/4.1/include
Using PKG_LIBS=-L/usr/local/Cellar/ffmpeg/4.1/lib -lavfilter
** libs
rm -f av.so fft.o formats.o info.o init.o video.o winfunc.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/ffmpeg/4.1/include  -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c fft.c -o fft.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/ffmpeg/4.1/include  -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c formats.c -o formats.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/ffmpeg/4.1/include  -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c info.c -o info.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/ffmpeg/4.1/include  -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c init.c -o init.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/ffmpeg/4.1/include  -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c video.c -o video.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/ffmpeg/4.1/include  -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c winfunc.c -o winfunc.o
/usr/local/clang8/bin/clang++ -std=gnu++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/clang8/lib -o av.so fft.o formats.o info.o init.o video.o winfunc.o -L/usr/local/Cellar/ffmpeg/4.1/lib -lavfilter -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/max/github/forks/ggplot2/revdep/checks.noindex/av/new/av.Rcheck/00LOCK-av/00new/av/libs
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
Error: package or namespace load failed for ‘av’ in dyn.load(file, DLLpath = DLLpath, ...):
 unable to load shared object '/Users/max/github/forks/ggplot2/revdep/checks.noindex/av/new/av.Rcheck/00LOCK-av/00new/av/libs/av.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/checks.noindex/av/new/av.Rcheck/00LOCK-av/00new/av/libs/av.so, 6): Library not loaded: /usr/local/opt/x265/lib/libx265.165.dylib
  Referenced from: /usr/local/opt/ffmpeg/lib/libavfilter.7.dylib
  Reason: image not found
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/av/new/av.Rcheck/av’

```
### CRAN

```
* installing *source* package ‘av’ ...
** package ‘av’ successfully unpacked and MD5 sums checked
** using staged installation
Found pkg-config cflags and libs!
Using PKG_CFLAGS=-I/usr/local/Cellar/ffmpeg/4.1/include
Using PKG_LIBS=-L/usr/local/Cellar/ffmpeg/4.1/lib -lavfilter
** libs
rm -f av.so fft.o formats.o info.o init.o video.o winfunc.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/ffmpeg/4.1/include  -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c fft.c -o fft.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/ffmpeg/4.1/include  -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c formats.c -o formats.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/ffmpeg/4.1/include  -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c info.c -o info.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/ffmpeg/4.1/include  -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c init.c -o init.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/ffmpeg/4.1/include  -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c video.c -o video.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/ffmpeg/4.1/include  -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c winfunc.c -o winfunc.o
/usr/local/clang8/bin/clang++ -std=gnu++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/clang8/lib -o av.so fft.o formats.o info.o init.o video.o winfunc.o -L/usr/local/Cellar/ffmpeg/4.1/lib -lavfilter -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/max/github/forks/ggplot2/revdep/checks.noindex/av/old/av.Rcheck/00LOCK-av/00new/av/libs
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
Error: package or namespace load failed for ‘av’ in dyn.load(file, DLLpath = DLLpath, ...):
 unable to load shared object '/Users/max/github/forks/ggplot2/revdep/checks.noindex/av/old/av.Rcheck/00LOCK-av/00new/av/libs/av.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/checks.noindex/av/old/av.Rcheck/00LOCK-av/00new/av/libs/av.so, 6): Library not loaded: /usr/local/opt/x265/lib/libx265.165.dylib
  Referenced from: /usr/local/opt/ffmpeg/lib/libavfilter.7.dylib
  Reason: image not found
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/av/old/av.Rcheck/av’

```
# BaalChIP

<details>

* Version: 1.10.0
* Source code: https://github.com/cran/BaalChIP
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 99

Run `revdep_details(,"BaalChIP")` for more info

</details>

## In both

*   R CMD check timed out
    

*   checking installed package size ... NOTE
    ```
      installed size is 200.0Mb
      sub-directories of 1Mb or more:
        data   96.0Mb
        doc     1.6Mb
        test  101.9Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    applyBayes: no visible binding for global variable ‘SNP_id’
    plot.filt.barplot: no visible binding for global variable ‘cellname’
    plot.filt.barplot: no visible binding for global variable ‘value’
    plot.filt.barplot: no visible binding for global variable ‘variable’
    plot.filt.boxplot: no visible binding for global variable ‘variable’
    plot.filt.boxplot: no visible binding for global variable ‘value’
    plot.filt.boxplot: no visible binding for global variable ‘coltype’
    plot.filt.pie: no visible binding for global variable ‘variable’
    plot.filt.pie: no visible binding for global variable ‘value.mean’
    plot.simul: no visible binding for global variable ‘readslen’
    plot.simul: no visible binding for global variable ‘perc_right’
    plotadjustment: no visible binding for global variable ‘value’
    plotadjustment: no visible binding for global variable ‘variable’
    Undefined global functions or variables:
      SNP_id cellname coltype perc_right readslen value value.mean variable
    ```

# BACA

<details>

* Version: 1.3
* Source code: https://github.com/cran/BACA
* Date/Publication: 2015-05-27 08:55:17
* Number of recursive dependencies: 74

Run `revdep_details(,"BACA")` for more info

</details>

## In both

*   checking whether package ‘BACA’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/BACA/new/BACA.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘BACA’ ...
** package ‘BACA’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/BACA/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/BACA/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/BACA/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘BACA’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/BACA/new/BACA.Rcheck/BACA’

```
### CRAN

```
* installing *source* package ‘BACA’ ...
** package ‘BACA’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/BACA/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/BACA/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/BACA/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘BACA’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/BACA/old/BACA.Rcheck/BACA’

```
# BACCT

<details>

* Version: 1.0
* Source code: https://github.com/cran/BACCT
* Date/Publication: 2016-06-25 19:07:22
* Number of recursive dependencies: 39

Run `revdep_details(,"BACCT")` for more info

</details>

## In both

*   checking whether package ‘BACCT’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/BACCT/new/BACCT.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘BACCT’ ...
** package ‘BACCT’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/BACCT/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/BACCT/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/BACCT/rjags/libs/rjags.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘BACCT’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/BACCT/new/BACCT.Rcheck/BACCT’

```
### CRAN

```
* installing *source* package ‘BACCT’ ...
** package ‘BACCT’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/BACCT/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/BACCT/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/BACCT/rjags/libs/rjags.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘BACCT’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/BACCT/old/BACCT.Rcheck/BACCT’

```
# bamdit

<details>

* Version: 3.3.2
* Source code: https://github.com/cran/bamdit
* Date/Publication: 2019-07-09 14:10:07 UTC
* Number of recursive dependencies: 61

Run `revdep_details(,"bamdit")` for more info

</details>

## In both

*   checking whether package ‘bamdit’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/bamdit/new/bamdit.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘bamdit’ ...
** package ‘bamdit’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/bamdit/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/bamdit/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/bamdit/rjags/libs/rjags.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘bamdit’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/bamdit/new/bamdit.Rcheck/bamdit’

```
### CRAN

```
* installing *source* package ‘bamdit’ ...
** package ‘bamdit’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/bamdit/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/bamdit/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/bamdit/rjags/libs/rjags.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘bamdit’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/bamdit/old/bamdit.Rcheck/bamdit’

```
# BayesPostEst

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/BayesPostEst
* URL: https://github.com/ShanaScogin/BayesPostEst
* BugReports: https://github.com/ShanaScogin/BayesPostEst/issues
* Date/Publication: 2019-12-14 23:10:02 UTC
* Number of recursive dependencies: 139

Run `revdep_details(,"BayesPostEst")` for more info

</details>

## In both

*   checking whether package ‘BayesPostEst’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/BayesPostEst/new/BayesPostEst.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘BayesPostEst’ ...
** package ‘BayesPostEst’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_interactive’
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_interactive’
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_interactive_cat’
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_interactive_cat’
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_logit’
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_logit’
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_probit’
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_probit’
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/BayesPostEst/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/BayesPostEst/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/BayesPostEst/rjags/libs/rjags.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘BayesPostEst’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/BayesPostEst/new/BayesPostEst.Rcheck/BayesPostEst’

```
### CRAN

```
* installing *source* package ‘BayesPostEst’ ...
** package ‘BayesPostEst’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_interactive’
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_interactive’
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_interactive_cat’
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_interactive_cat’
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_logit’
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_logit’
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_probit’
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_probit’
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/BayesPostEst/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/BayesPostEst/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/BayesPostEst/rjags/libs/rjags.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘BayesPostEst’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/BayesPostEst/old/BayesPostEst.Rcheck/BayesPostEst’

```
# BayesRS

<details>

* Version: 0.1.3
* Source code: https://github.com/cran/BayesRS
* Date/Publication: 2018-04-06 06:39:35 UTC
* Number of recursive dependencies: 62

Run `revdep_details(,"BayesRS")` for more info

</details>

## In both

*   checking whether package ‘BayesRS’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/BayesRS/new/BayesRS.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘BayesRS’ ...
** package ‘BayesRS’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/BayesRS/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/BayesRS/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/BayesRS/rjags/libs/rjags.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘BayesRS’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/BayesRS/new/BayesRS.Rcheck/BayesRS’

```
### CRAN

```
* installing *source* package ‘BayesRS’ ...
** package ‘BayesRS’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/BayesRS/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/BayesRS/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/BayesRS/rjags/libs/rjags.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘BayesRS’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/BayesRS/old/BayesRS.Rcheck/BayesRS’

```
# BNSP

<details>

* Version: 2.1.2
* Source code: https://github.com/cran/BNSP
* URL: http://www.bbk.ac.uk/ems/faculty/papageorgiou/BNSP
* Date/Publication: 2019-12-05 09:00:17 UTC
* Number of recursive dependencies: 68

Run `revdep_details(,"BNSP")` for more info

</details>

## In both

*   checking whether package ‘BNSP’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/BNSP/new/BNSP.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘BNSP’ ...
** package ‘BNSP’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/BNSP/cubature/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c BNSP_init.c -o BNSP_init.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/BNSP/cubature/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c BayesMult.c -o BayesMult.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/BNSP/cubature/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c BayesMultG.c -o BayesMultG.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/BNSP/cubature/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c BayesMultGV.c -o BayesMultGV.o
BayesMult.c:26:10: fatal error: 'gsl/gsl_matrix.h' file not found
#include <gsl/gsl_matrix.h>
         ^~~~~~~~~~~~~~~~~~
BayesMultGV.c:26:10: fatal error: 'gsl/gsl_matrix.h' file not found
#include <gsl/gsl_matrix.h>
         ^~~~~~~~~~~~~~~~~~
BayesMultG.c:26:10: fatal error: 'gsl/gsl_matrix.h' file not found
#include <gsl/gsl_matrix.h>
         ^~~~~~~~~~~~~~~~~~
1 error generated.
1 error generated.
1 error generated.
make: *** [BayesMult.o] Error 1
make: *** Waiting for unfinished jobs....
make: *** [BayesMultGV.o] Error 1
make: *** [BayesMultG.o] Error 1
ERROR: compilation failed for package ‘BNSP’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/BNSP/new/BNSP.Rcheck/BNSP’

```
### CRAN

```
* installing *source* package ‘BNSP’ ...
** package ‘BNSP’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/BNSP/cubature/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c BNSP_init.c -o BNSP_init.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/BNSP/cubature/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c BayesMult.c -o BayesMult.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/BNSP/cubature/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c BayesMultG.c -o BayesMultG.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/BNSP/cubature/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c BayesMultGV.c -o BayesMultGV.o
BayesMultGV.cBayesMultG.c:26:10: fatal error: 'gsl/gsl_matrix.h' file not found
#include <gsl/gsl_matrix.h>
         ^~~~~~~~~~~~~~~~~~
:26:10: fatal error: 'gsl/gsl_matrix.h' file not found
#include <gsl/gsl_matrix.h>
         ^~~~~~~~~~~~~~~~~~
BayesMult.c:26:10: fatal error: 'gsl/gsl_matrix.h' file not found
#include <gsl/gsl_matrix.h>
         ^~~~~~~~~~~~~~~~~~
1 error generated.
1 error generated.
1 error generated.
make: *** [BayesMult.o] Error 1
make: *** Waiting for unfinished jobs....
make: *** [BayesMultG.o] Error 1
make: *** [BayesMultGV.o] Error 1
ERROR: compilation failed for package ‘BNSP’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/BNSP/old/BNSP.Rcheck/BNSP’

```
# BPEC

<details>

* Version: 1.3.0
* Source code: https://github.com/cran/BPEC
* Date/Publication: 2018-08-29 20:56:48 UTC
* Number of recursive dependencies: 85

Run `revdep_details(,"BPEC")` for more info

</details>

## In both

*   checking whether package ‘BPEC’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/BPEC/new/BPEC.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘BPEC’ ...
** package ‘BPEC’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c bpecfunction.c -o bpecfunction.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c hashingfunctions.c -o hashingfunctions.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c loopfunctions.c -o loopfunctions.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c matrices.c -o matrices.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c randomnumbers.c -o randomnumbers.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c registerDynamicSymbol.c -o registerDynamicSymbol.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c treelikelifunctions.c -o treelikelifunctions.o
/usr/local/clang8/bin/clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/clang8/lib -o BPEC.so bpecfunction.o hashingfunctions.o loopfunctions.o matrices.o randomnumbers.o registerDynamicSymbol.o treelikelifunctions.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/max/github/forks/ggplot2/revdep/checks.noindex/BPEC/new/BPEC.Rcheck/00LOCK-BPEC/00new/BPEC/libs
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/BPEC/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/BPEC/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/BPEC/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘BPEC’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/BPEC/new/BPEC.Rcheck/BPEC’

```
### CRAN

```
* installing *source* package ‘BPEC’ ...
** package ‘BPEC’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c bpecfunction.c -o bpecfunction.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c hashingfunctions.c -o hashingfunctions.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c loopfunctions.c -o loopfunctions.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c matrices.c -o matrices.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c randomnumbers.c -o randomnumbers.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c registerDynamicSymbol.c -o registerDynamicSymbol.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c treelikelifunctions.c -o treelikelifunctions.o
/usr/local/clang8/bin/clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/clang8/lib -o BPEC.so bpecfunction.o hashingfunctions.o loopfunctions.o matrices.o randomnumbers.o registerDynamicSymbol.o treelikelifunctions.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/max/github/forks/ggplot2/revdep/checks.noindex/BPEC/old/BPEC.Rcheck/00LOCK-BPEC/00new/BPEC/libs
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/BPEC/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/BPEC/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/BPEC/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘BPEC’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/BPEC/old/BPEC.Rcheck/BPEC’

```
# bsam

<details>

* Version: 1.1.2
* Source code: https://github.com/cran/bsam
* URL: https://github.com/ianjonsen/bsam
* BugReports: https://github.com/ianjonsen/bsam/issues
* Date/Publication: 2017-07-01 02:50:50 UTC
* Number of recursive dependencies: 52

Run `revdep_details(,"bsam")` for more info

</details>

## In both

*   checking whether package ‘bsam’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/bsam/new/bsam.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘bsam’ ...
** package ‘bsam’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rjags’:
 .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/bsam/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/bsam/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/bsam/rjags/libs/rjags.so
  Reason: image not found
Error: package ‘rjags’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘bsam’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/bsam/new/bsam.Rcheck/bsam’

```
### CRAN

```
* installing *source* package ‘bsam’ ...
** package ‘bsam’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rjags’:
 .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/bsam/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/bsam/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/bsam/rjags/libs/rjags.so
  Reason: image not found
Error: package ‘rjags’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘bsam’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/bsam/old/bsam.Rcheck/bsam’

```
# BTSPAS

<details>

* Version: 2020.1.1
* Source code: https://github.com/cran/BTSPAS
* URL: http://www.stat.sfu.ca/~cschwarz/Consulting/Trinity/Phase2
* Date/Publication: 2019-12-04 22:20:26 UTC
* Number of recursive dependencies: 58

Run `revdep_details(,"BTSPAS")` for more info

</details>

## In both

*   checking whether package ‘BTSPAS’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/BTSPAS/new/BTSPAS.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘BTSPAS’ ...
** package ‘BTSPAS’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/BTSPAS/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/BTSPAS/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/BTSPAS/rjags/libs/rjags.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘BTSPAS’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/BTSPAS/new/BTSPAS.Rcheck/BTSPAS’

```
### CRAN

```
* installing *source* package ‘BTSPAS’ ...
** package ‘BTSPAS’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/BTSPAS/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/BTSPAS/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/BTSPAS/rjags/libs/rjags.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘BTSPAS’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/BTSPAS/old/BTSPAS.Rcheck/BTSPAS’

```
# ChIPseeker

<details>

* Version: 
* Source code: ???
* URL: http://ggplot2.tidyverse.org, https://github.com/tidyverse/ggplot2
* BugReports: https://github.com/tidyverse/ggplot2/issues
* Number of recursive dependencies: 0

Run `revdep_details(,"")` for more info

</details>

## Error before installation

### Devel

```

  There are binary versions available but the source versions are later:
           binary  source needs_compilation
checkmate   1.9.4   2.0.0              TRUE
nlme      3.1-143 3.1-144              TRUE

  Binaries will be installed


installing the source package ‘reactome.db’



```
### CRAN

```

  There are binary versions available but the source versions are later:
           binary  source needs_compilation
checkmate   1.9.4   2.0.0              TRUE
nlme      3.1-143 3.1-144              TRUE

  Binaries will be installed


installing the source package ‘reactome.db’



```
# clusterProfiler

<details>

* Version: 
* Source code: ???
* URL: http://ggplot2.tidyverse.org, https://github.com/tidyverse/ggplot2
* BugReports: https://github.com/tidyverse/ggplot2/issues
* Number of recursive dependencies: 0

Run `revdep_details(,"")` for more info

</details>

## Error before installation

### Devel

```

  There are binary versions available but the source versions are later:
           binary  source needs_compilation
checkmate   1.9.4   2.0.0              TRUE
nlme      3.1-143 3.1-144              TRUE

  Binaries will be installed


installing the source package ‘reactome.db’



```
### CRAN

```

  There are binary versions available but the source versions are later:
           binary  source needs_compilation
checkmate   1.9.4   2.0.0              TRUE
nlme      3.1-143 3.1-144              TRUE

  Binaries will be installed


installing the source package ‘reactome.db’



```
# CNPBayes

<details>

* Version: 1.13.5
* Source code: https://github.com/cran/CNPBayes
* URL: https://github.com/scristia/CNPBayes
* BugReports: https://github.com/scristia/CNPBayes/issues
* Date/Publication: 2019-01-05
* Number of recursive dependencies: 162

Run `revdep_details(,"CNPBayes")` for more info

</details>

## Newly broken

*   R CMD check timed out
    

## In both

*   checking examples ... ERROR
    ```
    ...
    Running examples in ‘CNPBayes-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggChains
    > ### Title: Trace plots of MCMC chains and mixture model densities
    > ### Aliases: ggChains ggMixture ggMixture,MultiBatchCopyNumber-method
    > ###   ggMixture,MultiBatchCopyNumberPooled-method
    > ###   ggMixture,MultiBatchModel-method ggMixture,MultiBatch-method
    > ###   ggMixture,MultiBatchPooled-method ggChains,MultiBatchModel-method
    > ###   ggChains,MultiBatchPooled-method
    > 
    > ### ** Examples
    > 
    >   sb <- SingleBatchModelExample
    >   iter(sb) <- 1000
    >   burnin(sb) <- 100
    >   sb <- posteriorSimulation(sb)
    >   fig.chains <- ggChains(sb)
    Error: 1 components of `...` had unexpected names.
    
    We detected these problematic arguments:
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      [31m──[39m [31m4. Failure: easy mendelian example (@test_trios.R#524) [39m [31m─────────────────────────────────────────────────────────────────────────────────[39m
      `z.m` not identical to 2L.
      1/1 mismatches
      [1] 3 - 2 == 1
      
      ══ testthat results  ═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 305 | SKIPPED: 3 | WARNINGS: 3 | FAILED: 4 ]
      1. Failure: sigma2_pooled (@test_SingleBatchPooled.R#24) 
      2. Failure: ggfun (@test_ggfuns.R#9) 
      3. Failure: kbatch (@test_multibatch.R#271) 
      4. Failure: easy mendelian example (@test_trios.R#524) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking for missing documentation entries ... WARNING
    ```
    ...
      generic 'sigma' and siglist 'MultiBatchPooled'
      generic 'sigma<-' and siglist 'MixtureModel'
      generic 'sigma<-' and siglist 'MultiBatchPooled'
      generic 'tau2' and siglist 'MultiBatch'
      generic 'theta' and siglist 'MultiBatch'
      generic 'theta<-' and siglist 'McmcChains,ANY'
      generic 'theta<-' and siglist 'MixtureModel,ANY'
      generic 'theta<-' and siglist 'MultiBatch,matrix'
      generic 'theta<-' and siglist 'MultiBatchModel,ANY'
      generic 'thin' and siglist 'MultiBatch'
      generic 'thin' and siglist 'MultiBatchList'
      generic 'thin<-' and siglist 'McmcParams,numeric'
      generic 'thin<-' and siglist 'MultiBatch,numeric'
      generic 'thin<-' and siglist 'MultiBatchList,numeric'
      generic 'triodata_lrr' and siglist 'TrioBatchModel'
      generic 'z' and siglist 'MultiBatch'
      generic 'zFreq' and siglist 'MultiBatch'
    All user-level objects in a package (including S4 classes and methods)
    should have documentation entries.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking for code/documentation mismatches ... WARNING
    ```
    ...
    Slots for class 'MultiBatch'
      Code: chains current_values data down_sample flags parameters specs
            summaries
      Docs: chains current_values data down_sample flags parameters
            summaries
    
    S4 class codoc mismatches from documentation object 'MultiBatchModel-class':
    Slots for class 'MultiBatchModel'
      Code: .internal.constraint .internal.counter batch batchElements data
            data.mean data.prec hyperparams k label_switch loglik logprior
            marginal_lik mcmc.chains mcmc.params modes mu nu.0 pi
            predictive probz sigma2 sigma2.0 tau2 theta u z zfreq zstar
      Inherited: k hyperparams theta sigma2 nu.0 sigma2.0 pi mu tau2
            predictive zstar data data.mean data.prec z zfreq probz u
            logprior loglik mcmc.chains batch batchElements modes
            mcmc.params label_switch marginal_lik .internal.constraint
            .internal.counter
      Docs: .internal.constraint batch batchElements data data.mean
            data.prec hyperparams is_mendelian k label_switch loglik
            logprior mcmc.chains mcmc.params modes mu nu.0 pi probz sigma2
            sigma2.0 tau2 theta z zfreq
    ```

*   checking Rd \usage sections ... WARNING
    ```
    ...
    
    Documented arguments not in \usage in documentation object 'iter<-':
      ‘force’
    
    Documented arguments not in \usage in documentation object 'mcmcParams':
      ‘force’
    
    Undocumented arguments in documentation object 'sigma<-'
      ‘value’
    
    Undocumented arguments in documentation object 'singleBatchGuided,MultiBatchList,MultiBatch-method'
      ‘x’ ‘guide’
    
    Undocumented arguments in documentation object 'theta'
      ‘value’
    
    Functions with \usage entries need to have the appropriate \alias
    entries, and all their arguments documented.
    The \usage entries must correspond to syntactically valid R code.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.2Mb
      sub-directories of 1Mb or more:
        R     2.1Mb
        doc   3.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘RcppArmadillo’
      All declared Imports should be used.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    batch<-,MultiBatch-numeric: no visible global function definition for
      ‘spec’
    batch<-,MultiBatch-numeric: no visible global function definition for
      ‘spec<-’
    coerce,McmcChains-list: no visible binding for global variable ‘s’
    computePrec,MultiBatch: no visible binding for global variable ‘prec’
    findSurrogates,MultiBatch: no visible binding for global variable ‘id’
    findSurrogates,MultiBatch: no visible binding for global variable
      ‘provisional_batch’
    findSurrogates,MultiBatch: no visible binding for global variable
      ‘batch_labels’
    sigma,MultiBatchCopyNumberPooled: no visible binding for global
      variable ‘s2’
    Undefined global functions or variables:
      . .gibbs_trios_mcmc2 .gibbs_trios_mcmc3 := batch_index batch_labels
      batches bk copy_number father id log_ratio maplabel medians model
      mother mprob nhom parents prec provisional_batch s s2 snpdat spec
      spec<- t.test value
    Consider adding
      importFrom("stats", "t.test")
    to your NAMESPACE file.
    ```

# CNVrd2

<details>

* Version: 1.22.0
* Source code: https://github.com/cran/CNVrd2
* URL: https://github.com/hoangtn/CNVrd2
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 96

Run `revdep_details(,"CNVrd2")` for more info

</details>

## In both

*   checking whether package ‘CNVrd2’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/CNVrd2/new/CNVrd2.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘CNVrd2’ ...
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rjags’:
 .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/CNVrd2/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/CNVrd2/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/CNVrd2/rjags/libs/rjags.so
  Reason: image not found
Error: package ‘rjags’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘CNVrd2’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/CNVrd2/new/CNVrd2.Rcheck/CNVrd2’

```
### CRAN

```
* installing *source* package ‘CNVrd2’ ...
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rjags’:
 .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/CNVrd2/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/CNVrd2/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/CNVrd2/rjags/libs/rjags.so
  Reason: image not found
Error: package ‘rjags’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘CNVrd2’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/CNVrd2/old/CNVrd2.Rcheck/CNVrd2’

```
# CollapsABEL

<details>

* Version: 0.10.11
* Source code: https://github.com/cran/CollapsABEL
* URL: https://bitbucket.org/kindlychung/collapsabel2/overview
* BugReports: https://bitbucket.org/kindlychung/collapsabel2/issues
* Date/Publication: 2016-12-11 20:35:07
* Number of recursive dependencies: 110

Run `revdep_details(,"CollapsABEL")` for more info

</details>

## In both

*   checking whether package ‘CollapsABEL’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/CollapsABEL/new/CollapsABEL.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘CollapsABEL’ ...
** package ‘CollapsABEL’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/CollapsABEL/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/CollapsABEL/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/CollapsABEL/rJava/libs/rJava.so
  Reason: image not found
Error: package ‘rJava’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘CollapsABEL’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/CollapsABEL/new/CollapsABEL.Rcheck/CollapsABEL’

```
### CRAN

```
* installing *source* package ‘CollapsABEL’ ...
** package ‘CollapsABEL’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/CollapsABEL/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/CollapsABEL/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/CollapsABEL/rJava/libs/rJava.so
  Reason: image not found
Error: package ‘rJava’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘CollapsABEL’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/CollapsABEL/old/CollapsABEL.Rcheck/CollapsABEL’

```
# CompGO

<details>

* Version: 1.20.0
* Source code: https://github.com/cran/CompGO
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 105

Run `revdep_details(,"CompGO")` for more info

</details>

## In both

*   checking whether package ‘CompGO’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/CompGO/new/CompGO.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘CompGO’ ...
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘RDAVIDWebService’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/CompGO/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/CompGO/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/CompGO/rJava/libs/rJava.so
  Reason: image not found
Error: package ‘RDAVIDWebService’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘CompGO’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/CompGO/new/CompGO.Rcheck/CompGO’

```
### CRAN

```
* installing *source* package ‘CompGO’ ...
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘RDAVIDWebService’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/CompGO/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/CompGO/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/CompGO/rJava/libs/rJava.so
  Reason: image not found
Error: package ‘RDAVIDWebService’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘CompGO’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/CompGO/old/CompGO.Rcheck/CompGO’

```
# crmPack

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/crmPack
* URL: https://github.com/roche/crmPack
* BugReports: https://github.com/roche/crmPack/issues
* Date/Publication: 2019-06-13 07:30:03 UTC
* Number of recursive dependencies: 62

Run `revdep_details(,"crmPack")` for more info

</details>

## In both

*   checking whether package ‘crmPack’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/crmPack/new/crmPack.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘crmPack’ ...
** package ‘crmPack’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/crmPack/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/crmPack/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/crmPack/rjags/libs/rjags.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘crmPack’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/crmPack/new/crmPack.Rcheck/crmPack’

```
### CRAN

```
* installing *source* package ‘crmPack’ ...
** package ‘crmPack’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/crmPack/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/crmPack/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/crmPack/rjags/libs/rjags.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘crmPack’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/crmPack/old/crmPack.Rcheck/crmPack’

```
# Crossover

<details>

* Version: 0.1-18
* Source code: https://github.com/cran/Crossover
* URL: https://github.com/kornl/Crossover/wiki
* BugReports: https://github.com/kornl/Crossover/issues
* Date/Publication: 2019-05-05 20:20:10 UTC
* Number of recursive dependencies: 71

Run `revdep_details(,"Crossover")` for more info

</details>

## In both

*   checking whether package ‘Crossover’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/Crossover/new/Crossover.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘Crossover’ ...
** package ‘Crossover’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/Crossover/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/Crossover/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c Crossover_init.c -o Crossover_init.o
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/Crossover/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/Crossover/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c searchCOD.cpp -o searchCOD.o
/usr/local/clang8/bin/clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/clang8/lib -o Crossover.so Crossover_init.o searchCOD.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/max/github/forks/ggplot2/revdep/checks.noindex/Crossover/new/Crossover.Rcheck/00LOCK-Crossover/00new/Crossover/libs
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/Crossover/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/Crossover/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/Crossover/rJava/libs/rJava.so
  Reason: image not found
Error: package ‘rJava’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘Crossover’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/Crossover/new/Crossover.Rcheck/Crossover’

```
### CRAN

```
* installing *source* package ‘Crossover’ ...
** package ‘Crossover’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/Crossover/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/Crossover/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c Crossover_init.c -o Crossover_init.o
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/Crossover/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/Crossover/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c searchCOD.cpp -o searchCOD.o
/usr/local/clang8/bin/clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/clang8/lib -o Crossover.so Crossover_init.o searchCOD.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/max/github/forks/ggplot2/revdep/checks.noindex/Crossover/old/Crossover.Rcheck/00LOCK-Crossover/00new/Crossover/libs
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/Crossover/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/Crossover/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/Crossover/rJava/libs/rJava.so
  Reason: image not found
Error: package ‘rJava’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘Crossover’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/Crossover/old/Crossover.Rcheck/Crossover’

```
# ctsem

<details>

* Version: 3.1.0
* Source code: https://github.com/cran/ctsem
* URL: https://github.com/cdriveraus/ctsem
* Date/Publication: 2020-01-27 10:10:02 UTC
* Number of recursive dependencies: 121

Run `revdep_details(,"ctsem")` for more info

</details>

## In both

*   R CMD check timed out
    

*   checking installed package size ... NOTE
    ```
      installed size is  8.7Mb
      sub-directories of 1Mb or more:
        R      1.0Mb
        libs   5.8Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘numDeriv’ ‘pkgbuild’
      All declared Imports should be used.
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# DaMiRseq

<details>

* Version: 1.8.0
* Source code: https://github.com/cran/DaMiRseq
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 228

Run `revdep_details(,"DaMiRseq")` for more info

</details>

## In both

*   checking whether package ‘DaMiRseq’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/DaMiRseq/new/DaMiRseq.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘DaMiRseq’ ...
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/DaMiRseq/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/DaMiRseq/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/DaMiRseq/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘DaMiRseq’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/DaMiRseq/new/DaMiRseq.Rcheck/DaMiRseq’

```
### CRAN

```
* installing *source* package ‘DaMiRseq’ ...
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/DaMiRseq/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/DaMiRseq/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/DaMiRseq/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘DaMiRseq’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/DaMiRseq/old/DaMiRseq.Rcheck/DaMiRseq’

```
# Deducer

<details>

* Version: 0.7-9
* Source code: https://github.com/cran/Deducer
* URL: http://www.deducer.org/manual.html http://www.fellstat.com
* Date/Publication: 2015-12-29 22:16:31
* Number of recursive dependencies: 122

Run `revdep_details(,"Deducer")` for more info

</details>

## In both

*   checking whether package ‘Deducer’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/Deducer/new/Deducer.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘Deducer’ ...
** package ‘Deducer’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/Deducer/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/Deducer/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/Deducer/rJava/libs/rJava.so
  Reason: image not found
Error: package ‘rJava’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘Deducer’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/Deducer/new/Deducer.Rcheck/Deducer’

```
### CRAN

```
* installing *source* package ‘Deducer’ ...
** package ‘Deducer’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/Deducer/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/Deducer/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/Deducer/rJava/libs/rJava.so
  Reason: image not found
Error: package ‘rJava’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘Deducer’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/Deducer/old/Deducer.Rcheck/Deducer’

```
# DiffBind

<details>

* Version: 2.12.0
* Source code: https://github.com/cran/DiffBind
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 165

Run `revdep_details(,"DiffBind")` for more info

</details>

## In both

*   R CMD check timed out
    

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘XLConnect’
    ```

*   checking R code for possible problems ... NOTE
    ```
    pv.DBAplotVolcano: no visible binding for global variable ‘Fold’
    pv.DBAplotVolcano: no visible binding for global variable ‘Legend’
    Undefined global functions or variables:
      Fold Legend
    ```

*   checking contents of ‘data’ directory ... NOTE
    ```
    Output for data("tamoxifen_analysis", package = "DiffBind"):
      
    Output for data("tamoxifen_counts", package = "DiffBind"):
      
    ```

*   checking compiled code ... NOTE
    ```
    File ‘DiffBind/libs/DiffBind.so’:
      Found ‘___stderrp’, possibly from ‘stderr’ (C)
        Object: ‘sam.o’
    
    Compiled code should not call entry points which might terminate R nor
    write to stdout/stderr instead of to the console, nor use Fortran I/O
    nor system RNGs.
    
    See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
    ```

# DistributionOptimization

<details>

* Version: 1.2.4
* Source code: https://github.com/cran/DistributionOptimization
* Date/Publication: 2019-11-25 14:30:02 UTC
* Number of recursive dependencies: 59

Run `revdep_details(,"DistributionOptimization")` for more info

</details>

## In both

*   checking whether package ‘DistributionOptimization’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/DistributionOptimization/new/DistributionOptimization.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘DistributionOptimization’ ...
** package ‘DistributionOptimization’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: object ‘OptimalNoBins’ is not exported by 'namespace:AdaptGauss'
Execution halted
ERROR: lazy loading failed for package ‘DistributionOptimization’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/DistributionOptimization/new/DistributionOptimization.Rcheck/DistributionOptimization’

```
### CRAN

```
* installing *source* package ‘DistributionOptimization’ ...
** package ‘DistributionOptimization’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: object ‘OptimalNoBins’ is not exported by 'namespace:AdaptGauss'
Execution halted
ERROR: lazy loading failed for package ‘DistributionOptimization’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/DistributionOptimization/old/DistributionOptimization.Rcheck/DistributionOptimization’

```
# DiversityOccupancy

<details>

* Version: 1.0.6
* Source code: https://github.com/cran/DiversityOccupancy
* Date/Publication: 2017-03-02 18:32:36
* Number of recursive dependencies: 86

Run `revdep_details(,"DiversityOccupancy")` for more info

</details>

## In both

*   checking whether package ‘DiversityOccupancy’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/DiversityOccupancy/new/DiversityOccupancy.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘DiversityOccupancy’ ...
** package ‘DiversityOccupancy’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/DiversityOccupancy/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/DiversityOccupancy/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/DiversityOccupancy/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘DiversityOccupancy’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/DiversityOccupancy/new/DiversityOccupancy.Rcheck/DiversityOccupancy’

```
### CRAN

```
* installing *source* package ‘DiversityOccupancy’ ...
** package ‘DiversityOccupancy’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/DiversityOccupancy/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/DiversityOccupancy/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/DiversityOccupancy/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘DiversityOccupancy’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/DiversityOccupancy/old/DiversityOccupancy.Rcheck/DiversityOccupancy’

```
# DuoClustering2018

<details>

* Version: 1.2.0
* Source code: https://github.com/cran/DuoClustering2018
* Date/Publication: 2019-05-07
* Number of recursive dependencies: 137

Run `revdep_details(,"DuoClustering2018")` for more info

</details>

## In both

*   checking whether package ‘DuoClustering2018’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/DuoClustering2018/new/DuoClustering2018.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘DuoClustering2018’ ...
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
Using temporary cache /var/folders/lb/xhxqmcrd7gv302_b1pdfykh80000gn/T//RtmpEnLrbp/BiocFileCache
snapshotDate(): 2019-10-22
Using temporary cache /var/folders/lb/xhxqmcrd7gv302_b1pdfykh80000gn/T//RtmpEnLrbp/BiocFileCache
Using temporary cache /var/folders/lb/xhxqmcrd7gv302_b1pdfykh80000gn/T//RtmpEnLrbp/BiocFileCache
Error: package or namespace load failed for ‘DuoClustering2018’:
 .onLoad failed in loadNamespace() for 'DuoClustering2018', details:
  call: FUN(X[[i]], ...)
  error: ‘duo_clustering_all_parameter_settings_v1’ not found in ExperimentHub
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/DuoClustering2018/new/DuoClustering2018.Rcheck/DuoClustering2018’

```
### CRAN

```
* installing *source* package ‘DuoClustering2018’ ...
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
Using temporary cache /var/folders/lb/xhxqmcrd7gv302_b1pdfykh80000gn/T//RtmpgoLGe2/BiocFileCache
snapshotDate(): 2019-10-22
Using temporary cache /var/folders/lb/xhxqmcrd7gv302_b1pdfykh80000gn/T//RtmpgoLGe2/BiocFileCache
Using temporary cache /var/folders/lb/xhxqmcrd7gv302_b1pdfykh80000gn/T//RtmpgoLGe2/BiocFileCache
Error: package or namespace load failed for ‘DuoClustering2018’:
 .onLoad failed in loadNamespace() for 'DuoClustering2018', details:
  call: FUN(X[[i]], ...)
  error: ‘duo_clustering_all_parameter_settings_v1’ not found in ExperimentHub
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/DuoClustering2018/old/DuoClustering2018.Rcheck/DuoClustering2018’

```
# dynr

<details>

* Version: 0.1.15-1
* Source code: https://github.com/cran/dynr
* Date/Publication: 2019-10-05 06:50:02 UTC
* Number of recursive dependencies: 119

Run `revdep_details(,"dynr")` for more info

</details>

## In both

*   checking whether package ‘dynr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/dynr/new/dynr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘dynr’ ...
** package ‘dynr’ successfully unpacked and MD5 sums checked
** using staged installation
checking for gsl-config... no
configure: error: gsl-config not found, is GSL installed?
ERROR: configuration failed for package ‘dynr’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/dynr/new/dynr.Rcheck/dynr’

```
### CRAN

```
* installing *source* package ‘dynr’ ...
** package ‘dynr’ successfully unpacked and MD5 sums checked
** using staged installation
checking for gsl-config... no
configure: error: gsl-config not found, is GSL installed?
ERROR: configuration failed for package ‘dynr’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/dynr/old/dynr.Rcheck/dynr’

```
# ELMER

<details>

* Version: 2.8.3
* Source code: https://github.com/cran/ELMER
* Date/Publication: 2019-09-06
* Number of recursive dependencies: 210

Run `revdep_details(,"ELMER")` for more info

</details>

## In both

*   R CMD check timed out
    

*   checking Rd \usage sections ... WARNING
    ```
    Undocumented arguments in documentation object 'heatmapGene'
      ‘correlation.method’ ‘scatter.plot.width’ ‘scatter.plot.height’
    
    Functions with \usage entries need to have the appropriate \alias
    entries, and all their arguments documented.
    The \usage entries must correspond to syntactically valid R code.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 75.6Mb
      sub-directories of 1Mb or more:
        doc  75.1Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    addMutCol: no visible binding for global variable 'Hugo_Symbol'
    calcDistNearestTSS: no visible binding for global variable
      'DistanceTSS'
    getRegionNearGenes : f: no visible binding for global variable 'Side'
    getRegionNearGenes: no visible binding for global variable 'ID'
    getTFtargets: no visible binding for global variable 'TF'
    heatmapGene: no visible global function definition for 'melt'
    heatmapGene: no visible binding for global variable 'mae'
    heatmapGene: no visible global function definition for 'stat_cor'
    Undefined global functions or variables:
      DistanceTSS Hugo_Symbol ID Side TF mae melt stat_cor
    ```

# epihet

<details>

* Version: 
* Source code: ???
* URL: http://ggplot2.tidyverse.org, https://github.com/tidyverse/ggplot2
* BugReports: https://github.com/tidyverse/ggplot2/issues
* Number of recursive dependencies: 0

Run `revdep_details(,"")` for more info

</details>

## Error before installation

### Devel

```

  There are binary versions available but the source versions are later:
           binary  source needs_compilation
checkmate   1.9.4   2.0.0              TRUE
nlme      3.1-143 3.1-144              TRUE
WGCNA        1.66    1.68              TRUE

  Binaries will be installed


installing the source package ‘reactome.db’



```
### CRAN

```

  There are binary versions available but the source versions are later:
           binary  source needs_compilation
checkmate   1.9.4   2.0.0              TRUE
nlme      3.1-143 3.1-144              TRUE
WGCNA        1.66    1.68              TRUE

  Binaries will be installed


installing the source package ‘reactome.db’



```
# esATAC

<details>

* Version: 1.6.1
* Source code: https://github.com/cran/esATAC
* URL: https://github.com/wzthu/esATAC
* BugReports: https://github.com/wzthu/esATAC/issues
* Date/Publication: 2019-05-15
* Number of recursive dependencies: 189

Run `revdep_details(,"esATAC")` for more info

</details>

## In both

*   checking whether package ‘esATAC’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/esATAC/new/esATAC.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘BSgenome.Hsapiens.UCSC.hg19’
    ```

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .BBSoptions
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

## Installation

### Devel

```
* installing *source* package ‘esATAC’ ...
** using staged installation
** libs
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DNDEBUG -DPLF_SYS_LINUX  -DR_EVN_FLAG -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/esATAC/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c BedLine.cpp -o BedLine.o
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DNDEBUG -DPLF_SYS_LINUX  -DR_EVN_FLAG -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/esATAC/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c BedUtils.cpp -o BedUtils.o
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DNDEBUG -DPLF_SYS_LINUX  -DR_EVN_FLAG -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/esATAC/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c ChrDivi.cpp -o ChrDivi.o
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DNDEBUG -DPLF_SYS_LINUX  -DR_EVN_FLAG -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/esATAC/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c CutCountPre.cpp -o CutCountPre.o
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DNDEBUG -DPLF_SYS_LINUX  -DR_EVN_FLAG -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/esATAC/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c CutSiteCount.cpp -o CutSiteCount.o
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DNDEBUG -DPLF_SYS_LINUX  -DR_EVN_FLAG -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/esATAC/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c LibComplexQC.cpp -o LibComplexQC.o
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DNDEBUG -DPLF_SYS_LINUX  -DR_EVN_FLAG -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/esATAC/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DNDEBUG -DPLF_SYS_LINUX  -DR_EVN_FLAG -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/esATAC/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c SortBed.cpp -o SortBed.o
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DNDEBUG -DPLF_SYS_LINUX  -DR_EVN_FLAG -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/esATAC/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c rcpp_wrapper.cpp -o rcpp_wrapper.o
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DNDEBUG -DPLF_SYS_LINUX  -DR_EVN_FLAG -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/esATAC/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c renamer.cpp -o renamer.o
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DNDEBUG -DPLF_SYS_LINUX  -DR_EVN_FLAG -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/esATAC/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c sam2bed.cc -o sam2bed.o
sam2bed.cc:125:16: warning: unused variable 'xspm' [-Wunused-variable]
    regmatch_t xspm[1];
               ^
sam2bed.cc:294:16: warning: unused variable 'xspm' [-Wunused-variable]
    regmatch_t xspm[1];
               ^
2 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/clang8/lib -o esATAC.so BedLine.o BedUtils.o ChrDivi.o CutCountPre.o CutSiteCount.o LibComplexQC.o RcppExports.o SortBed.o rcpp_wrapper.o renamer.o sam2bed.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/max/github/forks/ggplot2/revdep/checks.noindex/esATAC/new/esATAC.Rcheck/00LOCK-esATAC/00new/esATAC/libs
** R
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/esATAC/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/esATAC/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/esATAC/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘esATAC’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/esATAC/new/esATAC.Rcheck/esATAC’

```
### CRAN

```
* installing *source* package ‘esATAC’ ...
** using staged installation
** libs
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DNDEBUG -DPLF_SYS_LINUX  -DR_EVN_FLAG -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/esATAC/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c BedLine.cpp -o BedLine.o
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DNDEBUG -DPLF_SYS_LINUX  -DR_EVN_FLAG -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/esATAC/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c BedUtils.cpp -o BedUtils.o
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DNDEBUG -DPLF_SYS_LINUX  -DR_EVN_FLAG -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/esATAC/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c ChrDivi.cpp -o ChrDivi.o
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DNDEBUG -DPLF_SYS_LINUX  -DR_EVN_FLAG -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/esATAC/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c CutCountPre.cpp -o CutCountPre.o
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DNDEBUG -DPLF_SYS_LINUX  -DR_EVN_FLAG -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/esATAC/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c CutSiteCount.cpp -o CutSiteCount.o
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DNDEBUG -DPLF_SYS_LINUX  -DR_EVN_FLAG -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/esATAC/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c LibComplexQC.cpp -o LibComplexQC.o
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DNDEBUG -DPLF_SYS_LINUX  -DR_EVN_FLAG -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/esATAC/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DNDEBUG -DPLF_SYS_LINUX  -DR_EVN_FLAG -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/esATAC/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c SortBed.cpp -o SortBed.o
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DNDEBUG -DPLF_SYS_LINUX  -DR_EVN_FLAG -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/esATAC/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c rcpp_wrapper.cpp -o rcpp_wrapper.o
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DNDEBUG -DPLF_SYS_LINUX  -DR_EVN_FLAG -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/esATAC/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c renamer.cpp -o renamer.o
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DNDEBUG -DPLF_SYS_LINUX  -DR_EVN_FLAG -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/esATAC/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c sam2bed.cc -o sam2bed.o
sam2bed.cc:125:16: warning: unused variable 'xspm' [-Wunused-variable]
    regmatch_t xspm[1];
               ^
sam2bed.cc:294:16: warning: unused variable 'xspm' [-Wunused-variable]
    regmatch_t xspm[1];
               ^
2 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/clang8/lib -o esATAC.so BedLine.o BedUtils.o ChrDivi.o CutCountPre.o CutSiteCount.o LibComplexQC.o RcppExports.o SortBed.o rcpp_wrapper.o renamer.o sam2bed.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/max/github/forks/ggplot2/revdep/checks.noindex/esATAC/old/esATAC.Rcheck/00LOCK-esATAC/00new/esATAC/libs
** R
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/esATAC/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/esATAC/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/esATAC/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘esATAC’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/esATAC/old/esATAC.Rcheck/esATAC’

```
# evoper

<details>

* Version: 0.5.0
* Source code: https://github.com/cran/evoper
* URL: https://github.com/antonio-pgarcia/evoper
* BugReports: https://github.com/antonio-pgarcia/evoper/issues
* Date/Publication: 2018-08-30 23:20:06 UTC
* Number of recursive dependencies: 73

Run `revdep_details(,"evoper")` for more info

</details>

## In both

*   checking whether package ‘evoper’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/evoper/new/evoper.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘evoper’ ...
** package ‘evoper’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/evoper/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/evoper/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/evoper/rJava/libs/rJava.so
  Reason: image not found
Error: package ‘rJava’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘evoper’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/evoper/new/evoper.Rcheck/evoper’

```
### CRAN

```
* installing *source* package ‘evoper’ ...
** package ‘evoper’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/evoper/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/evoper/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/evoper/rJava/libs/rJava.so
  Reason: image not found
Error: package ‘rJava’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘evoper’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/evoper/old/evoper.Rcheck/evoper’

```
# ewoc

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/ewoc
* URL: https://github.com/dnzmarcio/ewoc/
* BugReports: https://github.com/dnzmarcio/ewoc/issues
* Date/Publication: 2018-01-20 21:16:49 UTC
* Number of recursive dependencies: 44

Run `revdep_details(,"ewoc")` for more info

</details>

## In both

*   checking whether package ‘ewoc’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/ewoc/new/ewoc.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ewoc’ ...
** package ‘ewoc’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/ewoc/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/ewoc/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/ewoc/rjags/libs/rjags.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘ewoc’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/ewoc/new/ewoc.Rcheck/ewoc’

```
### CRAN

```
* installing *source* package ‘ewoc’ ...
** package ‘ewoc’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/ewoc/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/ewoc/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/ewoc/rjags/libs/rjags.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘ewoc’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/ewoc/old/ewoc.Rcheck/ewoc’

```
# fgsea

<details>

* Version: 
* Source code: ???
* URL: http://ggplot2.tidyverse.org, https://github.com/tidyverse/ggplot2
* BugReports: https://github.com/tidyverse/ggplot2/issues
* Number of recursive dependencies: 0

Run `revdep_details(,"")` for more info

</details>

## Error before installation

### Devel

```

  There is a binary version available but the source version is later:
      binary  source needs_compilation
nlme 3.1-143 3.1-144              TRUE

  Binaries will be installed


installing the source package ‘reactome.db’



```
### CRAN

```

  There is a binary version available but the source version is later:
      binary  source needs_compilation
nlme 3.1-143 3.1-144              TRUE

  Binaries will be installed


installing the source package ‘reactome.db’



```
# fingerPro

<details>

* Version: 1.1
* Source code: https://github.com/cran/fingerPro
* URL: https://github.com/eead-csic-eesa
* Date/Publication: 2018-08-28 10:04:54 UTC
* Number of recursive dependencies: 141

Run `revdep_details(,"fingerPro")` for more info

</details>

## In both

*   checking whether package ‘fingerPro’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/fingerPro/new/fingerPro.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘fingerPro’ ...
** package ‘fingerPro’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/fingerPro/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/fingerPro/RcppGSL/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/fingerPro/RcppProgress/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/fingerPro/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/fingerPro/RcppGSL/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/fingerPro/RcppProgress/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c fingerprinting.cpp -o fingerprinting.o
In file included from RcppExports.cpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/fingerPro/RcppGSL/include/RcppGSL.h:25:
/Users/max/github/forks/ggplot2/revdep/library.noindex/fingerPro/RcppGSL/include/RcppGSLForward.h:26:10: fatal error: 'gsl/gsl_vector.h' file not found
#include <gsl/gsl_vector.h> 
         ^~~~~~~~~~~~~~~~~~
1 error generated.
make: *** [RcppExports.o] Error 1
make: *** Waiting for unfinished jobs....
In file included from fingerprinting.cpp:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/fingerPro/RcppGSL/include/RcppGSL.h:25:
/Users/max/github/forks/ggplot2/revdep/library.noindex/fingerPro/RcppGSL/include/RcppGSLForward.h:26:10: fatal error: 'gsl/gsl_vector.h' file not found
#include <gsl/gsl_vector.h> 
         ^~~~~~~~~~~~~~~~~~
1 error generated.
make: *** [fingerprinting.o] Error 1
ERROR: compilation failed for package ‘fingerPro’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/fingerPro/new/fingerPro.Rcheck/fingerPro’

```
### CRAN

```
* installing *source* package ‘fingerPro’ ...
** package ‘fingerPro’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/fingerPro/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/fingerPro/RcppGSL/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/fingerPro/RcppProgress/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/fingerPro/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/fingerPro/RcppGSL/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/fingerPro/RcppProgress/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c fingerprinting.cpp -o fingerprinting.o
In file included from RcppExports.cpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/fingerPro/RcppGSL/include/RcppGSL.h:25:
/Users/max/github/forks/ggplot2/revdep/library.noindex/fingerPro/RcppGSL/include/RcppGSLForward.h:26:10: fatal error: 'gsl/gsl_vector.h' file not found
#include <gsl/gsl_vector.h> 
         ^~~~~~~~~~~~~~~~~~
1 error generated.
make: *** [RcppExports.o] Error 1
make: *** Waiting for unfinished jobs....
In file included from fingerprinting.cpp:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/fingerPro/RcppGSL/include/RcppGSL.h:25:
/Users/max/github/forks/ggplot2/revdep/library.noindex/fingerPro/RcppGSL/include/RcppGSLForward.h:26:10: fatal error: 'gsl/gsl_vector.h' file not found
#include <gsl/gsl_vector.h> 
         ^~~~~~~~~~~~~~~~~~
1 error generated.
make: *** [fingerprinting.o] Error 1
ERROR: compilation failed for package ‘fingerPro’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/fingerPro/old/fingerPro.Rcheck/fingerPro’

```
# fsdaR

<details>

* Version: 0.4-9
* Source code: https://github.com/cran/fsdaR
* Date/Publication: 2020-01-14 16:40:02 UTC
* Number of recursive dependencies: 43

Run `revdep_details(,"fsdaR")` for more info

</details>

## In both

*   checking whether package ‘fsdaR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/fsdaR/new/fsdaR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘fsdaR’ ...
** package ‘fsdaR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/fsdaR/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/fsdaR/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/fsdaR/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘fsdaR’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/fsdaR/new/fsdaR.Rcheck/fsdaR’

```
### CRAN

```
* installing *source* package ‘fsdaR’ ...
** package ‘fsdaR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/fsdaR/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/fsdaR/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/fsdaR/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘fsdaR’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/fsdaR/old/fsdaR.Rcheck/fsdaR’

```
# G2Sd

<details>

* Version: 2.1.5
* Source code: https://github.com/cran/G2Sd
* URL: https://cran.r-project.org/package=G2Sd, http://regisgallon.wordpress.com/r-software/
* Date/Publication: 2015-12-07 22:13:45
* Number of recursive dependencies: 51

Run `revdep_details(,"G2Sd")` for more info

</details>

## In both

*   checking whether package ‘G2Sd’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/G2Sd/new/G2Sd.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘G2Sd’ ...
** package ‘G2Sd’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/G2Sd/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/G2Sd/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/G2Sd/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘G2Sd’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/G2Sd/new/G2Sd.Rcheck/G2Sd’

```
### CRAN

```
* installing *source* package ‘G2Sd’ ...
** package ‘G2Sd’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/G2Sd/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/G2Sd/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/G2Sd/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘G2Sd’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/G2Sd/old/G2Sd.Rcheck/G2Sd’

```
# GARS

<details>

* Version: 1.4.0
* Source code: https://github.com/cran/GARS
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 232

Run `revdep_details(,"GARS")` for more info

</details>

## In both

*   checking whether package ‘GARS’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/GARS/new/GARS.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘GARS’ ...
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/GARS/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/GARS/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/GARS/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘GARS’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/GARS/new/GARS.Rcheck/GARS’

```
### CRAN

```
* installing *source* package ‘GARS’ ...
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/GARS/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/GARS/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/GARS/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘GARS’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/GARS/old/GARS.Rcheck/GARS’

```
# genphen

<details>

* Version: 1.12.1
* Source code: https://github.com/cran/genphen
* Date/Publication: 2019-08-01
* Number of recursive dependencies: 89

Run `revdep_details(,"genphen")` for more info

</details>

## In both

*   R CMD check timed out
    

# ggdmc

<details>

* Version: 0.2.6.0
* Source code: https://github.com/cran/ggdmc
* URL: https://github.com/yxlin/ggdmc
* BugReports: https://github.com/yxlin/ggdmc/issues
* Date/Publication: 2019-04-29 05:10:03 UTC
* Number of recursive dependencies: 54

Run `revdep_details(,"ggdmc")` for more info

</details>

## In both

*   checking whether package ‘ggdmc’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/ggdmc/new/ggdmc.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ggdmc’ ...
** package ‘ggdmc’ successfully unpacked and MD5 sums checked
** using staged installation
checking whether the C++ compiler works... yes
checking for C++ compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... configure: error: in `/Users/max/github/forks/ggplot2/revdep/checks.noindex/ggdmc/new/ggdmc.Rcheck/00_pkg_src/ggdmc':
configure: error: cannot run C++ compiled programs.
If you meant to cross compile, use `--host'.
See `config.log' for more details
ERROR: configuration failed for package ‘ggdmc’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/ggdmc/new/ggdmc.Rcheck/ggdmc’

```
### CRAN

```
* installing *source* package ‘ggdmc’ ...
** package ‘ggdmc’ successfully unpacked and MD5 sums checked
** using staged installation
checking whether the C++ compiler works... yes
checking for C++ compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... configure: error: in `/Users/max/github/forks/ggplot2/revdep/checks.noindex/ggdmc/old/ggdmc.Rcheck/00_pkg_src/ggdmc':
configure: error: cannot run C++ compiled programs.
If you meant to cross compile, use `--host'.
See `config.log' for more details
ERROR: configuration failed for package ‘ggdmc’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/ggdmc/old/ggdmc.Rcheck/ggdmc’

```
# ggtern

<details>

* Version: 3.1.0
* Source code: https://github.com/cran/ggtern
* URL: http://www.ggtern.com
* Date/Publication: 2018-12-19 11:20:03 UTC
* Number of recursive dependencies: 46

Run `revdep_details(,"ggtern")` for more info

</details>

## Newly broken

*   checking whether package ‘ggtern’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/ggtern/new/ggtern.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ggtern’ ...
** package ‘ggtern’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** demo
** inst
** byte-compile and prepare package for lazy loading
Error in get(x, envir = ns, inherits = FALSE) : 
  object 'expand_default' not found
Error: unable to load R code in package ‘ggtern’
Execution halted
ERROR: lazy loading failed for package ‘ggtern’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/ggtern/new/ggtern.Rcheck/ggtern’

```
### CRAN

```
* installing *source* package ‘ggtern’ ...
** package ‘ggtern’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** demo
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (ggtern)

```
# GGtools

<details>

* Version: 5.20.0
* Source code: https://github.com/cran/GGtools
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 149

Run `revdep_details(,"GGtools")` for more info

</details>

## In both

*   R CMD check timed out
    

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      'MatrixEQTL', 'gwascat'
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 73.5Mb
      sub-directories of 1Mb or more:
        R       1.0Mb
        data   27.0Mb
        doc     1.6Mb
        parts   2.0Mb
        pup     2.0Mb
        rdas   10.3Mb
        vcf    28.8Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' call to 'Homo.sapiens' which was already attached by Depends.
      Please remove these calls from your code.
    Packages in Depends field not imported from:
      'Homo.sapiens' 'parallel'
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    topKfeats: no visible binding for global variable 'i2'
    tscan2df: no visible global function definition for '%dopar%'
    tscan2df: no visible global function definition for 'foreach'
    tscan2df: no visible binding for global variable 'i'
    tscan2gr: no visible global function definition for '%dopar%'
    tscan2gr: no visible global function definition for 'foreach'
    tscan2gr: no visible binding for global variable 'i'
    waldtests : <anonymous>: no visible global function definition for
      'wald.test'
    plot,gwSnpScreenResult-character: no visible global function definition
      for 'snpcount'
    plot,gwSnpScreenResult-character: no visible global function definition
      for 'snpsBySeqname'
    Undefined global functions or variables:
      %dopar% .N FDR Homo.sapiens Matrix_eQTL_engine SlicedData
      as.data.table assay assays bindcadd bindmaf chi.squared colData curp
      detectCores excl export.gff3 firstHalf firstThird foreach forestplot
      gwastagger gwrngs hg19.si.df hmm878 i i1 i2 lastThird maf mafs
      mclapply midThird modelLINEAR npc overlapsAny pl radiusUsed ranges<-
      rowRanges runOneSplit select setkey setkeyv setnames snp snpcount
      snpsBySeqname target tileGenome value wald.test x
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘MatrixEQTL’, ‘gwascat’
    ```

# GRENITS

<details>

* Version: 1.36.0
* Source code: https://github.com/cran/GRENITS
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 39

Run `revdep_details(,"GRENITS")` for more info

</details>

## In both

*   checking whether package ‘GRENITS’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/GRENITS/new/GRENITS.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘GRENITS’ ...
** using staged installation
** libs
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c AR1_Gauss_biocond.cc -o AR1_Gauss_biocond.o
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c AR1_Student_biocond.cc -o AR1_Student_biocond.o
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c AR1_biocond.cc -o AR1_biocond.o
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c PSplines_biocond.cc -o PSplines_biocond.o
AR1_Gauss_biocond.cc:49:30: warning: unused variable 'numDiag' [-Wunused-variable]
  int           num_fixedON, numDiag, p_sqr, free_gammas;
                             ^
AR1_biocond.cc:48:30: warning: unused variable 'numDiag' [-Wunused-variable]
  int           num_fixedON, numDiag, p_sqr, free_gammas;
                             ^
AR1_Student_biocond.cc:49:30: warning: unused variable 'numDiag' [-Wunused-variable]
  int           num_fixedON, numDiag, p_sqr, free_gammas;
                             ^
PSplines_biocond.cc:45:63: warning: unused variable 'numDiag' [-Wunused-variable]
  int                 num_fixedON, nodesSpline, degreeSpline, numDiag, free_gammas;
                                                              ^
1 warning generated.
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c R_C_interface.cpp -o R_C_interface.o
1 warning generated.
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c ReadMCMCFiles.cpp -o ReadMCMCFiles.o
1 warning generated.
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c commonFunctions.cc -o commonFunctions.o
1 warning generated.
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c functionsErrorModel.cc -o functionsErrorModel.o
commonFunctions.cc:142:3: error: no matching function for call to 'dtrtrs_'
  arma_fortran(arma_dtrtrs)( &upper_tri, &trs, &nu, &m, &nrhs, R.memptr(), 
  ^~~~~~~~~~~~~~~~~~~~~~~~~
/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/RcppArmadillo/include/armadillo_bits/compiler_setup.hpp:68:34: note: expanded from macro 'arma_fortran'
  #define arma_fortran(function) arma_fortran_sans_prefix_B(function)
                                 ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/RcppArmadillo/include/armadillo_bits/compiler_setup.hpp:44:48: note: expanded from macro 'arma_fortran_sans_prefix_B'
  #define arma_fortran_sans_prefix_B(function) function##_
                                               ^~~~~~~~~~~
<scratch space>:113:1: note: expanded from here
dtrtrs_
^~~~~~~
/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/RcppArmadillo/include/armadillo_bits/def_lapack.hpp:668:8: note: candidate function not viable: requires 13 arguments, but 10 were provided
  void arma_fortran(arma_dtrtrs)(const char* uplo, const char* trans, const char* diag, const blas_int* n, const blas_int* nrhs, const   double* a, const blas_int* lda,   double* b, const blas_int* ldb, blas_int* info, blas_len uplo_len, blas_len trans_len, blas_len diag_len);
       ^
/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/RcppArmadillo/include/armadillo_bits/compiler_setup.hpp:68:34: note: expanded from macro 'arma_fortran'
  #define arma_fortran(function) arma_fortran_sans_prefix_B(function)
                                 ^
/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/RcppArmadillo/include/armadillo_bits/compiler_setup.hpp:44:48: note: expanded from macro 'arma_fortran_sans_prefix_B'
  #define arma_fortran_sans_prefix_B(function) function##_
                                               ^
<scratch space>:40:1: note: expanded from here
dtrtrs_
^
commonFunctions.cc:440:59: warning: unused variable 'num_on' [-Wunused-variable]
  unsigned int                                            num_on;
                                                          ^
commonFunctions.cc:482:59: warning: unused variable 'num_on' [-Wunused-variable]
  unsigned int                                            num_on;
                                                          ^
2 warnings and 1 error generated.
make: *** [commonFunctions.o] Error 1
make: *** Waiting for unfinished jobs....
ERROR: compilation failed for package ‘GRENITS’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/GRENITS/new/GRENITS.Rcheck/GRENITS’

```
### CRAN

```
* installing *source* package ‘GRENITS’ ...
** using staged installation
** libs
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c AR1_Gauss_biocond.cc -o AR1_Gauss_biocond.o
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c AR1_Student_biocond.cc -o AR1_Student_biocond.o
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c AR1_biocond.cc -o AR1_biocond.o
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c PSplines_biocond.cc -o PSplines_biocond.o
AR1_biocond.cc:48:30: warning: unused variable 'numDiag' [-Wunused-variable]
  int           num_fixedON, numDiag, p_sqr, free_gammas;
                             ^
PSplines_biocond.cc:45:63: warning: unused variable 'numDiag' [-Wunused-variable]
  int                 num_fixedON, nodesSpline, degreeSpline, numDiag, free_gammas;
                                                              ^
AR1_Gauss_biocond.cc:49:30: warning: unused variable 'numDiag' [-Wunused-variable]
  int           num_fixedON, numDiag, p_sqr, free_gammas;
                             ^
AR1_Student_biocond.cc:49:30: warning: unused variable 'numDiag' [-Wunused-variable]
  int           num_fixedON, numDiag, p_sqr, free_gammas;
                             ^
1 warning generated.
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c R_C_interface.cpp -o R_C_interface.o
1 warning generated.
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c ReadMCMCFiles.cpp -o ReadMCMCFiles.o
1 warning generated.
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c commonFunctions.cc -o commonFunctions.o
1 warning generated.
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c functionsErrorModel.cc -o functionsErrorModel.o
commonFunctions.cc:142:3: error: no matching function for call to 'dtrtrs_'
  arma_fortran(arma_dtrtrs)( &upper_tri, &trs, &nu, &m, &nrhs, R.memptr(), 
  ^~~~~~~~~~~~~~~~~~~~~~~~~
/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/RcppArmadillo/include/armadillo_bits/compiler_setup.hpp:68:34: note: expanded from macro 'arma_fortran'
  #define arma_fortran(function) arma_fortran_sans_prefix_B(function)
                                 ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/RcppArmadillo/include/armadillo_bits/compiler_setup.hpp:44:48: note: expanded from macro 'arma_fortran_sans_prefix_B'
  #define arma_fortran_sans_prefix_B(function) function##_
                                               ^~~~~~~~~~~
<scratch space>:113:1: note: expanded from here
dtrtrs_
^~~~~~~
/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/RcppArmadillo/include/armadillo_bits/def_lapack.hpp:668:8: note: candidate function not viable: requires 13 arguments, but 10 were provided
  void arma_fortran(arma_dtrtrs)(const char* uplo, const char* trans, const char* diag, const blas_int* n, const blas_int* nrhs, const   double* a, const blas_int* lda,   double* b, const blas_int* ldb, blas_int* info, blas_len uplo_len, blas_len trans_len, blas_len diag_len);
       ^
/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/RcppArmadillo/include/armadillo_bits/compiler_setup.hpp:68:34: note: expanded from macro 'arma_fortran'
  #define arma_fortran(function) arma_fortran_sans_prefix_B(function)
                                 ^
/Users/max/github/forks/ggplot2/revdep/library.noindex/GRENITS/RcppArmadillo/include/armadillo_bits/compiler_setup.hpp:44:48: note: expanded from macro 'arma_fortran_sans_prefix_B'
  #define arma_fortran_sans_prefix_B(function) function##_
                                               ^
<scratch space>:40:1: note: expanded from here
dtrtrs_
^
commonFunctions.cc:440:59: warning: unused variable 'num_on' [-Wunused-variable]
  unsigned int                                            num_on;
                                                          ^
commonFunctions.cc:482:59: warning: unused variable 'num_on' [-Wunused-variable]
  unsigned int                                            num_on;
                                                          ^
2 warnings and 1 error generated.
make: *** [commonFunctions.o] Error 1
make: *** Waiting for unfinished jobs....
ERROR: compilation failed for package ‘GRENITS’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/GRENITS/old/GRENITS.Rcheck/GRENITS’

```
# GUIgems

<details>

* Version: 0.1
* Source code: https://github.com/cran/GUIgems
* Date/Publication: 2017-05-18 14:14:46 UTC
* Number of recursive dependencies: 43

Run `revdep_details(,"GUIgems")` for more info

</details>

## In both

*   checking whether package ‘GUIgems’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/GUIgems/new/GUIgems.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘GUIgems’ ...
** package ‘GUIgems’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** demo
** byte-compile and prepare package for lazy loading
Warning in fun(libname, pkgname) : couldn't connect to display ""
Error in structure(.External(.C_dotTcl, ...), class = "tclObj") : 
  [tcl] can't find package BWidget.

Error : package or namespace load failed for ‘rpanel’:
 unable to load R code in package ‘rpanel’
Error: unable to load R code in package ‘GUIgems’
Execution halted
ERROR: lazy loading failed for package ‘GUIgems’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/GUIgems/new/GUIgems.Rcheck/GUIgems’

```
### CRAN

```
* installing *source* package ‘GUIgems’ ...
** package ‘GUIgems’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** demo
** byte-compile and prepare package for lazy loading
Warning in fun(libname, pkgname) : couldn't connect to display ""
Error in structure(.External(.C_dotTcl, ...), class = "tclObj") : 
  [tcl] can't find package BWidget.

Error : package or namespace load failed for ‘rpanel’:
 unable to load R code in package ‘rpanel’
Error: unable to load R code in package ‘GUIgems’
Execution halted
ERROR: lazy loading failed for package ‘GUIgems’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/GUIgems/old/GUIgems.Rcheck/GUIgems’

```
# hbbr

<details>

* Version: 1.1.2
* Source code: https://github.com/cran/hbbr
* Date/Publication: 2019-10-25 08:20:02 UTC
* Number of recursive dependencies: 57

Run `revdep_details(,"hbbr")` for more info

</details>

## In both

*   checking whether package ‘hbbr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/hbbr/new/hbbr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘hbbr’ ...
** package ‘hbbr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rjags’:
 .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/hbbr/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/hbbr/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/hbbr/rjags/libs/rjags.so
  Reason: image not found
Error: package ‘rjags’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘hbbr’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/hbbr/new/hbbr.Rcheck/hbbr’

```
### CRAN

```
* installing *source* package ‘hbbr’ ...
** package ‘hbbr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rjags’:
 .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/hbbr/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/hbbr/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/hbbr/rjags/libs/rjags.so
  Reason: image not found
Error: package ‘rjags’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘hbbr’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/hbbr/old/hbbr.Rcheck/hbbr’

```
# HMP16SData

<details>

* Version: 1.4.1
* Source code: https://github.com/cran/HMP16SData
* URL: https://github.com/waldronlab/HMP16SData
* BugReports: https://github.com/waldronlab/HMP16SData/issues
* Date/Publication: 2019-05-23
* Number of recursive dependencies: 180

Run `revdep_details(,"HMP16SData")` for more info

</details>

## In both

*   checking whether package ‘HMP16SData’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/HMP16SData/new/HMP16SData.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘HMP16SData’ ...
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
Using temporary cache /var/folders/lb/xhxqmcrd7gv302_b1pdfykh80000gn/T//RtmpGMzVeI/BiocFileCache
snapshotDate(): 2019-10-22
Using temporary cache /var/folders/lb/xhxqmcrd7gv302_b1pdfykh80000gn/T//RtmpGMzVeI/BiocFileCache
Using temporary cache /var/folders/lb/xhxqmcrd7gv302_b1pdfykh80000gn/T//RtmpGMzVeI/BiocFileCache
Using temporary cache /var/folders/lb/xhxqmcrd7gv302_b1pdfykh80000gn/T//RtmpGMzVeI/BiocFileCache
Error: package or namespace load failed for ‘HMP16SData’:
 .onLoad failed in loadNamespace() for 'HMP16SData', details:
  call: function_list[[k]](value)
  error: ‘V13’ not found in ExperimentHub
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/HMP16SData/new/HMP16SData.Rcheck/HMP16SData’

```
### CRAN

```
* installing *source* package ‘HMP16SData’ ...
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
Using temporary cache /var/folders/lb/xhxqmcrd7gv302_b1pdfykh80000gn/T//Rtmp6E5Ujw/BiocFileCache
snapshotDate(): 2019-10-22
Using temporary cache /var/folders/lb/xhxqmcrd7gv302_b1pdfykh80000gn/T//Rtmp6E5Ujw/BiocFileCache
Using temporary cache /var/folders/lb/xhxqmcrd7gv302_b1pdfykh80000gn/T//Rtmp6E5Ujw/BiocFileCache
Using temporary cache /var/folders/lb/xhxqmcrd7gv302_b1pdfykh80000gn/T//Rtmp6E5Ujw/BiocFileCache
Error: package or namespace load failed for ‘HMP16SData’:
 .onLoad failed in loadNamespace() for 'HMP16SData', details:
  call: function_list[[k]](value)
  error: ‘V13’ not found in ExperimentHub
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/HMP16SData/old/HMP16SData.Rcheck/HMP16SData’

```
# iBMQ

<details>

* Version: 1.24.0
* Source code: https://github.com/cran/iBMQ
* URL: http://www.rglab.org
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 39

Run `revdep_details(,"iBMQ")` for more info

</details>

## In both

*   checking whether package ‘iBMQ’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/iBMQ/new/iBMQ.Rcheck/00install.out’ for details.
    ```

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .BBSoptions
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

## Installation

### Devel

```
* installing *source* package ‘iBMQ’ ...
** using staged installation
checking for gcc... /usr/local/clang8/bin/clang
checking whether the C compiler works... yes
checking for C compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether we are using the GNU C compiler... yes
checking whether /usr/local/clang8/bin/clang accepts -g... yes
checking for /usr/local/clang8/bin/clang option to accept ISO C89... none needed
checking how to run the C preprocessor... /usr/local/clang8/bin/clang -E
checking for pkg-config... /usr/local/bin/pkg-config
checking pkg-config is at least version 0.9.0... yes
checking for GSL... no
checking for gsl-config... no
checking for GSL - version >= 1.2... no
*** The gsl-config script installed by GSL could not be found
*** If GSL was installed in PREFIX, make sure PREFIX/bin is in
*** your path, or set the GSL_CONFIG environment variable to the
*** full path to gsl-config.
configure: error: Cannot find Gnu Scientific Library >=1.6
ERROR: configuration failed for package ‘iBMQ’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/iBMQ/new/iBMQ.Rcheck/iBMQ’

```
### CRAN

```
* installing *source* package ‘iBMQ’ ...
** using staged installation
checking for gcc... /usr/local/clang8/bin/clang
checking whether the C compiler works... yes
checking for C compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether we are using the GNU C compiler... yes
checking whether /usr/local/clang8/bin/clang accepts -g... yes
checking for /usr/local/clang8/bin/clang option to accept ISO C89... none needed
checking how to run the C preprocessor... /usr/local/clang8/bin/clang -E
checking for pkg-config... /usr/local/bin/pkg-config
checking pkg-config is at least version 0.9.0... yes
checking for GSL... no
checking for gsl-config... no
checking for GSL - version >= 1.2... no
*** The gsl-config script installed by GSL could not be found
*** If GSL was installed in PREFIX, make sure PREFIX/bin is in
*** your path, or set the GSL_CONFIG environment variable to the
*** full path to gsl-config.
configure: error: Cannot find Gnu Scientific Library >=1.6
ERROR: configuration failed for package ‘iBMQ’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/iBMQ/old/iBMQ.Rcheck/iBMQ’

```
# iCNV

<details>

* Version: 1.4.0
* Source code: https://github.com/cran/iCNV
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 91

Run `revdep_details(,"iCNV")` for more info

</details>

## In both

*   checking whether package ‘iCNV’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/iCNV/new/iCNV.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘iCNV’ ...
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package ‘BSgenome.Hsapiens.UCSC.hg19’ required by ‘CODEX’ could not be found
Execution halted
ERROR: lazy loading failed for package ‘iCNV’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/iCNV/new/iCNV.Rcheck/iCNV’

```
### CRAN

```
* installing *source* package ‘iCNV’ ...
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package ‘BSgenome.Hsapiens.UCSC.hg19’ required by ‘CODEX’ could not be found
Execution halted
ERROR: lazy loading failed for package ‘iCNV’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/iCNV/old/iCNV.Rcheck/iCNV’

```
# imageData

<details>

* Version: 0.1-59
* Source code: https://github.com/cran/imageData
* URL: http://chris.brien.name
* Date/Publication: 2019-05-15 13:50:02 UTC
* Number of recursive dependencies: 89

Run `revdep_details(,"imageData")` for more info

</details>

## In both

*   checking whether package ‘imageData’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/imageData/new/imageData.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘imageData’ ...
** package ‘imageData’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/imageData/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/imageData/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/imageData/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘imageData’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/imageData/new/imageData.Rcheck/imageData’

```
### CRAN

```
* installing *source* package ‘imageData’ ...
** package ‘imageData’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/imageData/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/imageData/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/imageData/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘imageData’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/imageData/old/imageData.Rcheck/imageData’

```
# infercnv

<details>

* Version: 1.0.4
* Source code: https://github.com/cran/infercnv
* URL: https://github.com/broadinstitute/inferCNV/wiki
* BugReports: https://github.com/broadinstitute/inferCNV/issues
* Date/Publication: 2019-09-16
* Number of recursive dependencies: 127

Run `revdep_details(,"infercnv")` for more info

</details>

## In both

*   checking whether package ‘infercnv’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/infercnv/new/infercnv.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘infercnv’ ...
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/infercnv/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/infercnv/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/infercnv/rjags/libs/rjags.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘infercnv’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/infercnv/new/infercnv.Rcheck/infercnv’

```
### CRAN

```
* installing *source* package ‘infercnv’ ...
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/infercnv/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/infercnv/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/infercnv/rjags/libs/rjags.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘infercnv’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/infercnv/old/infercnv.Rcheck/infercnv’

```
# InSilicoVA

<details>

* Version: 1.2.5
* Source code: https://github.com/cran/InSilicoVA
* URL: https://github.com/verbal-autopsy-software/InSilicoVA
* BugReports: https://github.com/verbal-autopsy-software/InSilicoVA/issues
* Date/Publication: 2018-10-29 05:40:11 UTC
* Number of recursive dependencies: 47

Run `revdep_details(,"InSilicoVA")` for more info

</details>

## In both

*   checking whether package ‘InSilicoVA’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/InSilicoVA/new/InSilicoVA.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘InSilicoVA’ ...
** package ‘InSilicoVA’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/InSilicoVA/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/InSilicoVA/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/InSilicoVA/rJava/libs/rJava.so
  Reason: image not found
Error: package ‘rJava’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘InSilicoVA’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/InSilicoVA/new/InSilicoVA.Rcheck/InSilicoVA’

```
### CRAN

```
* installing *source* package ‘InSilicoVA’ ...
** package ‘InSilicoVA’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/InSilicoVA/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/InSilicoVA/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/InSilicoVA/rJava/libs/rJava.so
  Reason: image not found
Error: package ‘rJava’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘InSilicoVA’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/InSilicoVA/old/InSilicoVA.Rcheck/InSilicoVA’

```
# jarbes

<details>

* Version: 1.7.2
* Source code: https://github.com/cran/jarbes
* Date/Publication: 2019-03-11 16:50:03 UTC
* Number of recursive dependencies: 64

Run `revdep_details(,"jarbes")` for more info

</details>

## In both

*   checking whether package ‘jarbes’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/jarbes/new/jarbes.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘jarbes’ ...
** package ‘jarbes’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/jarbes/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/jarbes/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/jarbes/rjags/libs/rjags.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘jarbes’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/jarbes/new/jarbes.Rcheck/jarbes’

```
### CRAN

```
* installing *source* package ‘jarbes’ ...
** package ‘jarbes’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/jarbes/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/jarbes/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/jarbes/rjags/libs/rjags.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘jarbes’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/jarbes/old/jarbes.Rcheck/jarbes’

```
# JointAI

<details>

* Version: 0.6.0
* Source code: https://github.com/cran/JointAI
* URL: https://nerler.github.io/JointAI
* BugReports: https://github.com/nerler/JointAI/issues
* Date/Publication: 2019-08-31 17:40:02 UTC
* Number of recursive dependencies: 85

Run `revdep_details(,"JointAI")` for more info

</details>

## In both

*   checking whether package ‘JointAI’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/JointAI/new/JointAI.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘JointAI’ ...
** package ‘JointAI’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rjags’:
 .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/JointAI/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/JointAI/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/JointAI/rjags/libs/rjags.so
  Reason: image not found
Error: package ‘rjags’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘JointAI’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/JointAI/new/JointAI.Rcheck/JointAI’

```
### CRAN

```
* installing *source* package ‘JointAI’ ...
** package ‘JointAI’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rjags’:
 .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/JointAI/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/JointAI/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/JointAI/rjags/libs/rjags.so
  Reason: image not found
Error: package ‘rjags’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘JointAI’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/JointAI/old/JointAI.Rcheck/JointAI’

```
# L0Learn

<details>

* Version: 1.2.0
* Source code: https://github.com/cran/L0Learn
* URL: https://arxiv.org/abs/1803.01454
* Date/Publication: 2019-08-30 15:00:02 UTC
* Number of recursive dependencies: 50

Run `revdep_details(,"L0Learn")` for more info

</details>

## In both

*   checking whether package ‘L0Learn’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/L0Learn/new/L0Learn.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘L0Learn’ ...
** package ‘L0Learn’ successfully unpacked and MD5 sums checked
** using staged installation
checking whether the C++ compiler works... yes
checking for C++ compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... configure: error: in `/Users/max/github/forks/ggplot2/revdep/checks.noindex/L0Learn/new/L0Learn.Rcheck/00_pkg_src/L0Learn':
configure: error: cannot run C++ compiled programs.
If you meant to cross compile, use `--host'.
See `config.log' for more details
ERROR: configuration failed for package ‘L0Learn’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/L0Learn/new/L0Learn.Rcheck/L0Learn’

```
### CRAN

```
* installing *source* package ‘L0Learn’ ...
** package ‘L0Learn’ successfully unpacked and MD5 sums checked
** using staged installation
checking whether the C++ compiler works... yes
checking for C++ compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... configure: error: in `/Users/max/github/forks/ggplot2/revdep/checks.noindex/L0Learn/old/L0Learn.Rcheck/00_pkg_src/L0Learn':
configure: error: cannot run C++ compiled programs.
If you meant to cross compile, use `--host'.
See `config.log' for more details
ERROR: configuration failed for package ‘L0Learn’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/L0Learn/old/L0Learn.Rcheck/L0Learn’

```
# lilikoi

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/lilikoi
* URL: https://github.com/lanagarmire/lilikoi
* BugReports: https://github.com/lanagarmire/lilikoi/issues
* Date/Publication: 2018-07-30 11:10:03 UTC
* Number of recursive dependencies: 150

Run `revdep_details(,"lilikoi")` for more info

</details>

## In both

*   checking whether package ‘lilikoi’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/lilikoi/new/lilikoi.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘lilikoi’ ...
** package ‘lilikoi’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/lilikoi/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/lilikoi/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/lilikoi/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘lilikoi’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/lilikoi/new/lilikoi.Rcheck/lilikoi’

```
### CRAN

```
* installing *source* package ‘lilikoi’ ...
** package ‘lilikoi’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/lilikoi/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/lilikoi/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/lilikoi/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘lilikoi’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/lilikoi/old/lilikoi.Rcheck/lilikoi’

```
# LINC

<details>

* Version: 
* Source code: ???
* URL: http://ggplot2.tidyverse.org, https://github.com/tidyverse/ggplot2
* BugReports: https://github.com/tidyverse/ggplot2/issues
* Number of recursive dependencies: 0

Run `revdep_details(,"")` for more info

</details>

## Error before installation

### Devel

```

  There are binary versions available but the source versions are later:
           binary  source needs_compilation
checkmate   1.9.4   2.0.0              TRUE
nlme      3.1-143 3.1-144              TRUE

  Binaries will be installed


Warning: unable to access index for repository https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6:
  cannot open URL 'https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6/PACKAGES'
Warning: unable to access index for repository https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6:
  cannot open URL 'https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6/PACKAGES'
installing the source package ‘reactome.db’



```
### CRAN

```

  There are binary versions available but the source versions are later:
           binary  source needs_compilation
checkmate   1.9.4   2.0.0              TRUE
nlme      3.1-143 3.1-144              TRUE

  Binaries will be installed


Warning: unable to access index for repository https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6:
  cannot open URL 'https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6/PACKAGES'
Warning: unable to access index for repository https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6:
  cannot open URL 'https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6/PACKAGES'
installing the source package ‘reactome.db’



```
# llama

<details>

* Version: 0.9.2
* Source code: https://github.com/cran/llama
* URL: https://bitbucket.org/lkotthoff/llama
* Date/Publication: 2018-07-11 14:30:03 UTC
* Number of recursive dependencies: 60

Run `revdep_details(,"llama")` for more info

</details>

## In both

*   checking whether package ‘llama’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/llama/new/llama.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘llama’ ...
** package ‘llama’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/llama/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/llama/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/llama/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘llama’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/llama/new/llama.Rcheck/llama’

```
### CRAN

```
* installing *source* package ‘llama’ ...
** package ‘llama’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/llama/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/llama/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/llama/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘llama’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/llama/old/llama.Rcheck/llama’

```
# LLSR

<details>

* Version: 0.0.2.19
* Source code: https://github.com/cran/LLSR
* URL: https://CRAN.R-project.org/package=LLSR
* BugReports: https://github.com/diegofcoelho/LLSR/issues
* Date/Publication: 2019-03-05 22:20:11 UTC
* Number of recursive dependencies: 66

Run `revdep_details(,"LLSR")` for more info

</details>

## In both

*   checking whether package ‘LLSR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/LLSR/new/LLSR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘LLSR’ ...
** package ‘LLSR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/LLSR/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/LLSR/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/LLSR/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘LLSR’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/LLSR/new/LLSR.Rcheck/LLSR’

```
### CRAN

```
* installing *source* package ‘LLSR’ ...
** package ‘LLSR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/LLSR/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/LLSR/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/LLSR/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘LLSR’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/LLSR/old/LLSR.Rcheck/LLSR’

```
# maEndToEnd

<details>

* Version: 
* Source code: ???
* URL: http://ggplot2.tidyverse.org, https://github.com/tidyverse/ggplot2
* BugReports: https://github.com/tidyverse/ggplot2/issues
* Number of recursive dependencies: 0

Run `revdep_details(,"")` for more info

</details>

## Error before installation

### Devel

```

  There are binary versions available but the source versions are later:
           binary  source needs_compilation
checkmate   1.9.4   2.0.0              TRUE
nlme      3.1-143 3.1-144              TRUE

  Binaries will be installed


Warning: unable to access index for repository https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6:
  cannot open URL 'https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6/PACKAGES'
Warning: unable to access index for repository https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6:
  cannot open URL 'https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6/PACKAGES'
installing the source packages ‘hugene10sttranscriptcluster.db’, ‘reactome.db’



```
### CRAN

```

  There are binary versions available but the source versions are later:
           binary  source needs_compilation
checkmate   1.9.4   2.0.0              TRUE
nlme      3.1-143 3.1-144              TRUE

  Binaries will be installed


Warning: unable to access index for repository https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6:
  cannot open URL 'https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6/PACKAGES'
Warning: unable to access index for repository https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6:
  cannot open URL 'https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6/PACKAGES'
installing the source packages ‘hugene10sttranscriptcluster.db’, ‘reactome.db’



```
# matchingMarkets

<details>

* Version: 1.0-2
* Source code: https://github.com/cran/matchingMarkets
* URL: http://matchingMarkets.org, http://klein.uk
* BugReports: https://github.com/thiloklein/matchingMarkets/issues
* Date/Publication: 2020-01-12 10:00:02 UTC
* Number of recursive dependencies: 52

Run `revdep_details(,"matchingMarkets")` for more info

</details>

## In both

*   checking whether package ‘matchingMarkets’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/matchingMarkets/new/matchingMarkets.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘matchingMarkets’ ...
** package ‘matchingMarkets’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppProgress/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c Options.cc -o Options.o
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppProgress/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppProgress/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c System.cc -o System.o
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppProgress/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c kprmatcher.cc -o kprmatcher.o
In file included from System.cc:24:
In file included from ../inst/include/System.h:28:
../inst/include/inttypes.h:34:13: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#   include <inttypes.h>
            ^~~~~~~~~~~~
            <IntTypes.h>
In file included from Options.cc:20:
In file included from ../inst/include/Sort.h:24:
In file included from ../inst/include/Vec.h:28:
../inst/include/inttypes.h:34:13: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#   include <inttypes.h>
            ^~~~~~~~~~~~
            <IntTypes.h>
In file included from Options.cc:21:
../inst/include/Options.h:147:13: warning: variable 'end' is uninitialized when used here [-Wuninitialized]
        if (end == NULL) 
            ^~~
../inst/include/Options.h:143:19: note: initialize the variable 'end' to silence this warning
        char*  end;
                  ^
                   = nullptr
../inst/include/Options.h:207:13: warning: variable 'end' is uninitialized when used here [-Wuninitialized]
        if (end == NULL) 
            ^~~
../inst/include/Options.h:204:20: note: initialize the variable 'end' to silence this warning
        char*   end;
                   ^
                    = nullptr
../inst/include/Options.h:275:13: warning: variable 'end' is uninitialized when used here [-Wuninitialized]
        if (end == NULL) 
            ^~~
../inst/include/Options.h:271:20: note: initialize the variable 'end' to silence this warning
        char*   end;
                   ^
                    = nullptr
In file included from kprmatcher.cc:41:
In file included from ../inst/include/kprmatcher.h:36:
In file included from ../inst/include/damatcher.h:36:
In file included from ../inst/include/problem.h:43:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:10: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#include <inttypes.h>                   // needed with g++-4.7 to declare intptr_t
         ^~~~~~~~~~~~
         <IntTypes.h>
In file included from kprmatcher.cc:41:
In file included from ../inst/include/kprmatcher.h:36:
In file included from ../inst/include/damatcher.h:36:
In file included from ../inst/include/problem.h:43:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:
../inst/include/inttypes.h:34:13: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#   include <inttypes.h>
            ^~~~~~~~~~~~
            <IntTypes.h>
In file included from Options.cc:24:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:10: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#include <inttypes.h>                   // needed with g++-4.7 to declare intptr_t
         ^~~~~~~~~~~~
         <IntTypes.h>
In file included from System.cc:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:10: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#include <inttypes.h>                   // needed with g++-4.7 to declare intptr_t
         ^~~~~~~~~~~~
         <IntTypes.h>
2 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppProgress/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c params.cc -o params.o
5 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppProgress/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c problem.cc -o problem.o
In file included from params.cc:32:
In file included from ../inst/include/Options.h:28:
../inst/include/inttypes.h:34:13: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#   include <inttypes.h>
            ^~~~~~~~~~~~
            <IntTypes.h>
In file included from params.cc:32:
../inst/include/Options.h:147:13: warning: variable 'end' is uninitialized when used here [-Wuninitialized]
        if (end == NULL) 
            ^~~
../inst/include/Options.h:143:19: note: initialize the variable 'end' to silence this warning
        char*  end;
                  ^
                   = nullptr
../inst/include/Options.h:207:13: warning: variable 'end' is uninitialized when used here [-Wuninitialized]
        if (end == NULL) 
            ^~~
../inst/include/Options.h:204:20: note: initialize the variable 'end' to silence this warning
        char*   end;
                   ^
                    = nullptr
../inst/include/Options.h:275:13: warning: variable 'end' is uninitialized when used here [-Wuninitialized]
        if (end == NULL) 
            ^~~
../inst/include/Options.h:271:20: note: initialize the variable 'end' to silence this warning
        char*   end;
                   ^
                    = nullptr
In file included from RcppExports.cpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include/RcppArmadillo.h:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:10: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#include <inttypes.h>                   // needed with g++-4.7 to declare intptr_t
         ^~~~~~~~~~~~
         <IntTypes.h>
In file included from RcppExports.cpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include/RcppArmadillo.h:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:
../inst/include/inttypes.h:34:13: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#   include <inttypes.h>
            ^~~~~~~~~~~~
            <IntTypes.h>
2 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppProgress/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c rcp.cc -o rcp.o
In file included from params.cc:36:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:10: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#include <inttypes.h>                   // needed with g++-4.7 to declare intptr_t
         ^~~~~~~~~~~~
         <IntTypes.h>
In file included from problem.cc:36:
In file included from ../inst/include/problem.h:43:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:10: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#include <inttypes.h>                   // needed with g++-4.7 to declare intptr_t
         ^~~~~~~~~~~~
         <IntTypes.h>
In file included from problem.cc:36:
In file included from ../inst/include/problem.h:43:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:
../inst/include/inttypes.h:34:13: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#   include <inttypes.h>
            ^~~~~~~~~~~~
            <IntTypes.h>
5 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppProgress/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c rpmatcher.cc -o rpmatcher.o
In file included from rcp.cc:33:
In file included from ../inst/include/rcp.h:41:
In file included from ../inst/include/problem.h:43:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:10: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#include <inttypes.h>                   // needed with g++-4.7 to declare intptr_t
         ^~~~~~~~~~~~
         <IntTypes.h>
In file included from rcp.cc:33:
In file included from ../inst/include/rcp.h:41:
In file included from ../inst/include/problem.h:43:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:
../inst/include/inttypes.h:34:13: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#   include <inttypes.h>
            ^~~~~~~~~~~~
            <IntTypes.h>
2 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppProgress/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c runMatch.cpp -o runMatch.o
In file included from rpmatcher.cc:46:
In file included from ../inst/include/rpmatcher.h:36:
In file included from ../inst/include/damatcher.h:36:
In file included from ../inst/include/problem.h:43:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:10: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#include <inttypes.h>                   // needed with g++-4.7 to declare intptr_t
         ^~~~~~~~~~~~
         <IntTypes.h>
In file included from rpmatcher.cc:46:
In file included from ../inst/include/rpmatcher.h:36:
In file included from ../inst/include/damatcher.h:36:
In file included from ../inst/include/problem.h:43:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:
../inst/include/inttypes.h:34:13: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#   include <inttypes.h>
            ^~~~~~~~~~~~
            <IntTypes.h>
2 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppProgress/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c stabit2Mat0.cpp -o stabit2Mat0.o
In file included from runMatch.cpp:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:10: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#include <inttypes.h>                   // needed with g++-4.7 to declare intptr_t
         ^~~~~~~~~~~~
         <IntTypes.h>
In file included from runMatch.cpp:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:
../inst/include/inttypes.h:34:13: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#   include <inttypes.h>
            ^~~~~~~~~~~~
            <IntTypes.h>
2 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppProgress/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c stabit2Mat1.cpp -o stabit2Mat1.o
2 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppProgress/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c stabit2Sel0.cpp -o stabit2Sel0.o
2 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppProgress/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c stabit2Sel1.cpp -o stabit2Sel1.o
In file included from stabit2Mat0.cpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include/RcppArmadillo.h:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:10: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#include <inttypes.h>                   // needed with g++-4.7 to declare intptr_t
         ^~~~~~~~~~~~
         <IntTypes.h>
In file included from stabit2Mat0.cpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include/RcppArmadillo.h:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:
../inst/include/inttypes.h:34:13: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#   include <inttypes.h>
            ^~~~~~~~~~~~
            <IntTypes.h>
In file included from stabit2Mat1.cpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include/RcppArmadillo.h:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:10: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#include <inttypes.h>                   // needed with g++-4.7 to declare intptr_t
         ^~~~~~~~~~~~
         <IntTypes.h>
In file included from stabit2Mat1.cpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include/RcppArmadillo.h:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:
../inst/include/inttypes.h:34:13: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#   include <inttypes.h>
            ^~~~~~~~~~~~
            <IntTypes.h>
In file included from stabit2Sel0.cpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include/RcppArmadillo.h:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:10: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#include <inttypes.h>                   // needed with g++-4.7 to declare intptr_t
         ^~~~~~~~~~~~
         <IntTypes.h>
In file included from stabit2Sel0.cpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include/RcppArmadillo.h:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:
../inst/include/inttypes.h:34:13: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#   include <inttypes.h>
            ^~~~~~~~~~~~
            <IntTypes.h>
In file included from stabit2Sel1.cpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include/RcppArmadillo.h:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:10: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#include <inttypes.h>                   // needed with g++-4.7 to declare intptr_t
         ^~~~~~~~~~~~
         <IntTypes.h>
In file included from stabit2Sel1.cpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include/RcppArmadillo.h:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:
../inst/include/inttypes.h:34:13: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#   include <inttypes.h>
            ^~~~~~~~~~~~
            <IntTypes.h>
2 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppProgress/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c stabit2Sel2.cpp -o stabit2Sel2.o
2 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppProgress/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c stabitSel2.cpp -o stabitSel2.o
In file included from stabit2Sel2.cpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include/RcppArmadillo.h:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:10: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#include <inttypes.h>                   // needed with g++-4.7 to declare intptr_t
         ^~~~~~~~~~~~
         <IntTypes.h>
In file included from stabit2Sel2.cpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include/RcppArmadillo.h:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:
../inst/include/inttypes.h:34:13: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#   include <inttypes.h>
            ^~~~~~~~~~~~
            <IntTypes.h>
In file included from stabitSel2.cpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include/RcppArmadillo.h:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:10: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#include <inttypes.h>                   // needed with g++-4.7 to declare intptr_t
         ^~~~~~~~~~~~
         <IntTypes.h>
In file included from stabitSel2.cpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include/RcppArmadillo.h:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:
../inst/include/inttypes.h:34:13: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#   include <inttypes.h>
            ^~~~~~~~~~~~
            <IntTypes.h>
2 warnings generated.
2 warnings generated.
2 warnings generated.
2 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/clang8/lib -o matchingMarkets.so Options.o RcppExports.o System.o kprmatcher.o params.o problem.o rcp.o rpmatcher.o runMatch.o stabit2Mat0.o stabit2Mat1.o stabit2Sel0.o stabit2Sel1.o stabit2Sel2.o stabitSel2.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm -fopenmp -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/max/github/forks/ggplot2/revdep/checks.noindex/matchingMarkets/new/matchingMarkets.Rcheck/00LOCK-matchingMarkets/00new/matchingMarkets/libs
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘matchingMarkets’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/matchingMarkets/new/matchingMarkets.Rcheck/matchingMarkets’

```
### CRAN

```
* installing *source* package ‘matchingMarkets’ ...
** package ‘matchingMarkets’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppProgress/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c Options.cc -o Options.o
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppProgress/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppProgress/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c System.cc -o System.o
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppProgress/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c kprmatcher.cc -o kprmatcher.o
In file included from System.cc:24:
In file included from ../inst/include/System.h:28:
../inst/include/inttypes.h:34:13: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#   include <inttypes.h>
            ^~~~~~~~~~~~
            <IntTypes.h>
In file included from Options.cc:20:
In file included from ../inst/include/Sort.h:24:
In file included from ../inst/include/Vec.h:28:
../inst/include/inttypes.h:34:13: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#   include <inttypes.h>
            ^~~~~~~~~~~~
            <IntTypes.h>
In file included from Options.cc:21:
../inst/include/Options.h:147:13: warning: variable 'end' is uninitialized when used here [-Wuninitialized]
        if (end == NULL) 
            ^~~
../inst/include/Options.h:143:19: note: initialize the variable 'end' to silence this warning
        char*  end;
                  ^
                   = nullptr
../inst/include/Options.h:207:13: warning: variable 'end' is uninitialized when used here [-Wuninitialized]
        if (end == NULL) 
            ^~~
../inst/include/Options.h:204:20: note: initialize the variable 'end' to silence this warning
        char*   end;
                   ^
                    = nullptr
../inst/include/Options.h:275:13: warning: variable 'end' is uninitialized when used here [-Wuninitialized]
        if (end == NULL) 
            ^~~
../inst/include/Options.h:271:20: note: initialize the variable 'end' to silence this warning
        char*   end;
                   ^
                    = nullptr
In file included from kprmatcher.cc:41:
In file included from ../inst/include/kprmatcher.h:36:
In file included from ../inst/include/damatcher.h:36:
In file included from ../inst/include/problem.h:43:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:10: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#include <inttypes.h>                   // needed with g++-4.7 to declare intptr_t
         ^~~~~~~~~~~~
         <IntTypes.h>
In file included from kprmatcher.cc:41:
In file included from ../inst/include/kprmatcher.h:36:
In file included from ../inst/include/damatcher.h:36:
In file included from ../inst/include/problem.h:43:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:
../inst/include/inttypes.h:34:13: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#   include <inttypes.h>
            ^~~~~~~~~~~~
            <IntTypes.h>
In file included from System.cc:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:10: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#include <inttypes.h>                   // needed with g++-4.7 to declare intptr_t
         ^~~~~~~~~~~~
         <IntTypes.h>
In file included from Options.cc:24:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:10: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#include <inttypes.h>                   // needed with g++-4.7 to declare intptr_t
         ^~~~~~~~~~~~
         <IntTypes.h>
2 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppProgress/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c params.cc -o params.o
5 warnings generated.
In file included from params.cc:32:
In file included from ../inst/include/Options.h:28:
../inst/include/inttypes.h:34:13: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#   include <inttypes.h>
            ^~~~~~~~~~~~
            <IntTypes.h>
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppProgress/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c problem.cc -o problem.o
In file included from params.cc:32:
../inst/include/Options.h:147:13: warning: variable 'end' is uninitialized when used here [-Wuninitialized]
        if (end == NULL) 
            ^~~
../inst/include/Options.h:143:19: note: initialize the variable 'end' to silence this warning
        char*  end;
                  ^
                   = nullptr
../inst/include/Options.h:207:13: warning: variable 'end' is uninitialized when used here [-Wuninitialized]
        if (end == NULL) 
            ^~~
../inst/include/Options.h:204:20: note: initialize the variable 'end' to silence this warning
        char*   end;
                   ^
                    = nullptr
../inst/include/Options.h:275:13: warning: variable 'end' is uninitialized when used here [-Wuninitialized]
        if (end == NULL) 
            ^~~
../inst/include/Options.h:271:20: note: initialize the variable 'end' to silence this warning
        char*   end;
                   ^
                    = nullptr
2 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppProgress/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c rcp.cc -o rcp.o
In file included from RcppExports.cpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include/RcppArmadillo.h:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:10: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#include <inttypes.h>                   // needed with g++-4.7 to declare intptr_t
         ^~~~~~~~~~~~
         <IntTypes.h>
In file included from RcppExports.cpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include/RcppArmadillo.h:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:
../inst/include/inttypes.h:34:13: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#   include <inttypes.h>
            ^~~~~~~~~~~~
            <IntTypes.h>
In file included from params.cc:36:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:10: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#include <inttypes.h>                   // needed with g++-4.7 to declare intptr_t
         ^~~~~~~~~~~~
         <IntTypes.h>
In file included from problem.cc:36:
In file included from ../inst/include/problem.h:43:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:10: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#include <inttypes.h>                   // needed with g++-4.7 to declare intptr_t
         ^~~~~~~~~~~~
         <IntTypes.h>
In file included from problem.cc:36:
In file included from ../inst/include/problem.h:43:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:
../inst/include/inttypes.h:34:13: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#   include <inttypes.h>
            ^~~~~~~~~~~~
            <IntTypes.h>
5 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppProgress/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c rpmatcher.cc -o rpmatcher.o
In file included from rcp.cc:33:
In file included from ../inst/include/rcp.h:41:
In file included from ../inst/include/problem.h:43:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:10: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#include <inttypes.h>                   // needed with g++-4.7 to declare intptr_t
         ^~~~~~~~~~~~
         <IntTypes.h>
In file included from rcp.cc:33:
In file included from ../inst/include/rcp.h:41:
In file included from ../inst/include/problem.h:43:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:
../inst/include/inttypes.h:34:13: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#   include <inttypes.h>
            ^~~~~~~~~~~~
            <IntTypes.h>
2 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppProgress/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c runMatch.cpp -o runMatch.o
2 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppProgress/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c stabit2Mat0.cpp -o stabit2Mat0.o
In file included from rpmatcher.cc:46:
In file included from ../inst/include/rpmatcher.h:36:
In file included from ../inst/include/damatcher.h:36:
In file included from ../inst/include/problem.h:43:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:10: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#include <inttypes.h>                   // needed with g++-4.7 to declare intptr_t
         ^~~~~~~~~~~~
         <IntTypes.h>
In file included from rpmatcher.cc:46:
In file included from ../inst/include/rpmatcher.h:36:
In file included from ../inst/include/damatcher.h:36:
In file included from ../inst/include/problem.h:43:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:
../inst/include/inttypes.h:34:13: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#   include <inttypes.h>
            ^~~~~~~~~~~~
            <IntTypes.h>
In file included from runMatch.cpp:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:10: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#include <inttypes.h>                   // needed with g++-4.7 to declare intptr_t
         ^~~~~~~~~~~~
         <IntTypes.h>
In file included from runMatch.cpp:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:
../inst/include/inttypes.h:34:13: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#   include <inttypes.h>
            ^~~~~~~~~~~~
            <IntTypes.h>
2 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppProgress/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c stabit2Mat1.cpp -o stabit2Mat1.o
2 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppProgress/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c stabit2Sel0.cpp -o stabit2Sel0.o
2 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppProgress/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c stabit2Sel1.cpp -o stabit2Sel1.o
In file included from stabit2Mat0.cpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include/RcppArmadillo.h:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:10: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#include <inttypes.h>                   // needed with g++-4.7 to declare intptr_t
         ^~~~~~~~~~~~
         <IntTypes.h>
In file included from stabit2Mat0.cpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include/RcppArmadillo.h:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:
../inst/include/inttypes.h:34:13: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#   include <inttypes.h>
            ^~~~~~~~~~~~
            <IntTypes.h>
In file included from stabit2Mat1.cpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include/RcppArmadillo.h:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:10: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#include <inttypes.h>                   // needed with g++-4.7 to declare intptr_t
         ^~~~~~~~~~~~
         <IntTypes.h>
In file included from stabit2Mat1.cpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include/RcppArmadillo.h:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:
../inst/include/inttypes.h:34:13: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#   include <inttypes.h>
            ^~~~~~~~~~~~
            <IntTypes.h>
In file included from stabit2Sel0.cpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include/RcppArmadillo.h:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:10: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#include <inttypes.h>                   // needed with g++-4.7 to declare intptr_t
         ^~~~~~~~~~~~
         <IntTypes.h>
In file included from stabit2Sel0.cpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include/RcppArmadillo.h:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:
../inst/include/inttypes.h:34:13: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#   include <inttypes.h>
            ^~~~~~~~~~~~
            <IntTypes.h>
In file included from stabit2Sel1.cpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include/RcppArmadillo.h:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:10: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#include <inttypes.h>                   // needed with g++-4.7 to declare intptr_t
         ^~~~~~~~~~~~
         <IntTypes.h>
In file included from stabit2Sel1.cpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include/RcppArmadillo.h:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:
../inst/include/inttypes.h:34:13: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#   include <inttypes.h>
            ^~~~~~~~~~~~
            <IntTypes.h>
2 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppProgress/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c stabit2Sel2.cpp -o stabit2Sel2.o
2 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include/ -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppProgress/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c stabitSel2.cpp -o stabitSel2.o
In file included from stabit2Sel2.cpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include/RcppArmadillo.h:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:10: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#include <inttypes.h>                   // needed with g++-4.7 to declare intptr_t
         ^~~~~~~~~~~~
         <IntTypes.h>
In file included from stabit2Sel2.cpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include/RcppArmadillo.h:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:
../inst/include/inttypes.h:34:13: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#   include <inttypes.h>
            ^~~~~~~~~~~~
            <IntTypes.h>
In file included from stabitSel2.cpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include/RcppArmadillo.h:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:10: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#include <inttypes.h>                   // needed with g++-4.7 to declare intptr_t
         ^~~~~~~~~~~~
         <IntTypes.h>
In file included from stabitSel2.cpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/RcppArmadillo/include/RcppArmadillo.h:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp.h:76:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/sugar/sugar.h:28:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/Rcpp/include/Rcpp/hash/hash.h:25:
../inst/include/inttypes.h:34:13: warning: non-portable path to file '<IntTypes.h>'; specified path differs in case from file name on disk [-Wnonportable-include-path]
#   include <inttypes.h>
            ^~~~~~~~~~~~
            <IntTypes.h>
2 warnings generated.
2 warnings generated.
2 warnings generated.
2 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/clang8/lib -o matchingMarkets.so Options.o RcppExports.o System.o kprmatcher.o params.o problem.o rcp.o rpmatcher.o runMatch.o stabit2Mat0.o stabit2Mat1.o stabit2Sel0.o stabit2Sel1.o stabit2Sel2.o stabitSel2.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm -fopenmp -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/max/github/forks/ggplot2/revdep/checks.noindex/matchingMarkets/old/matchingMarkets.Rcheck/00LOCK-matchingMarkets/00new/matchingMarkets/libs
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/matchingMarkets/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘matchingMarkets’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/matchingMarkets/old/matchingMarkets.Rcheck/matchingMarkets’

```
# mbgraphic

<details>

* Version: 1.0.1
* Source code: https://github.com/cran/mbgraphic
* Date/Publication: 2019-04-28 19:20:03 UTC
* Number of recursive dependencies: 91

Run `revdep_details(,"mbgraphic")` for more info

</details>

## In both

*   checking whether package ‘mbgraphic’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/mbgraphic/new/mbgraphic.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘mbgraphic’ ...
** package ‘mbgraphic’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mbgraphic/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mbgraphic/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c cmasum.cpp -o cmasum.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mbgraphic/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c mbgraphic_init.c -o mbgraphic_init.o
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mbgraphic/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c variableflip.cpp -o variableflip.o
/usr/local/clang8/bin/clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/clang8/lib -o mbgraphic.so RcppExports.o cmasum.o mbgraphic_init.o variableflip.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/max/github/forks/ggplot2/revdep/checks.noindex/mbgraphic/new/mbgraphic.Rcheck/00LOCK-mbgraphic/00new/mbgraphic/libs
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/mbgraphic/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/mbgraphic/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/mbgraphic/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘mbgraphic’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/mbgraphic/new/mbgraphic.Rcheck/mbgraphic’

```
### CRAN

```
* installing *source* package ‘mbgraphic’ ...
** package ‘mbgraphic’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mbgraphic/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mbgraphic/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c cmasum.cpp -o cmasum.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mbgraphic/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c mbgraphic_init.c -o mbgraphic_init.o
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mbgraphic/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c variableflip.cpp -o variableflip.o
/usr/local/clang8/bin/clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/clang8/lib -o mbgraphic.so RcppExports.o cmasum.o mbgraphic_init.o variableflip.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/max/github/forks/ggplot2/revdep/checks.noindex/mbgraphic/old/mbgraphic.Rcheck/00LOCK-mbgraphic/00new/mbgraphic/libs
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/mbgraphic/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/mbgraphic/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/mbgraphic/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘mbgraphic’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/mbgraphic/old/mbgraphic.Rcheck/mbgraphic’

```
# mcmcabn

<details>

* Version: 0.2
* Source code: https://github.com/cran/mcmcabn
* URL: https://www.math.uzh.ch/pages/mcmcabn/
* BugReports: https://git.math.uzh.ch/gkratz/mcmcabn/issues
* Date/Publication: 2019-07-01 19:00:03 UTC
* Number of recursive dependencies: 103

Run `revdep_details(,"mcmcabn")` for more info

</details>

## In both

*   checking whether package ‘mcmcabn’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/mcmcabn/new/mcmcabn.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘mcmcabn’ ...
** package ‘mcmcabn’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/mcmcabn/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/mcmcabn/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/mcmcabn/rjags/libs/rjags.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘mcmcabn’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/mcmcabn/new/mcmcabn.Rcheck/mcmcabn’

```
### CRAN

```
* installing *source* package ‘mcmcabn’ ...
** package ‘mcmcabn’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/mcmcabn/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/mcmcabn/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/mcmcabn/rjags/libs/rjags.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘mcmcabn’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/mcmcabn/old/mcmcabn.Rcheck/mcmcabn’

```
# MEAL

<details>

* Version: 1.14.0
* Source code: https://github.com/cran/MEAL
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 208

Run `revdep_details(,"MEAL")` for more info

</details>

## In both

*   checking whether package ‘MEAL’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/MEAL/new/MEAL.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘minfiData’
    ```

## Installation

### Devel

```
* installing *source* package ‘MEAL’ ...
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called 'IlluminaHumanMethylation450kmanifest'
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘MEAL’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/MEAL/new/MEAL.Rcheck/MEAL’

```
### CRAN

```
* installing *source* package ‘MEAL’ ...
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called 'IlluminaHumanMethylation450kmanifest'
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘MEAL’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/MEAL/old/MEAL.Rcheck/MEAL’

```
# MergeGUI

<details>

* Version: 0.2-1
* Source code: https://github.com/cran/MergeGUI
* Date/Publication: 2014-01-27 22:44:16
* Number of recursive dependencies: 42

Run `revdep_details(,"MergeGUI")` for more info

</details>

## In both

*   checking whether package ‘MergeGUI’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/MergeGUI/new/MergeGUI.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘MergeGUI’ ...
** package ‘MergeGUI’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** demo
** inst
** byte-compile and prepare package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/MergeGUI/RGtk2/libs/RGtk2.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/MergeGUI/RGtk2/libs/RGtk2.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/MergeGUI/RGtk2/libs/RGtk2.so
  Reason: image not found
Please install GTK+ from http://r.research.att.com/libs/GTK_2.24.17-X11.pkg
If the package still does not load, please ensure that GTK+ is installed and that it is on your PATH environment variable
IN ANY CASE, RESTART R BEFORE TRYING TO LOAD THE PACKAGE AGAIN
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/MergeGUI/cairoDevice/libs/cairoDevice.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/MergeGUI/cairoDevice/libs/cairoDevice.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/MergeGUI/cairoDevice/libs/cairoDevice.so
  Reason: image not found
In addition: Warning message:
Failed to load RGtk2 dynamic library, attempting to install it. 
Please install GTK+ from http://r.research.att.com/libs/GTK_2.18.5-X11.pkg
If the package still does not load, please ensure that GTK+ is installed and that it is on your PATH environment variable
IN ANY CASE, RESTART R BEFORE TRYING TO LOAD THE PACKAGE AGAIN
Error: package or namespace load failed for ‘gWidgetsRGtk2’:
 .onAttach failed in attachNamespace() for 'gWidgetsRGtk2', details:
  call: .Call(name, ..., PACKAGE = PACKAGE)
  error: "S_gtk_icon_factory_new" not available for .Call() for package "RGtk2"
Error: package ‘gWidgetsRGtk2’ could not be loaded
In addition: Warning message:
In fun(libname, pkgname) :
  Failed to load cairoDevice, attempting to install itError in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/MergeGUI/cairoDevice/libs/cairoDevice.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/MergeGUI/cairoDevice/libs/cairoDevice.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/MergeGUI/cairoDevice/libs/cairoDevice.so
  Reason: image not found

Execution halted
ERROR: lazy loading failed for package ‘MergeGUI’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/MergeGUI/new/MergeGUI.Rcheck/MergeGUI’

```
### CRAN

```
* installing *source* package ‘MergeGUI’ ...
** package ‘MergeGUI’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** demo
** inst
** byte-compile and prepare package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/MergeGUI/RGtk2/libs/RGtk2.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/MergeGUI/RGtk2/libs/RGtk2.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/MergeGUI/RGtk2/libs/RGtk2.so
  Reason: image not found
Please install GTK+ from http://r.research.att.com/libs/GTK_2.24.17-X11.pkg
If the package still does not load, please ensure that GTK+ is installed and that it is on your PATH environment variable
IN ANY CASE, RESTART R BEFORE TRYING TO LOAD THE PACKAGE AGAIN
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/MergeGUI/cairoDevice/libs/cairoDevice.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/MergeGUI/cairoDevice/libs/cairoDevice.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/MergeGUI/cairoDevice/libs/cairoDevice.so
  Reason: image not found
In addition: Warning message:
Failed to load RGtk2 dynamic library, attempting to install it. 
Please install GTK+ from http://r.research.att.com/libs/GTK_2.18.5-X11.pkg
If the package still does not load, please ensure that GTK+ is installed and that it is on your PATH environment variable
IN ANY CASE, RESTART R BEFORE TRYING TO LOAD THE PACKAGE AGAIN
Error: package or namespace load failed for ‘gWidgetsRGtk2’:
 .onAttach failed in attachNamespace() for 'gWidgetsRGtk2', details:
  call: .Call(name, ..., PACKAGE = PACKAGE)
  error: "S_gtk_icon_factory_new" not available for .Call() for package "RGtk2"
Error: package ‘gWidgetsRGtk2’ could not be loaded
In addition: Warning message:
In fun(libname, pkgname) :
  Failed to load cairoDevice, attempting to install itError in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/MergeGUI/cairoDevice/libs/cairoDevice.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/MergeGUI/cairoDevice/libs/cairoDevice.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/MergeGUI/cairoDevice/libs/cairoDevice.so
  Reason: image not found

Execution halted
ERROR: lazy loading failed for package ‘MergeGUI’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/MergeGUI/old/MergeGUI.Rcheck/MergeGUI’

```
# metaMix

<details>

* Version: 0.3
* Source code: https://github.com/cran/metaMix
* Date/Publication: 2019-02-11 16:20:03 UTC
* Number of recursive dependencies: 47

Run `revdep_details(,"metaMix")` for more info

</details>

## In both

*   checking whether package ‘metaMix’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/metaMix/new/metaMix.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘metaMix’ ...
** package ‘metaMix’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c fast_multinom_weight.cpp -o fast_multinom_weight.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c init.c -o init.o
/usr/local/clang8/bin/clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/clang8/lib -o metaMix.so fast_multinom_weight.o init.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/max/github/forks/ggplot2/revdep/checks.noindex/metaMix/new/metaMix.Rcheck/00LOCK-metaMix/00new/metaMix/libs
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'Rmpi', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/metaMix/Rmpi/libs/Rmpi.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/metaMix/Rmpi/libs/Rmpi.so, 6): Library not loaded: /usr/local/opt/openssl/lib/libcrypto.1.0.0.dylib
  Referenced from: /usr/local/opt/libevent/lib/libevent-2.1.6.dylib
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘metaMix’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/metaMix/new/metaMix.Rcheck/metaMix’

```
### CRAN

```
* installing *source* package ‘metaMix’ ...
** package ‘metaMix’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c fast_multinom_weight.cpp -o fast_multinom_weight.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c init.c -o init.o
/usr/local/clang8/bin/clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/clang8/lib -o metaMix.so fast_multinom_weight.o init.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/max/github/forks/ggplot2/revdep/checks.noindex/metaMix/old/metaMix.Rcheck/00LOCK-metaMix/00new/metaMix/libs
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'Rmpi', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/metaMix/Rmpi/libs/Rmpi.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/metaMix/Rmpi/libs/Rmpi.so, 6): Library not loaded: /usr/local/opt/openssl/lib/libcrypto.1.0.0.dylib
  Referenced from: /usr/local/opt/libevent/lib/libevent-2.1.6.dylib
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘metaMix’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/metaMix/old/metaMix.Rcheck/metaMix’

```
# methylGSA

<details>

* Version: 
* Source code: ???
* URL: http://ggplot2.tidyverse.org, https://github.com/tidyverse/ggplot2
* BugReports: https://github.com/tidyverse/ggplot2/issues
* Number of recursive dependencies: 0

Run `revdep_details(,"")` for more info

</details>

## Error before installation

### Devel

```

  There is a binary version available but the source version is later:
      binary  source needs_compilation
nlme 3.1-143 3.1-144              TRUE

  Binaries will be installed
  These will not be installed


Warning: unable to access index for repository https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6:
  cannot open URL 'https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6/PACKAGES'
Warning: unable to access index for repository https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6:
  cannot open URL 'https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6/PACKAGES'
Packages which are only available in source form, and may need
  compilation of C/C++/Fortran: ‘FDb.InfiniumMethylation.hg19’
  ‘IlluminaHumanMethylation450kmanifest’
installing the source package ‘reactome.db’



```
### CRAN

```

  There is a binary version available but the source version is later:
      binary  source needs_compilation
nlme 3.1-143 3.1-144              TRUE

  Binaries will be installed
  These will not be installed


Warning: unable to access index for repository https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6:
  cannot open URL 'https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6/PACKAGES'
Warning: unable to access index for repository https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6:
  cannot open URL 'https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6/PACKAGES'
Packages which are only available in source form, and may need
  compilation of C/C++/Fortran: ‘FDb.InfiniumMethylation.hg19’
  ‘IlluminaHumanMethylation450kmanifest’
installing the source package ‘reactome.db’



```
# mfbvar

<details>

* Version: 0.5.2
* Source code: https://github.com/cran/mfbvar
* URL: https://github.com/ankargren/mfbvar
* BugReports: https://github.com/ankargren/mfbvar/issues
* Date/Publication: 2020-01-09 13:40:02 UTC
* Number of recursive dependencies: 81

Run `revdep_details(,"mfbvar")` for more info

</details>

## In both

*   checking whether package ‘mfbvar’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/mfbvar/new/mfbvar.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘mfbvar’ ...
** package ‘mfbvar’ successfully unpacked and MD5 sums checked
** using staged installation
checking whether the C++ compiler works... yes
checking for C++ compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... configure: error: in `/Users/max/github/forks/ggplot2/revdep/checks.noindex/mfbvar/new/mfbvar.Rcheck/00_pkg_src/mfbvar':
configure: error: cannot run C++ compiled programs.
If you meant to cross compile, use `--host'.
See `config.log' for more details
ERROR: configuration failed for package ‘mfbvar’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/mfbvar/new/mfbvar.Rcheck/mfbvar’

```
### CRAN

```
* installing *source* package ‘mfbvar’ ...
** package ‘mfbvar’ successfully unpacked and MD5 sums checked
** using staged installation
checking whether the C++ compiler works... yes
checking for C++ compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... configure: error: in `/Users/max/github/forks/ggplot2/revdep/checks.noindex/mfbvar/old/mfbvar.Rcheck/00_pkg_src/mfbvar':
configure: error: cannot run C++ compiled programs.
If you meant to cross compile, use `--host'.
See `config.log' for more details
ERROR: configuration failed for package ‘mfbvar’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/mfbvar/old/mfbvar.Rcheck/mfbvar’

```
# MissingDataGUI

<details>

* Version: 0.2-5
* Source code: https://github.com/cran/MissingDataGUI
* Date/Publication: 2016-04-25 08:58:53
* Number of recursive dependencies: 104

Run `revdep_details(,"MissingDataGUI")` for more info

</details>

## In both

*   checking whether package ‘MissingDataGUI’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/MissingDataGUI/new/MissingDataGUI.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘MissingDataGUI’ ...
** package ‘MissingDataGUI’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/MissingDataGUI/RGtk2/libs/RGtk2.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/MissingDataGUI/RGtk2/libs/RGtk2.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/MissingDataGUI/RGtk2/libs/RGtk2.so
  Reason: image not found
Please install GTK+ from http://r.research.att.com/libs/GTK_2.24.17-X11.pkg
If the package still does not load, please ensure that GTK+ is installed and that it is on your PATH environment variable
IN ANY CASE, RESTART R BEFORE TRYING TO LOAD THE PACKAGE AGAIN
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/MissingDataGUI/cairoDevice/libs/cairoDevice.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/MissingDataGUI/cairoDevice/libs/cairoDevice.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/MissingDataGUI/cairoDevice/libs/cairoDevice.so
  Reason: image not found
In addition: Warning message:
Failed to load RGtk2 dynamic library, attempting to install it. 
Please install GTK+ from http://r.research.att.com/libs/GTK_2.18.5-X11.pkg
If the package still does not load, please ensure that GTK+ is installed and that it is on your PATH environment variable
IN ANY CASE, RESTART R BEFORE TRYING TO LOAD THE PACKAGE AGAIN
Error: package or namespace load failed for ‘gWidgetsRGtk2’:
 .onAttach failed in attachNamespace() for 'gWidgetsRGtk2', details:
  call: .Call(name, ..., PACKAGE = PACKAGE)
  error: "S_gtk_icon_factory_new" not available for .Call() for package "RGtk2"
Error: package ‘gWidgetsRGtk2’ could not be loaded
In addition: Warning message:
In fun(libname, pkgname) :
  Failed to load cairoDevice, attempting to install itError in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/MissingDataGUI/cairoDevice/libs/cairoDevice.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/MissingDataGUI/cairoDevice/libs/cairoDevice.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/MissingDataGUI/cairoDevice/libs/cairoDevice.so
  Reason: image not found

Execution halted
ERROR: lazy loading failed for package ‘MissingDataGUI’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/MissingDataGUI/new/MissingDataGUI.Rcheck/MissingDataGUI’

```
### CRAN

```
* installing *source* package ‘MissingDataGUI’ ...
** package ‘MissingDataGUI’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/MissingDataGUI/RGtk2/libs/RGtk2.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/MissingDataGUI/RGtk2/libs/RGtk2.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/MissingDataGUI/RGtk2/libs/RGtk2.so
  Reason: image not found
Please install GTK+ from http://r.research.att.com/libs/GTK_2.24.17-X11.pkg
If the package still does not load, please ensure that GTK+ is installed and that it is on your PATH environment variable
IN ANY CASE, RESTART R BEFORE TRYING TO LOAD THE PACKAGE AGAIN
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/MissingDataGUI/cairoDevice/libs/cairoDevice.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/MissingDataGUI/cairoDevice/libs/cairoDevice.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/MissingDataGUI/cairoDevice/libs/cairoDevice.so
  Reason: image not found
In addition: Warning message:
Failed to load RGtk2 dynamic library, attempting to install it. 
Please install GTK+ from http://r.research.att.com/libs/GTK_2.18.5-X11.pkg
If the package still does not load, please ensure that GTK+ is installed and that it is on your PATH environment variable
IN ANY CASE, RESTART R BEFORE TRYING TO LOAD THE PACKAGE AGAIN
Error: package or namespace load failed for ‘gWidgetsRGtk2’:
 .onAttach failed in attachNamespace() for 'gWidgetsRGtk2', details:
  call: .Call(name, ..., PACKAGE = PACKAGE)
  error: "S_gtk_icon_factory_new" not available for .Call() for package "RGtk2"
Error: package ‘gWidgetsRGtk2’ could not be loaded
In addition: Warning message:
In fun(libname, pkgname) :
  Failed to load cairoDevice, attempting to install itError in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/MissingDataGUI/cairoDevice/libs/cairoDevice.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/MissingDataGUI/cairoDevice/libs/cairoDevice.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/MissingDataGUI/cairoDevice/libs/cairoDevice.so
  Reason: image not found

Execution halted
ERROR: lazy loading failed for package ‘MissingDataGUI’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/MissingDataGUI/old/MissingDataGUI.Rcheck/MissingDataGUI’

```
# mleap

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/mleap
* URL: https://github.com/rstudio/mleap
* BugReports: https://github.com/rstudio/mleap/issues
* Date/Publication: 2020-01-10 22:20:02 UTC
* Number of recursive dependencies: 80

Run `revdep_details(,"mleap")` for more info

</details>

## In both

*   checking whether package ‘mleap’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/mleap/new/mleap.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘mleap’ ...
** package ‘mleap’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
Error: package or namespace load failed for ‘mleap’:
 .onLoad failed in loadNamespace() for 'mleap', details:
  call: NULL
  error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/mleap/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/mleap/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/mleap/rJava/libs/rJava.so
  Reason: image not found
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/mleap/new/mleap.Rcheck/mleap’

```
### CRAN

```
* installing *source* package ‘mleap’ ...
** package ‘mleap’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
Error: package or namespace load failed for ‘mleap’:
 .onLoad failed in loadNamespace() for 'mleap', details:
  call: NULL
  error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/mleap/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/mleap/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/mleap/rJava/libs/rJava.so
  Reason: image not found
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/mleap/old/mleap.Rcheck/mleap’

```
# mlm4omics

<details>

* Version: 1.2.0
* Source code: https://github.com/cran/mlm4omics
* URL: https://doi.org/10.1101/153049
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 79

Run `revdep_details(,"mlm4omics")` for more info

</details>

## In both

*   checking whether package ‘mlm4omics’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/mlm4omics/new/mlm4omics.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘mlm4omics’ ...
** using staged installation
** libs
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/mlmc_code.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/mmlm_code.stan
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"`"/Library/Frameworks/R.framework/Resources/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c init.cpp -o init.o
Wrote C++ file "stan_files/mmlm_code.cc"
Wrote C++ file "stan_files/mlmc_code.cc"
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"`"/Library/Frameworks/R.framework/Resources/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/mmlm_code.cc -o stan_files/mmlm_code.o
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"`"/Library/Frameworks/R.framework/Resources/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/mlmc_code.cc -o stan_files/mlmc_code.o
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:22:30: error: no template named 'conditional_t' in namespace 'std'; did you mean 'conditional'?
using double_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                        ~~~~~^~~~~~~~~~~~~
                             conditional
/usr/local/clang8/bin/../include/c++/v1/type_traits:429:33: note: 'conditional' declared here
    struct _LIBCPP_TEMPLATE_VIS conditional {typedef _If type;};
                                ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:22:30: error: no template named 'conditional_t' in namespace 'std'; did you mean 'conditional'?
using double_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                        ~~~~~^~~~~~~~~~~~~
                             conditional
/usr/local/clang8/bin/../include/c++/v1/type_traits:429:33: note: 'conditional' declared here
    struct _LIBCPP_TEMPLATE_VIS conditional {typedef _If type;};
                                ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:22:63: error: no template named 'remove_reference_t' in namespace 'std'; did you mean 'remove_reference'?
using double_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                                                         ~~~~~^~~~~~~~~~~~~~~~~~
                                                              remove_reference
/usr/local/clang8/bin/../include/c++/v1/type_traits:1112:50: note: 'remove_reference' declared here
template <class _Tp> struct _LIBCPP_TEMPLATE_VIS remove_reference        {typedef _Tp type;};
                                                 ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:26:31: error: no template named 'conditional_t' in namespace 'std'; did you mean 'conditional'?
using reverse_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                         ~~~~~^~~~~~~~~~~~~
                              conditional
/usr/local/clang8/bin/../include/c++/v1/type_traits:429:33: note: 'conditional' declared here
    struct _LIBCPP_TEMPLATE_VIS conditional {typedef _If type;};
                                ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:22:63: error: no template named 'remove_reference_t' in namespace 'std'; did you mean 'remove_reference'?
using double_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                                                         ~~~~~^~~~~~~~~~~~~~~~~~
                                                              remove_reference
/usr/local/clang8/bin/../include/c++/v1/type_traits:1112:50: note: 'remove_reference' declared here
template <class _Tp> struct _LIBCPP_TEMPLATE_VIS remove_reference        {typedef _Tp type;};
                                                 ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:26:64: error: no template named 'remove_reference_t' in namespace 'std'; did you mean 'remove_reference'?
using reverse_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                                                          ~~~~~^~~~~~~~~~~~~~~~~~
                                                               remove_reference
/usr/local/clang8/bin/../include/c++/v1/type_traits:1112:50: note: 'remove_reference' declared here
template <class _Tp> struct _LIBCPP_TEMPLATE_VIS remove_reference        {typedef _Tp type;};
                                                 ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:26:31: error: no template named 'conditional_t' in namespace 'std'; did you mean 'conditional'?
using reverse_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                         ~~~~~^~~~~~~~~~~~~
                              conditional
/usr/local/clang8/bin/../include/c++/v1/type_traits:429:33: note: 'conditional' declared here
    struct _LIBCPP_TEMPLATE_VIS conditional {typedef _If type;};
                                ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:31:28: error: no template named 'conditional_t' in namespace 'std'; did you mean 'conditional'?
using vari_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                      ~~~~~^~~~~~~~~~~~~
                           conditional
/usr/local/clang8/bin/../include/c++/v1/type_traits:429:33: note: 'conditional' declared here
    struct _LIBCPP_TEMPLATE_VIS conditional {typedef _If type;};
                                ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:26:64: error: no template named 'remove_reference_t' in namespace 'std'; did you mean 'remove_reference'?
using reverse_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                                                          ~~~~~^~~~~~~~~~~~~~~~~~
                                                               remove_reference
/usr/local/clang8/bin/../include/c++/v1/type_traits:1112:50: note: 'remove_reference' declared here
template <class _Tp> struct _LIBCPP_TEMPLATE_VIS remove_reference        {typedef _Tp type;};
                                                 ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:31:61: error: no template named 'remove_reference_t' in namespace 'std'; did you mean 'remove_reference'?
using vari_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                                                       ~~~~~^~~~~~~~~~~~~~~~~~
                                                            remove_reference
/usr/local/clang8/bin/../include/c++/v1/type_traits:1112:50: note: 'remove_reference' declared here
template <class _Tp> struct _LIBCPP_TEMPLATE_VIS remove_reference        {typedef _Tp type;};
                                                 ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:31:28: error: no template named 'conditional_t' in namespace 'std'; did you mean 'conditional'?
using vari_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                      ~~~~~^~~~~~~~~~~~~
                           conditional
/usr/local/clang8/bin/../include/c++/v1/type_traits:429:33: note: 'conditional' declared here
    struct _LIBCPP_TEMPLATE_VIS conditional {typedef _If type;};
                                ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:36:31: error: no template named 'conditional_t' in namespace 'std'; did you mean 'conditional'?
using forward_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                         ~~~~~^~~~~~~~~~~~~
                              conditional
/usr/local/clang8/bin/../include/c++/v1/type_traits:429:33: note: 'conditional' declared here
    struct _LIBCPP_TEMPLATE_VIS conditional {typedef _If type;};
                                ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:31:61: error: no template named 'remove_reference_t' in namespace 'std'; did you mean 'remove_reference'?
using vari_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                                                       ~~~~~^~~~~~~~~~~~~~~~~~
                                                            remove_reference
/usr/local/clang8/bin/../include/c++/v1/type_traits:1112:50: note: 'remove_reference' declared here
template <class _Tp> struct _LIBCPP_TEMPLATE_VIS remove_reference        {typedef _Tp type;};
                                                 ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:36:64: error: no template named 'remove_reference_t' in namespace 'std'; did you mean 'remove_reference'?
using forward_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                                                          ~~~~~^~~~~~~~~~~~~~~~~~
                                                               remove_reference
/usr/local/clang8/bin/../include/c++/v1/type_traits:1112:50: note: 'remove_reference' declared here
template <class _Tp> struct _LIBCPP_TEMPLATE_VIS remove_reference        {typedef _Tp type;};
                                                 ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:36:31: error: no template named 'conditional_t' in namespace 'std'; did you mean 'conditional'?
using forward_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                         ~~~~~^~~~~~~~~~~~~
                              conditional
/usr/local/clang8/bin/../include/c++/v1/type_traits:429:33: note: 'conditional' declared here
    struct _LIBCPP_TEMPLATE_VIS conditional {typedef _If type;};
                                ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:60:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<std::is_pointer<T>::value, reverse_return_t<T>>
    ~~~~~^~~~~~~~~~~
         enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:36:64: error: no template named 'remove_reference_t' in namespace 'std'; did you mean 'remove_reference'?
using forward_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                                                          ~~~~~^~~~~~~~~~~~~~~~~~
                                                               remove_reference
/usr/local/clang8/bin/../include/c++/v1/type_traits:1112:50: note: 'remove_reference' declared here
template <class _Tp> struct _LIBCPP_TEMPLATE_VIS remove_reference        {typedef _Tp type;};
                                                 ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:66:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<(!std::is_pointer<T>::value && !is_fvar<T>::value
    ~~~~~^~~~~~~~~~~
         enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:60:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<std::is_pointer<T>::value, reverse_return_t<T>>
    ~~~~~^~~~~~~~~~~
         enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:73:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<is_fvar<T>::value, forward_return_t<T>>
    ~~~~~^~~~~~~~~~~
         enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:66:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<(!std::is_pointer<T>::value && !is_fvar<T>::value
    ~~~~~^~~~~~~~~~~
         enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:79:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<std::is_arithmetic<T>::value, double_return_t<T>>
    ~~~~~^~~~~~~~~~~
         enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:73:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<is_fvar<T>::value, forward_return_t<T>>
    ~~~~~^~~~~~~~~~~
         enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:79:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<std::is_arithmetic<T>::value, double_return_t<T>>
    ~~~~~^~~~~~~~~~~
         enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:148:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<std::is_pointer<T>::value, reverse_return_t<T>>
    ~~~~~^~~~~~~~~~~
         enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:154:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<!std::is_pointer<T>::value, reverse_return_t<T>>
    ~~~~~^~~~~~~~~~~
         enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:148:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<std::is_pointer<T>::value, reverse_return_t<T>>
    ~~~~~^~~~~~~~~~~
         enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:154:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<!std::is_pointer<T>::value, reverse_return_t<T>>
    ~~~~~^~~~~~~~~~~
         enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigenForward.h:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/CholmodSupport:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigenForward.h:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/CholmodSupport:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigenForward.h:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/KroneckerProduct:34:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigenForward.h:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/KroneckerProduct:34:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigenForward.h:39:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/Polynomials:135:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigenForward.h:39:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/Polynomials:135:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigenForward.h:40:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/SparseExtra:51:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigenForward.h:40:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/SparseExtra:51:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/mat.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core/autodiffstackstorage.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/memory/stack_alloc.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/meta.hpp:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/index_type.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/is_vector.hpp:4:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/scal/meta/is_vector.hpp:23:26: error: no template named 'decay_t' in namespace 'std'; did you mean 'decay'?
    : bool_constant<std::decay_t<T>::RowsAtCompileTime == 1> {};
                    ~~~~~^~~~~~~
                         decay
/usr/local/clang8/bin/../include/c++/v1/type_traits:1363:29: note: 'decay' declared here
struct _LIBCPP_TEMPLATE_VIS decay
                            ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/mat.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core/autodiffstackstorage.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/memory/stack_alloc.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/meta.hpp:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/index_type.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/is_vector.hpp:4:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/scal/meta/is_vector.hpp:23:26: error: no template named 'decay_t' in namespace 'std'; did you mean 'decay'?
    : bool_constant<std::decay_t<T>::RowsAtCompileTime == 1> {};
                    ~~~~~^~~~~~~
                         decay
/usr/local/clang8/bin/../include/c++/v1/type_traits:1363:29: note: 'decay' declared here
struct _LIBCPP_TEMPLATE_VIS decay
                            ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/mat.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core/autodiffstackstorage.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/memory/stack_alloc.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/meta.hpp:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/index_type.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/is_vector.hpp:4:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/scal/meta/is_vector.hpp:36:41: error: no template named 'decay_t' in namespace 'std'; did you mean 'decay'?
    : std::integral_constant<bool, std::decay_t<T>::ColsAtCompileTime == 1> {};
                                   ~~~~~^~~~~~~
                                        decay
/usr/local/clang8/bin/../include/c++/v1/type_traits:1363:29: note: 'decay' declared here
struct _LIBCPP_TEMPLATE_VIS decay
                            ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/mat.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core/autodiffstackstorage.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/memory/stack_alloc.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/meta.hpp:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/index_type.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/is_vector.hpp:4:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/scal/meta/is_vector.hpp:36:41: error: no template named 'decay_t' in namespace 'std'; did you mean 'decay'?
    : std::integral_constant<bool, std::decay_t<T>::ColsAtCompileTime == 1> {};
                                   ~~~~~^~~~~~~
                                        decay
/usr/local/clang8/bin/../include/c++/v1/type_traits:1363:29: note: 'decay' declared here
struct _LIBCPP_TEMPLATE_VIS decay
                            ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/mat.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core/autodiffstackstorage.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/memory/stack_alloc.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/meta.hpp:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/index_type.hpp:4:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/is_vector.hpp:32:13: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    T, std::enable_if_t<internal::is_std_vector_impl<std::decay_t<T>>::value>>
       ~~~~~^~~~~~~~~~~
            enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/mat.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core/autodiffstackstorage.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/memory/stack_alloc.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/meta.hpp:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/index_type.hpp:4:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/is_vector.hpp:32:13: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    T, std::enable_if_t<internal::is_std_vector_impl<std::decay_t<T>>::value>>
       ~~~~~^~~~~~~~~~~
            enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/mat.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core/autodiffstackstorage.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/memory/stack_alloc.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/meta.hpp:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/index_type.hpp:4:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/is_vector.hpp:32:59: error: no template named 'decay_t' in namespace 'std'; did you mean 'decay'?
    T, std::enable_if_t<internal::is_std_vector_impl<std::decay_t<T>>::value>>
                                                     ~~~~~^~~~~~~
                                                          decay
/usr/local/clang8/bin/../include/c++/v1/type_traits:1363:29: note: 'decay' declared here
struct _LIBCPP_TEMPLATE_VIS decay
                            ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/mat.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core/autodiffstackstorage.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/memory/stack_alloc.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/meta.hpp:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/index_type.hpp:4:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/is_vector.hpp:32:59: error: no template named 'decay_t' in namespace 'std'; did you mean 'decay'?
    T, std::enable_if_t<internal::is_std_vector_impl<std::decay_t<T>>::value>>
                                                     ~~~~~^~~~~~~
                                                          decay
/usr/local/clang8/bin/../include/c++/v1/type_traits:1363:29: note: 'decay' declared here
struct _LIBCPP_TEMPLATE_VIS decay
                            ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/mat.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core/autodiffstackstorage.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/memory/stack_alloc.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/meta.hpp:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/index_type.hpp:5:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/scal/meta/index_type.hpp:28:27: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
struct index_type<T, std::enable_if_t<std::is_pointer<T>::value>> {
                     ~~~~~^~~~~~~~~~~
                          enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/mat.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core/autodiffstackstorage.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/memory/stack_alloc.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/meta.hpp:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/index_type.hpp:5:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/scal/meta/index_type.hpp:28:27: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
struct index_type<T, std::enable_if_t<std::is_pointer<T>::value>> {
                     ~~~~~^~~~~~~~~~~
                          enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
fatal error: too many errors emitted, stopping now [-ferror-limit=]
fatal error: too many errors emitted, stopping now [-ferror-limit=]
17 warnings and 20 errors generated.
make: *** [stan_files/mlmc_code.o] Error 1
make: *** Waiting for unfinished jobs....
17 warnings and 20 errors generated.
make: *** [stan_files/mmlm_code.o] Error 1
rm stan_files/mlmc_code.cc stan_files/mmlm_code.cc
ERROR: compilation failed for package ‘mlm4omics’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/mlm4omics/new/mlm4omics.Rcheck/mlm4omics’

```
### CRAN

```
* installing *source* package ‘mlm4omics’ ...
** using staged installation
** libs
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/mlmc_code.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/mmlm_code.stan
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"`"/Library/Frameworks/R.framework/Resources/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c init.cpp -o init.o
Wrote C++ file "stan_files/mmlm_code.cc"
Wrote C++ file "stan_files/mlmc_code.cc"
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"`"/Library/Frameworks/R.framework/Resources/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/mmlm_code.cc -o stan_files/mmlm_code.o
/usr/local/clang8/bin/clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"`"/Library/Frameworks/R.framework/Resources/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/mlmc_code.cc -o stan_files/mlmc_code.o
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:22:30: error: no template named 'conditional_t' in namespace 'std'; did you mean 'conditional'?
using double_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                        ~~~~~^~~~~~~~~~~~~
                             conditional
/usr/local/clang8/bin/../include/c++/v1/type_traits:429:33: note: 'conditional' declared here
    struct _LIBCPP_TEMPLATE_VIS conditional {typedef _If type;};
                                ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:22:63: error: no template named 'remove_reference_t' in namespace 'std'; did you mean 'remove_reference'?
using double_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                                                         ~~~~~^~~~~~~~~~~~~~~~~~
                                                              remove_reference
/usr/local/clang8/bin/../include/c++/v1/type_traits:1112:50: note: 'remove_reference' declared here
template <class _Tp> struct _LIBCPP_TEMPLATE_VIS remove_reference        {typedef _Tp type;};
                                                 ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:26:31: error: no template named 'conditional_t' in namespace 'std'; did you mean 'conditional'?
using reverse_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                         ~~~~~^~~~~~~~~~~~~
                              conditional
/usr/local/clang8/bin/../include/c++/v1/type_traits:429:33: note: 'conditional' declared here
    struct _LIBCPP_TEMPLATE_VIS conditional {typedef _If type;};
                                ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:22:30: error: no template named 'conditional_t' in namespace 'std'; did you mean 'conditional'?
using double_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                        ~~~~~^~~~~~~~~~~~~
                             conditional
/usr/local/clang8/bin/../include/c++/v1/type_traits:429:33: note: 'conditional' declared here
    struct _LIBCPP_TEMPLATE_VIS conditional {typedef _If type;};
                                ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:26:64: error: no template named 'remove_reference_t' in namespace 'std'; did you mean 'remove_reference'?
using reverse_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                                                          ~~~~~^~~~~~~~~~~~~~~~~~
                                                               remove_reference
/usr/local/clang8/bin/../include/c++/v1/type_traits:1112:50: note: 'remove_reference' declared here
template <class _Tp> struct _LIBCPP_TEMPLATE_VIS remove_reference        {typedef _Tp type;};
                                                 ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:22:63: error: no template named 'remove_reference_t' in namespace 'std'; did you mean 'remove_reference'?
using double_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                                                         ~~~~~^~~~~~~~~~~~~~~~~~
                                                              remove_reference
/usr/local/clang8/bin/../include/c++/v1/type_traits:1112:50: note: 'remove_reference' declared here
template <class _Tp> struct _LIBCPP_TEMPLATE_VIS remove_reference        {typedef _Tp type;};
                                                 ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:31:28: error: no template named 'conditional_t' in namespace 'std'; did you mean 'conditional'?
using vari_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                      ~~~~~^~~~~~~~~~~~~
                           conditional
/usr/local/clang8/bin/../include/c++/v1/type_traits:429:33: note: 'conditional' declared here
    struct _LIBCPP_TEMPLATE_VIS conditional {typedef _If type;};
                                ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:26:31: error: no template named 'conditional_t' in namespace 'std'; did you mean 'conditional'?
using reverse_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                         ~~~~~^~~~~~~~~~~~~
                              conditional
/usr/local/clang8/bin/../include/c++/v1/type_traits:429:33: note: 'conditional' declared here
    struct _LIBCPP_TEMPLATE_VIS conditional {typedef _If type;};
                                ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:31:61: error: no template named 'remove_reference_t' in namespace 'std'; did you mean 'remove_reference'?
using vari_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                                                       ~~~~~^~~~~~~~~~~~~~~~~~
                                                            remove_reference
/usr/local/clang8/bin/../include/c++/v1/type_traits:1112:50: note: 'remove_reference' declared here
template <class _Tp> struct _LIBCPP_TEMPLATE_VIS remove_reference        {typedef _Tp type;};
                                                 ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:26:64: error: no template named 'remove_reference_t' in namespace 'std'; did you mean 'remove_reference'?
using reverse_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                                                          ~~~~~^~~~~~~~~~~~~~~~~~
                                                               remove_reference
/usr/local/clang8/bin/../include/c++/v1/type_traits:1112:50: note: 'remove_reference' declared here
template <class _Tp> struct _LIBCPP_TEMPLATE_VIS remove_reference        {typedef _Tp type;};
                                                 ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:36:31: error: no template named 'conditional_t' in namespace 'std'; did you mean 'conditional'?
using forward_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                         ~~~~~^~~~~~~~~~~~~
                              conditional
/usr/local/clang8/bin/../include/c++/v1/type_traits:429:33: note: 'conditional' declared here
    struct _LIBCPP_TEMPLATE_VIS conditional {typedef _If type;};
                                ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:31:28: error: no template named 'conditional_t' in namespace 'std'; did you mean 'conditional'?
using vari_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                      ~~~~~^~~~~~~~~~~~~
                           conditional
/usr/local/clang8/bin/../include/c++/v1/type_traits:429:33: note: 'conditional' declared here
    struct _LIBCPP_TEMPLATE_VIS conditional {typedef _If type;};
                                ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:36:64: error: no template named 'remove_reference_t' in namespace 'std'; did you mean 'remove_reference'?
using forward_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                                                          ~~~~~^~~~~~~~~~~~~~~~~~
                                                               remove_reference
/usr/local/clang8/bin/../include/c++/v1/type_traits:1112:50: note: 'remove_reference' declared here
template <class _Tp> struct _LIBCPP_TEMPLATE_VIS remove_reference        {typedef _Tp type;};
                                                 ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:31:61: error: no template named 'remove_reference_t' in namespace 'std'; did you mean 'remove_reference'?
using vari_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                                                       ~~~~~^~~~~~~~~~~~~~~~~~
                                                            remove_reference
/usr/local/clang8/bin/../include/c++/v1/type_traits:1112:50: note: 'remove_reference' declared here
template <class _Tp> struct _LIBCPP_TEMPLATE_VIS remove_reference        {typedef _Tp type;};
                                                 ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:60:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<std::is_pointer<T>::value, reverse_return_t<T>>
    ~~~~~^~~~~~~~~~~
         enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:36:31: error: no template named 'conditional_t' in namespace 'std'; did you mean 'conditional'?
using forward_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                         ~~~~~^~~~~~~~~~~~~
                              conditional
/usr/local/clang8/bin/../include/c++/v1/type_traits:429:33: note: 'conditional' declared here
    struct _LIBCPP_TEMPLATE_VIS conditional {typedef _If type;};
                                ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:66:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<(!std::is_pointer<T>::value && !is_fvar<T>::value
    ~~~~~^~~~~~~~~~~
         enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:73:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<is_fvar<T>::value, forward_return_t<T>>
    ~~~~~^~~~~~~~~~~
         enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:36:64: error: no template named 'remove_reference_t' in namespace 'std'; did you mean 'remove_reference'?
using forward_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                                                          ~~~~~^~~~~~~~~~~~~~~~~~
                                                               remove_reference
/usr/local/clang8/bin/../include/c++/v1/type_traits:1112:50: note: 'remove_reference' declared here
template <class _Tp> struct _LIBCPP_TEMPLATE_VIS remove_reference        {typedef _Tp type;};
                                                 ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:60:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<std::is_pointer<T>::value, reverse_return_t<T>>
    ~~~~~^~~~~~~~~~~
         enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:79:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<std::is_arithmetic<T>::value, double_return_t<T>>
    ~~~~~^~~~~~~~~~~
         enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:66:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<(!std::is_pointer<T>::value && !is_fvar<T>::value
    ~~~~~^~~~~~~~~~~
         enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:148:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<std::is_pointer<T>::value, reverse_return_t<T>>
    ~~~~~^~~~~~~~~~~
         enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:73:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<is_fvar<T>::value, forward_return_t<T>>
    ~~~~~^~~~~~~~~~~
         enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
In file included from template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:154:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<!std::is_pointer<T>::value, reverse_return_t<T>>
    ~~~~~^~~~~~~~~~~
         enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:79:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<std::is_arithmetic<T>::value, double_return_t<T>>
    ~~~~~^~~~~~~~~~~
         enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:148:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<std::is_pointer<T>::value, reverse_return_t<T>>
    ~~~~~^~~~~~~~~~~
         enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:154:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<!std::is_pointer<T>::value, reverse_return_t<T>>
    ~~~~~^~~~~~~~~~~
         enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigenForward.h:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/CholmodSupport:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigenForward.h:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/CholmodSupport:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigenForward.h:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/KroneckerProduct:34:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigenForward.h:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/KroneckerProduct:34:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigenForward.h:39:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/Polynomials:135:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigenForward.h:40:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/SparseExtra:51:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigenForward.h:39:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/Polynomials:135:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigenForward.h:40:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/SparseExtra:51:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/mat.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core/autodiffstackstorage.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/memory/stack_alloc.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/meta.hpp:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/index_type.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/is_vector.hpp:4:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/scal/meta/is_vector.hpp:23:26: error: no template named 'decay_t' in namespace 'std'; did you mean 'decay'?
    : bool_constant<std::decay_t<T>::RowsAtCompileTime == 1> {};
                    ~~~~~^~~~~~~
                         decay
/usr/local/clang8/bin/../include/c++/v1/type_traits:1363:29: note: 'decay' declared here
struct _LIBCPP_TEMPLATE_VIS decay
                            ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/mat.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core/autodiffstackstorage.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/memory/stack_alloc.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/meta.hpp:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/index_type.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/is_vector.hpp:4:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/scal/meta/is_vector.hpp:36:41: error: no template named 'decay_t' in namespace 'std'; did you mean 'decay'?
    : std::integral_constant<bool, std::decay_t<T>::ColsAtCompileTime == 1> {};
                                   ~~~~~^~~~~~~
                                        decay
/usr/local/clang8/bin/../include/c++/v1/type_traits:1363:29: note: 'decay' declared here
struct _LIBCPP_TEMPLATE_VIS decay
                            ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/mat.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core/autodiffstackstorage.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/memory/stack_alloc.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/meta.hpp:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/index_type.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/is_vector.hpp:4:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/scal/meta/is_vector.hpp:23:26: error: no template named 'decay_t' in namespace 'std'; did you mean 'decay'?
    : bool_constant<std::decay_t<T>::RowsAtCompileTime == 1> {};
                    ~~~~~^~~~~~~
                         decay
/usr/local/clang8/bin/../include/c++/v1/type_traits:1363:29: note: 'decay' declared here
struct _LIBCPP_TEMPLATE_VIS decay
                            ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/mat.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core/autodiffstackstorage.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/memory/stack_alloc.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/meta.hpp:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/index_type.hpp:4:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/is_vector.hpp:32:13: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    T, std::enable_if_t<internal::is_std_vector_impl<std::decay_t<T>>::value>>
       ~~~~~^~~~~~~~~~~
            enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/mat.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core/autodiffstackstorage.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/memory/stack_alloc.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/meta.hpp:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/index_type.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/is_vector.hpp:4:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/scal/meta/is_vector.hpp:36:41: error: no template named 'decay_t' in namespace 'std'; did you mean 'decay'?
    : std::integral_constant<bool, std::decay_t<T>::ColsAtCompileTime == 1> {};
                                   ~~~~~^~~~~~~
                                        decay
/usr/local/clang8/bin/../include/c++/v1/type_traits:1363:29: note: 'decay' declared here
struct _LIBCPP_TEMPLATE_VIS decay
                            ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/mat.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core/autodiffstackstorage.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/memory/stack_alloc.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/meta.hpp:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/index_type.hpp:4:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/is_vector.hpp:32:59: error: no template named 'decay_t' in namespace 'std'; did you mean 'decay'?
    T, std::enable_if_t<internal::is_std_vector_impl<std::decay_t<T>>::value>>
                                                     ~~~~~^~~~~~~
                                                          decay
/usr/local/clang8/bin/../include/c++/v1/type_traits:1363:29: note: 'decay' declared here
struct _LIBCPP_TEMPLATE_VIS decay
                            ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/mat.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core/autodiffstackstorage.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/memory/stack_alloc.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/meta.hpp:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/index_type.hpp:4:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/is_vector.hpp:32:13: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    T, std::enable_if_t<internal::is_std_vector_impl<std::decay_t<T>>::value>>
       ~~~~~^~~~~~~~~~~
            enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mmlm_code.cc:3:
In file included from stan_files/mmlm_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/mat.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core/autodiffstackstorage.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/memory/stack_alloc.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/meta.hpp:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/index_type.hpp:5:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/scal/meta/index_type.hpp:28:27: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
struct index_type<T, std::enable_if_t<std::is_pointer<T>::value>> {
                     ~~~~~^~~~~~~~~~~
                          enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/mat.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core/autodiffstackstorage.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/memory/stack_alloc.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/meta.hpp:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/index_type.hpp:4:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/is_vector.hpp:32:59: error: no template named 'decay_t' in namespace 'std'; did you mean 'decay'?
    T, std::enable_if_t<internal::is_std_vector_impl<std::decay_t<T>>::value>>
                                                     ~~~~~^~~~~~~
                                                          decay
/usr/local/clang8/bin/../include/c++/v1/type_traits:1363:29: note: 'decay' declared here
struct _LIBCPP_TEMPLATE_VIS decay
                            ^
fatal error: too many errors emitted, stopping now [-ferror-limit=]
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/mat.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core/autodiffstackstorage.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/memory/stack_alloc.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/meta.hpp:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/index_type.hpp:5:
/Users/max/github/forks/ggplot2/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/scal/meta/index_type.hpp:28:27: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
struct index_type<T, std::enable_if_t<std::is_pointer<T>::value>> {
                     ~~~~~^~~~~~~~~~~
                          enable_if
/usr/local/clang8/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
fatal error: too many errors emitted, stopping now [-ferror-limit=]
17 warnings and 20 errors generated.
17 warnings and 20 errors generated.
make: *** [stan_files/mmlm_code.o] Error 1
make: *** Waiting for unfinished jobs....
make: *** [stan_files/mlmc_code.o] Error 1
rm stan_files/mlmc_code.cc stan_files/mmlm_code.cc
ERROR: compilation failed for package ‘mlm4omics’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/mlm4omics/old/mlm4omics.Rcheck/mlm4omics’

```
# morse

<details>

* Version: 3.2.5
* Source code: https://github.com/cran/morse
* URL: https://cran.r-project.org/package=morse
* BugReports: https://github.com/pveber/morse
* Date/Publication: 2019-09-27 08:50:02 UTC
* Number of recursive dependencies: 73

Run `revdep_details(,"morse")` for more info

</details>

## In both

*   checking whether package ‘morse’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/morse/new/morse.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘morse’ ...
** package ‘morse’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/morse/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/morse/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/morse/rjags/libs/rjags.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘morse’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/morse/new/morse.Rcheck/morse’

```
### CRAN

```
* installing *source* package ‘morse’ ...
** package ‘morse’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/morse/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/morse/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/morse/rjags/libs/rjags.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘morse’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/morse/old/morse.Rcheck/morse’

```
# MSnbase

<details>

* Version: 2.10.1
* Source code: https://github.com/cran/MSnbase
* URL: https://github.com/lgatto/MSnbase
* BugReports: https://github.com/lgatto/MSnbase/issues
* Date/Publication: 2019-05-31
* Number of recursive dependencies: 220

Run `revdep_details(,"MSnbase")` for more info

</details>

## In both

*   R CMD check timed out
    

*   checking installed package size ... NOTE
    ```
      installed size is 13.1Mb
      sub-directories of 1Mb or more:
        R      2.0Mb
        data   1.9Mb
        doc    7.7Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Authors@R field gives more than one person with maintainer role:
      Laurent Gatto <laurent.gatto@uclouvain.be> [aut, cre] (<https://orcid.org/0000-0002-1520-2268>)
      Johannes Rainer <Johannes.Rainer@eurac.edu> [aut, cre] (<https://orcid.org/0000-0002-6977-7147>)
      Sebastian Gibb <mail@sebastiangibb.de> [aut, cre]
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘Biobase:::.showAnnotatedDataFrame’ ‘MALDIquant:::.estimateNoise’
      ‘MALDIquant:::.localMaxima’ ‘MALDIquant:::.movingAverage’
      ‘MALDIquant:::.savitzkyGolay’
      ‘S4Vectors:::makeClassinfoRowForCompactPrinting’
      ‘S4Vectors:::makePrettyMatrixForCompactPrinting’
      ‘mzR:::.hasChromatograms’ ‘mzR:::.hasSpectra’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking S3 generic/method consistency ... NOTE
    ```
    Found the following apparent S3 methods exported but not registered:
      as.data.frame.MSnExp as.data.frame.MSnSet as.data.frame.Spectrum
      as.data.frame.mzRident as.matrix.FoICollection droplevels.MSnSet
      is.na.MSnSet t.MSnSet
    See section ‘Registering S3 methods’ in the ‘Writing R Extensions’
    manual.
    ```

# mwaved

<details>

* Version: 1.1.7
* Source code: https://github.com/cran/mwaved
* URL: https://github.com/jrwishart/mwaved
* BugReports: https://github.com/jrwishart/mwaved/issues
* Date/Publication: 2019-11-10 11:30:02 UTC
* Number of recursive dependencies: 70

Run `revdep_details(,"mwaved")` for more info

</details>

## In both

*   checking whether package ‘mwaved’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/mwaved/new/mwaved.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘mwaved’ ...
** package ‘mwaved’ successfully unpacked and MD5 sums checked
** using staged installation
checking for Rscript... yes
configure: Checking if FFTW3 library is available using pkg-config
checking for pkg-config... /usr/local/bin/pkg-config
checking pkg-config is at least version 0.9.0... yes
checking for FFTW... no
FFTW library not found, please install fftw-3-3-3 or greater
configure: creating ./config.status
config.status: creating src/Makevars
** libs
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mwaved/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mwaved/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c mwaved.cpp -o mwaved.o
mwaved.cpp:1:10: fatal error: 'fftw3.h' file not found
#include <fftw3.h>
         ^~~~~~~~~
1 error generated.
make: *** [mwaved.o] Error 1
make: *** Waiting for unfinished jobs....
ERROR: compilation failed for package ‘mwaved’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/mwaved/new/mwaved.Rcheck/mwaved’

```
### CRAN

```
* installing *source* package ‘mwaved’ ...
** package ‘mwaved’ successfully unpacked and MD5 sums checked
** using staged installation
checking for Rscript... yes
configure: Checking if FFTW3 library is available using pkg-config
checking for pkg-config... /usr/local/bin/pkg-config
checking pkg-config is at least version 0.9.0... yes
checking for FFTW... no
FFTW library not found, please install fftw-3-3-3 or greater
configure: creating ./config.status
config.status: creating src/Makevars
** libs
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mwaved/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
/usr/local/clang8/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/mwaved/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c mwaved.cpp -o mwaved.o
mwaved.cpp:1:10: fatal error: 'fftw3.h' file not found
#include <fftw3.h>
         ^~~~~~~~~
1 error generated.
make: *** [mwaved.o] Error 1
make: *** Waiting for unfinished jobs....
ERROR: compilation failed for package ‘mwaved’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/mwaved/old/mwaved.Rcheck/mwaved’

```
# nlmixr

<details>

* Version: 1.1.1-5
* Source code: https://github.com/cran/nlmixr
* URL: https://github.com/nlmixrdevelopment/nlmixr
* Date/Publication: 2020-01-27 13:20:02 UTC
* Number of recursive dependencies: 163

Run `revdep_details(,"nlmixr")` for more info

</details>

## In both

*   checking whether package ‘nlmixr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/nlmixr/new/nlmixr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘nlmixr’ ...
** package ‘nlmixr’ successfully unpacked and MD5 sums checked
** using staged installation
checking for gcc... /usr/local/clang8/bin/clang
checking whether the C compiler works... yes
checking for C compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... configure: error: in `/Users/max/github/forks/ggplot2/revdep/checks.noindex/nlmixr/new/nlmixr.Rcheck/00_pkg_src/nlmixr':
configure: error: cannot run C compiled programs.
If you meant to cross compile, use `--host'.
See `config.log' for more details
ERROR: configuration failed for package ‘nlmixr’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/nlmixr/new/nlmixr.Rcheck/nlmixr’

```
### CRAN

```
* installing *source* package ‘nlmixr’ ...
** package ‘nlmixr’ successfully unpacked and MD5 sums checked
** using staged installation
checking for gcc... /usr/local/clang8/bin/clang
checking whether the C compiler works... yes
checking for C compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... configure: error: in `/Users/max/github/forks/ggplot2/revdep/checks.noindex/nlmixr/old/nlmixr.Rcheck/00_pkg_src/nlmixr':
configure: error: cannot run C compiled programs.
If you meant to cross compile, use `--host'.
See `config.log' for more details
ERROR: configuration failed for package ‘nlmixr’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/nlmixr/old/nlmixr.Rcheck/nlmixr’

```
# OpenStreetMap

<details>

* Version: 0.3.4
* Source code: https://github.com/cran/OpenStreetMap
* URL: https://github.com/ifellows/ROSM http://www.fellstat.com http://blog.fellstat.com/?cat=15
* Date/Publication: 2019-05-31 17:40:02 UTC
* Number of recursive dependencies: 43

Run `revdep_details(,"OpenStreetMap")` for more info

</details>

## In both

*   checking whether package ‘OpenStreetMap’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/OpenStreetMap/new/OpenStreetMap.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘OpenStreetMap’ ...
** package ‘OpenStreetMap’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/OpenStreetMap/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/OpenStreetMap/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/OpenStreetMap/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘OpenStreetMap’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/OpenStreetMap/new/OpenStreetMap.Rcheck/OpenStreetMap’

```
### CRAN

```
* installing *source* package ‘OpenStreetMap’ ...
** package ‘OpenStreetMap’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/OpenStreetMap/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/OpenStreetMap/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/OpenStreetMap/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘OpenStreetMap’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/OpenStreetMap/old/OpenStreetMap.Rcheck/OpenStreetMap’

```
# openVA

<details>

* Version: 1.0.8
* Source code: https://github.com/cran/openVA
* URL: https://github.com/verbal-autopsy-software/openVA
* BugReports: https://github.com/verbal-autopsy-software/openVA/issues
* Date/Publication: 2019-02-18 06:40:02 UTC
* Number of recursive dependencies: 51

Run `revdep_details(,"openVA")` for more info

</details>

## In both

*   checking whether package ‘openVA’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/openVA/new/openVA.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘openVA’ ...
** package ‘openVA’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/openVA/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/openVA/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/openVA/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘openVA’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/openVA/new/openVA.Rcheck/openVA’

```
### CRAN

```
* installing *source* package ‘openVA’ ...
** package ‘openVA’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/openVA/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/openVA/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/openVA/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘openVA’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/openVA/old/openVA.Rcheck/openVA’

```
# petro.One

<details>

* Version: 0.2.3
* Source code: https://github.com/cran/petro.One
* URL: https://github.com/f0nzie/petro.One
* Date/Publication: 2019-01-13 16:20:03 UTC
* Number of recursive dependencies: 92

Run `revdep_details(,"petro.One")` for more info

</details>

## In both

*   checking whether package ‘petro.One’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/petro.One/new/petro.One.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘petro.One’ ...
** package ‘petro.One’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/petro.One/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/petro.One/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/petro.One/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘petro.One’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/petro.One/new/petro.One.Rcheck/petro.One’

```
### CRAN

```
* installing *source* package ‘petro.One’ ...
** package ‘petro.One’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/petro.One/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/petro.One/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/petro.One/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘petro.One’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/petro.One/old/petro.One.Rcheck/petro.One’

```
# phase1PRMD

<details>

* Version: 1.0.1
* Source code: https://github.com/cran/phase1PRMD
* Date/Publication: 2019-02-03 17:00:03 UTC
* Number of recursive dependencies: 79

Run `revdep_details(,"phase1PRMD")` for more info

</details>

## In both

*   checking whether package ‘phase1PRMD’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/phase1PRMD/new/phase1PRMD.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘phase1PRMD’ ...
** package ‘phase1PRMD’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/phase1PRMD/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/phase1PRMD/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/phase1PRMD/rjags/libs/rjags.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘phase1PRMD’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/phase1PRMD/new/phase1PRMD.Rcheck/phase1PRMD’

```
### CRAN

```
* installing *source* package ‘phase1PRMD’ ...
** package ‘phase1PRMD’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/phase1PRMD/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/phase1PRMD/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/phase1PRMD/rjags/libs/rjags.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘phase1PRMD’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/phase1PRMD/old/phase1PRMD.Rcheck/phase1PRMD’

```
# phase1RMD

<details>

* Version: 1.0.8
* Source code: https://github.com/cran/phase1RMD
* Date/Publication: 2017-11-27 08:49:15 UTC
* Number of recursive dependencies: 51

Run `revdep_details(,"phase1RMD")` for more info

</details>

## In both

*   checking whether package ‘phase1RMD’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/phase1RMD/new/phase1RMD.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘phase1RMD’ ...
** package ‘phase1RMD’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rjags’:
 .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/phase1RMD/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/phase1RMD/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/phase1RMD/rjags/libs/rjags.so
  Reason: image not found
Error: package ‘rjags’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘phase1RMD’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/phase1RMD/new/phase1RMD.Rcheck/phase1RMD’

```
### CRAN

```
* installing *source* package ‘phase1RMD’ ...
** package ‘phase1RMD’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rjags’:
 .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/phase1RMD/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/phase1RMD/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/phase1RMD/rjags/libs/rjags.so
  Reason: image not found
Error: package ‘rjags’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘phase1RMD’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/phase1RMD/old/phase1RMD.Rcheck/phase1RMD’

```
# PortfolioEffectHFT

<details>

* Version: 1.8
* Source code: https://github.com/cran/PortfolioEffectHFT
* URL: https://www.portfolioeffect.com/
* Date/Publication: 2017-03-24 19:54:25 UTC
* Number of recursive dependencies: 52

Run `revdep_details(,"PortfolioEffectHFT")` for more info

</details>

## In both

*   checking whether package ‘PortfolioEffectHFT’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/PortfolioEffectHFT/new/PortfolioEffectHFT.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘PortfolioEffectHFT’ ...
** package ‘PortfolioEffectHFT’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** demo
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/PortfolioEffectHFT/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/PortfolioEffectHFT/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/PortfolioEffectHFT/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘PortfolioEffectHFT’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/PortfolioEffectHFT/new/PortfolioEffectHFT.Rcheck/PortfolioEffectHFT’

```
### CRAN

```
* installing *source* package ‘PortfolioEffectHFT’ ...
** package ‘PortfolioEffectHFT’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** demo
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/PortfolioEffectHFT/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/PortfolioEffectHFT/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/PortfolioEffectHFT/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘PortfolioEffectHFT’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/PortfolioEffectHFT/old/PortfolioEffectHFT.Rcheck/PortfolioEffectHFT’

```
# qdap

<details>

* Version: 2.3.6
* Source code: https://github.com/cran/qdap
* URL: http://trinker.github.com/qdap/
* BugReports: http://github.com/trinker/qdap/issues
* Date/Publication: 2020-01-09 18:30:06 UTC
* Number of recursive dependencies: 100

Run `revdep_details(,"qdap")` for more info

</details>

## In both

*   checking whether package ‘qdap’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/qdap/new/qdap.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘qdap’ ...
** package ‘qdap’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/qdap/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/qdap/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/qdap/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘qdap’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/qdap/new/qdap.Rcheck/qdap’

```
### CRAN

```
* installing *source* package ‘qdap’ ...
** package ‘qdap’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/qdap/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/qdap/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/qdap/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘qdap’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/qdap/old/qdap.Rcheck/qdap’

```
# rcellminer

<details>

* Version: 2.6.0
* Source code: https://github.com/cran/rcellminer
* URL: http://discover.nci.nih.gov/cellminer/
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 110

Run `revdep_details(,"rcellminer")` for more info

</details>

## In both

*   checking whether package ‘rcellminer’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/rcellminer/new/rcellminer.Rcheck/00install.out’ for details.
    ```

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .BBSoptions
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

## Installation

### Devel

```
* installing *source* package ‘rcellminer’ ...
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/rcellminer/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/rcellminer/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/rcellminer/rJava/libs/rJava.so
  Reason: image not found
Error: package ‘rJava’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘rcellminer’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/rcellminer/new/rcellminer.Rcheck/rcellminer’

```
### CRAN

```
* installing *source* package ‘rcellminer’ ...
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/rcellminer/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/rcellminer/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/rcellminer/rJava/libs/rJava.so
  Reason: image not found
Error: package ‘rJava’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘rcellminer’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/rcellminer/old/rcellminer.Rcheck/rcellminer’

```
# RclusTool

<details>

* Version: 0.91.3
* Source code: https://github.com/cran/RclusTool
* URL: http://mawenzi.univ-littoral.fr/RclusTool/
* Date/Publication: 2020-02-04 16:20:05 UTC
* Number of recursive dependencies: 103

Run `revdep_details(,"RclusTool")` for more info

</details>

## In both

*   R CMD check timed out
    

# RcmdrPlugin.FuzzyClust

<details>

* Version: 1.1
* Source code: https://github.com/cran/RcmdrPlugin.FuzzyClust
* Date/Publication: 2016-09-04 09:36:21
* Number of recursive dependencies: 121

Run `revdep_details(,"RcmdrPlugin.FuzzyClust")` for more info

</details>

## In both

*   R CMD check timed out
    

# RDAVIDWebService

<details>

* Version: 1.22.0
* Source code: https://github.com/cran/RDAVIDWebService
* URL: http://www.bdmg.com.ar, http://david.abcc.ncifcrf.gov/
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 66

Run `revdep_details(,"RDAVIDWebService")` for more info

</details>

## In both

*   checking whether package ‘RDAVIDWebService’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/RDAVIDWebService/new/RDAVIDWebService.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘RDAVIDWebService’ ...
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/RDAVIDWebService/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/RDAVIDWebService/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/RDAVIDWebService/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘RDAVIDWebService’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/RDAVIDWebService/new/RDAVIDWebService.Rcheck/RDAVIDWebService’

```
### CRAN

```
* installing *source* package ‘RDAVIDWebService’ ...
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/RDAVIDWebService/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/RDAVIDWebService/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/RDAVIDWebService/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘RDAVIDWebService’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/RDAVIDWebService/old/RDAVIDWebService.Rcheck/RDAVIDWebService’

```
# Rdrools

<details>

* Version: 1.1.1
* Source code: https://github.com/cran/Rdrools
* Date/Publication: 2018-12-08 15:00:13 UTC
* Number of recursive dependencies: 79

Run `revdep_details(,"Rdrools")` for more info

</details>

## In both

*   checking whether package ‘Rdrools’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/Rdrools/new/Rdrools.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘Rdrools’ ...
** package ‘Rdrools’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/Rdrools/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/Rdrools/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/Rdrools/rJava/libs/rJava.so
  Reason: image not found
Error: package ‘rJava’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘Rdrools’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/Rdrools/new/Rdrools.Rcheck/Rdrools’

```
### CRAN

```
* installing *source* package ‘Rdrools’ ...
** package ‘Rdrools’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/Rdrools/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/Rdrools/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/Rdrools/rJava/libs/rJava.so
  Reason: image not found
Error: package ‘rJava’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘Rdrools’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/Rdrools/old/Rdrools.Rcheck/Rdrools’

```
# ReactomePA

<details>

* Version: 
* Source code: ???
* URL: http://ggplot2.tidyverse.org, https://github.com/tidyverse/ggplot2
* BugReports: https://github.com/tidyverse/ggplot2/issues
* Number of recursive dependencies: 0

Run `revdep_details(,"")` for more info

</details>

## Error before installation

### Devel

```

  There are binary versions available but the source versions are later:
           binary  source needs_compilation
checkmate   1.9.4   2.0.0              TRUE
nlme      3.1-143 3.1-144              TRUE

  Binaries will be installed


Warning: unable to access index for repository https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6:
  cannot open URL 'https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6/PACKAGES'
Warning: unable to access index for repository https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6:
  cannot open URL 'https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6/PACKAGES'
installing the source package ‘reactome.db’



```
### CRAN

```

  There are binary versions available but the source versions are later:
           binary  source needs_compilation
checkmate   1.9.4   2.0.0              TRUE
nlme      3.1-143 3.1-144              TRUE

  Binaries will be installed


Warning: unable to access index for repository https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6:
  cannot open URL 'https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6/PACKAGES'
Warning: unable to access index for repository https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6:
  cannot open URL 'https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6/PACKAGES'
installing the source package ‘reactome.db’



```
# rmcfs

<details>

* Version: 1.3.1
* Source code: https://github.com/cran/rmcfs
* URL: https://home.ipipan.waw.pl/m.draminski/mcfs.html
* Date/Publication: 2020-01-20 12:40:03 UTC
* Number of recursive dependencies: 65

Run `revdep_details(,"rmcfs")` for more info

</details>

## In both

*   checking whether package ‘rmcfs’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/rmcfs/new/rmcfs.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rmcfs’ ...
** package ‘rmcfs’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/rmcfs/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/rmcfs/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/rmcfs/rJava/libs/rJava.so
  Reason: image not found
Error: package ‘rJava’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘rmcfs’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/rmcfs/new/rmcfs.Rcheck/rmcfs’

```
### CRAN

```
* installing *source* package ‘rmcfs’ ...
** package ‘rmcfs’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/rmcfs/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/rmcfs/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/rmcfs/rJava/libs/rJava.so
  Reason: image not found
Error: package ‘rJava’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘rmcfs’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/rmcfs/old/rmcfs.Rcheck/rmcfs’

```
# RnBeads

<details>

* Version: 2.2.0
* Source code: https://github.com/cran/RnBeads
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 226

Run `revdep_details(,"RnBeads")` for more info

</details>

## In both

*   checking whether package ‘RnBeads’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/RnBeads/new/RnBeads.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘IlluminaHumanMethylation450kmanifest’
    
    Depends: includes the non-default packages:
      'BiocGenerics', 'S4Vectors', 'GenomicRanges', 'MASS', 'cluster',
      'ff', 'fields', 'ggplot2', 'gplots', 'gridExtra', 'limma',
      'matrixStats', 'illuminaio', 'methylumi', 'plyr'
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    ```

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

## Installation

### Devel

```
* installing *source* package ‘RnBeads’ ...
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: package ‘FDb.InfiniumMethylation.hg19’ required by ‘methylumi’ could not be found
Execution halted
ERROR: lazy loading failed for package ‘RnBeads’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/RnBeads/new/RnBeads.Rcheck/RnBeads’

```
### CRAN

```
* installing *source* package ‘RnBeads’ ...
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: package ‘FDb.InfiniumMethylation.hg19’ required by ‘methylumi’ could not be found
Execution halted
ERROR: lazy loading failed for package ‘RnBeads’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/RnBeads/old/RnBeads.Rcheck/RnBeads’

```
# Roleswitch

<details>

* Version: 1.22.0
* Source code: https://github.com/cran/Roleswitch
* URL: http://www.cs.utoronto.ca/~yueli/roleswitch.html
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 71

Run `revdep_details(,"Roleswitch")` for more info

</details>

## In both

*   R CMD check timed out
    

*   checking package dependencies ... NOTE
    ```
    Depends: includes the non-default packages:
      'pracma', 'reshape', 'plotrix', 'microRNA', 'biomaRt', 'Biostrings',
      'Biobase', 'DBI'
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    ```

*   checking R code for possible problems ... NOTE
    ```
    diagnosticPlot: no visible global function definition for ‘par’
    diagnosticPlot: no visible global function definition for ‘axis’
    diagnosticPlot: no visible global function definition for ‘plot’
    getSeedMatrix: no visible global function definition for ‘data’
    getTranscriptIDwithLongest3UTR: no visible global function definition
      for ‘aggregate’
    roleswitch: no visible global function definition for ‘aggregate’
    Undefined global functions or variables:
      aggregate axis data par plot
    Consider adding
      importFrom("graphics", "axis", "par", "plot")
      importFrom("stats", "aggregate")
      importFrom("utils", "data")
    to your NAMESPACE file.
    ```

# rpanel

<details>

* Version: 1.1-4
* Source code: https://github.com/cran/rpanel
* Date/Publication: 2018-05-07 14:58:19 UTC
* Number of recursive dependencies: 75

Run `revdep_details(,"rpanel")` for more info

</details>

## In both

*   checking whether package ‘rpanel’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/rpanel/new/rpanel.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rpanel’ ...
** package ‘rpanel’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** demo
** inst
** byte-compile and prepare package for lazy loading
Warning message:
In fun(libname, pkgname) : couldn't connect to display ""
Error in structure(.External(.C_dotTcl, ...), class = "tclObj") : 
  [tcl] can't find package BWidget.

Error: unable to load R code in package ‘rpanel’
Execution halted
ERROR: lazy loading failed for package ‘rpanel’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/rpanel/new/rpanel.Rcheck/rpanel’

```
### CRAN

```
* installing *source* package ‘rpanel’ ...
** package ‘rpanel’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** demo
** inst
** byte-compile and prepare package for lazy loading
Warning message:
In fun(libname, pkgname) : couldn't connect to display ""
Error in structure(.External(.C_dotTcl, ...), class = "tclObj") : 
  [tcl] can't find package BWidget.

Error: unable to load R code in package ‘rpanel’
Execution halted
ERROR: lazy loading failed for package ‘rpanel’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/rpanel/old/rpanel.Rcheck/rpanel’

```
# rrd

<details>

* Version: 0.2.2
* Source code: https://github.com/cran/rrd
* URL: https://github.com/andrie/rrd/, https://andrie.github.io/rrd/
* BugReports: https://github.com/andrie/rrd/issues
* Date/Publication: 2019-07-05 17:10:05 UTC
* Number of recursive dependencies: 60

Run `revdep_details(,"rrd")` for more info

</details>

## In both

*   checking whether package ‘rrd’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/rrd/new/rrd.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rrd’ ...
** package ‘rrd’ successfully unpacked and MD5 sums checked
** using staged installation
Package librrd was not found in the pkg-config search path.
Perhaps you should add the directory containing `librrd.pc'
to the PKG_CONFIG_PATH environment variable
No package 'librrd' found
Package librrd was not found in the pkg-config search path.
Perhaps you should add the directory containing `librrd.pc'
to the PKG_CONFIG_PATH environment variable
No package 'librrd' found
Using PKG_CFLAGS=
Using PKG_LIBS=-lrrd
------------------------- ANTICONF ERROR ---------------------------
Configuration failed because librrd was not found. Try installing:
 * deb: librrd-dev (Debian, Ubuntu)
 * rpm: rrdtool-devel (Fedora, CentOS, RHEL)
 * csw: rrdtool (Solaris)
 * brew: rrdtool (OSX)
If librrd is already installed, check that 'pkg-config' is in your
PATH and PKG_CONFIG_PATH contains a librrd.pc file. If pkg-config
is unavailable you can set INCLUDE_DIR and LIB_DIR manually via:
R CMD INSTALL --configure-vars='INCLUDE_DIR=... LIB_DIR=...'
--------------------------------------------------------------------
ERROR: configuration failed for package ‘rrd’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/rrd/new/rrd.Rcheck/rrd’

```
### CRAN

```
* installing *source* package ‘rrd’ ...
** package ‘rrd’ successfully unpacked and MD5 sums checked
** using staged installation
Package librrd was not found in the pkg-config search path.
Perhaps you should add the directory containing `librrd.pc'
to the PKG_CONFIG_PATH environment variable
No package 'librrd' found
Package librrd was not found in the pkg-config search path.
Perhaps you should add the directory containing `librrd.pc'
to the PKG_CONFIG_PATH environment variable
No package 'librrd' found
Using PKG_CFLAGS=
Using PKG_LIBS=-lrrd
------------------------- ANTICONF ERROR ---------------------------
Configuration failed because librrd was not found. Try installing:
 * deb: librrd-dev (Debian, Ubuntu)
 * rpm: rrdtool-devel (Fedora, CentOS, RHEL)
 * csw: rrdtool (Solaris)
 * brew: rrdtool (OSX)
If librrd is already installed, check that 'pkg-config' is in your
PATH and PKG_CONFIG_PATH contains a librrd.pc file. If pkg-config
is unavailable you can set INCLUDE_DIR and LIB_DIR manually via:
R CMD INSTALL --configure-vars='INCLUDE_DIR=... LIB_DIR=...'
--------------------------------------------------------------------
ERROR: configuration failed for package ‘rrd’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/rrd/old/rrd.Rcheck/rrd’

```
# rrepast

<details>

* Version: 0.7.0
* Source code: https://github.com/cran/rrepast
* URL: https://github.com/antonio-pgarcia/rrepast
* BugReports: https://github.com/antonio-pgarcia/RRepast/issues
* Date/Publication: 2018-06-25 18:29:13 UTC
* Number of recursive dependencies: 48

Run `revdep_details(,"rrepast")` for more info

</details>

## In both

*   checking whether package ‘rrepast’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/rrepast/new/rrepast.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rrepast’ ...
** package ‘rrepast’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/rrepast/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/rrepast/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/rrepast/rJava/libs/rJava.so
  Reason: image not found
Error: package ‘rJava’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘rrepast’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/rrepast/new/rrepast.Rcheck/rrepast’

```
### CRAN

```
* installing *source* package ‘rrepast’ ...
** package ‘rrepast’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/rrepast/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/rrepast/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/rrepast/rJava/libs/rJava.so
  Reason: image not found
Error: package ‘rJava’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘rrepast’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/rrepast/old/rrepast.Rcheck/rrepast’

```
# RSCAT

<details>

* Version: 1.1.0
* Source code: https://github.com/cran/RSCAT
* BugReports: https://github.com/act-org/RSCAT/issues
* Date/Publication: 2020-01-17 10:00:02 UTC
* Number of recursive dependencies: 66

Run `revdep_details(,"RSCAT")` for more info

</details>

## In both

*   checking whether package ‘RSCAT’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/RSCAT/new/RSCAT.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘RSCAT’ ...
** package ‘RSCAT’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/RSCAT/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/RSCAT/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/RSCAT/rJava/libs/rJava.so
  Reason: image not found
Error: package ‘rJava’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘RSCAT’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/RSCAT/new/RSCAT.Rcheck/RSCAT’

```
### CRAN

```
* installing *source* package ‘RSCAT’ ...
** package ‘RSCAT’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/RSCAT/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/RSCAT/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/RSCAT/rJava/libs/rJava.so
  Reason: image not found
Error: package ‘rJava’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘RSCAT’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/RSCAT/old/RSCAT.Rcheck/RSCAT’

```
# rstanarm

<details>

* Version: 2.19.2
* Source code: https://github.com/cran/rstanarm
* URL: https://mc-stan.org/rstanarm/, https://discourse.mc-stan.org
* BugReports: https://github.com/stan-dev/rstanarm/issues
* Date/Publication: 2019-10-03 18:00:02 UTC
* Number of recursive dependencies: 122

Run `revdep_details(,"rstanarm")` for more info

</details>

## In both

*   R CMD check timed out
    

*   checking installed package size ... NOTE
    ```
      installed size is 17.5Mb
      sub-directories of 1Mb or more:
        R      2.0Mb
        doc    1.2Mb
        libs  13.7Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# rsvg

<details>

* Version: 1.3
* Source code: https://github.com/cran/rsvg
* URL: https://github.com/jeroen/rsvg#readme
* BugReports: https://github.com/jeroen/rsvg/issues
* Date/Publication: 2018-05-10 12:17:28 UTC
* Number of recursive dependencies: 55

Run `revdep_details(,"rsvg")` for more info

</details>

## In both

*   checking whether package ‘rsvg’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/rsvg/new/rsvg.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rsvg’ ...
** package ‘rsvg’ successfully unpacked and MD5 sums checked
** using staged installation
Package librsvg-2.0 was not found in the pkg-config search path.
Perhaps you should add the directory containing `librsvg-2.0.pc'
to the PKG_CONFIG_PATH environment variable
No package 'librsvg-2.0' found
Homebrew 2.2.2
Homebrew/homebrew-core (git revision e932e; last commit 2020-01-03)
Homebrew/homebrew-cask (git revision 2816e; last commit 2020-01-03)
Using PKG_CFLAGS=-I/usr/local/opt/librsvg/include
Using PKG_LIBS=-L/usr/local/opt/librsvg/lib -lrsvg
------------------------- ANTICONF ERROR ---------------------------
Configuration failed because librsvg-2.0 was not found. Try installing:
 * deb: librsvg2-dev (Debian, Ubuntu, etc)
 * rpm: librsvg2-devel (Fedora, EPEL)
 * csw: librsvg_dev, sunx11_devel (Solaris)
 * brew: librsvg (OSX)
If librsvg-2.0 is already installed, check that 'pkg-config' is in your
PATH and PKG_CONFIG_PATH contains a librsvg-2.0.pc file. If pkg-config
is unavailable you can set INCLUDE_DIR and LIB_DIR manually via:
R CMD INSTALL --configure-vars='INCLUDE_DIR=... LIB_DIR=...'
--------------------------------------------------------------------
ERROR: configuration failed for package ‘rsvg’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/rsvg/new/rsvg.Rcheck/rsvg’

```
### CRAN

```
* installing *source* package ‘rsvg’ ...
** package ‘rsvg’ successfully unpacked and MD5 sums checked
** using staged installation
Package librsvg-2.0 was not found in the pkg-config search path.
Perhaps you should add the directory containing `librsvg-2.0.pc'
to the PKG_CONFIG_PATH environment variable
No package 'librsvg-2.0' found
Homebrew 2.2.2
Homebrew/homebrew-core (git revision e932e; last commit 2020-01-03)
Homebrew/homebrew-cask (git revision 2816e; last commit 2020-01-03)
Using PKG_CFLAGS=-I/usr/local/opt/librsvg/include
Using PKG_LIBS=-L/usr/local/opt/librsvg/lib -lrsvg
------------------------- ANTICONF ERROR ---------------------------
Configuration failed because librsvg-2.0 was not found. Try installing:
 * deb: librsvg2-dev (Debian, Ubuntu, etc)
 * rpm: librsvg2-devel (Fedora, EPEL)
 * csw: librsvg_dev, sunx11_devel (Solaris)
 * brew: librsvg (OSX)
If librsvg-2.0 is already installed, check that 'pkg-config' is in your
PATH and PKG_CONFIG_PATH contains a librsvg-2.0.pc file. If pkg-config
is unavailable you can set INCLUDE_DIR and LIB_DIR manually via:
R CMD INSTALL --configure-vars='INCLUDE_DIR=... LIB_DIR=...'
--------------------------------------------------------------------
ERROR: configuration failed for package ‘rsvg’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/rsvg/old/rsvg.Rcheck/rsvg’

```
# RtutoR

<details>

* Version: 1.2
* Source code: https://github.com/cran/RtutoR
* Date/Publication: 2018-09-14 07:50:07 UTC
* Number of recursive dependencies: 121

Run `revdep_details(,"RtutoR")` for more info

</details>

## In both

*   checking whether package ‘RtutoR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/RtutoR/new/RtutoR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘RtutoR’ ...
** package ‘RtutoR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/RtutoR/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/RtutoR/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/RtutoR/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘RtutoR’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/RtutoR/new/RtutoR.Rcheck/RtutoR’

```
### CRAN

```
* installing *source* package ‘RtutoR’ ...
** package ‘RtutoR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/RtutoR/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/RtutoR/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/RtutoR/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘RtutoR’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/RtutoR/old/RtutoR.Rcheck/RtutoR’

```
# RxODE

<details>

* Version: 0.9.1-9
* Source code: https://github.com/cran/RxODE
* URL: https://nlmixrdevelopment.github.io/RxODE/
* BugReports: https://github.com/nlmixrdevelopment/RxODE/issues
* Date/Publication: 2020-01-10 23:20:06 UTC
* Number of recursive dependencies: 121

Run `revdep_details(,"RxODE")` for more info

</details>

## In both

*   checking whether package ‘RxODE’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/RxODE/new/RxODE.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘installr’
    ```

## Installation

### Devel

```
* installing *source* package ‘RxODE’ ...
** package ‘RxODE’ successfully unpacked and MD5 sums checked
** using staged installation
checking for gcc... /usr/local/clang8/bin/clang
checking whether the C compiler works... yes
checking for C compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... configure: error: in `/Users/max/github/forks/ggplot2/revdep/checks.noindex/RxODE/new/RxODE.Rcheck/00_pkg_src/RxODE':
configure: error: cannot run C compiled programs.
If you meant to cross compile, use `--host'.
See `config.log' for more details
ERROR: configuration failed for package ‘RxODE’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/RxODE/new/RxODE.Rcheck/RxODE’

```
### CRAN

```
* installing *source* package ‘RxODE’ ...
** package ‘RxODE’ successfully unpacked and MD5 sums checked
** using staged installation
checking for gcc... /usr/local/clang8/bin/clang
checking whether the C compiler works... yes
checking for C compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... configure: error: in `/Users/max/github/forks/ggplot2/revdep/checks.noindex/RxODE/old/RxODE.Rcheck/00_pkg_src/RxODE':
configure: error: cannot run C compiled programs.
If you meant to cross compile, use `--host'.
See `config.log' for more details
ERROR: configuration failed for package ‘RxODE’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/RxODE/old/RxODE.Rcheck/RxODE’

```
# scPipe

<details>

* Version: 1.6.0
* Source code: https://github.com/cran/scPipe
* URL: https://github.com/LuyiTian/scPipe
* BugReports: https://github.com/LuyiTian/scPipe
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 117

Run `revdep_details(,"scPipe")` for more info

</details>

## In both

*   R CMD check timed out
    

*   checking whether package ‘scPipe’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      trimbarcode.cpp:92:5: warning: ignoring return value of function declared with 'warn_unused_result' attribute [-Wunused-result]
      transcriptmapping.cpp:756:5: warning: ignoring return value of function declared with 'warn_unused_result' attribute [-Wunused-result]
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/scPipe/new/scPipe.Rcheck/00install.out’ for details.
    ```

*   checking whether the namespace can be loaded with stated dependencies ... WARNING
    ```
    Error in dyn.load(file, DLLpath = DLLpath, ...) : 
      unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/scPipe/Rhtslib/libs/Rhtslib.so':
      dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/scPipe/Rhtslib/libs/Rhtslib.so, 6): Symbol not found: _lzma_easy_buffer_encode
      Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/scPipe/Rhtslib/libs/Rhtslib.so
      Expected in: flat namespace
     in /Users/max/github/forks/ggplot2/revdep/library.noindex/scPipe/Rhtslib/libs/Rhtslib.so
    Calls: <Anonymous> ... namespaceImport -> loadNamespace -> library.dynam -> dyn.load
    Execution halted
    
    A namespace must be able to be loaded with just the base namespace
    loaded: otherwise if the namespace gets loaded by a saved object, the
    session will be unable to start.
    
    Probably some imports need to be declared in the NAMESPACE file.
    ```

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .BBSoptions
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    anno_to_saf: no visible binding for global variable ‘GeneID’
    infer_gene_id_from_parent : <local>: no visible binding for global
      variable ‘transcript_id’
    infer_gene_id_from_parent : <local>: no visible binding for global
      variable ‘Parent’
    infer_gene_id_from_parent: no visible binding for global variable
      ‘type’
    infer_gene_id_from_parent: no visible binding for global variable
      ‘Parent’
    infer_gene_id_from_parent: no visible binding for global variable
      ‘gene_id’
    plot_demultiplex: no visible binding for global variable ‘status’
    plot_demultiplex: no visible binding for global variable ‘count’
    plot_demultiplex: no visible binding for global variable ‘label_y’
    plot_demultiplex: no visible binding for global variable ‘label_tx’
    Undefined global functions or variables:
      GeneID Parent count end gene_id label_tx label_y seqnames start
      status strand transcript_id type
    Consider adding
      importFrom("stats", "end", "start")
    to your NAMESPACE file.
    ```

# SeqFeatR

<details>

* Version: 0.3.1
* Source code: https://github.com/cran/SeqFeatR
* Date/Publication: 2019-04-12 12:02:37 UTC
* Number of recursive dependencies: 60

Run `revdep_details(,"SeqFeatR")` for more info

</details>

## In both

*   checking whether package ‘SeqFeatR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/SeqFeatR/new/SeqFeatR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘SeqFeatR’ ...
** package ‘SeqFeatR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Warning message:
In fun(libname, pkgname) : couldn't connect to display ""
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/SeqFeatR/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/SeqFeatR/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/SeqFeatR/rjags/libs/rjags.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘SeqFeatR’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/SeqFeatR/new/SeqFeatR.Rcheck/SeqFeatR’

```
### CRAN

```
* installing *source* package ‘SeqFeatR’ ...
** package ‘SeqFeatR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Warning message:
In fun(libname, pkgname) : couldn't connect to display ""
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/SeqFeatR/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/SeqFeatR/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/SeqFeatR/rjags/libs/rjags.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘SeqFeatR’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/SeqFeatR/old/SeqFeatR.Rcheck/SeqFeatR’

```
# sf

<details>

* Version: 0.8-1
* Source code: https://github.com/cran/sf
* URL: https://github.com/r-spatial/sf/, https://r-spatial.github.io/sf/
* BugReports: https://github.com/r-spatial/sf/issues/
* Date/Publication: 2020-01-28 11:20:07 UTC
* Number of recursive dependencies: 137

Run `revdep_details(,"sf")` for more info

</details>

## In both

*   checking whether package ‘sf’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/sf/new/sf.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘sf’ ...
** package ‘sf’ successfully unpacked and MD5 sums checked
** using staged installation
configure: CC: /usr/local/clang8/bin/clang
configure: CXX: /usr/local/clang8/bin/clang++ -std=gnu++11
checking for gdal-config... /usr/local/bin/gdal-config
checking gdal-config usability... yes
configure: GDAL: 2.4.1
checking GDAL version >= 2.0.1... yes
checking for gcc... /usr/local/clang8/bin/clang
checking whether the C compiler works... yes
checking for C compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether we are using the GNU C compiler... yes
checking whether /usr/local/clang8/bin/clang accepts -g... yes
checking for /usr/local/clang8/bin/clang option to accept ISO C89... none needed
checking how to run the C preprocessor... /usr/local/clang8/bin/clang -E
checking for grep that handles long lines and -e... /usr/bin/grep
checking for egrep... /usr/bin/grep -E
checking for ANSI C header files... rm: conftest.dSYM: is a directory
rm: conftest.dSYM: is a directory
yes
checking for sys/types.h... yes
checking for sys/stat.h... yes
checking for stdlib.h... yes
checking for string.h... yes
checking for memory.h... yes
checking for strings.h... yes
checking for inttypes.h... yes
checking for stdint.h... yes
checking for unistd.h... yes
checking gdal.h usability... yes
checking gdal.h presence... yes
checking for gdal.h... yes
checking GDAL: linking with --libs only... yes
checking GDAL: /usr/local/Cellar/gdal/2.4.1/share/gdal/pcs.csv readable... yes
checking GDAL: checking whether PROJ is available for linking:... yes
checking GDAL: checking whether PROJ is available fur running:... dyld: Library not loaded: /usr/local/opt/openssl/lib/libcrypto.1.0.0.dylib
  Referenced from: /usr/local/opt/gdal/lib/libgdal.20.dylib
  Reason: image not found
./configure: line 3625: 66275 Abort trap: 6           ./gdal_proj
no
configure: error: OGRCoordinateTransformation() does not return a coord.trans: PROJ not available?
ERROR: configuration failed for package ‘sf’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/sf/new/sf.Rcheck/sf’

```
### CRAN

```
* installing *source* package ‘sf’ ...
** package ‘sf’ successfully unpacked and MD5 sums checked
** using staged installation
configure: CC: /usr/local/clang8/bin/clang
configure: CXX: /usr/local/clang8/bin/clang++ -std=gnu++11
checking for gdal-config... /usr/local/bin/gdal-config
checking gdal-config usability... yes
configure: GDAL: 2.4.1
checking GDAL version >= 2.0.1... yes
checking for gcc... /usr/local/clang8/bin/clang
checking whether the C compiler works... yes
checking for C compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether we are using the GNU C compiler... yes
checking whether /usr/local/clang8/bin/clang accepts -g... yes
checking for /usr/local/clang8/bin/clang option to accept ISO C89... none needed
checking how to run the C preprocessor... /usr/local/clang8/bin/clang -E
checking for grep that handles long lines and -e... /usr/bin/grep
checking for egrep... /usr/bin/grep -E
checking for ANSI C header files... rm: conftest.dSYM: is a directory
rm: conftest.dSYM: is a directory
yes
checking for sys/types.h... yes
checking for sys/stat.h... yes
checking for stdlib.h... yes
checking for string.h... yes
checking for memory.h... yes
checking for strings.h... yes
checking for inttypes.h... yes
checking for stdint.h... yes
checking for unistd.h... yes
checking gdal.h usability... yes
checking gdal.h presence... yes
checking for gdal.h... yes
checking GDAL: linking with --libs only... yes
checking GDAL: /usr/local/Cellar/gdal/2.4.1/share/gdal/pcs.csv readable... yes
checking GDAL: checking whether PROJ is available for linking:... yes
checking GDAL: checking whether PROJ is available fur running:... dyld: Library not loaded: /usr/local/opt/openssl/lib/libcrypto.1.0.0.dylib
  Referenced from: /usr/local/opt/gdal/lib/libgdal.20.dylib
  Reason: image not found
./configure: line 3625: 65953 Abort trap: 6           ./gdal_proj
no
configure: error: OGRCoordinateTransformation() does not return a coord.trans: PROJ not available?
ERROR: configuration failed for package ‘sf’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/sf/old/sf.Rcheck/sf’

```
# simmr

<details>

* Version: 0.4.1
* Source code: https://github.com/cran/simmr
* Date/Publication: 2019-07-03 18:20:03 UTC
* Number of recursive dependencies: 76

Run `revdep_details(,"simmr")` for more info

</details>

## In both

*   checking whether package ‘simmr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/simmr/new/simmr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘simmr’ ...
** package ‘simmr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rjags’:
 .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/simmr/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/simmr/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/simmr/rjags/libs/rjags.so
  Reason: image not found
Error: package ‘rjags’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘simmr’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/simmr/new/simmr.Rcheck/simmr’

```
### CRAN

```
* installing *source* package ‘simmr’ ...
** package ‘simmr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rjags’:
 .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/simmr/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/simmr/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/simmr/rjags/libs/rjags.so
  Reason: image not found
Error: package ‘rjags’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘simmr’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/simmr/old/simmr.Rcheck/simmr’

```
# smartR

<details>

* Version: 0.62.0
* Source code: https://github.com/cran/smartR
* Date/Publication: 2018-11-30 23:40:03 UTC
* Number of recursive dependencies: 134

Run `revdep_details(,"smartR")` for more info

</details>

## In both

*   checking whether package ‘smartR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/smartR/new/smartR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘smartR’ ...
** package ‘smartR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rjags’:
 .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/smartR/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/smartR/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/smartR/rjags/libs/rjags.so
  Reason: image not found
Error: package ‘rjags’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘smartR’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/smartR/new/smartR.Rcheck/smartR’

```
### CRAN

```
* installing *source* package ‘smartR’ ...
** package ‘smartR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rjags’:
 .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/smartR/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/smartR/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/smartR/rjags/libs/rjags.so
  Reason: image not found
Error: package ‘rjags’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘smartR’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/smartR/old/smartR.Rcheck/smartR’

```
# SMITE

<details>

* Version: 
* Source code: ???
* URL: http://ggplot2.tidyverse.org, https://github.com/tidyverse/ggplot2
* BugReports: https://github.com/tidyverse/ggplot2/issues
* Number of recursive dependencies: 0

Run `revdep_details(,"")` for more info

</details>

## Error before installation

### Devel

```

  There are binary versions available but the source versions are later:
           binary  source needs_compilation
checkmate   1.9.4   2.0.0              TRUE
nlme      3.1-143 3.1-144              TRUE

  Binaries will be installed


Warning: unable to access index for repository https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6:
  cannot open URL 'https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6/PACKAGES'
Warning: unable to access index for repository https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6:
  cannot open URL 'https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6/PACKAGES'
installing the source package ‘reactome.db’



```
### CRAN

```

  There are binary versions available but the source versions are later:
           binary  source needs_compilation
checkmate   1.9.4   2.0.0              TRUE
nlme      3.1-143 3.1-144              TRUE

  Binaries will be installed


Warning: unable to access index for repository https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6:
  cannot open URL 'https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6/PACKAGES'
Warning: unable to access index for repository https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6:
  cannot open URL 'https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6/PACKAGES'
installing the source package ‘reactome.db’



```
# SNPhood

<details>

* Version: 1.14.0
* Source code: https://github.com/cran/SNPhood
* URL: https://bioconductor.org/packages/SNPhood
* BugReports: christian.arnold@embl.de
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 129

Run `revdep_details(,"SNPhood")` for more info

</details>

## In both

*   R CMD check timed out
    

*   checking installed package size ... NOTE
    ```
      installed size is 10.9Mb
      sub-directories of 1Mb or more:
        data   3.8Mb
        doc    6.4Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    BugReports field is not a suitable URL but appears to contain an email address
      not specified by mailto: nor contained in < >
    ```

*   checking R code for possible problems ... NOTE
    ```
    .calcBinomTestVector: no visible binding for global variable ‘pp’
    Undefined global functions or variables:
      pp
    ```

# spcosa

<details>

* Version: 0.3-9
* Source code: https://github.com/cran/spcosa
* Date/Publication: 2020-01-13 14:00:02 UTC
* Number of recursive dependencies: 75

Run `revdep_details(,"spcosa")` for more info

</details>

## In both

*   checking whether package ‘spcosa’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/spcosa/new/spcosa.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘spcosa’ ...
** package ‘spcosa’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** demo
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/spcosa/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/spcosa/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/spcosa/rJava/libs/rJava.so
  Reason: image not found
Error: package ‘rJava’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘spcosa’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/spcosa/new/spcosa.Rcheck/spcosa’

```
### CRAN

```
* installing *source* package ‘spcosa’ ...
** package ‘spcosa’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** demo
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/spcosa/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/spcosa/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/spcosa/rJava/libs/rJava.so
  Reason: image not found
Error: package ‘rJava’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘spcosa’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/spcosa/old/spcosa.Rcheck/spcosa’

```
# stpp

<details>

* Version: 2.0-3
* Source code: https://github.com/cran/stpp
* BugReports: https://github.com/stpp-GitHub-community
* Date/Publication: 2018-02-14 19:02:33 UTC
* Number of recursive dependencies: 83

Run `revdep_details(,"stpp")` for more info

</details>

## In both

*   checking whether package ‘stpp’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/stpp/new/stpp.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘stpp’ ...
** package ‘stpp’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
gfortran  -fPIC  -Wall -g -O2  -c astk.f -o astk.o
gfortran  -fPIC  -Wall -g -O2  -c circ.f -o circ.o
gfortran  -fPIC  -Wall -g -O2  -c covst.f -o covst.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c init.c -o init.o
circ.f:331:0:

       if(p2.eq.1) res = psit**(-p6) * stable(x/psit,p1)
 
Warning: ‘psit’ may be used uninitialized in this function [-Wmaybe-uninitialized]
gfortran  -fPIC  -Wall -g -O2  -c listafunction.f -o listafunction.o
gfortran  -fPIC  -Wall -g -O2  -c pcffunction.f -o pcffunction.o
gfortran  -fPIC  -Wall -g -O2  -c stikfunction.f -o stikfunction.o
/usr/local/clang8/bin/clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/clang8/lib -o stpp.so astk.o circ.o covst.o init.o listafunction.o pcffunction.o stikfunction.o -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/max/github/forks/ggplot2/revdep/checks.noindex/stpp/new/stpp.Rcheck/00LOCK-stpp/00new/stpp/libs
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error in structure(.External(.C_dotTcl, ...), class = "tclObj") : 
  [tcl] can't find package BWidget.

In addition: Warning message:
In fun(libname, pkgname) : couldn't connect to display ""
Error: package or namespace load failed for ‘rpanel’:
 unable to load R code in package ‘rpanel’
Error: package ‘rpanel’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘stpp’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/stpp/new/stpp.Rcheck/stpp’

```
### CRAN

```
* installing *source* package ‘stpp’ ...
** package ‘stpp’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
gfortran  -fPIC  -Wall -g -O2  -c astk.f -o astk.o
gfortran  -fPIC  -Wall -g -O2  -c circ.f -o circ.o
gfortran  -fPIC  -Wall -g -O2  -c covst.f -o covst.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c init.c -o init.o
circ.f:331:0:

       if(p2.eq.1) res = psit**(-p6) * stable(x/psit,p1)
 
Warning: ‘psit’ may be used uninitialized in this function [-Wmaybe-uninitialized]
gfortran  -fPIC  -Wall -g -O2  -c listafunction.f -o listafunction.o
gfortran  -fPIC  -Wall -g -O2  -c pcffunction.f -o pcffunction.o
gfortran  -fPIC  -Wall -g -O2  -c stikfunction.f -o stikfunction.o
/usr/local/clang8/bin/clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/clang8/lib -o stpp.so astk.o circ.o covst.o init.o listafunction.o pcffunction.o stikfunction.o -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/max/github/forks/ggplot2/revdep/checks.noindex/stpp/old/stpp.Rcheck/00LOCK-stpp/00new/stpp/libs
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error in structure(.External(.C_dotTcl, ...), class = "tclObj") : 
  [tcl] can't find package BWidget.

In addition: Warning message:
In fun(libname, pkgname) : couldn't connect to display ""
Error: package or namespace load failed for ‘rpanel’:
 unable to load R code in package ‘rpanel’
Error: package ‘rpanel’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘stpp’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/stpp/old/stpp.Rcheck/stpp’

```
# TCGAbiolinks

<details>

* Version: 2.12.6
* Source code: https://github.com/cran/TCGAbiolinks
* URL: https://github.com/BioinformaticsFMRP/TCGAbiolinks
* BugReports: https://github.com/BioinformaticsFMRP/TCGAbiolinks/issues
* Date/Publication: 2019-09-05
* Number of recursive dependencies: 265

Run `revdep_details(,"TCGAbiolinks")` for more info

</details>

## In both

*   R CMD check timed out
    

*   checking installed package size ... NOTE
    ```
      installed size is 96.0Mb
      sub-directories of 1Mb or more:
        R      1.7Mb
        data   3.9Mb
        doc   90.1Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    There are ::: calls to the package's namespace in its code. A package
      almost never needs to use ::: for its own objects:
      ‘move’
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘minet’
    TCGAquery_recount2: no visible binding for global variable ‘rse_gene’
    TCGAtumor_purity: no visible binding for global variable ‘Tumor.purity’
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘dNetInduce’
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘dNetPipeline’
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘dCommSignif’
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘visNet’
    TCGAvisualize_oncoprint: no visible binding for global variable ‘value’
    readExonQuantification: no visible binding for global variable ‘exon’
    readExonQuantification: no visible binding for global variable
      ‘coordinates’
    readIDATDNAmethylation: no visible global function definition for
      ‘openSesame’
    Undefined global functions or variables:
      Tumor.purity barcode c3net clinical coordinates dCommSignif
      dNetInduce dNetPipeline exon knnmi.cross limmacontrasts.fit
      limmamakeContrasts minet openSesame portions rse_gene value visNet
    ```

# TeachingDemos

<details>

* Version: 2.10
* Source code: https://github.com/cran/TeachingDemos
* Date/Publication: 2016-02-12 07:40:49
* Number of recursive dependencies: 74

Run `revdep_details(,"TeachingDemos")` for more info

</details>

## In both

*   R CMD check timed out
    

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘R2wd’
    ```

# trialr

<details>

* Version: 0.1.3
* Source code: https://github.com/cran/trialr
* URL: https://github.com/brockk/trialr
* BugReports: https://github.com/brockk/trialr/issues
* Date/Publication: 2020-01-08 22:30:10 UTC
* Number of recursive dependencies: 102

Run `revdep_details(,"trialr")` for more info

</details>

## In both

*   checking whether package ‘trialr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/trialr/new/trialr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘trialr’ ...
** package ‘trialr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/AugBin2T1A.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/BebopInPeps2.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/CrmEmpiricNormalPrior.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/CrmOneParamLogisticGammaPrior.stan
Wrote C++ file "stan_files/CrmEmpiricNormalPrior.cc"
Wrote C++ file "stan_files/CrmOneParamLogisticGammaPrior.cc"
Wrote C++ file "stan_files/AugBin2T1A.cc"
Wrote C++ file "stan_files/BebopInPeps2.cc"
Error in readRDS("/var/folders/lb/xhxqmcrd7gv302_b1pdfykh80000gn/T//Rtmp45Qwiv/file9f1c2321b36e") : 
  error reading from connection
Calls: .Last -> readRDS
Execution halted
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/CrmOneParamLogisticNormalPrior.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/CrmTwoParamLogisticNormalPrior.stan
make: *** [stan_files/BebopInPeps2.cc] Error 1
make: *** Waiting for unfinished jobs....
Wrote C++ file "stan_files/CrmOneParamLogisticNormalPrior.cc"
Wrote C++ file "stan_files/CrmTwoParamLogisticNormalPrior.cc"
rm stan_files/BebopInPeps2.cc stan_files/CrmEmpiricNormalPrior.cc stan_files/CrmTwoParamLogisticNormalPrior.cc stan_files/CrmOneParamLogisticNormalPrior.cc stan_files/CrmOneParamLogisticGammaPrior.cc stan_files/AugBin2T1A.cc
ERROR: compilation failed for package ‘trialr’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/trialr/new/trialr.Rcheck/trialr’

```
### CRAN

```
* installing *source* package ‘trialr’ ...
** package ‘trialr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/AugBin2T1A.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/BebopInPeps2.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/CrmEmpiricNormalPrior.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/CrmOneParamLogisticGammaPrior.stan
Wrote C++ file "stan_files/CrmEmpiricNormalPrior.cc"
Wrote C++ file "stan_files/CrmOneParamLogisticGammaPrior.cc"
Wrote C++ file "stan_files/AugBin2T1A.cc"
Wrote C++ file "stan_files/BebopInPeps2.cc"
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/CrmOneParamLogisticNormalPrior.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/CrmTwoParamLogisticNormalPrior.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/EffTox.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/ThallHierarchicalBinary.stan


Wrote C++ file "stan_files/EffTox.cc"
Wrote C++ file "stan_files/CrmTwoParamLogisticNormalPrior.cc"
Wrote C++ file "stan_files/CrmOneParamLogisticNormalPrior.cc"
Wrote C++ file "stan_files/ThallHierarchicalBinary.cc"
/usr/local/clang8/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c init.cpp -o init.o
Error in readRDS("/var/folders/lb/xhxqmcrd7gv302_b1pdfykh80000gn/T//Rtmp45Qwiv/file9f1c692ed645") : 
  error reading from connection
Calls: .Last -> readRDS
Execution halted
make: *** [stan_files/CrmTwoParamLogisticNormalPrior.cc] Error 1
make: *** Waiting for unfinished jobs....
rm stan_files/EffTox.cc stan_files/BebopInPeps2.cc stan_files/CrmEmpiricNormalPrior.cc stan_files/ThallHierarchicalBinary.cc stan_files/CrmTwoParamLogisticNormalPrior.cc stan_files/CrmOneParamLogisticNormalPrior.cc stan_files/CrmOneParamLogisticGammaPrior.cc stan_files/AugBin2T1A.cc
ERROR: compilation failed for package ‘trialr’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/trialr/old/trialr.Rcheck/trialr’

```
# vortexR

<details>

* Version: 1.1.6
* Source code: https://github.com/cran/vortexR
* URL: https://github.com/carlopacioni/vortexR/
* BugReports: https://github.com/carlopacioni/vortexR/issues
* Date/Publication: 2019-02-06 12:50:03 UTC
* Number of recursive dependencies: 122

Run `revdep_details(,"vortexR")` for more info

</details>

## In both

*   checking whether package ‘vortexR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/vortexR/new/vortexR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘vortexR’ ...
** package ‘vortexR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/vortexR/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/vortexR/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/vortexR/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘vortexR’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/vortexR/new/vortexR.Rcheck/vortexR’

```
### CRAN

```
* installing *source* package ‘vortexR’ ...
** package ‘vortexR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/vortexR/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/vortexR/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/vortexR/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘vortexR’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/vortexR/old/vortexR.Rcheck/vortexR’

```
# WaveSampling

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/WaveSampling
* URL: https://github.com/RJauslin/WaveSampling
* BugReports: https://github.com/RJauslin/WaveSampling/issues
* Date/Publication: 2020-01-30 12:00:09 UTC
* Number of recursive dependencies: 66

Run `revdep_details(,"WaveSampling")` for more info

</details>

## In both

*   checking whether package ‘WaveSampling’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/WaveSampling/new/WaveSampling.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘WaveSampling’ ...
** package ‘WaveSampling’ successfully unpacked and MD5 sums checked
** using staged installation
checking whether the C++ compiler works... yes
checking for C++ compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... configure: error: in `/Users/max/github/forks/ggplot2/revdep/checks.noindex/WaveSampling/new/WaveSampling.Rcheck/00_pkg_src/WaveSampling':
configure: error: cannot run C++ compiled programs.
If you meant to cross compile, use `--host'.
See `config.log' for more details
ERROR: configuration failed for package ‘WaveSampling’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/WaveSampling/new/WaveSampling.Rcheck/WaveSampling’

```
### CRAN

```
* installing *source* package ‘WaveSampling’ ...
** package ‘WaveSampling’ successfully unpacked and MD5 sums checked
** using staged installation
checking whether the C++ compiler works... yes
checking for C++ compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... configure: error: in `/Users/max/github/forks/ggplot2/revdep/checks.noindex/WaveSampling/old/WaveSampling.Rcheck/00_pkg_src/WaveSampling':
configure: error: cannot run C++ compiled programs.
If you meant to cross compile, use `--host'.
See `config.log' for more details
ERROR: configuration failed for package ‘WaveSampling’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/WaveSampling/old/WaveSampling.Rcheck/WaveSampling’

```
# XBSeq

<details>

* Version: 1.16.0
* Source code: https://github.com/cran/XBSeq
* URL: https://github.com/Liuy12/XBSeq
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 129

Run `revdep_details(,"XBSeq")` for more info

</details>

## In both

*   R CMD check timed out
    

*   checking whether the namespace can be loaded with stated dependencies ... NOTE
    ```
    Warning: no function found corresponding to methods exports from ‘XBSeq’ for: ‘conditions’, ‘conditions<-’, ‘dispTable’
    
    A namespace must be able to be loaded with just the base namespace
    loaded: otherwise if the namespace gets loaded by a saved object, the
    session will be unable to start.
    
    Probably some imports need to be declared in the NAMESPACE file.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    estimateRealCount,XBSeqDataSet: no visible global function definition
      for ‘assay’
    estimateRealCount,XBSeqDataSet: no visible global function definition
      for ‘assay<-’
    estimateSCV,XBSeqDataSet: no visible global function definition for
      ‘conditions’
    estimateSCV,XBSeqDataSet: no visible global function definition for
      ‘dispTable<-’
    Undefined global functions or variables:
      ..count.. DataFrame Gamma Group Sample SummarizedExperiment assay
      assay<- assays baseMean coefficients complete.cases conditions cor
      data ddelap dispTable dispTable<- dnbinom dpois formula glm
      log2FoldChange median optim p.adjust pbeta predict qbeta quantile
      rnbinom scvBiasCorrectionFits
    Consider adding
      importFrom("stats", "Gamma", "coefficients", "complete.cases", "cor",
                 "dnbinom", "dpois", "formula", "glm", "median", "optim",
                 "p.adjust", "pbeta", "predict", "qbeta", "quantile",
                 "rnbinom")
      importFrom("utils", "data")
    to your NAMESPACE file.
    ```

# XLConnect

<details>

* Version: 0.2-15
* Source code: https://github.com/cran/XLConnect
* URL: http://www.mirai-solutions.com https://github.com/miraisolutions/xlconnect
* BugReports: https://github.com/miraisolutions/xlconnect/issues
* Date/Publication: 2018-04-05 17:20:46 UTC
* Number of recursive dependencies: 41

Run `revdep_details(,"XLConnect")` for more info

</details>

## In both

*   checking whether package ‘XLConnect’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/XLConnect/new/XLConnect.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘XLConnect’ ...
** package ‘XLConnect’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** demo
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘XLConnectJars’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/XLConnect/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/XLConnect/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/XLConnect/rJava/libs/rJava.so
  Reason: image not found
Error: package ‘XLConnectJars’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘XLConnect’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/XLConnect/new/XLConnect.Rcheck/XLConnect’

```
### CRAN

```
* installing *source* package ‘XLConnect’ ...
** package ‘XLConnect’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** demo
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘XLConnectJars’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/XLConnect/rJava/libs/rJava.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/XLConnect/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/XLConnect/rJava/libs/rJava.so
  Reason: image not found
Error: package ‘XLConnectJars’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘XLConnect’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/XLConnect/old/XLConnect.Rcheck/XLConnect’

```
# zenplots

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/zenplots
* URL: https://github.com/great-northern-diver/zenplots
* Date/Publication: 2019-08-01 16:40:02 UTC
* Number of recursive dependencies: 105

Run `revdep_details(,"zenplots")` for more info

</details>

## In both

*   checking whether package ‘zenplots’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/zenplots/new/zenplots.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘zenplots’ ...
** package ‘zenplots’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c col_split.c -o col_split.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c init.c -o init.o
/usr/local/clang8/bin/clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/clang8/lib -o zenplots.so col_split.o init.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/max/github/forks/ggplot2/revdep/checks.noindex/zenplots/new/zenplots.Rcheck/00LOCK-zenplots/00new/zenplots/libs
** R
** data
** demo
** inst
** byte-compile and prepare package for lazy loading
Warning in fun(libname, pkgname) : couldn't connect to display ""
Error: .onLoad failed in loadNamespace() for 'loon', details:
  call: structure(.External(.C_dotTcl, ...), class = "tclObj")
  error: [tcl] couldn't connect to display "".
Execution halted
ERROR: lazy loading failed for package ‘zenplots’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/zenplots/new/zenplots.Rcheck/zenplots’

```
### CRAN

```
* installing *source* package ‘zenplots’ ...
** package ‘zenplots’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c col_split.c -o col_split.o
/usr/local/clang8/bin/clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c init.c -o init.o
/usr/local/clang8/bin/clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/clang8/lib -o zenplots.so col_split.o init.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/max/github/forks/ggplot2/revdep/checks.noindex/zenplots/old/zenplots.Rcheck/00LOCK-zenplots/00new/zenplots/libs
** R
** data
** demo
** inst
** byte-compile and prepare package for lazy loading
Warning in fun(libname, pkgname) : couldn't connect to display ""
Error: .onLoad failed in loadNamespace() for 'loon', details:
  call: structure(.External(.C_dotTcl, ...), class = "tclObj")
  error: [tcl] couldn't connect to display "".
Execution halted
ERROR: lazy loading failed for package ‘zenplots’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/zenplots/old/zenplots.Rcheck/zenplots’

```
# zooaRchGUI

<details>

* Version: 1.0.2
* Source code: https://github.com/cran/zooaRchGUI
* URL: http://www.zooarchgui.org/, https://zooarchgui.github.io/zooaRchGUI/
* BugReports: https://github.com/zooaRchGUI/zooaRchGUI/issues
* Date/Publication: 2017-06-15 15:09:03 UTC
* Number of recursive dependencies: 131

Run `revdep_details(,"zooaRchGUI")` for more info

</details>

## In both

*   checking whether package ‘zooaRchGUI’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/zooaRchGUI/new/zooaRchGUI.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘zooaRchGUI’ ...
** package ‘zooaRchGUI’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/zooaRchGUI/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/zooaRchGUI/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/zooaRchGUI/rjags/libs/rjags.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘zooaRchGUI’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/zooaRchGUI/new/zooaRchGUI.Rcheck/zooaRchGUI’

```
### CRAN

```
* installing *source* package ‘zooaRchGUI’ ...
** package ‘zooaRchGUI’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/zooaRchGUI/rjags/libs/rjags.so':
  dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/zooaRchGUI/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/zooaRchGUI/rjags/libs/rjags.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘zooaRchGUI’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/zooaRchGUI/old/zooaRchGUI.Rcheck/zooaRchGUI’

```
