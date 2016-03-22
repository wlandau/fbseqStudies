# fbseqStudies

This package reproduces the results of some journal articles that use the `fbseq` and `fbseqCUDA` packages. 

# Installation

## Option 1: install a stable release (recommended).

Navigate to a [list of stable releases](https://github.com/wlandau/fbseqStudies/releases) on the project's [GitHub page](https://github.com/wlandau/fbseqStudies). Download the desired `tar.gz` bundle, then install it either with `install.packages(..., repos = NULL, type="source")` from within R  `R CMD INSTALL` from the Unix/Linux command line.

## Option 2: use `install_github` to install the development version.

For this option, you need the `devtools` package, available from CRAN or GitHub. Open R and run 

```{r, eval=F}
library(devtools)
install_github("wlandau/fbseqStudies")
```

## Option 3: build the development version from the source.

Open a command line program such as Terminal in Mac/Linux and enter the following commands.

```
git clone git@github.com:wlandau/fbseqStudies.git
R CMD build fbseqStudies
R CMD INSTALL ...
```

where `...` is replaced by the name of the tarball produced by `R CMD build`. 