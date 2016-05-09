# Purpose

[`fbseqStudies`](https://github.com/wlandau/fbseqStudies) reproduces the results of academic studies of the methodology behind the [`fbseq`](https://github.com/wlandau/fbseq) and [`fbseqCUDA`](https://github.com/wlandau/fbseqCUDA) packages.

# System requirements

- Mac OS X or Linux. 
- The required R version and R packages listed in the  "Depends", "Imports", and "Suggests" fields of the "package's [DESCRIPTION](https://github.com/wlandau/fbseqStudies/blob/master/DESCRIPTION) file.
- A CUDA-enabled NVIDIA graphics processing unit (GPU) with compute capability 2.0 or greater.
- CUDA version 6.0 or greater. More information about CUDA is available through [NVIDIA](http://www.nvidia.com/object/cuda_home_new.html).

# Install

## Option 1: install a stable release (recommended).

Navigate to a [list of stable releases](https://github.com/wlandau/fbseqStudies/releases) on the project's [GitHub page](https://github.com/wlandau/fbseqStudies). Download the desired `tar.gz` bundle, then install it either with `install.packages(..., repos = NULL, type="source")` from within R  `R CMD INSTALL` from the Unix/Linux command line.

## Option 2: use `install_github` to install the development version.

For this option, you need the `devtools` package, available from CRAN or GitHub. Open R and run 

```
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

# Run a scaled-down version first

Before seriously running a long job with one of the functions in the next section, do a trial run first. For a trial run, set the ``"fbseqStudies.scaledown"`` option to ``TRUE``. 


```
library(fbseqStudies)
options("fbseqStudies.scaledown" = TRUE) # Scale down the computation.
paper_case() # Replicate the results of the case study paper.
```

Calling ``options("fbseqStudies.scaledown" = TRUE)`` selects the serial backend for [`fbseq`](https://github.com/wlandau/fbseq), ensures that datasets are small in the numbers of genes, and configures the MCMCs to run for only a few iterations. That way, ``paper_case()`` will complete in a few minutes on your home computer, as opposed to several days on a  machine with a CUDA-capable general-purpose graphics processing unit (GPU).


# Replicate the studies

Each function below reproduces the results of a paper. Each takes several days to run if `getOption("fbseqStudies.scaledown")` is `FALSE` (for serious analyses), but if your job is interrupted, simply calling the function again will resume the workflow roughly where it last left off. The same is true for the `*_mcmc()` functions described later.

- *`paper_computation()`* (publication pending)
- *`paper_case()`* (publication pending)
- *`paper_priors()`* (publication pending)

To run all 3 above functions in sequence, call the function `Landau_dissertation()`, which reproduces all the computation, figures, tables, etc. of the Statistics PhD dissertation of Will Landau (http://will-landau.com, will.landau@gmail.com).

For further control, choose among the following. For even finer-grained control, see the manual and help files.

- *`progress()`* In the above 3 `paper_*` functions, the rate limiting step is to produce directories called `real_mcmc`, `coverage_mcmc`, etc., each with `.rds` files inside. Each `.rds` file contains a single (simulated or real) RNA-seq dataset and all the analyses of that dataset. To check the progress of the analyses, run `progress("coverage_mcmc")`, for example. Running `progress("coverage_mcmc")` lists all the methods used to analyze each dataset in the `coverage_mcmc` directory produced by `coverage_mcmc()` in `paper_case()`.
- *`real_mcmc()`* Run different versions of the model on the Paschold et al. (2012) dataset. 
- *`computation_mcmc()`* Duplicate sections of the Paschold et al. (2012) dataset to create datasets of varying size. Fit the default model to see how runtime scales with the number of genes and the number of libraries.
- *`coverage_mcmc()`* Simulate different datasets from the model to assess the ability to recapture parameters of interest in credible intervals calculated from the estimated full joint posterior distribution. 
- *`comparison_mcmc()`* Simulate datasets from multiple scenarios and fit multiple versions of the model to assess gene detection power and the calibration of posterior probabilities.
- *`serial_runs()`* Run various analyses, including a fully Bayesian one, of the Paschold et al. (2012) dataset in a version of the software that makes no use of GPU computing at all.
- *`real_analyze()`* Extract useful data from the output of `real_mcmc()` to speed up figure generation.
- *`computation_analyze()`* Similar to `real_analyze()`, but for `computation_mcmc()`
- *`coverage_analyze()`* Similar to `real_analyze()`, but for `coverage_mcmc()`
- *`comparison_analyze()`* Similar to `real_analyze()`, but for `comparison_mcmc()`
- *`paper_case_figures()`* Once the `_mcmc()` and `_analyze()` functions are run in the current working directory, run this function to reproduce all the figures and tables of the case study paper.
- *`paper_case_figures()`* Once the `_mcmc()` and `_analyze()` functions are run in the current working directory, run this function to reproduce all the figures and tables of the paper comparing hierarchical distributions.