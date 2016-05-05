#' @include fit_edgeR.R fit_fbseq.R fit_Niemi.R
NULL

#' @title Function \code{fit}
#' @description run benchmark methods on simulation objects
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
#' @param benchmarks benchmark methods to run
#' @param fbseq_methods "ebayesFromFullybayes", "ebayesFromStarts", "ebayesFromTruth", "fullybayes", 
#' or a combination/vector of these. "fullybayes" must come before "ebayesFromFullybayes".
#' @param priors hierarchical distributions on the betas. Either a vector, each of whose elements 
#' is in the slot Configs()@priors, or a list of vectors, where each vector is in the slot Configs@priors.
#'
#' @param ncores number of cores for CPU-parallel methods
#' @param configs \code{Configs} object for \code{fbseq}
#' @param zeronormfactors TRUE/FALSE. If TRUE, starts@@h is set to 0.
fit = function(path, benchmarks = "edgeR", #c("edgeR", "Niemi"), 
  fbseq_methods = c("fullybayes", "ebayesFromFullybayes", "ebayesFromStarts", "ebayesFromTruth"), 
  priors = c("normal", special_beta_priors()), ncores = detectCores(), configs = Configs(),
  zeronormfactors = F){

  path = newdir(path)
  files = list.files(path)
  files = files[grep(".rds", files)]

  for(method in benchmarks){
    for(f in files){
      p = paste0(path, f)
      o = readRDS(paste0(path, f))
      s = o$scenario
      if(is.null(o$analyses[[method]]))
        o$analyses[[method]] = get(paste0("fit_", method))(
          counts = s@counts,
          design = s@design,
          group = s@supplement$group,
          ncores = ncores,
          scenario = s)
      saveRDS(o, p)
    }
  }

  for(prior in priors) for(fbseq_method in fbseq_methods) for(f in files){
    p = paste0(path, f)
    o = readRDS(paste0(path, f))
    s = o$scenario
    method = paste0(fbseq_method, "+", paste0(prior, collapse = ""))

    if(is.null(o$analyses[[method]])){
      o$analyses[[method]] = fit_fbseq(o, fbseq_method, prior, configs = configs, zeronormfactors = zeronormfactors)
      saveRDS(o, p)
    }
  }

  path
}
