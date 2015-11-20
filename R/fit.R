#' @title Function \code{fit}
#' @description run benchmark methods on simulation objects
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
#' @param benchmarks benchmark methods to run
#' @param fbseq_methods "fullybayes", "ebayes", "ebayes_from_truth" or a vector with a combination. "fullybayes" must come before "ebayes".
#' @param priors priors on the betas
#' @param ncores number of cores for CPU-parallel methods
#' @param debug debug mode, TRUE/FALSE
#' @param configs \code{Configs} object for \code{fbseq}
fit = function(path, benchmarks = c("edgeR", "Niemi"), fbseq_methods = c("fullybayes", "ebayes", "ebayesFromTruth"), 
  priors = special_beta_priors(), ncores = 1, debug = F, configs = Configs()){

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
          ncores = ncores)
      saveRDS(o, p)
    }
  }

  for(prior in priors) for(fbseq_method in fbseq_methods) for(f in files){
    p = paste0(path, f)
    o = readRDS(paste0(path, f))
    s = o$scenario
    method = paste0(fbseq_method, "+", prior)

    if(is.null(o$analyses[[method]])){
      o$analyses[[method]] = fit_fbseq(o, fbseq_method, prior, debug, configs = configs)
      saveRDS(o, p)
    }
  }

  path
}
