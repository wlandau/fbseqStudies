#' @title Function \code{fit_fbseq}
#' @description fits with fbseq
#' @export
#' @return information for a fitted model
#' @param sim a list, the current simulation object
#' @param method one of "ebayesFromFullybayes", "ebayesFromStarts", "ebayesFromTruth", or "fullybayes"
#' "ebayesFromFullybayes" requires a "fullybayes" analysis already present
#' @param prior prior distribution on betas
#' @param debug debug mode, TRUE/FALSE
#' @param configs \code{Configs} object for \code{fbseq}
fit_fbseq = function(sim, method = "fullybayes", prior = "normal", debug = F, configs = Configs()){
  logs = mysink()
  t = my.proc.time()

  s = sim$scenario
  configs@priors = prior
  if(any(prior %in% special_beta_priors())){
    configs@parameter_sets_return = c(configs@parameter_sets_return, "xi")
    configs@parameter_sets_update = c(configs@parameter_sets_update, "xi")
  }
  if(debug){
    configs@iterations = 10
    configs@burnin = 0
    configs@thin = 1
  }

  starts = Starts()
  hyper = c("nu", "sigmaSquared", "tau", "theta")
  if(grepl("ebayes", method))
    for(p in hyper){
      configs@parameter_sets_return = setdiff(configs@parameter_sets_return, p)
      configs@parameter_sets_update = setdiff(configs@parameter_sets_update, p)
    }

  if(method == "ebayesFromFullybayes"){
    est = estimates(sim$analyses[[paste0("fullybayes+", prior)]]$chains)
    for(p in hyper) slot(starts, p) = est[grep(p, rownames(est)), "mean"]
  }

  if(method == "ebayesFromTruth"){
    truth = s@supplement$truth
    if(!is(truth, "Starts")) return(NULL)
    for(p in hyper) slot(starts, p) = slot(truth, p)
  }
 
  if(sim$simulation != "paschold") starts@h = 0
  chain = Chain(s, configs, starts)
  configs = Configs(chain)
  starts = Starts(chain)
  chains = fbseq(chain)

  est = estimates(chains)
  beta = matrix(est[grep("beta_", rownames(est)), "mean"], nrow = chains[[1]]@G)
  colnames(beta) = paste0("beta_", 1:5)
  eff = effect_sizes(chains)
  colnames(eff) = paste0("effect_", colnames(eff))
  prob = probs(chains)
  colnames(prob) = paste0("prob_", colnames(prob))
  est = cbind(beta, prob, eff)
  rownames(est) = rownames(s@counts)

  unsink(logs)
  list(analysis = paste0(method, "+", prior), estimates = est, chains = chains, 
    runtime = my.proc.time() - t, configs = configs, starts = starts, psrf = psrf(chains),
    ess = effectiveSize(mcmc_samples(chains)))
}
