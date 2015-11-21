#' @title Function \code{fit_fbseq}
#' @description fits with fbseq
#' @export
#' @return information for a fitted model
#' @param sim a list, the current simulation object
#' @param method one of "ebayes", "ebayesFromTruth", or "fullybayes"
#' @param prior prior distribution on betas
#' @param debug debug mode, TRUE/FALSE
#' @param configs \code{Configs} object for \code{fbseq}
fit_fbseq = function(sim, method = "fullybayes", prior = "normal", debug = F, configs = Configs()){
  logs = mysink()
  t = my.proc.time()

  s = sim$scenario
  configs@priors = prior
  if(debug){
    configs@iterations = 10
    configs@burnin = 0
    configs@thin = 0
    configs@ess = 0
    configs@max_attempts_diag = 1
    configs@max_attempts_ess = 0
  }

  hyper = c("nu", "omegaSquared", "sigmaSquared", "tau", "theta")
  starts = Starts()

  if(method == "ebayes"){
    ch = sim$analyses[[paste0("fullybayes+", prior)]]$chain
    for(p in hyper){
      slot(starts, p) = slot(ch, paste0(p, "PostMean"))
      configs@parameter_sets_return = setdiff(configs@parameter_sets_return, p)
      configs@parameter_sets_update = setdiff(configs@parameter_sets_update, p)
    }
  }

  if(method == "ebayesFromTruth"){
    truth = s@supplement$truth
    if(!is(truth, "Starts")) return(NULL)
    for(p in hyper){
      slot(starts, p) = slot(truth, p)
      configs@parameter_sets_return = setdiff(configs@parameter_sets_return, p)
      configs@parameter_sets_update = setdiff(configs@parameter_sets_update, p)
    }
  }
 
  if(sim$simulation != "paschold") starts@h = 0
  chain = Chain(s, configs, starts)
  configs = Configs(chain)
  starts = Starts(chain)
  chain = fbseq(chain)

  beta = matrix(chain@betaPostMean, nrow = chain@G)
  colnames(beta) = paste0("beta_", 1:5)
  eff = effect_sizes(chain)
  prob = probs(chain)
  est = cbind(beta, prob, eff)
  rownames(est) = rownames(s@counts)

  unsink(logs)
  list(analysis = paste0(method, "+", prior), estimates = est, chain = chain, 
    runtime = my.proc.time() - t, configs = configs, starts = starts)
}
