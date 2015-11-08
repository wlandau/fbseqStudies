#' @title Function \code{fit_fbseq}
#' @description fits with fbseq
#' @export
#' @return information for a fitted model
#' @param sim a list, the current simulation object
#' @param depth one of "ebayes", "ebayes_from_truth", or "fullybayes"
#' @param prior prior distribution on betas
#' @param debug debug mode, TRUE/FALSE
fit_fbseq = function(sim, depth = "fullybayes", prior = "normal", debug = F){
  logs = mysink()
  t = my.proc.time()

  s = sim$scenario
  configs = Configs(priors = prior, iterations = 1e3, burnin = 1e5, thin = 1e2)
  if(debug){
    configs@iterations = 10
    configs@burnin = 0
    configs@thin = 0
    configs@ess = 2
    configs@max_attempts = 2
  }

  hyper = c("nu", "omegaSquared", "sigmaSquared", "tau", "theta")
  starts = Starts()

  if(depth == "ebayes"){
    ch = sim$analyses[[paste0("fullybayes_", prior)]]$chain
    for(p in hyper){
      slot(starts, p) = slot(ch, paste0(p, "PostMean"))
      configs@parameter_sets_return = setdiff(configs@parameter_sets_return, p)
      configs@parameter_sets_update = setdiff(configs@parameter_sets_update, p)
    }
  }

  if(depth == "ebayes_from_truth"){
    truth = s@supplement$truth
    if(!is(truth, "Starts")) return(NULL)
    for(p in hyper){
      slot(starts, p) = slot(truth, p)
      configs@parameter_sets_return = setdiff(configs@parameter_sets_return, p)
      configs@parameter_sets_update = setdiff(configs@parameter_sets_update, p)
    }
  }
 
  chain = Chain(s, configs, starts)
  chain = fbseq(chain)

  beta = matrix(chain@betaPostMean, nrow = chain@G)
  colnames(beta) = paste0("beta_", 1:5)
  eff = effect_sizes(chain)
  prob = probs(chain)
  est = cbind(beta, eff, prob)
  rownames(est) = rownames(s@counts)

  unsink(logs)
  list(analysis = paste0(depth, "_", prior), estimates = est, chain = chain, runtime = my.proc.time() - t)
}
