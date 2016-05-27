#' @title Function \code{OpenMP_runs}
#' @description Execute OpenMP MCMC on Paschold data.
#' @export
#' @return path to results
#' @param path to directory to save simulations and results
OpenMP_runs = function(path = newdir()){
  path = newdir(path)
  data(paschold)
  paschold = get("paschold")

  burnin = 1000
  thin = 1
  iterations = 1000
  verbose = 10
  scaledown()

  for(mode in c("fullybayes", "ebayes", "ibayes")){
    if(mode == "ebayes"){
      configs = Configs(
        burnin = burnin, 
        iterations = iterations, 
        thin = thin, 
        verbose = verbose, 
        parameter_sets_return = c("beta", "epsilon", "gamma"),
        parameter_sets_update = c("beta", "epsilon", "gamma"))
      starts = Starts(
        nu = 2.5828448546, tau = 0.0067762724, 
        sigmaSquared = c(
          11.4259849453, 
          0.0447753762, 
          0.0340855288, 
          0.0004743057, 
          0.0763393484),
        theta = c(
          2.8237625966, 
          0.0022742791, 
          -0.0072736245, 
          -0.0045192501, 
          0.0079677045))
    } else if(mode == "ibayes"){
      configs = Configs(
        burnin = burnin, 
        iterations = iterations, 
        thin = thin, 
        verbose = verbose, 
        parameter_sets_return = c("beta", "epsilon", "gamma"),
        parameter_sets_update = c("beta", "epsilon", "gamma"))
      starts = Starts(
        nu = 0.001, tau = 0.01, 
        sigmaSquared = c(1000, 100, 100, 100, 100),
        theta = rep(0, 5))
    } else if(mode == "fullybayes"){
      configs = Configs(
        burnin = burnin, 
        iterations = iterations, 
        thin = thin, 
        verbose = verbose)
      starts = Starts()
    } else {
      stop("bad mode specified")
    }

    chain = Chain(paschold, configs, starts)
    saveRDS(chain, paste0(path, mode, "_OpenMP_chain_begin.rds"))
    t = proc.time()
    chains = fbseq(chain, backend = "OpenMP", threads = detectCores())
    runtime = proc.time() - t
    saveRDS(runtime, paste0(path, mode, "_OpenMP_runtime.rds"))
    saveRDS(chains, paste0(path, mode, "_OpenMP_chains_end.rds"))
    saveRDS(psrf(chains), paste0(path, mode, "_OpenMP_psrf.rds"))
  }
}
