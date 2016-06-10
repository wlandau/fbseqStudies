#' @title Function \code{simulation_priors}
#' @description simulates a small differential expression scenario
#' @export
#' @return a list if pertinent scenario information
#' @param genes number of genes
#' @param libraries number of libraries. Must be even.
#' @param prior on the beta_{2, g} parameters. one of "normal", "Laplace", or "t"
simulation_priors = function(genes = 1e4, libraries = 16, prior = "normal"){
  stopifnot(!(libraries %% 2))
  design = cbind(beta_1 = rep(1, libraries), beta_2 = rep(c(-1, 1), each = libraries/2))
  group = 1 + (design[,2] == 1)
  s = Starts()
  SD = sqrt(0.05)

  counts = NA
  while(any(!is.finite(counts))){
    beta = matrix(0, nrow = genes, ncol = ncol(design))
    colnames(beta) = colnames(design)
    rownames(beta) = paste0("gene_", 1:genes)
  
    beta[,1] = rnorm(genes, 3, 1)
    if(prior == "normal"){
      xi = rep(1, genes)
      x = rnorm(genes, 0, SD)
    } else if(prior == "Laplace"){
      xi = rexp(genes)
      x = rnorm(genes, 0, SD*sqrt(xi))
    } else if(prior == "t"){
      xi = 1/rgamma(genes, shape = s@q, rate = s@r)
      x = rnorm(genes, 0, SD*sqrt(xi))
    } else {
      stop("prior must be normal, Laplace, or t.")
    }
    beta[,2] = x
    truth = Starts(nu = 3, tau = 0.01, sigmaSquared = c(1, SD^2), theta = c(3, 0), 
      beta = as.numeric(beta), h = rep(0, libraries), xi = c(rep(1, genes), xi))
    truth@gamma = 1/rgamma(genes, shape = truth@nu/2, rate = truth@nu*truth@tau/2)
    gammat = matrix(rep(truth@gamma, times = libraries), ncol = libraries)
    epsilon = matrix(rnorm(libraries*genes, 0, sqrt(gammat)), ncol = libraries)
    truth@epsilon = as.numeric(epsilon)
    mu = t(design %*% t(beta))
    counts = matrix(rpois(genes*libraries, exp(epsilon + mu)), nrow = genes)
  }

  colnames(counts) = rownames(design) = paste0("group_", group, "_", 1:length(group))
  rownames(counts) = paste0("gene_", 1:genes)

  contrasts = list(
    high = c(beta_1 = 0, beta_2 = 1),
    low = c(beta_1 = 0, beta_2 = -1)
  )
  q = qnorm(0.9, 0, SD)
  bounds = c(high = q, low = q)
  propositions = list(high = 1, low = 2)

  supplement = list(
    beta = beta,
    mu = mu,
    prior = prior,
    simulation = paste0("priors", prior),
    group = group,
    truth = truth
  )

  scenario = Scenario(
    counts = counts, 
    design = design, 
    contrasts = contrasts, 
    bounds = bounds, 
    propositions = propositions,
    supplement = supplement
  )

  list(scenario = scenario, analyses = list(), simulation = paste0("priors", prior))
}
