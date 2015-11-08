#' @title Function \code{simulation_simple}
#' @description simulates a scenario using a naive method
#' @export
#' @return a list if pertinent scenario information
#' @param genes number of genes
#' @param libraries number of libraries
simulation_simple = function(genes = 3.5e4, libraries = 16){
  data(paschold)
  paschold = get("paschold")

  ns = 0:(libraries -1) %% ncol(paschold@counts) + 1
  group = (ns + (ns %% 2)) / 2
  design = paschold@design[ns,]

  beta = matrix(0, nrow = genes, ncol = ncol(design))
  colnames(beta) = colnames(design)
  rownames(beta) = paste0("gene_", 1:genes)
  
  beta[,1] = rnorm(genes, 5, 0.1)
  beta[,2] = sample(c(0, -2, 2), genes, prob = c(0.6, 0.2, 0.2), replace = T)
  beta[,3] = sample(c(0, -2, 2), genes, prob = c(0.6, 0.2, 0.2), replace = T)
  beta[,4] = rnorm(genes, 0.1, 0.1)
  beta[,5] = rnorm(genes, 0.1, 0.1)

  beta = t(apply(beta, 1, function(x){
    if(abs(sum(x[2:3])) == 2) x[2:3] = 0
    x
  }))

  lambda = t(design %*% t(beta))
  disp = 0.01

  counts = matrix(rnbinom(n = prod(dim(lambda)), mu = exp(lambda), size = 1/disp), nrow = genes)
  rownames(counts) = paste0("gene_", 1:genes)
  colnames(counts) = paste0("library_", 1:libraries)

  truth = list(
    beta = beta,
    lambda = lambda,
    disp = disp
  )

  supplement = list(
    simulation = "simple",
    group = group,
    truth = truth
  )

  scenario = Scenario(
    counts = counts, 
    design = design, 
    contrasts = paschold@contrasts, 
    bounds = paschold@bounds, 
    propositions = paschold@propositions,
    supplement = supplement
  )

  list(scenario = scenario, analyses = list())
}
