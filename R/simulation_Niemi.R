#' @include fit_Niemi.R
NULL

#' @title Function \code{simulation_Niemi}
#' @description simulates a scenario using Niemi
#' @export
#' @return a list if pertinent scenario information
#' @param genes number of genes
#' @param libraries number of libraries
#' @param fit output object from fit_Niemi
#' @param ncores number of cores 
simulation_Niemi = function(genes = 3e4, libraries = 16, fit = NULL, ncores = detectCores()){
  data(paschold)
  paschold = get("paschold")
  scaledown()
  l = simulation_paschold()
  if(is.null(fit)) fit = fit_Niemi(paschold@counts, paschold@design, group = l$scenario@supplement$group, 
    ncores = ncores)

  beta = fit$estimates[,grep("beta_", colnames(fit$estimates))]
  gs = sample.int(dim(fit$estimates)[1], genes, replace = T)
  ns = 0:(libraries -1) %% ncol(paschold@counts) + 1
  group = (ns + (ns %% 2)) / 2

  beta = beta[gs,]
  disp = exp(fit$psi[gs])
  norm_factors = exp(fit$hyperparameters$c[ns])

  norm_mat = matrix(rep(norm_factors, each = genes), nrow = genes)
  disp_mat = matrix(rep(disp, times = libraries), nrow = genes)

  design = paschold@design[ns,]
  lambda = t(design %*% t(beta)) + log(norm_mat)

  counts = matrix(rnbinom(n = prod(dim(lambda)), mu = exp(lambda), size = 1/disp_mat), nrow = genes)

  libnames = colnames(paschold@counts)
  libnames = gsub("B73xMo17_Mo17xB73", "hybrids", libnames)
  libnames = gsub("B73xMo17", "hybrid1", libnames)
  libnames = gsub("Mo17xB73", "hybrid2", libnames)
  libnames = gsub("B73", "parent1", libnames)
  libnames = gsub("Mo17", "parent2", libnames)
  libnames = gsub("_.*", "", libnames)
  libnames = paste0(libnames[ns], "_", 1:libraries)

  rownames(counts) = paste0("gene_", 1:genes)
  colnames(counts) = rownames(design) = libnames

  truth = list(
    fit = fit, 
    gs = gs,
    ns = ns,
    beta = beta,
    lambda = lambda,
    disp = disp,
    norm_factors = norm_factors
  )

  supplement = list(
    simulation = "Niemi",
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

  list(scenario = scenario, analyses = list(), simulation = "Niemi")
}
