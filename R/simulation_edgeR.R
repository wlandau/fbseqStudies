#' @title Function \code{simulation_edgeR}
#' @description simulates a scenario using edgeR
#' @export
#' @return a list if pertinent scenario information
#' @param fit output object from fit_edgeR
#' @param genes number of genes
#' @param libraries number of libraries
simulation_edgeR = function(fit, genes = 3.5e4, libraries = 16){
  data(paschold)
  paschold = get("paschold")

  beta = fit$fit$coef
  beta[,1] = beta[,1] + rowMeans(fit$fit$offset)

  gs = sample.int(nrow(paschold@counts), genes, replace = T)
  ns = 0:(libraries -1) %% ncol(paschold@counts) + 1

  beta = beta[gs,]
  disp = fit$fit$dispersion[gs]
  norm_factors = fit$dge$samples$norm.factors[ns]

  norm_mat = matrix(rep(norm_factors, each = genes), nrow = genes)
  disp_mat = matrix(rep(disp, times = libraries), nrow = genes)

  design = paschold@design[ns,]
  lambda = t(design %*% t(beta)) + norm_mat

  counts = matrix(rnbinom(n = prod(dim(lambda)), mu = exp(lambda), size = 1/disp_mat), nrow = genes)
  rownames(counts) = rownames(paschold@counts)[gs]
  colnames(counts) = rownames(design)

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
    simulation = "edgeR",
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
