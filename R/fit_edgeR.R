#' @title Function \code{fit_edgeR}
#' @description fits edgeR
#' @export
#' @return a list of components of a fit object from \code{edgeR}
#' @param counts RNA-seq count datset
#' @param design design matrix
#' @param group group vector
#' @param ncores number of cores for parallel execution
#' @param scenario \code{fbseq::Scenario} object
fit_edgeR = function(counts, design, group = NULL, ncores = 1, scenario = NULL){
  data(paschold)
  if(is.null(scenario)) scenario = get("paschold")
  t = my.proc.time()
  dge = DGEList(counts = counts)
  dge = calcNormFactors(dge)
  norm_factors = dge$samples$norm.factors
  dge = estimateGLMCommonDisp(dge, design)
  dge = estimateGLMTagwiseDisp(dge, design)
  fit = glmFit(y = dge, design = design)
  beta = fit$coef
  beta[,1] = beta[,1] + rowMeans(fit$offset)

  contr = beta %*% do.call(cbind, scenario@contrasts)
  prob0 = 1 - sapply(scenario@contrasts, function(x){glmLRT(fit, contrast = x)$table$PValue})
  prob0[contr < 0] = 0
  prob = sapply(scenario@propositions, function(x){apply(matrix(prob0[,x], ncol = length(x)), 1, min)})

  chain = Chain(scenario)
  chain@G = nrow(beta)
  chain@betaPostMean = as.numeric(beta)
  chain@betaPostMeanSquare = rep(Inf, length(as.numeric(chain@betaPostMean)))
  chain@gammaPostMean = chain@gammaPostMeanSquare = rep(0, chain@G)
  chain@gene_names = character(0)
  effectSizes = effect_sizes(chain)

  ns = paste0(rep(c("high", "low"), 3), "_parent_hybrid", rep(c("s", 1, 2), each = 2))
  colnames(prob) = paste0("prob_", ns)[1:dim(prob)[2]]
  colnames(effectSizes) = paste0("effect_", ns[1:dim(effectSizes)[2]])

  list(analysis = "edgeR", 
       estimates = cbind(beta, prob, effectSizes), 
       dispersion = fit$dispersion,
       norm_factors = dge$samples$norm.factors,
       runtime = my.proc.time() - t)
}
