#' @title Function \code{fit_edgeR}
#' @description fits edgeR
#' @export
#' @return a list of components of a fit object from \code{edgeR}
#' @param counts A data frame or matrix of RNA-seq read counts.
#' @param design design matrix
fit_edgeR = function(counts, design){
  data(paschold)
  paschold = get("paschold")
  logs = mysink()
  t = my.proc.time()
  dge = DGEList(counts = counts)
  dge = calcNormFactors(dge)
  norm_factors = dge$samples$norm.factors
  dge = estimateGLMCommonDisp(dge, design)
  dge = estimateGLMTagwiseDisp(dge, design)
  fit = glmFit(y = dge, design = design)
  beta = fit$coef
  beta[,1] = beta[,1] + rowMeans(fit$offset)
  normfactors = dge$samples$norm.factors

  contr = beta %*% do.call(cbind, paschold@contrasts)
  prob0 = 1 - sapply(paschold@contrasts, function(x){glmLRT(fit, contrast = x)$table$PValue})
  prob0[contr < 0] = 0
  prob = sapply(paschold@propositions, function(x){apply(prob0[,x], 1, min)})

  chain = Chain(paschold)
  chain@betaPostMean = as.numeric(beta)
  chain@gene_names = character(0)
  effectSizes = effect_sizes(chain)

  ns = paste0(rep(c("high", "low"), 3), "_parent_hybrid", rep(c("s", 1, 2), each = 2))
  colnames(prob) = paste0("prob_", ns)
  colnames(effectSizes) = paste0("effect_", ns)

  unsink(logs)
  list(estimates = cbind(beta, prob, effectSizes), dge = dge, fit = fit, runtime = my.proc.time() - t)
}
