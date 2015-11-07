#' @title Function \code{fit_edgeR}
#' @description fits edgeR
#' @export
#' @return a list of components of a fit object from \code{edgeR}
#' @param counts A data frame or matrix of RNA-seq read counts.
#' @param design design matrix
fit_edgeR = function(counts, design){
  logs = mysink()
  t = my.proc.time()
  dge = DGEList(counts = counts)
  dge = calcNormFactors(dge)
  norm_factors = dge$samples$norm.factors
  dge = estimateGLMCommonDisp(dge, design)
  dge = estimateGLMTagwiseDisp(dge, design)
  fit = glmFit(y = dge, design = design)
  unsink(logs)
  list(dge = dge, fit = fit, runtime = my.proc.time() - t)
}
