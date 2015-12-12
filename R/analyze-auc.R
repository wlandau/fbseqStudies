#' @title Function \code{auc}
#' @description calculates areas under ROC curves
#' @export
#' @return area under an ROC curve
#' @param roc data frame with columns named \code{fpr} and \code{tpr}
#' @param cutoff fpr cutoff below which area will be calculated
auc = function(roc, cutoff = 0.1){
  fpr = roc$fpr
  tpr = roc$tpr
  fn = stepfun(x = fpr, y = c(0, tpr))
  xs = seq(from = 0, to = cutoff, length.out = 1e3)
  trapz(x = xs, y = fn(xs))
}
