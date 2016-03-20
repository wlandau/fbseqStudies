#' @title Function \code{relevel_heterosis}
#' @description relevel heterosis status vector for plotting labels
#' @export
#' @param x factor to relevel
#' @return releveled factor
relevel_heterosis = function(x){
  x = as.character(x)
  x[x == "high-parent_hybrids"] = "high mean"
  x[x == "low-parent_hybrids"] = "low mean"
  x[x == "high-parent_hybrid1"] = "high H12"
  x[x == "low-parent_hybrid1"] = "low H12"
  x[x == "high-parent_hybrid2"] = "high H21"
  x[x == "low-parent_hybrid2"] = "low H21"
  ordered(x, levels = c("high H12", "high H21", "high mean", "low H12",  "low H21", "low mean", "high", "low"))
}


#' @title Function \code{relevel_heterosis_paschold}
#' @description relevel heterosis status vector for plotting labels
#' @export
#' @param x factor to relevel
#' @return releveled factor
relevel_heterosis_paschold = function(x){
  x = as.character(x)
  x = gsub("-parent_", " ", x)
  x = gsub("B73xMo17_Mo17xB73", "mean", x)
  ordered(x, levels = c("high B73xMo17", "high Mo17xB73", "high mean", "low B73xMo17",  "low Mo17xB73", "low mean"))
}
