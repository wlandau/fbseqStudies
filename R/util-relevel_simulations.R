#' @title Function \code{relevel_simulations}
#' @description relevel simulations vector for plotting labels
#' @export
#' @param x factor to relevel
#' @return releveled factor
relevel_simulations = function(x){
  out = as.character(x)
  out[out == "simple"] = "Simple"
  out[out == "model"] = "Model"
  out[out == "priorsnormal"] = "normal"
  out[out == "priorsLaplace"] = "Laplace"
  out[out == "priorst"] = "t"
  ordered(out, levels = c(
    "normal", 
    "Laplace", 
    "t",
    "Simple", 
    "Model",
    "edgeR",
    "Niemi"))
}
