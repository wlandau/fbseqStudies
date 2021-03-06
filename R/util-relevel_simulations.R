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
#  out[out == "priorshorseshoe"] = "horseshoe"
  ordered(out, levels = c(
    "normal", 
    "t",
    "Laplace", 
#    "horseshoe",
    "Simple", 
    "Model",
    "edgeR",
    "Niemi"))
}
