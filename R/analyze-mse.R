#' @title Function \code{mse}
#' @description function for calculating mean squared error
#' @export
#' @return mean squared error
#' @param from directory with simulation lists
#' @param to output directory
mse = function(from, to){
  from = newdir(from)
  to = newdir(to)
  fs = list.files(from)
  fs = fs[grep(".rds", fs)]
  out = NULL
  for(f in fs){
    print(paste0(f, "mse"))
    l = readRDS(paste0(from, f))
    for(a in l$analyses){
      print(paste0("  ", a$analysis))
      m = c(meta(f), a$analysis)
      est = a$estimates[,grep("beta", colnames(a$estimates))]
      tr = l$scenario@supplement$truth
      if(class(tr) == "Starts"){
        truth = matrix(l$scenario@supplement$truth@beta, ncol = ncol(est))
      } else {
        truth = matrix(as.numeric(as.matrix(l$scenario@supplement$truth$beta)), ncol = ncol(est))
      }
      colnames(truth) = colnames(est) = paste0("beta_", 1:dim(est)[2])
      rownames(truth) = rownames(est)
      x = matrix(apply(est-truth, 2, function(x){mean(x^2)}), nrow = 1)
      colnames(x) = colnames(truth)
      df = data.frame(
        simulation = l$simulation,
        analysis = a$analysis,
        genes = as.integer(m[["genes"]]),
        libraries = as.integer(m[["libraries"]]),
        rep = as.integer(m[["rep"]]), stringsAsFactors = F)
      df = cbind(df, x)
      out = rbind(out, df)
    }
  }
  saveRDS(out, paste0(to, "mse.rds"))
  return(to)
}
