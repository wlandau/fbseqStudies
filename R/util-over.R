#' @title Function \code{over_sims}
#' @description apply a function over simulation lists and save to another directory
#' @export
#' @param from directory with simulation lists
#' @param to output directory
#' @param fun function to apply to each simulation list
over_sims = function(from, to, fun){
  from = newdir(from)
  to = newdir(to)
  fs = list.files(from)
  fs = fs[grep(".rds", fs)]
  for(f in fs){
    print(f)
    l = readRDS(paste0(from, f))
    out = fun(l)
    saveRDS(out, paste0(to, f))
  }
}

#' @title Function \code{over_ans}
#' @description apply a function over every \code{analyses} element of every simulation list in a directory
#' @export
#' @param from directory with simulation lists
#' @param to output directory
#' @param fun function to apply to each \code{analyses} element of every simulation list. Must take in 
#' the simulation list and analysis list, in that order.
over_ans = function(from, to, fun){
  from = newdir(from)
  to = newdir(to)
  fs = list.files(from)
  fs = fs[grep(".rds", fs)]
  for(f in fs){
    print(f)
    l = readRDS(paste0(from, f))
    for(a in l$analyses){
      print(paste0("  ", a$analysis))
      m = c(meta(f), a$analysis)
      f2 = paste0(to, paste(m, collapse = "_"), ".rds")
      if(file.exists(f2)) next
      out = fun(l, a)
      saveRDS(out, f2)
    }
  }
}

#' @title Function \code{over_out}
#' @description apply a function over every analysis-specific output file in a given directory
#' @export
#' @param from directory with simulation lists
#' @param to output directory
#' @param fun function to apply over every analysis-specific output file in a given directory
over_out = function(from, to, fun){
  from = newdir(from)
  to = newdir(to)
  fs = list.files(from)
  fs = fs[grep(".rds", fs)]
  for(f in fs){
    print(f)
    l = readRDS(paste0(from, f))
    if(file.exists(paste0(to, f))) next
    out = fun(l)
    saveRDS(out, paste0(to, f))
  }
}
