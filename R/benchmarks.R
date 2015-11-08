#' @title Function \code{benchmarks}
#' @description run benchmark methods on simulation objects
#' @export
#' @return a list if pertinent scenario information
#' @param path to directory to save simulations and results
#' @param methods methods to run
#' @param ncores number of cores to use
benchmarks = function(path, methods = c("edgeR", "Niemi"), ncores = 1){
  path = newdir(path)
  files = list.files(path)
  files = files[grep(".rds", files)]
  for(method in methods)
    for(f in files){
      p = paste0(path, f)
      o = readRDS(paste0(path, f))
      s = o$scenario
      o$analyses[[method]] = get(paste0("fit_", method))(
        counts = s@counts,
        design = s@design,
        group = s@supplement$group,
        ncores = ncores)
      saveRDS(o, p)
    }
}
