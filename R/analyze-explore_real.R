#' @title Function \code{explore_real}
#' @description explore_reals in computation study
#' @export
#' @param from directory with simulation lists
#' @param to output directory
explore_real = function(from, to){
  from = newdir(from)
  to = newdir(to)
  for(f in list.files(from)){
    print(f)
    tof = newdir(paste0(to, paste(meta(f), collapse = "_")))
    l = readRDS(paste0(from, f))
    a = l$analyses[["fullybayes+normal"]]
    m = mcmc_samples(a$chains)

    m_hyper = m[,c("nu", "tau", paste0("theta_", 1:5), paste0("sigmaSquared_", 1:5))]
    d = melt(m_hyper)
    pl = ggplot(d) + geom_histogram(aes_string(x = "value")) + facet_wrap(as.formula("~variable"), scales = "free_x") + mytheme()
    ggsave(paste0(tof, "hyperhist.pdf"), plot = pl, width = 8, height = 6)
    
    m_beta = m[,grep("beta", colnames(m))]
    d = melt(m_beta)
    pl = ggplot(d) + geom_histogram(aes_string(x = "value")) + facet_wrap(as.formula("~variable"), scales = "free_x") + mytheme()
    ggsave(paste0(tof, "betahist.pdf"), plot = pl, width = 8, height = 6)

    m_gamma = m[,grep("gamma", colnames(m))]
    d = melt(m_gamma)
    pl = ggplot(d) + geom_histogram(aes_string(x = "value")) + facet_wrap(as.formula("~variable"), scales = "free_x") + mytheme()
    ggsave(paste0(tof, "gammahist.pdf"), plot = pl, width = 8, height = 3)

    p = as.data.frame(probs(a$chains))
    d = melt(p)
    pl = ggplot(d) + geom_histogram(aes_string(x = "value")) + facet_wrap(as.formula("~variable"), scales = "free_x") + mytheme()
    ggsave(paste0(tof, "probhist.pdf"), plot = pl, width = 8, height = 6)

    e = estimates(a$chains)
    e = e[grep("beta_", rownames(e)),]
    e$parameter = ordered(gsub("_[0-9]*$", "", rownames(e)), levels = paste0("beta_", 1:5))
    e = e[order(e$mean),]
    e$index = 1:dim(e)[1]
    pl = ggplot(e) + mytheme_straight() + facet_wrap(as.formula("~parameter"), scales = "free") + 
      geom_segment(aes_string(x = "index", xend = "index", y = "lower_ci_0.95", yend = "upper_ci_0.95"), 
        color = "darkGray") + 
      geom_point(aes_string(x = "index", y = "mean"), size = I(0.5))
    ggsave(paste0(tof, "cred.png"), plot = pl, width = 8, height = 6)

    p = as.data.frame(probs(a$chains))
    p$geneID = rownames(p)
    data(paschold)
    ct = paschold@counts %*% kronecker(diag(4), matrix(1, nrow = 4))/4
    ct = cbind(ct, paschold@counts)
    colnames(ct) = c(paste0(unique(gsub("_[0-9]$", "", colnames(paschold@counts))), "_mean_count"), colnames(paschold@counts))
    p = cbind(p, ct)
    d = melt(p, id.vars = c("geneID", colnames(ct)))
    d = d[,c(23, 22, 1:21)]
    colnames(d)[1:2] = c("heterosis_prob", "heterosis_type")
    d = d[order(d$heterosis_prob, decreasing = T),]
    write.table(d, paste0(tof, "heterosis_probs.txt"), row.names = F)
  }
}
