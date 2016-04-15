#' @include util-analyses.R util-simulations.R util-relevel_analyses.R util-relevel_simulations.R util-relevel_heterosis.R util-mytheme.R util-clean_df.R
NULL

#' @title Function \code{paper_case_figures}
#' @description Reproduce the figures and tables of the case study paper
#' @export
paper_case_figures = function(){

# library(fbseqStudies); library(xtable); library(reshape2); library(plyr); library(pracma); library(ggthemes); library(actuar); setwd("~/home/work/projects/thesis_data/results"); library(gridExtra); library(readr)

# control parms
dir = newdir("case_study_paper_figures")
extns = c("pdf", "ps", "eps")
mycolors = c("black", "blue", "red", "green", "purple")
gray = "#707070"

# credible interval info
l = as.data.frame(readRDS("coverage_analyze/ci/ci.rds"))
l$rep = ordered(l$rep, levels = 1:max(as.integer(l$rep)))
l = l[l$analysis == "fullybayes+normal",]

# fig:hypercoverage
dir_hypercoverage = newdir(paste0(dir, "fig-hypercoverage"))
l0 = l[!grepl("beta", l$parameter),]
l1 = ddply(l0, c("type", "simulation", "libraries", "analysis"), function(x){
#  if(mean(x[x$level == 0.5,]$cover) >= 0.5) return(NULL)
  x
})
types = c("nu", "tau", #paste0("blank", 1:3), 
  paste0("theta[", 1:5, "]"), paste0("sigma[", 1:5, "]^2"))
l1$type = ordered(l1$type, levels = types)
pl = ggplot(l1) + 
  geom_segment(data = l1[l1$level == 0.5,], mapping = aes_string(x = "rep", xend = "rep", y = "lower", yend = "upper"), size = 2) +
  geom_segment(data = l1[l1$level == 0.95,], mapping = aes_string(x = "rep", xend = "rep", y = "lower", yend = "upper")) + 
  facet_wrap(as.formula("~type"), scales = "free_y", labeller = label_parsed) + # , drop = F, ncol = 5) + 
  geom_abline(aes_string(slope = "0", intercept = "truth")) + 
  xlab("simulated dataset") +
  ylab("credible interval") + 
  mytheme_pub()
for(extn in extns)
  ggsave(paste0(dir_hypercoverage, "fig-hypercoverage.", extn), pl, height = 7, width = 9, dpi = 1200)

# fig:betarates
dir_betarates = newdir(paste0(dir, "fig-betarates"))
level = 0.95
l0 = l[grepl("beta", l$parameter) & l$level == level,]
l0$type = gsub("\\[", "[list(g", l0$type)
l0$type = gsub("]", ")]", l0$type)
l1 = ddply(l0, c("type", "rep"), function(x){
  data.frame(type = x$type[1], rep = x$rep[1], coverage = mean(x$cover))
})
saveRDS(l1, paste0(dir_betarates, "betarates.rds"))
pl = ggplot(l1) +
  geom_point(aes_string(x = "rep", y = "coverage")) + 
  geom_hline(yintercept = level) + 
  facet_wrap(as.formula("~type"), labeller = label_parsed) +
  xlab("simulated dataset") + ylab("coverage rate") +
  mytheme_pub()
for(extn in extns)
  ggsave(paste0(dir_betarates, "fig-betarates.", extn), pl, height = 6, width = 6, dpi = 1200)

# fig:modelroc
dir_modelroc = newdir(paste0(dir, "fig-modelroc"))
df = readRDS("coverage_analyze/roc_long/roc_long.rds")
df = df[df$analysis == "fullybayes+normal",]
df = case_clean_df(df)
pl = ggplot(df) +
  geom_line(aes_string(x = "fpr", y = "tpr", group = "file"), color = "black") + 
  geom_abline(slope = 1, intercept = 0, linetype = 2) + 
  facet_wrap(as.formula("~heterosis")) + xlab("false positive rate") + ylab("true positive rate") +
  mytheme_pub() + theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_modelroc, "fig-modelroc.", extn), pl, height = 6, width = 6, dpi = 1200)

# fig:modelcalibration
dir_modelcalibration = newdir(paste0(dir, "fig-modelcalibration"))
df = readRDS("coverage_analyze/calibration_long/calibration_long.rds")
df = df[df$analysis == "fullybayes+normal",]
df = case_clean_df(df)
pl = ggplot(df) +
  geom_line(aes_string(x = "probability", y = "proportion", group = "file"), color = "black") + 
  geom_abline(slope = 1, intercept = 0, linetype = 2) + 
  facet_wrap(as.formula("~heterosis")) +
  xlab("estimated posterior probability") + 
  ylab("proportion of genes with heterosis") +
  mytheme_pub() + theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_modelcalibration, "fig-modelcalibration.", extn), pl, height = 6, width = 6, dpi = 1200)

# fig:betacred
dir_betacred = newdir(paste0(dir, "fig-betacred"))
level = 0.95
l0 = l[grepl("beta", l$parameter) & l$level == level & l$rep == 1 & !l$cover,]
l0$type = gsub("\\[", "[list(g", l0$type)
l0$type = gsub("]", ")]", l0$type)
l0 = l0[order(l0$truth),]
l0 = ddply(l0, "type", function(x){
  x$interval = 1:dim(x)[1]
  x
})
hmean = rep(0, dim(l0)[1])
hmean[l0$type == "beta[list(g1)]"] = 3
hmean[l0$type == "beta[list(g2)]"] = 0
hmean[l0$type == "beta[list(g3)]"] = -0.007
hmean[l0$type == "beta[list(g4)]"] = -0.005
hmean[l0$type == "beta[list(g5)]"] = 0.008
l0$hmean = hmean
pl = ggplot(l0) +
  geom_segment(aes_string(x = "interval", xend = "interval", y = "lower", yend = "upper"), color = gray) +
  geom_point(aes_string(x = "interval", y = "truth"), color = "black", size = I(0.5)) + 
  geom_hline(aes_string(yintercept = "hmean")) +
  facet_wrap(as.formula("~type"), scales = "free", labeller = label_parsed) +
  xlab("credible interval") + ylab("true parameter value") + 
  mytheme_pub()
for(extn in extns)
  ggsave(paste0(dir_betacred, "fig-betacred.", extn), pl, height = 6, width = 7, dpi = 1200)
pl1 = ggplot(l0) +
  geom_segment(aes_string(x = "interval", xend = "interval", y = "lower", yend = "upper"), color = gray) +
  geom_point(aes_string(x = "interval", y = "truth"), color = "black", size = I(0.5)) + 
  geom_hline(aes_string(yintercept = "hmean")) +
  facet_wrap(as.formula("~type"), scales = "free", labeller = label_parsed, nrow = 1) +
  xlab("credible interval") + ylab("true parameter value") + 
  mytheme_pub() + theme(strip.text.x = element_blank()) + theme(axis.text.x = element_blank()) #+ theme(axis.text.x = element_text(angle = -80, hjust = 0))

# fig:betacoveragetrend
dir_betacoveragetrend = newdir(paste0(dir, "fig-betacoveragetrend"))
level = 0.95
l0 = l[grepl("beta", l$parameter) & l$level == level,]
l0$type = gsub("\\[", "[list(g", l0$type)
l0$type = gsub("]", ")]", l0$type)
l1 = ddply(l0, c("rep", "type"), function(z){
  k = ksmooth(x = z$truth, y = z$cover, bandwidth = 4*sd(z$truth))
  fn = stepfun(x = k$x, y = c(0, k$y))
  xs = seq(from = min(k$x), to = max(k$x), length.out = 4e2)
  ys = fn(xs)
  data.frame(truth = xs, cover = ys, type = z$type[1], rep = z$rep[[1]])
}, .progress = "text")
hmean = rep(0, dim(l1)[1])
hmean[l1$type == "beta[list(g1)]"] = 3
hmean[l1$type == "beta[list(g2)]"] = 0
hmean[l1$type == "beta[list(g3)]"] = -0.007
hmean[l1$type == "beta[list(g4)]"] = -0.005
hmean[l1$type == "beta[list(g5)]"] = 0.008
l1$hmean = hmean
pl = ggplot(l1) + 
  geom_line(aes_string(x = "truth", y = "cover", group = "rep"), alpha = 0.5) + 
  geom_abline(slope = 0, intercept = level, linetype = "dotted") +
  geom_vline(aes_string(xintercept = "hmean")) +
  facet_wrap(as.formula("~type"), scales = "free_x", labeller = label_parsed) +
  xlab("true parameter value") +
  ylab("local coverage rate") +
  mytheme_pub() + theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns[!grepl("ps", extns)])
  ggsave(paste0(dir_betacoveragetrend, "fig-betacoveragetrend.", extn), pl, height = 6, width = 7, dpi = 1200)
for(extn in extns[grepl("ps", extns)])
  ggsave(paste0(dir_betacoveragetrend, "fig-betacoveragetrend.", extn), pl, device=cairo_ps,
 height = 6, width = 7, dpi = 1200)
pl2 = ggplot(l1) + 
  geom_line(aes_string(x = "truth", y = "cover", group = "rep"), alpha = 0.5) + 
  geom_abline(slope = 0, intercept = level, linetype = "dotted") +
  geom_vline(aes_string(xintercept = "hmean")) +
  facet_grid(as.formula("~type"), scales = "free_x", labeller = label_parsed) +
  xlab("true parameter value") + 
  ylab("local coverage rate") +
  mytheme_pub() + theme(axis.text.x = element_text(angle = -80, hjust = 0))

# fig:betashrink
tryCatch({
dir_betashrink = newdir(paste0(dir, "fig-betashrink"))
file = paste0(dir_betashrink, "fig-betashrink.")
pdf(paste0(file, "pdf"), width = 10)
grid.arrange(pl2, pl1, ncol = 1)
dev.off()
for(ex in c("ps", "eps")){
  cairo_ps(paste0(file, ex), width = 10)
  grid.arrange(pl2, pl1, ncol = 1)
  dev.off()
}}, error = function(e) print("Could not make fig-betashrink."))

# fig:roc16 and fig:roc32
for(N in c(16, 32)){
  dir_roc = newdir(paste0(dir, "fig-roc", N))
  d = readRDS("comparison_analyze/roc_long/roc_long.rds")
  d = case_clean_df(d)
  d = d[d$libraries == N,]

  pl = ggplot(d) + 
    geom_line(aes_string(x = "fpr", y = "tpr", group = "file", color = "analysis", linetype = "analysis")) +
    facet_grid(as.formula("simulation~heterosis")) +
    xlab("false positive rate") + 
    ylab("true positive rate") +
    scale_color_manual(name = "analysis method", labels = case_analyses(), values = mycolors[1:length(case_analyses())]) +
    scale_linetype_manual(name = "analysis method", labels = case_analyses(), values = 1:length(case_analyses())) +
    mytheme_pub() +
    theme(axis.text.x = element_text(angle = -80, hjust = 0))
  for(extn in extns)
    ggsave(paste0(dir_roc, "fig-roc", N, ".", extn), pl, height = 8, width = 10, dpi = 1200)
}

# fig:auc
dir_auc = newdir(paste0(dir, "fig-auc"))
d = readRDS("comparison_analyze/auc_long/auc_long.rds")
d = case_clean_df(d)
pl = ggplot(d) + 
  geom_line(aes_string(x = "analysis", y = "auc_1", group = "libraries", linetype = "libraries"), color = "black") +
  geom_point(aes_string(x = "analysis", y = "auc_1", pch = "libraries"), color = "black") +
  facet_grid(as.formula("simulation~heterosis"), scales = "fixed") +
  xlab("analysis method") + 
  ylab("area under ROC curve") +
  labs(pch = "N", linetype = "N") +
  mytheme_pub() +
  theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_auc, "fig-auc.", extn), pl, height = 8, width = 10, dpi = 1200)

# fig:comparecal16 and fig:comparecal32
for(N in c(16, 32)){
  dir_comparecal = newdir(paste0(dir, "fig-comparecal", N))
  d = readRDS("comparison_analyze/calibration_long/calibration_long.rds")
  d = d[d$libraries == N,]
  d = case_clean_df(d)

  pl = ggplot(d) + 
    geom_abline(slope = 1, intercept = 0, color = gray) +
    geom_line(aes_string(x = "probability", y = "proportion", group = "file", color = "analysis", linetype = "analysis")) +
    facet_grid(as.formula("simulation~heterosis")) +
    xlab("estimated posterior probability") + 
    ylab("proportion of genes with heterosis") +
    scale_color_manual(name = "analysis method", labels = case_analyses(), values = mycolors[1:length(case_analyses())]) +
    scale_linetype_manual(name = "analysis method", labels = case_analyses(), values = 1:length(case_analyses())) +
    mytheme_pub() +
    theme(axis.text.x = element_text(angle = -80, hjust = 0))
  for(extn in extns)
    ggsave(paste0(dir_comparecal, "fig-comparecal", N, ".", extn), pl, height = 8, width = 10, dpi = 1200)
}

# fig:comparecalerror
dir_comparecalerror = newdir(paste0(dir, "fig-comparecalerror"))
df = readRDS("comparison_analyze/calibration_long/calibration_long.rds")
df$error = abs(df$proportion - df$probability)
d = ddply(df, c("file", "heterosis"), function(x){
  x$meanerror = trapz(x = x$probability, y = x$error)
  x[1,]
})
d = case_clean_df(d)
pl = ggplot(d) + 
  geom_line(aes_string(x = "analysis", y = "meanerror", group = "libraries", linetype = "libraries"), color = "black") +
  geom_point(aes_string(x = "analysis", y = "meanerror", pch = "libraries"), color = "black") +
  facet_grid(as.formula("simulation~heterosis"), scales = "fixed") +
  xlab("analysis method") + 
  ylab("calibration error") +
  labs(pch = "N", linetype = "N") +
  mytheme_pub() +
  theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_comparecalerror, "fig-comparecalerror.", extn), pl, height = 8, width = 10, dpi = 1200)

# paschold data analysis
l = readRDS("real_mcmc/paschold_39656_16_1.rds")
a = l$analyses[["fullybayes+normal"]]
m = mcmc_samples(a$chains)
e = estimates(a$chains, level = 0.95)
e0 = e

#fig:logcounts
dir_logcounts = newdir(paste0(dir, "fig-logcounts"))
data(paschold)
d = melt(log(get("paschold")@counts + 1))
pl = ggplot(d) + stat_density(aes_string(x = "value", y = "..density.."), color = gray, fill = gray) + 
  mytheme_pub() + 
  xlab("log(count + 1)")
for(extn in extns)
  ggsave(paste0(dir_logcounts, "fig-logcounts.", extn), pl, height = 6, width = 6, dpi = 1200)

# fig:hyperhist
dir_hyperhist = newdir(paste0(dir, "fig-hyperhist"))
m_hyper = m[,c("nu", "tau", paste0("theta_", 1:5), paste0("sigmaSquared_", 1:5))]
cn = colnames(m_hyper)
for(i in 1:5) cn = gsub(paste0("_", i), paste0("\\[", i, "\\]"), cn)
cn = gsub("sigmaSquared", "sigma", cn)
cn[grep("sigma", cn)] = paste0(cn[grep("sigma", cn)], "^2")
colnames(m_hyper) = cn
d = melt(m_hyper, id.vars = NULL)
pl = ggplot(d) + 
  stat_density(aes_string(x = "value", y = "..density.."), color = gray, fill = gray) + 
  facet_wrap(as.formula("~variable"), scales = "free", labeller = label_parsed) + 
  mytheme_pub() +
  theme(axis.text.x = element_text(angle = -80, hjust = 0)) + 
  xlab("parameter value") + 
  ylab("density")
for(extn in extns)
  ggsave(paste0(dir_hyperhist, "fig-hyperhist.", extn), pl, height = 8, width = 10, dpi = 1200)

# fig:betahist
dir_betahist = newdir(paste0(dir, "fig-betahist"))
m_beta = m[,grep("beta", colnames(m))]
cn = colnames(m_beta)
cn = do.call(rbind, strsplit(cn, "_"))
c2 = apply(cn, 1, function(x){
  paste0(x[1], "[list(", x[3],"~~", x[2], ")]")
})
colnames(m_beta) = c2
d = melt(m_beta, id.vars = NULL)
d = ddply(d, "variable", function(x){
  x$lower = quantile(x$value, 0.025)
  x$upper = quantile(x$value, 0.975)
  x$median = quantile(x$value, 0.5)
  x$max = max(x$value)
  x$min = min(x$value)
  x
})
rownames(cn) = c2

ci1 = ddply(d, "variable", function(x){x[1,]})
ci2 = e[apply(cn, 1, paste0, collapse="_"),]
ci2$variable = ci1$variable
rownames(ci2) = ci2$variable
ci2$min = ci1$min
ci2$max = ci1$max

x1 = data.frame(variable = ci1$variable, lower = ci1$lower, upper = ci1$upper, center = ci1$median, Interval = "quantile", y = 1)
x2 = data.frame(variable = ci2$variable, lower = ci2$lower_ci_0.95, upper = ci2$upper_ci_0.95, center = ci2$mean, Interval = "normal approximation", y = 2)
ci = rbind(x1, x2)
ci$mean = c(ci2$mean, ci2$mean)
ci$sd = c(ci2$sd, ci2$sd)
ci$y = -ci$y/(15*sqrt(2 *pi * ci$sd^2))
ci$lb = ci$y - abs(ci$y*0.15)

nrm = ddply(ci2, "variable", function(x){
  value = seq(from = x$min[1], to = x$max[1], length.out = 100)
  norm = dnorm(value, x$mean[1], x$sd[1])
  data.frame(x[rep(1, 100),], value = value, norm = norm)
})

pl = ggplot() + 
  stat_density(data = d, mapping = aes_string(x = "value", y = "..density.."), color = gray, fill = gray) + 
  geom_line(data = nrm, mapping = aes_string(x = "value", y = "norm"), linetype = 2) +
  geom_errorbarh(data = ci, mapping = aes_string(x = "center", y = "y", xmin="lower", xmax="upper", linetype = "Interval"), height = 0) +
  geom_point(data = ci, mapping = aes_string(x = "lower", y = "y"), size = 0.5) +
  geom_point(data = ci, mapping = aes_string(x = "upper", y = "y"), size = 0.5) +
  geom_point(data = ci, mapping = aes_string(x = "upper", y = "lb"), alpha = 0, size = 0) +
  facet_wrap(as.formula("~variable"), scales = "free", labeller = label_parsed) + 
  mytheme_pub() + 
  theme(legend.position = c(0.9, 0.1), plot.margin = unit(c(0.5, 1, 0.5, 0.5), "cm")) + 
  xlab("parameter value") + 
  ylab("density") + 
  labs(linetype = "95% credible interval")
for(extn in extns)
  ggsave(paste0(dir_betahist, "fig-betahist.", extn), pl, height = 10, width = 10, dpi = 1200)

# fig:gammahist
dir_gammahist = newdir(paste0(dir, "fig-gammahist"))
m_gamma = m[,grep("gamma", colnames(m))]
cn = colnames(m_gamma)
cn = do.call(rbind, strsplit(cn, "_"))
c2 = apply(cn, 1, function(x){
  paste0(x[1], "[", x[2],"]")
})
colnames(m_gamma) = c2
d = melt(m_gamma, id.vars = NULL)

d = ddply(d, "variable", function(x){
  x$lower = quantile(x$value, 0.025)
  x$upper = quantile(x$value, 0.975)
  x$median = quantile(x$value, 0.5)
  x$max = max(x$value)
  x$min = min(x$value)
  x
})
rownames(cn) = c2

ci1 = ddply(d, "variable", function(x){x[1,]})
ci2 = e[apply(cn, 1, paste0, collapse="_"),]
ci2$variable = ci1$variable
rownames(ci2) = ci2$variable
ci2$min = ci1$min
ci2$max = ci1$max

x1 = data.frame(variable = ci1$variable, lower = ci1$lower, upper = ci1$upper, center = ci1$median, Interval = "quantile", y = 1)
x2 = data.frame(variable = ci2$variable, lower = ci2$lower_ci_0.95, upper = ci2$upper_ci_0.95, center = ci2$mean, Interval = "inv-gamma approx", y = 2)
ci = rbind(x1, x2)
ci$mean = c(ci2$mean, ci2$mean)
ci$sd = c(ci2$sd, ci2$sd)
mn = ci$mean
s = ci$sd
shape = mn^2/s^2 + 2
scale = mn*(shape - 1)
md = scale/(shape+1)
ci$y = -ci$y/(23*md)
ci$lb = ci$y - abs(ci$y*0.15)

ig = ddply(ci2, "variable", function(x){
  value = seq(from = x$min[1], to = x$max[1], length.out = 100)
  m = x$mean[1]
  s = x$sd[1]
  shape = m^2/s^2 + 2
  scale = m*(shape - 1)
  ig = dinvgamma(value, shape = shape, scale = scale)
  data.frame(x[rep(1, 100),], value = value, ig)
})

pl = ggplot() + 
  stat_density(data = d, mapping = aes_string(x = "value", y = "..density.."), color = gray, fill = gray) + 
  geom_line(data = ig, mapping = aes_string(x = "value", y = "ig"), linetype = 2) +
  geom_errorbarh(data = ci, mapping = aes_string(x = "center", y = "y", xmin="lower", xmax="upper", linetype = "Interval"), height = 0) +
  geom_point(data = ci, mapping = aes_string(x = "lower", y = "y"), size = 0.5) +
  geom_point(data = ci, mapping = aes_string(x = "upper", y = "y"), size = 0.5) +
  geom_point(data = ci, mapping = aes_string(x = "upper", y = "lb"), alpha = 0, size = 0) +
  facet_wrap(as.formula("~variable"), ncol = 2, scales = "free", labeller = label_parsed) + 
  mytheme_pub() + 
  theme(legend.position = c(0.75, 0.25)) + #, axis.text.x = element_text(angle = -80, hjust = 0)) + 
  xlab("parameter value") + 
  ylab("density") + 
  labs(linetype = "95% credible interval")
for(extn in extns)
  ggsave(paste0(dir_gammahist, "fig-gammahist.", extn), pl, height = 6, width = 6, dpi = 1200)

# fig:betapostmeanhist
dir_betapostmeanhist = newdir(paste0(dir, "fig-betapostmeanhist"))
e = estimates(a$chains)
e = e[grep("beta_", rownames(e)),]
e$parameter = gsub("_[0-9]*$", "", rownames(e))
s = do.call(rbind, strsplit(e$parameter, "_"))
ns = apply(s, 1, function(x){
  paste0(x[1], "[list(g", x[2], ")]")
})
e$parameter = ordered(ns, levels = paste0("beta[list(g", 1:5, ")]"))
pl = ggplot(e) + 
  facet_wrap(as.formula("~parameter"), scales = "free", labeller = label_parsed) + 
  stat_density(aes_string(x = "mean", y = "..density.."), color = gray, fill = gray) + 
  xlab("estimated posterior mean") +
  ylab("density") +
  mytheme_pub() + 
  theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_betapostmeanhist, "fig-betapostmeanhist.", extn), pl, height = 6, width = 8, dpi = 1200)

# fig:pascholdcred
dir_pascholdcred = newdir(paste0(dir, "fig-pascholdcred"))
d = ddply(e, "parameter", function(x){
  x = x[sample(dim(x)[1], 1e3),]
  x = x[order(x$mean),]
  x$index = 1:dim(x)[1]
  x
})
pl = ggplot(d) + 
  facet_wrap(as.formula("~parameter"), scales = "free", labeller = label_parsed) + 
  geom_segment(aes_string(x = "index", xend = "index", y = "lower_ci_0.95", yend = "upper_ci_0.95"), 
    color = gray) + 
  geom_point(aes_string(x = "index", y = "mean"), size = I(0.15)) + 
  xlab("index") +
  ylab("parameter value") +
  mytheme_pub() + 
  theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_pascholdcred, "fig-pascholdcred.", extn), pl, height = 6, width = 8, dpi = 1200)

# fig:probhist
dir_probhist = newdir(paste0(dir, "fig-probhist"))
p = as.data.frame(probs(a$chains))
d = melt(p, id.vars = NULL)
d$variable = relevel_heterosis_paschold(d$variable)
pl = ggplot(d) + 
  geom_histogram(aes_string(x = "value", y = "..density.."), color = gray, fill = gray) + 
  facet_wrap(as.formula("~variable"), scales = "free_x") + 
  xlab("estimated posterior probability") + 
  ylab("density") +
  mytheme_pub() + theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_probhist, "fig-probhist.", extn), pl, height = 6, width = 8, dpi = 1200)

# compare with paschold results
data(paschold)
paschold = get("paschold")
ct = paschold@counts
data(tableS3table1)
groups = get("tableS3table1")
cn = c("high-parent_B73xMo17", "high-parent_Mo17xB73", "low-parent_B73xMo17", "low-parent_Mo17xB73")
p = as.data.frame(probs(a$chains)[,cn])
colnames(p) = c("hph_bm", "hph_mb", "lph_bm", "lph_mb")
p$Gene = rownames(p)
d = melt(p, id.vars = "Gene")
g2 = data.frame(
  hph_bm = groups$BxM %in% 5:6,
  hph_mb = groups$MxB %in% 5:6,
  lph_bm = groups$BxM %in% 7:8,
  lph_mb = groups$MxB %in% 7:8,
  Gene = rownames(p)
)
d2 = melt(g2, id.vars = "Gene")
colnames(d) = c("Gene", "Heterosis", "Probability")
d$Paschold = ifelse(d2$value, "discovery", "nondiscovery") 
levels(d$Heterosis) = c(
  "hph_bm" = "high B73xMo17", 
  "hph_mb" = "high Mo17xB73", 
  "lph_bm" = "low B73xMo17", 
  "lph_mb" = "low Mo17xB73")
paschold_status = d

# fig:comparehprobs
dir_comparehprobs = newdir(paste0(dir, "fig-comparehprobs"))
pl = ggplot(d) + 
  geom_histogram(aes_string(x = "Probability", y = "..density.."), color = gray, fill = gray) + 
  facet_grid(Paschold~Heterosis, scales = "free_y") + 
  xlab("estimated posterior probability") +
  ylab("density") +
  mytheme_pub() +
  theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_comparehprobs, "fig-comparehprobs.", extn), pl, height = 6, width = 8, dpi = 1200)


# Table S1 (TableS1.csv)
dir_TableS1 = newdir(paste0(dir, "TableS1"))
e = e0
p = as.data.frame(probs(a$chains))
geneID = rownames(p)
colnames(p) = paste0("probability_", colnames(p))
colnames(p) = gsub("parent", "parent-heterosis", colnames(p))
colnames(p) = gsub("B73xMo17_Mo17xB73", "hybrid-mean", colnames(p))
data(paschold)
paschold = get("paschold")
ct = paschold@counts
colnames(ct) = paste0(colnames(paschold@counts), "_count-data")
p = cbind(geneID, ct, p)

highB73xMo17 = paschold_status[paschold_status$Heterosis == "high B73xMo17",]
lowB73xMo17 = paschold_status[paschold_status$Heterosis == "low B73xMo17",]
highMo17xB73 = paschold_status[paschold_status$Heterosis == "high Mo17xB73",]
lowMo17xB73 = paschold_status[paschold_status$Heterosis == "low Mo17xB73",]

rownames(highB73xMo17) = highB73xMo17$Gene
rownames(lowB73xMo17) = lowB73xMo17$Gene
rownames(highMo17xB73) = highMo17xB73$Gene
rownames(lowMo17xB73) = lowMo17xB73$Gene

p[["Paschold-results_high-parent-heterosis_B73xMo17"]] = highB73xMo17[p$geneID,"Paschold"]
p[["Paschold-results_low-parent-heterosis_B73xMo17"]] = lowB73xMo17[p$geneID,"Paschold"]
p[["Paschold-results_high-parent-heterosis_Mo17xB73"]] = highMo17xB73[p$geneID,"Paschold"]
p[["Paschold-results_low-parent-heterosis_Mo17xB73"]] = lowMo17xB73[p$geneID,"Paschold"]

rownames(p) = NULL
for(i in 1:5){
  p[[paste0("beta_g", i, "_mean")]] = e[grepl(paste0("beta_", i), rownames(e)), "mean"]
  p[[paste0("beta_g", i, "_standard-deviation")]] = e[grepl(paste0("beta_", i), rownames(e)), "sd"]
}
p[["gamma_mean"]] = e[grepl("gamma", rownames(e)), "mean"]
p[["gamma_standard-deviation"]] = e[grepl("gamma", rownames(e)), "sd"]
write.csv(p, paste0(dir_TableS1, "TableS1.csv"), row.names = F)


# credible interval info for comparison study
l = as.data.frame(readRDS("comparison_analyze/ci/ci.rds"))
l$rep = ordered(l$rep, levels = 1:max(as.integer(l$rep)))
lc = l[l$analysis %in% c("fullybayes+normal", "ebayesFromFullybayes+normal", "ebayesFromTruth+normal"),]
l = l[l$analysis == "fullybayes+normal",]

# fig:comparebetarates
dir_comparebetarates = newdir(paste0(dir, "fig-comparebetarates"))
level = 0.95
l0 = lc[grepl("beta", lc$parameter) & lc$level == level,]
l0$heterosis = NA
l0 = clean_df(l0)
l0$type = gsub("\\[", "[list(g", l0$type)
l0$type = gsub("]", ")]", l0$type)
l1 = ddply(l0, c("type", "rep", "simulation", "analysis", "libraries"), function(x){
  data.frame(type = x$type[1], rep = x$rep[1], coverage = mean(x$cover))
})
saveRDS(l1, paste0(dir_comparebetarates, "comparebetarates.rds"))
pl = ggplot(l1) +
  geom_hline(yintercept = level, color = gray) + 
  geom_line(aes_string(x = "analysis", y = "coverage", group = "libraries", linetype = "libraries")) + 
  geom_point(aes_string(x = "analysis", y = "coverage", pch = "libraries")) + 
  facet_grid(as.formula("simulation~type"), labeller = label_parsed) +
  mytheme_pub() + theme(axis.text.x = element_text(angle = -80, hjust = 0)) + xlab("analysis method") + ylab("coverage rate") +
  labs(linetype = "N", pch = "N")
for(extn in extns)
  ggsave(paste0(dir_comparebetarates, "fig-comparebetarates.", extn), pl, height = 6, width = 8, dpi = 1200)

# fig:comparebetacred
dir_comparebetacred = newdir(paste0(dir, "fig-comparebetacred"))
level = 0.95
l2 = l[grepl("beta", l$parameter) & l$level == level & l$rep == 1 & !l$cover & l$libraries == 16,]
l2$heterosis = NA
l2 = clean_df(l2)
l2$type = gsub("\\[", "[list(g", l2$type)
l2$type = gsub("]", ")]", l2$type)
l2 = l2[order(l2$truth),]
l2 = ddply(l2, c("type", "simulation", "analysis"), function(x){
  x$interval = 1:dim(x)[1]/dim(x)[1]
  x
})
hmean = rep(0, dim(l2)[1])
hmean[l2$type == "beta[list(g1)]" & l2$simulation == "Model"] = 3
hmean[l2$type == "beta[list(g2)]" & l2$simulation == "Model"] = 0
hmean[l2$type == "beta[list(g3)]" & l2$simulation == "Model"] = -0.007
hmean[l2$type == "beta[list(g4)]" & l2$simulation == "Model"] = -0.005
hmean[l2$type == "beta[list(g5)]" & l2$simulation == "Model"] = 0.008
hmean[l2$type == "beta[list(g1)]" & l2$simulation == "Simple"] = 3
for(g in 2:5)
  hmean[l2$type == paste0("beta[list(g", g, ")]") & l2$simulation == "Simple"] = 0
for(g in 1:5){
  i = l2$type == paste0("beta[list(g", g, ")]") & l2$simulation == "edgeR"
  hmean[i] = mean(l2[i,"truth"])
}
l2$hmean = hmean
pl = ggplot(l2) +
  geom_segment(aes_string(x = "interval", xend = "interval", y = "lower", yend = "upper"), color = gray) +
  geom_point(aes_string(x = "interval", y = "truth"), color = "black", size = I(0.5)) + 
  geom_hline(aes_string(yintercept = "hmean")) +
  facet_grid(as.formula("type~simulation"), scales = "free", labeller = label_parsed) +
  xlab("credible interval") + ylab("true parameter value") + 
  mytheme_pub() + theme(axis.text.x = element_blank())
for(extn in extns)
  ggsave(paste0(dir_comparebetacred, "fig-comparebetacred.", extn), pl, height = 6, width = 7, dpi = 1200)
for(s in unique(l2$simulation)){
  dir_comparebetacred = newdir(paste0(dir, "fig-comparebetacred", s))
  pl = ggplot(l2[l2$simulation == s,]) +
    geom_segment(aes_string(x = "interval", xend = "interval", y = "lower", yend = "upper"), color = gray) +
    geom_point(aes_string(x = "interval", y = "truth"), color = "black", size = I(0.5)) + 
    geom_hline(aes_string(yintercept = "hmean")) +
    facet_wrap(as.formula("~type"), scales = "free", labeller = label_parsed) +
    xlab("credible interval") + ylab("true parameter value") + 
    mytheme_pub() + theme(axis.text.x = element_blank())
  for(extn in extns)
    ggsave(paste0(dir_comparebetacred, "fig-comparebetacred", s, ".", extn), pl, height = 6, width = 7, dpi = 1200)
}

# fig:comparebetacoveragetrend
level = 0.95
l0 = l[grepl("beta", l$parameter) & l$level == level,]
l0$heterosis = NA
l0 = clean_df(l0)
l0$type = gsub("\\[", "[list(g", l0$type)
l0$type = gsub("]", ")]", l0$type)
l1 = ddply(l0, c("rep", "type", "libraries", "simulation", "analysis"), function(z){
  k = ksmooth(x = z$truth, y = z$cover, bandwidth = 4*sd(z$truth))
  fn = stepfun(x = k$x, y = c(0, k$y))
  xs = seq(from = min(k$x), to = max(k$x), length.out = 4e2)
  ys = fn(xs)
  data.frame(truth = xs, cover = ys, type = z$type[1], rep = z$rep[[1]])
}, .progress = "text")
hmean = rep(0, dim(l1)[1])
hmean[l1$type == "beta[list(g1)]" & l1$simulation == "Model"] = 3
hmean[l1$type == "beta[list(g2)]" & l1$simulation == "Model"] = 0
hmean[l1$type == "beta[list(g3)]" & l1$simulation == "Model"] = -0.007
hmean[l1$type == "beta[list(g4)]" & l1$simulation == "Model"] = -0.005
hmean[l1$type == "beta[list(g5)]" & l1$simulation == "Model"] = 0.008
hmean[l1$type == "beta[list(g1)]" & l1$simulation == "Simple"] = 3
for(g in 2:5)
  hmean[l1$type == paste0("beta[list(g", g, ")]") & l1$simulation == "Simple"] = 0
for(g in 1:5){
  i = l1$type == paste0("beta[list(g", g, ")]") & l1$simulation == "edgeR"
  hmean[i] = mean(l1[i,"truth"])
}
l1$hmean = hmean
dir_comparebetacoveragetrend = newdir(paste0(dir, "fig-comparebetacoveragetrend"))
pl = ggplot(l1) + 
    geom_line(aes_string(x = "truth", y = "cover", linetype = "libraries")) + 
    geom_abline(slope = 0, intercept = level, color = gray) +
    geom_vline(aes_string(xintercept = "hmean")) +
    facet_grid(as.formula("type~simulation"), scales = "free", labeller = label_parsed) +
    xlab("true parameter value") +
    ylab("local coverage rate") +
    mytheme_pub() + theme(axis.text.x = element_text(angle = -80, hjust = 0)) + 
    labs(linetype = "N")
for(extn in extns)
  ggsave(paste0(dir_comparebetacoveragetrend, "fig-comparebetacoveragetrend.", extn), pl, height = 8, width = 8, dpi = 1200)
for(s in unique(l1$simulation)){
  dir_comparebetacoveragetrend = newdir(paste0(dir, "fig-comparebetacoveragetrend", s))
  d = l1[l1$simulation == s,]
  pl = ggplot(d) + 
    geom_line(aes_string(x = "truth", y = "cover", linetype = "libraries")) + 
    geom_abline(slope = 0, intercept = level, color = gray) +
    geom_vline(aes_string(xintercept = "hmean")) +
    facet_wrap(as.formula("~type"), scales = "free_x", labeller = label_parsed) +
    xlab("true parameter value") +
    ylab("local coverage rate") +
    mytheme_pub() + theme(axis.text.x = element_text(angle = -80, hjust = 0)) +
    labs(linetype = "N")
  for(extn in extns)
    ggsave(paste0(dir_comparebetacoveragetrend, "fig-comparebetacoveragetrend", s, ".", extn), pl, height = 7, width = 7, dpi = 1200)
}

# fig:comparebetashrink
for(s in unique(l1$simulation)){tryCatch({
  pl1 = ggplot(l1[l1$simulation == s,]) + 
    geom_line(aes_string(x = "truth", y = "cover", linetype = "libraries")) + 
    geom_abline(slope = 0, intercept = level) +
    #geom_vline(aes_string(xintercept = "hmean")) +
    facet_grid(as.formula("~type"), scales = "free_x", labeller = label_parsed) +
    xlab("true parameter value") +
    ylab("local coverage rate") +
    mytheme_pub() + theme(legend.position = "top", axis.text.x = element_text(angle = -80, hjust = 0)) + labs(linetype = "N") 

  pl2 = ggplot(l2[l2$simulation == s,]) +
    geom_segment(aes_string(x = "interval", xend = "interval", y = "lower", yend = "upper"), color = gray) +
    geom_point(aes_string(x = "interval", y = "truth"), color = "black", size = I(0.5)) + 
    geom_hline(aes_string(yintercept = "hmean")) +
    facet_wrap(as.formula("~type"), scales = "free", labeller = label_parsed, nrow = 1) +
    xlab("credible interval") + ylab("true parameter value") + 
    mytheme_pub() + theme(axis.text.x = element_blank(), strip.text.y = element_blank())

  dir_comparebetashrink = newdir(paste0(dir, "fig-comparebetashrink", s))
  file = paste0(dir_comparebetashrink, "fig-comparebetashrink", s, ".")
  pdf(paste0(file, "pdf"), width = 10)
  grid.arrange(pl1, pl2, ncol = 1)
  dev.off()
  for(ex in c("ps", "eps")){
    cairo_ps(paste0(file, ex), width = 10)
    grid.arrange(pl2, pl1, ncol = 1)
    dev.off()
  }
}, error = function(e) print(paste0("Could not make fig-comparebetashrink for ", s, ".")))}

# fig-volcano, fig-cts, fig-effectsize, fig-pvalhist
d = read_csv(paste0(dir, "TableS1/TableS1.csv"))
dir_volcano = newdir(paste0(dir, "fig-volcano"))
dir_cts = newdir(paste0(dir, "fig-cts"))
dir_effectsize = newdir(paste0(dir, "fig-effectsize"))
dir_pvalhist = newdir(paste0(dir, "fig-pvalhist"))

cts = d[,grepl("count.data", colnames(d))]
lc = log(cts + 1)
dh = lc[,grepl("^B73xMo17_|^Mo17xB73_", colnames(lc))]
dp1 = lc[,grepl("^B73_", colnames(lc))]
dp2 = lc[,grepl("^Mo17_", colnames(lc))]

hmean = apply(dh, 1, mean) 
p1mean = apply(dp1, 1, mean)
p2mean = apply(dp2, 1, mean)
effect_size = hmean - pmax(p1mean, p2mean)
effect_size[effect_size < 0] = 0

v1 = apply(dh, 1, var)
v2 = ifelse(p1mean > p2mean, apply(dp1, 1, var), apply(dp2, 1, var))
n1 = ncol(dh)
n2 = ifelse(p1mean > p2mean, ncol(dp1), ncol(dp2))
sds = sqrt(v1/n1 + v2/n2)
t_statistic = effect_size/sds
df = (v1/n1 + v2/n2)^2/((v1/n1)^2/(n1-1) + (v2/n2)^2/(n2-1))
pval = pt(t_statistic, df = df, lower.tail = F)
pval[!is.finite(pval)] = 1

pmean = d[["probability_high-parent-heterosis_hybrid-mean"]]
pB73xMo17 = d[["probability_high-parent-heterosis_B73xMo17"]]
pMo17xB73 = d[["probability_high-parent-heterosis_Mo17xB73"]]

group = rep("neither discovery", dim(d)[1])
group[d[["Paschold-results_high-parent-heterosis_B73xMo17"]] == "discovery"] = "B73xMo17 discovery"
group[d[["Paschold-results_high-parent-heterosis_Mo17xB73"]] == "discovery"] = "Mo17xB73 discovery"
group[d[["Paschold-results_high-parent-heterosis_B73xMo17"]] == "discovery" & 
      d[["Paschold-results_high-parent-heterosis_Mo17xB73"]] == "discovery"] = 
  "both discoveries"

probability = pmean
#probability[group == "B73xMo17 discovery"] = pB73xMo17[group == "B73xMo17 discovery"]
#probability[group == "Mo17xB73 discovery"] = pMo17xB73[group == "Mo17xB73 discovery"]

x = data.frame(
  gene = d$geneID,
  probability = probability,
  effect_size = effect_size,
  pval = pval,
  group = group)

pl = ggplot(x[x$group %in% c("both discoveries", "neither discovery"),]) + 
  stat_binhex(aes_string(x = "effect_size", y = "probability"), bins = 100) +
  geom_hline(yintercept = 0.5) +
  facet_wrap(as.formula("~group")) +
  scale_fill_gradient(guide = F, name = "count", trans = "log", low = "#707070", high = "black") +
  xlab("effect size") + 
  ylab("heterosis probability") +
  mytheme_pub()
for(extn in extns)
ggsave(paste0(dir_volcano, "fig-volcano.", extn), pl, height = 6, width = 9, dpi = 1200)

x$status = "agreement"
x$status[x$probability > 0.5 & x$group == "neither discovery"] = "disagreement"
#x$status[x$probability < 0.6 & x$group == "both discoveries"] = "lowyes"
outlier = "GRMZM2G429000"
x$status[x$gene == outlier] = "disagreement"

dp1 = lc[,grepl("^B73_", colnames(lc))]
dp2 = lc[,grepl("^Mo17_", colnames(lc))]
dh12 = lc[,grepl("^B73xMo17_", colnames(lc))] 
dh21 = lc[,grepl("^Mo17xB73_", colnames(lc))]

x$B73 = apply(dp1, 1, mean)
x$Mo17 = apply(dp2, 1, mean)
x$B73xMo17 = apply(dh12, 1, mean)
x$Mo17xB73 = apply(dh21, 1, mean)
x$meancount = apply(lc, 1, mean)

xplot = x[x$gene != outlier,]
vars = t(combn(c("B73", "Mo17", "B73xMo17", "Mo17xB73"), 2))
for(i in 1:dim(vars)[1]){
v1 = vars[i,1]
v2 = vars[i,2]
pl = ggplot(xplot) + 
  stat_binhex(aes_string(x = v1, y = v2), bins = 100) +
  geom_point(data = x[x$gene == outlier,], mapping = aes_string(x = v1, y = v2), pch = 17, size = 5) +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(as.formula("~status")) +
  mytheme_pub() +
  scale_fill_gradient(guide = F, name = "count", trans = "log", low = "#A0A0A0", high = "#303030")
  for(extn in extns)
ggsave(paste0(dir_cts, "fig-cts-", v1, "-", v2,".", extn), pl, width = 9, height = 6, dpi = 1200)
}

for(v in vars){
pl = ggplot(xplot) + 
  stat_binhex(aes_string(x = v, y = "effect_size"), bins = 100) +
  facet_wrap(as.formula("~status")) +
  ylab("effect size") +
  mytheme_pub() +
  scale_fill_gradient(guide = F, name = "count", trans = "log", low = "#707070", high = "black")
for(extn in extns)
ggsave(paste0(dir_effectsize, "fig-effectsize-vs-", v, ".", extn), pl, width = 9, height = 6, dpi = 1200)
}

# top = x$status == "disagreement" & x$probability > 0.9006
top = x$status == "disagreement" & x$probability > 0.9006

pl = ggplot(x[top,]) + 
  geom_histogram(aes_string(x = "pval"), bins = 20) +
  geom_vline(xintercept = x$pval[x$gene == outlier]) +
  mytheme_pub()
for(extn in extns)
ggsave(paste0(dir_pvalhist, "fig-pvalhist.", extn), pl, width = 9, height = 6, dpi = 1200)

} # paper_case_figures
