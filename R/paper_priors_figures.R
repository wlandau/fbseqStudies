#' @include util-analyses.R util-simulations.R util-relevel_analyses.R util-relevel_simulations.R util-relevel_heterosis.R util-mytheme.R
NULL

#' @title Function \code{paper_priors_figures}
#' @description Reproduce the figures and tables of the case study paper
#' @export
paper_priors_figures = function(){

# library(fbseqStudies); library(xtable); library(reshape2); library(plyr); library(pracma); library(ggthemes); library(actuar); setwd("~/home/work/projects/thesis_data/results")

# control parms
dir = newdir("priors_study_paper_figures")
extns = c("pdf", "ps", "eps")
mycolors = c("black", "blue", "red")
gray = "#707070"

# fig-priorshyperhist
dir_priorshyperhist = newdir(paste0(dir, "fig-priorshyperhist"))
m_hyper = NULL
for(f in list.files("priors_mcmc")) if(as.integer(meta(f)["libraries"]) == 8 & as.integer(meta(f)["rep"]) == 1){
  print(f)
  l = readRDS(paste0("priors_mcmc/", f))
  for(a in l$analyses){
    m = mcmc_samples(a$chains)
    m = m[,grep("nu|tau|theta|sigma", colnames(m))]
    m_long = melt(m, id.vars = NULL)
    m_long$simulation = gsub("priors", "", l$simulation)
    a$analysis = gsub("normalnormal", "normal", a$analysis)
    a$analysis = gsub("normalLaplace", "Laplace", a$analysis)
    a$analysis = gsub("normalt", "t", a$analysis)
    m_long$analysis = gsub("fullybayes\\+", "", a$analysis)
    tr = l$scenario@supplement$truth
    for(n in c("simulation", "analysis")) m_long[[n]] = ordered(m_long[[n]], levels = priors_analyses())
    m_long = ddply(m_long, "variable", function(x){
      s = unlist(strsplit(as.character(x$variable[1]), "_"))
      if(length(s) > 1){
        x$truth = slot(tr, s[1])[as.integer(s[2])]
      } else {
        x$truth = slot(tr, s[1])
      }
      x
    })
    m_hyper = rbind(m_hyper, m_long)
  }
}
m_hyper$simulation = ordered(m_hyper$simulation, labels = paste(levels(m_hyper$simulation), "sim"))
m_hyper$analysis = ordered(m_hyper$analysis,  labels = paste(levels(m_hyper$analysis), "analysis"))
for(v in unique(m_hyper$variable)){
  d = m_hyper[m_hyper$variable == v,]
  pl = ggplot(d) +
    stat_density(aes_string(x = "value", y = "..density.."), size = 0.75) + 
    geom_vline(xintercept = d$truth[1]) + 
    facet_grid(as.formula("simulation~analysis"), scales = "free") +
    mytheme_pub() 
  for(extn in extns)
    ggsave(paste0(dir_priorshyperhist, "fig-priorshyperhist-", d$variable[1], ".", extn), pl, height = 8, width = 6, dpi = 1200)
}

# fig-priorshypercoverage
dir_priorshypercoverage = newdir(paste0(dir, "fig-priorshypercoverage"))
l = as.data.frame(readRDS("priors_analyze/ci/ci.rds"))
l$analysis = gsub("normalnormal", "normal", l$analysis)
l$analysis = gsub("normalLaplace", "Laplace", l$analysis)
l$analysis = gsub("normalt", "t", l$analysis)
l$rep = ordered(l$rep, levels = 1:max(as.integer(l$rep)))
l = l[grepl("fullybayes", l$analysis) & grepl("nu|tau|theta|sigma", l$type) & l$libraries == 8,]
l$parameter = as.factor(as.character(l$parameter))
l$type = as.factor(as.character(l$type))
l$simulation = gsub("priors", "", as.character(l$simulation))
l$analysis = gsub("fullybayes\\+", "", as.character(l$analysis))
for(n in c("simulation", "analysis")) l[[n]] = ordered(l[[n]], levels = priors_analyses())
l$simulation = ordered(l$simulation, labels = paste(levels(l$simulation), "sim"))
l$analysis = ordered(l$analysis,  labels = paste(levels(l$analysis), "analysis"))
x = ddply(l, "parameter", function(d){
  pl = ggplot(d) + 
    geom_segment(data = d[d$level == 0.5,], mapping = aes_string(x = "rep", xend = "rep", y = "lower", yend = "upper"), size = 2) + 
    geom_segment(data = d[d$level == 0.95,], mapping = aes_string(x = "rep", xend = "rep", y = "lower", yend = "upper")) + 
    facet_grid(as.formula("simulation~analysis"), scales = "free") + 
    geom_abline(intercept = d$truth[1], slope = 0) + 
    xlab("simulated dataset") +
    ylab("credible interval") + 
    mytheme_pub() + theme(legend.position = "none") +
    scale_linetype_manual(values = c("TRUE" = 1, "FALSE" = 2))
  for(extn in extns)
    ggsave(paste0(dir_priorshypercoverage, "fig-priorshypercoverage-", d$parameter[1], ".", extn), pl, height = 6, width = 6, dpi = 1200)
})

# credible interval info
l = as.data.frame(readRDS("priors_analyze/ci/ci.rds"))
l$analysis = gsub("normalnormal", "normal", l$analysis)
l$analysis = gsub("normalLaplace", "Laplace", l$analysis)
l$analysis = gsub("normalt", "t", l$analysis)
l$rep = ordered(l$rep, levels = 1:max(as.integer(l$rep)))
l = l[grep("fullybayes", l$analysis) & l$type == "beta[2]" & l$libraries == 8,]

# fig:priorsbetarates
dir_priorsbetarates = newdir(paste0(dir, "fig-priorsbetarates"))
level = 0.95
l0 = l[grepl("beta", l$parameter) & l$level == level,]
l0$type = gsub("\\[", "[list(g", l0$type)
l0$type = gsub("]", ")]", l0$type)
l1 = ddply(l0, c("type", "rep", "analysis", "simulation"), function(x){
  data.frame(simulation = x$simulation[1], analysis = x$analysis[1], type = x$type[1], rep = x$rep[1], coverage = mean(x$cover))
})
l1$simulation = gsub("priors", "", as.character(l1$simulation))
l1$analysis = gsub("fullybayes\\+", "", as.character(l1$analysis))
for(n in c("simulation", "analysis")) l1[[n]] = ordered(l1[[n]], levels = priors_analyses())
l1$simulation = ordered(l1$simulation, labels = paste(levels(l1$simulation), "sim"))
l1$analysis = ordered(l1$analysis,  labels = paste(levels(l1$analysis), "analysis"))
pl = ggplot(l1) +
  geom_point(aes_string(x = "rep", y = "coverage"), size = 0.75) + 
  geom_hline(yintercept = level) + 
  facet_grid(as.formula("simulation~analysis")) +
  xlab("simulated dataset") +
  mytheme_pub() + theme(strip.text = element_text(size = 14))
for(extn in extns)
  ggsave(paste0(dir_priorsbetarates, "fig-priorsbetarates.", extn), pl, height = 8, width = 6, dpi = 1200)

# fig:priorsbetacred
dir_priorsbetacred = newdir(paste0(dir, "fig-priorsbetacred"))
level = 0.95
l0 = l[grepl("beta", l$parameter) & l$level == level & l$rep == 1 & !l$cover,]
l0$type = gsub("\\[", "[list(g", l0$type)
l0$type = gsub("]", ")]", l0$type)
l0 = l0[order(l0$truth),]
l1 = ddply(l0, c("analysis", "simulation", "type"), function(x){
  x$interval = 1:dim(x)[1]
  x
})
l1$simulation = gsub("priors", "", as.character(l1$simulation))
l1$analysis = gsub("fullybayes\\+", "", as.character(l1$analysis))
for(n in c("simulation", "analysis")) l1[[n]] = ordered(l1[[n]], levels = priors_analyses())
l1$simulation = ordered(l1$simulation, labels = paste(levels(l1$simulation), "sim"))
l1$analysis = ordered(l1$analysis,  labels = paste(levels(l1$analysis), "analysis"))
pl = ggplot(l1) +
  geom_segment(aes_string(x = "interval", xend = "interval", y = "lower", yend = "upper"), color = gray) +
  geom_point(aes_string(x = "interval", y = "truth"), color = "black", size = I(0.5)) + 
  geom_hline(yintercept = 0) + 
  facet_grid(as.formula("simulation~analysis"), scales = "free") +
  xlab("credible interval") + ylab("parameter value") + 
  mytheme_pub() + theme(strip.text.x = element_text(size = 14))
for(extn in extns)
  ggsave(paste0(dir_priorsbetacred, "fig-priorsbetacred.", extn), pl, height = 8, width = 7, dpi = 1200)

# fig:priorsbetacoveragetrend
dir_priorsbetacoveragetrend = newdir(paste0(dir, "fig-priorsbetacoveragetrend"))
level = 0.95
l0 = l[grepl("beta", l$parameter) & l$level == level,]
l0$type = gsub("\\[", "[list(g", l0$type)
l0$type = gsub("]", ")]", l0$type)
l1 = ddply(l0, c("simulation", "analysis", "rep", "type"), function(z){
  k = ksmooth(x = z$truth, y = z$cover, bandwidth = 4*sd(z$truth))
  fn = stepfun(x = k$x, y = c(0, k$y))
  xs = seq(from = min(k$x), to = max(k$x), length.out = 4e2)
  ys = fn(xs)
  data.frame(simulation = z$simulation[1], analysis = z$analysis[1], truth = xs, cover = ys, type = z$type[1], rep = z$rep[1])
}, .progress = "text")
l1$simulation = gsub("priors", "", as.character(l1$simulation))
l1$analysis = gsub("fullybayes\\+", "", as.character(l1$analysis))
for(n in c("simulation", "analysis")) l1[[n]] = ordered(l1[[n]], levels = priors_analyses())
l1$simulation = ordered(l1$simulation, labels = paste(levels(l1$simulation), "sim"))
l1$analysis = ordered(l1$analysis,  labels = paste(levels(l1$analysis), "analysis"))
pl = ggplot(l1) + 
  geom_line(aes_string(x = "truth", y = "cover", group = "rep"), alpha = 0.5) + 
  geom_abline(slope = 0, intercept = level, linetype = "dotted") +
  geom_vline(xintercept = 0) + 
  facet_grid(as.formula("simulation~analysis"), scales = "free") +
  xlab("true parameter value") +
  ylab("local coverage rate") +
  mytheme_pub() + theme(strip.text = element_text(size = 14))
for(extn in extns[!grepl("ps", extns)])
  ggsave(paste0(dir_priorsbetacoveragetrend, "fig-priorsbetacoveragetrend.", extn), pl, height = 6, width = 8, dpi = 1200)
for(extn in extns[grepl("ps", extns)])
  ggsave(paste0(dir_priorsbetacoveragetrend, "fig-priorsbetacoveragetrend.", extn), pl, device=cairo_ps,
 height = 6, width = 7, dpi = 1200)

# fig:priorsmodelcalibration
dir_priorsmodelcalibration = newdir(paste0(dir, "fig-priorsmodelcalibration"))
df = readRDS("priors_analyze/calibration_long/calibration_long.rds")
df = df[df$heterosis == "high" & df$libraries == 8,]
df$simulation = gsub("priors", "", as.character(df$simulation))
df$analysis = gsub("fullybayes\\+", "", as.character(df$analysis))
for(n in c("simulation", "analysis")) df[[n]] = ordered(df[[n]], levels = priors_analyses())
df$simulation = ordered(df$simulation, labels = paste(levels(df$simulation), "sim"))
df$analysis = ordered(df$analysis,  labels = paste(levels(df$analysis), "analysis"))
pl = ggplot(df) +
  geom_line(aes_string(x = "probability", y = "proportion", group = "file"), color = "black") + 
  geom_abline(slope = 1, intercept = 0, linetype = 2) + 
  facet_grid(as.formula("simulation~analysis")) + xlab("probability") + ylab("proportion") +
  mytheme_pub() + theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_priorsmodelcalibration, "fig-priorsmodelcalibration.", extn), pl, height = 8, width = 6, dpi = 1200)

# fig:priorscomparecalerror
dir_priorscomparecalerror = newdir(paste0(dir, "fig-priorscomparecalerror"))
df = readRDS("priors_analyze/calibration_long/calibration_long.rds")
df$error = abs(df$proportion - df$probability)
d = ddply(df, c("file", "heterosis"), function(x){
  x$meanerror = trapz(x = x$probability, y = x$error)
  x[1,]
})
d = d[d$heterosis == "high" & d$libraries == 8,]
d$simulation = gsub("priors", "", as.character(d$simulation))
d$analysis = gsub("fullybayes\\+", "", as.character(d$analysis))
for(n in c("simulation", "analysis")) d[[n]] = ordered(d[[n]], levels = priors_analyses())
d$simulation = ordered(d$simulation, labels = paste(levels(d$simulation), "sim"))
pl = ggplot(d) + 
  geom_line(aes_string(x = "analysis", y = "meanerror", group = "rep"), color = "black") +
  geom_point(aes_string(x = "analysis", y = "meanerror"), color = "black") +
  facet_grid(as.formula("~simulation"), scales = "fixed") +
  xlab("analysis") + 
  ylab("calibration error") +
  labs(pch = "N") +
  mytheme_pub() +
  theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_priorscomparecalerror, "fig-priorscomparecalerror.", extn), pl, height = 5, width = 6, dpi = 1200)

# fig:priorsroc
dir_priorsroc = newdir(paste0(dir, "fig-priorsroc"))
d = readRDS("priors_analyze/roc_long/roc_long.rds")
d = d[d$heterosis == "high" & d$libraries == 8,]
d$simulation = gsub("priors", "", as.character(d$simulation))
d$analysis = gsub("fullybayes\\+", "", as.character(d$analysis))
for(n in c("simulation", "analysis")) d[[n]] = ordered(d[[n]], levels = priors_analyses())
d$simulation = ordered(d$simulation, labels = paste(levels(d$simulation), "sim"))
d$analysis = ordered(d$analysis,  labels = paste(levels(d$analysis), "analysis"))
pl = ggplot(d) + 
  geom_line(aes_string(x = "fpr", y = "tpr", group = "file")) +
  facet_grid(as.formula("simulation~analysis")) +
  xlab("false positive rate") + 
  ylab("true positive rate") +
  scale_color_manual(name = "analysis", labels = levels(d$analysis), values = mycolors[1:length(levels(d$analysis))]) +
  scale_linetype_manual(name = "analysis", labels = levels(d$analysis), values = 1:length(levels(d$analysis))) +
  mytheme_pub() +
  theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_priorsroc, "fig-priorsroc.", extn), pl, height = 6, width = 6, dpi = 1200)

# fig:priorsauc
dir_priorsauc = newdir(paste0(dir, "fig-priorsauc"))
d = readRDS("priors_analyze/auc_long/auc_long.rds")
d = d[d$heterosis == "high" & d$libraries == 8,]
d$simulation = gsub("priors", "", as.character(d$simulation))
d$analysis = gsub("fullybayes\\+", "", as.character(d$analysis))
for(n in c("simulation", "analysis")) d[[n]] = ordered(d[[n]], levels = priors_analyses())
d$simulation = ordered(d$simulation, labels = paste(levels(d$simulation), "sim"))
pl = ggplot(d) + 
  geom_line(aes_string(x = "analysis", y = "auc_1", group = "rep"), color = "black") +
  geom_point(aes_string(x = "analysis", y = "auc_1"), color = "black") +
  facet_grid(as.formula("~simulation"), scales = "fixed") +
  xlab("analysis") + 
  ylab("area under ROC curve") +
  labs(pch = "N") +
  mytheme_pub() +
  theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_priorsauc, "fig-priorsauc.", extn), pl, height = 5, width = 6, dpi = 1200)







## compare with case study

# credible interval info
l = as.data.frame(readRDS("coverage_analyze/ci/ci.rds"))
l$rep = ordered(l$rep, levels = 1:max(as.integer(l$rep)))
l = l[grep("fullybayes", l$analysis),]

# fig:betarates
dir_betarates = newdir(paste0(dir, "fig-betarates"))
level = 0.95
l0 = l[grepl("beta", l$parameter) & l$level == level,]
l0$type = gsub("\\[", "[list(g", l0$type)
l0$type = gsub("]", ")]", l0$type)
l1 = ddply(l0, c("type", "rep", "analysis"), function(x){
  data.frame(analysis = x$analysis[1], type = x$type[1], rep = x$rep[1], coverage = mean(x$cover))
})
l1$analysis = ordered(gsub("fullybayes\\+", "", as.character(l1$analysis)), levels = priors_analyses())
pl = ggplot(l1) +
  geom_point(aes_string(x = "rep", y = "coverage"), size = 0.75) + 
  geom_hline(yintercept = level) + 
  facet_grid(as.formula("type~analysis"), labeller = label_parsed) +
  xlab("simulated dataset") +
  mytheme_pub() + theme(strip.text = element_text(size = 14))
for(extn in extns)
  ggsave(paste0(dir_betarates, "fig-betarates.", extn), pl, height = 8, width = 6, dpi = 1200)

# fig:betacred
dir_betacred = newdir(paste0(dir, "fig-betacred"))
level = 0.95
l0 = l[grepl("beta", l$parameter) & l$level == level & l$rep == 1 & !l$cover,]
l0$type = gsub("\\[", "[list(g", l0$type)
l0$type = gsub("]", ")]", l0$type)
l0 = l0[order(l0$truth),]
l0 = ddply(l0, c("analysis", "type"), function(x){
  x$interval = 1:dim(x)[1]
  x
})
l0$analysis = ordered(gsub("fullybayes\\+", "", as.character(l0$analysis)), levels = priors_analyses())
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
  facet_grid(as.formula("type~analysis"), scales = "free", labeller = label_parsed) +
  xlab("credible interval") + ylab("parameter value") + 
  mytheme_pub() + theme(strip.text.x = element_text(size = 14))
for(extn in extns)
  ggsave(paste0(dir_betacred, "fig-betacred.", extn), pl, height = 8, width = 7, dpi = 1200)

# fig:betacoveragetrend
dir_betacoveragetrend = newdir(paste0(dir, "fig-betacoveragetrend"))
level = 0.95
l0 = l[grepl("beta", l$parameter) & l$level == level,]
l0$type = gsub("\\[", "[list(g", l0$type)
l0$type = gsub("]", ")]", l0$type)
l1 = ddply(l0, c("analysis", "rep", "type"), function(z){
  k = ksmooth(x = z$truth, y = z$cover, bandwidth = 4*sd(z$truth))
  fn = stepfun(x = k$x, y = c(0, k$y))
  xs = seq(from = min(k$x), to = max(k$x), length.out = 4e2)
  ys = fn(xs)
  data.frame(analysis = z$analysis[1], truth = xs, cover = ys, type = z$type[1], rep = z$rep[1])
}, .progress = "text")
l1$analysis = ordered(gsub("fullybayes\\+", "", as.character(l1$analysis)), levels = priors_analyses())
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
  facet_grid(as.formula("analysis~type"), scales = "free_x", labeller = label_parsed) +
  xlab("true parameter value") +
  ylab("local coverage rate") +
  mytheme_pub() + theme(strip.text = element_text(size = 14))
for(extn in extns[!grepl("ps", extns)])
  ggsave(paste0(dir_betacoveragetrend, "fig-betacoveragetrend.", extn), pl, height = 6, width = 8, dpi = 1200)
for(extn in extns[grepl("ps", extns)])
  ggsave(paste0(dir_betacoveragetrend, "fig-betacoveragetrend.", extn), pl, device=cairo_ps,
 height = 6, width = 7, dpi = 1200)

# fig:modelcalibration
dir_modelcalibration = newdir(paste0(dir, "fig-modelcalibration"))
df = readRDS("coverage_analyze/calibration_long/calibration_long.rds")
df$heterosis = relevel_heterosis(df$heterosis)
df$analysis = ordered(gsub("fullybayes\\+", "", as.character(df$analysis)), levels = priors_analyses())
pl = ggplot(df) +
  geom_line(aes_string(x = "probability", y = "proportion", group = "file"), color = "black") + 
  geom_abline(slope = 1, intercept = 0, linetype = 2) + 
  facet_grid(as.formula("heterosis~analysis")) + xlab("probability") + ylab("proportion") +
  mytheme_pub() + theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_modelcalibration, "fig-modelcalibration.", extn), pl, height = 8, width = 6, dpi = 1200)

# fig:roc16 and fig:roc32
for(N in c(16, 32)){
  dir_roc = newdir(paste0(dir, "fig-roc", N))
  d = readRDS("comparison_analyze/roc_long/roc_long.rds")
  d = d[d$libraries == N,]
  d = priors_clean_df(d)
  pl = ggplot(d) + 
    geom_line(aes_string(x = "fpr", y = "tpr", group = "file", color = "analysis", linetype = "analysis")) +
    facet_grid(as.formula("simulation~heterosis")) +
    xlab("false positive rate") + 
    ylab("true positive rate") +
    scale_color_manual(name = "analysis", labels = levels(d$analysis), values = mycolors[1:length(levels(d$analysis))]) +
    scale_linetype_manual(name = "analysis", labels = levels(d$analysis), values = 1:length(levels(d$analysis))) +
    mytheme_pub() +
    theme(axis.text.x = element_text(angle = -80, hjust = 0))
  for(extn in extns)
    ggsave(paste0(dir_roc, "fig-roc", N, ".", extn), pl, height = 8, width = 10, dpi = 1200)
}

# fig:auc
dir_auc = newdir(paste0(dir, "fig-auc"))
d = readRDS("comparison_analyze/auc_long/auc_long.rds")
d = priors_clean_df(d)
pl = ggplot(d) + 
  geom_line(aes_string(x = "analysis", y = "auc_1", group = "libraries"), color = "black") +
  geom_point(aes_string(x = "analysis", y = "auc_1", pch = "libraries"), color = "black") +
  facet_grid(as.formula("simulation~heterosis"), scales = "fixed") +
  xlab("Analysis") + 
  ylab("Area under ROC curve") +
  labs(pch = "N") +
  mytheme_pub() +
  theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_auc, "fig-auc.", extn), pl, height = 8, width = 10, dpi = 1200)

# fig:comparecal16 and fig:comparecal32
for(N in c(16, 32)){
  dir_comparecal = newdir(paste0(dir, "fig-comparecal", N))
  d = readRDS("comparison_analyze/calibration_long/calibration_long.rds")
  d = d[d$libraries == N,]
  d = priors_clean_df(d)
  pl = ggplot(d) + 
    geom_abline(slope = 1, intercept = 0, color = gray) +
    geom_line(aes_string(x = "probability", y = "proportion", group = "file", color = "analysis", linetype = "analysis")) +
    facet_grid(as.formula("simulation~heterosis")) +
    xlab("probability") + 
    ylab("proportion") +
    scale_color_manual(name = "analysis", labels = levels(d$analysis), values = mycolors[1:length(levels(d$analysis))]) +
    scale_linetype_manual(name = "analysis", labels = levels(d$analysis), values = 1:length(levels(d$analysis))) +
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
d = priors_clean_df(d)
pl = ggplot(d) + 
  geom_line(aes_string(x = "analysis", y = "meanerror", group = "libraries"), color = "black") +
  geom_point(aes_string(x = "analysis", y = "meanerror", pch = "libraries"), color = "black") +
  facet_grid(as.formula("simulation~heterosis"), scales = "fixed") +
  xlab("analysis") + 
  ylab("calibration error") +
  labs(pch = "N") +
  mytheme_pub() +
  theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_comparecalerror, "fig-comparecalerror.", extn), pl, height = 8, width = 10, dpi = 1200)

# paschold data analysis
l = readRDS("real_mcmc/paschold_39656_16_1.rds")
as = list(normal = l$analyses[["fullybayes+normal"]],
  Laplace = l$analyses[["fullybayes+Laplace"]],
  t = l$analyses[["fullybayes+t"]])
m = lapply(as, function(a) mcmc_samples(a$chains))
e = lapply(as, function(a) estimates(a$chains, level = 0.95))

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
d = NULL
for(prior in priors_analyses()){
  m_hyper = m[[prior]][,c("nu", "tau", paste0("theta_", 1:5), paste0("sigmaSquared_", 1:5))]
  cn = colnames(m_hyper)
  for(i in 1:5) cn = gsub(paste0("_", i), paste0("\\[", i, "\\]"), cn)
  cn = gsub("sigmaSquared", "sigma", cn)
  cn[grep("sigma", cn)] = paste0(cn[grep("sigma", cn)], "^2")
  colnames(m_hyper) = cn
  d0 = melt(m_hyper, id.vars = NULL)
  d0$analysis = prior
  d = rbind(d, d0)
}
d$analysis = ordered(d$analysis, levels = priors_analyses())
for(v in unique(d$variable)){ # c("nu|tau", "theta", "sigma")
  pl = ggplot(d[d$variable == v,]) + 
    stat_density(aes_string(x = "value", y = "..density.."), color = gray, fill = gray) + 
    facet_grid(as.formula("analysis~variable"), scales = "free", labeller = label_parsed) + 
    mytheme_pub() +
    theme(strip.text.x = element_text(size = 14), axis.text.x = element_text(angle = -80, hjust = 0)) + 
    xlab("parameter value") + 
    ylab("density")
  for(extn in extns)
    ggsave(paste0(dir_hyperhist, "fig-hyperhist-", v, ".", extn), pl, height = 8, width = 6, dpi = 1200)
}

# loop over type of hierarchical distribution
l = readRDS("real_mcmc/paschold_39656_16_1.rds")
for(prior in priors_analyses()){

a = l$analyses[[paste0("fullybayes+", prior)]]
m = mcmc_samples(a$chains)
e = estimates(a$chains, level = 0.95)

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
  theme(strip.text.x = element_text(size = 14), legend.position = c(0.875, 0.1)) + #, axis.text.x = element_text(angle = -80, hjust = 0)) + 
  xlab("parameter value") + 
  ylab("density") + 
  labs(linetype = "95% credible interval")
for(extn in extns)
  ggsave(paste0(dir_betahist, "fig-betahist-", prior, ".", extn), pl, height = 10, width = 10, dpi = 1200)

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
  theme(axis.text.x = element_text(angle = -80, hjust = 0), strip.text.x = element_text(size = 14))
for(extn in extns)
  ggsave(paste0(dir_betapostmeanhist, "fig-betapostmeanhist-", prior, ".", extn), pl, height = 6, width = 8, dpi = 1200)

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
  theme(axis.text.x = element_text(angle = -80, hjust = 0), strip.text.x = element_text(size = 14))
for(extn in extns)
  ggsave(paste0(dir_pascholdcred, "fig-pascholdcred-", prior, ".", extn), pl, height = 6, width = 8, dpi = 1200)

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
  mytheme_pub()
for(extn in extns)
  ggsave(paste0(dir_probhist, "fig-probhist-", prior, ".", extn), pl, height = 6, width = 8, dpi = 1200)
}
}
