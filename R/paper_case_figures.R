#' @title Function \code{paper_case_figures}
#' @description Reproduce the figures and tables of the case study paper
#' @export
paper_case_figures = function(){

# library(fbseqStudies); library(reshape2); library(plyr); library(ggthemes)
# setwd("~/home/work/projects/thesis_data/results")

# control parms
extns = c("pdf", "ps", "eps", "tiff")

# credible interval info
l = as.data.frame(readRDS("coverage_analyze/ci/ci.rds"))
l$rep = ordered(l$rep, levels = 1:10)
dir = newdir("case_study_paper_figures")

# fig:hypercoverage
dir_hypercoverage = newdir(paste0(dir, "fig_hypercoverage"))
level = 0.5
l0 = l[!grepl("beta", l$parameter) & l$level == level,]
l1 = ddply(l0, c("type", "simulation", "libraries", "analysis"), function(x){
  if(mean(x$cover) >= level) return(NULL)
  x
})
pl = ggplot(l1) + 
  geom_segment(aes_string(x = "rep", xend = "rep", y = "lower", yend = "upper")) + 
  facet_wrap(as.formula("~type"), scales = "free_y", labeller = label_parsed) + 
  geom_abline(aes(slope = 0, intercept = l1$truth)) + 
  xlab("simulated dataset") +
  ylab("credible interval") + 
  theme_few() + theme(strip.text.x = element_text(size = 14))
for(extn in extns)
  ggsave(paste0(dir_hypercoverage, "fig_hypercoverage.", extn), pl, height = 4, width = 5, dpi = 1200)

# fig:betarates
dir_betarates = newdir(paste0(dir, "fig_betarates"))
level = 0.95
l0 = l[grepl("beta", l$parameter) & l$level == level,]
l1 = ddply(l0, c("type", "rep"), function(x){
  data.frame(type = x$type[1], rep = x$rep[1], coverage = mean(x$cover))
})
pl = ggplot(l1) +
  geom_point(aes_string(x = "rep", y = "coverage")) + 
  geom_hline(yintercept = level) + 
  facet_wrap(as.formula("~type"), labeller = label_parsed) +
  xlab("simulated dataset") +
  theme_few() + theme(strip.text.x = element_text(size = 14))
for(extn in extns)
  ggsave(paste0(dir_betarates, "fig_betarates.", extn), pl, height = 4, width = 5, dpi = 1200)

# fig:hphcalibration
dir_hphcalibration = newdir(paste0(dir, "fig_hphcalibration"))
df = ggplot2_df("~/home/work/projects/thesis_data/results/coverage_analyze/calibration")
df = df[df$heterosis == "high-parent_hybrid1",]
pl = ggplot(df) +
  geom_line(aes_string(x = "probability", y = "proportion", group = "file", linetype = "analysis"), color = "black") + 
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  theme_few() + theme(legend.position = "none")
for(extn in extns)
  ggsave(paste0(dir_hphcalibration, "fig_hphcalibration.", extn), pl, height = 4, width = 4, dpi = 1200)

# fig:betacred
dir_betacred = newdir(paste0(dir, "fig_betacred"))
level = 0.95
l0 = l[grepl("beta", l$parameter) & l$level == level & l$rep == 1 & !l$cover,]
l0 = l0[order(l0$truth),]
l0 = ddply(l0, "type", function(x){
  x$interval = 1:dim(x)[1]
  x
})
pl = ggplot(l0) +
  geom_segment(aes_string(x = "interval", xend = "interval", y = "lower", yend = "upper"), color = "darkGray") +
  geom_point(aes_string(x = "interval", y = "truth"), color = "black", size = I(0.5)) + 
  facet_wrap(as.formula("~type"), scales = "free", labeller = label_parsed) +
  xlab("credible interval") + ylab("parameter value") + 
  theme_few() + theme(strip.text.x = element_text(size = 14))
for(extn in extns)
  ggsave(paste0(dir_betacred, "fig_betacred.", extn), pl, height = 6, width = 7, dpi = 1200)

# fig:betacoveragetrend
dir_betacoveragetrend = newdir(paste0(dir, "fig_betacoveragetrend"))
level = 0.95
l0 = l[grepl("beta", l$parameter) & l$level == level,]
l1 = ddply(l0, c("rep", "type"), function(z){
  k = ksmooth(x = z$truth, y = z$cover, bandwidth = 0.1)
  fn = stepfun(x = k$x, y = c(0, k$y))
  xs = seq(from = min(k$x), to = max(k$x), length.out = 4e2)
  ys = fn(xs)
  data.frame(truth = xs, cover = ys, type = z$type[1], rep = z$rep[[1]])
}, .progress = "text")
pl = ggplot(l1) + 
  geom_line(aes_string(x = "truth", y = "cover", group = "rep")) + 
  geom_abline(slope = 0, intercept = level) +
  facet_wrap(as.formula("~type"), scales = "free_x", labeller = label_parsed) +
  xlab("parameter value") +
  ylab("coverage") +
  theme_few() + theme(strip.text.x = element_text(size = 14))
for(extn in extns)
  ggsave(paste0(dir_betacoveragetrend, "fig_betacoveragetrend.", extn), pl, height = 6, width = 7, dpi = 1200)


# fig:roc

# fig:auc

# fig:compare-cal

# fig:hyperhist

# fig:betahist

# fig:gammahist

# fig:cred

# fig:probhist

# tables of interesting genes

}
