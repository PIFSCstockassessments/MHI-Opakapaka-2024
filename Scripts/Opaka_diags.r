## Code to run diagnostics for opakapaka SS model 
library(r4ss)
library(ss3diags)

rep_base <- SS_output(dir = file.path(main_dir, "Model", "19_Base"))
SS_plots(rep_base)

r0vec <- seq(4.5,6.5,by=0.2)

profile(dir = file.path(main_dir, "Model", "19_Base", "profile"), 
newctlfile = "control_modified.ss",
string = "SR_LN", 
exe = "ss_opt_win", 
profilevec = r0vec)
promods <- SSgetoutput(dirvec = file.path(main_dir, "Model", "19_Base", "profile"), keyvec = 1:length(r0vec))
promods.sum <- SSsummarize(promods)
SSplotProfile(promods.sum, profile.string = "SR_LN",   profile.label = "SR_LN(R0)", print = TRUE, plotdir = file.path(main_dir, "Model", "19_Base"))
PinerPlot(promods.sum, component = "Length_like")
PinerPlot(promods.sum, component = "Surv_like", main = "Changes in survey likelihoods by fleet", print = TRUE, plotdir = file.path(main_dir, "Model", "19_Base"))

retro(dir = file.path(main_dir, "Model", "19_Base"), exe = "ss_opt_win")

retromods <- SSgetoutput(dirvec = c(file.path(main_dir, "Model", "19_Base",  "retrospectives", "retro0"),
file.path(main_dir, "Model", "19_Base",  "retrospectives", "retro-1"),
file.path(main_dir, "Model", "19_Base",  "retrospectives", "retro-2"),
file.path(main_dir, "Model", "19_Base",  "retrospectives", "retro-3"),
file.path(main_dir, "Model", "19_Base",  "retrospectives", "retro-4"),
file.path(main_dir, "Model", "19_Base",  "retrospectives", "retro-5")
))
retromods.sum <- SSsummarize(retromods)
SSplotRetro(retromods.sum, print_plot = TRUE, use_png = TRUE, subplots = "SSB", plotdir = file.path(main_dir, "Model", "19_Base"))
SSplotRetro(retromods.sum, print_plot = TRUE, use_png = TRUE, subplots = "F", plotdir = file.path(main_dir, "Model", "19_Base"))
compmods <- SSgetoutput(dirvec = c(file.path(main_dir, "Model", "14_Base"),
    file.path(main_dir, "Model", "15_frs_weightcomp_lambda0"),
    file.path(main_dir, "Model", "15b_subset_weightcomps"),
    file.path(main_dir, "Model", "15c_rm_outlier_weights"),
    file.path(main_dir, "Model", "15d_rm_outlier_cpue")
    ))
compmods.sum <- SSsummarize(compmods)
SSplotComparisons(compmods.sum, legendlabels = c("With Weight Comps", "No Weight Comps", "Subset Weights", "Rm High Weights", "Rm Low CPUE"))

rep <- SS_output(file.path(main_dir, "Model", "19_Base"))
SS_plots(rep)
modsum <- SSsummarize(list(rep_base, rep, rep_base_new))
SSplotComparisons(modsum)
SSMethod.TA1.8(rep, type = "size", fleet = 1)
library(ss3diags)
SSplotRunstest(rep, add = TRUE)
SSplotJABBAres(rep)

modeldir <- tail(dir(system.file("extdata", package = "r4ss"), full.names = TRUE), 1)
numjitter <- 25
jit.likes <- jitter(
  dir = file.path(main_dir, "Model", "19_Base", "jitter"), Njitter = 50,
  jitter_fraction = 0.1, exe = "ss_opt_win"
)

jitmods <- SSgetoutput(dirvec = file.path(main_dir, "Model", "19_Base", "jitter"), keyvec = 1:25)
jitsum <- SSsummarize(jitmods)
rep$likelihoods_used
jitsum$likelihoods
plot(x = seq(1,25,by=1), y = jitsum$likelihoods[1,-26], pch = 16, xlab = "jitter run", ylab = "Total Likelihood")
abline(h = 3.10408e+03)
