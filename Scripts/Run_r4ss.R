require(r4ss); require(this.path)

dir <- file.path(this.path::here(.. = 1), "Model","01_Updated LH")


report <- r4ss::SS_output(dir,verbose = FALSE, printstats = FALSE)
r4ss::SS_plots(report, dir = dir, pdf=F, png=T)


r4ss::SS_tune_comps(report, dir = dir)

report$Dirichlet_Multinomial_pars



