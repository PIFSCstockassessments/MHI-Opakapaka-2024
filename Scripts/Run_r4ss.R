#remotes::install_github("r4ss/r4ss")

require(r4ss); require(this.path)

dir         <- file.path(this.path::here(.. = 1), "Model","12b_GrowthAndrewsRefitCV065")

report <- r4ss::SS_output(dir,verbose = FALSE, printstats = FALSE)
r4ss::SS_plots(report, dir = dir, pdf=F, png=T)


# Run R0 profile
profile.vec <- seq(4.5,6.3,0.3)
profile.par <- "SR_LN(R0)"

#Copy files
dir.profile <- file.path(dir,"R0_profile")
r4ss::copy_SS_inputs(dir.old = dir, dir.new = dir.profile, create.dir = TRUE,
                     overwrite = TRUE, recursive = TRUE, use_ss_new = TRUE,
                     copy_exe = TRUE, copy_par = FALSE, dir.exe = dir, verbose = TRUE)

# Make changes to starter file
starter <- r4ss::SS_readstarter(file.path(dir.profile, "starter.ss"))
starter[["ctlfile"]] <- "control_modified.ss"
# make sure the prior likelihood is calculated
# for non-estimated quantities
starter[["prior_like"]] <- 1
r4ss::SS_writestarter(starter, dir = dir.profile, overwrite = TRUE)

profile <- r4ss::profile(
  dir = dir.profile, 
  exe = "ss_opt_win",
  oldctlfile = "control.ss",
  newctlfile = "control_modified.ss",
  string = profile.par,
  profilevec = profile.vec
)

profilemodels  <- SSgetoutput(dirvec = dir.profile, keyvec = 1:length(profile.vec))
profilesummary <- SSsummarize(profilemodels)
SSplotProfile(profilesummary, # summary object
              profile.string = "SR_LN", # substring of profile parameter
              profile.label = "R0"
)

#r4ss::SS_tune_comps(report, dir = dir)
#report$Dirichlet_Multinomial_pars



