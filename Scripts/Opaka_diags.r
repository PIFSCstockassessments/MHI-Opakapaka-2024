## Code to run diagnostics for opakapaka SS model 
library(r4ss)
library(ss3diags)
library(tidyverse)

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
SSplotProfile(promods.sum, profile.string = "SR_LN",   profile.label = "SR_LN(R0)", print = TRUE, plotdir = file.path(main_dir, "Model", "19_Base"), col = cols, ptsize = 18, pheight = 8, pwidth = 10)
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
SSplotRetro(retromods.sum, print_plot = TRUE, use_png = TRUE, subplots = "SSB", plotdir = file.path(main_dir, "Model", "19_Base"), col = cols, pwidth = 10, pheight = 7, punits = "in", ptsize = 18)
SSplotRetro(retromods.sum, print_plot = TRUE, use_png = TRUE, subplots = "F", plotdir = file.path(main_dir, "Model", "19_Base"), col = cols, pwidth = 10, pheight = 7, punits = "in", ptsize = 18)
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


jit.likes <- jitter(
  dir = file.path(main_dir, "Model", "19_Base", "jitter"), Njitter = 50,
  jitter_fraction = 0.1, exe = "ss_opt_win"
)

jitmods <- SSgetoutput(dirvec = file.path(main_dir, "Model", "19_Base", "jitter"), keyvec = 1:50)
jitsum <- SSsummarize(jitmods)
rep$likelihoods_used
jitsum$likelihoods
png(filename = file.path(main_dir, "Model", "19_Base", "jitter.png"),  width = 8, height = 8, units = "in", res = 300, pointsize = 18)
plot(x = seq(1,50,by=1), y = jitsum$likelihoods[1,-51], pch = 16, xlab = "Jitter Run", ylab = "Total Likelihood", col = "#0085CA")
abline(h = rep_base$likelihoods_used$values[1], col = "#FF8400", lwd = 2)
dev.off()

### plots for presentation
cols <- JABBA::ss3col(7,1)
cols <- c("#0085CA", "#1EBEC7", "#76BC21", "#737BE6", "#FF8400", "#DB2207")

SSplotRunstest(rep_base, subplots = "cpue", print_plot = TRUE, plotdir = file.path(main_dir, "Model", "19_Base", "plots"), ptsize = 14)
SSplotJABBAres(rep_base, subplots = "cpue", print_plot = TRUE, plotdir = file.path(main_dir, "Model", "19_Base", "plots"), ptsize = 14, pwidth = 8, pheight = 5, punits = "in", col = cols)

load(file = file.path(main_dir, "..", "Deep-7-Benchmark-2024", "Final_Model_Runs", "003_Opaka_Base", "fit_test.Rdata"))
assign("fit_paka", fit_test)
load(file = file.path(main_dir, "..", "Deep-7-Benchmark-2024", "Final_Model_Runs", "001_Base_case", "fit_test.Rdata"))


ss3_b <- rep_base$timeseries %>% 
  select(Yr, Bio_all) %>% 
  filter(Yr >= 1949 & Yr <= 2023) %>% 
  mutate(Model = "'Opakapaka SS3",
         Bio_lbs = Bio_all * 2204.62)
deep7_jabba <- fit_test$timeseries[,,"B"] %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Yr") %>% 
  mutate(Yr = as.numeric(Yr), Bio_all = mu, Model = "Deep 7 JABBA", Bio_lbs = Bio_all * 10e5,
         lci = NA, uci = NA) %>% 
  select(Yr, Bio_all, Model, Bio_lbs, lci, uci) 

fit_paka$timeseries[,,"B"] %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Yr") %>% 
  mutate(Yr = as.numeric(Yr), Bio_all = mu, Model = "'Opakapaka JABBA", Bio_lbs = Bio_all * 10e5) %>% 
  select(Yr, Bio_all, Model, Bio_lbs, lci, uci) %>% 
  bind_rows(ss3_b) %>% 
  #bind_rows(deep7_jabba) %>% 
  mutate(Bio_lbs = Bio_lbs/1000000,
         lci = ifelse(is.na(lci), Bio_lbs, lci),
         uci = ifelse(is.na(uci), Bio_lbs, uci)) %>% 
  ggplot(aes(x = Yr, y = Bio_lbs, group = Model)) + 
  geom_ribbon(aes(x = Yr, ymin = lci, ymax = uci, fill = Model), alpha = .35, show.legend = F) +
  geom_line(aes(color = Model, linetype = Model), linewidth = 1.25) + 
  scale_color_manual(values = cols[c(1,5)]) +
  scale_fill_manual(values = cols[c(1,5)]) +
  theme_classic() +
  theme(text = element_text(size=24), legend.position = "bottom")+
  labs(x = "Year", y = "Exploitable Biomass (million lbs)") 
ggsave(filename = file.path(main_dir, "Model", "19_Base", "plots", "opaka_biomass_comparisons.png"), width = 10, height = 6, units = "in")

ss3_f <- rep_base$derived_quants %>% 
  filter(str_detect(Label, "F_1|F_2")) %>% 
  separate(col = Label, into = c("F", "Yr"), sep = "_") %>% 
  select(Yr, Value, StdDev) %>% 
  filter(Yr >= 1949 & Yr <= 2023) %>% 
  mutate(Model = "'Opakapaka SS3",
         Yr = as.numeric(Yr),
         lci = NA,
         uci = NA,
         Hr = Value) %>% 
  select(Yr, Hr, Model, lci, uci)

deep7_F <- fit_test$timeseries[,,"F"] %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Yr") %>% 
  mutate(Yr = as.numeric(Yr), Hr = mu, Model = "Deep 7 JABBA",
         lci = NA, uci = NA) %>% 
  select(Yr, Hr, Model, lci, uci)

fit_paka$timeseries[,,"F"] %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Yr") %>% 
  mutate(Yr = as.numeric(Yr), Hr = mu, Model = "'Opakapaka JABBA") %>% 
  select(Yr, Hr, Model, lci, uci) %>% 
  bind_rows(ss3_f) %>% 
  #bind_rows(deep7_F) %>% 
  ggplot(aes(x = Yr, y = Hr, group = Model)) + 
  geom_ribbon(aes(x = Yr, ymin = lci, ymax = uci, fill = Model), alpha = .35, show.legend = F) +
  geom_line(aes(color = Model, linetype = Model), linewidth = 1.25) + 
  geom_hline(yintercept = 0.151, color = cols[5]) +
  geom_hline(yintercept = 0.128, color = cols[1]) +
  #geom_hline(yintercept = 0.116, color = cols[5]) +
  geom_text(label = expression(H[MSY]), aes(x = 1950, y = 0.158), color = cols[5]) +
  geom_text(label = expression(H[MSY]), aes(x = 1950, y = 0.135), color = cols[1]) +
  #geom_text(label = expression(H[MSY]), aes(x = 1950, y = 0.121), color = cols[5]) +
  scale_color_manual(values = cols[c(1,5)]) +
  scale_fill_manual(values = cols[c(1,5)]) +
  theme_classic() +
  theme(text = element_text(size=24), legend.position = "bottom")+
  labs(x = "Year", y = "Harvest Rate")
ggsave(filename = file.path(main_dir, "Model", "19_Base", "plots", "opaka_harvestrate_comparisons.png"), width = 10, height = 6, units = "in")

##Fig for the report ####
hr.p <- fit_paka$timeseries[,,"F"] %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Yr") %>% 
  mutate(Yr = as.numeric(Yr), Hr = mu, Model = "JABBA") %>% 
  select(Yr, Hr, Model, lci, uci) %>% 
  bind_rows(ss3_f) %>% 
  ggplot(aes(x = Yr, y = Hr, group = Model)) + 
  geom_ribbon(aes(x = Yr, ymin = lci, ymax = uci, fill = Model), alpha = .25) +
  geom_line(aes(color = Model, linetype = Model), linewidth = 1.25) + 
  geom_hline(yintercept = 0.151, color = "#FF3300FF") +
  geom_hline(yintercept = 0.128, color = "#0033FFFF") +
  geom_text(label = expression(H[MSY]), aes(x = 1950, y = 0.158), color = "#FF3300FF") +
  geom_text(label = expression(H[MSY]), aes(x = 1950, y = 0.135), color = "#0033FFFF") +
  scale_color_manual(values = c("#0033FFFF", "#FF3300FF")) +
  scale_fill_manual(values = c("#0033FFFF", "#FF3300FF")) +
  theme_classic() +
  labs(x = "Year", y = "Harvest Rate")

eb.p <- fit_paka$timeseries[,,"B"] %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Yr") %>% 
  mutate(Yr = as.numeric(Yr), Bio_all = mu, Model = "JABBA", Bio_lbs = Bio_all * 10e5) %>% 
  select(Yr, Bio_all, Model, Bio_lbs, lci, uci) %>% 
  bind_rows(ss3_b) %>% 
  mutate(Bio_lbs = Bio_lbs/1000000,
         lci = ifelse(is.na(lci), Bio_lbs, lci),
         uci = ifelse(is.na(uci), Bio_lbs, uci)) %>% 
  ggplot(aes(x = Yr, y = Bio_lbs, group = Model)) + 
  geom_ribbon(aes(x = Yr, ymin = lci, ymax = uci, fill = Model), alpha = .25) +
  geom_line(aes(color = Model, linetype = Model), linewidth = 1.25) + 
  scale_color_manual(values = c("#0033FFFF", "#FF3300FF")) +
  scale_fill_manual(values = c("#0033FFFF", "#FF3300FF")) +
  theme_classic() +
  labs(x = "Year", y = "Exploitable Biomass (million lbs)")
library(patchwork)
eb.p/hr.p
ggsave(filename = file.path(main_dir, "Model", "19_Base", "plots", "EBio_harvestrate_comparisons.png"), width = 10, height = 6, units = "in")
######
rep_base$sizeselex %>%
  filter(Factor == "Lsel") %>% 
  slice_tail(by = Fleet) %>% 
  pivot_longer(cols = -c(Factor, Fleet, Yr, Sex, Label)) %>% 
  mutate(Length.bin = as.numeric(name),
         Fleet = factor(Fleet, labels = c("Commercial", "BFISH Camera", "Non-Commercial", "BFISH Fishing"))) %>% 
  ggplot(aes(x = Length.bin, y = value, group = Fleet)) +
  geom_point(aes(color = Fleet, shape = Fleet), size = 2) +
  geom_line(aes(color = Fleet), size = 1) +
  scale_color_manual(values = cols) +
  scale_shape(solid = F) +
  theme_classic() +
  theme(legend.position = "bottom", text = element_text(size=24)) +
  labs(x = "Length (cm)", y = "Selectivity")
ggsave(filename = file.path(main_dir, "Model", "19_Base", "plots", "selectivity.png"), width = 10, height = 6, units = "in")


FISH.LENGTHS <- read.csv(file.path(main_dir,"Data", "paka_catch_2016_2022.csv")) %>% 
  filter(SPECIES_CD == "PRFI" & !is.na(LENGTH_CM)) %>% #filter for fish at 29cm or above bc that was done for index standardization (29cm = size limit)
  select(c(YEAR, SAMPLE_ID, LENGTH_CM)) %>% 
  mutate(YEAR = as.numeric(YEAR)+1,
         GROUP = "RESFISH")

CAM.LENGTHS <- read.csv(file.path(main_dir, "Data", "2022_CAM_LENGTHS.csv")) %>% 
  filter(SPECIES_CD == "PRFI" & !is.na(MEAN_MM)) %>%
  separate(col = BFISH, into = c("BFISH", "YEAR", "Seas")) %>% 
  mutate(LENGTH_CM = MEAN_MM/10,
         GROUP = "CAM",
         YEAR = as.numeric(YEAR) + 1) %>% 
  select(YEAR, DROP_CD, LENGTH_CM, GROUP)

FRS.LENGTHS_early <- read.csv(file.path(main_dir, "Data", "PICDR-113273 HDAR Commercial Catch Calendar Year 1948 to Fiscal Year 1993.csv")) %>% 
  filter(SPECIES == 19)
FRS.LENGTHS_late <- read.csv(file.path(main_dir, "Data", "PICDR-113273 HDAR Commercial Catch Fiscal Year 1994 to 2022-10-26.csv")) %>% 
  filter(SPECIES == 19)
FRS.LENGTHS <- rbind(FRS.LENGTHS_early, FRS.LENGTHS_late)
FRS_1 <- FRS.LENGTHS %>% 
  filter(FYEAR >= 1949) %>% 
  filter(CAUGHT == 1 & LBS <= 21) %>% 
  mutate(kg = (LBS / 2.205),
         L = (kg/1.75e-05)^(1/2.99)) %>%
  mutate(YEAR = FYEAR, 
         LENGTH_CM = L,
         GROUP = "FRS") 
bfish <- bind_rows(CAM.LENGTHS, FISH.LENGTHS) %>% 
  select(-c(DROP_CD, SAMPLE_ID)) %>% 
  filter(LENGTH_CM >= 29)

FRS_1 %>% 
  select(YEAR, LENGTH_CM, GROUP) %>%
  bind_rows(bfish) %>%
  mutate(GROUP = factor(GROUP, labels = c("BFISH Camera", "Commercial", "BFISH Fishing"))) %>% 
  ggplot(aes(x = LENGTH_CM)) +
  geom_density(aes(group = GROUP, fill = GROUP, color = GROUP), alpha = .5) +
  scale_fill_manual(values = cols[c(2,1,4)], name = "Fleet") +
  scale_color_manual(values = cols[c(2,1,4)], name = "Fleet") +
  theme_classic() +
  theme(legend.position = "bottom", text = element_text(size=24)) +
  labs(x = "Length (cm)")
ggsave(filename = file.path(main_dir, "Model", "19_Base", "plots", "len_comp_by_fleet.png"), width = 10, height = 6, units = "in")

FRS_1 %>% 
  ggplot(aes(x = kg)) +
  geom_histogram(binwidth = 0.45) + 
  facet_wrap(~FYEAR) 



CAM.LENGTHS %>% 
  left_join(sample_size_dt %>% 
              filter(gear_type == "camera" & 
                       all_lengths == TRUE & 
                       data_treatment == 1) %>% 
              rename("YEAR" = "year") %>% 
              mutate(YEAR = as.numeric(YEAR) + 1)) %>% 
  mutate(Label = paste0(YEAR, " (N = ", input_sample_size, ")")) %>% 
  ggplot(aes(x = LENGTH_CM)) +
  geom_histogram(binwidth = 5, fill = cols[1]) + 
  facet_wrap(~Label) +
  theme_classic() + 
  labs(x = "Length (cm)") + 
  theme(text = element_text(size=24)) 
ggsave(filename = file.path(main_dir, "Model", "19_Base", "plots", "len_comp_camera.png"), width = 10, height = 6, units = "in")

FISH.LENGTHS %>% 
  left_join(sample_size_dt %>% 
            filter(gear_type == "research_fishing" & 
                     all_lengths == TRUE & 
                     data_treatment == 1) %>% 
              rename("YEAR" = "year") %>% 
              mutate(YEAR = as.numeric(YEAR) + 1)) %>% 
  mutate(Label = paste0(YEAR, " (N = ", input_sample_size, ")")) %>% 
  ggplot(aes(x = LENGTH_CM)) +
  geom_histogram(binwidth = 5, fill = cols[1]) + 
  facet_wrap(~Label) +
  theme_classic() + 
  labs(x = "Length (cm)") + 
  theme(text = element_text(size=24)) 
ggsave(filename = file.path(main_dir, "Model", "19_Base", "plots", "len_comp_fishing.png"), width = 10, height = 6, units = "in")

### Kobe plots
ss_kobe <- rep_base$Kobe %>% 
  mutate(F.Fmsy = F.Fmsy/0.151,
         Model = "'Opakapaka SS3") %>% 
  filter(Yr >= 1949 & Yr <= 2023)

jabba_kobe <- fit_paka$kbtrj %>% 
  group_by(year) %>% 
  select(year, stock, harvest) %>% 
  summarise(B.Bmsy = quantile(stock, 0.5),
            F.Fmsy = quantile(harvest, 0.5)) %>% 
  rename("Yr" = "year") %>% 
  mutate(Model = "'Opakapaka JABBA")

deep7_kobe <- fit_test$kbtrj %>% 
  group_by(year) %>% 
  select(year, stock, harvest) %>% 
  summarise(B.Bmsy = quantile(stock, 0.5),
            F.Fmsy = quantile(harvest, 0.5)) %>% 
  rename("Yr" = "year") %>% 
  mutate(Model = "Deep 7 JABBA")

kobe.df <- bind_rows(jabba_kobe, ss_kobe) %>% 
  mutate(Model = factor(Model))
         #alpha = ifelse(Model == "'Opakapaka JABBA", 0.25, 1))



# Kobe plot layout setting
x_max  <- max(kobe.df$B.Bmsy + .1)
x_min  <- 0
y_max  <- 1.3
y_min  <- 0
MSST_x <- max(0.5,0.865) #bfrac = 1-M

## Overfished triangles/trapezoids
tri_y  <- c(y_min,1,y_min)  
tri_x  <- c(x_min,MSST_x,MSST_x)
poly_y <- c(y_min,y_max,y_max,1)
poly_x <- c(x_min,x_min,MSST_x,MSST_x)
kb <- ggplot()+
  ylab(expression(H/H[CR]))+
  xlab(expression(B/B[MSY]))+
  scale_x_continuous(expand=c(0,0),limits=c(x_min,x_max))+
  scale_y_continuous(expand=c(0,0),limits=c(y_min,y_max))+
  geom_polygon(aes(x=tri_x,y=tri_y),fill="#FFFF65",linewidth=1, linetype = 2)+
  geom_polygon(aes(x=c(MSST_x,x_max,x_max,MSST_x), y=c(1,1,y_min,y_min)),fill="#B1DC6B")+
  geom_polygon(aes(x=poly_x, y=poly_y),fill="#FF6C57",linewidth=1, linetype = 2)+
  geom_polygon(aes(x=c(MSST_x,x_max,x_max,MSST_x), y=c(1,1,y_max,y_max)),fill="#FFAB38",linewidth=1, linetype = 2) +
  geom_segment(aes(x = MSST_x, xend = x_max, y = 1, yend = 1), linetype = 2) +
  geom_segment(aes(x = x_min, xend = MSST_x, y = 0, yend = 1), linetype = 2) +
  geom_segment(aes(x = MSST_x, xend = MSST_x, y = 0, yend = y_max), linetype = 2)
  
# kb + 
# geom_path(data = kobe.df %>% filter(Model == "Deep 7 JABBA"),  
#              aes(x = B.Bmsy, y = F.Fmsy, color = Model, alpha = Model), linewidth = 1, show.legend = F) +
#   geom_point(data = kobe.df,  
#              aes(x = B.Bmsy, y = F.Fmsy, shape = Model, color = Model, alpha = Model), size = 3) +
#   scale_color_manual(values = cols[c(1,4,5)]) +
#   geom_point(data = kobe.df %>% filter(Yr == 2023), aes(x = B.Bmsy, y = F.Fmsy), fill = "#CBCFD1", color = "#323C46", size = 4, show.legend = F, shape = c(21, 24), stroke = 1.5, alpha = c(0.95,1)) +
#   scale_alpha_manual(values = c(0.45, 1)) +
#   guides(shape = guide_legend(override.aes = list(size = 5))) +
#   theme_classic() +
#   theme(legend.position = "bottom", text = element_text(size=24)) 
# ggsave(filename = file.path(main_dir, "Model", "19_Base", "plots", "kobe_comparisons.png"), width = 10, height = 8, units = "in")

# kb + 
#   geom_path(data = kobe.df %>% filter(Model == "Deep 7 JABBA"),  
#             aes(x = B.Bmsy, y = F.Fmsy), color = cols[5], linewidth = 1, show.legend = F) +
#   geom_point(data = kobe.df %>% filter(Model == "Deep 7 JABBA"),  
#              aes(x = B.Bmsy, y = F.Fmsy, shape = Model, color = Model), size = 3) +
#   scale_color_manual(values = cols[c(5)]) +
#   scale_shape_manual(values = c(15)) +
#   geom_point(data = kobe.df %>% filter(Model == "Deep 7 JABBA" & Yr == 2023), aes(x = B.Bmsy, y = F.Fmsy), fill = "#CBCFD1", color = "#323C46", size = 4, show.legend = F, shape = c(22), stroke = 1.5, alpha = c(1)) +
#   guides(shape = guide_legend(override.aes = list(size = 5))) +
#   theme_classic() +
#   theme(legend.position = "bottom", text = element_text(size=24)) 
# ggsave(filename = file.path(main_dir, "Model", "19_Base", "plots", "kobe_deep7only.png"), width = 10, height = 8, units = "in")
# 
# kb + 
#   geom_path(data = kobe.df %>% filter(Model == "Deep 7 JABBA" | Model == "'Opakapaka JABBA"),  
#             aes(x = B.Bmsy, y = F.Fmsy, color = Model, alpha = Model), linewidth = 1, show.legend = F) +
#   geom_point(data = kobe.df %>% filter(Model == "Deep 7 JABBA"| Model == "'Opakapaka JABBA"),  
#              aes(x = B.Bmsy, y = F.Fmsy, shape = Model, color = Model, alpha = Model), size = 3) +
#   scale_color_manual(values = c(cols[c(1)], "grey50")) +
#   scale_alpha_manual(values = c(1, .45)) +
#   scale_shape_manual(values = c(17, 16)) +
#   geom_point(data = kobe.df %>% 
#                filter(Yr == 2023) %>% 
#                filter(Model == "Deep 7 JABBA" | Model == "'Opakapaka JABBA"), 
#              aes(x = B.Bmsy, y = F.Fmsy), 
#              fill = "#CBCFD1", color = "#323C46", size = 4, 
#              show.legend = F, stroke = 1.5,  shape = c(24, 21)) +
#   guides(shape = guide_legend(override.aes = list(size = 5))) +
#   theme_classic() +
#   theme(legend.position = "bottom", text = element_text(size=24)) 
# ggsave(filename = file.path(main_dir, "Model", "19_Base", "plots", "kobe_deep7_pakajabba.png"), width = 10, height = 8, units = "in")

kb + 
  geom_path(data = kobe.df,  
            aes(x = B.Bmsy, y = F.Fmsy, color = Model), linewidth = 1, show.legend = F) +
  geom_point(data = kobe.df,  
             aes(x = B.Bmsy, y = F.Fmsy, shape = Model, color = Model), size = 3) +
  scale_color_manual(values = cols[c(1,5)]) +
  #scale_alpha_manual(values = c(.4, 1, .4)) +
  scale_shape_manual(values = c(17,16)) +
  geom_point(data = kobe.df %>% 
               filter(Yr == 2023), 
             aes(x = B.Bmsy, y = F.Fmsy), 
             fill = "#CBCFD1", color = "#323C46", size = 4, 
             show.legend = F, stroke = 1.5,  shape = c(24, 21)) +
  guides(shape = guide_legend(override.aes = list(size = 5))) +
  theme_classic() +
  theme(legend.position = "bottom", text = element_text(size=24)) 
ggsave(filename = file.path(main_dir, "Model", "19_Base", "plots", "kobe_pakajabba_pakaSS.png"), width = 10, height = 8, units = "in")


SSplotData(rep_base,
           subplots = 2,
           fleetcol = cols[c(1,2,3,4)],
           fleetnames = c("Commercial", "BFISH Camera", "Non-Commercial", "BFISH Fishing"),
           print = TRUE, 
           plotdir = file.path(main_dir, "Model", "19_Base", "plots"), ptsize = 16,
           pwidth = 10, pheight = 8, punits = "in")

SSplotData(rep_base,
           subplots = 1,
           datatypes = c("catch", "cpue"),
           fleets = c(1,2),
           fleetcol = cols[c(1,2)],
           fleetnames = c("Commercial", "BFISH"),
           print = TRUE, 
           plotdir = file.path(main_dir, "Model", "19_Base", "plots"), ptsize = 16,
           pwidth = 10, pheight = 8, punits = "in")
