library(tidyverse)
library(fishualize)
library(r4ss)
mod_dir <- file.path(main_dir, "Model", "20_adjusting_settings")

rep <- SS_output(mod_dir)
SS_plots(rep)
base_mods <- SSgetoutput(dirvec = c(file.path(main_dir, "Model", "19_Base"), file.path(main_dir, "Model", "20_adjusting_settings")))
base_mods_sum <- SSsummarize(base_mods)
SSplotComparisons(base_mods_sum, subplots = c(2,4,7,11,14), new = F)

head(rep$timeseries)

rep$derived_quants %>% 
  filter(str_detect(Label, "Bratio")) %>% 
  separate(col = Label, into = c("lab", "Year"), sep = "_") %>% 
  mutate(Year = as.numeric(Year),
         lci = Value - 2*StdDev,
         uci = Value + 2*StdDev) %>% 
  ggplot(aes(x = Year)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), fill = "#1914ce", alpha = .25) +
  geom_line(aes(y = Value), color = "#1914ce", lwd = 1.2) +
  geom_hline(yintercept = 0.865) +
  theme_bw() +
  labs(y = expression('B'/'B'[MSY]))
ggsave(filename = file.path(mod_dir, "BBmsy.png"))


rep$Kobe %>% 
  filter(Yr < 2024) %>% 
  ggplot(aes(x = Yr)) +
  geom_line(aes(y = F.Fmsy), color = "#1914ce", lwd = 1.2) +
  #geom_hline(yintercept = 1) +
  theme_bw() +
  labs(y = expression('F'/'F'[MSY]), x = "Year")
ggsave(filename = file.path(mod_dir, "FFmsy.png"))

SSplotData(rep, fleetcol = c("#fb771d", "#8beb83", "#04c4f8", "#1914ce"))

mu.f = rep$Kobe$F.Fmsy[-76]
mu.b = rep$Kobe$B.Bmsy[-76]

years=rep$Kobe$Yr[-76]
N = length(years)
# fit kernel function
# kernelF <- gplots::ci2d(b,of,nbins=151,factor=1.5,ci.levels=c(0.50,0.80,0.75,0.90,0.95),show="none",col=1,xlab= ifelse(jabba$settings$harvest.label=="Fmsy",expression(paste(F/F[MSY])),expression(paste(H/H[MSY]))),ylab=expression(paste(B/B[MSY])))

Par = list(mfrow=c(1,1),mar = c(3.5, 3.5, 0.2, 0.1), mgp =c(2.,0.5,0), tck = -0.02,cex=0.8)


#Create plot
ylab=expression(paste(F/F[MSY]))
xlab=expression(paste(B/(B[MSY])))


# Kobe plot layout setting
x_max  <- max(mu.b + .1)
x_min  <- 0
y_max  <- 1.3
y_min  <- 0
MSST_x <- max(0.5,0.865) #bfrac = 1-M
max_yr <- max(years)

## Overfished triangles/trapezoids
tri_y  <- c(y_min,1,y_min)  
tri_x  <- c(x_min,MSST_x,MSST_x)
poly_y <- c(y_min,y_max,y_max,1)
poly_x <- c(x_min,x_min,MSST_x,MSST_x)
png(file = file.path(mod_dir, "Kobe.png"), width = 5, height = 3.5, res = 200, units = "in")
par(Par)
plot(1000,1000,type="b", xlim=c(0,x_max +0.05), ylim=c(0,y_max),lty=3,ylab=ylab,xlab=xlab,xaxs="i",yaxs="i")

polygon(x = c(MSST_x, x_max, x_max, MSST_x), y = c(1,1, y_min, y_min),col="palegreen3",border=0)
polygon(x = tri_x, y = tri_y, col="gold",border=0)
polygon(x = c(MSST_x,x_max,x_max,MSST_x), y = c(1,1,y_max,y_max),col="darkorange",border=0)
polygon(x = poly_x, y = poly_y, col="brown2",border=0)


lines(x = c(MSST_x, x_max), y = c(1,1), lty = 3, lwd = 0.7)
lines(x = c(x_min, MSST_x), y = c(y_min, 1), lty = 3, lwd = 0.7)
lines(x = c(MSST_x, MSST_x), y = c(y_min, y_max), lty = 3, lwd = 0.7)

points(mu.b,mu.f,bg="grey35",pch=21,cex=1.9)
lines(mu.b, mu.f)
points(mu.b[N], mu.f[N], pch = 21, bg = "white", cex = 1.9)
points(mu.b[1], mu.f[1], pch = 24, bg = "white", cex = 1.9)
legend("topright", legend = c("1949", "2023"), pch = c(24, 21), pt.bg = "white", pt.cex=c(rep(1.3,2)),bty="n")
dev.off()

SSplotRecdevs(rep)

load(file = "C:/Users/Megumi.Oshima/Documents/Deep-7-Benchmark-2024/Results/54_Opaka_Base/fit_test.Rdata")

jabba.b <- fit_test$timeseries[,,"B"] %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Yr") %>% 
  select(Yr, mu) %>% 
  mutate(Yr = as.numeric(Yr),
         Model = "JABBA")

ss.b <- rep$timeseries %>% 
  mutate(mu = (Bio_all *2205)/1000000, 
         Model = "SS") %>% 
  select(Yr, mu, Model)
bind_rows(jabba.b, ss.b) %>% 
ggplot(aes(x = Yr, y = mu)) +
  geom_line(aes(color = Model), lwd = 1.2) +
  theme_bw() +
  scale_color_fish_d(option = "Synchiropus_splendidus") +
  labs(x = "Year", y = "Biomass (million lbs)")
