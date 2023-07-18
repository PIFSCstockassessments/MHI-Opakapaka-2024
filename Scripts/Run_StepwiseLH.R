#library(remotes)
#remotes::install_github("PIFSCstockassessments/StepwiseLH")

require(StepwiseLH); require(tidyverse) 

# Get estimates
# Based on L99 of research fishing (n=381) of 73.0 cm (FL)
set.seed(123); Data <-Get_distributions(Family="Lutjanidae", Lmax.mean=(730/0.89), Lmax.SD=1, M_method="Hamel_Cope_2022",n_iter=10000)

Linf <- median(Data$Linf*0.89)
Lmat <- median(Data$Lmat*0.89)
K    <- median(Data$K)
Amax <- median(Data$Amax)
M    <- median(Data$M)
A0   <- median(Data$A0)
Linf.95 <- quantile(Data$Linf*.89, c(0.05,0.95))

sd(Data$Linf*0.85)
median(Data$M)
mean(log(Data$M))
sd(log(Data$M))
sd(Data$Amax)
quantile(Data$M,c(0.05,0.95))


# Plot the growth curve
Age  <- seq(0,Amax,by=0.5)
f    <- function(x) Linf*(1-exp(-K*(x-A0)))
set.seed(123); Data <-data.frame(AGE=Age,LENGTH=f(Age)) 
ggplot(data=Data,aes(x=AGE,y=LENGTH))+geom_line()+theme_bw()


