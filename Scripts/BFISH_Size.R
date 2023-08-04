require(data.table);require(this.path); require(this.path); require(openxlsx); require(ggplot2); require(tidyverse);options(scipen = 999)
root_dir <- this.path::here(.. = 1)

load(file.path(root_dir,"Data","2022.lencomp_dt.RData"))
A <- fread(file.path(root_dir,"Data","strata_area_size.csv"))

D <- lencomp_dt

D$Min_Depth   <- "NA"
D[depth<400]$Min_Depth <- "330m"
D[depth<330]$Min_Depth <- "220m"
D[depth<220]$Min_Depth <- "170m"
D[depth<170]$Min_Depth <- "110m"
D[depth<110]$Min_Depth <- "75m"

D <- D %>% mutate(Length_Bin_Min=length_cm-(length_cm%%5))
D <- D %>% left_join(A,by="Min_Depth") %>% rename(Abund=Prop_Area)

table(D[gear=="camera"]$Min_Depth,D[gear=="camera"]$Length_Bin_Min)


D1 <- D %>% group_by(gear,Length_Bin_Min) %>% summarize(Abund=sum(Abund),N=n()) %>% 
        mutate(Abund_Prop=Abund/sum(Abund),N_Prop=N/sum(N))

ggplot(data=D1)+geom_line(aes(x=Length_Bin_Min,y=Abund_Prop),col="blue",stat="identity")+geom_line(aes(x=Length_Bin_Min,y=N_Prop),col="red")+facet_wrap(~gear)


D2 <- D %>% group_by(gear,Min_Depth,Length_Bin_Min) %>% summarize(N=n()) %>% 
  mutate(N_Prop=N/sum(N))

ggplot(data=D2)+geom_bar(aes(x=Length_Bin_Min,y=N_Prop),stat="identity",fill="black")+facet_wrap(~Min_Depth)



