require(data.table);require(this.path); require(this.path); require(openxlsx); require(ggplot2); require(tidyverse);options(scipen = 999)
root_dir <- this.path::here(.. = 1)

A <- fread(file.path(root_dir,"Data","strata_area_size.csv")) %>% select(MIN_DEPTH,AREA_HA,PROP_AREA)

B <- fread(file.path(root_dir,"Data","2022_CAM_SAMPLE_TIME.csv")) %>% rename(DEPTH=OFFICIAL_DEPTH_M) %>% mutate(YEAR=as.numeric( str_sub(DROP_DATE,1,4))) %>% 
      select(YEAR,DROP_CD,DEPTH)

# Number of drops by YEAR x DEPTH
B$MIN_DEPTH   <- "NA"
B[DEPTH<400]$MIN_DEPTH <- "330m"
B[DEPTH<330]$MIN_DEPTH <- "220m"
B[DEPTH<220]$MIN_DEPTH <- "170m"
B[DEPTH<170]$MIN_DEPTH <- "110m"
B[DEPTH<110]$MIN_DEPTH <- "75m"

BS <- B %>% group_by(YEAR,MIN_DEPTH) %>% summarize(N_DROP=n())

C <- fread(file.path(root_dir,"Data","2022_CAM_LENGTHS.csv")) %>% mutate(LENGTH_CM=MEAN_MM/10,LENGTH_BIN_MIN=LENGTH_CM-(LENGTH_CM%%5)) %>% 
      filter(COMMON_NAME=="Opakapaka") %>%  select(DROP_CD,LENGTH_CM,LENGTH_BIN_MIN)

# Merge to get depth info of length data
C <- C %>% inner_join(B,by="DROP_CD") 

# Merge # of drops info with length data to calculate Length count per drop
C <- C %>% inner_join(BS,by=c("YEAR","MIN_DEPTH"))

# Summarize per length bin
CS <- C %>% group_by(YEAR,MIN_DEPTH,N_DROP,LENGTH_BIN_MIN) %>% summarize(N=n()) %>% mutate(N_PER_DROP=N/N_DROP)

# Add the habitat area information and calculate total N per bin per year per habitat (assuming 1 drop = 1 hectare)
D <- CS %>% inner_join(A,by="MIN_DEPTH") %>% mutate(TOTAL_N=N_PER_DROP*AREA_HA)

# Summarize the years together
D1 <- D %>% group_by(MIN_DEPTH,LENGTH_BIN_MIN) %>% summarize(TOTAL_N=sum(TOTAL_N))
ggplot(data=D1)+geom_bar(aes(x=LENGTH_BIN_MIN,y=TOTAL_N),stat="identity")+facet_wrap(~MIN_DEPTH,ncol=1)

# Summarize the depths together
D2 <- D %>% group_by(LENGTH_BIN_MIN) %>% summarize(TOTAL_N=sum(TOTAL_N),N=sum(N)) %>% 
        mutate(TOTAL_N_PROP=TOTAL_N/sum(TOTAL_N))
ggplot(data=D2)+geom_bar(aes(x=LENGTH_BIN_MIN,y=TOTAL_N),stat="identity")
ggplot(data=D2)+geom_bar(aes(x=LENGTH_BIN_MIN,y=TOTAL_N_PROP),stat="identity")
ggplot(data=D2)+geom_bar(aes(x=LENGTH_BIN_MIN,y=N),stat="identity")

# Check finale results and create data file for input into SS model
D3 <- D %>% group_by(YEAR,LENGTH_BIN_MIN) %>% summarize(N=sum(N),TOTAL_N=sum(TOTAL_N)) %>% 
        mutate(N_PROP=N/sum(N),TOTAL_N_PROP=TOTAL_N/sum(TOTAL_N))
ggplot(data=D3)+geom_line(aes(x=LENGTH_BIN_MIN,y=N_PROP),col="blue")+geom_line(aes(x=LENGTH_BIN_MIN,y=TOTAL_N_PROP),col="red")+facet_wrap(~YEAR)

D4 <- D3 %>% mutate(RECONST_N=round(TOTAL_N_PROP*sum(N),2))
ggplot(data=D4)+geom_line(aes(x=LENGTH_BIN_MIN,y=RECONST_N),stat="identity",col="blue")+geom_line(aes(x=LENGTH_BIN_MIN,y=N),stat="identity",col="red")+facet_wrap(~YEAR)

D5 <- D4 %>% select(YEAR, LENGTH_BIN_MIN, N=RECONST_N,2)

write.csv(D5,file=file.path(root_dir,"Outputs","Reconstructed_Bfish_SizeComp.csv"),row.names=F)
