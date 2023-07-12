pacman::p_load("this.path", "tidyverse","data.table","readxl","StepwiseLH","LBSPR", "r4ss")

main_dir <- this.path::here(..=1)
LH <- read_excel(file.path(main_dir,"Data","Priors Data",paste0("Deep7_LH.xlsx"))) 

S.CODE <- LH%>% 
  filter(SEX=="F") %>% 
  select(SCIENTIFIC_NAME,SPECIES,SPECIES_ID,LW_A,LW_B,BINWIDTH)

BFISH.LENGTHS <- fread(file.path(main_dir,"Data", "paka_catch_2016_2022.csv")) %>% 
  filter(SPECIES_CD == "PRFI" & !is.na(LENGTH_CM)) %>% 
  #separate(col = BFISH, into = c("BFISH", "Year", "seas"), sep = "_") %>% 
  select(c(YEAR, SAMPLE_ID, LENGTH_CM)) %>% 
  mutate(YEAR = as.numeric(YEAR)+1)

CATCH <- read.csv(file.path(main_dir, "Data", "Opakapaka_catch.csv")) %>% 
  pivot_longer(cols = -Year, names_to = "fleet_name", values_to = "catch") %>% 
  mutate(catch = catch/1000000) %>% 
  filter(Year > 1948) %>% 
  mutate(seas = 1,
         fleet = ifelse(fleet_name == "Commercial",1,2), 
         catch_se = .01) %>% 
  rename(year = Year) %>% 
  select(year, seas, fleet, catch, catch_se) %>% 
  arrange(fleet, year) 

CATCH %>% group_by(fleet) %>% slice_tail(n = 3) %>% summarise(mean(catch))  #0.0677, 0.0700

initcatch <- data.frame(year = c(-999,-999), seas = c(1,1), fleet = c(1,2), catch = c(0,0), catch_se = c(0.01,0.01))
catch23 <- data.frame(year = c(2023,2023), seas = c(1,1), fleet = c(1,2), catch = c(0.0677,0.0700), catch_se = c(0.01,0.01))
CATCH <- bind_rows(initcatch, CATCH, catch23) %>% arrange(fleet, year)
  

bfish_cpue <-read.csv(file.path(main_dir, "Data", "BFISH_index.csv")) %>% 
  filter(model_number == 69) %>%  #jb.params$BFISH_index_mod
  filter(str_detect(category, "PRFI")) %>% 
  dplyr::select(c(time, estimate, cv)) %>% 
  mutate(seas = 7,
         index = 3,
         time = time+1) %>% 
  rename(yr = time,
         obs = estimate, 
         obs_log = cv) %>% 
  select(yr, seas, index, obs, obs_log)

CPUE <- read.csv(file.path(main_dir, "Data", "opakapaka_FRS.csv")) %>% 
  mutate(seas = 7,
         index = 1) %>% 
  mutate(obs_log = stderr/obs) %>% 
  select(yr, seas, index, obs, obs_log) %>% 
  bind_rows(bfish_cpue) %>% 
  filter(yr > 1948)

# Length comps
BFISH.LENGTHS %>% ggplot() + geom_histogram(aes(LENGTH_CM), binwidth = 2) + facet_wrap(~YEAR, scales = "free")
BFISH.LENGTHS %>% group_by(YEAR) %>% summarise(N =n()) #want to combine 2019 and 2020 into superperiod bc sample sizes are 30 and 39 respectively 
BIN_SIZE = 5
BFISH.LENGTHS$LENGTH_BIN_START <- BFISH.LENGTHS$LENGTH_CM-(BFISH.LENGTHS$LENGTH_CM%%BIN_SIZE)
lencomp <- BFISH.LENGTHS %>% 
  select(-SAMPLE_ID) %>% 
  group_by(YEAR, LENGTH_BIN_START) %>% 
  summarise(Nsamp = n()) %>% 
  mutate(LENGTH_BIN_START = paste0("l", LENGTH_BIN_START)) %>% 
  pivot_wider(names_from = LENGTH_BIN_START, values_from = Nsamp, values_fill = 0) %>% 
  ungroup() %>% 
  mutate(Yr = YEAR, 
         Seas = 1, 
         FltSvy = 3, 
         Gender = 0, 
         Part = 0,
         Seas = ifelse(Yr == 2019|Yr == 2020, -1, 1),
         FltSvy = ifelse(Yr == 2020, -3, 3),
         Nsamp = rowSums(select(., starts_with("l")))) %>% 
  select(Yr, Seas, FltSvy, Gender, Part, Nsamp, starts_with("l"))

## Writing data to data.ss file
data <- SS_readdat_3.30(file = file.path(main_dir, "Model", "data.ss"))
data$Nfleets <- 3
data$styr <- 1949
data$endyr <- 2023
data$catch <- CATCH 
data$fleetinfo <- data.frame(type = c(1,1,3), surveytiming = c(-1,-1,1), area = c(1,1,1),
                             units = c(1,1,1), need_catch_mult = c(0,0,0), 
                             fleetname = c("FRS", "Non_comm", "BFISH"))
data$CPUE <- CPUE
data$CPUEinfo <- data.frame(Fleet = c(1,2,3), Units =c(1,1,1), Errtype = c(0,0,0), SD_Report = c(0,0,0))


data$lencomp <- as.data.frame(lencomp)
data$lbin_vector <- sort(unique(BFISH.LENGTHS$LENGTH_BIN_START))
data$N_lbins <- length(data$lbin_vector)
data$len_info <- data.frame(
  mintailcomp = rep(-1, 3),
  addtocomp = rep(0.001, 3), 
  combine_M_F = rep(0,3),
  CompressBins = rep(0,3), 
  CompError = rep(0,3),
  ParmSelect = c(1,2,3),
  minsamplesize = c(.001, .001, .001)
)

SS_writedat_3.30(data, outfile = file.path(main_dir, "Model", "data.ss"), overwrite = TRUE)
