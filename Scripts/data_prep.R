## Script to prep data inputs for opakapaka SS model 
### M.Oshima 11/3/2023
### for supplementary material for 2024 Deep7 assessment report
pacman::p_load("this.path", "tidyverse","data.table","readxl","r4ss")

main_dir <- this.path::here(..=1)

CATCH <- read.csv(file.path(main_dir, "Data", "Paka_catch_SS.csv")) %>% 
  pivot_longer(cols = -Year, names_to = "fleet_name", values_to = "catch") %>% 
  mutate(catch = catch*0.000453592) %>% #convert to mt
  filter(Year > 1948) %>% 
  mutate(seas = 1,
         fleet = ifelse(fleet_name == "Commercial",1,3), 
         catch_se = .01) %>% 
  rename(year = Year) %>% 
  select(year, seas, fleet, catch, catch_se) %>% 
  arrange(fleet, year) %>% 
  mutate(catch = ifelse(is.na(catch), 0.001, catch))
#get first year catch for initialF for commercial fleet
yr1C <- CATCH %>% filter(year == 1949 & fleet == 1) %>% pull(catch)

initcatch <- data.frame(year = c(-999,-999), 
                        seas = c(1,1), 
                        fleet = c(1,3), 
                        catch = c(yr1C,0), #not estimating initialF for non-comm fleet
                        catch_se = c(0.01,0.01))

CATCH <- bind_rows(initcatch, CATCH) %>% arrange(fleet, year)
write.csv(CATCH, file = file.path(main_dir, "Outputs", "catch.csv"), row.names = F)
  
#camera CPUE
bfish_cpue <- read.csv(file.path(main_dir, "Data", "index_dt.csv")) %>% 
  filter(model_number == 77) %>%  
  dplyr::select(c(time, estimate, cv)) %>% 
  mutate(seas = 7,
         index = 2,
         time = time+1,
         obs_log = sqrt(log(1+cv^2))) %>% 
  rename(yr = time,
         obs = estimate) %>% 
  select(yr, seas, index, obs, obs_log)
#research fishing CPUE
bfish_fish_cpue <- read.csv(file.path(main_dir, "Data", "index_dt.csv")) %>% 
  filter(model_number == 74) %>%  
  dplyr::select(c(time, estimate, cv)) %>% 
  mutate(seas = 7,
         index = 4,
         time = time+1,
         obs_log = sqrt(log(1+cv^2))) %>% 
  rename(yr = time,
         obs = estimate) %>% 
  select(yr, seas, index, obs, obs_log)
## FRS CPUE, BFISH camera, and BFISH research fishing
CPUE <- read.csv(file.path(main_dir, "Data", "cpue_paka_2023.csv")) %>% 
  mutate(seas = 7,
         index = 1) %>% 
  rename("obs" = "Mean_index",
        "stderr" = "SE_index",
        "yr" = "X") %>% 
  mutate(cv = stderr/obs,
         obs_log = sqrt(log(1+cv^2))) %>% 
  select(yr, seas, index, obs, obs_log) %>% 
  bind_rows(bfish_cpue) %>% 
  bind_rows(bfish_fish_cpue) %>% 
  filter(yr > 1948 & !is.na(obs)) %>%
  arrange(index, yr)

# Length comps
BIN_SIZE = 5
load(file.path(main_dir, "Data", "2022.lencomp_dt.RData"))
load(file.path(main_dir, "Data", "2022.sample_size_dt.RData"))
rc_bfish_len <- read.csv(file.path(main_dir, "Outputs", "Reconstructed_BFISH_SizeComp.csv"))

BFISH.LENGTHS <- lencomp_dt %>% 
  filter(gear == "camera" & all_lengths == TRUE & data_treatment == 1)
BFISH.fish <- lencomp_dt %>% 
  filter(gear == "research fishing" & all_lengths == TRUE & data_treatment == 1)
Ninput <- sample_size_dt %>% 
  filter(gear_type == "camera" & all_lengths == TRUE & data_treatment == 1) %>% 
  pull( input_sample_size)
Ninput.fish <- sample_size_dt %>% 
  filter(gear_type == "research_fishing" & all_lengths == TRUE & data_treatment == 1) %>% 
  pull( input_sample_size)

bfish_len <- BFISH.LENGTHS
bfish_len$LENGTH_BIN_START <- bfish_len$length_cm-(bfish_len$length_cm%%BIN_SIZE)
lencomp <- bfish_len %>% 
  group_by(year, LENGTH_BIN_START) %>%
  summarise(Nsamp = n()) %>%
  mutate(LENGTH_BIN_START = paste0("l", LENGTH_BIN_START)) %>% 
  pivot_wider(names_from = LENGTH_BIN_START, values_from = Nsamp, values_fill = 0) %>% 
  ungroup() %>% 
  mutate(Yr = as.numeric(year) + 1, 
         Seas = 1, 
         FltSvy = 2, 
         Gender = 0, 
         Part = 0,
         Nsamp = Ninput) %>% 
  select(Yr, Seas, FltSvy, Gender, Part, Nsamp, starts_with("l"))
## NOTE: If using super-years, need to combine data from years before putting into model. Open lencomp df in excel and set super-years and combine data onto rows with the positive fleet. Also will need to rearrange the column order in excel so that length bins are going from left to right in increasing order.
write.csv(lencomp, file = file.path(main_dir, "Outputs", "bfish_camera_lencomp_raw.csv"), row.names = F)

bfish_len_fish <- BFISH.fish
bfish_len_fish$LENGTH_BIN_START <- bfish_len_fish$length_cm-(bfish_len_fish$length_cm%%BIN_SIZE)
lencomp_fish <- bfish_len_fish %>% 
  group_by(year, LENGTH_BIN_START) %>% 
  summarise(Nsamp = n()) %>% 
  mutate(LENGTH_BIN_START = paste0("l", LENGTH_BIN_START)) %>% 
  pivot_wider(names_from = LENGTH_BIN_START, values_from = Nsamp, values_fill = 0) %>% 
  ungroup() %>% 
  mutate(Yr = as.numeric(year) + 1, 
         Seas = 1, 
         FltSvy = 4, 
         Gender = 0, 
         Part = 0,
         Nsamp = Ninput.fish) %>% 
  select(Yr, Seas, FltSvy, Gender, Part, Nsamp, starts_with("l"))
## NOTE: If using super-years, need to combine data from years before putting into model. Open lencomp df in excel and set super-years and combine data onto rows with the positive fleet
write.csv(lencomp_fish, file = file.path(main_dir, "Outputs", "bfish_fishing_lencomp_raw.csv"), row.names = F)
# Size-frequency  
# FRS catch data
FRS.LENGTHS_early <- read.csv(file.path(main_dir, "Data", "PICDR-113273 HDAR Commercial Catch Calendar Year 1948 to Fiscal Year 1993.csv")) %>% 
  filter(SPECIES == 19)
FRS.LENGTHS_late <- read.csv(file.path(main_dir, "Data", "PICDR-113273 HDAR Commercial Catch Fiscal Year 1994 to 2022-10-26.csv")) %>% 
  filter(SPECIES == 19)
FRS.LENGTHS <- rbind(FRS.LENGTHS_early, FRS.LENGTHS_late)
# --Use only records from the MHI------------------------------------
#Read in key table from last assessment for MHI areas (mhi_areas.csv)
#but keep only those grids Reg Kokbun identified as valid from the
#CPUE data workshop
areasReggie=read.csv(file.path(main_dir, "Data", "BF_Area_Grid_Reggie.csv"),header=T)
valid=areasReggie[which(areasReggie$Valid.==""),]$area

#Remove non-valid subareas (A and B) from area 16123 as well as 
#records from 16123 without a subarea specified
####----#####
mhidata=FRS.LENGTHS[FRS.LENGTHS$AREA%in%valid,] #valid areas
mhidata=mhidata[!mhidata$SUBAREA%in%c("A","B"),] #remove known invalid subareas (16123A and 16123B)
mhidata=mhidata[!(mhidata$AREA==16123 & is.na(mhidata$SUBAREA)),] #remove the 16123 records without a subarea distinction
##1-fish trip weight frequencies
frs.wgt <- mhidata %>% 
  filter(FYEAR >= 1949) %>% 
  filter(CAUGHT == 1 & LBS <= 21) %>% ## select 1-fish trips where fish is less than state record weight to filter out any mistakes
  select(FYEAR, LBS) %>% 
  mutate(LBS = round(LBS))
frs.effN <- frs.wgt %>% 
  group_by(FYEAR) %>% 
  summarise(Nsamp = n())

frs.wgtcomp <- frs.wgt %>% 
  group_by(FYEAR, LBS) %>% 
  summarise(N = n()) %>% 
  mutate(LBS = paste0("w", LBS)) %>% 
  pivot_wider(names_from = LBS, values_from = N, values_fill = 0) %>% 
  ungroup() %>% 
  mutate(
    method = 1,
    year = FYEAR,
    month = 7,
    fleet = 2,
    gender = 0, 
    Part = 0, 
    Nsamp = frs.effN$Nsamp
  ) %>% 
  select(method, year, month, fleet, gender, Part, Nsamp, contains("w"))

write.csv(frs.wgtcomp, file.path(main_dir, "Outputs", "weight.comp.csv"))

## Writing data to data.ss file
data <- SS_readdat_3.30(file = file.path(main_dir, "Model", "19_Base", "data.ss"))
data$Nfleets <- 4
data$styr <- 1949
data$endyr <- 2023
data$catch <- CATCH 
data$fleetinfo <- data.frame(type = c(1,3,1,3), surveytiming = c(-1,1,-1,1), area = c(1,1,1,1),
                             units = c(1,1,1,1), need_catch_mult = c(0,0,0,0), 
                             fleetname = c("FRS", "BFISH", "Non_comm", "BFISH_ResFish"))
data$CPUE <- CPUE
data$CPUEinfo <- data.frame(Fleet = c(1,2,3,4), Units =c(1,1,1,1), Errtype = c(0,0,0,0), SD_Report = c(0,0,0,0))


##data$lencomp manually add after combining super years and rearranging column order for bfish_camera_lencomp_raw.csv and bfish_fishing_lencomp_raw.csv
## Need to do the following manually or after you add in length comps so info matches expected 
# data$lbin_vector <- sort(unique(bfish_len$LENGTH_BIN_START))
# data$N_lbins <- length(data$lbin_vector)
# data$len_info <- data.frame(
#   mintailcomp = rep(0, 3),
#   addtocomp = rep(0.0001, 3), 
#   combine_M_F = rep(0,3),
#   CompressBins = rep(0,3), 
#   CompError = rep(1,3),
#   ParmSelect = c(1,1,1),
#   minsamplesize = c(.1, .1, .1)
# )

SS_writedat_3.30(data, outfile = file.path(main_dir, "Model", "19_Base", "data.ss"), overwrite = TRUE)
## Manually change settings and parameters for control file
