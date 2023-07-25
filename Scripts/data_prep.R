pacman::p_load("this.path", "tidyverse","data.table","readxl","r4ss")

main_dir <- this.path::here(..=1)
#LH <- read_excel(file.path(main_dir,"Data","Priors Data",paste0("Deep7_LH.xlsx"))) 
# 
# S.CODE <- LH%>% 
#   filter(SEX=="F") %>% 
#   select(SCIENTIFIC_NAME,SPECIES,SPECIES_ID,LW_A,LW_B,BINWIDTH)

BFISH.LENGTHS <- fread(file.path(main_dir,"Data", "paka_catch_2016_2022.csv")) %>% 
  filter(SPECIES_CD == "PRFI" & !is.na(LENGTH_CM)) %>% #& LENGTH_CM >= 29 filter for fish at 29cm or above bc that was done for index standardization (29cm = size limit)
  select(c(YEAR, SAMPLE_ID, LENGTH_CM)) %>% 
  mutate(YEAR = as.numeric(YEAR)+1,
         GROUP = "RESFISH")

CAM.LENGTHS <- read.csv(file.path(main_dir, "Data", "2022_CAM_LENGTHS.csv")) %>% 
  filter(SPECIES_CD == "PRFI" & MEAN_MM >= 290) %>% #& MEAN_MM >= 290
  separate(col = BFISH, into = c("BFISH", "YEAR", "Seas")) %>% 
  mutate(LENGTH_CM = MEAN_MM/10,
         GROUP = "CAM",
         YEAR = as.numeric(YEAR) + 1) %>% 
  select(YEAR, DROP_CD, LENGTH_CM, GROUP)


psu <- read.csv(file.path(main_dir, "Data", "2022_CAM_SAMPLE_TIME.csv")) 

N.samp.cam <- CAM.LENGTHS %>% merge(psu, by = "DROP_CD") %>% 
  select(PSU, DROP_CD, YEAR, LENGTH_CM, GROUP) %>% 
  group_by(YEAR) %>% 
  summarise(Nsamp_cam = length(unique(PSU))) #effN is aggregated to the number of unique PSU sampling areas per year

N.samp.fishing <- BFISH.LENGTHS %>% 
  group_by(YEAR) %>% 
  summarise(Nsamp_fishing = length(unique(SAMPLE_ID))) #SAMPLE_ID is already aggregated at PSU level

## TODO: Do we need to adjust input sample size (before DM adjustment?) When I plot the distributions by sample ID, there are only a few with clustering, so maybe ok for now? Double check with everyone.
BFISH.LENGTHS %>% 
  ggplot(aes(x = YEAR, y = LENGTH_CM, GROUP = SAMPLE_ID, color = SAMPLE_ID)) + geom_point(show.legend = FALSE)

BFISH.LENGTHS %>% 
  ggplot(aes(x = LENGTH_CM, GROUP = SAMPLE_ID)) + 
  geom_histogram(show.legend = FALSE) + facet_wrap(~SAMPLE_ID)

Neff <- N.samp.cam$Nsamp_cam

Neff <- BFISH.LENGTHS %>% 
  group_by(YEAR) %>% 
  summarise(Nsamp_fishing = n()) %>% 
  pull(Nsamp_fishing)

#Compare length comps for camera and fishing
bind_rows(BFISH.LENGTHS, CAM.LENGTHS) %>% 
  select(-c(SAMPLE_ID, DROP_CD)) %>% 
  ggplot(aes(x = LENGTH_CM, y = after_stat(density), group = GROUP, color = GROUP)) +
  geom_freqpoly() +
  theme_classic()

CATCH <- read.csv(file.path(main_dir, "Data", "Opakapaka_catch.csv")) %>% 
  pivot_longer(cols = -Year, names_to = "fleet_name", values_to = "catch") %>% 
  mutate(catch = catch*0.000453592) %>% #convert to mt
  filter(Year > 1948) %>% 
  mutate(seas = 1,
         fleet = ifelse(fleet_name == "Commercial",2,3), 
         catch_se = .01) %>% 
  rename(year = Year) %>% 
  select(year, seas, fleet, catch, catch_se) %>% 
  arrange(fleet, year) 

CATCH %>% group_by(fleet) %>% slice_tail(n = 3) %>% summarise(mean(catch))  #0.0677, 0.0700

initcatch <- data.frame(year = c(-999,-999), seas = c(1,1), fleet = c(2,3), catch = c(0,0), catch_se = c(0.01,0.01))
catch23 <- data.frame(year = c(2023,2023), seas = c(1,1), fleet = c(2,3), catch = c(0.0677,0.0700), catch_se = c(0.01,0.01))
CATCH <- bind_rows(initcatch, CATCH, catch23) %>% arrange(fleet, year)
  

bfish_cpue <-read.csv(file.path(main_dir, "Data", "BFISH_index.csv")) %>% 
  filter(model_number == 69) %>%  #jb.params$BFISH_index_mod
  filter(str_detect(category, "PRFI")) %>% 
  dplyr::select(c(time, estimate, cv)) %>% 
  mutate(seas = 7,
         index = 1,
         time = time+1) %>% 
  rename(yr = time,
         obs = estimate, 
         obs_log = cv) %>% 
  select(yr, seas, index, obs, obs_log)

CPUE <- read.csv(file.path(main_dir, "Data", "opakapaka_FRS_cpue.csv")) %>% 
  mutate(seas = 7,
         index = 2) %>% 
  rename("obs" = "Mean_index",
        "stderr" = "SE_index") %>% 
  mutate(obs_log = stderr/obs) %>% 
  select(yr, seas, index, obs, obs_log) %>% 
  bind_rows(bfish_cpue) %>% 
  filter(yr > 1948) %>% 
  arrange(index, yr)

# Length comps
BIN_SIZE = 5
# bfish_len <- bind_rows(BFISH.LENGTHS, CAM.LENGTHS) %>% 
#   select(-c(SAMPLE_ID, DROP_CD)) 
bfish_len <- CAM.LENGTHS
bfish_len$LENGTH_BIN_START <- bfish_len$LENGTH_CM-(bfish_len$LENGTH_CM%%BIN_SIZE)
lencomp <- bfish_len %>% 
  group_by(YEAR, LENGTH_BIN_START) %>% 
  summarise(Nsamp = n()) %>% 
  mutate(LENGTH_BIN_START = paste0("l", LENGTH_BIN_START)) %>% 
  pivot_wider(names_from = LENGTH_BIN_START, values_from = Nsamp, values_fill = 0) %>% 
  ungroup() %>% 
  mutate(Yr = YEAR, 
         Seas = 1, 
         FltSvy = 1, 
         Gender = 0, 
         Part = 0,
         Nsamp = Neff) %>% 
  select(Yr, Seas, FltSvy, Gender, Part, Nsamp, starts_with("l"))
## NOTE: If using super-years, need to combine data from years before putting into model. Open lencomp df in excel and set super-years and combine data onto rows with the positive fleet
write.csv(lencomp, file = file.path(main_dir, "Data", "bfish_camera_lencomp.csv"))
# Mean weight 
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
####----#####
mean_weights <- mhidata %>% 
  filter(CAUGHT < 500) %>%  #TODO: Ask John if this is a good cut off or should it be higher/lower
  filter(CAUGHT > 0 & LBS <= CAUGHT * 21) %>% 
  mutate(kg = LBS / 2.205,
         kg_per_trip = kg/CAUGHT) %>% 
  group_by(FYEAR) %>% 
  summarise(mean_kg_per_trip = mean(kg_per_trip),
            N_caught = sum(CAUGHT), 
            W_caught = sum(kg), 
            mean_caught = W_caught/N_caught)
  
sd(mean_weights$mean_caught)
sd(mean_weights$mean_kg_per_trip)
mean_weights %>% ggplot(aes(x = FYEAR)) +
  geom_point(aes(y = mean_kg_per_trip), color = "blue") +
  geom_point(aes(y = mean_caught), color = "orange")

mean_weight_df <- mean_weights %>% 
  select(FYEAR, mean_kg_per_trip) %>% 
  mutate(Month = 7,
         Fleet = 2,
         Partition = 0, 
         Type = 2,
         CV = 0.19/mean_kg_per_trip) %>% 
  rename("Year" = FYEAR,
         "Observation" = mean_kg_per_trip) %>% 
  select(Year, Month, Fleet, Partition, Type, Observation, CV)

### FRS length comps
# frs.len <- mhidata %>% 
#   filter(FYEAR >= 1949) %>% 
#   filter(CAUGHT == 1 & LBS <= 21) %>% 
#   mutate(kg = (LBS / 2.205),
#          L = (kg/1.75e-05)^(1/2.99)) %>% 
#   select(FYEAR, kg, L)
# frs.effN <- frs.len %>% 
#   group_by(FYEAR) %>% 
#   summarise(Nsamp = n())
frs.len$LENGTH_BIN_START <- frs.len$L-(frs.len$L%%BIN_SIZE)
frs.lencomp <- frs.len %>% 
  rename("Year" = "FYEAR") %>% 
  mutate(Year = as.numeric(Year),
         LENGTH_BIN_START = as.numeric(LENGTH_BIN_START)) %>% 
  group_by(Year, LENGTH_BIN_START) %>% 
  summarise(Nsamp = n()) %>%
  mutate(LENGTH_BIN_START = paste0("l", LENGTH_BIN_START)) %>% 
  pivot_wider(names_from = LENGTH_BIN_START, values_from = Nsamp, values_fill = 0) %>% 
  ungroup() %>% 
  mutate(Yr = Year, 
         Seas = 1, 
         FltSvy = 2, 
         Gender = 0, 
         Part = 0,
         Nsamp = frs.effN$Nsamp
  ) %>% 
  select(Yr, Seas, FltSvy, Gender, Part, Nsamp, starts_with("l"))
write.csv(frs.lencomp,file.path(main_dir, "Model", "07_FRS1_lencomp", "lencomp.csv"))


frs.wgt <- mhidata %>% 
  filter(FYEAR >= 1949) %>% 
  filter(CAUGHT == 1 & LBS <= 21) %>% 
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

write.csv(frs.wgtcomp, file.path(main_dir, "Model", "07_FRS1_lencomp", "weight.comp.csv"))

# UFA Length Data
ufa <- read.csv(file.path(main_dir, "Data", "UFA_paka_lengths.csv"))

ufa.effN <- ufa %>% 
  # group_by(Year, Date) %>% 
  # summarise(Ntrips = n_distinct(Vessel)) %>% 
  # ungroup() %>% 
  group_by(Year) %>% 
  summarise(Nsamp = n()) # sum(Ntrips)

ufa$length_bin_start <- ufa$FL-(ufa$FL%%BIN_SIZE)
ufa.lencomp <- ufa %>% 
  group_by(Year, length_bin_start) %>% 
  summarise(Nsamp = n()) %>% 
  mutate(length_bin_start = paste0("l", length_bin_start)) %>% 
  pivot_wider(names_from = length_bin_start, values_from = Nsamp, values_fill = 0) %>% 
  ungroup() %>% 
  mutate(Yr = Year, 
         Seas = 1, 
         FltSvy = 2, 
         Gender = 0, 
         Part = 0,
         Seas = ifelse(Yr == 2019|Yr == 2020, -1, 1),
         FltSvy = ifelse(Yr == 2020, -2, 2),
         Nsamp = ufa.effN$Nsamp
         ) %>% 
  select(Yr, Seas, FltSvy, Gender, Part, Nsamp, starts_with("l"))


## Writing data to data.ss file
data <- SS_readdat_3.30(file = file.path(main_dir, "Model", "07_FRS1_lencomp", "data.ss"))
data$Nfleets <- 3
data$styr <- 1949
data$endyr <- 2023
data$catch <- CATCH 
data$fleetinfo <- data.frame(type = c(3,1,1), surveytiming = c(1,-1,-1), area = c(1,1,1),
                             units = c(1,1,1), need_catch_mult = c(0,0,0), 
                             fleetname = c("BFISH", "FRS", "Non_comm"))
data$CPUE <- CPUE
data$CPUEinfo <- data.frame(Fleet = c(1,2,3), Units =c(1,1,1), Errtype = c(0,0,0), SD_Report = c(0,0,0))


data$lencomp <- as.data.frame(frs.lencomp)
data$lbin_vector <- sort(unique(bfish_len$LENGTH_BIN_START))
data$N_lbins <- length(data$lbin_vector)
data$len_info <- data.frame(
  mintailcomp = rep(0, 3),
  addtocomp = rep(0.0001, 3), 
  combine_M_F = rep(0,3),
  CompressBins = rep(0,3), 
  CompError = rep(1,3),
  ParmSelect = c(1,1,1),
  minsamplesize = c(.1, .1, .1)
)

data$use_meanbodywt <- 1
data$meanbodywt <- as.data.frame(mean_weight_df)
data$DF_for_meanbodywt <- 75

SS_writedat_3.30(data, outfile = file.path(main_dir, "Model", "06_BFISH_all", "data.ss"), overwrite = TRUE)
