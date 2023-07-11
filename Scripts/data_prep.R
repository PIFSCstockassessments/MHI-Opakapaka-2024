pacman::p_load("this.path", "tidyverse","data.table","readxl","StepwiseLH","LBSPR")

main_dir <- this.path::here(..=1)
LH <- read_excel(file.path(main_dir,"Data","Priors Data",paste0("Deep7_LH.xlsx"))) 

S.CODE <- LH%>% 
  filter(SEX=="F") %>% 
  select(SCIENTIFIC_NAME,SPECIES,SPECIES_ID,LW_A,LW_B,BINWIDTH)

BFISH.LENGTHS <- fread(file.path(main_dir,"Data", "BFISH_Res_Fishing_Lengths.csv")) %>% 
  filter(SPECIES_CD == "PRFI" & !is.na(LENGTH_CM)) %>% 
  separate(col = BFISH, into = c("BFISH", "Year", "seas"), sep = "_") %>% 
  select(-c(V1, SPECIES_CD, SCIENTIFIC_NAME, COMMON_NAME, WEIGHT_LB, BFISH)) %>% 
  mutate(Year = as.numeric(Year)+1)

CATCH <- read.csv(file.path(main_dir, "Data", "Opakapaka_catch.csv")) %>% 
  mutate(Catch = Catch/1000000) %>% 
  filter(Year > 1948) %>% 
  mutate(seas = 1,
         fleet = 1, 
         catch_se = .01) %>% 
  select(Year, seas, fleet, Catch, catch_se)

bfish_cpue <-read.csv(file.path(main_dir, "Data", "BFISH_index.csv")) %>% 
  filter(model_number == 69) %>%  #jb.params$BFISH_index_mod
  filter(str_detect(category, "PRFI")) %>% 
  dplyr::select(c(time, estimate, cv)) %>% 
  mutate(month = 7,
         fleet = 2,
         time = time+1) %>% 
  rename(yr = time,
         obs = estimate, 
         stderr = cv) %>% 
  select(yr, month, fleet, obs, stderr)

CPUE <- read.csv(file.path(main_dir, "Data", "opakapaka_FRS.csv")) %>% 
  mutate(month = 7,
         fleet = 1) %>% 
  mutate(stderr = stderr/obs) %>% 
  select(yr, month, fleet, obs, stderr) %>% 
  bind_rows(bfish_cpue) %>% 
  filter(yr > 1948)

