library(tidyverse)
library(haven)
library(here)
library(sf)
library(mapview)
library(lubridate)
library(tbeptools)

prj <- 4326
tbsegshedcr <- st_make_valid(tbsegshed)

# macroalgae relevant descriptions im fimraw
dscrp <- c('Acanthophora spp.', 'Algae: Drift', 'Algae: Filamentous green', 'Algae: Filamentous red', 'Algae: Mixed', 'Caulerpa spp.', 'Dapis/Lyngbya spp. (filamentous cyanobacteria)', 'Gracilaria spp.', 'Ulva spp.', 'None')

# raw data ----------------------------------------------------------------

# import original SAS data
phyraw <- read_sas(here('data/raw/tbm_physical.sas7bdat'))
habraw <- read_sas(here('data/raw/tbm_habitat.sas7bdat'))
fimraw <- read_sas(here('data/raw/fim_codes.sas7bdat'))
hydraw <- read_sas(here('data/raw/tbm_hydrolab.sas7bdat'))

# process data ------------------------------------------------------------

# physical data
# select AM projects (monhtly FIM sampling)
# select gear 
#     20 (21.3m offshore and shoreline seines)
#     160 (183m haul seines)
#     300/301 (6.1m otter trawls)
#     23 (21-m boat seines)
# zones A-E for TB proper, then clip by TB segments
# year >= 1998, but make sure to use only 2005 to present for trawls (300/301)
phydat <- phyraw %>% 
  mutate(date = ymd(date)) %>% 
  filter(Project_1 == 'AM'| Project_2 == 'AM' | Project_3 == 'AM') %>% 
  filter(Gear %in% c(20, 160, 300, 301)) %>% 
  filter(Zone %in% c('A', 'B', 'C', 'D', 'E')) %>% 
  filter(year(date) >= 1998) %>% 
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = prj) %>% 
  .[tbsegshedcr, ] %>% 
  select(date, Reference, BycatchQuantity, Gear, Dist_tow)

# fim codes
# select the codes that apply to the by catch in the habitat data
fimdat <- fimraw %>% 
  filter(FieldName == 'BycatchType') %>% 
  select(ByCatchType = Code, Description)

# habitat data
# join with fimdat, filter those not matched (only four recs)
habdat <- habraw %>% 
  select(Reference, ByCatchType, ByCatchRatio) %>% 
  left_join(fimdat, by = 'ByCatchType') %>% 
  filter(!is.na(Description))

# combine phydat and habdat
# get cpue using conversion factors
# remove duplicate true negatives
# intersect with bay segments
alldat <- phydat %>% 
  inner_join(habdat, by = 'Reference') %>% 
  mutate(
    conv = case_when(
      Gear == 20 ~ 140 / 100,
      Gear == 160 ~ 4120 / 100,
      Gear %in% c(300, 301) ~ (Dist_tow * 4 * 1853) / 100
    ), 
    cpue_gper100m2 = ByCatchRatio * 0.1 * (BycatchQuantity / conv),
    ByCatchType = gsub('^\\.$', 'NO', ByCatchType), 
    Description = gsub('^No bycatch type recorded \\(null\\)$', 'None', Description)
  ) %>% 
  unique() %>% 
  st_intersection(tbsegshedcr) %>% 
  select(date, Reference, Gear, Description, cpue_gper100m2, long_name, bay_segment)

# this dataset selects only gear 20 and 160 (used in Hall et al.)
# filter by major bay segments
# filters by relevant descriptors for macroalgae, including empty sets
# removes 'None' from References that have macroalgae 
# changes all other macroalgae descriptors to mixed
# then takes an average at each reference
algdat <- alldat %>% 
  st_set_geometry(NULL) %>% 
  filter(Gear %in% c(20, 160)) %>% 
  filter(bay_segment %in% c('HB', 'LTB', 'MTB', 'OTB')) %>% 
  filter(Description %in% dscrp) %>% 
  group_by(Reference) %>%
  filter(
    if(any(Description != 'None')) 
      Description != 'None'
    else
      T
  ) %>% 
  ungroup() %>% 
  mutate(
    cpue_gper100m2 = ifelse(is.na(cpue_gper100m2), 0, cpue_gper100m2)
  ) %>% 
  group_by(date, Gear, long_name, bay_segment) %>% 
  summarise(
    cpue_gper100m2 = mean(cpue_gper100m2), 
    .groups = 'drop'
  ) %>% 
  mutate(
    bay_segment = factor(bay_segment, levels = c('OTB', 'HB', 'MTB', 'LTB')), 
    Gear = factor(Gear, levels = c(20, 160), labels = c('21.3m', '183m')),
    mo = month(date, label = T),
    yr = year(date),
    doy = yday(date)
  ) 

# save output -------------------------------------------------------------

save(alldat, file = here('data/alldat.RData'))

csvout <- alldat %>% 
  mutate(
    Longitude = st_coordinates(.)[, 1], 
    Latitude = st_coordinates(.)[, 2]
  ) %>% 
  st_set_geometry(NULL)

write.csv(csvout, here('data/raw/csvout.csv'))

save(algdat, file = here('data/algdat.RData'))
