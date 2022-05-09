library(tidyverse)
library(haven)
library(here)
library(sf)
library(mapview)
library(lubridate)

prj <- 4326
tbsegshedcr <- st_make_valid(tbsegshed)

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
alldat <- phydat %>% 
  inner_join(habdat, by = 'Reference')


