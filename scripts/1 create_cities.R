pacman::p_load(rio,
               tidyverse,
               magrittr,
               sf,
               tmap,
               raster,
               fasterize,
               tictoc,
               future,
               furrr,
               haven,
               stargazer,
               fixest,
               zoo,
               knitr,
               magick,
               plyr)


path_paper <- paste0(getwd(), "/papers_jeppe/8 trade/")
path_paper

# ---------------------------------------------------------
# Load data
# ---------------------------------------------------------
# ----- City data: load
cities <- haven::read_dta("2_data in stages/9_anatem.dta",
                                 encoding = "utf8") %>% 
  as.data.frame() %>% 
  mutate_all(na_if,"") %>% 
  filter(!is.na(longitude) & !is.na(latitude)) # remove cities without coordinates

# create temporary cities at some point within a Centennia polygon
temp <- cities %>%
  filter(source_pol_control == "CENTENNIA") %>% 
  distinct(., city_id, .keep_all = T)

# subset cities to those at some point within a Centennia polyogn + the period 1000-2000
cities <- cities %>%
  filter(city_id %in% temp$city_id) %>%
  filter(year %in% seq(1000, 2000, 10))
rm(temp)

# select necessary variables
cities <- cities %>% 
  dplyr::select(c(city,
                  country,
                  latitude,
                  longitude,
                  year,
                  city_id,
                  city_pop_i,
                  pol_control_i,
                  has_cap,
                  is_cap,
                  cap_city,
                  cap_country,
                  cap_lat,
                  cap_lon,
                  city_pop,
                  pol_control,
                  GEOUNIT,
                  SOVEREIGNT,
                  SUBREGION,
                  REGION_WB,
                  REGION_UN,
                  CONTINENT,
                  ADM0_A3,
                  ISO_A3,
                  cap_SUBREGION,
                  cap_REGION_WB,
                  cap_REGION_UN,
                  cap_CONTINENT,
                  cap_SOVEREIGNT,
                  source,
                  intpl_length,
                  source_pol_control)) %>% 
  as.data.frame()

# ----- City data: remove cities never above 5k
# temporary dataset with max pop. values
pop_max <- cities %>% 
  filter(!is.na(city_pop)) %>% 
  group_by(city_id) %>% 
  slice(base::which.max(city_pop)) %>% 
  ungroup() %>% 
  arrange(country, city) %>% 
  filter(city_pop >= 5000)

# remove cities that never made it above 5k
cities <- cities %>%
  filter(city_id %in% pop_max$city_id)
rm(pop_max)

# ----- City data: create polity factor variables (different variable format, categorical, needed for FE)
cities <- cities %>% 
  dplyr::rename(state = pol_control,
                pop = city_pop_i) %>% 
  mutate(year_fac = factor(year),
         state_fac = factor(state),
         city_id_fac = factor(city_id))

# ----- Create century variables + halfcentury variables
# century for 1740 = 1700
# century for 1750 = 1700
# halfcentury for 1740 = 1700
# halfcentury for 1750 = 1750
cities <- cities %>% 
  mutate(century = factor(plyr::round_any(year, 100, floor))) %>% 
  mutate(halfcentury = factor(plyr::round_any(year, 50, floor))) %>% 
  mutate(century_th = as.numeric(substring(as.character(century), 1, 2))) %>% 
  # for potential plotting purposes, "15th century"
  mutate(century_th = century_th + 1) %>% 
  mutate(century_th = factor(paste(century_th,
                                   "th",
                                   sep = "")))

# create indicators for different temporal data structures This allows us to to subset to e.g. 50yr intervals
cities <- cities %>% 
  mutate(has_source = factor(ifelse(!is.na(source), 1, 0))) %>%
  mutate(round_20 = factor(plyr::round_any(year, 20)),
         round_30 = factor(plyr::round_any(year, 30)),
         round_40 = factor(plyr::round_any(year, 40)),
         round_50 = factor(plyr::round_any(year, 50)),
         round_60 = factor(plyr::round_any(year, 60)),
         round_70 = factor(plyr::round_any(year, 70)),
         round_90 = factor(plyr::round_any(year, 90)),
         round_80 = factor(plyr::round_any(year, 80)),
         round_100 = factor(plyr::round_any(year, 100))) %>% 
  mutate(is_20 = factor(ifelse(year == round_20, 1, 0)),
         is_30 = factor(ifelse(year == round_30, 1, 0)),
         is_40 = factor(ifelse(year == round_40, 1, 0)),
         is_50 = factor(ifelse(year == round_50, 1, 0)),
         is_60 = factor(ifelse(year == round_60, 1, 0)),
         is_70 = factor(ifelse(year == round_70, 1, 0)),
         is_80 = factor(ifelse(year == round_80, 1, 0)),
         is_90 = factor(ifelse(year == round_90, 1, 0)),
         is_100 = factor(ifelse(year == round_100, 1, 0))) %>% 
  dplyr::select(-c(round_20, round_30, round_40, round_50, round_60,
                   round_70, round_80, round_90, round_100))

# ---------------------------------------------------------
# Export data
# ---------------------------------------------------------
# ----- Full sample
temp <- cities

save(temp,
     file = paste0(path_paper,
                   "data/df_full/cities_raw.Rdata"))

# ----- Balanced sample: only cities present from 1000-2000
temp <- cities %>%
  group_by(city_id) %>% 
  dplyr::mutate(fyear = min(year),
                lyear = max(year)) %>% 
  ungroup() %>% 
  dplyr::mutate(at_start = ifelse(fyear == 1000, 1, 0),
                at_end = ifelse(lyear == 2000, 1, 0)) %>% 
  dplyr::mutate(in_balance = at_start * at_end) %>% 
  dplyr::select(-c(at_start, at_end))

temp <- temp %>% 
  filter(in_balance == 1)

save(temp,
     file = paste0(path_paper,
                   "data/df_balanced/cities_raw.Rdata"))


# ----- Non-interpolated sample
temp <- cities %>% 
  filter(!is.na(city_pop))

save(temp,
     file = paste0(path_paper,
                   "data/df_pure/cities_raw.Rdata"))

