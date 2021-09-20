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
               stringi,
               broom,
               hrbrthemes,
               readr,
               nngeo,
               plyr)

path_data <- paste0(getwd(), "/papers_jeppe/8 trade/data/df_full/")
path_data

path_paper <- paste0(getwd(), "/papers_jeppe/8 trade/")
path_paper


# ============================================================================
# CALCULATE URBAN POTENTIAL
# ============================================================================

# ---------------------------------------------------------
# Load data
# ---------------------------------------------------------
cities <- rio::import(paste0(path_data,
                             "cities_raw.Rdata")) %>% 
  mutate_all(na_if,"")

# ---------------------------------------------------------
# Create spatial distance matrix
# ---------------------------------------------------------
# I create two full set of cities (identical sets), only unique ones because coordinates are time-invariant
pts1 <- cities %>% 
  distinct(., city_id, .keep_all = TRUE) %>% # unique cities
  dplyr::select(city_id, latitude, longitude) %>% # only need city_id and coordinates
  st_as_sf(.,
           coords = c("longitude", "latitude"),
           crs = 4326) # convert to spatial feature

# repeat
pts2 <- cities %>% 
  distinct(., city_id, .keep_all = TRUE) %>%
  dplyr::select(city_id, latitude, longitude) %>% 
  st_as_sf(.,
           coords = c("longitude", "latitude"),
           crs = 4326)

# create distance matrix between any city j and any city i (includes a diagonal of distance == 0)
dist_unclass <- st_distance(pts1, pts2,
                            by_element = FALSE) %>% 
  unclass() %>% 
  as.data.frame()

nn <- ncol(dist_unclass) # in this case, a matrix of 2881 x 2881

# reshape the matrix to long
# this way we get a data format that is not 2881 x 2881 but instead:
# a data set where each row is a dyade of cities incl. the dyadic distance
dist_unclass <- dist_unclass %>% 
  pivot_longer(cols = everything(),
               names_to = "v_1",
               values_to = "dist") %>% 
  mutate(v_2 = paste("V", sort(rep(seq(1:nn),nn)),
                     sep = "")) %>%
  mutate(dist_km = dist / 1000) %>% 
  as.data.frame()

# remove dyades between the same cities
dist_unclass <- dist_unclass %>%
  filter(v_1 != v_2)

# create temporary version of the dataset two times, both will be merged on to what we just created
# this way we can identify city dyads
cit_1 <- cities %>% 
  distinct(., city_id, .keep_all = TRUE) %>%
  dplyr::mutate(v_1 = paste("V",row_number(), sep = "")) %>% 
  dplyr::mutate(city_id_1 = city_id) %>% 
  dplyr::select(c(v_1, city_id_1))

# repeat
cit_2 <- cities %>% 
  distinct(., city_id, .keep_all = TRUE) %>%
  dplyr::mutate(v_2 = paste("V",row_number(), sep = "")) %>% 
  dplyr::mutate(city_id_2 = city_id) %>% 
  dplyr::select(c(v_2, city_id_2))

# merge them onto the distance data
dist_unclass <- dist_unclass %>% 
  left_join(., cit_1, by = "v_1") %>%
  left_join(., cit_2, by = "v_2") %>% 
  arrange(city_id_1,
          dist_km,
          city_id_2) %>% 
  mutate(match_id = paste(city_id_1,
                          city_id_2,
                          sep = "_")) %>%  
  dplyr::select(c(match_id,
                  dist_km))

# for the Urban Potential formula, we need a denominator that is not less than 1. Otherwise it gets a bit funky
# i recode all dyadic distances less than 1km to be equal to 1km
# actually, the formula in the paper should reflect this: dyadic distance is capped at 1 (lower threshold)
dist_unclass <- dist_unclass %>% 
  mutate(dist_km = ifelse(dist_km >= 1, dist_km, 1))

min(dist_unclass$dist_km) #should be >= 1

nrow(dist_unclass) - (nrow(cit_1) * (nrow(cit_2) - 1)) # this should be zero (checks the dimensions)

rm(cit_1, cit_2, pts1, pts2) # clean up

# *************************************************************
# URBAN POTENTIAL ALL
# *************************************************************

# ---------------------------------------------------------
# Split up data
# ---------------------------------------------------------
# keep only Centennias codings of political authority. all other is NA
cit <- cities %>% 
  mutate(state = ifelse(source_pol_control == "CENTENNIA", state, NA)) %>% 
  dplyr::select(c(city_id,
                  pop,
                  year,
                  state)) %>% 
  as.data.frame()

# split it into a list so we can use parallel processing
# one list element for each year (= decade)
# = list of 101 elements (1000, 1010, ..., 2000)
cit_list <- split(cit,
                  f = cit$year)

rm(cit) # clean up

# ---------------------------------------------------------
# Define function: Urban Potential
# ---------------------------------------------------------
fun_corr <- function(cit, year_value, intercity, up_all, up_polity, up_nonpolity, df) {
  
  # save the year value
  year_value <- cit$year[1]
  
  # create city dyads
  intercity <- tidyr::expand_grid(cit, cit,
                                  .name_repair = "minimal")
  
  # rename variables so they are not identical
  colnames(intercity) <- c("city_id", "pop", "year", "state",
                           "city_id2", "pop2", "year2", "state2")
  
  # remove dyads of the same city
  intercity <- intercity %>%
    filter(city_id != city_id2) 
  
  # create a match id of the two city_id's
  intercity <- intercity %>%   
    dplyr::mutate(match_id = paste(city_id,
                                   city_id2,
                                   sep = "_")) %>% 
    # merge the dyadic distances we created above onto the dyads
    left_join(.,
              dist_unclass,
              by = "match_id")
  
  # calculate Urban Potential All
  up_all <- intercity %>% 
    dplyr::mutate(pop_dist = pop2 / dist_km) %>% 
    group_by(city_id) %>% 
    dplyr::summarise(up_all = sum(pop_dist, na.rm = T)) %>% 
    ungroup()
  
  # calculate Domestic Urban Potential
  up_polity <- intercity %>% 
    filter(!is.na(state)) %>% 
    filter(state == state2) %>% 
    dplyr::mutate(pop_dist = pop2 / dist_km) %>% 
    group_by(city_id) %>% 
    dplyr::summarise(up_polity = sum(pop_dist, na.rm = T)) %>% 
    ungroup()
  
  # calculate Foreign Urban Potential
  up_nonpolity <- intercity %>% 
    filter(!is.na(state)) %>% 
    filter(state != state2) %>% 
    dplyr::mutate(pop_dist = pop2 / dist_km) %>% 
    group_by(city_id) %>% 
    dplyr::summarise(up_nonpolity = sum(pop_dist, na.rm = T)) %>% 
    ungroup()
  
  # merge the three Urban Potential measures
  df <- up_all %>% 
    left_join(.,
              up_polity,
              by = "city_id") %>% 
    left_join(.,
              up_nonpolity,
              by = "city_id") %>% 
    dplyr::mutate(year = year_value)
  
  # df is our output
  return(df)
  
}


# ---------------------------------------------------------
# Apply the function we just created
# ---------------------------------------------------------
# ----- Mac
# (you might need to play around with this on Windows)
required_MB <- 1000 # we need some RAM space
options(future.globals.maxSize = required_MB*1024^2) # set the max RAM. Check your PC!

# set the number of cores. I use all cores but 1, so close ALL other applications
no_cores <- availableCores() - 1

# set up the parallel processing session
# depending on Mac/Windows you might need to work around with multisession/multicore/multiprocess
# see: https://cran.r-project.org/web/packages/future/vignettes/future-1-overview.html
# check your activity monitor, there should be several processes running
future::plan(multisession, workers = no_cores) 

# apply the function we just created to the list of 101 elements. this repeats the function with each year/decade
tic()
model_list <- future_map(cit_list, 
                         fun_corr, 
                         .progress = T,
                         .options = furrr_options(seed = TRUE,
                                                  scheduling = 1))
toc()
future::plan(sequential) # stop parallel processing

# ---------------------------------------------------------
# Unpack data
# ---------------------------------------------------------
# unpack the data from the list
model_data <- bind_rows(model_list, .id = "year_id")

# check your UP measures, only the up_all should never be NA
any(is.na(model_data$up_all))
any(is.na(model_data$up_polity))
any(is.na(model_data$up_nonpolity))

rm(model_list, cit_list, dist_unclass) # clean up


# ============================================================================
# CREATE DATA
# ============================================================================

# ---------------------------------------------------------
# Load data
# ---------------------------------------------------------
# ----- Centennia data
centennia <- st_read(dsn = "5_centennia/centennia_full",
                     layer = "centennia_full",
                     crs = 4326) %>% 
  mutate(area_km2 = unclass(st_area(.))/1000000)

# create a non-spatial version of the centennia
cent <- centennia %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry) %>% 
  arrange(state, year)

rm(centennia) # clean up

# ---------------------------------------------------------
# Prepare data
# ---------------------------------------------------------
# ----- Aggregate urban population data + number of cities by state
urban_pop <- cities %>% 
  filter(!is.na(state)) %>% 
  group_by(state, year) %>% 
  dplyr::summarize(urban_pop = sum(pop, na.rm = T),
                   city_count = n()) %>%
  ungroup()

# ----- Centennia data
cent <- cent %>% 
  dplyr::select(c(state, year, area_km2)) # select needed variables

# ----- Geodata
# load the geodata prepared in the geodata scripts
geodata <- rio::import(paste0(path_paper,
                              "data/geo_data/0_geodata_final.Rdata")) %>% 
  dplyr::select(-c(area_km2))

# ----- Border distance data
# load the border distance data prepared in the border dist scripts
border_dist <- rio::import(paste0(path_paper,
                                  "data/border_data/0_borderdata_final.Rdata")) %>% 
  mutate(border_all = border_distAll / 1000,
         border_polity = border_distPolity / 1000) %>% # convert to km instead of m
  dplyr::select(-c(border_distAll, border_distPolity))


# ---------------------------------------------------------
# Merge data
# ---------------------------------------------------------
# add urban potential data at the city-year level
cities <- cities %>% 
  tidylog::left_join(.,
                     model_data,
                     by = c("city_id", "year"))

# add aggregate urban data at the state-year level
cities <- cities %>% 
  tidylog::left_join(.,
                     urban_pop,
                     by = c("state", "year"))

# add centennia data at the state-year level
cities <- cities %>% 
  tidylog::left_join(.,
                     cent,
                     by = c("state", "year"))

# add geodata at the city-year level
cities <- cities %>%
  tidylog::left_join(.,
                     geodata,
                     by = c("city_id"))

# add border distance data at the city-year level
cities <- cities %>% 
  tidylog::left_join(.,
                     border_dist,
                     by = c("city_id", "year"))

# create one Muslim trade route dummy instead of many variables
cities <- cities %>%
  mutate(MuslimTradeDummy = case_when(year %in% seq(1100, 1290, 10) ~ Muslim1100,
                                      year %in% seq(1300, 1490, 10) ~ Muslim1300,
                                      year %in% seq(1500, 1690, 10) ~ Muslim1500,
                                      year %in% seq(1700, 2000, 10) ~ Muslim1700,
                                      year < 1100 ~ NA_real_)
  ) %>% 
  mutate(MuslimTradeDummy = case_when(year %in% seq(1100, 1290, 10) ~ muslimtr11,
                                      year %in% seq(1300, 1490, 10) ~ muslimtr13,
                                      year %in% seq(1500, 1690, 10) ~ muslimtr15,
                                      year %in% seq(1700, 2000, 10) ~ muslimtr17,
                                      year < 1100 ~ NA_real_)
  ) %>% 
  dplyr::select(-c(Muslim1100, Muslim1300, Muslim1500, Muslim1700,
                   muslimtr11, muslimtr13, muslimtr15, muslimtr17))

# create dummies for border distance: within 500km, 250km, ..., 10km
cities <- cities %>% 
  mutate(border500 = ifelse(border_all < 500, 1, 0),
         border250 = ifelse(border_all < 250, 1, 0),
         border100 = ifelse(border_all < 100, 1, 0),
         border050 = ifelse(border_all < 50, 1, 0),
         border025 = ifelse(border_all < 25, 1, 0),
         border010 = ifelse(border_all < 10, 1, 0))

# do the same for polity borders
cities <- cities %>% 
  mutate(p_border500 = ifelse(border_polity < 500, 1, 0),
         p_border250 = ifelse(border_polity < 250, 1, 0),
         p_border100 = ifelse(border_polity < 100, 1, 0),
         p_border050 = ifelse(border_polity < 50, 1, 0),
         p_border025 = ifelse(border_polity < 25, 1, 0),
         p_border010 = ifelse(border_polity < 10, 1, 0))

# save our original Domestic Urban Potential measure in a new variable
cities <- cities %>%
  mutate(up_polityOrig = up_polity)

# create new domestic urban potential measures, depending on the number of other cities within the state
# we don't want the measure to be too sensitive to a lack of other cities
cities <- cities %>%
  mutate(up_polity_one = ifelse(city_count > 0, up_polity, NA)) %>% # domestic urban potential for cities with at least one other domestic city
  mutate(up_polity_three = ifelse(city_count > 2, up_polity, NA)) %>% # at least three others
  mutate(up_polity_ten = ifelse(city_count > 9, up_polity, NA)) %>% # at least 10 others
  mutate(up_polity = ifelse(city_count > 4, up_polity, NA)) # at least five others

# create regional definitions
cities <- cities %>% 
  mutate(region_v2 = case_when(SUBREGION == "Eastern Europe" ~ "Eastern Europe",
                               SUBREGION == "Western Europe" ~ "Western Europe",
                               # Western part of Southern Europe
                               SUBREGION == "Southern Europe" & SOVEREIGNT == "Spain" ~ "Western Europe",
                               SUBREGION == "Southern Europe" & SOVEREIGNT == "Portugal" ~ "Western Europe",
                               SUBREGION == "Southern Europe" & SOVEREIGNT == "Italy" ~ "Western Europe",
                               SUBREGION == "Southern Europe" & SOVEREIGNT == "San Marino" ~ "Western Europe",
                               SUBREGION == "Southern Europe" & SOVEREIGNT == "Croatia" ~ "Western Europe",
                               SUBREGION == "Southern Europe" & SOVEREIGNT == "Malta" ~ "Western Europe",
                               SUBREGION == "Southern Europe" & SOVEREIGNT == "Slovenia" ~ "Western Europe",
                               # Eastern part of Southern Europe
                               SUBREGION == "Southern Europe" & SOVEREIGNT == "Greece" ~ "Eastern Europe",
                               SUBREGION == "Southern Europe" & SOVEREIGNT == "Bosnia and Herzegovina" ~ "Eastern Europe",
                               SUBREGION == "Southern Europe" & SOVEREIGNT == "Republic of Serbia" ~ "Eastern Europe",
                               SUBREGION == "Southern Europe" & SOVEREIGNT == "Albania" ~ "Eastern Europe",
                               SUBREGION == "Southern Europe" & SOVEREIGNT == "Macedonia" ~ "Eastern Europe",
                               SUBREGION == "Southern Europe" & SOVEREIGNT == "Montenegro" ~ "Eastern Europe",
                               SUBREGION == "Southern Europe" & SOVEREIGNT == "Kosovo" ~ "Eastern Europe",
                               # Western part of Northern Europe
                               SUBREGION == "Northern Europe" & SOVEREIGNT == "Denmark" ~ "Western Europe",
                               SUBREGION == "Northern Europe" & SOVEREIGNT == "United Kingdom" ~ "Western Europe",
                               SUBREGION == "Northern Europe" & SOVEREIGNT == "Norway" ~ "Western Europe",
                               SUBREGION == "Northern Europe" & SOVEREIGNT == "Ireland" ~ "Western Europe",
                               SUBREGION == "Northern Europe" & SOVEREIGNT == "Sweden" ~ "Western Europe",
                               SUBREGION == "Northern Europe" & SOVEREIGNT == "Finland" ~ "Western Europe",
                               SUBREGION == "Northern Europe" & SOVEREIGNT == "Iceland" ~ "Western Europe",
                               # Eastern part of Northern Europe
                               SUBREGION == "Northern Europe" & SOVEREIGNT == "Latvia" ~ "Eastern Europe",
                               SUBREGION == "Northern Europe" & SOVEREIGNT == "Lithuania" ~ "Eastern Europe",
                               SUBREGION == "Northern Europe" & SOVEREIGNT == "Estonia" ~ "Eastern Europe")) %>% 
  mutate(region_v3 = case_when(cap_SUBREGION == "Eastern Europe" ~ "Eastern Europe",
                               cap_SUBREGION == "Western Europe" ~ "Western Europe",
                               # Western part of Southern Europe
                               cap_SUBREGION == "Southern Europe" & cap_SOVEREIGNT == "Spain" ~ "Western Europe",
                               cap_SUBREGION == "Southern Europe" & cap_SOVEREIGNT == "Portugal" ~ "Western Europe",
                               cap_SUBREGION == "Southern Europe" & cap_SOVEREIGNT == "Italy" ~ "Western Europe",
                               cap_SUBREGION == "Southern Europe" & cap_SOVEREIGNT == "San Marino" ~ "Western Europe",
                               cap_SUBREGION == "Southern Europe" & cap_SOVEREIGNT == "Croatia" ~ "Western Europe",
                               cap_SUBREGION == "Southern Europe" & cap_SOVEREIGNT == "Malta" ~ "Western Europe",
                               cap_SUBREGION == "Southern Europe" & cap_SOVEREIGNT == "Slovenia" ~ "Western Europe",
                               # Eastern part of Southern Europe
                               cap_SUBREGION == "Southern Europe" & cap_SOVEREIGNT == "Greece" ~ "Eastern Europe",
                               cap_SUBREGION == "Southern Europe" & cap_SOVEREIGNT == "Bosnia and Herzegovina" ~ "Eastern Europe",
                               cap_SUBREGION == "Southern Europe" & cap_SOVEREIGNT == "Republic of Serbia" ~ "Eastern Europe",
                               cap_SUBREGION == "Southern Europe" & cap_SOVEREIGNT == "Albania" ~ "Eastern Europe",
                               cap_SUBREGION == "Southern Europe" & cap_SOVEREIGNT == "Macedonia" ~ "Eastern Europe",
                               cap_SUBREGION == "Southern Europe" & cap_SOVEREIGNT == "Montenegro" ~ "Eastern Europe",
                               cap_SUBREGION == "Southern Europe" & cap_SOVEREIGNT == "Kosovo" ~ "Eastern Europe",
                               # Western part of Northern Europe
                               cap_SUBREGION == "Northern Europe" & cap_SOVEREIGNT == "Denmark" ~ "Western Europe",
                               cap_SUBREGION == "Northern Europe" & cap_SOVEREIGNT == "United Kingdom" ~ "Western Europe",
                               cap_SUBREGION == "Northern Europe" & cap_SOVEREIGNT == "Norway" ~ "Western Europe",
                               cap_SUBREGION == "Northern Europe" & cap_SOVEREIGNT == "Ireland" ~ "Western Europe",
                               cap_SUBREGION == "Northern Europe" & cap_SOVEREIGNT == "Sweden" ~ "Western Europe",
                               cap_SUBREGION == "Northern Europe" & cap_SOVEREIGNT == "Finland" ~ "Western Europe",
                               cap_SUBREGION == "Northern Europe" & cap_SOVEREIGNT == "Iceland" ~ "Western Europe",
                               # Eastern part of Northern Europe
                               cap_SUBREGION == "Northern Europe" & cap_SOVEREIGNT == "Latvia" ~ "Eastern Europe",
                               cap_SUBREGION == "Northern Europe" & cap_SOVEREIGNT == "Lithuania" ~ "Eastern Europe",
                               cap_SUBREGION == "Northern Europe" & cap_SOVEREIGNT == "Estonia" ~ "Eastern Europe")) %>% 
  mutate(region_v2 = factor(region_v2),
         region_v3 = factor(region_v3),
         REGION_UN = factor(REGION_UN),
         cap_REGION_UN = factor(cap_REGION_UN))

# ---------------------------------------------------------
# Export data
# ---------------------------------------------------------
# ----- R
save(cities,
     file = paste0(path_data,
                   "cities.Rdata"))

# ----- Stata
haven::write_dta(data = cities,
                 path = paste0(path_data,
                               "cities.dta"),
                 version = 14)

rm(model_data, cent, border_dist, geodata, urban_pop, cities)
