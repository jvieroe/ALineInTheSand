# ---------------------------------------------------------
#           A Line in the Sand
#           ANATEM
#           Jeppe Vieroe
#           This version: 27/08/2021
# ---------------------------------------------------------

# ----- Preliminaries -----
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
               readr)


path_paper <- paste0(getwd(), "/papers_jeppe/8 trade/")
path_paper

path_data <- paste0(getwd(), "/papers_jeppe/8 trade/data/")
path_data

path_graph <- paste0(getwd(), "/papers_jeppe/8 trade/tex/")
path_graph

# define a dictionary which translates variable names to other labels when exporting regressions with etable()
myDict <- c("(Intercept)" = "Constant",
            "delta_uppone" = "$\\Delta$ ln(DUP)",
            "delta_upaone" = "$\\Delta$ ln(TUP)",
            "delta_popone" = "$\\Delta$ ln(Pop.)",
            "delta_cap1" = "$\\Delta$ Capital",
            "delta_ncit1" = "$\\Delta$ No. cities in polity",
            "delta_area1" = "$\\Delta$ ln(area)",
            "westphalian1" = "Post Westphalian Peace",
            "vienna1" = "Post Congress of Vienna",
            "hyw1" = "Post Hundred Years War",
            "city_id" = "City",
            "year" = "Year",
            "century" = "Century",
            "halfcentury" = "Halfcentury",
            "century1000" = "1000",
            "century1100" = "1100",
            "century1200" = "1200",
            "century1300" = "1300",
            "century1400" = "1400",
            "century1500" = "1500",
            "century1600" = "1600",
            "century1700" = "1700",
            "century1800" = "1800",
            "century1900" = "1900",
            "century2000" = "2000",
            ###
            "halfcentury1000" = "1000",
            "halfcentury1050" = "1050",
            "halfcentury1100" = "1100",
            "halfcentury1150" = "1150",
            "halfcentury1200" = "1200",
            "halfcentury1250" = "1250",
            "halfcentury1300" = "1300",
            "halfcentury1350" = "1350",
            "halfcentury1400" = "1400",
            "halfcentury1450" = "1450",
            "halfcentury1500" = "1500",
            "halfcentury1550" = "1550",
            "halfcentury1600" = "1600",
            "halfcentury1650" = "1650",
            "halfcentury1700" = "1700",
            "halfcentury1750" = "1750",
            "halfcentury1800" = "1800",
            "halfcentury1850" = "1850",
            "halfcentury1900" = "1900",
            "halfcentury1950" = "1950",
            "halfcentury2000" = "2000",
            ###
            "REGION_UN^halfcentury" = "Region $\\times$ halfcentury",
            "hyde_rural" = "HYDE Rural",
            "REGION_UN" = "Region",
            "REGION_UNAsia" = "Asia",
            "REGION_UNEurope" = "Europe",
            "REGION_UNAfrica" = "Africa")

# ----- Load + prep. data -----

# Load data set
cities_full <- rio::import(paste0(path_data,
                                  "df_full/",
                                  "cities.Rdata"))

# ----- create semicentennial data structure
cit_050 <- cities_full %>%
  arrange(city_id, year) %>% 
  group_by(city_id) %>% 
  # lag state 100 years back
  mutate(lstate1 = dplyr::lag(state, 1),
         lstate2 = dplyr::lag(state, 2),
         lstate3 = dplyr::lag(state, 3),
         lstate4 = dplyr::lag(state, 4),
         lstate5 = dplyr::lag(state, 5),
         lstate6 = dplyr::lag(state, 6),
         lstate7 = dplyr::lag(state, 7),
         lstate8 = dplyr::lag(state, 8),
         lstate9 = dplyr::lag(state, 9),
         lstate10 = dplyr::lag(state, 10),
         lstate11 = dplyr::lag(state, 11),
         lstate12 = dplyr::lag(state, 12),
         lstate13 = dplyr::lag(state, 13),
         lstate14 = dplyr::lag(state, 14),
         lstate15 = dplyr::lag(state, 15)) %>% 
  # lag polityborder 100km 100 years back
  mutate(l1_p_border100 = dplyr::lag(p_border100, 1),
         l2_p_border100 = dplyr::lag(p_border100, 2),
         l3_p_border100 = dplyr::lag(p_border100, 3),
         l4_p_border100 = dplyr::lag(p_border100, 4),
         l5_p_border100 = dplyr::lag(p_border100, 5)) %>% 
  # lag polityborder 50km 100 years back
  mutate(l1_p_border050 = dplyr::lag(p_border050, 1),
         l2_p_border050 = dplyr::lag(p_border050, 2),
         l3_p_border050 = dplyr::lag(p_border050, 3),
         l4_p_border050 = dplyr::lag(p_border050, 4),
         l5_p_border050 = dplyr::lag(p_border050, 5)) %>% 
  # lag pborder 100km 100 years back
  mutate(l1_border100 = dplyr::lag(border100, 1),
         l2_border100 = dplyr::lag(border100, 2),
         l3_border100 = dplyr::lag(border100, 3),
         l4_border100 = dplyr::lag(border100, 4),
         l5_border100 = dplyr::lag(border100, 5)) %>% 
  # lag border 50km 100 years back
  mutate(l1_border050 = dplyr::lag(border050, 1),
         l2_border050 = dplyr::lag(border050, 2),
         l3_border050 = dplyr::lag(border050, 3),
         l4_border050 = dplyr::lag(border050, 4),
         l5_border050 = dplyr::lag(border050, 5)) %>% 
  ungroup() %>% 
  # create dummies for stable proximity to borders
  mutate(bord100 = case_when(border100 == 1 & l1_border100 == 1 & l2_border100 == 1 & l3_border100 == 1
                             & l4_border100 & l5_border100 == 1 ~ 1,
                             TRUE ~ 0)) %>% 
  mutate(bord050 = case_when(border050 == 1 & l1_border050 == 1 & l2_border050 == 1 & l3_border050 == 1
                             & l4_border050 & l5_border050 == 1 ~ 1,
                             TRUE ~ 0)) %>% 
  mutate(pbord100 = case_when(p_border100 == 1 & l1_p_border100 == 1 & l2_p_border100 == 1 & l3_p_border100 == 1
                             & l4_p_border100 & l5_p_border100 == 1 ~ 1,
                             TRUE ~ 0)) %>% 
  mutate(pbord050 = case_when(p_border050 == 1 & l1_p_border050 == 1 & l2_p_border050 == 1 & l3_p_border050 == 1
                             & l4_p_border050 & l5_p_border050 == 1 ~ 1,
                             TRUE ~ 0)) %>% 
  # create dummy for stable political affiliations
  mutate(state_const1 = case_when(state == lstate1 & state == lstate2 & state == lstate3 &
                                     state == lstate4 & state == lstate5 ~ 1,
                                   TRUE ~ 0),
         state_const2 = case_when(state == lstate1 & state == lstate2 & state == lstate3 &
                                     state == lstate4 & state == lstate5 & state == lstate6 & 
                                      state == lstate7 & state == lstate8 & state == lstate9 & 
                                      state == lstate10 ~ 1,
                                    TRUE ~ 0),
         state_const3 = case_when(state == lstate1 & state == lstate2 & state == lstate3 &
                                    state == lstate4 & state == lstate5 & state == lstate6 & 
                                    state == lstate7 & state == lstate8 & state == lstate9 & 
                                    state == lstate10 & state == lstate11 & state == lstate12 &
                                    state == lstate13 & state == lstate14 & state == lstate15 ~ 1,
                                  TRUE ~ 0)) %>% 
  # clean up
  dplyr::select(-c(lstate1, lstate2, lstate3, lstate4, lstate5, lstate6, lstate7,
                   lstate8, lstate9, lstate10, lstate11, lstate12, lstate13,
                   lstate14, lstate15)) %>% 
  # keep only semicentennial observations
  filter(is_50 == 1) %>% 
  arrange(city_id, year) %>% 
  group_by(city_id) %>% 
  mutate(lyear = dplyr::lag(year, 1)) %>% 
  ungroup() %>% 
  mutate(dyear = year - lyear) %>% 
  filter(dyear == 50) %>% 
  dplyr::select(-c(dyear, lyear)) %>% 
  dplyr::select(-up_polity) %>%
  # use original Domestic UP measure, not limited to number of domestic cities
  mutate(up_polity = up_polityOrig) %>% 
  # log transform variables
  mutate(ln_pop = log(pop),
         ln_upp = log(up_polity),
         ln_upa = log(up_all),
         ln_areakm2 = log(area_km2)) %>% 
  arrange(city_id, year) %>% 
  group_by(city_id) %>%
  # lag key variables
  mutate(l_pop1 = dplyr::lag(ln_pop, 1),
         l_pop2 = dplyr::lag(ln_pop, 2),
         l_pop3 = dplyr::lag(ln_pop, 3),
         l_upa1 = dplyr::lag(ln_upa,1),
         l_upa2 = dplyr::lag(ln_upa,2),
         l_upa3 = dplyr::lag(ln_upa,3),
         l_upp1 = dplyr::lag(ln_upp,1),
         l_upp2 = dplyr::lag(ln_upp,2),
         l_upp3 = dplyr::lag(ln_upp,3)) %>% 
  # lag controls
  mutate(l_cap1 = dplyr::lag(is_cap, 1),
         l_cap2 = dplyr::lag(is_cap, 2),
         l_cap3 = dplyr::lag(is_cap, 3),
         l_citycount1 = dplyr::lag(city_count, 1),
         l_citycount2 = dplyr::lag(city_count, 2),
         l_citycount3 = dplyr::lag(city_count, 3),
         l_area1 = dplyr::lag(ln_areakm2, 1),
         l_area2 = dplyr::lag(ln_areakm2, 2),
         l_area3 = dplyr::lag(ln_areakm2, 3)) %>% 
  mutate(l_muslimtrade1 = dplyr::lag(MuslimTradeDummy, 1),
         l_muslimtrade2 = dplyr::lag(MuslimTradeDummy, 2),
         l_muslimtrade3 = dplyr::lag(MuslimTradeDummy, 3)) %>%
  ungroup() %>% 
  # create delta for key variables
  mutate(delta_popone = ln_pop - l_pop1,
         delta_poptwo = l_pop1 - l_pop2,
         delta_popthree = l_pop2 - l_pop3,
         # UP polity
         delta_uppone = ln_upp - l_upp1,
         delta_upptwo = l_upp1 - l_upp2,
         delta_uppthree = l_upp2 - l_upp3,
         # UP all
         delta_upaone = ln_upa - l_upa1,
         delta_upatwo = l_upa1 - l_upa2,
         delta_upathree = l_upa2 - l_upa3) %>%
  # create delta for control variables
  mutate(delta_cap1 = is_cap - l_cap1,
         delta_cap2 = l_cap1 - l_cap2,
         delta_cap3 = l_cap2 - l_cap3,
         delta_ncit1 = city_count - l_citycount1,
         delta_ncit2 = l_citycount1 - l_citycount2,
         delta_ncit3 = l_citycount2 - l_citycount3,
         delta_area1 = ln_areakm2 - l_area1,
         delta_area2 = l_area1 - l_area2,
         delta_area3 = l_area2 - l_area3) %>% 
  mutate(delta_muslim1 = MuslimTradeDummy - l_muslimtrade1,
         delta_muslim2 = l_muslimtrade1 - l_muslimtrade2,
         delta_muslim3 = l_muslimtrade2 - l_muslimtrade3) %>%
  # clean up
  dplyr::select(c(year,
                  city_id,
                  pop,
                  year,
                  state,
                  # key variables
                  delta_popone,
                  delta_poptwo,
                  delta_popthree,
                  delta_uppone,
                  delta_upptwo,
                  delta_uppthree,
                  delta_upaone,
                  delta_upatwo,
                  delta_upathree,
                  # time-varying polity controls
                  delta_cap1,
                  delta_cap2,
                  delta_cap3,
                  delta_ncit1,
                  delta_ncit2,
                  delta_ncit3,
                  delta_area1,
                  delta_area2,
                  delta_area3,
                  # time-varying trade controls
                  delta_muslim1,
                  delta_muslim2,
                  delta_muslim3,
                  # time-invariant trade controls
                  atlantic,
                  mediter,
                  roman,
                  DistRiver,
                  # time-invariant geographic controls
                  suitRainfed,
                  suitIrrigated,
                  gridID,
                  ruggedness,
                  # time-varying border controls
                  border100,
                  border050,
                  p_border100,
                  p_border050,
                  bord100,
                  bord050,
                  pbord100,
                  pbord050,
                  # state consistency dummies
                  state_const1,
                  state_const2,
                  state_const3,
                  halfcentury,
                  century,
                  #
                  l_pop1,
                  l_pop2,
                  l_pop3,
                  ln_pop,
                  ln_upa,
                  ln_upp,
                  pop,
                  up_all,
                  up_polity,
                  REGION_UN, SUBREGION, SOVEREIGNT, REGION_WB, CONTINENT,
                  cap_REGION_UN, cap_SUBREGION, cap_SOVEREIGNT, cap_REGION_WB, cap_CONTINENT,
                  region_v2, region_v3)) %>% 
  # in the past there were issues with inifinite values -- filter out just in case
  filter(!is.infinite(delta_uppone) & !is.infinite(delta_upptwo) & !is.infinite(delta_uppthree))

# Save state consistency data with dummies for stable political affiliations (needed for appendix robustness)
temp <- cit_050 %>% 
  dplyr::select(city_id,
                year,
                state_const2)


# ----- Plot: the approach of DUP/TUP + various maps -----
# ----- Eurasia data
eurasia_union <- st_read(dsn = "5_centennia/eurasia_map_union",
                         layer = "eurasia_map_union",
                         crs = 4326)

# ----- Centennia data
centennia <- st_read(dsn = "5_centennia/centennia_full",
                     layer = "centennia_full",
                     crs = 4326) %>% 
  mutate(area_km2 = unclass(st_area(.))/1000000)

# ----- plot cities
# convert to spatial
cit_sf <- cities_full %>% 
  dplyr::select(c(city, country, state, latitude, longitude)) %>% 
  distinct(., city_id, .keep_all = T) %>% 
  st_as_sf(.,
           coords = c("longitude", "latitude"),
           crs = 4326)

ggplot() +
  geom_sf(data = eurasia_union, fill = "grey95", alpha = 0.75, size = 0.25) +
  geom_sf(data = cit_sf, size = .1) +
  theme_list$theme_anatem_map +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

ggsave(plot = last_plot(),
       file = paste0(path_graph,
                     "/city_map.png"))
knitr::plot_crop(paste0(path_graph,
                        "/city_map.png"),
                 quiet = T)

# ----- Plot Logic of TUP/DUP
# define subsample
year_val <- 1350
states <- c("FRANCE", "ARAGONESE", "CASTILE", "NAVARRE", "BRITAIN")

# filter city in time and space
cit_sf <- cities_full %>% 
  filter(year == year_val) %>% 
  filter(state %in% states) %>% 
  dplyr::select(c(city, country, state, latitude, longitude)) %>% 
  st_as_sf(.,
           coords = c("longitude", "latitude"),
           crs = 4326)

# filter centennia in time and space
cent <- centennia %>% 
  filter(year == year_val) %>% 
  filter(state %in% states)

# code Aragonese dummies
cent <- cent %>% 
  mutate(is_aragonese = ifelse(state == "ARAGONESE", "1", 0))

cit_sf <- cit_sf %>% 
  mutate(is_aragonese = ifelse(state == "ARAGONESE", "1", 0))

# plot it
ggplot() +
  geom_sf(data = eurasia_union, fill = "grey98", size = 0.10) +
  geom_sf(data = cent, fill = "grey90", size = 0.35,
          alpha = 0.75) +
  geom_sf(data = cit_sf, aes(shape = is_aragonese,
                             fill = is_aragonese),
          color = "black",
          size = 2.0) +
  coord_sf(xlim = c(-10, 5),
           ylim = c(40, 47)) +
  scale_color_manual(values = c("white", "black")) +
  scale_fill_manual(values = c("white", "black")) +
  scale_shape_manual(values = c(24, 21)) +
  theme_list$theme_anatem +
  theme(legend.position = "none") +
  theme(panel.grid = element_blank())

ggsave(plot = last_plot(),
       file = paste0(path_graph,
                     "cent_coding2.png"),
       height = 6,
       width = 9)
knitr::plot_crop(paste0(path_graph,
                        "cent_coding2.png"),
                 quiet = T)


# ----- plot logic of coding using centennia
ggplot() +
  geom_sf(data = eurasia_union, fill = "grey98", size = 0.10) +
  geom_sf(data = cent, aes(fill = state), size = 0.35,
          alpha = 0.75) +
  geom_sf(data = cit_sf, aes(shape = state),
          color = "black",
          size = 3.0) +
  coord_sf(xlim = c(-10, 5),
           ylim = c(40, 47)) +
  scale_color_manual(values = c("black", "grey60", "black", "grey60", "grey")) +
  scale_shape_manual(values = c(1, 2, 4, 7, 10)) +
  scale_fill_manual(values = c("grey30", "grey40", "grey60", "grey80", "grey90")) +
  theme_list$theme_anatem +
  guides(shape = guide_legend(nrow = 3,
                              ncol = 2,
                              byrow = TRUE),
         fill = guide_legend(nrow = 3,
                             ncol = 2,
                             byrow = TRUE)) +
  theme(panel.grid = element_blank())

ggsave(plot = last_plot(),
       file = paste0(path_graph,
                     "/cent_coding.png"),
       height = 6,
       width = 9)
knitr::plot_crop(paste0(path_graph,
                        "/cent_coding.png"),
                 quiet = T)


# ----- plot centennia AD 1600
# filter to 1600
cent <- centennia %>% 
  filter(year == 1600)

# plot it
ggplot() +
  geom_sf(data = eurasia_union, fill = "white", size = 0.25) +
  geom_sf(data = cent, color = "black", fill = "grey95", alpha = 0.75, size = 0.10) +
  # scale_fill_viridis_c(option = "viridis",
  #                      direction = -1,
  #                      na.value = "grey50") +
  # scale_fill_gradient(low = "white",
  #                     high = "steelblue") +
  # scale_fill_gradient(low="#56B1F7", high="#132B43") +
  theme_list$theme_anatem_map +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank())

ggsave(plot = last_plot(),
       file = paste0(path_graph,
                     "/cent_map.png"))
knitr::plot_crop(paste0(path_graph,
                        "/cent_map.png"),
                 quiet = T)


# ----- plot development in N cities
temp <- cities_full

# count cities
map_df1 <- temp %>% 
  group_by(year) %>% 
  dplyr::summarize(count_cit1 = n()) %>% 
  mutate(type = "No. cities")

# count cities not interpolated
map_df2 <- temp %>% 
  filter(!is.na(source)) %>% 
  group_by(year) %>% 
  dplyr::summarize(count_cit2 = n()) %>% 
  mutate(type = "No. actual observations")

# count cities with (interpolated) pop. data
map_df3 <- temp %>% 
  filter(!is.na(pop)) %>% 
  group_by(year) %>% 
  dplyr::summarize(count_cit3= n()) %>% 
  mutate(type = "No. cities with pop. data")

# rename variables
map_df1 <- map_df1 %>% 
  dplyr::rename(count_cit = count_cit1)
map_df2 <- map_df2 %>% 
  dplyr::rename(count_cit = count_cit2)
map_df3 <- map_df3 %>% 
  dplyr::rename(count_cit = count_cit3)

# combine data
map_df <- rbind(map_df2, map_df3)

# plot it
ggplot(map_df) +
  geom_line(aes(x = year, y = count_cit, col = type, linetype = type)) +
  scale_color_manual(values = c("grey60", "black", "black")) +
  scale_linetype_manual(values = c("solid", "solid", "dashed")) +
  scale_x_continuous(breaks = seq(1000, 2000, 100),
                     labels = seq(1000, 2000, 100),
                     limits = c(1000, 2000),
                     expand = c(0.02, 0.02)) +
  scale_y_continuous(breaks = seq(0, 2500, 500),
                     labels = seq(0, 2500, 500),
                     limits = c(0, 2700),
                     expand = c(0.02, 0.02)) +
  theme_list$theme_anatem +
  theme(axis.title.y = element_blank()) +
  guides(color = guide_legend(nrow = 3,
                              ncol = 1,
                              byrow = TRUE),
         fill = "none")

ggsave(plot = last_plot(),
       file = paste0(path_graph,
                     "/city_count.png"))
knitr::plot_crop(paste0(path_graph,
                        "/city_count.png"),
                 quiet = T)


# ----- Plot area and number of states
# calculate mean area over time
map_df <- cities_full %>% 
  mutate(logarea = log(area_km2)) %>%
  group_by(year) %>% 
  dplyr::summarize(mean_size = mean(logarea, na.rm = T),
            median_size = median(logarea, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(type = "Average area (ln)")

# plot it
ggplot(map_df) +
  geom_line(aes(x = year, y = mean_size), color = "black") +
  scale_x_continuous(breaks = seq(1000, 2000, 100),
                     labels = seq(1000, 2000, 100),
                     limits = c(1000, 2000),
                     expand = c(0.02, 0.02)) +
  scale_y_continuous(breaks = seq(12, 14, 0.25),
                     labels = scaleFUN,
                     limits = c(12.1, 13.85)) +
  theme_list$theme_anatem +
  theme(axis.title.y = element_blank())

ggsave(plot = last_plot(),
       file = paste0(path_graph,
                     "/plot_area.png"))
knitr::plot_crop(paste0(path_graph,
                        "/plot_area.png"),
                 quiet = T)

# calculate no. states over time
map_df <- cities_full %>% 
  mutate(temp = ifelse(source_pol_control == "CENTENNIA",
                       state,
                       NA)) %>% 
  group_by(year) %>% 
  distinct(., state) %>% 
  dplyr::summarize(metric = n()) %>% 
  mutate(type = "No. states")

# plot it
ggplot(map_df) +
  geom_line(aes(x = year, y = metric)) +
  scale_x_continuous(breaks = seq(1000, 2000, 100),
                     labels = seq(1000, 2000, 100),
                     limits = c(1000, 2000),
                     expand = c(0.02, 0.02)) +
  scale_y_continuous(breaks = seq(30, 80, 10),
                     labels = seq(30, 80, 10)) +
  theme_list$theme_anatem +
  theme(axis.title.y = element_blank())

ggsave(plot = last_plot(),
       file = paste0(path_graph,
                     "/plot_nstates.png"))
knitr::plot_crop(paste0(path_graph,
                        "/plot_nstates.png"),
                 quiet = T)


rm(map_df, map_df1, map_df2, map_df3, temp)
rm(cit_sf, cent, eurasia_union, year_val, states, centennia)

# ----- Plot pop vs. TUP/DUP -----
# Figure 2.8 (I)
ggplot(cit_050, aes(x = ln_pop, y = ln_upa)) +
  geom_point(shape = 21, fill = NA, color = "black", alpha = 0.25) +
  geom_smooth(method = "loess", aes(colour="Loess", fill="Loess")) +
  geom_smooth(method = "lm", aes(colour="LM", fill = "LM")) +
  scale_fill_manual(values = c("grey75", "grey75"), name = "Model")  +
  scale_colour_manual(values = c("black", "grey55"), name = "Model") +
  labs(x = "ln(pop.)",
       y = "ln(Total Urban Potential [TUP])") +
  theme_list$theme_anatem

ggsave(plot = last_plot(),
       file = paste0(path_graph,
                     "scatter_TUP.png"),
       width = 9,
       height = 6)
knitr::plot_crop(paste0(path_graph,
                        "scatter_TUP.png"),
                 quiet = T)

# Figure 2.8 (II)
ggplot(cit_050, aes(x = ln_pop, y = ln_upp)) +
  geom_point(shape = 21, fill = NA, color = "black", alpha = 0.25) +
  geom_smooth(method = "loess", aes(colour="Loess", fill="Loess")) +
  geom_smooth(method = "lm", aes(colour="LM", fill = "LM")) +
  scale_fill_manual(values = c("grey75", "grey75"), name = "Model")  +
  scale_colour_manual(values = c("black", "grey55"), name = "Model") +
  labs(x = "ln(pop.)",
       y = "ln(Domestic Urban Potential [DUP])") +
  theme_list$theme_anatem

ggsave(plot = last_plot(),
       file = paste0(path_graph,
                     "scatter_DUP.png"),
       width = 9,
       height = 6)
knitr::plot_crop(paste0(path_graph,
                        "scatter_DUP.png"),
                 quiet = T)

# Figure 2.7
ggplot(cit_050) +
  geom_density(aes(ln_upp), fill = "grey55", color = NA, alpha = 0.5) +
  geom_density(aes(ln_upa), fill = "grey75", color = NA, alpha = 0.5) +
  geom_density(aes(ln_pop), fill = "black", color = NA, alpha = 0.5) +
  annotate("text", x = 3.5, y = 0.1, label = "Domestic Urban Potential") +
  annotate("text", x = 13, y = 0.3, label = "Total Urban Potential") +
  annotate("text", x = 6, y = 0.4, label = "Urban Population") +
  geom_curve(aes(x = 5.25, 
                 xend = 3.5, 
                 y = 0.03, 
                 yend = 0.09),
             curvature = -0.25,
             angle = 90,
             size = .1) +
  geom_curve(aes(x = 8.0, 
                 xend = 6.5, 
                 y = 0.325, 
                 yend = 0.39),
             curvature = -0.35,
             angle = 90,
             size = .1) +
  geom_curve(aes(x = 10.0, 
                 xend = 12.0, 
                 y = 0.39, 
                 yend = 0.315),
             curvature = -0.15,
             angle = 90,
             size = .1) +
  theme_list$theme_anatem +
  theme(axis.title.x = element_blank())

ggsave(plot = last_plot(),
       file = paste0(path_graph,
                     "combined_density.png"),
       width = 9,
       height = 6)
knitr::plot_crop(paste0(path_graph,
                        "combined_density.png"),
                 quiet = T)

mean(cit_050$ln_pop, na.rm = T)

# ---------------------------------------------------------
# Estimations: tables and figures
# ---------------------------------------------------------
# ----- Table 2.3 -----
mlist <- list()
mlist$m1 <- feols(delta_popone
                  ~ delta_uppone
                  + delta_upaone,
                  data = subset(cit_050, state_const1 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m2 <- feols(delta_popone
                  ~ delta_uppone
                  + delta_upaone
                  | REGION_UN^halfcentury,
                  data = subset(cit_050, state_const1 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m3 <- feols(delta_popone
                  ~ delta_uppone
                  + delta_upaone
                  + delta_cap1
                  | REGION_UN^halfcentury, 
                  data = subset(cit_050, state_const1 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m4 <- feols(delta_popone
                  ~ delta_uppone
                  + delta_upaone
                  + delta_cap1
                  + delta_ncit1
                  | REGION_UN^halfcentury, 
                  data = subset(cit_050, state_const1 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m5 <- feols(delta_popone
                  ~ delta_uppone
                  + delta_upaone
                  + delta_cap1
                  + delta_ncit1 + delta_area1
                  | REGION_UN^halfcentury, 
                  data = subset(cit_050, state_const1 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m6 <- feols(delta_popone
                  ~ delta_uppone
                  + delta_upaone
                  + delta_cap1
                  + delta_ncit1*delta_area1
                  | REGION_UN^halfcentury, 
                  data = subset(cit_050, state_const1 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))

# check results
etable(mlist,
       keep = c("upp", "upa"))


# export table
etable(mlist,
       file = paste0(path_graph,
                     "tab_raw.tex"),
       replace = T,
       sdBelow = T,
       # extraline = list("{title: \\midrule Model details} Time invariant geographic variables $\\times$ halfcentury" =
       #                    c("","", "", "", "$\\checkmark$", "$\\checkmark$"),
       #                  "$\\Delta$ Time variant geographic variables" =
       #                    c("", "", "", "", "", "$\\checkmark$")),
       tex = T,
       dict = myDict,
       signifCode = c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       digits = "r3",
       drop = c("atlantic", "mediter", "roman", "DistRiver",
                "suitRainfed", "suitIrrigated", "ruggedness", "delta_muslim1"),
       fitstat = c("n", "ar2"),
       order = c("!Constant", "!area", "!No. cities", "!Capital"),
       powerBelow = -10,
       depvar = T,
       style.tex = style.tex("aer",
                             tabular = "*",
                             fixef.where = "var",
                             fixef.suffix = " fixed effects",
                             fixef.title = "\\midrule",
                             var.title = "\\midrule",
                             stats.title = "\\midrule",
                             yesNo = "$\\checkmark$",
                             tablefoot = F))


# ----- Figure 2.9 -----
# run models
mlist <- list()
# excl controls
mlist$m1 <- feols(delta_popone
                  ~ delta_uppone:halfcentury
                  + delta_upaone:halfcentury
                  + halfcentury
                  | REGION_UN^halfcentury, data = subset(cit_050, state_const1 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
# incl controls
mlist$m2 <- feols(delta_popone
                  ~ delta_uppone:halfcentury
                  + delta_upaone:halfcentury
                  + delta_cap1
                  + delta_ncit1*delta_area1
                  + halfcentury
                  | REGION_UN^halfcentury, data = subset(cit_050, state_const1 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))

# export coefficients
ct1 <- coeftable(mlist$m1) %>% 
  tibble::rownames_to_column(., "predictor") %>% 
  filter(grepl(("delta_upp"), predictor)) %>% 
  dplyr::rename(estimate = Estimate,
                std_error = 'Std. Error') %>%
  dplyr::select(c(predictor, estimate, std_error)) %>% 
  dplyr::mutate(conf_low = estimate - (1.96 * std_error),
                conf_high = estimate + (1.96 * std_error)) %>% 
  dplyr::mutate(century = readr::parse_number(predictor)) %>% 
  arrange(century) %>% 
  mutate(type = "Excl. controls")

ct2 <- coeftable(mlist$m2) %>% 
  tibble::rownames_to_column(., "predictor") %>% 
  filter(grepl(("delta_upp"), predictor)) %>% 
  dplyr::rename(estimate = Estimate,
                std_error = 'Std. Error') %>%
  dplyr::select(c(predictor, estimate, std_error)) %>% 
  dplyr::mutate(conf_low = estimate - (1.96 * std_error),
                conf_high = estimate + (1.96 * std_error)) %>% 
  dplyr::mutate(century = readr::parse_number(predictor)) %>% 
  arrange(century) %>% 
  mutate(type = "Incl. controls")

# combine coefficients in one dataset
ct <- rbind(ct1, ct2)

# plot coefficients
ggplot(ct, aes(x = century, y = estimate, ymin = conf_low, ymax = conf_high)) +
  geom_ribbon(fill = "grey65", alpha = .3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_line(col = "black", size = .5) +
  scale_x_continuous(breaks = seq(1000, 2000, 100),
                     labels = seq(1000, 2000, 100)) +
  theme_list$theme_anatem +
  facet_wrap(~ type, ncol = 1)

ggsave(plot = last_plot(),
       file = paste0(path_graph,
                     "wave1facet.png"),
       width = 9,
       height = 9)
knitr::plot_crop(paste0(path_graph,
                        "wave1facet.png"),
                 quiet = T)

# ----- Figure 2.10 -----
# run models
mlist <- list()
mlist$m1 <- feols(delta_popone
                  ~ delta_uppone:halfcentury
                  + delta_upaone:halfcentury
                  + delta_cap1
                  + delta_ncit1*delta_area1
                  + halfcentury
                  | REGION_UN^halfcentury, data = subset(cit_050, state_const1 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m2 <- feols(delta_popone
                  ~ delta_uppone:halfcentury
                  + delta_upaone:halfcentury
                  + delta_upptwo:halfcentury
                  + delta_upatwo:halfcentury
                  + delta_cap1
                  + delta_ncit1*delta_area1
                  + halfcentury
                  | REGION_UN^halfcentury, data = subset(cit_050, state_const2 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m3 <- feols(delta_popone
                  ~ delta_uppone:halfcentury
                  + delta_upaone:halfcentury
                  + delta_upptwo:halfcentury
                  + delta_upatwo:halfcentury
                  + delta_uppthree:halfcentury
                  + delta_upathree:halfcentury
                  + delta_cap1
                  + delta_ncit1*delta_area1
                  + halfcentury
                  | REGION_UN^halfcentury, data = subset(cit_050, state_const3 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))

# extract coefficients
ct <- coeftable(mlist$m2) %>% 
  tibble::rownames_to_column(., "predictor") %>% 
  filter(grepl(("delta_upp"), predictor)) %>% 
  dplyr::rename(estimate = Estimate,
                std_error = 'Std. Error') %>%
  dplyr::select(c(predictor, estimate, std_error)) %>% 
  dplyr::mutate(conf_low = estimate - (1.96 * std_error),
                conf_high = estimate + (1.96 * std_error)) %>% 
  dplyr::mutate(century = readr::parse_number(predictor)) %>% 
  dplyr::mutate(polity1 = str_detect(predictor, "delta_uppone", negate = FALSE),
                polity2 = str_detect(predictor, "delta_upptwo", negate = FALSE),
                polity3 = str_detect(predictor, "delta_uppthree", negate = FALSE)) %>% 
  dplyr::mutate(seqs = case_when(polity1 == T ~ "Contemporary effect",
                                 polity2 == T ~ "Lagged effect (one half-century)",
                                 polity3 == T ~ "Lagged effect (two half-centuries)")) %>% 
  dplyr::mutate(seqs = factor(seqs,
                              levels = c("Contemporary effect", "Lagged effect (one half-century)", "Lagged effect (two half-centuries)"))) %>% 
  dplyr::select(-c(polity1, polity2, polity3)) %>% 
  arrange(seqs, century)

# plot it
ggplot(ct, aes(x = century, y = estimate, ymin = conf_low, ymax = conf_high)) +
  geom_ribbon(fill = "grey65", alpha = .3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_line(col = "black", size = .5) +
  scale_x_continuous(breaks = seq(1000, 2000, 100),
                     labels = seq(1000, 2000, 100)) +
  facet_wrap(~ seqs, ncol = 1) +
  theme_list$theme_anatem

ggsave(plot = last_plot(),
       file = paste0(path_graph,
                     "wave2multivariate.png"),
       width = 9,
       height = 9)
knitr::plot_crop(paste0(path_graph,
                        "wave2multivariate.png"),
                 quiet = T)



# ----- Figure 2.11 -----
mlist <- list()
mlist$m1 <- feols(delta_popone
                  ~ delta_uppone:halfcentury
                  + delta_upaone:halfcentury
                  + delta_cap1
                  + delta_ncit1*delta_area1
                  + halfcentury
                  | REGION_UN^century, data = subset(cit_050, state_const1 == 1 & bord100 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m2 <- feols(delta_popone
                  ~ delta_uppone:halfcentury
                  + delta_upaone:halfcentury
                  + delta_cap1
                  + delta_ncit1*delta_area1
                  + halfcentury
                  | REGION_UN^century, data = subset(cit_050, state_const1 == 1 & bord050 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m3 <- feols(delta_popone
                  ~ delta_uppone:halfcentury
                  + delta_upaone:halfcentury
                  + delta_cap1
                  + delta_ncit1*delta_area1
                  + halfcentury
                  | REGION_UN^century, data = subset(cit_050, state_const1 == 1 & pbord100 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m4 <- feols(delta_popone
                  ~ delta_uppone:halfcentury
                  + delta_upaone:halfcentury
                  + delta_cap1
                  + delta_ncit1*delta_area1
                  + halfcentury
                  | REGION_UN^century, data = subset(cit_050, state_const1 == 1 & pbord050 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))

# inspect results
etable(mlist)

# create empty list
fit_list <- list()

# create vector for the loop
values <- seq(1, 4, 1)

# loop to extract coefficients
for (i in seq_along(values)) {
  
  model <- mlist[[values[i]]]
  
  model_no <- values[i]
  
  ct <- coeftable(model) %>% 
    tibble::rownames_to_column(., "predictor") %>% 
    filter(grepl(("delta_uppone"), predictor)) %>% 
    dplyr::rename(estimate = Estimate,
                  std_error = 'Std. Error') %>%
    dplyr::select(c(predictor, estimate, std_error)) %>% 
    dplyr::mutate(conf_low = estimate - (1.96 * std_error),
                  conf_high = estimate + (1.96 * std_error)) %>% 
    dplyr::mutate(century = readr::parse_number(predictor)) %>% 
    dplyr::mutate(m_no = model_no) %>% 
    arrange(century)
  
  fit_list[[i]] <- ct
  
}

# extract data from list that the loop feeds into
fit_data <- bind_rows(fit_list) %>% 
  mutate(type = case_when(m_no %in% c(1) ~ "Distance to border < 100km",
                          m_no %in% c(2) ~ "Distance to border < 50km",
                          m_no %in% c(3) ~ "Distance to polity border < 100km",
                          m_no %in% c(4) ~ "Distance to polity border < 50km"))

# plot it
ggplot(fit_data, aes(x = century, y = estimate, ymin = conf_low, ymax = conf_high)) +
  geom_ribbon(fill = "grey65", alpha = .3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_line(col = "black", size = .5) +
  scale_x_continuous(breaks = seq(1000, 2000, 100),
                     labels = seq(1000, 2000, 100)) +
  scale_y_continuous(breaks = seq(-0.2, 1.0, 0.2),
                     labels = scaleFUN) +
  theme_list$theme_anatem +
  facet_wrap(~ type)

ggsave(plot = last_plot(),
       file = paste0(path_graph,
                     "BorderFacet.png"))
knitr::plot_crop(paste0(path_graph,
                        "BorderFacet.png"),
                 quiet = T)



# ----- Figure 2.12 -----
# z standardize both UP measures
cit_050 <- cit_050 %>% 
  mutate(delta_uppZ = scale(delta_uppone, center = T, scale = T),
         delta_upaZ = scale(delta_upaone, center = T, scale = T))

mlist <- list()
# With both
mlist$m1 <- feols(delta_popone
                  ~ delta_uppZ:halfcentury
                  + delta_upaZ:halfcentury
                  + halfcentury
                  | REGION_UN^halfcentury, data = subset(cit_050, state_const1 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m2 <- feols(delta_popone
                  ~ delta_uppZ:halfcentury
                  + delta_upaZ:halfcentury
                  + delta_cap1
                  + delta_ncit1*delta_area1
                  + atlantic:halfcentury
                  + mediter:halfcentury
                  + roman:halfcentury
                  + DistRiver:halfcentury
                  + suitRainfed:halfcentury
                  + suitIrrigated:halfcentury
                  + ruggedness:halfcentury
                  + halfcentury
                  + delta_muslim1
                  | REGION_UN^century, data = subset(cit_050, state_const1 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))

ct1 <- coeftable(mlist$m2) %>% 
  tibble::rownames_to_column(., "predictor") %>% 
  filter(grepl(("delta_uppZ"), predictor)) %>% 
  dplyr::rename(estimate = Estimate,
                std_error = 'Std. Error') %>%
  dplyr::select(c(predictor, estimate, std_error)) %>% 
  dplyr::mutate(conf_low = estimate - (1.96 * std_error),
                conf_high = estimate + (1.96 * std_error)) %>% 
  dplyr::mutate(century = readr::parse_number(predictor)) %>% 
  arrange(century) %>% 
  mutate(type = "Domestic Urban Potential")

ct2 <- coeftable(mlist$m2) %>% 
  tibble::rownames_to_column(., "predictor") %>% 
  filter(grepl(("delta_upaZ"), predictor)) %>% 
  dplyr::rename(estimate = Estimate,
                std_error = 'Std. Error') %>%
  dplyr::select(c(predictor, estimate, std_error)) %>% 
  dplyr::mutate(conf_low = estimate - (1.96 * std_error),
                conf_high = estimate + (1.96 * std_error)) %>% 
  dplyr::mutate(century = readr::parse_number(predictor)) %>% 
  arrange(century) %>% 
  mutate(type = "Total Urban Potential")
  
ct <- rbind(ct1, ct2)

ggplot(ct, aes(x = century, y = estimate, ymin = conf_low, ymax = conf_high)) +
  #geom_ribbon(aes(group = type), fill = "grey65", alpha = .3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(group = type, col = type)) +
  geom_line(aes(group = type, col = type), size = .5) +
  # geom_smooth(aes(group = type, col = type),
  #             method = "loess") +
  scale_color_manual(values = c("black", "grey75")) +
  scale_x_continuous(breaks = seq(1000, 2000, 100),
                     labels = seq(1000, 2000, 100)) +
  scale_y_continuous(limits = c(-0.28, 0.75)) +
  theme_list$theme_anatem

ggsave(plot = last_plot(),
       file = paste0(path_graph,
                     "wave1_UPA_UPP.png"))
knitr::plot_crop(paste0(path_graph,
                        "wave1_UPA_UPP.png"),
                 quiet = T)

ggplot(ct, aes(x = century, y = estimate, ymin = conf_low, ymax = conf_high)) +
  #geom_ribbon(aes(group = type), fill = "grey65", alpha = .3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  # geom_point(aes(group = type, col = type)) +
  # geom_line(aes(group = type, col = type), size = .5) +
  geom_smooth(aes(group = type, col = type),
              method = "loess", se = T) +
  scale_color_manual(values = c("black", "grey55")) +
  scale_x_continuous(breaks = seq(1000, 2000, 100),
                     labels = seq(1000, 2000, 100)) +
  #scale_y_continuous(limits = c(-0.28, 0.75)) +
  theme_list$theme_anatem

ggsave(plot = last_plot(),
       file = paste0(path_graph,
                     "wave1_UPA_UPP_loess.png"))
knitr::plot_crop(paste0(path_graph,
                        "wave1_UPA_UPP_loess.png"),
                 quiet = T)


# ----- Figure 2.13 -----
# Create regional variable
cit_050 <- cit_050 %>% 
  mutate(region_v1 = factor(case_when(REGION_UN == "Africa" ~ "MENA",
                                      REGION_UN == "Asia" ~ "MENA",
                                      REGION_UN == "Europe" ~ "Europe")))


# run model with threeway interaction
mlist <- list()
mlist$m1 <- feols(delta_popone
                  ~ delta_uppone:halfcentury:region_v1
                  + delta_upaone:halfcentury:region_v1
                  + delta_cap1
                  + delta_ncit1*delta_area1
                  | halfcentury + region_v1 + halfcentury^region_v1 + REGION_UN^halfcentury, data = subset(cit_050, state_const1 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))

# extract coefficients
ct <- coeftable(mlist$m1) %>% 
  tibble::rownames_to_column(., "predictor") %>% 
  filter(grepl(("delta_upp"), predictor)) %>% 
  dplyr::rename(estimate = Estimate,
                std_error = 'Std. Error') %>%
  dplyr::select(c(predictor, estimate, std_error)) %>% 
  dplyr::mutate(conf_low = estimate - (1.96 * std_error),
                conf_high = estimate + (1.96 * std_error)) %>% 
  dplyr::mutate(century = readr::parse_number(predictor)) %>% 
  dplyr::mutate(Europe = str_detect(predictor, "Europe", negate = FALSE)) %>% 
  dplyr::mutate(region = ifelse(Europe == T, "Europe", "MENA")) %>% 
  arrange(region, century)

# plot it
ggplot(ct, aes(x = century, y = estimate, ymin = conf_low, ymax = conf_high)) +
  geom_ribbon(fill = "grey65", alpha = .3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_line(col = "black", size = .5) +
  scale_x_continuous(breaks = seq(1000, 2000, 100),
                     labels = seq(1000, 2000, 100)) +
  theme_list$theme_anatem +
  facet_wrap(~ region, ncol = 1)

ggsave(plot = last_plot(),
       file = paste0(path_graph,
                     "EuropeMENA.png"),
       width = 9,
       height = 9)
knitr::plot_crop(paste0(path_graph,
                        "EuropeMENA.png"),
                 quiet = T)



