# ----- Table A.1 -----
names(cit_050)
class(cit_050$state_const1)

# create data with numerical variables only
df <- cit_050 %>% 
  mutate(halfcentury = as.numeric(halfcentury)) %>% 
  dplyr::select(c(city_id,
                  year,
                  pop,
                  delta_popone,
                  delta_poptwo,
                  delta_popthree,
                  delta_uppone,
                  delta_upptwo,
                  delta_uppthree,
                  delta_upaone,
                  delta_upatwo,
                  delta_upathree,
                  delta_cap1,
                  delta_cap2,
                  delta_cap3,
                  delta_ncit1,
                  delta_ncit2,
                  delta_ncit3,
                  delta_area1,
                  delta_area2,
                  delta_area3,
                  halfcentury,
                  state_const1,
                  state_const2,
                  state_const3,
                  border100,
                  border050,
                  p_border100,
                  p_border050,
                  l_pop1,
                  l_pop2,
                  l_pop3,
                  up_all,
                  up_polity))

# create summary stats
library(psych)
df2 <- print(psych::describe(df),
             digits = 3)

# export summary stats
kable(df2, "latex", booktabs = TRUE)
# --- insert these into latex table

rm(df, df2)


# ----- Load + prep. balanced dataset -----
cities_bala <- rio::import(paste0(path_data,
                                  "df_balanced/",
                                  "cities.Rdata"))


# create semicentennial data structure
cit_050b <- cities_bala %>%
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
  filter(!is.infinite(delta_uppone) & !is.infinite(delta_upptwo) & !is.infinite(delta_uppthree))


# ----- Load + prep. non-interpolated dataset -----
cities_pure <- rio::import(paste0(path_data,
                                  "df_pure/",
                                  "cities.Rdata"))

# create centennial data structure
cit_100 <- cities_pure %>% 
  group_by(city_id) %>% 
  mutate(lyear = dplyr::lag(year, 1)) %>% 
  ungroup() %>% 
  mutate(dyear = year - lyear) %>% 
  filter(dyear == 100) %>%  
  dplyr::select(-c(dyear, lyear)) %>% 
  dplyr::select(-up_polity) %>% 
  mutate(up_polity = up_polityOrig) %>% 
  mutate(ln_pop = log(pop),
         ln_upp = log(up_polity),
         ln_upa = log(up_all),
         ln_areakm2 = log(area_km2)) %>% 
  arrange(city_id, year) %>% 
  group_by(city_id) %>%
  # lag key variables
  mutate(l_pop1 = dplyr::lag(ln_pop, 1),
         l_upa1 = dplyr::lag(ln_upa,1),
         l_upp1 = dplyr::lag(ln_upp,1)) %>% 
  # lag controls
  mutate(l_cap1 = dplyr::lag(is_cap, 1),
         l_citycount1 = dplyr::lag(city_count, 1),
         l_area1 = dplyr::lag(ln_areakm2, 1)) %>% 
  ungroup() %>% 
  # create delta for key variables
  mutate(delta_popone = ln_pop - l_pop1,
         # UP polity
         delta_uppone = ln_upp - l_upp1,
         # UP all
         delta_upaone = ln_upa - l_upa1) %>%
  # create delta for control variables
  mutate(delta_cap1 = is_cap - l_cap1,
         delta_ncit1 = city_count - l_citycount1,
         delta_area1 = ln_areakm2 - l_area1) %>% 
  dplyr::select(c(year,
                  city_id,
                  pop,
                  year,
                  state,
                  # key variables
                  delta_popone,
                  delta_uppone,
                  delta_upaone,
                  # time-varying polity controls
                  delta_cap1,
                  delta_ncit1,
                  delta_area1,
                  l_pop1,
                  REGION_UN, SUBREGION, SOVEREIGNT, REGION_WB, CONTINENT,
                  region_v2)) %>% 
  filter(!is.infinite(delta_uppone))

# add state consistency data
cit_100 <- cit_100 %>% 
  tidylog::left_join(.,
                     temp,
                     by = c("city_id", "year"))

cit_100 <- cit_100 %>% 
  mutate(century = factor(year))



# ----- Table A.2 -----
mlist <- list()
mlist$m1 <- feols(delta_popone
                  ~ delta_uppone
                  + delta_upaone,
                  data = cit_050,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m2 <- feols(delta_popone
                  ~ delta_uppone
                  + delta_upaone
                  | REGION_UN^halfcentury,
                  data = cit_050,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m3 <- feols(delta_popone
                  ~ delta_uppone
                  + delta_upaone
                  + delta_cap1
                  | REGION_UN^halfcentury, 
                  data = cit_050,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m4 <- feols(delta_popone
                  ~ delta_uppone
                  + delta_upaone
                  + delta_cap1
                  + delta_ncit1
                  | REGION_UN^halfcentury, 
                  data = cit_050,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m5 <- feols(delta_popone
                  ~ delta_uppone
                  + delta_upaone
                  + delta_cap1
                  + delta_ncit1 + delta_area1
                  | REGION_UN^halfcentury, 
                  data = cit_050,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m6 <- feols(delta_popone
                  ~ delta_uppone
                  + delta_upaone
                  + delta_cap1
                  + delta_ncit1*delta_area1
                  | REGION_UN^halfcentury, 
                  data = cit_050,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))

etable(mlist,
       keep = c("delta_uppone", "delta_upaone"))

etable(mlist,
       file = paste0(path_graph,
                     "/tab_raw_NoSubset.tex"),
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




# ----- Table A.3 -----
mlist <- list()
mlist$m1 <- feols(delta_popone
                  ~ delta_uppone
                  + delta_upaone,
                  data = subset(cit_050b, state_const1 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m2 <- feols(delta_popone
                  ~ delta_uppone
                  + delta_upaone
                  | REGION_UN^halfcentury,
                  data = subset(cit_050b, state_const1 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m3 <- feols(delta_popone
                  ~ delta_uppone
                  + delta_upaone
                  + delta_cap1
                  | REGION_UN^halfcentury, 
                  data = subset(cit_050b, state_const1 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m4 <- feols(delta_popone
                  ~ delta_uppone
                  + delta_upaone
                  + delta_cap1
                  + delta_ncit1
                  | REGION_UN^halfcentury, 
                  data = subset(cit_050b, state_const1 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m5 <- feols(delta_popone
                  ~ delta_uppone
                  + delta_upaone
                  + delta_cap1
                  + delta_ncit1 + delta_area1
                  | REGION_UN^halfcentury, 
                  data = subset(cit_050b, state_const1 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m6 <- feols(delta_popone
                  ~ delta_uppone
                  + delta_upaone
                  + delta_cap1
                  + delta_ncit1*delta_area1
                  | REGION_UN^halfcentury, 
                  data = subset(cit_050b, state_const1 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))

etable(mlist,
       keep = c("delta_uppone", "delta_upaone"))

etable(mlist,
       file = paste0(path_graph,
                     "/tab_rawBAL.tex"),
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




# ----- Figure A.1 -----
mlist <- list()
mlist$m1 <- feols(delta_popone
                  ~ delta_uppone:halfcentury
                  + delta_upaone:halfcentury
                  + delta_cap1
                  + delta_ncit1*delta_area1
                  + halfcentury
                  | REGION_UN^halfcentury, data = subset(cit_050b, state_const1 == 1),
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
                  | REGION_UN^halfcentury, data = subset(cit_050b, state_const1 == 1),
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
                  | REGION_UN^halfcentury, data = subset(cit_050b, state_const1 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))


ct <- coeftable(mlist$m1) %>% 
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
  dplyr::mutate(seqs = case_when(polity1 == T ~ "One",
                                 polity2 == T ~ "Two",
                                 polity3 == T ~ "Three")) %>% 
  dplyr::mutate(seqs = factor(seqs,
                              levels = c("One", "Two", "Three"))) %>% 
  dplyr::select(-c(polity1, polity2, polity3)) %>% 
  arrange(seqs, century)

ggplot(ct, aes(x = century, y = estimate, ymin = conf_low, ymax = conf_high)) +
  geom_ribbon(fill = "grey65", alpha = .3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_line(col = "black", size = .5) +
  scale_x_continuous(breaks = seq(1000, 2000, 100),
                     labels = seq(1000, 2000, 100)) +
  theme_list$theme_anatem

ggsave(plot = last_plot(),
       file = paste0(path_graph,
                     "/wave1multivariateBAL.png"))
knitr::plot_crop(paste0(path_graph,
                        "/wave1multivariateBAL.png"),
                 quiet = T)




# ----- Table A.4 -----

mlist <- list()
mlist$m1 <- feols(delta_popone
                  ~ delta_uppone
                  + delta_upaone,
                  data = subset(cit_100, state_const2 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m2 <- feols(delta_popone
                  ~ delta_uppone
                  + delta_upaone
                  | REGION_UN^century,
                  data = subset(cit_100, state_const2 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m3 <- feols(delta_popone
                  ~ delta_uppone
                  + delta_upaone
                  + delta_cap1
                  | REGION_UN^century, 
                  data = subset(cit_100, state_const2 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m4 <- feols(delta_popone
                  ~ delta_uppone
                  + delta_upaone
                  + delta_cap1
                  + delta_ncit1
                  | REGION_UN^century, 
                  data = subset(cit_100, state_const2 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m5 <- feols(delta_popone
                  ~ delta_uppone
                  + delta_upaone
                  + delta_cap1
                  + delta_ncit1 + delta_area1
                  | REGION_UN^century, 
                  data = subset(cit_100, state_const2 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m6 <- feols(delta_popone
                  ~ delta_uppone
                  + delta_upaone
                  + delta_cap1
                  + delta_ncit1*delta_area1
                  | REGION_UN^century, 
                  data = subset(cit_100, state_const2 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))

etable(mlist,
       keep = c("delta_uppone", "delta_upaone"))

etable(mlist,
       file = paste0(path_graph,
                     "tab_rawPURE.tex"),
       replace = T,
       sdBelow = T,
       # extraline = list("{title: \\midrule Model details} Time invariant geographic variables $\\times$ year" =
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


# ----- Figure A.2 -----
mlist <- list()
mlist$m1 <- feols(delta_popone
                  ~ delta_uppone:century
                  + delta_upaone:century
                  + delta_cap1
                  + delta_ncit1*delta_area1
                  + century
                  | REGION_UN^century, data = subset(cit_100, state_const2 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))


ct <- coeftable(mlist$m1) %>% 
  tibble::rownames_to_column(., "predictor") %>% 
  filter(grepl(("delta_upp"), predictor)) %>% 
  dplyr::rename(estimate = Estimate,
                std_error = 'Std. Error') %>%
  dplyr::select(c(predictor, estimate, std_error)) %>% 
  dplyr::mutate(conf_low = estimate - (1.96 * std_error),
                conf_high = estimate + (1.96 * std_error)) %>% 
  dplyr::mutate(century = readr::parse_number(predictor)) %>% 
  arrange(century)

ggplot(ct, aes(x = century, y = estimate, ymin = conf_low, ymax = conf_high)) +
  geom_ribbon(fill = "grey65", alpha = .3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_line(col = "black", size = .5) +
  scale_x_continuous(breaks = seq(1000, 2000, 100),
                     labels = seq(1000, 2000, 100)) +
  theme_list$theme_anatem

ggsave(plot = last_plot(),
       file = paste0(path_graph,
                     "wave1multivariatePURE.png"))
knitr::plot_crop(paste0(path_graph,
                        "wave1multivariatePURE.png"),
                 quiet = T)


# ----- Table A.5 -----
mlist <- list()
mlist$m1 <- feols(delta_popone
                  ~ delta_uppone:halfcentury
                  + delta_upaone:halfcentury
                  + halfcentury
                  | REGION_UN^halfcentury, data = subset(cit_050, state_const1 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m2 <- feols(delta_popone
                  ~ delta_uppone:halfcentury
                  + delta_upaone:halfcentury
                  + delta_cap1
                  + delta_ncit1*delta_area1
                  + halfcentury
                  | REGION_UN^halfcentury, data = subset(cit_050, state_const1 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m3 <- feols(delta_popone
                  ~ delta_uppone:halfcentury
                  + delta_upaone:halfcentury
                  + halfcentury
                  | REGION_UN^halfcentury, data = cit_050,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m4 <- feols(delta_popone
                  ~ delta_uppone:halfcentury
                  + delta_upaone:halfcentury
                  + delta_cap1
                  + delta_ncit1*delta_area1
                  + halfcentury
                  | REGION_UN^halfcentury, data = cit_050,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))

etable(mlist, keep = c("uppone", "upaone"))

etable(mlist,
       file = paste0(path_graph,
                     "/tab_upp_hist.tex"),
       replace = T,
       sdBelow = F,
       extraline = list("{title: \\midrule Model details} City- and polity-level controls" =
                          c("","$\\checkmark$", "", "$\\checkmark$"),
                        "Restricted sample" =
                          c("$\\checkmark$", "$\\checkmark$", "", "")),
       tex = T,
       dict = myDict,
       signifCode = c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       digits = "r3",
       keep = c("DUP", "TUP"),
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










# ----- Figure A.3 -----
mlist <- list()
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

ct <- coeftable(mlist$m3) %>% 
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
                     "wave3multivariate.png"),
       width = 9,
       height = 9)
knitr::plot_crop(paste0(path_graph,
                        "wave3multivariate.png"),
                 quiet = T)



# ----- Figure A.4 -----
mlist <- list()
mlist$m3 <- feols(delta_popone
                  ~ delta_upaZ:halfcentury
                  + delta_cap1
                  + delta_ncit1*delta_area1
                  + halfcentury
                  | REGION_UN^century, data = subset(cit_050, state_const1 == 1),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m4 <- feols(delta_popone
                  ~ delta_upaZ:halfcentury
                  + delta_cap1
                  + delta_ncit1*delta_area1
                  + halfcentury
                  | REGION_UN^century, data = cit_050,
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))

# extract coefficients
ct3 <- coeftable(mlist$m3) %>% 
  tibble::rownames_to_column(., "predictor") %>% 
  filter(grepl(("delta_upaZ"), predictor)) %>% 
  dplyr::rename(estimate = Estimate,
                std_error = 'Std. Error') %>%
  dplyr::select(c(predictor, estimate, std_error)) %>% 
  dplyr::mutate(conf_low = estimate - (1.96 * std_error),
                conf_high = estimate + (1.96 * std_error)) %>% 
  dplyr::mutate(century = readr::parse_number(predictor)) %>% 
  arrange(century) %>% 
  mutate(type = "State consistency subset")

ct4 <- coeftable(mlist$m4) %>% 
  tibble::rownames_to_column(., "predictor") %>% 
  filter(grepl(("delta_upaZ"), predictor)) %>% 
  dplyr::rename(estimate = Estimate,
                std_error = 'Std. Error') %>%
  dplyr::select(c(predictor, estimate, std_error)) %>% 
  dplyr::mutate(conf_low = estimate - (1.96 * std_error),
                conf_high = estimate + (1.96 * std_error)) %>% 
  dplyr::mutate(century = readr::parse_number(predictor)) %>% 
  arrange(century) %>% 
  mutate(type = "All")

# gather data
ct <- rbind(ct3, ct4)

# plot it
ggplot(ct, aes(x = century, y = estimate, ymin = conf_low, ymax = conf_high)) +
  #geom_ribbon(aes(group = type), fill = "grey65", alpha = .3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(group = type, col = type)) +
  geom_line(aes(group = type, col = type), size = .5) +
  # geom_smooth(aes(group = type, col = type),
  #             method = "loess") +
  scale_color_manual(values = c("black", "grey55")) +
  scale_x_continuous(breaks = seq(1000, 2000, 100),
                     labels = seq(1000, 2000, 100)) +
  #scale_y_continuous(limits = c(-0.29, 0.75)) +
  theme_list$theme_anatem

ggsave(plot = last_plot(),
       file = paste0(path_graph,
                     "wave1_UPA_raw.png"))
knitr::plot_crop(paste0(path_graph,
                        "wave1_UPA_raw.png"),
                 quiet = T)

ggplot(ct, aes(x = century, y = estimate, ymin = conf_low, ymax = conf_high)) +
  #geom_ribbon(aes(group = type), fill = "grey65", alpha = .3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  # geom_point(aes(group = type, col = type)) +
  # geom_line(aes(group = type, col = type), size = .5) +
  geom_smooth(aes(group = type, col = type),
              method = "loess") +
  scale_color_manual(values = c("black", "grey55")) +
  scale_x_continuous(breaks = seq(1000, 2000, 100),
                     labels = seq(1000, 2000, 100)) +
  #scale_y_continuous(limits = c(-0.29, 0.75)) +
  theme_list$theme_anatem

ggsave(plot = last_plot(),
       file = paste0(path_graph,
                     "wave1_UPA_raw_loess.png"))
knitr::plot_crop(paste0(path_graph,
                        "wave1_UPA_raw_loess.png"),
                 quiet = T)



# ----- Figure A.5 -----
mlist <- list()
mlist$m1 <- feols(delta_popone
                  ~ delta_upaZ:halfcentury
                  + delta_cap1
                  + delta_ncit1*delta_area1
                  + halfcentury
                  | REGION_UN^century, data = subset(cit_050, REGION_UN == "Europe"),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m2 <- feols(delta_popone
                  ~ delta_upaZ:halfcentury
                  + delta_cap1
                  + delta_ncit1*delta_area1
                  + halfcentury
                  | REGION_UN^century, data = subset(cit_050, REGION_UN == "Asia" | REGION_UN == "Africa"),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m3 <- feols(delta_popone
                  ~ delta_upaZ:halfcentury
                  + delta_cap1
                  + delta_ncit1*delta_area1
                  + halfcentury
                  | REGION_UN^century, data = subset(cit_050, region_v2 == "Western Europe"),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))
mlist$m4 <- feols(delta_popone
                  ~ delta_upaZ:halfcentury
                  + delta_cap1
                  + delta_ncit1*delta_area1
                  + halfcentury
                  | REGION_UN^century, data = subset(cit_050, region_v2 == "Eastern Europe"),
                  panel.id = c('city_id', 'year'),
                  cluster = c("city_id"))

# extract data
ct1 <- coeftable(mlist$m1) %>% 
  tibble::rownames_to_column(., "predictor") %>% 
  filter(grepl(("delta_upaZ"), predictor)) %>% 
  dplyr::rename(estimate = Estimate,
                std_error = 'Std. Error') %>%
  dplyr::select(c(predictor, estimate, std_error)) %>% 
  dplyr::mutate(conf_low = estimate - (1.96 * std_error),
                conf_high = estimate + (1.96 * std_error)) %>% 
  dplyr::mutate(century = readr::parse_number(predictor)) %>% 
  arrange(century) %>% 
  mutate(type = "Europe")

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
  mutate(type = "MENA")

ct3 <- coeftable(mlist$m3) %>% 
  tibble::rownames_to_column(., "predictor") %>% 
  filter(grepl(("delta_upaZ"), predictor)) %>% 
  dplyr::rename(estimate = Estimate,
                std_error = 'Std. Error') %>%
  dplyr::select(c(predictor, estimate, std_error)) %>% 
  dplyr::mutate(conf_low = estimate - (1.96 * std_error),
                conf_high = estimate + (1.96 * std_error)) %>% 
  dplyr::mutate(century = readr::parse_number(predictor)) %>% 
  arrange(century) %>% 
  mutate(type = "Western Europe")

ct4 <- coeftable(mlist$m4) %>% 
  tibble::rownames_to_column(., "predictor") %>% 
  filter(grepl(("delta_upaZ"), predictor)) %>% 
  dplyr::rename(estimate = Estimate,
                std_error = 'Std. Error') %>%
  dplyr::select(c(predictor, estimate, std_error)) %>% 
  dplyr::mutate(conf_low = estimate - (1.96 * std_error),
                conf_high = estimate + (1.96 * std_error)) %>% 
  dplyr::mutate(century = readr::parse_number(predictor)) %>% 
  arrange(century) %>% 
  mutate(type = "Eastern Europe")

# gather data
ct <- rbind(ct1,
            ct2,
            ct3,
            ct4)

# plot it
ggplot(ct, aes(x = century, y = estimate, ymin = conf_low, ymax = conf_high)) +
  geom_ribbon(aes(group = type), fill = "grey65", alpha = .3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_line(size = .5) +
  # geom_smooth(aes(group = type, col = type),
  #             method = "loess") +
  scale_color_manual(values = c("black", "grey55")) +
  scale_x_continuous(breaks = seq(1000, 2000, 100),
                     labels = seq(1000, 2000, 100)) +
  #scale_y_continuous(limits = c(-0.29, 0.75)) +
  theme_list$theme_anatem +
  facet_wrap(~ type, scales = "free_y")

ggsave(plot = last_plot(),
       file = paste0(path_graph,
                     "Regional_UPA_raw.png"))
knitr::plot_crop(paste0(path_graph,
                        "Regional_UPA_raw.png"),
                 quiet = T)









# ----- fin -----






