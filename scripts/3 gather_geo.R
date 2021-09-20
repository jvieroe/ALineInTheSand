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
               nngeo)

path_paper <- paste0(getwd(), "/papers_jeppe/8 trade/")
path_paper

path_data <- paste0(getwd(), "/papers_jeppe/8 trade/data/")
path_data

path_graph <- paste0(getwd(), "/papers_jeppe/8 trade/tex/")
path_graph


# ---------------------------------------------------------
# Load data
# ---------------------------------------------------------
agri <- rio::import(paste0(path_data,
                            "geo_data/agricultural_suitability.Rdata"))

coast <- rio::import(paste0(path_data,
                            "geo_data/coast_data.Rdata"))

grid <- rio::import(paste0(path_data,
                            "geo_data/grid_data.Rdata"))

muslim <- rio::import(paste0(path_data,
                            "geo_data/muslimtrade_data.Rdata"))

river <- rio::import(paste0(path_data,
                             "geo_data/river_data.Rdata"))

roman <- rio::import(paste0(path_data,
                            "geo_data/roman_data.Rdata"))

ruggedness <- rio::import(paste0(path_data,
                            "geo_data/ruggedness_data.Rdata"))


# gather in ONE dataset
geo_data <- agri %>% 
  tidylog::left_join(.,
                     coast,
                     by = "city_id") %>% 
  tidylog::left_join(.,
                     grid,
                     by = "city_id") %>% 
  tidylog::left_join(.,
                     muslim,
                     by = "city_id") %>% 
  tidylog::left_join(.,
                     river,
                     by = "city_id") %>% 
  tidylog::left_join(.,
                     roman,
                     by = "city_id") %>% 
  tidylog::left_join(.,
                     ruggedness,
                     by = "city_id")

rm(agri, coast, grid, muslim, river, roman, ruggedness, rural)  


# ----- Create binary measures
names(geo_data)

threshold <- 25 # distance threshold for access to trade (25km)

geo_data <- geo_data %>% 
  mutate(atlantic = ifelse(DistAtlantic <= 25*1000, 1, 0),
         mediter = ifelse(DistMedi <= 25*1000, 1, 0),
         roman = ifelse(DistRoman <= 5*1000, 1, 0),
         muslimtr11 = ifelse(Muslim1100 <= 5*1000, 1, 0),
         muslimtr13 = ifelse(Muslim1300 <= 5*1000, 1, 0),
         muslimtr15 = ifelse(Muslim1500 <= 5*1000, 1, 0),
         muslimtr17 = ifelse(Muslim1700 <= 5*1000, 1, 0))

# export data
save(geo_data,
     file = paste0(path_data,
                   "geo_data/0_geodata_final.Rdata"))

rm(geo_data)
