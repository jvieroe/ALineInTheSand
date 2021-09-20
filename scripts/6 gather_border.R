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
border_all <- rio::import(paste0(path_data,
                                 "border_data/border_distAll.Rdata"))

border_polity <- rio::import(paste0(path_data,
                                    "border_data/border_distPolity.Rdata"))

# gather data in one file
border <- border_all %>% 
  tidylog::left_join(.,
                     border_polity,
                     by = c("city_id", "year"))

rm(border_all, border_polity)  

# export data
save(border,
     file = paste0(path_data,
                   "border_data/0_borderdata_final.Rdata"))

rm(border)
