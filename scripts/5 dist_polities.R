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
               rmapshaper)

path_paper <- paste0(getwd(), "/papers_jeppe/8 trade/")
path_paper

path_data <- paste0(getwd(), "/papers_jeppe/8 trade/data/")
path_data

path_graph <- paste0(getwd(), "/papers_jeppe/8 trade/tex/")
path_graph

# ---------------------------------------------------------
# Load and prepare data
# ---------------------------------------------------------
set_crs <- 3035 # define CRS

# load cities
cities <- rio::import(paste0(path_paper,
                             "data/df_full/",
                             "cities_raw.Rdata"))

# transform to spatial feature
cit_sf <- cities %>% 
  dplyr::select(city_id, year, latitude, longitude) %>% 
  st_as_sf(.,
           coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  st_transform(.,
               crs = set_crs)

# ----- Centennia
cent <- st_read(dsn = "5_centennia/centennia_full",
                layer = "centennia_full",
                crs = 4326) %>% 
  st_transform(.,
               crs = set_crs)

# ---------------------------------------------------------
# Split data
# ---------------------------------------------------------
# define a sequence to do this within
year_sec <- seq(1000, 2000, 10)


# filter the city data to fit the time period
cit_sf <- cit_sf %>% 
  filter(year %in% year_sec)

# filter the centennia data to fit the time period
cent <- cent %>% 
  filter(year %in% year_sec)

# split the city data into a list
cit_list <- split(cit_sf,
                  f = cit_sf$year)

# split the centennia data into a list
cent_list <- split(cent,
                   f = cent$year)

rm(cit_sf, cities, cent)

# ---------------------------------------------------------
# Define function
# ---------------------------------------------------------
fun_dist <- function(cit_sf, cent, year_value, border_dist, df) {
  
  # save the year value
  year_value <- cit_sf$year[1]
  
  cit_sf <- cit_sf %>%
    dplyr::mutate(row_id = row_number())
  
  # keep only borders between polity polygons
  cent <- cent %>%
    ms_simplify(keep = .2, keep_shapes = TRUE)

  cent <- cent %>%
    ms_innerlines() %>%
    as_tibble() %>%
    st_as_sf()
  
  # calculate distance to nearest polity border
  border_dist <- st_nn(cit_sf, cent, k = 1, returnDist = T, progress = FALSE)
  border_dist <- border_dist$dist %>% unlist()
  
  cit_sf <- cit_sf %>% 
    as.data.frame() %>% 
    dplyr::select(c(city_id, row_id))
  
  # save border distance data
  df <- border_dist %>% 
    as.data.frame() %>% 
    dplyr::mutate(row_id = row_number(),
                  year = year_value) %>% 
    left_join(.,
              cit_sf,
              by = "row_id") %>% 
    dplyr::select(-row_id)
  
  return(df)
  
}


# ---------------------------------------------------------
# Apply function
# ---------------------------------------------------------
# ----- Mac
required_MB <- 1000
options(future.globals.maxSize = required_MB*1024^2)
no_cores <- availableCores() - 2
plan(multisession, workers = no_cores)
tic()
dist_list <- furrr::future_map2(cit_list,
                                cent_list,
                                fun_dist,
                                .progress = T,
                                .options = furrr_options(seed = TRUE,
                                                         scheduling = 1))
toc()
plan(sequential)

# all version: 37 min
# polity version: 33

# ---------------------------------------------------------
# Unpack data
# ---------------------------------------------------------
dist_data <- bind_rows(dist_list, .id = "year_id") %>% 
  dplyr::select(-year_id)

# rename variables
colnames(dist_data) <- c("border_distPolity", "year", "city_id")

# ---------------------------------------------------------
# Export data
# ---------------------------------------------------------
save(dist_data,
     file = paste0(path_data,
                   "border_data/border_distPolity.Rdata"))





