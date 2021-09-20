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
               parallel)

path_paper <- paste0(getwd(), "/papers_jeppe/8 trade/")
path_paper

path_data <- paste0(getwd(), "/papers_jeppe/8 trade/data/")
path_data

path_graph <- paste0(getwd(), "/papers_jeppe/8 trade/tex/")
path_graph

# ---------------------------------------------------------
# Load master data
# ---------------------------------------------------------
# ----- City data
cities <- rio::import(paste0(path_paper,
                             "data/df_full/",
                             "cities_raw.Rdata"))

# convert to spatial feature, keep only unique cities
cit_sf <- cities %>% 
  dplyr::select(city_id, latitude, longitude) %>% 
  distinct(., city_id, .keep_all = T) %>% 
  st_as_sf(.,
           coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  dplyr::mutate(row_id = row_number())

# ----- Eurasia data
eurasia_union <- st_read(dsn = "5_centennia/eurasia_map_union",
                         layer = "eurasia_map_union",
                         crs = 4326)

# ---------------------------------------------------------
# Agricultural suitability (Zabel)
# ---------------------------------------------------------
library(raster)
library(rgdal)

# load agricultural suitability data
suit_rainfed <- raster("Zabel (2014)/rainfed/overall_suitability_1/overall_suitability_1.bil")
suit_irrigated <- raster("Zabel (2014)/irrigated/overall_suitability_1/overall_suitability_1.bil")
suit_both <- raster("Zabel (2014)/rainfed_irrigated/overall_cropsuit_i_1961-1990/overall_cropsuit_i_1961-1990.bil")

# create empty list
zabel_list <- list()

# add the suitability data to the list, one dataset pr element
zabel_list[[1]] <- suit_rainfed
zabel_list[[2]] <- suit_irrigated
zabel_list[[3]] <- suit_both

# ----- Define function to fix the suitability data
fun_fixZabel <- function(zabel, zabel_fixed) { 
  
  zabel_fixed <- crop(zabel, extent(eurasia_union))
  zabel_fixed <- mask(zabel_fixed, eurasia_union)
  crs(zabel_fixed) <- CRS('+init=EPSG:4326')
  
  return(zabel_fixed)
  
}

# ----- Apply function, crops the data to the Centennia region
# set the number of cores. I use all cores but 1, so close ALL other applications
no_cores <- 3

# set up the parallel processing session
# depending on Mac/Windows you might need to work around with multisession/multicore/multiprocess
# see: https://cran.r-project.org/web/packages/future/vignettes/future-1-overview.html
# check your activity monitor, there should be several processes running
plan(multisession, workers = no_cores)

# apply the function
tic()
zlist <- furrr::future_map(zabel_list,
                           fun_fixZabel,
                           .progress = T,
                           .options = furrr_options(seed = TRUE,
                                                    scheduling = 1))
toc()
future::plan(sequential)

# extract the fixed suitability data from the new list
suit_rainfed <- zlist[[1]]
suit_irrigated <- zlist[[2]]
suit_both <- zlist[[3]]

# ----- Plot data
# library(RColorBrewer)
# col_scale <- brewer.pal(9, "Greens")
# 
# tm_shape(suit_rainfed) +
#   tm_raster(palette = col_scale) +
#   tm_legend(position = c("right", "bottom"),
#             title = "",
#             legend.show = T,
#             legend.outside = F,
#             legend.title.size = .01,
#             legend.title.color = "white")
# 
# tmap_save(filename = paste0(path_graph,
#                             "rainfed.png"))
# knitr::plot_crop(paste0(path_graph,
#                         "rainfed.png"),
#                  quiet = T)
# 
# tm_shape(suit_irrigated) +
#   tm_raster(palette = col_scale) +
#   tm_legend(position = c("right", "bottom"),
#             title = "",
#             legend.show = T,
#             legend.outside = F,
#             legend.title.size = .01,
#             legend.title.color = "white")
# 
# tmap_save(filename = paste0(path_graph,
#                             "irrigated.png"))
# knitr::plot_crop(paste0(path_graph,
#                         "irrigated.png"),
#                  quiet = T)
# 
# tm_shape(suit_both) +
#   tm_raster(palette = col_scale) +
#   tm_legend(position = c("right", "bottom"),
#             title = "",
#             legend.show = T,
#             legend.outside = F,
#             legend.title.size = .01,
#             legend.title.color = "white")
# 
# tmap_save(filename = paste0(path_graph,
#                             "both.png"))
# knitr::plot_crop(paste0(path_graph,
#                         "both.png"),
#                  quiet = T)

# ----- Match to cities, measure suitability at the city level (time invariant)
buffer_lenth <- 50 # set buffer zone (50km)

# calculate suitability
rainfed <- extract(suit_rainfed, cit_sf,
                   buffer = buffer_lenth*1000)
irrigated <- extract(suit_irrigated, cit_sf,
                     buffer = buffer_lenth*1000)
both <- extract(suit_both, cit_sf,
                buffer = buffer_lenth*1000)


# tidy the data, calculate mean suitability and add it to the unique city data
cit_sf$suitRainfed <- unlist(lapply(rainfed,
                                    function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
cit_sf$suitIrrigated <- unlist(lapply(irrigated,
                                      function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
cit_sf$suitBoth <- unlist(lapply(both,
                                 function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))

# prepare for export
agri <- cit_sf %>% 
  as.data.frame() %>% 
  dplyr::select(-c(geometry, row_id))

# ----- Export data
save(agri,
     file = paste0(path_data,
                   "geo_data/agricultural_suitability.Rdata"))

rm(agri, suit_rainfed, suit_irrigated, suit_both, zabel_list, zlist,
   irrigated, rainfed, both)


# ---------------------------------------------------------
# Grid cells
# ---------------------------------------------------------
# ----- Varying size grid
# prepare city data
cit_sf <- cities %>% 
  dplyr::select(city_id, latitude, longitude) %>% 
  distinct(., city_id, .keep_all = T) %>% 
  st_as_sf(.,
           coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  dplyr::mutate(row_id = row_number())

# make a grid out of the eurasia map
grid_sf <- st_make_grid(eurasia_union,
                        n = c(50, 50),
                        #cellsize = c(grid_spacing, grid_spacing), # if we want equal area grids
                        square = TRUE) %>%
  st_as_sf() %>% 
  dplyr::mutate(area_km2 = unclass(st_area(.))/1000000) %>% # calculate km^2
  dplyr::mutate(length = sqrt(area_km2)) %>% # calculate width/height of each cell
  dplyr::mutate(gridID = row_number()) # create gridID

hist(grid_sf$area_km2)
hist(grid_sf$length)

st_crs(eurasia_union)
st_crs(grid_sf)

# plot it
ggplot() +
  geom_sf(data = eurasia_union, fill = "white", size = 0.25) +
  geom_sf(data = grid_sf, fill = NA, size = 0.1) +
  theme_list$theme_anatem_map +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

# savep plots
ggsave(plot = last_plot(),
       file = paste0(path_graph,
                     "grid_map.png"))
knitr::plot_crop(paste0(path_graph,
                        "grid_map.png"),
                 quiet = T)

# match grids to cities
grids <- st_join(cit_sf, grid_sf, join = st_nearest_feature) %>% 
  as.data.frame() %>% 
  dplyr::select(-c(geometry, row_id))

# ---------------------------------------------------------
# Export data on grid ID at the city level
# ---------------------------------------------------------
save(grids,
     file = paste0(path_data,
                   "geo_data/grid_data.Rdata"))

rm(grids, grid_sf)

# ----- Plot cities
cit_sf <- cities %>% 
  distinct(., city_id, .keep_all = T) %>%
  dplyr::select(city, latitude, longitude) %>% 
  st_as_sf(.,
           coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  dplyr::mutate(row_id = row_number())

ggplot() +
  geom_sf(data = eurasia_union, fill = "grey95", size = 0.25) +
  geom_sf(data = cit_sf, size = 0.05) +
  theme_list$theme_anatem_map +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

ggsave(plot = last_plot(),
       file = paste0(path_graph,
                     "city_map.png"))
knitr::plot_crop(paste0(path_graph,
                        "city_map.png"),
                 quiet = T)


# ---------------------------------------------------------
# Oceans
# ---------------------------------------------------------
# prepare cities
cit_sf <- cities %>% 
  dplyr::select(city_id, latitude, longitude) %>% 
  distinct(., city_id, .keep_all = T) %>% 
  st_as_sf(.,
           coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  dplyr::mutate(row_id = row_number())

# ----- Ocean data
coast <- st_read(dsn = "NE/ne_10m_geography_marine_polys",
                 layer = "ne_10m_geography_marine_polys",
                 crs = 4326)

# crop it to the eurasia region
coast <- st_crop(coast,
                 eurasia_union)

# ----- create one Atlantic ocean polygon instead of many
atlantic_coast <- coast %>%
  filter(name %in% c("North Atlantic Ocean",
                     "South Atlantic Ocean",
                     "English Channel",
                     "Irish Sea",
                     "Inner Seas",
                     "Bay of Biscay",
                     "North Sea",
                     "Bristol Channel",
                     "Norwegian Sea",
                     "Vestfjorden",
                     "Greenland Sea",
                     "Denmark Strait",
                     "Skagerrak",
                     "Kattegat",
                     "Øresund")) %>% 
  st_union()

# ggplot() +
#   geom_sf(data = eurasia_union, fill = "grey60", col = "black", size = 0.25) +
#   geom_sf(data = atlantic_coast, fill = "grey90", col = "black", size = 0.25) +
#   theme_list$theme_anatem_map +
#   theme(axis.line = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank())


# ----- create one Mediterranean Sea polygon instead of many
medi_coast <- coast %>%
  filter(name %in% c("Alboran Sea",
                     "Mediterranean Sea",
                     "Balearic Sea",
                     "Golfe du Lion",
                     "Ligurian Sea",
                     "Tyrrhenian Sea",
                     "Adriatic Sea",
                     "Ionian Sea",
                     "Gulf of Gabès",
                     "Gulf of Sidra",
                     "Sea of Crete",
                     "Aegean Sea")) %>% 
  st_union()

# ggplot() +
#   geom_sf(data = eurasia_union, fill = "grey60", col = "black", size = 0.25) +
#   geom_sf(data = medi_coast, fill = "grey90", col = "black", size = 0.25) +
#   theme_list$theme_anatem_map +
#   theme(axis.line = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank())

rm(coast)

# ----- Calculate distances from cities to diff. oceans
dist_atlantic <- st_distance(cit_sf, atlantic_coast) %>% unclass()
dist_medi <- st_distance(cit_sf, medi_coast) %>% unclass()

# gather distances in one dataset
maritime_df <- data.frame(DistAtlantic = dist_atlantic,
                          DistMedi = dist_medi) %>% 
  dplyr::mutate(row_id = row_number()) %>% 
  tidylog::left_join(.,
                     cit_sf,
                     by = "row_id") %>% 
  as.data.frame() %>% 
  dplyr::select(-c(row_id, geometry))


# export maritime distances data
save(maritime_df,
     file = paste0(path_data,
                   "geo_data/coast_data.Rdata"))

rm(maritime_df, medi_coast, atlantic_coast, dist_atlantic, dist_medi)

# ---------------------------------------------------------
# Rivers
# ---------------------------------------------------------
# prepare cities
cit_sf <- cities %>% 
  dplyr::select(city_id, latitude, longitude) %>% 
  distinct(., city_id, .keep_all = T) %>% 
  st_as_sf(.,
           coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  dplyr::mutate(row_id = row_number())

# ----- River/lake data
river_lake <- st_read(dsn = "NE/ne_10m_rivers_lake_centerlines",
                      layer = "ne_10m_rivers_lake_centerlines",
                      crs = 4326)

# crop to the eurasian region
river_lake <- st_intersection(river_lake,
                              eurasia_union)

# plot it
ggplot() +
  geom_sf(data = eurasia_union, fill = "white", col = "black", size = 0.35) +
  geom_sf(data = river_lake, col = "grey50", size = 0.25) +
  theme_list$theme_anatem_map +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

# save plot
ggsave(plot = last_plot(),
       file = paste0(path_graph,
                     "river_map.png"))
knitr::plot_crop(paste0(path_graph,
                        "river_map.png"),
                 quiet = T)


# ----- Calculate distances from cities to rivers
tic()
dist_river <- st_nn(cit_sf, river_lake, k = 1, returnDist = T,
                 progress = T,
                 parallel = 4)
toc()
dist_river <- unlist(dist_river[[2]])

# gather river distance data
river_df <- data.frame(DistRiver = dist_river) %>% 
  dplyr::mutate(row_id = row_number()) %>% 
  tidylog::left_join(.,
                     cit_sf,
                     by = "row_id") %>% 
  as.data.frame() %>% 
  dplyr::select(-c(row_id, geometry))

# export data
save(river_df,
     file = paste0(path_data,
                   "geo_data/river_data.Rdata"))

rm(river_df, dist_river, river_lake)


# ---------------------------------------------------------
# Roman Roads
# ---------------------------------------------------------
# prepare cities
cit_sf <- cities %>% 
  dplyr::select(city_id, latitude, longitude) %>% 
  distinct(., city_id, .keep_all = T) %>% 
  st_as_sf(.,
           coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  dplyr::mutate(row_id = row_number())

# ----- Roman roads data
roman_roads <- st_read(dsn = "McCormick et al 2013a/dataverse_files",
                       layer = "roman_roads_v2008")

# transform CRS of roads
roman_roads <- st_transform(roman_roads, crs = 4326)

# plot it
ggplot() +
  geom_sf(data = eurasia_union, fill = "white", size = 0.25) +
  geom_sf(data = roman_roads, col = "firebrick", size = .2) +
  coord_sf(xlim = c(-15, 40),
           ylim = c(25, 60)) +
  theme_list$theme_anatem_map +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

# save plot 
ggsave(plot = last_plot(),
       file = paste0(path_graph,
                     "romanroads_map.png"))
knitr::plot_crop(paste0(path_graph,
                        "romanroads_map.png"),
                 quiet = T)

# ----- Calculate distances from cities to roman roads
tic()
dist_roman <- st_nn(cit_sf, roman_roads, k = 1, returnDist = T,
                    progress = T,
                    parallel = 10)
toc()

# unpack distances
dist_roman <- unlist(dist_roman[[2]])

# create distance dataset to be exported
roman_df <- data.frame(DistRoman = dist_roman) %>% 
  dplyr::mutate(row_id = row_number()) %>% 
  tidylog::left_join(.,
                     cit_sf,
                     by = "row_id") %>% 
  as.data.frame() %>% 
  dplyr::select(-c(row_id, geometry))

# export data
save(roman_df,
     file = paste0(path_data,
                   "geo_data/roman_data.Rdata"))

rm(dist_roman, roman_df, roman_roads)


# ---------------------------------------------------------
# Muslim Trade Routes
# ---------------------------------------------------------
# prepare cities (time variant!)
cit_sf <- cities %>% 
  dplyr::select(city_id, latitude, longitude) %>% 
  distinct(., city_id, .keep_all = T) %>% 
  st_as_sf(.,
           coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  dplyr::mutate(row_id = row_number())

# ----- Silk Roads data
year_sec <- seq(1100, 1700, 200) # the muslim trade routes data appear in 1100, 1300, 1500, 1700

# create empty list for the silk roads data
silk_roads_list <- list()

# load the data in a loop
for (i in seq_along(year_sec)) {
  
  layer_path <- paste0("Trade Routes ", year_sec[[i]])
  
  print(layer_path)
  
  silkroad <- st_read(dsn = "trade routes/traderoutes",
                      layer = layer_path,
                      crs = 4326)
  
  silkroad <- st_crop(silkroad,eurasia_union) # crop to the eurasia region
  
  silk_roads_list[[i]] <- silkroad
  
}

# extract from list
sr1100 <- silk_roads_list[[1]]
sr1300 <- silk_roads_list[[2]]
sr1500 <- silk_roads_list[[3]]
sr1700 <- silk_roads_list[[4]]

# ----- Plot roads
# road_size = 0.75
# ggplot() +
#   geom_sf(data = eurasia_union, fill = "white", size = 0.25) +
#   geom_sf(data = sr1100, col = "firebrick", size = road_size) +
#   theme_list$theme_anatem_map +
#   theme(axis.line = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank())
# 
# ggsave(plot = last_plot(),
#        file = paste0(path_graph,
#                      "/silk1100_map.png"))
# knitr::plot_crop(paste0(path_graph,
#                         "/silk1100_map.png"),
#                  quiet = T)
# 
# ggplot() +
#   geom_sf(data = eurasia_union, fill = "white", size = 0.25) +
#   geom_sf(data = sr1300, col = "firebrick", size = road_size) +
#   theme_list$theme_anatem_map +
#   theme(axis.line = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank())
# 
# ggsave(plot = last_plot(),
#        file = paste0(path_graph,
#                      "/silk1300_map.png"))
# knitr::plot_crop(paste0(path_graph,
#                         "/silk1300_map.png"),
#                  quiet = T)
# 
# ggplot() +
#   geom_sf(data = eurasia_union, fill = "white", size = 0.25) +
#   geom_sf(data = sr1500, col = "firebrick", size = road_size) +
#   theme_list$theme_anatem_map +
#   theme(axis.line = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank())
# ggsave(plot = last_plot(),
#        file = paste0(path_graph,
#                      "/silk1500_map.png"))
# knitr::plot_crop(paste0(path_graph,
#                         "/silk1500_map.png"),
#                  quiet = T)
# 
# ggplot() +
#   geom_sf(data = eurasia_union, fill = "white", size = 0.25) +
#   geom_sf(data = sr1700, col = "firebrick", size = road_size) +
#   theme_list$theme_anatem_map +
#   theme(axis.line = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank())
# ggsave(plot = last_plot(),
#        file = paste0(path_graph,
#                      "/silk1700_map.png"))
# knitr::plot_crop(paste0(path_graph,
#                         "/silk1700_map.png"),
#                  quiet = T)

# ----- Calculate distances from cities to muslim trade routes
tic()
d_silk11 <- st_nn(cit_sf, sr1100, k = 1, returnDist = T,
                  progress = T,
                  parallel = 10)
d_silk11 <- unlist(d_silk11[[2]])

d_silk13 <- st_nn(cit_sf, sr1300, k = 1, returnDist = T,
                  progress = T,
                  parallel = 10)
d_silk13 <- unlist(d_silk13[[2]])

d_silk15 <- st_nn(cit_sf, sr1500, k = 1, returnDist = T,
                  progress = T,
                  parallel = 10)
d_silk15 <- unlist(d_silk15[[2]])

d_silk17 <- st_nn(cit_sf, sr1700, k = 1, returnDist = T,
                  progress = T,
                  parallel = 10)
d_silk17 <- unlist(d_silk17[[2]])
toc()

# gather the four time-variant distances in one dataset
muslim_df <- data.frame(Muslim1100 = d_silk11,
                        Muslim1300 = d_silk13,
                        Muslim1500 = d_silk15,
                        Muslim1700 = d_silk17) %>% 
  dplyr::mutate(row_id = row_number()) %>% 
  tidylog::left_join(.,
                     cit_sf,
                     by = "row_id") %>% 
  as.data.frame() %>% 
  dplyr::select(-c(row_id, geometry))

# export the distance data
save(muslim_df,
     file = paste0(path_data,
                   "geo_data/muslimtrade_data.Rdata"))

rm(d_silk11, d_silk13, d_silk15, d_silk17,
   sr1100, sr1300, sr1500, sr1700, muslim_df, silkroad,
   silk_roads_list)


# ---------------------------------------------------------
# Ruggedness (Shaver et al. 2019)
# ---------------------------------------------------------
# prepare cities
cit_sf <- cities %>% 
  dplyr::select(city_id, latitude, longitude) %>% 
  distinct(., city_id, .keep_all = T) %>% 
  st_as_sf(.,
           coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  dplyr::mutate(row_id = row_number())

# ----- Shaver et al. data
shaver <- raster("Shaver et al 2019/shaver_eurasia/ruggedness.tif")
crs(shaver)
crs(cit_sf)

# ----- Extract city-level ruggedness data
buffer_lenth <- 50 # set buffer zone (50km)

# extract data at the city level
rugged_data <- extract(shaver, cit_sf,
                       buffer = buffer_lenth*1000)

# prepare for export
temp <- cit_sf %>% 
  as.data.frame() %>% 
  dplyr::select(c(city_id))

# tidy data, calculate average ruggedness
temp$ruggedness <- unlist(lapply(rugged_data,
                                 function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
  

# ----- Export data
save(temp,
     file = paste0(path_data,
                   "geo_data/ruggedness_data.Rdata"))

rm(shaver, temp, cit_sf, rugged_data)

rm(cities, eurasia_union, cit_list)
