
library(sf)     
library(raster)
library(ggplot2)

## Map data --------------------------------------------------------------------

## Load map data
strata <- read_sf("data-raw/DFO_NL_survey_strat/2HJ3KLNOP_Strata_Polygons_WGS84.shp", 
                  layer = "2HJ3KLNOP_Strata_Polygons_WGS84") %>% 
    st_transform(crs = 4326)
strata <- strata[strata$DIV %in% c("2J", "3K", "3L"), ]      
strata_utm <- st_transform(x = strata, crs = 32622)
nl <- read_sf("data-raw/NL_shapefile/NL.shp", layer = "NL")
nl_utm <- st_transform(x = nl, crs = 32622) # utm zone 22
na <- read_sf("data-raw/north_america_bathy/bathy.shp", layer = "bathy")
na_utm <- st_transform(x = na, crs = 32622)
na_land <- read_sf("data-raw/north_america/bound_p.shp", layer = "bound_p")
na_land <- na_land[na_land$COUNTRY != "water/agua/d'eau", ]
nafo <- read_sf("data-raw/NAFO_divisions/Divisions.shp", layer = "Divisions") # downloaded these data here: http://www.nafo.int/about/frames/area.html
nafo <- nafo[!is.na(nafo$ZONE), ]
nafo <- nafo["ZONE"]
nafo_utm <- st_transform(x = nafo, crs = 32622)
nafo_centroids <- st_centroid(nafo)
nafo_centroids <- data.frame(ZONE = nafo_centroids$ZONE, st_coordinates(nafo_centroids))
nafo_centroids_utm <- st_centroid(nafo_utm)
nafo_centroids_utm <- data.frame(ZONE = nafo_centroids_utm$ZONE, st_coordinates(nafo_centroids_utm))
bathy <- raster("data-raw/GEBCO/NAFO_GEBCO_data.nc")  # downloaded these data here: http://www.gebco.net/
bathy_utm <- projectRaster(bathy, crs = CRS("+proj=utm +zone=22 +datum=WGS84 +units=m +no_defs"))

fall_transects <- read_sf("data-raw/1990sAcoustictransects/NAFC_Fall_Acoustic_survey.shp", 
                          layer = "NAFC_Fall_Acoustic_survey")
fall_transects <- st_transform(fall_transects, crs = 4326)
ussr_transects <- read_sf("data-raw/1990sAcoustictransects/USSR_Fall_Acoustic_Survey.shp", 
                          layer = "USSR_Fall_Acoustic_Survey")
ussr_transects <- st_transform(ussr_transects, crs = 4326)
spring_ussr_transects <- read_sf("data-raw/1990sAcoustictransects/Bakanev1992_CapelinSurveyTrack.shp", 
                                 layer = "Bakanev1992_CapelinSurveyTrack")
spring_ussr_transects <- st_set_crs(spring_ussr_transects, 32621) # crs missing...guess utm zone 21
spring_ussr_transects <- st_transform(spring_ussr_transects, crs = 4326)
raw_sp_transects <- read.csv("data-raw/mostly_clearn_cruise_track.csv")

cap <- read.csv("data-raw/Fall_MS_catches_of_capelin_all_set_types_1978-2016.csv")
cap$lon <- cap$LONG_DEC
cap$lat <- cap$LAT_DEC
cap <- st_as_sf(cap, coords = c("lon", "lat"), crs = 4326)
cap_utm <-  st_transform(x = cap, crs = 32622)


## Blind spot area -------------------------------------------------------------

inshore_strata <- strata_utm[strata_utm$STRAT %in% c(608, 609:616, 618:619, 784:800), ]
inshore_strata <- st_union(inshore_strata)
stock_sets <- cap_utm[cap_utm$DIV %in% c("2J", "3K", "3L"), ]
stock_nafo <- nafo_utm[nafo_utm$ZONE %in% c("2J", "3K", "3L"), ]
stock_nafo <- st_union(stock_nafo)
stock_strata <- st_union(strata_utm)
stock_strata <- st_intersection(stock_nafo, stock_strata)
land_buff <- st_buffer(nl_utm, dist = 50000)
coast_buff <- st_intersection(stock_nafo, land_buff)
blind_spot <- st_difference(coast_buff, stock_strata)
plot(stock_nafo)
plot(stock_strata, add = TRUE, col = "grey")
plot(stock_sets, add = TRUE, pch = ".")
plot(nl_utm, add = TRUE)
plot(land_buff, add = TRUE)
plot(coast_buff, border = "blue", add = TRUE)
plot(blind_spot, add = TRUE, col = "red")
plot(inshore_strata, add = TRUE, col = "green")

## minimum density in blind spot assuming uniform distribution
kg <- c(3000000000, 6000000000)
km2 <- st_area(blind_spot) / 1000000
alt_km2 <- (st_area(blind_spot) + st_area(inshore_strata)) / 1000000
as.numeric(km2)
as.numeric(kg / km2) # kg / km^2
as.numeric(alt_km2)
as.numeric(kg / alt_km2)

blind_spot_ll <- st_transform(blind_spot, crs = 4326)
inshore_strata_ll <- st_transform(inshore_strata, crs = 4326)


## Save objects ----------------------------------------------------------------

save.image(file = "data/Fig1_map_data.RData")
