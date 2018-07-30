
## Map of center of gravity

library(sf)     
library(raster)
library(ggplot2)

## Map and model data ----------------------------------------------------------

#load("analysis/VAST_output_1995_4/Save.RData")
#Year_Set <- 1985:2016
load("analysis/Campelen_model6_v2/.RData")
strata <- read_sf("data/strata_boundaries/all_strata.shp", # import shapefiles
                  layer = "all_strata")         
strata_utm <- st_transform(x = strata, crs = 32622)
nl <- read_sf("data/NL_shapefile/NL.shp", layer = "NL")
nl_utm <- st_transform(x = nl, crs = 32622) # utm zone 22
na <- read_sf("data/north_america_bathy/bathy.shp", layer = "bathy")
na_utm <- st_transform(x = na, crs = 32622)
nafo <- read_sf("data/NAFO_divisions/Divisions.shp", layer = "Divisions") # downloaded these data here: http://www.nafo.int/about/frames/area.html
nafo <- nafo[!is.na(nafo$ZONE), ]
nafo <- nafo["ZONE"]
nafo_utm <- st_transform(x = nafo, crs = 32622)
nafo_centroids <- st_centroid(nafo)
nafo_centroids <- data.frame(ZONE = nafo_centroids$ZONE, st_coordinates(nafo_centroids))
nafo_centroids_utm <- st_centroid(nafo_utm)
nafo_centroids_utm <- data.frame(ZONE = nafo_centroids_utm$ZONE, st_coordinates(nafo_centroids_utm))
load("data/GADM/NAFO_GADM_data.Rdata") # holds sp objects "land0" and "land1"
land0 <- st_as_sf(land0)
land1 <- st_as_sf(land1)
bathy <- raster("data/GEBCO/NAFO_GEBCO_data.nc")  # downloaded these data here: http://www.gebco.net/
bathy_utm <- projectRaster(bathy, crs = CRS("+proj=utm +zone=22 +datum=WGS84 +units=m +no_defs"))

fall_transects <- read_sf("data/1990s Acoustic transects/NAFC_Fall_Acoustic_survey.shp", 
                          layer = "NAFC_Fall_Acoustic_survey")
fall_transects <- st_transform(fall_transects, crs = 4326)
ussr_transects <- read_sf("data/1990s Acoustic transects/USSR_Fall_Acoustic_Survey.shp", 
                          layer = "USSR_Fall_Acoustic_Survey")
ussr_transects <- st_transform(ussr_transects, crs = 4326)

cap <- read.csv("data/Fall_MS_catches_of_capelin_all_set_types_1978-2016.csv")
cap$lon <- cap$LONG_DEC
cap$lat <- cap$LAT_DEC
cap <- st_as_sf(cap, coords = c("lon", "lat"), crs = 4326)
cap_utm <-  st_transform(x = cap, crs = 32622)

index <- read.csv("data/capelin_biomass_spring_3L.csv")


## Blind spot area -------------------------------------------------------------

inshore_strata <- strata_utm[strata_utm$strata %in% c(608, 609:616, 618:619, 784:800), ]
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


## study area map --------------------------------------------------------------

nafo_map <- function(xlim = c(-60, -48), ylim = c(46, 55)) {
    par(mar = c(2.5, 2.5, 1, 1))
    plot(nafo["ZONE"], main = "", xlim = xlim, ylim = ylim, col = NA, axes = TRUE)
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "gray")
    plot(nafo["ZONE"], add = TRUE, col = "white", border = "grey50")
    contour(bathy, add = TRUE, col = "lightgrey", levels = -c(200, 500, 1000, 2000))
    #plot(blind_spot_ll, add = TRUE, col = "#EF3B2C", border = NA)
    #plot(inshore_strata_ll, add = TRUE, col = "#FEE0D2", border = NA)
    plot(nafo["ZONE"], add = TRUE, col = NA, border = "grey50")
    #plot(land1, add = TRUE, col = NA, border = "grey50")
    text(nafo_centroids$X, nafo_centroids$Y, nafo_centroids$ZONE, cex = 0.75, col = "black")
    box()
}

bay_labs <- data.frame(x = c(-52.77359, -52.93869, -53.18632, -54.87852, -56.24054, -53.92924),
                       y = c(47.93766, 48.30130, 49.02857, 49.93766, 50.32727, 46.63896),
                       lab = c("Conception Bay", "Trinity Bay", "Bonavista Bay", "Notre Dame Bay", "White Bay",
                               "St. Mary's Bay"),
                       pos = c(rep(4, 5), 2))
land_labs <- data.frame(x = c(-56.28181, -58.30419, -58.59393),
                        y = c(48.58701, 52.79481, 51.74396),
                        lab = c("Newfoundland", "Labrador", "Quebec"))

png("analysis/nafo_map.png", units = "in", res = 600, height = 7, width = 6)
nafo_map()
with(bay_labs, text(x, y, lab, srt = 45, pos = pos, cex = 0.75, offset = 0))
with(land_labs, text(x, y, lab))
dev.off()

png("analysis/survey_map.png", units = "in", res = 600, height = 7, width = 6)
nafo_map()
#plot(cap["vts"], add = TRUE, pch = ".", col = "#7fc97f")
plot(fall_transects, add = TRUE, col = "#beaed4", lwd = 2)
plot(ussr_transects, add = TRUE, col = "#fdc086", lwd = 2)
with(bay_labs, text(x, y, lab, srt = 45, pos = pos, cex = 0.75, offset = 0))
with(land_labs, text(x, y, lab))
dev.off()

## center of gravity -----------------------------------------------------------

base_map <- function(xlim = c(-59, -49), ylim = c(46.5, 53)) {
    par(mar = c(2.5, 2.5, 1, 1))
    plot(nafo["ZONE"], main = "", xlim = xlim, ylim = ylim, col = NA, axes = TRUE)
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "gray")
    plot(nafo["ZONE"], add = TRUE, col = "white", border = "grey50")
    text(nafo_centroids$X, nafo_centroids$Y, nafo_centroids$ZONE, cex = 0.75, col = "grey50")
    contour(bathy, add = TRUE, col = "lightgrey", levels = -c(200, 500, 1000, 2000))
    plot(blind_spot_ll, add = TRUE, col = "#EF3B2C", border = NA)
    plot(inshore_strata_ll, add = TRUE, col = "#FEE0D2", border = NA)
    plot(nafo["ZONE"], add = TRUE, col = NA, border = "grey50")
    box()
}


base_map_fr <- function(xlim = c(-59, -49), ylim = c(46.5, 53)) {
    par(mar = c(2.5, 2.5, 1, 1))
    plot(nafo["ZONE"], main = "", xlim = xlim, ylim = ylim, col = NA, axes = FALSE)
    degAxis(2)
    degAxis(1, labels = paste0(seq(60, 50, -2), "°O"))
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "gray")
    plot(nafo["ZONE"], add = TRUE, col = "white", border = "grey50")
    text(nafo_centroids$X, nafo_centroids$Y, nafo_centroids$ZONE, cex = 0.75, col = "grey50")
    contour(bathy, add = TRUE, col = "grey50", levels = -c(200, 500, 1000, 2000))
    plot(blind_spot_ll, add = TRUE, col = "#EF3B2C", border = NA)
    plot(inshore_strata_ll, add = TRUE, col = "#FEE0D2", border = NA)
    plot(nafo["ZONE"], add = TRUE, col = NA, border = "grey50")
    box()
}

base_map_utm <- function(xlim = c(-2e+05, 7e+05), ylim = c(5200000, 6000000)) {
    par(mar = rep(1, 4))
    plot(nafo_utm["ZONE"], main = "", xlim = xlim, ylim = ylim, col = NA, axes = FALSE)
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "gray")
    plot(nafo_utm["ZONE"], add = TRUE, col = "white", border = "grey50")
    text(nafo_centroids_utm$X, nafo_centroids_utm$Y, nafo_centroids$ZONE, cex = 0.75, col = "grey50")
    plot(blind_spot, add = TRUE, col = "#EF3B2C", border = NA)
    plot(inshore_strata, add = TRUE, col = "#FEE0D2", border = NA)
    contour(bathy_utm, add = TRUE, col = "grey50", levels = -c(200, 500, 1000, 2000))
    plot(nafo_utm["ZONE"], add = TRUE, col = NA, border = "grey50")
    box()
}
base_map_utm()

convert <- function(mat) {
    mat <- st_as_sf(data.frame(mat), coords = c("Easting", "Northing"), crs = 32622)
    mat <- st_transform(mat, crs = 4326)
    data.frame(year = Year_Set, st_coordinates(mat))
}

## Function for drawing an ellipse, where the centroid = c(h, k) and the
## width and height = a and b.
ellipse <- function(h, k, a, b) {
    theta <- seq(0, 2*pi, length = 100)
    x = h + a * cos(theta)
    y = k + b * sin(theta)
    cbind(x, y)
}

nt <- Save$TmbData$n_t
sdrep <- Save$Opt[["SD"]]
vals <- names(sdrep$value)
ind <- vals %in% "mean_Z_cym"
est <- matrix(sdrep$value[ind], ncol = 2, dimnames = list(Year_Set, c("Easting", "Northing")))
sd <- matrix(sdrep$sd[ind], ncol = 2, dimnames = list(Year_Set, c("Easting", "Northing")))

## make ellipses
polys_utm <- lapply(rownames(est), function(yr) {
    xy <- ellipse(est[yr, "Easting"], est[yr, "Northing"], 2 * sd[yr, "Easting"], 2 * sd[yr, "Northing"])
    xy <- xy * 1000
    st_polygon(list(xy))
})
polys_utm <- st_sfc(polys_utm, crs = 32622)
polys <- st_transform(polys_utm, crs = 4326)

lwr <- est - 2 * sd
upr <- est + 2 * sd
est <- est * 1000
lwr <- lwr * 1000
upr <- upr * 1000
est_utm <- data.frame(year = Year_Set, est)


## convert estimates to lat lon
est <- convert(est)
lwr <- convert(lwr)
upr <- convert(upr)

jet_colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                       "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

#plot(bms ~ year, data = index, log = "y")

breaks <- cut(log(Index$Table$Estimate_metric_tons), nt)
cols <- colorRampPalette(rev(c("#E41A1C", "#377EB8")))(nt)[breaks]

# pdf("analysis/center_of_gravity_map.pdf", height = 8, width = 7)
# base_map(xlim = c(-58, -50), ylim = c(47, 54))
# #segments(x0 = lwr$X, x1 = upr$X, y0 = lwr$Y, y1 = upr$Y)
# #text(est$X, est$Y, substr(est$year, 3, 4))
# text(est$X, est$Y, est$year, col = cols, cex = 0.75)
# dev.off()

pdf("analysis/center_of_gravity_map_utm.pdf", height = 7, width = 7)
base_map_utm(xlim = c(-1.5e+05, 7e+05), ylim = c(5200000, 6000000))
# for (i in seq(nt)[-1]) {
#     arrows(x0 = est_utm$Easting[i - 1], x1 = est_utm$Easting[i],
#            y0 = est_utm$Northing[i - 1], y1 = est_utm$Northing[i], length = 0.1)
# }
lines(est_utm$Easting, est_utm$Northing)
text(est_utm$Easting, est_utm$Northing, est$year, col = cols)
dev.off()

#pdf("analysis/center_of_gravity_map.pdf", height = 7, width = 7)
png("analysis/center_of_gravity_map.png", units = "in", res = 600, height = 7, width = 7)
breaks <- cut(est$year, nt)
cols <- colorRampPalette(rev(c("#E41A1C", "#377EB8")))(nt)[breaks]
base_map(xlim = c(-59.5, -50), ylim = c(46.5, 53))
x <- est$X
y <- est$Y
#segments(x[-length(x)], y[-length(y)], x[-1L], y[-1L], col = cols[-1])
arrows(x[-length(x)], y[-length(y)], x[-1L], y[-1L], col = cols[-1], length = 0.05)
text(est$X[1], est$Y[1], est$year[1], pos = 4, col = cols[1 + 1], offset = 0.2)
text(est$X[14], est$Y[14], est$year[14], pos = 2, col = cols[14 + 1], offset = 0.2) # 2008
text(est$X[17], est$Y[17], est$year[17], pos = 3, col = cols[17 + 1], offset = 0.2) # 2011
text(est$X[22], est$Y[22], est$year[22], pos = 4, col = cols[22 + 1], offset = 0.2) # 2016
dev.off()


#pdf("analysis/center_of_gravity_map.pdf", height = 7, width = 7)
png("analysis/center_of_gravity_map_fr.png", units = "in", res = 600, height = 7, width = 7)
breaks <- cut(est$year, nt)
cols <- colorRampPalette(rev(c("#E41A1C", "#377EB8")))(nt)[breaks]
base_map_fr(xlim = c(-59.5, -50), ylim = c(46.5, 53))
x <- est$X
y <- est$Y
#segments(x[-length(x)], y[-length(y)], x[-1L], y[-1L], col = cols[-1])
arrows(x[-length(x)], y[-length(y)], x[-1L], y[-1L], col = cols[-1], length = 0.05)
text(est$X[1], est$Y[1], est$year[1], pos = 4, col = cols[1 + 1], offset = 0.2)
text(est$X[14], est$Y[14], est$year[14], pos = 2, col = cols[14 + 1], offset = 0.2) # 2008
text(est$X[17], est$Y[17], est$year[17], pos = 3, col = cols[17 + 1], offset = 0.2) # 2011
text(est$X[22], est$Y[22], est$year[22], pos = 4, col = cols[22 + 1], offset = 0.2) # 2016
dev.off()

plot(est$year, est$X, ylim = range(est$X, lwr$X, upr$X), type = "n", ylab = "Longitude")
polygon(c(est$year, rev(est$year)), c(lwr$X, rev(upr$X)), col = "#FEE0D2", border = NA)
lines(est$year, est$X, col = "#EF3B2C", lwd = 2)

plot(est$year, est$Y, ylim = range(est$Y, lwr$Y, upr$Y), type = "n", ylab = "Longitude")
polygon(c(est$year, rev(est$year)), c(lwr$Y, rev(upr$Y)), col = "#FEE0D2", border = NA)
lines(est$year, est$Y, col = "#EF3B2C", lwd = 2)






#pdf("analysis/center_of_gravity_map_v2.pdf", height = 7, width = 7)
png("analysis/center_of_gravity_map_v2.png", units = "in", res = 600, height = 7, width = 7)
breaks <- cut(est$year, nt)
cols <- colorRampPalette(rev(c("#E41A1C", "#377EB8")))(nt)[breaks]
base_map(xlim = c(-59.5, -50), ylim = c(46.5, 53))
x <- est$X
y <- est$Y
plot(st_union(polys), add = TRUE, lty = 3)
arrows(x[-length(x)], y[-length(y)], x[-1L], y[-1L], col = "black", length = 0.05)
text(est$X[1], est$Y[1], est$year[1], pos = 4, offset = 0.2)
text(est$X[14], est$Y[14], est$year[14], pos = 2, offset = 0.2) # 2008
text(est$X[17], est$Y[17], est$year[17], pos = 3, offset = 0.2) # 2011
text(est$X[22], est$Y[22], est$year[22], pos = 4, offset = 0.2) # 2016
dev.off()


