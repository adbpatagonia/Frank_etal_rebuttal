
library(sf)     
library(raster)
library(ggplot2)

load("data/Fig1_map_data.RData")
source("R/shadow_text.R")

nafo_map <- function(xlim = c(-62, -44), ylim = c(42.7, 55)) {
    par(mar = c(2.5, 2.5, 1, 1))
    plot(na_land["COUNTRY"], main = "", xlim = xlim, ylim = ylim, 
         col = "grey", border = "grey50", axes = TRUE, lwd = 0.1)
    contour(bathy, add = TRUE, col = "lightgrey", levels = -c(200, 500, 1000, 2000),
            lwd = 0.5)
    plot(nafo["ZONE"], add = TRUE, col = NA, border = "grey50",
         lwd = 0.5)
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
shelf_labs <- data.frame(x = c(-50.96442, -44.81966, -62.00778),
                         y = c(45.85559, 47.04317, 43.93283),
                         lab = c("Grand\nBank", "Flemish\nCap", "Scotian\nShelf"))

# png("analysis/nafo_map.png", units = "in", res = 600, height = 7, width = 6)
# nafo_map()
# with(bay_labs, text(x, y, lab, srt = 45, pos = pos, cex = 0.75, offset = 0))
# with(land_labs, text(x, y, lab))
# with(shelf_labs, text(x, y, lab))
# dev.off()


## manually clean up spring transects
nafo_map(xlim = range(-raw_sp_transects$Longitude, na.rm = TRUE),
         ylim = range(raw_sp_transects$Latitude, na.rm = TRUE))
with(raw_sp_transects, lines(-Longitude, Latitude))
# xy <- locator()
sp_transects <- cbind(x = c(-53.25851, -50.19064, -50.08107, -52.94077, -53.11607, 
                            -50.11394, -50.01533, -53.32425, -52.73259, -49.73046, 
                            -49.04019, -52.68876, -52.61207, -47.21042, -47.17755,
                            -52.59015, -52.67781, -47.27616, -47.28712, -52.73259,
                            -52.78737, -47.27616, -47.45147, -52.78737),
                      y = c(49.91514, 49.90782, 49.57105, 49.56373, 49.24161, 49.24161,
                            48.90484, 48.91216, 48.56807, 48.57539, 48.23862, 48.22398, 
                            47.90918, 47.89453, 47.56509, 47.56509, 47.23564, 47.22832,
                            46.89887, 46.89887, 46.57674, 46.56210, 46.23265, 46.23998))
id <- rep(seq(24 / 2), each = 2)
sp_transects <- lapply(seq(24 / 2), function(i) {
    st_linestring(sp_transects[id %in% i, ])
})
sp_transects <- st_sfc(sp_transects, crs = 4269)

tb_transects <- cbind(x = c(-53.00651, -52.67781, -52.79833, -53.0832, -53.24755, 
                            -53.00651, -53.0832, -53.35712, -53.44478, -53.26947, 
                            -53.36808, -53.52147, -53.59817, -53.43382, -53.52147, 
                            -53.65295, -53.77348, -53.58721, -53.1599, -53.09416, 
                            -53.00651, -53.11607, -53.0832, -52.87503, -52.84216, 
                            -53.00651, -52.92981, -52.80929, -52.64494, -52.86407),
                      y = c(48.46558, 48.20202, 48.15809, 48.39236, 48.34112, 48.15809, 
                            48.06292, 48.28255, 48.1947, 48.04828, 47.9531, 48.07756, 
                            47.99703, 47.87989, 47.75543, 47.89453, 47.76275, 47.62365, 
                            47.55776, 47.52116, 47.56509, 47.66026, 47.79936, 47.65294, 
                            47.76275, 47.87257, 47.97506, 47.84329, 47.90185, 48.06292))
id <- rep(seq(30 / 2), each = 2)
tb_transects <- lapply(seq(30 / 2), function(i) {
    st_linestring(tb_transects[id %in% i, ])
})
tb_transects <- st_sfc(tb_transects, crs = 4269)

nafo_map(xlim = range(-raw_sp_transects$Longitude, na.rm = TRUE),
         ylim = range(raw_sp_transects$Latitude, na.rm = TRUE))
plot(sp_transects, add = TRUE, col = "green")
plot(tb_transects[1:9], add = TRUE, col = "red")


survey_map <- function() {
    nafo_map()
    plot(fall_transects, add = TRUE, col = cols[3], lwd = 2, lty = 1)
    plot(ussr_transects, add = TRUE, col = cols[5], lwd = 2, lty = 1)
    plot(sp_transects, add = TRUE, col = cols[2], lwd = 2, lty = 1)
    plot(spring_ussr_transects, add = TRUE, col = cols[4], lwd = 2, lty = 1)
    plot(tb_transects[1:9], add = TRUE, col = cols[1], lwd = 2, lty = 1)
    with(bay_labs, shadowtext(x, y, lab, srt = 45, pos = pos, cex = 0.75, offset = 0))
    with(land_labs, shadowtext(x, y, lab))
    with(shelf_labs, shadowtext(x, y, lab))
    shadowtext(nafo_centroids$X, nafo_centroids$Y, nafo_centroids$ZONE, cex = 0.75, col = "black")
    legend("topright", legend = c("2J3K Fall Canada", '2J3K Fall USSR',
                                  "3L Spring Canada", "3LNO Spring USSR",
                                  'Trinity Bay (3L)'),
           lty = 1, lwd = 2, col = cols[c(3, 5, 2, 4, 1)], seg.len = 2)
    box()
}

## make survey area map

cols <- head(viridis::inferno(6), 5)

png("output/Fig1_survey_map.png", units = "in", res = 600, height = 7, width = 7.1)
survey_map()
dev.off()

postscript("output/Fig1_survey_map.eps", height = 7, width = 7.1)
survey_map()
dev.off()


cols <- head(viridis::viridis(6), 5)

png("output/Fig1_survey_map-alternative.png", units = "in", res = 600, height = 7, width = 7.1)
survey_map()
dev.off()

postscript("output/Fig1_survey_map-alternative.eps", height = 7, width = 7.1)
survey_map()
dev.off()
