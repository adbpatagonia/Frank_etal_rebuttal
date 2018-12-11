
library(sf)     
library(raster)
library(ggplot2)

load("data/Fig1_map_data.RData")
load("../CenterOfGravity/cgi_data.RData")
source("R/shadow_text.R")

xlim <- c(-60, -48)
ylim <- c(46.5, 54.5)

elipses <- cgi_data$cap_ellipse
xy <- st_coordinates(cgi_data$cap_center)
x <- xy[, "X"]
y <- xy[, "Y"]
z <- cgi_data$cap_center$year

cgi_map <- function() {
    
    par(mar = c(2.5, 2.5, 1, 1))
    plot(strata[, "STRAT"], main = "",
         col = ifelse(strata$Strat_Type == "InshoreNew", "#fddbc7", "NA"), 
         border = "white", 
         lwd = 0.4, xlim = xlim, ylim = ylim, axes = TRUE)
    plot(blind_spot_ll, col = "#f4a582", add = TRUE, border = "white", lwd = 0.4)
    contour(bathy, add = TRUE, col = "lightgrey", levels = -c(200, 500, 1000, 2000),
            lwd = 0.5)
    plot(nafo["ZONE"], add = TRUE, col = NA, border = "grey50",
         lwd = 0.5)
    plot(st_union(elipses), add = TRUE, lty = 3, xlim = xlim, ylim = ylim, axes = TRUE, 
         border = "black")
    plot(na_land["COUNTRY"], add = TRUE, col = "grey", border = "grey50", lwd = 0.5)
    arrows(x[-length(x)], y[-length(y)], x[-1L], y[-1L], col = cols[-1], length = 0.05)
    
    yr <- 1983
    ind <- which(z == yr)
    shadowtext(x[ind], y[ind], yr, col = cols[ind], pos = 4, offset = 0.2)
    
    yr <- 1987
    ind <- which(z == yr)
    shadowtext(x[ind], y[ind], yr, col = cols[ind], pos = 4, offset = 0.2)
    
    yr <- 1991
    ind <- which(z == yr)
    shadowtext(x[ind], y[ind], yr, col = cols[ind], pos = 4, offset = 0.2)
    
    yr <- 1996
    ind <- which(z == yr)
    shadowtext(x[ind], y[ind], yr, col = cols[ind], pos = 4, offset = 0.2)
    
    yr <- 2004
    ind <- which(z == yr)
    shadowtext(x[ind], y[ind], yr, col = cols[ind], pos = 3, offset = 0.2)
    
    yr <- 2011
    ind <- which(z == yr)
    shadowtext(x[ind], y[ind], yr, col = cols[ind], pos = 3, offset = 0.2)
    
    yr <- 2016
    ind <- which(z == yr)
    shadowtext(x[ind], y[ind], yr, col = cols[ind], pos = 4, offset = 0.2)
    
    box()
    
}

cols <- rev(viridis::inferno(length(z) + 10)) %>% tail(n = length(z))

png("output/Fig4_center_of_gravity_map.png", units = "in", res = 600, height = 7, width = 7)
cgi_map()
dev.off()

postscript("output/Fig4_center_of_gravity_map.eps", height = 7, width = 7)
cgi_map()
dev.off()


cols <- rev(viridis::viridis(length(z) + 10)) %>% tail(n = length(z))

png("output/Fig4_center_of_gravity_map-alternative.png", units = "in", res = 600, height = 7, width = 7)
cgi_map()
dev.off()

postscript("output/Fig4_center_of_gravity_map-alternative.eps", height = 7, width = 7)
cgi_map()
dev.off()

