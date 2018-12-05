
source("figures/map/map_data.R")
load("CenterOfGravity/cgi_data.RData")

base_map <- function(xlim = c(-60, -48), ylim = c(46, 54)) {
    par(mar = c(2.5, 2.5, 1, 1))
    plot(na_land["COUNTRY"], main = "", xlim = xlim, ylim = ylim, 
         col = "grey", border = "grey50", axes = TRUE, lwd = 0.1)
    contour(bathy, add = TRUE, col = "lightgrey", levels = -c(200, 500, 1000, 2000),
            lwd = 0.5)
    plot(blind_spot_ll, add = TRUE, col = "#EF3B2C", border = NA)
    plot(inshore_strata_ll, add = TRUE, col = "#FEE0D2", border = NA)
    plot(nafo["ZONE"], add = TRUE, col = NA, border = "grey50",
         lwd = 0.5)
    box()
}



xlim <- c(-60, -48)
ylim <- c(46.5, 54.5)

elipses <- cgi_data$cap_ellipse
xy <- st_coordinates(cgi_data$cap_center)
x <- xy[, "X"]
y <- xy[, "Y"]
z <- cgi_data$cap_center$year
cols <- rev(viridis::inferno(length(z) + 10)) %>% tail(n = length(z))
# cols <- rep("black", length(z))
# cols <- colorRampPalette(c("black", "grey40"))(length(z))

png("figures/center_of_gravity_map_v3.png", units = "in", res = 600, height = 7, width = 7)
par(mar = c(2.5, 2.5, 1, 1))
plot(strata[, "STRAT"], main = "",
     col = ifelse(strata$Strat_Type == "InshoreNew", "#fddbc7", "#d1e5f0"), 
     border = "white", 
     lwd = 0.4, xlim = xlim, ylim = ylim, axes = TRUE)
plot(blind_spot_ll, col = "#f4a582", add = TRUE, border = "white", lwd = 0.4)
# contour(bathy, add = TRUE, col = "lightgrey", levels = -c(200, 500, 1000, 2000),
#         lwd = 0.5)
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
dev.off()


