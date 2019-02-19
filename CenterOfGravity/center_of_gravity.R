
library(data.table)
library(RGeostats)
library(sf)
library(plotly)

## Function for making an ellipse from SI.cgi output
## (from source code of SI.cgi)
cgi_to_ellipse <- function(cgi) {
    axes <- cgi$axes
    center <- cgi$center
    dx <- axes[2, 1] - axes[1, 1]
    dy <- axes[2, 2] - axes[1, 2]
    teta <- atan(dy/dx)
    a <- 0.5 * sqrt(dx^2 + dy^2)
    b <- 0.5 * sqrt((axes[4, 1] - axes[3, 1])^2 + 
                        (axes[4, 2] - axes[3, 2])^2)
    alpha <- seq(0, 360, 0.25)
    ca <- cosd(alpha)
    sa <- sind(alpha)
    rayon <- (a * b)/sqrt((b * ca)^2 + (a * sa)^2)
    x <- rayon * ca
    y <- rayon * sa
    xrot <- x * cos(teta) + y * sin(teta)
    yrot <- x * sin(teta) - y * cos(teta)
    x <- xrot + center[1]
    y <- yrot + center[2]
    xy <- projec.invert(x, y)
    cbind(x = xy$x, y = xy$y)
}

## Read data and simplify names
cap <- fread("CenterOfGravity/Fall_MS_catches_of_capelin_All_1978-2017_dup_removed.csv")
names(cap) <- tolower(names(cap))
setnames(cap, c("surveyyear", "commonname", "std_number", "std_weight"), 
              c(      "year",       "name",     "number",     "weight"))
cap <- st_as_sf(cap, coords = c("long_dec", "lat_dec"), crs = 4326)

## Load map data
strata <- read_sf("CenterOfGravity/DFO_NL_survey_strat/2HJ3KLNOP_Strata_Polygons_WGS84.shp", 
                  layer = "2HJ3KLNOP_Strata_Polygons_WGS84") %>% 
    st_transform(crs = 4326)
stock_strata <- strata[strata$DIV %in% c("2J", "3K", "3L"), ]

## Make a survey area polygon
## (some polygon lines do not overlap, so add buffer to make union clean)
survey_area <- st_buffer(stock_strata, dist = 0.11) %>% st_union()
survey_area <- st_sf(data.frame(survey_area = "2J3KL", geom = survey_area))
plot(survey_area)
bbox <- st_bbox(survey_area)

## Convert to RGeostats polygon
xy <- st_coordinates(survey_area)
cgi_poly <- polygon.create(data.frame(xy[, c("X", "Y")]))
plot(cgi_poly, asp = 1)


## Make containers for SI.cgi output
years <- 1983:2016
cap_ellipse <- survey_ellipse <- vector("list", length(years))
cap_center <- survey_center <- data.frame(year = years, x = NA, y = NA)

## Loop through years and calculate cgi
for (i in seq_along(years)) {
    
    ## Subset capelin data
    sub_cap <- cap[cap$year == years[i] & cap$div %in% c("2J", "3K", "3L"), ]
    xy <- st_coordinates(sub_cap)
    cgi_dat <- db.create(rank = seq(nrow(sub_cap)), 
                         x1 = xy[, "X"], x2 = xy[, "Y"], 
                         z1 = sub_cap$number)
    projec.define(projection = "mean", db = cgi_dat)
    
    ## Add influence surface
    cgi_dat <- infl(cgi_dat, nodes = c(500, 500), 
                    origin = c(bbox["xmin"], bbox["ymin"]), 
                    extend = c(bbox["xmax"] - bbox["xmin"], bbox["ymax"] - bbox["ymin"]),
                    dmax = 100, polygon = cgi_poly, plot = TRUE, asp = 1, title = years[i]) 
    
    ## Start plot
    plot(cgi_dat, asp = 1, title = years[i])
    plot(cgi_poly, col = 8, add = TRUE)
    plot(db.add(cgi_dat, S = 1), add = TRUE, col = "lightgrey", pch = 20)
    
    ## Calculate cgi
    cap_cgi <- SI.cgi(cgi_dat, flag.plot = TRUE, flag.inertia = TRUE, col = 2)
    survey_cgi <- SI.cgi(db.add(cgi_dat, S = z1 >= 0), flag.plot = TRUE, flag.inertia = TRUE, col = 1)
    
    ## Extract data
    cap_ellipse[[i]] <- cgi_to_ellipse(cap_cgi)
    survey_ellipse[[i]] <- cgi_to_ellipse(survey_cgi)
    center <- cap_cgi$center
    cap_center[cap_center$year == years[i], c("x", "y")] <- projec.invert(center[1], center[2])
    center <- survey_cgi$center
    survey_center[survey_center$year == years[i], c("x", "y")] <- projec.invert(center[1], center[2])
    
    print(i)
    
}


## Convert ellipse coordinates into an sf object
poly_list <- lapply(cap_ellipse, function(x) st_polygon(list(x)))
polys <- st_sfc(poly_list, crs = 4326)
cap_ellipse <- st_sf(data.frame(year = years, geom = polys))
poly_list <- lapply(survey_ellipse, function(x) st_polygon(list(x)))
polys <- st_sfc(poly_list, crs = 4326)
survey_ellipse <- st_sf(data.frame(year = years, geom = polys))

## Convert centers to sf points
cap_center <- st_as_sf(cap_center, coords = c("x", "y"), crs = 4326)
survey_center <- st_as_sf(survey_center, coords = c("x", "y"), crs = 4326)

## Export data
cgi_data <- list(cap = cap, stock_strata = stock_strata, survey_area = survey_area,
                 cap_ellipse = cap_ellipse, survey_ellipse = survey_ellipse,
                 cap_center = cap_center, survey_center = survey_center)
save(cgi_data, file = "CenterOfGravity/cgi_data.RData")



