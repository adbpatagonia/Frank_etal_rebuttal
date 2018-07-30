## load libraries
require(GISTools)
require(maps)
library(raster)
library(rgdal)
library(spatial)
require(sfsmisc)
require(extrafont)
## import and project map data

#setwd("D:/Buren_files/MEGA/papersAle/Frank_etal_2016_rebuttal/figures/map/")

na <- readOGR("D:/Buren_files/MEGA/papersAle/Frank_etal_2016_rebuttal/figures/map/north_america/bound_p.shp", layer = "bound_p")
ecan <- na[na$NAME == "Newfoundland and Labrador / Terre-Neuve-et-Labrador" |
          na$NAME == "Nova Scotia / Nouvelle-Écosse" |
          na$NAME == "Prince Edward Island / Île-du-Prince-Édouard" |
          na$NAME == "Saint-Pierre et Miquelon" |
          na$NAME == "New Brunswick / Nouveau-Brunswick" |
          na$NAME == "Quebec / Québec" |
          na$NAME == "Nunavut" |
          na$NAME == "Maine" |
          na$NAME == "New Hampshire" |
          na$NAME == "Massachusetts" |
          na$NAME == "Kalaallit Nunaat",]
ecan.nad <- spTransform(ecan, CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) # projection with long lat
nafo <-  readOGR("D:/Buren_files/MEGA/papersAle/Frank_etal_2016_rebuttal/figures/map/NAFO_SHP/NAFO_Divns.shp", layer = "NAFO_Divns")
nafo.nad <- spTransform(nafo, CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) # projection with long lat
 bathy.proj.nad <- raster("D:/Buren_files/MEGA/papersAle/Frank_etal_2016_rebuttal/figures/map/bathy/nl_nad.grd", values = TRUE)
 bathy.gull.nad <- crop(bathy.proj.nad, extent(-54, -50, 46, 49)) # projection with long lat

 NAFCtransects <- readOGR("D:/Buren_files/MEGA/papersAle/Frank_etal_2016_rebuttal/figures/map/1990sAcoustictransects/NAFC_Fall_Acoustic_survey.shp", 
                          layer = "NAFC_Fall_Acoustic_survey")
 USSRtransects <- readOGR("D:/Buren_files/MEGA/papersAle/Frank_etal_2016_rebuttal/figures/map/1990sAcoustictransects/USSR_Fall_Acoustic_survey.shp", 
                          layer = "USSR_Fall_Acoustic_survey")


  latitudelabel <- expression(paste('Latitude (',degree,'N)'))
  longitudelabel <- expression(paste('Longitude (',degree,'W)'))

## map

 loadfonts(device="postscript")
# cairo_ps(filename = "map.ps", 
#          width = 8.5, height = 8.5, pointsize = 12,
#          onefile = FALSE, family = "Arial", bg = "white",
#          antialias = c("default"))
par(mar=c(3,3,2,2))
plot(nafo.nad[c(37,31,3:10,13:14,16:19,20,22,27:30,33,34,35,36,39:49,51,53,57,58,60:69,71,72,74,75),],  xlim=c(-60,-47), ylim=c(46,56), bg="white")
  plot(ecan.nad, col="grey",  xlim=c(-60,-47), ylim=c(46,56), bg="white",add=T)
contour(bathy.proj.nad, levels=c(-100,-200,-400,-1000,-2000), add=TRUE, col="grey")
degAxis(1, seq(-60,-44,6), mgp=c(3,0.5,0))
  mtext(longitudelabel,1,line=2,cex=1)
degAxis(2, seq(46,56,5))  
  mtext(latitudelabel,2,line=2,cex=1)
#   lines(c(-55.65,-52.50),c(53.23,55.07),lwd=5,col='red')
#   lines(c(-52.97,-49),c(48.73,50),lwd=5,col='red')
#   text(-55,49.9,"Notre  Dame /nBay",srt=-19,cex=0.9)
#   text(-52.8,47.8,"CB",srt=55,cex=1.2)
#   text(-53.5,48,"Trinity Bay",srt=55,cex=0.9)
#   text(-52.3,47.6,"SJ",cex=1.2)
   text(-48,53.5,"2J",cex=1.5)
   text(-48,50.5,"3K",cex=1.5)
   text(-48,47,"3L",cex=1.5)
 #  text(-54.3,54.3,"Seal Island Line",srt=43,cex=0.9)
#   text(-51,49.7,"Bonavista Line",srt=28,cex=0.9)
   points(  -53.7818,	47.6358,pch=16,cex=1,col='black')
   points(-53.181375964,47.683192975,pch=16,cex=1,col='black')
   points(-52.712017,47.5612626,pch=16,col='black',cex=1)  
   text(-52.5,47.5612626,"St John's",srt=0,cex=0.9,adj=0)   

#arrows(x0=-53.181375964,y0=47.683192975,x1=-52.55,y1=48, length = 0.1, angle = 20, code = 3,col='black',lwd=1.5)
#arrow.plot(a1=matrix(-53.181375964,47.683192975), a2=matrix(-52.55,48))
 #    points(-52.583611,47.534722,pch=18,col='black',cex=1.5)   # stn 27
#lines(x=c(-53,-52.55),y=c(47.75,48),lwd=2)
#plot(arrow(angle = 90, length = unit(0.25, "inches"),ends = "last", type = "closed"))
p.arrows(x1=-53,x2=-52.6,y1=47.75,y2=48, fill = "black",size=0.5,lwd=1.85)
text(-52.5, 48, "Bryant's Cove",cex=0.9,adj=0)
text(-54,46.2, "Bellevue Beach",cex=0.9,adj=0) 
p.arrows(x1=-53.7818,x2=-53.7818,y1=47.55,y2=46.4, fill = "black",size=0.5,lwd=1.85)
#map.scale(x=-52, y=56,relwidth=0.1)
#map.scale(x=-52, y=56, ratio=FALSE, relwidth=0.1)
#north.arrow(xb=-46.5, yb=56, len=0.1, lab="N",col="black")
box()
#dev.off()


lines(NAFCtransects, col = 'darkgreen')
lines(USSRtransects, col = 'blue')
