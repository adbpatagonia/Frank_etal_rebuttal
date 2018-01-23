setwd('D:/Buren_files/MEGA/papersAle/Cod_Condition')
library(ggplot2)


codst <- read.csv('cod+turbot_fall_2J3KL_stomach_content_weights.csv',header=T)

codst <- codst[c(3:5,7:9)]
names(codst) <- c('sp','syear','length','depth','nafo','stomweight')

codst <- subset(codst,sp=='Cod')
codst <- subset(codst,nafo=='2J'| nafo=='3K' | nafo=='3L') 
codst <- subset(codst,syear>1980 & syear<2013) 
codst <- subset(codst,length > 29 & length < 56)
#codst <- subset(codst,Month==1 | Month>9) 

#codst <- subset(codst,stomweight>0)
#codst$stomweight <- with(codst, stomweight/length^3)



#codst$syear <- codst$syear+1

## create data frames to store the information on density distribution and measures of central tendency for each year and nafo div
conddens <- data.frame('x'=NA,'y'=NA,'year'=NA,'nafo'=NA)
centraltendency <- data.frame('meank'=NA,'mediank'=NA,'year'=NA,'nafo'=NA)

## loop through years and nafo divisions to obtain density distributions, means and medians of condition
for (i in min(unique(codst$syear)):max(unique(codst$syear))){
   for (j in unique(codst$nafo)){
      dat <- subset(codst, syear==i & nafo==j)
      if (nrow(dat)>2){
         yy <- density(na.omit(dat$stomweight))
         yy <- data.frame('x'=yy$x,'y'=yy$y)
         yy$year <- rep(i,nrow(yy))
         yy$nafo <- rep(j,nrow(yy))
         conddens <- rbind(conddens,yy)
         ct <- data.frame('meank'=mean(dat$stomweight),'mediank'=  median(dat$stomweight),'year'=i,'nafo'=j)
         centraltendency <- rbind(centraltendency,ct)
      }
   }
}



## obtain the max density per year and nafo div and merge it with density distribution
maxy <- aggregate(conddens$y, by=list(conddens$nafo,conddens$year),FUN=max)
names(maxy) <- c ('nafo','year','maxy')
conddens <- merge(conddens, maxy)

## merge data frames with information on density distributions and centraltendency
#conddens <- merge(conddens,centraltendency)


## sum year to the max density per year and nafo div so that each histogram is ploted along the x-axis in the right year.
## The max density each curve takes is slightly shifted down so that distribution in year t does not overlap with distribution in year t+1
conddens$yy <- with(conddens,year+(y/(maxy+maxy*.02)))

## obtain the range of conditions that encompass 95% of the data per year and nfo div and merge it with density distribution data
bounds <- aggregate(conddens$x, by=list(conddens$nafo,conddens$year),FUN= function(x) quantile(x, probs = c(0.025, 0.975)))
bounds <- data.frame(bounds[,1:2],bounds[,3][,1],bounds[,3][,2])
names(bounds) <- c ('nafo','year','lb','ub')
conddens <- merge(conddens, bounds)


conddens <- rbind(conddens,data.frame(year=rep(1997,3),nafo=(c('2J','3K','3L')),x=rep(0,3),y=rep(0,3),maxy=rep(0,3),yy=rep(1997,3),lb=rep(0,3),ub=rep(0,3)))
conddens <- rbind(conddens,data.frame(year=rep(2007,3),nafo=(c('2J','3K','3L')),x=rep(0,3),y=rep(0,3),maxy=rep(0,3),yy=rep(2008,3),lb=rep(0,3),ub=rep(0,3)))
#conddens <- rbind(conddens,data.frame(year=rep(2008,1),nafo=(c('3K')),x=rep(0,1),y=rep(0,1),maxy=rep(0,1),yy=rep(2008.0005,1),lb=rep(0,1),ub=rep(0,1)))
conddens[which(conddens$year==2008 & conddens$nafo=='3K' & conddens$yy==2008),'yy'] <-  2008.0005
conddens <- conddens[order(conddens$yy),]

#conddens <- rbind(conddens,data.frame(expand.grid(year=1998:2006,nafo=c('2J','3K','3L')),x=rep(NA,nrow(expand.grid(year=1998:2006,nafo=c('2J','3K','3L')))),y=rep(NA,nrow(expand.grid(year=1998:2006,nafo=c('2J','3K','3L')))),maxy=rep(NA,nrow(expand.grid(year=1998:2006,nafo=c('2J','3K','3L')))),meank=rep(0,nrow(expand.grid(year=1998:2006,nafo=c('2J','3K','3L')))),mediank=rep(NA,nrow(expand.grid(year=1998:2006,nafo=c('2J','3K','3L')))),yy=rep(NA,nrow(expand.grid(year=1998:2006,nafo=c('2J','3K','3L')))),lb=rep(NA,nrow(expand.grid(year=1998:2006,nafo=c('2J','3K','3L')))),ub=rep(NA,nrow(expand.grid(year=1998:2006,nafo=c('2J','3K','3L'))))))
#

#conddens[is.na(conddens$x),'yy'] <- conddens[is.na(conddens$x),'year'] 
#conddens[is.na(conddens$x),'x'] <- 0 

## Start ggplot

 ## define working dataset, x and y variables

 p <- ggplot(conddens, aes(yy, x))
 ## this draws the histograms
 p <- p + geom_line()
 ## Plot by nafo Div
 p <- p + facet_grid(nafo ~ ., scales="free_y")
 p <- p + facet_grid(nafo ~ .) 
 ## Add x and y axis labels
 p <- p + xlab("Year") + ylab('Weight of stomach contents (g)')
 ## Add the grey ribbons that encompass 95% of data. Argument alpha controls the amount of transparency
 p <- p + geom_ribbon(data = conddens,
                       aes(x = yy,
                           ymax = ub,
                           ymin = lb),
                           alpha = 0.15)
 ## Add the median per year and nafo div
# p <- p + geom_line(data=conddens,aes(x=yy,y=mediank),colour='red',lwd=1)
 ## the next 3 lines plot Lambert and Dutil's benchmark values. I commented them  out
# hline.data <- data.frame(nafo = rep(c('2J','3K','3L'),each=2),z = rep(c(0.7,0.85),3))
# colores <-  rep(c('darkgreen','red'),3)
# p <- p + geom_hline(data=hline.data, aes(yintercept = z,colour=colores),linetype='longdash')

 ## Aesthetic options
 p <- p  + theme(axis.text.x = element_text(angle = 90,  vjust = 0.5, hjust=0.5))  +  theme(legend.position="none")
 p <- p + scale_x_continuous(breaks=seq(from=1978,to=2016,by=2))
# p <- p + scale_y_continuous(breaks=seq(from=0.3,to=1.5,by=0.6))
 p <- p + theme_bw()
 ##print plot
 print(p)
 

pdf(file='cod_stom_weights_MKA.pdf',width=9.5,height=6.5, family = "sans", bg = "white",   pointsize = 8)
   print(p)
dev.off() 

jpeg(file='Cod_stom_weights.jpg',width=9.5,height=6.5,units='in',res=600, family = "sans", bg = "white",   pointsize = 8)
   print(p)
dev.off() 


centraltendency <- rbind(centraltendency,data.frame(meank=rep(NA,nrow(expand.grid(year=1998:2006,nafo=c('2J','3K','3L')))),mediank=rep(NA,nrow(expand.grid(year=1998:2006,nafo=c('2J','3K','3L')))),expand.grid(year=1998:2006,nafo=c('2J','3K','3L'))))
centraltendency <- centraltendency[2:nrow(centraltendency),]


aggregate(codst$spcode,by=list(codst$syear,codst$nafo),FUN=length)

 q <- ggplot((centraltendency), aes(year, meank),colour='blue',lwd=.9)
 ## this draws the histograms
 q <- q + geom_line()
 ## Plot by nafo Div
 q <- q + facet_grid(nafo ~ ., scales="free_y")
# q <- q + facet_grid(nafo ~ .) 
 ## Add x and y axis labels
 q <- q + xlab("Year") + ylab('Weight of stomach contents (g)')
 q <- q + geom_line(data=(centraltendency),aes(x=year,y=meank),colour='blue',lwd=.9)
 q <- q + geom_line(data=(centraltendency),aes(x=year,y=mediank),colour='red',lwd=.9)
  q <- q + scale_x_continuous(breaks=seq(from=1978,to=2006,by=2))
# q <- q + scale_y_continuous(breaks=seq(from=0.3,to=1.5,by=0.6))
 q <- q + theme_bw()
 ##print plot
 
 pdf(file='cod_stom_meanweights.pdf',width=9.5,height=6.5, family = "sans", bg = "white",   pointsize = 8)
 print(q)
dev.off()


library(doBy)
library(reshape2)
iqr <- summaryBy(stomweight~syear+nafo, data=codst, FUN=quantile, na.rm=TRUE)#, use="pair")
iqr <- iqr[,c(1,2,4,6)]

iqr <- melt(iqr, id=c("syear","nafo"))




 p <- ggplot(iqr, aes(syear, value))
 ## this draws the histograms
 p <- p + geom_point()
 ## Plot by nafo Div
 p <- p + facet_grid(nafo ~ ., scales="free_y")
 #p <- p + facet_grid(nafo ~ .) 
 ## Add x and y axis labels
 p <- p + xlab("Year") + ylab('Weight of stomach contents (g)')
 ## Add the grey ribbons that encompass 95% of data. Argument alpha controls the amount of transparency
# p <- p + geom_ribbon(data = conddens,
#                       aes(x = yy,
#                           ymax = ub,
#                           ymin = lb),
#                           alpha = 0.15)
 ## Add the median per year and nafo div
 p <- p + geom_line(data=conddens,aes(x=yy,y=mediank),colour='red',lwd=1)
 ## the next 3 lines plot Lambert and Dutil's benchmark values. I commented them  out
# hline.data <- data.frame(nafo = rep(c('2J','3K','3L'),each=2),z = rep(c(0.7,0.85),3))
# colores <-  rep(c('darkgreen','red'),3)
# p <- p + geom_hline(data=hline.data, aes(yintercept = z,colour=colores),linetype='longdash')

 ## Aesthetic options
 p <- p  + theme(axis.text.x = element_text(angle = 90,  vjust = 0.5, hjust=0.5))  +  theme(legend.position="none")
 p <- p + scale_x_continuous(breaks=seq(from=1978,to=2006,by=2))
# p <- p + scale_y_continuous(breaks=seq(from=0.3,to=1.5,by=0.6))
 p <- p + theme_bw()
 ##print plot
 print(p)