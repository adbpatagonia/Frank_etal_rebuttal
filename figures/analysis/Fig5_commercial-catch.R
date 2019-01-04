# A simplified script for figure for Hannah...

library(plyr)
library(tidyr)
library(ggplot2)
library(gtable)
library(grid)

# load data sets...
# first contains older data
inshore <- read.csv("data/Fig5_inshore-commercial.csv",head=TRUE,stringsAsFactors = FALSE)
# more recent catches
recent.years <- read.csv(file="data/Fig5_corrected length and weigh freqs by year nafo and age - no month lw.txt",header=TRUE,stringsAsFactors = FALSE,skip=1)


# format data
inshore$method <- as.factor(inshore$method)
inshore$area <- as.factor(inshore$area)
inshore$division <- as.factor(inshore$division)
inshore$sample.type <- as.factor(inshore$sample.type)
inshore$sex <- as.factor(inshore$sex)
inshore$measurement <- as.factor(inshore$measurement)

# clean up the inshore data set - drop lengths and weights, focus on "total" rather than males and females for sex and drop division
inshore <- inshore[which(inshore$measurement=="numbers" & inshore$sex=="total"),colnames(inshore)[which(!(colnames(inshore) %in% "division"))]]

# drop more columns...
inshore <- inshore[,c("year","method","area","age.1","age.2","age.3","age.4","age.5","age.6")]

# there are observation in 1993, 1994, and 1997 were data is across 3K/3L - these years are going to be duplicated and included as 3K and 3L
weird.data <- inshore[which(inshore$area=="3K/3L"),]
inshore.drop.weird <- inshore[which(inshore$area!="3K/3L"),]
weird.data.3L <- weird.data
weird.data.3L$area <- "3L"
weird.data.3K <- weird.data
weird.data.3K$area <- "3K"

inshore.corrected <- rbind(rbind(inshore.drop.weird,weird.data.3L),weird.data.3K)

# shift to long form...
inshore.long <- gather(inshore.corrected,key="age.grp",value="number",-year,-method,-area)

# drop method = fishery as this is simply the sum of the catches across fisheries
inshore.long <- inshore.long[which(inshore.long$method!="fishery"),]

# create proportions for plot...
inshore.count.age <- ddply(inshore.long,.(area,year,age.grp),summarise,count=sum(number,na.rm=TRUE))
inshore.count.total <- ddply(inshore.long,.(area,year),summarise,total=sum(number,na.rm=TRUE))
inshore.count.merge <- merge(inshore.count.age,inshore.count.total,all.x=TRUE)
inshore.count.merge$prop <- inshore.count.merge$count/inshore.count.merge$total

# clean up age for plotting...
inshore.count.merge$age.grp <- sub("a(ge)\\.([0-9])","A\\1 \\2",inshore.count.merge$age.grp)



# clean up recent data...
recent.years <- recent.years[,c("NAFO.division","Year","Sex","Age","N..millions.")]
colnames(recent.years) <- c("area","year","sex","age","number")
recent.years$age.grp <- paste("Age ",recent.years$age,sep="")

# get proportions for graph...
recent.count.age <- ddply(recent.years,.(area,year,age.grp),summarise,count=sum(number,na.rm=TRUE)) 
recent.count.total <- ddply(recent.years,.(area,year),summarise,total=sum(number,na.rm=TRUE))
recent.count.merge <- merge(recent.count.age,recent.count.total,all.x=TRUE)
recent.count.merge$prop <- recent.count.merge$count/recent.count.merge$total



# join the older inshore data with the newer recent data...
# drop years from inshore that are present in recent...
inshore.count.purge <- inshore.count.merge[which(!inshore.count.merge$year %in% unique(recent.count.merge$year)),]
# join the data
allyears <- rbind(inshore.count.purge,recent.count.merge)


# make our plot
# Div 3L plot
p1  <-  ggplot() + 
  geom_bar(aes(y = prop, x = year, fill = age.grp), data = allyears[which(allyears$area=="3L"),],stat="identity") + 
  scale_y_continuous(limits=c(0,1.01),expand=c(0,0), breaks = c(0, 0.5, 1)) +
  scale_x_continuous(limits=c(1979.5,2017.5),expand=c(0,0)) +
  theme_classic() +
  labs(x="Year",y="Proportion",fill="Age") +
  ggtitle("Inshore commercial - Division 3L") +
  theme(axis.text = element_text(face="bold",size=12),axis.title=element_text(face="bold",size=16),
        legend.title=element_text(face="bold",size=14),legend.text=element_text(size=12),axis.ticks=element_line(size=2)) +
  scale_fill_viridis_d()
p1

# Div 3K plot
p2  <-  ggplot() + geom_bar(aes(y = prop, x = year, fill = (age.grp)), data = allyears[which(allyears$area=="3K"),],
                          stat="identity") +
  scale_y_continuous(limits=c(0,1.01),expand=c(0,0), breaks = c(0, 0.5, 1)) +
  scale_x_continuous(limits=c(1979.5,2017.5),expand=c(0,0)) +
  theme_classic() +
  labs(x="Year",y="Proportion",fill="Age") +
  ggtitle("Inshore commercial - Division 3K") +
  theme(axis.text = element_text(face="bold",size=12),axis.title=element_text(face="bold",size=16),
        legend.title=element_text(face="bold",size=14),legend.text=element_text(size=12),axis.ticks=element_line(size=2)) +
  scale_fill_viridis_d()
p2

# reorganize stuff to put them onto a single plot
g1 <- ggplotGrob(p2)
g2 <- ggplotGrob(p1)

g <- rbind(g1,g2,size="first")
g$widths <- unit.pmax(g1$widths,g2$widths)
pdf(file="E:\\Capelin\\commercial\\agecomposition3k3l.pdf",width = 6.14,height=7.09)
grid.newpage()
grid.draw(g)
dev.off()

# just for kicks, using facet instead...
p3  <-  ggplot() + geom_bar(aes(y = prop, x = year, fill = age.grp), data = allyears,stat="identity")+scale_y_continuous(limits=c(0,1.01),expand=c(0,0))+scale_x_continuous(limits=c(1979.5,2017.5),expand=c(0,0))+theme_classic()+labs(x="Year",y="Proportion",fill="Age")+ggtitle("Inshore commercial - Division 3L")+theme(axis.text = element_text(face="bold",size=12),axis.title=element_text(face="bold",size=16),legend.title=element_text(face="bold",size=14),legend.text=element_text(size=12),axis.ticks=element_line(size=2))+facet_grid(area~.)
p3
