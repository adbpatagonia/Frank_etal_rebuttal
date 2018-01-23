## load libraries
library(ggplot2)
library(dplyr)
library(plotly)
library(cowplot)

############################ COD CONDITION DATA#########

## read data
codcond <- read.csv("data/codcondrealdata.csv", header = T)

## assign the correct year. Survey year for fish collected in January 1988 was defined as 1987. By summing one to the year, we are using
## fish from Oct 1987-Jan 1988 and assigning them to 1988
codcond$year_corr <- codcond$syear + 1

## There were 4 fish that looked like outliers - extreme low or high condition
codcond <- subset(codcond, springfulton > 0.1 & springfulton < 1.5)



## bootstrap 90 % CI for springfulton by year, and nafo Division
bootful <- codcond %>%
  group_by(year_corr, NAFO) %>%
  do(sample_n(., 100, replace = TRUE)) %>%
  summarise(lci = quantile(springfulton, probs = 0.05, na.rm = T),
            uci = quantile(springfulton, probs = 0.95, na.rm = T),
            med = quantile(springfulton, probs = 0.5, na.rm = T))


dodge <- position_dodge(width = 0.5)
p <-   ggplot(data = bootful, aes(x = year_corr, y = med, shape = NAFO, colour = NAFO)) +
  #  geom_ribbon(aes(ymin = lcitw, ymax = ucitw,  fill = fage, linetype = NA), alpha = 0.3) +
  #geom_line(size = 1, position = dodge) +
  geom_point(size = 1.5, position = dodge) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0, size = 1, alpha = 0.5, position = dodge) +
#  scale_x_continuous(limits = c(min(mm$x), max(mm$x)), breaks = c(min(mm$x):max(mm$x)), labels = c(8:12, 1:7)) +
#  scale_colour_manual(values = mypalette) +
#  scale_fill_manual(values =  mypalette) +
  xlab('Year') +
  ylab("Fulton's K (spring)") +
  ggtitle("Median (and bootstrap 90% CI) Fultons K, by year and NAFO Div") +
  theme(plot.title = element_text(size = 12, face = "bold"))
ggplotly(p)
tiff(file = "output/condovertime_1panel.tif", width = 26, height = 15.5, units = "cm", family = "sans", bg = "white", pointsize = 8, res = 600, compression = "lzw")
  print(p)
dev.off()



## create data frames to store the information on density distribution and measures of central tendency for each year and nafo div
conddens <- data.frame(x = NA, y = NA, year = NA, nafo = NA)
centraltendency <- data.frame(meank = NA, mediank = NA, year = NA, nafo = NA)

## loop through years and nafo divisions to obtain density distributions, means and medians of condition
for (i in min(unique(codcond$year_corr)):max(unique(codcond$year_corr))) {
  for (j in unique(codcond$NAFO)) {
    dat <- subset(codcond, year_corr == i & NAFO == j)
    if (nrow(dat) > 2) {
      yy <- density(dat$springfulton)
      yy <- data.frame(x = yy$x, y = yy$y)
      yy$year <- rep(i, nrow(yy))
      yy$nafo <- rep(j, nrow(yy))
      conddens <- rbind(conddens, yy)
      ct <- data.frame(meank = mean(dat$springfulton), mediank = median(dat$springfulton), year = i, nafo = j)
      centraltendency <- rbind(centraltendency, ct)
    }
  }
}

## obtain the max density per year and nafo div and merge it with density distribution
maxy <- aggregate(conddens$y, by = list(conddens$nafo, conddens$year), FUN = max)
names(maxy) <- c("nafo", "year", "maxy")
conddens <- merge(conddens, maxy)

## merge data frames with information on density distributions and centraltendency
conddens <- merge(conddens, centraltendency)

## sum year to the max density per year and nafo div so that each histogram is ploted along the x-axis in the right year.  The max density
## each curve takes is slightly shifted down so that distribution in year t does not overlap with distribution in year t+1
conddens$yy <- with(conddens, year + (y/(maxy + maxy * 0.02)))

## obtain the range of conditions that encompass 95% of the data per year and nfo div and merge it with density distribution data
bounds <- aggregate(conddens$x, by = list(conddens$nafo, conddens$year), FUN = function(x) quantile(x, probs = c(0.025, 0.975)))
bounds <- data.frame(bounds[, 1:2], bounds[, 3][, 1], bounds[, 3][, 2])
names(bounds) <- c("nafo", "year", "lb", "ub")
conddens <- merge(conddens, bounds)

## define the label for the y-axis
fultonlab <- expression(paste("Fulton's ", italic(K[s])))


## Start ggplot

## define working dataset, x and y variables
p <- ggplot(conddens, aes(yy, x))
## this draws the histograms
p <- p + geom_line()
## Plot by NAFO Div
p <- p + facet_grid(nafo ~ .)
## Add x and y axis labels
p <- p + xlab("Year") + ylab(fultonlab)
## Add the grey ribbons that encompass 95% of data. Argument alpha controls the amount of transparency
p <- p + geom_ribbon(data = conddens, aes(x = yy, ymax = ub, ymin = lb), alpha = 0.15)
## Add the median per year and nafo div
p <- p + geom_line(data = conddens, aes(x = yy, y = mediank), colour = "red", lwd = 1)
## the next 3 lines plot Lambert and Dutil's benchmark values. I commented them out hline.data <- data.frame(nafo =
## rep(c('2J','3K','3L'),each=2),z = rep(c(0.7,0.85),3)) colores <- rep(c('darkgreen','red'),3) p <- p + geom_hline(data=hline.data,
## aes(yintercept = z,colour=colores),linetype='longdash')

## Aesthetic options
p <- p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5)) + theme(legend.position = "none")
p <- p + scale_x_continuous(breaks = seq(from = 1978, to = 2014, by = 2), limits = c(1982, 2014))
p <- p + scale_y_continuous(breaks = seq(from = 0.3, to = 1.5, by = 0.6))
p <- p + theme_bw()
## print plot
print(p)


## save plot to pdf file setwd('') pdf(file='condovertime.pdf',width=9.5,height=6.5, family = 'sans', bg = 'white', pointsize = 8) print(p)
## dev.off()

tiff(file = "output/condovertime.tif", width = 26, height = 15.5, units = "cm", family = "sans", bg = "white", pointsize = 8, res = 600, compression = "lzw")
  print(p)
dev.off()
