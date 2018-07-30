
library(cowplot)
library(scales)
library(RColorBrewer)
setwd('D:/Buren_files/MEGA/papersAle/Frank_etal_2016_rebuttal/figures')
cb <- read.csv('capelin_biomass.csv')

every_nth <- function(x, nth, empty = TRUE, inverse = FALSE) 
{
  if (!inverse) {
    if (empty) {
      x[1:nth == 1] <- ""
      x
    } else {
      x[1:nth != 1]
    }
  } else {
    if (empty) {
      x[1:nth != 1] <- ""
      x
    } else {
      x[1:nth == 1]
    }
  }
}

dg <- 0.5



custom_breaks <- 1975:2017

mypalette <- brewer.pal(5, "Set1")


cb <- transform(cb,
                     area = factor(area,levels=c(
                       "fa",
                       "ussrfa",
                       "sa",
                       "ussrsa",
                       'tb')))


p <- ggplot(data = cb, aes(x = year, y = biomass, group = area, shape = area, color = area))
p <- p + geom_linerange(aes(ymin = (lcl), ymax = (ucl)), 
                        position = position_dodge(width = dg), size = 0.5, na.rm = T)
#p <- p + geom_line(linetype = 3)
p <- p + geom_point(position = position_dodge(width = dg), size = 1.5)
p <- p + scale_y_log10(labels = comma, limits = c(1,10000), breaks = c(1,10,100,1000,10000))
p <- p + annotation_logticks(sides = "l")
p <- p + theme_set(theme_cowplot())
p <- p + labs(x = 'Year', y = 'Capelin biomass (kilotonnes)')
p <- p + theme(legend.title = element_blank())#, axis.text.x  = element_text(angle=90, vjust=0.5))
p <- p + scale_color_manual(values =  mypalette[c(1,2,4,3,5)], 
                            breaks = c( levels(unique(cb$area))),
                            labels = c("2J3K Fall Canada", '2J3K Fall USSR', "3L Spring Canada", "3LNO Spring USSR", 'Trinity Bay (3L)'))
p <- p +   scale_shape_manual(breaks = c( levels(unique(cb$area))),
                              values = c(15, 16, 18, 17, 8),
                              labels = c("2J3K Fall Canada", '2J3K Fall USSR', "3L Spring Canada", "3LNO Spring USSR", 'Trinity Bay (3L)'))
p <- p + scale_x_continuous(breaks = seq(1975, 2015, by = 5))
p <- p + theme(legend.position = c(0.05, 0.18), 
               legend.text = element_text(size = 7),
               legend.key.size = unit(10, 'point'))
p <- p + guides(shape = guide_legend(override.aes = list(linetype = c(0, 0, 1, 0, 1))))
p



save_plot("Fig4.png", p, base_aspect_ratio = 1.8) # make room for figure legend)

# 
# p <- ggplot(data = subset(cb, year > 1990), aes(x = year, y = biomass, group = area, shape = area, color = area))
# p <- p + geom_point(position = position_dodge(width = dg), size = 1.5)
# p <- p + geom_linerange(aes(ymin = (lcl), ymax = (ucl)), position = position_dodge(width = dg), size = 0.7)
# #p <- p + scale_y(labels = comma, limits = c(1,10000), breaks = c(1,10,100,1000,10000))
# p <- p + theme_set(theme_cowplot())
# p <- p + labs(x = 'Year', y = 'Capelin biomass (kilotonnes)')
# p <- p + theme(legend.title = element_blank())#, axis.text.x  = element_text(angle=90, vjust=0.5))
# p <- p + scale_color_manual(values = c("black", "grey40"), 
#                             breaks = c('tb','offshore'),
#                             labels = c("Trinity Bay", "Offshore"))
# p <- p +   scale_shape_discrete(breaks = c('tb','offshore'),
#                                 labels = c("Trinity Bay", "Offshore"))
# # p <- p + scale_x_continuous(breaks = custom_breaks,
# #                             labels = every_nth(custom_breaks, 5, inverse = TRUE))
# p <- p + scale_x_continuous(breaks = seq(1991, 2015, by = 5))
# #p <- p + geom_segment(aes(x = seq(1985, 2017, by = 1), y = rep(1, 33), xend = seq(1985, 2017, by = 1), yend = rep(10000, 33)))
# p <- p + theme(legend.position = c(0.05, 1))
# p
# 
# cb %>% 
#   group_by(year) 
# 
# 
# a <- cb %>% 
#   group_by(year) %>%
#   summarise(aa = biomass[area == "tb"] / biomass[area == "offshore"] )
