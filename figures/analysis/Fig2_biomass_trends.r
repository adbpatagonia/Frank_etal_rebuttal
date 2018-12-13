# Figure 2 capelin collapse paper
# Capelin biomass from multiple sources

# install ggsidekick from GitHub
#devtools::install_github("seananderson/ggsidekick")

#load libraries ----
library(cowplot)
library(scales)
library(RColorBrewer)
library(ggsidekick)

# load data ----
cb <- read.csv('data/Fig2_capelin_biomass.csv')

# data wrangling ----
# * order of surveys ----
cb <- transform(cb,
                area = factor(area,levels=c(
                  "fa",
                  "ussrfa",
                  "sa",
                  "ussrsa",
                  'tb'),
                labels = c(
                  "2J3K Fall Canada", 
                  '2J3K Fall USSR', 
                  "3L Spring Canada", 
                  "3LNO Spring USSR", 
                  'Trinity Bay (3L)')))


# plot vars ----
# point dodging
dg <- 0.5
# breaks
custom_breaks <- 1975:2017

# original plot ----
# mypalette <- brewer.pal(5, "Set1")
# p <- ggplot(data = cb, aes(x = year, y = biomass, group = area, shape = area, color = area))
# p <- p + geom_linerange(aes(ymin = (lcl), ymax = (ucl)), 
#                         position = position_dodge(width = dg), size = 0.5, na.rm = T)
# #p <- p + geom_line(linetype = 3)
# p <- p + geom_point(position = position_dodge(width = dg), size = 1.5)
# p <- p + scale_y_log10(labels = comma, limits = c(1,10000), breaks = c(1,10,100,1000,10000))
# p <- p + annotation_logticks(sides = "l")
# p <- p + theme_set(theme_cowplot())
# p <- p + labs(x = 'Year', y = 'Capelin biomass (kilotonnes)')
# p <- p + theme(legend.title = element_blank())#, axis.text.x  = element_text(angle=90, vjust=0.5))
# p <- p + scale_color_manual(values =  mypalette[c(1,2,4,3,5)], 
#                             breaks = c( levels(unique(cb$area))),
#                             labels = c("2J3K Fall Canada", '2J3K Fall USSR', "3L Spring Canada", "3LNO Spring USSR", 'Trinity Bay (3L)'))
# p <- p +   scale_shape_manual(breaks = c( levels(unique(cb$area))),
#                               values = c(15, 16, 18, 17, 8),
#                               labels = c("2J3K Fall Canada", '2J3K Fall USSR', "3L Spring Canada", "3LNO Spring USSR", 'Trinity Bay (3L)'))
# p <- p + scale_x_continuous(breaks = seq(1975, 2015, by = 5))
# p <- p + theme(legend.position = c(0.05, 0.18), 
#                legend.text = element_text(size = 7),
#                legend.key.size = unit(10, 'point'))
# p <- p + guides(shape = guide_legend(override.aes = list(linetype = c(0, 0, 1, 0, 1))))
# p


# plot in revied manuscript ----
#mypalette <- head(viridis::inferno(6), 5)
mypalette <- head(viridis::viridis(6), 5)
mypalette <- mypalette[c(2, 5, 3, 4, 1)]

p <- ggplot(data = cb, aes(x = year, y = biomass, group = area, shape = area, color = area))
p <- p + geom_linerange(aes(ymin = (lcl), ymax = (ucl)), 
                        position = position_dodge(width = dg), size = 0.5, na.rm = T,
                        show.legend = FALSE)

p <- p + geom_line(position = position_dodge(width = dg), size = 0.35, linetype = 1,
                   show.legend = FALSE)

p <- p + geom_point(position = position_dodge(width = dg), size = 2.5, stroke = 1, fill = "white")

p <- p + scale_y_log10(labels = comma, limits = c(1,10000), breaks = c(1,10,100,1000,10000))
p <- p + annotation_logticks(sides = "l")
#p <- p + theme_set(theme_cowplot())
p <- p + labs(x = 'Year', y = 'Capelin biomass (kilotonnes)')
p <- p + theme(legend.title = element_blank())#, axis.text.x  = element_text(angle=90, vjust=0.5))
p <- p + scale_color_manual(values =  mypalette)
p <- p +   scale_shape_manual(breaks = c( levels(unique(cb$area))),
                              values = c(16, 15, 21, 22, 18),
                              labels = c("2J3K Fall Canada", '2J3K Fall USSR', "3L Spring Canada", "3LNO Spring USSR", 'Trinity Bay (3L)'))
p <- p + scale_x_continuous(breaks = seq(1975, 2015, by = 5))



p <- p + theme_sleek()
p <- p + theme(legend.position = c(0.15, 0.18), 
               legend.text = element_text(size = 7),
               legend.key.size = unit(10, 'point'),
               legend.title=element_blank())

p <- p + guides(shape = guide_legend(override.aes = list(linetype = c(0, 0, 1, 1, 1))))




# save plots ----
# * png ----
cowplot::ggsave("output/Fig2_biomass_trends.png", p, width = 7.4, height = 4)
# * eps ----
setEPS()
postscript("output/Fig2_biomass_trends.eps", width = 7.4, height = 4)
p
dev.off()


# alternative colours ----
mypalette <- brewer.pal(5, "Spectral")
p2 <- p + scale_color_viridis_d(option = 'D')

cowplot::ggsave("output/Fig2_biomass_trends-alternative.png", p2, width = 7.4, height = 4)
setEPS()
postscript("output/Fig2_biomass_trends-alternative.eps", width = 7.4, height = 4)
p2
dev.off()
