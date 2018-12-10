# Figure 3 capelin collapse paper
# Seasonal Capelin biomass

# install ggsidekick from GitHub
#devtools::install_github("seananderson/ggsidekick")

#load libraries ----
library(cowplot)
library(scales)
library(RColorBrewer)
library(ggsidekick)

# load data ----
dens <- read.csv('data/Fig6_LarvalDensities.csv')

# data wrangling ----
dens$period <- factor(ifelse(dens$year < 1990, 'Pre 1991', 'Post 1991'))
# * order of period ----
dens <- transform(dens,
                 period = factor(period,levels=c(
                   'Pre 1991', 'Post 1991')))



# plot ----

# * plot a, seasonal biomass ----
laby <- expression('#Capelin larvae '~m^-2)
p <- ggplot(data = dens, aes(x = factor(year), y = density)) +
  geom_col(fill = 'black') +
 # scale_y_log10(labels = comma, limits = c(1,14000), breaks = c(1,10,100,1000,10000)) +
  
  xlab('Year') +
  ylab(laby) +
  facet_grid(~period, scales = 'free_x', switch = 'x', space = 'free_x') +
  theme_sleek() +
  theme(strip.placement = 'outside',
        strip.background.x = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black')) 

# save plots ----
# * widht and height of plot ----
w <- 7.4
h <- 4
# * png ----
cowplot::ggsave("output/Fig6_LarvalDensity.png", p, width = w, height = h)
# * eps ----
setEPS()
postscript("output/Fig6_LarvalDensity.eps", width = w, height = h)
p
dev.off()

