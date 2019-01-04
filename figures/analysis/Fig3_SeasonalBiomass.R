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
mat <- read.csv('data/Fig3b_MaturityStage.csv')
age <- read.csv('data/Fig3c_AgeClasses.csv')
seasbio <- read.csv('data/Fig3a_SeasonalBiomass.csv')

# data wrangling ----
# capelin densities ----
seasbio$density <- with(seasbio, biomass.kg./surveyareakm2)
# * order of maturities ----
mat <- transform(mat,
                maturity = factor(maturity,levels=c(
                  "S/R",
                  "Mat",
                  "Imm")))
# * order of ages ----
age <- transform(age,
                 age = factor(age,levels=c(
                   "5", '4', '3', '2', '1'),
                   labels = c(
                     'Age 5', 'Age 4', 'Age 3', 'Age 2', 'Age 1'
                   )))
# * order of months ----
mat <- transform(mat,
                 month = factor(month,levels=c(
                   'Jan', 'May', 'Jun', 'Sep', 'Oct')))
age <- transform(age,
                 month = factor(month,levels=c(
                   'Jan', 'May', 'Jun', 'Sep', 'Oct')))

# * order of years ----
seasbio <- transform(seasbio,
                     Year = factor(Year, levels = c(
                                  '2003', '2004', '2005')))
# * order of months ----
seasbio <- transform(seasbio,
                     Month = factor(Month, levels = c(
                       'Jan', 'May', 'Jun', 'Sep', 'Oct')))


# plot ----

# * plot a, seasonal biomass ----
laby <- expression('Biomass density (kg '~km^-2~')')
pa <- ggplot(data = seasbio, aes(x = Month, y = density)) +
  geom_col(fill = 'black') +
#  scale_y_log10(labels = comma, limits = c(1,14000), breaks = c(1,10,100,1000,10000)) +
  
  xlab('') +
  ylab(laby) +
  facet_grid(~Year, scales = 'free_x', switch = 'x', space = 'free_x') +
  theme_sleek() +
  theme(strip.placement = 'outside',
        strip.background.x = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black')) 

# add log ticks
# a <- annotation_logticks(sides='l')
# a$data <- data.frame(x = NA, Year = '2003')
# pa <- pa + a

# * plot b, maturity class ----
pb <- ggplot(data = mat, aes(x = month, y = prop, fill = maturity)) +
  geom_col(width = 0.5) +
  scale_fill_viridis_d(option = 'D', name = '') +
  xlab('') +
  ylab('Proportion') +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  theme_sleek() +
  theme(legend.position = 'top')

# * plot c, age class ----
cols <- viridis::viridis(6)[2:6]
pc <- ggplot(data = age, aes(x = month, y = prop, fill = age)) +
  geom_col(width = 0.5) +
  scale_fill_manual(values = cols, name = '') +
  xlab('Month') +
  ylab('Proportion') +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  theme_sleek() +
  theme(legend.position = 'top') 


# * join plots ----
p <- cowplot::plot_grid(pa, pb, pc, ncol = 1, labels = 'auto', hjust = -0.01, axis = 'r')#rel_heights = c(.9, 0.9, 1))


# save plots ----
# * widht and height of plot ----
w <- 6
h <- 9
# * png ----
cowplot::ggsave("output/Fig3_seasonal_biomass.png", p, width = w, height = h)
# * eps ----
setEPS()
postscript("output/Fig3_seasonal_biomass.eps", width = w, height = h)
p
dev.off()

