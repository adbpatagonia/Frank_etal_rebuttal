## load libraries ----
library(dplyr)
library(plotly)
library(cowplot)

## source functions ----
#source('R/geom_flat_violin.r')

## read data ----
codst <- read.csv("data/cod+turbot_fall_2J3KL_stomach_content_weights.csv", header = T)
nboot <- 10000

## data management ----
codst <- codst[c(3:5, 7:10)]
names(codst) <- c("sp", "syear", "length", "depth", "nafo", "stomweight", "empty")
codst <- subset(codst, sp == "Cod")
codst <- subset(codst, nafo == "2J" | nafo == "3K" | nafo == "3L")
codst <- subset(codst, syear > 1980)
codst <- subset(codst, length > 29 & length < 56)
codst$index <- 1:nrow(codst)



## plot ----
# trick to include all years (including those with no data in plot)
y <- expand.grid(syear = min(codst$syear):max(codst$syear), nafo = unique(codst$nafo))
codstplot <- merge(y, codst, by = c('syear', 'nafo'), all.x = T)

# dodge
dodge <- position_dodge(width = 0.5)


## bootstrap 90 % CI for stomach weight by year, and nafo Division ----
bootsw <- codst %>%
  group_by(syear, nafo) %>%
  do(sample_n(., nboot, replace = TRUE)) %>%
  summarise(lci = quantile(stomweight, probs = 0.025, na.rm = T),
            uci = quantile(stomweight, probs = 0.975, na.rm = T),
            med = mean(stomweight, na.rm = T))

p.stomw <-   ggplot(data = bootsw, aes(x = syear, y = med, shape = nafo, colour = nafo)) +
  geom_point(size = 1.5, position = dodge) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0, size = 1, alpha = 0.5, position = dodge) +
  xlab('Year') +
  ylab("Stomach contents weight (g)") +
  scale_x_continuous(limits = c(1980, 2015), breaks = seq(1980, 2015,5)) +
  theme(plot.title = element_text(size = 12, face = "bold"))

empties <- codst %>%
  filter(complete.cases(empty)) %>%
  group_by(syear, nafo, empty) %>%
  summarise(n = n()) %>%
  mutate(perc = n / sum(n)) %>%
  filter(empty == 'YES')

# load percent empty bootstrap
load('interimsteps/bootperc.rdata')


 bootbin <- cbind(yy,t(apply(bootperc, 1, quantile, probs = c(0.025,0.975), na.rm = TRUE)))
 names(bootbin) <- c("syear", "nafo" ,'lci', 'uci')


# merge bootstrap data with mean percent empty
empties <- inner_join(empties, bootbin, by = c("syear", "nafo"))


# trick to include all years (including those with no data in plot)
y <- expand.grid(syear = min(empties$syear):max(empties$syear), nafo = unique(empties$nafo))
emptiesplot <- merge(y, empties, by = c('syear', 'nafo'), all.x = T)
p.empties <-   ggplot(data = emptiesplot, aes(x = syear, y = perc, shape = nafo, colour = nafo)) +
#  geom_line() +
  geom_point(position = dodge) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0, size = 1, alpha = 0.5, position = dodge) +
  xlab('Year') +
  scale_x_continuous(limits = c(1980, 2015), breaks = seq(1980, 2015,5)) +
  ylab("Pecent of empty stomachs")



#save(p.empties, file = 'interimsteps/pempties.rdata')
#save(p.stomw, file = 'interimsteps/pstomw.rdata')

# tiff(file = "output/Stomach_Contents_Weight_points.tif", width = 30, height = 15.5, units = "cm", family = "sans", bg = "white", pointsize = 8, res = 600, compression = "lzw")
# print(p.stomw)
# dev.off()
# tiff(file = "output/percent_empty_stomachs.tif", width = 30, height = 15.5, units = "cm", family = "sans", bg = "white", pointsize = 8, res = 600, compression = "lzw")
# print(p.empties)
# dev.off()

# # plot
# p <-   ggplot(data = codstplot, aes(x = factor(syear), y = stomweight, fill = nafo, colour = nafo)) +
#   geom_violin(position = dodge) +
#   xlab('Year') +
#   ylab("Stomch contents weight (g)") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# # save plot
# #ggsave("output/Stomach_Contents_Weight.png", height = 5, width = 9)
# tiff(file = "output/Stomach_Contents_Weight.tif", width = 30, height = 15.5, units = "cm", family = "sans", bg = "white", pointsize = 8, res = 600, compression = "lzw")
#    print(p)
# dev.off()
