## load libraries
library(dplyr)
library(plotly)
library(cowplot)

############################ COD CONDITION DATA#########

## read data
codcond <- read.csv("data/codcondrealdata.csv", header = T)
nboot <- 10000

## assign the correct year. Survey year for fish collected in January 1988 was defined as 1987. By summing one to the year, we are using
## fish from Oct 1987-Jan 1988 and assigning them to 1988
codcond$year_corr <- codcond$syear + 1

## There is a clear mistake in one weight (it states 49 kg, it should be 0.49 kg)
codcond[which(codcond$GUTTED_WEIGHT_KG == 49),'GUTTED_WEIGHT_KG'] <- 0.49
codcond[which(codcond$GUTTED_WEIGHT_KG == 6.94),'GUTTED_WEIGHT_KG'] <- 0.694

## calculate the proportion of gutted weight
codcond$propgut <- codcond$GUTTED_WEIGHT_KG/codcond$WEIGHT_KG

## There were 4 fish that looked like outliers - extreme low or high condition
codcond <- subset(codcond, springfulton > 0.1 & springfulton < 1.5)


# dodge the violins
dodge <- position_dodge(width = 0.5)
# plot
p <-   ggplot(data = codcond, aes(x = factor(year_corr), y = springfulton, fill = NAFO, colour = NAFO)) +
  geom_violin(position = dodge) +
  xlab('Year') +
  ylab("Fulton's K (spring)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



## bootstrap 90 % CI for springfulton by year, and nafo Division
bootful <- codcond %>%
  group_by(year_corr, NAFO) %>%
  do(sample_n(., nboot, replace = TRUE)) %>%
  summarise(lci = quantile(springfulton, probs = 0.025, na.rm = T),
            uci = quantile(springfulton, probs = 0.975, na.rm = T),
            med = mean(springfulton,  na.rm = T))

fultonlab <- expression(paste("Fulton's ", italic(K[s])))
dodge <- position_dodge(width = 0.5)
p.condition <-   ggplot(data = bootful, aes(x = year_corr, y = med, shape = NAFO, colour = NAFO)) +
  geom_point(size = 1.5, position = dodge) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0, size = 1, alpha = 0.5, position = dodge) +
  scale_x_continuous(limits = c(1980, 2015), breaks = seq(1980, 2015,5)) +
  xlab('Year') +
  ylab(fultonlab) +
  geom_hline(yintercept = 0.7,  lty = 2) +
  geom_hline(yintercept = 0.85, lty = 2) +
  theme(legend.justification = c(1,1), legend.position = c(.98,1))
p.condition

save(p.condition, file = 'interimsteps/pcondition.rdata')

# tiff(file = "output/condovertime_1panel.tif", width = 30, height = 15.5, units = "cm", family = "sans", bg = "white", pointsize = 8, res = 600, compression = "lzw")
#   print(p.condition)
# dev.off()


## hepatosomatic index ----
## HSI = 100 (LW/W)
## LW = liver weight; W = somatic weight (total weight less gonad and stomach content weights)
boothsi <- codcond %>%
  mutate(HSI = (100 * LIVER_WEIGHT_GR)/(GUTTED_WEIGHT_KG*1000)) %>%
  group_by(year_corr, NAFO) %>%
  do(sample_n(., nboot, replace = TRUE)) %>%
  summarise(lci = quantile(HSI, probs = 0.025, na.rm = T),
            uci = quantile(HSI, probs = 0.975, na.rm = T),
            med = mean(HSI,  na.rm = T))

p.hsi <-   ggplot(data = boothsi, aes(x = year_corr, y = med, shape = NAFO, colour = NAFO)) +
  geom_point(size = 1.5, position = dodge) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0, size = 1, alpha = 0.5, position = dodge) +
  scale_x_continuous(limits = c(1980, 2015), breaks = seq(1980, 2015,5)) +
  xlab('Year') +
  ylab('HSI')# +
  theme(legend.position = "none")
p.hsi



## proportion of somatic weight ----
meanps <- codcond %>%
  filter(propgut < 1) %>%
  group_by(year_corr, NAFO) %>%
  summarise(med = mean(propgut,  na.rm = T))
bootps <- codcond %>%
  filter(propgut < 1) %>%
  group_by(year_corr, NAFO) %>%
  do(sample_n(., nboot, replace = TRUE)) %>%
  summarise(lci = quantile(propgut, probs = 0.025, na.rm = T),
            uci = quantile(propgut, probs = 0.975, na.rm = T))  %>%
  right_join(meanps, by = c('year_corr', 'NAFO'))



p.ps <-   ggplot(data = bootps, aes(x = year_corr, y = med, shape = NAFO, colour = NAFO)) +
  geom_point(size = 1.5, position = dodge) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0, size = 1, alpha = 0.5, position = dodge) +
  scale_x_continuous(limits = c(1980, 2015), breaks = seq(1980, 2015,5)) +
  xlab('Year') +
  ylab('Proportion somatic weight') +
  theme(legend.position = "none")

p.ps
ggplotly(p.ps)
