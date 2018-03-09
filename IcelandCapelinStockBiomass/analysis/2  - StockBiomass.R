## In step 1 I extracted data from tables in report, exported them to csv, and edited them to tidy data in Excel
## Now I will merge abundance, and mean weight at age data to create a stock biomass plot

# load libraries
library(cowplot)
library(dplyr)

# read data
abun <- read.csv('data/iceland_capelin_abundance.csv', header = T) %>%
  mutate(abun = abundance * 1000000000)
meanw <- read.csv('data/iceland_capelin_meanweight.csv', header = T)

left_join(abun, meanw, by = c('year', 'age', 'maturity')) %>%
  mutate(biomass = abun * meanweight * 1e-12) %>%    # stock biomass in million tonnes
  group_by(year) %>%
  summarize(stockbiomass = sum(biomass, na.rm = T)) %>%
  ggplot(aes(x = year, y = (stockbiomass) )) + geom_line() + geom_point() + ylab('stock biomass (million tonnes)')


icelandcap <- left_join(abun, meanw, by = c('year', 'age', 'maturity')) %>%
  mutate(biomass = abun * meanweight * 1e-12) %>%    # stock biomass in million tonnes
  group_by(year) %>%
  summarize(stockbiomass = sum(biomass, na.rm = T))
