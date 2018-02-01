library(plotly)
library(dplyr)
setwd('D:/Buren_files/MEGA/papersAle/Frank_etal_2016_rebuttal/IcelandCod')


## cod ----
icecod <- read.table('Cod_Iceland.txt',  header = T, sep = ' ')
icecod17 <- read.table('Cod_Iceland_2017.txt',  header = T, sep = ' ')


with(icecod, plot(year, SSB))
with(icecod17, plot(year, age4.biom))


ggplotly(ggplot(data = icecod, aes(x = year, y = SSB)) + geom_point())


ggplotly(
  ggplot(data = filter(icecod, year > 1977 & year < 1999), 
         aes(x = year, y = SSB)) + 
    geom_point()
  )

 
ggplotly(
  ggplot(data = filter(icecod17, year > 1977 & year < 1999), 
         aes(x = year, y = age4.biom)) + 
    geom_point()
)


## capelin ----
# read data
abun <- read.csv('D:/Buren_files/MEGA/papersAle/Frank_etal_2016_rebuttal/IcelandCapelinStockBiomass/data/iceland_capelin_abundance.csv', header = T) %>%
  mutate(abun = abundance * 1000000000)
meanw <- read.csv('D:/Buren_files/MEGA/papersAle/Frank_etal_2016_rebuttal/IcelandCapelinStockBiomass/data/iceland_capelin_meanweight.csv', header = T)

left_join(abun, meanw, by = c('year', 'age', 'maturity')) %>%
  mutate(biomass = abun * meanweight * 1e-12) %>%    # stock biomass in million tonnes
  group_by(year) %>%
  summarize(stockbiomass = sum(biomass, na.rm = T)) %>%
  ggplot(aes(x = year, y = stockbiomass)) + geom_line() + ylab('stock biomass (million tonnes)')


icelandcap <- left_join(abun, meanw, by = c('year', 'age', 'maturity')) %>%
  mutate(biomass = abun * meanweight * 1e-12) %>%    # stock biomass in million tonnes
  group_by(year) %>%
  summarize(stockbiomass = sum(biomass, na.rm = T))


select(icecod17, year, age4.biom) %>%
  left_join(icelandcap) %>%
  mutate(ratiocapcod = stockbiomass/age4.biom) %>%
  filter(year > 1977 & year < 1999) %>%
  ggplot(aes(x = year, y = ratiocapcod)) + geom_line()


select(icecod, year, SSB) %>%
  left_join(icelandcap) %>%
  mutate(ratiocapcod = stockbiomass/SSB) %>%
  filter(year > 1977 & year < 1999) %>%
  ggplot(aes(x = year, y = ratiocapcod)) + geom_line()

