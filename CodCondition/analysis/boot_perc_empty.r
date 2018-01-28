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



empties <- codst %>%
  filter(complete.cases(empty)) %>%
  group_by(syear, nafo, empty) %>%
  summarise(n = n()) %>%
  mutate(perc = n / sum(n)) %>%
  filter(empty == 'YES')

## warning ----
# this takes a long time to run
# I ran it once and then saved output
# rewrote a bit of the code to load the svaed object
yy <- expand.grid(syear = unique(empties$syear), nafo = unique(empties$nafo))
nrep <- nboot
bootperc <- matrix(data = NA, nrow = nrow(yy), ncol = nrep)
for (i in 1:nrep) {
  temp <- codst %>%
    filter(complete.cases(empty)) %>%
    group_by(syear, nafo) %>%
    do(sample_n(., nboot, replace = TRUE)) %>%
    group_by(syear, nafo, empty) %>%
    summarise(n = n()) %>%
    mutate(perc = n / sum(n)) %>%
    filter(empty == 'YES') %>%
    right_join(yy, by = c("syear", "nafo"))
  bootperc[,i] <- temp$perc
}

# save output
save(bootperc,yy, file = 'interimsteps/bootperc.rdata')
