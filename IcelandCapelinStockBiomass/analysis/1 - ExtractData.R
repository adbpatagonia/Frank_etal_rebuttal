## import data from pdf file ICES 2017 NWWG Report

# load libraries
library(tabulizer)

# extract data from pdfs
abun <- do.call(rbind, extract_tables('D:/Buren_files/MEGA/papersAle/Frank_etal_2016_rebuttal/IcelandCapelinStockBiomass/data-raw/ICES_2017_iceland_capelin_abundance.pdf'))
meanw <- do.call(rbind, extract_tables('D:/Buren_files/MEGA/papersAle/Frank_etal_2016_rebuttal/IcelandCapelinStockBiomass/data-raw/ICES_2017_iceland_capelin_meanweight.pdf'))

# save data to csv - modify in Excel
write.csv(abun, 'data-raw/ICES_2017_iceland_capelin_abundance.csv', row.names = F)
write.csv(meanw, 'data-raw/ICES_2017_iceland_capelin_meanweight.csv', row.names = F)
